REPORT  zbpci029_avgactual MESSAGE-ID zs.
************************************************************************
*  Project:    Cost Of Gas(Enbridge)                                   *
*  Date:       Jun 2021                                                *
*  Author:     Jaydeep Waychal/Durgaprakash Biruduraju                 *
*  Object ID                                                           *
*  Program Description:                                                *
*  This program will extract data for Average Use actuals for          *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date     Issue  By      Description                                   *

************************************************************************

TABLES: ce11100.

TYPES:  BEGIN OF ty_ce11100,
          gjahr       LIKE ce11100-gjahr,
          perde       LIKE ce11100-bukrs,
          bukrs       LIKE ce11100-bukrs,
          vkorg       LIKE ce11100-vkorg,
          kstar       LIKE ce11100-kstar,
          wwprg       LIKE ce11100-wwprg,
          wwsub       LIKE ce11100-wwsub,
          wwrat       LIKE ce11100-wwrat,
          wwser       LIKE ce11100-wwser,
          wwsld       LIKE ce11100-wwsld,
          vvbrv       LIKE ce11100-vvbrv,
        END OF ty_ce11100.

TYPES:  BEGIN OF ty_output,
          account(20)     TYPE c,
          category(6)     TYPE c,
          datasrc(20)     TYPE c,
          entity(3)       TYPE c,
          intco(8)        TYPE c,
          rptcurrency(2)  TYPE c,
          time(8)         TYPE c,
          customer(16)    TYPE c,
          dso(2)          TYPE c,
          service(13)     TYPE c,
          projectst(12)   TYPE c,
          paths(7)        TYPE c,
          amount(15)      TYPE p DECIMALS 2,
        END OF ty_output.

DATA: lv_local      TYPE integer,
      wa_ce11100    LIKE coep,
      s_ce11100     TYPE ty_ce11100,
      t_ce11100     LIKE TABLE OF s_ce11100,
      t_ce11100_sum LIKE TABLE OF s_ce11100,
      s_output      TYPE ty_output,
      t_output      LIKE TABLE OF s_output,
      t_output_sum  LIKE TABLE OF s_output,
      st_datarec    TYPE string,
      t_data        LIKE TABLE OF st_datarec.

DATA: msg(80)           TYPE c,
      lv_account(20)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(20)    TYPE c,
      lv_entity(3)      TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_customer(16)   TYPE c,
      lv_dso(2)         TYPE c,
      lv_service(13)    TYPE c,
      lv_projectst(12)  TYPE c,
      lv_paths(7)       TYPE c,
      lv_amount(15)     TYPE c,
      w_amount(15)      TYPE p DECIMALS 2.

DATA: tuchfile          LIKE rfpdo-rfbifile.

CONSTANTS: c_delimtr  TYPE c VALUE ','.

************************************************************************
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

PARAMETERS:
p_bukrs     LIKE t001-bukrs OBLIGATORY,
p_paledg    LIKE ce11100-paledger OBLIGATORY,
p_perio     LIKE ce11100-perio     OBLIGATORY.

SELECT-OPTIONS:
s_vrgar     FOR ce11100-vrgar     OBLIGATORY,
s_kndnr     FOR ce11100-kndnr     OBLIGATORY,
s_kstar     FOR ce11100-kstar     OBLIGATORY DEFAULT '390598'.

SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local  RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_file   TYPE string
                     DEFAULT 'H:\SAPTEMP\ODLActual.csv',
            p_server RADIOBUTTON GROUP rad1,
            csvfile  LIKE        rfpdo-rfbifile.

SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/cog/'
         INTO csvfile.

************************************************************************
************************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File' ##NO_TEXT,
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  IF p_local = 'X'.
    PERFORM check_file_path.
  ENDIF.

************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'cog.tch' INTO tuchfile.

  IF p_bukrs = 'UGL'.
    CONCATENATE csvfile 'SAP-' p_perio(4) '-' p_perio+5(2) '-ODL.csv'
           INTO csvfile.
  ELSEIF p_bukrs ='EGD'.
    CONCATENATE csvfile 'SAP-EGD-' p_perio(4) '-' p_perio+5(2) '-ODL.csv'
            INTO csvfile.
  ENDIF.

  IF p_local = 'X'.
    lv_local = 1.
  ELSE.
    lv_local = 0.
  ENDIF.

  PERFORM get_db_data.
*  PERFORM sumarize_data.
*  PERFORM print_report.                                      "SDP57440
  PERFORM generate_report.                                  "SDP57440
  IF lv_local = 0.
    PERFORM create_touch_file.
  ENDIF.

*----------------------------------------------------------------------*
FORM get_db_data.

  SELECT gjahr perde bukrs vkorg kstar wwprg wwsub wwrat wwser wwsld
         SUM( vvbrv )
    INTO  TABLE t_ce11100_sum
    FROM ce11100
    WHERE paledger  = p_paledg
      AND vrgar     IN s_vrgar
      AND versi     = ''
      AND perio     =  p_perio
      AND bukrs     = p_bukrs
      AND kstar     IN s_kstar
      AND kokrs     = '10'
      AND kndnr     IN s_kndnr
    GROUP BY gjahr perde bukrs vkorg kstar wwprg wwsub wwrat wwser wwsld .

ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.
  SORT t_ce11100 ASCENDING BY
       gjahr perde wwser wwprg wwrat wwsld kstar vkorg.

  CLEAR t_ce11100_sum.

  LOOP AT t_ce11100 INTO s_ce11100.
*Last sorted field is vkorg                                   "SDP57440
    AT END OF vkorg.                                        "SDP57440
      SUM.
      APPEND s_ce11100 TO t_ce11100_sum.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "sumarize_data

*&---------------------------------------------------------------------*
*&      Form  generate_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM generate_report.                                       "SDP57440
  DATA: t_bpcmap TYPE STANDARD TABLE OF zfit_bpcmaprev,
        w_bpcmap TYPE zfit_bpcmaprev.

  REFRESH: t_bpcmap.

  SELECT * FROM zfit_bpcmaprev INTO TABLE t_bpcmap.     "#EC CI_NOWHERE

  lv_entity = p_bukrs.
                                                            "SDP57440
*Add output table to the csvfile.                           "SDP57440
  LOOP AT t_ce11100_sum INTO s_ce11100.                     "SDP57440
                                                            "SDP57440
    CLEAR: w_amount, lv_amount.                             "SDP57440
    IF s_ce11100-vvbrv < 0.                                 "SDP57440
      w_amount = s_ce11100-vvbrv * -1.                      "SDP57440
      lv_amount = w_amount.                                 "SDP57440
      SHIFT lv_amount LEFT DELETING LEADING ' '.            "SDP57440
      CONCATENATE '-' lv_amount INTO lv_amount.             "SDP57440
    ELSE.                                                   "SDP57440
      w_amount = s_ce11100-vvbrv.                           "SDP57440
      lv_amount = w_amount.                                 "SDP57440
      SHIFT lv_amount LEFT DELETING LEADING ' '.            "SDP57440
    ENDIF.
    clear lv_service.
    IF s_ce11100-wwrat is not INITIAL.
      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWRAT'
                                                 value = s_ce11100-wwrat.
      IF w_bpcmap-bukrs is not INITIAL.
        CLEAR w_bpcmap.
        READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWRAT'
                                                   value = s_ce11100-wwrat
                                                   bukrs = p_bukrs.
      ENDIF.
      IF w_bpcmap-service IS NOT INITIAL.
        lv_service = w_bpcmap-service.
      ELSE.
        lv_service = s_ce11100-wwrat.
      ENDIF.
    ENDIF.                                                  "SDP57440
    CONCATENATE   lv_entity s_ce11100-gjahr s_ce11100-perde "SDP57440
                  s_ce11100-wwsub lv_service                "SDP57440
                  s_ce11100-kstar lv_amount                 "SDP57440
                  s_ce11100-vkorg                           "SDP57440
                  INTO st_datarec SEPARATED BY c_delimtr.   "SDP57440
    IF lv_amount <> 0.                                      "SDP57440
      APPEND st_datarec TO t_data.                          "SDP57440
      CLEAR st_datarec.                                     "SDP57440
    ENDIF.                                                  "SDP57440
  ENDLOOP.                                                  "SDP57440
  IF t_data IS NOT INITIAL.
    IF lv_local = 0.                                        "SDP57440
                                                            "SDP57440
      PERFORM open_csvfile.                                 "SDP57440
                                                            "SDP57440
      LOOP AT t_data INTO st_datarec.                       "SDP57440
        TRANSFER st_datarec TO csvfile.                     "SDP57440
      ENDLOOP.                                              "SDP57440
                                                            "SDP57440
      PERFORM close_csvfile.                                "SDP57440
*      WRITE: 'File Outputed Successfully to: ', csvfile.    "SDP57440
      WRITE: text-015, csvfile.
                                                            "SDP57440
    ELSE.                                                   "SDP57440
      CALL FUNCTION 'GUI_DOWNLOAD'                          "SDP57440
        EXPORTING                                           "SDP57440
          filename                = p_file                  "SDP57440
        TABLES                                              "SDP57440
          data_tab                = t_data                  "SDP57440
        EXCEPTIONS                                          "SDP57440
          file_write_error        = 1                       "SDP57440
          no_batch                = 2                       "SDP57440
          gui_refuse_filetransfer = 3                       "SDP57440
          invalid_type            = 4                       "SDP57440
          no_authority            = 5                       "SDP57440
          unknown_error           = 6                       "SDP57440
          header_not_allowed      = 7                       "SDP57440
          separator_not_allowed   = 8                       "SDP57440
          filesize_not_allowed    = 9                       "SDP57440
          header_too_long         = 10                      "SDP57440
          dp_error_create         = 11                      "SDP57440
          dp_error_send           = 12                      "SDP57440
          dp_error_write          = 13                      "SDP57440
          unknown_dp_error        = 14                      "SDP57440
          access_denied           = 15                      "SDP57440
          dp_out_of_memory        = 16                      "SDP57440
          disk_full               = 17                      "SDP57440
          dp_timeout              = 18                      "SDP57440
          file_not_found          = 19                      "SDP57440
          dataprovider_exception  = 20                      "SDP57440
          control_flush_error     = 21                      "SDP57440
          OTHERS                  = 22.                     "SDP57440
      IF sy-subrc <> 0.                                     "SDP57440
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno   "SDP57440
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.   "SDP57440
      ENDIF.                                                "SDP57440
*      WRITE: 'File Outputed Successfully to: ', p_file.     "SDP57440
      WRITE: text-015, p_file.
    ENDIF.                                                  "SDP57440
  ELSE.
    MESSAGE i019 WITH text-014.
  ENDIF.

ENDFORM.                                                    "SDP57440

*----------------------------------------------------------------------*
FORM open_csvfile.
  OPEN DATASET csvfile FOR OUTPUT
                       IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "OPEN_CSVFILE

*----------------------------------------------------------------------*
FORM close_csvfile.
  CLOSE DATASET csvfile.
  IF sy-subrc NE '0'.
*    MESSAGE e019 WITH 'unsuccessfl close' csvfile msg.
    MESSAGE e019 WITH text-016 csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "CLOSE_ALL_FILES

*&---------------------------------------------------------------------*
*Create Touch File.
*----------------------------------------------------------------------*
FORM create_touch_file.
  OPEN DATASET tuchfile FOR OUTPUT
                        IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH tuchfile msg.
    STOP.
  ENDIF.

*  TRANSFER 'This is a touch file' TO tuchfile.
  TRANSFER text-017 TO tuchfile.

  CLOSE DATASET tuchfile.
  IF sy-subrc NE '0'.
*    MESSAGE e019 WITH 'unsuccessfl close' tuchfile msg.
    MESSAGE e019 WITH text-016 tuchfile msg.
    STOP.
  ENDIF.

ENDFORM.                    "CREATE_TOUCH_FILE

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098 ##TEXT_POOL.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path ##TEXT_POOL.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
