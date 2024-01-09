REPORT  zbpci030_othactual MESSAGE-ID zs.
************************************************************************
*  Project:    Cost Of Gas(Enbridge)                                   *
*  Date:       07 july 2021                                            *
*  Author:     Jaydeep Waychal/Durgaprakash Biruduraju                 *
*  Object ID                                                           *
*  Program Description:                                                *
*  This program will extract data for Other Revenue actuals for        *
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
          artnr       LIKE ce11100-artnr,
          bukrs       LIKE ce11100-bukrs,
          werks       LIKE ce11100-werks,
          wwprg       LIKE ce11100-wwprg,
          wwsub       LIKE ce11100-wwsub,
          wwrat       LIKE ce11100-wwrat,
          wwser       LIKE ce11100-wwser,
          wwpce       LIKE ce11100-wwpce,
          vvbrv       LIKE ce11100-vvbrv,
          vvord       LIKE ce11100-vvord,
          vvori       LIKE ce11100-vvori,
        END OF ty_ce11100.

TYPES:  BEGIN OF ty_output,
          account(15)     TYPE c,
          category(6)     TYPE c,
          datasrc(12)     TYPE c,
          entity(3)       TYPE c,
          intco(8)        TYPE c,
          rptcurrency(2)  TYPE c,
          time(8)         TYPE c,
          customer(16)    TYPE c,
          dso(6)          TYPE c,
          service(13)     TYPE c,
          projectst(12)   TYPE c,
          paths(10)        TYPE c,
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
      lv_account(15)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(12)    TYPE c,
      lv_entity(3)      TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_customer(16)   TYPE c,
      lv_dso(6)         TYPE c,
      lv_service(13)    TYPE c,
      lv_projectst(12)  TYPE c,
      lv_paths(10)       TYPE c,
      lv_amount(15)     TYPE c.

DATA: tuchfile          LIKE rfpdo-rfbifile.

CONSTANTS: c_delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

PARAMETERS:
p_bukrs     LIKE t001-bukrs OBLIGATORY,
p_paledg    LIKE ce11100-paledger OBLIGATORY,
p_perio     LIKE ce11100-perio     OBLIGATORY.

SELECT-OPTIONS:
s_vrgar     FOR ce11100-vrgar     OBLIGATORY,
s_wwpce     FOR ce11100-wwpce     OBLIGATORY.

SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_file    TYPE        string DEFAULT 'H:\SAPTEMP\OTHActual.csv',
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile.

SELECTION-SCREEN END OF BLOCK b1.

*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/cog/' INTO csvfile.

*************************************************************************
*************************************************************************

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

*************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'cog.tch' INTO tuchfile.

  IF p_bukrs = 'UGL'.
    CONCATENATE csvfile 'SAP-' p_perio(4) '-' p_perio+5(2) '-Oth.csv' INTO csvfile.
  ELSEIF p_bukrs = 'EGD'.
    CONCATENATE csvfile 'SAP-EGD-' p_perio(4) '-' p_perio+5(2) '-Oth.csv' INTO csvfile.
  ENDIF.

  IF p_local = 'X'.
    lv_local = 1.
  ELSE.
    lv_local = 0.
  ENDIF.

  PERFORM get_db_data.
*  PERFORM sumarize_data.
  PERFORM print_report.
  IF lv_local = 0.
    PERFORM create_touch_file.
  ENDIF.

*----------------------------------------------------------------------*
FORM get_db_data.

  SELECT gjahr perde artnr bukrs werks wwprg wwsub wwrat wwser wwpce
    SUM( vvbrv ) SUM( vvord ) SUM( vvori )
    INTO TABLE t_ce11100_sum
    FROM ce11100
    WHERE paledger  = p_paledg
      AND vrgar     IN s_vrgar
      AND versi     = ''
      AND perio     =  p_perio
      AND bukrs     = p_bukrs
      AND wwpce     IN s_wwpce
    GROUP BY gjahr perde artnr bukrs werks wwprg wwsub wwrat wwser wwpce.

ENDFORM.                    "get_db_data

*----------------------------------------------------------------------*
FORM sumarize_data.
  SORT t_ce11100 ASCENDING BY gjahr perde wwser wwprg wwrat werks wwpce.

  CLEAR t_ce11100_sum.

  LOOP AT t_ce11100 INTO s_ce11100.
*Last sorted field is wwpce
    AT END OF wwpce.
      SUM.
      APPEND s_ce11100 TO t_ce11100_sum.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "sumarize_data

*----------------------------------------------------------------------*
FORM print_report.

  CLEAR t_output.

*Set the hard-coded values.

  lv_category     = 'Actual' ##NO_TEXT.
  lv_datasrc      = 'SAP_OtherRev'.
  lv_entity       = p_bukrs.
  lv_intco        = 'No_IntCo'.
  lv_rptcurrency  = 'LC'.
  lv_dso          = 'No_DSO'.
  lv_paths        = 'No_DelArea'.
  lv_projectst    = 'No_ProjectST'.

  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011 text-012 text-013
              INTO st_datarec SEPARATED BY c_delimtr.

  APPEND st_datarec TO t_data.

  DATA: lv_blart(2) TYPE c.

  DATA: t_bpcmap TYPE STANDARD TABLE OF zfit_bpcmap,
        w_bpcmap TYPE zfit_bpcmap.

  REFRESH: t_bpcmap.

  SELECT * FROM zfit_bpcmap INTO TABLE t_bpcmap.        "#EC CI_NOWHERE

  LOOP AT t_ce11100_sum INTO s_ce11100.
    CLEAR:  st_datarec, lv_blart, lv_account, lv_customer, lv_service, lv_time, lv_amount.

    CONCATENATE s_ce11100-wwpce+4 '_' s_ce11100-wwsub '_rev' INTO lv_account.

    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWPCE'
                                               value = s_ce11100-wwpce+4.
    IF w_bpcmap-account IS INITIAL.
      lv_account = s_ce11100-wwpce+4.
    ELSE.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'ARTNR'
                                                   value = s_ce11100-artnr+12.
      IF w_bpcmap-account IS NOT INITIAL.
        lv_account = w_bpcmap-account.
      ELSE.
        lv_account = s_ce11100-wwpce+4.
      ENDIF.
    ENDIF.


    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'ACCOUNT'
                                               value = lv_account. "'0000390686'
    IF sy-subrc NE 0.
      lv_customer     = 'No_Customer'.
      lv_service      = 'No_Service'.
    ELSE.
      lv_customer  = w_bpcmap-customer.
      lv_service   = w_bpcmap-service.
    ENDIF.


    CONCATENATE s_ce11100-gjahr s_ce11100-perde+1(2) '00' INTO lv_time.

    lv_amount = s_ce11100-vvord + s_ce11100-vvori + s_ce11100-vvbrv.

    s_output-account      = lv_account.
    s_output-category     = lv_category.
    s_output-datasrc      = lv_datasrc.
    s_output-entity       = lv_entity.
    s_output-intco        = lv_intco.
    s_output-rptcurrency  = lv_rptcurrency.
    s_output-time         = lv_time.
    s_output-customer     = lv_customer.
    s_output-dso          = lv_dso.
    s_output-service      = lv_service.
    s_output-paths        = lv_paths.
    s_output-projectst    = lv_projectst.
    s_output-amount       = lv_amount.

    APPEND s_output TO t_output.



  ENDLOOP.

*Summarize the final data.
  SORT t_output ASCENDING BY account time paths .

  CLEAR t_output_sum.

  LOOP AT t_output INTO s_output.
*Last sorted field is paths
    AT END OF paths.
      SUM.
      APPEND s_output TO t_output_sum.
    ENDAT.
  ENDLOOP.

*Add output table to the csvfile.
  LOOP AT t_output_sum INTO s_output.

    IF s_output-amount < 0.
      s_output-amount = s_output-amount * -1.
      lv_amount = s_output-amount.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_output-amount.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.

    CONCATENATE   s_output-account s_output-category s_output-datasrc
                  s_output-entity  s_output-intco s_output-rptcurrency
                  s_output-time s_output-customer s_output-dso
                  s_output-service s_output-paths s_output-projectst
                  lv_amount
                  INTO st_datarec SEPARATED BY c_delimtr.

    IF lv_amount <> 0.
      APPEND st_datarec TO t_data.
    ENDIF.
  ENDLOOP.

  IF lv_local = 0.

    PERFORM open_csvfile.

    LOOP AT t_data INTO st_datarec.
      TRANSFER st_datarec TO csvfile.
    ENDLOOP.

    PERFORM close_csvfile.
*    WRITE: 'File Output Successfully to: ', csvfile.
    WRITE: text-014, csvfile.

  ELSE.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_file
      TABLES
        data_tab                = t_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*    WRITE: 'File Output Successfully to: ', p_file.
    WRITE: text-014, p_file.
  ENDIF.

ENDFORM.                    "print_report

*----------------------------------------------------------------------*
FORM open_csvfile.
  OPEN DATASET csvfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
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
    MESSAGE e019 WITH text-015 csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "CLOSE_ALL_FILES

*&---------------------------------------------------------------------*
*Create Touch File.
*----------------------------------------------------------------------*
FORM create_touch_file.
  OPEN DATASET tuchfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH tuchfile msg.
    STOP.
  ENDIF.

*  TRANSFER 'This is a touch file' TO tuchfile.
  WRITE: text-016, tuchfile.

  CLOSE DATASET tuchfile.
  IF sy-subrc NE '0'.
*    MESSAGE e019 WITH 'unsuccessfl close' tuchfile msg.
    MESSAGE e019 WITH text-015 tuchfile msg.
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
