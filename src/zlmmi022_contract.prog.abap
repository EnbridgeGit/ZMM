*&---------------------------------------------------------------------*
*& Report  ZLMMI022_CONTRACT
*&
*&---------------------------------------------------------------------*

REPORT  zlmmi022_contract.
************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI022_CONTRACT                             *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 23, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts Contract header data and     *
*&                       Outbound CSV file send to SE FTP server.      *
*&                       This file is being used in ARIBA Spend Viz    *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No : D30K925438                                           *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*

TABLES: ekko. " PO Header Master table

TYPES: BEGIN OF ty_ekko,
       ebeln TYPE ekko-ebeln,
       konnr TYPE ekko-konnr,
       verkf TYPE ekko-verkf,
       END OF ty_ekko.

TYPES: BEGIN OF ty_final,
       konnr(30) TYPE c,
       verkf(60) TYPE c,
       END OF ty_final.

TYPES: BEGIN OF  ty_file,
       line TYPE string,
       END OF ty_file.

TYPES: BEGIN OF ty_zprog, "Cutoum table data
       zclnt        TYPE mandt,
       zprog        TYPE zprog_name,      " Program Name
       zexec_date   TYPE zdate,           "Last Execution date
       zexec_time   TYPE ztime,           "Time of Execution
       END OF ty_zprog.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA: it_ekko  TYPE STANDARD TABLE OF ty_ekko,
      wa_ekko  TYPE ty_ekko,
      it_file  TYPE STANDARD TABLE OF ty_file,
      wa_file  TYPE ty_file,
      it_final TYPE STANDARD TABLE OF ty_final,
      wa_final TYPE ty_final,
      git_zprog TYPE STANDARD TABLE OF ty_zprog,"  Internal Table for structure ty_zprog
      gwa_zprog TYPE ty_zprog.                  " Work Area for git_zprog

DATA: lv_path  TYPE string,  " Folder path
      lv_ldate TYPE sy-datum,  " Last execution date
      lv_ltime TYPE sy-uzeit,  " Last execution time
      gv_flag.

CONSTANTS: gc_ppath   TYPE localfile VALUE 'H:\my documents\',  " All .CSV files to Presentations server
           gc_fpath   TYPE localfile VALUE '/usr/sap/interfaces/D30/ARIBA/'.
*           gc_fpath   TYPE localfile VALUE '\\fifileserver.gtna.gt.ds\data\FI\DEV\Out\I_PTP_MM_020\'.


************************************************************************
*                           Selection Screen                           *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: rb_appl RADIOBUTTON GROUP grp1
                    USER-COMMAND cmd
                    DEFAULT 'X'.
PARAMETERS: rb_pres RADIOBUTTON GROUP grp1.

PARAMETERS: p_fpath  TYPE rlgrap-filename
                     DEFAULT gc_fpath MODIF ID csv.       " Application server path
PARAMETERS: p_ppath  TYPE rlgrap-filename
                     DEFAULT gc_ppath MODIF ID pre.       " Presentaion server path

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  IF NOT rb_pres IS INITIAL.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'CSV'.
          screen-input = '0'.
          screen-output = '0'.
          screen-invisible = '1'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF NOT rb_appl IS INITIAL.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PRE'.
          screen-input = '0'.
          screen-output = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.

**  .CSV file name validations
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath.
  PERFORM f_directory_selection CHANGING p_fpath.

** Presentation server path
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ppath.
  PERFORM f_directory_selection CHANGING p_ppath.

AT SELECTION-SCREEN.
  IF rb_appl IS NOT INITIAL.
    IF p_fpath IS INITIAL.
      CLEAR p_fpath.
      MOVE gc_fpath TO p_fpath.
    ENDIF.
  ENDIF.
  IF rb_pres IS NOT INITIAL.
    IF p_ppath IS INITIAL.
      CLEAR p_ppath.
      MOVE gc_ppath TO p_ppath.
    ENDIF.
  ENDIF.


************************************************************************
*                        START-OF-SELECTION                            *
************************************************************************
START-OF-SELECTION.
  PERFORM f_get_data.          " Get data from database & prepare final output
  PERFORM f_send_file .        " Send file to path

  IF gv_flag EQ 'X'.
    MESSAGE 'File Downloaded Successfuly..' TYPE 'S'.
  ELSE.
    MESSAGE 'Error While Downloading' TYPE 'E'.
  ENDIF.

************************************************************************
*                        END-OF-SELECTION                              *
************************************************************************
END-OF-SELECTION.

**Inserting execution date&time
  CLEAR gwa_zprog.
  gwa_zprog-zclnt = sy-mandt.
  gwa_zprog-zprog = sy-cprog.
  gwa_zprog-zexec_date = sy-datum.
  gwa_zprog-zexec_time = sy-uzeit.
  MODIFY zexec_date FROM gwa_zprog.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_GET_PROGRAM_PARMS
*&---------------------------------------------------------------------*
*      Get last execution date and finalise date for selection
*----------------------------------------------------------------------*
FORM f_get_program_parms .
  CLEAR:lv_ldate, lv_ltime.
** Get Last Execution data from custom table zexec_date
  SELECT zclnt zprog zexec_date zexec_time FROM zexec_date INTO TABLE git_zprog
    WHERE zprog EQ sy-cprog.
  IF sy-subrc EQ 0.
    SORT git_zprog BY zexec_date zexec_time DESCENDING.
    READ TABLE git_zprog INTO gwa_zprog INDEX 1.
    lv_ldate = gwa_zprog-zexec_date.
    lv_ltime = gwa_zprog-zexec_time.
    CLEAR gwa_zprog.
  ENDIF.
ENDFORM.                    " F_GET_PROGRAM_PARMS

*&---------------------------------------------------------------------*
*&      Form  F_DIRECTORY_SELECTION
*&---------------------------------------------------------------------*
*      Select Directory for download
*----------------------------------------------------------------------*

FORM f_directory_selection  CHANGING p_p_path TYPE localfile.
DATA lv_path TYPE string.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder = lv_path
    EXCEPTIONS
      OTHERS          = 0.
  IF sy-subrc EQ 0.
    CONCATENATE lv_path '\' INTO lv_path.
    p_p_path = lv_path.
    WRITE: p_p_path.
  ENDIF.
ENDFORM.                    " F_DIRECTORY_SELECTION

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       Get data from database tables
*----------------------------------------------------------------------*
FORM f_get_data .

**  Get data from table tcurc
  SELECT ebeln konnr verkf FROM ekko INTO TABLE it_ekko
     WHERE konnr NE space
*       AND ( aedat GE lv_ldate AND aedat LE sy-datum ) " PO Created On
*       AND kdate GE sy-datum  " Agriment Valid date
       AND loekz EQ space.    " Deletion indicator

  IF it_ekko[] IS NOT INITIAL.
    SORT it_ekko BY konnr DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_ekko COMPARING konnr.
**  File Header
    CONCATENATE '"ContractId"'
                '"ContractName"'
                INTO wa_file-line SEPARATED BY ','.

    INSERT wa_file INTO it_file INDEX 1. " Insert as first line
    CLEAR wa_file.

** File Data
    LOOP AT it_ekko INTO wa_ekko.
      wa_final-konnr = wa_ekko-konnr.
      wa_final-verkf = wa_ekko-verkf.

      APPEND wa_final TO it_final.

      CONDENSE: wa_final-konnr,
                wa_final-verkf.

      IF wa_final-konnr NE space.
        CONCATENATE '"' wa_final-konnr '"' INTO wa_final-konnr.
      ENDIF.

      IF wa_final-verkf NE space.
        CONCATENATE '"' wa_final-verkf '"' INTO wa_final-verkf.
      ENDIF.

      CONCATENATE wa_final-konnr wa_final-verkf INTO wa_file-line SEPARATED BY ','.
      APPEND wa_file TO it_file.
      CLEAR : wa_file, wa_final, wa_ekko.
    ENDLOOP.

  ELSE.
    MESSAGE 'No Data Selected' TYPE 'E'.
  ENDIF.

ENDFORM.                    " F_GET_DATA


*&---------------------------------------------------------------------*
*&      Form  F_SEND_FILE
*&---------------------------------------------------------------------*
*       Send output CSV file
*----------------------------------------------------------------------*
FORM f_send_file .
  DATA: lv_fname  TYPE string,  "File name
        lv_ffname TYPE string,  "Full File name
        lv_msg         TYPE c,  "Error messages
        lv_date(10)    TYPE c,  " Preferred date formate
        lv_clnt        TYPE sy-mandt, " System client
        lv_sys         TYPE sy-sysid. " Systerm Name

  lv_sys   = sy-sysid. " Logon System Name
  lv_clnt  = sy-mandt. " Lognon Client Name

** Date formate dd-mm-yyyy
  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum+0(4) INTO lv_date SEPARATED BY '-'.
**prepare file name
  CONCATENATE lv_sys lv_clnt '_UG_Contract_' lv_date '_' sy-uzeit '.CSV' INTO lv_fname.
  IF rb_appl IS NOT INITIAL
        AND p_fpath IS NOT INITIAL. " File download to Application server
** Combine path and file name
    CONCATENATE p_fpath lv_fname INTO lv_ffname.

**  Open Dataset to write data
    OPEN DATASET lv_ffname FOR OUTPUT IN TEXT MODE MESSAGE lv_msg ENCODING DEFAULT.
    IF sy-subrc EQ 0.
      LOOP AT it_file INTO wa_file.
        TRANSFER wa_file-line TO lv_ffname.
        CLEAR wa_file.
      ENDLOOP.
      REFRESH it_file.
      gv_flag = 'X'.  " File download message flag
    ELSE.
**  Error handling here
      MESSAGE 'File open error' TYPE 'E'.
    ENDIF.
**  Close the Dataset
    CLOSE DATASET lv_ffname.
  ENDIF.

  IF rb_pres IS NOT INITIAL
    AND p_ppath IS NOT INITIAL. " File download to Presentation server
    CLEAR gv_flag.
** Combine path and file name
    CONCATENATE p_ppath lv_fname INTO lv_ffname.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_ffname
        filetype                = 'ASC'
      TABLES
        data_tab                = it_file
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

    IF sy-subrc EQ 0.
      gv_flag = 'X'.
    ENDIF.
    REFRESH it_file.
  ENDIF.
ENDFORM.                    " F_SEND_FILE
