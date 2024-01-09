*&---------------------------------------------------------------------*
*& Report  ZLMMI032_UNIT_OF_MEASURE
*&
*&---------------------------------------------------------------------*

REPORT  zlmmi032_unit_of_measure.
************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI032_UNIT_OF_MEASURE                      *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 17, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts Unit Of Measure data and     *
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


TABLES: t006. " Unit Of Measures Master table

TYPES: BEGIN OF ty_t006,
       msehi TYPE t006-msehi,
       isocode TYPE t006-isocode,
       END OF ty_t006.

TYPES: BEGIN OF ty_final,
       msehi(15) TYPE c,
       isocode(15) TYPE c,
       pref(15)  TYPE c,
       END OF ty_final.

TYPES: BEGIN OF  ty_file,
       line TYPE string,
       END OF ty_file.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA: it_t006 TYPE STANDARD TABLE OF ty_t006,
      wa_t006 TYPE ty_t006,
      it_file  TYPE STANDARD TABLE OF ty_file,
      wa_file  TYPE ty_file,
      it_final  TYPE STANDARD TABLE OF ty_final,
      wa_final  TYPE ty_final.

DATA: gv_flag.

CONSTANTS: lc_pref(4) TYPE c VALUE 'TRUE', " Preffered key on file
           gc_ppath   TYPE localfile VALUE 'H:\my documents\',  " All .CSV files to Presentations server
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

**  Get data from table t006
  SELECT msehi isocode FROM t006 INTO TABLE it_t006 WHERE msehi NE space.

  IF it_t006[] IS NOT INITIAL.
**  File Header
    CONCATENATE '"Key"'
                '"Value"'
                '"Preferred"' INTO wa_file-line SEPARATED BY ','.

    INSERT wa_file INTO it_file INDEX 1. " Insert as first line

** File Data
    LOOP AT it_t006 INTO wa_t006.
      wa_final-msehi = wa_t006-msehi.
      wa_final-isocode = wa_t006-isocode.
      wa_final-pref  = lc_pref.
      APPEND wa_final TO it_final.

      IF wa_final-pref NE space.
        CONCATENATE '"' wa_final-pref '"' INTO wa_final-pref.
      ENDIF.

      IF wa_final-msehi NE space.
        CONCATENATE '"' wa_final-msehi '"' INTO wa_final-msehi.
      ENDIF.

      IF wa_final-isocode NE space.
        CONCATENATE '"' wa_final-isocode '"' INTO wa_final-isocode.
      ENDIF.

      CONCATENATE wa_final-msehi wa_final-isocode wa_final-pref INTO wa_file-line SEPARATED BY ','.
      APPEND wa_file TO it_file.
      CLEAR : wa_file, wa_final, wa_t006.
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
  CONCATENATE lv_sys lv_clnt '_UG_UOMMap_' lv_date '_' sy-uzeit '.CSV' INTO lv_fname.

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
