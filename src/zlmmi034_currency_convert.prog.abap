*&---------------------------------------------------------------------*
*& Report  ZLMMI034_CURRENCY_CONVERT
*&
*&---------------------------------------------------------------------*
************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :   ZLMMI034_CURRENCY_CONVERT                    *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  March 11, 2015                                *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts Currency Key data and        *
*&                       Outbound CSV file send to SE FTP server.      *
*&                       This file is being used in ARIBA Spend Viz    *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No : D30K925483                                           *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*
REPORT  zlmmi034_currency_convert.

TABLES: tcurr. " Currency Rates Master table

TYPES: BEGIN OF ty_tcurr,
       fcurr TYPE tcurr-fcurr, "From currency
       tcurr TYPE tcurr-tcurr, "To-currency
       gdatu TYPE tcurr-gdatu, "Valid from
       ukurs TYPE tcurr-ukurs, "Exchange Rate
       END OF ty_tcurr.

TYPES: BEGIN OF ty_final,
       uname(15) TYPE c,  " Unique Name
       fcurr(15) TYPE c,
       tcurr(15) TYPE c,
       gdatu(15) TYPE c,
       ukurs(30) TYPE c,
       END OF ty_final.

TYPES: BEGIN OF  ty_file,
       line TYPE string,
       END OF ty_file.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA: it_tcurr TYPE STANDARD TABLE OF ty_tcurr,
      wa_tcurr TYPE ty_tcurr,
      it_file  TYPE STANDARD TABLE OF ty_file,
      wa_file  TYPE ty_file,
      it_final  TYPE STANDARD TABLE OF ty_final,
      wa_final  TYPE ty_final.

DATA: gv_flag.  " Folder path

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
  DATA: lv_date(10),
        lv_dd(2),
        lv_mm(2),
        lv_yyyy(4).
**  Get data from table tcurc
  SELECT fcurr tcurr gdatu ukurs FROM tcurr INTO TABLE it_tcurr WHERE kurst EQ 'M'
                                                                  AND fcurr NE space
                                                                  AND tcurr NE space.

  IF it_tcurr[] IS NOT INITIAL.
**  File Header
    CONCATENATE '"UniqueName"'
                '"FromCurrency"'
                '"ToCurrency"'
                '"Rate"'
                '"Date"' INTO wa_file-line SEPARATED BY ','.

    INSERT wa_file INTO it_file INDEX 1. " Insert as first line

** File Data
    LOOP AT it_tcurr INTO wa_tcurr.

**  Unique name on file
      CONCATENATE wa_tcurr-fcurr ':' wa_tcurr-tcurr INTO wa_final-uname.
      wa_final-fcurr = wa_tcurr-fcurr.
      wa_final-tcurr = wa_tcurr-tcurr.

**Display sign in front of value
      IF wa_tcurr-ukurs IS NOT INITIAL.
        IF wa_tcurr-ukurs LT 0.
          wa_tcurr-ukurs =  wa_tcurr-ukurs * -1.
          wa_tcurr-ukurs = 1 / wa_tcurr-ukurs.
        ENDIF.
        wa_final-ukurs = wa_tcurr-ukurs.
*        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*          CHANGING
*            value = wa_final-ukurs.
      ENDIF.


**  Date conversion
      IF wa_tcurr-gdatu IS NOT INITIAL.
        CLEAR: lv_date,lv_yyyy, lv_mm, lv_dd.
        CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
          EXPORTING
            input  = wa_tcurr-gdatu
          IMPORTING
            output = lv_date.
        SPLIT lv_date AT '/' INTO: lv_mm lv_dd lv_yyyy.
        CONCATENATE lv_yyyy
                    lv_mm
                    lv_dd INTO wa_final-gdatu SEPARATED BY '-'.
      ELSE.
        CONTINUE.
      ENDIF.

      APPEND wa_final TO it_final.

      CONDENSE:wa_final-uname,
               wa_final-fcurr,
               wa_final-tcurr,
               wa_final-gdatu,
               wa_final-ukurs.

      IF wa_final-uname NE space.
        CONCATENATE '"' wa_final-uname '"' INTO wa_final-uname.
      ENDIF.

      IF wa_final-fcurr NE space.
        CONCATENATE '"' wa_final-fcurr '"' INTO wa_final-fcurr.
      ENDIF.

      IF wa_final-tcurr NE space.
        CONCATENATE '"' wa_final-tcurr '"' INTO wa_final-tcurr.
      ENDIF.

      IF wa_final-gdatu NE space.
        CONCATENATE '"' wa_final-gdatu '"' INTO wa_final-gdatu.
      ENDIF.

      IF wa_final-ukurs NE space.
        CONCATENATE '"' wa_final-ukurs '"' INTO wa_final-ukurs.
      ENDIF.

      CONCATENATE wa_final-uname
                  wa_final-fcurr
                  wa_final-tcurr
                  wa_final-ukurs
                  wa_final-gdatu
       INTO wa_file-line SEPARATED BY ','.
      APPEND wa_file TO it_file.
      CLEAR : wa_file, wa_final, wa_tcurr.
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
  CONCATENATE lv_sys lv_clnt '_US_CurrencyConversion_' lv_date '_' sy-uzeit '.CSV' INTO lv_fname.
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
