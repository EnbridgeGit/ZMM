*&---------------------------------------------------------------------*
*& Report  ZLMMI020_GL_ACCOUNT
*&---------------------------------------------------------------------*
************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI020_GL_ACCOUNT                         *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 13, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts GL Account Masterdata        *
*&                       and places the Outbound CSV files on          *
*&                       SE FTP server. These files are used in        *
*&                       Spend Viz data upload                         *
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
REPORT  zlmmi020_gl_account.

INCLUDE zlmmi020_gl_account_top.

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
  PERFORM f_get_program_parms. "Get progam execution date details
  PERFORM f_prepare_header.     " Prepare header for Account.csv file
  PERFORM f_get_data.           " Get data from Master tables
  IF git_ska1[] IS NOT INITIAL.
    PERFORM f_prepare_file.       " Prepare final internal table
    PERFORM f_send_file USING gc_acnt. " send file to location
  ELSE.
    MESSAGE 'NO Data Selected' TYPE 'E'.
  ENDIF.

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
*&      Form  F_GET_PROGRAM_PARMS
*&---------------------------------------------------------------------*
*      Get last execution date and finalise date for selection
*----------------------------------------------------------------------*
FORM f_get_program_parms .
  CLEAR:gv_ldate, gv_ltime.
** Get Last Execution data from custom table zexec_date
  SELECT zclnt zprog zexec_date zexec_time FROM zexec_date INTO TABLE git_zprog
    WHERE zprog EQ sy-cprog.
  IF sy-subrc EQ 0.
    SORT git_zprog BY zexec_date zexec_time DESCENDING.
    READ TABLE git_zprog INTO gwa_zprog INDEX 1.
    gv_ldate = gwa_zprog-zexec_date.
    gv_ltime = gwa_zprog-zexec_time.
    CLEAR gwa_zprog.
  ENDIF.
ENDFORM.                    " F_GET_PROGRAM_PARMS
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_HEADER
*&---------------------------------------------------------------------*
*       Prepare header for Account.csv file
*----------------------------------------------------------------------*
FORM f_prepare_header .
**Header Preparation
  CONCATENATE '"AccountId"'
              '"CompanyCode"'
              '"AccountName"'
              '"MajorAccountId"'
              '"MajorAccountName"'
              '"ChartOfAccountsId"'
              '"ChartOfAccountsName"'
              INTO gwa_file-line SEPARATED BY ','.
  INSERT gwa_file INTO git_file INDEX 1.  " Insert as first line
  CLEAR gwa_file.
ENDFORM.                    " F_PREPARE_HEADER


*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       Get data from Master tables
*----------------------------------------------------------------------*
FORM f_get_data .
**  Read data from database tables

**  Ska1 table data
  SELECT ktopl
         saknr
    INTO CORRESPONDING FIELDS OF TABLE git_ska1 FROM ska1
    WHERE ktopl EQ gc_chacc  " COAT for UG system
      AND ( erdat GT gv_ldate AND erdat LE sy-datum )  " Record Creation date
      AND xloev EQ space.     " Deletion flag

  IF git_ska1[] IS NOT INITIAL.
**    Read data from skat table for GL a/c description
    SELECT ktopl saknr txt50 FROM skat INTO TABLE git_skat FOR ALL ENTRIES IN git_ska1
      WHERE ktopl EQ git_ska1-ktopl
        AND saknr = git_ska1-saknr
        AND spras EQ 'EN'.

**      Read data from t004t for Chart of a/c description
    SELECT ktopl ktplt FROM t004t INTO TABLE git_t004t FOR ALL ENTRIES IN git_ska1
      WHERE ktopl EQ git_ska1-ktopl
        AND spras EQ 'EN'.

  ENDIF.
ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_FILE
*&---------------------------------------------------------------------*
*       Prepare final internal table
*----------------------------------------------------------------------*
FORM f_prepare_file .

  LOOP AT git_ska1 INTO gwa_ska1.
    gwa_final-ktopl = gwa_ska1-ktopl.
    gwa_final-saknr = gwa_ska1-saknr.

**Harcode Business unit in place of companycode
    gwa_final-bukrs = gc_bunit.

**  Read GL A/C description
    READ TABLE git_skat INTO gwa_skat WITH KEY ktopl = gwa_ska1-ktopl
                                               saknr = gwa_ska1-saknr.
    IF sy-subrc EQ 0.
      gwa_final-txt50 = gwa_skat-txt50.
      CLEAR gwa_skat.
    ENDIF.
**    Read Chart of A/C desription
    READ TABLE git_t004t INTO gwa_t004t WITH KEY ktopl = gwa_ska1-ktopl.
    IF sy-subrc EQ 0.
      gwa_final-ktplt = gwa_t004t-ktplt.
      CLEAR gwa_t004t.
    ENDIF.
    APPEND gwa_final TO git_final.
    CLEAR gwa_final.

  ENDLOOP.

  LOOP AT git_final INTO gwa_final.

    CONDENSE: gwa_final-ktopl,
              gwa_final-saknr,
              gwa_final-bukrs,
              gwa_final-txt50,
              gwa_final-ktplt.

    IF gwa_final-ktopl NE space.
      CONCATENATE '"' gwa_final-ktopl '"' INTO gwa_final-ktopl.
    ENDIF.

    IF gwa_final-saknr NE space.
      CONCATENATE '"' gwa_final-saknr '"' INTO gwa_final-saknr.
    ENDIF.

    IF gwa_final-bukrs NE space.
      CONCATENATE '"' gwa_final-bukrs '"' INTO gwa_final-bukrs.
    ENDIF.

    IF gwa_final-txt50 NE space.
      CONCATENATE '"' gwa_final-txt50 '"' INTO gwa_final-txt50.
    ENDIF.

    IF gwa_final-ktplt NE space.
      CONCATENATE '"' gwa_final-ktplt '"' INTO gwa_final-ktplt.
    ENDIF.

    CONCATENATE gwa_final-saknr
                gwa_final-bukrs
                gwa_final-txt50
                gwa_final-ktopl
                gwa_final-ktplt
                gwa_final-ktopl
                gwa_final-ktplt
                INTO gwa_file-line SEPARATED BY ','.
    APPEND gwa_file TO git_file.
    CLEAR: gwa_final, gwa_file.

  ENDLOOP.

ENDFORM.                    " F_PREPARE_FILE

*&---------------------------------------------------------------------*
*&      Form  F_SEND_FILE
*&---------------------------------------------------------------------*
*       Send output CSV file
*----------------------------------------------------------------------*
FORM f_send_file USING p_name TYPE string.
  DATA: lv_fname  TYPE string,  "File name
       lv_ffname TYPE string,  "Full File name
       lv_msg         TYPE c,  "Error messages
       lv_date(10)    TYPE c,  " Preferred date formate
       lv_clnt        TYPE sy-mandt, " System client
       lv_sys         TYPE sy-sysid, " Systerm Name
       lv_name        TYPE string.   " Dynamic file name

  lv_name  = p_name. "file name
  lv_sys   = sy-sysid. " Logon System Name
  lv_clnt  = sy-mandt. " Lognon Client Name

** Date formate dd-mm-yyyy
  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum+0(4) INTO lv_date SEPARATED BY '-'.
**prepare file name
  CONCATENATE lv_sys lv_clnt lv_name lv_date '_' sy-uzeit '.CSV' INTO lv_fname.

  IF rb_appl IS NOT INITIAL
      AND p_fpath IS NOT INITIAL. " File download to Application server
** Combine path and file name
    CONCATENATE p_fpath lv_fname INTO lv_ffname.

**  Open Dataset to write data
    OPEN DATASET lv_ffname FOR OUTPUT IN TEXT MODE MESSAGE lv_msg ENCODING DEFAULT.
    IF sy-subrc EQ 0.
      LOOP AT git_file INTO gwa_file.
        TRANSFER gwa_file-line TO lv_ffname.
        CLEAR gwa_file.
      ENDLOOP.
      REFRESH git_file.
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
        data_tab                = git_file
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
    REFRESH git_file.
  ENDIF.
ENDFORM.                    " F_SEND_FILE
