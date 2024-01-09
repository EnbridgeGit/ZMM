*&---------------------------------------------------------------------*
*& Report  ZLMMI025_COMMODITY
*&
*&---------------------------------------------------------------------*

REPORT  zlmmi025_commodity.
************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZZLMMI025_COMMODITY                           *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 11, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts Material Group data and      *
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

TABLES: t023.  "MaterialGroup Master data

TYPES:BEGIN OF ty_t023,
      matkl TYPE t023-matkl, " Material Group
     END OF ty_t023.

TYPES: BEGIN OF ty_t023t,
       matkl TYPE mara-matkl,  " Material Group
       wgbez TYPE t023t-wgbez, " Matl Grp Desc.
       spras TYPE t023t-spras, " Language Key
       END OF ty_t023t.

TYPES: BEGIN OF ty_final,
        matkl(35) TYPE c,  "CommodityId
        wgbez(50) TYPE c,  "CommodityName
        matyp(20) TYPE c,  "CommodityType
      END OF ty_final.

TYPES: BEGIN OF ty_file,
      line TYPE string,   " Record in a file.
      END OF ty_file.

TYPES: BEGIN OF ty_zprog, "Cutoum table data
       zprog        TYPE zprog_name,      " Program Name
       zexec_date   TYPE zdate,           "Last Execution date
       zexec_time   TYPE ztime,           "Time of Execution
       END OF ty_zprog.


************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:git_final TYPE STANDARD TABLE OF ty_final, "Internal Table for structure ty_final
     gwa_final TYPE ty_final,                 " Work Area for git_final
     git_file TYPE STANDARD TABLE OF ty_file, "  Internal Table for structure ty_file
     gwa_file TYPE ty_file,                   " Work Area for git_file
     git_t023 TYPE STANDARD TABLE OF ty_t023, " Internal table for structure ty_t023
     gwa_t023 TYPE ty_t023,                   " Work Area for git_t023
     git_t023t TYPE STANDARD TABLE OF ty_t023t,  " Internal table for structure ty_t023t
     gwa_t023t TYPE ty_t023t,                 " Work Area for git_t023t
     git_prog TYPE STANDARD TABLE OF ty_zprog,"  Internal Table for structure ty_zprog
     gwa_prog TYPE ty_zprog.                  " Work Area for git_zprog

************************************************************************
*                          Custom Data Types                           *
************************************************************************
DATA: gv_ldate TYPE sy-datum,  " Last execution date
      gv_ltime TYPE sy-uzeit,  " Last Execution time
      gv_path  TYPE string,    " Selected download path
      gv_flag.

CONSTANTS: lc_bunit(2) TYPE c VALUE 'UG',
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
*  PERFORM f_get_program_parms. "Get progam execution date details
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
*&      Form  F_GET_PROGRAM_PARMS
*&---------------------------------------------------------------------*
*      Get last execution date and finalise date for selection
*----------------------------------------------------------------------*
FORM f_get_program_parms .
  CLEAR:gv_ldate, gv_ltime.
** Get Last Execution data from custom table zexec_date
  SELECT zprog zexec_date zexec_time FROM zexec_date INTO TABLE git_prog
    WHERE zprog EQ sy-cprog.
  IF sy-subrc NE 0.
    SORT git_prog BY zexec_date zexec_time DESCENDING.
    READ TABLE git_prog INTO gwa_prog INDEX 1.
    gv_ldate = gwa_prog-zexec_date.
    gv_ltime = gwa_prog-zexec_time.
    CLEAR gwa_prog.
  ENDIF.

ENDFORM.                    " F_GET_PROGRAM_PARMS
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*      Fetch data from database
*----------------------------------------------------------------------*
FORM f_get_data .
  REFRESH: git_t023, git_final, git_file, git_t023t.
  SELECT matkl FROM t023 INTO TABLE git_t023
    WHERE matkl NE space.
  IF git_t023[] IS NOT INITIAL.
    SELECT matkl wgbez spras FROM t023t INTO TABLE git_t023t FOR ALL ENTRIES IN git_t023
      WHERE matkl = git_t023-matkl.
  ELSE.
    MESSAGE 'No data selected' TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CLEAR gwa_t023.

  LOOP AT git_t023 INTO gwa_t023.
***Prefix material group with business unit to make unique in Ariba
*    IF gwa_t023-matkl IS NOT INITIAL.
*      CONCATENATE lc_bunit gwa_t023-matkl INTO gwa_final-matkl SEPARATED BY '-'.
*    ENDIF.
    gwa_final-matkl = gwa_t023-matkl.
    READ TABLE git_t023t INTO gwa_t023t WITH KEY matkl = gwa_t023-matkl
                                                 spras = 'EN'.
    IF sy-subrc EQ 0.
      gwa_final-wgbez = gwa_t023t-wgbez.
    ENDIF.

    APPEND gwa_final TO git_final.
    CLEAR: gwa_final, gwa_t023t.
  ENDLOOP.

**  Prepare file header
  CONCATENATE '"CommodityId"'
              '"CommodityName"'
              '"CommodityType"'
              INTO gwa_file-line SEPARATED BY ','.
  INSERT gwa_file INTO git_file INDEX 1.
  CLEAR gwa_file.

**  Prepare final data

  LOOP AT git_final INTO gwa_final.
    CONDENSE: gwa_final-matkl,
              gwa_final-wgbez.

**Material group
    IF gwa_final-matkl NE space.
      CONCATENATE '"' gwa_final-matkl '"' INTO gwa_final-matkl.
    ELSE.
      CONTINUE.
    ENDIF.
** Material group description
    IF gwa_final-wgbez NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-wgbez WITH '""'.
      CONCATENATE '"' gwa_final-wgbez '"' INTO gwa_final-wgbez.
    ENDIF.

    CONCATENATE gwa_final-matkl
                gwa_final-wgbez
                gwa_final-matyp
                INTO gwa_file-line SEPARATED BY ','.
    APPEND gwa_file TO git_file.
    CLEAR: gwa_final, gwa_file.
  ENDLOOP.

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
  CONCATENATE lv_sys lv_clnt '_UG_ERPCommodity_' lv_date '_' sy-uzeit '.CSV' INTO lv_fname.
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
