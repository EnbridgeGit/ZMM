*&---------------------------------------------------------------------*
*& Report  ZLMMI023_COST_CENTER
*&
*&---------------------------------------------------------------------*

REPORT  zlmmi023_cost_center.

************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI023_COST_CENTER                          *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 18, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts Cost Center PO data and      *
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
TABLES: csks. " Cost Center Master Data table

TYPES: BEGIN OF ty_csks,
       kostl TYPE csks-kostl,
       bukrs TYPE csks-bukrs,
       END OF ty_csks.

**Cost Center Texts
TYPES: BEGIN OF ty_cskt,
       kostl TYPE cskt-kostl,
       ltext TYPE cskt-ltext,
       END OF ty_cskt.

TYPES: BEGIN OF ty_final,
       kostl(50) TYPE c,
       bukrs(50) TYPE c,
       ltext(150) TYPE c,
       END OF ty_final.

TYPES: BEGIN OF  ty_file,
       line TYPE string,
       END OF ty_file.

TYPES:BEGIN OF ty_zprog, "Cutoum table data
      zclnt        TYPE mandt,
      zprog        TYPE zprog_name,      " Program Name
      zexec_date   TYPE zdate,           "Last Execution date
      zexec_time   TYPE ztime,           "Time of Execution
      END OF ty_zprog.


************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA: it_csks TYPE STANDARD TABLE OF ty_csks,
      wa_csks TYPE ty_csks,
      it_cskt TYPE STANDARD TABLE OF ty_cskt,
      wa_cskt TYPE ty_cskt,
      it_file  TYPE STANDARD TABLE OF ty_file,
      wa_file  TYPE ty_file,
      it_final  TYPE STANDARD TABLE OF ty_final,
      wa_final  TYPE ty_final,
      it_zprog TYPE STANDARD TABLE OF ty_zprog," Internal Table for structure ty_zprog
      wa_zprog TYPE ty_zprog.                  " Work Area for git_zprog

************************************************************************
*                          Custom Data Types                           *
************************************************************************
DATA: lv_ldate TYPE sy-datum,  " Last execution date
      lv_ltime TYPE sy-uzeit,  " Last Execution time
      lv_path  TYPE string,    " Selected download path
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
  PERFORM f_get_program_parms. "Get progam execution date details
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
  CLEAR wa_zprog.
  wa_zprog-zclnt = sy-mandt.
  wa_zprog-zprog = sy-cprog.
  wa_zprog-zexec_date = sy-datum.
  wa_zprog-zexec_time = sy-uzeit.
  MODIFY zexec_date FROM wa_zprog.
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
  CLEAR:lv_ldate, lv_ltime.
** Get Last Execution data from custom table zexec_date
  SELECT zclnt zprog zexec_date zexec_time FROM zexec_date INTO TABLE it_zprog
    WHERE zprog EQ sy-cprog.
  IF sy-subrc EQ 0.
    SORT it_zprog BY zexec_date zexec_time DESCENDING.
    READ TABLE it_zprog INTO wa_zprog INDEX 1.
    lv_ldate = wa_zprog-zexec_date.
    lv_ltime = wa_zprog-zexec_time.
    CLEAR wa_zprog.
  ENDIF.
ENDFORM.                    " F_GET_PROGRAM_PARMS

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       Get data from database tables
*----------------------------------------------------------------------*
FORM f_get_data .

**  Get data from table csks
  SELECT kostl bukrs FROM csks INTO TABLE it_csks
    WHERE datbi GE sy-datum  "Valid to date
     AND ( ersda GT lv_ldate AND ersda LE sy-datum ).  " Record Creation date

  IF it_csks[] IS NOT INITIAL.

    SELECT kostl ltext FROM cskt INTO TABLE it_cskt FOR ALL ENTRIES IN it_csks
      WHERE spras EQ 'EN' " Language
        AND kostl EQ it_csks-kostl
        AND datbi GE sy-datum. " Valid to date
    SORT it_cskt BY kostl.

**  File Header
    CONCATENATE '"CostCenterId"'
                '"CompanyCode"'
                '"CostCenterName"' INTO wa_file-line SEPARATED BY ','.

    INSERT wa_file INTO it_file INDEX 1. " Insert as first line

** File Data
    LOOP AT it_csks INTO wa_csks.
      wa_final-kostl = wa_csks-kostl.
      wa_final-bukrs = wa_csks-bukrs.

** Read CostCenter name
      READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_csks-kostl BINARY SEARCH.
      IF  sy-subrc EQ 0.
        wa_final-ltext = wa_cskt-ltext.
        CLEAR wa_cskt.
      ENDIF.

      APPEND wa_final TO it_final.

      IF wa_final-ltext NE space.
        CONCATENATE '"' wa_final-ltext '"' INTO wa_final-ltext.
      ENDIF.

      IF wa_final-kostl NE space.
        CONCATENATE '"' wa_final-kostl '"' INTO wa_final-kostl.
      ENDIF.

      IF wa_final-bukrs NE space.
        CONCATENATE '"' wa_final-bukrs '"' INTO wa_final-bukrs.
      ENDIF.

      CONCATENATE wa_final-kostl wa_final-bukrs wa_final-ltext INTO wa_file-line SEPARATED BY ','.
      APPEND wa_file TO it_file.
      CLEAR : wa_file, wa_final, wa_csks.
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
  CONCATENATE lv_sys lv_clnt '_UG_CostCenter_' lv_date '_' sy-uzeit '.CSV' INTO lv_fname.
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
