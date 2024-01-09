*&---------------------------------------------------------------------*
*& Report  ZLMMI027_USER
*&
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI027_USER                                 *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 23, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts SAP ECC USER's masterdata and*
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
REPORT  zlmmi027_user.

TABLES: usr01,  " User Master Record
        usr21,  " User key
        adrp.

TYPES: BEGIN OF ty_usr21,
       bname      LIKE usr01-bname,       " User ID
       persnumber LIKE usr21-persnumber,  " Person Number
      END OF ty_usr21.

TYPES: BEGIN OF ty_adrp,
       persnumber LIKE usr21-persnumber,  " Person Number
       name_first LIKE adrp-name_first,   " First Name
       name_last  LIKE adrp-name_last,    " Last Name
      END OF ty_adrp.

TYPES: BEGIN OF ty_usr01,
       bname  LIKE usr01-bname,       " User ID
       class  LIKE usr02-class,       " User group
      END OF ty_usr01.

TYPES: BEGIN OF ty_final,
       usrid(20)  TYPE c,       " User ID
       uname(100) TYPE c,       " User Name(First, Last)
       supid(30)  TYPE c,       " Supervisor ID
       END OF ty_final.

TYPES: BEGIN OF ty_file,
       line TYPE string, " Record in a file
       END OF ty_file.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA: it_usr21 TYPE STANDARD TABLE OF ty_usr21,
      wa_usr21 TYPE ty_usr21,
      it_usr01 TYPE STANDARD TABLE OF ty_usr01,
      wa_usr01 TYPE ty_usr01,
      it_adrp TYPE STANDARD TABLE OF ty_adrp,
      wa_adrp TYPE ty_adrp,
      it_file  TYPE STANDARD TABLE OF ty_file,
      wa_file  TYPE ty_file,
      it_final  TYPE STANDARD TABLE OF ty_final,
      wa_final  TYPE ty_final.

DATA: gv_flag TYPE c.

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
    MESSAGE 'File Downloaded Succefully' TYPE 'S'.
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

**Read data from USR01 and USR02 tables
  SELECT a~bname
         b~class
    INTO TABLE it_usr01 FROM usr01 AS a
    INNER JOIN usr02 AS b ON b~bname EQ a~bname
    WHERE a~bname NE space.

  IF it_usr01[] IS NOT INITIAL.
    SELECT bname
           persnumber
     FROM usr21 INTO TABLE it_usr21 FOR ALL ENTRIES IN it_usr01
      WHERE bname EQ it_usr01-bname.

    IF it_usr21[] IS NOT INITIAL.
      SELECT persnumber
             name_first
             name_last
        FROM adrp INTO TABLE it_adrp FOR ALL ENTRIES IN it_usr21
        WHERE  persnumber EQ it_usr21-persnumber.
    ENDIF.
  ELSE.
    MESSAGE 'No Data Selected' TYPE 'E'.
  ENDIF.

  LOOP AT it_usr01 INTO wa_usr01.

    wa_final-usrid = wa_usr01-bname.
*    wa_final-supid = wa_usr01-class.
    wa_final-supid = ' '.

**Read First & Last Names
    READ TABLE it_usr21 INTO wa_usr21 WITH KEY bname = wa_usr01-bname.
    IF sy-subrc EQ 0.
      READ TABLE it_adrp INTO wa_adrp WITH KEY  persnumber = wa_usr21-persnumber.
      IF sy-subrc EQ 0.
        CONCATENATE wa_adrp-name_first wa_adrp-name_last INTO wa_final-uname SEPARATED BY space.
        CLEAR wa_adrp.
      ENDIF.
      CLEAR wa_usr21.
    ENDIF.
    APPEND wa_final TO it_final.
    CLEAR: wa_final , wa_usr01.
  ENDLOOP.

**  Prepare File Header
  CONCATENATE '"UserId"'
              '"UserName"'
              '"SupervisorId"'
              INTO wa_file-line SEPARATED BY ','.
  INSERT wa_file INTO it_file INDEX 1.  " Insert as first line.

**  Prepare finale output format
  LOOP AT it_final INTO wa_final.

**  Delete unwanted spaces
    CONDENSE: wa_final-usrid,
              wa_final-uname,
              wa_final-supid.

    IF wa_final-usrid NE space.
      CONCATENATE '"' wa_final-usrid '"' INTO wa_final-usrid.
    ENDIF.

    IF wa_final-uname NE space.
      CONCATENATE '"' wa_final-uname '"' INTO wa_final-uname.
    ENDIF.

    IF wa_final-supid NE space.
      CONCATENATE '"' wa_final-supid '"' INTO wa_final-supid.
    ENDIF.
**Create a Record in file
    CONCATENATE wa_final-usrid
                wa_final-uname
                wa_final-supid
                INTO wa_file-line SEPARATED BY ','.
    APPEND wa_file TO it_file.
    CLEAR: wa_final, wa_file.
  ENDLOOP.
ENDFORM.                    " F_GET_DATA


*&---------------------------------------------------------------------*
*&      Form  F_SEND_FILE
*&---------------------------------------------------------------------*
*       Send output CSV file
*----------------------------------------------------------------------*
FORM f_send_file .
  DATA: lv_fname   TYPE string,  "File name
        lv_ffname  TYPE string,  "Full File name
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
  CONCATENATE lv_sys lv_clnt '_US_User_' lv_date '_' sy-uzeit '.CSV' INTO lv_fname.

  IF rb_appl IS NOT INITIAL
      AND p_fpath IS NOT INITIAL. " File download to Application server
    CLEAR gv_flag.
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
