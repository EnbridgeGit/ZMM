REPORT  zlmmi006_service_master_load MESSAGE-ID zs.

************************************************************************
*  Client:    Spectra Energy.                                          *
*  Author:    Brian Boundy                                             *
*  Date:      January, 2011.                                           *
*  Track #:   TR804.                                                   *
*                                                                      *
*  Description:                                                        *
*     - The purpose of this program is to create a BDC session for     *
*       posting skf  from the EXCEL sheet using                        *
*       bapi_acc_stat_key_fig_post                                     *
*                                                                      *
************************************************************************
* ---------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
*--------  ---- ------- ---------------------------------------------- *
* 2012/09/07 M Khan   TR995 Change C: drive to H: drive with directory,*
*                           file selection using F4.                   *
*                                                                      *
************************************************************************


*Formated Input record
TYPES:

  BEGIN OF ty_line,
    service     LIKE bapisrv_asmd-service,
    short_text  LIKE bapisrv_asmdt-short_text,
    matl_group  LIKE bapisrv_asmd-matl_group,
    base_uom    LIKE bapisrv_asmd-base_uom,
    serv_cat    LIKE bapisrv_asmd-serv_cat,
    chgtext     LIKE bapisrv_asmd-chgtext,
    division    LIKE bapisrv_asmd-division,
    val_class   LIKE bapisrv_asmd-val_class,
    tax_ind     LIKE bapisrv_asmd-tax_ind,
    del_ind     LIKE bapisrv_asmd-del_ind,
  END OF ty_line.


DATA:  BEGIN OF exceltab OCCURS 0.
        INCLUDE STRUCTURE kcde_cells.
DATA:  END OF exceltab.

* Working Data
DATA: s_line        TYPE ty_line,
      t_line        LIKE TABLE OF s_line,
      bapi_asmd     TYPE bapisrv_asmd,
      bapi_asmdx    TYPE bapisrv_asmdx,
      bapi_numcheck TYPE bapiflag-bapiflag,
      bapi_asmdt    TYPE bapisrv_asmdt,
      bapi_asmdt_t  LIKE TABLE OF bapi_asmdt,

      bapi_rc       LIKE bapiret2,
      bapi_return   LIKE TABLE OF bapi_rc.

*======================================================================*
*              SELECTION SCREEN                                        *
*======================================================================*
*
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME.
PARAMETERS:  p_filein  LIKE rlgrap-filename OBLIGATORY DEFAULT
*                       'c:\saptemp\Service Master Load.xlsx'. "TR995
                        'H:\saptemp\Service Master Load.xlsx'. "TR995

SELECTION-SCREEN END OF BLOCK box.

******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILEIN.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      P_FILEIN = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILEIN.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON P_FILEIN.                           "TR995
  PERFORM CHECK_FILE_PATH.                               "TR995
*End of TR995 changes

*======================================================================*
*              Main Processing Block                                   *
*======================================================================*


PERFORM upload_exce_to_internal_tab.
IF t_line IS INITIAL.
  STOP.
ELSE.
  PERFORM create_bapi.
ENDIF.




*======================================================================*
*              Upload EXCEL Data                                       *
*======================================================================*
FORM upload_exce_to_internal_tab.

  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      filename                = p_filein
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 32
      i_end_row               = 999
    TABLES
      intern                  = exceltab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_FOR_INTERACTION'
      EXPORTING
        headline = '!! ERROR !!'
        text1    = 'Unsuccessful EXCEL Upload '
        text2    = 'Please check the file path/name and try again'
        text3    = ' '
        text4    = 'Press OK Button to Continue'
        button_1 = 'OK'.
    STOP.
  ENDIF.

  CLEAR: s_line, t_line.

  LOOP AT exceltab.

    IF exceltab-row < 2.
      CONTINUE.
    ENDIF.

    CASE exceltab-col.
      WHEN 1.  MOVE exceltab-value TO s_line-service.
      WHEN 2.  MOVE exceltab-value TO s_line-short_text.
      WHEN 3.  MOVE exceltab-value TO s_line-serv_cat.
      WHEN 4.  MOVE exceltab-value TO s_line-base_uom.
      WHEN 5.  MOVE exceltab-value TO s_line-chgtext.
      WHEN 6.  MOVE exceltab-value TO s_line-matl_group.
      WHEN 7.  MOVE exceltab-value TO s_line-division.
      WHEN 8.  MOVE exceltab-value TO s_line-val_class.
      WHEN 9.  MOVE exceltab-value TO s_line-tax_ind.
      WHEN 10. MOVE exceltab-value TO s_line-del_ind.
      WHEN OTHERS.
    ENDCASE.
    AT END OF row.
      APPEND s_line TO t_line.
      CLEAR  s_line.
    ENDAT.
  ENDLOOP.
ENDFORM.                    "UPLOAD_EXCE_TO_INTERNAL_TAB

*======================================================================*
*              Create Bapi                                             *
*======================================================================*
FORM create_bapi.

  DATA: lc_flg(1) TYPE c.

  LOOP AT t_line INTO s_line.
    CLEAR: bapi_asmd, bapi_asmdx, bapi_numcheck, bapi_asmdt, bapi_asmdt_t, bapi_return.


    "Set the Values
    bapi_asmd-service     = s_line-service.
    bapi_asmd-matl_group  = s_line-matl_group.
    bapi_asmd-base_uom    = s_line-base_uom.
    bapi_asmd-serv_cat    = s_line-serv_cat.
    bapi_asmd-division    = s_line-division.
    bapi_asmd-val_class   = s_line-val_class.
    "Set the Change Indicators
    bapi_asmdx-service    = 'X'.
    bapi_asmdx-matl_group = 'X'.
    bapi_asmdx-base_uom   = 'X'.
    bapi_asmdx-serv_cat   = 'X'.
    bapi_asmdx-division   = 'X'.
    bapi_asmdx-val_class  = 'X'.



    "Set the indicators: C Clear, X Set, nothing, no change.
    IF s_line-chgtext     = 'C'.
      bapi_asmd-chgtext   = ''.
      bapi_asmdx-chgtext  = 'X'.

    ELSEIF s_line-chgtext = 'X'.
      bapi_asmd-chgtext   = 'X'.
      bapi_asmdx-chgtext  = 'X'.
    ENDIF.

    IF s_line-tax_ind     = 'C'.
      bapi_asmd-tax_ind   = ''.
      bapi_asmdx-tax_ind  = 'X'.

    ELSEIF s_line-tax_ind = 'X'.
      bapi_asmd-tax_ind   = 'X'.
      bapi_asmdx-tax_ind  = 'X'.
    ENDIF.

    IF s_line-del_ind     = 'C'.
      bapi_asmd-del_ind   = ''.
      bapi_asmdx-del_ind  = 'X'.

    ELSEIF s_line-del_ind = 'X'.
      bapi_asmd-del_ind   = 'X'.
      bapi_asmdx-del_ind  = 'X'.
    ENDIF.


    bapi_numcheck = 'X'.

    bapi_asmdt-language   = 'EN'.
    bapi_asmdt-short_text = s_line-short_text.

    APPEND bapi_asmdt TO bapi_asmdt_t.

    "Check if the Service exists.
    CALL FUNCTION 'LEISTUNG_READ'
      EXPORTING
        r_asnum         = s_line-service
      EXCEPTIONS
        asmd_not_found  = 1
        asmdt_not_found = 2
        auth_no_begru   = 3
        OTHERS          = 4.

    IF sy-subrc <> 0.
      "Does Not Exists, So Create
      CALL FUNCTION 'BAPI_SERVICE_CREATE'
        EXPORTING
          im_service_data       = bapi_asmd
          im_service_datax      = bapi_asmdx
          no_number_range_check = bapi_numcheck
        TABLES
          return                = bapi_return
          service_description   = bapi_asmdt_t.

    ELSE.
      "Exists So Change

      "Remove updates to Service and Serv_cat since they are keys
      CLEAR: bapi_asmdx-service, bapi_asmdx-serv_cat.

      IF bapi_asmd-matl_group  = ''.
        CLEAR bapi_asmdx-matl_group.
      ENDIF.
      IF bapi_asmd-base_uom   = ''.
        CLEAR bapi_asmdx-base_uom .
      ENDIF.
      IF bapi_asmd-serv_cat  = ''.
        CLEAR bapi_asmdx-serv_cat.
      ENDIF.
      IF bapi_asmd-division   = ''.
        CLEAR bapi_asmdx-division.
      ENDIF.
      IF bapi_asmd-val_class  = ''.
        CLEAR bapi_asmdx-val_class.
      ENDIF.


      CALL FUNCTION 'BAPI_SERVICE_CHANGE'
        EXPORTING
          servicenumber       = s_line-service
          im_service_data     = bapi_asmd
          im_service_datax    = bapi_asmdx
        TABLES
          return              = bapi_return
          service_description = bapi_asmdt_t.

    ENDIF.



    lc_flg = ''.
    LOOP AT bapi_return INTO bapi_rc.
      WRITE: bapi_rc-message.
      IF bapi_rc-type = 'A' OR bapi_rc-type = 'E'.
        lc_flg = 'X'.
      ENDIF.
    ENDLOOP.


    IF lc_flg = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.



  ENDLOOP.




ENDFORM.                    "create_bapi

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path & file    TR995
*&---------------------------------------------------------------------*
FORM CHECK_FILE_PATH.
DATA: pathnfile type string,
      LV_BOL TYPE C.        "abap_bool.

move p_filein to pathnfile.
IF pathnfile CS 'C:' OR pathnfile CS 'c:'.
   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
ELSE.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
      EXPORTING
        FILE                 = pathnfile
      RECEIVING
        RESULT               = lv_bol
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        WRONG_PARAMETER      = 3
        NOT_SUPPORTED_BY_GUI = 4
        OTHERS               = 5
            .
*    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

IF lv_bol IS INITIAL.
   CONCATENATE TEXT-100 pathnfile into pathnfile.
   MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH pathnfile.
ENDIF.
ENDIF.
ENDFORM.
