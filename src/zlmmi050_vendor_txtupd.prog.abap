***********************************************************************
* Program Name: ZLMMI050_VENDOR_TXTUPD                               *
* Date        : September 2021                                       *
* Author      : Rajendra Nagiri(nagirir).                            *
* Program Description:This program Update Vendor Long Text when file *
*                     uploaded from user's desktop/presentation sever*
*                     User Can also download vendor Long data in csv *
*                     format to user's desktop/presentation server   *
**********************************************************************
*CHANGES:                                                            *
*Issue      By      Date       Description                           *
**********************************************************************

REPORT  zlmmi050_vendor_txtupd.
TYPE-POOLS: slis, truxs.
TABLES: lfm1.
CONSTANTS: c_0002 TYPE thead-tdid VALUE '0002',
           c_lfm1 TYPE thead-tdobject VALUE 'LFM1',
           c_tabsep TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
           c_64   TYPE i VALUE 64,
           c_x    TYPE c VALUE 'X',
           c_s    TYPE c VALUE 'S',
           c_e    TYPE c VALUE 'E'.
**********************************************************************
*VARIABLE DECLARATION
**********************************************************************
TYPES: BEGIN OF ty_vendor,
  lifnr TYPE lifnr,
  ekorg TYPE ekorg,
  ltext TYPE char30000,
  END OF ty_vendor.

TYPES: BEGIN OF ty_alvout,
  lifnr TYPE lifnr,
  ekorg TYPE ekorg,
  ltext TYPE char30000,
  messg TYPE char50,
  END OF ty_alvout.

TYPES: BEGIN OF ty_lfm1,
  lifnr TYPE elifn,
  ekorg TYPE ekorg,
  END OF ty_lfm1.

TYPES: BEGIN OF ty_out,
  lifnr TYPE lifnr,
  ekorg TYPE ekorg,
  ltext TYPE char30000,
  END OF ty_out.

DATA: t_vendor TYPE STANDARD TABLE OF ty_vendor  ##NEEDED,
      w_vendor TYPE ty_vendor                    ##NEEDED,
      t_alvout TYPE STANDARD TABLE OF ty_alvout  ##NEEDED,
      w_alvout TYPE ty_alvout                    ##NEEDED,
      t_exceltab(5000) OCCURS  0 WITH HEADER LINE ##NEEDED,
      w_head  TYPE thead                          ##NEEDED,
      t_lines TYPE STANDARD TABLE OF tline        ##NEEDED,
      w_lines TYPE tline                          ##NEEDED,
      gv_rc   TYPE i                              ##NEEDED,
      gt_file_table  TYPE filetable               ##NEEDED,
      gwa_file_table TYPE file_table              ##NEEDED,
      t_fcat TYPE slis_t_fieldcat_alv             ##NEEDED,
      w_fcat TYPE slis_fieldcat_alv               ##NEEDED,
      gd_repid LIKE sy-repid                      ##NEEDED.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_upload RADIOBUTTON GROUP rad USER-COMMAND frad1 DEFAULT 'X' MODIF ID m1.
PARAMETERS: p_infile TYPE ibipparms-path.

PARAMETERS: p_downld RADIOBUTTON GROUP rad MODIF ID m1.
SELECT-OPTIONS: s_lifnr FOR lfm1-lifnr,
                s_ekorg FOR lfm1-ekorg.
PARAMETERS: p_otfile TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  p_infile = 'H:/INPUT/Filename.dat'(001).
  p_otfile = 'H:/OUTPUT/Filename.dat'(002).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_infile.
  PERFORM f4help_inputfile CHANGING p_infile.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_otfile.
  PERFORM f4help_inputfile CHANGING p_otfile.
**********************************************************************
*START OF SELECTION
**********************************************************************
START-OF-SELECTION.

  IF p_upload IS NOT INITIAL.
    PERFORM data_read.
    PERFORM data_validate.
    PERFORM data_update.
    PERFORM data_display.
  ELSEIF p_downld IS NOT INITIAL.
    PERFORM data_download.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  DATA_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_read .

  DATA: lv_filename TYPE string.

  lv_filename = p_infile.
  CLEAR: t_exceltab.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
      filetype                = 'ASC'
*     has_field_separator     = 'X'
    TABLES
      data_tab                = t_exceltab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
*Convert file data to internal table
  IF t_exceltab[] IS NOT INITIAL.
    LOOP AT t_exceltab.
      SPLIT t_exceltab AT c_tabsep INTO
                                w_vendor-lifnr
                                w_vendor-ekorg
                                w_vendor-ltext.
      APPEND w_vendor TO t_vendor.
    ENDLOOP.
  ELSE.
    MESSAGE 'Empty File Or Error Reading File'(011) TYPE c_e.
  ENDIF.
  DELETE t_vendor INDEX 1. " Remove Header
*  SORT t_vendor BY lifnr ekorg. " Sort if same vendor at multiple places

ENDFORM.                    " DATA_READ
*&---------------------------------------------------------------------*
*&      Form  DATA_VALIDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_validate .

  TYPES: BEGIN OF ty_lifnr,
    lifnr TYPE lifnr,
    END OF ty_lifnr.

  TYPES: BEGIN OF ty_t024e,
   ekorg TYPE ekorg,
   END OF ty_t024e.
  DATA: t_lifnr TYPE STANDARD TABLE OF ty_lifnr,
        t_t024e TYPE STANDARD TABLE OF ty_t024e,
        t_lfm1 TYPE STANDARD TABLE OF ty_lfm1.

  SELECT lifnr ekorg FROM lfm1                          "#EC CI_NOWHERE
    INTO TABLE t_lfm1.
  SELECT lifnr FROM lfa1                                "#EC CI_NOWHERE
    INTO TABLE t_lifnr.
  SELECT ekorg FROM t024e
    INTO TABLE t_t024e.
  REFRESH: t_alvout.
  LOOP AT t_vendor INTO w_vendor.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_vendor-lifnr
      IMPORTING
        output = w_vendor-lifnr.
    CLEAR: w_alvout.
    w_alvout-lifnr = w_vendor-lifnr.
    w_alvout-ekorg = w_vendor-ekorg.
    w_alvout-ltext = w_vendor-ltext.
    READ TABLE t_lifnr TRANSPORTING NO FIELDS WITH KEY lifnr = w_vendor-lifnr.
    IF sy-subrc <> 0.
      w_alvout-messg = 'Vendor Does Not Exist'(006).
    ENDIF.
    READ TABLE t_t024e TRANSPORTING NO FIELDS WITH KEY ekorg = w_vendor-ekorg.
    IF sy-subrc <> 0.
      w_alvout-messg = 'Invalid Purchase Org'(008).
    ENDIF.
    READ TABLE t_lfm1 TRANSPORTING NO FIELDS WITH KEY lifnr = w_vendor-lifnr
                                                      ekorg = w_vendor-ekorg.
    IF sy-subrc <> 0.
      w_alvout-messg = 'Invalid Vendor & Purchase Org Combination'(021).
    ENDIF.
    CLEAR: w_vendor.
    APPEND w_alvout TO t_alvout.
  ENDLOOP.
ENDFORM.                    " DATA_VALIDATE
*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_update.
  DATA: lw_dummy TYPE ty_vendor.
  LOOP AT t_alvout INTO w_alvout  WHERE messg IS INITIAL.
    lw_dummy = w_alvout.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_alvout-lifnr
      IMPORTING
        output = w_alvout-lifnr.

    CONCATENATE w_alvout-lifnr w_alvout-ekorg INTO w_head-tdname.
    w_head-tdspras  = sy-langu.
    w_head-tdid     = c_0002.
    w_head-tdobject = c_lfm1.

    CLEAR: w_lines.
    MOVE '*' TO w_lines-tdformat.
    MOVE w_alvout-ltext TO w_lines-tdline.
    APPEND w_lines TO t_lines.

    AT END OF lifnr ##LOOP_AT_OK.
      CALL FUNCTION 'SAVE_TEXT' ##FM_SUBRC_OK
        EXPORTING
          client          = sy-mandt
          header          = w_head
          savemode_direct = c_x
        TABLES
          lines           = t_lines.
      IF sy-subrc <> 0 ##FM_SUBRC_OK.
        w_alvout-messg = 'Error updating Vendor'(009).
        MODIFY t_alvout FROM w_alvout.
      ELSE.
        w_alvout-lifnr = lw_dummy-lifnr.
        w_alvout-ekorg = lw_dummy-ekorg.
        w_alvout-ltext = lw_dummy-ltext.
        w_alvout-messg = 'Vendor Text Updated Successfully'(010).
        MODIFY t_alvout FROM w_alvout.
      ENDIF.
      CLEAR: w_alvout, w_head, w_lines, t_lines, w_alvout.
      REFRESH: t_lines.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  F4HELP_INPUTFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4help_inputfile CHANGING file.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title = 'Select A File' ##NO_TEXT
    CHANGING
      file_table   = gt_file_table
      rc           = gv_rc.
  IF sy-subrc = 0.
    READ TABLE gt_file_table INTO gwa_file_table INDEX 1.
    file = gwa_file_table-filename.
  ENDIF.

ENDFORM.                    " F4HELP_INPUTFILE
*&---------------------------------------------------------------------*
*&      Form  DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_display .
  CLEAR: w_fcat.
  REFRESH: t_fcat.
  w_fcat-fieldname   = 'LIFNR'.
  w_fcat-seltext_m   = 'Vendor'(012).
  w_fcat-col_pos     = 1.
  APPEND w_fcat TO t_fcat.

  CLEAR  w_fcat.
  w_fcat-fieldname   = 'EKORG'.
  w_fcat-seltext_m   = 'Purchase Org.'(014).
  w_fcat-col_pos     = 2.
  APPEND w_fcat TO t_fcat.

  CLEAR  w_fcat.
  w_fcat-fieldname   = 'LTEXT'.
  w_fcat-seltext_m   = 'Text'(015).
  w_fcat-col_pos     = 3.
  w_fcat-outputlen   = 40.
  APPEND w_fcat TO t_fcat.

  CLEAR w_fcat.
  w_fcat-fieldname   = 'MESSG'.
  w_fcat-seltext_m   = 'Status/Message'(016).
  w_fcat-col_pos     = 4.
  w_fcat-outputlen   = 50.
  APPEND w_fcat TO t_fcat.

  gd_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gd_repid
      it_fieldcat        = t_fcat[]
      i_save             = c_x
    TABLES
      t_outtab           = t_alvout "vendor
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2 ##FM_SUBRC_OK.

ENDFORM.                    " DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  DATA_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_download .
  DATA: lt_lfm1     TYPE STANDARD TABLE OF ty_lfm1,
        lw_lfm1     TYPE ty_lfm1,
        lt_out      TYPE STANDARD TABLE OF ty_out,
        lw_out      TYPE ty_out,
        lv_name     TYPE thead-tdname,
        lv_filename TYPE string.

* EXCEL table header

  CLEAR: w_vendor, lw_lfm1, w_lines, lw_out.
  REFRESH: t_vendor, lt_lfm1, t_lines, lt_out.

  SELECT lifnr ekorg
    FROM lfm1
    INTO TABLE lt_lfm1 ##needed
    WHERE lifnr IN s_lifnr
    AND   ekorg IN s_ekorg.

  LOOP AT lt_lfm1 INTO lw_lfm1.
    CLEAR: lv_name, lw_out.
    AT FIRST.
      CLEAR: lw_out.
      lw_out-lifnr = 'Vendor'(012).
      lw_out-ekorg = 'POrg'(022).
      lw_out-ltext = 'Long Text'(023).
*      lw_out-ermsg = 'Error/Status Message'(024).
      APPEND lw_out TO lt_out.
    ENDAT.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lw_lfm1-lifnr
      IMPORTING
        output = lw_lfm1-lifnr.

    CONCATENATE lw_lfm1-lifnr lw_lfm1-ekorg INTO lv_name.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = c_0002
        language                = sy-langu
        name                    = lv_name
        object                  = c_lfm1
      TABLES
        lines                   = t_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8 ##FM_SUBRC_OK.
    IF t_lines IS INITIAL.
      CLEAR: lw_out.
      lw_out-lifnr = lw_lfm1-lifnr.
      lw_out-ekorg = lw_lfm1-ekorg.
      lw_out-ltext = ''.
      APPEND lw_out TO lt_out.
    ELSE.
      CLEAR: lw_out.
      LOOP AT  t_lines INTO w_lines.
        IF w_lines-tdformat IS NOT INITIAL.
          APPEND lw_out TO lt_out.
          CLEAR: lw_out.
          lw_out-lifnr = lw_lfm1-lifnr.
          lw_out-ekorg = lw_lfm1-ekorg.
          CONCATENATE lw_out-ltext w_lines-tdline INTO lw_out-ltext.
        ELSE.
          CONCATENATE lw_out-ltext w_lines-tdline INTO lw_out-ltext.
        ENDIF.
        AT LAST.
          lw_out-lifnr = lw_lfm1-lifnr.
          lw_out-ekorg = lw_lfm1-ekorg.
          APPEND lw_out TO lt_out.
          CLEAR: lw_out.
        ENDAT.
      ENDLOOP.
      DELETE lt_out WHERE ltext IS INITIAL.
    ENDIF.
    CLEAR: w_lines, t_lines, lw_out, lw_lfm1.
    REFRESH: t_lines.
  ENDLOOP.

  LOOP AT lt_out INTO lw_out.
    CLEAR: t_exceltab.
    CONCATENATE lw_out-lifnr
                lw_out-ekorg
                lw_out-ltext
                INTO t_exceltab SEPARATED BY c_tabsep.

    APPEND t_exceltab.
  ENDLOOP.
  lv_filename = p_otfile.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
*     filetype                = 'ASC'
*     write_field_separator   = lv_check
    TABLES
      data_tab                = t_exceltab
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
* Implement suitable error handling here
  ELSE .
    CONCATENATE 'File Downloaded At:'(025) lv_filename INTO lv_filename SEPARATED BY space.
    MESSAGE lv_filename TYPE c_s.
  ENDIF.
  CLEAR: t_exceltab.
ENDFORM.                    " DATA_DOWNLOAD
