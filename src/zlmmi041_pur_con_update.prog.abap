*&---------------------------------------------------------------------*
*& Report  ZLMMI041_PUR_CON_UPDATE
*&
*&---------------------------------------------------------------------*
************************************************************************
*                            COGS                                      *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI041_PUR_CON_UPDATE                       *
*& Author             :  Jaydeep Waychal/Prashan Durbhaka              *
*& Creation Date      :  March 10, 2021                                *
*& Object ID          :  S01K90083                                     *
*& Application Area   :  MM                                            *
*& Description        :  Program to upload file and update existing    *
*                        Purchange Contract and provide the final      *
*                        report output with ALV display of the uploaded*
*                        Purchase contract records details.            *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*

REPORT  zlmmi041_pur_con_update NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: ekko,zmmt_mastagree.

* Types Declarations *
TYPES : BEGIN OF ty_upload,
           string TYPE string,
        END OF ty_upload,

        BEGIN OF ty_input_data,
            ebeln    TYPE ebeln,
            zzekgrp  TYPE ztrbuy,
            zzmsa    TYPE zzmsa,
        END OF ty_input_data,

        BEGIN OF ty_final,
            ebeln           TYPE ebeln,
            zzparty_agmt_id TYPE zzparty,
            zzekgrp         TYPE ztrbuy,
        END OF ty_final,

        BEGIN OF ty_msgout,
          ebeln    TYPE ebeln,
       zzparty_agmt_id TYPE zzparty,
          zzekgrp      TYPE ztrbuy,
          errmsg(2000) TYPE c,
          errtyp(1)    TYPE c,  "S - success , E-Error
        END OF ty_msgout.

*Validation
TYPES: BEGIN OF ty_agmid,
            zzparty_agmt_id TYPE zzparty,
            zzcpid          TYPE zcpid,
       END OF ty_agmid.

*Internal Table Declarations *.
DATA: lta_upload       TYPE STANDARD TABLE OF ty_upload,
      lta_input_data   TYPE STANDARD TABLE OF ty_input_data,
      lta_final        TYPE STANDARD TABLE OF ty_final,
      lta_msgout       TYPE STANDARD TABLE OF ty_msgout,
      lta_agmtid       TYPE STANDARD TABLE OF ty_agmid.

* Work Area Declarations *
DATA: lwa_upload       TYPE ty_upload,
      lwa_input_data   TYPE ty_input_data,
      lwa_final        TYPE ty_final,
      lwa_msgout       TYPE ty_msgout,
      lwa_agmtid       TYPE ty_agmid.

* Variable Declarations *
DATA: lv_sep(1)      TYPE c VALUE ',',
      lv_tstflg      TYPE c,
      lv_errtype     TYPE c,
      lv_cnt(5)      TYPE n,
      lv_path        TYPE btch0000-text80.


** Declaration for ALV Grid **
DATA : gr_table TYPE REF TO cl_salv_table.
** Declarations for ALV Functions
DATA : gr_functions TYPE REF TO cl_salv_functions_list.

** declaration for ALV Columns
DATA : gr_columns    TYPE REF TO cl_salv_columns_table,
       gr_column     TYPE REF TO cl_salv_column_table,
       lt_column_ref TYPE salv_t_column_ref,
       ls_column_ref TYPE salv_s_column_ref.

** declaration for Layout Settings
DATA : gr_layout     TYPE REF TO cl_salv_layout,
       gr_layout_key TYPE salv_s_layout_key.
** Declaration for Global Display Settings
DATA : gr_display TYPE REF TO cl_salv_display_settings,
       lv_title   TYPE lvc_title.

** Declaration for Aggregate Function Settings
DATA : gr_aggr    TYPE REF TO cl_salv_aggregations.

** Declaration for Sort Function Settings
DATA : gr_sort    TYPE REF TO cl_salv_sorts.

** Declaration for Table Selection settings
DATA : gr_select  TYPE REF TO cl_salv_selections.

** Declaration for Top of List settings
DATA : gr_content TYPE REF TO cl_salv_form_element.


* Constant Declarations *
CONSTANTS: lc_doccat   TYPE c VALUE 'K',
           lc_gstrnspt TYPE c VALUE 'T',
           lc_gsstorge TYPE c VALUE 'S',
           lc_status   TYPE c VALUE 'A',
           lc_telphone1(3) TYPE c VALUE 'G00',
           lc_telphone2(3) TYPE c VALUE 'G99'.


************************************************************************
* Selection screen *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETER: p_fname TYPE rlgrap-filename.
PARAMETER: p_dl AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* AT SELECTION-SCREEN processes
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

*-- Form to Select a File From a Particular Location
  PERFORM get_filename_f4 .

************************************************************************
* initialization *
************************************************************************


************************************************************************
* Start-of-selection *
************************************************************************
START-OF-SELECTION.

*** To upload flat file data into the internal table.
  PERFORM file_upload.
*** Split data from input file into an internal table
  PERFORM split_data.
*** Validate input data for test mode and upload
  PERFORM validate_data.
*  IF p_dl IS INITIAL.
  PERFORM upload_customdata.
*  ENDIF.
  IF lta_msgout[] IS NOT INITIAL.
    PERFORM alv_grid_display.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_filename_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_filename_f4 .

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
    CHANGING
      file_name     = p_fname
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " get_filename_f4

*&---------------------------------------------------------------------*
*&      Form  file_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM file_upload .

  DATA : lv_file TYPE string.

  lv_file = p_fname.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file
*     filetype                = 'ASC'
*     has_field_separator     = 'X'
    TABLES
      data_tab                = lta_upload
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " file_upload
*&---------------------------------------------------------------------*
*&      Form  split_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM split_data .
**Moving data into internal table to validate
  IF lta_upload IS NOT INITIAL.
    LOOP AT lta_upload INTO lwa_upload.
      SPLIT lwa_upload-string AT lv_sep INTO
                                lwa_input_data-ebeln
                                lwa_input_data-zzmsa
                                lwa_input_data-zzekgrp.
      APPEND lwa_input_data TO lta_input_data.
      CLEAR lwa_upload.
    ENDLOOP.
  ELSE.
    MESSAGE text-002 TYPE 'E'.
  ENDIF.
ENDFORM.                    " split_data

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_data.

  DATA:  lv_ebeln TYPE ebeln,
         lv_ekkolifnr TYPE lifnr,
         lv_lfm1lifnr TYPE lifnr,
         lv_ekorg  TYPE ekorg,
         lv_zzmsa  TYPE zzmsa,
         lv_ekgrp  TYPE ekgrp,
         lv_lines  TYPE n,
         lv_errflg TYPE c.

  IF lta_input_data IS NOT INITIAL.
    lv_errtype = 'E'.
***Validate input file data
    DELETE lta_input_data INDEX 1.
    LOOP AT lta_input_data INTO lwa_input_data.
***Scheduling Agreement
      SELECT SINGLE ebeln lifnr ekorg
        FROM ekko
        INTO (lv_ebeln, lv_ekkolifnr, lv_ekorg)
        WHERE ebeln = lwa_input_data-ebeln
          AND bstyp = lc_doccat.
      IF sy-subrc = 0.
        MOVE lv_ebeln TO lwa_final-ebeln.
      ELSE.
* Use Text element/ existing message class
        MOVE-CORRESPONDING lwa_input_data TO lwa_msgout.
        MOVE 'Invalid Purchase Contract.' TO lwa_msgout-errmsg.
        lwa_msgout-errtyp = lv_errtype.
        APPEND lwa_msgout TO lta_msgout.
        CONTINUE.
      ENDIF. "Scheduling Agreement

***Master Agreement
      IF lwa_msgout-errmsg IS INITIAL.
        REFRESH: lta_agmtid.
        IF lwa_input_data-zzmsa IS NOT INITIAL.
          SELECT zzparty_agmt_id zzcpid
            FROM zmmt_mastagree
            INTO TABLE lta_agmtid
          WHERE zzmsa = lwa_input_data-zzmsa
            AND (  zzmsatype = lc_gstrnspt
             OR zzmsatype = lc_gsstorge )
            AND zzstatus = lc_status.          "#EC CI_NOFIELD
          IF sy-subrc = 0.
            DESCRIBE TABLE lta_agmtid LINES lv_lines. "get no of rows
            IF lv_lines GT 1.
              MOVE-CORRESPONDING lwa_input_data TO lwa_msgout.
              lwa_msgout-errtyp = lv_errtype.
              lv_errflg = 'X'.
              MOVE 'Incorrect data, multiple Master Service agreement found.' TO lwa_msgout-errmsg.
              APPEND lwa_msgout TO lta_msgout.
            ELSE.
              READ TABLE lta_agmtid INTO lwa_agmtid INDEX 1.
              IF sy-subrc = 0.
                SELECT SINGLE lifnr
                      FROM lfm1
                      INTO lv_lfm1lifnr
                    WHERE eikto = lwa_agmtid-zzcpid
                      AND ekorg = lv_ekorg.                "#EC CI_NOFIRST
                IF sy-subrc = 0.
                  SHIFT lv_lfm1lifnr LEFT DELETING LEADING '0'.
                  SHIFT lv_ekkolifnr LEFT DELETING LEADING '0'.
                  IF lv_lfm1lifnr EQ lv_ekkolifnr.
                    MOVE lwa_agmtid-zzparty_agmt_id TO lwa_final-zzparty_agmt_id.
                  ELSE.
                    MOVE-CORRESPONDING lwa_input_data TO lwa_msgout.
                    lwa_msgout-errtyp = lv_errtype.
                    lv_errflg = 'X'.
                    CONCATENATE 'MSA Vendor (' lv_lfm1lifnr ') and Contract Vendor (' lv_ekkolifnr ') mismatch.' INTO lwa_msgout-errmsg .
                    APPEND lwa_msgout TO lta_msgout.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            MOVE-CORRESPONDING lwa_input_data TO lwa_msgout.
            lwa_msgout-errtyp = lv_errtype.
            lv_errflg = 'X'.
            MOVE 'Invalid Master Service Agreement.' TO lwa_msgout-errmsg.
            APPEND lwa_msgout TO lta_msgout.
          ENDIF.
        ENDIF.
****Trading Buyer:
        IF lwa_input_data-zzekgrp IS NOT INITIAL.
          SELECT SINGLE ekgrp
            FROM t024
            INTO lv_ekgrp
            WHERE ekgrp = lwa_input_data-zzekgrp
              AND ( ( ekgrp GE lc_telphone1 ) AND (  ekgrp LE lc_telphone2 ) ).
          IF sy-subrc = 0.
            MOVE lv_ekgrp TO lwa_final-zzekgrp.
          ELSE.
            MOVE-CORRESPONDING lwa_input_data TO lwa_msgout.
            lwa_msgout-errtyp = lv_errtype.
            lv_errflg = 'X'.
            MOVE 'Invalid Trading buyer.' TO lwa_msgout-errmsg.
            APPEND lwa_msgout TO lta_msgout.
          ENDIF.  " Trading buyer
        ENDIF.
        IF lv_errflg IS INITIAL.
          APPEND lwa_final TO lta_final.
          CLEAR: lwa_final,lwa_input_data.
        ENDIF.
      ENDIF.
      CLEAR : lwa_input_data, lwa_msgout, lwa_final, lv_errflg .
    ENDLOOP.
  ENDIF.
ENDFORM.                    " VALIDATE_DATA

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_CUSTOMDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_customdata .

*custom Fields Extension in Scheduling Agreement
  DATA: bapi_te_purcon  TYPE bapi_te_meoutheader,                 "Extension Tables
        bapi_te_purconx TYPE bapi_te_meoutheaderx,                "Extension Tables
        lta_return      TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
        lwa_return      TYPE bapiret2,
        lv_msgtxt(2000) TYPE c,
        ext_container   TYPE TABLE OF bapiparex WITH HEADER LINE. "Extension Container

  IF lta_final IS NOT INITIAL.
    LOOP AT lta_final INTO lwa_final.
      CLEAR: bapi_te_purcon,bapi_te_purconx.
***Prepare structure for extension
*Prapare the Extension table.
      bapi_te_purcon-number            = lwa_final-ebeln.
      bapi_te_purcon-zzparty_agmt_id   = lwa_final-zzparty_agmt_id.
      bapi_te_purcon-zzekgrp           = lwa_final-zzekgrp.

*Mark Fields for updation
      bapi_te_purconx-number           = lwa_final-ebeln.
      bapi_te_purconx-zzparty_agmt_id  = 'X'.
      bapi_te_purconx-zzekgrp          = 'X'.


*Prepare the Extension Container
      ext_container-structure  = 'BAPI_TE_MEOUTHEADER'.
      ext_container-valuepart1 = bapi_te_purcon.

      APPEND ext_container.

      ext_container-structure  = 'BAPI_TE_MEOUTHEADERX'.
      ext_container-valuepart1 = bapi_te_purconx.

      APPEND ext_container.

      IF p_dl IS NOT INITIAL.
        lv_tstflg  = 'X'.
      ENDIF.

      CALL FUNCTION 'BAPI_CONTRACT_CHANGE'
       EXPORTING
        purchasingdocument          = lwa_final-ebeln
*       HEADER                      =
*       HEADERX                     =
        testrun                     = lv_tstflg
*     IMPORTING
*       EXP_HEADER                  =
      TABLES
*       ITEM                        =
*       ITEMX                       =
        extensionin                 = ext_container[]
        return                      = lta_return.

      MOVE-CORRESPONDING lwa_final TO lwa_msgout.
      READ TABLE lta_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        LOOP AT lta_return INTO lwa_return WHERE type = 'E'.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = lwa_return-id
                msgnr               = lwa_return-number
                msgv1               = lwa_return-message_v1
                msgv2               = lwa_return-message_v2
                msgv3               = lwa_return-message_v3
                msgv4               = lwa_return-message_v4
              IMPORTING
                message_text_output = lv_msgtxt.
            IF sy-subrc EQ 0.
              lwa_msgout-errtyp = 'E'.
              MOVE lv_msgtxt TO lwa_msgout-errmsg.
              APPEND  lwa_msgout TO lta_msgout.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
*        IF p_dl IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        IF sy-subrc EQ 0.
          lwa_msgout-errtyp = 'S'.
          MOVE 'Custom fields data updated successfully in Purchase Contract.' TO lwa_msgout-errmsg.
          APPEND  lwa_msgout TO lta_msgout.
        ENDIF.
*        ENDIF.
      ENDIF.
      CLEAR: lwa_msgout,lwa_final.
      REFRESH: ext_container[].
    ENDLOOP.
  ENDIF.
  SORT lta_msgout BY errtyp.
ENDFORM.                    " UPLOAD_CUSTOMDATA

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid_display.


  IF lta_msgout[] IS NOT INITIAL.
    TRY.
        CALL METHOD cl_salv_table=>factory
*          EXPORTING
*            list_display = if_salv_c_bool_sap=>false
          IMPORTING
            r_salv_table = gr_table
          CHANGING
            t_table      = lta_msgout[].
      CATCH cx_salv_msg .
    ENDTRY.
  ENDIF.

  IF gr_table IS INITIAL.
    MESSAGE 'Error Creating ALV Grid ' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
** Get functions details
  gr_functions = gr_table->get_functions( ).

** Activate All Buttons in Tool Bar
  gr_functions->set_all( if_salv_c_bool_sap=>true ).

******* Layout Settings  *******
  CLEAR : gr_layout, gr_layout_key.
  MOVE sy-repid TO gr_layout_key-report.                        "Set Report ID as Layout Key"

  gr_layout = gr_table->get_layout( ).                          "Get Layout of Table"
  gr_layout->set_key( gr_layout_key ).                          "Set Report Id to Layout"
  gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ). "No Restriction to Save Layout"

******* Global Display Settings  *******
  CLEAR : gr_display.

  IF lta_final IS NOT INITIAL.
    CLEAR lv_title.
    MOVE 'Update Custom Fields for Purchase Contract' TO lv_title.
  ENDIF.

  gr_display = gr_table->get_display_settings( ).               " Global Display settings"
  gr_display->set_striped_pattern( if_salv_c_bool_sap=>true ).  "Activate Strip Pattern"
  gr_display->set_list_header( lv_title ).                      "Report Header"

******* Aggregate Function Settings *******
  gr_aggr = gr_table->get_aggregations( ).                      "Get Aggregate Functions"

******* Sort Functions *******
  gr_sort = gr_table->get_sorts( ).

******* Table Selection Settings *******
  gr_select = gr_table->get_selections( ).
  IF gr_select IS NOT INITIAL.
    gr_select->set_selection_mode( if_salv_c_selection_mode=>row_column ). "Allow single row Selection"
  ENDIF.

******* Top of List settings *******
  PERFORM top_of_page CHANGING gr_content.
  gr_table->set_top_of_list( gr_content ).

******* Event Register settings *******

** Get the columns from ALV Table
  gr_columns = gr_table->get_columns( ).
  IF gr_columns IS NOT INITIAL.
    REFRESH : lt_column_ref.
    CLEAR   : ls_column_ref.
    lt_column_ref = gr_columns->get( ).

** Get columns properties
    gr_columns->set_optimize( if_salv_c_bool_sap=>true ).
    gr_columns->set_key_fixation( if_salv_c_bool_sap=>true ).

** Individual Column Properties.
    IF lta_final IS NOT INITIAL.
      PERFORM column_settings.
    ENDIF.

  ENDIF.
  CALL METHOD gr_table->display.
ENDFORM.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GR_CONTENT  text
*----------------------------------------------------------------------*
FORM top_of_page  CHANGING lr_content TYPE REF TO cl_salv_form_element.
  DATA : lr_grid TYPE REF TO cl_salv_form_layout_grid,
           lr_text  TYPE REF TO cl_salv_form_text,
           lr_label TYPE REF TO cl_salv_form_label,
           lr_head  TYPE string.

  IF lta_msgout IS NOT INITIAL.
    MOVE 'Update Custom Fields Purchase Contract' TO lr_head.
    CREATE OBJECT lr_grid.
    DESCRIBE TABLE lta_msgout LINES lv_cnt.
  ENDIF.

** Header of Top of Page **
  lr_grid->create_header_information( row     = 1
                                      column  = 1
                                      text    = lr_head
                                      tooltip = lr_head ).
** Add Row **
  lr_grid->add_row( ).

*** Add Label in Grid **
*  lr_label = lr_grid->create_label( row = 2
*                                    column = 1
*                                    text = 'No of Records'
*                                    tooltip = 'No of Records' ).
*** Add Text in The Grid **
*  lr_text = lr_grid->create_text( row = 2
*                                  column = 2
*                                  text = lv_cnt
*                                  tooltip = lv_cnt ).
*** Set Label and Text Link **
*  lr_label->set_label_for( lr_text ).
*
*** Move lr_grid to lr_content **
*  lr_content = lr_grid.
ENDFORM.                    " TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  COLUMN_SETTINGS_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM column_settings.
  LOOP AT lt_column_ref INTO ls_column_ref.
    TRY.
        gr_column ?= gr_columns->get_column( ls_column_ref-columnname ).
      CATCH cx_salv_not_found.
    ENDTRY.

    IF gr_column IS NOT INITIAL.
      IF gr_column->get_columnname( ) = 'EBELN'.
        gr_column->set_short_text( 'PUR_CON').
        gr_column->set_medium_text( 'PURCHASE CONTRACT').
        gr_column->set_long_text( 'PURCHASE CONTRACT').
      ENDIF.

      IF gr_column->get_columnname( ) = 'ZZPARTY_AGMT_ID'.
        gr_column->set_short_text( 'PRAGMT_ID').
        gr_column->set_medium_text( 'MASTER SER AGREEMENT').
        gr_column->set_long_text( 'MASTER SERVICE AGREEMENT').
      ENDIF.

      IF gr_column->get_columnname( ) = 'ZZEKGRP'.
        gr_column->set_short_text( 'TRD_BUYR').
        gr_column->set_medium_text( 'TRADING BUYER').
        gr_column->set_long_text( 'TRADING BUYER').
      ENDIF.

      IF gr_column->get_columnname( ) = 'ERRMSG'.
        gr_column->set_short_text( 'MESSAGE').
        gr_column->set_medium_text( 'MESSAGE').
        gr_column->set_long_text( 'MESSAGE').
      ENDIF.

      IF gr_column->get_columnname( ) = 'ERRTYP'.
        gr_column->set_short_text( 'MSG_TYPE').
        gr_column->set_medium_text( 'MESSAGE TYPE').
        gr_column->set_long_text( 'MESSAGE TYPE').
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " COLUMN_SETTINGS_PO
