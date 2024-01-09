*&---------------------------------------------------------------------*
*& Report  ZLMMI044_SPLY_DEAL_SEND
*&
*&---------------------------------------------------------------------*
************************************************************************
*                            COGS                                      *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI044_SPLY_DEAL_SEND                       *
*& Author             :  Jaydeep Waychal/Prashan Durbhaka              *
*& Creation Date      :  April 2, 2021                                 *
*& Object ID          :  S01K900837                                    *
*& Application Area   :  MM                                            *
*& Description        :  Program to send Supply Deals information to   *
*&                       Openlink by triggering IDOC output type.      *
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
REPORT  zlmmi044_sply_and_contrct_send NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: ekko,nast.

TYPES: BEGIN OF tt_output,
        kappl TYPE sna_kappl,
        objky TYPE na_objkey,
        kschl TYPE sna_kschl,
        parnr TYPE na_parnr,
        erdat TYPE na_erdat,
        usnam TYPE usnam,
        vstat TYPE na_vstat,
        message(30) TYPE c.
TYPES: END OF tt_output.
*Create an internal table and work area of type nast.
DATA: it_nast TYPE STANDARD TABLE OF nast,
      wa_nast TYPE nast,
      lt_output TYPE STANDARD TABLE OF tt_output,
      wa_output TYPE tt_output,
      lv_cnt(5)      TYPE n.

* Process the output type using the fm 'WFMC_MESSAGES_PROCESS'

DATA: lt_msg_nast TYPE TABLE OF msg0,
lt_msg_nast1 TYPE TABLE OF msg0,
wa_msg0 LIKE msg0,

t_disp  TYPE TABLE OF naliv2.


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

************************************************************************
* Selection screen *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_ebeln FOR ekko-ebeln NO INTERVALS.
PARAMETER: p_kappl TYPE nast-kappl DEFAULT 'EV'.
PARAMETER: p_kschl TYPE nast-kschl DEFAULT 'INEU'.
PARAMETER: p_parnr TYPE nast-parnr DEFAULT 'SONIC'.
PARAMETER: p_parvw TYPE nast-parvw DEFAULT 'LS'.
PARAMETER: p_nacha TYPE nast-nacha DEFAULT '6'.

PARAMETER: p_dl AS CHECKBOX USER-COMMAND check.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: p_splyd   RADIOBUTTON GROUP rad1 MODIF ID ebl DEFAULT 'X',
            p_cntrt   RADIOBUTTON GROUP rad1 MODIF ID ebl.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screenoutput.


************************************************************************
* initialization *
************************************************************************

************************************************************************
* Start-of-selection *
************************************************************************
START-OF-SELECTION.
**Validate Data
  PERFORM validate_data.
  IF lt_output[] IS NOT INITIAL.
**If test mode is selected
    IF p_dl IS NOT INITIAL.
      PERFORM alv_grid_display.
    ELSE.
*** Build final internal table to trigger output.
      PERFORM build_data.
      IF it_nast[] IS NOT INITIAL.
*      PERFORM validate_data.
        PERFORM trigger_output.
        IF lt_msg_nast IS NOT INITIAL.
          PERFORM alv_grid_display.
        ENDIF.
      ELSE.
        MESSAGE text-001 TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SCREENOUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screenoutput .
**Select option visible on checkbox
  IF p_dl  = ''.
    LOOP AT SCREEN.
      IF  screen-group1 = 'EBL'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF p_dl = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'EBL' .
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SCREENOUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_data .

**Assign values to the work area.
  LOOP AT lt_output into wa_output where message = 'VALID'.
*  LOOP AT s_ebeln.
    wa_nast-mandt  = sy-mandt.
    wa_nast-kappl  = p_kappl.              "Application area
    wa_nast-objky  = wa_output-objky.          "object key. Po, shipment etc
    wa_nast-kschl  = p_kschl.              "output type to be processed
    wa_nast-spras  = 'EN'.                 "language
    wa_nast-parnr  = p_parnr.              "message partner
    wa_nast-parvw  = p_parvw.              "partner function '
    wa_nast-erdat  = sy-datum.             "current date
    wa_nast-eruhr  = sy-uzeit.             "current time
    wa_nast-nacha  = p_nacha.              "message transmission medium
    wa_nast-anzal  = '0'.                  "number of messages
    wa_nast-vsztp  = '4'.                 "Dispatch time
    wa_nast-vstat  = '0'.                 "processing status
    wa_nast-usnam = sy-uname.
    wa_nast-aende = 'X'.
    APPEND wa_nast TO it_nast.
  ENDLOOP.
ENDFORM.                    " BUILD_DATA
*&---------------------------------------------------------------------*
*&      Form  TRIGGER_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trigger_output .
  CLEAR: lt_msg_nast.

  LOOP AT it_nast INTO wa_nast.
    CLEAR: wa_msg0,
           lt_msg_nast1.
    REFRESH: lt_msg_nast1.
**Now Update the NAST table with the fm 'RV_MESSAGE_UPDATE_SINGLE'
    CALL FUNCTION 'RV_MESSAGE_UPDATE_SINGLE'
      EXPORTING
        msg_nast = wa_nast.

    MOVE-CORRESPONDING wa_nast TO wa_msg0.
*    APPEND wa_msg0 TO lt_msg_nast.
    APPEND wa_msg0 TO lt_msg_nast1.

    CALL FUNCTION 'WFMC_MESSAGES_PROCESS'
      EXPORTING
        pi_display_id = 'NALIV2'
        pi_no_dialog  = 'X'
      TABLES
        tx_messages   = lt_msg_nast1
        tx_display    = t_disp.
    APPEND LINES OF lt_msg_nast1 to lt_msg_nast.
  ENDLOOP.

ENDFORM.                    " TRIGGER_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid_display .

IF p_dl IS INITIAL.
  CLEAR: lt_output[].
  LOOP AT lt_msg_nast INTO wa_msg0.
    MOVE-CORRESPONDING wa_msg0 TO wa_output.
    CASE wa_msg0-vstat.
      WHEN '0'.
        wa_output-message = 'NOT PROCESSED'.
      WHEN '1'.
        wa_output-message = 'SUCCESSFULLY PROCESSED'.
      WHEN '2'.
        wa_output-message = 'INCORRECTLY PROCESSED'.
    ENDCASE.
    APPEND wa_output TO lt_output.
  ENDLOOP.
ENDIF.

  IF lt_output[] IS NOT INITIAL.
    TRY.
        CALL METHOD cl_salv_table=>factory
*          EXPORTING
*            list_display = if_salv_c_bool_sap=>false
          IMPORTING
            r_salv_table = gr_table
          CHANGING
            t_table      = lt_output[].
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

*  IF lt_output[] IS NOT INITIAL.
*    CLEAR lv_title.
*    MOVE 'Send Scheduling Agreements to Openlink' TO lv_title.
*  ENDIF.
*
*  gr_display = gr_table->get_display_settings( ).               " Global Display settings"
*  gr_display->set_striped_pattern( if_salv_c_bool_sap=>true ).  "Activate Strip Pattern"
*  gr_display->set_list_header( lv_title ).                      "Report Header"

******* Aggregate Function Settings *******
  gr_aggr = gr_table->get_aggregations( ).                      "Get Aggregate Functions"

******* Sort Functions *******
  gr_sort = gr_table->get_sorts( ).

******* Table Selection Settings *******
  gr_select = gr_table->get_selections( ).
  IF gr_select IS NOT INITIAL.
    gr_select->set_selection_mode( if_salv_c_selection_mode=>row_column ). "Allow single row Selection"
  ENDIF.

******** Top of List settings *******
*  PERFORM top_of_page CHANGING gr_content.
*  gr_table->set_top_of_list( gr_content ).

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
    IF lt_output IS NOT INITIAL.
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

  IF lt_output IS NOT INITIAL.
    MOVE 'Send Scheduling Agreements to Openlink' TO lr_head.
    CREATE OBJECT lr_grid.
    DESCRIBE TABLE lt_output LINES lv_cnt.
  ENDIF.

** Header of Top of Page **
  lr_grid->create_header_information( row     = 1
                                      column  = 1
                                      text    = lr_head
                                      tooltip = lr_head ).
** Add Row **
  lr_grid->add_row( ).

** Move lr_grid to lr_content **
  lr_content = lr_grid.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  COLUMN_SETTINGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM column_settings .

  LOOP AT lt_column_ref INTO ls_column_ref.
    TRY.
        gr_column ?= gr_columns->get_column( ls_column_ref-columnname ).
      CATCH cx_salv_not_found.
    ENDTRY.

    IF gr_column IS NOT INITIAL.
      IF gr_column->get_columnname( ) = 'MESSAGE'.
        gr_column->set_short_text( 'Message').
        gr_column->set_medium_text( 'Message').
        gr_column->set_long_text( 'Message').
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " COLUMN_SETTINGS
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_data .
  DATA: lv_bukrs(3) TYPE c VALUE 'UGL',
        lv_bstyp(1) TYPE c,
        lv_ekorg(4) TYPE c VALUE 'GASA',
        lv_frgke(1) TYPE c VALUE 'Y'.
  TYPES : BEGIN OF ty_ekko,
            ebeln TYPE ebeln,
            bukrs TYPE bukrs,
    END OF ty_ekko.
  TYPES: rng_bsart TYPE RANGE OF ekko-bsart.

  DATA: lta_ekko TYPE STANDARD TABLE OF ty_ekko,
        lwa_ekko TYPE ty_ekko,
        lta_bsart TYPE rng_bsart,
        lwa_rng  TYPE LINE OF rng_bsart.

  IF p_splyd IS NOT INITIAL.
    lv_bstyp = 'L'.
  ELSEIF p_cntrt IS NOT INITIAL.
    lv_bstyp = 'K'.
    lwa_rng-sign = 'I'.
    lwa_rng-option = 'EQ'.
    lwa_rng-low = 'ZJ'.
    lwa_rng-low = 'ZK'.
    APPEND lwa_rng TO lta_bsart.
  ENDIF.
  SELECT ebeln bukrs
    FROM ekko
    INTO TABLE  lta_ekko
    WHERE ebeln IN s_ebeln
    AND bukrs EQ lv_bukrs
    AND bstyp EQ lv_bstyp
    AND bsart IN lta_bsart
    AND ekorg EQ lv_ekorg
    AND frgke EQ lv_frgke.
  SORT lta_ekko BY ebeln.
  LOOP AT s_ebeln.
    wa_output-kappl = p_kappl.
    wa_output-objky = s_ebeln-low.
    wa_output-kschl = p_kschl.
    wa_output-parnr = p_parnr.
    wa_output-erdat = sy-datum.
    wa_output-usnam = sy-uname.
    wa_output-vstat = '0'.

    READ TABLE lta_ekko INTO lwa_ekko WITH KEY ebeln = s_ebeln-low BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_output-message = 'VALID'.
    ELSE.
      wa_output-message = 'INVALID'.
    ENDIF.
    APPEND wa_output TO lt_output.

  ENDLOOP.
ENDFORM.                   " VALIDATE_DATA
