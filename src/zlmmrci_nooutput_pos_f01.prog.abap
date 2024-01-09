*&---------------------------------------------------------------------*
*&  Include           ZLMMRCI_NOOUTPUT_POS_F01
*&---------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 11-Oct-2018                                          *
* Created By    : SKAKUMANU                                            *
* Correction No : D30K929174                                           *
* Object ID     : CI Item                                              *
* Description   : Report to display POs with no output triggered       *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_EKKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ekko .
* Get PO data
  SELECT  ebeln	"Purchasing Document Number
          bukrs	"Company Code
          bsart	"Purchasing Document Type
          aedat	"Date on Which Record Was Created
          ernam	"Name of Person who Created the Object
          lifnr	"Vendor Account Number
          ekorg	"Purchasing Organization
          ekgrp	"Purchasing Group
    FROM ekko
    INTO TABLE gt_ekko
    WHERE bstyp EQ 'F'
      AND aedat IN s_aedat
      AND ekorg IN s_ekorg.
ENDFORM.                    " GET_EKKO
*&---------------------------------------------------------------------*
*&      Form  GET_NAST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_nast .
* Get Output details
  IF gt_ekko IS NOT INITIAL.
*   We dont have application to pass, and the time interval
*   is for last one month only, this will not cause any performance issue
    SELECT
    objky	"Object key
    FROM nast
    INTO TABLE gt_nast
    FOR ALL ENTRIES IN gt_ekko
    WHERE objky	= gt_ekko-ebeln.
    IF sy-subrc = 0.
      SORT gt_nast BY objky.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_NAST
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .
* If there are any outputs exists
  IF gt_nast IS NOT INITIAL.
    LOOP AT gt_ekko INTO gs_ekko.
*     Check if there is any output exists for PO
      READ TABLE gt_nast
      INTO gs_nast
      WITH KEY objky = gs_ekko-ebeln BINARY SEARCH.
      IF sy-subrc NE 0.
*       If no output exists get the record to output table
        MOVE-CORRESPONDING: gs_ekko TO gs_final.
        APPEND gs_final TO gt_final.
        CLEAR: gs_final, gs_ekko.
      ENDIF.
    ENDLOOP.
* If there are no outputs for POs
  ELSE.
*   Append all POs to output
    LOOP AT gt_ekko INTO gs_ekko.
      MOVE-CORRESPONDING gs_ekko TO gs_final.
      APPEND gs_final TO gt_final.
      CLEAR:gs_final, gs_ekko.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_alv .
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_final.
    ##NO_HANDLER CATCH cx_salv_msg .
  ENDTRY.
ENDFORM.                    " INITIALIZE_ALV
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM field_catalog .
  DATA: lo_functions TYPE REF TO cl_salv_functions.
  IF go_alv IS NOT INITIAL.
*    Get columns of ALV
    CALL METHOD go_alv->get_columns
      RECEIVING
        value = go_columns.

*    Get column
    TRY.
        go_column ?= go_columns->get_column('EBELN').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Purch.Doc'(002) ).
    go_column->set_medium_text( 'Purch.Doc'(002) ).
    go_column->set_short_text( 'Purch.Doc'(002) ).

    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '15'.

    TRY.
        go_column ?= go_columns->get_column('BUKRS').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Company code'(003) ).
    go_column->set_medium_text( 'Company code'(003) ).
    go_column->set_short_text( 'Company Cd'(004) ).

    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '10'.

    TRY.
        go_column ?= go_columns->get_column('BSART').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Document Type'(005) ).
    go_column->set_medium_text( 'Document Type'(005) ).
    go_column->set_short_text( 'Doc. Type'(006) ).

    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '10'.

    TRY.
        go_column ?= go_columns->get_column('ERNAM').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Created By'(007) ).
    go_column->set_medium_text( 'Created By'(007) ).
    go_column->set_short_text( 'Created By'(007) ).

    TRY.
        go_column ?= go_columns->get_column('LIFNR').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Vendor'(008) ).
    go_column->set_medium_text( 'Vendor'(008) ).
    go_column->set_short_text( 'Vendor'(008) ).

    TRY.
        go_column ?= go_columns->get_column('EKORG').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Purchasing org.'(009) ).
    go_column->set_medium_text( 'Purchasing org.'(009) ).
    go_column->set_short_text( 'Purch org.'(010) ).

    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '10'.

    TRY.
        go_column ?= go_columns->get_column('EKGRP').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Purchasing grp.'(011) ).
    go_column->set_medium_text( 'Purchasing grp.'(011) ).
    go_column->set_short_text( 'Purch grp.'(012) ).

    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '10'.

    TRY.
        go_column ?= go_columns->get_column('AEDAT').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Created on'(013) ).
    go_column->set_medium_text( 'Created on'(013) ).
    go_column->set_short_text( 'Created on'(013) ).

    lo_functions = go_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

  ENDIF.
ENDFORM.                    " FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  IF go_alv IS NOT INITIAL.
    go_alv->display( ).
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
