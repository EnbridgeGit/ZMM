REPORT zmmmi015. " NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.

************************************************************************
*    Program     :  ZMMMI015 - MM: EXTRACT COMPANY WIDE QOH AND AVP
*    Programmer  :  Ric Aarssen
*    Date        :  February 5, 1998
*    Modified    :  October 28, 1998
************************************************************************
*    1998/10/28 - excluded the P2* plants from the calculation of the
*                 company wide average unit price
* 98/06/29 #553 mdemeest add manufacturer & manufacturer#/model #
* 2012/01/10 - SAHMAD - Changes to the material master due to the MDM
*implementation have resulted in missing data from this report. The report
*is pulling information from fields/objects no longer applicable post-MDM.
*The report needs to be modified to pull information from new fields/objects
*in the material master. Converted to ALV as requested by functional
*****************************  TABLES   ********************************

TABLES: mara, mbew.

*------ Internal table for accumulating quantities for Company ---------
DATA: BEGIN OF table_matnr OCCURS 0,
        matkl            LIKE mara-matkl,       "Material Group
        matnr            LIKE mara-matnr,       "Material Number
*        t_werks          like mbew-bwkey,       "Plant
        maktx            LIKE makt-maktx,
        qoh              LIKE mbew-lbkum,       "Quantity on Hand
        aup              LIKE mbew-verpr,       "Average Unit Price
        mstae            LIKE mara-mstae,
        name1            LIKE lfa1-name1,
        mfrnr            LIKE mara-mfrnr,
        mfrpn            LIKE mara-mfrpn,
      END OF table_matnr.
DATA: gt_matnr LIKE TABLE OF table_matnr,
      gs_matnr LIKE table_matnr.
DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.
*---------------------- Work Area --------------------------------------
DATA: w_qoh           LIKE mbew-lbkum,          "Qty on Hand
      w_value         LIKE mbew-salk3,          "Dollar Value
      w_sloc_count(2) TYPE p,                   "Storage Loc Count
      w_aup_total     LIKE mbew-verpr.          "Total Average Price
DATA: gt_mara TYPE TABLE OF mara,
      gs_mara TYPE mara.
DATA: gv_counter TYPE i.
*- Field symbols for dynamic internal table
FIELD-SYMBOLS: <it_table> TYPE table,
               <wa_table> TYPE ANY.
*&---------------------------------------------------------------------*
*&       Class LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: hotspot_click1 FOR EVENT link_click OF cl_salv_events_table
                         IMPORTING row column.
ENDCLASS.               "LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_handler
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD hotspot_click1.
    PERFORM handle_click1 USING row column.
  ENDMETHOD.                    "hotspot_click1
ENDCLASS.               "lcl_event_handler
***********************  SELECTION SCREEN  *****************************
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) text-001.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.
SELECT-OPTIONS: s_matkl FOR mara-matkl.
SELECTION-SCREEN END OF BLOCK box1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) text-002.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME.
SELECT-OPTIONS: p_matnr FOR mara-matnr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK box2.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) text-900.
***************************  MAIN ROUTINE  *****************************
*top-of-page is NOT being used for the headings since the heading is to
* appear only once on the report for ease when the report gets dumped to
* excel
*-----------------------------------------------------------------------
START-OF-SELECTION.
  CLEAR: table_matnr,
         gt_matnr.

  gv_counter = 1.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM dynamic_it.
  PERFORM fill_data.
  CALL SCREEN 100.
** print report header
*  WRITE: /1 text-rpt, sy-repid COLOR COL_GROUP,
*         60 text-hdg,                                               "Title
*        142 text-dte, sy-datum, text-amp, sy-uzeit.
*  WRITE: / text-clt UNDER text-rpt, sy-mandt UNDER sy-repid,
*           text-014 UNDER text-hdg,
*           text-pge UNDER text-dte, sy-pagno UNDER sy-datum.
*  ULINE.
*  FORMAT COLOR COL_NORMAL.
*
*  WRITE: /80 text-010, 90 text-012.
*  WRITE: /1 text-003, 15 text-008, 65 text-007, text-011 UNDER text-010,
*            text-013 UNDER text-012, 106 text-015, 126 text-016.
*  ULINE.
*  WRITE: /.
*  MOVE 7 TO matrctr.
*
*
**------- sort table by material number within material group -----------
*  SORT table_matnr ASCENDING BY matkl matnr.
*  LOOP AT table_matnr.
*    AT NEW matkl.                              "Heading for Material Group
*      ADD 2 TO matrctr.
*      SKIP TO LINE matrctr.
*      WRITE: /1 'GROUP', table_matnr-matkl.
*      ADD 1 TO matrctr.
*    ENDAT.
*
*    object = table_matnr-matnr.
**PERFORM FIND_CHARACTERISTIC.
**-------------------- write report from table --------------------------
*    MOVE matrctr TO: manuctr, modelctr.
** If/new-page was added to handle the situtation when material info is
** written on the last line of the page.  Material info would print, then
** a new page, and then the relevant manufacturer/model info.
**-----------------------------------------------------------------------
*    IF sy-linno > 56.
*      NEW-PAGE.
*    ENDIF.
*************
*ENDLOOP
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get Data for output
*----------------------------------------------------------------------*
FORM get_data .

*-------------- read material master for a material group --------------
  SELECT * FROM mara
     WHERE matkl IN s_matkl
       AND matnr IN p_matnr.
*------ read material valuation table & accumulate QOH and $value ------
    CLEAR: table_matnr, w_qoh, w_value, w_sloc_count, w_aup_total.
    SELECT * FROM mbew
      WHERE matnr = mara-matnr.
      IF mbew-bwkey+1(1) <> '2'.
        w_qoh   = w_qoh + mbew-lbkum.
        w_value = w_value + mbew-salk3.
        IF mbew-verpr > 0.
          w_aup_total  = w_aup_total + mbew-verpr.
          w_sloc_count = w_sloc_count + 1.
        ENDIF.
      ENDIF.
    ENDSELECT.
*------ calculate aup and move material record to internal table -------
    MOVE mara-matkl        TO table_matnr-matkl.
    MOVE mara-matnr        TO table_matnr-matnr.
    MOVE w_qoh             TO table_matnr-qoh.
    MOVE mara-mstae        TO table_matnr-mstae.
    IF w_value > 0.
      table_matnr-aup = w_value / w_qoh.
    ELSE.
      table_matnr-aup = w_aup_total / w_sloc_count.
    ENDIF.

    SELECT SINGLE maktx INTO gs_matnr-maktx FROM makt   "new
      WHERE matnr = table_matnr-matnr.
      move gs_matnr-maktx to table_matnr-maktx.

*---------------- add records to table work area -----------------------
    APPEND table_matnr.
  ENDSELECT.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM process_data .

  DATA: fv_counter TYPE i.

  SORT table_matnr ASCENDING BY matkl matnr.
  LOOP AT table_matnr.
    CLEAR: gt_mara,
           gs_mara,
           gs_matnr.
    MOVE-CORRESPONDING table_matnr TO gs_matnr.

    SELECT SINGLE maktx INTO gs_matnr-maktx FROM makt
          WHERE matnr = table_matnr-matnr.

    SELECT * INTO TABLE gt_mara  FROM mara
      WHERE bmatn = table_matnr-matnr
        AND mtart = 'HERS'.
    IF sy-subrc = 0.
      fv_counter = 0.
      LOOP AT gt_mara INTO gs_mara.
        fv_counter = fv_counter + 1.
        SELECT SINGLE name1 INTO gs_matnr-name1 FROM lfa1
                            WHERE lifnr = gs_mara-mfrnr.
        gs_matnr-mfrnr = gs_mara-mfrnr.
        gs_matnr-mfrpn = gs_mara-mfrpn.
        APPEND gs_matnr TO gt_matnr.
      ENDLOOP.
      IF fv_counter > gv_counter.
        gv_counter = fv_counter.
      ENDIF.
    ELSE.
      APPEND gs_matnr TO gt_matnr.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'STAT_01'.
  SET TITLEBAR 'T01'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.

  DATA: lr_table1 TYPE REF TO cl_salv_table,
        lr_functions TYPE REF TO cl_salv_functions,
        lr_con1 TYPE REF TO cl_gui_custom_container,
        lr_display TYPE REF TO cl_salv_display_settings,
        lr_event TYPE REF TO lcl_event_handler,
        lr_events_salv TYPE REF TO cl_salv_events_table,
        lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column TYPE REF TO cl_salv_column_table,
        lv_lines TYPE n LENGTH 5,
        lv_msg TYPE lvc_title.
  DATA: lt_columns TYPE  salv_t_column_ref,
        ls_columns LIKE LINE OF lt_columns,
        fl_columnname TYPE lvc_fname,
        fl_columnnumber TYPE n,
        lv_long_text TYPE scrtext_l,
        lv_short_text TYPE scrtext_s.
******************Container 1*************************
  CREATE OBJECT lr_con1
    EXPORTING
*    parent                      =
      container_name              = 'ALV_CON1'
*    style                       =
*    lifetime                    = lifetime_default
*    repid                       =
*    dynnr                       =
*    no_autodef_progid_dynnr     =
*  EXCEPTIONS
*    cntl_error                  = 1
*    cntl_system_error           = 2
*    create_error                = 3
*    lifetime_error              = 4
*    lifetime_dynpro_dynpro_link = 5
*    others                      = 6
      .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
          r_container    = lr_con1
          container_name = 'ALV_CON1'
        IMPORTING
          r_salv_table   = lr_table1
        CHANGING
          t_table        = <it_table>. "gt_matnr.
    CATCH cx_salv_msg .
  ENDTRY.
*Function settings
  lr_functions = lr_table1->get_functions( ).
  lr_functions->set_all( abap_true ).
*Display Setting
  lr_display = lr_table1->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
*  lr_display->set_list_header( lv_msg ).
*Event
  lr_events_salv = lr_table1->get_event( ).
  CREATE OBJECT lr_event.
  SET HANDLER: lr_event->hotspot_click1
               FOR lr_events_salv.
*Hotspot
  CALL METHOD lr_table1->get_columns
    RECEIVING
      value = lr_columns.
  TRY.
      lr_column ?= lr_columns->get_column( 'MATNR' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.

  CALL METHOD lr_columns->get
    RECEIVING
      value = lt_columns.

  LOOP AT lt_columns INTO ls_columns.
    CHECK ls_columns-columnname(5) = 'MFRPN' OR
          ls_columns-columnname(5) = 'MFRNR' OR
          ls_columns-columnname(5) = 'NAME1'.
    fl_columnname = ls_columns-columnname(5).
    fl_columnnumber = ls_columns-columnname+5(3).
    CONDENSE fl_columnnumber NO-GAPS.
*Set the column Header
    TRY.
        lr_column ?= lr_columns->get_column( ls_columns-columnname ).
      CATCH cx_salv_not_found .
    ENDTRY.
    IF ls_columns-columnname(5) = 'NAME1'.
      lv_long_text  = 'Name'.
      lv_short_text = 'Name'.
    ELSE.
      CALL METHOD lr_column->get_long_text
        RECEIVING
          value = lv_long_text.
      CALL METHOD lr_column->get_short_text
        RECEIVING
          value = lv_short_text.
    ENDIF.
    CONCATENATE lv_long_text '- ' fl_columnnumber INTO lv_long_text.
    CONCATENATE lv_short_text '- ' fl_columnnumber INTO lv_short_text.
    CALL METHOD lr_column->set_long_text
      EXPORTING
        value = lv_long_text.
    CALL METHOD lr_column->set_short_text
      EXPORTING
        value = lv_short_text.
  ENDLOOP.
*Set the column Header
*  TRY.
*    lr_column ?= lr_columns->get_column( '' ).
*    CATCH cx_salv_not_found .
*  ENDTRY.
*  CALL METHOD lr_column->set_long_text
*    EXPORTING
*      value = 'Type'.
*  CALL METHOD lr_column->set_short_text
*    EXPORTING
*      value = 'Type'.
*******output length
*  TRY.
*      lr_column ?= lr_columns->get_column( '' ).
*      CALL METHOD lr_column->set_output_length
*        EXPORTING
*          value = 40.
*    CATCH cx_salv_not_found .
*  ENDTRY.
******************************
 CALL METHOD lr_table1->display.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_CLICK1
*&---------------------------------------------------------------------*
*       Call MM03 Transaction
*----------------------------------------------------------------------*
FORM handle_click1  USING    p_row
                             p_column.
*Read the talbe with row-id .
  DATA: ls_matnr LIKE LINE OF gt_matnr.
  FIELD-SYMBOLS: <fs_comp>.
*  READ TABLE gt_matnr INTO ls_matnr INDEX p_row.
  READ TABLE  <it_table> INTO <wa_table> INDEX p_row.
  ASSIGN COMPONENT 'MATNR' OF STRUCTURE <wa_table> TO <fs_comp>.
*  lv_matnr =
   CASE p_column.
    WHEN 'MATNR'.
      SET PARAMETER ID 'MAT' FIELD <fs_comp>.
      CALL TRANSACTION 'MM03'
           AND SKIP FIRST SCREEN.

  ENDCASE.

ENDFORM.                    " HANDLE_CLICK1
*&---------------------------------------------------------------------*
*&      Form  DYNAMIC_IT
*&---------------------------------------------------------------------*
*       Create Dynamic internal table
*----------------------------------------------------------------------*
FORM dynamic_it .

  DATA: it_field_dom TYPE cl_abap_structdescr=>component_table,
        wa_field_dom LIKE LINE OF it_field_dom.
*-Table description for dynamic table
  DATA: wr_struct_descr TYPE REF TO cl_abap_structdescr,
        wr_tab_descr TYPE REF TO cl_abap_tabledescr.

  DATA: fv_counter TYPE i,
        fv_cnt TYPE string.
  DATA: fl_mfrnr TYPE string,
        fl_mfrpn TYPE string,
        fl_name1 TYPE string.
  DATA: wr_table_data TYPE REF TO data,
        wr_line_data TYPE REF TO data.


*cl_abap_structdescr=>component_table will have all the components of dynamic internal table

  wa_field_dom-name = 'MATKL'.
  wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'MATKL' ).
  APPEND wa_field_dom TO it_field_dom.

  wa_field_dom-name = 'MATNR'.
  wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'MATNR' ).
  APPEND wa_field_dom TO it_field_dom.

  wa_field_dom-name = 'MAKTX'.
  wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'MAKTX' ).
  APPEND wa_field_dom TO it_field_dom.

  wa_field_dom-name = 'QOH'.
  wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'LBKUM' ).
  APPEND wa_field_dom TO it_field_dom.

  wa_field_dom-name = 'AUP'.
  wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'VERPR' ).
  APPEND wa_field_dom TO it_field_dom.

  wa_field_dom-name = 'MSTAE'.
  wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'MSTAE' ).
  APPEND wa_field_dom TO it_field_dom.
  fv_counter = 1.
  WHILE fv_counter <= gv_counter.

    fv_cnt = fv_counter.
    CONCATENATE 'MFRNR' fv_cnt INTO fl_mfrnr.
    CONCATENATE 'MFRPN' fv_cnt INTO fl_mfrpn.
    CONCATENATE 'NAME1' fv_cnt INTO fl_name1.
    CONDENSE fl_mfrnr NO-GAPS.
    CONDENSE fl_mfrpn NO-GAPS.
    CONDENSE fl_name1 NO-GAPS.
    wa_field_dom-name = fl_mfrnr.
    wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'MFRNR' ).
    APPEND wa_field_dom TO it_field_dom.

    wa_field_dom-name = fl_name1.
    wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'NAME1' ).
    APPEND wa_field_dom TO it_field_dom.

    wa_field_dom-name = fl_mfrpn.
    wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'MFRPN' ).
    APPEND wa_field_dom TO it_field_dom.



    fv_counter = fv_counter + 1.

  ENDWHILE.

*Important step: to add CELLTAB field to the dynamic internal table
*This field will be required to control the ALV settings at cell level.
  wa_field_dom-name = 'CELLTAB'.
  wa_field_dom-type ?= cl_abap_datadescr=>describe_by_name( 'LVC_T_STYL' ).
  APPEND wa_field_dom TO it_field_dom.
***Create line structure
  CALL METHOD cl_abap_structdescr=>create
    EXPORTING
      p_components = it_field_dom
      p_strict     = cl_abap_structdescr=>false
    RECEIVING
      p_result     = wr_struct_descr.
**Create table type based on the structure
  CALL METHOD cl_abap_tabledescr=>create
    EXPORTING
      p_line_type  = wr_struct_descr
      p_table_kind = cl_abap_tabledescr=>tablekind_std
    RECEIVING
      p_result     = wr_tab_descr.
**Assign the tables
  CREATE DATA wr_table_data TYPE HANDLE wr_tab_descr.
  ASSIGN wr_table_data->* TO <it_table>.
  CREATE DATA wr_line_data TYPE HANDLE wr_struct_descr.
  ASSIGN wr_line_data->* TO <wa_table>.
***********************************************************************
ENDFORM.                    " DYNAMIC_IT
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_data .

  FIELD-SYMBOLS: <fs_comp>,
                 <it_comp> TYPE SORTED TABLE.
  DATA: lv_cnt TYPE i,
        lv_cntr TYPE string.
  DATA: fl_mfrnr TYPE string,
        fl_mfrpn TYPE string,
        fl_name1 TYPE string.

  SORT gt_matnr BY matnr mfrnr mfrpn.
  LOOP AT table_matnr.
    CLEAR <wa_table>.
    ASSIGN COMPONENT 'MATKL' OF STRUCTURE <wa_table> TO <fs_comp>.
    IF sy-subrc = 0.
      <fs_comp> = table_matnr-matkl.
    ENDIF.
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <wa_table> TO <fs_comp>.
    IF sy-subrc = 0.
      <fs_comp> = table_matnr-matnr.
    ENDIF.
    ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <wa_table> TO <fs_comp>.
    IF sy-subrc = 0.
      <fs_comp> = table_matnr-maktx.
    ENDIF.
    ASSIGN COMPONENT 'QOH' OF STRUCTURE <wa_table> TO <fs_comp>.
    IF sy-subrc = 0.
      <fs_comp> = table_matnr-qoh.
    ENDIF.
    ASSIGN COMPONENT 'AUP' OF STRUCTURE <wa_table> TO <fs_comp>.
    IF sy-subrc = 0.
      <fs_comp> = table_matnr-aup.
    ENDIF.
    ASSIGN COMPONENT 'MSTAE' OF STRUCTURE <wa_table> TO <fs_comp>.
    IF sy-subrc = 0.
      <fs_comp> = table_matnr-mstae.
    ENDIF.
    lv_cnt = 1.
    READ TABLE gt_matnr WITH KEY matnr = table_matnr-matnr TRANSPORTING NO FIELDS.
    LOOP AT gt_matnr INTO gs_matnr WHERE matnr = table_matnr-matnr.
      lv_cntr = lv_cnt.
      CONCATENATE 'MFRNR' lv_cntr INTO fl_mfrnr.
      CONCATENATE 'MFRPN' lv_cntr INTO fl_mfrpn.
      CONCATENATE 'NAME1' lv_cntr INTO fl_name1.
      CONDENSE fl_mfrnr NO-GAPS.
      CONDENSE fl_mfrpn NO-GAPS.
      CONDENSE fl_name1 NO-GAPS.
      ASSIGN COMPONENT fl_mfrnr OF STRUCTURE <wa_table> TO <fs_comp>.
      IF sy-subrc = 0.
        <fs_comp> = gs_matnr-mfrnr.
      ENDIF.
      ASSIGN COMPONENT fl_mfrpn OF STRUCTURE <wa_table> TO <fs_comp>.
      IF sy-subrc = 0.
        <fs_comp> = gs_matnr-mfrpn.
      ENDIF.
      ASSIGN COMPONENT fl_name1 OF STRUCTURE <wa_table> TO <fs_comp>.
      IF sy-subrc = 0.
        <fs_comp> = gs_matnr-name1.
      ENDIF.
      lv_cnt = lv_cnt + 1.
    ENDLOOP.
    APPEND <wa_table> TO <it_table>.
  ENDLOOP.
ENDFORM.                    " FILL_DATA
