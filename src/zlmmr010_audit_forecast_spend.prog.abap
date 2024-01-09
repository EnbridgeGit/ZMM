*&---------------------------------------------------------------------*
*& Report  ZLMMR010_AUDIT_FORECAST_SPEND
*&---------------------------------------------------------------------*
*& Program Name       : ZLMMR010_AUDIT_FORECAST_SPEND                  *
*& Author             : Sajjad Ahmad                                   *
*& Creation Date      : 16-Jan-2012                                    *
*& Object ID          : QR99 - Audit Report Forecast Spend             *
*& Application Area   : MM                                             *
*& Description        : Report displays prior 3 full yrs of historical *
*&                      data of material spend by year to forecast the *
*&                      future spend for DOA purposes.                 *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*Modification Log(Latest Version on Top)                               *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 16-Jan-2012                                          *
* Modified By   : Sajjad Ahmad                                         *
* Correction No : D30K919046                                           *
* Description   : Initial version                                      *
*----------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 07-Jun-2016                                          *
* Modified By   : Praveena Anusuri                                     *
* Correction No : D30K926912                                           *
* Description   : ACR-749 Replace the field 'Control Indicator' with   *
*                 new field 'PO Type' with values MRP or Non MRP, which*
*                 will be used to select MRP or Non MRP purchase orders*
*----------------------------------------------------------------------*
REPORT  zlmmr010_audit_forecast_spend.
TYPE-POOLS: vrm.  "(+)PANUSURI Ticket ACR-749
TABLES: ekko,
        ekpo,
        mara,
        eban.

DATA: gv_counter TYPE i.

TYPES: BEGIN OF ty_output,
        monam   TYPE t015m-monam,
        netwr_3 TYPE ekpo-netwr, "current year -3
        netwr_2 TYPE ekpo-netwr, "current year -2
        netwr_1 TYPE ekpo-netwr,  "current year -1
        netwr_f TYPE ekpo-netwr, "Forecast year
        netwr_c TYPE ekpo-netwr,  "Year to Date
       END OF ty_output.
TYPES: BEGIN OF ty_tab,
          monat(2),
          bedat  TYPE ekko-bedat,
          ebeln  TYPE ekko-ebeln,
          waers  TYPE ekko-waers,
          wkurs  TYPE ekko-wkurs,
          matnr  TYPE ekpo-matnr,
          werks  TYPE ekpo-werks,
          netwr  TYPE ekpo-netwr,
*BOC by PANUSURI Ticket ACR-749
*          banfn  TYPE ekpo-banfn,
*          bnfpo  TYPE ekpo-bnfpo,
*          estkz  TYPE eban-estkz,
*EOC by PANUSURI Ticket ACR-749
          matkl  TYPE mara-matkl,
       END OF ty_tab.

DATA: gt_output TYPE TABLE OF ty_output.
DATA:   gv_year3(4),
        gv_year2(4),
        gv_year1(4),
        gv_year(4).
*BOI by PANUSURI Ticket ACR-749
DATA: ta_potyp_list  TYPE vrm_values,
      wa_cond(72)    TYPE c,
      ta_cond        LIKE TABLE OF wa_cond.
*EOI by PANUSURI Ticket ACR-749

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*PARAMETERS: p_estkz TYPE eban-estkz.
PARAMETERS: pa_potyp TYPE c AS LISTBOX VISIBLE LENGTH 9."(+)PANUSURI Ticket ACR-749
*SELECT-OPTIONS: s_estkz FOR eban-estkz NO INTERVALS,   "(-)PANUSURI Ticket ACR-749
SELECT-OPTIONS: s_ekgrp FOR ekko-ekgrp,                 "(+)PANUSURI Ticket ACR-749
                s_mtart FOR ekpo-mtart,
                s_werks FOR ekpo-werks,
                s_matkl FOR mara-matkl,
                s_matnr FOR ekpo-matnr.
SELECTION-SCREEN END OF BLOCK b1.

*BOI by PANUSURI Ticket ACR-749
INITIALIZATION.
  PERFORM set_potyp_list.
*EOI by PANUSURI Ticket ACR-749

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM alv_display.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get Data
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_tab TYPE TABLE OF ty_tab,
        ls_tab LIKE LINE OF lt_tab,
        ls_output LIKE LINE OF gt_output.
  DATA: lv_edat TYPE sy-datum,
        lv_sdat TYPE sy-datum,
        lv_monum TYPE t015m-monum,
        lv_mm(2),
        lv_day(2),
        lv_year(4),
        lv_pocad TYPE netwr.

  lv_edat = sy-datum.
  lv_mm  = '01'.
  lv_day = '01'.
  gv_year3 = lv_edat(4) - 3. "sdat(4).
  gv_year2 = lv_edat(4) - 2.
  gv_year1 = lv_edat(4) - 1.
  CONCATENATE gv_year3 lv_mm lv_day INTO lv_sdat.

*BOC by PANUSURI Ticket ACR-749
*  SELECT a~bedat a~ebeln a~waers a~wkurs b~matnr
*         b~werks b~netwr b~banfn b~bnfpo
*         INTO CORRESPONDING FIELDS OF TABLE lt_tab
*         FROM ekko AS a
*         INNER JOIN ekpo AS b  ON a~mandt = b~mandt
*                              AND a~ebeln = b~ebeln
*         WHERE b~matnr <> space
*           AND ( a~bedat >= lv_sdat AND a~bedat <= lv_edat )
*           AND b~loekz = space
*           AND a~bstyp = 'F'
*           AND b~matnr IN s_matnr
*           AND b~werks IN s_werks
*           AND b~mtart IN s_mtart.
*EOC by PANUSURI Ticket ACR-749
*BOI by PANUSURI Ticket ACR-749
* Build dynamic WHERE clause.
  PERFORM build_dyn_where USING lv_sdat lv_edat.

  SELECT a~bedat
         a~ebeln
         a~waers
         a~wkurs
         b~matnr
         b~werks
         b~netwr
         INTO CORRESPONDING FIELDS OF TABLE lt_tab
         FROM ekko AS a
         INNER JOIN ekpo AS b
         ON  a~mandt = b~mandt
         AND a~ebeln = b~ebeln
         WHERE (ta_cond).
*EOI by PANUSURI Ticket ACR-749

  LOOP AT lt_tab INTO ls_tab.
*BOC by PANUSURI Ticket ACR-749
*    SELECT SINGLE estkz INTO ls_tab-estkz
*      FROM eban
*       WHERE banfn = ls_tab-banfn
*         AND bnfpo = ls_tab-bnfpo.
*EOC by PANUSURI Ticket ACR-749
    SELECT SINGLE matkl INTO ls_tab-matkl
      FROM mara
       WHERE matnr = ls_tab-matnr.
    ls_tab-monat = ls_tab-bedat+4(2).
*    MODIFY lt_tab FROM ls_tab TRANSPORTING estkz matkl monat.  "(-)PANUSURI Ticke ACR-749
    MODIFY lt_tab FROM ls_tab TRANSPORTING matkl monat.         "(+)PANUSURI Ticke ACR-749
  ENDLOOP.
*delete data which exclude by selection screen
*  if p_estkz is not initial.
*  DELETE lt_tab WHERE estkz  NOT IN s_estkz. "(-)PANUSURI Ticke ACR-749
*  endif.
  DELETE lt_tab WHERE matkl NOT IN s_matkl.
*Prepare data for output.
  SORT lt_tab BY monat. "bedat.
  LOOP AT lt_tab INTO ls_tab.
    lv_monum = ls_tab-bedat+4(2).
    lv_year  = ls_tab-bedat(4).
    AT NEW monat. "bedat(6).
      SELECT SINGLE monam INTO ls_output-monam
         FROM t015m WHERE spras = 'E'
                      AND monum = lv_monum.
    ENDAT.
    IF ls_tab-waers <> 'CAD'.
      lv_pocad = ls_tab-netwr * ls_tab-wkurs.
    ELSE.
      lv_pocad = ls_tab-netwr.
    ENDIF.
    IF lv_year = gv_year3.
      ls_output-netwr_3 = ls_output-netwr_3 + lv_pocad.
    ENDIF.
    IF lv_year = gv_year2.
      ls_output-netwr_2 = ls_output-netwr_2 + lv_pocad.
    ENDIF.
    IF lv_year = gv_year1.
      ls_output-netwr_1 = ls_output-netwr_1 + lv_pocad.
    ENDIF.
    IF lv_year = sy-datum(4).
      ls_output-netwr_c = ls_output-netwr_c + lv_pocad.
    ENDIF.
    AT END OF monat. "bedat(6). "at end of month
      ls_output-netwr_f =  ls_output-netwr_3 + ls_output-netwr_2 + ls_output-netwr_1.
      ls_output-netwr_f = ls_output-netwr_f / 3. "divided by 3
      APPEND ls_output TO gt_output.
      CLEAR ls_output.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_display .


  DATA: lr_table1 TYPE REF TO cl_salv_table,
        lr_functions TYPE REF TO cl_salv_functions,
        lr_con1 TYPE REF TO cl_gui_custom_container,
        lr_display TYPE REF TO cl_salv_display_settings,
*       lr_event TYPE REF TO lcl_event_handler,
        lr_events_salv TYPE REF TO cl_salv_events_table,
        lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column TYPE REF TO cl_salv_column_table,
        lv_lines TYPE n LENGTH 5,
        lv_msg TYPE lvc_title,
        lv_title_l TYPE scrtext_l,
        lv_title_s TYPE scrtext_s,
        lv_title_m TYPE scrtext_m.
******************Container 1*************************

*  CREATE OBJECT lr_con1
*    EXPORTING
**    parent                      =
*      container_name              = 'ALV_CON1'
**    style                       =
**    lifetime                    = lifetime_default
**    repid                       =
**    dynnr                       =
**    no_autodef_progid_dynnr     =
**  EXCEPTIONS
**    cntl_error                  = 1
**    cntl_system_error           = 2
**    create_error                = 3
**    lifetime_error              = 4
**    lifetime_dynpro_dynpro_link = 5
**    others                      = 6
*      .
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.


  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*          r_container    = lr_con1
*          container_name = 'ALV_CON1'
        IMPORTING
          r_salv_table   = lr_table1
        CHANGING
          t_table        = gt_output.
    CATCH cx_salv_msg .
  ENDTRY.
*Function settings
  lr_functions = lr_table1->get_functions( ).
  lr_functions->set_all( abap_true ).
*Display Setting
  lr_display = lr_table1->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
*  lr_display->set_list_header( lv_msg ).
**Event
*  lr_events_salv = lr_table1->get_event( ).
*  CREATE OBJECT lr_event.
*  SET HANDLER: lr_event->hotspot_click1
*               FOR lr_events_salv.
  CALL METHOD lr_table1->get_columns
    RECEIVING
      value = lr_columns.
*  TRY.
*      lr_column ?= lr_columns->get_column( 'MATNR' ).
*    CATCH cx_salv_not_found .
*  ENDTRY.
**Hotspot.
*  CALL METHOD lr_column->set_cell_type
*    EXPORTING
*      value = if_salv_c_cell_type=>hotspot.
*Set the column Header
  TRY.
      lv_title_l = gv_year3.
      lv_title_s = gv_year3.
      lv_title_m = gv_year3.
      lr_column ?= lr_columns->get_column( 'NETWR_3' ).
    CATCH cx_salv_not_found .
      MESSAGE e000(zfi01) WITH 'Not found' ' ' ' ' ' '.
*   & & & & &

  ENDTRY.
  CALL METHOD lr_column->set_long_text
    EXPORTING
      value = lv_title_l.
  CALL METHOD lr_column->set_medium_text
    EXPORTING
      value = lv_title_m.
  CALL METHOD lr_column->set_short_text
    EXPORTING
      value = lv_title_s.
  TRY.
      lv_title_l = gv_year2.
      lv_title_s = gv_year2.
      lv_title_m = gv_year2.
      lr_column ?= lr_columns->get_column( 'NETWR_2' ).
    CATCH cx_salv_not_found .
      MESSAGE e000(zfi01) WITH 'Not found' ' ' ' ' ' '.
*   & & & & &

  ENDTRY.
  CALL METHOD lr_column->set_long_text
    EXPORTING
      value = lv_title_l.
  CALL METHOD lr_column->set_medium_text
    EXPORTING
      value = lv_title_m.
  CALL METHOD lr_column->set_short_text
    EXPORTING
      value = lv_title_s.

  TRY.
      lv_title_l = gv_year1.
      lv_title_s = gv_year1.
      lv_title_m = gv_year1.
      lr_column ?= lr_columns->get_column( 'NETWR_1' ).
    CATCH cx_salv_not_found .
      MESSAGE e000(zfi01) WITH 'Not found' ' ' ' ' ' '.
*   & & & & &

  ENDTRY.
  CALL METHOD lr_column->set_long_text
    EXPORTING
      value = lv_title_l.
  CALL METHOD lr_column->set_medium_text
    EXPORTING
      value = lv_title_m.
  CALL METHOD lr_column->set_short_text
    EXPORTING
      value = lv_title_s.
  TRY.
      lv_title_l = 'Year to Date'.
      lv_title_s = 'Yeat to Date'.
      lv_title_m = 'Year to Date'.
      lr_column ?= lr_columns->get_column( 'NETWR_C' ).
    CATCH cx_salv_not_found .
      MESSAGE e000(zfi01) WITH 'Not found' ' ' ' ' ' '.
*   & & & & &

  ENDTRY.
  CALL METHOD lr_column->set_long_text
    EXPORTING
      value = lv_title_l.
  CALL METHOD lr_column->set_medium_text
    EXPORTING
      value = lv_title_m.
  CALL METHOD lr_column->set_short_text
    EXPORTING
      value = lv_title_s.
  TRY.
      CONCATENATE 'Forecast' sy-datum(4) INTO lv_title_l.
      CONCATENATE 'Forecast' sy-datum(4) INTO lv_title_s.
      CONCATENATE 'Forecast' sy-datum(4) INTO lv_title_m.
      lr_column ?= lr_columns->get_column( 'NETWR_F' ).
    CATCH cx_salv_not_found .
      MESSAGE e000(zfi01) WITH 'Not found' ' ' ' ' ' '.
*   & & & & &

  ENDTRY.
  CALL METHOD lr_column->set_long_text
    EXPORTING
      value = lv_title_l.
  CALL METHOD lr_column->set_medium_text
    EXPORTING
      value = lv_title_m.
  CALL METHOD lr_column->set_short_text
    EXPORTING
      value = lv_title_s.
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

ENDFORM.                    " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SET_POTYP_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_potyp_list .
  DATA: lwa_potyp_list TYPE vrm_value.

  CLEAR: lwa_potyp_list,
         ta_potyp_list.
  REFRESH ta_potyp_list.

  lwa_potyp_list-key = 'M'.
  lwa_potyp_list-text = 'MRP'.
  APPEND lwa_potyp_list TO ta_potyp_list.
  CLEAR lwa_potyp_list.

  lwa_potyp_list-key = 'N'.
  lwa_potyp_list-text = 'Non MRP'.
  APPEND lwa_potyp_list TO ta_potyp_list.
  CLEAR lwa_potyp_list.

* Set status values
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'PA_POTYP'
      values          = ta_potyp_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

ENDFORM.                    " SET_POTYP_LIST
*&---------------------------------------------------------------------*
*&      Form  BUILD_DYN_WHERE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_dyn_where USING p_lv_sdat TYPE sy-datum
                           p_lv_edat TYPE sy-datum.
  DATA: lv_sdat              TYPE sy-datum,
        lv_edat              TYPE sy-datum,
        lwa_cond_tmp(72)     TYPE c.
  CONSTANTS: lco_mrp(4)      TYPE c VALUE '450%',
             lco_non_mrp1(4) TYPE c VALUE '440%',
             lco_non_mrp2(4) TYPE c VALUE '455%',
             lco_bstyp       TYPE c VALUE 'F'.

  lv_sdat = p_lv_sdat.
  lv_edat = p_lv_edat.
  IF pa_potyp IS NOT INITIAL.
    IF pa_potyp EQ 'M'. "MRP
      CONCATENATE 'A~EBELN' 'LIKE' INTO wa_cond SEPARATED BY space.
      CONCATENATE ''''lco_mrp'''' INTO lwa_cond_tmp.
      CONCATENATE wa_cond lwa_cond_tmp INTO wa_cond SEPARATED BY space.
      APPEND wa_cond TO ta_cond.
      CLEAR: wa_cond,
             lwa_cond_tmp.
    ELSEIF pa_potyp EQ 'N'. "Non MRP
      CONCATENATE '(' 'A~EBELN' 'LIKE' INTO wa_cond SEPARATED BY space.
      CONCATENATE ''''lco_non_mrp1'''' INTO lwa_cond_tmp.
      CONCATENATE wa_cond lwa_cond_tmp 'OR' 'A~EBELN' 'LIKE' INTO wa_cond SEPARATED BY space.
      CLEAR lwa_cond_tmp.
      CONCATENATE ''''lco_non_mrp2'''' INTO lwa_cond_tmp.
      CONCATENATE wa_cond lwa_cond_tmp ')' INTO wa_cond SEPARATED BY space.
      APPEND wa_cond TO ta_cond.
      CLEAR: wa_cond,
             lwa_cond_tmp.
    ENDIF.
  ELSE.
    CONCATENATE '(' 'A~EBELN' 'LIKE' INTO wa_cond SEPARATED BY space.
    CONCATENATE ''''lco_mrp'''' INTO lwa_cond_tmp.
    CONCATENATE wa_cond lwa_cond_tmp 'OR' 'A~EBELN' 'LIKE' INTO wa_cond SEPARATED BY space.
    CLEAR lwa_cond_tmp.
    CONCATENATE ''''lco_non_mrp1'''' INTO lwa_cond_tmp.
    CONCATENATE wa_cond lwa_cond_tmp 'OR' 'A~EBELN' 'LIKE' INTO wa_cond SEPARATED BY space.
    CLEAR lwa_cond_tmp.
    CONCATENATE ''''lco_non_mrp2'''' INTO lwa_cond_tmp.
    CONCATENATE wa_cond lwa_cond_tmp ')' INTO wa_cond SEPARATED BY space.
    APPEND wa_cond TO ta_cond.
    CLEAR: wa_cond,
           lwa_cond_tmp.
  ENDIF.
  IF ta_cond IS NOT INITIAL.
*   Purchasing document category
    CONCATENATE 'AND' 'A~BSTYP' 'EQ' INTO wa_cond SEPARATED BY space.
    CONCATENATE ''''lco_bstyp'''' INTO lwa_cond_tmp.
    CONCATENATE wa_cond lwa_cond_tmp INTO wa_cond SEPARATED BY space.
    APPEND wa_cond TO ta_cond.
    CLEAR: wa_cond,
           lwa_cond_tmp.
*   Purchasing group
    IF s_ekgrp IS NOT INITIAL.
      CONCATENATE 'AND' 'A~EKGRP' 'IN' 'S_EKGRP' INTO wa_cond SEPARATED BY space.
      APPEND wa_cond TO ta_cond.
      CLEAR: wa_cond.
    ENDIF.
*   Purchasing document date
    CONCATENATE 'AND' '(' 'A~BEDAT' 'GE' 'LV_SDAT'
                'AND' 'A~BEDAT' 'LE' 'LV_EDAT' ')' INTO wa_cond SEPARATED BY space.
    APPEND wa_cond TO ta_cond.
    CLEAR: wa_cond.
*   Deletion indicator in purchasing document
    CONCATENATE 'AND' 'B~LOEKZ' 'EQ' '''' '''' INTO wa_cond SEPARATED BY space.
    APPEND wa_cond TO ta_cond.
    CLEAR: wa_cond.
*   Material number
    CONCATENATE 'AND' 'B~MATNR' 'NE' '''' '''' INTO wa_cond SEPARATED BY space.
    APPEND wa_cond TO ta_cond.
    CLEAR: wa_cond.
*   Material number
    IF s_matnr IS NOT INITIAL.
      CONCATENATE 'AND' 'B~MATNR' 'IN' 'S_MATNR' INTO wa_cond SEPARATED BY space.
      APPEND wa_cond TO ta_cond.
      CLEAR: wa_cond.
    ENDIF.
*   Plant
    IF s_werks IS NOT INITIAL.
      CONCATENATE 'AND' 'B~WERKS' 'IN' 'S_WERKS' INTO wa_cond SEPARATED BY space.
      APPEND wa_cond TO ta_cond.
      CLEAR: wa_cond.
    ENDIF.
*   Material Type
    IF s_mtart IS NOT INITIAL.
      CONCATENATE 'AND' 'B~MTART' 'IN' 'S_MTART' INTO wa_cond SEPARATED BY space.
      APPEND wa_cond TO ta_cond.
      CLEAR: wa_cond.
    ENDIF.
  ENDIF.

ENDFORM.                    " BUILD_DYN_WHERE
