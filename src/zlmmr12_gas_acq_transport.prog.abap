*&---------------------------------------------------------------------*
*& Report  ZLMMR12_GAS_ACQ_TRANSPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zlmmr12_gas_acq_transport.

INCLUDE <icon>.

TABLES: ekko, ekpo, esll.

TYPES: BEGIN OF ty_output.
        INCLUDE STRUCTURE esll.
*         include structure konp.
TYPES:
USERF2_QTY type P LENGTH 13 DECIMALS 1,
ebeln TYPE ekko-ebeln ,
ebelp TYPE ekpo-ebelp ,
loekz TYPE ekpo-loekz ,
lifnr TYPE ekko-lifnr ,
land1 TYPE lfa1-land1 ,
name1 TYPE lfa1-name1 ,
ort01 TYPE lfa1-ort01 ,
mcod1 TYPE lfa1-mcod1 ,
konzs TYPE lfa1-konzs ,
ktokk TYPE lfa1-ktokk ,
bedat TYPE ekko-bedat ,
kdatb TYPE ekko-kdatb ,
   kdate TYPE ekko-kdate ,
ihran TYPE ekko-ihran ,
ekgrp TYPE ekko-ekgrp ,
aedat TYPE ekpo-aedat ,
txz01 TYPE ekpo-txz01 ,
*         packno TYPE ekpo-packno,
matnr TYPE ekpo-matnr,
ematn TYPE ekpo-ematn,
bukrs TYPE ekpo-bukrs,
werks TYPE ekpo-werks ,
lgort TYPE ekpo-lgort,
bednr TYPE ekpo-bednr,
*ekp_matkl TYPE ekpo-matkl ,
wgbez TYPE t023t-wgbez,
infnr TYPE ekpo-infnr,
idnlf TYPE ekpo-idnlf,
ktmng TYPE ekpo-ktmng,
ekp_menge TYPE ekpo-menge,
ekp_meins TYPE ekpo-meins,
bprme TYPE ekpo-bprme,
bpumz TYPE ekpo-bpumz,
bpumn TYPE ekpo-bpumn,
umrez TYPE ekpo-umrez,
umren TYPE ekpo-umren,
netpr TYPE ekpo-netpr,
ekp_peinh TYPE ekpo-peinh,
ekp_netwr TYPE ekpo-netwr,
ekp_brtwr TYPE ekpo-brtwr,
ekp_mwskz TYPE ekpo-mwskz,
ekp_uebto TYPE ekpo-uebto,
ekp_uebtk TYPE ekpo-uebtk,
untto TYPE ekpo-untto,
wepos TYPE ekpo-wepos,
weunb TYPE ekpo-weunb,
repos TYPE ekpo-repos,
webre TYPE ekpo-webre,
konnr TYPE ekpo-konnr,
ktpnr TYPE ekpo-ktpnr,
lmein TYPE ekpo-lmein,
evers TYPE ekpo-evers,
zwert TYPE ekpo-zwert,
prdat TYPE ekpo-prdat,
bstyp TYPE ekpo-bstyp,
effwr TYPE ekpo-effwr,
sobkz TYPE ekpo-sobkz,
meprf TYPE ekpo-meprf,
kappl TYPE a081-kappl,
kschl TYPE a081-kschl,
kont_pack TYPE a081-kont_pack,
kont_zeile TYPE a081-kont_zeile,
*         WERKS
datbi TYPE a081-datbi,
datab TYPE a081-datab,
knumh TYPE a081-knumh,
*         KNUMH type konp-knumh,
kopos TYPE konp-kopos,
*         KSCHL type konp-kschl,
krech TYPE konp-krech,
kbetr TYPE konp-kbetr,
konwa TYPE konp-konwa,
kpein TYPE konp-kpein,
kmein TYPE konp-kmein,
kumza TYPE konp-kumza,
kumne TYPE konp-kumne,
kmeins TYPE konp-meins,
loevm_ko TYPE konp-loevm_ko,
*         userf2_number TYPE esll-userf2_num ,
**         meins TYPE esll-meins ,
**         srvpos TYPE esll-srvpos ,
*         konwa TYPE konp-konwa ,
*         konp TYPE konp-kbetr ,
*         kpein TYPE konp-kpein ,
service_value TYPE p DECIMALS 2,
com_st_date TYPE sy-datum,
com_end_date TYPE sy-datum,
com_months TYPE vtbbewe-atage,
com_days TYPE vtbbewe-atage,
com_rept1 TYPE p LENGTH 14 DECIMALS 2,
com_rept2 TYPE p LENGTH 14 DECIMALS 2,
com_st_date_c TYPE sy-datum,
com_end_date_c TYPE sy-datum,
com_months_c TYPE vtbbewe-atage,
com_days_c TYPE vtbbewe-atage,
com_cur_yr1 TYPE p LENGTH 14 DECIMALS 2,
com_cur_yr2 TYPE p LENGTH 14 DECIMALS 2,
END OF ty_output.
DATA: gt_output TYPE TABLE OF ty_output,
      gs_output LIKE LINE OF gt_output,
      gt_ekko TYPE TABLE OF ekko,
      gs_ekko TYPE ekko,
      gt_ekpo TYPE TABLE OF ekpo,
      gs_ekpo TYPE ekpo,
      gt_esll TYPE TABLE OF esll,
      gs_esll TYPE esll,
      gt_esll_sub TYPE TABLE OF esll,
      gs_esll_sub TYPE esll,
      gt_konp TYPE TABLE OF konp,
      gs_konp TYPE konp,
      gt_a081 TYPE TABLE OF a081,
      gs_a081 TYPE a081,
      gs_lfa1 TYPE lfa1,
      gt_lfa1 TYPE TABLE OF lfa1,
      gt_t023t TYPE TABLE OF t023t,
      gs_t023t TYPE t023t,
      gt_comp TYPE TABLE OF ty_output WITH HEADER LINE,
      gt_comp_tab TYPE TABLE OF rstrucinfo WITH HEADER LINE.

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
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

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_kdatb  TYPE ekko-kdate OBLIGATORY,
            p_kdate  TYPE ekko-kdate OBLIGATORY,
            p_eff_dt TYPE ekko-kdate OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln,
                s_bstyp FOR ekko-bstyp,
                s_bsart FOR ekko-bsart,
                s_frgke FOR ekko-frgke,
                s_inco2 FOR ekko-inco2,
                s_waers FOR ekko-waers,
                s_ekorg FOR ekko-ekorg,
                s_ekgrp FOR ekko-ekgrp,
                s_lifnr FOR ekko-lifnr,
                s_aedat FOR ekko-aedat,
                s_bedat FOR ekko-bedat,
                s_ihran FOR ekko-ihran.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_werks  FOR ekpo-werks,
                s_matkl  FOR esll-matkl,
                s_txz01  FOR ekpo-txz01,
                s_aedati FOR ekpo-aedat.
PARAMETERS:  p_loekz AS CHECKBOX,
             p_del   AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_gtcs RADIOBUTTON GROUP grp1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(30) text-006.
SELECTION-SCREEN COMMENT 35(20) text-007.
PARAMETERS: p_dsc1 TYPE char096.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_gtcr RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 3(30) text-008.
SELECTION-SCREEN COMMENT 35(20) text-009.
PARAMETERS: p_dsc2 TYPE char096.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-005.
PARAMETERS: p_vari TYPE slis_vari DEFAULT '/DEFAULT' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b5.

START-OF-SELECTION.
*Get Internal table fields
  CALL FUNCTION 'GET_COMPONENT_LIST'
    EXPORTING
      program    = sy-repid
      fieldname  = 'GT_COMP'
    TABLES
      components = gt_comp_tab[].

  PERFORM get_data.
  PERFORM prepare_data.
  IF r_gtcr = 'X'.
    PERFORM determine_commitments.
  ENDIF.
  PERFORM display_alv.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Retrieve data for processing
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_konp TYPE TABLE OF konp,
        lt_esll TYPE TABLE OF esll,
        lv_packno TYPE esll-packno.

  CLEAR: gt_ekko,
         gt_ekpo,
         gt_esll,
         gt_konp,
         gt_a081,
         gs_lfa1,
         gt_esll_sub,
         gt_lfa1,
         gt_t023t.

  SELECT * FROM ekko INTO TABLE gt_ekko
    WHERE
**       ( ( kdatb <= p_kdatb and kdate >= p_kdatb ) OR
**         ( kdatb <= p_kdate and kdate >= p_kdate ) or
**         ( kdate >= p_kdatb and kdatb <= p_kdate ) ) and
        ebeln IN s_ebeln AND
        bstyp IN s_bstyp AND
        bsart IN s_bsart AND
        frgke IN s_frgke AND
        inco2 IN s_inco2 AND
        waers IN s_waers AND
        ekorg IN s_ekorg AND
        ekgrp IN s_ekgrp AND
        lifnr IN s_lifnr AND
        aedat IN s_aedat AND
        bedat IN s_bedat AND
        ihran IN s_ihran.

  DELETE gt_ekko WHERE kdatb > p_kdate OR
                            kdate < p_kdatb.
  IF gt_ekko IS INITIAL.
    WRITE: / 'No PO Data for output'.
    STOP.
  ENDIF.
  IF p_loekz IS NOT INITIAL.
    SELECT * FROM ekpo INTO TABLE gt_ekpo
      FOR ALL ENTRIES IN gt_ekko
      WHERE ebeln = gt_ekko-ebeln
        AND werks IN s_werks AND
            txz01 IN s_txz01 AND
*             loekz IN s_loekz AND
            aedat IN s_aedati. " AND
*             matkl IN s_matkl.
  ELSE.
    SELECT * FROM ekpo INTO TABLE gt_ekpo
      FOR ALL ENTRIES IN gt_ekko
      WHERE ebeln = gt_ekko-ebeln
        AND werks IN s_werks AND
            txz01 IN s_txz01 AND
*             loekz IN s_loekz AND
            loekz = space  AND
            aedat IN s_aedati. " AND
*             matkl IN s_matkl.
    PERFORM po_header_del.
  ENDIF.
  IF gt_ekpo IS INITIAL.
    WRITE: / 'No PO line item Data for output'.
    STOP.
  ENDIF.
**Matrial group
*  SELECT * INTO TABLE gt_t023t FROM t023t
*    FOR ALL ENTRIES IN gt_ekpo
*       WHERE spras = 'E'
*         AND matkl = gt_ekpo-matkl.
*Vendor name
  SELECT * INTO TABLE gt_lfa1 FROM lfa1
    FOR ALL ENTRIES IN gt_ekko
     WHERE lifnr = gt_ekko-lifnr.
*Retrieve Service lines for contracts
  SELECT * FROM esll INTO TABLE gt_esll
    FOR ALL ENTRIES IN gt_ekpo
    WHERE packno = gt_ekpo-packno.
*  LOOP AT gt_ekpo INTO gs_ekpo.
*    CLEAR lt_esll.
*    lv_packno = gs_ekpo-ebeln.
*    SELECT * FROM esll INTO TABLE lt_esll
*      WHERE packno = lv_packno.
*    CHECK sy-subrc = 0.
*    APPEND LINES OF lt_esll TO gt_esll.
*  ENDLOOP.
  IF gt_esll IS NOT INITIAL.
    IF p_del IS NOT INITIAL.
      SELECT * FROM esll INTO TABLE gt_esll_sub
        FOR ALL ENTRIES IN gt_esll
        WHERE packno = gt_esll-sub_packno
          AND matkl IN s_matkl.
    ELSE.
      SELECT * FROM esll INTO TABLE gt_esll_sub
          FOR ALL ENTRIES IN gt_esll
          WHERE packno = gt_esll-sub_packno
            AND matkl IN s_matkl
            AND del = space. "in s_Del.
    ENDIF.
  ENDIF.
  IF gt_esll_sub IS NOT INITIAL.
*Matrial group
    SELECT * INTO TABLE gt_t023t FROM t023t
      FOR ALL ENTRIES IN gt_esll_sub
         WHERE spras = 'E'
           AND matkl = gt_esll_sub-matkl.
*Retrieve condition records.
    SELECT * FROM a081 INTO TABLE gt_a081
      FOR ALL ENTRIES IN gt_esll_sub
      WHERE kont_pack = gt_esll_sub-packno
        AND kont_zeile = gt_esll_sub-introw
        AND ( datbi >= p_eff_dt AND datab <= p_eff_dt ).
    IF gt_a081 IS NOT INITIAL.
      SELECT * FROM konp INTO TABLE gt_konp
        FOR ALL ENTRIES IN gt_a081
        WHERE knumh = gt_a081-knumh
          AND ( kschl = 'PRS1' OR kschl = 'PRS2' OR
                kschl = 'PRS3' OR kschl = 'PRS' ) AND
              LOEVM_KO = space.
      lt_konp[] = gt_konp[].
      DELETE lt_konp WHERE kschl = 'PRS'.
      LOOP AT lt_konp INTO gs_konp.
        READ TABLE gt_konp WITH KEY knumh = gs_konp-knumh
                                    kschl = gs_konp-kschl
                                TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          DELETE gt_konp WHERE knumh = gs_konp-knumh
                           AND kschl = 'PRS'.
        ENDIF.
      ENDLOOP.
    ENDIF.                                                  "gt_a081
  ENDIF. " gt_esll_sub

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*       Prepare data for output
*----------------------------------------------------------------------*
FORM prepare_data .

  DATA: lv_tabix1 TYPE sy-tabix,
        lv_tabix2 TYPE sy-tabix,
        lv_tabix3 TYPE sy-tabix,
        lv_tabix4 TYPE sy-tabix,
        lv_tabix5 TYPE sy-tabix,
        lv_sub_packno TYPE esll-sub_packno,
        lv_append_check,
        lv_lifnr TYPE lfa1-lifnr.

  SORT gt_ekko BY ebeln.
  SORT gt_ekpo BY ebeln ebelp.
  SORT gt_esll BY packno.
  SORT gt_esll_sub BY packno.
  SORT gt_a081 BY kont_pack kont_zeile knumh.
  SORT gt_konp BY knumh kschl.
*PO Header
  LOOP AT gt_ekko INTO gs_ekko.
    CLEAR: gs_output,
           lv_append_check,
           gs_lfa1.
    gs_output-bedat = gs_ekko-bedat.
    gs_output-kdatb = gs_ekko-kdatb.
    gs_output-kdate = gs_ekko-kdate.
    gs_output-ihran = gs_ekko-ihran.
    gs_output-ebeln = gs_ekko-ebeln.
    gs_output-lifnr = gs_ekko-lifnr.
    gs_output-ekgrp = gs_ekko-ekgrp.
    READ TABLE gt_lfa1 INTO gs_lfa1
          WITH KEY lifnr = gs_output-lifnr.
    gs_output-name1 = gs_lfa1-name1.
    gs_output-land1 = gs_lfa1-land1.
    gs_output-ort01 = gs_lfa1-ort01.
    gs_output-mcod1 = gs_lfa1-mcod1.
    gs_output-konzs = gs_lfa1-konzs.
    gs_output-ktokk = gs_lfa1-ktokk.
    READ TABLE gt_ekpo WITH KEY
       ebeln = gs_ekko-ebeln TRANSPORTING NO FIELDS.
    lv_tabix1 = sy-tabix.
*PO line item
    LOOP AT gt_ekpo INTO gs_ekpo
         FROM lv_tabix1.
      IF gs_ekpo-ebeln <> gs_ekko-ebeln.
        EXIT.
      ENDIF.
*      MOVE-CORRESPONDING gs_ekpo TO gs_output.
      gs_output-ebelp = gs_ekpo-ebelp.
      gs_output-loekz = gs_ekpo-loekz.
      gs_output-aedat = gs_ekpo-aedat.
      gs_output-txz01 = gs_ekpo-txz01.
      gs_output-matnr = gs_ekpo-matnr.
      gs_output-ematn = gs_ekpo-ematn.
      gs_output-bukrs = gs_ekpo-bukrs.
      gs_output-werks = gs_ekpo-werks.
      gs_output-lgort = gs_ekpo-lgort.
      gs_output-bednr = gs_ekpo-bednr.
      gs_output-infnr = gs_ekpo-infnr.
      gs_output-idnlf = gs_ekpo-idnlf.
      gs_output-ktmng = gs_ekpo-ktmng.
      gs_output-bprme = gs_ekpo-bprme.
      gs_output-bpumz = gs_ekpo-bpumz.
      gs_output-bpumn = gs_ekpo-bpumn.
      gs_output-umrez = gs_ekpo-umrez.
      gs_output-umren = gs_ekpo-umren.
      gs_output-netpr = gs_ekpo-netpr.
      gs_output-untto = gs_ekpo-untto.
      gs_output-wepos = gs_ekpo-wepos.
      gs_output-weunb = gs_ekpo-weunb.
      gs_output-repos = gs_ekpo-repos.
      gs_output-webre = gs_ekpo-webre.
      gs_output-konnr = gs_ekpo-konnr.
      gs_output-ktpnr = gs_ekpo-ktpnr.
      gs_output-lmein = gs_ekpo-lmein.
      gs_output-evers = gs_ekpo-evers.
      gs_output-zwert = gs_ekpo-zwert.
      gs_output-prdat = gs_ekpo-prdat.
      gs_output-bstyp = gs_ekpo-bstyp.
      gs_output-effwr = gs_ekpo-effwr.
      gs_output-sobkz = gs_ekpo-sobkz.
      gs_output-meprf = gs_ekpo-meprf.
*      gs_output-ekp_matkl = gs_ekpo-matkl.
      gs_output-ekp_menge = gs_ekpo-menge.
      gs_output-ekp_meins = gs_ekpo-meins.
      gs_output-ekp_peinh = gs_ekpo-peinh.
      gs_output-ekp_netwr = gs_ekpo-netwr.
      gs_output-ekp_brtwr = gs_ekpo-brtwr.
      gs_output-ekp_mwskz = gs_ekpo-mwskz.
      gs_output-ekp_uebto = gs_ekpo-uebto.
      gs_output-ekp_uebtk = gs_ekpo-uebtk.
*      CLEAR: gs_t023t.
**Material group description
*      READ TABLE gt_t023t INTO gs_t023t
*            WITH KEY matkl = gs_output-ekp_matkl.
*      gs_output-wgbez = gs_t023t-wgbez.
****
      READ TABLE gt_esll WITH KEY packno = gs_ekpo-packno
        TRANSPORTING NO FIELDS.
      lv_tabix2 = sy-tabix.
*Line of service
      LOOP AT gt_esll INTO gs_esll FROM lv_tabix2.
        IF gs_esll-packno <> gs_ekpo-packno.
          EXIT.
        ENDIF.
        lv_sub_packno = gs_esll-sub_packno.
        gs_output-sub_packno = gs_esll-sub_packno.
        READ TABLE gt_esll_sub WITH KEY packno = lv_sub_packno
        TRANSPORTING NO FIELDS.
        lv_tabix3 = sy-tabix.
*Line of service
        LOOP AT gt_esll_sub INTO gs_esll_sub FROM lv_tabix3.
          IF gs_esll_sub-packno <> lv_sub_packno.
            EXIT.
          ENDIF.
          MOVE-CORRESPONDING gs_esll_sub TO gs_output.
          gs_output-USERF2_QTY = gs_output-USERF2_NUM.
*Material group description
          CLEAR: gs_t023t.
          READ TABLE gt_t023t INTO gs_t023t
                WITH KEY matkl = gs_output-matkl.
          gs_output-wgbez = gs_t023t-wgbez.
          READ TABLE gt_a081 WITH KEY
                             kont_pack = gs_esll_sub-packno
                             kont_zeile = gs_esll_sub-introw
                             TRANSPORTING NO FIELDS.
          lv_tabix4 = sy-tabix.
*Contract conditions
          LOOP AT gt_a081 INTO gs_a081 FROM lv_tabix4.
            IF gs_a081-kont_pack <> gs_esll_sub-packno OR
               gs_a081-kont_zeile <> gs_esll_sub-introw.
              EXIT.
            ENDIF.
            gs_output-knumh = gs_a081-knumh.
            READ TABLE gt_konp WITH KEY knumh = gs_a081-knumh
                                        TRANSPORTING NO FIELDS.
            lv_tabix5 = sy-tabix.
*Condition
            LOOP AT gt_konp INTO gs_konp FROM lv_tabix5.
              IF gs_konp-knumh <> gs_a081-knumh.
                EXIT.
              ENDIF.
              lv_lifnr = gs_output-lifnr.
              MOVE-CORRESPONDING gs_konp TO gs_output.
              gs_output-lifnr = lv_lifnr.
              gs_output-kmeins = gs_konp-meins.
              gs_output-service_value = 0.
              IF gs_konp-kpein <> 0.
                gs_output-service_value =
                ( gs_esll_sub-userf2_num * gs_konp-kbetr )
                / gs_konp-kpein.
              ENDIF.
              APPEND gs_output TO gt_output.
              lv_append_check = 'X'.
            ENDLOOP. "KONP
            IF lv_append_check IS INITIAL.
              APPEND gs_output TO gt_output.
              lv_append_check = 'X'.
            ENDIF.
          ENDLOOP.                                          " A081
          IF lv_append_check IS INITIAL.
            APPEND gs_output TO gt_output.
            lv_append_check = 'X'.
          ENDIF.
        ENDLOOP. "ESLL_SUB
*        IF lv_append_check IS INITIAL.
*          gs_output-mandt = gs_esll-mandt.
*          gs_output-matkl = gs_esll-matkl.
*          APPEND gs_output TO gt_output.
*          lv_append_check = 'X'.
*        ENDIF.
      ENDLOOP. "ESLL
*      IF lv_append_check IS INITIAL.
*        APPEND gs_output TO gt_output.
*        lv_append_check = 'X'.
*      ENDIF.
    ENDLOOP. "EKPO
*    IF lv_append_check IS INITIAL.
*      APPEND gs_output TO gt_output.
*      lv_append_check = 'X'.
*    ENDIF.
  ENDLOOP. "Ekko
ENDFORM.                    " PREPARE_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
FORM display_alv .
  DATA: lr_table TYPE REF TO cl_salv_table,
        lr_events_salv TYPE REF TO cl_salv_events_table,
        lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column TYPE REF TO cl_salv_column_table,
        lr_event TYPE REF TO lcl_event_handler,
        lr_layout TYPE REF TO cl_salv_layout,
        ls_key TYPE salv_s_layout_key,
        lr_functions TYPE REF TO cl_salv_functions,
        lr_display TYPE REF TO cl_salv_display_settings,
        lv_function TYPE salv_de_function,
        lv_pos TYPE salv_de_function_pos,
        lr_functions_list TYPE REF TO cl_salv_functions_list,
        lv_text TYPE string,
        lv_icon TYPE string,
        lv_msg TYPE lvc_title,
        lr_title TYPE REF TO cl_salv_form_element.

  DATA: lr_content TYPE REF TO cl_salv_form_element.
  DATA: lr_grid     TYPE REF TO cl_salv_form_layout_grid,
        lv_label      TYPE string,
        lv_date(10) TYPE c,
        lv_date2(10) TYPE c.

  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*          r_container    = lr_con1
*          container_name = 'ALV_CON1'
        IMPORTING
          r_salv_table   = lr_table
        CHANGING
          t_table        = gt_output.
    CATCH cx_salv_msg .
  ENDTRY.
*Function settings
  lr_functions = lr_table->get_functions( ).
  lr_functions->set_all( abap_true ).
*Display Setting
  lr_display = lr_table->get_display_settings( ).

  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
*  CONCATENATE 'Client ' sy-sysid sy-MANDT sy-datum sy-UZEIT
*                            INTO lv_msg separated by space.
*  lr_display->set_list_header( lv_msg ).
*Event
  lr_events_salv = lr_table->get_event( ).
  CREATE OBJECT lr_event.
  SET HANDLER: lr_event->hotspot_click1
               FOR lr_events_salv.
*Set layout
  lr_layout = lr_table->get_layout( ).
  ls_key-report = sy-repid.
  lr_layout->set_key( ls_key ).
  lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  CALL METHOD lr_layout->set_initial_layout
    EXPORTING
      value = p_vari.
*Get columns
  CALL METHOD lr_table->get_columns
    RECEIVING
      value = lr_columns.
******Change ALV Fields  - title etc.
  PERFORM alv_fields USING lr_columns lr_column.
******Set ALV Header
*... Create top_of_list contents.
  CREATE OBJECT lr_grid.
*  Lr_grid->create_text(
*    row    = 1
*    column = 1
*    text   = sy-title ).
  IF p_dsc1 IS INITIAL AND
     p_dsc2 IS INITIAL.
    lr_grid->create_label(
       row    = 1
       column = 1
       text   = sy-title ).
  ELSE.
    lr_grid->create_label(
          row    = 1
          column = 1
          text   = p_dsc1 ).
    lr_grid->create_label(
      row    = 2
      column = 1
      text   = p_dsc2 ).
  ENDIF.
  WRITE p_kdatb TO  lv_date.
  WRITE p_kdate TO  lv_date2.
  CONCATENATE 'Validity Period Start / End: ' lv_date '--' lv_date2
              INTO lv_label SEPARATED BY space.
  lr_grid->create_label(
   row    = 3
   column = 1
   text   = lv_label ).
*  WRITE p_kdate TO  lv_date.
*  CONCATENATE 'Validity Period End' lv_date
*              INTO lv_label SEPARATED BY space.
*  lr_grid->create_label(
*   row    = 3
*   column = 1
*   text   = lv_label ).
  WRITE p_eff_dt TO lv_date.
  CONCATENATE 'Effective Date for Pricing: ' lv_date
              INTO lv_label SEPARATED BY space.
  lr_grid->create_label(
   row    = 4
   column = 1
   text   = lv_label ).
  WRITE sy-datum TO lv_date.
  CONCATENATE 'Client ' sy-sysid sy-mandt lv_date
              sy-uzeit(2) ':' sy-uzeit+2(2)
              INTO lv_label SEPARATED BY space.

  lr_grid->create_label(
   row    = 5
   column = 1
   text   = lv_label ).

  lr_content = lr_grid.
  lr_table->set_top_of_list( lr_content ).
******Display ALV
  CALL METHOD lr_table->display.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  handle_click1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW      text
*      -->P_COLUMN   text
*----------------------------------------------------------------------*
FORM handle_click1  USING    p_row TYPE salv_de_row
                             p_column TYPE salv_de_column.
*Read the talbe with row-id .
  DATA: ls_output LIKE LINE OF gt_output,
        lv_answer,
        lv_msg type c length 100.

  READ TABLE gt_output INTO ls_output INDEX p_row.
  CONCATENATE 'Do you want to Display or Change'
        ls_output-ebeln INTO lv_msg SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar                    = 'Transaction Process Mode'
*     DIAGNOSE_OBJECT             = ' '
      text_question               = lv_msg
      text_button_1               = 'Display'
*     ICON_BUTTON_1               = ' '
      text_button_2               = 'Change'
*     ICON_BUTTON_2               = ' '
*     DEFAULT_BUTTON              = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
   IMPORTING
     answer                      = lv_answer
*   TABLES
*     PARAMETER                   =
   EXCEPTIONS
     text_not_found              = 1
     OTHERS                      = 2 .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CHECK lv_answer <> 'A'.
  READ TABLE gt_output INTO ls_output INDEX p_row.
  CASE p_column.
    WHEN 'EBELN'.
      IF ls_output-bstyp = 'K'. "Contract
        SET PARAMETER ID 'CTR' FIELD ls_output-ebeln.
        IF lv_answer = '2'.
          CALL TRANSACTION 'ME32K' AND SKIP FIRST SCREEN.
        ELSE.
          CALL TRANSACTION 'ME33K' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
      IF ls_output-bstyp = 'L'. "Sch. Agreement
        SET PARAMETER ID 'CTR' FIELD ls_output-ebeln.
        IF lv_answer = '2'.
          CALL TRANSACTION 'ME32L' AND SKIP FIRST SCREEN.
        ELSE.
          CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
      IF ls_output-bstyp = 'F'. "PO
        SET PARAMETER ID 'BES' FIELD ls_output-ebeln.
        IF lv_answer = '2'.
          CALL TRANSACTION 'ME22N' AND SKIP FIRST SCREEN.
        ELSE.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
      IF ls_output-bstyp = 'A'. " RFQ
        SET PARAMETER ID 'ANF' FIELD ls_output-ebeln.
        IF lv_answer = '2'.
          CALL TRANSACTION 'ME42' AND SKIP FIRST SCREEN.
        ELSE.
          CALL TRANSACTION 'ME43' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    " HANDLE_CLICK1
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDS
*&---------------------------------------------------------------------*
*       Change ALV Fields (if require)
*----------------------------------------------------------------------*
*      -->P_LR_COLUMNS  text
*      -->P_LR_COLUMN  text
*----------------------------------------------------------------------*
FORM alv_fields  USING    p_lr_columns TYPE REF TO
                                       cl_salv_columns_table
                          p_lr_column  TYPE REF TO
                                       cl_salv_column_table.

   DATA: lv_column TYPE lvc_fname,
        lv_long_text TYPE scrtext_l,
        lv_short_text TYPE scrtext_s,
        lv_med_text TYPE scrtext_m,
        lv_hide TYPE c,
        lv_decimal type c,
        lv_no_dec type LVC_DECMLS.

  TRY.
      p_lr_column ?= p_lr_columns->get_column( 'EBELN' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD p_lr_column->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.


  LOOP AT gt_comp_tab.
    CLEAR: lv_long_text,
           lv_short_text,
           lv_med_text,
           lv_hide,
           lv_decimal,
           lv_no_dec.
    lv_column = gt_comp_tab-compname.

    IF lv_column = 'LOEVM_KO'.
      lv_long_text = 'Conditions (Pricing) Deletion Indictor'.
      lv_short_text = 'Cond Del Ind.'.
    ENDIF.
    IF lv_column = 'DEL'.
      lv_long_text = 'Lines Serv Deletion Indictor'.
      lv_short_text = 'Lines Srv Del'.
    ENDIF.
    IF lv_column = 'LOEKZ'.
      lv_long_text = 'PO Line Deletion Indictor'.
      lv_short_text = 'POL Deletion'.
    ENDIF.
    IF lv_column = 'BEDAT'.
      lv_long_text = 'Contract Date'.
      lv_short_text = 'Cont Date'.
    ENDIF.
    IF lv_column = 'KDATB'.
      lv_long_text = 'Service Start Date'.
      lv_short_text = 'Srv St Date'.
    ENDIF.
    IF lv_column = 'KDATE'.
      lv_long_text = 'Contract End Date'.
      lv_short_text = 'Cont End Date'.
    ENDIF.
    IF lv_column = 'IHRAN'.
      lv_long_text = 'Renewal Decision Date'.
      lv_short_text = 'Ren Dec.Date'.
    ENDIF.
    IF lv_column = 'TXZ01'.
      lv_long_text = 'Delivery Service'.
      lv_short_text = 'Delv Srv'.
*
    ENDIF.
    IF lv_column = 'USERF2_QTY'.
      lv_long_text = 'Quantity (UserF2)'.
      lv_short_text = 'Quantity'.
      lv_med_text = 'Quantity'.
    ENDIF.
    IF lv_column = 'USERF2_NUM'.
         lv_hide = 'X'.
    ENDIF.
    IF lv_column = 'KBETR'.
      lv_long_text = 'Gross Price'.
      lv_short_text = 'Price'.
      lv_med_text = 'Gross Price'.
    ENDIF.
    IF lv_column = 'MCOD1'.
      lv_long_text = 'Search Term'.
      lv_short_text = 'Search Term'.
    ENDIF.
    IF lv_column = 'NAME1'.
      lv_long_text = 'Vendor Name'.
      lv_short_text = 'Vendor Name'.
    ENDIF.
    IF lv_column = 'SERVICE_VALUE'.
      lv_long_text = 'Service Value'.
      lv_short_text = 'Service Value'.
    ENDIF.
    IF lv_column = 'COM_ST_DATE'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_long_text = 'Commitment Start Date'.
        lv_short_text = 'Com.St.Date'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_END_DATE'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_long_text = 'Commitment End Date'.
        lv_short_text = 'Com.End Date'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_MONTHS'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_long_text = 'Commitment Months'.
        lv_short_text = 'Comm Months'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_DAYS'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_long_text = 'Commitment Days'.
        lv_short_text = 'Comm Days'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_REPT1'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_long_text = 'Demand Service'.
        lv_short_text = 'Demand Serv.'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_REPT2'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_long_text = 'Commodity Service'.
        lv_short_text = 'Commod. Serv.'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_ST_DATE_C'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_med_text = 'CYr Com.S.Date'.
        lv_long_text = 'Cur. Year Com St. Date'.
        lv_short_text = 'CYr C.SDate'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_END_DATE_C'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_med_text = 'CYr Com.E.Date'.
        lv_long_text = 'Cur. Year Com End Date'.
        lv_short_text = 'CYr C.EDate'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_MONTHS_C'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_med_text = 'CYr Months'.
        lv_long_text = 'Cur. Year Months'.
        lv_short_text = 'CYr Months'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_DAYS_C'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_med_text = 'CYr Days'.
        lv_long_text = 'Cur. Year Days'.
        lv_short_text = 'CYr Days'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_CUR_YR1'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_med_text = 'CYr Dem.Serv'.
        lv_long_text = 'Cur. Year Demand Service'.
        lv_short_text = 'CYr Serv'.
      ENDIF.
    ENDIF.
    IF lv_column = 'COM_CUR_YR2'.
      IF r_gtcr IS INITIAL.
        lv_hide = 'X'.
      ELSE.
        lv_med_text = 'CYr Comd.Serv.'.
        lv_long_text = 'Cur. Year Commodity Service'.
        lv_short_text = 'CYr Comm.Serv.'.
      ENDIF.
    ENDIF.
    IF lv_long_text IS NOT INITIAL.
*    Set the column Header
      TRY.
          p_lr_column ?= p_lr_columns->get_column( lv_column ).
        CATCH cx_salv_not_found .
      ENDTRY.
      CALL METHOD p_lr_column->set_long_text
        EXPORTING
          value = lv_long_text.

      CALL METHOD p_lr_column->set_short_text
        EXPORTING
          value = lv_short_text.

      IF lv_med_text IS NOT INITIAL.
        CALL METHOD p_lr_column->set_medium_text
          EXPORTING
            value = lv_med_text.
      ENDIF.
    ENDIF.
    IF lv_hide IS NOT INITIAL.
      TRY.
          p_lr_column ?= p_lr_columns->get_column( lv_column ).
        CATCH cx_salv_not_found.
      ENDTRY.
      CALL METHOD p_lr_column->set_visible
        EXPORTING
          value = if_salv_c_bool_sap=>false.
    ENDIF.
 ENDLOOP.

ENDFORM.                    " ALV_FIELDS
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_COMMITMENTS
*&---------------------------------------------------------------------*
*       Determine Commitments
*----------------------------------------------------------------------*
FORM determine_commitments .

  DATA: lv_sdat TYPE sy-datum,
        lv_edat TYPE sy-datum.

  CONCATENATE sy-datum(4) '0101' INTO lv_sdat.
  CONCATENATE sy-datum(4) '1231' INTO lv_edat.
  LOOP AT gt_output INTO gs_output.
    IF p_kdatb >= gs_output-kdatb.
      gs_output-com_st_date = p_kdatb.
    ELSE.
      gs_output-com_st_date = gs_output-kdatb.
    ENDIF.
    IF p_kdate > gs_output-kdate.
      gs_output-com_end_date = gs_output-kdate.
    ELSE.
      gs_output-com_end_date = p_kdate.
    ENDIF.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from          = gs_output-com_st_date
*        I_KEY_DAY_FROM       =
        i_date_to            = gs_output-com_end_date
*        I_KEY_DAY_TO         =
*        I_FLG_SEPARATE       = ' '
     IMPORTING
       e_days               = gs_output-com_days
       e_months             = gs_output-com_months
*        E_YEARS              =
              .
    IF gs_output-matkl = '2001' OR
       gs_output-matkl = '2007'.
      gs_output-com_rept1 = gs_output-service_value *
                            gs_output-com_months.
    ELSEIF gs_output-matkl = '2002'.
      gs_output-com_rept2 = gs_output-service_value *
                            gs_output-com_days.
    ENDIF.
    IF gs_output-com_st_date > lv_sdat.
      gs_output-com_st_date_c = gs_output-com_st_date.
    ELSE.
      gs_output-com_st_date_c = lv_sdat.
    ENDIF.
*    gs_output-com_st_date_c = p_kdatb.
    IF gs_output-com_end_date > lv_edat.
      gs_output-com_end_date_c = lv_edat.
    ELSE.
      gs_output-com_end_date_c = gs_output-com_end_date.
    ENDIF.
*    if p_kdate <= gs_output-kdate.
*      gs_output-com_end_date_c = p_kdate.
*    else.
*      gs_output-com_end_date_c = gs_output-kdate.
*    endif.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
     EXPORTING
       i_date_from          = gs_output-com_st_date_c
*        I_KEY_DAY_FROM       =
       i_date_to            = gs_output-com_end_date_c
*        I_KEY_DAY_TO         =
*        I_FLG_SEPARATE       = ' '
    IMPORTING
      e_days               = gs_output-com_days_c
      e_months             = gs_output-com_months_c
*        E_YEARS              =
             .
    IF gs_output-matkl = '2001' OR
       gs_output-matkl = '2007'.
      gs_output-com_cur_yr1 = gs_output-service_value *
                              gs_output-com_months_c.
    ELSEIF gs_output-matkl = '2002'.
      gs_output-com_cur_yr2 = gs_output-service_value *
                            gs_output-com_days_c.
    ENDIF.
    MODIFY gt_output FROM gs_output TRANSPORTING com_st_date
                                                com_end_date
                                                com_months
                                                com_days
                                                com_rept1
                                                com_rept2
                                                com_st_date_c
                                                com_end_date_c
                                                com_months_c
                                                com_days_c
                                                com_cur_yr1
                                                com_cur_yr2.
  ENDLOOP.

ENDFORM.                    " DETERMINE_COMMITMENTS
*&---------------------------------------------------------------------*
*&      Form  PO_HEADER_DEL
*&---------------------------------------------------------------------*
*       Delete PO header
*----------------------------------------------------------------------*
FORM po_header_del .

  LOOP AT gt_ekko INTO gs_ekko.
    READ TABLE gt_ekpo WITH KEY ebeln = gs_ekko-ebeln
                               TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    DELETE gt_ekko WHERE ebeln = gs_ekko-ebeln.
  ENDLOOP.

ENDFORM.                    " PO_HEADER_DEL
