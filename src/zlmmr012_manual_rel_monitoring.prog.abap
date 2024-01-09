*&---------------------------------------------------------------------*
*& Report  ZLMMR012_MANUAL_REL_MONITORING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zlmmr012_manual_rel_monitoring MESSAGE-ID z_scm.


TABLES: ekko, rbkp.

TYPES: BEGIN OF ty_out,
        belnr LIKE rseg-belnr,
        buzei LIKE rseg-buzei,
        gjahr LIKE rseg-gjahr,
        blart LIKE rbkp-blart,
        ebeln LIKE ekpo-ebeln,
        ebelp LIKE ekpo-ebelp,
        bsart LIKE ekko-bsart,
        budat TYPE char10,
        cpudt TYPE char10,
        zfbdt TYPE char10,
        gbudat TYPE char10,
        bukrs LIKE rseg-bukrs,
        wrbtr LIKE rseg-wrbtr,
        waers LIKE rbkp-waers,
        menge LIKE ekpo-menge,
        dmenge LIKE ekpo-menge,
        bdate TYPE char10,
        reason(10) TYPE c,
        user LIKE cdhdr-username,
        udate TYPE char10,
       END OF ty_out.

DATA: t_out TYPE TABLE OF ty_out WITH HEADER LINE,
      wa_out TYPE ty_out,
      w_num TYPE i.

DATA: BEGIN OF t_rbkp OCCURS 0,
      belnr LIKE rbkp-belnr,
      gjahr LIKE rbkp-gjahr,
      blart LIKE rbkp-blart,
      budat LIKE rbkp-budat,
      cpudt LIKE rbkp-cpudt,
      waers LIKE rbkp-waers,
      zfbdt LIKE rbkp-zfbdt,
      END OF t_rbkp.

DATA: BEGIN OF t_rseg OCCURS 0,
      belnr LIKE rseg-belnr,
      gjahr LIKE rseg-gjahr,
      buzei LIKE rseg-buzei,
      ebeln LIKE rseg-ebeln,
      ebelp LIKE rseg-ebelp,
      bukrs LIKE rseg-bukrs,
      wrbtr LIKE rseg-wrbtr,
      END OF t_rseg.

DATA: BEGIN OF t_ekko OCCURS 0,
      ebeln LIKE ekko-ebeln,
      bsart LIKE ekko-bsart,
      END OF t_ekko.

DATA: BEGIN OF t_ekpo OCCURS 0,
      ebeln LIKE ekpo-ebeln,
      ebelp LIKE ekpo-ebelp,
      menge LIKE ekpo-menge,
      END OF t_ekpo.

DATA: BEGIN OF t_ekbe OCCURS 0,
      ebeln LIKE ekbe-ebeln,
      ebelp LIKE ekbe-ebelp,
      gjahr TYPE ekbe-gjahr,    "BGANAPATHIRA 11/20/2014
      belnr TYPE ekbe-belnr,    "BGANAPATHIRA 11/20/2014
      buzei TYPE ekbe-buzei,    "BGANAPATHIRA 11/20/2014
      bwart LIKE ekbe-bwart,
      budat LIKE ekbe-budat,
      menge LIKE ekbe-menge,
      END OF t_ekbe.

DATA: w_objid(14) TYPE c.

DATA: BEGIN OF t_cdhdr OCCURS 0,
      objectid LIKE cdhdr-objectid,
      changenr LIKE cdhdr-changenr,
      username LIKE cdhdr-username,
      udate LIKE cdhdr-udate,
      END OF t_cdhdr.

DATA: BEGIN OF t_cdpos OCCURS 0,
      objectid LIKE cdpos-objectid,
      changenr LIKE cdpos-changenr,
      tabname LIKE cdpos-tabname,
      fname  LIKE cdpos-fname,
      chngind   LIKE cdpos-chngind,
      value_new  LIKE cdpos-value_new,
      value_old  LIKE cdpos-value_old,
      END OF t_cdpos.

* Data for ALV
TYPE-POOLS: slis.
DATA: t_fc   TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     t_sort TYPE slis_t_sortinfo_alv.
DATA: z_variant LIKE disvariant.
DATA: z_varid LIKE disvariant-variant VALUE '/DEFAULT'.
DATA: g_variant_flag.
DATA: g_selmod.

DATA: z_layout TYPE slis_layout_alv.
DATA: t_header   TYPE slis_t_listheader.
DATA: z_text(60).
DATA: gs_line TYPE slis_listheader.

SELECTION-SCREEN BEGIN OF BLOCK rma WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln,
                s_belnr FOR rbkp-belnr,
                s_gjahr FOR rbkp-gjahr,
                s_budat FOR rbkp-budat,
                s_bukrs FOR rbkp-bukrs.
SELECTION-SCREEN END OF BLOCK rma.

AT SELECTION-SCREEN.
  IF s_ebeln[] IS INITIAL AND s_belnr[] IS INITIAL AND
     s_gjahr[] IS INITIAL AND s_budat[] IS INITIAL AND
     s_bukrs[] IS INITIAL.
    MESSAGE e100 WITH 'Specify At Least One Selection Criteria'.
  ENDIF.

START-OF-SELECTION.

  IF s_belnr[] IS INITIAL AND s_bukrs[] IS INITIAL AND
     s_gjahr[] IS INITIAL AND s_budat[] IS INITIAL.
    PERFORM get_data_from_rseg.
  ELSE.
    PERFORM get_data_from_rbkp.
  ENDIF.

END-OF-SELECTION.
  PERFORM prepare_output.
  PERFORM output_alv_report.
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM output_alv_report .
  PERFORM build_fc.
  PERFORM build_header.

  CLEAR z_layout.
  z_layout-info_fieldname    = 'LINECOLOR'.
  z_layout-colwidth_optimize = 'X'.
  z_layout-detail_popup = 'X'.
  z_layout-numc_sum = 'X'.
  z_layout-get_selinfos = 'X'.
  z_layout-confirmation_prompt = 'X'.
  z_layout-box_rollname = 'S'.

* variant settings
  z_variant-report = sy-repid.
  z_variant-variant = z_varid.

* call ALV function to output report

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_top_of_page  = 'TOP_OF_PAGE'
      i_callback_user_command = 'USER_COMMAND'
      is_layout               = z_layout
      it_fieldcat             = t_fc[]
      i_default               = 'X'
      i_save                  = 'X'
      is_variant              = z_variant
    TABLES
      t_outtab                = t_out.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " OUTPUT_ALV_REPORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fc .
  DATA: w_ind TYPE i.

  w_ind = 0.

  t_fc-fieldname   = 'BELNR'.
  t_fc-seltext_m   = 'Invoice Doc'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'BUZEI'.
  t_fc-seltext_m   = 'Inv Item'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'GJAHR'.
  t_fc-seltext_m   = 'Year'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'BLART'.
  t_fc-seltext_m   = 'Inv Doc Type'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'EBELN'.
  t_fc-seltext_m   = 'PO Document'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'EBELP'.
  t_fc-seltext_m   = 'PO Item'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'BSART'.
  t_fc-seltext_m   = 'PO Doc Type'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'BUDAT'.
  t_fc-seltext_m   = 'Inv Posting Date'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'CPUDT'.
  t_fc-seltext_m   = 'Inv Entry Date'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'ZFBDT'.
  t_fc-seltext_m   = 'Baseline Date'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'GBUDAT'.
  t_fc-seltext_m   = 'GR Posting Date'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'BUKRS'.
  t_fc-seltext_m   = 'Co Code'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'WRBTR'.
  t_fc-seltext_m   = 'Inv Amount'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'WAERS'.
  t_fc-seltext_m   = 'Currency'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'MENGE'.
  t_fc-seltext_m   = 'PO Quantity'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'DMENGE'.
  t_fc-seltext_m   = 'To Be Delivered'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'BDATE'.
  t_fc-seltext_m   = 'Inv Block Date'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'REASON'.
  t_fc-seltext_m   = 'Blocking Reason'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'USER'.
  t_fc-seltext_m   = 'Released By'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'UDATE'.
  t_fc-seltext_m   = 'Release Date'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.
ENDFORM.                    " BUILD_FC
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.
ENDFORM.                    "TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_header .
  DATA: c_num(3) TYPE c.

  c_num = w_num.

  CLEAR gs_line.
  gs_line-typ  = 'H'.
  gs_line-info = 'Historical Blocked Invoices Report'.
  APPEND gs_line TO t_header.

  CONCATENATE 'Number of Records' c_num INTO z_text SEPARATED BY space.

  CLEAR gs_line.
  gs_line-typ  = 'S'.
  gs_line-info = z_text.
  APPEND gs_line TO t_header.
  CLEAR z_text.
ENDFORM.                    " BUILD_HEADER

************************************************************************
* User commands
************************************************************************
FORM user_command USING rf_ucomm LIKE sy-ucomm
                       rs       TYPE slis_selfield.

* User command
  IF rf_ucomm = '&IC1'.
    CASE rs-fieldname.
      WHEN 'BELNR'.
        IF rs-tabindex GE 1.
          READ TABLE t_out INTO wa_out INDEX rs-tabindex.
          SET PARAMETER ID 'RBN' FIELD rs-value.
          SET PARAMETER ID 'GJR' FIELD wa_out-gjahr.
          CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN .
        ENDIF.
      WHEN OTHERS.
        MESSAGE e100 WITH 'Please Click on Invoice Document'.
    ENDCASE.
  ENDIF.
  CLEAR rf_ucomm.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_RSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_rseg .

  SELECT belnr gjahr buzei ebeln ebelp bukrs wrbtr FROM rseg INTO TABLE t_rseg
  WHERE belnr IN s_belnr AND
        gjahr IN s_gjahr AND
        ebeln IN s_ebeln AND
        bukrs IN s_bukrs.
  IF sy-subrc = 0.
    SELECT belnr gjahr blart budat cpudt waers zfbdt FROM rbkp INTO TABLE t_rbkp
      FOR ALL ENTRIES IN t_rseg
      WHERE belnr = t_rseg-belnr AND
            gjahr = t_rseg-gjahr AND
            budat IN s_budat.
    IF sy-subrc = 0.
      SELECT ebeln bsart FROM ekko INTO TABLE t_ekko
        FOR ALL ENTRIES IN t_rseg
        WHERE ebeln = t_rseg-ebeln.

      SELECT ebeln ebelp menge FROM ekpo INTO TABLE t_ekpo
        FOR ALL ENTRIES IN t_rseg
        WHERE ebeln = t_rseg-ebeln.

      SELECT ebeln ebelp
        gjahr belnr buzei           "BGANAPATHIRA SDP 76540
        bwart budat menge FROM ekbe INTO TABLE t_ekbe
        FOR ALL ENTRIES IN t_rseg
        WHERE ebeln = t_rseg-ebeln
          AND vgabe = '1'.            "(+)SDP 76540 BGANAPATHIRA..
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_DATA_FROM_RSEG
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_RBKP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_rbkp .

  SELECT belnr gjahr blart budat cpudt waers zfbdt FROM rbkp INTO TABLE t_rbkp
    WHERE belnr IN s_belnr AND
          gjahr IN s_gjahr AND
          budat IN s_budat AND
          bukrs IN s_bukrs.

  IF sy-subrc = 0.
    SELECT belnr gjahr buzei ebeln ebelp bukrs wrbtr FROM rseg INTO TABLE t_rseg
       FOR ALL ENTRIES IN t_rbkp
      WHERE belnr = t_rbkp-belnr AND
            gjahr = t_rbkp-gjahr AND
            ebeln IN s_ebeln.
    IF sy-subrc = 0.
      SELECT ebeln bsart FROM ekko INTO TABLE t_ekko
        FOR ALL ENTRIES IN t_rseg
        WHERE ebeln = t_rseg-ebeln.

      SELECT ebeln ebelp menge FROM ekpo INTO TABLE t_ekpo
        FOR ALL ENTRIES IN t_rseg
        WHERE ebeln = t_rseg-ebeln.

      SELECT ebeln ebelp
        gjahr belnr buzei           "BGANAPATHIRA SDP 76540
        bwart budat menge FROM ekbe INTO TABLE t_ekbe
        FOR ALL ENTRIES IN t_rseg
        WHERE ebeln = t_rseg-ebeln
          AND vgabe = '1'.            "(+)SDP 76540 BGANAPATHIRA.
    ENDIF.
  ENDIF.


ENDFORM.                    " GET_DATA_FROM_RBKP
*&---------------------------------------------------------------------*
*&      Form  PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_output .
  DATA: w_date TYPE datum.
  DATA: lwa_ekbe LIKE LINE OF t_ekbe,     "(+)SDP 76540 BGANAPATHIRA
        lv_index TYPE sy-tabix.           "(+)SDP 76540 BGANAPATHIRA


  SORT t_ekbe BY ebeln ebelp bwart budat ASCENDING.
  SORT: t_rbkp BY belnr gjahr,          "(+)SDP 76540 BGANAPATHIRA
        t_ekko BY ebeln,                "(+)SDP 76540 BGANAPATHIRA
        t_ekpo BY ebeln ebelp,          "(+)SDP 76540 BGANAPATHIRA
        t_ekbe BY ebeln ebelp.          "(+)SDP 76540 BGANAPATHIRA
  LOOP AT t_rseg.
    CLEAR: t_out, wa_out, w_objid.
    REFRESH: t_cdhdr, t_cdpos.
    READ TABLE t_rbkp WITH KEY belnr = t_rseg-belnr gjahr = t_rseg-gjahr
                        BINARY SEARCH.            "(+)SDP 76540 BGANAPATHIRA
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE t_ekko WITH KEY ebeln = t_rseg-ebeln
                        BINARY SEARCH.            "(+)SDP 76540 BGANAPATHIRA
    IF sy-subrc NE 0.
      CLEAR t_ekko.
    ENDIF.

    READ TABLE t_ekpo WITH KEY ebeln = t_rseg-ebeln ebelp = t_rseg-ebelp
                        BINARY SEARCH.            "(+)SDP 76540 BGANAPATHIRA.
    IF sy-subrc NE 0.
      CLEAR t_ekpo.
    ENDIF.

    READ TABLE t_ekbe WITH KEY ebeln = t_rseg-ebeln ebelp = t_rseg-ebelp
*                    bwart = '101'.                 "(-)SDP 76540 BGANAPATHIRA
                          BINARY SEARCH.            "(+)SDP 76540 BGANAPATHIRA
    IF sy-subrc NE 0.
      CLEAR t_ekbe.
    ENDIF.

    t_out-belnr = t_rseg-belnr.
    t_out-buzei = t_rseg-buzei.
    t_out-gjahr = t_rseg-gjahr.
    t_out-blart = t_rbkp-blart.
    t_out-ebeln = t_rseg-ebeln.
    t_out-ebelp = t_rseg-ebelp.
    t_out-bsart = t_ekko-bsart.
    WRITE t_rbkp-budat TO t_out-budat.
    WRITE t_rbkp-cpudt TO t_out-cpudt.
    WRITE t_rbkp-zfbdt TO t_out-zfbdt.
    WRITE t_ekbe-budat TO t_out-gbudat.
    t_out-bukrs = t_rseg-bukrs.
    t_out-wrbtr = t_rseg-wrbtr.
    t_out-waers = t_rbkp-waers.
    t_out-menge = t_ekpo-menge.
    IF NOT t_ekko-bsart = 'ZF' AND NOT t_ekko-bsart = 'ZT'.
      t_out-dmenge = t_ekpo-menge.
*- Begin of Change SDP 76540 Perf Issue
*      LOOP AT t_ekbe WHERE ebeln = t_rseg-ebeln AND ebelp = t_rseg-ebelp AND bwart = '101'.
*        t_out-dmenge = t_out-dmenge - t_ekbe-menge.
*      ENDLOOP.
      READ TABLE t_ekbe TRANSPORTING NO FIELDS WITH KEY ebeln = t_rseg-ebeln
                                                        ebelp = t_rseg-ebelp BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_index = sy-tabix.
        LOOP AT t_ekbe INTO lwa_ekbe FROM lv_index.
          IF lwa_ekbe-ebeln = t_rseg-ebeln AND lwa_ekbe-ebelp = t_rseg-ebelp.
            IF lwa_ekbe-bwart = '101'.
              t_out-dmenge = t_out-dmenge - lwa_ekbe-menge.
            ELSEIF lwa_ekbe-bwart = '102'.
              t_out-dmenge = t_out-dmenge + lwa_ekbe-menge.
            ENDIF.
          ELSE.
            CLEAR: lv_index.
            EXIT.
          ENDIF.

        ENDLOOP.
      ENDIF.
*- End of Change SDP 76540 Perf Issue

    ENDIF.

    CONCATENATE t_rseg-belnr t_rseg-gjahr INTO w_objid.
    CONDENSE w_objid NO-GAPS.
    CLEAR: t_cdhdr[], t_cdpos[], t_cdhdr, t_cdpos.    "(+) SDP 83745 3/30/2015
    SELECT objectid changenr username udate FROM cdhdr INTO TABLE t_cdhdr
      WHERE objectclas = 'INCOMINGINVOICE' AND
            objectid = w_objid.
    IF sy-subrc = 0.
      SELECT objectid changenr tabname fname chngind value_new value_old FROM cdpos INTO TABLE t_cdpos
        FOR ALL ENTRIES IN t_cdhdr
        WHERE objectclas = 'INCOMINGINVOICE' AND
              objectid = t_cdhdr-objectid AND
              changenr = t_cdhdr-changenr. " AND
*              TABNAME = 'RSEG'.
      IF sy-subrc = 0.
        READ TABLE t_cdpos WITH KEY fname = 'SPGRM' value_new = ' ' value_old = 'X'.
        IF sy-subrc = 0.
          t_out-reason = 'Quantity'.
          READ TABLE t_cdhdr WITH KEY objectid = t_cdpos-objectid changenr = t_cdpos-changenr.
          t_out-user = t_cdhdr-username.
          WRITE t_cdhdr-udate TO t_out-udate.
        ELSE.
          READ TABLE t_cdpos WITH KEY fname = 'SPGRP' value_new = ' ' value_old = 'X'.
          IF sy-subrc = 0.
            t_out-reason = 'Price'.
            READ TABLE t_cdhdr WITH KEY objectid = t_cdpos-objectid changenr = t_cdpos-changenr.
            t_out-user = t_cdhdr-username.
            WRITE t_cdhdr-udate TO t_out-udate.
*- Begin of Change for SDP 83745 (Include manual released invoices without fixing qty variance)
          ELSE.
            READ TABLE t_cdpos WITH KEY tabname = 'RBKP_BLOCKED' fname = 'KEY' chngind = 'D'.
            IF sy-subrc = 0.
              t_out-reason = 'Manual Rel'(002).
              READ TABLE t_cdhdr WITH KEY objectid = t_cdpos-objectid changenr = t_cdpos-changenr.
              t_out-user = t_cdhdr-username.
              WRITE t_cdhdr-udate TO t_out-udate.
            ENDIF.
*- End of Change for SDP 83745 (Include manual released invoices without fixing qty variance)
          ENDIF.
        ENDIF.

        IF t_cdhdr-udate LT t_rbkp-cpudt.
          READ TABLE t_cdpos WITH KEY tabname = 'RBKP' fname = 'BUDAT'.
          IF sy-subrc = 0.
            w_date = t_cdpos-value_old.
            WRITE w_date TO t_out-bdate.
          ENDIF.
        ELSE.
          WRITE t_rbkp-cpudt TO t_out-bdate.
        ENDIF.

      ENDIF.
    ENDIF.

    IF t_out-reason IS INITIAL.
      CONTINUE.
    ENDIF.

    APPEND t_out.
  ENDLOOP.

  DESCRIBE TABLE t_out LINES w_num.

ENDFORM.                    " PREPARE_OUTPUT
