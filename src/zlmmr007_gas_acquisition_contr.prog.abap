REPORT  zlmmr007_gas_acquisition_contr.
*&---------------------------------------------------------------------*
*& Report  ZLMMR007_GAS_ACQUISITION_CONTR
**
*&                                                                     *
*&---------------------------------------------------------------------*
*  Author:      Brian Boundy                                           *
*  Date:        March, 2011.                                           *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*    - This program will be used for adhoc reporting by gas aquisitions*
*    to monitor purchases, deliveries, and invoice verification.       *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*2012/08/01 - GYMANA - TR804-A                                         *
*                      Change IR_AMT_FC source field to ekbe-refwr and *
*                      created new total field GR_AMT_TOT_LC.          *
*20150129     SAHMAD   SDP80424 - Workflow data inclusion.
*2021/03/01   MEDISETA COG Changes to include MSA, Locations data in
*                      output
*&---------------------------------------------------------------------*
TYPE-POOLS: slis, abap.
TABLES: ekpo, ekko, ekbe, konv, swr_wihdr, swr_widtl.


DATA: BEGIN OF lt_report1 OCCURS 1,
        mandt       LIKE ekpo-mandt,
        ebeln       LIKE ekpo-ebeln,
        ebelp       LIKE ekpo-ebelp,
        loekz       LIKE ekpo-loekz,
        aedat       LIKE ekpo-aedat,
        txz01       LIKE ekpo-txz01,
        matnr       LIKE ekpo-matnr,
        ematn       LIKE ekpo-ematn,
        bukrs       LIKE ekpo-bukrs,
        werks       LIKE ekpo-werks,
        lgort       LIKE ekpo-lgort,
        bednr       LIKE ekpo-bednr,
        matkl       LIKE ekpo-matkl,
        infnr       LIKE ekpo-infnr,
        idnlf       LIKE ekpo-idnlf,
        ktmng       LIKE ekpo-ktmng,
        menge       LIKE ekpo-menge,
        meins       LIKE ekpo-meins,
        bprme       LIKE ekpo-bprme,
        bpumz       LIKE ekpo-bpumz,
        bpumn       LIKE ekpo-bpumn,
        umrez       LIKE ekpo-umrez,
        umren       LIKE ekpo-umren,
        netpr       LIKE ekpo-netpr,
        peinh       LIKE ekpo-peinh,
        netwr       LIKE ekpo-netwr,
        brtwr       LIKE ekpo-brtwr,
        mwskz       LIKE ekpo-mwskz,
        uebto       LIKE ekpo-uebto,
        uebtk       LIKE ekpo-uebtk,
        untto       LIKE ekpo-untto,
        wepos       LIKE ekpo-wepos,
        weunb       LIKE ekpo-weunb,
        repos       LIKE ekpo-repos,
        webre       LIKE ekpo-webre,
        konnr       LIKE ekpo-konnr,
        ktpnr       LIKE ekpo-ktpnr,
        lmein       LIKE ekpo-lmein,
        evers       LIKE ekpo-evers,
        zwert       LIKE ekpo-zwert,
        prdat       LIKE ekpo-prdat,
        bstyp       LIKE ekpo-bstyp,
        effwr       LIKE ekpo-effwr,
        sobkz       LIKE ekpo-sobkz,
        meprf       LIKE ekpo-meprf,
        packno      LIKE ekpo-packno,
        ekbukrs     LIKE ekko-bukrs,
        ekbstyp     LIKE ekko-bstyp,
        bsart       LIKE ekko-bsart,
        ekloekz     LIKE ekko-loekz,
        ekaedat     LIKE ekko-aedat,
        ernam       LIKE ekko-ernam,
        pincr       LIKE ekko-pincr,
        lponr       LIKE ekko-lponr,
        lifnr       LIKE ekko-lifnr,
        spras       LIKE ekko-spras,
        zterm       LIKE ekko-zterm,
        ekorg       LIKE ekko-ekorg,
        ekgrp       LIKE ekko-ekgrp,
        waers       LIKE ekko-waers,
        wkurs       LIKE ekko-wkurs,
        kufix       LIKE ekko-kufix,
        bedat       LIKE ekko-bedat,
        kdatb       LIKE ekko-kdatb,
        kdate       LIKE ekko-kdate,
        ihrez       LIKE ekko-ihrez,
        verkf       LIKE ekko-verkf,
        telf1       LIKE ekko-telf1,
        llief       LIKE ekko-llief,
        ekkonnr     LIKE ekko-konnr,
        inco1       LIKE ekko-inco1,
        inco2       LIKE ekko-inco2,
        knumv       LIKE ekko-knumv,
        kalsm       LIKE ekko-kalsm,
        frggr       LIKE ekko-frggr,
        frgsx       LIKE ekko-frgsx,
        frgke       LIKE ekko-frgke,
        frgzu       LIKE ekko-frgzu,
        frgrl       LIKE ekko-frgrl,
        rlwrt       LIKE ekko-rlwrt,
        zzsent_ind  LIKE ekko-zzsent_ind,
        zzcondayqty LIKE ekko-zzcondayqty,
        zzconprice  LIKE ekko-zzconprice,
*  Start of COG Change
        ZZPARTY_AGMT_ID like ekko-ZZPARTY_AGMT_ID,
        ZZMSA         like ZMMT_MASTAGREE-ZZMSA,
        ZZEKGRP     like ekko-ZZEKGRP,
        EKNAM         like T024-EKNAM,
        zzparty     like ekko-zzparty,
        zztrloc1    like ekko-zztrloc1,
        ZZTRLOCALIAS1 like ZMMT_LOCMAST-ZZTRLOCALIAS,
        zztrloc2    like ekko-zztrloc2,
        ZZTRLOCALIAS2 like ZMMT_LOCMAST-ZZTRLOCALIAS,
        zztrloc3    like ekko-zztrloc3,
        ZZTRLOCALIAS3 like ZMMT_LOCMAST-ZZTRLOCALIAS,
        zztrloc4    like ekko-zztrloc4,
        ZZTRLOCALIAS4 like ZMMT_LOCMAST-ZZTRLOCALIAS,
*  End of COG Change
        land1       LIKE lfa1-land1,
        name1       LIKE lfa1-name1,
        name2       LIKE lfa1-name2,
        name3       LIKE lfa1-name3,
        name4       LIKE lfa1-name4,
        ort01       LIKE lfa1-ort01,
        ort02       LIKE lfa1-ort02,
        pfach       LIKE lfa1-pfach,
        pstl2       LIKE lfa1-pstl2,
        pstlz       LIKE lfa1-pstlz,
        regio       LIKE lfa1-regio,
        sortl       LIKE lfa1-sortl,
        stras       LIKE lfa1-stras,
        adrnr       LIKE lfa1-adrnr,
        mcod1       LIKE lfa1-mcod1,
        konzs       LIKE lfa1-konzs,
        ktokk       LIKE lfa1-ktokk,
        gr_qty_tot  LIKE ekbe-bpmng,
        gr_amt_tot  LIKE ekbe-wrbtr,
        gr_amt_tot_lc LIKE ekbe-dmbtr,                     "TR804-A
        ir_qty_tot  LIKE ekbe-bpmng,
        ir_amt_fc   LIKE ekbe-refwr,                      "TR804-A
        ir_amt_lc   LIKE ekbe-reewr,
        act_unit_pr(13)  TYPE p DECIMALS 4,
        rem_del_qty LIKE ekpo-menge,
        net_amt     LIKE ekbe-wrbtr,
        gr_cnt      LIKE ekbe-ebelp,
        gr_qty_gjs  LIKE ekbe-bamng,
        kbetr       LIKE konv-kbetr,
        kpein       LIKE konv-kpein,
        disc_sur    TYPE p DECIMALS 4,
        index_pr    TYPE p DECIMALS 4,
        wi_id       LIKE swr_wihdr-wi_id, "Work item
        wi_cd       LIKE swr_wihdr-wi_cd, "Work Item Creation Date
        wi_creator  LIKE swr_wihdr-wi_creator, "
        wi_led      LIKE swr_widtl-wi_led, "Document Approval Date
        wi_aagent   LIKE swr_widtl-wi_aagent,
        wi_days     TYPE i, "difference b/w WI creation and compl date
        wi_stat     LIKE swr_widtl-wi_stat, "Work Item Status
        wi_item_stat(35),
      END OF lt_report1.



DATA: ls_report     LIKE LINE OF lt_report1,
      lt_report_tmp LIKE lt_report1 OCCURS 1,
      lt_report     LIKE lt_report1 OCCURS 1,
      ls_ekbe       LIKE ekbe.
*  Start of COG Change
TYPES: BEGIN OF ty_msa,
         zzparty_agmt_id type zmmt_mastagree-zzparty_agmt_id,
         zzmsa type zmmt_mastagree-zzmsa,
       END OF ty_msa,

       BEGIN OF ty_tb,
         ekgrp type t024-ekgrp,
         eknam type t024-eknam,
       END OF ty_tb.
DATA: lt_locmast type STANDARD TABLE OF zmmt_locmast,
      ls_locmast type zmmt_locmast,
      ls_msa type ty_msa,
      ls_tb type ty_tb,
      lt_msa type standard table of ty_msa,
      lt_tb type standard table of ty_tb.
*  End of COG Change
*ALV Stuff
DATA: gl_head01(60)  TYPE c,
      gl_head02(60)  TYPE c,
      es_variant    LIKE disvariant,
      is_variant    LIKE disvariant.





***********************************************************************
*                      SELECTION SCREEN                               *
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-101.
SELECT-OPTIONS:
  s_bednr   FOR ekpo-bednr OBLIGATORY NO-EXTENSION,
  s_ebeln   FOR ekpo-ebeln,
  s_loekz   FOR ekpo-loekz,
  s_werks   FOR ekpo-werks,
  s_lgort   FOR ekpo-lgort,
  s_matnr   FOR ekpo-matnr.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-102.
SELECT-OPTIONS:
  s_lifnr       FOR ekko-lifnr,
  s_bsart       FOR ekko-bsart,
  s_frgke       FOR ekko-frgke      DEFAULT 'Y',
  s_inco1       FOR ekko-inco1,
  s_waers       FOR ekko-waers,
  s_aedat       FOR ekko-aedat,
  s_bedat       FOR ekko-bedat,
  s_sent_i      FOR ekko-zzsent_ind DEFAULT 'Y'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME TITLE text-103.
PARAMETER:  p_gs  RADIOBUTTON GROUP rad1 USER-COMMAND rad1 DEFAULT 'X',
            p_gt  RADIOBUTTON GROUP rad1,
            p_lp  RADIOBUTTON GROUP rad1.
SELECT-OPTIONS: s_bldat FOR ekbe-bldat MODIF ID lp NO-EXTENSION.

SELECTION-SCREEN SKIP.
PARAMETERS p_varint   LIKE  disvariant-variant.    "Display Variant

SELECTION-SCREEN END OF BLOCK c1.


***********************************************************************
*                  SELECT DISPLAY VARIANT                             *
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varint.
  is_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = is_variant
      i_save        = 'A'
    IMPORTING
      es_variant    = es_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.
  p_varint = es_variant-variant.



***********************************************************************
*                  SELECT SCREEN PROCESSING                           *
***********************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'LP'.
      IF p_lp = abap_false.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'ONLI'. "Run Report button.
    IF p_lp = abap_true.
      IF s_bldat IS INITIAL.
        MESSAGE e019(zs) WITH text-901.
      ENDIF.
    ENDIF.
  ENDIF.



***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
START-OF-SELECTION.

  PERFORM get_db_data.
  PERFORM get_workflow_data.
  PERFORM display_alv_grid_data.


***********************************************************************
*                             GET_DB_DATA                             *
***********************************************************************
FORM get_db_data.

  DATA: lv_bstyp LIKE ekpo-bstyp,
        lv_bldat LIKE ekbe-bldat,
        lt_konv TYPE TABLE OF konv,
        ls_konv LIKE LINE OF lt_konv.

  IF p_gt = abap_true.
    "Gas Trans is purchdoc category F, others are L"
    lv_bstyp = 'F'.
  ELSE.
    lv_bstyp = 'L'.
  ENDIF.
*  Start of COG Change
  SELECT * from zmmt_locmast INTO TABLE lt_locmast.
  IF sy-subrc = 0.
    sort lt_locmast by zztrloc.
  ENDIF.
*  End of COG Change

  SELECT  ekpo~mandt ekpo~ebeln ekpo~ebelp ekpo~loekz ekpo~aedat
          ekpo~txz01 ekpo~matnr ekpo~ematn ekpo~bukrs ekpo~werks
          ekpo~lgort ekpo~bednr ekpo~matkl ekpo~infnr ekpo~idnlf
          ekpo~ktmng ekpo~menge ekpo~meins ekpo~bprme ekpo~bpumz
          ekpo~bpumn ekpo~umrez ekpo~umren ekpo~netpr ekpo~peinh
          ekpo~netwr ekpo~brtwr ekpo~mwskz ekpo~uebto ekpo~uebtk
          ekpo~untto ekpo~wepos ekpo~weunb ekpo~repos ekpo~webre
          ekpo~konnr ekpo~ktpnr ekpo~lmein ekpo~evers ekpo~zwert
          ekpo~prdat ekpo~bstyp ekpo~effwr ekpo~sobkz ekpo~meprf
          ekpo~packno
          ekko~bukrs AS ekbukrs
          ekko~bstyp AS ekbstyp
          ekko~bsart
          ekko~loekz AS ekloekz
          ekko~aedat AS ekaedat
          ekko~ernam ekko~pincr ekko~lponr ekko~lifnr ekko~spras
          ekko~zterm ekko~ekorg ekko~ekgrp ekko~waers ekko~wkurs
          ekko~kufix ekko~bedat ekko~kdatb ekko~kdate ekko~ihrez
          ekko~verkf ekko~telf1 ekko~llief
          ekko~konnr AS ekkonnr
          ekko~inco1 ekko~inco2 ekko~knumv ekko~kalsm ekko~frggr
          ekko~frgsx ekko~frgke ekko~frgzu ekko~frgrl ekko~rlwrt
          ekko~zzsent_ind ekko~zzcondayqty ekko~zzconprice
 " Start of COG Change
          ekko~zztrloc1 ekko~zztrloc2 ekko~zztrloc3 ekko~zztrloc4
          ekko~zzparty ZZPARTY_AGMT_ID ZZEKGRP
 " End of COG Change
          lfa1~land1 lfa1~name1 lfa1~name2 lfa1~name3 lfa1~name4
          lfa1~ort01 lfa1~ort02 lfa1~pfach lfa1~pstl2 lfa1~pstlz
          lfa1~regio lfa1~sortl lfa1~stras lfa1~adrnr lfa1~mcod1
          lfa1~konzs lfa1~ktokk
      INTO CORRESPONDING FIELDS OF TABLE lt_report_tmp
      FROM ekpo INNER JOIN ekko
                  ON ekpo~ebeln = ekko~ebeln
                INNER JOIN lfa1
                  ON ekko~lifnr = lfa1~lifnr
      WHERE ekpo~bednr      IN s_bednr
        AND ekpo~ebeln      IN s_ebeln
        AND ekpo~loekz      IN s_loekz
        AND ekpo~werks      IN s_werks
        AND ekpo~lgort      IN s_lgort
        AND ekpo~matnr      IN s_matnr
        AND ekko~lifnr      IN s_lifnr
        AND ekko~bsart      IN s_bsart
        AND ekko~frgke      IN s_frgke
        AND ekko~inco1      IN s_inco1
        AND ekko~waers      IN s_waers
        AND ekko~aedat      IN s_aedat
        AND ekko~bedat      IN s_bedat
        AND ekko~zzsent_ind IN s_sent_i
        AND ekpo~bstyp      = lv_bstyp.
  IF SY-SUBRC = 0.
    select ZZPARTY_AGMT_ID ZZMSA
      FROM zmmt_mastagree into table lt_msa
      FOR ALL ENTRIES IN lt_report_tmp
      WHERE ZZPARTY_AGMT_ID = lt_report_tmp-ZZPARTY_AGMT_ID.

    SELECT ekgrp eknam
      FROM t024 into table lt_tb
      FOR ALL ENTRIES IN lt_report_tmp
      WHERE ekgrp = lt_report_tmp-zzekgrp.
  ENDIF.

  LOOP AT lt_report_tmp INTO ls_report.

    CLEAR lv_bldat.

    SELECT vgabe bpmng dmbtr wrbtr shkzg                       "TR804-A
           refwr reewr bldat bamng                             "TR804-A
      FROM ekbe
      INTO CORRESPONDING FIELDS OF ls_ekbe
      WHERE ebeln = ls_report-ebeln
        AND ebelp = ls_report-ebelp
        AND bldat IN s_bldat
      ORDER BY bldat.

      IF lv_bldat <> ls_ekbe-bldat(6) AND p_lp = abap_true.

        IF lv_bldat IS NOT INITIAL.
          IF ls_report-gr_qty_tot IS INITIAL.
            ls_report-act_unit_pr = 0.
          ELSE.
            ls_report-act_unit_pr = ls_report-gr_amt_tot /
                                    ls_report-gr_qty_tot.
          ENDIF.
          ls_report-rem_del_qty = ls_report-menge -
                                  ls_report-gr_qty_tot.
          ls_report-net_amt     = ls_report-gr_amt_tot -
                                  ls_report-ir_amt_fc.
          APPEND ls_report TO lt_report.
        ENDIF.

        lv_bldat = ls_ekbe-bldat(6).
        ls_report-bednr = lv_bldat.
        CLEAR:  ls_report-gr_qty_tot, ls_report-gr_amt_tot,
                ls_report-gr_amt_tot_lc,                       "TR804-A
                ls_report-ir_qty_tot, ls_report-ir_amt_fc,
                ls_report-ir_amt_lc, ls_report-act_unit_pr,
                ls_report-rem_del_qty.

      ENDIF.

      IF ls_ekbe-vgabe = '1'.
        "Goods Receipt
        IF ls_ekbe-shkzg = 'S'.
          "Debit
          ls_report-gr_qty_tot = ls_report-gr_qty_tot + ls_ekbe-bpmng.
          ls_report-gr_amt_tot = ls_report-gr_amt_tot + ls_ekbe-wrbtr.
          ls_report-gr_amt_tot_lc = ls_report-gr_amt_tot_lc +
                                    ls_ekbe-dmbtr.             "TR804-A
          ls_report-gr_qty_gjs = ls_report-gr_qty_gjs + ls_ekbe-bamng.
        ELSEIF ls_ekbe-shkzg = 'H'.
          "Credit
          ls_report-gr_qty_tot = ls_report-gr_qty_tot - ls_ekbe-bpmng.
          ls_report-gr_amt_tot = ls_report-gr_amt_tot - ls_ekbe-wrbtr.
          ls_report-gr_amt_tot_lc = ls_report-gr_amt_tot_lc -
                                    ls_ekbe-dmbtr.             "TR804-A
          ls_report-gr_qty_gjs = ls_report-gr_qty_gjs  - ls_ekbe-bamng.
        ENDIF.
        ls_report-gr_cnt = ls_report-gr_cnt + 1.
      ELSEIF ls_ekbe-vgabe = '2'.
        "Goods Issue
        IF ls_ekbe-shkzg = 'S'.
          "Debit
          ls_report-ir_qty_tot = ls_report-ir_qty_tot +
                                 ls_ekbe-bpmng.
          ls_report-ir_amt_fc  = ls_report-ir_amt_fc  +        "TR804-A
                                 ls_ekbe-refwr.                "TR804-A
          ls_report-ir_amt_lc  = ls_report-ir_amt_lc  +
                                 ls_ekbe-reewr.
        ELSEIF ls_ekbe-shkzg = 'H'.
          "Credit
          ls_report-ir_qty_tot = ls_report-ir_qty_tot -
                                 ls_ekbe-bpmng.
          ls_report-ir_amt_fc  = ls_report-ir_amt_fc  -        "TR804-A
                                 ls_ekbe-refwr.                "TR804-A
          ls_report-ir_amt_lc  = ls_report-ir_amt_lc  -
                                 ls_ekbe-reewr.
        ENDIF.
      ENDIF.
    ENDSELECT.
    IF p_gs = 'X'.
      CLEAR: lt_konv.
      SELECT * FROM konv INTO TABLE lt_konv
                    WHERE knumv = ls_report-knumv
                      AND kposn = ls_report-ebelp
                      AND ( kschl = 'ZC00' OR
                            kschl = 'ZC01' ).
      LOOP AT lt_konv INTO ls_konv.
        CHECK ls_konv-kbetr IS NOT INITIAL.
*    get first non-zero record.
        ls_report-kbetr = ls_konv-kbetr.
        ls_report-kpein = ls_konv-kpein.
        IF ls_konv-kpein IS NOT INITIAL.
          ls_report-disc_sur = ls_konv-kbetr / ls_konv-kpein.
        ELSE.
          CLEAR ls_report-disc_sur.
        ENDIF.
        EXIT.
      ENDLOOP.
    ENDIF.
* Start of COG Change
    clear ls_locmast.
    READ TABLE lt_locmast into ls_locmast
      with KEY zztrloc = ls_report-zztrloc1.
    IF sy-subrc = 0.
      ls_report-zztrlocalias1 = ls_locmast-zztrlocalias.
    ENDIF.

    clear ls_locmast.
    READ TABLE lt_locmast into ls_locmast
      with KEY zztrloc = ls_report-zztrloc2.
    IF sy-subrc = 0.
      ls_report-zztrlocalias2 = ls_locmast-zztrlocalias.
    ENDIF.

    clear ls_locmast.
    READ TABLE lt_locmast into ls_locmast
      with KEY zztrloc = ls_report-zztrloc3.
    IF sy-subrc = 0.
      ls_report-zztrlocalias3 = ls_locmast-zztrlocalias.
    ENDIF.

    clear ls_locmast.
    READ TABLE lt_locmast into ls_locmast
      with KEY zztrloc = ls_report-zztrloc4.
    IF sy-subrc = 0.
      ls_report-zztrlocalias4 = ls_locmast-zztrlocalias.
    ENDIF.

    clear ls_msa.
    READ TABLE lt_msa into ls_msa
      with KEY zzparty_agmt_id = ls_report-zzparty_agmt_id.
    IF sy-subrc = 0.
      ls_report-zzmsa = ls_msa-zzmsa.
    ENDIF.

    clear ls_tb.
    READ TABLE lt_tb into ls_tb
      with KEY ekgrp = ls_report-zzekgrp.
    IF sy-subrc = 0.
      ls_report-eknam = ls_tb-eknam.
    ENDIF.
* End of COG Change
    IF lv_bldat IS NOT INITIAL OR p_lp = abap_false.
      IF ls_report-gr_qty_tot IS INITIAL.
        ls_report-act_unit_pr = 0.
      ELSE.
        ls_report-act_unit_pr = ls_report-gr_amt_tot /
                                ls_report-gr_qty_tot.
      ENDIF.
      ls_report-rem_del_qty = ls_report-menge - ls_report-gr_qty_tot.
      ls_report-net_amt     = ls_report-gr_amt_tot -
                              ls_report-ir_amt_fc.
      ls_report-index_pr = ls_report-act_unit_pr -
                           ls_report-disc_sur.
      APPEND ls_report TO lt_report.
    ENDIF.

  ENDLOOP.


ENDFORM.                    "get_db_data


***********************************************************************
*                        DISPLAY_ALV_GRID_DATA                        *
***********************************************************************
FORM display_alv_grid_data.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat LIKE LINE OF lt_fieldcat,
        ls_layout   TYPE slis_layout_alv,
        ls_variant  LIKE disvariant,
        lt_sort     TYPE slis_t_sortinfo_alv,
        ls_sort     LIKE LINE OF lt_sort.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'LT_REPORT1'
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

*Customize the layout
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra = 'X'.

* use variant from selection screen
  ls_variant-report = sy-repid.
  ls_variant-variant = p_varint.

* update field catalog (hide/reposition/etc)
  "SHOW ALL
  LOOP AT lt_fieldcat INTO ls_fieldcat.
    ls_fieldcat-no_out = abap_false.
    MODIFY lt_fieldcat FROM ls_fieldcat.
  ENDLOOP.


  DEFINE hide_column.
    loop at lt_fieldcat into ls_fieldcat.
      case ls_fieldcat-fieldname.
        when &1.
          ls_fieldcat-no_out = abap_true.          " Hide Columns
          ls_fieldcat-key    = ' '.          " Key columns-not first
      endcase.
      modify lt_fieldcat from ls_fieldcat.
    endloop.
  END-OF-DEFINITION.
  hide_column:
         'MANDT','LOEKZ','AEDAT','TXZ01','MATNR','EMATN','BUKRS',
         'MATKL','INFNR','IDNLF','KTMNG','MENGE','BPRME','BPUMZ',
         'BPUMN','UMREZ','UMREN','NETPR','PEINH','NETWR','BRTWR',
         'MWSKZ','UEBTO','UEBTK','UNTTO','WEPOS','WEUNB','REPOS',
         'WEBRE','KONNR','KTPNR','LMEIN','EVERS','ZWERT','PRDAT',
         'BSTYP','EFFWR','SOBKZ','MEPRF','PACKNO',

         'EKBUKRS','EKBSTYP','EKLOEKZ','EKAEDAT','ERNAM','PINCR',
         'LPONR','SPRAS','ZTERM','EKORG','EKGRP','WKURS','KUFIX',
         'BEDAT','KDATB','KDATE','IHREZ','VERKF','TELF1','LLIEF',
         'EKKONNR','INCO1','INCO2','KNUMV','KALSM','FRGGR',
         'FRGSX','FRGKE','FRGZU','FRGRL','RLWRT','ZZSENT_IND',
         'ZZCONDAYQTY','ZZCONPRICE',

         'LAND1','NAME1','NAME2','NAME3','NAME4','ORT01','ORT02',
         'PFACH','PSTL2','PSTLZ','REGIO','SORTL','STRAS','ADRNR',
         'MCOD1','KONZS','KTOKK',

         'IR_AMT_LC','GR_CNT','NET_AMT'.


  "Fix names of custom column
  LOOP AT lt_fieldcat INTO ls_fieldcat.
    CASE ls_fieldcat-fieldname.

      WHEN 'GR_QTY_TOT'.
        ls_fieldcat-seltext_l = 'Actual GR Qty'.
        ls_fieldcat-seltext_m = 'Actual GR Qty'.
        ls_fieldcat-seltext_s = 'GR Qty'.
        ls_fieldcat-reptext_ddic = 'GR Qty'.
      WHEN 'GR_AMT_TOT'.
        ls_fieldcat-seltext_l = 'Actual GR Amount'.
        ls_fieldcat-seltext_m = 'Actual GR Amount'.
        ls_fieldcat-seltext_s = 'GR Amount'.
        ls_fieldcat-reptext_ddic = 'GR Amount'.
      WHEN 'GR_AMT_TOT_LC'.                                    "TR804-A
        ls_fieldcat-seltext_l = 'Actual GR Amount LC'.         "TR804-A
        ls_fieldcat-seltext_m = 'Actual GR Amount LC'.         "TR804-A
        ls_fieldcat-seltext_s = 'GR Amount LC'.                "TR804-A
        ls_fieldcat-reptext_ddic = 'GR Amount LC'.             "TR804-A
      WHEN 'IR_QTY_TOT'.
        ls_fieldcat-seltext_l = 'Actual IR Qty'.
        ls_fieldcat-seltext_m = 'Actual IR Qty'.
        ls_fieldcat-seltext_s = 'IR Qty'.
        ls_fieldcat-reptext_ddic = 'IR Qty'.
      WHEN 'IR_AMT_FC'.
        ls_fieldcat-seltext_l = 'Actual IR Amount'.
        ls_fieldcat-seltext_m = 'Actual IR Amount'.
        ls_fieldcat-seltext_s = 'IR Amount'.
        ls_fieldcat-reptext_ddic = 'IR Amount'.
      WHEN 'IR_AMT_LC'.
        ls_fieldcat-seltext_l = 'Actual IR Amount LC'.
        ls_fieldcat-seltext_m = 'Actual IR Amount LC'.
        ls_fieldcat-seltext_s = 'IR Amount LC'.
        ls_fieldcat-reptext_ddic = 'IR Amount LC'.
      WHEN 'ACT_UNIT_PR'.
        ls_fieldcat-seltext_l = 'Net Price'.
        ls_fieldcat-seltext_m = 'Net Price'.
        ls_fieldcat-seltext_s = 'Net Price'.
        ls_fieldcat-reptext_ddic = 'Net Price'.
      WHEN 'REM_DEL_QTY'.
        ls_fieldcat-seltext_l = 'Rem Delivery'.
        ls_fieldcat-seltext_m = 'Rem Delivery'.
        ls_fieldcat-seltext_s = 'Rem Del'.
        ls_fieldcat-reptext_ddic = 'Rem Del'.
      WHEN 'GR_CNT'.
        ls_fieldcat-seltext_l = 'GR Count'.
        ls_fieldcat-seltext_m = 'GR Count'.
        ls_fieldcat-seltext_s = 'GR Cnt'.
        ls_fieldcat-reptext_ddic = 'GR Cnt'.
      WHEN 'NET_AMT'.
        ls_fieldcat-seltext_l = 'Amount Not Invoiced'.
        ls_fieldcat-seltext_m = 'Amount Not Invoiced'.
        ls_fieldcat-seltext_s = 'Amount n. Inv.'.
        ls_fieldcat-reptext_ddic = 'Amount n. Inv.'.
      WHEN 'GR_QTY_GJS'.
        ls_fieldcat-seltext_l = 'GR quantity in Base UoM'.
        ls_fieldcat-seltext_m = 'GR quantity in Base UoM'.
        ls_fieldcat-seltext_s = 'GR quantity in Base UoM'.
        ls_fieldcat-reptext_ddic = 'GR quantity in Base UoM'.
      WHEN 'KBETR'.

      WHEN 'KPEIN'.

      WHEN 'DISC_SUR'.
        ls_fieldcat-seltext_l = 'Discount-Surcharge'.
        ls_fieldcat-seltext_m = 'Discount-Surcharge'.
        ls_fieldcat-seltext_s = 'Discount-Surcharge'.
        ls_fieldcat-reptext_ddic = 'Discount-Surcharge'.
      WHEN 'INDEX_PR'.
        ls_fieldcat-seltext_l = 'Index Price'.
        ls_fieldcat-seltext_m = 'Index Price'.
        ls_fieldcat-seltext_s = 'Index Price'.
        ls_fieldcat-reptext_ddic = 'Index Price'.
      WHEN 'WI_ID'.
        ls_fieldcat-seltext_l = 'Work Item number'.
        ls_fieldcat-seltext_m = 'Work Item'.
        ls_fieldcat-seltext_s = 'Work Item'.
        ls_fieldcat-reptext_ddic = 'Work Item'.
      WHEN 'WI_CD'.
        ls_fieldcat-seltext_l = 'Work Item Creation Date'.
        ls_fieldcat-seltext_m = 'WItem Cr. Date'.
        ls_fieldcat-seltext_s = 'WI Cr. Date'.
        ls_fieldcat-reptext_ddic = 'WItem Cr. Date'.
      WHEN 'WI_CREATOR'.
        ls_fieldcat-seltext_l = 'Work Item Creator'.
        ls_fieldcat-seltext_m = 'Work Item Creator'.
        ls_fieldcat-seltext_s = 'WI Creator'.
        ls_fieldcat-reptext_ddic = 'Work Item Creator'.
      WHEN 'WI_LED'.
        ls_fieldcat-seltext_l = 'Approval Date'.
        ls_fieldcat-seltext_m = 'Approval Date'.
        ls_fieldcat-seltext_s = 'Appr Date'.
        ls_fieldcat-reptext_ddic = 'Approval Date'.
      WHEN 'WI_AAGENT'.
        ls_fieldcat-seltext_l = 'Approver'.
        ls_fieldcat-seltext_m = 'Approver'.
        ls_fieldcat-seltext_s = 'Approver'.
        ls_fieldcat-reptext_ddic = 'Approver'.
      WHEN 'WI_DAYS'.
        ls_fieldcat-seltext_l = 'Days to complete'.
        ls_fieldcat-seltext_m = 'Days to complete'.
        ls_fieldcat-seltext_s = 'Days to complete'.
        ls_fieldcat-reptext_ddic = 'Days to complete'.
      WHEN 'WI_STAT'.
        ls_fieldcat-seltext_l = 'Work Item Status'.
        ls_fieldcat-seltext_m = 'Work Item Status'.
        ls_fieldcat-seltext_s = 'WI Status'.
        ls_fieldcat-reptext_ddic = 'Work Item Status'.
      WHEN 'WI_ITEM_STAT'.
        ls_fieldcat-seltext_l = 'Work Item Status'.
        ls_fieldcat-seltext_m = 'Work Item Status'.
        ls_fieldcat-seltext_s = 'WI Status'.
        ls_fieldcat-reptext_ddic = 'Work Item Status'.
      WHEN 'EKNAM'.
        ls_fieldcat-seltext_l = 'Trd Buyer Name'.
        ls_fieldcat-seltext_m = 'Trd Buyer Name'.
        ls_fieldcat-seltext_s = 'TBy Name'.
        ls_fieldcat-reptext_ddic = 'Trd Buyer Name'.
    ENDCASE.
    MODIFY lt_fieldcat FROM ls_fieldcat.
  ENDLOOP.


  CLEAR: ls_sort, lt_sort.
  ls_sort-spos = 1.
  ls_sort-fieldname = 'EBELN'.
  APPEND ls_sort TO lt_sort.

  CLEAR ls_sort.
  ls_sort-spos = 2.
  ls_sort-fieldname = 'EBELP'.
  APPEND ls_sort TO lt_sort.

  CLEAR ls_sort.
  ls_sort-spos = 3.
  ls_sort-fieldname = 'BEDNR'.
  APPEND ls_sort TO lt_sort.
* Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat             = lt_fieldcat
      is_layout               = ls_layout
      i_callback_top_of_page  = 'ALV_TOP_OF_PAGE'
      i_callback_program      = sy-repid
      i_callback_user_command = 'ALV_USER_COMMAND'
      i_save                  = 'A'
      is_variant              = ls_variant
      it_sort                 = lt_sort
    TABLES
      t_outtab                = lt_report
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.


ENDFORM.                    "display_alv_grid_data
*************************************************************
*                        TOP OF PAGE                        *
*************************************************************
FORM alv_top_of_page.
  DATA: ls_line TYPE slis_listheader.
  DATA: lt_top_of_page TYPE slis_t_listheader.

  DATA: lv_date_from(10)    TYPE c,
        lv_date_to(10)      TYPE c,
        lv_report_type(50)  TYPE c.


  IF p_lp = abap_true.

    CONCATENATE s_bldat-low(4) '/'
            s_bldat-low+4(2) '/'
            s_bldat-low+6(2)
            INTO lv_date_from.
    CONCATENATE s_bldat-high(4) '/'
                s_bldat-high+4(2) '/'
                s_bldat-high+6(2)
                INTO lv_date_to.

    IF s_bldat-high IS NOT INITIAL.
      CONCATENATE text-104 lv_date_from text-105 lv_date_to
        INTO gl_head01 SEPARATED BY space.
    ELSE.
      CONCATENATE text-104 lv_date_from
        INTO gl_head01 SEPARATED BY space.
    ENDIF.

  ELSE.
    IF s_bednr-high IS NOT INITIAL.
      CONCATENATE text-104 s_bednr-low text-105 s_bednr-high
        INTO gl_head01 SEPARATED BY space.
    ELSE.
      CONCATENATE text-104 s_bednr-low
        INTO gl_head01 SEPARATED BY space.
    ENDIF.
  ENDIF.

  MOVE text-clt  TO gl_head02+0(7).
  MOVE sy-sysid  TO gl_head02+8(5).
  MOVE sy-mandt  TO gl_head02+14(4).
  MOVE text-dte  TO gl_head02+21(5).
  WRITE sy-datum TO gl_head02+27(10).
  MOVE text-tme  TO gl_head02+40(5).
  WRITE sy-uzeit TO gl_head02+46(10).


  IF p_gs = abap_true.
    lv_report_type = %_p_gs_%_app_%-text.
  ELSEIF p_gt = abap_true.
    lv_report_type = %_p_gt_%_app_%-text.
  ELSE.
    lv_report_type = %_p_lp_%_app_%-text.
  ENDIF.

* - Heading Line: Type H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = sy-title.             "sy-title.
  APPEND ls_line TO lt_top_of_page.

* - Heading Line: Type H
  CLEAR ls_line.
  ls_line-typ   = 'H'.
  ls_line-info = lv_report_type.
  APPEND ls_line TO lt_top_of_page.

* - Heading Line: Type H
  CLEAR ls_line.
  ls_line-typ   = 'H'.
  ls_line-info = gl_head01.
  APPEND ls_line TO lt_top_of_page.

* - Action Line:  Type A
  CLEAR ls_line.
  ls_line-typ   = 'A'.
  ls_line-info = gl_head02.
  APPEND ls_line TO lt_top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.
ENDFORM.                    "ALV_TOP_OF_PAGE
*************************************************************
*                        USER COMMAND                       *
*************************************************************
FORM alv_user_command USING     r_ucomm LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.
  IF r_ucomm = '&IC1'.
    IF rs_selfield-tabindex NE 0.
      READ TABLE lt_report INDEX rs_selfield-tabindex INTO ls_report.
      CASE rs_selfield-sel_tab_field.
        WHEN 'LT_REPORT1-EBELN'.
          IF ls_report-bstyp = 'F'.
            "Contract release order

            "display purchase order
            CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
              EXPORTING
                i_ebeln              = ls_report-ebeln
                i_ebelp              = ls_report-ebelp
                i_enjoy              = 'X'
              EXCEPTIONS
                not_found            = 1
                no_authority         = 2
                invalid_call         = 3
                preview_not_possible = 4
                OTHERS               = 5.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

          ELSE.
            "Scheduling Agreement
            SET PARAMETER ID 'SAG' FIELD ls_report-ebeln.
            CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
          ENDIF.

      ENDCASE.
    ENDIF.
  ENDIF.

ENDFORM.                    "alv_user_command
*&---------------------------------------------------------------------*
*&      Form  GET_WORKFLOW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_workflow_data .

  DATA: fl_wi_rh_task TYPE swr_wihdr-wi_rh_task,
        fl_objkey  TYPE swotobjid-objkey,
        fl_objtype TYPE swotobjid-objtype VALUE 'BUS2013',
        lt_swr_wihdr TYPE TABLE OF swr_wihdr,
        wa_swr_wihdr LIKE LINE OF lt_swr_wihdr,
        wa_swr_widtl TYPE swr_widtl,
        lt_task_wihdr TYPE TABLE OF swr_wihdr,
        lt_task_wf TYPE TABLE OF swr_wihdr,
        lt_task_ts TYPE TABLE OF swr_wihdr,
        ls_task TYPE swr_wihdr,
        lt_swr_cont TYPE TABLE OF swr_cont,
        ls_swr_cont LIKE LINE OF lt_swr_cont,
        ls_swr_widtl TYPE swr_widtl,
        lv_reject TYPE xfeld.

  LOOP AT lt_report INTO ls_report.
    fl_objkey = ls_report-ebeln.

    CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
    EXPORTING
*        OBJECT_POR                     =
       objtype                        = fl_objtype
       objkey                         = fl_objkey
       top_level_items                = ' '
       selection_status_variant       = 0000 "All instances of workflow
*        TIME                           =
*        TEXT                           = 'X'
*        OUTPUT_ONLY_TOP_LEVEL          = ' '
*        LANGUAGE                       = SY-LANGU
*        DETERMINE_TASK_FILTER          = 'X'
*        REMOVED_OBJECTS                = ' '
*      IMPORTING
*        RETURN_CODE                    =
         TABLES
*        TASK_FILTER                    =
           worklist                       = lt_task_wihdr
*        MESSAGE_LINES                  =
*        MESSAGE_STRUCT                 =
                 .
    lt_task_wf[] = lt_task_wihdr[].
    lt_task_ts[] = lt_task_wihdr[].
    DELETE lt_task_wf WHERE wi_rh_task <> 'WS02000015'.
    DELETE lt_task_ts WHERE wi_rh_task <> 'TS02000078'.
    IF lt_task_wf[] IS NOT INITIAL.
      READ TABLE lt_task_wf INTO ls_task INDEX 1.
      ls_report-wi_id = ls_task-wi_id.
      ls_report-wi_cd = ls_task-wi_cd.
      ls_report-wi_creator = ls_task-wi_creator+2(35).
      ls_report-wi_stat = ls_task-wi_stat.
      ls_report-wi_item_stat = ls_task-statustext.
    ENDIF.
    CLEAR ls_task.
    SORT lt_task_ts BY wi_id wi_cd wi_ct.
    LOOP AT lt_task_ts INTO ls_task.
      CLEAR: lt_swr_cont.
      CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
           EXPORTING
             workitem_id                    =  ls_task-wi_id
*           LANGUAGE                       = SY-LANGU
*           USER                           = SY-UNAME
*         IMPORTING
*           RETURN_CODE                    =
*           IFS_XML_CONTAINER              =
*           IFS_XML_CONTAINER_SCHEMA       =
          TABLES
            simple_container               = lt_swr_cont
*           MESSAGE_LINES                  =
*           MESSAGE_STRUCT                 =
*           SUBCONTAINER_BOR_OBJECTS       =
*           SUBCONTAINER_ALL_OBJECTS       =
                   .
      CLEAR lv_reject.
      LOOP AT lt_swr_cont INTO ls_swr_cont.
        IF ls_swr_cont-element = '_WI_COMP_EVENT_NAME' AND
           ls_swr_cont-value   = 'ZRELEASEREJECT'.
          ls_report-wi_aagent = space.
          lv_reject = 'X'.
          EXIT.
        ENDIF.
        IF ls_swr_cont-element = '_WI_ACTUAL_AGENT'.
          ls_report-wi_aagent = ls_swr_cont-value+2(35).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF lv_reject IS INITIAL AND
       ls_report-wi_aagent IS NOT INITIAL.
      CLEAR ls_swr_widtl.
      CALL FUNCTION 'SAP_WAPI_GET_WORKITEM_DETAIL'
              EXPORTING
                workitem_id           = ls_task-wi_id
*                USER                  = SY-UNAME
*                LANGUAGE              = SY-LANGU
             IMPORTING
                workitem_detail       = ls_swr_widtl
*                RETURN_CODE           =
*              TABLES
*                MESSAGE_LINES         =
*                MESSAGE_STRUCT        =
                      .
      ls_report-wi_led = ls_swr_widtl-wi_aed. "wi_led.
    ENDIF.
    IF ls_report-wi_led IS NOT INITIAL.
      ls_report-wi_days = ls_report-wi_led - ls_report-wi_cd.
    ENDIF.
    MODIFY lt_report FROM ls_report TRANSPORTING
                                    wi_id wi_cd wi_creator
                                    wi_stat wi_item_stat
                                    wi_aagent wi_led wi_days.
  ENDLOOP.

ENDFORM.                    " GET_WORKFLOW_DATA
