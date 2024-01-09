REPORT rm06bb20 NO STANDARD PAGE HEADING LINE-SIZE 132.
*  82148  4.0A  27.08.97   CF
*  82269  3.1I  10.10.97   CF
*  95303  4.0B  11.02.98   KB

*----------------------------------------------------------------------
* CHANGES for UGL
* 2008/04/18 mdemeest - requested by Deb Bossy - add date & time
*                     - Copied RM06BB20 to ZRM06BB20 - source & texts
*                     - changes indicated by UGL
*----------------------------------------------------------------------*
*        Tabellen                                                      *
*----------------------------------------------------------------------*
TABLES: eban, rm06b, t001w, t001k, lfm1, mt06e, mtcom, t165, ebkn,
        prps, resb.
*----------------------------------------------------------------------*
*        Strukturen für MEMORY                                         *
*----------------------------------------------------------------------*
INCLUDE rm06bbco.
INCLUDE fm06lcim.

DATA: omess(100).
DATA: hreswk LIKE eban-reswk.
DATA: hflief LIKE eban-flief.
DATA: zpos LIKE sy-fdpos,
      hpos LIKE sy-fdpos,
      epos LIKE sy-fdpos,
      wpos LIKE sy-fdpos,
      ppos LIKE sy-fdpos.

*----------------------------------------------------------------------*
* Select-Options                  (Änderung: 31.03.98, Robert Fischer) *
*----------------------------------------------------------------------*
SELECT-OPTIONS:
   s_ekgrp FOR eban-ekgrp MEMORY ID ekg,
   s_ekorg FOR eban-ekorg MEMORY ID eko,
   s_flief FOR eban-flief MATCHCODE OBJECT kred MEMORY ID lif,
   s_konnr FOR eban-konnr MATCHCODE OBJECT mekk,
   s_werks FOR eban-werks MEMORY ID wrk,
   s_reswk FOR eban-reswk,
   s_beswk FOR eban-beswk MEMORY ID bwk,  "CCP
   s_banpr FOR eban-banpr.                "DCM

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-009.
"NO INTERVALS.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_gekgrp LIKE rm06a-p_grupp.          "Purchasing group
SELECTION-SCREEN COMMENT  3(35) text-s01.
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_glfdat LIKE rm06a-p_grupp.          "Delivery date
SELECTION-SCREEN COMMENT 42(35) text-s05.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_gwerks LIKE rm06a-p_grupp.          "Plant
SELECTION-SCREEN COMMENT  3(35) text-s02.
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_gltsnr LIKE rm06a-p_grupp.          "Vendor subrange
SELECTION-SCREEN COMMENT 42(35) text-s06.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_glgort LIKE rm06a-p_grupp.          "Storage location
SELECTION-SCREEN COMMENT  3(35) text-s04.
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_gbanfn LIKE rm06a-p_grupp.          "Requisition
SELECTION-SCREEN COMMENT 42(35) text-s09.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_gpstyp LIKE rm06a-p_grupp.          "Item category
SELECTION-SCREEN COMMENT  3(35) text-s03.
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_gbnfpo LIKE rm06a-p_grupp.          "Requisition
SELECTION-SCREEN COMMENT 42(35) text-s07.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_gbukrs LIKE rm06a-p_grupp DEFAULT 'X'.  "Company code
SELECTION-SCREEN COMMENT  3(35) text-s10.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-016.
"NO INTERVALS.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_eterz LIKE rm06a-p_eterz."Generate sched. lines
SELECTION-SCREEN COMMENT 3(35) text-s08.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_poserr LIKE rm06a-p_poserr.         "Omit faulty items
SELECTION-SCREEN COMMENT  3(35) text-s11.
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_test   LIKE rm06a-p_testl.          "Test run
SELECTION-SCREEN COMMENT 42(35) text-s22.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_detpro LIKE rm06a-p_dprot.          "Detailed protocol
SELECTION-SCREEN COMMENT  3(35) text-s21.
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_sebakz LIKE rm06a-set_ebakz DEFAULT '1'.  "EBAKZ
SELECTION-SCREEN COMMENT 42(35) text-s23.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS:
   s_matkl FOR eban-matkl,
   s_frgdt FOR eban-frgdt,             "Freigabedatum
   s_lfdat FOR eban-lfdat,             "Lieferdatum
   s_dispo FOR eban-dispo,             "Disponent
   s_banfn FOR eban-banfn MATCHCODE OBJECT mban,
   s_matnr FOR eban-matnr MATCHCODE OBJECT mat1,
   s_bednr FOR eban-bednr,
   s_bsart FOR eban-bsart,             "Bestellart
   s_pstyp FOR rm06b-epstp,            "Positionstyp
   s_knttp FOR eban-knttp,             "Kontierungstyp
   s_statu FOR eban-statu.             "Bearbeitungsstatus

*----------------------------------------------------------------------*
*        Parameters                                                    *
*----------------------------------------------------------------------*
PARAMETERS:
   p_afnam LIKE eban-afnam,            "Anforderer
   p_txz01 LIKE eban-txz01,            "Kurztext
   p_vrtypk LIKE rm06a-p_vrtyp DEFAULT 'X'.

*- für Positionstyp im internen Format --------------------------------*
RANGES: r_pstyp FOR eban-pstyp.
INCLUDE fm06bcs3.


* Begin CCP
*----------------------------------------------------------------------*
*          Make field BESWK invisible if CCP process is not active     *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  DATA: lf_ccp_active.
  CALL FUNCTION 'ME_CCP_ACTIVE_CHECK'
    IMPORTING
      ef_ccp_active = lf_ccp_active.
  IF lf_ccp_active IS INITIAL.
    LOOP AT SCREEN.
      SEARCH screen-name FOR 'S_BESWK'.
      IF sy-subrc EQ 0.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
* End CCP


AT SELECTION-SCREEN ON s_pstyp.
  CALL FUNCTION 'ME_ITEM_CATEGORY_SELOPT_INPUT'
    TABLES
      ext_pstyp = s_pstyp
      int_pstyp = r_pstyp.

*----------------------------------------------------------------------*
*  F4 auf dem Selektionsbild / F4 on the selection screen              *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_pstyp-low.

  CALL FUNCTION 'HELP_VALUES_EPSTP'
       EXPORTING
            program = sy-cprog
            dynnr   = sy-dynnr
            fieldname = 'S_PSTYP-LOW'
*            BSART   =
*            BSTYP   =
       IMPORTING
            epstp   = s_pstyp-low
       EXCEPTIONS
            OTHERS  = 1.

* Anschluß Immobilienverwaltung 4.6A
* F4-Hilfe für Feld Objektart bei Feldern für Immobilienverwaltung
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_obart-low.
  INCLUDE ifviimslf4.

INITIALIZATION.

  DATA: l_set_variant  LIKE rsvar-variant VALUE 'SAP&STANDARD',
        l_report       LIKE rsvar-report  VALUE 'RM06BB20'.


  IF sy-slset IS INITIAL.
* set variant for report
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = l_report
        variant              = l_set_variant
      EXCEPTIONS
        variant_not_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

START-OF-SELECTION.

*- Lesen Bestellanforderungen -----------------------------------------*

  PERFORM get_re_sel_options.                               "CF 4.6A

  RANGES: r_zugba FOR eban-zugba,
          r_vrtyp FOR eban-vrtyp,
          r_bsakz FOR eban-bsakz,
          r_loekz FOR eban-loekz,
          r_ebakz FOR eban-ebakz,
          r_afnam FOR eban-afnam,
          r_txz01 FOR eban-txz01.
*DCM no blocked items
  RANGES: r_blckd FOR eban-blckd.
  r_blckd-sign   = 'I'.
  r_blckd-option = 'EQ'.
  r_blckd-low    = ' '.
  APPEND r_blckd.

  r_zugba-sign   = 'I'.
  r_zugba-option = 'EQ'.
  r_zugba-low    = 'X'.
  APPEND r_zugba.
  IF p_vrtypk NE space.
    r_vrtyp-sign   = 'I'.
    r_vrtyp-option = 'NE'.
    r_vrtyp-low    = 'L'.
    APPEND r_vrtyp.
  ELSE.
    r_vrtyp-sign   = 'I'.
    r_vrtyp-option = 'EQ'.
    r_vrtyp-low    = space.
    APPEND r_vrtyp.
  ENDIF.
  r_bsakz-sign   = 'I'.
  r_bsakz-option = 'NE'.
  r_bsakz-low    = 'R'.
  APPEND r_bsakz.
  r_loekz-sign   = 'I'.
  r_loekz-option = 'EQ'.
  r_loekz-low    = ' '.
  APPEND r_loekz.
  r_ebakz-sign   = 'I'.
  r_ebakz-option = 'EQ'.
  r_ebakz-low    = ' '.
  APPEND r_ebakz.
  IF p_afnam NE space.
    r_afnam-sign   = 'I'.
    r_afnam-option = 'CP'.
    r_afnam-low    = p_afnam.
    APPEND r_afnam.
  ENDIF.
  IF p_txz01 NE space.
    r_txz01-sign   = 'I'.
    r_txz01-option = 'CP'.
    r_txz01-low    = p_txz01.
    APPEND r_txz01.
  ENDIF.


  CALL FUNCTION 'ME_READ_EBAN_MULTIPLE'
*    EXPORTING
*         I_DYNSEL            =
*         I_DYNFIE            =
*    IMPORTING
*         E_REQS_WITHOUT_AUTH =
       TABLES
            te_eban             = ban
*         TE_EBKN             =
*         TI_TACT             =
            ti_banfn_range      = s_banfn
            ti_matnr_range      = s_matnr
            ti_matkl_range      = s_matkl
            ti_werks_range      = s_werks
            ti_ekgrp_range      = s_ekgrp
            ti_bsakz_range      = r_bsakz
            ti_bsart_range      = s_bsart
            ti_pstyp_range      = r_pstyp
            ti_knttp_range      = s_knttp
            ti_lfdat_range      = s_lfdat
            ti_frgdt_range      = s_frgdt
            ti_dispo_range      = s_dispo
            ti_statu_range      = s_statu
            ti_flief_range      = s_flief
            ti_bednr_range      = s_bednr
            ti_konnr_range      = s_konnr
            ti_reswk_range      = s_reswk
            ti_ekorg_range      = s_ekorg
            ti_vrtyp_range      = r_vrtyp
            ti_txz01_range      = r_txz01
            ti_afnam_range      = r_afnam
            ti_kostl_range      = s_kostl
            ti_aufnr_range      = s_aufnr
            ti_anln1_range      = s_anln1
            ti_anln2_range      = s_anln2
            ti_vbeln_range      = s_vbeln
            ti_vbelp_range      = s_vbelp
            ti_nplnr_range      = s_nplnr
            ti_vornr_range      = s_vornr
            ti_zugba_range      = r_zugba
            ti_eban_loekz_range      = r_loekz
            ti_ebkn_loekz_range      = r_loekz
            ti_ebakz_range      = r_ebakz

            ti_beswk_range      = s_beswk
            ti_blckd_range      = r_blckd
            ti_banpr_range      = s_banpr
***           TI_FRGRL_RANGE      =
***           TI_GSFRG_RANGE      =
**            TI_PS_PSP_PNR_RANGE =
            ti_pspid_ext_range  = s_psext
            ti_imkeys           = t_imkeys.
* ELSE.
*   SELECT * FROM EBAN APPENDING TABLE BAN
*                      WHERE EKGRP IN S_EKGRP
*                        AND ZUGBA EQ 'X'
*                        AND EKORG IN S_EKORG
*                        AND FLIEF IN S_FLIEF
*                        AND KONNR IN S_KONNR
*                        AND WERKS IN S_WERKS
*                        AND RESWK IN S_RESWK
*                        AND BSAKZ NE 'R'
*                        AND VRTYP NE 'L'
*                        AND LOEKZ EQ SPACE
*                        AND EBAKZ EQ SPACE
*                        AND BANFN IN S_BANFN
*                        AND MATNR IN S_MATNR
*                        AND MATKL IN S_MATKL
*                        AND BEDNR IN S_BEDNR
*                        AND BSART IN S_BSART
*                        AND PSTYP IN R_PSTYP
*                        AND KNTTP IN S_KNTTP
*                        AND LFDAT IN S_LFDAT
*                        AND FRGDT IN S_FRGDT
*                        AND DISPO IN S_DISPO
*                        AND STATU IN S_STATU.
* ENDIF.
*  Prüfen Bestellanforderungen ----------------------------------------*
  LOOP AT ban.
    eban = ban.
    PERFORM check_para.
    IF sy-subrc NE 0.
      DELETE ban.
    ELSE.
      PERFORM bukrs_umlag.
    ENDIF.
  ENDLOOP.

*- Übergeben Banf's an Bestellgenerierungstransaktion -----------------*
  READ TABLE ban INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s261(me).


    IF sy-batch EQ space AND
       sy-binpt EQ space AND                                "195283
       sy-tcode EQ 'ME59'.
      LEAVE TO TRANSACTION 'ME59'.
*    ELSEIF sy-batch EQ space AND
*           sy-calld EQ space.
*      LEAVE TO TRANSACTION sy-tcode.
    ELSE.
      LEAVE.
    ENDIF.
  ELSE.
    par-poserr = p_poserr.
    par-gekgrp = p_gekgrp.
    par-gwerks = p_gwerks.
    par-glgort = p_glgort.
    par-gpstyp = p_gpstyp.
    par-glfdat = p_glfdat.
    par-gltsnr = p_gltsnr.
    par-gbanfn = p_gbanfn.
    par-gbnfpo = p_gbnfpo.
    par-gbukrs = p_gbukrs.
    par-geterz = p_eterz.
    par-test   = p_test.
    par-set_ebakz = p_sebakz.
    EXPORT ban par TO MEMORY ID 'C-ME59'.
    CALL TRANSACTION 'ME59' AND SKIP FIRST SCREEN.
    IMPORT bpr FROM MEMORY ID 'B-ME59'.
    READ TABLE bpr INDEX 1.
    IF sy-subrc NE 0.
      MESSAGE s261(me).
      IF sy-batch EQ space AND
         sy-binpt EQ space AND                              "195283
         sy-calld EQ space.
        LEAVE TO TRANSACTION sy-tcode.
      ELSE.
        LEAVE.
      ENDIF.
    ELSE.
      PERFORM protokoll.
      FREE MEMORY ID 'B-ME59'.                              "95303/KB
    ENDIF.
  ENDIF.

*eject
*----------------------------------------------------------------------*
*  Prüfen buchungskreisübergreifende Umlagerung                        *
*----------------------------------------------------------------------*
FORM bukrs_umlag.

  DATA: l_ownls             TYPE own_ls,
        l_same_company_code TYPE xfeld.

  CHECK ban-reswk NE space.
  CHECK ban-flief NE space.
  CHECK ban-werks NE ban-reswk.

  CALL FUNCTION 'MMPUR_COMPARE_COMPANY_CODES'
    EXPORTING
      im_plant1            = ban-werks
      im_plant2            = ban-reswk
    IMPORTING
      ex_same_company_code = l_same_company_code
    EXCEPTIONS
      error                = 1.

  IF NOT l_same_company_code IS INITIAL.
    CALL FUNCTION 'AIP01_PLANT_DETERMINE'
      EXPORTING
        i_werks = ban-reswk
      IMPORTING
        e_ownls = l_ownls
      EXCEPTIONS
        OTHERS  = 1.
    IF NOT l_ownls IS INITIAL.
      CLEAR ban-flief.
      MODIFY ban.
    ENDIF.
  ENDIF.

ENDFORM.                    "BUKRS_UMLAG

*----------------------------------------------------------------------*
*        Selektionsparameter prüfen                                    *
*----------------------------------------------------------------------*
FORM check_para.
  sy-subrc = 1.
*- keine vollbestellten Banfs (sicherheitshalber, eigentlich
*- schon über ZUGBA abgefangen, prinzipiell aber auch ZUGBA = 'X'
*- bei Vollbestellung möglich wegen View-Update in LEBNUU01
  CHECK ban-bsmng < ban-menge.
*- keine teilbestellte Banfs aus Vertriebsbeleg -----------------------*
  IF ban-estkz EQ 'V'.
    CHECK ban-bsmng EQ 0.
  ENDIF.
*------- Pruefen Kurztext ---------------------------------------------*
  IF p_txz01 NE space.
    IF eban-txz01 NP p_txz01.
      EXIT.
    ENDIF.
  ENDIF.
*------- keine Dienstleistungspositionen
  IF ban-pstyp EQ '9'.                                      " ab 30f
    IF t165 IS INITIAL.
      SELECT SINGLE * FROM t165.
      sy-subrc = 1.                                         "82269
    ENDIF.
    IF t165-cont_price EQ space.
      sy-subrc = 1.
      EXIT.
    ENDIF.
  ENDIF.

*------- Pruefen Anforderer -------------------------------------------*
  IF p_afnam NE space.
    IF eban-afnam NP p_afnam.
      EXIT.
    ENDIF.
  ENDIF.
*- Keine Banfen mit zugeordneten Kontrakten ---------------------------*
  IF p_vrtypk EQ space.
    CHECK ban-vrtyp NE 'K'.
  ENDIF.
*- Requisition marked for external procurement ? ----------------------*
  CHECK eban-eprofile IS INITIAL.

  CLEAR sy-subrc.

ENDFORM.                    "CHECK_PARA

*----------------------------------------------------------------------*
*        Protokoll ausgeben                                            *
*----------------------------------------------------------------------*
FORM protokoll.

  DATA: l_group_counter TYPE i,                             "159650
        l_bpr_tabix     LIKE sy-tabix.
*- Initialisierung
  CLEAR l_group_counter.

*- Position zusätzlicher Gruppenstufen ermitteln ----------------------*
  zpos = 26.
  IF p_gekgrp NE space.
    epos = zpos.
    zpos = zpos + 4.
  ENDIF.
  IF p_gwerks NE space.
    wpos = zpos.
    zpos = zpos + 5.
  ENDIF.
  IF p_gpstyp NE space.
    ppos = zpos.
    zpos = zpos + 2.
  ENDIF.
  hpos = zpos + 15 + 1 + 100.
  NEW-PAGE LINE-SIZE hpos.

  LOOP AT bpr.
*- Tabix
    l_bpr_tabix = sy-tabix.
*- Gruppenwechsel Lieferant/Lieferwerk --------------------------------*
    IF hflief NE bpr-flief OR
       hreswk NE bpr-reswk.
      IF hreswk NE space OR
         hflief NE space.
        ULINE.
      ENDIF.
      FORMAT COLOR COL_GROUP INTENSIFIED OFF.
      IF bpr-reswk NE space.
        WRITE: / sy-vline,
               2 text-003 COLOR COL_GROUP INTENSIFIED,
                 bpr-reswk.
      ELSE.
        WRITE: / sy-vline,
               2 text-001 COLOR COL_GROUP INTENSIFIED,
                 bpr-flief.
      ENDIF.
      hpos = zpos + 15.
      POSITION hpos.
      WRITE sy-vline.
      hpos = hpos + 1 + 100.
      POSITION hpos.
      WRITE sy-vline.
    ENDIF.
    hflief = bpr-flief.
    hreswk = bpr-reswk.

*- Gruppenwechsel Bestellung ------------------------------------------*
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    IF bpr-banfn EQ space.
*- Neue Gruppe                                                   "159650
      l_group_counter = l_group_counter + 1.
      bpr-group = l_group_counter.
      MODIFY bpr INDEX l_bpr_tabix TRANSPORTING group.
*- Allgemeine Gruppenbegriffe -----------------------------------------*
      WRITE: / sy-vline,
             2 bpr-ekorg,
             7 bpr-bsart.
      IF bpr-konnr EQ space.
        WRITE 12 text-012.
      ELSE.
        WRITE 12 bpr-konnr.
      ENDIF.
*- Zusätzliche Gruppenbegriffe ----------------------------------------*
      IF p_gekgrp NE space.
        POSITION epos.
        WRITE bpr-ekgrp.
      ENDIF.
      IF p_gwerks NE space.
        POSITION wpos.
        WRITE bpr-werks.
      ENDIF.
      IF p_gpstyp NE space.
        CALL FUNCTION 'ME_ITEM_CATEGORY_OUTPUT'
          EXPORTING
            pstyp = bpr-pstyp
          IMPORTING
            epstp = rm06b-epstp.
        POSITION ppos.
        WRITE rm06b-epstp.
      ENDIF.
*- Zähler und Meldung -------------------------------------------------*
      POSITION zpos.
      WRITE:   (7) bpr-zbanf NO-SIGN,
               (7) bpr-zbest NO-SIGN.
      hpos = zpos + 15.
      POSITION hpos.
      WRITE sy-vline.
      hpos = hpos + 1.
      POSITION hpos.
      IF bpr-ebeln NE space.
        FORMAT COLOR COL_POSITIVE.
        WRITE: text-010,
               bpr-ebeln,
               text-011.
      ELSE.
        IF bpr-zbest NE 0 AND
           p_test NE space.
          FORMAT COLOR COL_POSITIVE.
          WRITE: text-015.
        ELSE.
          IF NOT bpr-msgno IS INITIAL.
            PERFORM mzeile USING hpos.
          ELSE.
            FORMAT COLOR COL_NEGATIVE.
            WRITE text-014.
          ENDIF.
        ENDIF.
      ENDIF.
      hpos = hpos + 100.
      POSITION hpos.
      WRITE sy-vline.
      HIDE: bpr-reswk,
            bpr-flief,
            bpr-ekorg,
            bpr-bsart,
            bpr-konnr,
            bpr-group.                                      "159650

*- Detailzeile Banf ---------------------------------------------------*
    ELSE.
      bpr-group = l_group_counter.                          "159650
      MODIFY bpr INDEX l_bpr_tabix TRANSPORTING group.
      CHECK p_detpro NE space.
      WRITE: /  sy-vline,
             12 text-013,
                bpr-banfn,
                bpr-bnfpo.
      hpos = zpos + 15.
      POSITION hpos.
      WRITE sy-vline.
      hpos = hpos + 1.
      IF NOT bpr-msgno IS INITIAL.
        PERFORM mzeile USING hpos.
      ENDIF.
      hpos = hpos + 100.
      POSITION hpos.
      WRITE sy-vline.
    ENDIF.
  ENDLOOP.
  ULINE.
  CLEAR bpr.
ENDFORM.                    "PROTOKOLL
*----------------------------------------------------------------------*
*        Message ausgeben                                              *
*----------------------------------------------------------------------*
FORM mzeile USING mze_posi.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = bpr-msgid
      msgnr               = bpr-msgno
      msgv1               = bpr-msgv1
      msgv2               = bpr-msgv2
      msgv3               = bpr-msgv3
      msgv4               = bpr-msgv4
    IMPORTING
      message_text_output = omess.
  FORMAT COLOR COL_NEGATIVE.
  POSITION mze_posi.
  WRITE omess.
ENDFORM.                    "MZEILE

*eject
*----------------------------------------------------------------------*
*        Zeitpunkte                                                    *
*----------------------------------------------------------------------*

*- Line-selection: Banfs zur Bestellung -------------------------------*
AT LINE-SELECTION.
  NEW-PAGE LINE-SIZE 119.
  CHECK sy-lsind EQ 1.
  CHECK bpr-flief NE space OR
        bpr-reswk NE space.

  LOOP AT bpr WHERE flief EQ bpr-flief AND
                    reswk EQ bpr-reswk AND
                    ekorg EQ bpr-ekorg AND
                    bsart EQ bpr-bsart AND
                    konnr EQ bpr-konnr AND
                    group EQ bpr-group AND                  "159650
                    banfn NE space.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE: / sy-vline,
           2 bpr-banfn,
          12 sy-vline,
          13 bpr-bnfpo,
          18 sy-vline.
    IF NOT bpr-msgno IS INITIAL.
      PERFORM mzeile USING '19'.                        "#EC NO_M_RISC3
    ENDIF.
    WRITE 119 sy-vline.
  ENDLOOP.
  ULINE.
  CLEAR bpr.

*- Top of page --------------------------------------------------------*
TOP-OF-PAGE.

  hpos = zpos + 15 + 1.
  SET LEFT SCROLL-BOUNDARY COLUMN hpos.
  FORMAT COLOR COL_HEADING INTENSIFIED.
  ULINE.
  NEW-LINE NO-SCROLLING.
  WRITE: / text-002.
*- Zusätzliche Gruppenbegriffe ----------------------------------------*
  IF p_gekgrp NE space.
    POSITION epos.
    WRITE text-006.
  ENDIF.
  IF p_gwerks NE space.
    POSITION wpos.
    WRITE text-005.
  ENDIF.
  IF p_gpstyp NE space.
    POSITION ppos.
    WRITE text-007.
  ENDIF.
  POSITION zpos.
  WRITE text-008.
*-------------------------- BEGIN OG UGL CHANGE ------------------- UGL
  hpos = zpos + 15 + 1 + 60.                                      " UGL
  position hpos.                                                  " UGL
  write:  sy-datum, ' @ ', sy-uzeit, '   ', sy-sysid.             " UGL
*----------------------------- END OF UGL CHANGE -----------------  UGL
  hpos = zpos + 15 + 1 + 100.
  POSITION hpos.
  WRITE sy-vline.
  ULINE.
*- Top of page --------------------------------------------------------*
TOP-OF-PAGE DURING LINE-SELECTION.
  SET LEFT SCROLL-BOUNDARY COLUMN 19.
  FORMAT COLOR COL_HEADING INTENSIFIED.
  ULINE.
  NEW-LINE NO-SCROLLING.
  WRITE: / text-004.
  ULINE.
*&---------------------------------------------------------------------*
*&      Form  GET_RE_SEL_OPTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_re_sel_options.

  CHECK NOT s_swenr[]  IS INITIAL OR
        NOT s_sgenr[]  IS INITIAL OR
        NOT s_sgrnr[]  IS INITIAL OR
        NOT s_smenr[]  IS INITIAL OR
        NOT s_smive[]  IS INITIAL OR
        NOT s_svwnr[]  IS INITIAL OR
        NOT s_snksl[]  IS INITIAL OR
        NOT s_sempsl[] IS INITIAL OR
        NOT s_recnnr[] IS INITIAL OR
        NOT s_obart[]  IS INITIAL OR
        NOT p_dvon     IS INITIAL OR
        NOT p_dbis     IS INITIAL.

  CALL FUNCTION 'REMD_GET_IMKEY_FOR_SELECT_OPT'
       EXPORTING
            i_bukrs  = i_bukrs
            i_dstich = p_stich
*            I_DVON   = P_DVON
*            I_DBIS   = P_DBIS
       TABLES
            s_swenr  = s_swenr
            s_sgenr  = s_sgenr
            s_sgrnr  = s_sgrnr
            s_smenr  = s_smenr
            s_smive  = s_smive
            s_svwnr  = s_svwnr
            s_snksl  = s_snksl
            s_empsl  = s_sempsl
            s_recnnr = s_recnnr
            s_obart  = s_obart
            s_vonbis = s_vonbis
            e_imkeys = t_imkeys.

* vorläufig
  DELETE t_imkeys WHERE imkey = space.

ENDFORM.                               " GET_RE_SEL_OPTIONS
