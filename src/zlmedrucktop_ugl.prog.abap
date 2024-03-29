* 2009/01/15 mdemeest TR580 Upgrade ER 6.0/SAPWeaver 7.00   UGL
*                           All changes indicated with UGL  UGL
*                           in rightmost column             UGL
*---------------------------------------------------------- UGL

FUNCTION-POOL medruck.                 "MESSAGE-ID ..

*ENHANCEMENT-POINT LMEDRUCKTOP_04 SPOTS ES_SAPLMEDRUCK STATIC.

TYPE-POOLS: meein, addi, msfo, mmpur.

INCLUDE fmmexdir.
INCLUDE rvadtabl.
INCLUDE fm06pfvd.
INCLUDE fm06lccd.
INCLUDE rm06debg.       "def_makro for active debbuging tool

*---------------------------------------------------------- UGL
TABLES: t052u,                                             "UGL
        ekan.                                              "UGL

DATA:   mthday(4).                                         "UGL
*---------------------------------------------------------- UGL


TABLES: ekko, ekpo, ekpa, eket, *eket, ekek, ekeh, ekes, ekkn,
        eslh, esll, ml_esll, esuh,
        pekko, pekpo, pekpov, pekpos, ekvkp,
        rm06p, rm11p,
        komk, komp, komvd, ekomd,
        cpkme,
        addr1_sel, addr1_val,
        eine, eina, *eine,
        thead, *thead, stxh,
        tinct, ttxit, tmsi2, t001w, t006, t006a, *t006a, *t006,
        t024, t024e, t027a, t027b, t052, t161n, t161, t161m, t001,
        t166a, t165p, t166c, t166k, t166p, t166t, t166u, t165m, t165a,
        t163d, t163p,
        tmamt, tq05, tq05t,
        mara, marc, mt06e, makt, *mara, mdpa, mdpm,
        vbak, vbkd, vbap, *vbkd,
        v_htnm, rampl,
        drad, drat,
        lfa1, lfm1, sadr,
        wtad_buying_print_addi, wtad_buying_print_extra_text,
        econf_out,
        tmppf, fpltdr, rmipm.

RANGES: r1_tdname FOR stxh-tdname,
        r2_tdname FOR stxh-tdname.

CONSTANTS: neu  VALUE '1',
           aend VALUE '2',
           mahn VALUE '3',
           absa VALUE '4',
           lpet VALUE '5',
           lpma VALUE '6',
           aufb VALUE '7',
           lpae VALUE '8',
           lphe VALUE '9',
           lpje VALUE 'A'.                                  "399710

CONSTANTS: default_kalsm LIKE t683-kalsm VALUE 'MS0000',
           default_kalsm_stamm LIKE t683-kalsm VALUE 'MS0001'.

DATA:
 mdpmx  LIKE mdpm  OCCURS 0 WITH HEADER LINE,
 mdsbx  LIKE mdsb  OCCURS 0 WITH HEADER LINE,
 xtmsi2 LIKE tmsi2 OCCURS 0 WITH HEADER LINE,
 xt165p LIKE t165p OCCURS 0 WITH HEADER LINE,
 xt166k LIKE t166k OCCURS 0 WITH HEADER LINE,
 xt166p LIKE t166p OCCURS 0 WITH HEADER LINE,
 xt166a LIKE t166a OCCURS 0 WITH HEADER LINE,
 xthead LIKE thead OCCURS 0 WITH HEADER LINE,
 xekek  LIKE ekek  OCCURS 0 WITH HEADER LINE,
 tekpo  LIKE ekpo  OCCURS 0 WITH HEADER LINE,
 tesll  LIKE esll  OCCURS 0 WITH HEADER LINE,               "737535
 teket  LIKE eket  OCCURS 0 WITH HEADER LINE,
 uekek  LIKE ekek  OCCURS 0 WITH HEADER LINE,
 tkomk  LIKE komk  OCCURS 0 WITH HEADER LINE,
 tkomv  LIKE komv  OCCURS 0 WITH HEADER LINE,
 d_tkomv          LIKE komv       OCCURS 0 WITH HEADER LINE,
 tkomvd           LIKE komvd      OCCURS 0 WITH HEADER LINE,
 d_tkomvd         LIKE komvd      OCCURS 0 WITH HEADER LINE,
 tekomd           LIKE ekomd      OCCURS 0 WITH HEADER LINE,
 mpos_tab         LIKE mpos       OCCURS 0 WITH HEADER LINE,
 zykl_tab         LIKE mmpt       OCCURS 0 WITH HEADER LINE,
 xpekpov          LIKE pekpov     OCCURS 0 WITH HEADER LINE,
 tconf_out        LIKE econf_out  OCCURS 0 WITH HEADER LINE,
 addr_groups      LIKE adagroups  OCCURS 0 WITH HEADER LINE,
 error_table      LIKE addr_error OCCURS 0 WITH HEADER LINE,
 htnamp           LIKE rampl      OCCURS 0 WITH HEADER LINE,
 tfpltdr          LIKE fpltdr     OCCURS 0 WITH HEADER LINE,
 leistung_thead   LIKE stxh       OCCURS 0 WITH HEADER LINE,
 gliederung_thead LIKE stxh       OCCURS 0 WITH HEADER LINE,
 gliederung       LIKE ml_esll    OCCURS 0 WITH HEADER LINE,
 leistung         LIKE ml_esll    OCCURS 0 WITH HEADER LINE.

DATA: l_addis_in_orders TYPE LINE OF addi_buying_print_itab
                                        OCCURS 0 WITH HEADER LINE.
DATA: variablen TYPE msfo_tab_variablen WITH HEADER LINE.

DATA:
  formel TYPE  msfo_formel,
  addr_fields    LIKE sadrfields,
  addr_reference LIKE addr_ref,
  result         LIKE itcpp,
  intnast        LIKE snast,
  dkomk          LIKE komk,
  xdrflg         LIKE t166p-drflg,
  h_menge_etfz   LIKE eket-wemng,      "Einteilungs-FZ
  h_menge_offen  LIKE eket-wemng,      "offene Einteilungsmenge
  h_menge_aend   LIKE eket-wemng,      "Veränderung der Einteilungsmenge
  xpekpo         LIKE pekpo,
  qv_text_i      LIKE tq09t-kurztext,  "Bezeichnung Qualitätsvereinb.
  tl_text_i      LIKE tq09t-kurztext,  "Bezeichnung techn. Lieferb.
  lvs_recipient  LIKE swotobjid,       "Internet
  lvs_sender     LIKE swotobjid,       "Internet
  lvs_comm_type     TYPE   ad_comm,
  lvs_comm_values   TYPE   szadr_comm_values,
  xaend             TYPE meein_xaend,
  xaetx             TYPE meein_xaetx,
  xdruvo            TYPE druvo,
  dunwitheket       TYPE xfeld,      "flag for dunning with eket 384808
  zg_kz,                               "Zeugnis erforderlich
  timeflag,
  xprotect,
  xfz,
  sto_flag,                                                 "670912
  date_on_item,                                              "Fashion
  kopfkond,
  spoolid(10),                         "Spoolidnummer
  save_el(30),
  sum-euro-price LIKE komk-fkwrt_euro,                      "# 339347
  euro-price LIKE ekpo-effwr,
  etuhrtxt       LIKE rm06p-phtxt,                          " 390056
  elementn(30).                        "Name des Elements


*- Key für HEKET ------------------------------------------------------*
DATA: BEGIN OF heket OCCURS 10.
        INCLUDE STRUCTURE eket.
DATA:       tflag LIKE sy-calld,
      END OF heket.

DATA: BEGIN OF heketkey,
         mandt LIKE eket-mandt,
         ebeln LIKE eket-ebeln,
         ebelp LIKE eket-ebelp,
         etenr LIKE eket-etenr,
      END OF heketkey.

*- HTN-Abwicklung
DATA: BEGIN OF htnmat OCCURS 0.
        INCLUDE STRUCTURE v_htnm.
DATA:  revlv LIKE rampl-revlv,
      END OF htnmat.

*- Struktur des Archivobjekts -----------------------------------------*
DATA: BEGIN OF xobjid,
        objky  LIKE nast-objky,
        arcnr  LIKE nast-optarcnr,
      END OF xobjid.

* Struktur für zugehörigen Sammelartikel
DATA: BEGIN OF sekpo.
        INCLUDE STRUCTURE ekpo.
DATA:   first_varpos,
      END OF sekpo.

*- Key für xekpo ------------------------------------------------------*
DATA: BEGIN OF xekpokey,
         mandt LIKE ekpo-mandt,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
      END OF xekpokey.

*- Key für Änderungsbelegtabelle------------------------------------
DATA: BEGIN OF xaendkey,
         ebelp LIKE ekpo-ebelp,
         zekkn LIKE ekkn-zekkn,
         etenr LIKE eket-etenr,
         ctxnr LIKE t166c-ctxnr,
         rounr LIKE t166c-rounr,
         insert,
         flag_adrnr,
      END OF xaendkey.

*- Tabelle der Zahlungbedingungen--------------------------------------*
DATA: BEGIN OF zbtxt OCCURS 5,
         line(50),
      END OF zbtxt.

*- Key für Texttabelle -------------------------------------------------
DATA: BEGIN OF xtheadkey,
         tdobject LIKE thead-tdobject,
         tdname LIKE thead-tdname,
         tdid LIKE thead-tdid,
      END OF xtheadkey.

DATA: BEGIN OF thead_key,
        mandt    LIKE sy-mandt,
        tdobject LIKE stxh-tdobject,
        tdname   LIKE stxh-tdname,
        tdid     LIKE stxh-tdid,
        tdspras  LIKE stxh-tdspras.
DATA: END OF thead_key.

*- Tabelle der verknüpften Dokumente -----------------------------------
DATA: BEGIN OF doktab OCCURS 0.
        INCLUDE STRUCTURE drad.
DATA  dktxt LIKE drat-dktxt.
DATA: END OF doktab.

*-  Key für QM-Texte
DATA: BEGIN OF qm_text_key OCCURS 5,
         tdobject LIKE thead-tdobject,
         tdname LIKE thead-tdname,
         tdid LIKE thead-tdid,
         tdtext LIKE ttxit-tdtext,
      END OF qm_text_key.

*- Preview indicator for Enjoy-Transaction
DATA: enjpreview TYPE c.

INCLUDE wstr_definition. "Holds BADI global definition
*ENHANCEMENT-POINT LMEDRUCKTOP_03 SPOTS ES_SAPLMEDRUCK STATIC.
*BOI by PANUSURI Ticket 57610
*DATA: gwa_ekpo TYPE ekpo.  "(-)PANUSURI ticket 88669              "UGL
*EOI by PANUSURI Ticket 57610

DATA: sadr1 TYPE sadr."(+)PANUSURI ticket 71397                     "UGL
