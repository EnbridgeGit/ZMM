* 2000/10/10 mdemeest 4.6B Copied M07DROP to ZNMIM001
* 2013/01/28 btboundy 600 to 605 Upgrade, re-coppied "UGL" code into SAP Standard

***INCLUDE M07DRTOP .
PROGRAM sapm07dr MESSAGE-ID m7.

DATA  lgortsplit LIKE am07m-xselk.
DATA  ladr LIKE sadr.
DATA  *ladr LIKE sadr.

INCLUDE: mm07mabc.                   "Equates für Alphabet
INCLUDE: m07drtop_pdf.     "Tabellen und Datendeklarationen PDF "1098706

TABLES:
   afko,
   afpo,                                               "note 312712
   aufk,
   itcpo,
   mkpf,
  *mkpf,
   mseg,
  *mseg,
   ekko,
   ekkn,
  *ekko,
   ekpo,
   ekbe,
   lfa1,
  *lfa1,
   thead,
   am07m,
  *am07m,
   t001,
   t001w,
  *t001w,
   t027b,
   t027c,
   t027d,
   t156,
   t156t,
   t157e,
  *t157e,
   t159m,
   t159n,
   t159o,
   t159p,
   t159s,
   t159e,
   t024,
   t024d,
  *t024,
   t064b,
   nast,
  *nast,
  tnapr.

TABLES: mabdr,
        mtcom,
        mtcor,
        twlad.

*--------------------------  UGL Changes  -------------------------- UGL
* the following is for printing out the serial number in TRANSFER    UGL
TABLES:  equi.                                                      "UGL

* this table is for the RETURN document (movement 935)               UGL
TABLES: zmwap.                                                      "UGL

* this table is for sorting the items in order of bin location       UGL
TABLES: mard.                                                       "UGL

* this table is for getting the storage location name                UGL
TABLES: t001l.                                                      "UGL

* this table is used for outputting zmwap data                       UGL
TABLES: zmwaps.                                                     "UGL

* this table is used to import zmwap data from cluster tables        UGL
TABLES: indx.                                                       "UGL

* the following are for printing out serial numbers in TRANSFER      UGL
DATA: BEGIN OF zequi OCCURS 10.                                     "UGL
        INCLUDE STRUCTURE equi.                                       "UGL
DATA: END OF zequi.                                                 "UGL

*-------------------------- End of UGL Changes  -------------------- UGL

*---------------------------------------------------------------------*
* Reportinterne Daten                                                 *
*---------------------------------------------------------------------*

*--- Strukturen ------------------------------------------------------*
DATA: x_protab TYPE c,
      x_bwart TYPE c,
      bwart LIKE mseg-bwart,
      eintraege LIKE sy-tfill,
      anzahl LIKE mseg-weanz,
      anzahl1(7) TYPE n,
      x_kont TYPE c,               "Mehrfachkontierung
      x_kont1 TYPE c,              "Einfache Kontierung (WE unbewertet)
      x_form TYPE c,
      x_open TYPE c.

DATA: BEGIN OF jahr,
        jahr(4) TYPE n,
        monat(2) TYPE n,
        tag(2) TYPE n,
      END OF jahr.
DATA:
   BEGIN OF we,
     version1 VALUE '1',            "Standardversion
     version2 VALUE '2',            "Version mit Prüftext
     version3 VALUE '3',            "WE-Sammelschein
   END OF we,

   BEGIN OF alt,
      matnr LIKE mseg-matnr,
      bwart LIKE mseg-bwart,
      ebeln LIKE mseg-ebeln,
      usnam LIKE mkpf-usnam,
   END OF alt,

   BEGIN OF kunde,
      kdauf LIKE mseg-kdauf,
      kdpos LIKE mseg-kdpos,
      kdein LIKE mseg-kdein,
   END OF kunde,

   BEGIN OF anlage,
      anln1 LIKE mseg-anln1,
      anln2 LIKE mseg-anln2,
   END OF anlage,

   BEGIN OF belpos,
      mblnr LIKE mseg-mblnr,
      zeile LIKE mseg-zeile,
   END OF belpos,

    xskkz,
    xpsty,

    xkdanr    VALUE 'C',
    xkde      VALUE 'E',
    xfert     VALUE 'F',
    xrvkdanr  VALUE 'A',
    xanlage   VALUE 'A',
    xanln1    VALUE 'I',
    xkostl    VALUE 'K',
    xprojn    VALUE 'P',
    xmunbw    VALUE 'M',
    xumlag    VALUE 'U',
    xvbelg    VALUE 'V',
    xnplan    VALUE 'N',
    xkonsi(2) VALUE 'KB',
    xwabel(2) VALUE 'WA',
    xwibel(2) VALUE 'WI',
    xwebel(2) VALUE 'WE',

     xsele,                             "Position selektiert
     xkont,                             "Kontierungswechsel
     xsamm,                             "Sammel-WE-Schein
     xanha.                             "Mehrfachkontierung

DATA BEGIN OF theader.
        INCLUDE STRUCTURE thead.
DATA END OF theader.

* Global structures to print Multi-step returns data
DATA: gs_msr_head TYPE msr_s_print_head,
      gs_msr_item TYPE msr_s_print_item.

*--- Interne Tabellen -------------------------------------------------*
DATA:
   BEGIN OF dummy OCCURS 0,
     dummy,
   END OF dummy.

DATA:
   BEGIN OF ausgabe OCCURS 20,
      mblnr LIKE mseg-mblnr,
   END OF ausgabe.

DATA: BEGIN OF xekkn OCCURS 50.
        INCLUDE STRUCTURE ekkn.
DATA: END OF xekkn.

DATA: BEGIN OF xmseg OCCURS 50.
        INCLUDE STRUCTURE mseg.
DATA: END OF xmseg.

DATA: BEGIN OF dtext OCCURS 1.
        INCLUDE STRUCTURE tline.
DATA: END OF dtext.

DATA: BEGIN OF dktext OCCURS 1.
        INCLUDE STRUCTURE tline.
DATA: END OF dktext.

DATA: BEGIN OF dptext OCCURS 1.
        INCLUDE STRUCTURE thead.
DATA: END OF dptext.

DATA: BEGIN OF nast_key,
        mblnr LIKE mkpf-mblnr,
        mjahr LIKE mkpf-mjahr,
        zeile LIKE mseg-zeile,
      END OF nast_key.

DATA: BEGIN OF beltab OCCURS 20.
        INCLUDE STRUCTURE mseg.
DATA: vgart LIKE mkpf-vgart,
      blart LIKE mkpf-blart,
      blaum LIKE mkpf-blaum,
      bldat LIKE mkpf-bldat,
      budat LIKE mkpf-budat,
      cpudt LIKE mkpf-cpudt,
      cputm LIKE mkpf-cputm,
      aedat LIKE mkpf-aedat,
      usnam LIKE mkpf-usnam,
      tcode LIKE mkpf-tcode,
      xblnr LIKE mkpf-xblnr,
      bktxt LIKE mkpf-bktxt,
      frath LIKE mkpf-frath,
      frbnr LIKE mkpf-frbnr,
      wever LIKE mkpf-wever,
      kzdru LIKE t156-kzdru,
      END OF beltab.

DATA: BEGIN OF traptab OCCURS 50.
        INCLUDE STRUCTURE mseg.
DATA:   vgart LIKE mkpf-vgart,
        blart LIKE mkpf-blart,
        blaum LIKE mkpf-blaum,
        bldat LIKE mkpf-bldat,
        budat LIKE mkpf-budat,
        cpudt LIKE mkpf-cpudt,
        cputm LIKE mkpf-cputm,
        aedat LIKE mkpf-aedat,
        usnam LIKE mkpf-usnam,
        tcode LIKE mkpf-tcode,
        xblnr LIKE mkpf-xblnr,
        bktxt LIKE mkpf-bktxt,
        frath LIKE mkpf-frath,
        frbnr LIKE mkpf-frbnr,
        wever LIKE mkpf-wever,
        lgpbe LIKE mard-lgpbe,                                      "UGL
        lgpbe_rcv LIKE mard-lgpbe,             "(+)PANUSURI ticket 63176
      END OF traptab.

DATA: BEGIN OF inthead OCCURS 50.
        INCLUDE STRUCTURE thead.
DATA: END OF inthead.

DATA: BEGIN OF intline OCCURS 50.
        INCLUDE STRUCTURE tline.
DATA: END OF intline.

DATA: BEGIN OF intline1 OCCURS 50.
        INCLUDE STRUCTURE tline.
DATA: END OF intline1.
*-------- Hilfsfelder -------------------------------------------------*
DATA: index_z LIKE sy-tabix,
      drucker LIKE rm07m-ldest,
      old_tdform LIKE t159o-tdform,
      r_werks LIKE t001w-werks,
      r_name1 LIKE t001w-name1,
      x_form3 TYPE c,
      x_nopdest TYPE c,
      language LIKE t001w-spras,
      zaehler_m LIKE sy-tabix,
      edruck  TYPE c.

DATA: old_mkpf LIKE mkpf-usnam,
      old_budat LIKE mkpf-budat,
      old_cpudt LIKE mkpf-cpudt,
      old_werks LIKE t001w-werks,
      old_name1 LIKE t001w-name1,
      old_mblnr LIKE mkpf-mblnr,
      old_lifnr LIKE ekko-lifnr,
      old_ebeln LIKE ekko-ebeln,
      old_ekgrp LIKE ekko-ekgrp,
      old_reswk LIKE ekko-reswk,
      old_linam LIKE am07m-name1,
      old_lina2 LIKE am07m-name2,
      old_eknam LIKE t024-eknam,
      old_ektel LIKE t024-ektel,
      old_ematn LIKE mseg-ematn,
      old_lfa1  LIKE lfa1,
      old_lgort LIKE mseg-lgort,
      old_ladr LIKE ladr,

      save_mkpf LIKE mkpf-usnam,
      save_budat LIKE mkpf-budat,
      save_cpudt LIKE mkpf-cpudt,
      save_werks LIKE t001w-werks,
      save_name1 LIKE t001w-name1,
      save_mblnr LIKE mkpf-mblnr,
      save_lifnr LIKE ekko-lifnr,
      save_reswk LIKE ekko-reswk,
      save_ebeln LIKE ekko-ebeln,
      save_ekgrp LIKE ekko-ekgrp,
      save_linam LIKE am07m-name1,
      save_lina2 LIKE am07m-name2,
      save_eknam LIKE t024-eknam,
      save_ektel LIKE t024-ektel,
      save_ematn LIKE mseg-ematn,
      save_lfa1  LIKE lfa1,
      save_lgort LIKE mseg-lgort,
      save_ladr LIKE ladr,

      offwhile TYPE c,
      & TYPE C VALUE '&',
      blank TYPE c VALUE ' ',
      fenster(5) TYPE c VALUE 'WIN  ',
      fcount(2) TYPE c,
      text1(21) TYPE c VALUE 'INTLINE-TDLINE+  (  )',
      text2(22) TYPE c VALUE 'INTLINE1-TDLINE+  (  )',
      text5(22) TYPE c VALUE 'INTLINE1-TDLINE+  (  )',
      hifeld1(21) TYPE c VALUE 'INTLINE-TDLINE+  (10)',
      shift(2) TYPE n,
      fpage LIKE sy-pagno,
      xscreen(1) TYPE c,
      retco LIKE sy-subrc,
      mblnr LIKE mkpf-mblnr,
      zeile LIKE mseg-zeile,
      new_page,
      xkopfdr,                    "Kopf bereits gedruckt
      n_vornr LIKE resb-vornr.    "Netzplanvorgang

* Data declaration for PDF

DATA: gt_mseg TYPE TABLE OF mseg,
      gs_mseg TYPE mseg,
      gt_mabdr TYPE TABLE OF mabdr,
      gs_mabdr TYPE mabdr,
      gt_am07m TYPE TABLE OF am07m,
      gs_am07m TYPE am07m,
      gs_mkpf TYPE mkpf,
      gt_t001w TYPE TABLE OF t001w,
      gs_t001w TYPE t001w,
      gt_t001 TYPE TABLE OF t001,
      gs_t001 TYPE t001,
      gt_pos_zeile TYPE tty_item_details,
      gs_pos_zeile TYPE str_item_details,
      gt_main TYPE tty_itm_dtls,
      gs_main TYPE str_itm_dtls,
      gt_lfa1 TYPE TABLE OF lfa1,
      gs_lfa1 TYPE lfa1,
      gt_t156t TYPE TABLE OF t156t,
      gs_t156t TYPE t156t,
      gt_t156 TYPE TABLE OF t156,
      gs_t156 TYPE t156,
      gs_wlb3_details TYPE str_wlb3_details,
      gt_wlb3_details TYPE tty_wlb3_details,
      gs_we03_details TYPE str_we03_details,
      gt_we03_details TYPE tty_we03_details.
DATA: gs_mat_document   TYPE  mkpf,
      gs_mvt_type       TYPE  t156t,
      gt_items          TYPE  STANDARD TABLE OF ops_sapm07dr_items_pdf,
      gs_items          TYPE  ops_sapm07dr_items_pdf,
      gs_barcode        TYPE  t159p,
      gs_plants         TYPE  werks_d,
      gs_name1          TYPE  name1,
      gs_stor_location  TYPE  mblpo.
