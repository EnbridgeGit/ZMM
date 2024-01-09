************************************************************************
*        Datenteil Modulpool SAPFM06L                                  *
************************************************************************
PROGRAM SAPWE06L MESSAGE-ID ME.

*----------------------------------------------------------------------*
*        DB - Tabellen
*----------------------------------------------------------------------*
DATA: BEGIN OF XEKEK       OCCURS 20.         "added 1999/06/01 mdemeest
        INCLUDE STRUCTURE UEKEK      .        "from fm06ecdv
DATA: END OF XEKEK      .

DATA: BEGIN OF YEKEK       OCCURS 20.
        INCLUDE STRUCTURE UEKEK      .
DATA: END OF YEKEK      .

DATA: UPD_EKEK      .
*----------------------------------------------"end of add ------------*

INCLUDE FM06LTO1.
TABLES: EKAB,
        EKBE,
        CDHDR,
        LFA1,
        KOMK,
        MKPF,
        MARA,
        MTCOM,
        RESB.

TABLES: RBKP,
        KNA1.                      "Add for upgrade 1999/06/01  mdemeest
*----------------------------------------------------------------------*
*        ATAB- Tabellen
*----------------------------------------------------------------------*
TABLES:  TTXIT,
         TPRG,
         T001, *T001,
         T001K, *T001K,
         T001W, *T001W,
         T160B,
         T160L,
         T161A,
         T163C,
         T16FC,
         T16FD,
         T16FG.

*----------------------------------------------------------------------*
*        Interne Tabellen
*----------------------------------------------------------------------*
*------- Tabelle der Köpfe --------------------------------------------*
DATA: BEGIN OF LEKKO OCCURS 20.
        INCLUDE STRUCTURE EKKO.
DATA:   Z_NETWR LIKE EKPO-NETWR.            "JLEE 11/18/1996
DATA: END OF LEKKO.
*------- Tabelle der Belegnummern für Prefetch-------------------------*
DATA: BEGIN OF IT_EKKO OCCURS 20.
        INCLUDE STRUCTURE EKBEL.
DATA:   Z_NETWR LIKE EKPO-NETWR.            "JLEE 11/18/1996
DATA: END OF IT_EKKO.

*------- Tabelle der Positionen ---------------------------------------*
DATA: BEGIN OF LEKPO OCCURS 60.
        INCLUDE STRUCTURE EKPO.
DATA: END OF LEKPO.
*------- Tabelle der Belegpositionen für Prefetch----------------------*
DATA: BEGIN OF IT_EKPO OCCURS 20.
        INCLUDE STRUCTURE EKBEL.
DATA: END OF IT_EKPO.

*------- Tabelle der Lieferanten --------------------------------------*
DATA: BEGIN OF LLFA1 OCCURS 20.
        INCLUDE STRUCTURE LFA1.
DATA: END OF LLFA1.
DATA: BEGIN OF LFA1KEY,
         MANDT LIKE EKKO-MANDT,
         LIFNR LIKE EKKO-LIFNR,
      END OF LFA1KEY.
*------- Tabelle der Lieferantennummern für Prefetch-------------------*
DATA: BEGIN OF IT_LFA1 OCCURS 20,
        LIFNR LIKE LFA1-LIFNR,
      END OF IT_LFA1.

*------- Tabelle der Unterpositionen ----------------------------------*
DATA: BEGIN OF UPOT OCCURS 10.
        INCLUDE STRUCTURE EKPO.
DATA: END OF UPOT.

*------- Tabelle der Einteilungen -------------------------------------*
DATA: BEGIN OF ETT OCCURS 10.
        INCLUDE STRUCTURE BEKET.
DATA: END OF ETT.

*------- Tabelle der Einteilungen (DB-Stand) für Funktionsbausteine ---*
DATA: BEGIN OF YEKET OCCURS 1.
        INCLUDE STRUCTURE UEKET.
DATA: END OF YEKET.

*------- Tabelle der Belegkonditionen (nur für Bezugsnebenkosten) -----*
DATA: BEGIN OF TKOMV OCCURS 50.
        INCLUDE STRUCTURE KOMV.
DATA: END OF TKOMV.

*------ Tabelle der Werksdaten ----------------------------------------*
DATA: BEGIN OF IT_T001W OCCURS 0.
        INCLUDE STRUCTURE T001W.
DATA: END OF IT_T001W.

*------- Tabelle der PF-Status ----------------------------------------*
DATA: BEGIN OF PFTAB OCCURS 5,
        PFKEY LIKE SY-PFKEY,
        TITLE(70) TYPE C,
      END OF PFTAB.

*------- Tabelle der ausgeschlossenen Funktionen ----------------------*
DATA: BEGIN OF EXCL OCCURS 5,
        FUNKTION(4),
      END OF EXCL.

*------- Erfassungsmengeneinheiten der Komponenten --------------------*
DATA: BEGIN OF T_ERFME OCCURS 0,
         EBELN LIKE EKBE-EBELN,
         EBELP LIKE EKBE-EBELP,
         MATNR LIKE EKBE-MATNR,
         ERFME LIKE RESB-ERFME,
      END OF T_ERFME.

*------- Feldleiste für Wechsel der BET-Hauptfelder -------------------*
DATA: BEGIN OF CBET,
         LFGJA LIKE EKBE-LFGJA,
         LFBNR LIKE EKBE-LFBNR,
         LFPOS LIKE EKBE-LFPOS,
      END OF CBET.

*------- Feldleiste für Wechsel der BZT-Hauptfelder -------------------*
DATA: BEGIN OF CBZT,
         STUNR LIKE EKBZ-STUNR,
         ZAEHK LIKE EKBZ-ZAEHK,
      END OF CBZT.

*------- Key zum Lesen der Konditionen  -------------------------------*
DATA: BEGIN OF KOMVKEY,
         MANDT LIKE KOMV-MANDT,
         KNUMV LIKE KOMV-KNUMV,
         KPOSN LIKE KOMV-KPOSN,
         STUNR LIKE KOMV-STUNR,
         ZAEHK LIKE KOMV-ZAEHK,
      END OF KOMVKEY.

*------ XNAST/YNAST für Nachrichtensteuerung --------------------------
DATA:    BEGIN OF XNAST OCCURS 50.     " Tabelle der Nachrichten (akt.)
           INCLUDE STRUCTURE VNAST.
*ATA:      SELKZ LIKE DV70A-SELKZ,     " Dynamischer Teil
*          PNAME LIKE DV70A-PNAME,
*          MSGNA LIKE DV70A-MSGNA,
*          UPDAT.
DATA:    END OF XNAST.

DATA:    BEGIN OF YNAST OCCURS 50.     " Tabelle der Nachrichten (alt)
           INCLUDE STRUCTURE NAST.
DATA:    END OF YNAST.

DATA: XOBJKY LIKE NAST-OBJKY,          "Key für Nachrichtensteuerung
      NACHRAPPL LIKE NAST-KAPPL.

*------- Tabelle der Selektionsparameter ------------------------------*
DATA: BEGIN OF XT160S OCCURS 1.
        INCLUDE STRUCTURE T160S.
DATA: END OF XT160S.
*------- Kennzeichen für Selektivität ---------------------------------*
DATA: SELK,                            "Kopf
      SELP.                            "Position.
*------- Kennzeichen für Zusätzliche Zugriffe -------------------------*
DATA: EKABKS,                          "Selektion Abrufdoku Kopf
      EKABKA,                          "Ausgabe   Abrufdoku Kopf
      EKABPS,                          "Selektion Abrufdoku Position
      EKABPA,                          "Ausgabe   Abrufdoku Position
      EKETPS,                          "Selektion Einteilungen Position
      EKETPA,                          "Ausgabe   Einteilungen Position
      EKBEPS,                          "Selektion Bestellentw. Position
      EKBEPA,                          "Ausgabe   Bestellentw. Position
      LFA1KS,                          "Selektion Lieferant Kopf
      LFA1KA.                          "Ausgabe   Lieferant Kopf
DATA GRBSTYP LIKE EKKO-BSTYP.
DATA: SAVEEKKO LIKE EKKO.
DATA: SAVEEKPO LIKE EKPO.
DATA: XAUTH.

*----------------------------------------------------------------------*
*        Feldsymbole
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <F_OLD>, <F_NEW>, <KEY>.

*----------------------------------------------------------------------*
*        Hilfsfelder
*----------------------------------------------------------------------*
*------- Hide-Felder Kopf ---------------------------------------------*
DATA: BEGIN OF HIDK,
        BSTYP LIKE EKKO-BSTYP,
        EBELN LIKE EKKO-EBELN,
        LIFNR LIKE EKKO-LIFNR,
        BUKRS LIKE EKKO-BUKRS,
        EKORG LIKE EKKO-EKORG,
        WAERS LIKE EKKO-WAERS,
        WKURS LIKE EKKO-WKURS,
        RESWK LIKE EKKO-RESWK,
        GJAHR LIKE EKBE-GJAHR,
        KNUMV LIKE EKBE-KNUMV,
        EKBEL LIKE EKBE-EBELN,
        EKPOS LIKE EKBE-EBELP,
        BUZEI LIKE EKBE-BUZEI,
        LOGSY LIKE EKAB-LOGSY,
        EBELD LIKE EKAB-EBELD,
        AENDA,                         "Änderungsbeleganzeigeberechtigun
      END OF HIDK.

*------- Hide-Felder Position -----------------------------------------*
DATA: BEGIN OF HIDP,
        EBELP LIKE EKPO-EBELP,
        BSTAE LIKE EKPO-BSTAE,
        BUKRS LIKE EKPO-BUKRS,
        KTMNG LIKE EKPO-KTMNG,
        MEINS LIKE EKPO-MEINS,
        BPRME LIKE EKPO-BPRME,
        WEBRE LIKE EKPO-WEBRE,
        LOEKZ LIKE EKPO-LOEKZ,              "upgrade 1999/06/01 mdemeest
        OWEMG LIKE RM06A-OWEMG,        "offene WE-Menge
        OREMG LIKE RM06A-OREMG,        "offene RE-Menge
        OWEWR LIKE RM06A-OWEWR,        "offener WE-Wert
        OREWR LIKE RM06A-OREWR,        "offener RE-Wert
        OWEPZ LIKE RM06A-OWEPZ,        "offen Prozent WE
        OREPZ LIKE RM06A-OREPZ,        "offen Prozent RE
        STATU LIKE EKPO-STATU,         "Status
        WERKS LIKE EKPO-WERKS,
        AENDA,                         "Änderungsbeleganzeigeberechtigun
        MARKI,                         "Kennzeichen markierbar
        ATTYP LIKE EKPO-ATTYP,         "Artikeltyp
        PACKNO LIKE EKPO-PACKNO,       "Paketnummer (Dienstleistungen)
        PSTYP  LIKE EKPO-BSTYP,
      END OF HIDP.

*------- Hilfelder für Umrechnungen -----------------------------------*
DATA: BEGIN OF H,
        NETPR LIKE EKPO-NETPR,
        MENGE LIKE EKPO-MENGE,
        AVIMG LIKE EKES-MENGE,
        AVIRL,
        SETMG LIKE RM06A-SETMG,
        LAGMG LIKE RM06A-LAGMG,
        OFZMG LIKE RM06A-OFZMG,
        SWEMG LIKE RM06A-SWEMG,
        WEMNG LIKE EKBES-WEMNG,
        WAMNG LIKE EKBES-WAMNG,
        WERT  LIKE EKKO-KTWRT,
      END OF H.

*-- Indizes zum Markieren Intervall -----------------------------------*
DATA: BEGIN OF MINT,
        BEG LIKE SY-LILLI,
        END LIKE SY-LILLI,
        ACT LIKE SY-LILLI,
      END OF MINT.

*------- Allgemeine Hilfsfelder ---------------------------------------*
DATA: PREISANZ,                          "Preisanzeige
      ARCHIV,                            "Archivauswertung
      ARCHIVE_DATE LIKE SY-DATLO,    "TK 4.0B EURO "1999/06/01 mdemeest
      AENDANZ,
      PREISPOS,                          "Preisanzeigeber. Position
      AENDPOS,
      XOBJEKT(10),                       "Hilfsfeld Preis.ber.
      MANDANT LIKE EKKO-MANDT,           "Hilfsfeld Mandant
      DATUM(10) TYPE C,                  "Hilfsfeld Datum
      PERIODE(1) TYPE C,                 "Hilfsfeld Periode
      PFKEY LIKE SY-PFKEY,               "Hilfsfeld PF-Status
      SKIPFLAG,                          "Kennzeichen neue Seite
      UEB1FLAG,                          "Kennzeichen Überschrift
      EXITFLAG,                          "Kennzeichen Verlassen Routine
      FIRSTLINE LIKE SY-LINNO,           "Erste Zeile nach Überschrift
      INLINE,                            "Liste in Liste
      LSIND LIKE SY-LSIND,               "Hilfsfeld Liststufe
      VORZ,                              "Vorzeichen
      NEXPO,                             "Hilfsfeld naechste Position
      COLFLAG,                           "Flag für Streifenmuster
      LINFLAG,                           "Flag für Linesize
      TEXT(20) TYPE C,                   "Hilfsfeld Bildtitel
      F1 TYPE F,                         "Rechenfeld
      SUPRZ(3) TYPE P DECIMALS 2,        "Allg. Prozentfeld
      REICHW LIKE EKET-EINDT,            "Reichweite
      F_DATE LIKE EKET-EINDT,            "Datum 1. Abruf/Einteilung
      L_DATE LIKE EKET-EINDT,            "Datum letzter Abruf/Enteilung
      T_LINE(3) TYPE P,                  "Tabellenzeilen
      ZEILE LIKE SY-LILLI,               "Zeilenindex
      HMEINS LIKE EKPO-MEINS,            "Hilfsfeld Mengeneinheit
      HMEINS_WERKS LIKE EKPO-MEINS,
      HMEINS_BUKRS LIKE EKPO-MEINS,
      OLD_BSTYP LIKE EKKO-BSTYP,
      NIMMERLEIN LIKE EKET-EINDT VALUE '99991231',
      ANSWER,
      EKMKZ.

*------- Hilfsfelder Bestellentwicklung -------------------------------*
DATA: VORZSP,                            "Vorzeichen Sperrbestand
      EKBE_SUMME,                        "Kennzeichen Summe
      EKBE_WEUNB,                        "Kennzeichen WE unbewertet
      EKBE_KONT,                         "Kennzeichen Kontierungszuordn.
      EKBE_DIEN,                         "Kennzeichen Dienstleistung
      CMATNR LIKE EKBE-MATNR,            "Gruppenwechsel Material
      CWERKS LIKE EKBE-WERKS,            "Gruppenwechsel Werk
      CZEKKN LIKE EKBE-ZEKKN,            "Gruppenwechsel Kontierung
      EKBE_SUMNG LIKE EKBE-MENGE,        "Hilfsfeld Summe Menge
      EKBE_SUWRT LIKE EKBE-DMBTR,        "Hilfsfeld Summe Werte
      EKBE_SUFWR LIKE EKBE-WRBTR,        "Hilfsfeld Summe Werte
      EKBE_SUWAE LIKE EKBE-WAERS.        "Hilfsfeld Summe Werte

*------- Hilfsfelder Abrufdoku ----------------------------------------*
DATA: EKAB_SUMNG  LIKE EKAB-MENGE,       "Summe der Abrufe
      EKAB_SUWRT  LIKE EKAB-NETWR,       "Summe Werte der Abrufe
      EKAB_SUPRZ(3) TYPE P DECIMALS 2.   "Erfüllungsgrad


*------- Hilfsfelder Selektion ----------------------------------------*
DATA: VFELD1(11) TYPE P,                  "Vergleichsfeld
      VFELD2(11) TYPE P.                  "Vergleichsfeld

*------- Hilfsfelder Änderungsdokumentation ---------------------------*
DATA: INDTEXT(60)  TYPE C.                 "Art der Änderung
DATA: CDHDR_FLAG.
INCLUDE FM06ECDF.

*------- Hilfsfelder Dienstleistungsabwicklung ------------------------*
DATA: BEGIN OF GLIEDERUNG OCCURS 200.
          INCLUDE STRUCTURE ESLL.
DATA: END OF GLIEDERUNG.

*------- Hilfsfelder Freigabe -----------------------------------------*
DATA: XFRGCO LIKE T16FC-FRGCO,
      XFRGOP.

INCLUDE FM06LCS1.                      "Selektionsbedingungen 1
INCLUDE FM06LCS2.                      "Selektionsbedingungen 2
INCLUDE FM06LCS3.                      "nur Werk
INCLUDE FM06LCS4.                      "Selektionsbedingungen 4
INCLUDE FM06LCEK.                      "Common-Part Listen allgemein
INCLUDE FM06LCEV.                      "Common-Part Listen für Log. DB
INCLUDE FM06LCAD.                      "Common-Part Abrufdoku
INCLUDE FM06LCBE.                      "Common-Part Bestellentwicklung
INCLUDE FM06LCCD.                      "Common-Part Änderungsbelege
INCLUDE FM06BU01.                      "Common-Part Banfzuordnung
INCLUDE FM06LU01.                      "Common-Part WE/RE-Auswahl
INCLUDE FM06LU02.                      "Common-Part WE Mehrfachauswahl
INCLUDE FMMEXDIR.
INCLUDE ZNMIM008.                      "11/18/1996 JLEE
