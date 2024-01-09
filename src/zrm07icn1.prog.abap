*-----------------------------------------------------------------------
* CHANGES - All changes indicated by "UGL
* 2007/02/20 mdemeest Group range of days into one document rather  "UGL
*                     than several.                                 "UGL
*            Copy RM07ICN1 to ZRM07ICN1                             "UGL
*            In ZRM07ICN1, change:                                  "UGL
*               include from RM07IBDC to ZRM07IBDC                  "UGL
*            Copy RM07IBDC to ZRM07IBDC                             "UGL
*            Change recursive calls from RM07ICN1 to ZRM07ICN1 in   "UGL
*             ZRM07IBDC - otherwise you get error M7 651            "UGL
*-------------------------------------------------------------------"UGL
*
REPORT ZRM07ICN1 MESSAGE-ID M7.

*------ Datenbankviews für Lagerbestände
TABLES: V_MMIM_LN,
        V_MMIM_LC,
        V_MMIM_LB,
        V_MMIM_MST,      "View auf Materialverwaltungsstammsatz
        V_MMIM_CYC,      "View auf Cycle-Counting-Materialien
        T009,
        T009B,
        MARC.
*       MARV.                                            "note 372744
* &003 begin: neues Selektinkriterium
TABLES: AM07M.
* &003 end

*---- Data-definitionen
DATA: BEGIN OF XV_LN OCCURS 0.
        INCLUDE STRUCTURE V_MMIM_LN.
DATA:   IDATU LIKE MCHB-ERSDA.
DATA: END OF XV_LN.

DATA: BEGIN OF XV_LC OCCURS 0.
        INCLUDE STRUCTURE V_MMIM_LC.
DATA: END OF XV_LC.

DATA: t_labst TYPE mchb-clabs.                                   "572028

DATA: BEGIN OF XV_LB OCCURS 0.
        INCLUDE STRUCTURE V_MMIM_LB.
DATA: END OF XV_LB.

DATA: BEGIN OF XMSTA OCCURS 0.
        INCLUDE STRUCTURE V_MMIM_MST.
DATA: END OF XMSTA.

DATA: BEGIN OF XV_CYC OCCURS 0.
        INCLUDE STRUCTURE V_MMIM_CYC.
DATA: END OF XV_CYC.

DATA: BEGIN OF XWMIM OCCURS 0,
        WERKS LIKE MARC-WERKS,
        MATNR LIKE MARA-MATNR,
        IDATU LIKE LAGP-IDATU,
        NIDAT LIKE LAGP-IDATU,
        KZPMI TYPE C.
DATA: END OF XWMIM.

DATA: BEGIN OF WMDAT,
        V_DAT     LIKE SY-DATUM,
        B_DAT     LIKE SY-DATUM,
      END OF WMDAT.

DATA: BEGIN OF XV_CYC_KEY,
        MANDT     LIKE MARD-MANDT,
        MATNR     LIKE MARD-MATNR,
        WERKS     LIKE MARC-WERKS,
      END OF XV_CYC_KEY.

DATA: BEGIN OF XMSTA_KEY,
        MANDT     LIKE MARD-MANDT,
        MATNR     LIKE MARD-MATNR,
        BWKEY     LIKE MBEW-BWKEY,
      END OF XMSTA_KEY.

*---- Da diese Felder nicht in Selektionsdynpro, hier Definition
DATA: XLABST TYPE C VALUE 'X'.
DATA: XINSME.
DATA: XSPEME.
* &001 + &003 begin: Anpassung Wertartikelinventur
DATA: XWART.
DATA: SWBSTW.
DATA: XUMKEW.
* &001 end
* &jk01 begin (Änderung wegen warengruppenreinen Belegen)
DATA: KEORD    LIKE IKPF-KEORD VALUE '  '.
* &jk01 end
DATA: XIMAT TYPE C VALUE 'X'.
DATA: XICHA TYPE C VALUE 'X'.

* Sonstige DATA-Definitionen für Cycle-Counting
DATA: ABS_DLINL LIKE SCAL-FACDATE.
DATA: MIN_DLINL LIKE SCAL-FACDATE.
DATA: ABS_DATUM LIKE SCAL-FACDATE.
DATA: DATUM_INT LIKE BKPF-BUDAT.
DATA: WORKDAY   LIKE SCAL-INDICATOR.
DATA: INV_INIT  TYPE C.
DATA: X_WM_CALL TYPE C.
data: anziv like am07m-anziv.

* set global indicator to read also special stock            note 501666
data: gl_sobkz type c value ' '.                            "note 501666
*&jhm0001
selection-screen begin of block 0 with frame title text-052.
* Selektionsdynpro
SELECT-OPTIONS: R_MATNR FOR MARC-MATNR MEMORY ID MAT
                                       MATCHCODE OBJECT MAT1,
                R_WERKS FOR MARC-WERKS MEMORY ID WRK,
                R_LGORT FOR V_MMIM_LN-LGORT MEMORY ID LAG,
                R_CHARG FOR V_MMIM_LC-CHARG MEMORY ID CHA
                                       MATCHCODE OBJECT MCH1,
                R_MTART FOR V_MMIM_LN-MTART MEMORY ID MTA,
                R_MATKL FOR V_MMIM_LN-MATKL,
                R_LGPBE FOR V_MMIM_LN-LGPBE.

* &003 begin: neues Selektionskriterium
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XDELE LIKE MARC-LVORM DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(50) TEXT-021 FOR FIELD XDELE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
*selection-screen comment /1(79) text-034.
* &003 end

* &004 begin: neues Selektionskriterium
SELECTION-SCREEN COMMENT /1(79) TEXT-038.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XLABST1 LIKE AM07M-XLABST DEFAULT 'X'
            MODIF ID 001.
SELECTION-SCREEN COMMENT 3(50) TEXT-039 FOR FIELD XLABST1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XINSME1 LIKE AM07M-XINSME.
SELECTION-SCREEN COMMENT 3(50) TEXT-040 FOR FIELD XINSME1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XSPEME1 LIKE AM07M-XSPEME.
SELECTION-SCREEN COMMENT 3(50) TEXT-041 FOR FIELD XSPEME1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(79) TEXT-034.

* &004 end: neues Selektionskriterium

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT 1(30) TEXT-030 FOR FIELD SWBST.
SELECTION-SCREEN POSITION 35.
PARAMETERS: SWBST LIKE AM07M-SWBST.
SELECTION-SCREEN POSITION 60.
* &003 begin: neues Selektionskriterium
PARAMETERS: XUMKE LIKE AM07M-XUMKE.
SELECTION-SCREEN COMMENT 62(21) TEXT-037 FOR FIELD XUMKE.
*PARAMETERS: XNULB LIKE AM07M-XNULB.
*SELECTION-SCREEN COMMENT 62(11) TEXT-031.
* &003 end
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
* &003 begin: neues Selektionskriterium
*PARAMETERS: XDELE LIKE MARC-LVORM DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 3(50) TEXT-021.
PARAMETERS: XNULB LIKE AM07M-XNULB .
SELECTION-SCREEN COMMENT 3(35) TEXT-031 FOR FIELD XNULB.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XONUL LIKE AM07M-XONUL .
SELECTION-SCREEN COMMENT 3(35) TEXT-035 FOR FIELD XONUL.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XNEGB LIKE AM07M-KNEGB .
SELECTION-SCREEN COMMENT 3(45) TEXT-036 FOR FIELD XNEGB.
* &003 end
SELECTION-SCREEN END OF LINE.
*&jhm0001
selection-screen end of block 0.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(79) TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) TEXT-019 FOR FIELD GIDAT.
SELECTION-SCREEN POSITION 35.
* &001 begin: Anpassungen Zeitintervalle
*PARAMETERS: GIDAT LIKE AM07M-GIDAV DEFAULT SY-DATUM.
PARAMETERS: GIDAT LIKE AM07M-GIDAV DEFAULT SY-DATLO.
* &001 end
SELECTION-SCREEN COMMENT 56(03) TEXT-020.
SELECTION-SCREEN POSITION 60.
* &001 begin: Anpassungen Zeitintervalle
*PARAMETERS: GBDAT LIKE AM07M-GIDAV DEFAULT SY-DATUM.
PARAMETERS: GBDAT LIKE AM07M-GIDAV DEFAULT SY-DATLO.
* &001 end
SELECTION-SCREEN END OF LINE.
PARAMETERS: INVNU LIKE IKPF-INVNU.
PARAMETERS: XBLNI LIKE IKPF-XBLNI.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: SPERR LIKE IKPF-SPERR.
SELECTION-SCREEN COMMENT 3(27) TEXT-022 FOR FIELD SPERR.
SELECTION-SCREEN POSITION 35.
PARAMETERS: XBUFI LIKE IKPF-XBUFI.
SELECTION-SCREEN COMMENT 37(27) TEXT-024 FOR FIELD XBUFI.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(79) TEXT-007.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: BATIN LIKE AM07M-BATIN.
SELECTION-SCREEN COMMENT 3(28) TEXT-011 FOR FIELD BATIN.
SELECTION-SCREEN COMMENT 35(24) TEXT-013 FOR FIELD MAPPE.
SELECTION-SCREEN POSITION 60.
PARAMETERS: MAPPE LIKE AM07M-MAPPE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XPROT LIKE AM07M-XPROT.
SELECTION-SCREEN COMMENT 3(28) TEXT-012 FOR FIELD XPROT.
SELECTION-SCREEN COMMENT 35(24) TEXT-014 FOR FIELD MAXPO.
SELECTION-SCREEN POSITION 60.
PARAMETERS: MAXPO LIKE AM07M-MAXPO.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(79) TEXT-008.
* Lagerplatz
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XLGPBE LIKE AM07M-MIXLGPBE.
SELECTION-SCREEN COMMENT 3(28) TEXT-009 FOR FIELD XLGPBE.
* neuer Beleg
SELECTION-SCREEN POSITION 35.
PARAMETERS: XGRUPP LIKE AM07M-MIXGRUPP.
SELECTION-SCREEN COMMENT 37(60) TEXT-016 FOR FIELD XGRUPP.

SELECTION-SCREEN END OF LINE.
* Warengruppe
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XMATKL LIKE AM07M-MIXMATKL.
SELECTION-SCREEN COMMENT 3(28) TEXT-010 FOR FIELD XMATKL.
* Grupp.krit merken
SELECTION-SCREEN POSITION 35.
PARAMETERS: XSAVEG LIKE AM07M-MIXSAVEG.
SELECTION-SCREEN COMMENT 37(44) TEXT-050 FOR FIELD XSAVEG.

SELECTION-SCREEN END OF LINE.

*------- Hauptinclude
*INCLUDE: RM07IBDC.                                              "UGL
INCLUDE: ZRM07IBDC.                                              "UGL


*---------------------------------------------------------------------*
*       INITIALIZATION                                                *
*---------------------------------------------------------------------*
INITIALIZATION.
  XCYCL = X.
  LSIZE = 90.
  IMPORT XWMIM FROM MEMORY ID 'WM_IM_CYCLE'.
  IF SY-SUBRC IS INITIAL.
    X_WM_CALL = X.
    IMPORT WMDAT FROM MEMORY ID 'WM_IM_DATE'.
    IF NOT SY-SUBRC IS INITIAL.
* &001 begin: Anpassungen an Zeitzonen
*     WMDAT-V_DAT = SY-DATUM.
*     WMDAT-B_DAT = SY-DATUM.
      WMDAT-V_DAT = SY-DATLO.
      WMDAT-B_DAT = SY-DATLO.
* &001 end
    ENDIF.
    GIDAT = WMDAT-V_DAT.
    GBDAT = WMDAT-B_DAT.
  ENDIF.
*---------------------------------------------------------------------*
*       START-OF-SELECTION                                            *
*---------------------------------------------------------------------*
START-OF-SELECTION.

  SOBKZ = SPACE.
  INVENTREF = XBLNI.
  IF BATIN IS INITIAL.
    SET PF-STATUS '100'.
  ELSE.
    SET PF-STATUS '200'.
  ENDIF.
  IF GBDAT < GIDAT.
    GBDAT = GIDAT.
  ENDIF.

XLABST = XLABST1.
XSPEME = XSPEME1.
XINSME = XINSME1.
* KEORD festlegen
  IF NOT XSAVEG IS INITIAL.
    IF NOT XLGPBE IS INITIAL.
      KEORD = C_LGPBE.
    ENDIF.
    IF NOT XMATKL IS INITIAL.
      KEORD = C_MATKL.
    ENDIF.
  ELSE.
    KEORD = C_LEER.
  ENDIF.


*---------------------------------------------------------------------*
*       Datenbeschaffung                                              *
*---------------------------------------------------------------------*

  IF X_WM_CALL IS INITIAL.
    READ TABLE R_CHARG INDEX 1.
    IF NOT SY-SUBRC IS INITIAL.
      SELECT * FROM V_MMIM_LN APPENDING TABLE XV_LN
                                        WHERE MATNR IN R_MATNR
                                        AND   WERKS IN R_WERKS
                                        AND   LGORT IN R_LGORT
                                        AND   MTART IN R_MTART
                                        AND   MATKL IN R_MATKL
                                        AND   LGPBE IN R_LGPBE
                                        AND   ABCIN NE SPACE.
    ENDIF.

    SELECT * FROM V_MMIM_LC APPENDING TABLE XV_LC
                                      WHERE MATNR IN R_MATNR
                                      AND   WERKS IN R_WERKS
                                      AND   LGORT IN R_LGORT
                                      AND   CHARG IN R_CHARG
                                      AND   MTART IN R_MTART
                                      AND   MATKL IN R_MATKL
                                      AND   LGPBE IN R_LGPBE
                                      AND   ABCIN NE SPACE.

    SELECT * FROM V_MMIM_LB APPENDING TABLE XV_LB
                                      WHERE MATNR IN R_MATNR
                                      AND   WERKS IN R_WERKS
                                      AND   LGORT IN R_LGORT
                                      AND   CHARG IN R_CHARG
                                      AND   MTART IN R_MTART
                                      AND   MATKL IN R_MATKL
                                      AND   LGPBE IN R_LGPBE
                                      AND   ABCIN NE SPACE.
  ELSE.

*-- Range zurücksetzten
    PERFORM RANGES_INIT.

*-- Ersten Eintrag in Übergabetabelle lesen
    READ TABLE XWMIM INDEX 1.
    OLD_WERKS = XWMIM-WERKS.

*-- Übergeben Sätze auswerten
    LOOP AT XWMIM.
      IF OLD_WERKS NE XWMIM-WERKS.
        IF C_MATNR > 0.
          PERFORM WM_CYCLE_MATERIAL.
        ENDIF.
        OLD_WERKS = XWMIM-WERKS.
      ENDIF.
      R_MATNR-LOW = XWMIM-MATNR.
      COLLECT R_MATNR.
      DESCRIBE TABLE R_MATNR LINES C_MATNR.
      IF C_MATNR > 250.
        PERFORM WM_CYCLE_MATERIAL.
      ENDIF.
    ENDLOOP.
*-- restliche Sätze lesen
    IF C_MATNR > 0.
      PERFORM WM_CYCLE_MATERIAL.
    ENDIF.
    SORT XV_CYC BY MANDT MATNR WERKS.
    LOOP AT XWMIM.
      MOVE-CORRESPONDING XWMIM TO XV_CYC_KEY.
      MOVE SY-MANDT TO XV_CYC_KEY-MANDT.
      READ TABLE XV_CYC WITH KEY XV_CYC_KEY BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        MOVE-CORRESPONDING XV_CYC TO XV_LN.
        MOVE XWMIM-IDATU TO XV_LN-DLINL.
        MOVE XWMIM-IDATU TO XV_LN-IDATU.
        APPEND XV_LN.
      ENDIF.
    ENDLOOP.
    REFRESH XV_CYC.
    REFRESH XWMIM.

  ENDIF.

* Range zurücksetzten
  PERFORM RANGES_INIT.

  PERFORM  MARV_BESTIMMEN.                                 "note 210283

  LOOP AT XV_LN.
    IF XV_LN-DLINL IS INITIAL OR XV_LN-DLINL EQ SPACE.
      R_MATNR-LOW = XV_LN-MATNR.
      COLLECT R_MATNR.
      DESCRIBE TABLE R_MATNR LINES C_MATNR.
      IF C_MATNR > 250.
        PERFORM MATERIALSTATUS_LESEN.
      ENDIF.
    ENDIF.
    ON CHANGE OF XV_LN-WERKS.
      R_WERKS-LOW = XV_LN-WERKS.
      COLLECT R_WERKS.
    ENDON.
  ENDLOOP.
  RC = SY-SUBRC.
* restliche Sätze für Materialstatus lesen
  IF C_MATNR > 0.
    PERFORM MATERIALSTATUS_LESEN.
  ENDIF.
  SORT XMSTA BY MANDT MATNR BWKEY.

  LOOP AT XV_LC.
    ON CHANGE OF XV_LC-WERKS.
      R_WERKS-LOW = XV_LC-WERKS.
      COLLECT R_WERKS.
    ENDON.
  ENDLOOP.
  RC2 = SY-SUBRC.


  LOOP AT XV_LB.
    ON CHANGE OF XV_LB-WERKS.
      R_WERKS-LOW = XV_LB-WERKS.
      COLLECT R_WERKS.
    ENDON.
  ENDLOOP.

* Sätze gefunden ?
  IF NOT RC IS INITIAL AND NOT RC2 IS INITIAL
     AND NOT SY-SUBRC IS INITIAL.
* P.Pfaff: begin: Message ausgetauscht
    MESSAGE S198.
*   MESSAGE S083.
* P.Pfaff: end
    PERFORM ANFORDERUNGSBILD.
  ENDIF.

* Die notwendigen Daten zur späteren Prüfung der Bestandsebenen werden
* eingelesen
  PERFORM PRUEFUNG_VORBEREITEN.

* Das Inventurdatumsintervall wird in Fabrikdatum umgerechnet
  PERFORM INTERVALL_UMRECHNEN.

* check of marv to get information MDJIN and MARV-LFGJA note 353075
  perform marv_single_read.                            "note 353075

  IF X_WM_CALL IS INITIAL.

*-- Prüfen der Bestandseinheiten - normale Materialien
    LOOP AT XV_LN.
      CLEAR PRE_TAB.
      MOVE-CORRESPONDING XV_LN TO PRE_TAB.
    READ TABLE WERKS_LFGJA WITH KEY WERKS = PRE_TAB-WERKS BINARY SEARCH.
    MOVE WERKS_LFGJA-LFGJA TO PRE_TAB-LFGJA.                "note 210283
      IF KEORD = C_MATKL.     " Festwert "Warengruppe"
        MOVE XV_LN-MATKL TO PRE_TAB-ORDNG.
      ENDIF.
      IF KEORD = C_LGPBE.     " Festwert "Lagerplatz"
        MOVE XV_LN-LGPBE TO PRE_TAB-ORDNG.
      ENDIF.
      PERFORM BSE_PRUEFEN.
    ENDLOOP.

*-- Prüfen der Bestandseinheiten - Chargenmaterialien
    LOOP AT XV_LC.
      CLEAR PRE_TAB.
      MOVE-CORRESPONDING XV_LC TO PRE_TAB.
    READ TABLE WERKS_LFGJA WITH KEY WERKS = PRE_TAB-WERKS BINARY SEARCH.
    MOVE WERKS_LFGJA-LFGJA TO PRE_TAB-LFGJA.                "note 210283
      IF NOT XV_LC-LVORM_B IS INITIAL OR
         NOT XV_LC-LVORM   IS INITIAL OR
         NOT XV_LC-LVORM_1 IS INITIAL.
        MOVE X TO PRE_TAB-LVORM.
      ENDIF.
      t_labst = xv_lc-labst + xv_lc-einme.                       "572028
      MOVE t_labst TO pre_tab-labst.                             "572028
      IF KEORD = C_MATKL.     " Festwert "Warengruppe"
        MOVE XV_LC-MATKL TO PRE_TAB-ORDNG.
      ENDIF.
      IF KEORD = C_LGPBE.     " Festwert "Lagerplatz"
        MOVE XV_LC-LGPBE TO PRE_TAB-ORDNG.
      ENDIF.
      PERFORM BSE_PRUEFEN.
    ENDLOOP.

*-- Prüfen der Bestandseinheiten - getrennt bewertete Materialien
    LOOP AT XV_LB.
      CLEAR PRE_TAB.
      MOVE-CORRESPONDING XV_LB TO PRE_TAB.
    READ TABLE WERKS_LFGJA WITH KEY WERKS = PRE_TAB-WERKS BINARY SEARCH.
    MOVE WERKS_LFGJA-LFGJA TO PRE_TAB-LFGJA.                "note 210283
      IF NOT XV_LC-LVORM_B IS INITIAL OR
         NOT XV_LC-LVORM   IS INITIAL.
        MOVE X TO PRE_TAB-LVORM.
      ENDIF.
      PERFORM BSE_PRUEFEN.
    ENDLOOP.

*-- Prüfung der Buchhaltungsdaten für die bis hier selektierten Daten
    PERFORM PRUEFUNG_DURCHFUEHREN.

  ELSE.

    LOOP AT XV_LN.
      CLEAR XWMIM.
      MOVE-CORRESPONDING XV_LN TO PRE_TAB.
    READ TABLE WERKS_LFGJA WITH KEY WERKS = PRE_TAB-WERKS BINARY SEARCH.
    MOVE WERKS_LFGJA-LFGJA TO PRE_TAB-LFGJA.                "note 210283
      PERFORM INVENTURZYKLUS_PRUEFEN USING PRE_TAB-KZILL
                                           PRE_TAB-KZVLL
                                           PRE_TAB-DLINL
                                           EINS.
      MOVE-CORRESPONDING PRE_TAB TO XWMIM.
      APPEND XWMIM.
    ENDLOOP.

*-- Daten auslesen
    EXPORT XWMIM TO MEMORY ID 'IM_WM_CYCLE'.

  ENDIF.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM AUSGABE                                                  *
*---------------------------------------------------------------------*
*       Bildschirmausgabe aller Sätze der internen Tabelle TAB.       *
*---------------------------------------------------------------------*
FORM AUSGABE.
  CASE KEORD.
    WHEN C_LEER.      "Ordnungseigenschaft ist nicht gefüllt
    IF XMATKL IS INITIAL AND XLGPBE IS INITIAL.
      SORT TAB BY CYCLE GPDAT WERKS LGORT MATNR CHARG BSTAR.
    ELSE.
      IF XMATKL IS INITIAL.
* &004 begin: Sortierung umgestellt
*       SORT TAB BY CYCLE GPDAT LGPBE WERKS LGORT MATNR CHARG BSTAR.
*       SORT TAB BY CYCLE GPDAT WERKS LGORT LGPBE MATNR CHARG BSTAR."UGL
       SORT TAB BY WERKS LGORT lgpbe MATNR.                         "UGL
* &004 end
      ELSE.
* &004 begin: Sortierung umgestellt
*       SORT TAB BY CYCLE GPDAT MATKL WERKS LGORT MATNR CHARG BSTAR.
        SORT TAB BY WERKS LGORT MATKL MATNR.                        "UGL
* &004 end
      ENDIF.
    ENDIF.
    WHEN OTHERS.    " Ordnungseigenschaft ist gefüllt
      SORT TAB BY WERKS LGORT ORDNG MATNR CHARG BSTAR.
  ENDCASE.
  IF NOT BATIN IS INITIAL AND COUNT-ININT > 0.
    PERFORM MAPPE_AUFBAUEN.
    MESSAGE S832 WITH MAPPE.
  ENDIF.
  IF COUNT-PENDG > 0.
    HEAD_TYP = D.
    SET TITLEBAR '100'.
    NEW-PAGE NO-HEADING NO-TITLE LINE-SIZE LSIZE.
    LOOP AT TAB WHERE CYCLE EQ P.
      PERFORM NEXT_ROW USING 2 SPACE.
      WRITE:2 TAB-GPDAT NO-GAP COLOR COL_NEGATIVE INTENSIFIED,
                                SY-VLINE NO-GAP,
              TAB-ABCIN NO-GAP, SY-VLINE NO-GAP,
              TAB-WERKS NO-GAP, SY-VLINE NO-GAP,
              TAB-LGORT NO-GAP, SY-VLINE NO-GAP,
              TAB-MATNR NO-GAP, SY-VLINE NO-GAP,
              TAB-CHARG NO-GAP, SY-VLINE NO-GAP,
              TAB-BSTAR NO-GAP, SY-VLINE NO-GAP,
              TAB-LGPBE NO-GAP, SY-VLINE NO-GAP,
              TAB-MATKL NO-GAP.
    ENDLOOP.
    PERFORM CLOSE_GRID.
  ENDIF.
  IF NOT XPROT IS INITIAL AND COUNT-ININT > 0.
    HEAD_TYP = A.
    SET TITLEBAR '100'.
    NEW-PAGE NO-HEADING NO-TITLE LINE-SIZE LSIZE.
    LOOP AT TAB WHERE CYCLE EQ O OR CYCLE EQ X.
      PERFORM NEXT_ROW USING 2 SPACE.
      IF TAB-GPDAT < TAB-GIDAT.
        WRITE:2 TAB-GPDAT NO-GAP COLOR COL_NEGATIVE INTENSIFIED,
                                  SY-VLINE NO-GAP.
      ELSE.
        WRITE:2 TAB-GPDAT NO-GAP, SY-VLINE NO-GAP.
      ENDIF.
      WRITE:  TAB-GIDAT NO-GAP, SY-VLINE NO-GAP,
              TAB-ABCIN NO-GAP, SY-VLINE NO-GAP,
              TAB-WERKS NO-GAP, SY-VLINE NO-GAP,
              TAB-LGORT NO-GAP, SY-VLINE NO-GAP,
              TAB-MATNR NO-GAP, SY-VLINE NO-GAP,
              TAB-CHARG NO-GAP, SY-VLINE NO-GAP,
              TAB-BSTAR NO-GAP, SY-VLINE NO-GAP,
              TAB-LGPBE NO-GAP, SY-VLINE NO-GAP,
              TAB-MATKL NO-GAP.
    ENDLOOP.
    PERFORM CLOSE_GRID.

  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM INTERVALL_UMRECHNEN                                      *
*---------------------------------------------------------------------*
FORM INTERVALL_UMRECHNEN.
  LOOP AT XWEBU.

*-- Das aktuelle Datum wird in das Fabrikdatum umgerechnet
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
         EXPORTING
              CORRECT_OPTION               = '+'
* &001 begin: Anpassung an Zeitzonen
*             DATE                         = SY-DATUM
              DATE                         = SY-DATLO
* &001 end
              FACTORY_CALENDAR_ID          = XWEBU-FABKL
         IMPORTING
              FACTORYDATE                  = XWEBU-ACTUD
         EXCEPTIONS
              CALENDAR_BUFFER_NOT_LOADABLE = 02
              DATE_AFTER_RANGE             = 04
              DATE_BEFORE_RANGE            = 06
              DATE_INVALID                 = 08
              FACTORY_CALENDAR_NOT_FOUND   = 10.
    IF NOT SY-SUBRC IS INITIAL.
* &001 begin: Anpassung an Zeitzonen
*     PERFORM FEHLER_DATUM USING SY-SUBRC SY-DATUM XWEBU-FABKL.
      PERFORM FEHLER_DATUM USING SY-SUBRC SY-DATLO XWEBU-FABKL.
* &001 end
    ENDIF.

*-- Das untere Intervalldatum wird in das Fabrikdatum umgerechnet
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
         EXPORTING
              CORRECT_OPTION               = '+'
              DATE                         = GIDAT
              FACTORY_CALENDAR_ID          = XWEBU-FABKL
         IMPORTING
              FACTORYDATE                  = XWEBU-FRSTD
              WORKINGDAY_INDICATOR         = WORKDAY
         EXCEPTIONS
              CALENDAR_BUFFER_NOT_LOADABLE = 02
              DATE_AFTER_RANGE             = 04
              DATE_BEFORE_RANGE            = 06
              DATE_INVALID                 = 08
              FACTORY_CALENDAR_NOT_FOUND   = 10.
    IF NOT SY-SUBRC IS INITIAL.
      PERFORM FEHLER_DATUM USING SY-SUBRC GIDAT XWEBU-FABKL.
    ENDIF.
    IF NOT WORKDAY IS INITIAL.

*---- Das unterer Intervalldatum in ein korr. Fabrikdatum umgerechnet
      CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
           EXPORTING
                FACTORYDATE                  = XWEBU-FRSTD
                FACTORY_CALENDAR_ID          = XWEBU-FABKL
           IMPORTING
                DATE                         = GIDAT
           EXCEPTIONS
                CALENDAR_BUFFER_NOT_LOADABLE = 02
                FACTORYDATE_AFTER_RANGE      = 04
                FACTORYDATE_BEFORE_RANGE     = 06
                FACTORYDATE_INVALID          = 08
                FACTORY_CALENDAR_ID_MISSING  = 10
                FACTORY_CALENDAR_NOT_FOUND   = 12.
      IF NOT SY-SUBRC IS INITIAL.
        PERFORM FEHLER_DATUM USING SY-SUBRC XWEBU-FRSTD XWEBU-FABKL.
      ENDIF.
    ENDIF.

*-- Wenn das obere Intervalldatum größer dem unteren ist
    IF GBDAT > GIDAT.
      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
           EXPORTING
                CORRECT_OPTION               = '+'
                DATE                         = GBDAT
                FACTORY_CALENDAR_ID          = XWEBU-FABKL
           IMPORTING
                FACTORYDATE                  = XWEBU-LASTD
           EXCEPTIONS
                CALENDAR_BUFFER_NOT_LOADABLE = 02
                DATE_AFTER_RANGE             = 04
                DATE_BEFORE_RANGE            = 06
                DATE_INVALID                 = 08
                FACTORY_CALENDAR_NOT_FOUND   = 10.
      IF NOT SY-SUBRC IS INITIAL.
        PERFORM FEHLER_DATUM USING SY-SUBRC GBDAT XWEBU-FABKL.
      ENDIF.
    ELSE.
      XWEBU-LASTD = XWEBU-FRSTD.
    ENDIF.

*-- Der erste Tag des Geschäftsjahres wird ermittelt
    CALL FUNCTION 'MARV_SINGLE_READ'
         EXPORTING
              bukrs      = XWEBU-BUKRS
         IMPORTING
              wmarv      = marv
         EXCEPTIONS
              not_found  = 1
              wrong_call = 2
              OTHERS     = 3.
*-- Es wird die erste Periode gesetzt
    MARV-LFMON = 1.
    WHILE MARV-LFMON < 17 AND XWEBU-FDYEA IS INITIAL.
      CALL FUNCTION 'PERIOD_DAY_DETERMINE'
           EXPORTING
                I_GJAHR              = MARV-LFGJA
                I_MONAT              = MARV-LFMON
                I_PERIV              = XWEBU-PERIV
           IMPORTING
                E_FDAY               = XWEBU-FDYEA
           EXCEPTIONS
                ERROR_PERIOD         = 01
                ERROR_PERIOD_VERSION = 02
                FIRSTDAY_NOT_DEFINED = 03
                PERIOD_NOT_DEFINED   = 04.
      IF NOT SY-SUBRC IS INITIAL.
        MARV-LFMON = MARV-LFMON + 1.
      ENDIF.
    ENDWHILE.
    IF XWEBU-FDYEA IS INITIAL OR NOT SY-SUBRC IS INITIAL.
      MESSAGE ID 'F5' TYPE 'E' NUMBER 234 WITH XWEBU-PERIV.
    ENDIF.

*-- Der letzte Tag des Geschäftsjahres wird ermittelt
    MARV-LFGJA = MARV-LFGJA + 1.
*-- Es wird die erste Periode gesetzt
    MARV-LFMON = 1.
*-- Several plants in XWEBU: DATUM_INT has to be cleared
    CLEAR DATUM_INT.                                      "note 538003
    WHILE MARV-LFMON < 17 AND DATUM_INT IS INITIAL.
      CALL FUNCTION 'PERIOD_DAY_DETERMINE'
           EXPORTING
                I_GJAHR              = MARV-LFGJA
                I_MONAT              = MARV-LFMON
                I_PERIV              = XWEBU-PERIV
           IMPORTING
                E_FDAY               = DATUM_INT
           EXCEPTIONS
                ERROR_PERIOD         = 01
                ERROR_PERIOD_VERSION = 02
                FIRSTDAY_NOT_DEFINED = 03
                PERIOD_NOT_DEFINED   = 04.
      IF NOT SY-SUBRC IS INITIAL.
        MARV-LFMON = MARV-LFMON + 1.
      ENDIF.
    ENDWHILE.
    IF DATUM_INT IS INITIAL OR NOT SY-SUBRC IS INITIAL.
      MESSAGE ID 'F5' TYPE 'E' NUMBER 234 WITH XWEBU-PERIV.
    ELSE.
*---- Datum einen Tag zurücksetzen -> Vorgeschäftsjahr
      DATUM_INT = DATUM_INT - 1.
      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
           EXPORTING
                CORRECT_OPTION               = '-'
                DATE                         = DATUM_INT
                FACTORY_CALENDAR_ID          = XWEBU-FABKL
           IMPORTING
                FACTORYDATE                  = XWEBU-LDYEA
           EXCEPTIONS
                CALENDAR_BUFFER_NOT_LOADABLE = 02
                DATE_AFTER_RANGE             = 04
                DATE_BEFORE_RANGE            = 06
                DATE_INVALID                 = 08
                FACTORY_CALENDAR_NOT_FOUND   = 10.
      IF NOT SY-SUBRC IS INITIAL.
        PERFORM FEHLER_DATUM USING SY-SUBRC GBDAT XWEBU-FABKL.
      ENDIF.
    ENDIF.
    MODIFY XWEBU.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*     FORM INVENTURZYKLUS_PREUFEN                                     *
*---------------------------------------------------------------------*
FORM INVENTURZYKLUS_PRUEFEN USING M_KZILL
                                  M_KZVLL
                                  M_DLINL_TEMP                 "HW583142
                                  M_BSTAR.

* Datum sichern (wegen fehlender using/changing Funktionalität) HW583142
  DATA: M_DLINL LIKE MARD-DLINL.                               "HW583142
  M_DLINL = M_DLINL_TEMP.                                      "HW583142

* Kennzeichen, ob das Material neu ist zurücksetzen
  CLEAR INV_INIT.

* &jhm01 Reparatur cycle counting
* Abfrage Kalkulationssicht-Buchhaltungssicht
  PERFORM KALKU_PRUEF USING PRE_TAB-MTART.

* Rückbuchung prüfen
  IF XRUEJ IS INITIAL.
    AKTIV = M_KZILL.
  ELSE.
    AKTIV = M_KZVLL.
  ENDIF.

* Bestimmung des Inventurdatums aus dem letzten Inventurdatum
  MOVE-CORRESPONDING PRE_TAB TO T159C_KEY.
  MOVE SY-MANDT TO T159C_KEY-MANDT.
  READ TABLE XT159C WITH KEY T159C_KEY BINARY SEARCH.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE S651 WITH 'V_159C' T159C_KEY-WERKS T159C_KEY-ABCIN.
    PERFORM ANFORDERUNGSBILD.
  ENDIF.
  READ TABLE XWEBU WITH KEY WERKS = PRE_TAB-WERKS.               "554586
  if sy-subrc <> 0.                                              "554586
    MESSAGE e001 WITH 'xwebu' xwebu-werks.                       "554586
  ENDIF.                                                         "554586
* Wenn das letzte Inventurdatum nicht bekannt ist, wird das Datum
* der Anlage der Buchhaltungsdaten für MARD-Materialien oder
* das Anlagedatum der Charge bei MCHB-Materialien gesetzt
  IF M_DLINL IS INITIAL OR M_DLINL EQ SPACE.
    INV_INIT = X.
    IF NOT PRE_TAB-ERSDA IS INITIAL.
      IF PRE_TAB-ERSDA < XWEBU-FDYEA.
        M_DLINL = XWEBU-FDYEA.
      ELSE.
        M_DLINL = PRE_TAB-ERSDA.
      ENDIF.
    ELSE.
      MOVE SY-MANDT TO XMSTA_KEY-MANDT.
      MOVE PRE_TAB-MATNR TO XMSTA_KEY-MATNR.
      MOVE XWEBU-BWKEY TO XMSTA_KEY-BWKEY.
      READ TABLE XMSTA WITH KEY XMSTA_KEY BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        IF XMSTA-ERSDA < XWEBU-FDYEA.
          M_DLINL = XWEBU-FDYEA.
        ELSE.
          M_DLINL = XMSTA-ERSDA.
        ENDIF.
      ELSE.
        M_DLINL = XWEBU-FDYEA.
      ENDIF.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            CORRECT_OPTION               = '+'
            DATE                         = M_DLINL
            FACTORY_CALENDAR_ID          = XWEBU-FABKL
       IMPORTING
            FACTORYDATE                  = ABS_DLINL
       EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 02
            DATE_AFTER_RANGE             = 04
            DATE_BEFORE_RANGE            = 06
            DATE_INVALID                 = 08
            FACTORY_CALENDAR_NOT_FOUND   = 10.
  IF NOT SY-SUBRC IS INITIAL.
    PERFORM FEHLER_DATUM USING SY-SUBRC M_DLINL XWEBU-FABKL.
  ENDIF.
  ABS_DLINL = ABS_DLINL + XT159C-ININV.
  MIN_DLINL = XWEBU-ACTUD - XT159C-PZINV.

* Bestandseinheit ist zu inventarisieren
  IF AKTIV-XIAKT IS INITIAL.

*-- BSE wurde neu angelegt und noch nicht inventarisiert
    IF NOT INV_INIT IS INITIAL.
      IF ABS_DLINL > XWEBU-LDYEA.
        ABS_DLINL = XWEBU-LDYEA.
      ENDIF.
    ENDIF.

*-- BSE hätte schon längst in einen Inventurbeleg aufgenommen werden s.
    IF ABS_DLINL < XWEBU-ACTUD.
      TAB-CYCLE = O.
      XWMIM-KZPMI = MINUS.
      COUNT-OVERD = COUNT-OVERD + 1.

*-- BSE nicht überfällig aber nicht im Intervall
    ELSEIF ABS_DLINL < XWEBU-FRSTD.
      TAB-CYCLE = N.
      XWMIM-KZPMI = STERN.
      COUNT-OUTOF = COUNT-OUTOF + 1.

*-- BSE innerhalb des Intervalls
    ELSEIF ABS_DLINL <= XWEBU-LASTD.
      TAB-CYCLE = X.
      COUNT-ININT = COUNT-ININT + 1.

*-- BSE ist noch nicht an der Reihe
    ELSE.
      IF X_WM_CALL IS INITIAL.
        CHECK X IS INITIAL.
      ELSE.
        XWMIM-KZPMI = PLUS.
      ENDIF.
    ENDIF.
  ELSE.

*-- Inventur für BSE sollte schon abgeschlossen sein
    IF ABS_DLINL < MIN_DLINL.
      TAB-CYCLE = P.
      COUNT-PENDG = COUNT-PENDG + 1.
    ELSE.

*---- BSE ist nicht zu betrachten
      CHECK X IS INITIAL.
    ENDIF.
  ENDIF.

* Weitere Bestandseinheit für Mappe
  MOVE-CORRESPONDING PRE_TAB TO TAB.

* GIDAT muß aus ABS_DLINL berechnet werden
  CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
       EXPORTING
            FACTORYDATE                  = ABS_DLINL
            FACTORY_CALENDAR_ID          = XWEBU-FABKL
       IMPORTING
            DATE                         = TAB-GPDAT
       EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 02
            FACTORYDATE_AFTER_RANGE      = 04
            FACTORYDATE_BEFORE_RANGE     = 06
            FACTORYDATE_INVALID          = 08
            FACTORY_CALENDAR_ID_MISSING  = 10
            FACTORY_CALENDAR_NOT_FOUND   = 12.
  IF NOT SY-SUBRC IS INITIAL.
* &001 begin: Anpassung Zeitzonen
*   PERFORM FEHLER_DATUM USING SY-SUBRC SY-DATUM XWEBU-FABKL.
    PERFORM FEHLER_DATUM USING SY-SUBRC SY-DATLO XWEBU-FABKL.
* &001 end
  ENDIF.
  IF TAB-GPDAT < GIDAT.
    TAB-GIDAT = GIDAT.
  ELSE.
    TAB-GIDAT = TAB-GPDAT.
  ENDIF.
  IF X_WM_CALL IS INITIAL.
    TAB-BSTAR = M_BSTAR.
    APPEND TAB.
  ELSE.
    XWMIM-NIDAT = TAB-GIDAT.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FEHLER_DATUM                                             *
*---------------------------------------------------------------------*
*  -->  LOC_SUBRC                                                     *
*  -->  LOC_DATUM                                                     *
*---------------------------------------------------------------------*
FORM FEHLER_DATUM USING LOC_SUBRC LOC_DATUM LOC_FACTO.
  CASE LOC_SUBRC.
    WHEN 4.
      MESSAGE S523 WITH LOC_DATUM.
    WHEN 6.
      MESSAGE S524 WITH LOC_DATUM.
    WHEN 10.
      MESSAGE S526 WITH LOC_FACTO.
    WHEN 12.
      MESSAGE S526 WITH LOC_FACTO.
    WHEN OTHERS.
      MESSAGE S525 WITH LOC_DATUM.
  ENDCASE.
  PERFORM ANFORDERUNGSBILD.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM MATERIALSTATUS_LESEN                                     *
*---------------------------------------------------------------------*
FORM MATERIALSTATUS_LESEN.

  SELECT * FROM V_MMIM_MST APPENDING TABLE XMSTA
                               WHERE MATNR IN R_MATNR
                                 AND STATM EQ 'B'.
  CLEAR   C_MATNR.
  CLEAR   R_MATNR.
  REFRESH R_MATNR.
  R_MATNR-SIGN   = 'I'.
  R_MATNR-OPTION = 'EQ'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM WM_CYCLE_MATERIAL                                        *
*---------------------------------------------------------------------*
FORM WM_CYCLE_MATERIAL.

  SELECT * FROM V_MMIM_CYC APPENDING TABLE XV_CYC
                           FOR ALL ENTRIES IN R_MATNR
                               WHERE MATNR EQ R_MATNR-LOW
                                 AND WERKS EQ OLD_WERKS
                                 AND ABCIN NE SPACE.

*  SELECT * FROM V_MMIM_CYC APPENDING TABLE XV_CYC
*                               WHERE MATNR IN R_MATNR
*                                 AND WERKS EQ OLD_WERKS
*                                 AND ABCIN NE SPACE.

  PERFORM RANGES_INIT.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM RANGES_INIT                                              *
*---------------------------------------------------------------------*
*       Zurücksetzen der RANGES                                       *
*---------------------------------------------------------------------*
FORM RANGES_INIT.

  CLEAR   C_MATNR.
  CLEAR   R_MATNR.
  CLEAR   R_WERKS.
  REFRESH R_MATNR.
  REFRESH R_WERKS.
  R_MATNR-SIGN   = 'I'.
  R_MATNR-OPTION = 'EQ'.
  R_WERKS-SIGN   = 'I'.
  R_WERKS-OPTION = 'EQ'.

ENDFORM.
