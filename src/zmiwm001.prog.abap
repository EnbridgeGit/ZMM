REPORT RM07IDRU MESSAGE-ID M7.

*---- Datendefinition -------------------------------------------------*

*--- Externe Tabellen -------------------------------------------------*
TABLES: ISEG,
        T159N,                         "Formularzuweisung
        V_MMIM_IPO.

TABLES: SSCRFIELDS.

TABLES: T001L.                         "JL 09/25/1996

*--------------------------
data:  GV_ENHANCEMENT TYPE ISC_MATERIALINFO_LESEN,
       GV_ZEILI(3), " type ISEG-ZEILI,
       GV_LGPBE type MABDR-LGPBE.
*--- Variablen --------------------------------------------------------
DATA: KZ,
      XMOVE,
      INDEX_VKBW LIKE SY-TABIX,
      INDEX_OHNE LIKE SY-TABIX,
      INDEX_I LIKE SY-TABIX,
      INDEX_J LIKE SY-TABIX,
      INDEX_W LIKE SY-TABIX,
      INDEX_O LIKE SY-TABIX,
      INDEX_G LIKE SY-TABIX,
      INDEX_X LIKE SY-TABIX,
      INDEX_L LIKE SY-TABIX,
      INDEX_V LIKE SY-TABIX,
      INDEX_Y LIKE SY-TABIX,
      INVENT  LIKE T159N-TDFORM,
      INVENT_VKBW LIKE T159N-TDFORM,
      M_ZAEHLER LIKE SY-TABIX,
      RAMAX LIKE SY-TABIX VALUE 20,
      RZAEHLER LIKE SY-TABIX.

DATA: NEW_PAGE.

DATA: START TYPE C.

*--- Interne Tabelle der Belege, die nicht gedruckt werden sollen, ----*
*--- wenn beim Lesen des Inventurkopfes kein Eintrag gefunden wird ----*
DATA: BEGIN OF DEL OCCURS 5.
        INCLUDE STRUCTURE V_MMIM_IPO.
DATA: END OF DEL.

*--- Interne Tabelle der Positionen, die nicht gedruckt werden  -------*
*--- sollen, wenn der Status der Positionen nicht übereinstimmt -------*
DATA: BEGIN OF BEL OCCURS 5.
        INCLUDE STRUCTURE V_MMIM_IPO.
DATA:   STEXT LIKE T064T-STEXT.
DATA: END OF BEL.

DATA: BEGIN OF IBLNR_KEY,
        MANDT LIKE ISEG-MANDT,
        IBLNR LIKE ISEG-IBLNR,
        GJAHR LIKE ISEG-GJAHR.
DATA: END OF IBLNR_KEY.

DATA: BEGIN OF XIKPF OCCURS 20.
        INCLUDE STRUCTURE IKPF.
DATA: END OF XIKPF.

DATA: BEGIN OF YIKPF OCCURS 20.
        INCLUDE STRUCTURE IKPF.
DATA: END OF YIKPF.

DATA: BEGIN OF ZISEG OCCURS 20.
        INCLUDE STRUCTURE ISEG.
DATA: BTEXT LIKE T064B-BTEXT.          "Bestandsartentext
DATA: GIDAT LIKE IKPF-GIDAT.           "Geplantes AufDat
DATA: MAKTX LIKE MAKT-MAKTX.           "Materialkurztext
DATA: STEXT LIKE T064T-STEXT.          "Status d. Position
DATA: LGPBE LIKE MABDR-LGPBE.          "JL 09/18/1996
DATA: END OF ZISEG.

DATA: BEGIN OF VISEG OCCURS 20.
        INCLUDE STRUCTURE ISEG.
DATA: BTEXT LIKE T064B-BTEXT.          "Bestandsartentext
DATA: GIDAT LIKE IKPF-GIDAT.           "Geplantes AufDat
DATA: MAKTX LIKE MAKT-MAKTX.           "Materialkurztext
DATA: STEXT LIKE T064T-STEXT.          "Status d. Position
DATA: END OF VISEG.

DATA: BEGIN OF XV_IPO OCCURS 20.
        INCLUDE STRUCTURE V_MMIM_IPO.
DATA: END OF XV_IPO.

*---- Include-Reports -------------------------------------------------*
INCLUDE MM07MABC.
INCLUDE RM07MAUT.
INCLUDE RM07MSQL.

SELECT-OPTIONS:
  R_IBLNR FOR V_MMIM_IPO-IBLNR MEMORY ID IBN,      "Inventurbeleg
  R_GJAHR FOR V_MMIM_IPO-GJAHR MEMORY ID GJA,      "Geschäftsjahr
  R_WERKS FOR V_MMIM_IPO-WERKS MEMORY ID WRK,      "Werk
  R_LGORT FOR IKPF-LGORT MEMORY ID LAG,            "Lagerort
  R_GIDAT FOR IKPF-GIDAT,                          "Gepl. Aufnahmedatum
  R_XBLNI FOR IKPF-XBLNI.                          "Inventurreferenz

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) TEXT-030.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: STIBN LIKE AM07M-XSELK.                "Statusausw. InvBeleg
SELECTION-SCREEN COMMENT 3(14) TEXT-031.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: STIPO LIKE AM07M-XSELK.                "Statusausw. InvPos
SELECTION-SCREEN COMMENT 3(18) TEXT-032.
SELECTION-SCREEN END OF LINE.
PARAMETERS:
    STIB1  LIKE AM07M-XSELK NO-DISPLAY,            "noch nicht gezählt
    STIB2  LIKE AM07M-XSELK NO-DISPLAY,            "teilweise gezählt
    STIB3  LIKE AM07M-XSELK NO-DISPLAY,            "vollständig gezählt
    STIP1  LIKE AM07M-XSELK NO-DISPLAY,            "noch nicht gezählt
    STIP2  LIKE AM07M-XSELK NO-DISPLAY,            "nur gezählt
    STIP3  LIKE AM07M-XSELK NO-DISPLAY,            "ausgebucht
    STIP4  LIKE AM07M-XSELK NO-DISPLAY,            "nachgezählt
    STIP5  LIKE AM07M-XSELK NO-DISPLAY,            "gelöscht
    STIP6  LIKE AM07M-XSELK NO-DISPLAY.            "nur gez. + gelöscht

AT SELECTION-SCREEN.
  IF NOT STIBN IS INITIAL.                      "Statusauswahl Inv.beleg
    PERFORM FUNCTION_CALLB.
    CLEAR STIBN.
  ENDIF.
  IF NOT STIPO IS INITIAL.                   "Statusauswahl Inv.position
    PERFORM FUNCTION_CALLP.
    CLEAR STIPO.
  ENDIF.

*---- Initialisierung -------------------------------------------------*
 INITIALIZATION.
  STIB1 = X.                                   "-
  STIB2 = X.                                   " -> alle Belege
  STIB3 = X.                                   "-

  STIP1 = X.                                   "-
  STIP2 = X.                                   " -
  STIP3 = X.                                   "  -> alle Positionen
  STIP4 = X.                                   " -
  STIP5 = X.                                   " -
  STIP6 = X.                                   "-

*---- Beginn der Verarbeitung -----------------------------------------*
START-OF-SELECTION.

  DESCRIBE TABLE R_IBLNR LINES INDEX_I.
  DESCRIBE TABLE R_GJAHR LINES INDEX_J.
  DESCRIBE TABLE R_WERKS LINES INDEX_W.
  DESCRIBE TABLE R_LGORT LINES INDEX_O.
  DESCRIBE TABLE R_GIDAT LINES INDEX_G.
  DESCRIBE TABLE R_XBLNI LINES INDEX_X.
  IF INDEX_I IS INITIAL AND
     INDEX_J IS INITIAL AND
     INDEX_W IS INITIAL AND
     INDEX_O IS INITIAL.
    IF NOT INDEX_G IS INITIAL OR
       NOT INDEX_X IS INITIAL.
*     nur gepl. Aufnahmedatum oder Inventurreferenz eingegeben
*     -> erst Inventurbelegkopf lesen und dann die Positionen
      PERFORM KOPF_POS_LESEN.
    ENDIF.
  ELSE.
*   entweder Inv.beleg, Gesch.jahr, Werk, Lgort oder Kombination eingeg.
*   -> erst Inventurbelegpositionen lesen und dann den Kopf
    PERFORM POS_KOPF_LESEN.
  ENDIF.
* In beiden Fällen zusätzliche Informationen zu den Positionen lesen
  PERFORM ERGAENZEN_POS.

*---- Ende der Verarbeitung -------------------------------------------*
END-OF-SELECTION.
  IF NOT NO_CHANCE IS INITIAL.
    MESSAGE I122 WITH XIKPF-WERKS.
    EXIT.
  ENDIF.

  SORT YISEG BY IBLNR ZEILI MATNR MAKTX CHARG BTEXT MEINS.
  DESCRIBE TABLE YISEG LINES INDEX_L.
  IF NOT INDEX_L IS INITIAL.
    LOOP AT YISEG.
      ON CHANGE OF YISEG-WERKS.
        PERFORM WERK_LESEN USING YISEG-WERKS.
        YISEG-WAERS = T001-WAERS.
        MODIFY YISEG.
      ENDON.
      IF T001K-XVKBW IS INITIAL.
        MOVE-CORRESPONDING YISEG TO ZISEG.
        APPEND ZISEG.
        SELECT * FROM T159N WHERE REPID = SY-REPID
                              AND XGUEL = X
                              AND KZDRU = '4'.
          INVENT = T159N-TDFORM.
          EXIT.
        ENDSELECT.
        IF NOT SY-SUBRC IS INITIAL OR INVENT IS INITIAL.
          MESSAGE I811.
          IF NOT SY-CALLD IS INITIAL.
            LEAVE.
          ELSE.
            LEAVE TO TRANSACTION SY-TCODE.
          ENDIF.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING YISEG TO VISEG.
        APPEND VISEG.
        SELECT * FROM T159N WHERE REPID = SY-REPID
                              AND XGUEL = X
                              AND KZDRU = '5'.
          INVENT_VKBW = T159N-TDFORM.
          EXIT.
        ENDSELECT.
        IF NOT SY-SUBRC IS INITIAL OR INVENT_VKBW IS INITIAL.
          MESSAGE I811.
          IF NOT SY-CALLD IS INITIAL.
            LEAVE.
          ELSE.
            LEAVE TO TRANSACTION SY-TCODE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE ZISEG LINES INDEX_OHNE.
  DESCRIBE TABLE VISEG LINES INDEX_VKBW.
  IF NOT INDEX_OHNE IS INITIAL.
    CALL FUNCTION 'OPEN_FORM'
         EXPORTING
              FORM     = INVENT
              DEVICE   = 'PRINTER'
         EXCEPTIONS
              CANCELED = 1
              FORM     = 2
              UNCLOSED = 3.
    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE S899.
      WHEN 2.
        MESSAGE S770 WITH INVENT.
      WHEN 3.
        MESSAGE S771 WITH INVENT.
      WHEN OTHERS.
        PERFORM AUSGABE_ZISEG.
        IF INDEX_VKBW IS INITIAL.
          CALL FUNCTION 'CLOSE_FORM'.
          COMMIT WORK.
          MESSAGE S815.
        ELSE.
          CALL FUNCTION 'END_FORM'.
        ENDIF.
    ENDCASE.
  ENDIF.

  IF NOT INDEX_VKBW IS INITIAL.
    IF NOT INDEX_OHNE IS INITIAL.
      CALL FUNCTION 'START_FORM'
           EXPORTING
                FORM     = INVENT_VKBW
                LANGUAGE = SY-LANGU
           EXCEPTIONS
                FORMAT   = 1
                FORM     = 2.
      CASE SY-SUBRC.
        WHEN 1.
          MESSAGE S780 WITH INVENT_VKBW.
        WHEN 2.
          MESSAGE S770 WITH INVENT_VKBW.
        WHEN OTHERS.
          PERFORM AUSGABE_VISEG.
          CALL FUNCTION 'CLOSE_FORM'.
          COMMIT WORK.
          MESSAGE S815.
      ENDCASE.
      IF NOT SY-CALLD IS INITIAL.
        LEAVE.
      ELSE.
        LEAVE TO TRANSACTION SY-TCODE.
      ENDIF.
    ELSE.
      CALL FUNCTION 'OPEN_FORM'
           EXPORTING
                FORM     = INVENT_VKBW
                DEVICE   = 'PRINTER'
           EXCEPTIONS
                CANCELED = 1
                FORM     = 2
                UNCLOSED = 3.
      CASE SY-SUBRC.
        WHEN 1.
          MESSAGE S899.
        WHEN 2.
          MESSAGE S770 WITH INVENT_VKBW.
        WHEN 3.
          MESSAGE S771 WITH INVENT_VKBW.
        WHEN OTHERS.
          PERFORM AUSGABE_VISEG.
          CALL FUNCTION 'CLOSE_FORM'.
          COMMIT WORK.
          MESSAGE S815.
      ENDCASE.
      IF NOT SY-CALLD IS INITIAL.
        LEAVE.
      ELSE.
        LEAVE TO TRANSACTION SY-TCODE.
      ENDIF.
    ENDIF.
  ENDIF.

*---- Formroutinen    -------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM AUSGABE_ZISEG                                            *
*----------------------------------------------------------------------*
*--- Formroutine, die die selektierten Belegpositionen sortiert -------*
*--- und aufbereitet ausgibt                              -------------*
*----------------------------------------------------------------------*
FORM AUSGABE_ZISEG.
* find out description of storage location                "JL 09/24/1996
SELECT SINGLE * FROM T001L WHERE WERKS = ZISEG-WERKS      "JL 09/24/1996
                             AND LGORT = ZISEG-LGORT.     "JL 09/24/1996

* update internal table ziseg so that it contains bin loc "JL 09/24/1996
* as one of its fields.                                   "JL 09/24/1996
LOOP AT ZISEG.                                            "JL 09/23/1996
   SELECT SINGLE * FROM MARD WHERE MATNR = ZISEG-MATNR    "JL 09/23/1996
                               AND WERKS = ZISEG-WERKS    "JL 09/23/1996
                               AND LGORT = ZISEG-LGORT.   "JL 09/23/1996
   MOVE MARD-LGPBE TO ZISEG-LGPBE.                        "JL 09/23/1996
   MODIFY ZISEG.                                          "JL 09/23/1996
ENDLOOP.                                                  "JL 09/23/1996

* sort the output in the order of bin loc, then material# "JL 09/23/1996
* sort ziseg by iblnr zeili matnr maktx charg btext meins.
  SORT ZISEG BY IBLNR LGPBE MATNR ZEILE MAKTX CHARG BTEXT MEINS.
  LOOP AT ZISEG.
    KZ_SELECT = X.
    ON CHANGE OF ZISEG-IBLNR OR ZISEG-LIFNR OR ZISEG-KUNNR
              OR ZISEG-KDAUF OR ZISEG-PS_PSP_PNR.
      MOVE-CORRESPONDING ZISEG TO ISEG.
      SELECT SINGLE * FROM IKPF WHERE IBLNR EQ ZISEG-IBLNR
                                  AND GJAHR EQ ZISEG-GJAHR.
      MOVE ZISEG-GIDAT TO IKPF-GIDAT.
      MOVE ZISEG-SOBKZ TO IKPF-SOBKZ.
      CASE ZISEG-SOBKZ.
        WHEN 'E'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB0'.
        WHEN 'K'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'M'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'O'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'Q'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB4'.
        WHEN 'V'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB2'.
        WHEN 'W'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB2'.
        WHEN OTHERS.
          PERFORM UEBERSCHRIFT.
      ENDCASE.
    ENDON.
    ON CHANGE OF ZISEG-MATNR OR ZISEG-IBLNR.
      PERFORM MATERIALINFO_LESEN USING ZISEG-MATNR
                                       ZISEG-WERKS
                                       ZISEG-LGORT
                                 changing  GV_ENHANCEMENT.
    ENDON.
    ON CHANGE OF ZISEG-MATNR OR ZISEG-BSTAR OR ZISEG-CHARG OR
                 ZISEG-IBLNR OR ZISEG-LIFNR OR ZISEG-KUNNR OR
                 ZISEG-KDAUF OR ZISEG-PS_PSP_PNR.
      MOVE-CORRESPONDING ZISEG TO ISEG.
      MOVE ZISEG-STEXT TO T064T-STEXT.
      MOVE ZISEG-BTEXT TO T064B-BTEXT.
      CASE ZISEG-SOBKZ.
        WHEN 'E'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB0'.
        WHEN 'K'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'M'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'O'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'Q'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB4'.
        WHEN 'V'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB2'.
        WHEN 'W'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB2'.
        WHEN OTHERS.
          PERFORM POSITIONEN.
      ENDCASE.
    ENDON.
    ON CHANGE OF ZISEG-IBLNR OR ZISEG-LIFNR OR ZISEG-KUNNR OR
                 ZISEG-KDAUF OR ZISEG-PS_PSP_PNR.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'POS_ENDPAGE'
                TYPE    = 'BOTTOM'.
    ENDON.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM AUSGABE_VISEG                                            *
*----------------------------------------------------------------------*
*--- Formroutine, die die selektierten Belegpositionen sortiert -------*
*--- und aufbereitet ausgibt                              -------------*
*----------------------------------------------------------------------*
FORM AUSGABE_VISEG.
  SORT VISEG BY IBLNR ZEILI MATNR MAKTX CHARG BTEXT MEINS.
  LOOP AT VISEG.
    KZ_SELECT = X.
    ON CHANGE OF VISEG-IBLNR OR VISEG-LIFNR OR VISEG-KUNNR
              OR VISEG-KDAUF OR VISEG-PS_PSP_PNR.
      MOVE-CORRESPONDING VISEG TO ISEG.
      SELECT SINGLE * FROM IKPF WHERE IBLNR EQ VISEG-IBLNR
                                  AND GJAHR EQ VISEG-GJAHR.
      MOVE VISEG-GIDAT TO IKPF-GIDAT.
      MOVE VISEG-SOBKZ TO IKPF-SOBKZ.
      CASE VISEG-SOBKZ.
        WHEN 'E'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB0'.
        WHEN 'K'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'M'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'O'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'Q'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB4'.
        WHEN 'V'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB2'.
        WHEN 'W'.
          PERFORM UEB_SONDERBESTAND USING 'POS_UEB2'.
        WHEN OTHERS.
          PERFORM UEBERSCHRIFT.
      ENDCASE.
    ENDON.
    ON CHANGE OF VISEG-MATNR OR VISEG-IBLNR.
      PERFORM MATERIALINFO_LESEN USING VISEG-MATNR
                                       VISEG-WERKS
                                       VISEG-LGORT
                                 Changing gv_ENHANCEMENT.
    ENDON.
    ON CHANGE OF VISEG-MATNR OR VISEG-BSTAR OR VISEG-CHARG OR
                 VISEG-IBLNR OR VISEG-LIFNR OR VISEG-KUNNR OR
                 VISEG-KDAUF OR VISEG-PS_PSP_PNR.
      MOVE-CORRESPONDING VISEG TO ISEG.
      MOVE VISEG-STEXT TO T064T-STEXT.
      MOVE VISEG-BTEXT TO T064B-BTEXT.
      CASE VISEG-SOBKZ.
        WHEN 'E'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB0'.
        WHEN 'K'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'M'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'O'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB1'.
        WHEN 'Q'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB4'.
        WHEN 'V'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB2'.
        WHEN 'W'.
          PERFORM POS_SONDERBESTAND USING 'POS_UEB2'.
        WHEN OTHERS.
          PERFORM POSITIONEN.
      ENDCASE.
    ENDON.
    ON CHANGE OF VISEG-IBLNR OR VISEG-LIFNR OR VISEG-KUNNR OR
                 VISEG-KDAUF OR VISEG-PS_PSP_PNR.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'POS_ENDPAGE'
                TYPE    = 'BOTTOM'.
    ENDON.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ERGAENZEN_POS                                            *
*----------------------------------------------------------------------*
*--- Formroutine, die: 1. die Bestandsart liest,              ---------*
*---                   2. die Texte zur Inventur liest,       ---------*
*---                   3. ein Update auf die YISEG durchführt ---------*
*----------------------------------------------------------------------*
FORM ERGAENZEN_POS.
  LOOP AT YISEG.
    SORT YIKPF BY MANDT IBLNR GJAHR.
    MOVE-CORRESPONDING YISEG TO IBLNR_KEY.
    READ TABLE YIKPF WITH KEY IBLNR_KEY BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      MOVE YIKPF-WERKS TO YISEG-WERKS.
      MOVE YIKPF-SOBKZ TO YISEG-SOBKZ.
      PERFORM BSTAR_LESEN USING YISEG-BSTAR.
      PERFORM T064T_LESEN.
      YISEG-STEXT = T064T-STEXT.
      IF NOT SY-SUBRC IS INITIAL.
        CLEAR T064T.
      ENDIF.
      YISEG-BTEXT = T064B-BTEXT.
      YISEG-GIDAT = YIKPF-GIDAT.
      MODIFY YISEG.
    ELSE.
      DELETE YISEG.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FUNCTION_CALLB                                           *
*---------------------------------------------------------------------*
*--- Formroutine, die den Funktionsbaustein Window   -----------------*
*--- Beleg aufruft                                   -----------------*
*---------------------------------------------------------------------*
FORM FUNCTION_CALLB.
  CALL FUNCTION 'RM_SENDEN_BILD_INM'
       EXPORTING
            IX_SELB1 = STIB1
            IX_SELB2 = STIB2
            IX_SELB3 = STIB3
       IMPORTING
            IX_SELB1 = STIB1
            IX_SELB2 = STIB2
            IX_SELB3 = STIB3
            IX_START = START.

  IF START = X.
    SY-UCOMM = 'ONLI'.
    SSCRFIELDS-UCOMM = 'ONLI'.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FUNCTION_CALLP                                           *
*---------------------------------------------------------------------*
*--- Formroutine, die den Funktionsbaustein Window   -----------------*
*--- Position aufruft                                -----------------*
*---------------------------------------------------------------------*
FORM FUNCTION_CALLP.
  CALL FUNCTION 'RM_SENDEN_BILD_INMP'
       EXPORTING
            IX_SELP1 = STIP1
            IX_SELP2 = STIP2
            IX_SELP3 = STIP3
            IX_SELP4 = STIP4
            IX_SELP5 = STIP5
            IX_SELP6 = STIP6
       IMPORTING
            IX_SELP1 = STIP1
            IX_SELP2 = STIP2
            IX_SELP3 = STIP3
            IX_SELP4 = STIP4
            IX_SELP5 = STIP5
            IX_SELP6 = STIP6
            IX_START = START.

  IF START = X.
    SY-UCOMM = 'ONLI'.
    SSCRFIELDS-UCOMM = 'ONLI'.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM KOPF_LESEN                                               *
*---------------------------------------------------------------------*
*--- Formroutine, die zu bereits gelesenen Positionen     ------------*
*--- die zugehörigen Köpfe liest und zu diesen       -----------------*
*--- Köpfen die Statusauswahl aufruft                -----------------*
*---------------------------------------------------------------------*
FORM KOPF_LESEN.
  SELECT * FROM IKPF WHERE IBLNR IN R_IBLNR
                       AND GJAHR IN R_GJAHR
                       AND WERKS IN R_WERKS
                       AND LGORT IN R_LGORT
                       AND GIDAT IN R_GIDAT
                       AND XBLNI IN R_XBLNI.
    MOVE-CORRESPONDING IKPF TO XIKPF.
    COLLECT XIKPF.
  ENDSELECT.
  DESCRIBE TABLE XIKPF LINES INDEX_L.
  IF NOT INDEX_L IS INITIAL.
    LOOP AT XIKPF.
*     Statusauswahl Kopf
      PERFORM STATUS_BELEG.
    ENDLOOP.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM KOPF_POS_LESEN                                           *
*----------------------------------------------------------------------*
*--- Formroutine, die zuerst die Inventurbelegköpfe und dann die ------*
*--- dazugehörigen Positionen liest, wenn entweder ein geplantes  -----*
*--- Aufnahmedatum oder eine Inventurreferenz eingegeben wurden   -----*
*----------------------------------------------------------------------*
FORM KOPF_POS_LESEN.
  SELECT * FROM IKPF APPENDING TABLE XIKPF
                         WHERE GIDAT IN R_GIDAT
                           AND XBLNI IN R_XBLNI.
  DESCRIBE TABLE XIKPF LINES INDEX_L.
  IF INDEX_L IS INITIAL.
    MESSAGE S840.
    EXIT.
  ELSE.
    LOOP AT XIKPF.
*     Statusauswahl Inventurbelege
      PERFORM STATUS_BELEG.
    ENDLOOP.
  ENDIF.
  DESCRIBE TABLE YIKPF LINES INDEX_V.
  IF INDEX_V IS INITIAL.
    MESSAGE S840.
    EXIT.
  ELSE.
*   Ranges zurücksetzen und neu füllen
    CLEAR R_IBLNR.
    REFRESH R_IBLNR.
    R_IBLNR-SIGN   = 'I'.
    R_IBLNR-OPTION = 'EQ'.
    CLEAR R_GJAHR.
    REFRESH R_GJAHR.
    R_GJAHR-SIGN   = 'I'.
    R_GJAHR-OPTION = 'EQ'.
    CLEAR R_WERKS.
    REFRESH R_WERKS.
    R_WERKS-SIGN   = 'I'.
    R_WERKS-OPTION = 'EQ'.
    CLEAR R_LGORT.
    REFRESH R_LGORT.
    R_LGORT-SIGN   = 'I'.
    R_LGORT-OPTION = 'EQ'.
    LOOP AT YIKPF.
      MOVE YIKPF-IBLNR TO R_IBLNR-LOW.
      COLLECT R_IBLNR.
      MOVE YIKPF-GJAHR TO R_GJAHR-LOW.
      COLLECT R_GJAHR.
      MOVE YIKPF-WERKS TO R_WERKS-LOW.
      COLLECT R_WERKS.
      MOVE YIKPF-LGORT TO R_LGORT-LOW.
      COLLECT R_LGORT.
      RZAEHLER = RZAEHLER + 1.
      M_ZAEHLER = M_ZAEHLER + 1.
      IF RZAEHLER = RAMAX.
*       Die zu den Köpfen gehörenden Pos. werden portionsweise gelesen
        PERFORM POS_LESEN.
        REFRESH R_IBLNR.
        REFRESH R_GJAHR.
        REFRESH R_WERKS.
        REFRESH R_LGORT.
        CLEAR RZAEHLER.
        IF M_ZAEHLER = INDEX_V.
          CLEAR M_ZAEHLER.
          CLEAR INDEX_V.
          EXIT.
        ENDIF.
      ENDIF.

      IF M_ZAEHLER = INDEX_V.
*       Die zu den Köpfen gehörenden Pos. werden portionsweise gelesen
        PERFORM POS_LESEN.
        REFRESH R_IBLNR.
        REFRESH R_GJAHR.
        REFRESH R_WERKS.
        REFRESH R_LGORT.
        CLEAR RZAEHLER.
        CLEAR M_ZAEHLER.
        CLEAR INDEX_V.
        EXIT.
      ENDIF.
    ENDLOOP.
    DESCRIBE TABLE YISEG LINES INDEX_L.
    IF INDEX_L IS INITIAL.
      MESSAGE S840.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM POS_LESEN                                                *
*---------------------------------------------------------------------*
*--- Formroutine, die zu bereits gelesenen Köpfen die ----------------*
*--- zugehörigen Positionen liest und zu diesen      -----------------*
*--- Positionen die Statusauswahl aufruft            -----------------*
*---------------------------------------------------------------------*
FORM POS_LESEN.
  SELECT * FROM V_MMIM_IPO APPENDING TABLE XV_IPO
                           WHERE IBLNR IN R_IBLNR
                             AND GJAHR IN R_GJAHR
                             AND WERKS IN R_WERKS
                             AND LGORT IN R_LGORT.
  DESCRIBE TABLE XV_IPO LINES INDEX_L.
  IF NOT INDEX_L IS INITIAL.
    LOOP AT XV_IPO.
      CLEAR XMOVE.
*     Statusauswahl Position
      PERFORM STATUS_POS.
    ENDLOOP.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM POS_KOPF_LESEN                                           *
*----------------------------------------------------------------------*
*--- Formroutine, die zuerst die Inventurbelegpositionen und dann------*
*--- die dazugehörigen Köpfe liest, wenn entweder ein Inventur-   -----*
*--- beleg, ein Geschäftsjahr, ein Werk oder ein Lagerort einge-  -----*
*--- geben wurden                                                 -----*
*----------------------------------------------------------------------*
FORM POS_KOPF_LESEN.
  SELECT * FROM V_MMIM_IPO APPENDING TABLE XV_IPO
                           WHERE IBLNR IN R_IBLNR
                             AND GJAHR IN R_GJAHR
                             AND WERKS IN R_WERKS
                             AND LGORT IN R_LGORT.
  DESCRIBE TABLE XV_IPO LINES INDEX_L.
  IF INDEX_L IS INITIAL.
    MESSAGE S840.
    EXIT.
  ELSE.
    LOOP AT XV_IPO.
      CLEAR XMOVE.
*     Statusauswahl Inventurpositionen
      PERFORM STATUS_POS.
    ENDLOOP.
  ENDIF.
  DESCRIBE TABLE YISEG LINES INDEX_Y.
  IF INDEX_Y IS INITIAL.
    MESSAGE S840.
    EXIT.
  ELSE.
*   Ranges zurücksetzen und neu füllen
    CLEAR R_IBLNR.
    REFRESH R_IBLNR.
    R_IBLNR-SIGN   = 'I'.
    R_IBLNR-OPTION = 'EQ'.
    CLEAR R_GJAHR.
    REFRESH R_GJAHR.
    R_GJAHR-SIGN   = 'I'.
    R_GJAHR-OPTION = 'EQ'.
    CLEAR R_WERKS.
    REFRESH R_WERKS.
    R_WERKS-SIGN   = 'I'.
    R_WERKS-OPTION = 'EQ'.
    CLEAR R_LGORT.
    REFRESH R_LGORT.
    R_LGORT-SIGN   = 'I'.
    R_LGORT-OPTION = 'EQ'.

    LOOP AT YISEG.
      MOVE YISEG-IBLNR TO R_IBLNR-LOW.
      COLLECT R_IBLNR.
      MOVE YISEG-GJAHR TO R_GJAHR-LOW.
      COLLECT R_GJAHR.
      MOVE YISEG-WERKS TO R_WERKS-LOW.
      COLLECT R_WERKS.
      MOVE YISEG-LGORT TO R_LGORT-LOW.
      COLLECT R_LGORT.
      RZAEHLER = RZAEHLER + 1.
      M_ZAEHLER = M_ZAEHLER + 1.
      IF RZAEHLER = RAMAX.
*       Die zu den Pos. gehörenden Köpfe werden portionsweise gelesen
        PERFORM KOPF_LESEN.
        REFRESH R_IBLNR.
        REFRESH R_GJAHR.
        REFRESH R_WERKS.
        REFRESH R_LGORT.
        CLEAR RZAEHLER.
        IF M_ZAEHLER = INDEX_Y.
          CLEAR M_ZAEHLER.
          CLEAR INDEX_Y.
          EXIT.
        ENDIF.
      ENDIF.

      IF M_ZAEHLER = INDEX_Y.
*       Die zu den Pos. gehörenden Köpfe werden portionsweise gelesen
        PERFORM KOPF_LESEN.
        REFRESH R_IBLNR.
        REFRESH R_GJAHR.
        REFRESH R_WERKS.
        REFRESH R_LGORT.
        CLEAR RZAEHLER.
        CLEAR M_ZAEHLER.
        CLEAR INDEX_Y.
        EXIT.
      ENDIF.
    ENDLOOP.
    DESCRIBE TABLE YIKPF LINES INDEX_L.
    IF INDEX_L IS INITIAL.
      MESSAGE S840.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM POS_SONDERBESTAND                                        *
*---------------------------------------------------------------------*
*--- Formroutine, die abhängig vom Sonderbestand die -----------------*
*--- Positionszeilen ausgibt                         -----------------*
*---------------------------------------------------------------------*
*  -->  Parameter, ob Sonderbestand Lieferant oder Kunde              *
*---------------------------------------------------------------------*
FORM POS_SONDERBESTAND USING UEB.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = UEB
            TYPE     = 'TOP'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'POS_BELZEILE'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT  = UEB
            FUNCTION = 'DELETE'
            TYPE     = 'TOP'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM POSITIONEN                                               *
*---------------------------------------------------------------------*
*--- Formroutine, die die Positionszeilen ausgibt    -----------------*
*--- wenn kein Sonderbestand vorliegt                -----------------*
*---------------------------------------------------------------------*
FORM POSITIONEN.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'POS_UEB3'
            TYPE     = 'TOP'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'POS_BELZEILE'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT  = 'POS_UEB3'
            FUNCTION = 'DELETE'
            TYPE     = 'TOP'.
* CALL FUNCTION 'WRITE_FORM'
*      EXPORTING
*           ELEMENT = 'POS_ENDPAGE'
*           TYPE    = 'BOTTOM'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM STATUS_BELEG                                             *
*---------------------------------------------------------------------*
*--- Formroutine, die zu bereits gelesenen Köpfen die     ------------*
*--- Statusauswahl und die Berechtigung prüft        -----------------*
*---------------------------------------------------------------------*
FORM STATUS_BELEG.
  IF STIB1 = X.                    "alle noch nicht gezählten Bel.
    IF XIKPF-ZSTAT EQ SPACE.
      MOVE-CORRESPONDING XIKPF TO YIKPF.
      COLLECT YIKPF.
    ENDIF.
  ENDIF.
  IF STIB2 = X.                        "alle angezählten Belege
    IF XIKPF-ZSTAT = A.
      MOVE-CORRESPONDING XIKPF TO YIKPF.
      COLLECT YIKPF.
    ENDIF.
  ENDIF.
  IF STIB3 = X.                      "alle vollständig gez. Belege
    IF XIKPF-ZSTAT = X.
      MOVE-CORRESPONDING XIKPF TO YIKPF.
      COLLECT YIKPF.
    ENDIF.
  ENDIF.
  CLEAR AUTH04.
  PERFORM INVENTUR_IB_DRUCK USING ACTVT04
                            XIKPF-WERKS.
  IF NO_CHANCE IS INITIAL.
    IF NOT AUTH04 IS INITIAL.
      NO_CHANCE = X.
    ENDIF.
  ENDIF.
  CHECK AUTH04 IS INITIAL.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM STATUS_POS                                               *
*---------------------------------------------------------------------*
*--- Formroutine, die zu bereits gelesenen Positionen     ------------*
*--- die Statusauswahl prüft                         -----------------*
*---------------------------------------------------------------------*
FORM STATUS_POS.
  IF STIP1 = X.                         "alle noch nicht gez. Pos.
    IF XV_IPO-XZAEL IS INITIAL AND
       XV_IPO-XNZAE IS INITIAL AND
       XV_IPO-XLOEK IS INITIAL.
      MOVE-CORRESPONDING XV_IPO TO YISEG.
      APPEND YISEG.
      XMOVE = X.
      CHECK XMOVE NE X.
    ENDIF.
  ENDIF.
  IF STIP2 = X.                          "alle nur gezählten Pos.
    IF XV_IPO-XZAEL = X AND
       XV_IPO-XDIFF IS INITIAL AND
       XV_IPO-XNZAE IS INITIAL AND
       XV_IPO-XLOEK IS INITIAL.
      MOVE-CORRESPONDING XV_IPO TO YISEG.
      APPEND YISEG.
      XMOVE = X.
      CHECK XMOVE NE X.
    ENDIF.
  ENDIF.
  IF STIP3 = X.                          "alle ausgebuchten Pos.
    IF XV_IPO-XDIFF = X.
      MOVE-CORRESPONDING XV_IPO TO YISEG.
      APPEND YISEG.
      XMOVE = X.
      CHECK XMOVE NE X.
    ENDIF.
  ENDIF.
  IF STIP4 = X.                          "alle nachgezählten Pos.
    IF XV_IPO-XNZAE = X.
      MOVE-CORRESPONDING XV_IPO TO YISEG.
      APPEND YISEG.
      XMOVE = X.
      CHECK XMOVE NE X.
    ENDIF.
  ENDIF.
  IF STIP5 = X.                        "alle gelöschten Positionen
    IF XV_IPO-XLOEK = X.
      MOVE-CORRESPONDING XV_IPO TO YISEG.
      APPEND YISEG.
      XMOVE = X.
      CHECK XMOVE NE X.
    ENDIF.
  ENDIF.
  IF STIP6 = X.                 "alle nur gez. und gelöschten Pos.
    IF XV_IPO-XLOEK = X AND
       XV_IPO-XZAEL = X AND
       XV_IPO-XDIFF IS INITIAL.
      MOVE-CORRESPONDING XV_IPO TO YISEG.
      APPEND YISEG.
      XMOVE = X.
      CHECK XMOVE NE X.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UEB_SONDERBESTAND                                        *
*---------------------------------------------------------------------*
*--- Formroutine, die abhängig vom Sonderbestand die -----------------*
*--- Ueberschriftenzeilen ausgibt                    -----------------*
*---------------------------------------------------------------------*
*  -->  Parameter, ob Sonderbestand Lieferant oder Kunde              *
*---------------------------------------------------------------------*
FORM UEB_SONDERBESTAND USING UEB.
  IF NEW_PAGE IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = UEB
              TYPE    = 'TOP'.
    NEW_PAGE = X.
  ELSE.
*   CALL FUNCTION 'WRITE_FORM'
*        EXPORTING
*             ELEMENT = 'POS_ENDPAGE'
*             TYPE    = 'BOTTOM'.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'POS_NEWPAGE'.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = UEB.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM UEBERSCHRIFT                                             *
*---------------------------------------------------------------------*
*--- Formroutine, die die Ueberschriftenzeilen       -----------------*
*--- ausgibt wenn kein Sonderbestand vorliegt        -----------------*
*---------------------------------------------------------------------*
FORM UEBERSCHRIFT.
  IF NEW_PAGE IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'POS_UEB3'
              TYPE    = 'TOP'.
    NEW_PAGE = X.
  ELSE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'POS_NEWPAGE'.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'POS_UEB3'.
  ENDIF.
ENDFORM.

*eject
