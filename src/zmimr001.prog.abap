REPORT RM07IDIF MESSAGE-ID M7 NO STANDARD PAGE HEADING LINE-SIZE 131.


*---- Datendefinition -------------------------------------------------*

*---- Externe Tabellen ------------------------------------------------*
TABLES: ISEG,
        MCHA,
        TCURX,
        VM07I.

TABLES: T001L.      "JL 10/03/96  -  for storage location name

*--- Variablen --------------------------------------------------------*
DATA: H_BWKEY    LIKE T001W-BWKEY,
      H_XRUEM,
      BLANK(256) TYPE C VALUE ' ',
      EXPONENT   TYPE I,
      X_INTENSIFIED TYPE C,
      X_SELKZ    TYPE C,
      Y_SELKZ    TYPE C,
      V_HEAD(4)  TYPE C,
      V_REST     LIKE SY-LINNO,
      KENNG(5)   TYPE C VALUE 'IIKPF',
      INDEX_L    LIKE SY-TABIX,
      INDEX_S    LIKE SY-TABIX,
      MEINS      LIKE ISEG-MEINS,
      WAERS      LIKE ISEG-WAERS,
      REFE1      LIKE AM07M-IWERT,
      IWERT      LIKE AM07M-IWERT.

*--- Interne Tabelle für nicht ausbuchbare Belege ---------------------*
DATA: BEGIN OF BEL OCCURS 50,
        IBLNR LIKE ISEG-IBLNR,
        WERKS LIKE ISEG-WERKS,
        LGORT LIKE ISEG-LGORT,
        GJAHR LIKE ISEG-GJAHR,
        BLDAT LIKE IKPF-BLDAT,
        BUDAT LIKE IKPF-BUDAT,
        SPERR LIKE IKPF-SPERR,
        ZSTAT LIKE IKPF-ZSTAT,
        DSTAT LIKE IKPF-DSTAT,
      END OF BEL.

DATA: BEGIN OF SUMTAB OCCURS 50,
        WERKS LIKE ISEG-WERKS,
        LGORT LIKE ISEG-LGORT,
        MATNR LIKE ISEG-MATNR,
        CHARG LIKE ISEG-CHARG,
        IBLNR LIKE ISEG-IBLNR,
        ZEILI LIKE ISEG-ZEILI,
        GJAHR LIKE ISEG-GJAHR,
        BUCHM LIKE ISEG-BUCHM,
        MENGE LIKE ISEG-MENGE,
        DIFMG LIKE VM07I-DIFMG,
        MEINS LIKE ISEG-MEINS,
        DMBTR LIKE ISEG-DMBTR,
        VKWRT LIKE ISEG-VKWRT,
        EXVKW LIKE ISEG-EXVKW,
        WAERS LIKE ISEG-WAERS,
      END OF SUMTAB.

*------- Interne Tabelle für Inventurbelegpositionsdaten --------------*
DATA: BEGIN OF XISEG OCCURS 100.
        INCLUDE STRUCTURE ISEG.
DATA: END OF XISEG.

*----- Strukturen fuer KEY's zum direkten Lesen interner Tabellen -----*
DATA: BEGIN OF IKPF_KEY,
        MANDT LIKE SY-MANDT,
        IBLNR LIKE IKPF-IBLNR,
        GJAHR LIKE IKPF-GJAHR,
      END OF IKPF_KEY.

DATA: BEGIN OF MAKT_KEY,
        MANDT LIKE SY-MANDT,
        MATNR LIKE MAKT-MATNR,
      END OF MAKT_KEY.

DATA: BEGIN OF BEL_KEY,
        IBLNR LIKE IKPF-IBLNR,
        WERKS LIKE IKPF-WERKS,
        LGORT LIKE IKPF-LGORT,
      END OF BEL_KEY.

DATA: PARAMS LIKE PRI_PARAMS,    "JL 10/03/96
      VALID  TYPE C.             "JL 10/03/96

*---- Parameter       -------------------------------------------------*
SELECT-OPTIONS GJAHR FOR ISEG-GJAHR.
PARAMETER IIWERT(13) TYPE C.               "Schwellenwert im CHAR-Format
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: SUMAT LIKE AM07M-XPROT.
SELECTION-SCREEN COMMENT 3(30) TEXT-102.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: SUWERK LIKE AM07M-XPROT DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(30) TEXT-103.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: SULORT LIKE AM07M-XPROT DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(30) TEXT-104.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XSEIT LIKE AM07M-XSELK.
SELECTION-SCREEN COMMENT 3(28) TEXT-105.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.                      "JL 10/03/96
PARAMETERS: PRT_OP1 RADIOBUTTON GROUP RAD1,  "JL 10/03/96
            PRT_OP2 RADIOBUTTON GROUP RAD1,  "JL 10/03/96
            PRT_OP3 RADIOBUTTON GROUP RAD1.  "JL 10/03/96

*---- Include-Reports -------------------------------------------------*
INCLUDE MM07MABC.
INCLUDE RM07MAUT.
*include rm07msql.                           "JL 10/17/96
INCLUDE ZNMIM004.                            "JL 10/17/96
INCLUDE RM07MUSR.
INCLUDE RM07MEND.

*--- Feldleiste für Darstellung Buchungsperiode -----------------------*
DATA: BEGIN OF BUPER,
        MONAT LIKE IKPF-MONAT,
        PUNKT VALUE '.',
        GJAHR LIKE IKPF-GJAHR,
      END OF BUPER.

*---- Initialisierung -------------------------------------------------*
INITIALIZATION.
  CLEAR: IM_SELP1,
         IM_SELP3,
         IM_SELP4,
         IM_SELP5,
         IM_SELP6.
  IM_SELB1 = IM_SELB2 = IM_SELB3 = X.

*---- Seitenüberschrift
TOP-OF-PAGE.

  CASE V_HEAD.

    WHEN 'SBEL'.
      DESCRIBE TABLE YISEG LINES INDEX_L.
      IF NOT INDEX_L IS INITIAL.
        SKIP 2.
      ENDIF.
      FORMAT RESET.
      WRITE:/(60) TEXT-030.
      SKIP 1.
      WRITE: /(68) SY-ULINE.
      WRITE: / SY-VLINE NO-GAP.
      FORMAT COLOR 1 INTENSIFIED.
      WRITE: 2(66) BLANK NO-GAP,
             2 TEXT-040 NO-GAP,
            68 SY-VLINE NO-GAP.
      FORMAT RESET.
      WRITE: /(68) SY-ULINE.
  ENDCASE.

*----------------------------------------------------------------------*
*        TOP-OF-PAGE DURING LINE-SELECTION
*----------------------------------------------------------------------*
TOP-OF-PAGE DURING LINE-SELECTION.

  FORMAT RESET.
  WRITE: /(60) TEXT-070.
  SKIP.
  WRITE: /(88) SY-ULINE.
  WRITE: / SY-VLINE NO-GAP.
  FORMAT COLOR 1 INTENSIFIED.
  WRITE: 2(86) BLANK NO-GAP.
  IF SUMAT IS INITIAL.
    WRITE: 2 TEXT-109 NO-GAP.
  ELSE.
    WRITE: 2 TEXT-110 NO-GAP.
  ENDIF.
  WRITE: 88 SY-VLINE NO-GAP.
  FORMAT RESET.
  WRITE: / SY-VLINE NO-GAP.
  FORMAT COLOR 1 INTENSIFIED.
  WRITE: 2(86) BLANK NO-GAP.
  WRITE: 2 TEXT-111 NO-GAP,
        88 SY-VLINE NO-GAP.
  FORMAT RESET.
  WRITE: /(88) SY-ULINE.

*---- Beginn der Verarbeitung -----------------------------------------*
START-OF-SELECTION.
  IF SUMAT IS INITIAL AND
     SULORT IS INITIAL AND
     SUWERK IS INITIAL.
    SET PF-STATUS '99'.
  ELSE.
    SET PF-STATUS '100'.
  ENDIF.
  CLEAR NO_CHANCE.
  SET TITLEBAR '100'.                      "4.6B - added quotes

*---- Beschaffen der Daten --------------------------------------------*
GET ISEG.
  PERFORM XISEG_FUELLEN.

*---- Ende der Verarbeitung -------------------------------------------*
END-OF-SELECTION.

IF PRT_OP1 = 'X'.                                  "JL 10/03/96
   "show print parameter                           "JL 10/03/96
   CALL FUNCTION 'GET_PRINT_PARAMETERS'            "JL 10/03/96
        IMPORTING OUT_PARAMETERS = PARAMS          "JL 10/03/96
                  VALID          = VALID.          "JL 10/03/96
   NEW-PAGE PRINT ON PARAMETERS PARAMS NO DIALOG.  "JL 10/03/96
ELSEIF PRT_OP2 = 'X'.                              "JL 10/03/96
   "do not show print parameter                    "JL 10/03/96
   CALL FUNCTION 'SET_PRINT_PARAMETERS'            "JL 10/03/96
        EXPORTING COPIES      = '1'                "JL 10/03/96
                  DESTINATION = 'SP4S'             "JL 10/03/96
                  IMMEDIATELY = ' '                "JL 10/03/96
                  LINE_COUNT  = 59                 "JL 10/03/96
                  LINE_SIZE   = 131.               "JL 10/03/96
   NEW-PAGE PRINT ON PARAMETERS PARAMS NO DIALOG.  "JL 10/03/96
   MESSAGE S567.                                   "JL 10/03/96
ENDIF.                                             "JL 10/03/96

* Schwellwert mit 100 wegen interner Darstellung multiplizieren
* IWERT = IWERT * 100.

* Verarbeitungbeginn
  PERFORM BELEGKOPF_LESEN_ARRAY USING KENNG.
  PERFORM KURZTEXT_LESEN_ARRAY.
  PERFORM YISEG_FUELLEN.
  IF NOT NO_CHANCE IS INITIAL.
    MESSAGE S124.
  ENDIF.
  DESCRIBE TABLE YISEG LINES INDEX_L.
  IF NOT INDEX_L IS INITIAL.
    PERFORM AUSGABE_YISEG.
  ENDIF.
  DESCRIBE TABLE BEL LINES INDEX_L.
  IF NOT INDEX_L IS INITIAL.
    PERFORM AUSGABE_BEL.
  ENDIF.

*---- Formroutinen ----------------------------------------------------*
FORM HEADER_BELEG.

  IF V_HEAD IS INITIAL.
    SKIP 2.
  ELSE.
    CLEAR V_HEAD.
  ENDIF.
  V_REST = 90 - SY-LINNO.
  IF V_REST < 8.
    NEW-PAGE.
  ENDIF.
*-----------------------------------------------------------------------
* output the page header                          "JL 10/03/96
*-----------------------------------------------------------------------
SELECT SINGLE * FROM T001W WHERE WERKS = YISEG-WERKS.    "JL 10/03/96
SELECT SINGLE * FROM T001L WHERE WERKS = YISEG-WERKS     "JL 10/03/96
                             AND LGORT = YISEG-LGORT.    "JL 10/03/96
  BUPER-MONAT = YISEG-MONAT.
  BUPER-GJAHR = YISEG-GJAHR.
  MOVE X TO X_SELKZ.
  WRITE:/ 'Physical Inventory Differences Document' COLOR 1. "JL 10/03
  SKIP.                                            "JL 10/03/96
  FORMAT COLOR OFF INTENSIFIED.
  WRITE:/(18)   TEXT-010.
  FORMAT RESET.
  WRITE: 20 YISEG-IBLNR.
  FORMAT COLOR OFF INTENSIFIED.
  WRITE: 39(18) TEXT-012.
  FORMAT RESET.
* write: 58 yiseg-werks.
  WRITE: 58 YISEG-WERKS, '-', T001W-ORT01.        "JL 10/03/96
  HIDE:  YISEG-IBLNR, YISEG-GJAHR, X_SELKZ.
  FORMAT COLOR OFF INTENSIFIED.
* write:/(18)   text-011.
  WRITE:/(18)   TEXT-Z11.                         "JL 10/03/96
  FORMAT RESET.
* write: 20 buper.
  WRITE: 20 YISEG-ZLDAT.                          "JL 10/03/96
  FORMAT COLOR OFF INTENSIFIED.
  WRITE: 39(18) TEXT-013.
  FORMAT RESET.
* write: 58 yiseg-lgort.
  WRITE: 58 YISEG-LGORT, '-', T001L-LGOBE.        "JL 10/03/96
  HIDE:  YISEG-IBLNR, YISEG-GJAHR, X_SELKZ.
  CASE YISEG-SOBKZ.
    WHEN E.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE:/(18)   TEXT-014.
      FORMAT RESET.
      WRITE: 20 YISEG-SOBKZ.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE: 39(18) TEXT-015.
      FORMAT RESET.
      WRITE: 58 YISEG-KDAUF.
    WHEN K.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE:/(18)   TEXT-014.
      FORMAT RESET.
      WRITE: 20 YISEG-SOBKZ.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE: 39(18) TEXT-016.
      FORMAT RESET.
      WRITE: 58 YISEG-LIFNR.
    WHEN M.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE:/(18)   TEXT-014.
      FORMAT RESET.
      WRITE: 20 YISEG-SOBKZ.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE: 39(18) TEXT-016.
      FORMAT RESET.
      WRITE: 58 YISEG-LIFNR.
    WHEN O.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE:/(18)   TEXT-014.
      FORMAT RESET.
      WRITE: 20 YISEG-SOBKZ.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE: 39(18) TEXT-016.
      FORMAT RESET.
      WRITE: 58 YISEG-LIFNR.
    WHEN Q.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE:/(18)   TEXT-014.
      FORMAT RESET.
      WRITE: 20 YISEG-SOBKZ.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE: 39(18) TEXT-018.
      FORMAT RESET.
      WRITE: 58 YISEG-PS_PSP_PNR.
    WHEN V.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE:/(18)   TEXT-014.
      FORMAT RESET.
      WRITE: 20 YISEG-SOBKZ.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE: 39(18) TEXT-017.
      FORMAT RESET.
      WRITE: 58 YISEG-KUNNR.
    WHEN W.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE:/(18) TEXT-014.
      FORMAT RESET.
      WRITE: 20 YISEG-SOBKZ.
      FORMAT COLOR OFF INTENSIFIED.
      WRITE: 39(18) TEXT-017.
      FORMAT RESET.
      WRITE: 58 YISEG-KUNNR.
  ENDCASE.
*-----------------------------------------------------------------------
* output the item header                          "JL 10/03/96
*-----------------------------------------------------------------------
  HIDE:  YISEG-IBLNR, YISEG-GJAHR, X_SELKZ.
  CLEAR X_SELKZ.
  SKIP.
                                                  "JL 10/03/96
* write: /(106) sy-uline.                         "JL 10/03/96
* write: / sy-vline no-gap.                       "JL 10/03/96
  FORMAT COLOR 1 INTENSIFIED.
* write: /2(87) blank no-gap,                     "JL 10/03/96
*        2 text-020 no-gap,                       "JL 10/03/96
  ULINE.                                          "JL 10/03/96
  FORMAT COLOR 1 INTENSIFIED.
  WRITE: /1     SY-VLINE NO-GAP,                  "JL 10/03/96
         2      'Itm',                            "JL 10/03/96
         6      'Material',                       "JL 10/03/96
         15     'Short',                          "JL 10/03/96
         65     'Book',                           "JL 10/03/96
         71     'Quantity',                       "JL 10/03/96
         81     'Difference',                     "JL 10/03/96
         95     'Difference',                     "JL 10/03/96
        131     SY-VLINE NO-GAP.                  "JL 10/03/96
*      106 sy-vline no-gap.                       "JL 10/03/96
* format reset.                                   "JL 10/03/96
* write: / sy-vline no-gap.                       "JL 10/03/96
  FORMAT COLOR 1 INTENSIFIED.                     "JL 10/03/96
* write: /2(87) blank no-gap,                     "JL 10/03/96
*        2 text-025 no-gap,                       "JL 10/03/96
  WRITE: /1     SY-VLINE NO-GAP,                  "JL 10/03/96
         2      'No.',                            "JL 10/03/96
         6      'No.',                            "JL 10/03/96
         15     'Description',                    "JL 10/03/96
         56     'UoM',                            "JL 10/03/96
         63     'Record',                         "JL 10/03/96
         72     'Counted',                        "JL 10/03/96
         81     '     (+/-)',                     "JL 10/03/96
         95     '     Value',                     "JL 10/03/96
         108    'Comment',                        "JL 10/03/96
         131    SY-VLINE NO-GAP.                  "JL 10/03/96
  ULINE.                                          "JL 10/03/96

  FORMAT RESET.
* write: /(106) sy-uline.                         "JL 10/03/96

ENDFORM.

*----------------------------------------------------------------------*
*    Formroutine, die die selektierten Belegpositionen sortiert        *
*    und aufbereitet ausgibt                                           *
*----------------------------------------------------------------------*
FORM AUSGABE_YISEG.
  V_HEAD = X.
* sort yiseg by iblnr zeili monat.                  "JL 10/03/96
* sort the output in order of absolute value of difference
  LOOP AT YISEG.                                    "JL 10/03/96
     YISEG-ADIFF = ABS( YISEG-DMBTR ).              "JL 10/03/96
     MODIFY YISEG.                                  "JL 10/03/96
  ENDLOOP.                                          "JL 10/03/96
  SORT YISEG BY IBLNR ADIFF DESCENDING ZEILI MONAT. "JL 10/03/96
  LOOP AT YISEG.
    CLEAR BEL.
    MOVE YISEG-IBLNR TO BEL_KEY-IBLNR.
    MOVE YISEG-LGORT TO BEL_KEY-LGORT.
    MOVE YISEG-WERKS TO BEL_KEY-WERKS.

*-- Prüfen, ob Beleg nicht unter den nicht-ausbuchbaren Belegen ist
    READ TABLE BEL WITH KEY BEL_KEY BINARY SEARCH.
    IF NOT SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING YISEG TO SUMTAB.
      APPEND SUMTAB.
      ON CHANGE OF YISEG-IBLNR OR YISEG-LIFNR
                OR YISEG-KDAUF OR YISEG-KUNNR.
        IF SY-LINNO NE 1.
          WRITE: /(106) SY-ULINE.
          SKIP 1.
        ENDIF.
        IF XSEIT IS INITIAL.
          NEW-PAGE.
        ENDIF.
        PERFORM HEADER_BELEG.
      ENDON.
      ON CHANGE OF YISEG-WERKS.
        PERFORM WERK_LESEN USING YISEG-WERKS.
      ENDON.
      ON CHANGE OF YISEG-MATNR.
        PERFORM KURZTEXT_LESEN USING YISEG-MATNR.
      ENDON.
      ON CHANGE OF YISEG-IBLNR.
        X_INTENSIFIED = N.
      ENDON.
*     write: / sy-vline no-gap.     "JL 10/03/96
      IF X_INTENSIFIED = N.
        FORMAT COLOR 2 INTENSIFIED.
      ELSE.
        FORMAT COLOR 2 INTENSIFIED OFF.
      ENDIF.

*----------begin of block----------JL 10/03/96--------------------------
*     WRITE: 2(87)  BLANK NO-GAP,
*            2      YISEG-ZEILI NO-GAP,      "Item No.
*            6      YISEG-MATNR NO-GAP,      "Material No.
*            28     YISEG-CHARG NO-GAP,      "Batch
*            40(17) YISEG-BTEXT NO-GAP,      "Stock Type
*            64(25) MAKT-MAKTX  NO-GAP,      "Description
*            89     YISEG-VKWRT NO-GAP,      "Sales Value
*           106     SY-VLINE    NO-GAP.
*     FORMAT RESET.
*     WRITE: / SY-VLINE NO-GAP.
*     IF X_INTENSIFIED = N.
*       FORMAT COLOR 2 INTENSIFIED.
*       X_INTENSIFIED = J.
*     ELSE.
*       FORMAT COLOR 2 INTENSIFIED OFF.
*       X_INTENSIFIED = N.
*     ENDIF.
*     WRITE: 2(87) BLANK NO-GAP,
*            2  YISEG-BUCHM NO-GAP,                      "Book qty
*            21 YISEG-MENGE NO-GAP,                      "Qty cnted
*            39 YISEG-DIFMG NO-GAP,                      "Diff qty
*            60 YISEG-MEINS NO-GAP,                      "UoM
*            63 YISEG-DMBTR CURRENCY YISEG-WAERS NO-GAP, "LC amount
*            82 YISEG-WAERS NO-GAP,                      "Currency
*            89 YISEG-EXVKW NO-GAP,                      "Ext sa value
*           106    SY-VLINE NO-GAP.

      FORMAT COLOR 2 INTENSIFIED OFF.
      WRITE: /1     SY-VLINE NO-GAP,
             2      YISEG-ZEILI NO-GAP,                      "Item No.
             6      YISEG-MATNR NO-GAP,                      "Material #
             15(40) MAKT-MAKTX  NO-GAP,                      "Descriptio
             56     YISEG-MEINS NO-GAP,                      "UoM
             60(10) YISEG-BUCHM DECIMALS 0 NO-GAP,  "quan13  "Book qty
             70(10) YISEG-MENGE DECIMALS 0 NO-GAP,  "quan13  "Qty cnted
             82(10) YISEG-DIFMG DECIMALS 0 NO-GAP,  "quan13  "Diff qty
             92(14) YISEG-DMBTR CURRENCY YISEG-WAERS NO-GAP, "LC amount
             131    SY-VLINE NO-GAP.
      ULINE.
*----------end of block------------JL 10/03/96--------------------------

      FORMAT RESET.
    ELSE.
      DELETE YISEG.
    ENDIF.
  ENDLOOP.
* write: /(106) sy-uline.             "JL 10/03/96
ENDFORM.

*----------------------------------------------------------------------*
*    Formroutine, die die nicht ausbuchbaren Belege sortiert           *
*    und aufbereitet ausgibt                                           *
*----------------------------------------------------------------------*
FORM AUSGABE_BEL.
  NEW-PAGE.
  V_HEAD = 'SBEL'.
  SORT BEL BY WERKS LGORT IBLNR.
  LOOP AT BEL.
    WRITE: / SY-VLINE NO-GAP.
    FORMAT COLOR 2 INTENSIFIED OFF.
    WRITE: 2(66) BLANK NO-GAP,
           2 BEL-WERKS NO-GAP,
           9 BEL-LGORT NO-GAP,
          16 BEL-IBLNR NO-GAP.
    MOVE X TO Y_SELKZ.
    HIDE:  BEL-IBLNR, BEL-GJAHR, Y_SELKZ.
    CLEAR Y_SELKZ.
    WRITE: 28 BEL-BLDAT NO-GAP,
           41 BEL-BUDAT NO-GAP,
           53 BEL-SPERR NO-GAP,
           59 BEL-ZSTAT NO-GAP,
           65 BEL-DSTAT NO-GAP,
           68 SY-VLINE NO-GAP.
    FORMAT RESET.
  ENDLOOP.
  CLEAR V_HEAD.
  WRITE: /(68) SY-ULINE.
ENDFORM.

*----------------------------------------------------------------------*
*                Fromroutine, die die YISEG fuellt.                    *
*----------------------------------------------------------------------*
FORM YISEG_FUELLEN.
  SORT XISEG BY IBLNR ZEILI.
  SORT IIKPF BY MANDT IBLNR GJAHR.
  SORT IMAKT BY MATNR.
  LOOP AT XISEG.
    CLEAR YISEG.
    MOVE-CORRESPONDING XISEG TO YISEG.
    IKPF_KEY-MANDT = XISEG-MANDT.
    IKPF_KEY-IBLNR = XISEG-IBLNR.
    IKPF_KEY-GJAHR = XISEG-GJAHR.
    READ TABLE IIKPF WITH KEY IKPF_KEY BINARY SEARCH.
    ON CHANGE OF IIKPF-IBLNR.
      ON CHANGE OF IIKPF-WERKS.
        PERFORM WERK_LESEN USING IIKPF-WERKS.
        SELECT SINGLE * FROM TCURX WHERE CURRKEY = T001-WAERS.
        IF SY-SUBRC = 0.
          EXPONENT = TCURX-CURRDEC.
        ELSE.
          EXPONENT = 2.
        ENDIF.
        IF EXPONENT NE 0.
          IWERT = IIWERT * ( EXP( EXPONENT * LOG( 10 ) ) ).
        ELSE.
          IWERT = IIWERT.
        ENDIF.
        H_BWKEY = T001W-BWKEY.
      ENDON.
      PERFORM MARV_LESEN USING T001K-BUKRS.

*---- Beleg ist noch nicht komlett ausgebucht aber begonnen zu zählen
      IF IIKPF-DSTAT NE X AND IIKPF-LSTAT IS INITIAL
                          AND NOT IIKPF-ZSTAT IS INITIAL.
        IF IIKPF-GJAHR NE MARV-LFGJA OR IIKPF-MONAT NE MARV-LFMON.
          IF IIKPF-GJAHR EQ MARV-VMGJA AND IIKPF-MONAT EQ MARV-VMMON.
            H_XRUEM = X.
          ELSE.
            MOVE-CORRESPONDING YISEG TO BEL.
            MOVE-CORRESPONDING IIKPF TO BEL.
            MOVE-CORRESPONDING T001W TO BEL.
            COLLECT BEL.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDON.

    YISEG-WAERS = T001-WAERS.
    YISEG-MONAT = IIKPF-MONAT.
    YISEG-BWKEY = H_BWKEY.
    YISEG-XRUEM = H_XRUEM.
    PERFORM BSTAR_LESEN USING YISEG-BSTAR.
    YISEG-BTEXT = T064B-BTEXT.
    PERFORM MARC_LESEN USING YISEG-MATNR YISEG-WERKS.
    IF NOT MARC-BWTTY IS INITIAL.
      PERFORM MCHA_LESEN USING YISEG-MATNR YISEG-WERKS YISEG-CHARG.
      YISEG-BWTAR = MCHA-BWTAR.
    ENDIF.

*-- Prüfung, ob Position gezählt ist. Wenn ja, dann APPEND nur,
*   wenn IM_SELP1 nicht initial!
    IF YISEG-XZAEL IS INITIAL.
      CHECK NOT IM_SELP1 IS INITIAL.
    ELSE.
      PERFORM MTART_ERMITTELN USING YISEG-MATNR YISEG-WERKS.
      IF V134W-WERTU = X.
        PERFORM MBEW_LESEN USING YISEG-MATNR YISEG-BWKEY YISEG-BWTAR.
      ENDIF.
      YISEG-DIFMG = YISEG-MENGE - YISEG-BUCHM.

*---- Prüfen auf Differenzmenge
      IF NOT YISEG-DIFMG IS INITIAL.
        IF YISEG-DMBTR IS INITIAL.
          IF YISEG-XDIFF IS INITIAL.
            IF V134W-WERTU = X.
              PERFORM WERT_ERMITTELN.
            ENDIF.
          ENDIF.
        ELSE.
          IF YISEG-DIFMG < 0.
            YISEG-DMBTR = YISEG-DMBTR * -1.
          ENDIF.
        ENDIF.
        IF YISEG-DMBTR < 0.
          REFE1 = YISEG-DMBTR * -1.
        ELSE.
          REFE1 = YISEG-DMBTR.
        ENDIF.
      ELSE.
        CLEAR YISEG-DMBTR.
        CLEAR REFE1.
      ENDIF.

*---- Schwellenwertprüfung
      IF NOT IWERT IS INITIAL.
        CHECK REFE1 GE IWERT.
      ENDIF.
    ENDIF.
    APPEND YISEG.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SUMME_AUSGEBEN                                           *
*---------------------------------------------------------------------*
FORM SUMME_AUSGEBEN.
  DESCRIBE TABLE SUMTAB LINES INDEX_S.
  IF NOT INDEX_S IS INITIAL.
    NEW-PAGE.
    SET PF-STATUS '101'.
    X_INTENSIFIED = N.
    SORT SUMTAB.
    LOOP AT SUMTAB.
      MEINS = SUMTAB-MEINS.
      WAERS = SUMTAB-WAERS.
      IF NOT SUMAT IS INITIAL.
        AT END OF MATNR.
          SUM.
          WRITE: / SY-VLINE NO-GAP.
          IF X_INTENSIFIED = N.
            FORMAT COLOR 2 INTENSIFIED.
          ELSE.
            FORMAT COLOR 2 INTENSIFIED OFF.
          ENDIF.
          WRITE: 2(69) BLANK NO-GAP,
                 2 SUMTAB-MATNR NO-GAP,
                 23 SUMTAB-BUCHM NO-GAP,
                 45 SUMTAB-MENGE NO-GAP,
                 71 SUMTAB-VKWRT NO-GAP,
                 88 SY-VLINE NO-GAP.
          FORMAT RESET.
          WRITE: / SY-VLINE NO-GAP.
          IF X_INTENSIFIED = N.
            FORMAT COLOR 2 INTENSIFIED.
            X_INTENSIFIED = J.
          ELSE.
            FORMAT COLOR 2 INTENSIFIED OFF.
            X_INTENSIFIED = N.
          ENDIF.
          WRITE: 2(86) BLANK NO-GAP,
                 7 SUMTAB-WERKS NO-GAP,
                12 SUMTAB-LGORT NO-GAP,
                22 SUMTAB-DIFMG NO-GAP,
                42 MEINS NO-GAP,
                46 SUMTAB-DMBTR CURRENCY WAERS NO-GAP,
                64 WAERS NO-GAP,
                71 SUMTAB-EXVKW NO-GAP,
                88 SY-VLINE NO-GAP.
          FORMAT RESET.
          IF SULORT IS INITIAL AND
             SUWERK IS INITIAL.
            WRITE: /(88) SY-ULINE.
          ENDIF.
        ENDAT.
      ENDIF.
      IF NOT SULORT IS INITIAL.
        AT END OF LGORT.
          SUM.
          IF NOT SUMAT IS INITIAL.
            WRITE: /(88) SY-ULINE.
          ENDIF.
          WRITE: / SY-VLINE NO-GAP.
          IF NOT SUWERK IS INITIAL.
            FORMAT COLOR 3 INTENSIFIED OFF.
          ELSE.
            FORMAT COLOR 3 INTENSIFIED.
          ENDIF.
          WRITE: 2(86) BLANK NO-GAP.
          IF SUMTAB-LGORT IS INITIAL.
            WRITE: 2(35) TEXT-115.
          ELSE.
            WRITE: 2(18) TEXT-113.
          ENDIF.
          WRITE: 39   '*' NO-GAP,
                 61   '*' NO-GAP,
                 71 SUMTAB-VKWRT NO-GAP,
                 88 SY-VLINE NO-GAP.
          FORMAT RESET.
          WRITE: / SY-VLINE NO-GAP.
          IF NOT SUWERK IS INITIAL.
            FORMAT COLOR 3 INTENSIFIED OFF.
          ELSE.
            FORMAT COLOR 3 INTENSIFIED.
          ENDIF.
          WRITE: 2(86) BLANK NO-GAP,
                 7 SUMTAB-WERKS NO-GAP,
                 12 SUMTAB-LGORT NO-GAP,
                 39 '*' NO-GAP,
                 42 '*' NO-GAP,
                 46 SUMTAB-DMBTR CURRENCY WAERS NO-GAP,
                 64 WAERS NO-GAP,
                 71 SUMTAB-EXVKW NO-GAP,
                 88 SY-VLINE NO-GAP.
          FORMAT RESET.
          WRITE: /(88) SY-ULINE.
        ENDAT.
      ENDIF.
      IF NOT SUWERK IS INITIAL.
        AT END OF WERKS.
          SUM.
          IF NOT SUMAT IS INITIAL
             AND SULORT IS INITIAL.
            WRITE: /(88) SY-ULINE.
          ENDIF.
          WRITE: / SY-VLINE NO-GAP.
          FORMAT COLOR 3 INTENSIFIED.
          WRITE: 2(86) BLANK NO-GAP,
                 2(18) TEXT-114 NO-GAP,
                 39   '*' NO-GAP,
                 61   '*' NO-GAP,
                 71 SUMTAB-VKWRT NO-GAP,
                 88 SY-VLINE NO-GAP.
          FORMAT RESET.
          WRITE: / SY-VLINE NO-GAP.
          FORMAT COLOR 3 INTENSIFIED.
          WRITE: 2(86) BLANK NO-GAP,
                 7 SUMTAB-WERKS NO-GAP,
                39 '*' NO-GAP,
                42 '*' NO-GAP,
                46 SUMTAB-DMBTR CURRENCY WAERS NO-GAP,
                64 WAERS NO-GAP,
                71 SUMTAB-EXVKW NO-GAP,
                88 SY-VLINE NO-GAP.
          FORMAT RESET.
          WRITE: /(88) SY-ULINE.
        ENDAT.
      ENDIF.
    ENDLOOP.
  ELSE.
    MESSAGE S745.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* Formroutine, die die in der ISEG selektierten Saetze in die XISEG    *
* schreibt.                                                            *
*----------------------------------------------------------------------*
FORM XISEG_FUELLEN.
  CLEAR: AUTH03, AUTH04.
  PERFORM INVENTUR_DB USING ACTVT03
                            ISEG-WERKS.
  IF NO_CHANCE IS INITIAL.
    IF NOT AUTH03 IS INITIAL.
      NO_CHANCE = X.
    ENDIF.
  ENDIF.
  CHECK AUTH03 IS INITIAL.
  PERFORM INVENTUR_DB USING ACTVT04
                            ISEG-WERKS.
  IF NO_CHANCE IS INITIAL.
    IF NOT AUTH04 IS INITIAL.
      NO_CHANCE = X.
    ENDIF.
  ENDIF.
  CHECK AUTH04 IS INITIAL.
  CHECK ISEG-GJAHR IN GJAHR.
  MOVE-CORRESPONDING ISEG TO XISEG.
  ON CHANGE OF ISEG-IBLNR.
    XKOPF-BELNR = ISEG-IBLNR.
    APPEND XKOPF.
  ENDON.
  ON CHANGE OF ISEG-MATNR.
    XMATN-MATNR = ISEG-MATNR.
    APPEND XMATN.
  ENDON.
  APPEND XISEG.
ENDFORM.

*-------------------- Ermitteln Materialstamm C-Segment ---------------*
FORM MARC_LESEN USING MATNR WERKS.
  SELECT SINGLE * FROM MARC WHERE MATNR = MATNR
                              AND WERKS = WERKS.
ENDFORM.

*-------------------- Ermitteln Chargen -------------------------------*
FORM MCHA_LESEN USING MATNR WERKS CHARG.
  SELECT SINGLE * FROM MCHA WHERE MATNR = MATNR
                              AND WERKS = WERKS
                              AND CHARG = CHARG.
ENDFORM.

*-------------------- PF-Tasten Belegung ------------------------------*
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN UCOM-BACK.
      PERFORM ANFORDERUNGSBILD.
    WHEN UCOM-ENDE.
      PERFORM BEENDEN.
    WHEN UCOM-SUMM.
      PERFORM SUMME_AUSGEBEN.
    WHEN UCOM-AUSR.
      IF X_SELKZ IS INITIAL AND Y_SELKZ IS INITIAL.
        MESSAGE S753.
      ENDIF.
      IF NOT X_SELKZ IS INITIAL.
        READ CURRENT LINE.
        CLEAR X_SELKZ.
        SET PARAMETER ID 'IBN' FIELD YISEG-IBLNR.
        SET PARAMETER ID 'GJR' FIELD YISEG-GJAHR.
        CALL TRANSACTION 'MI02' AND SKIP FIRST SCREEN.
      ELSE.
        IF NOT Y_SELKZ IS INITIAL.
          READ CURRENT LINE.
          CLEAR Y_SELKZ.
          SET PARAMETER ID 'IBN' FIELD BEL-IBLNR.
          SET PARAMETER ID 'GJR' FIELD BEL-GJAHR.
          CALL TRANSACTION 'MI02' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    WHEN 'MI03'.
      IF X_SELKZ IS INITIAL AND Y_SELKZ IS INITIAL.
        MESSAGE S753.
      ENDIF.
      IF NOT X_SELKZ IS INITIAL.
        READ CURRENT LINE.
        CLEAR X_SELKZ.
        SET PARAMETER ID 'IBN' FIELD YISEG-IBLNR.
        SET PARAMETER ID 'GJR' FIELD YISEG-GJAHR.
        CALL TRANSACTION 'MI03' AND SKIP FIRST SCREEN.
      ELSE.
        IF NOT Y_SELKZ IS INITIAL.
          READ CURRENT LINE.
          CLEAR Y_SELKZ.
          SET PARAMETER ID 'IBN' FIELD BEL-IBLNR.
          SET PARAMETER ID 'GJR' FIELD BEL-GJAHR.
          CALL TRANSACTION 'MI03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
  ENDCASE.
*eject
