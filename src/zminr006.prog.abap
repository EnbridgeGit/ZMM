REPORT ZMINR006 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR006
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  July 25, 1996
*
* This report will list the information on all outstanding reservations
* by plant and storage locations.
*
************************************************************************
* 97/10/09 md7140 #242 Added Material Group as selection criteria
************************************************************************
TABLES  : MARA, MARC, MARD, RESB, MSEG, MBEW.
DATA    : QTYHAND         TYPE I,
          TEMP-BDMNG      LIKE RESB-BDMNG,
          TEMP-LGORT      LIKE MARD-LGORT,
          TOTAL_VALUE     TYPE P DECIMALS 2.

DATA    : BEGIN OF TABLE1 OCCURS 5000,
              MATNR     LIKE MARD-MATNR,
              WERKS     LIKE MARD-WERKS,
              LGORT     LIKE MARD-LGORT,
              MATKL     LIKE MARA-MATKL,
              LVORM     LIKE MARD-LVORM,
              RSPOS     LIKE RESB-RSPOS,
              RSNUM     LIKE RESB-RSNUM,
              MEINS     LIKE RESB-MEINS,
              BDTER     LIKE RESB-BDTER,
              BWART     LIKE RESB-BWART,
              BDMNG     TYPE I,
              T_QTYHAND LIKE QTYHAND,
          END OF TABLE1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_PLANT         FOR   MARC-WERKS,
*    s_storlo        for   mard-lgort no intervals,
     S_STORLO        FOR   MARD-LGORT,
     S_MATKL         FOR   MARA-MATKL,                          "#242
     S_MATNUM        FOR   MARC-MATNR.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
     PARAMETERS: P_DATE    LIKE SY-DATUM  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX2.

*******************************  MAIN  *********************************
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_NEGATIVE INVERSE ON,
       58 TEXT-002 COLOR COL_HEADING,
      105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE:  / TEXT-CLT UNDER TEXT-RPT, SY-MANDT,
       56 TEXT-901 INTENSIFIED OFF,
           P_DATE COLOR COL_GROUP INVERSE ON, TEXT-014,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
WRITE: /.
ULINE.
FORMAT COLOR COL_NORMAL.
WRITE: /1 TEXT-004, 13 TEXT-021, 20 TEXT-007, 33 TEXT-008.
WRITE: 40 TEXT-024, 56 TEXT-025, 70 TEXT-010, 82 TEXT-011.
WRITE: 90 TEXT-010, 100 TEXT-011, 109 TEXT-016, 122 TEXT-018.

WRITE: /1 TEXT-005, 13 TEXT-006, 20 TEXT-005, 40 TEXT-009, 58 TEXT-006.
WRITE: 69 TEXT-023, 90 TEXT-015, 109 TEXT-017, 125 TEXT-019.
ULINE.
WRITE: /.

START-OF-SELECTION.
FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
CLEAR TABLE1.
REFRESH TABLE1.

SELECT * FROM RESB
  WHERE MATNR IN S_MATNUM
    AND WERKS IN S_PLANT
    AND LGORT IN S_STORLO
    AND XLOEK NE 'X'
    AND KZEAR NE 'X'.

  IF RESB-LGORT EQ SPACE.
     MOVE 'A001' TO TEMP-LGORT.
  ELSE.
     TEMP-LGORT = RESB-LGORT.
  ENDIF.                                     "End of RESB-LGORT EQ SPACE

  SELECT * FROM MARA                                               "#242
    WHERE MATNR = RESB-MATNR
      AND MATKL IN S_MATKL.

    TEMP-BDMNG = RESB-BDMNG - RESB-ENMNG.
    IF SY-SUBRC EQ 0.                                   "#1 if
       SELECT SINGLE * FROM MARD
         WHERE MATNR = RESB-MATNR
           AND WERKS = RESB-WERKS
           AND LGORT = TEMP-LGORT
           AND LVORM NE 'X'.
           QTYHAND = QTYHAND + MARD-LABST + MARD-UMLME +
                               MARD-INSME + MARD-SPEME + MARD-EINME.

           IF SY-SUBRC EQ 0.                          "#2 if
              MOVE-CORRESPONDING MARD TO TABLE1.
              MOVE RESB-RSNUM TO TABLE1-RSNUM.
              MOVE RESB-RSPOS TO TABLE1-RSPOS.
              MOVE TEMP-LGORT TO TABLE1-LGORT.
              MOVE TEMP-BDMNG TO TABLE1-BDMNG.
              MOVE RESB-MEINS TO TABLE1-MEINS.
              MOVE RESB-BDTER TO TABLE1-BDTER.
              MOVE RESB-BWART TO TABLE1-BWART.
              MOVE QTYHAND TO TABLE1-T_QTYHAND.
              MOVE MARA-MATKL TO TABLE1-MATKL.                   "#242

              APPEND TABLE1.
              CLEAR TABLE1.
              CLEAR QTYHAND.
           ENDIF.                                  "end of #2 IF
    ENDIF.                                         "end of #1 IF
   ENDSELECT.                                      "end of MARA
ENDSELECT.                                         "end of RESB

SORT TABLE1 BY WERKS  ASCENDING
               LGORT  ASCENDING
               MATKL  ASCENDING
               BDTER  ASCENDING
               MATNR  ASCENDING.

LOOP AT TABLE1.
   SELECT * FROM MARC WHERE MATNR = TABLE1-MATNR
                        AND WERKS = TABLE1-WERKS
                        AND LVORM NE 'X'.
       IF TABLE1-BDTER < P_DATE.
             ON CHANGE OF TABLE1-WERKS OR TABLE1-LGORT OR TABLE1-MATKL.
                 WRITE: /.
                 WRITE: /.
                 FORMAT COLOR COL_KEY.
                 WRITE: / TEXT-020, TABLE1-WERKS(4) NO-GAP,
                         TEXT-022 NO-GAP,
                                    TABLE1-LGORT NO-GAP.
                 WRITE: / TEXT-027 UNDER TEXT-020,
                         TABLE1-MATKL UNDER TABLE1-WERKS.
                 FORMAT COLOR COL_NORMAL.
             ENDON.
          SELECT SINGLE * FROM MBEW WHERE MATNR = TABLE1-MATNR
                                      AND BWKEY = TABLE1-WERKS
                                      AND BWTAR = SPACE.
             TOTAL_VALUE = MBEW-VERPR * TABLE1-T_QTYHAND.

*         select single * from mara where matnr = table1-matnr.
         WRITE: / TABLE1-MATNR+12(6) COLOR COL_NEGATIVE INTENSIFIED OFF.
             WRITE: 14 MARC-DISMM, 20 TABLE1-RSNUM.
             WRITE: 33 TABLE1-RSPOS, 40 TABLE1-BDTER.
             WRITE: 59 TABLE1-BWART.
             WRITE: 70 TABLE1-BDMNG, 82 TABLE1-MEINS.
             WRITE: 88 TABLE1-T_QTYHAND,100 MARA-MEINS, 103 MBEW-VERPR.
             WRITE: 119 TOTAL_VALUE COLOR COL_TOTAL.
       ENDIF.
   ENDSELECT.
ENDLOOP.
