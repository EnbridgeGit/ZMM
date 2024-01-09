REPORT ZMINR018 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR018
*    PROGRAMMER  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  January 9 1997
*
*
*
*
************************************************************************
TABLES  : MARA, MARC, MARD, RESB, MAKT, MBEW, MVER.
DATA    : QTYHAND           TYPE I,
          TEMP-BDMNG        LIKE RESB-BDMNG,
          SAFETY-REORDER    TYPE I,
          DATE_AMOUNT       TYPE I,
          TOTAL_ENMNG       LIKE RESB-ENMNG,
          TOTAL_BDMNG       LIKE RESB-BDMNG,
          DIFFERENCE        TYPE I,
          C_DAY             TYPE I,
          C_MONTH           TYPE I,
          C_YEAR            TYPE I,
          P_DAY             LIKE C_DAY,
          P_MONTH           LIKE C_MONTH,
          P_YEAR            LIKE C_YEAR,
          C_TOTAL           TYPE I,
          P_TOTAL           TYPE I,
          CONSUMPTION       TYPE I,
          TOTAL_VALUE       TYPE P DECIMALS 2.

DATA    : BEGIN OF TABLE1 OCCURS 5000,
              MATNR         LIKE MARD-MATNR,
              WERKS         LIKE MARD-WERKS,
              LGORT         LIKE MARD-LGORT,
              LVORM         LIKE MARD-LVORM,
              DISMM         LIKE MARC-DISMM,
              MINBE         LIKE MARC-MINBE,
              MATKL         LIKE MARA-MATKL,
              EISBE         LIKE MARC-EISBE,
              BDMNG         LIKE RESB-BDMNG,
              T_CONSUMPTION LIKE CONSUMPTION,
              T_QTYHAND     LIKE QTYHAND,
          END OF TABLE1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_WERKS         FOR   MARC-WERKS,
     S_LGORT         FOR   MARD-LGORT,
     S_MATKL         FOR   MARA-MATKL OBLIGATORY,
     S_MATNR         FOR   MARC-MATNR OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX1.

INCLUDE <ICON>.
*******************************  MAIN  *********************************
TOP-OF-PAGE.
WRITE: / TEXT-499, 131 TEXT-498.
WRITE: / TEXT-500, 131 TEXT-501.
WRITE: / ICON_DATE AS ICON.
WRITE: SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 246 SY-REPID COLOR COL_NEGATIVE.

WRITE: / ICON_TIME AS ICON.
WRITE: SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 110 TEXT-002 COLOR COL_HEADING.
WRITE: 246 'PAGE:' INTENSIFIED OFF.
WRITE: 251(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE: /108(24).
ULINE.

FORMAT COLOR COL_NORMAL.
WRITE: /193 TEXT-008, 230 TEXT-016.
WRITE: /1 TEXT-004, 20 TEXT-006, 35 TEXT-006, 84 TEXT-023.
WRITE: 102 TEXT-021, 116 TEXT-026, 138 TEXT-018, 152 TEXT-011.
WRITE: 174 TEXT-025, 193 TEXT-009, 215 TEXT-028, 230 TEXT-017.
WRITE: 247 TEXT-018.
WRITE: /1 TEXT-005, 20 TEXT-007, 35 TEXT-013, 84 TEXT-011.
WRITE: 102 TEXT-022, 116 TEXT-027, 152 TEXT-012, 193 TEXT-010.
WRITE: 215 TEXT-029, 230 TEXT-020, 247 TEXT-019.
ULINE.
WRITE: /.

************************************************************************
START-OF-SELECTION.
FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
CLEAR TABLE1.
REFRESH TABLE1.

PERFORM CAL_DATES.

SELECT * FROM MARA WHERE MATNR IN S_MATNR
                     AND MATKL IN S_MATKL
                     AND LVORM NE 'X'.
IF SY-SUBRC EQ 0.
   SELECT * FROM MARC WHERE MATNR = MARA-MATNR
                      AND WERKS IN S_WERKS
                      AND LVORM NE 'X'.

   IF SY-SUBRC EQ 0.
        SELECT * FROM MARD WHERE MATNR = MARC-MATNR
                             AND WERKS = MARC-WERKS
                             AND LGORT IN S_LGORT
                             AND LVORM NE 'X'.
              QTYHAND = QTYHAND + MARD-LABST + MARD-UMLME +
              MARD-INSME + MARD-SPEME + MARD-EINME.
*                      SELECT * FROM RESB WHERE MATNR = MARD-MATNR
*                                            AND WERKS = MARD-WERKS
*                                            AND LGORT = MARD-LGORT
*                                            AND XLOEK NE 'X'
*                                            AND KZEAR NE 'X'.
*                       TEMP-BDMNG = RESB-BDMNG - RESB-ENMNG.
*                       ENDSELECT.

                       C_MONTH = SY-DATUM+4(2).
                       C_YEAR  = SY-DATUM(4).
                       SELECT * FROM MVER WHERE MATNR = MARD-MATNR
                                            AND GJAHR = C_YEAR.
                       PERFORM CALC USING C_MONTH CHANGING C_TOTAL.
                       ENDSELECT.
                           IF C_MONTH NE 12.
                                P_MONTH = 12 - C_MONTH.
                                P_YEAR  = C_YEAR - 1.
                           SELECT * FROM MVER WHERE MATNR = MARD-MATNR
                                                AND GJAHR = P_YEAR.
                           PERFORM CALC2 USING P_MONTH CHANGING P_TOTAL.
                           ENDSELECT.
                           ENDIF.
                    CONSUMPTION = C_TOTAL + P_TOTAL.

                    MOVE-CORRESPONDING MARD TO TABLE1.
                    MOVE MARC-MINBE TO TABLE1-MINBE.
                    MOVE MARC-EISBE TO TABLE1-EISBE.
                    MOVE MARC-DISMM TO TABLE1-DISMM.
                    MOVE MARA-MATKL TO TABLE1-MATKL.
                    MOVE TEMP-BDMNG TO TABLE1-BDMNG.
                    MOVE CONSUMPTION TO TABLE1-T_CONSUMPTION.
                    MOVE QTYHAND TO TABLE1-T_QTYHAND.
                    APPEND TABLE1.
                    CLEAR TABLE1.
                    CLEAR QTYHAND.
        ENDSELECT.
     ENDIF.
   ENDSELECT.
ENDIF.
ENDSELECT.

SORT TABLE1 BY MATKL  ASCENDING
               WERKS  ASCENDING
               MATNR  ASCENDING.

*LOOP AT TABLE1.
*WRITE: / TABLE1.
*ENDLOOP.
*STOP.

LOOP AT TABLE1.
CLEAR:  DIFFERENCE, SAFETY-REORDER.
SAFETY-REORDER = TABLE1-MINBE + TABLE1-EISBE + TABLE1-BDMNG.
IF TABLE1-T_QTYHAND > SAFETY-REORDER.
     DIFFERENCE = TABLE1-T_QTYHAND - SAFETY-REORDER.

            SELECT SINGLE * FROM MAKT WHERE SPRAS = SY-LANGU
                                        AND MATNR = TABLE1-MATNR.
                SELECT SINGLE * FROM MBEW WHERE MATNR = TABLE1-MATNR
                                            AND BWKEY = TABLE1-WERKS
                                            AND BWTAR = SPACE.
                TOTAL_VALUE = MBEW-VERPR * TABLE1-T_QTYHAND.

     ON CHANGE OF TABLE1-MATKL.
        WRITE: /.
        WRITE: /.
        WRITE: / TEXT-903, TABLE1-MATKL COLOR COL_KEY INVERSE ON.
        ULINE: /1(21).
     ENDON.

     ON CHANGE OF TABLE1-WERKS.
        WRITE: /.
     ENDON.

        WRITE: / TABLE1-WERKS, 6 TEXT-900, 8 TABLE1-LGORT.
        WRITE: 20 TABLE1-MATNR+12(6) COLOR COL_NEGATIVE INTENSIFIED ON.
        WRITE: 35 MAKT-MAKTX, 75 TABLE1-MINBE.
        WRITE: 92 TABLE1-EISBE, 110 TABLE1-BDMNG.
        WRITE: 133 SAFETY-REORDER.
        WRITE: 149 TABLE1-T_QTYHAND COLOR COL_KEY INTENSIFIED ON.
        WRITE: 172 DIFFERENCE, 192 TABLE1-T_CONSUMPTION.
        WRITE: 216 TABLE1-DISMM.
        WRITE: 223 MBEW-VERPR COLOR COL_KEY INTENSIFIED ON.
        WRITE: 237 TOTAL_VALUE COLOR COL_TOTAL.
        WRITE: 254 ICON_CALCULATION AS ICON.
*    EXIT.
ENDIF.
ENDLOOP.

*-----------------------------------------------------------------------
*       SUBROUTINE CALC
*-----------------------------------------------------------------------
*     - This routine calculates the CURRENT years portion of total
*       consumption.
*-----------------------------------------------------------------------
FORM CALC USING MONTH CHANGING TOTAL.

CASE MONTH.
WHEN '1'.
    TOTAL = MVER-MGV01.
WHEN '2'.
    TOTAL = MVER-MGV01 + MVER-MGV02.
WHEN '3'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03.
WHEN '4'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04.
WHEN '5'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04
            + MVER-MGV05.
WHEN '6'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04
          + MVER-MGV05 + MVER-MGV06.
WHEN '7'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04
          + MVER-MGV05 + MVER-MGV06 + MVER-MGV07.
WHEN '8'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04
          + MVER-MGV05 + MVER-MGV06 + MVER-MGV07 + MVER-MGV08.
WHEN '9'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04
          + MVER-MGV05 + MVER-MGV06 + MVER-MGV07 + MVER-MGV08
          + MVER-MGV09.
WHEN '10'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04
          + MVER-MGV05 + MVER-MGV06 + MVER-MGV07 + MVER-MGV08
          + MVER-MGV09 + MVER-MGV10.
WHEN '11'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04
          + MVER-MGV05 + MVER-MGV06 + MVER-MGV07 + MVER-MGV08
          + MVER-MGV09 + MVER-MGV10 + MVER-MGV11.
WHEN '12'.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04
          + MVER-MGV05 + MVER-MGV06 + MVER-MGV07 + MVER-MGV08
          + MVER-MGV09 + MVER-MGV10 + MVER-MGV11 + MVER-MGV12.
WHEN OTHERS.
    TOTAL = MVER-MGV01 + MVER-MGV02 + MVER-MGV03 + MVER-MGV04
          + MVER-MGV05 + MVER-MGV06 + MVER-MGV07 + MVER-MGV08
          + MVER-MGV09 + MVER-MGV10 + MVER-MGV11 + MVER-MGV12.
ENDCASE.
ENDFORM.
*-----------------------------------------------------------------------
*       SUBROUTINE CALC2
*-----------------------------------------------------------------------
*     - This routine calculates the PREVIOUS years portion of total
*       consumption.
*-----------------------------------------------------------------------
FORM CALC2 USING MONTH CHANGING TOTAL.

CASE MONTH.
WHEN '1'.
    TOTAL = MVER-MGV12.
WHEN '2'.
    TOTAL = MVER-MGV12 + MVER-MGV11.
WHEN '3'.
    TOTAL = MVER-MGV12 + MVER-MGV11 + MVER-MGV10.
WHEN '4'.
    TOTAL = MVER-MGV12 + MVER-MGV11 + MVER-MGV10 + MVER-MGV09.
WHEN '5'.
    TOTAL = MVER-MGV12 + MVER-MGV11 + MVER-MGV10 + MVER-MGV09
            + MVER-MGV08.
WHEN '6'.
    TOTAL = MVER-MGV12 + MVER-MGV11 + MVER-MGV10 + MVER-MGV09
          + MVER-MGV08 + MVER-MGV07.
WHEN '7'.
    TOTAL = MVER-MGV12 + MVER-MGV11 + MVER-MGV10 + MVER-MGV09
          + MVER-MGV08 + MVER-MGV07 + MVER-MGV06.
WHEN '8'.
    TOTAL = MVER-MGV12 + MVER-MGV11 + MVER-MGV10 + MVER-MGV09
          + MVER-MGV08 + MVER-MGV07 + MVER-MGV06 + MVER-MGV05.
WHEN '9'.
    TOTAL = MVER-MGV12 + MVER-MGV11 + MVER-MGV10 + MVER-MGV09
          + MVER-MGV08 + MVER-MGV07 + MVER-MGV06 + MVER-MGV05
          + MVER-MGV04.
WHEN '10'.
    TOTAL = MVER-MGV12 + MVER-MGV11 + MVER-MGV10 + MVER-MGV09
          + MVER-MGV08 + MVER-MGV07 + MVER-MGV06 + MVER-MGV05
          + MVER-MGV04 + MVER-MGV03.
WHEN '11'.
    TOTAL = MVER-MGV12 + MVER-MGV11 + MVER-MGV10 + MVER-MGV09
          + MVER-MGV08 + MVER-MGV07 + MVER-MGV06 + MVER-MGV05
          + MVER-MGV04 + MVER-MGV03 + MVER-MGV02.
WHEN OTHERS.
    TOTAL = 0.
ENDCASE.
ENDFORM.

************************************************************************
FORM CAL_DATES.








ENDFORM.
