REPORT ZMINR023 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR023
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  December 31 1996
*
* This ABAP will calculate the total Dollar Amount in inventory for
* each Storage Location.
*
************************************************************************
TABLES  : MARA, MARC, MARD, MBEW.
DATA    : QTYHAND         TYPE I,
          CHECK1(1)       TYPE C VALUE 'N',
          LGORT_TOTAL     TYPE P DECIMALS 2,
          TOTAL_VALUE     TYPE P DECIMALS 2,
          TEMP-WERKS      LIKE MARD-WERKS,
          TEMP-LGORT      LIKE MARD-LGORT.

DATA    : BEGIN OF TABLE1 OCCURS 5000,
              MATNR        LIKE MARD-MATNR,
              WERKS        LIKE MARD-WERKS,
              LGORT        LIKE MARD-LGORT,
              LVORM        LIKE MARD-LVORM,
*              LGORT_TOTAL  LIKE LGORT_TOTAL,
              T_QTYHAND    LIKE TOTAL_VALUE,
          END OF TABLE1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_STORLO        FOR   MARD-LGORT.
*     S_PLANT         FOR   MARC-WERKS,
*     S_MATNUM        FOR   MARC-MATNR.
SELECTION-SCREEN END OF BLOCK BOX1.

*******************************  MAIN  *********************************
TOP-OF-PAGE.
WRITE: / SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 81 'PAGE:' INTENSIFIED OFF.
WRITE: 87(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.

WRITE: /1 SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 30 TEXT-002 COLOR COL_HEADING.
ULINE.
WRITE: /.
FORMAT COLOR COL_NORMAL.
WRITE: /1 TEXT-004, 28 TEXT-018.
WRITE: /1 TEXT-005, 28 TEXT-019.
ULINE.
WRITE: /.

************************************************************************
START-OF-SELECTION.
FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
CLEAR TABLE1.
REFRESH TABLE1.

        SELECT * FROM MARD WHERE LVORM NE 'X'
                             AND LGORT IN S_STORLO
*                             AND WERKS IN S_PLANT
                             ORDER BY WERKS LGORT.

              ON CHANGE OF MARD-LGORT OR MARD-WERKS.
              IF CHECK1 EQ 'Y'.
                 MOVE TEMP-WERKS TO TABLE1-WERKS.
                 MOVE TEMP-LGORT TO TABLE1-LGORT.
*                 MOVE QTYHAND TO TABLE1-T_QTYHAND.
                 MOVE LGORT_TOTAL TO TABLE1-T_QTYHAND.
                 APPEND TABLE1.
                 CLEAR: TABLE1, QTYHAND, LGORT_TOTAL.
              ENDIF.
              ENDON.

              MOVE 'Y' TO CHECK1.

              QTYHAND = QTYHAND + MARD-LABST + MARD-UMLME +
              MARD-INSME + MARD-SPEME + MARD-EINME.

            SELECT SINGLE * FROM MBEW WHERE MATNR = MARD-MATNR
                                        AND BWKEY = MARD-WERKS
                                        AND BWTAR = SPACE.

            TOTAL_VALUE = MBEW-VERPR * QTYHAND.
            LGORT_TOTAL = LGORT_TOTAL + TOTAL_VALUE.
            CLEAR: QTYHAND, TOTAL_VALUE.
            MOVE MARD-WERKS TO TEMP-WERKS.
            MOVE MARD-LGORT TO TEMP-LGORT.
        ENDSELECT.

SORT TABLE1 BY WERKS  ASCENDING
               LGORT  ASCENDING.

LOOP AT TABLE1.
        ON CHANGE OF TABLE1-WERKS.
             WRITE: /.
        ENDON.
        WRITE: /1 TABLE1-WERKS, 6 TEXT-900, 8 TABLE1-LGORT.
        WRITE: 20 TABLE1-T_QTYHAND.
ENDLOOP.
