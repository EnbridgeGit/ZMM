REPORT ZMINR055 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR055
*    Programmer  :  Mark Dufault
*    Client      :
*    Date        :  October 13,2000
*
*
*
*
*
************************************************************************
TABLES  : MSEG, MAKT.
DATA    : MANU-ATINN    VALUE 'Y',
          CHECK1(1)     VALUE 'Y',
          CHECK2        LIKE CHECK1.

DATA    : BEGIN OF TABLE1 OCCURS 50000,
              MATNR        LIKE MSEG-MATNR,
              BWART        LIKE MSEG-BWART,
              PLPLA        LIKE MSEG-PLPLA,
              MENGE        LIKE MSEG-MENGE,
              MEINS        LIKE MSEG-MEINS,
              WEMPF        LIKE MSEG-WEMPF,
              MAKTX        LIKE MAKT-MAKTX,
              WERKS        LIKE MSEG-WERKS,
              LGORT        LIKE MSEG-LGORT,
          END OF TABLE1.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_MBLNR         FOR   MSEG-MBLNR.

SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-008.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-009.
INCLUDE <ICON>.
*******************************  MAIN  *********************************
TOP-OF-PAGE.
  WRITE: / ICON_DATE AS ICON.
  WRITE: SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
  WRITE: 55 'STOREROOM DOCUMENT DISPLAY'.
  WRITE: 123 'PAGE:' INTENSIFIED OFF.
  WRITE: 129(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.

  WRITE: / ICON_TIME AS ICON.
  WRITE: SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
  WRITE: 40 TEXT-011 COLOR COL_HEADING, 60 'SORTED BY BIN'.

  ULINE.
  WRITE: /1 TEXT-003, 40 TEXT-001, 60 TEXT-006, 80 TEXT-006.
  WRITE: /1 TEXT-004, 40 TEXT-002, 60 TEXT-018, 80 TEXT-007.
  ULINE.
  WRITE: /.

************************************************************************
START-OF-SELECTION.
CLEAR TABLE1.
REFRESH TABLE1.

SELECT * FROM MSEG WHERE MBLNR IN S_MBLNR
                     ORDER BY PLPLA.

         MOVE MSEG-MATNR   TO TABLE1-MATNR.
         MOVE MSEG-BWART   TO TABLE1-BWART.
         MOVE MSEG-PLPLA   TO TABLE1-PLPLA.
         MOVE MSEG-MENGE   TO TABLE1-MENGE.
         MOVE MSEG-MEINS   TO TABLE1-MEINS.
         MOVE MSEG-WEMPF   TO TABLE1-WEMPF.
         MOVE MSEG-WERKS   TO TABLE1-WERKS.
         MOVE MSEG-LGORT   to TABLE1-LGORT.

    SELECT * FROM MAKT WHERE MATNR EQ MSEG-MATNR.
          MOVE MAKT-MAKTX  TO TABLE1-MAKTX.
         APPEND TABLE1.


    ENDSELECT.



CLEAR TABLE1.
ENDSELECT.

SORT TABLE1 BY PLPLA  ASCENDING.

   WRITE: /1 TEXT-010 INTENSIFIED OFF.
   WRITE: S_MBLNR+3(10) COLOR COL_NEGATIVE INVERSE ON.
   WRITE: '/', S_MBLNR+3(10) COLOR COL_NEGATIVE INVERSE ON.
   WRITE: /1 'MATERIAL       BIN ', 22 'DESCRIPTION'.
   WRITE: 55 'QUANTITY', 69 'UM', 74 'MV TY'.
   WRITE: 80 'RECIPIENT'.
   WRITE: /.
LOOP AT TABLE1.

   WRITE: /1 TABLE1-MATNR, 10 TABLE1-PLPLA, 22 TABLE1-MAKTX.
   WRITE: 51 TABLE1-MENGE, 69 TABLE1-MEINS, 74 TABLE1-BWART.
   WRITE: 80 TABLE1-WEMPF, 94 TABLE1-WERKS, 99 TABLE1-LGORT.
ENDLOOP.

