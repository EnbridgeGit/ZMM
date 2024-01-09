REPORT ZMINR011 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR011
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  January 22, 1997.
*
* This ABAP will output a list of all the Manufacturer Parts Numbers and
* the BIN NUMBER location, for the Plant and Storage Location entered
* at the selection screen.
*
* Changes:
* 2013/05/02 GYMANA - Replace characteristic field AUSP-ATWRT
* SDP42691            with MARA-MFRPN.
************************************************************************
TABLES  : MARA, MARC, MARD, MAKT, AUSP, CABN.
DATA    : MANU-ATINN    LIKE CABN-ATINN,
          CHECK1(1)     VALUE 'Y',
          CHECK2(1)     VALUE 'Y'.

DATA    : BEGIN OF TABLE1 OCCURS 50000,
              MATNR        LIKE MARA-MATNR,
              WERKS        LIKE MARD-WERKS,
              LGORT        LIKE MARD-LGORT,
              MFRPN        LIKE MARA-MFRPN,                    "SDP42691
              LGPBE        LIKE MARD-LGPBE,
          END OF TABLE1.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_MATNR         FOR   MARA-MATNR,
     S_WERKS         FOR   MARC-WERKS NO INTERVALS NO-EXTENSION,
     S_LGORT         FOR   MARD-LGORT NO INTERVALS NO-EXTENSION.
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
  WRITE: 123 'PAGE:' INTENSIFIED OFF.
  WRITE: 129(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.

  WRITE: / ICON_TIME AS ICON.
  WRITE: SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
  WRITE: 40 TEXT-011 COLOR COL_HEADING.

  ULINE.
  WRITE: /1 TEXT-003, 40 TEXT-001, 60 TEXT-006, 80 TEXT-006.
  WRITE: /1 TEXT-004, 40 TEXT-002, 60 TEXT-018, 80 TEXT-007.
  ULINE.
  WRITE: /.

************************************************************************
START-OF-SELECTION.
PERFORM GET_ATINN.
CLEAR TABLE1.
REFRESH TABLE1.

SELECT * FROM MARD WHERE LVORM NE 'X'
                     AND MATNR IN S_MATNR
                     AND LGORT IN S_LGORT
                     AND WERKS IN S_WERKS
                     ORDER BY WERKS LGORT MATNR.

         MOVE MARD-MATNR   TO TABLE1-MATNR.
         MOVE MARD-WERKS   TO TABLE1-WERKS.
         MOVE MARD-LGORT   TO TABLE1-LGORT.
         MOVE MARD-LGPBE   TO TABLE1-LGPBE.

    SELECT * FROM MARA                                         "SDP42691
     WHERE BMATN = MARD-MATNR                                  "SDP42691
       AND MTART = 'HERS'.                                     "SDP42691
                                                               "SDP42691
      IF SY-SUBRC = 0.                                         "SDP42691
        MOVE MARA-MFRPN TO TABLE1-MFRPN.                       "SDP42691
        MOVE 'N' TO CHECK2.                                    "SDP42691
        APPEND TABLE1.                                         "SDP42691
        CLEAR TABLE1-MFRPN.                                    "SDP42691
      ENDIF.                                                   "SDP42691
    ENDSELECT.                                                 "SDP42691

* 2013/05/02 GYMANA SDP42691 - commented out
*    SELECT * FROM AUSP WHERE OBJEK EQ MARD-MATNR.
*      IF AUSP-ATINN EQ MANU-ATINN.
*         MOVE AUSP-ATWRT   TO TABLE1-ATWRT.
*         MOVE MARD-MATNR   TO TABLE1-MATNR.
*         MOVE MARD-WERKS   TO TABLE1-WERKS.
*         MOVE MARD-LGORT   TO TABLE1-LGORT.
*         MOVE MARD-LGPBE   TO TABLE1-LGPBE.
*         MOVE 'N' TO CHECK2.
*         APPEND TABLE1.
*         CLEAR: TABLE1-ATWRT.
*      ENDIF.
*    ENDSELECT.

    IF CHECK2 EQ 'Y'.
        APPEND TABLE1.
    ENDIF.

CLEAR TABLE1.
MOVE 'Y' TO CHECK2.
ENDSELECT.

SORT TABLE1 BY MFRPN  ASCENDING
               LGPBE  ASCENDING
               MATNR  ASCENDING
               WERKS  ASCENDING
               LGORT  ASCENDING.

LOOP AT TABLE1.
IF CHECK1 EQ 'Y'.
   WRITE: /1 TEXT-010 INTENSIFIED OFF.
   WRITE: TABLE1-WERKS COLOR COL_NEGATIVE INVERSE ON.
   WRITE: '/', TABLE1-LGORT COLOR COL_NEGATIVE INVERSE ON.
   WRITE: /.
   WRITE: /.
ENDIF.
MOVE 'N' TO CHECK1.

SELECT SINGLE * FROM MAKT WHERE MATNR = TABLE1-MATNR
                            AND SPRAS = SY-LANGU.

   WRITE: /1 TABLE1-MFRPN, 40 TABLE1-LGPBE, 60 TABLE1-MATNR.
   WRITE: 80 MAKT-MAKTX.
ENDLOOP.

*---------------------------------------------------------------------*
*       FORM GET_ATINN                                                *
*---------------------------------------------------------------------*
*                                                                     *
*---------------------------------------------------------------------*
FORM GET_ATINN.
    SELECT SINGLE * FROM CABN WHERE ATNAM = 'MANUFACTURER_PART_NUMBER'.
        MOVE CABN-ATINN  TO  MANU-ATINN.
ENDFORM.
