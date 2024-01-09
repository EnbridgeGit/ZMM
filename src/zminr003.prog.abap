REPORT ZMINR003 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR003
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  July 17 1996
*
* This report will display the information on slow moving stock for an
* adjustable time period and for which there are no outstanding
* reservations.
*
*    Program miodified : May 07, 1998 Nesh Laurencic - Omnilogic
*    #511 Issue Log
************************************************************************
TABLES  : MARA, MARC, MARD, RESB, MSEG, MKPF, MAKT, MBEW.
DATA    : QTYHAND         TYPE I,
          LINE_COUNT      TYPE I,
          OCCURS_COUNT    TYPE I,
          DATE_AMOUNT     TYPE I,
          TOTAL_ENMNG     LIKE RESB-ENMNG,
          TOTAL_BDMNG     LIKE RESB-BDMNG,
          DIFFFER         LIKE RESB-ENMNG,
          TOTAL_VALUE     TYPE P DECIMALS 2,
          FLAG TYPE C.
DATA    : BEGIN OF TABLE1 OCCURS 5000,
              MATNR     LIKE MARD-MATNR,
              WERKS     LIKE MARD-WERKS,
              LGORT     LIKE MARD-LGORT,
              LVORM     LIKE MARD-LVORM,
              T_QTYHAND LIKE QTYHAND,
              MATKL LIKE MARA-MATKL,   "Material Group
          END OF TABLE1.
* To improve the perfomance we added this working areas
DATA: BEGIN OF IMSEG OCCURS 1000,
      MBLNR LIKE MSEG-MBLNR,
      MATNR LIKE MSEG-MATNR,
      BUDAT LIKE MKPF-BUDAT,
      END OF IMSEG.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_PLANT         FOR   MARC-WERKS,
     S_STORLO        FOR   MARD-LGORT ,
     S_MATNUM        FOR   MARC-MATNR.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECT-OPTIONS S_MATGRP FOR MARA-MATKL.
SELECTION-SCREEN SKIP 3.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-100.
PARAMETERS: P1_DATE    LIKE SY-DATUM OBLIGATORY.
SELECTION-SCREEN COMMENT 46(23) TEXT-200.
PARAMETERS:         P2_DATE   LIKE SY-DATUM OBLIGATORY DEFAULT SY-DATUM.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX2.

*******************************  MAIN  *********************************
TOP-OF-PAGE.
  WRITE: /1 TEXT-REP, SY-REPID,
        104 TEXT-DAT, SY-DATUM COLOR 4 INTENSIFIED ON,
        TEXT-TIM, SY-UZEIT.

  WRITE: 62 TEXT-002 COLOR COL_HEADING.
  WRITE:
         /1 TEXT-CLI, SY-MANDT,
        123 'PAGE:' INTENSIFIED OFF,
        129(3) SY-PAGNO COLOR  4 INTENSIFIED ON.

  WRITE: 58 TEXT-901 INTENSIFIED OFF.
  WRITE: P1_DATE COLOR  4 INTENSIFIED ON.
  WRITE: 76 TEXT-902 INTENSIFIED OFF.
  WRITE: P2_DATE COLOR 4 INTENSIFIED ON.
  WRITE: /.
  WRITE: / TEXT-903 INTENSIFIED OFF.
* 18 p_matgrp color col_key.
  ULINE.
  WRITE: /.
  FORMAT COLOR COL_NORMAL.
  WRITE: /1 TEXT-004, 23 TEXT-006, 33 TEXT-006, 68 TEXT-008.
  WRITE: 90 TEXT-011, 107 TEXT-016, 126 TEXT-018.
  WRITE: /1 TEXT-005, 23 TEXT-007, 33 TEXT-013, 72 TEXT-009.
  WRITE: 95 TEXT-012, 107 TEXT-017, 126 TEXT-019.
  ULINE.
  WRITE: /.

START-OF-SELECTION.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  CLEAR TABLE1.
  REFRESH TABLE1.

  SELECT * FROM MARA WHERE MATNR IN S_MATNUM
                       AND MATKL IN S_MATGRP
                       AND LVORM NE 'X'.


    SELECT * FROM MARC WHERE MATNR = MARA-MATNR
                       AND WERKS IN S_PLANT
                       AND LVORM NE 'X'.   "nOT DELETED

      IF SY-SUBRC EQ 0.
        SELECT * FROM MARD WHERE MATNR = MARC-MATNR
                        AND WERKS = MARC-WERKS
                        AND LGORT IN S_STORLO
                        AND LVORM NE 'X'.

* Valuated stock with unrestricted use   MARD-LABST
* stock in transfer (from one storage location to another) MARD-UMLME
* Stock in quality inspection MARD-INSME
* Blocked stock  MARD-SPEME
* Restricted-use stock  MARD-EINME
          QTYHAND = QTYHAND + MARD-LABST + MARD-UMLME +
          MARD-INSME + MARD-SPEME + MARD-EINME.

          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING MARD TO TABLE1.
            MOVE MARA-MATKL TO TABLE1-MATKL.
            MOVE QTYHAND TO TABLE1-T_QTYHAND.
            APPEND TABLE1.
            CLEAR TABLE1.
            CLEAR QTYHAND.
          ENDIF.
        ENDSELECT.
      ENDIF.
    ENDSELECT.
  ENDSELECT.

  SORT TABLE1 BY WERKS  ASCENDING
                 MATNR  ASCENDING.
  LOOP AT TABLE1.
  REFRESH IMSEG.
  CLEAR IMSEG.
  CLEAR DIFFFER.

    IF TABLE1-T_QTYHAND NE 0.
* Reservation/dependent requirements
      CLEAR:  RESB, TOTAL_ENMNG, TOTAL_BDMNG.
      SELECT * FROM RESB WHERE WERKS = TABLE1-WERKS
                           AND MATNR = TABLE1-MATNR
                           AND LGORT = TABLE1-LGORT
                           AND XLOEK NE 'X'   "NOT DELETED
                           AND KZEAR NE 'X'.  "fINAL ISSUE FOR THIS RES

        TOTAL_ENMNG = TOTAL_ENMNG + RESB-ENMNG.  "Quantity withdrawn
        TOTAL_BDMNG = TOTAL_BDMNG + RESB-BDMNG.  "Requirements quantity
      ENDSELECT.
* REQUIREMENTS  - QUANT. WITHDRAWN
      DIFFFER = TOTAL_BDMNG - TOTAL_ENMNG.

* NESH WILL CHANGE THIS CHRITERIA
      IF DIFFFER < 1. "          No outstanding reservation
        PERFORM CHECK_IF_THERE_IS_ACTIVITY.  "nl

        IF FLAG = 1.
          FLAG = 0.
          CONTINUE.
        ELSE.
          PERFORM FIND_LAST_TRANSACTION.    "nl
          PERFORM PROCESS_AND_PRINT.
          CHECK SY-SUBRC = 0.
        ENDIF.
      ENDIF.
    ENDIF.
    FLAG = 0.
  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_AND_PRINT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_AND_PRINT.
  FLAG = 1.
  SELECT SINGLE * FROM MAKT WHERE SPRAS = SY-LANGU
                              AND MATNR = TABLE1-MATNR.
  SELECT SINGLE * FROM MBEW WHERE MATNR = TABLE1-MATNR
                              AND BWKEY = TABLE1-WERKS
                              AND BWTAR = SPACE.
  TOTAL_VALUE = MBEW-VERPR * TABLE1-T_QTYHAND.

  ON CHANGE OF TABLE1-WERKS.
    WRITE: /.
  ENDON.
  WRITE: /1 TABLE1-WERKS, 6 TEXT-900, 8 TABLE1-LGORT.
  WRITE: 15 TABLE1-MATKL.
  WRITE: 24 TABLE1-MATNR+12(6) COLOR COL_NEGATIVE.
  WRITE: 33 MAKT-MAKTX, 76 IMSEG-BUDAT.
  WRITE: 88 TABLE1-T_QTYHAND COLOR COL_KEY.
  WRITE: 100 MBEW-VERPR.
  WRITE: 120 TOTAL_VALUE COLOR COL_TOTAL.

ENDFORM.                               " PROCESS_AND_PRINT
*&---------------------------------------------------------------------*
*&      Form  CHECK_IF_THERE_IS_ACTIVITY
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_IF_THERE_IS_ACTIVITY.
  SELECT * FROM MSEG WHERE MATNR = TABLE1-MATNR
                       AND WERKS = TABLE1-WERKS
                       AND LGORT = TABLE1-LGORT.
    MOVE:
         MSEG-MBLNR TO IMSEG-MBLNR,
         MSEG-MATNR TO IMSEG-MATNR.

    SELECT SINGLE * FROM MKPF WHERE
                                    MBLNR = MSEG-MBLNR AND
                                    MJAHR = MSEG-MJAHR AND
*                                         budat ge p1_date and
                                    BUDAT LE P2_DATE.
    CHECK SY-SUBRC = 0.
    MOVE MKPF-BUDAT TO IMSEG-BUDAT.
    APPEND IMSEG.
  ENDSELECT.
  SORT IMSEG BY BUDAT DESCENDING.
  LOOP AT IMSEG WHERE
                       BUDAT GE P1_DATE AND
                       BUDAT LE P2_DATE.
  ENDLOOP.
  IF SY-SUBRC = 0.
    FLAG = 1.
  ENDIF.

ENDFORM.                               " CHECK_IF_THERE_IS_ACTIVITY
*&---------------------------------------------------------------------*
*&      Form  FIND_LAST_TRANSACTION
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_LAST_TRANSACTION.
  LOOP AT IMSEG WHERE BUDAT LT P1_DATE.
    IF SY-TABIX = 1.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " FIND_LAST_TRANSACTION
