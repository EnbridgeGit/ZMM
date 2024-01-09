REPORT ZMMMR052 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132
                MESSAGE-ID ZM.
************************************************************************
*  Author:      Mohammad Khan
*  Date:        July 2000.
*  Description:
*     - The purpose of this program is to produce the
*       Commodity Delivery Analysis Report.
************************************************************************
* Changes:
*
*
*
************************************************************************

TABLES: LFA1,                          "Vendor master (general section)
        MAKT,                          "Material Descriptions
        S012,                          "PURCHIS: Purchasing Statistics
        T001,                          "Company code
       T023T,                          "Material Group Descriptions
        EKPO,                          "Purchasing Document Item
        EKKO,                          "Purchasing Document Header
        MSEG,                          "Document Segment: Material
        EBAN.                          "Purchase Requisition

INCLUDE <SYMBOL>.
DATA:
    BEGIN OF SAVE_TABLE OCCURS 10000,
       MATKL        LIKE S012-MATKL,   "Material Group
       MATNR        LIKE S012-MATNR,   "Material
       LIFNR        LIKE S012-LIFNR,   "Vendor
       WERKS        LIKE S012-WERKS,   "Plant
       SPMON        LIKE S012-SPMON,   "Period
       ALIEF        LIKE S012-ALIEF,   "No of Deliveries
       LFZTA        LIKE S012-LFZTA,   "Amount
   END OF SAVE_TABLE.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
       MATKL        LIKE S012-MATKL,   "Material Group
       MATNR        LIKE S012-MATNR,   "Material
       LIFNR        LIKE S012-LIFNR,   "Vendor
       WERKS        LIKE S012-WERKS,   "Plant
       SPMON        LIKE S012-SPMON,   "Date
       ALIEF1       LIKE S012-ALIEF,   "No of Deliveries for period 1
       LFZTA1       LIKE S012-LFZTA,   "Amount for period 1
       MEANS1       TYPE I,            "Mean Delivery for period 1
       ALIEF2       LIKE S012-ALIEF,   "No of Deliveries for period 2
       LFZTA2       LIKE S012-LFZTA,   "Amount for period 2
       MEANS2       TYPE I,            "Mean Delivery for period 2
       ALIEF3       LIKE S012-ALIEF,   "No of Deliveries for period 3
       LFZTA3       LIKE S012-LFZTA,   "Amount for period 3
       MEANS3       TYPE I,            "Mean Delivery for period 3
       MAKTX        LIKE MAKT-MAKTX,   "Description
   END OF BIG_TABLE.

DATA:
    BEGIN OF SUM_TABLE OCCURS 10000,
       MATKL        LIKE S012-MATKL,   "Material Group
       MATNR        LIKE S012-MATNR,   "Material
       WERKS        LIKE S012-WERKS,   "Plant
       ALIEF1       LIKE S012-ALIEF,   "No of Deliveries for period 1
       ALIEF2       LIKE S012-ALIEF,   "No of Deliveries for period 2
       ALIEF3       LIKE S012-ALIEF,   "No of Deliveries for period 3
       LFZTA1       LIKE S012-LFZTA,   "Amount for period 1
       LFZTA2       LIKE S012-LFZTA,   "Amount for period 2
       LFZTA3       LIKE S012-LFZTA,   "Amount for period 3
       MEANS1       TYPE I,            "Mean Delivery for period 1
       MEANS2       TYPE I,            "Mean Delivery for period 2
       MEANS3       TYPE I,            "Mean Delivery for period 3
       MAKTX        LIKE MAKT-MAKTX,   "Description
   END OF SUM_TABLE.

DATA:  LIFNRDESC     LIKE LFA1-NAME1.
DATA:  TEMPWERKS     LIKE S012-WERKS.
DATA:  TEMPMATKL     LIKE S012-MATKL.
DATA:  TEMPMATNR     LIKE S012-MATNR.
DATA:  TEMPLIFNR     LIKE S012-LIFNR.
DATA:  TEMPSPMON     LIKE S012-SPMON.
DATA:  LIFNRLFZTA1   LIKE S012-LFZTA.
DATA:  LIFNRLFZTA2   LIKE S012-LFZTA.
DATA:  LIFNRLFZTA3   LIKE S012-LFZTA.
DATA:  MATNRLFZTA1   LIKE S012-LFZTA.
DATA:  MATNRLFZTA2   LIKE S012-LFZTA.
DATA:  MATNRLFZTA3   LIKE S012-LFZTA.
DATA:  LIFNRALIEF1   LIKE S012-ALIEF.
DATA:  LIFNRALIEF2   LIKE S012-ALIEF.
DATA:  LIFNRALIEF3   LIKE S012-ALIEF.
DATA:  MATNRALIEF1   LIKE S012-ALIEF.
DATA:  MATNRALIEF2   LIKE S012-ALIEF.
DATA:  MATNRALIEF3   LIKE S012-ALIEF.
DATA:  DEL_DATE      LIKE S012-SPMON.
DATA:  WPERIOD.
DATA:  BEGIN OF PERIOD1,
         LOW  LIKE S012-SPMON,
         DASH(1) VALUE '-',
         HIGH LIKE S012-SPMON,
       END OF PERIOD1.
DATA:  BEGIN OF PERIOD2,
         LOW  LIKE S012-SPMON,
         DASH(1) VALUE '-',
         HIGH LIKE S012-SPMON,
       END OF PERIOD2.
DATA:  BEGIN OF PERIOD3,
         LOW  LIKE S012-SPMON,
         DASH(1) VALUE '-',
         HIGH LIKE S012-SPMON,
       END OF PERIOD3.
DATA:  COUNTER       TYPE I.
DATA:  FLAGPRT(3)    TYPE C.

DATA:  COUNTER21 TYPE I,                "         # po for period 1
       COUNTER22 TYPE I,                "         # po for period 2
       COUNTER23 TYPE I.                "         # po for period 3
* data:  counter3 type i.                 "         # receipts

* Two tables for counting purposses.
DATA: BEGIN OF COUNTAB1 OCCURS 50,
       MBLNR LIKE MSEG-MBLNR,
       MJAHR LIKE MSEG-MJAHR,
       ZEILE LIKE MSEG-ZEILE,          "Item in material document
       BWART LIKE MSEG-BWART,
       MENGE LIKE MSEG-MENGE,          "Quantity
       PERIOD(1) TYPE C.               "Period
DATA: END OF COUNTAB1.

DATA: BEGIN OF COUNTAB2 OCCURS 50.
        INCLUDE STRUCTURE COUNTAB1.
DATA: END OF COUNTAB2.

DATA:  FLAG TYPE C VALUE 0.            " Flag

* Following is just for output.
DATA:  COUNTPO1(4)  TYPE C,            "# of po for period 1
       COUNTPO2(4)  TYPE C,            "# of po for period 2
       COUNTPO3(4)  TYPE C,            "# of po for period 3
*      countrec(4)  type c,            "# of receipts
       TMPMEANS1(4) TYPE C,
       TMPMEANS2(4) TYPE C,
       TMPMEANS3(4) TYPE C.

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOXA WITH FRAME TITLE TEXT-117.
 SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 1(62) TEXT-118.
 SELECTION-SCREEN END OF LINE.

 SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 1(72) TEXT-119.
 SELECTION-SCREEN END OF LINE.

 SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 1(62) TEXT-120.
 SELECTION-SCREEN END OF LINE.

 SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 1(62) TEXT-121.
 SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOXA.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
PARAMETERS:
   P_CCODE LIKE PRPS-PBUKR DEFAULT 'UGL'.              "Company code
SELECT-OPTIONS:  SMATKL FOR S012-MATKL OBLIGATORY,     "Material Group
                 SSPMON FOR S012-SPMON OBLIGATORY,     "Selected Period
                 SWERKS FOR S012-WERKS DEFAULT 'P100' TO 'P320'. "Plant
*     smatnr for s012-matnr.       " For testing only
SELECTION-SCREEN END OF BLOCK BOX.

*---------------------  TOP-OF-PAGE  -----------------------------------
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         58 T001-BUTXT COLOR 4 INTENSIFIED ON,
        100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.

  WRITE: /1 TEXT-CLT, SY-MANDT, SY-SYSID,
         50 TEXT-TTL COLOR 4 INTENSIFIED ON.
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.

  WRITE: /.

  SELECT SINGLE * FROM T023T
     WHERE MATKL = SUM_TABLE-MATKL
       AND SPRAS = SY-LANGU.
  WRITE: / TEXT-007 INTENSIFIED OFF, SUM_TABLE-MATKL COLOR COL_KEY,
           T023T-WGBEZ COLOR COL_KEY.  "Material info
  ULINE.
  WRITE: /1 TEXT-009,   10 TEXT-010,  35 TEXT-011, 46 TEXT-012,
            80 PERIOD1, 96 PERIOD2,  112 PERIOD3.
  WRITE: /80 TEXT-013,  88 TEXT-014,
            96 TEXT-013, 104 TEXT-014,
           112 TEXT-013, 120 TEXT-014.

  ULINE.

*-------------------  START-OF-SELECTION  ------------------------------
* select all records from S012 table that satisfy selection criteria
*-----------------------------------------------------------------------
START-OF-SELECTION.

 LOOP AT SSPMON.
      IF SY-TABIX = 1.
         MOVE SSPMON-LOW  TO PERIOD1-LOW.
         MOVE SSPMON-HIGH TO PERIOD1-HIGH.
      ELSEIF SY-TABIX = 2.
         MOVE SSPMON-LOW  TO PERIOD2-LOW.
         MOVE SSPMON-HIGH TO PERIOD2-HIGH.

      ELSEIF SY-TABIX = 3.
         MOVE SSPMON-LOW  TO PERIOD3-LOW.
         MOVE SSPMON-HIGH TO PERIOD3-HIGH.
      ELSE.
         EXIT.
      ENDIF.
 ENDLOOP.

SELECT SINGLE * FROM T001                "Company Code
  WHERE BUKRS = P_CCODE.


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Reading S012 Data'
       EXCEPTIONS
            OTHERS = 1.

  SELECT * FROM S012
     WHERE WERKS IN SWERKS                      "Plant
       AND MATKL IN SMATKL                      "Material Group
       AND SPMON IN SSPMON                      "Select Period (yyyy/mm)
       AND ALIEF > 0                            "No. of deliveries
       AND MATNR NE SPACE.                      "Eliminate NIS items
*      and matnr in smatnr.                     "for testing only

    PERFORM BUILD_SAVE_TABLE.

  ENDSELECT.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Building Big Table'
       EXCEPTIONS
            OTHERS = 1.

  PERFORM BUILD_BIG_TABLE.


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Display Table '
       EXCEPTIONS
            OTHERS = 1.

  PERFORM DISPLAY_TABLE.

  WRITE: /.
  WRITE: / TEXT-028 UNDER TEXT-TTL.    "End of Report

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*-----------------------  BUILD_SAVE_TABLE  ----------------------------
*  This subroutine moves all data to SAVE_TABLE.
*-----------------------------------------------------------------------

FORM BUILD_SAVE_TABLE.
  CLEAR SAVE_TABLE.
  MOVE S012-WERKS     TO SAVE_TABLE-WERKS.             "Plant
  MOVE S012-MATKL     TO SAVE_TABLE-MATKL.             "Material Group
  MOVE S012-MATNR     TO SAVE_TABLE-MATNR.             "Material
  MOVE S012-LIFNR     TO SAVE_TABLE-LIFNR.             "Vendor
  MOVE S012-ALIEF     TO SAVE_TABLE-ALIEF.             "No of Deliveries
  MOVE S012-LFZTA     TO SAVE_TABLE-LFZTA.             "Amount.
  MOVE S012-SPMON     TO SAVE_TABLE-SPMON.             "Period.
  APPEND SAVE_TABLE.

ENDFORM.
*-----------------------  BUILD_BIG_TABLE  -----------------------------
*  This subroutine moves all data to BIG_TABLE.
*-----------------------------------------------------------------------

FORM BUILD_BIG_TABLE.
  SORT SAVE_TABLE BY MATKL MATNR LIFNR WERKS.
  LOOP AT SAVE_TABLE.
    AT NEW WERKS.
      TEMPWERKS = SAVE_TABLE-WERKS.
    ENDAT.

    AT NEW SPMON.
      TEMPSPMON = SAVE_TABLE-SPMON.
    ENDAT.

    AT NEW MATKL.
      TEMPMATKL = SAVE_TABLE-MATKL.
    ENDAT.

    AT NEW MATNR.
      TEMPMATNR = SAVE_TABLE-MATNR.
    ENDAT.

    AT NEW LIFNR.
      TEMPLIFNR = SAVE_TABLE-LIFNR.
    ENDAT.

IF SAVE_TABLE-SPMON GE PERIOD1-LOW AND
   SAVE_TABLE-SPMON LE PERIOD1-HIGH.
    LIFNRLFZTA1 = LIFNRLFZTA1 + SAVE_TABLE-LFZTA.
    LIFNRALIEF1 = LIFNRALIEF1 + SAVE_TABLE-ALIEF.

    MATNRLFZTA1 = MATNRLFZTA1 + SAVE_TABLE-LFZTA.
    MATNRALIEF1 = MATNRALIEF1 + SAVE_TABLE-ALIEF.
ELSEIF SAVE_TABLE-SPMON GE PERIOD2-LOW AND
       SAVE_TABLE-SPMON LE PERIOD2-HIGH.
         LIFNRLFZTA2 = LIFNRLFZTA2 + SAVE_TABLE-LFZTA.
         LIFNRALIEF2 = LIFNRALIEF2 + SAVE_TABLE-ALIEF.

         MATNRLFZTA2 = MATNRLFZTA2 + SAVE_TABLE-LFZTA.
         MATNRALIEF2 = MATNRALIEF2 + SAVE_TABLE-ALIEF.

ELSE.
         LIFNRLFZTA3 = LIFNRLFZTA3 + SAVE_TABLE-LFZTA.
         LIFNRALIEF3 = LIFNRALIEF3 + SAVE_TABLE-ALIEF.

         MATNRLFZTA3 = MATNRLFZTA3 + SAVE_TABLE-LFZTA.
         MATNRALIEF3 = MATNRALIEF3 + SAVE_TABLE-ALIEF.
ENDIF.

    AT END OF LIFNR.
      BIG_TABLE-WERKS = TEMPWERKS.
      BIG_TABLE-MATKL = TEMPMATKL.
      BIG_TABLE-MATNR = TEMPMATNR.
      BIG_TABLE-LIFNR = TEMPLIFNR.
      BIG_TABLE-SPMON = TEMPSPMON.
      BIG_TABLE-LFZTA1 = LIFNRLFZTA1.
      BIG_TABLE-LFZTA2 = LIFNRLFZTA2.
      BIG_TABLE-LFZTA3 = LIFNRLFZTA3.
      BIG_TABLE-ALIEF1 = LIFNRALIEF1.
      BIG_TABLE-ALIEF2 = LIFNRALIEF2.
      BIG_TABLE-ALIEF3 = LIFNRALIEF3.

      IF LIFNRLFZTA1 > 0   AND
         LIFNRALIEF1 > 0.
         BIG_TABLE-MEANS1 = LIFNRLFZTA1 DIV LIFNRALIEF1.
      ENDIF.
      IF LIFNRLFZTA2 > 0   AND
         LIFNRALIEF2 > 0.
         BIG_TABLE-MEANS2 = LIFNRLFZTA2 DIV LIFNRALIEF2.
      ENDIF.
      IF LIFNRLFZTA3 > 0   AND
         LIFNRALIEF3 > 0.
         BIG_TABLE-MEANS3 = LIFNRLFZTA3 DIV LIFNRALIEF3.
      ENDIF.
      APPEND BIG_TABLE.
      CLEAR: LIFNRLFZTA1, LIFNRALIEF1,
             LIFNRLFZTA2, LIFNRALIEF2,
             LIFNRLFZTA3, LIFNRALIEF3.
      CLEAR BIG_TABLE.
    ENDAT.

    AT END OF MATNR.
      SELECT SINGLE * FROM MAKT
        WHERE MATNR = TEMPMATNR        "Get Description
          AND SPRAS = SY-LANGU.
      SUM_TABLE-MAKTX = MAKT-MAKTX.
      IF SY-SUBRC = '0'.

        SUM_TABLE-WERKS = TEMPWERKS.
        SUM_TABLE-MATKL = TEMPMATKL.
        SUM_TABLE-MATNR = TEMPMATNR.
        SUM_TABLE-LFZTA1 = MATNRLFZTA1.
        SUM_TABLE-LFZTA2 = MATNRLFZTA2.
        SUM_TABLE-LFZTA3 = MATNRLFZTA3.
        SUM_TABLE-ALIEF1 = MATNRALIEF1.
        SUM_TABLE-ALIEF2 = MATNRALIEF2.
        SUM_TABLE-ALIEF3 = MATNRALIEF3.
        IF MATNRLFZTA1 > 0 AND
           MATNRALIEF1 > 0.
           SUM_TABLE-MEANS1 = MATNRLFZTA1 DIV MATNRALIEF1.
        ENDIF.
        IF MATNRLFZTA2 > 0 AND
           MATNRALIEF2 > 0.
           SUM_TABLE-MEANS2 = MATNRLFZTA2 DIV MATNRALIEF2.
        ENDIF.
        IF MATNRLFZTA3 > 0 AND
           MATNRALIEF3 > 0.
           SUM_TABLE-MEANS3 = MATNRLFZTA3 DIV MATNRALIEF3.
        ENDIF.
        APPEND SUM_TABLE.
        CLEAR: MATNRLFZTA1, MATNRALIEF1,
               MATNRLFZTA2, MATNRALIEF2,
               MATNRLFZTA3, MATNRALIEF3.
        CLEAR SUM_TABLE.
      ENDIF.
    ENDAT.

  ENDLOOP.
ENDFORM.
*----------------------  DISPLAY_TABLE  --------------------------------
*  This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.
  SORT SUM_TABLE BY MATKL MATNR WERKS.
  LOOP AT SUM_TABLE.

    AT NEW MATKL.
      NEW-PAGE.
    ENDAT.

    AT NEW MATNR.
      RESERVE 2 LINES.
      CLEAR FLAGPRT.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Reading makt data'
       EXCEPTIONS
            OTHERS = 1.

      SELECT SINGLE * FROM MAKT
        WHERE MATNR = SUM_TABLE-MATNR  "Get Description
          AND SPRAS = SY-LANGU.
      IF SY-SUBRC = '0'.
        FORMAT COLOR 6 INVERSE ON.
        WRITE: / SUM_TABLE-MATNR UNDER TEXT-009,  "Material
                 MAKT-MAKTX UNDER TEXT-010.       "Material Desc
        FORMAT RESET.
        FLAGPRT = 'yes'.
      ENDIF.
    ENDAT.
    CLEAR COUNTER.
    LOOP AT BIG_TABLE
       WHERE WERKS IN SWERKS
         AND MATKL = SUM_TABLE-MATKL
         AND MATNR = SUM_TABLE-MATNR.
      COUNTER = COUNTER + 1.
      LIFNRDESC = TEXT-005.

      SELECT SINGLE * FROM LFA1        "Get Vendor Description
         WHERE LIFNR = BIG_TABLE-LIFNR.
      IF SY-SUBRC = 0.
        LIFNRDESC = LFA1-NAME1.
      ENDIF.
      IF FLAGPRT = 'yes'.
        WRITE BIG_TABLE-MEANS1 TO TMPMEANS1.
        WRITE BIG_TABLE-MEANS2 TO TMPMEANS2.
        WRITE BIG_TABLE-MEANS3 TO TMPMEANS3.
        FORMAT COLOR 2.

        WRITE: / BIG_TABLE-LIFNR UNDER TEXT-011,          "Vendor
                 LIFNRDESC       UNDER TEXT-012,          "Vendor Name
              88 TMPMEANS1, 104 TMPMEANS2, 120 TMPMEANS3. "Mean Delivery
        WRITE: 30 SYM_RIGHT_HAND AS SYMBOL.
        CLEAR: TMPMEANS1, TMPMEANS2, TMPMEANS3.
        FORMAT RESET.
        PERFORM COUNT_PURCHASE_ORDERS.

      ENDIF.
    ENDLOOP.                           "End of BIG_TABLE loop

  ENDLOOP.                             "End of SUM_TABLE loop
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  count_purchase_orders
*&---------------------------------------------------------------------*
*       We are counting purchase orders & receipts                     *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COUNT_PURCHASE_ORDERS.
  CLEAR: EKPO, EKKO, MSEG.
* Find purchase orders

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Reading ekpo data'
       EXCEPTIONS
            OTHERS = 1.

  SELECT * FROM EKPO WHERE
               LOEKZ NE 'L' AND              " Not deleted
               MATNR EQ BIG_TABLE-MATNR AND
               MATKL EQ BIG_TABLE-MATKL AND
               WERKS IN SWERKS.
    CLEAR FLAG.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Reading ekko data'
       EXCEPTIONS
            OTHERS = 1.

    SELECT SINGLE * FROM EKKO WHERE    "Check Vendor #
                             EBELN EQ EKPO-EBELN.
    CHECK EKKO-LIFNR EQ BIG_TABLE-LIFNR.

    SELECT SINGLE * FROM EBAN WHERE    "Check purchase order date
                   EBELN EQ EKPO-EBELN AND
                   MATNR EQ EKPO-MATNR AND
                   WERKS EQ EKPO-WERKS.
    MOVE EBAN-LFDAT+0(6) TO DEL_DATE.
    CHECK DEL_DATE IN SSPMON.
    IF     EBAN-LFDAT+0(6) GE PERIOD1-LOW  AND
           EBAN-LFDAT+0(6) LE PERIOD1-HIGH.
           WPERIOD = '1'.
    ELSEIF EBAN-LFDAT+0(6) GE PERIOD2-LOW  AND
           EBAN-LFDAT+0(6) LE PERIOD2-HIGH.
           WPERIOD = '2'.
    ELSE.
           WPERIOD = '3'.
    ENDIF.

    SELECT * FROM MSEG  WHERE
             (  BWART EQ '101'  OR                       "Movement type
                BWART EQ '102' ) AND
                MATNR EQ EKPO-MATNR AND
                WERKS EQ EKPO-WERKS      AND
                LIFNR EQ BIG_TABLE-LIFNR AND
                EBELN EQ EKKO-EBELN.                      " P.O. number
      CLEAR: COUNTAB1, COUNTAB2.

      IF MSEG-BWART = '101'.
        MOVE-CORRESPONDING MSEG TO COUNTAB1.
        MOVE WPERIOD TO COUNTAB1-PERIOD.
        APPEND COUNTAB1.
      ELSE.
        MOVE-CORRESPONDING MSEG TO COUNTAB2.
        MOVE WPERIOD TO COUNTAB2-PERIOD.
        APPEND COUNTAB2.
      ENDIF.
    ENDSELECT.

    LOOP AT COUNTAB1.
      COUNTAB1-MBLNR = COUNTAB1-MBLNR + 1.
      CLEAR COUNTAB2.
      READ TABLE COUNTAB2 WITH KEY
                             MBLNR = COUNTAB1-MBLNR
                             MJAHR = COUNTAB1-MJAHR
                             ZEILE = COUNTAB1-ZEILE.
      IF SY-SUBRC <> 0 AND FLAG = 0.
        CASE COUNTAB1-PERIOD.
          WHEN '1'.
               COUNTER21 = COUNTER21 + 1.
          WHEN '2'.
               COUNTER22 = COUNTER22 + 1.
          WHEN '3'.
               COUNTER23 = COUNTER23 + 1.
        ENDCASE.
*       counter3 = counter3 + 1.       " This is special case,
        FLAG = 1.                      " Just one receipt and one
                                       " order.
      ELSE.
        IF COUNTAB2-MENGE <> COUNTAB1-MENGE.
*         counter3 = counter3 + 1.     "Number of receippts
          IF FLAG = 0.
             CASE COUNTAB1-PERIOD.
               WHEN '1'.
                    COUNTER21 = COUNTER21 + 1.
               WHEN '2'.
                    COUNTER22 = COUNTER22 + 1.
               WHEN '3'.
                    COUNTER23 = COUNTER23 + 1.
             ENDCASE.
            FLAG = 1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    REFRESH: COUNTAB1, COUNTAB2.
* we are still looping through all purchase orders !!!!!!!!!!!!!!!!
  ENDSELECT.

                                       "end select for EKPO
* Print totals for PO and Receipts
  MOVE: COUNTER21 TO COUNTPO1,           " Just because of format
        COUNTER22 TO COUNTPO2,
        COUNTER23 TO COUNTPO3.
*       counter3 to countrec.
  WRITE: 80 COUNTPO1, 96 COUNTPO2, 112 COUNTPO3.
  CLEAR: COUNTER21, COUNTER22, COUNTER23,
          COUNTPO1, COUNTPO2, COUNTPO3.
  REFRESH: COUNTAB1, COUNTAB2.
ENDFORM.                               " COUNT_PURCHASE_ORDERS
