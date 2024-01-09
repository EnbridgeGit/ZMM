REPORT ZMMMR038 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132
                MESSAGE-ID ZM.
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        August 1997
*  Description:
*     - The purpose of this program is to produce the
*       Delivery/Lead Time Summary by Plant/Material Group
************************************************************************
*                                                                      *
*    -- OBSOLETE -- OBSOLETE -- OBSOLETE -- OBSOLETE -- OBSOLETE --    *
*                                                                      *
************************************************************************
* 2009/09/30 mdemeest - OBSOLETE - per D. Prizaio.- replaced by
*                                  ZMMMR058
* 2000/05/24 gyamna   #583 added new Info Rec Lead time column and
*                          server ident field in the heading. Disabled
*                          printing of the Receipts column.
* 1997/11/13        #244   Two new fields added: Number of Orders and
*                          Receipts - changes made by Nesh N. Laurencic
* 1997/08/15 mdemeest      Development Request DRMM0191 for M. Dufault

* 1997/09/17 mdemeest      Fix DIFF by eliminating items with no
*                          material #
*                          Column headings on subsequent pages
*
************************************************************************

TABLES: LFA1,                          "Vendor master (general section)
        MARC,                          "Material Master: C Segment
        MAKT,                          "Material Descriptions
        S012,                          "PURCHIS: Purchasing Statistics
       T001W,                          "Plants/Locations
       T023T,                          "Material Group Descriptions
        EKPO,                          "Purchasing Document Item
        EKKO,                          "Purchasing Document Header
        MSEG,                          "Document Segment: Material
        EINA,                          "Purchasing Info Record: Gen Data
        EINE,                          "Purchasing Info Record: Org Data
        trdirt.

INCLUDE <SYMBOL>.
DATA:
    BEGIN OF SAVE_TABLE OCCURS 10000,
       WERKS        LIKE S012-WERKS,   "Plant
       MATKL        LIKE S012-MATKL,   "Material Group
       MATNR        LIKE S012-MATNR,   "Material
       LIFNR        LIKE S012-LIFNR,   "Vendor
       ALIEF        LIKE S012-ALIEF,   "No of Deliveries
       LFZTA        LIKE S012-LFZTA,   "Amount
       MEANS        TYPE I,
   END OF SAVE_TABLE.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
       WERKS        LIKE S012-WERKS,   "Plant
       MATKL        LIKE S012-MATKL,   "Material Group
       MATNR        LIKE S012-MATNR,   "Material
       LIFNR        LIKE S012-LIFNR,   "Vendor
       SPMON        LIKE S012-SPMON,   "Date
       ALIEF        LIKE S012-ALIEF,   "No of Deliveries
       LFZTA        LIKE S012-LFZTA,   "Amount
       MEANS        TYPE I,            "Mean Delivery
       MAKTX        LIKE MAKT-MAKTX,   "Description
   END OF BIG_TABLE.

DATA:
    BEGIN OF SUM_TABLE OCCURS 10000,
       WERKS        LIKE S012-WERKS,   "Plant
       MATKL        LIKE S012-MATKL,   "Material Group
       MATNR        LIKE S012-MATNR,   "Material
       ALIEF        LIKE S012-ALIEF,   "No of Deliveries
       LFZTA        LIKE S012-LFZTA,   "Amount
       MEANS        TYPE I,            "Mean Delivery
       PLIFZ        LIKE MARC-PLIFZ,   "Lead Time
       APLFZ        LIKE EINE-APLFZ,   "I/R Lead Time
       DIFF         TYPE I,
       ABSDIFF      TYPE I,
       MAKTX        LIKE MAKT-MAKTX,   "Description
   END OF SUM_TABLE.



DATA:  LIFNRDESC     LIKE LFA1-NAME1.
DATA:  TEMPWERKS     LIKE S012-WERKS.
DATA:  TEMPMATKL     LIKE S012-MATKL.
DATA:  TEMPMATNR     LIKE S012-MATNR.
DATA:  TEMPLIFNR     LIKE S012-LIFNR.
DATA:  TEMPAPLFZ     LIKE EINE-APLFZ.
DATA:  LIFNRLFZTA    LIKE S012-LFZTA.
DATA:  LIFNRALIEF    LIKE S012-ALIEF.
DATA:  MATNRLFZTA    LIKE S012-LFZTA.
DATA:  MATNRALIEF    LIKE S012-ALIEF.
DATA:  COUNTER       TYPE I.
DATA:  FLAGPRT(3)    TYPE C.

DATA:  COUNTER2 TYPE I.                "         # po
DATA:  COUNTER3 TYPE I.                "         # receipts

* Two tables for counting purposses.
DATA: BEGIN OF COUNTAB1 OCCURS 50,
       MBLNR LIKE MSEG-MBLNR,
       MJAHR LIKE MSEG-MJAHR,
       ZEILE LIKE MSEG-ZEILE,          "Item in material document
       BWART LIKE MSEG-BWART,
       MENGE LIKE MSEG-MENGE.          "Quantity
DATA: END OF COUNTAB1.

DATA: BEGIN OF COUNTAB2 OCCURS 50.
        INCLUDE STRUCTURE COUNTAB1.
DATA: END OF COUNTAB2.

DATA:  FLAG TYPE C VALUE 0.            " Flag

* Following is just for output.
DATA:  COUNTPO(4)   TYPE C,            "# of po
       COUNTREC(4)  TYPE C,            "# of receipts
       TMPMEANS(4)  TYPE C,
       TMPDIFF(4)   TYPE C,
       TMPPLIFZ(4)  TYPE C,
       TMPAPLFZ(4)  TYPE C.

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS:  SWERKS FOR S012-WERKS OBLIGATORY,      "Plant
                 SMATKL FOR S012-MATKL OBLIGATORY,      "Material Group
                 SSPMON FOR S012-SPMON OBLIGATORY,      "Selected Period
                 SMATNR FOR S012-MATNR."Material Number
PARAMETERS:      P_DAYS TYPE I.
SELECTION-SCREEN END OF BLOCK BOX.

*---------------------  TOP-OF-PAGE  -----------------------------------
TOP-OF-PAGE.
  WRITE: /35 trdirt-text+4(66).        "OBSOLETE COMMENT
  skip 2.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         50 TEXT-TTL COLOR 4 INTENSIFIED ON,
        100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.

  WRITE: /1 TEXT-CLT, SY-MANDT, SY-SYSID.
  IF SSPMON+9(6) CO '0'.
    WRITE: TEXT-004 UNDER TEXT-TTL, SSPMON+3(6) COLOR 4 INTENSIFIED ON.
  ELSE.
    WRITE: TEXT-002 UNDER TEXT-TTL, SSPMON+3(6) COLOR 4 INTENSIFIED ON,
           TEXT-003, SSPMON+9(6) COLOR 4 INTENSIFIED ON.
  ENDIF.
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.

  WRITE: /.

  SELECT SINGLE * FROM T023T
     WHERE MATKL = SUM_TABLE-MATKL
       AND SPRAS = SY-LANGU.
  WRITE: / TEXT-006 INTENSIFIED OFF, SUM_TABLE-WERKS COLOR COL_KEY,
           T001W-NAME1 COLOR COL_KEY.  "Plant info
  WRITE: / TEXT-007 INTENSIFIED OFF, SUM_TABLE-MATKL COLOR COL_KEY,
           T023T-WGBEZ COLOR COL_KEY.  "Material info
  ULINE.
  WRITE: /1 TEXT-009,  10 TEXT-010,  35 TEXT-011, 46 TEXT-012,
         80 TEXT-200,  88 TEXT-013,  98 TEXT-015,
        108 TEXT-017, 118 TEXT-016.
*        80 text-200,  87 text-201,
*        97 text-013, 107 text-015, 117 text-017, 127 text-016.
  WRITE: / TEXT-014 UNDER TEXT-013, TEXT-018 UNDER TEXT-015,
           TEXT-019 UNDER TEXT-017.
  ULINE.

*-------------------  START-OF-SELECTION  ------------------------------
* select all records from S012 table that satisfy selection criteria
*-----------------------------------------------------------------------
START-OF-SELECTION.

  select single * from TRDIRT
     where   name = 'ZMMMR038'
      and   sprsl = sy-langu.

  SELECT * FROM S012
     WHERE WERKS IN SWERKS             "Plant
       AND MATKL IN SMATKL             "Material Group
      AND SPMON IN SSPMON              "Select Period (yyyy/mm)
       AND MATNR IN SMATNR             "Material
       AND ALIEF > 0
       AND MATNR NE SPACE.             "Eliminate NIS items

    PERFORM BUILD_SAVE_TABLE.

  ENDSELECT.

  PERFORM BUILD_BIG_TABLE.

  PERFORM DISPLAY_TABLE.

  WRITE: /.
  WRITE: / TEXT-028 UNDER TEXT-TTL.    "End of Report

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*-----------------------  BUILD_BIG_TABLE  -----------------------------
*  This subroutine moves all data to BIG_TABLE.
*-----------------------------------------------------------------------

FORM BUILD_SAVE_TABLE.
  CLEAR SAVE_TABLE.
  MOVE S012-WERKS     TO SAVE_TABLE-WERKS.             "Plant
  MOVE S012-MATKL     TO SAVE_TABLE-MATKL.             "Material Group
  MOVE S012-MATNR     TO SAVE_TABLE-MATNR.             "Material
  MOVE S012-LIFNR     TO SAVE_TABLE-LIFNR.             "Vendor
  MOVE S012-ALIEF     TO SAVE_TABLE-ALIEF.             "No of Deliveries
  MOVE S012-LFZTA     TO SAVE_TABLE-LFZTA.             "Amount.
  APPEND SAVE_TABLE.
* write: / 'save', save_table-werks, save_table-matkl,
*                  save_table-matnr, save_table-lifnr,
*                  save_table-lfzta, save_table-alief.

ENDFORM.
*-----------------------  BUILD_BIG_TABLE  -----------------------------
*  This subroutine moves all data to BIG_TABLE.
*-----------------------------------------------------------------------

FORM BUILD_BIG_TABLE.
  SORT SAVE_TABLE BY WERKS MATKL MATNR LIFNR.
  LOOP AT SAVE_TABLE.
    AT NEW WERKS.
      TEMPWERKS = SAVE_TABLE-WERKS.
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

    LIFNRLFZTA = LIFNRLFZTA + SAVE_TABLE-LFZTA.
    LIFNRALIEF = LIFNRALIEF + SAVE_TABLE-ALIEF.

    MATNRLFZTA = MATNRLFZTA + SAVE_TABLE-LFZTA.
    MATNRALIEF = MATNRALIEF + SAVE_TABLE-ALIEF.

    AT END OF LIFNR.
      BIG_TABLE-WERKS = TEMPWERKS.
      BIG_TABLE-MATKL = TEMPMATKL.
      BIG_TABLE-MATNR = TEMPMATNR.
      BIG_TABLE-LIFNR = TEMPLIFNR.
      BIG_TABLE-LFZTA = LIFNRLFZTA.
      BIG_TABLE-ALIEF = LIFNRALIEF.
      BIG_TABLE-MEANS = LIFNRLFZTA DIV LIFNRALIEF.
      APPEND BIG_TABLE.
      CLEAR: LIFNRLFZTA, LIFNRALIEF.
      CLEAR BIG_TABLE.
    ENDAT.

    AT END OF MATNR.
      SELECT SINGLE * FROM MARC        "Get Lead Time from MARC
          WHERE WERKS = TEMPWERKS
            AND MATNR = TEMPMATNR.
      IF  SY-SUBRC = 0.
        SUM_TABLE-PLIFZ = MARC-PLIFZ.
      ENDIF.

      CLEAR TEMPAPLFZ.
      SELECT * FROM EINA        "Get I/R Lead Time from EINE
       WHERE MATNR = TEMPMATNR
         AND LIFNR = TEMPLIFNR.
         SELECT * FROM EINE
          WHERE INFNR = EINA-INFNR
            AND LOEKZ NE 'X'.
         IF  SY-SUBRC = 0.
             SUM_TABLE-APLFZ = TEMPAPLFZ + EINE-APLFZ.
         ENDIF.
         ENDSELECT.
      ENDSELECT.

      SELECT SINGLE * FROM MAKT
        WHERE MATNR = TEMPMATNR        "Get Description
          AND SPRAS = SY-LANGU.
      SUM_TABLE-MAKTX = MAKT-MAKTX.
      IF SY-SUBRC = '0'.

        SUM_TABLE-WERKS = TEMPWERKS.
        SUM_TABLE-MATKL = TEMPMATKL.
        SUM_TABLE-MATNR = TEMPMATNR.
        SUM_TABLE-LFZTA = MATNRLFZTA.
        SUM_TABLE-ALIEF = MATNRALIEF.
        SUM_TABLE-MEANS = MATNRLFZTA DIV MATNRALIEF.
        SUM_TABLE-DIFF = SUM_TABLE-MEANS - SUM_TABLE-PLIFZ.
        SUM_TABLE-ABSDIFF = ABS( SUM_TABLE-DIFF ).
        APPEND SUM_TABLE.
        CLEAR: MATNRLFZTA, MATNRALIEF.
        CLEAR SUM_TABLE.
      ENDIF.
    ENDAT.

  ENDLOOP.
ENDFORM.
*----------------------  DISPLAY_TABLE  --------------------------------
*  This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.
  SORT SUM_TABLE BY WERKS MATKL ABSDIFF DESCENDING MATNR.
  LOOP AT SUM_TABLE.

    AT NEW WERKS.
      SELECT SINGLE * FROM T001W
         WHERE WERKS = SUM_TABLE-WERKS.
    ENDAT.

    IF  SUM_TABLE-ABSDIFF < P_DAYS.
      CONTINUE.
    ENDIF.                             "Limits report

    AT NEW MATKL.
      NEW-PAGE.
    ENDAT.

    AT NEW ABSDIFF.
    ENDAT.

    AT NEW MATNR.
      RESERVE 2 LINES.
      CLEAR FLAGPRT.
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
       WHERE WERKS = SUM_TABLE-WERKS
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
        WRITE BIG_TABLE-MEANS TO TMPMEANS.
        FORMAT COLOR 2.

        WRITE: / BIG_TABLE-LIFNR UNDER TEXT-011,        "Vendor
                 LIFNRDESC       UNDER TEXT-012,        "Vendor Name
                 TMPMEANS        UNDER TEXT-013.        "Mean Delivery
*                big_table-means under text-013.        "Mean Delivery
        WRITE: 30 SYM_RIGHT_HAND AS SYMBOL.
        CLEAR TMPMEANS.
        FORMAT RESET.
* Added routine   -  Nesh N. Laurencic/ Omnilogic Systems Group
        PERFORM COUNT_PURCHASE_ORDERS.

      ENDIF.
    ENDLOOP.                           "End of BIG_TABLE loop

    IF COUNTER > 1.
      WRITE SUM_TABLE-MEANS TO TMPMEANS.
      WRITE: / TEXT-001 UNDER  TEXT-012 COLOR COL_TOTAL,    "WEIGHTED
               TMPMEANS UNDER TEXT-013.
*               sum_table-means under text-013.
    ENDIF.
    FORMAT COLOR 2.
    WRITE:  SUM_TABLE-PLIFZ TO TMPPLIFZ.
    WRITE:  TMPPLIFZ UNDER TEXT-015.   "Lead Time
*   write:  sum_table-plifz under text-015.              "Lead Time

    WRITE:  SUM_TABLE-APLFZ TO TMPAPLFZ.
    WRITE:  TMPAPLFZ UNDER TEXT-017.   "I/R Lead Time

    WRITE SUM_TABLE-DIFF TO TMPDIFF.
    IF  SUM_TABLE-DIFF < 0.
      WRITE: TMPDIFF UNDER TEXT-016
                        COLOR COL_NEGATIVE INVERSE ON.
    ELSE.
      WRITE:  TMPDIFF  UNDER TEXT-016. "Difference
    ENDIF.
    FORMAT RESET.

*    IF  SUM_TABLE-DIFF < 0.
*      WRITE: SUM_TABLE-DIFF UNDER TEXT-016
*                      COLOR COL_NEGATIVE INVERSE ON.
*    ELSE.
*      WRITE:  SUM_TABLE-DIFF  UNDER TEXT-016.              "Difference
*    ENDIF.

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
  SELECT * FROM EKPO WHERE
                           LOEKZ NE 'L' AND              " Not deleted
                           MATNR EQ BIG_TABLE-MATNR AND
                           WERKS EQ BIG_TABLE-WERKS.
    CLEAR FLAG.
    SELECT SINGLE * FROM EKKO WHERE    "Check Vendor #
                             EBELN EQ EKPO-EBELN.
    CHECK EKKO-LIFNR EQ BIG_TABLE-LIFNR.

* We will count order only if it's not open order
* That means: It must exist at least one receipt.

    SELECT * FROM MSEG  WHERE
                                  (  BWART EQ '101'  OR   "Movement type
                                  BWART EQ '102' ) AND
                                  MATNR EQ BIG_TABLE-MATNR AND
                                  WERKS EQ BIG_TABLE-WERKS AND
                                  LIFNR EQ BIG_TABLE-LIFNR AND
                                  EBELN EQ EKKO-EBELN.    " P.O. number
      CLEAR: COUNTAB1, COUNTAB2.
* Find all material documents (receipts) , and movement 102
* and store them in 2 diff. internal tables.
* Ask functional guys about 102 movement type.
* When they create document 102, it's always document number for
* corresponding document with movement type 101 but icreased for 1.
* Example:  101  - doc no. 5000000000
*           102  - doc no. 5000000001
* This means that 102 refers to 101. That sometimes means cuncellation,
* so we have to check if the quantities match each other.
      IF MSEG-BWART = '101'.
        MOVE-CORRESPONDING MSEG TO COUNTAB1.
        APPEND COUNTAB1.
      ELSE.
        MOVE-CORRESPONDING MSEG TO COUNTAB2.
        APPEND COUNTAB2.
      ENDIF.
    ENDSELECT.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.
* Dont be confused, this is PO specific. Purchase order can be valid or
* no valid, we are going to count receipts using the same routine.
    LOOP AT COUNTAB1.
      COUNTAB1-MBLNR = COUNTAB1-MBLNR + 1.
      CLEAR COUNTAB2.
      READ TABLE COUNTAB2 WITH KEY
                             MBLNR = COUNTAB1-MBLNR
                             MJAHR = COUNTAB1-MJAHR
                             ZEILE = COUNTAB1-ZEILE.
      IF SY-SUBRC <> 0 AND FLAG = 0.
        COUNTER2 = COUNTER2 + 1.
        COUNTER3 = COUNTER3 + 1.       " This is special case,
        FLAG = 1.                      " Just one receipt and one
                                       " order.
      ELSE.
        IF COUNTAB2-MENGE <> COUNTAB1-MENGE.
          COUNTER3 = COUNTER3 + 1.     "Number of receippts
          IF FLAG = 0.
            COUNTER2 = COUNTER2 + 1.
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
  MOVE: COUNTER2 TO COUNTPO,           " Just because of format
        COUNTER3 TO COUNTREC.                               "
  WRITE: COUNTPO UNDER  TEXT-200.
* write: countpo under  text-200,
*        countrec under text-201.
  CLEAR: COUNTER2, COUNTER3, COUNTPO, COUNTREC.
  REFRESH: COUNTAB1, COUNTAB2.
ENDFORM.                               " COUNT_PURCHASE_ORDERS
