REPORT ZMINR001 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR001
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra-Union
*    Date        :  July 4th, 1996
*    Modified    :  April 13th, 1998 by Ric Aarssen
*
* This report will display information on materials with  MRP type
* 'VB' OR 'VM' where the quantity reserved is greater than the
* (quantity on hand + quantity on order + qauntity on requisition).
************************************************************************
* CHANGE LOG
* 98/04/13 - Ric Aarssen
*          - compressed report layout removing storage location
*            assumption will be only main storage location
*          - added material group as part of the selection criteria
*          - added valuation table to get average unit price
*          - added matl group, AUP, Consumption columns
*          - added a conversion calculation for issue unit of measure
* 97/08/08 - Mary Lou DeMeester
*          - added additional selection on RESB to select only
*            reservations with RSSTA = 'M' ==> active reservations
* 03/25/97 - Lee Haire
*          - removed storage location check on reservations.
*          - originally outstanding transfers were storage location to
*            storage location transfers (MARD-UMLME).  This was
*            changed to use ONLY plant to plant transfers (MARC-UMLMC).
* 03/24/97 - Lee Haire
*          - Added check for final issue indicator on reservation
*            line item (RESB-KZEAR).
*          - removed check against storage location for quantities on
*            order and quantities on requisitions.  There is an
*            assumption that all PO's and req. will be for the main
*            storage location.
*          - added check for debit/credit indicator on received
*            quantities and reservations.
*
* 03/18/97 - Lee Haire
*          - Added check for del. ind. on PO's and Requisitions.
*          - Rearranged ordering of LVORM and DISMM in WHERE clause for
*            MARC.
*          - Rearranged ordering of LOEKZ in WHERE clause for RESB.
*          - Removed Function MATERIAL_UNIT_CONVERSION - wasn't ever
*            being called!
*          - get received quantity against the PO; Quantity on order
*            becomes PO quantity - received quantity.
*          - Should only count the quantities from those requisitions
*            for which a PO has not been issued.  Check EBAN-STATU is
*            not set to 'B'.
* 03/17/97 - Lee Haire
*          - Reserved quantity not calculated properly (RESB-XLOEK?)
*          - added parameter for storage location
*          - Return code should not be checked after SELECT * FROM MARC.
*          - Removed use of INDIC variable
************************************************************************

*****************************  TABLES   ********************************
TABLES  : RESB, EBAN, MARA, MARC, MARD, EKPO,
          EKBE,              "03/18/97 - LH
          MBEW,              "98/04/13 - RA for vmver (AUP)
          MVER.              "- RA for mgvXX (corr. mths consumption)

**************************  DATA ELEMENTS  *****************************
DATA    : QTYHAND        TYPE I,
          QTYREQUI       TYPE I,
          QTYORDER       TYPE I,
          QTYPO          TYPE I,
          QTYRECVD       TYPE I,
          QTYPO_UOM      TYPE I,  "convert Purch UOM to issue   04/14 ra
          QTYRECVD_UOM   TYPE I,  "                                   ra
          GRAND_TOTAL    TYPE I,
          QTYTRANS       TYPE I,
          TEMPBDMNG      TYPE I,
          DIFFERN        TYPE I,
          PRV_CONSUMP    LIKE MVER-MGV01, "calculate cur 12 mths consump
          CUR_CONSUMP    LIKE MVER-MGV01,                            "ra
          TTL_CONSUMP(7) TYPE P DECIMALS 0.                          "ra
*---------------------  Report Area  ----------------------1998/04/13 ra
DATA    : R_MATKL(5)       TYPE C,
          R_AUP(8)         TYPE C,
          R_CONSUMP(7)     TYPE C,
          R_QTYHAND(6)     TYPE C,
          R_QTYTRANS(6)    TYPE C,
          R_QTYORDER(6)    TYPE C,
          R_QTYREQUI(6)    TYPE C,
*          r_grand_total    type c,
          R_TEMPBDMNG(6)   TYPE C,
          R_DIFFERN(7)     TYPE C.
*---------------------- Work Area --------------------------------------
DATA    : INDIC          TYPE P,
          COUNTER        TYPE I,
          W_LGORT        LIKE MARD-LGORT VALUE 'A001',    "1998/04/13 ra
          STRIND         TYPE I,             "Starting point in table ra
          ENDIND         TYPE I,             "Ending point in table   ra
          CURYR          LIKE MVER-GJAHR,             "current year   ra
          PRVYR          LIKE MVER-GJAHR.             "previous year  ra

***********************  SELECTION SCREEN  *****************************
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-118.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_MATKL        FOR  MARA-MATKL,                      "1998/04/13 ra
     S_MATNUM       FOR  MARC-MATNR,
     S_PLANT        FOR  MARC-WERKS,
*    s_lgort        for  mard-lgort default 'A001',       "03/17/97 - LH
     S_MRPTYP       FOR  MARC-DISMM NO INTERVALS.
SELECTION-SCREEN END OF BLOCK BOX1.

******************************** MAIN **********************************
TOP-OF-PAGE.
*------------------------ print report header -------------1998/04/13 ra
WRITE: / TEXT-RPT, SY-REPID.
WRITE: / TEXT-CLT, SY-MANDT.
WRITE: / SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON,
         46 TEXT-119 COLOR COL_HEADING,
*         58 text-101 color col_heading, s_matkl-low,
*         text-102 color col_heading, s_matkl-high,
         123 'PAGE:' INTENSIFIED OFF,
         129(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE.
WRITE: /.
FORMAT COLOR COL_NORMAL.
WRITE: /1 TEXT-103, 50 TEXT-105.
WRITE: /1 TEXT-104, 12 TEXT-101,  20 TEXT-100, 30 TEXT-102,
       40 TEXT-107, 50 TEXT-106,  60 TEXT-108, 70 TEXT-121,
       80 TEXT-109, 90 TEXT-111, 100 TEXT-115,
       113 TEXT-122, 123 TEXT-117.
ULINE.
WRITE: /.

************************************************************************
START-OF-SELECTION.
* set indices for selection of consumption from mver table-1998/04/13 ra
* current & previous year moved to consumption table
* set indices to take required 12 month period
   CURYR = SY-DATUM(4).
   PRVYR = SY-DATUM(4) - 1.
   STRIND = SY-DATUM+4(2).
   IF  SY-DATUM+4(2) = '01'.            "if january, adjust end month
       ENDIND = '12'.
     ELSE.
       ENDIND = SY-DATUM+4(2) - 1.
   ENDIF.

FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
PERFORM CLEAR.
*check material group in mara.                            "1998/04/13 ra
SELECT * FROM MARA WHERE MATKL IN S_MATKL
                     AND MATNR IN S_MATNUM.

* get outstanding transfer from MARC.
  SELECT * FROM MARC WHERE MATNR = MARA-MATNR
                       AND WERKS IN S_PLANT
                       AND LVORM NE 'X'
                       AND DISMM IN S_MRPTYP
                       ORDER BY MATNR WERKS.
    QTYTRANS = QTYTRANS + MARC-UMLMC.

*-- get average unit price                                "1998/04/13 ra
    SELECT * FROM MBEW WHERE MATNR = MARC-MATNR
                                AND BWKEY = MARC-WERKS.
      MOVE MBEW-VMVER TO R_AUP.
    ENDSELECT.   "MBEW

*-- get most recent 12 month consumption                  "1998/04/13 ra
    SELECT * FROM MVER WHERE MATNR = MARC-MATNR
                         AND WERKS = MARC-WERKS
                         AND GJAHR = PRVYR.
      ADD MVER-MGV01 FROM STRIND TO 12 GIVING PRV_CONSUMP.
    ENDSELECT.   "MVER  previous yrs consumption
    IF ENDIND <> 12.
      SELECT * FROM MVER WHERE MATNR = MARC-MATNR
                           AND WERKS = MARC-WERKS
                           AND GJAHR = CURYR.
        ADD MVER-MGV01 FROM 1 TO ENDIND GIVING CUR_CONSUMP.
      ENDSELECT.   "MVER  current yrs consumption
    ENDIF.
    TTL_CONSUMP = PRV_CONSUMP + CUR_CONSUMP.

*-- get quantity on hand
    SELECT * FROM MARD WHERE MATNR = MARC-MATNR
                         AND WERKS = MARC-WERKS
                         AND LGORT = W_LGORT.
      QTYHAND = QTYHAND + MARD-LABST.
    ENDSELECT.   "MARD

*-- get PO quantity
    SELECT * FROM EKPO WHERE LOEKZ = SPACE
                         AND MATNR = MARC-MATNR
                         AND WERKS = MARC-WERKS
                         AND ELIKZ <> 'X'.

*     calc quantity on order from purchase uom to unit of issue
      QTYPO_UOM = ( EKPO-MENGE * EKPO-UMREZ ) / EKPO-UMREN.
      QTYPO = QTYPO + QTYPO_UOM.

*---  get received quantity against PO
      SELECT * FROM EKBE WHERE EBELN = EKPO-EBELN
                           AND EBELP = EKPO-EBELP
                           AND BEWTP = 'E'
                           AND VGABE = '1'.
*       convert receipt from purchase uom to unit of issue
        QTYRECVD_UOM = ( EKBE-MENGE * EKPO-UMREZ ) / EKPO-UMREN.
        CASE EKBE-SHKZG.
          WHEN 'S'.                    "debit
            QTYRECVD = QTYRECVD + QTYRECVD_UOM.
          WHEN 'H'.                    "credit
            QTYRECVD = QTYRECVD - QTYRECVD_UOM.
        ENDCASE.
      ENDSELECT.   "EKBE
    ENDSELECT.   "EKPO
    QTYORDER = QTYPO - QTYRECVD.
*-- check for negative receipts normally caused by blanket PO's       ra
    IF QTYORDER < 0.
       MOVE 0 TO QTYORDER.
    ENDIF.
*-- get purchase requisitions quantities
    SELECT * FROM EBAN WHERE LOEKZ = SPACE
                         AND STATU <> 'B'
                         AND MATNR = MARC-MATNR
                         AND WERKS = MARC-WERKS.
      QTYREQUI = QTYREQUI + EBAN-MENGE.
    ENDSELECT.   "EBAN

*-- get reservation quantities
    SELECT * FROM RESB WHERE XLOEK = SPACE
                         AND KZEAR = SPACE
                         AND MATNR = MARC-MATNR
                         AND WERKS = MARC-WERKS
                         AND RSSTA = 'M'.           "active reservations
*                           AND LGORT = MARD-LGORT.       "03/25/97 - LH
      CASE RESB-SHKZG.
        WHEN 'S'.                      "debit
          TEMPBDMNG = TEMPBDMNG - RESB-BDMNG.
        WHEN 'H'.                      "credit
          TEMPBDMNG = TEMPBDMNG + RESB-BDMNG.
      ENDCASE.
    ENDSELECT.   "RESB
    GRAND_TOTAL = QTYHAND + QTYORDER + QTYREQUI + QTYTRANS.
    DIFFERN = GRAND_TOTAL - TEMPBDMNG.

*-- when a shortfall exists write out to report
    IF TEMPBDMNG > GRAND_TOTAL.
      MOVE TTL_CONSUMP TO R_CONSUMP.
      MOVE QTYHAND     TO R_QTYHAND.
      MOVE QTYTRANS    TO R_QTYTRANS.
      MOVE QTYORDER    TO R_QTYORDER.
      MOVE QTYREQUI    TO R_QTYREQUI.
      MOVE TEMPBDMNG   TO R_TEMPBDMNG.
      MOVE DIFFERN     TO R_DIFFERN.
      WRITE:/ MARD-MATNR+12(6) COLOR COL_NEGATIVE INTENSIFIED OFF,
              MARA-MATKL UNDER TEXT-101,                             "ra
              MARC-WERKS UNDER TEXT-100,
              R_AUP      UNDER TEXT-102,                             "ra
              R_CONSUMP  UNDER TEXT-107,                             "ra
              MARC-DISMM UNDER TEXT-106,
              R_QTYHAND UNDER TEXT-108,  R_QTYTRANS UNDER TEXT-121,
              R_QTYORDER UNDER TEXT-109, R_QTYREQUI UNDER TEXT-111,
              GRAND_TOTAL UNDER TEXT-115 COLOR COL_KEY,
              R_TEMPBDMNG UNDER TEXT-122,
              R_DIFFERN UNDER TEXT-117 COLOR COL_TOTAL.
    ENDIF.
    PERFORM CLEAR.
  ENDSELECT.      "MARC
ENDSELECT.      "MARA

***************************** SUBROUTINES ******************************
FORM CLEAR.
  CLEAR: QTYHAND, QTYTRANS, QTYREQUI, QTYORDER, DIFFERN, TEMPBDMNG.
  CLEAR: QTYRECVD, QTYPO.
  CLEAR: PRV_CONSUMP, CUR_CONSUMP, TTL_CONSUMP.           "1998/04/13 ra
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_RET_CODE                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CHECK_RET_CODE.
  IF SY-SUBRC NE 0.
    INDIC = 1.
  ENDIF.
ENDFORM.
