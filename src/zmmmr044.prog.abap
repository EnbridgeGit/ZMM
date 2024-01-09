REPORT ZMMMR044 NO STANDARD PAGE HEADING LINE-SIZE 158 LINE-COUNT 58
       MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMR044
*    Programmer  :  Nancy Gilligan - OmniLogic
*    Date        :  November, 1998
*
* This ABAP produces a report to assist the buyers in reviewing price
* increases for materials that have been purchased and their effect
* on the bottom line.  Based on this report, the buyer will be looking
* for unauthorized price increases and if required, will contact the
* vendor for justification.
*
* This report was requested by Dave Lambert under DRMM0261, issue log
* number 565.
************************************************************************

************************************************************************
*****************************  DATA  ***********************************
TABLES: EKKO,                    "Purchasing Document Header
        EKPO,                    "Purchasing Document Item
        EINE,                    "Purchasing Info - Pur. Org. data
        EINA,                    "Purchasing Info - General data
        MARA,                    "Material Master
        KONP,                    "Conditions (Item)
        A018,                    "Material Info Record
        MAKT,                    "Material Descriptions
        T023T,                   "Material Groups
        T024,                    "Purchasing Groups
        LFA1.                    "Vendor master (general section)

DATA   : BEGIN OF IT_PRICE OCCURS 10000,
           EKGRP   LIKE EINE-EKGRP,
           MATKL   LIKE MARA-MATKL,
           LIFNR   LIKE EINA-LIFNR,
           MATNR   LIKE MARA-MATNR,
           MAKTX   LIKE MAKT-MAKTX,
           MEINS   LIKE EINA-MEINS,
           QTY     LIKE EKPO-MENGE,
           NETWR   LIKE EKPO-NETWR,
           BEGIN   LIKE KONP-KBETR,
           CURRENT LIKE KONP-KBETR,
         END OF IT_PRICE.

DATA:   NO_DATA TYPE I VALUE 0,
        CHANGE(4) TYPE P DECIMALS 1,
        AVERAGE TYPE P DECIMALS 2,
        EFFECT TYPE P DECIMALS 2,
        GO_AHEAD(3) VALUE 'no',
        PAGE_TOTAL TYPE P DECIMALS 2,
        EKGRP1  LIKE EINE-EKGRP,
        MATKL1  LIKE MARA-MATKL,
        LIFNR1  LIKE EINA-LIFNR.


************************************************************************
***********************  SELECTION SCREEN  *****************************
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:   S_EKGRP     FOR   EINE-EKGRP OBLIGATORY.
SELECT-OPTIONS:   S_MATKL     FOR   MARA-MATKL.
SELECT-OPTIONS:   S_LIFNR     FOR   EINA-LIFNR MATCHCODE OBJECT KRED.
SELECT-OPTIONS:   S_MATNR     FOR   MARA-MATNR.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
PARAMETERS:     P_DATE1        LIKE   MKPF-BUDAT OBLIGATORY,
                P_DATE2        LIKE   MKPF-BUDAT OBLIGATORY
                                   DEFAULT SY-DATUM.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK BOX1.

************************************************************************
*************************  TOP OF PAGE  ********************************
TOP-OF-PAGE.
* standard page header
FORMAT INTENSIFIED ON.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,
        131 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,  SY-SYSID.

  WRITE: 45 TEXT-TTL.
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO,
         /52 P_DATE1, TEXT-DSH, P_DATE2.
       SKIP 1.
  ULINE.

PERFORM WRITE_TITLES.

FORMAT INTENSIFIED OFF.

************************************************************************
*************************  INITIALIZATION  *****************************
INITIALIZATION.
CONCATENATE SY-DATUM(4) '0101' INTO P_DATE1.

************************************************************************
************************  AT SELECTION SCREEN  *************************
AT SELECTION-SCREEN.
IF  S_MATKL IS INITIAL AND S_LIFNR IS INITIAL AND S_MATNR IS INITIAL.
  MESSAGE E060 WITH TEXT-001 TEXT-002.
ENDIF.

************************************************************************
*******************************  MAIN  *********************************

START-OF-SELECTION.
clear: it_price.
refresh: it_price.

PERFORM GET_DATA.
PERFORM WRITE_DATA.

IF NO_DATA = 0.
  SKIP 3.
  WRITE: /60 TEXT-050.
ENDIF.


************************************************************************
**************************  SUBROUTINES  *******************************

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.

SELECT * FROM EINA WHERE MATNR IN S_MATNR
                     AND LIFNR IN S_LIFNR.
  SELECT * FROM EINE WHERE INFNR = EINA-INFNR
                       AND EKGRP IN S_EKGRP.
    SELECT SINGLE * FROM MARA WHERE MATNR = EINA-MATNR
                                AND MATKL IN S_MATKL.
* get description of material
    IF SY-SUBRC = 0.
      SELECT SINGLE MAKTX FROM MAKT INTO MAKT-MAKTX
                                WHERE SPRAS = SY-LANGU
                                  AND MATNR = MARA-MATNR.

* store data values
         IT_PRICE-EKGRP = EINE-EKGRP.
         IT_PRICE-MATKL = MARA-MATKL.
         IT_PRICE-LIFNR = EINA-LIFNR.
         IT_PRICE-MATNR = MARA-MATNR.
         IT_PRICE-MAKTX = MAKT-MAKTX.
         IT_PRICE-MEINS = EINA-MEINS.
    ENDIF.                                      "end subrc

* for each material #: (get order details)
    IF SY-SUBRC = 0.
      SELECT * FROM EKKO WHERE LIFNR = EINA-LIFNR
                                  AND BEDAT BETWEEN P_DATE1 AND P_DATE2.

        SELECT * FROM EKPO WHERE EBELN = EKKO-EBELN
                             AND MATNR = MARA-MATNR.
*                            and loekz ne 'L'.
        IF SY-SUBRC = 0.
          IT_PRICE-QTY = IT_PRICE-QTY + EKPO-MENGE.
          IT_PRICE-NETWR = IT_PRICE-NETWR + EKPO-NETWR.
          GO_AHEAD = 'yes'.
        ENDIF.
        ENDSELECT.            "EKPO
      ENDSELECT.              "EKKO

* get price info
    IF GO_AHEAD = 'yes'.
      SELECT * FROM A018 WHERE KAPPL = 'M'
                           AND KSCHL = 'ZZZZ'
                           AND LIFNR = EINA-LIFNR
                           AND MATNR = MARA-MATNR
                           AND EKORG = EINE-EKORG
                           AND ESOKZ = EINE-ESOKZ
                           AND ( ( DATAB < P_DATE1 AND DATBI > P_DATE1 )
                           OR ( DATAB < P_DATE2 AND DATBI > P_DATE2 ) ).
         SELECT SINGLE * FROM KONP WHERE KNUMH = A018-KNUMH.

* store data values
         IF P_DATE1 BETWEEN A018-DATAB AND A018-DATBI.
            IT_PRICE-BEGIN = KONP-KBETR.
         ELSEIF P_DATE2 BETWEEN A018-DATAB AND A018-DATBI.
            IT_PRICE-CURRENT = KONP-KBETR.
         ENDIF.
      ENDSELECT.                                          "select a018

* only keep if the values have changed
      IF IT_PRICE-BEGIN <> IT_PRICE-CURRENT AND GO_AHEAD = 'yes'.
        APPEND IT_PRICE.
        GO_AHEAD = 'no'.
      ENDIF.
    ENDIF.
   ENDIF.

  ENDSELECT.   "EINE
ENDSELECT.  "EINA

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA
*&---------------------------------------------------------------------*
FORM WRITE_DATA.

SORT IT_PRICE BY EKGRP MATKL LIFNR MATNR.

loop at it_price.
  NO_DATA = 1.
* get calculated values
  PERFORM GET_CALCS.
  FORMAT INTENSIFIED OFF.


WRITE: /   IT_PRICE-MATNR UNDER TEXT-020,
           IT_PRICE-MAKTX UNDER TEXT-021,
           IT_PRICE-MEINS UNDER TEXT-022,
           IT_PRICE-QTY UNDER TEXT-023,
           IT_PRICE-BEGIN UNDER TEXT-025,
           IT_PRICE-CURRENT UNDER TEXT-027,
           CHANGE UNDER TEXT-028,
           AVERAGE UNDER TEXT-029,
           EFFECT UNDER TEXT-031.

PAGE_TOTAL = EFFECT + PAGE_TOTAL.

* break the page on change
MATKL1 = IT_PRICE-MATKL.
LIFNR1 = IT_PRICE-LIFNR.
EKGRP1 = IT_PRICE-EKGRP.
AT END OF MATKL.
  IT_PRICE-LIFNR = LIFNR1.
  PERFORM PAGE_TOT.
  NEW-PAGE.
ENDAT.

AT END OF LIFNR.
  PERFORM PAGE_TOT.
  NEW-PAGE.
ENDAT.

AT END OF EKGRP.
  IT_PRICE-MATKL = MATKL1.
  IT_PRICE-LIFNR = LIFNR1.
  PERFORM PAGE_TOT.
  NEW-PAGE.
ENDAT.

ENDLOOP.

ENDFORM.                    " WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_TITLES
*&---------------------------------------------------------------------*
FORM WRITE_TITLES.

IF NO_DATA = 0.
   PERFORM WRITE_SELECTIONS.
ELSE.
   PERFORM GET_DESCRIPTIONS.
   PERFORM WRITE_INFO.
ENDIF.

* report titles
ULINE.
*  top line
WRITE: 65 TEXT-023,
       80 TEXT-025,
       95 TEXT-027,
       125 TEXT-029,
       142 TEXT-031.
*  second line
WRITE: /   TEXT-020,                        "material
        20 TEXT-021,                        "description
        60 TEXT-022,                        "uom
           TEXT-024 UNDER TEXT-023,         "qty
           TEXT-026 UNDER TEXT-025,         "begin
           TEXT-026 UNDER TEXT-027,         "current
       114 TEXT-028,                        "change
           TEXT-030 UNDER TEXT-029,         "average
           TEXT-032 UNDER TEXT-031.         "effect
SKIP.

ENDFORM.                    " WRITE_TITLES
*&---------------------------------------------------------------------*
*&      Form  GET_CALCS
*&---------------------------------------------------------------------*
FORM GET_CALCS.

IF IT_PRICE-BEGIN NE 0.
  CHANGE = ( ( IT_PRICE-CURRENT / IT_PRICE-BEGIN ) * 100 ) - 100.
ELSE.
  CHANGE = 0.
ENDIF.

IF IT_PRICE-QTY NE 0.
  AVERAGE = IT_PRICE-NETWR / IT_PRICE-QTY.
ELSE.
  AVERAGE = 0.
ENDIF.

EFFECT = IT_PRICE-NETWR - ( IT_PRICE-QTY * IT_PRICE-BEGIN ).

ENDFORM.                    " GET_CALCS
*&---------------------------------------------------------------------*
*&      Form  WRITE_INFO
*&---------------------------------------------------------------------*
FORM WRITE_INFO.

* info section
WRITE: TEXT-010, 20 IT_PRICE-EKGRP, 26 TEXT-DSH, T024-EKNAM,
      / TEXT-011, IT_PRICE-MATKL UNDER IT_PRICE-EKGRP,
        TEXT-DSH UNDER TEXT-DSH, T023T-WGBEZ UNDER T024-EKNAM,
      / TEXT-012, IT_PRICE-LIFNR UNDER IT_PRICE-EKGRP,
        TEXT-DSH UNDER TEXT-DSH, LFA1-NAME1 UNDER T024-EKNAM.

ENDFORM.                    " WRITE_INFO

*&---------------------------------------------------------------------*
*&      Form  GET_DESCRIPTIONS
*&---------------------------------------------------------------------*
FORM GET_DESCRIPTIONS.

* get descriptions
   SELECT SINGLE EKNAM FROM T024 INTO T024-EKNAM
                          WHERE EKGRP = IT_PRICE-EKGRP.
   SELECT SINGLE WGBEZ FROM T023T INTO T023T-WGBEZ
                          WHERE MATKL = IT_PRICE-MATKL
                            AND SPRAS = SY-LANGU.
   SELECT SINGLE NAME1 FROM LFA1 INTO LFA1-NAME1
                          WHERE LIFNR = IT_PRICE-LIFNR.

ENDFORM.                    " GET_DESCRIPTIONS

*&---------------------------------------------------------------------*
*&      Form  WRITE_SELECTIONS
*&---------------------------------------------------------------------*
FORM WRITE_SELECTIONS.

SKIP.
WRITE: TEXT-040.
SKIP.

* Buyer Code:
WRITE: / TEXT-010.
LOOP AT S_EKGRP.
  WRITE: S_EKGRP-LOW.
ENDLOOP.
IF NOT S_EKGRP-HIGH IS INITIAL.
  WRITE: TEXT-015, S_EKGRP-HIGH.
ENDIF.

* Material Group:
WRITE: / TEXT-011.
LOOP AT S_MATKL.
  WRITE: S_MATKL-LOW.
ENDLOOP.
IF NOT S_MATKL-HIGH IS INITIAL.
  WRITE: TEXT-015, S_MATKL-HIGH.
ENDIF.

* Vendor:
WRITE: / TEXT-012.
LOOP AT S_LIFNR.
  WRITE: S_LIFNR-LOW.
ENDLOOP.
IF NOT S_LIFNR-HIGH IS INITIAL.
  WRITE: TEXT-015, S_LIFNR-HIGH.
ENDIF.

* Material Number:
WRITE: / TEXT-013.
LOOP AT S_MATNR.
  WRITE: S_MATNR-LOW.
ENDLOOP.
IF NOT S_MATNR-HIGH IS INITIAL.
  WRITE: TEXT-015, S_MATNR-HIGH.
ENDIF.

* Dates:
WRITE: / TEXT-DTE, P_DATE1, TEXT-015, P_DATE2.

SKIP.

ENDFORM.                    " WRITE_SELECTIONS

*&---------------------------------------------------------------------*
*&      Form  PAGE_TOT
*&---------------------------------------------------------------------*
FORM PAGE_TOT.

SKIP 2.
FORMAT INTENSIFIED ON.
WRITE: 25 TEXT-045, TEXT-010, IT_PRICE-EKGRP,
                 TEXT-011, IT_PRICE-MATKL,
                 TEXT-012, IT_PRICE-LIFNR,
                 PAGE_TOTAL UNDER EFFECT.
FORMAT INTENSIFIED OFF.

CLEAR PAGE_TOTAL.

ENDFORM.                    " PAGE_TOT
