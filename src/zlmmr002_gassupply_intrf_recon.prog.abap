*&--------------------------------------------------------------------*
*& Report  ZLMMR002_GASSUPPLY_INTRF_RECON
*&
*&--------------------------------------------------------------------*
*  Author:      Mohammad T. Khan                                      *
*  Date:        March, 2011.                                          *
*  Issue Log:   TR804                                                 *
*  Description:                                                       *
*   - This program will report daily gas supply purchases by selecting*
*      material documents created by the CARE-to-SAP interface.       *
*&--------------------------------------------------------------------*
*CHANGES****                                                          *
*&                                                                    *
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*

REPORT  ZLMMR002_GASSUPPLY_INTRF_RECON.
TYPE-POOLS: SLIS.

TABLES: MKPF,       "Header: Material Document
        MSEG,       "Document Segment: Material
        LFA1.       "Vendor Master (General Section)

DATA: BEGIN OF BIGTAB OCCURS 1,
        BLDAT  LIKE MKPF-BLDAT,    "Document Date in Document
        MBLNR  LIKE MKPF-MBLNR,    "Number of Material Document
        ERFMG  LIKE MSEG-ERFMG,    "Quantity in Unit of Entry
        ERFME  LIKE MSEG-ERFME,    "Unit of Entry
        MJAHR	 LIKE MKPF-MJAHR,     "Material Document Year
        BLART  LIKE MKPF-BLART,    "Document Type
        BUDAT	 LIKE MKPF-BUDAT,     "Posting Date in the Document
        CPUDT  LIKE MKPF-CPUDT,	 "Day On Which Accting Doc Was Entered
        CPUTM  LIKE MKPF-CPUTM,    "Time of Entry
        AEDAT  LIKE MKPF-AEDAT,    "Changed On
        USNAM	 LIKE MKPF-USNAM,    "User Name
        XBLNR	 LIKE MKPF-XBLNR,    "Reference Document Number
        BKTXT	 LIKE MKPF-BKTXT,    "Document Header Text
        FRATH	 LIKE MKPF-FRATH,    "Unplanned delivery costs
        FRBNR	 LIKE MKPF-FRBNR,    "# of Bill of Lading at Goods Recpt
        XABLN	 LIKE MKPF-XABLN,    "Goods Receipt/Issue Slip Number
        TCODE2 LIKE MKPF-TCODE2,   "Transaction Code
        ZEILE	 LIKE MSEG-ZEILE,    "Item in Material Document
        BWART  LIKE MSEG-BWART,   "Movement Type (Inventory Management)
        MATNR  LIKE MSEG-MATNR,	"Material Number
        WERKS	 LIKE MSEG-WERKS,   "Plant
        LGORT	 LIKE MSEG-LGORT,   "Storage Location
        SOBKZ	 LIKE MSEG-SOBKZ,   "Special Stock Indicator
        LIFNR	 LIKE MSEG-LIFNR,   "Vendor Account Number
        KUNNR	 LIKE MSEG-KUNNR,   "Account Number of Customer
        KDAUF  LIKE MSEG-KDAUF,   "Sales Order Number
        KDPOS  LIKE MSEG-KDPOS,	"Item Number in Sales Order
        KDEIN	 LIKE MSEG-KDEIN,   "Delivery Schedule for Sales Order
        SHKZG	 LIKE MSEG-SHKZG,   "Debit/Credit Indicator
        WAERS	 LIKE MSEG-WAERS,   "Currency Key
        DMBTR	 LIKE MSEG-DMBTR,   "Amount in Local Currency
        BNBTR	 LIKE MSEG-BNBTR,   "Delivery costs in local currency
        MENGE	 LIKE MSEG-MENGE,   "Quantity
        MEINS	 LIKE MSEG-MEINS,   "Base Unit of Measure
        BPMNG	 LIKE MSEG-BPMNG,   "Quantity in P.Order Price Unit
        BPRME	 LIKE MSEG-BPRME,   "Order Price Unit (Purchasing)
        EBELN	 LIKE MSEG-EBELN,   "Purchase Order Number
        EBELP	 LIKE MSEG-EBELP,   "Item Number of Purchasing Document
        LFBJA	 LIKE MSEG-LFBJA,   "Fiscal Year of a Reference Document
        LFBNR	 LIKE MSEG-LFBNR,   "Doc No. of a Reference Document
        LFPOS	 LIKE MSEG-LFPOS,   "Item of a Reference Document
        ELIKZ	 LIKE MSEG-ELIKZ,   "Delivery Completed Indicator
        SGTXT	 LIKE MSEG-SGTXT,   "Item Text
        KOSTL	 LIKE MSEG-KOSTL,   "Cost Center
        AUFNR	 LIKE MSEG-AUFNR,   "Order Number
        GJAHR	 LIKE MSEG-GJAHR,   "Fiscal Year
        BUKRS	 LIKE MSEG-BUKRS,   "Company Code
        KZBEW	 LIKE MSEG-KZBEW,   "Movement Indicato
        LIFNR2 LIKE LFA1-LIFNR,   "Account Number of Vendor
        NAME1  LIKE LFA1-NAME1,   "Name 1
        ORT01  LIKE LFA1-ORT01,   "City
        PSTLZ  LIKE LFA1-PSTLZ,   "Postal Code
        KONZS  LIKE LFA1-KONZS,   "Group Key
        KTOKK  LIKE LFA1-KTOKK,   "Vendor Account Group
      END OF BIGTAB.

DATA: BEGIN OF LFA1_TAB OCCURS 1,
        LIFNR2 LIKE LFA1-LIFNR,   "Account Number of Vendor
        NAME1  LIKE LFA1-NAME1,   "Name 1
        ORT01  LIKE LFA1-ORT01,   "City
        PSTLZ  LIKE LFA1-PSTLZ,   "Postal Code
        KONZS  LIKE LFA1-KONZS,   "Group Key
        KTOKK  LIKE LFA1-KTOKK,   "Vendor Account Group
      END OF LFA1_TAB.

DATA: W_HEAD01(60)  TYPE C,
      W_HEAD02(60)  TYPE C,
      ES_VARIANT    LIKE DISVARIANT,
      IS_VARIANT    LIKE DISVARIANT,
      W_MJAHR  LIKE MKPF-MJAHR.

***********************************************************************
*                      SELECTION SCREEN                               *
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-101.
PARAMETERS P_MONTH(2) TYPE C.                           "Period(Month)
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(3) TEXT-102.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS S_BLDAT FOR MKPF-BLDAT.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
    S_BLART FOR MKPF-BLART DEFAULT 'WE',                "Doc. Type
    S_BWART FOR MSEG-BWART,                            "Movement Type
    S_EBELN FOR MSEG-EBELN.                            "Purchase Order#
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS:
    S_BUDAT FOR MKPF-BUDAT,                            "Posting Date
    S_USNAM FOR MKPF-USNAM,	                            "User
    S_CPUDT FOR MKPF-CPUDT,                            "Entered on Date
    S_WERKS FOR MSEG-WERKS,	                            "Plant
    S_LGORT FOR MSEG-LGORT,	                            "Stor. Location
    S_MATNR FOR MSEG-MATNR DEFAULT 'NATGAS',           "Material
    S_FRBNR FOR MKPF-FRBNR,	                            "Bill of Lading
    S_XBLNR FOR MKPF-XBLNR,                            "Reference
    S_XABLN FOR MKPF-XABLN,	                            "GR/Issue Slip
    S_MBLNR FOR MKPF-MBLNR.	                            "Material Doc
SELECTION-SCREEN SKIP.
PARAMETERS PVARIANT LIKE DISVARIANT-VARIANT.           "Display Variant
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

***********************************************************************
*                  SELECT DISPLAY VARIANT                             *
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZLMMR002_GASSUPPLY_INTRF_RECON'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = IS_VARIANT
*           I_TABNAME_HEADER    =
*           I_TABNAME_ITEM      =
*           IT_DEFAULT_FIELDCAT =
            I_SAVE              = 'A'
        IMPORTING
*           E_EXIT              =
            ES_VARIANT          = ES_VARIANT
       EXCEPTIONS
            NOT_FOUND           = 1
            PROGRAM_ERROR       = 2
            OTHERS              = 3.
  PVARIANT = ES_VARIANT-VARIANT.

***********************************************************************
*                  AT SELECTION SCREEN                                *
***********************************************************************
AT SELECTION-SCREEN.

DATA: W_ANSWER(1),
W_QUEST(150) VALUE 'Please enter data in DOCUMENT TYPE field when yo' &
'u have entered data in MOVEMENT TYPE and PURCHASE ORDER fields on  ' &
'the variant screen.',
W_TITLE(72) VALUE
'To Continue Press OK. To go back to variant screen, Press CANCEL'.

IF S_BLART = SPACE.
CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
   TITLEBAR                    = 'WARNING'
*   DIAGNOSE_OBJECT             = ' '
    TEXT_QUESTION               = W_QUEST
    TEXT_BUTTON_1               = 'Continue'
    ICON_BUTTON_1               = 'ICON_CHECKED'
    TEXT_BUTTON_2               = 'Cancel'
    ICON_BUTTON_2               = 'ICON_CANCEL'
*   DEFAULT_BUTTON              = '1'
    DISPLAY_CANCEL_BUTTON       = ''
*   USERDEFINED_F1_HELP         = ' '
*   START_COLUMN                = 25
*   START_ROW                   = 6
*   POPUP_TYPE                  =
*   IV_QUICKINFO_BUTTON_1       = ' '
*   IV_QUICKINFO_BUTTON_2       = ' '
  IMPORTING
    ANSWER                      = W_ANSWER
* TABLES
*   PARAMETER                   =
* EXCEPTIONS
*   TEXT_NOT_FOUND              = 1
*   OTHERS                      = 2
  .
IF W_ANSWER = 2.
   STOP.
ENDIF.
ENDIF.

  IF P_MONTH = SPACE AND S_BLDAT-LOW = '00000000'.
     MESSAGE E019(ZS) WITH TEXT-006.
  ENDIF.
  IF P_MONTH <> SPACE AND S_BLDAT-LOW <> '00000000'.
     MESSAGE E019(ZS) WITH TEXT-007.
  ENDIF.


***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
START-OF-SELECTION.
IF S_BLDAT-LOW = '00000000'.
   PERFORM GET_DATE_FROM_PERIOD.
ENDIF.
PERFORM GET_DB_DATA.
IF NOT BIGTAB[] IS INITIAL.
   PERFORM GET_VENDOR_DATA.
   PERFORM MERGE_DATA.
   PERFORM DISPLAY_ALV_GRID_DATA.
ENDIF.
***********************************************************************
*                     GET_DATE_FROM_PERIOD                            *
***********************************************************************
FORM GET_DATE_FROM_PERIOD.

S_BLDAT-SIGN = 'I'.
S_BLDAT-OPTION = 'BT'.
CONCATENATE SY-DATUM(4) P_MONTH '01' INTO S_BLDAT-LOW.
CALL FUNCTION 'FKK_LAST_DAY_OF_MONTH'
   EXPORTING
     DAY_IN = S_BLDAT-LOW
   IMPORTING
     LAST_DAY_OF_MONTH = S_BLDAT-HIGH
   EXCEPTIONS
     DAY_IN_NO_DATE = 1.

 APPEND S_BLDAT.
ENDFORM.
***********************************************************************
*                             GET_DB_DATA                             *
***********************************************************************
FORM GET_DB_DATA.

SELECT  MKPF~BLDAT MKPF~MBLNR MSEG~ERFMG MSEG~ERFME MKPF~MJAHR
        MKPF~BLART MKPF~BUDAT MKPF~CPUDT MKPF~CPUTM MKPF~AEDAT
        MKPF~USNAM MKPF~XBLNR MKPF~BKTXT MKPF~FRATH MKPF~FRBNR
        MKPF~XABLN MKPF~TCODE2 MSEG~ZEILE MSEG~BWART MSEG~MATNR
        MSEG~WERKS MSEG~LGORT MSEG~SOBKZ MSEG~LIFNR MSEG~KUNNR
        MSEG~KDAUF MSEG~KDPOS MSEG~KDEIN MSEG~SHKZG MSEG~WAERS
        MSEG~DMBTR MSEG~BNBTR MSEG~MENGE MSEG~MEINS MSEG~BPMNG
        MSEG~BPRME MSEG~EBELN MSEG~EBELP MSEG~LFBJA MSEG~LFBNR
        MSEG~LFPOS MSEG~ELIKZ MSEG~SGTXT MSEG~KOSTL MSEG~AUFNR
        MSEG~GJAHR MSEG~BUKRS MSEG~KZBEW
*  INTO TABLE BIGTAB
    INTO CORRESPONDING FIELDS OF TABLE BIGTAB
    FROM  ( MKPF INNER JOIN MSEG
                  ON MSEG~MBLNR = MKPF~MBLNR
                 AND MSEG~MJAHR = MKPF~MJAHR )
  WHERE  MKPF~MBLNR IN S_MBLNR
    AND  MKPF~BLART IN S_BLART
    AND  MKPF~BLDAT IN S_BLDAT
    AND  MKPF~BUDAT IN S_BUDAT
    AND  MKPF~CPUDT IN S_CPUDT
    AND  MKPF~USNAM IN S_USNAM
    AND  MKPF~FRBNR IN S_FRBNR
    AND  MKPF~XBLNR IN S_XBLNR
    AND  MKPF~XABLN IN S_XABLN
    AND  MSEG~MATNR IN S_MATNR
    AND  MSEG~WERKS IN S_WERKS
    AND  MSEG~LGORT IN S_LGORT
    AND  MSEG~BWART IN S_BWART
    AND  MSEG~EBELN IN S_EBELN.

 IF SY-SUBRC <> 0.
    WRITE: /1 'NO DATA SELECTED'.
    STOP.
 ENDIF.

 SORT BIGTAB BY BLDAT MBLNR.

 LOOP AT BIGTAB WHERE SHKZG = 'H'.
      BIGTAB-DMBTR = BIGTAB-DMBTR * -1.
      BIGTAB-BNBTR = BIGTAB-BNBTR * -1.
      BIGTAB-MENGE = BIGTAB-MENGE * -1.
      BIGTAB-ERFMG = BIGTAB-ERFMG * -1.
      BIGTAB-BPMNG = BIGTAB-BPMNG * -1.

      MODIFY BIGTAB.
 ENDLOOP.

ENDFORM.

***********************************************************************
*                         GET_VENDOR_DATA                             *
***********************************************************************
FORM GET_VENDOR_DATA.
   SELECT LIFNR NAME1 PSTLZ KONZS KTOKK
     INTO TABLE LFA1_TAB
     FROM LFA1
      FOR ALL ENTRIES IN BIGTAB
    WHERE LIFNR = BIGTAB-LIFNR.
   SORT LFA1_TAB BY LIFNR2.
ENDFORM.

***********************************************************************
*                            MERGE_DATA                               *
***********************************************************************
FORM MERGE_DATA.

LOOP AT BIGTAB.
  IF BIGTAB-LIFNR <> SPACE.
     READ TABLE LFA1_TAB WITH KEY LIFNR2 = BIGTAB-LIFNR BINARY SEARCH.
     IF SY-SUBRC = 0.
        MOVE-CORRESPONDING LFA1_TAB TO BIGTAB.
     ENDIF.
  ENDIF.
  MODIFY BIGTAB.
ENDLOOP.

ENDFORM.
***********************************************************************
*                        DISPLAY_ALV_GRID_DATA                        *
***********************************************************************
FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

DATA: W_DATE_FROM(10) TYPE C,
      W_DATE_TO(10)   TYPE C.

CONCATENATE S_BLDAT-LOW(4) '/' S_BLDAT-LOW+4(2) '/' S_BLDAT-LOW+6(2)
                                                   INTO W_DATE_FROM.
CONCATENATE S_BLDAT-HIGH(4) '/' S_BLDAT-HIGH+4(2) '/' S_BLDAT-HIGH+6(2)
                                                   INTO W_DATE_TO.

CONCATENATE TEXT-004 W_DATE_FROM TEXT-005 W_DATE_TO
            INTO W_HEAD01 SEPARATED BY SPACE.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-COUNTFNAME = 'BPMNG'.
  LAYOUT-ZEBRA = 'X'.
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.
  CLEAR:  FIELDCAT, FC_STR.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'BIGTAB'
       I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)

DEFINE HIDE_COLUMN.
LOOP AT FIELDCAT INTO FC_STR.
     CASE FC_STR-FIELDNAME.
        WHEN &1.
        IF FC_STR-FIELDNAME <> 'MBLNR'.
           FC_STR-NO_OUT = 'X'.            " Hide Columns
        ENDIF.
        FC_STR-KEY    = ' '.               " Key columns-not first
     ENDCASE.
     MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.
END-OF-DEFINITION.

     HIDE_COLUMN:
     'MJAHR', 'BLART', 'BUDAT', 'CPUDT', 'CPUTM',  'AEDAT',  'USNAM',
     'XBLNR', 'BKTXT', 'FRATH', 'FRBNR', 'XABLN',  'TCODE2', 'ZEILE',
     'BWART', 'MATNR', 'WERKS', 'LGORT', 'SOBKZ',  'KUNNR',
*     'BWART', 'MATNR', 'WERKS', 'LGORT', 'SOBKZ',  'LIFNR',  'KUNNR',
     'KDAUF', 'KDPOS', 'KDEIN', 'SHKZG', 'WAERS',  'DMBTR',  'BNBTR',
     'MENGE',  'MEINS', 'BPRME', 'EBELN', 'EBELP',  'LFBJA',
     'LFBNR', 'LFPOS',  'ELIKZ', 'SGTXT', 'KOSTL', 'AUFNR',  'GJAHR',
     'BUKRS', 'KZBEW',  'MBLNR', 'LIFNR2', 'NAME1', 'ORT01',  'PSTLZ',
     'KONZS', 'KTOKK'.
 .

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
          I_SAVE       = 'A'
          IS_VARIANT   = variant
          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = BIGTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- Action Line:  Type A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.
ENDFORM.
