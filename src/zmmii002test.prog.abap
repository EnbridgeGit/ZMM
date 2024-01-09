REPORT ZFMMI002 MESSAGE-ID ZS NO STANDARD PAGE HEADING
                           LINE-COUNT 58 LINE-SIZE 170.
************************************************************************
*  Author    : Glenn Ymana                        SAP : East
*  Date      : August, 2009              Program Type : Interface
*  Issue Log : TR659
************************************************************************
*  Description:
*     - The purpose of this program is to input an Excel spreadsheet and
*       post Goods Movement data via a BAPI.
*
*       The first line of the excel spreadsheet contain header
*       info and are ignored ( sy-tabix < 2)
*
*-----------------------------------------------------------------------
* CHANGES
*
************************************************************************

DATA: BEGIN OF GMHEAD.
        INCLUDE STRUCTURE BAPI2017_GM_HEAD_01.
DATA: END OF GMHEAD.

DATA: BEGIN OF GMCODE.
        INCLUDE STRUCTURE BAPI2017_GM_CODE.
DATA: END OF GMCODE.

DATA: BEGIN OF MTHEAD.
        INCLUDE STRUCTURE BAPI2017_GM_HEAD_RET.
DATA: END OF MTHEAD.

DATA: BEGIN OF ITAB OCCURS 100.
        INCLUDE STRUCTURE BAPI2017_GM_ITEM_CREATE.
DATA: END OF ITAB.

DATA: BEGIN OF ERRMSG OCCURS 10.
        INCLUDE STRUCTURE BAPIRET2.
DATA: END OF ERRMSG.

DATA: WMENGE LIKE ISEG-MENGE,
      ERRFLAG.

DATA: BEGIN OF WA OCCURS 0,
        PLANT(4),              "Plant
        STOR_LOC(4),           "Storage Location
        INT_ORDER(12),         "Internal Order Number
        CITY(12),              "Goods Receipient/ Ship To Party
        MATERIAL(6),           "Material Number
        QTY(13),               "Quantity
        ITEM_TEXT(50),         "Item text / TAG
      END OF WA.

* Table to store records invalid data.
DATA: BEGIN OF ERROR_TAB OCCURS 0,
        PLANT        LIKE WA-PLANT,
        STOR_LOC     LIKE WA-STOR_LOC,
        INT_ORDER    LIKE WA-INT_ORDER,
        CITY         LIKE WA-CITY,
        MATERIAL     LIKE WA-MATERIAL,
        QTY          LIKE WA-QTY,
        ITEM_TEXT    LIKE WA-ITEM_TEXT,
        MSG(50),
      END OF ERROR_TAB.

DATA: INREC(400),
      INFILE(70).

DATA:
      W_LN_CNTR   TYPE I VALUE 0,
      W_MSG(60)   TYPE C,
      CHAR(21)    TYPE C,
      NODATA(1)   VALUE '/',
      PREVPLANT   LIKE WA-PLANT,
      PREVSTORLOC LIKE WA-STOR_LOC.

* check stub text indicator (to allow a slash '/' in BDC fields)
DATA: TEXT_IND(1) TYPE C VALUE 'N'.

DATA: DELIMITER VALUE '09' TYPE X.

FIELD-SYMBOLS: <F1>.

PARAMETERS: P_FILE(60) TYPE C.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BOX0 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-100.

PARAMETERS: P_FILEIN LIKE FILENAME-FILEEXTERN OBLIGATORY DEFAULT
                     '/usr/sap/interfaces/P01/IFMM001/goodsmvmt.chk'.

SELECTION-SCREEN SKIP.
PARAMETERS: P_DSIZE(4) TYPE C OBLIGATORY DEFAULT '2000'.
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN END OF BLOCK BOX0.

*---------------------------------------------------------------------*
* Start-Of-Selection
*---------------------------------------------------------------------*
START-OF-SELECTION.

*  PERFORM OPEN_FILES.
*  PERFORM INPUT_FILE_TO_WA.
  PERFORM WRITE_ERR_HDG.
*  PERFORM POST_GOODS_MVMT.
*  CLOSE DATASET: P_FILEIN.

*---------------------------------------------------------------------*
*  Post Goods Movement
*---------------------------------------------------------------------*
FORM POST_GOODS_MVMT.
  GMHEAD-PSTNG_DATE = SY-DATUM.
  GMHEAD-DOC_DATE = SY-DATUM.
  GMHEAD-PR_UNAME = SY-UNAME.
  GMCODE-GM_CODE = '03'.     "03 - MB1A - Goods Issue

  LOOP AT WA.
    IF WA-PLANT <> PREVPLANT OR
       WA-STOR_LOC <> PREVSTORLOC.
      PERFORM CALL_GOODSMVT_BAPI.
      MOVE WA-PLANT    TO PREVPLANT.
      MOVE WA-STOR_LOC TO PREVSTORLOC.
      REFRESH ITAB.
    ENDIF.

* Determine movement type
    IF STRLEN( WA-INT_ORDER ) = '6'.
      ITAB-MOVE_TYPE = '261'.
    ELSEIF STRLEN( WA-INT_ORDER ) = '11'.
      ITAB-MOVE_TYPE = '221'.
    ENDIF.
    ITAB-MVT_IND    = ' '.
    ITAB-PLANT      = WA-PLANT.
    ITAB-STGE_LOC   = WA-STOR_LOC.
    ITAB-ORDERID    = WA-INT_ORDER.
    ITAB-GR_RCPT    = WA-CITY.
    ITAB-MATERIAL   = WA-MATERIAL.
    ITAB-QUANTITY   = WA-QTY.
    ITAB-ITEM_TEXT  = WA-ITEM_TEXT.
  ENDLOOP.
ENDFORM.                    "post_goods_mvmt

*---------------------------------------------------------------------*
*  Call Goods Movement BAPI to post Goods Issue document
*---------------------------------------------------------------------*
FORM CALL_GOODSMVT_BAPI.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
  EXPORTING
    GOODSMVT_HEADER             = GMHEAD
    GOODSMVT_CODE               = GMCODE
*   TESTRUN                     = ' '
* IMPORTING
    GOODSMVT_HEADRET            = MTHEAD
*   MATERIALDOCUMENT            =
*   MATDOCUMENTYEAR             =
  TABLES
    GOODSMVT_ITEM               = ITAB
*   GOODSMVT_SERIALNUMBER       =
    RETURN                      = ERRMSG
          .
  CLEAR ERRFLAG.
  LOOP AT ERRMSG.
    IF ERRMSG-TYPE EQ 'E'.
      WRITE:/'Error in function', ERRMSG-MESSAGE.
      ERRFLAG = 'X'.
    ELSE.
      WRITE:/ ERRMSG-MESSAGE.
    ENDIF.
  ENDLOOP.

  IF ERRFLAG IS INITIAL.
    COMMIT WORK AND WAIT.
    IF SY-SUBRC NE 0.
      WRITE:/ 'Error in updating'.
      EXIT.
    ELSE.
      WRITE:/ MTHEAD-MAT_DOC, MTHEAD-DOC_YEAR.
    ENDIF.
  ENDIF.

ENDFORM.                    "call_goodsmvt_bapi

*-----------------------------------------------------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields.
*-----------------------------------------------------------------------
FORM INPUT_FILE_TO_WA.
  DO.
    CLEAR: WA, INREC.
    READ DATASET P_FILEIN INTO INREC.
    IF SY-INDEX < 2.
    ELSEIF INREC IS INITIAL.
      EXIT.
    ELSE.
      SPLIT INREC AT DELIMITER INTO
        WA-PLANT WA-STOR_LOC WA-INT_ORDER WA-CITY WA-MATERIAL
        WA-QTY WA-ITEM_TEXT.
      APPEND WA.
    ENDIF.
  ENDDO.
  SORT WA.
ENDFORM.                    "INPUT_FILE_TO_WA

*-----------------------------------------------------------------------
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_FILES.
  DATA: MSG(100).
*-----------------------------------------------------------------------
  OPEN DATASET P_FILEIN FOR INPUT IN TEXT MODE MESSAGE MSG.
  IF ( SY-SUBRC <> 0 ).
    MESSAGE E002 WITH INFILE MSG.
  ENDIF.

ENDFORM.                    "OPEN_FILES

*---------------------------------------------------------------------*
* Generate Error Report Header
*---------------------------------------------------------------------*

FORM WRITE_ERR_HDG.
  NEW-PAGE.
  CLEAR W_LN_CNTR.
  FORMAT INTENSIFIED ON.
  WRITE: /1 TEXT-001, 41 TEXT-002.
  WRITE: 106 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: /1 TEXT-003,
         45 TEXT-004,
         62 SY-DATUM.
  WRITE: 121 TEXT-PGE, SY-PAGNO.
  SKIP.

  WRITE:  /1  TEXT-005,
          10  TEXT-006,
          22  TEXT-007,
          30  TEXT-008,
          40  TEXT-010,
          61  TEXT-011,
          74  TEXT-012.
  WRITE:  /   TEXT-013 UNDER TEXT-006,
              TEXT-014 UNDER TEXT-007.
  SKIP.
  MOVE '6' TO W_LN_CNTR.

  FORMAT INTENSIFIED OFF.
ENDFORM.                    "WRITE_ERR_HDG
*---------------------------------------------------------------------*
* Generate Error Detail.
*---------------------------------------------------------------------*
FORM GENERATE_ERROR_MSG.

*  SKIP.
  WRITE: /1 TEXT-ERR,
          8 ERROR_TAB-MSG.
*  SKIP.
  W_LN_CNTR = W_LN_CNTR + 1.
  CLEAR ERROR_TAB.

ENDFORM.                    "GENERATE_ERROR_MSG

*---------------------------------------------------------------------*
* Generate Error Detail Line 1.
*---------------------------------------------------------------------*
*FORM GENERATE_ERROR_LINE1.
*  IF W_LN_CNTR >= 55.
*    PERFORM WRITE_ERR_HDG.
*  ENDIF.
*  MOVE-CORRESPONDING WA TO ERROR_TAB.
*
*  WRITE: /   ERROR_TAB-CO_CODE     UNDER TEXT-005,
*             ERROR_TAB-EMPNO       UNDER TEXT-006,
*             ERROR_TAB-EMP_NAME    UNDER TEXT-007,
*             ERROR_TAB-PSTKEY      UNDER TEXT-009,
*             ERROR_TAB-GLACCT      UNDER TEXT-010,
*             ERROR_TAB-TAX_CD      UNDER TEXT-011,
*             ERROR_TAB-HOME_CC     UNDER TEXT-012,
*        (15) ERROR_TAB-AMOUNT      UNDER TEXT-013 DECIMALS 2,
*             ERROR_TAB-EARN_DED_CD UNDER TEXT-015,
*             ERROR_TAB-PAY_DATE    UNDER TEXT-016,
*        (18) ERROR_TAB-MTH_COMP    UNDER TEXT-017 DECIMALS 3,
*        (4)  ERROR_TAB-FTE         UNDER TEXT-018 DECIMALS 2,
*        (12) ERROR_TAB-QUANTITY    UNDER TEXT-019 DECIMALS 3.
*  W_LN_CNTR = W_LN_CNTR + 1.
*ENDFORM.
