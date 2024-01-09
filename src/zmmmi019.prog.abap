REPORT ZMMMI019 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.

************************************************************************
* Program     :  ZMMMI019 - MM: EXTRACT COMPANY WIDE QOH AND AVP
* Programmer  :  M DeMeester
* Date        :  April 26, 2000
************************************************************************
* 2000/04/26 mdemeest #777 Original Request - Using ZMMMI019 as a basis
*                          create identical report, maintaining order
*                          of materials as entered in variant
************************************************************************
* This ABAP will retrieve the Material Numbers in the order entered
* in the variant. It will also calculate the Company wide QOH
* and AUP for each material found.
* This ABAP will call in two #INCLUDES.  One will contain
* the data structure and the other will contain the sub-routines
* used get the material number associated with the class entered.
* The second part of the sub-routine will be to get the
* characteristics for the material numbers retrieved in the first
************************************************************************
*****************************  TABLES   ********************************

TABLES: MARA, MBEW.

*------ Internal table for accumulating quantities for Company ---------
DATA: BEGIN OF TABLE_MATNR OCCURS 0,
        MATKL            LIKE MARA-MATKL,       "Material Group
        MATNR            LIKE MARA-MATNR,       "Material Number
        PRIMARY          LIKE AUSP-ATWRT,       "Primary Characteristic
        SECONDARY        LIKE AUSP-ATWRT,       "Secondary Character
        QOH              LIKE MBEW-LBKUM,       "Quantity on Hand
        AUP              LIKE MBEW-VERPR,       "Average Unit Price
      END OF TABLE_MATNR.

**************************  DATA ELEMENTS  *****************************

*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_PRIMARY   LIKE CABN-ATINN,       "Primary Characteristic
       G_ATINN_SECONDARY LIKE CABN-ATINN,       "Secondary Character
       G_ATINN_MANUNAME  LIKE CABN-ATINN,       "Manufacturer Name
       G_ATINN_MANUPART  LIKE CABN-ATINN,       "Manufacturer Part Numb
       G_ATINN_MODEL     LIKE CABN-ATINN.       "Model Part Number

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

*-----------------------------------------------------------------------
DATA: PRI_LGTH        LIKE AUSP-ATWRT,          "Primary Length
      SEC_LGTH        LIKE AUSP-ATWRT,          "Secondary Length
*---------------------- Work Area --------------------------------------
      W_MATKL         LIKE MARA-MATKL,          "Material Group
      W_QOH           LIKE MBEW-LBKUM,          "Qty on Hand
      W_VALUE         LIKE MBEW-SALK3,          "Dollar Value
      W_SLOC_COUNT(2) TYPE P,                   "Storage Loc Count
      W_AUP_TOTAL     LIKE MBEW-VERPR,          "Total Average Price
      MATRCTR         TYPE I,                   "Line ctr for material
      MANUCTR         LIKE MATRCTR,             "Line ctr for manufact
      MODELCTR        LIKE MATRCTR.             "Line ctr for part/model

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECT-OPTIONS: P_MATNR FOR MARA-MATNR NO INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN SKIP.


***************************  MAIN ROUTINE  *****************************
*top-of-page is NOT being used for the headings since the heading is to
* appear only once on the report for ease when the report gets dumped to
* excel
*-----------------------------------------------------------------------
START-OF-SELECTION.
WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_GROUP,
       60 TEXT-HDG,                                               "Title
      142 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
         TEXT-014 UNDER TEXT-HDG,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
ULINE.
FORMAT COLOR COL_NORMAL.

WRITE: /80 TEXT-010, 90 TEXT-012.
WRITE: /1 TEXT-003, 15 TEXT-008, 45 TEXT-007, TEXT-011 UNDER TEXT-010,
          TEXT-013 UNDER TEXT-012, 106 TEXT-015, 126 TEXT-016.
ULINE.
WRITE: /.
MOVE 7 TO MATRCTR.

*----------------- obtain characteristics object numbers ---------------
MOVE 'PRIMARY_DESCRIPTION'     TO CHARIC.      "Characteristics Required
PERFORM GET_ATINN.
MOVE G_ATINN                   TO G_ATINN_PRIMARY.

MOVE 'SECONDARY_DESCRIPTION'   TO CHARIC.
PERFORM GET_ATINN.
MOVE G_ATINN                   TO G_ATINN_SECONDARY.

MOVE 'MANUFACTURER_NAME'       TO CHARIC.
PERFORM GET_ATINN.
MOVE G_ATINN                   TO G_ATINN_MANUNAME.

MOVE 'MANUFACTURER_PART_NUMBER' TO CHARIC.
PERFORM GET_ATINN.
MOVE G_ATINN                   TO G_ATINN_MANUPART.

MOVE 'MODEL_NUMBER'            TO CHARIC.
PERFORM GET_ATINN.
MOVE G_ATINN                   TO G_ATINN_MODEL.

*-------------- read material master for a material group --------------
LOOP AT P_MATNR.
  SELECT * FROM MARA
    WHERE MATNR = P_MATNR+3(18).


*------ read material valuation table & accumulate QOH and $value ------
  CLEAR: TABLE_MATNR, W_QOH, W_VALUE, W_SLOC_COUNT, W_AUP_TOTAL.
  SELECT * FROM MBEW
    WHERE MATNR = MARA-MATNR.
    IF MBEW-BWKEY+1(1) <> '2'.
      W_QOH   = W_QOH + MBEW-LBKUM.
      W_VALUE = W_VALUE + MBEW-SALK3.
      IF MBEW-VERPR > 0.
         W_AUP_TOTAL  = W_AUP_TOTAL + MBEW-VERPR.
         W_SLOC_COUNT = W_SLOC_COUNT + 1.
      ENDIF.
    ENDIF.
  ENDSELECT.                                         "End of MBEW select
*------ calculate aup and move material record to internal table -------
  MOVE MARA-MATKL        TO TABLE_MATNR-MATKL.
  MOVE MARA-MATNR        TO TABLE_MATNR-MATNR.
  MOVE PRI_LGTH          TO TABLE_MATNR-PRIMARY.
  MOVE SEC_LGTH          TO TABLE_MATNR-SECONDARY.
  MOVE W_QOH             TO TABLE_MATNR-QOH.
  IF W_VALUE > 0.
     TABLE_MATNR-AUP = W_VALUE / W_QOH.
   ELSE.
     TABLE_MATNR-AUP = W_AUP_TOTAL / W_SLOC_COUNT.
  ENDIF.

*---------------- add records to table work area -----------------------
  APPEND TABLE_MATNR.
  ENDSELECT.
ENDLOOP.

*------- sort table by material number within material group -----------
*ort table_matnr ascending by matkl matnr.
LOOP AT TABLE_MATNR.
* at new matkl.                              "Heading for Material Group
*  add 2 to matrctr.
*  skip to line matrctr.
*  write: /1 'GROUP', table_matnr-matkl.
*  add 1 to matrctr.

  OBJECT = TABLE_MATNR-MATNR.
  PERFORM FIND_CHARACTERISTIC.
*-------------------- write report from table --------------------------
MOVE MATRCTR TO: MANUCTR, MODELCTR.
* If/new-page was added to handle the situtation when material info is
* written on the last line of the page.  Material info would print, then
* a new page, and then the relevant manufacturer/model info.
*-----------------------------------------------------------------------
IF SY-LINNO > 56.
   NEW-PAGE.
ENDIF.

SKIP TO LINE MATRCTR.
WRITE: / TABLE_MATNR-MATNR            UNDER TEXT-003,
         PRI_LGTH                     UNDER TEXT-008,
     (8) TABLE_MATNR-QOH DECIMALS 0   UNDER TEXT-010,
         TABLE_MATNR-AUP              UNDER TEXT-012.
      IF SEC_LGTH  NE 'N/A'.
         WRITE: SEC_LGTH              UNDER TEXT-007.
      ENDIF.

LOOP AT CHAR_TAB.
  IF G_ATINN_MANUNAME = CHAR_TAB-ATINN.
     SKIP TO LINE MANUCTR.
     WRITE: CHAR_TAB-ATWRT UNDER TEXT-015.
     ADD 1 TO MANUCTR.
  ENDIF.
  IF G_ATINN_MANUPART = CHAR_TAB-ATINN.
     SKIP TO LINE MODELCTR.
     WRITE: CHAR_TAB-ATWRT UNDER TEXT-016 NO-GAP, '(1)'.
     ADD 1 TO MODELCTR.
  ENDIF.
  IF G_ATINN_MODEL = CHAR_TAB-ATINN.
     SKIP TO LINE MODELCTR.
     WRITE: CHAR_TAB-ATWRT UNDER TEXT-016 NO-GAP, '(2)'.
     ADD 1 TO MODELCTR.
  ENDIF.
ENDLOOP.
IF MANUCTR = MATRCTR.                       "If no model or manufacturer
   IF MODELCTR = MATRCTR.
      ADD 1 TO: MATRCTR, MANUCTR, MODELCTR.
   ENDIF.
ENDIF.
IF MANUCTR > MATRCTR.
  MOVE MANUCTR TO MATRCTR.
ENDIF.
IF MODELCTR > MATRCTR.
  MOVE MODELCTR TO MATRCTR.
ENDIF.
ENDLOOP.

END-OF-SELECTION.
*------------------------ TOP-OF-PAGE  ---------------------------------
* This routine is used only to reset counters for the next page.
* Headings are only required on the first page of the report because
* users may download it to a spreadsheet.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  MOVE 1 TO: MATRCTR, MODELCTR, MANUCTR.

*****************************  SUB-ROUTINES  ***************************

*------------------------ get_atinn sub-routine ------------------------
* Routine used to get the internal character number for material class
*-----------------------------------------------------------------------
FORM GET_ATINN.
  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
           CLASS_TYPE                   = '001'
           FEATURE_NEUTRAL_NAME         = CHARIC
       IMPORTING
           FEATURE_ID                   = G_ATINN
       EXCEPTIONS
           INVALID_CLASS_TYPE           = 1
           MISSING_FEATURE_INFORMATION  = 2
           NO_FEATURE_FOUND             = 3
           NO_FEATURE_VALID             = 4
           NO_LANGUAGE                  = 5
           OTHERS                       = 6.
 IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO DETERMINE THE CHARACTERISTICS OF', CHARIC.
 ENDIF.
ENDFORM.

*--------------------- find_characteristic routine ---------------------
* Routine used to get the characteristic of the material
*-----------------------------------------------------------------------
FORM FIND_CHARACTERISTIC.
    REFRESH CHAR_TAB.
    CALL FUNCTION 'CLFM_SELECT_AUSP'
        EXPORTING
            MAFID           = 'O'
            CLASSTYPE       = '001'
            OBJECT          = OBJECT
        TABLES
            EXP_AUSP        = CHAR_TAB
        EXCEPTIONS
            NO_VALUES       = 1
            OTHERS          = 2.
*loop at char_tab.
*write: / char_tab-atinn, char_tab-atwrt.
*endloop.
* Character values are in "ATWRT", (numeric values would be in "ATFLV")
 CLEAR: PRI_LGTH, SEC_LGTH.
 IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
* primary description
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_PRIMARY BINARY SEARCH.
    IF SY-SUBRC EQ 0.
       MOVE CHAR_TAB-ATWRT TO PRI_LGTH.
    ENDIF.
* secondary description
   READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_SECONDARY BINARY SEARCH.
    IF SY-SUBRC EQ 0.
       MOVE CHAR_TAB-ATWRT TO SEC_LGTH.
    ENDIF.
 ENDIF.
 CLEAR: OBJECT.
ENDFORM.

***************************  END OF PROGRAM  ***************************
