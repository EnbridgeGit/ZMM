REPORT ZMMMR050 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.

************************************************************************
*    Program     :  ZMMMR050 - MM: INVENTORY CATALOG BY MATERIAL GROUP
*    Programmer  :  Ric Aarssen
*    Date        :  February 25, 1999
************************************************************************
*    This ABAP will retrieve the Material Numbers associated with the
*    material groups. It will also find the keyword and short
*    description for each material found. Sorted by material number
*    within material group prior to printing.
************************************************************************
* CHANGES
* 99/10/27 mdemeest #--- Fix record selection - some materials were
*                        not being listed
* 99/10/26 mdemeest #--- Added Primary & Secondary Descriptions
* 99/10/25 mdemeest #--- Changed sort to GROUP/KEYWORD/MATERIAL#
************************************************************************
*****************************  TABLES   ********************************

* tables: mara, marc, mard, makt.
TABLES: ENT1027, MARC, MARD.

*------ Internal table for accumulating quantities for Company ---------
DATA: BEGIN OF TABLE_MATNR OCCURS 0,
        MATKL            LIKE ENT1027-MATKL,    "Material Group
        KEYWORD          LIKE AUSP-ATWRT,       "Keyword Characteristic
        MATNR            LIKE ENT1027-MATNR,    "Material Number
*       maktx            like ent1027-maktx,    "Short Description
        PRIMARY          LIKE AUSP-ATWRT,       "Primary Description
        SECONDARY        LIKE AUSP-ATWRT,       "Secondary Description
      END OF TABLE_MATNR.

**************************  DATA ELEMENTS  *****************************

*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_KEYWORD   LIKE CABN-ATINN,       "keyword Characteristic
       G_ATINN_PRIMARY   LIKE CABN-ATINN,       "Primary Description
       G_ATINN_SECONDARY LIKE CABN-ATINN.       "Secondary Description c

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

*-----------------------------------------------------------------------
data: w_keyword       like ausp-atwrt,          "keyword Description
      W_PRIMARY       LIKE AUSP-ATWRT,          "Primary Description
      W_SECONDARY     LIKE AUSP-ATWRT,          "Secondary Description
*---------------------- Work Area --------------------------------------
      REQUIRED(3)     TYPE C,                   "Required on report
      check_point     type i,                   "Check MM/PP status
      CHECK_MARC      TYPE I,                   "Check for Plant record
      MATRCTR         TYPE I.                   "Line ctr for material

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS: S_MATKL FOR ENT1027-MATKL.
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.

***************************  MAIN ROUTINE  *****************************
*top-of-page is NOT being used for the headings since the heading is to
* appear only once on the report for ease when the report gets dumped to
* excel
*-----------------------------------------------------------------------
START-OF-SELECTION.
* print report header
WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_GROUP,
       38 TEXT-HDG,                                               "Title
      100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
ULINE.
FORMAT COLOR COL_NORMAL.

WRITE: /1 TEXT-004, 31 TEXT-003, 41 TEXT-008.
ULINE.
WRITE: /.
MOVE 5 TO MATRCTR.

*----------------- obtain characteristics object numbers ---------------
MOVE 'KEYWORD'                 TO CHARIC.      "Characteristics Required
PERFORM GET_ATINN.
MOVE G_ATINN                   TO G_ATINN_KEYWORD.

MOVE 'PRIMARY_DESCRIPTION'     TO CHARIC.      "Characteristics Required
PERFORM GET_ATINN.
MOVE G_ATINN                   TO G_ATINN_PRIMARY.


MOVE 'SECONDARY_DESCRIPTION'   TO CHARIC.      "Characteristics Required
PERFORM GET_ATINN.
MOVE G_ATINN                   TO G_ATINN_SECONDARY.

*-------------- read material master for a material group --------------
CLEAR: TABLE_MATNR.
SELECT MATKL MATNR FROM ENT1027
       INTO CORRESPONDING FIELDS OF TABLE_MATNR
       WHERE SPRAS = SY-LANGU
       AND   MATKL IN S_MATKL
       AND   LVORM <> 'X'.
  CLEAR REQUIRED.
  SELECT * FROM MARC WHERE MATNR = TABLE_MATNR-MATNR
                     AND   LVORM <> 'X'.
    IF   MARC-MMSTA = '01'
      OR MARC-MMSTA = '02'
      OR MARC-MMSTA = '04'.
      SELECT SINGLE * FROM MARD WHERE MATNR = MARC-MATNR
                           AND WERKS = MARC-WERKS
                           AND LABST > 0.
        IF SY-SUBRC = 0.
          MOVE 'YES' TO REQUIRED.
        ENDIF.
    ELSE.
       MOVE 'YES' TO REQUIRED.
    ENDIF.
  ENDSELECT.                              "END of MARC selection
* if record selected, get characteristics & add to table
  IF REQUIRED = 'YES'.
    OBJECT = TABLE_MATNR-MATNR.
    PERFORM FIND_CHARACTERISTIC.
    MOVE W_KEYWORD            TO TABLE_MATNR-KEYWORD.
    MOVE W_PRIMARY            TO TABLE_MATNR-PRIMARY.
    MOVE W_SECONDARY          TO TABLE_MATNR-SECONDARY.
*---------------- add records to table work area -----------------------
    APPEND TABLE_MATNR.
  CLEAR: TABLE_MATNR.
ENDIF.
ENDSELECT.

*------- sort table by material number within material group -----------
SORT TABLE_MATNR ASCENDING BY MATKL KEYWORD MATNR.
LOOP AT TABLE_MATNR.
  AT NEW MATKL.                              "Heading for Material Group
  ADD 1 TO MATRCTR.
  SKIP TO LINE MATRCTR.
  WRITE: /1 'GROUP', TABLE_MATNR-MATKL.
  ADD 1 TO MATRCTR.
  ENDAT.

*-------------------- write report from table --------------------------
IF SY-LINNO > 65.
   NEW-PAGE.
ENDIF.

SKIP TO LINE MATRCTR.
WRITE: / TABLE_MATNR-KEYWORD          UNDER TEXT-004,
         TABLE_MATNR-MATNR            UNDER TEXT-003,
*         table_matnr-maktx            under text-008.
         TABLE_MATNR-PRIMARY          UNDER TEXT-008 NO-GAP,
         TABLE_MATNR-SECONDARY.
ADD 1 TO MATRCTR.
ENDLOOP.
END-OF-SELECTION.
*------------------------ TOP-OF-PAGE  ---------------------------------
* This routine is used only to reset counters for the next page.
* Headings are only required on the first page of the report because
* users may download it to a spreadsheet.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  MOVE 1 TO: MATRCTR.

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
  CLEAR: W_KEYWORD, W_PRIMARY, W_SECONDARY.
  IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
* keyword description
   READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_KEYWORD BINARY SEARCH.
   IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATWRT TO W_KEYWORD.
   ENDIF.
* primary description
   READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_PRIMARY BINARY SEARCH.
   IF SY-SUBRC EQ 0.
      MOVE CHAR_TAB-ATWRT TO W_PRIMARY.
   ENDIF.
* secondary description.
   READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_SECONDARY BINARY SEARCH.
   IF SY-SUBRC EQ 0.
      IF CHAR_TAB-ATWRT <> 'N/A'.
         MOVE CHAR_TAB-ATWRT TO W_SECONDARY.
      ENDIF.
   ENDIF.
 ENDIF.
 CLEAR: OBJECT.
ENDFORM.

***************************  END OF PROGRAM  ***************************
