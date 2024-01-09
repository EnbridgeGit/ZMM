REPORT ZMMMR051 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.

************************************************************************
*    Program     :  ZMMMR051 - MM: List of Materials without storloc
*    Programmer  :  M L DeMeester
*    Date        :  November 3, 1999
************************************************************************
*    This ABAP will list any material which does not have a storage
*    location.
************************************************************************
* CHANGES
* 99/11/04 mdemeest #--- List plant if no storage location
* 99/11/03 mdemeest #--- New request from Mike McCarty
************************************************************************
*****************************  TABLES   ********************************

TABLES: ENT1027, MARD, MARC.

*------ Internal table for accumulating quantities for Company ---------
DATA: BEGIN OF TABLE_MATNR OCCURS 0,
        WERKS            LIKE MARC-WERKS,
        MATNR            LIKE ENT1027-MATNR,    "Material Number
        MAKTX            LIKE ENT1027-MAKTX,    "Short Description
      END OF TABLE_MATNR.

**************************  DATA ELEMENTS  *****************************


***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:   S_MATNR   FOR ENT1027-MATNR,
                  S_WERKS   FOR MARD-WERKS.
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
*-------------- read material master for a material group --------------
CLEAR: TABLE_MATNR.
SELECT MATNR MAKTX FROM ENT1027
       INTO CORRESPONDING FIELDS OF TABLE_MATNR
       WHERE MATNR IN S_MATNR
         AND SPRAS = SY-LANGU
         AND LVORM <> 'X'.
   SELECT SINGLE * FROM MARD WHERE MATNR = TABLE_MATNR-MATNR
                               AND WERKS IN S_WERKS
                               AND LVORM <> 'X'.
      IF SY-SUBRC = '4'.
         SELECT * FROM MARC WHERE MATNR = TABLE_MATNR-MATNR
                              AND LVORM <> 'X'.
           IF SY-SUBRC = '4'.
              APPEND TABLE_MATNR.
           ELSE.
              MOVE MARC-WERKS TO TABLE_MATNR-WERKS.
              APPEND TABLE_MATNR.
           ENDIF.
         ENDSELECT.
      ENDIF.
ENDSELECT.

SORT TABLE_MATNR BY WERKS MATNR.
LOOP AT TABLE_MATNR.
  WRITE: / TABLE_MATNR-WERKS UNDER TEXT-002,
           TABLE_MATNR-MATNR UNDER TEXT-003,
           TABLE_MATNR-MAKTX UNDER TEXT-008.
ENDLOOP.
SKIP 2.
WRITE: / TEXT-004 UNDER TEXT-HDG.

TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_GROUP,
       38 TEXT-HDG,                                               "Title
      100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
ULINE.
FORMAT COLOR COL_NORMAL.

WRITE: /1 TEXT-002, 10 TEXT-003, 30 TEXT-008.
ULINE.
WRITE: /.


***************************  END OF PROGRAM  ***************************
