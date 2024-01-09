REPORT ZMWMC004 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
*----------------------------------------------------------------------
* Program:    ZMWMC004
* Created On: September 22, 1998
* Created By: Ric Aarssen
*----------------------------------------------------------------------
* Modified:   October 16, 1998
*   - We were asked to include the Westburn Plants in the old Centra
*     teritory and all Dealer locations
***** ***** ***** ***** ***** ***** ***** ****** ***** ***** ***** *****
*  Modified on December 1, 1998 by Nancy Gilligan, OmniLogic under     *
*   key D30K906521.                                                    *
*   - added report to show extracted data to compare with report from  *
*     ZMINR025.                                                        *
************************************************************************
*-----------------------------------------------------------------------
* (1) This program extracts data for Union Energy separation:
*               - Material Master Data Load
*     It is an extracted copy of ZMWMI004 Plant create file for WIS
*        warehouses
*
* (2) Output file:
*        Logical File  - ZMWMC004_01
*        Physical File - /usr/sap/interfaces/P01/UEC/ZMWMC004.SAP
*
*----------------------------------------------------------------------
*    TABLE DECLARATIONS
*----------------------------------------------------------------------
TABLES:
       MARA,    "Material Master - General Data
       MARC,    "Material Master - Plant Data
       MARD,    "Material Master - Storage Location data
       MAKT,    "Material Decription
       MBEW,    "Material Valuation
       MARM,    "Units of Measure
       T001W,
       T001L,
       T023T,
       ENT1027.


*----------------------------------------------------------------------
*    OUTPUT RECORD LAYOUT
*----------------------------------------------------------------------
DATA:
    OUTFILE(100),
    TOTAL_VALUE(8)  TYPE P DECIMALS 2,
*--> plant record to be downloaded to Union Energy
    BEGIN OF REC,
       LVORM          LIKE MARD-LVORM, "Delete flag
       WERKS          LIKE MARC-WERKS, "Plant
       LGORT          LIKE MARD-LGORT, "Storage Location
       MATNR          LIKE MARA-MATNR, "Material Number
       MAKTX          LIKE MAKT-MAKTX, "Description
       MATKL          LIKE MARA-MATKL, "Material Group
       MEINS          LIKE MARA-MEINS, "Base Uom
       BSTME          LIKE MARA-BSTME, "Purchasing UOM
       AUSME          LIKE MARC-AUSME, "Issuing uom
       UMREZ(5)             TYPE C,    "Conv factor - Num
       UMREN(5)             TYPE C,    "Conv factor - Denom
       QOH(13)              TYPE C,    "Quantity on Hand
       EISBE(13)            TYPE C,    "Safety Stock
       MINBE(13)            TYPE C,    "Reorder point
       VERPR(13)            TYPE C,    "Average unit price
       DISMM          LIKE MARC-DISMM, "MRP Type
       HAZARD(1)                         TYPE C,    "Hazard Indicator
       MMSTA          LIKE MARC-MMSTA, "Obsolete Indicator
       KZKRI          LIKE MARC-KZKRI, "Critical Indicator
       ABCIN          LIKE MARC-ABCIN, "ABC Indicator
       PLIFZ(4)             TYPE C,    "Planned Delivery Time
       NEW_USED(4)                       TYPE C,    "Characteristic
    END OF REC.


DATA    : BEGIN OF TABLE1 OCCURS 5000,
              WERKS        LIKE MARD-WERKS,
              LGORT        LIKE MARD-LGORT,
              MATKL        LIKE ENT1027-MATKL,
              MATNR        LIKE MARA-MATNR,
              LVORM        LIKE MARD-LVORM,
              QTYHAND(13)  TYPE C,
              VALUE        LIKE TOTAL_VALUE,
              MAKTX        LIKE ENT1027-MAKTX,
              VERPR        LIKE TOTAL_VALUE,
          END OF TABLE1.


*----------------------------------------------------------------------
*    WORKING STORAGE DATA DECLARATIONS
*----------------------------------------------------------------------
*--> characteristic fields
DATA:  MATERIAL       LIKE MARA-MATNR,
       CHARIC         LIKE CABNT-ATBEZ,
       NAME           LIKE CABNT-ATBEZ,"Characteristic Name
       NEUTNAME       LIKE CABN-ATNAM, "Characteristic Neutral Name
       OBJECT         LIKE AUSP-OBJEK. "Required type to pass to FM

DATA: G_ATINN LIKE AUSP-ATINN.

DATA:  BEGIN OF CHAR_TAB     OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA:  END OF CHAR_TAB.

*---------------------------------------------------------------------
*    SELECTION CRITERIA EVENT
*---------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 10(60) TEXT-001.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    SELECT-OPTIONS:  S_WERKS FOR MARC-WERKS.    "Plant for loop
*    select-options:  s_lgort for mard-lgort.    "test
*    select-options:  s_matnr for mara-matnr.    "test
  SELECTION-SCREEN END OF BLOCK BOX2.

  SELECTION-SCREEN SKIP.
    PARAMETERS:      DEALERS AS CHECKBOX.                "dealers only
  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
*--> output file logical path
    PARAMETER:       LFILE LIKE FILENAME-FILEINTERN
                                DEFAULT 'ZMWMC004_01_UEC' OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

*---------------------------------------------------------------------
*    MAIN PROCESSING EVENT
*---------------------------------------------------------------------

TOP-OF-PAGE.
WRITE: /1 TEXT-019, SY-REPID COLOR COL_GROUP INTENSIFIED OFF INVERSE ON,
       50 TEXT-002, 105 TEXT-024, SY-DATUM, TEXT-025, SY-UZEIT.
WRITE: / TEXT-022 UNDER TEXT-019, SY-MANDT UNDER SY-REPID,
         TEXT-026 UNDER TEXT-024, (3) SY-PAGNO.
ULINE.

WRITE: /44 TEXT-021.
WRITE: /45 TEXT-012, 80 TEXT-006, 89 TEXT-013, 129 TEXT-031.    "MRadsma
WRITE: /33 TEXT-011,                                  "Quantity
           TEXT-017 UNDER TEXT-012,                   "AVG Moving Price
        62 TEXT-027,                                  "Value
           TEXT-018 UNDER TEXT-006,                   "Material Number
           TEXT-015 UNDER TEXT-013,                   "Description
           TEXT-032 UNDER TEXT-031.                   "Del flag  MRadsma
ULINE.

START-OF-SELECTION.
*--> Logical file name
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            CLIENT           = SY-MANDT
            LOGICAL_FILENAME = LFILE
            OPERATING_SYSTEM = SY-OPSYS
       IMPORTING
            FILE_NAME        = OUTFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 01.
  IF ( SY-SUBRC = 1 ).
    WRITE: 'function return code=',
            SY-SUBRC.
  ENDIF.
*--> open output file
  OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE.
  IF  SY-SUBRC NE '0'.
    WRITE: 'open error.  return code=', SY-SUBRC.
  ENDIF.
  PERFORM GET_ATINN.
  PERFORM SET-UP-PLANT.

END-OF-SELECTION.
  CLOSE DATASET OUTFILE.
  PERFORM WRITE_REPORT.

*-----------------------------------------------------------------------
*     FORM GET_ATINN
*-----------------------------------------------------------------------
* - This routine returns the internal charac. number for the charac.
*   'NEW_USED'.
*-----------------------------------------------------------------------
FORM GET_ATINN.

  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
            CLASS_TYPE                   = '001'
*            FEATURE_ID                  = CHAR_TAB-ATINN            "LH
            FEATURE_NEUTRAL_NAME         = 'NEW_USED'
       IMPORTING
*            FEATURE_NAME                = NAME                      "LH
*            FEATURE_NEUTAL_NAME         = NEUTNAME                  "LH
            FEATURE_ID                  = G_ATINN
       EXCEPTIONS
            INVALID_CLASS_TYPE          = 1
            MISSING_FEATURE_INFORMATION = 2
            NO_FEATURE_FOUND            = 3
            NO_FEATURE_VALID            = 4
            NO_LANGUAGE                 = 5
            OTHERS                      = 6.
  IF  SY-SUBRC <> 0.
    WRITE: / 'Unable to determine internal characteristic for NEW_USED'.
  ENDIF.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM SET-UP-PLANT
*-----------------------------------------------------------------------
*-->  select plant information from MARC
*-----------------------------------------------------------------------
FORM SET-UP-PLANT.

  SELECT * FROM MARC WHERE WERKS IN S_WERKS
*                          and matnr in s_matnr  "test
        ORDER BY MATNR.
    MOVE MARC-WERKS         TO REC-WERKS.

*   move marc-ausme - must write to ensure english translation
    WRITE MARC-AUSME        TO REC-AUSME.

    MOVE MARC-EISBE         TO REC-EISBE.
    MOVE MARC-DISMM         TO REC-DISMM.
    MOVE MARC-MMSTA         TO REC-MMSTA.
    MOVE MARC-KZKRI         TO REC-KZKRI.
    MOVE MARC-ABCIN         TO REC-ABCIN.
    MOVE MARC-MINBE         TO REC-MINBE.
    MOVE MARC-PLIFZ         TO REC-PLIFZ.
*   write marc-plifz right-justified to rec-plifz.

    PERFORM GET_MATERIAL_INFO USING MARC-MATNR.
    MOVE 0                  TO REC-VERPR.
    SELECT * FROM MBEW
        WHERE MATNR = MARC-MATNR AND
              BWKEY = MARC-WERKS.
      MOVE MBEW-VERPR          TO REC-VERPR.
      EXIT.
    ENDSELECT.

*--> select storage information from MARD
    MOVE 0              TO REC-QOH.
    MOVE SPACE          TO REC-LVORM.
    MOVE SPACE          TO REC-LGORT.
    SELECT * FROM MARD
         WHERE MATNR = MARC-MATNR AND
               WERKS = MARC-WERKS.
*           and lgort in s_lgort.    "test
* get material group info from ent1027 instead of mara & makt
          SELECT SINGLE * FROM ENT1027 WHERE MATNR = MARD-MATNR
                                         AND SPRAS = SY-LANGU.

      IF DEALERS = 'X'.
         IF MARD-LGORT(1) = 'D' AND MARD-LABST > 0.
            PERFORM MOVE_STORAGE_INFO.
            PERFORM LOAD_TABLE1.
*           clear: mara, mard, marc, makt, mbew, marm,         "MDEMEEST
*                  t001w, t001l, t023t, ent1027.
         ENDIF.
      ELSE.
         PERFORM MOVE_STORAGE_INFO.
         PERFORM LOAD_TABLE1.
*       clear: mara, mard, marc, makt, mbew, marm,             "MDEMEEST
*               t001w, t001l, t023t, ent1027.
      ENDIF.
    ENDSELECT.
*--> do not write record if plant but no storage locations
*    IF  SY-SUBRC = '4' AND DEALERS <> 'X'.
*      TRANSFER REC TO OUTFILE LENGTH 161.
*    ENDIF.

  ENDSELECT.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM MOVE_STORAGE_INFO
*-----------------------------------------------------------------------
FORM MOVE_STORAGE_INFO.
      MOVE 0              TO REC-QOH.
      ADD MARD-LABST      TO REC-QOH.
      ADD MARD-INSME      TO REC-QOH.
      MOVE MARD-LVORM     TO REC-LVORM.
      MOVE MARD-LGORT     TO REC-LGORT.

      TABLE1-QTYHAND = REC-QOH.
      TABLE1-VALUE = REC-QOH * MBEW-VERPR.

      TRANSFER REC TO OUTFILE LENGTH 161.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_MATERIAL_INFO
*-----------------------------------------------------------------------
FORM GET_MATERIAL_INFO USING F_MATNR.
*--> Select material info from MARA

  SELECT SINGLE * FROM MARA WHERE MATNR = F_MATNR.
  MOVE MARA-MATNR              TO REC-MATNR.
  MOVE MARA-MATKL              TO REC-MATKL.
  WRITE MARA-MEINS             TO REC-MEINS.
*--> must use "write" rather than "move" for unit of measure
*--> internal interpretation takes place with the "write"
*   move mara-bstme              to rec-bstme.
  WRITE MARA-BSTME             TO REC-BSTME.

*--> convert to a Y/N indicator
  move 'N'                     to rec-hazard.
  IF  MARA-STOFF = 'HAZARD'.
    MOVE 'Y'                 TO REC-HAZARD.
  ENDIF.
*--> select material description from MAKT
  MOVE SPACE                    TO REC-MAKTX.
  SELECT SINGLE * FROM MAKT
      WHERE MATNR = MARA-MATNR AND
            SPRAS = SY-LANGU.
  IF  SY-SUBRC = '0'.
    MOVE MAKT-MAKTX       TO REC-MAKTX.
  ENDIF.

*--> get NEW_USED code from characteristics
  MOVE MARA-MATNR              TO OBJECT.
  MOVE 'NEW_USED'              TO CHARIC.
  MOVE SPACE                   TO REC-NEW_USED.

*--> select conversion factors from table MARM
  MOVE 1                  TO REC-UMREZ.
  MOVE 1                  TO REC-UMREN.
  SELECT SINGLE * FROM MARM
      WHERE MATNR = MARA-MATNR AND
            MEINH = MARA-BSTME.
  IF SY-SUBRC = 0.
    MOVE MARM-UMREZ  TO REC-UMREZ.
    MOVE MARM-UMREN  TO REC-UMREN.
  ENDIF.
*--> only need to check for characteristic 'NEW_USED' if material
*    group starts with '03'.
  IF MARA-MATKL(2) = '03'.
    PERFORM CHAR-NEW-USED.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM CHAR-NEW-USED
*-----------------------------------------------------------------------
*--> characteristic - NEW_USED
*-----------------------------------------------------------------------
FORM CHAR-NEW-USED.
  REFRESH CHAR_TAB.
  CALL FUNCTION 'CLFM_SELECT_AUSP'
       EXPORTING
            MAFID     = 'O'
            CLASSTYPE = '001'
            OBJECT    = OBJECT
       TABLES
            EXP_AUSP  = CHAR_TAB
       EXCEPTIONS
            NO_VALUES = 1
            OTHERS    = 2.
  IF  SY-SUBRC <> 0.
    MOVE SPACE                  TO REC-NEW_USED.
  ELSE.
    SORT CHAR_TAB BY ATINN.
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN BINARY SEARCH.
    CHECK SY-SUBRC = 0.
    MOVE CHAR_TAB-ATWRT      TO REC-NEW_USED.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  WRITE_REPORT
*&---------------------------------------------------------------------*
FORM WRITE_REPORT.

SORT TABLE1 BY WERKS  ASCENDING
            LGORT  ASCENDING
            MATKL  ASCENDING
            MATNR  ASCENDING.

LOOP AT TABLE1.
  AT NEW WERKS.                                 "Plant Name
     NEW-PAGE.
     SELECT SINGLE * FROM T001W WHERE WERKS = TABLE1-WERKS.
     WRITE: /1 TEXT-020, 19 TABLE1-WERKS COLOR COL_NEGATIVE INVERSE ON,
               TEXT-023, T001W-NAME1.
  ENDAT.
  AT NEW LGORT.                                 "Storage Location Name
     SELECT SINGLE * FROM T001L WHERE WERKS = TABLE1-WERKS
                                  AND LGORT = TABLE1-LGORT.
     WRITE: / TEXT-010 UNDER TEXT-020,
              19 TABLE1-LGORT COLOR COL_NEGATIVE
              INVERSE ON,
              TEXT-023 UNDER TEXT-023,
              T001L-LGOBE UNDER T001W-NAME1.
  ENDAT.
  AT NEW MATKL.
     SELECT SINGLE * FROM T023T WHERE MATKL = TABLE1-MATKL
                                  AND SPRAS = SY-LANGU.
     WRITE: / TEXT-004 UNDER TEXT-020,
              19 TABLE1-MATKL COLOR COL_NEGATIVE
              INVERSE ON,
              TEXT-023 UNDER TEXT-023,
              T023T-WGBEZ UNDER T001W-NAME1.
  ENDAT.
        WRITE: / TABLE1-QTYHAND UNDER TEXT-011.           "Quantity
        WRITE: TABLE1-VERPR   UNDER TEXT-012.               "AVG Price
        WRITE: TABLE1-VALUE COLOR COL_TOTAL UNDER TEXT-027. "Value
        WRITE: TABLE1-MATNR UNDER TEXT-006.                 "Material#
        WRITE: TABLE1-MAKTX UNDER TEXT-013.                 "Description
        WRITE: 132 TABLE1-LVORM.                    " del flag   MRadsma

  AT END OF MATKL.                                 "Material Group Total
     SUM.
        WRITE: /.
        ULINE: 62(16).
        WRITE: /20 TEXT-030, TABLE1-WERKS, TABLE1-LGORT, TABLE1-MATKL,
              TABLE1-VALUE UNDER TEXT-027 COLOR COL_KEY INTENSIFIED ON.
        WRITE: /.
  ENDAT.
  AT END OF LGORT.                                 "Storage Loc Total
     SUM.
        WRITE: /.
        ULINE: 62(16).
        WRITE: /20 TEXT-030, TABLE1-WERKS, TABLE1-LGORT,
              TABLE1-VALUE UNDER TEXT-027 COLOR COL_KEY INTENSIFIED ON.
        WRITE: /.
  ENDAT.
  AT END OF WERKS.                                  "Plant Total
     SUM.
        WRITE: /.
        ULINE: 62(16).
        WRITE: /20 TEXT-030, TABLE1-WERKS,
               TABLE1-VALUE UNDER TEXT-027 COLOR COL_KEY INTENSIFIED ON.
        WRITE: /.
  ENDAT.
  AT LAST.                                           "Report Total
     SUM.
        WRITE: /.
        ULINE: 62(16).
        WRITE: /20 TEXT-007,
               TABLE1-VALUE UNDER TEXT-027 COLOR COL_KEY INTENSIFIED ON.
  ENDAT.
ENDLOOP.

ENDFORM.                    " WRITE_REPORT
*&---------------------------------------------------------------------*
*&      Form  LOAD_TABLE1
*&---------------------------------------------------------------------*
FORM LOAD_TABLE1.

             TABLE1-VERPR   = MBEW-VERPR.
             TABLE1-MATKL   = ENT1027-MATKL.
             TABLE1-MAKTX   = ENT1027-MAKTX.
             TABLE1-WERKS   = MARD-WERKS.
             TABLE1-LGORT   = MARD-LGORT.
             TABLE1-MATNR   = MARC-MATNR.
             TABLE1-LVORM   = MARD-LVORM.

    APPEND TABLE1.
    CLEAR TABLE1.
ENDFORM.                    " LOAD_TABLE1
