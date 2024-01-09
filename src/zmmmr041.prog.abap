REPORT ZMMMR041 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58
               MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMR041 - MM: Infomration Record Text Audit Report
*    Programmer  :  M DeMeester
*    Date        :  May 14, 1998
*
*    This ABAP will retrieve the Infomration records relating to the
*    requested materials.  This program also calculates the company-wide
*    QOH and AUP for each material found.
************************************************************************
* 98/06/12 md7140 #--- Do not display info records flagged for deletion
* 98/05/14 md7140 #516 Information Record text Audit Report
************************************************************************

*****************************  TABLES   ********************************

TABLES: MARA,
        MBEW,               "Valuation table
        T023T,              "Material Group Descriptions
        LFA1,               "Vendor detail
        STXH,
        EINE,
        EINA.                     "Purchasing Info Record - General Data

**************************  DATA ELEMENTS  *****************************
*-----------------------------------------------------------------------
DATA: BEGIN OF MAT_TABLE OCCURS 0,
        MATKL            LIKE MARA-MATKL,       "Material Group
        MATNR            LIKE MARA-MATNR,       "Material Number
        LIFNR            LIKE EINA-LIFNR,       "Vendor
        INFNR            LIKE EINA-INFNR,       "Info Record Id
        IDNLF            LIKE EINA-IDNLF,       "Vendor Material Number
        NAME1            LIKE LFA1-NAME1,       "Vendor name
      END OF MAT_TABLE.
*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_PRIMARY   LIKE CABN-ATINN,       "Primary Characteristic
       G_ATINN_SECONDARY LIKE CABN-ATINN,       "Secondary Character
       G_ATINN_MODEL     LIKE CABN-ATINN,       "Model Number
       G_ATINN_PARTNO    LIKE CABN-ATINN.       "Mfg Part Number

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

DATA: BEGIN OF CHARTAB OCCURS 20,
        MATNR         LIKE MARA-MATNR,
        INFO          LIKE AUSP-ATWRT,
        IND(3)        TYPE C.
DATA: END OF CHARTAB.


*---------- READ_TEXT  Function Call Data Elements  --------------------
DATA:   BEGIN OF THEADTAB OCCURS 100.
         INCLUDE STRUCTURE THEAD.
DATA:   END OF THEADTAB.

DATA:   BEGIN OF TINLINETAB OCCURS 100. "Aktuelle Inlinezeile
         INCLUDE STRUCTURE TLINE.
DATA:   END   OF TINLINETAB.
*-----------------------------------------------------------------------
DATA: PRI_LGTH        LIKE AUSP-ATWRT,          "Primary Length
      SEC_LGTH        LIKE AUSP-ATWRT,          "Secondary Length
      INFNR            LIKE EINA-INFNR,       "Info Record Id
      IDNLF            LIKE EINA-IDNLF,       "Vendor Material Number
      NAME1            LIKE LFA1-NAME1,       "Vendor name
      TDNAME           LIKE STXH-TDNAME,      "Concatentate info
      AUTH(1)  TYPE C,
*---------------------- Work Area --------------------------------------
      MATKL           LIKE MARA-MATKL,
      QOH           LIKE MBEW-LBKUM,            "Qty on Hand
      VALUE         LIKE MBEW-SALK3,            "Dollar Value
      SLOC_COUNT(2) TYPE P,                     "Storage Loc Count
      AUP_TOTAL     LIKE MBEW-VERPR,            "Total Average Price
      AUP           LIKE MBEW-VERPR.            "Total Average Price

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS: S_MATKL FOR MARA-MATKL,
                S_MATNR FOR MARA-MATNR,
                S_LIFNR FOR EINA-LIFNR.
SELECTION-SCREEN END OF BLOCK BOX1.

AT SELECTION-SCREEN.
  IF ( S_MATKL IS INITIAL ) AND
     ( S_MATNR IS INITIAL ).
     MESSAGE E100 WITH
         'Please enter Material, Material Group or both'.
  ENDIF.
*--------------------  TOP-OF-PAGE  ------------------------------------
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_GROUP,
        65 TEXT-HDG,                                          "Title
      140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT,
         SY-MANDT UNDER SY-REPID, SY-SYSID,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
WRITE: /.
FORMAT COLOR COL_NORMAL.
WRITE: /2 TEXT-015, MATKL, TEXT-DSH, T023T-WGBEZ.        "Material Group
***************************  MAIN ROUTINE  *****************************



START-OF-SELECTION.
*----------------- obtain characteristics object numbers ---------------
MOVE 'PRIMARY_DESCRIPTION'       TO CHARIC.   "Characteristics Required
PERFORM GET_ATINN.
MOVE G_ATINN                     TO G_ATINN_PRIMARY.

MOVE 'SECONDARY_DESCRIPTION'     TO CHARIC.
PERFORM GET_ATINN.
MOVE G_ATINN                     TO G_ATINN_SECONDARY.

MOVE 'MODEL_NUMBER'              TO CHARIC.
PERFORM GET_ATINN.
MOVE G_ATINN                     TO G_ATINN_MODEL.

MOVE 'MANUFACTURER_PART_NUMBER'  TO CHARIC.
PERFORM GET_ATINN.
MOVE G_ATINN                     TO G_ATINN_PARTNO.


*-------------- read material master for a material group --------------
SELECT * FROM MARA
    WHERE MATKL IN S_MATKL
      AND MATNR IN S_MATNR
      AND LVORM <> 'X'.
      SELECT * FROM EINA
         WHERE MATNR = MARA-MATNR
           AND LIFNR IN S_LIFNR
           AND LOEKZ =' '.                      "Eliminate flagged delet
         SELECT SINGLE * FROM LFA1              "Get non-blocked vendors
            WHERE LIFNR = EINA-LIFNR
              AND SPERM <> 'X'.
           IF SY-SUBRC = '0'.
              MOVE MARA-MATNR TO MAT_TABLE-MATNR.
              MOVE MARA-MATKL TO MAT_TABLE-MATKL.
              MOVE EINA-LIFNR TO MAT_TABLE-LIFNR.
              MOVE EINA-INFNR TO MAT_TABLE-INFNR.
              MOVE EINA-IDNLF TO MAT_TABLE-IDNLF.
              MOVE LFA1-NAME1 TO MAT_TABLE-NAME1.
              APPEND MAT_TABLE.
           ENDIF.
     ENDSELECT.
ENDSELECT.
END-OF-SELECTION.

SORT MAT_TABLE BY MATKL MATNR LIFNR.
PERFORM DISPLAY_MAT_TABLE.

WRITE: / TEXT-017 UNDER TEXT-HDG.



*----------------------  CALCULATE_QOH_AUP  ----------------------------
FORM CALCULATE_QOH_AUP.
 CLEAR: QOH, VALUE, SLOC_COUNT, AUP_TOTAL.
 SELECT * FROM MBEW
   WHERE MATNR = MAT_TABLE-MATNR.
   QOH   = QOH + MBEW-LBKUM.
   VALUE = VALUE + MBEW-SALK3.
   IF MBEW-VERPR > 0.
      AUP_TOTAL  = AUP_TOTAL + MBEW-VERPR.
      SLOC_COUNT = SLOC_COUNT + 1.
    ENDIF.
 ENDSELECT.

 IF VALUE > 0.
    AUP = VALUE / QOH.
 ELSE.
    AUP = AUP_TOTAL / SLOC_COUNT.
 ENDIF.
ENDFORM.



*****************************  SUB-ROUTINES  ***************************

*----------------------------  GET_ATINN -------------------------------
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

*----------------------- FIND_CHARACTERISTIC ---------------------------
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
* Character values are in "ATWRT", (numeric values would be in "ATFLV")
 CLEAR: PRI_LGTH, SEC_LGTH.
 REFRESH: CHARTAB.
 IF SY-SUBRC EQ 0.
    SORT CHAR_TAB BY ATINN.
* primary length                            (only one)
    READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_PRIMARY BINARY SEARCH.
    IF SY-SUBRC EQ 0.
       MOVE CHAR_TAB-ATWRT TO PRI_LGTH.
    ENDIF.
* secondary length                          (only one)
   READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN_SECONDARY BINARY SEARCH.
    IF SY-SUBRC EQ 0 AND CHAR_TAB-ATWRT <> 'N/A'.
       MOVE CHAR_TAB-ATWRT TO SEC_LGTH.
    ENDIF.
* manufacturing part number & model number  (could be several)
   LOOP AT CHAR_TAB.
     IF CHAR_TAB-ATINN = G_ATINN_PARTNO.      " ==> mfg part no.
        MOVE MARA-MATNR     TO CHARTAB-MATNR.
        MOVE CHAR_TAB-ATWRT TO CHARTAB-INFO.
        MOVE '(1)'          TO CHARTAB-IND.
        APPEND CHARTAB.
     ENDIF.
     IF CHAR_TAB-ATINN = G_ATINN_MODEL.       " ==> model number
        MOVE MARA-MATNR     TO CHARTAB-MATNR.
        MOVE CHAR_TAB-ATWRT TO CHARTAB-INFO.
        MOVE '(2)'          TO CHARTAB-IND.
        APPEND CHARTAB.
     ENDIF.
   ENDLOOP.
 ENDIF.
 CLEAR: OBJECT.
ENDFORM.

*------------------  PRINT_MATERIAL ------------------------------------
FORM DISPLAY_MAT_TABLE.

LOOP AT MAT_TABLE.
  MOVE MAT_TABLE-INFNR TO INFNR.                 "Work fields for report
  MOVE MAT_TABLE-IDNLF TO IDNLF.
  MOVE MAT_TABLE-NAME1 TO NAME1.

  AT NEW MATKL.
     NEW-PAGE.
     MOVE MAT_TABLE-MATKL TO MATKL.
     SELECT SINGLE * FROM T023T
        WHERE MATKL = MATKL
          AND SPRAS = SY-LANGU.
  ENDAT.

 AT NEW MATNR.
    IF SY-LINNO > 50.
       NEW-PAGE.
    ENDIF.
    OBJECT = MAT_TABLE-MATNR.                        "Get Characteristic
    PERFORM FIND_CHARACTERISTIC.

    PERFORM CALCULATE_QOH_AUP.                       "Average Unit Price

    ULINE.
    PERFORM PRINT_VERT.
    WRITE: 2 TEXT-002,  11 TEXT-003, 80 TEXT-004,    "Material Headings
         100 TEXT-006, 125 TEXT-008.
    PERFORM PRINT_VERT.
    WRITE: SY-ULINE(8) UNDER TEXT-002,
           SY-ULINE(11) UNDER TEXT-003,
           SY-ULINE(11) UNDER TEXT-004,
           SY-ULINE(11) UNDER TEXT-006,
           SY-ULINE(20) UNDER TEXT-008.
    PERFORM PRINT_VERT.
    PERFORM PRINT_VERT.

    WRITE: MAT_TABLE-MATNR  UNDER TEXT-002,              "Material Info
           PRI_LGTH         UNDER TEXT-003 NO-GAP, SEC_LGTH,
      (12) QOH              UNDER TEXT-004,
      (12) AUP              UNDER TEXT-006.

    LOOP AT CHARTAB.
      WRITE: CHARTAB-INFO   UNDER TEXT-008, CHARTAB-IND.
      PERFORM PRINT_VERT.
    ENDLOOP.
    IF SY-TFILL = 0.
       PERFORM PRINT_VERT.
    ENDIF.

   PERFORM PRINT_VERT.                                  "Vendor Headings
   WRITE: 11 TEXT-009,  25 TEXT-010, 60 TEXT-011, 75 TEXT-012,
          95 TEXT-013, 125 TEXT-014.
   PERFORM PRINT_VERT.
   WRITE: SY-ULINE(6)  UNDER TEXT-009, SY-ULINE(11) UNDER TEXT-010,
          SY-ULINE(12) UNDER TEXT-011, SY-ULINE(13) UNDER TEXT-012,
          SY-ULINE(23) UNDER TEXT-013, SY-ULINE(20) UNDER TEXT-014.
 ENDAT.

 AT NEW LIFNR.
    PERFORM PRINT_VERT.
    PERFORM PRINT_VERT.
    WRITE: MAT_TABLE-LIFNR UNDER TEXT-009,   "Vendor number
                     INFNR UNDER TEXT-011,   "Info record number
                     IDNLF UNDER TEXT-013,   "Vendor Material Number
                     NAME1 UNDER TEXT-010.   "Vendor Name

    SELECT SINGLE * FROM EINE                "Vendor Price
       WHERE INFNR = INFNR.
       CLEAR TDNAME.
    IF SY-SUBRC = 0.
       PERFORM AUTHORITY_CHECK.
       IF AUTH = 'X'.
          WRITE: EINE-EFFPR UNDER TEXT-012.
       ENDIF.
       CONCATENATE INFNR EINE-EKORG '0' INTO TDNAME.
    ENDIF.

    PERFORM GET_INFO_TEXT.
    IF SY-SUBRC = 0.                                   "Print Info text
       LOOP AT TINLINETAB.
         WRITE: (40) TINLINETAB-TDLINE UNDER TEXT-014.
         PERFORM PRINT_VERT.
         IF TINLINETAB-TDLINE+40(40) <> SPACE.
            WRITE: TINLINETAB-TDLINE+40(40) UNDER TEXT-014.
            PERFORM PRINT_VERT.
         ENDIF.
       ENDLOOP.
    ENDIF.
 ENDAT.
* if sy-subrc <> 0.                 " ==> no info records
*    perform print_vert.
*    perform print_vert.
*    write: 50 text-016.
* endif.


 AT END OF MATNR.
    ULINE.
 ENDAT.

ENDLOOP.
ENDFORM.

FORM PRINT_VERT.
 WRITE: /1 SY-VLINE, 170 SY-VLINE.
ENDFORM.

*  if sy-subrc <> 0.                 " ==> no info records
*     perform print_vert.
*     perform print_vert.
*     write: 50 text-016.
*  endif.

FORM GET_INFO_TEXT.
  SELECT * FROM STXH
    WHERE TDOBJECT = 'EINE'
      AND TDNAME = TDNAME
      AND TDID = 'BT  '
      AND TDSPRAS = SY-LANGU.
  IF SY-SUBRC = '0'.
     MOVE-CORRESPONDING STXH TO THEADTAB.
     APPEND THEADTAB.
     CALL FUNCTION 'READ_TEXT'
       EXPORTING
           ID                           = THEADTAB-TDID
           LANGUAGE                     = THEADTAB-TDSPRAS
           NAME                         = THEADTAB-TDNAME
           OBJECT                       = THEADTAB-TDOBJECT
       IMPORTING HEADER                 = THEADTAB
       TABLES LINES                     = TINLINETAB
       EXCEPTIONS
           ID                           = 1
           LANGUAGE                     = 2
           NAME                         = 3
           NOT_FOUND                    = 4
           OBJECT                       = 5
           OTHERS                       = 6.
  ENDIF.
 IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO process info text records', SY-SUBRC.
 ENDIF.

  ENDSELECT.

ENDFORM.
*---------------------- AUTHORITY_CHECK  -------------------------------
FORM AUTHORITY_CHECK.
  MOVE 'X'       TO AUTH.
  AUTHORITY-CHECK OBJECT 'M_EINF_EKG'
    ID 'ACTVT' FIELD '03'
    ID 'EKGRP' FIELD EINE-EKGRP.
  IF SY-SUBRC <> '0'.
     CLEAR AUTH.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'M_EINF_EKO'
    ID 'ACTVT' FIELD '03'
    ID 'EKORG' FIELD EINE-EKORG.
  IF SY-SUBRC <> '0'.
     CLEAR AUTH.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'M_EINF_WRK'
    ID 'ACTVT' FIELD '03'
    ID 'WERKS' FIELD EINE-WERKS.
  IF SY-SUBRC <> '0'.
     CLEAR AUTH.
  ENDIF.

* authority-check object 'S_SCD0'      "TOO RESTRICTIVE
*   id 'ACTVT' field '08'.
* if sy-subrc <> '0'.
*    clear auth.
* endif.

  AUTHORITY-CHECK OBJECT 'V_KONH_VKS'
    ID 'ACTVT' FIELD '03'
    ID 'KSCHL' FIELD '*'.
  IF SY-SUBRC <> '0'.
     CLEAR AUTH.
  ENDIF.
ENDFORM.


***************************  END OF PROGRAM  ***************************
