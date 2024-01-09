REPORT ZMMMR043 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65
               MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMR043 - MM: Product Price History Report
*    Programmer  :  M DeMeester
*    Date        :  June 24, 1998
*
*    This ABAP will retrieve the Infomration records related to the
*    requested materials.
************************************************************************
* 98/06/24 md7140 #548 Product Price History Report
************************************************************************

*****************************  TABLES   ********************************

TABLES: EINA,         "Purchasing Info Record
        MARA,
        A018,        "Material Info Record - Validity Periods
        KONH,        "Condition Header
        KONP,        "Condition Items
        LFA1,        "Vendor table
        EKKO,        "Purchase Order Header
        EKPO,        "Purchase Order Items
        T023T.       "Material Group Description
*tables: mara,
*       mbew,               "Valuation table
*       t023t,              "Material Group Descriptions
*       lfa1,               "Vendor detail
*       stxh,
*       eine,
*       eina.                     "Purchasing Info Record - General Data

**************************  DATA ELEMENTS  *****************************
*-----------------------------------------------------------------------
DATA: BEGIN OF MAT_TABLE OCCURS 0,
       LIFNR            LIKE EINA-LIFNR,       "Vendor
       MATKL            LIKE MARA-MATKL,       "Material Group
       MATNR            LIKE MARA-MATNR,       "Material Number
       DATAB            LIKE A018-DATAB,       "Period Start Date
       DATBI            LIKE A018-DATBI,       "Period End Date
       KBETR            LIKE KONP-KBETR,       "Rate
       ERDAT            LIKE KONH-ERDAT,       "Condition Created Header
       MENGE            LIKE EKPO-MENGE,       "Quantity
     END OF MAT_TABLE.

*------ Characteristic Function Call Data Elements  --------------------
DATA:  OBJECT            LIKE AUSP-OBJEK,       "REQUIRED
       CHARIC            LIKE CABN-ATNAM,       "REQUIRED
       G_ATINN           LIKE CABN-ATINN,       "REQUIRED
       G_ATINN_PRIMARY   LIKE CABN-ATINN,       "Primary Characteristic
       G_ATINN_SECONDARY LIKE CABN-ATINN.       "Secondary Character

DATA: BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.

DATA: BEGIN OF CHARTAB OCCURS 20,
       MATNR         LIKE MARA-MATNR,
       INFO          LIKE AUSP-ATWRT,
       IND(3)        TYPE C.
DATA: END OF CHARTAB.


*---------- READ_TEXT  Function Call Data Elements  --------------------
*data:   begin of theadtab occurs 100.
*        include structure thead.
*dta:   end of theadtab.

*ata:   begin of tinlinetab occurs 100. "Aktuelle Inlinezeile
*       include structure tline.
*ata:   end   of tinlinetab.
*-----------------------------------------------------------------------
DATA: PRI_LGTH        LIKE AUSP-ATWRT,          "Primary Length
      SEC_LGTH        LIKE AUSP-ATWRT,         "Secondary Length
      TEMP            LIKE KONP-KBETR,
      CHANGE          LIKE KONP-KBETR,
      TEMP_MENGE       LIKE EKPO-MENGE,       "Quantity
      FLAG(1)          TYPE C.                "Print flag
*     infnr            like eina-infnr,       "Info Record Id
*     idnlf            like eina-idnlf,       "Vendor Material Number
*     name1            like lfa1-name1,       "Vendor name
*     tdname           like stxh-tdname,      "Concatentate info
*     auth(1)  type c,
*---------------------- Work Area --------------------------------------
*     matkl           like mara-matkl,
*    qoh           like mbew-lbkum,            "Qty on Hand
*      value         like mbew-salk3,            "Dollar Value
*     sloc_count(2) type p,                     "Storage Loc Count
*     aup_total     like mbew-verpr,            "Total Average Price
*     aup           like mbew-verpr.            "Total Average Price

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-014.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS: S_LIFNR FOR EINA-LIFNR,               "Vendor
                S_MATKL FOR MARA-MATKL,               "Material Group
                S_MATNR FOR MARA-MATNR.               "Material
PARAMETERS:     P_DATUM LIKE A018-DATAB.              "Validity Date
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
       50 TEXT-HDG,                                             "Title
      102 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT,
         SY-MANDT UNDER SY-REPID, SY-SYSID,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
WRITE: /.
FORMAT COLOR COL_NORMAL.
WRITE: /1 TEXT-001, 18 MAT_TABLE-LIFNR, TEXT-DSH,               "Vendor
           LFA1-NAME1.
WRITE: / TEXT-002 UNDER TEXT-001,                               "Mat Grp
         MAT_TABLE-MATKL UNDER MAT_TABLE-LIFNR,
         TEXT-DSH UNDER TEXT-DSH,
         T023T-WGBEZ UNDER LFA1-NAME1.
ULINE.
WRITE: /72 TEXT-005, 102 TEXT-009, 124 TEXT-012.
WRITE: /1 TEXT-003,  10 TEXT-004, 72 TEXT-006, 83 TEXT-007,
      94 TEXT-008, 113 TEXT-011,
         TEXT-010 UNDER TEXT-009, TEXT-013 UNDER TEXT-012.
WRITE: / SY-ULINE(8)  UNDER TEXT-003,
         SY-ULINE(11) UNDER TEXT-004,
         SY-ULINE(10) UNDER TEXT-006,
         SY-ULINE(10) UNDER TEXT-007,
         SY-ULINE(06) UNDER TEXT-008,
         SY-ULINE(10) UNDER TEXT-009,
         SY-ULINE(09) UNDER TEXT-011,
         SY-ULINE(10) UNDER TEXT-012.

***************************  MAIN ROUTINE  *****************************



START-OF-SELECTION.
*----------------- obtain characteristics object numbers ---------------
MOVE 'PRIMARY_DESCRIPTION'       TO CHARIC.   "Characteristics Required
PERFORM GET_ATINN.
MOVE G_ATINN                     TO G_ATINN_PRIMARY.

MOVE 'SECONDARY_DESCRIPTION'     TO CHARIC.
PERFORM GET_ATINN.
MOVE G_ATINN                     TO G_ATINN_SECONDARY.


*-------------- read material master for a material group --------------
SELECT * FROM EINA                       "Select all active Info Records
  WHERE LIFNR IN S_LIFNR AND             "which satisfy select criteria
        MATNR IN S_MATNR AND
        LOEKZ <> 'X'.
     PERFORM UPDATE_MAT_TABLE.
ENDSELECT.                               "End of EINA select

END-OF-SELECTION.

SORT MAT_TABLE BY LIFNR MATKL MATNR DATAB.

PERFORM DISPLAY_TABLE.
WRITE: / TEXT-END UNDER TEXT-HDG.

*--------------------------  UPDATE_MAT_TABLE  -------------------------
FORM UPDATE_MAT_TABLE.
  MAT_TABLE-LIFNR = EINA-LIFNR.
  MAT_TABLE-MATNR = EINA-MATNR.

  SELECT SINGLE * FROM MARA
    WHERE MATNR = EINA-MATNR
      AND MATKL IN S_MATKL.

  IF SY-SUBRC = '0'.                      "Must satisfy MatGrp selection
     MAT_TABLE-MATKL = MARA-MATKL.
     SELECT * FROM A018                   "Could be several periods
        WHERE KAPPL = 'M '
          AND KSCHL = 'ZZZZ'
          AND LIFNR = EINA-LIFNR
          AND MATNR = EINA-MATNR.
       MAT_TABLE-DATBI = A018-DATBI.
       MAT_TABLE-DATAB = A018-DATAB.

       SELECT SINGLE * FROM KONP                   "Rate
          WHERE KNUMH = A018-KNUMH.
       IF SY-SUBRC = 0.
          MAT_TABLE-KBETR = KONP-KBETR.
       ENDIF.

       SELECT SINGLE * FROM KONH                   "Created Date
          WHERE KNUMH = A018-KNUMH.
       IF SY-SUBRC = 0.
          MAT_TABLE-ERDAT = KONH-ERDAT.
       ENDIF.

       CLEAR TEMP_MENGE.
       SELECT * FROM EKKO
          WHERE LIFNR = MAT_TABLE-LIFNR
            AND BEDAT BETWEEN A018-DATAB AND A018-DATBI.
          SELECT * FROM EKPO
             WHERE EBELN = EKKO-EBELN
               AND MATNR = EINA-MATNR
               AND LOEKZ <> 'X'.
             ADD EKPO-MENGE TO TEMP_MENGE.
          ENDSELECT.
       ENDSELECT.
       MAT_TABLE-MENGE = TEMP_MENGE.

       IF P_DATUM = SPACE.
          APPEND MAT_TABLE.
       ELSEIF MAT_TABLE-DATBI >= P_DATUM.
          APPEND MAT_TABLE.
       ENDIF.
      ENDSELECT.                          "End of A018 Select
   ENDIF.                                 "End of MatGrp sy-subrc test
ENDFORM.


*select * from mara
*    where matkl in s_matkl
*      and matnr in s_matnr
*      and lvorm <> 'X'.
*      select * from eina
*         where matnr = mara-matnr
*           and lifnr in s_lifnr
*          and loekz =' '.                      "Eliminate flagged delet
*        select single * from lfa1              "Get non-blocked vendors
*           where lifnr = eina-lifnr
*             and sperm <> 'X'.
*          if sy-subrc = '0'.
*             move mara-matnr to mat_table-matnr.
*             move mara-matkl to mat_table-matkl.
*             move eina-lifnr to mat_table-lifnr.
*             move eina-infnr to mat_table-infnr.
*             move eina-idnlf to mat_table-idnlf.
*             move lfa1-name1 to mat_table-name1.
*             append mat_table.
*          endif.
*    endselect.
*endselect.

FORM DISPLAY_TABLE.
 CLEAR TEMP.
 LOOP AT MAT_TABLE.
  AT NEW LIFNR.
     NEW-PAGE.
     SELECT SINGLE * FROM LFA1                 "Get non-blocked vendors
        WHERE LIFNR = MAT_TABLE-LIFNR.
     CLEAR: TEMP, CHANGE.
  ENDAT.

  AT NEW MATKL.
     NEW-PAGE.
     SELECT SINGLE * FROM T023T                 "Get MatGrp Description
        WHERE MATKL = MAT_TABLE-MATKL
          AND SPRAS = SY-LANGU.
     CLEAR: TEMP, CHANGE.
  ENDAT.

  AT NEW MATNR.
    OBJECT = MAT_TABLE-MATNR.                        "Get Characteristic
    PERFORM FIND_CHARACTERISTIC.
    FLAG = 'X'.
    CLEAR: TEMP, CHANGE.
  ENDAT.

  IF TEMP <> 0.
     CHANGE = ( MAT_TABLE-KBETR - TEMP ) * 100 / TEMP.
  ENDIF.
  WRITE: / MAT_TABLE-DATAB UNDER TEXT-006,           "Period From Date
           MAT_TABLE-DATBI UNDER TEXT-007,           "Period To Date
       (7) MAT_TABLE-KBETR UNDER TEXT-008,           "Rate
           MAT_TABLE-ERDAT UNDER TEXT-009,           "Date Created
       (6) CHANGE UNDER TEXT-011 NO-GAP NO-ZERO DECIMALS 0, "Change
           TEXT-PCT,                                 "Percent sign
      (10) MAT_TABLE-MENGE UNDER TEXT-012 DECIMALS 0."PO Qty

  IF FLAG = 'X'.       "Prints material/desc only on first line of group
    WRITE: MAT_TABLE-MATNR UNDER TEXT-003,           "Material Number
           PRI_LGTH UNDER TEXT-004 NO-GAP, SEC_LGTH. "Description
    CLEAR FLAG.
  ENDIF.

  AT END OF MATNR.
    WRITE: /.
  ENDAT.

  TEMP = MAT_TABLE-KBETR.
 ENDLOOP.
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
 ENDIF.
 CLEAR: OBJECT.
ENDFORM.

*------------------  PRINT_MATERIAL ------------------------------------
FORM DISPLAY_MAT_TABLE.

LOOP AT MAT_TABLE.
* move mat_table-infnr to infnr.                 "Work fields for report
* move mat_table-idnlf to idnlf.
* move mat_table-name1 to name1.

  AT NEW LIFNR.
     NEW-PAGE.
*    move mat_table-matkl to matkl.
*    select single * from t023t
*       where matkl = matkl
*         and spras = sy-langu.
  ENDAT.

  AT NEW MATKL.
     NEW-PAGE.
*    move mat_table-matkl to matkl.
*    select single * from t023t
*       where matkl = matkl
*         and spras = sy-langu.
  ENDAT.

 AT NEW MATNR.
    OBJECT = MAT_TABLE-MATNR.                        "Get Characteristic
    PERFORM FIND_CHARACTERISTIC.


    ULINE.

*   write: mat_table-matnr  under text-002,              "Material Info
*          pri_lgth         under text-003 no-gap, sec_lgth,
*     (12) qoh              under text-004,
*     (12) aup              under text-006.

*   loop at chartab.
*     write: chartab-info   under text-008, chartab-ind.
*     perform print_vert.
*   endloop.
*   if sy-tfill = 0.
*      perform print_vert.
*   endif.

*  perform print_vert.                                  "Vendor Headings
*  write: 11 text-009,  25 text-010, 60 text-011, 75 text-012,
*         95 text-013, 125 text-014.
*  perform print_vert.
*  write: sy-uline(6)  under text-009, sy-uline(11) under text-010,
*         sy-uline(12) under text-011, sy-uline(13) under text-012,
*         sy-uline(23) under text-013, sy-uline(20) under text-014.
 ENDAT.

*at new lifnr.
*   perform print_vert.
*   perform print_vert.
*   write: mat_table-lifnr under text-009,   "Vendor number
*                    infnr under text-011,   "Info record number
*                    idnlf under text-013,   "Vendor Material Number
*                    name1 under text-010.   "Vendor Name
*
*   select single * from eine                "Vendor Price
*      where infnr = infnr.
*      clear tdname.
*   if sy-subrc = 0.
*      perform authority_check.
*      if auth = 'X'.
*         write: eine-effpr under text-012.
*      endif.
*      concatenate infnr eine-ekorg '0' into tdname.
*   endif.

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
 WRITE: /1 SY-VLINE, 132 SY-VLINE.
ENDFORM.

*  if sy-subrc <> 0.                 " ==> no info records
*     perform print_vert.
*     perform print_vert.
*     write: 50 text-016.
*  endif.

*---------------------- AUTHORITY_CHECK  -------------------------------
*orm authority_check.
*&move 'X'       to auth.
* authority-check object 'M_EINF_EKG'
*   id 'ACTVT' field '03'
*   id 'EKGRP' field eine-ekgrp.
* if sy-subrc <> '0'.
*    clear auth.
* endif.

* authority-check object 'M_EINF_EKO'
*   id 'ACTVT' field '03'
*   id 'EKORG' field eine-ekorg.
* if sy-subrc <> '0'.
*    clear auth.
* endif.
*
* authority-check object 'M_EINF_WRK'
*   id 'ACTVT' field '03'
*   id 'WERKS' field eine-werks.
* if sy-subrc <> '0'.
*    clear auth.
* endif.


* authority-check object 'V_KONH_VKS'
*   id 'ACTVT' field '03'
*   id 'KSCHL' field '*'.
* if sy-subrc <> '0'.
*    clear auth.
* endif.
*endform.


***************************  END OF PROGRAM  ***************************
