REPORT ZMMMR058 NO STANDARD PAGE HEADING.
*
************************************************************************
*  Author:      Mohammad T. Khan
*  Date:        February 2003
*  Description:
*     - The purpose of this program is to produce the
*       Delivery/Lead Time Summary by Plant/Material Group.
*       This program cloned from ZMMMR038.
************************************************************************
* 2012/09/04 M Khan   TR995 Change C: drive to H: drive with directory,
*                           file selection using F4 & move the
*                           hard-coded file path/name to variant.
*
* 2006/06/23 mdemeest 4.7 - fix excel spreadsheet columns
*-----------------------------------------------------------------------
TABLES: LFA1,                          "Vendor master (general section)
        MARC,                          "Material Master: C Segment
        MAKT,                          "Material Descriptions
        S012,                          "PURCHIS: Purchasing Statistics
       T001W,                          "Plants/Locations
       T023T,                          "Material Group Descriptions
        EKPO,                          "Purchasing Document Item
        EKKO,                          "Purchasing Document Header
        MSEG,                          "Document Segment: Material
        EINA,                          "Purchasing Info Record: Gen Data
        EINE.                          "Purchasing Info Record: Org Data
DATA:
    BEGIN OF SAVE_TABLE OCCURS 0,
       WERKS        LIKE S012-WERKS,   "Plant
       MATKL        LIKE S012-MATKL,   "Material Group
       MATNR        LIKE S012-MATNR,   "Material
       LIFNR        LIKE S012-LIFNR,   "Vendor
       ALIEF        LIKE S012-ALIEF,   "No of Deliveries
       LFZTA        LIKE S012-LFZTA,   "Amount
       MEANS        TYPE I,
   END OF SAVE_TABLE.

DATA:
    BEGIN OF BIG_TABLE OCCURS 10000,
       WERKS        LIKE S012-WERKS,   "Plant
       MATKL        LIKE S012-MATKL,   "Material Group
       MATNR        LIKE S012-MATNR,   "Material
       LIFNR        LIKE S012-LIFNR,   "Vendor
       SPMON        LIKE S012-SPMON,   "Date
       ALIEF        LIKE S012-ALIEF,   "No of Deliveries
       LFZTA        LIKE S012-LFZTA,   "Amount
       MEANS        TYPE I,            "Mean Delivery
       MAKTX        LIKE MAKT-MAKTX,   "Description
   END OF BIG_TABLE.

DATA:
    BEGIN OF SUM_TABLE OCCURS 0,
       WERKS        LIKE S012-WERKS,   "Plant
       MATKL        LIKE S012-MATKL,   "Material Group
       MATNR        LIKE S012-MATNR,   "Material
       ALIEF        LIKE S012-ALIEF,   "No of Deliveries
       LFZTA        LIKE S012-LFZTA,   "Amount
       MEANS        TYPE I,            "Mean Delivery
       PLIFZ        LIKE MARC-PLIFZ,   "Lead Time
       APLFZ        LIKE EINE-APLFZ,   "I/R Lead Time
       DIFF         TYPE I,
       ABSDIFF      TYPE I,
       MAKTX        LIKE MAKT-MAKTX,   "Description
       EKGRP        LIKE MARC-EKGRP,   "Buyer
   END OF SUM_TABLE.

DATA:
    BEGIN OF EXCLTAB OCCURS 0,
       WERKS        LIKE S012-WERKS,   "Plant
       PNAME        LIKE T001W-NAME1,  "Plant Description
       MATKL        LIKE S012-MATKL,   "Material Group
       WGBEZ        LIKE T023T-WGBEZ,  "Material Group Description
       MATNR        LIKE S012-MATNR,   "Material
       MAKTX        LIKE MAKT-MAKTX,   "Material Description
       EKGRP        LIKE MARC-EKGRP,   "Buyer
       LIFNR        LIKE S012-LIFNR,   "Vendor Acctount #
       VNAME        LIKE LFA1-NAME1,   "Vendor Name
       COUNTPO(4)   TYPE C,            "Orders
       MEANS(9)     TYPE C,            "Mean Delivery
       PLIFZ(12)     TYPE C,            "MM Lead Time
       APLFZ(12)     TYPE C,            "I/R Lead Time
       DIFF(4)      TYPE C,
   END OF EXCLTAB.

DATA:  LIFNRDESC    LIKE LFA1-NAME1,
       TEMPWERKS    LIKE S012-WERKS,
       TEMPMATKL    LIKE S012-MATKL,
       TEMPMATNR    LIKE S012-MATNR,
       TEMPLIFNR    LIKE S012-LIFNR,
       TEMPAPLFZ    LIKE EINE-APLFZ,
       LIFNRLFZTA   LIKE S012-LFZTA,
       LIFNRALIEF   LIKE S012-ALIEF,
       MATNRLFZTA   LIKE S012-LFZTA,
       MATNRALIEF   LIKE S012-ALIEF,
       FLAG TYPE C VALUE 0,
       COUNTER      TYPE I,
       FLAGPRT(3)   TYPE C,
       RETCODE      LIKE SY-SUBRC,
       W_OPTION(11) TYPE C VALUE 'START_EXCEL',
       W_TEXT1(07)  TYPE C,
       W_TEXT2(07)  TYPE C,
       W_HEAD01(23) TYPE C,
       W_HEAD02(40) TYPE C,
       W_REPTTL    LIKE SY-TITLE,          "Report Title
       W_DLVPERIOD  LIKE S012-SPMON,
       COUNTER2     TYPE I,                "         # po
       COUNTER3     TYPE I.                "         # receipts

* Two tables for counting purposses.
DATA: BEGIN OF COUNTAB1 OCCURS 50,
       MBLNR LIKE MSEG-MBLNR,
       MJAHR LIKE MSEG-MJAHR,
       ZEILE LIKE MSEG-ZEILE,          "Item in material document
       BWART LIKE MSEG-BWART,          "Movement Type
       MENGE LIKE MSEG-MENGE.          "Quantity
DATA: END OF COUNTAB1.

DATA: BEGIN OF COUNTAB2 OCCURS 50.
        INCLUDE STRUCTURE COUNTAB1.
DATA: END OF COUNTAB2.

DATA: BEGIN OF PROT_HEADER OCCURS 1,
        SPALTENNAME(20) TYPE C,
        DDIC_TABLE(5) TYPE C,
        DDIC_FIELD(5) TYPE C,
        KEY TYPE C,
      END OF PROT_HEADER.

DATA  ERRORTAB    LIKE HRERROR    OCCURS 0 WITH HEADER LINE.
*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS:  SWERKS FOR S012-WERKS OBLIGATORY,      "Plant
                 SMATKL FOR S012-MATKL,                 "Material Group
                 SSPMON FOR S012-SPMON OBLIGATORY,      "Selected Period
                 SLIFNR FOR S012-LIFNR,                 "Vendor Acct #
                 SMATNR FOR S012-MATNR.                 "Material Number
PARAMETERS:      P_DAYS TYPE I.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,          "Print Report
                P_EXCL RADIOBUTTON GROUP RBCR,          "Excel Sheet
                P_FILE LIKE RLGRAP-FILENAME DEFAULT 'H:\SAPTEMP'. "TR995

SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN END OF BLOCK BOX.

******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      P_FILE = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILE.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON P_FILE.
   PERFORM CHECK_FILE_PATH.
*End of TR995 changes
*-------------------  START-OF-SELECTION  ------------------------------
* select all records from S012 table that satisfy selection criteria
*-----------------------------------------------------------------------
START-OF-SELECTION.

  SELECT WERKS MATKL MATNR LIFNR ALIEF LFZTA
    INTO TABLE SAVE_TABLE
    FROM S012
   WHERE SPMON IN SSPMON             "Select Period (yyyy/mm)
     AND LIFNR IN SLIFNR             "Vendor Acct. #
     AND MATNR IN SMATNR                                    "Material
     AND MATNR NE SPACE              "Eliminate NIS items
     AND WERKS IN SWERKS                                    "Plant
     AND MATKL IN SMATKL             "Material Group
     AND ALIEF > 0.
  PERFORM BUILD_BIG_TABLE.

  PERFORM DISPLAY_TABLE.
  if p_rprt = 'X'.
     PERFORM CREATE_OUTPUT_REPORT.
  else.
     perform create_excel_spreadsheet.
  endif.


************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*-----------------------  BUILD_BIG_TABLE  -----------------------------
*  This subroutine moves all data to BIG_TABLE.
*-----------------------------------------------------------------------

FORM BUILD_BIG_TABLE.
  SORT SAVE_TABLE BY WERKS MATKL MATNR LIFNR.
  LOOP AT SAVE_TABLE.
    AT NEW WERKS.
      TEMPWERKS = SAVE_TABLE-WERKS.
    ENDAT.

    AT NEW MATKL.
      TEMPMATKL = SAVE_TABLE-MATKL.
    ENDAT.

    AT NEW MATNR.
      TEMPMATNR = SAVE_TABLE-MATNR.
    ENDAT.

    AT NEW LIFNR.
      TEMPLIFNR = SAVE_TABLE-LIFNR.
    ENDAT.

    LIFNRLFZTA = LIFNRLFZTA + SAVE_TABLE-LFZTA.
    LIFNRALIEF = LIFNRALIEF + SAVE_TABLE-ALIEF.

    MATNRLFZTA = MATNRLFZTA + SAVE_TABLE-LFZTA.
    MATNRALIEF = MATNRALIEF + SAVE_TABLE-ALIEF.

    AT END OF LIFNR.
      BIG_TABLE-WERKS = TEMPWERKS.
      BIG_TABLE-MATKL = TEMPMATKL.
      BIG_TABLE-MATNR = TEMPMATNR.
      BIG_TABLE-LIFNR = TEMPLIFNR.
      BIG_TABLE-LFZTA = LIFNRLFZTA.
      BIG_TABLE-ALIEF = LIFNRALIEF.
      BIG_TABLE-MEANS = LIFNRLFZTA DIV LIFNRALIEF.
      APPEND BIG_TABLE.
      CLEAR: LIFNRLFZTA, LIFNRALIEF.
      CLEAR BIG_TABLE.
    ENDAT.

    AT END OF MATNR.
      SELECT SINGLE * FROM MARC        "Get Lead Time from MARC
          WHERE WERKS = TEMPWERKS
            AND MATNR = TEMPMATNR.
      IF  SY-SUBRC = 0.
        SUM_TABLE-PLIFZ = MARC-PLIFZ.
        SUM_TABLE-EKGRP = MARC-EKGRP.
      ENDIF.

      CLEAR TEMPAPLFZ.
      SELECT * FROM EINA               "Get I/R Lead Time from EINE
       WHERE MATNR = TEMPMATNR
         AND LIFNR = TEMPLIFNR.
        SELECT * FROM EINE
         WHERE INFNR = EINA-INFNR
           AND LOEKZ NE 'X'.
          IF  SY-SUBRC = 0.
            SUM_TABLE-APLFZ = TEMPAPLFZ + EINE-APLFZ.
          ENDIF.
        ENDSELECT.
      ENDSELECT.

      SELECT SINGLE * FROM MAKT
        WHERE MATNR = TEMPMATNR        "Get Description
          AND SPRAS = SY-LANGU.
      SUM_TABLE-MAKTX = MAKT-MAKTX.
      IF SY-SUBRC = '0'.

        SUM_TABLE-WERKS = TEMPWERKS.
        SUM_TABLE-MATKL = TEMPMATKL.
        SUM_TABLE-MATNR = TEMPMATNR.
        SUM_TABLE-LFZTA = MATNRLFZTA.
        SUM_TABLE-ALIEF = MATNRALIEF.
        SUM_TABLE-MEANS = MATNRLFZTA DIV MATNRALIEF.
        SUM_TABLE-DIFF = SUM_TABLE-MEANS - SUM_TABLE-PLIFZ.
        SUM_TABLE-ABSDIFF = ABS( SUM_TABLE-DIFF ).
        APPEND SUM_TABLE.
        CLEAR: MATNRLFZTA, MATNRALIEF.
        CLEAR SUM_TABLE.
      ENDIF.
    ENDAT.

  ENDLOOP.
ENDFORM.
*----------------------  DISPLAY_TABLE  --------------------------------
*  This subroutine displays data in the format of the report.
*-----------------------------------------------------------------------
FORM DISPLAY_TABLE.
  SORT SUM_TABLE BY WERKS MATKL ABSDIFF DESCENDING MATNR.
  LOOP AT SUM_TABLE.

    AT NEW WERKS.
      SELECT SINGLE * FROM T001W
         WHERE WERKS = SUM_TABLE-WERKS.
    ENDAT.

    IF  SUM_TABLE-ABSDIFF < P_DAYS.
      CONTINUE.
    ENDIF.                             "Limits report

    AT NEW MATKL.
      SELECT SINGLE * FROM T023T
       WHERE MATKL = SUM_TABLE-MATKL
         AND SPRAS = SY-LANGU.
    ENDAT.

    AT NEW MATNR.
      CLEAR: FLAGPRT, EXCLTAB.
      SELECT SINGLE * FROM MAKT
        WHERE MATNR = SUM_TABLE-MATNR                   "Get Description
          AND SPRAS = SY-LANGU.
      IF SY-SUBRC = '0'.
        MOVE: SUM_TABLE-MATNR  TO  EXCLTAB-MATNR,       "Material
              MAKT-MAKTX       TO  EXCLTAB-MAKTX,       "Material Desc
              SUM_TABLE-WERKS  TO  EXCLTAB-WERKS,
              T001W-NAME1      TO  EXCLTAB-PNAME,
              SUM_TABLE-MATKL  TO  EXCLTAB-MATKL,
              T023T-WGBEZ      TO  EXCLTAB-WGBEZ.
        FLAGPRT = 'yes'.
      ENDIF.
    ENDAT.
    CLEAR COUNTER.
    LOOP AT BIG_TABLE
       WHERE WERKS = SUM_TABLE-WERKS
         AND MATKL = SUM_TABLE-MATKL
         AND MATNR = SUM_TABLE-MATNR.
      COUNTER = COUNTER + 1.
      LIFNRDESC = TEXT-005.
      SELECT SINGLE * FROM LFA1                  "Get Vendor Description
         WHERE LIFNR = BIG_TABLE-LIFNR.
      IF SY-SUBRC = 0.
        LIFNRDESC = LFA1-NAME1.
      ENDIF.
      IF FLAGPRT = 'yes'.
        WRITE BIG_TABLE-MEANS TO EXCLTAB-MEANS.        "Mean Delivery
        MOVE: BIG_TABLE-LIFNR TO EXCLTAB-LIFNR,        "Vendor
              LIFNRDESC       TO EXCLTAB-VNAME.        "Vendor Name
* Added routine   -  Nesh N. Laurencic/ Omnilogic Systems Group
        PERFORM COUNT_PURCHASE_ORDERS.
        WRITE:  SUM_TABLE-PLIFZ TO EXCLTAB-PLIFZ,
                SUM_TABLE-APLFZ TO EXCLTAB-APLFZ,
                SUM_TABLE-DIFF  TO EXCLTAB-DIFF.
        MOVE    SUM_TABLE-EKGRP TO  EXCLTAB-EKGRP.
        APPEND EXCLTAB.
      ENDIF.
    ENDLOOP.                            "End of BIG_TABLE loop
  ENDLOOP.                              "End of SUM_TABLE loop
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  count_purchase_orders
*&---------------------------------------------------------------------*
*       We are counting purchase orders & receipts                     *
*----------------------------------------------------------------------*
FORM COUNT_PURCHASE_ORDERS.
  CLEAR: EKPO, EKKO, MSEG.
* Find purchase orders
  SELECT * FROM EKPO WHERE
                           LOEKZ NE 'L' AND              " Not deleted
                           MATNR EQ BIG_TABLE-MATNR AND
                           WERKS EQ BIG_TABLE-WERKS.
    CLEAR FLAG.
    SELECT SINGLE * FROM EKKO WHERE       "Check Vendor #
                             EBELN EQ EKPO-EBELN.
    CHECK EKKO-LIFNR EQ BIG_TABLE-LIFNR.
    MOVE  EKKO-BEDAT+0(6) TO W_DLVPERIOD. "Check Delv. Period
    CHECK W_DLVPERIOD     IN SSPMON.

* We will count order only if it's not open order
* That means: It must exist at least one receipt.

    SELECT * FROM MSEG  WHERE
                                  (  BWART EQ '101'  OR   "Movement type
                                  BWART EQ '102' ) AND
                                  MATNR EQ BIG_TABLE-MATNR AND
                                  WERKS EQ BIG_TABLE-WERKS AND
                                  LIFNR EQ BIG_TABLE-LIFNR AND
                                  EBELN EQ EKKO-EBELN.    " P.O. number
      CLEAR: COUNTAB1, COUNTAB2.
* Find all material documents (receipts) , and movement 102
* and store them in 2 diff. internal tables.
* Ask functional guys about 102 movement type.
* When they create document 102, it's always document number for
* corresponding document with movement type 101 but icreased for 1.
* Example:  101  - doc no. 5000000000
*           102  - doc no. 5000000001
* This means that 102 refers to 101. That sometimes means cuncellation,
* so we have to check if the quantities match each other.
      IF MSEG-BWART = '101'.
        MOVE-CORRESPONDING MSEG TO COUNTAB1.
        APPEND COUNTAB1.
      ELSE.
        MOVE-CORRESPONDING MSEG TO COUNTAB2.
        APPEND COUNTAB2.
      ENDIF.
    ENDSELECT.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.
* Dont be confused, this is PO specific. Purchase order can be valid or
* no valid, we are going to count receipts using the same routine.
    LOOP AT COUNTAB1.
      COUNTAB1-MBLNR = COUNTAB1-MBLNR + 1.
      CLEAR COUNTAB2.
      READ TABLE COUNTAB2 WITH KEY
                             MBLNR = COUNTAB1-MBLNR
                             MJAHR = COUNTAB1-MJAHR
                             ZEILE = COUNTAB1-ZEILE.
      IF SY-SUBRC <> 0 AND FLAG = 0.
        COUNTER2 = COUNTER2 + 1.
        COUNTER3 = COUNTER3 + 1.       " This is special case,
        FLAG = 1.                      "Just one receipt and one order.
      ELSE.
        IF COUNTAB2-MENGE <> COUNTAB1-MENGE.
          COUNTER3 = COUNTER3 + 1.     "Number of receippts
          IF FLAG = 0.
            COUNTER2 = COUNTER2 + 1.
            FLAG = 1.


          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    REFRESH: COUNTAB1, COUNTAB2.
* we are still looping through all purchase orders !!!!!!!!!!!!!!!!
  ENDSELECT.

  "end select for EKPO
* Print totals for PO and Receipts
  WRITE  COUNTER2 TO EXCLTAB-COUNTPO.  " Just because of format
  CLEAR: COUNTER2, COUNTER3.
  REFRESH: COUNTAB1, COUNTAB2.
ENDFORM.                               " COUNT_PURCHASE_ORDERS

**********************************************************************

FORM CREATE_OUTPUT_REPORT.
  PERFORM PROT_HEADER.

  CONCATENATE SY-REPID  '-'  TEXT-TTL INTO W_REPTTL
               SEPARATED BY SPACE.
  CONCATENATE  SSPMON+3(4) '/' SSPMON+7(2)  INTO W_TEXT1.
  CONCATENATE  SSPMON+9(4) '/' SSPMON+13(2) INTO W_TEXT2.
  CONCATENATE  TEXT-002 W_TEXT1 TEXT-003 W_TEXT2 INTO W_HEAD01
          SEPARATED BY SPACE.
*  CONCATENATE SY-DATUM+0(4) SY-DATUM+4(2) SY-DATUM+6(2)
*              INTO W_HEAD02 SEPARATED BY '/'.
*  CONCATENATE 'As Of:' W_HEAD02 INTO W_HEAD02 SEPARATED BY SPACE.

   MOVE TEXT-016  TO W_HEAD02+0(5).
   WRITE SY-DATUM TO W_HEAD02+6(10).
   MOVE TEXT-CLT  TO W_HEAD02+21(7).
   MOVE SY-MANDT  TO W_HEAD02+29(4).
   MOVE SY-SYSID  TO W_HEAD02+34(5).

  IF P_RPRT = 'X'.
    CLEAR W_OPTION.
*    IF SY-BATCH = 'X'.
*      W_OPTION = 'LINESELMOD:1'.
*    ENDIF.
  ENDIF.

  CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
       EXPORTING
            BASIC_LIST_TITLE    = W_REPTTL
            FILE_NAME           = SY-CPROG
            HEAD_LINE1          = W_HEAD01
            HEAD_LINE2          = W_HEAD02
            ADDITIONAL_OPTIONS  = W_OPTION
       IMPORTING
            RETURN_CODE         = RETCODE
       TABLES
            DATA_TAB            = EXCLTAB
            FIELDNAME_TAB       = PROT_HEADER
            ERROR_TAB           = ERRORTAB
       EXCEPTIONS
            DOWNLOAD_PROBLEM    = 1
            NO_DATA_TAB_ENTRIES = 2
            TABLE_MISMATCH      = 3
            PRINT_PROBLEMS      = 4
            OTHERS              = 5.
  IF SY-SUBRC NE 0.
    WRITE: /1 'TABLE DOWNLOAD UNSUCCESSFULL - REASON = ', SY-SUBRC.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM PROT_HEADER                                              *
*---------------------------------------------------------------------*
FORM PROT_HEADER.
  MOVE TEXT-C01 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C02 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C03 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C04 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C05 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C06 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C6A TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C07 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C08 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C09 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C10 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C11 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C12 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-C13 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
ENDFORM.                               " PROT_HEADER

form create_excel_spreadsheet.

data:  begin of lt_fnames occurs 0,
       text(60)  type c,
       end of lt_fnames.

  lt_fnames-text = text-C01.
  append lt_fnames.
  lt_fnames-text = text-C02.
  append lt_fnames.
  lt_fnames-text = text-C03.
  append lt_fnames.
  lt_fnames-text = text-C04.
  append lt_fnames.
  lt_fnames-text = text-C05.
  append lt_fnames.
  lt_fnames-text = text-C06.
  append lt_fnames.
  lt_fnames-text = text-C6A.
  append lt_fnames.
  lt_fnames-text = text-C07.
  append lt_fnames.
  lt_fnames-text = text-C08.
  append lt_fnames.
  lt_fnames-text = text-C09.
  append lt_fnames.
  lt_fnames-text = text-C10.
  append lt_fnames.
  lt_fnames-text = text-C11.
  append lt_fnames.
  lt_fnames-text = text-C12.
  append lt_fnames.
  lt_fnames-text = text-C13.
  append lt_fnames.

  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
       EXPORTING
*          FILE_NAME                = 'H:\SAPTEMP' "TR995
           FILE_NAME                = P_FILE       "TR995
           CREATE_PIVOT             = 0
       TABLES
           DATA_TAB                 = EXCLTAB
           FIELDNAMES               = LT_FNAMES
       EXCEPTIONS
          FILE_NOT_EXIST            = 1
          FILENAME_EXPECTED         = 2
          COMMUNICATION_ERROR       = 3
          OLE_OBJECT_METHOD_ERROR   = 4
          OLE_OBJECT_PROPERTY_ERROR = 5
          INVALID_FILENAME          = 6
          INVALID_PIVOT_FIELDS      = 7
          DOWNLOAD_PROBLEM          = 8
          OTHERS                    = 9.
  IF SY-SUBRC <> 0.
    endif.
endform.

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM CHECK_FILE_PATH.
DATA: sep_file type string,
      sep_path type string,
      LV_BOL TYPE C.        "abap_bool.

*Separate Path and file
     CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
       EXPORTING
         FULL_NAME           = P_FILE
      IMPORTING
        STRIPPED_NAME       = sep_file
        FILE_PATH           = sep_path
      EXCEPTIONS
        X_ERROR             = 1
        OTHERS              = 2
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

*lv_dir = sep_path.
IF sep_path CS 'C:' OR sep_path CS 'c:'.
   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
ELSE.
*Check if directory path exist or not.
CALL METHOD cl_gui_frontend_services=>directory_exist
  EXPORTING
    directory            = sep_path      "lv_dir
  RECEIVING
    result               = lv_bol
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    wrong_parameter      = 3
    not_supported_by_gui = 4
    OTHERS               = 5.
IF lv_bol IS INITIAL.
   CONCATENATE TEXT-099 sep_path sep_file into SEP_PATH.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
   MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH SEP_PATH.
ENDIF.
ENDIF.
ENDFORM.
