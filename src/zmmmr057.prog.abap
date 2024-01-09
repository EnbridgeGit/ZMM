REPORT zmmmr057.
TYPE-POOLS: slis.

************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        April 2002
*  Description:
*     - The purpose of this program is to produce the
*       Plant Supply Summary by Plant/Material Group
************************************************************************
* 2012/09/06 M Khan   TR995 Change C: drive to H: drive with directory
*                           file selection using F4 & move
*                           hard-coded file path/name to variant.
*
* 2008/09/15 mdemeest TR277 - Remove plant text
* 2008/07/21 mdemeest TR277 - fix excel spreadsheet headers
* 02/02/12 mokhan  Issue: 1039 Changes to replace classic report to ALV
*                              with Excel Sheet option through variants.
*
* 2002/04/25 - mdemeest Copied ZMMMR037 as base
*
************************************************************************

TABLES: ent1027, marc, mard, mbew, mver, resb, t023t, ekpo, ekbe.
*        T001W.

DATA:
   BEGIN OF wa  OCCURS 0,
       matnr         LIKE ent1027-matnr,       "Material Number
       matkl         LIKE ent1027-matkl,       "Material Group
       maktx         LIKE ent1027-maktx,       "Description
   END OF wa.

DATA:
    BEGIN OF big_table OCCURS 10000,
       werks      LIKE marc-werks,   "Plant
*       LGORT     LIKE MARD-LGORT,   "i1039 Storage Location
*       NAME1     LIKE T001W-NAME1,  "i1039 Plant Description 2008/09/15
       matkl      LIKE mara-matkl,   "Material Group
       wgbez      LIKE t023t-wgbez,  "i1039 Mat.Group description
       maktx      LIKE makt-maktx,   "Description
       matnr(6) TYPE c,  "LIKE MARA-MATNR,      "Material Number
       dismm(4)  TYPE c,             "MRP Type
       labst(8) TYPE c,              "Qty on hand TR277
       minbe(7) TYPE c,              "Reorder     TR277
       mabst(7) TYPE c,              "Maximum Stock Level
       qoo(8)   TYPE c,              "Quantity on Order
       plifz(8)  TYPE c,             "Lead Time
       bstrf(4)  TYPE c,             "Rounding Value

       consump(12) TYPE c,           "Last 12 months consump
   END OF big_table.


DATA:  plant_qoh(7) TYPE c,     "LIKE MARD-LABST.
       plant_minbe(7) TYPE i,   "Reorder
       plant_mabst(7) TYPE i,   "maxlvl
       plant_qoo(7)   TYPE i,   "qoo
       plant_plifz(4) TYPE i,   "lead time
       plant_bstrf(4) TYPE i,
       plant_consump(4) TYPE i.
DATA:  plant_aup(8)  TYPE p DECIMALS 2.

DATA:  strind        TYPE i.                    "Starting point in table
DATA:  endind        TYPE i.                    "Ending point in table
DATA:  curyr         LIKE mver-gjahr.           "Current year
DATA:  prvyr         LIKE mver-gjahr.           "Previous year
DATA:  prev_matkl    LIKE ent1027-matkl.  "i1039 Material Group
DATA:  prev_werks    LIKE marc-werks.     "i1039 Plant
DATA:  w_head01(132) TYPE c.
DATA:  w_head02(132) TYPE c.

DATA:  ttl_consump   LIKE mver-mgv01,
       ttl_consump2  LIKE mver-mgv01,
       ytd_consump   LIKE mver-mgv01,
       ttl_reserv    LIKE resb-bdmng,
       ttl_qoo       LIKE ekpo-menge.
*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-000.
SELECT-OPTIONS:  swerks FOR marc-werks,            "Plant
                 slgort FOR mard-lgort,            "Storage Location
                 smatkl FOR ent1027-matkl,         "Material Group
                 sdismm FOR marc-dismm.            "Material type
SELECTION-SCREEN END OF BLOCK box1.
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-100.
PARAMETERS:     p_rprt RADIOBUTTON GROUP rbcr,            "PRINT REPORT
                p_excl RADIOBUTTON GROUP rbcr.            "EXCEL FILE
PARAMETERS      p_file like rlgrap-filename default 'H:\SAPTEMP'. "TR995
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-101.
SELECTION-SCREEN COMMENT 1(43) text-102.
SELECTION-SCREEN END OF BLOCK box3.
SELECTION-SCREEN END OF BLOCK box.

* End of selection screen

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
*-----------------------------------------------------------------------

START-OF-SELECTION.

* set indices for selection of consumption from mver table
* current & previous year moved to consumption table
* set indices to take required 12 month period
  curyr = sy-datum(4).
  strind = sy-datum+4(2).
  endind = sy-datum+4(2) - 1.
  prvyr = sy-datum(4) - 1.

  SELECT * FROM marc
     WHERE werks IN swerks                       "Plant
       AND dismm IN sdismm.                      "Material Type
    SELECT * FROM ent1027
       WHERE matkl IN smatkl                   "Material Groups
         AND matnr = marc-matnr.
      PERFORM calculate_plant_qoh.
      PERFORM calculate_consumption.
      PERFORM determine_reservations.
      PERFORM determine_qty_on_order.
      PERFORM build_big_table.
    ENDSELECT.

  ENDSELECT.


  IF p_rprt = 'X'.
    PERFORM display_alv.
  ELSE.
    PERFORM display_excel.
  ENDIF.

*-----------------------------------------------------------------------

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*-----------------------------------------------------------------------
*     FORM BUILD_BIG_TABLE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine moves all data to BIG_TABLE.
*-----------------------------------------------------------------------

FORM build_big_table.
  DATA: wrk_bstrf TYPE i.
  CLEAR big_table.
  MOVE marc-werks     TO big_table-werks.
  MOVE marc-matnr+12(6)     TO big_table-matnr.
  MOVE marc-bstrf     TO plant_bstrf.
  MOVE plant_bstrf       TO big_table-bstrf.
*      MOVE MARC-DISMM     TO BIG_TABLE-DISMM.            "MRP Type
*      MOVE MARC-MABST     TO BIG_TABLE-MABST.            "Maximum
  MOVE marc-plifz      TO plant_plifz.               "Lead time
  MOVE plant_plifz     TO big_table-plifz.
*      MOVE MARC-MINBE     TO BIG_TABLE-MINBE.            "Reorder
  MOVE ent1027-matkl  TO big_table-matkl.            "Material Group
  MOVE ent1027-maktx  TO big_table-maktx.            "Description
  MOVE plant_qoh      TO big_table-labst.            "Qty on Hand
  MOVE ttl_consump    TO plant_consump.
  MOVE plant_consump    TO big_table-consump.          "Consumptions
*      MOVE TTL_RESERV     TO BIG_TABLE-RESERV.           "Reservations
  MOVE ttl_qoo        TO plant_qoo.
  MOVE plant_qoo      TO big_table-qoo.              "QOO
  IF prev_matkl <> ent1027-matkl.                           "i1039
    prev_matkl =  ent1027-matkl.                            "i1039
    PERFORM material_group_title.                           "i1039
  ENDIF.                                                    "i1039
  MOVE t023t-wgbez    TO big_table-wgbez.   "i1039 Mat Grp Descrpton
*      IF PREV_WERKS <> MARC-WERKS.              "i1039    "2008/09/15
*         PREV_WERKS =  MARC-WERKS.              "i1039
*         PERFORM PLANT_TITLE.                   "i1039
*      ENDIF.                                    "i1039
*     MOVE T001W-NAME1    TO BIG_TABLE-NAME1.   "i1039 Plant Description

*  some fields are written based on MRP strategy (DISMM)
  IF marc-dismm <> '  '.
    MOVE: marc-dismm    TO big_table-dismm.   "i1039 MRP Type
    MOVE  marc-minbe     TO plant_minbe.
    MOVE plant_minbe    TO big_table-minbe.   "i1039 Reorder Point
  ENDIF.

  IF marc-dismm = 'VB' OR marc-dismm = 'V1'.
    MOVE marc-mabst     TO plant_mabst.
    MOVE plant_mabst    TO big_table-mabst.   "i1039 Max.reorder level
  ENDIF.
  APPEND big_table.
  CLEAR  big_table.
ENDFORM.                    "BUILD_BIG_TABLE

*-----------------------------------------------------------------------
*     FORM CALCULATE_PLANT_QOH
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine calculates the qoh for a plant by summing
*     qoh from individual storage locations within the plant
*-----------------------------------------------------------------------
FORM calculate_plant_qoh.
  CLEAR plant_qoh.
  SELECT * FROM mard
      WHERE werks = marc-werks
        AND matnr = marc-matnr
        AND lgort IN slgort.
    ADD mard-labst TO plant_qoh.                "Total Plant qoh
  ENDSELECT.
ENDFORM.                    "CALCULATE_PLANT_QOH

*------------------------------------------------------------------*
*FORM PLANT_TITLE.
*  SELECT SINGLE * FROM T001W
*       WHERE WERKS = BIG_TABLE-WERKS.
*ENDFORM.
*------------------------------------------------------------------*

FORM calculate_consumption.
  CLEAR: ttl_consump, ttl_consump2, ytd_consump.
* calculate consumption from previous year
  SELECT * FROM mver
     WHERE matnr = marc-matnr
       AND werks = marc-werks
       AND gjahr = prvyr.
    IF sy-datum+4(2) = '01'.
      ADD mver-mgv01 FROM 1 TO 12 GIVING ttl_consump.
    ELSE.
      ADD mver-mgv01 FROM strind TO 12 GIVING ttl_consump.
    ENDIF.
  ENDSELECT.                            "END OF MVER FROM PREVIOUS YEAR

* calculate consumption from current year
  SELECT * FROM mver
     WHERE matnr = marc-matnr
       AND werks = marc-werks
       AND gjahr = curyr.
    IF sy-datum+4(2) = '01'.
    ELSE.
      ADD mver-mgv01 FROM 1 TO endind GIVING ttl_consump2.
      ADD ttl_consump2 TO ttl_consump.
    ENDIF.

    ADD mver-mgv01 FROM 1 TO sy-datum+4(2) GIVING ytd_consump.
  ENDSELECT.                             "END OF MVER FROM CURRENT YEAR

ENDFORM.                    "CALCULATE_CONSUMPTION

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_RESERVATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM determine_reservations.
*    CLEAR TTL_RESERV.
*    SELECT * FROM RESB
*       WHERE MATNR = MARC-MATNR
*         AND WERKS = MARC-WERKS
*         AND RSSTA = 'M'
*         AND XLOEK = SPACE                      "Item has been deleted
*         AND KZEAR = SPACE.                       "Item is final issued
*         IF  RESB-SHKZG = 'H'.
*             TTL_RESERV = TTL_RESERV + RESB-BDMNG.
*         ELSE.
*             TTL_RESERV = TTL_RESERV - RESB-BDMNG.
*         ENDIF.
*     ENDSELECT.
ENDFORM.                    "DETERMINE_RESERVATIONS

*-----------------------------------------------------------------------
*     FORM DETERMINE_QTY_ON_ORDER
*-----------------------------------------------------------------------
*   - This subroutine calculates the quantity outstanding for a
*     material on a PO
*-----------------------------------------------------------------------
FORM determine_qty_on_order.
  CLEAR ttl_qoo.
  SELECT * FROM ekpo
      WHERE matnr = marc-matnr
        AND werks = marc-werks
        AND loekz <> 'L'
        AND elikz <> 'X'.
    ttl_qoo = ttl_qoo + ekpo-menge.
    SELECT * FROM ekbe
        WHERE ebeln = ekpo-ebeln
          AND ebelp = ekpo-ebelp
          AND bewtp = 'E'.
      IF ekbe-shkzg = 'H'.                                "Credit
        ttl_qoo = ttl_qoo + ekbe-menge.
      ELSE.                                               "Debit
        ttl_qoo = ttl_qoo - ekbe-menge.
      ENDIF.
    ENDSELECT.
  ENDSELECT.
  ttl_qoo = ( ttl_qoo * ekpo-umrez ) / ekpo-umren.   "1998/04/15  ra
ENDFORM.                    "DETERMINE_QTY_ON_ORDER

*&---------------------------------------------------------------------*
*&      Form  BIG_TABLE_DUMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM big_table_dump.
*    write: /2 big_table-posid,
*                111 big_table-plan,            "YTD Plan
*                129 big_table-actual,          "Actuals
*                147 big_table-variance,        "Variance(actual - plan
*                165 big_table-locked,          "Appr Budget Comt
*                183 big_table-variance,        "Total Commitment
*                183 big_table-commit,          "Total Commitment
*                201 big_table-annplan,         "Annual Plan
*                219 wa-remplan,                "Remaining Plan
*                237 big_table-uncommit.        "Uncommitted Budget
ENDFORM.                    "BIG_TABLE_DUMP

*-----------------------------------------------------------------------
FORM material_group_title.
  SELECT SINGLE * FROM t023t
     WHERE matkl = big_table-matkl.
ENDFORM.                    "MATERIAL_GROUP_TITLE
*-----------------------------------------------------------------------
FORM display_alv.
  DATA: fieldcat TYPE slis_t_fieldcat_alv,
        fc_str   TYPE slis_fieldcat_alv,
        layout   TYPE slis_layout_alv,
        title    TYPE lvc_title,
        repid    LIKE sy-repid,
        variant  LIKE disvariant,
        sort     TYPE slis_t_sortinfo_alv,
        sort_str TYPE slis_sortinfo_alv.

  repid = sy-repid.
  layout-colwidth_optimize = 'X'.
  variant-report = repid.
  SORT big_table BY werks matkl maktx.
* create field catalog
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*       I_PROGRAM_NAME         = REPID
*       I_INTERNAL_TABNAME     = 'BIG_TABLE'
*       I_INCLNAME             = REPID
*    CHANGING
*       CT_FIELDCAT            = FIELDCAT
*    EXCEPTIONS
*       INCONSISTENT_INTERFACE = 1
*       PROGRAM_ERROR          = 2
*       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)

  fc_str-fieldname = 'WERKS'.
  fc_str-key    = ' '.                  " Key columns -not first
  fc_str-seltext_l = text-001.          " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

*          FC_STR-FIELDNAME =  'NAME1'.         "Plant Text 2008/09/15
*          FC_STR-SELTEXT_L = TEXT-002.         " Alternative col header
*          FC_STR-DDICTXT = 'L'.
*          APPEND FC_STR TO FIELDCAT.
*          CLEAR  FC_STR.

  fc_str-fieldname =  'MATKL'.
  fc_str-seltext_l = text-003.          " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'WGBEZ'.
  fc_str-seltext_l = text-004.          " Alternative col header
  fc_str-ddictxt = 'L'.                 " Use Large system text
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'MAKTX'.
  fc_str-seltext_l = text-005.          " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'MATNR'.
*          FC_STR-KEY    = ' '.                 " Key columns -not first
  fc_str-seltext_l = text-006.          " Alternative col header
  fc_str-ddictxt = 'L'.                 " Use small system text
  fc_str-just = 'R'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'DISMM'.
  fc_str-seltext_l = text-012.          " Alternative col header
  fc_str-ddictxt = 'L'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.


  fc_str-fieldname = 'LABST'.
  fc_str-seltext_l = text-007.          " Alternative col header
  fc_str-ddictxt = 'L'.
  fc_str-just = 'R'.
*          FC_STR-NO_ZERO = 'X'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'MINBE'.
  fc_str-seltext_l = text-008.          " Alternative col header
  fc_str-ddictxt = 'L'.
  fc_str-just = 'R'.
*          FC_STR-NO_ZERO = 'X'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'MABST'.
  fc_str-seltext_l = text-009.          " Alternative col header
  fc_str-ddictxt = 'L'.
  fc_str-just = 'R'.
*          FC_STR-NO_ZERO = 'X'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'QOO'.
  fc_str-seltext_l = text-010.          " Alternative col header
  fc_str-ddictxt = 'L'.
  fc_str-just = 'R'.
*          FC_STR-NO_ZERO = 'X'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'PLIFZ'.
  fc_str-seltext_l = text-011.          " Alternative col header
  fc_str-ddictxt = 'L'.
  fc_str-just = 'R'.
*          FC_STR-NO_ZERO = 'X'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

  fc_str-fieldname = 'BSTRF'.
  fc_str-seltext_l = text-11a.          " Alternative col header
  fc_str-ddictxt = 'L'.
  fc_str-just = 'R'.
*          FC_STR-NO_ZERO = 'X'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.


  fc_str-fieldname = 'CONSUMP'.
*          FC_STR-REF_FIELDNAME = 'MGV01'.
*          FC_STR-REF_TABNAME = 'MVER'.
  fc_str-seltext_l = text-014.          " Alternative col header
  fc_str-ddictxt = 'L'.
*          FC_STR-DO_SUM  = 'X'.                 " Do Sum
  fc_str-just = 'R'.
*          FC_STR-NO_ZERO = 'X'.
  APPEND fc_str TO fieldcat.
  CLEAR  fc_str.

*          FC_STR-FIELDNAME = 'RESERV'.
*          FC_STR-NO_OUT = 'X'.                  " Hide column
*          APPEND FC_STR TO FIELDCAT.
*          CLEAR  FC_STR.

* Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
             it_fieldcat  = fieldcat
             is_layout    = layout
             i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
             i_callback_program     =  repid
*          I_SAVE       = 'A'
*          IS_VARIANT   = variant
*          IT_SORT        = sort
*          I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
      TABLES
             t_outtab = big_table
      EXCEPTIONS
             program_error = 1
      OTHERS               = 2.

ENDFORM.                    "DISPLAY_ALV
*************************************************************

FORM alv_top_of_page.
  DATA: ls_line TYPE slis_listheader.
  DATA: lt_top_of_page TYPE slis_t_listheader.
  DATA: datum1(10).
  DATA: uzeit1(10).

  IF slgort = ' '.
*   MOVE 'ALL' TO W_HEAD01.
    CONCATENATE text-015 'ALL' INTO w_head01.
  ELSE.
    MOVE text-015 TO w_head01.
    LOOP AT slgort.
      IF slgort+1(2) = 'EQ'.
        CONCATENATE w_head01 slgort+3(4) INTO w_head01 SEPARATED BY ';'.
      ELSEIF slgort+1(2) = 'BT'.
        CONCATENATE w_head01 slgort+3(4) INTO w_head01 SEPARATED BY ';'.
        CONCATENATE w_head01 slgort+7(4) INTO w_head01 SEPARATED BY '-'.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF w_head01+28(1) = ';'.
    MOVE ' ' TO w_head01+28(1).
  ENDIF.

  WRITE sy-datum TO datum1 DD/MM/YYYY.
  WRITE sy-uzeit TO uzeit1 USING EDIT MASK '__:__:__'.
  CONCATENATE text-clt sy-sysid sy-mandt text-dte datum1 '@' uzeit1
              INTO w_head02 SEPARATED BY space.

*1- HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = sy-title.             "sy-title.
  APPEND ls_line TO lt_top_of_page.

*2- SELECTION LINE: TYPE S
  CLEAR ls_line.
  ls_line-typ   = 'S'.
  ls_line-key   = ' '.
  ls_line-info  = w_head01.
*  CONCATENATE SY-SYSID SY-MANDT INTO LS_LINE-INFO
*              SEPARATED BY SPACE.
  APPEND ls_line TO lt_top_of_page.

*3- ACTION LINE:  TYPE A
  CLEAR ls_line.
*  ls_line-typ  = 'A'.
  ls_line-typ   = 'S'.
  ls_line-key  = ' '.
  ls_line-info  = w_head02.
*  WRITE SY-DATUM TO DATUM1 DD/MM/YYYY.
*  WRITE SY-UZEIT TO UZEIT1 USING EDIT MASK '__:__:__'.
*  CONCATENATE DATUM1 '@' UZEIT1 INTO LS_LINE-INFO
*                     SEPARATED BY SPACE.
  APPEND ls_line TO lt_top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.

ENDFORM.                               " ALV_TOP_OF_PAGE
*******************************************************
FORM display_excel.
  DATA: BEGIN OF lt_fnames OCCURS 0,
        text(60) TYPE c,
        END OF lt_fnames.

  lt_fnames-text = text-001.
  APPEND lt_fnames.
*        LT_FNAMES-TEXT = TEXT-002.   "Removed 2008/09/15
*        APPEND LT_FNAMES.
  lt_fnames-text = text-003.
  APPEND lt_fnames.
  lt_fnames-text = text-004.
  APPEND lt_fnames.
  lt_fnames-text = text-005.
  APPEND lt_fnames.
  lt_fnames-text = text-006.
  APPEND lt_fnames.

  lt_fnames-text = text-012.
  APPEND lt_fnames.

  lt_fnames-text = text-007.
  APPEND lt_fnames.
  lt_fnames-text = text-008.
  APPEND lt_fnames.
  lt_fnames-text = text-009.
  APPEND lt_fnames.
  lt_fnames-text = text-010.
  APPEND lt_fnames.
  lt_fnames-text = text-011.
  APPEND lt_fnames.
  lt_fnames-text = text-11a.
  APPEND lt_fnames.
  lt_fnames-text = text-014.
  APPEND lt_fnames.




  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
       EXPORTING
*           file_name                 = 'C:\SAPTEMP' "TR995
            file_name                 = P_FILE       "TR995
            create_pivot              = 0
*           DATA_SHEET_NAME           = ' '
*           PIVOT_SHEET_NAME          = ' '
*           PASSWORD                  = ' '
*           PASSWORD_OPTION           = 0
       TABLES
*           PIVOT_FIELD_TAB           =
            data_tab                  = big_table
            fieldnames                = lt_fnames
       EXCEPTIONS
            file_not_exist            = 1
            filename_expected         = 2
            communication_error       = 3
            ole_object_method_error   = 4
            ole_object_property_error = 5
            invalid_filename          = 6
            invalid_pivot_fields      = 7
            download_problem          = 8
            OTHERS                    = 9.
  IF sy-subrc NE 0.
  ENDIF.

ENDFORM.                    "DISPLAY_EXCEL

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
