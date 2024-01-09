REPORT ZMINR025N.

TYPE-POOLS: SLIS.
************************************************************************
*    Program     :  ZMINR025
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  January 18, 1997.
*
* This ABAP will display each material's quantity and total moving price
* within each storage location.  The user will have the choice of
* entering a plant, storage location, material number, material group.
************************************************************************
* 97/04/18 md7140 add material group
*                 access ENT1027 instead of MARA & MAKT.
************************************************************************
* 98/06/01 MRadsma report all materials including 'flagged for deletion'
* 02/02/12 mokhan  Issue: 1039 Changes to replace classic report to ALV
*                              with Excel Sheet option through variants.
*
* 2012/08/30 M Khan   TR995 Change C: drive to H: drive with directory,
*                           file selection using F4 & move the
*                           hard-coded file path/name to variant.
************************************************************************

TABLES  : ENT1027, MARC, MARD, MBEW, T001L, T001W, T023T.

DATA    : BEGIN OF TABLE1 OCCURS 5000,
              WERKS        LIKE MARD-WERKS,     "Plant
              NAME1        LIKE T001W-NAME1,    "Plant Description
              LGORT        LIKE MARD-LGORT,     "Storage location
              LGOBE        LIKE T001L-LGOBE,    "Location Description
              MATKL        LIKE ENT1027-MATKL,  "Material Group
              WGBEZ        LIKE T023T-WGBEZ,    "Material Class Descrptn
              QTYHAND      TYPE I,              "Quantity
              VERPR        LIKE MBEW-VERPR,     "Moving average price
              VALUE        LIKE MBEW-STPRS,     "$ Value
*              MATNR        LIKE MARA-MATNR,     "Material number
              MATNR(6) type c,                  "Material number
              MAKTX        LIKE ENT1027-MAKTX,  "Mat.Number Description
              LVORM        LIKE MARD-LVORM,     "Flaged for deletion
          END OF TABLE1.
*----------------------------------------------------------------------
* 2006/06/20 mdemeest TABLE2 is a copy of TABLE1 except that the fields
*                     are all defined as character.  This is for the
*                     excel spreadsheet - MS_EXCEL_OLE_STANDARD_DAT
*                     Column headings on packed fields cause abap to
*                     abend.
*-----------------------------------------------------------------------
DATA    : BEGIN OF TABLE2 OCCURS 5000,
              WERKS        LIKE MARD-WERKS,     "Plant
              NAME1        LIKE T001W-NAME1,    "Plant Description
              LGORT        LIKE MARD-LGORT,     "Storage location
              LGOBE        LIKE T001L-LGOBE,    "Location Description
              MATKL        LIKE ENT1027-MATKL,  "Material Group
              WGBEZ        LIKE T023T-WGBEZ,    "Material Class Descrptn
              QTYHAND(10)  type c,              "Quantity
              VERPR(10)    type c,              "Moving average price
              VALUE(10)    type c,              "$ Value
              MATNR(6) type c,                  "Material number
              MAKTX        LIKE ENT1027-MAKTX,  "Mat.Number Description
              LVORM        LIKE MARD-LVORM,     "Flaged for deletion
          END OF TABLE2.


DATA    : W_QNTY          TYPE I,
          CHECK1(1)       TYPE C VALUE 'N',
          CHECK2          LIKE CHECK1 VALUE 'Y',
          W_VALUE         LIKE MBEW-STPRS,
          MCLASS_FOUND    TYPE C.

*****************VARIANTS SELECTION *************************

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS:
     S_WERKS         FOR   MARC-WERKS,
     S_LGORT         FOR   MARD-LGORT,
     S_MATKL         FOR   ENT1027-MATKL,
     S_MATNR         FOR   ENT1027-MATNR.
SELECTION-SCREEN END OF BLOCK BOX1.
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-100.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,            "PRINT REPORT
                P_EXCL RADIOBUTTON GROUP RBCR,            "EXCEL FILE
                P_FILE like rlgrap-filename DEFAULT 'H:\SAPTEMP\ZMINR025'. "TR995

SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX.
INCLUDE <ICON>.

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
AT SELECTION-SCREEN ON P_FILE.                           "TR995
  PERFORM CHECK_FILE_PATH.                               "TR995
*End of TR995 changes
*******************************************************

START-OF-SELECTION.

* insert code to fill ITAB here

  SELECT MATNR WERKS LGORT LVORM LABST UMLME INSME SPEME EINME
    INTO (MARD-MATNR, MARD-WERKS, MARD-LGORT, MARD-LVORM, MARD-LABST,
          MARD-UMLME, MARD-INSME, MARD-SPEME, MARD-EINME)
    FROM  MARD
    WHERE MATNR IN S_MATNR
      AND WERKS IN S_WERKS
      AND LGORT IN S_LGORT
    ORDER BY WERKS LGORT MATNR.

    ON CHANGE OF MARD-MATNR.
      MCLASS_FOUND = 'Y'.
      PERFORM GET_MATERIAL_DESCRIPTION_ETC.
      IF MCLASS_FOUND = 'N'.
         CONTINUE.
      ENDIF.
    ENDON.

    ON CHANGE OF MARD-WERKS.
      PERFORM GET_PLANT_DESCRIPTION.
    ENDON.

    ON CHANGE OF MARD-LGORT.
      PERFORM GET_LOCATION_DESCRIPTION.
    ENDON.

*-----------------------------------------------------------------------
* Extra materials are selected for the storage location file, since the
* material group is found only on "MARA" --> ENT1027.  The Material
* should only be reported, if the material group is within the selection
* parameters.
*-----------------------------------------------------------------------

    if mclass_found = 'Y'.
       PERFORM BUILD_TABLE1.
    endif.

  ENDSELECT.

  IF P_RPRT = 'X'.
     PERFORM OUTPUT_ALV.
  ELSE.
     PERFORM OUTPUT_EXCEL.
  ENDIF.

END-OF-SELECTION.

*******************************************************
FORM GET_MATERIAL_DESCRIPTION_ETC.

* Get Material Description & Material Class
  SELECT SINGLE MATKL MAKTX
    INTO (ENT1027-MATKL, ENT1027-MAKTX)
    FROM ENT1027
   WHERE MATNR = MARD-MATNR
     AND SPRAS = SY-LANGU
     AND MATKL IN S_MATKL.
  IF SY-SUBRC <> 0.
     MCLASS_FOUND = 'N'.
     EXIT.
  ENDIF.

* Get Material Group Description
  SELECT SINGLE WGBEZ INTO T023T-WGBEZ
    FROM T023T
   WHERE SPRAS = SY-LANGU
     AND MATKL = ENT1027-MATKL.

* Get Moving Average Price
  SELECT SINGLE VERPR INTO MBEW-VERPR
    FROM MBEW
   WHERE MATNR = MARD-MATNR
     AND BWKEY = MARD-WERKS
     AND BWTAR = SPACE.

ENDFORM.
*******************************************************
FORM GET_PLANT_DESCRIPTION.

  SELECT SINGLE NAME1 INTO T001W-NAME1
    FROM T001W
   WHERE WERKS = MARD-WERKS.

ENDFORM.
*******************************************************
FORM GET_LOCATION_DESCRIPTION.

  SELECT SINGLE LGOBE INTO T001L-LGOBE
    FROM T001L
   WHERE WERKS = MARD-WERKS
     AND LGORT = MARD-LGORT.

ENDFORM.
*******************************************************
FORM BUILD_TABLE1.
  DATA: S_LENGTH TYPE I.
*  DATA: H_DMMY  TYPE X VALUE '/'.
  W_QNTY = W_QNTY  + MARD-LABST + MARD-UMLME +
        MARD-INSME + MARD-SPEME + MARD-EINME.
  W_VALUE = MBEW-VERPR * W_QNTY.

  MOVE MBEW-VERPR    TO TABLE1-VERPR.
  MOVE ENT1027-MATKL TO TABLE1-MATKL.
  MOVE ENT1027-MAKTX TO TABLE1-MAKTX.
  MOVE MARD-WERKS    TO TABLE1-WERKS.
  MOVE MARD-LGORT    TO TABLE1-LGORT.
  MOVE MARD-MATNR+12(6)    TO TABLE1-MATNR.
  MOVE MARD-LVORM    TO TABLE1-LVORM.
  MOVE W_QNTY        TO TABLE1-QTYHAND.
  MOVE W_VALUE       TO TABLE1-VALUE.
  MOVE T001W-NAME1   TO TABLE1-NAME1.
  MOVE T001L-LGOBE   TO TABLE1-LGOBE.
  MOVE T023T-WGBEZ   TO TABLE1-WGBEZ.
*  S_LENGTH = STRLEN( T001W-NAME1 ).
*  IF S_LENGTH < 10.
*     MOVE '.' TO TABLE1-NAME1+9(1).
*  ENDIF.
*  S_LENGTH = STRLEN( MARD-LVORM ).
*  IF S_LENGTH < 10.
*     MOVE '.' TO TABLE1-LVORM+7(1).
*  ENDIF.
  APPEND TABLE1.
  CLEAR: TABLE1, W_QNTY.

ENDFORM.
*******************************************************
FORM OUTPUT_ALV.

SORT TABLE1 BY WERKS LGORT MATKL MATNR.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  VARIANT-REPORT = REPID.

          FC_STR-FIELDNAME = 'WERKS'.
          FC_STR-KEY    = ' '.                " Key columns -not first
          FC_STR-SELTEXT_L = TEXT-001.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'NAME1'.
          FC_STR-KEY    = ' '.                " Key columns -notfirst
          FC_STR-SELTEXT_L = TEXT-002.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'LGORT'.
          FC_STR-KEY    = ' '.                " Key columns -not first
          FC_STR-SELTEXT_L = TEXT-003.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'LGOBE'.
          FC_STR-SELTEXT_L = TEXT-004.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'MATKL'.
          FC_STR-SELTEXT_L = TEXT-005.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'WGBEZ'.
          FC_STR-SELTEXT_L = TEXT-006.        " Alternative col header
          FC_STR-DDICTXT = 'L'.               " Use Large system text
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'QTYHAND'.
          FC_STR-SELTEXT_L = TEXT-007.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-COL_POS = 7.
          FC_STR-JUST = 'R'.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'VERPR'.
          FC_STR-SELTEXT_L = TEXT-008.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-COL_POS = 8.
          FC_STR-JUST = 'R'.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'VALUE'.
          FC_STR-TABNAME   = TABLE1.
          FC_STR-REF_FIELDNAME = 'STPRS'.
          FC_STR-REF_TABNAME = 'MBEW'.
          FC_STR-SELTEXT_L = TEXT-009.        " Alternative col header
          FC_STR-DDICTXT = 'L'.               " Use small system text
          FC_STR-COL_POS = 9.                 " Column Position
          FC_STR-DO_SUM  = 'X'.
          FC_STR-JUST = 'R'.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'MATNR'.
          FC_STR-KEY    = ' '.                " Key columns -not first
          FC_STR-SELTEXT_S = TEXT-010.        " Alternative col header
          FC_STR-DDICTXT = 'S'.               " Use small system text
          FC_STR-COL_POS = 10.
          FC_STR-DO_SUM  = 'X'.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'MAKTX'.
          FC_STR-SELTEXT_L = TEXT-011.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-COL_POS = 11.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

          FC_STR-FIELDNAME = 'LVORM'.
          FC_STR-OUTPUTLEN = 20.
          FC_STR-SELTEXT_L = TEXT-012.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-COL_POS = 12.
          APPEND FC_STR TO FIELDCAT.
          CLEAR  FC_STR.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
*          I_SAVE       = 'A'
*          IS_VARIANT   = variant
*          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = TABLE1
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: DATUM1(10).
  DATA: UZEIT1(10).

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- SELECTION LINE: TYPE S
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'S'.
  LS_LINE-KEY   = 'CLIENT: '.
  CONCATENATE SY-SYSID SY-MANDT INTO ls_line-info
              SEPARATED BY SPACE.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
*  ls_line-typ  = 'A'.
  LS_LINE-TYP   = 'S'.
  LS_LINE-KEY  = 'DATE:'.
  WRITE SY-DATUM TO DATUM1 DD/MM/YYYY.
  WRITE SY-UZEIT TO UZEIT1 USING EDIT MASK '__:__:__'.
  CONCATENATE DATUM1 '@' UZEIT1 INTO LS_LINE-INFO
                     SEPARATED BY SPACE.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.

ENDFORM.                               " ALV_TOP_OF_PAGE
*******************************************************
FORM OUTPUT_EXCEL.
    DATA: BEGIN OF LT_FNAMES OCCURS 0,
          TEXT(60) TYPE C,
          END OF LT_FNAMES.

        LT_FNAMES-TEXT = TEXT-001.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-002.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-003.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-004.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-005.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-006.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-007.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-008.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-009.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-010.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-011.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-012.
        APPEND LT_FNAMES.

*
loop at table1.
   move-corresponding table1 to table2.
   append table2.
endloop.


  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
       EXPORTING
*           FILE_NAME                 = 'C:\SAPTEMP' "TR995
            FILE_NAME                 = P_FILE       "TR995
            CREATE_PIVOT              = 0
*           DATA_SHEET_NAME           = ' '
*           PIVOT_SHEET_NAME          = ' '
*           PASSWORD                  = ' '
*           PASSWORD_OPTION           = 0
       TABLES
*           PIVOT_FIELD_TAB           =
            DATA_TAB                  = TABLE2
            FIELDNAMES                = LT_FNAMES
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
  IF SY-SUBRC NE 0.
  ENDIF.

ENDFORM.

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
