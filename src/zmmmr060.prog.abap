REPORT ZMMMR060.

TYPE-POOLS: SLIS.
*
************************************************************************
*  Author:      Glenn Ymana
*               Mohammad Khan
*  Date:        May 2003
*  Description:
*     - The purpose of this program is to produce the
*       a listing of materials issued I/O number, PCE, material no.
*       This report can also be downloaded into an excel spreadsheet
*       for further data manipulation.
*
************************************************************************
* CHANGES
* 2012/09/04 M Khan   TR995 Change C: drive to H: drive with directory,
*                           file selection using F4 & move the
*                           hard-coded file path/name to variant.
* 2010/01/18 - lritchie - TR 789 - added user name, reference & show
*                                  negatives
* 2007/02/13 - mdemeest - TR277 - Add Material Group Selection
*-----------------------------------------------------------------------
TABLES: MKPF,                          "Document Segment: Header
        PRPS,                          "WBS Element Master Data
        MAKT,                          "Material Descriptions
        MSEG,                          "Document Segment: Material
        MARA.                          "Material Master       2007/02/13

DATA:
    BEGIN OF ITABLE OCCURS 0,
       WERKS        LIKE MSEG-WERKS,       "Plant
       LGORT        LIKE MSEG-LGORT,       "Storage Location
       matkl        like mara-matkl,       "Material Group    2007/02/13
       MATNR        LIKE MSEG-MATNR,       "Material
       MAKTX        LIKE MAKT-MAKTX,       "Material Description
       ERFMG        LIKE MSEG-ERFMG,       "Quantity
       DMBTR        LIKE MSEG-DMBTR,       "Amount
       VGART        LIKE MKPF-VGART,       "Trans./Event Type
       BWART        LIKE MSEG-BWART,       "Movement Type
       BUDAT        LIKE MKPF-BUDAT,       "Posting Date
       WEMPF        LIKE MSEG-WEMPF,       "Issued to
       xblnr        like mkpf-xblnr,       "Reference  2010/01/18
       SAKTO        LIKE MSEG-SAKTO,       "G/L Acct.(PCE) 2010/01/18
       AUFNR        LIKE MSEG-AUFNR,       "Internal order No.
       PSPNR        LIKE MSEG-PS_PSP_PNR,  "WBS Element
*      SAKTO        LIKE MSEG-SAKTO,       "G/L Acct.(PCE) 2010/01/18
       MBLNR        LIKE MSEG-MBLNR,       "Material Document
       SHKZG        LIKE MSEG-SHKZG,       "Debit/Credit Indicator
       usnam        like mkpf-usnam,       "User name  2010/01/18
   END OF ITABLE.

DATA:
    BEGIN OF EXCLTAB OCCURS 0,
       WERKS        LIKE MSEG-WERKS,       "Plant
       LGORT        LIKE MSEG-LGORT,       "Storage Location
       matkl(6)    type c,"like mara-matkl,"Material Group    2007/02/13
       MATNR(7)    type c,"LIKE MSEG-MATNR,"Material
       MAKTX        LIKE MAKT-MAKTX,       "Material Description
       ERFMG(10)    TYPE c,                "Quantity
*       ERFMG        LIKE MSEG-ERFMG,       "Quantity
*      DMBTR        LIKE MSEG-DMBTR,       "Amount
       dmbtr(10)    type c,                "amount
       VGART        LIKE MKPF-VGART,       "Trans./Event Type
       BWART        LIKE MSEG-BWART,       "Movement Type
       BUDAT        LIKE MKPF-BUDAT,       "Posting Date
       WEMPF        LIKE MSEG-WEMPF,       "Issued to
       xblnr        like mkpf-xblnr,       "reference  2010/01/18
       SAKTO        LIKE MSEG-SAKTO,       "G/L Acct.(PCE)  2010/01/18
       AUFNR        LIKE MSEG-AUFNR,       "Internal order No.
       POSID        LIKE PRPS-POSID,       "WBS Element
*      SAKTO        LIKE MSEG-SAKTO,       "G/L Acct.(PCE)  2010/01/18
       MBLNR        LIKE MSEG-MBLNR,       "Material Document
       usnam        like mkpf-usnam,       "user name  2010/01/18
    END OF EXCLTAB.

DATA:  W_HEAD01(50) TYPE C,
       W_HEAD02(60) TYPE C.

RANGES: SMJAHR FOR MSEG-MJAHR.
*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS:  SAUFNR FOR MSEG-AUFNR.               "Internal Order
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(3) TEXT-100.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS:  SPOSID FOR PRPS-POSID.               "WBS Number
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME. " TITLE TEXT-001.
SELECT-OPTIONS:  SBUDAT FOR MKPF-BUDAT OBLIGATORY,    "Selected Period
                 SWERKS FOR MSEG-WERKS OBLIGATORY,    "Plant
                 SLGORT FOR MSEG-LGORT,  "Storage Location "2010/01/18
                 SMATNR FOR MSEG-MATNR,               "Material Number
                 SMATKL for mara-matkl,       "Material Group 2007/02/13
                 SBWART FOR MSEG-BWART,               "Movement Type
*                SLGORT FOR MSEG-LGORT, "Storage Location  "2010/01/18
                 susnam for mkpf-usnam, "User name 2010/01/18
                 SVGART FOR MKPF-VGART.               "Trans/Event Type
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-001.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,          "Print Report
                P_EXCL RADIOBUTTON GROUP RBCR,          "Excel Sheet
                P_FILE LIKE RLGRAP-FILENAME DEFAULT 'H:\SAPTEMP'. "TR995
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON SAUFNR.
   IF SAUFNR <> SPACE AND SPOSID <> SPACE.
      PERFORM CHECK_VARIANTS.
   ENDIF.

AT SELECTION-SCREEN ON SPOSID.
   IF SAUFNR <> SPACE AND SPOSID <> SPACE.
      PERFORM CHECK_VARIANTS.
   ENDIF.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP RBCR.
   IF SY-BATCH = 'X' AND P_EXCL = 'X'.
      WRITE: /1 'EXCEL Option - Foreground Only'.
   ENDIF.

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

  PERFORM GET_DOCUMENT_YEAR.
  PERFORM CHECK_VARIANTS.
  PERFORM DATA_SELECTION.
  IF NOT EXCLTAB[] IS INITIAL.
     SORT EXCLTAB BY WERKS LGORT MATNR.
*     move 'End of Report ' to excltab-maktx.
*     append excltab.
     IF P_RPRT = 'X'.
        PERFORM OUTPUT_ALV.
     ELSE.
        PERFORM OUTPUT_EXCEL.
     ENDIF.
  else.
    write: /1 'End of Report'.
  ENDIF.

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*----------------------  GET_DOCUMENT_YEAR------------------------------
*  This subroutine gets year from the posting date.
*-----------------------------------------------------------------------
FORM GET_DOCUMENT_YEAR.

  LOOP AT SBUDAT.
       MOVE SBUDAT-SIGN      TO SMJAHR-SIGN.
       MOVE SBUDAT-OPTION    TO SMJAHR-OPTION.
       MOVE SBUDAT-LOW+0(4)  TO SMJAHR-LOW+0(4).
       MOVE SBUDAT-HIGH+0(4) TO SMJAHR-HIGH+0(4).
       APPEND SMJAHR.
       CLEAR  SMJAHR.
  ENDLOOP.
ENDFORM.

*----------------------  CHECK_VARIANTS --------------------------------
*  This subroutine checks the variants and if internal order field and
*  WBS field both are filled with data, then it displays a message.
*-----------------------------------------------------------------------
FORM CHECK_VARIANTS.
     IF SAUFNR <> SPACE AND SPOSID <> SPACE.
        CALL FUNCTION 'POPUP_FOR_INTERACTION'
             EXPORTING
             HEADLINE    = '!! ERROR !!'
             TEXT1       = 'Please Enter Internal order OR WBS Number'
             BUTTON_1    = 'OK'.
     ENDIF.
     IF SY-BATCH = 'X' AND P_EXCL = 'X'.
        CALL FUNCTION 'POPUP_FOR_INTERACTION'
             EXPORTING
             HEADLINE    = '!! ERROR !!'
             TEXT1       = 'Excel Sheet Can run Foreground only'
             BUTTON_1    = 'OK'.
     ENDIF.
ENDFORM.

*----------------------  DATA_SELECTION --------------------------------
*  This subroutine determines which DB selection query should run,
*  which is depending upon variants.
*-----------------------------------------------------------------------
FORM DATA_SELECTION.
  IF SPOSID <> SPACE.
     PERFORM DATA_SELECTION_WITH_WBS.
  ELSE.
     PERFORM DATA_SELECTION_WITH_IORDER.
  ENDIF.
ENDFORM.

*----------------------  DATA_SELECTION_WITH_WBS -----------------------
*  This subroutine selects data depending upon WBS data of variants.
*-----------------------------------------------------------------------
FORM DATA_SELECTION_WITH_WBS.

  SELECT MSEG~WERKS MSEG~LGORT MARA~MATKL
         MSEG~MATNR MAKT~MAKTX MSEG~ERFMG
         mkpf~usnam mkpf~xblnr                       "2010/01/18
         MSEG~DMBTR MKPF~VGART MSEG~BWART MKPF~BUDAT MSEG~WEMPF
         MSEG~AUFNR PRPS~POSID MSEG~SAKTO MSEG~MBLNR MSEG~SHKZG
   INTO (MSEG-WERKS, MSEG-LGORT, MARA-MATKL,
         MSEG-MATNR, MAKT-MAKTX, MSEG-ERFMG,
         mkpf-usnam, mkpf-xblnr,                     "2010/01/18
         MSEG-DMBTR, MKPF-VGART, MSEG-BWART, MKPF-BUDAT, MSEG-WEMPF,
         MSEG-AUFNR, PRPS-POSID, MSEG-SAKTO, MSEG-MBLNR, MSEG-SHKZG)
    FROM ( ( ( ( MSEG INNER JOIN MKPF
                 ON MKPF~MBLNR = MSEG~MBLNR
                AND MKPF~MJAHR = MSEG~MJAHR )
                    INNER JOIN PRPS
                 ON PRPS~PSPNR = MSEG~PS_PSP_PNR )
                    INNER JOIN MAKT
                 ON MAKT~MATNR = MSEG~MATNR )
                    INNER JOIN MARA                 "2007/02/13
                 ON MARA~MATNR = MSEG~MATNR )       "2007/02/13
   WHERE MSEG~MJAHR IN SMJAHR
     AND MSEG~BWART IN SBWART
     AND MSEG~MATNR IN SMATNR
     AND MSEG~WERKS IN SWERKS
     AND MSEG~LGORT IN SLGORT
     AND MSEG~AUFNR IN SAUFNR
     AND MKPF~BUDAT IN SBUDAT
     and mkpf~usnam in susnam                       "2010/01/18
     AND MKPF~VGART IN SVGART
     AND PRPS~POSID IN SPOSID
     AND MARA~MATKL IN SMATKL                       "2007/02/13
     AND MAKT~SPRAS EQ 'EN'.

     MOVE: MSEG-WERKS TO EXCLTAB-WERKS,
           MSEG-LGORT TO EXCLTAB-LGORT,
           MAKT-MAKTX TO EXCLTAB-MAKTX,
           MKPF-VGART TO EXCLTAB-VGART,
           MSEG-BWART TO EXCLTAB-BWART,
           MKPF-BUDAT TO EXCLTAB-BUDAT,
           MSEG-WEMPF TO EXCLTAB-WEMPF,
           MSEG-AUFNR TO EXCLTAB-AUFNR,
           PRPS-POSID TO EXCLTAB-POSID,
           MSEG-SAKTO TO EXCLTAB-SAKTO,
           MSEG-MBLNR TO EXCLTAB-MBLNR,
           mkpf-usnam to excltab-usnam,             "2010/01/18
           mkpf-xblnr to excltab-xblnr,             "2010/01/18
           mara-matkl to excltab-matkl.             "2007/02/13

*    WRITE MSEG-ERFMG TO EXCLTAB-ERFMG DECIMALS 0.  "2010/01/18
     concatenate: MSEG-MATNR+12(6) 'x' inTO EXCLTAB-MATNR.

     IF MSEG-SHKZG = 'H'.
        mseg-dmbtr = mseg-dmbtr * -1.
        mseg-erfmg = mseg-erfmg * -1.               "2010/01/18
     ENDIF.
     move mseg-dmbtr to excltab-dmbtr.
     WRITE MSEG-ERFMG TO EXCLTAB-ERFMG DECIMALS 0.  "2010/01/18

*     write mseg-dmbtr to excltab-dmbtr decimals 2.
     APPEND EXCLTAB.
     CLEAR  EXCLTAB.
  ENDSELECT.
ENDFORM.

*----------------------  DATA_SELECTION_WITH_IORDER --------------------
*  This subroutine selects data depending upon WBS data of variants.
*-----------------------------------------------------------------------
FORM DATA_SELECTION_WITH_IORDER.
  SELECT MSEG~WERKS MSEG~LGORT MARA~MATKL          "2007/02/13
         MSEG~MATNR MAKT~MAKTX MSEG~ERFMG
         MSEG~DMBTR MKPF~VGART MSEG~BWART MKPF~BUDAT MSEG~WEMPF
         mkpf~xblnr mseg~sakto                     "2010/01/18
*        MSEG~AUFNR MSEG~PS_PSP_PNR MSEG~SAKTO MSEG~MBLNR MSEG~SHKZG
         MSEG~AUFNR MSEG~PS_PSP_PNR MSEG~MBLNR MSEG~SHKZG "2010/01/18
         mkpf~usnam                                "2010/01/18
    INTO TABLE ITABLE
    FROM ( ( ( MSEG INNER JOIN MKPF
               ON MKPF~MBLNR = MSEG~MBLNR
              AND MKPF~MJAHR = MSEG~MJAHR )
                  INNER JOIN MAKT
               ON MAKT~MATNR = MSEG~MATNR )
                  INNER JOIN MARA
               ON MARA~MATNR = MSEG~MATNR )        "2007/02/13
   WHERE MSEG~MJAHR IN SMJAHR
     AND MSEG~BWART IN SBWART
     AND MSEG~MATNR IN SMATNR
     AND MSEG~WERKS IN SWERKS
     AND MSEG~LGORT IN SLGORT
     AND MSEG~AUFNR IN SAUFNR
     AND MKPF~BUDAT IN SBUDAT
     and mkpf~usnam in susnam                      "2010/01/18
     AND MKPF~VGART IN SVGART
     AND MARA~MATKL in SMATKL                      "2007/02/13
     AND MAKT~SPRAS EQ 'EN'.

IF NOT ITABLE[] IS INITIAL.
   PERFORM BUILD_EXCLTAB.
ENDIF.

ENDFORM.

*----------------------------  BUILD_EXCLTAB ---------------------------
*  This subroutine builds the EXCLTAB which is used for reporting.
*-----------------------------------------------------------------------
FORM BUILD_EXCLTAB.
  DATA: TEXT1(1) TYPE C.
   REFRESH EXCLTAB.
   LOOP AT ITABLE.
        move '______' to excltab-matnr.
        MOVE-CORRESPONDING ITABLE TO EXCLTAB.
        move itable-matnr+12(6) to excltab-matnr.
        move itable-matkl+0(6)  to excltab-matkl.
        WRITE ITABLE-ERFMG TO EXCLTAB-ERFMG DECIMALS 0.
        IF ITABLE-SHKZG = 'H'.
*           ITABLE-DMBTR = ITABLE-DMBTR * -1.
*           MOVE ITABLE-DMBTR TO EXCLTAB-DMBTR.
            concatenate '-' excltab-dmbtr into excltab-dmbtr.
            condense excltab-dmbtr no-gaps.                 "2010/01/18
           concatenate '-' excltab-erfmg into excltab-erfmg."2010/01/18
           condense excltab-erfmg no-gaps.                  "2010/01/18
        ENDIF.
        IF ITABLE-PSPNR <> SPACE.
           SELECT SINGLE POSID INTO EXCLTAB-POSID
             FROM PRPS
            WHERE PSPNR = ITABLE-PSPNR.
        ENDIF.
        APPEND EXCLTAB.
        CLEAR  EXCLTAB.
   ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM OUTPUT_ALV                                               *
*---------------------------------------------------------------------*
FORM OUTPUT_ALV.

DATA: WDATE1(10) TYPE C,
      WDATE2(10) TYPE C.

  CONCATENATE SBUDAT-LOW+0(4) '/' SBUDAT-LOW+4(2) '/'
              SBUDAT-LOW+6(2) INTO WDATE1.
  CONCATENATE SBUDAT-HIGH+0(4) '/' SBUDAT-HIGH+4(2) '/'
              SBUDAT-HIGH+6(2) INTO WDATE2.
  CONCATENATE TEXT-002 WDATE1 TEXT-003 WDATE2 INTO W_HEAD01
              SEPARATED BY SPACE.
  MOVE TEXT-016  TO W_HEAD02+0(5).
  WRITE SY-DATUM TO W_HEAD02+6(10).
  MOVE TEXT-017  TO W_HEAD02+18(5).
  WRITE SY-UZEIT TO W_HEAD02+25(10).

  MOVE TEXT-CLT  TO W_HEAD02+36(7).
  MOVE SY-MANDT  TO W_HEAD02+43(4).
  MOVE SY-SYSID  TO W_HEAD02+48(5).

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

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
     EXPORTING
        I_PROGRAM_NAME         = REPID
        I_INTERNAL_TABNAME     = 'EXCLTAB'
        I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
LOOP AT FIELDCAT INTO FC_STR.

CASE FC_STR-FIELDNAME.
     WHEN 'WERKS'.
          FC_STR-SELTEXT_L = TEXT-C01.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'LGORT'.
          FC_STR-SELTEXT_L = TEXT-C02.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'MATKL'.                                      "2007/0213
          FC_STR-SELTEXT_L = TEXT-C31.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'MATNR'.
          FC_STR-SELTEXT_L = TEXT-C03.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'MAKTX'.
          FC_STR-SELTEXT_L = TEXT-C04.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'ERFMG'.
          FC_STR-SELTEXT_L = TEXT-C05.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
          fc_str-just = 'R'.                    "2010/01/18
     WHEN 'DMBTR'.
         FC_STR-SELTEXT_L = TEXT-C06.          " Alternative col header
*         FC_STR-DDICTXT = 'X'.                 " Use Large system text
         FC_STR-ddictxt = 'L'.
*          FC_STR-DO_SUM  = 'X'.                 " Do Sum
         fc_str-just = 'R'.                    "2010/01/18
     WHEN 'VGART'.
          FC_STR-SELTEXT_L = TEXT-C07.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'BWART'.
          FC_STR-SELTEXT_L = TEXT-C08.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'BUDAT'.
          FC_STR-SELTEXT_L = TEXT-C09.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
     WHEN 'WEMPF'.
          FC_STR-SELTEXT_S = TEXT-C10.          " Alternative col header
          FC_STR-DDICTXT = 'S'.                 " Use small system text
     WHEN 'AUFNR'.
          FC_STR-SELTEXT_S = TEXT-C11.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'POSID'.
          FC_STR-SELTEXT_L = TEXT-C12.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'SAKTO'.
          FC_STR-SELTEXT_L = TEXT-C13.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'MBLNR'.
          FC_STR-KEY    = ' '.                " Key columns -not first
          FC_STR-SELTEXT_L = TEXT-C14.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'USNAM'.                             "2010/01/18
          FC_STR-SELTEXT_L = TEXT-C33.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'XBLNR'.                             "2010/01/18
          FC_STR-SELTEXT_L = TEXT-C32.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN OTHERS.
* fc_str-no_out = 'X'.           " hide column
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

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
           T_OUTTAB = EXCLTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.


ENDFORM.
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- SELECTION LINE: TYPE S
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
*  LS_LINE-KEY   = 'CLIENT: '.
  LS_LINE-INFO  = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
*  ls_line-typ  = 'S'.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
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
  LOOP AT EXCLTAB.
  IF EXCLTAB-POSID <> SPACE.
     CONCATENATE EXCLTAB-POSID+0(2) '-' EXCLTAB-POSID+2(2) '-'
                 EXCLTAB-POSID+4(3) '-' EXCLTAB-POSID+7(4)
                 INTO EXCLTAB-POSID.
     MODIFY EXCLTAB.
  ENDIF.
  ENDLOOP.

        LT_FNAMES-TEXT = TEXT-C01.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C02.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C31.     "2007/02/13
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C03.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C04.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C05.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C06.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C07.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C08.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C09.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C10.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C32.        "2010/01/18
        APPEND LT_FNAMES.                 "2010/01/18
        LT_FNAMES-TEXT = TEXT-C13.        "2010/01/18
        APPEND LT_FNAMES.                 "2010/01/18
        LT_FNAMES-TEXT = TEXT-C11.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C12.
        APPEND LT_FNAMES.
*       LT_FNAMES-TEXT = TEXT-C13.        "2010/01/18
*       APPEND LT_FNAMES.                 "2010/01/18
        LT_FNAMES-TEXT = TEXT-C14.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-C33.        "2010/01/18
        APPEND LT_FNAMES.                 "2010/01/18


  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
       EXPORTING
*          FILE_NAME                 = 'C:\SAPTEMP'  "TR995
            FILE_NAME                 = P_FILE       "TR995
            CREATE_PIVOT              = 0
*           DATA_SHEET_NAME           = ' '
*           PIVOT_SHEET_NAME          = ' '
*           PASSWORD                  = ' '
*           PASSWORD_OPTION           = 0
       TABLES
*           PIVOT_FIELD_TAB           =
            DATA_TAB                  = EXCLTAB
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
