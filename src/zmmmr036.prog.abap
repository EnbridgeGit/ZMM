REPORT ZMMMR036 no standard page heading LINE-Size 132 line-count 65
       message-id zm.



******************************************************************
*       Owner: Centra/Union                                      *
*  Programmer: Marv Radsma                                       *
*        Date: Dec 11, 1996                                      *
*  Request ID: DRMM0132                                          *
*                                                                *
* This program produces a report listing hazardous materials.    *
* The output will depend on the supplied parameters ie. only     *
* the description is provided when no additional selection data  *
* is provided.                                                   *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YY/MM/DD - USERID - MOD# - DESCRIPTION                         *
* -------------------------------------------------------------- *
*          -        -      -                                     *
* 2012/08/30 M Khan   TR995 Change C: drive to H: drive with     *
*                     directory, file selection using F4 & move  *
*                     the hard-coded file path/name to variant.  *
* 2008/09/19  mdemeest TR277 - Add Hazardous Material parameter  *
*                              to variant, reformat report       *
* 98/06/16 - md7140 - #531 - Amend selection criteria            *
******************************************************************
TYPE-POOLS:  SLIS.               "For EXCEL SPREADSHEET

TABLES: ENT1036,   "Material Master with description (MARA & MAKT)
        MARD,      "Material master, storage location/batch segment
        marc.      "Plant description

*TABLES: MARA,      "Material master, general data
*        MAKT,      "Material descriptions

DATA:   LN_CNTR    TYPE I,                        "Line Counter
        BEGIN OF MAT_TAB     OCCURS 5000,         "Required data
            WERKS    LIKE MARD-WERKS,             "Plant
            LGORT    LIKE MARD-LGORT,             "Storage Location
            MATNR(8)  type c,                         "material Number
            MAKTX    LIKE ent1036-maktx,              "Material  Desc
            INSME(17) type c, "LIKE MARD-INSME,       "Quantity on Hand
            LGPBE(12) type c, "LIKE MARD-LGPBE,       "Bin Location
            stoff    like ent1036-stoff,            "Hazardous material
        END OF MAT_TAB.

* This first block produces a border like frame around the following: *

SELECTION-SCREEN BEGIN OF BLOCK INTRO WITH FRAME title text-001.


SELECTION-SCREEN SKIP.

* The following second block produces a frame within the first block. *

SELECTION-SCREEN BEGIN OF BLOCK criteria with frame title text-101.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:
  s_plant     for marc-werks modif id abc.        "Plant code

SELECTION-SCREEN SKIP.
select-options:
  s_storag    for mard-lgort modif id abc.        "Storage Location

SELECTION-SCREEN SKIP.
select-options:
  s_stoff     for ent1036-stoff modif id abc.

selection-screen begin of line.
selection-screen comment 5(75)  text-200.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 5(60)  text-201.
selection-screen end of line.



SELECTION-SCREEN END OF BLOCK criteria.


SELECTION-SCREEN SKIP.


SELECTION-SCREEN BEGIN OF BLOCK reportoptions with frame title text-100.
SELECTION-SCREEN SKIP.

parameters:
  p_rprt  radiobutton group RBRC,               "report
  p_excl  radiobutton group RBRC,               "excel
  p_file like rlgrap-filename DEFAULT 'H:\SAPTEMP\ZMMMR036'. "TR995

SELECTION-SCREEN SKIP.

SELECTION-SCREEN END OF BLOCK reportoptions.


SELECTION-SCREEN SKIP.

* This statement ends the first block. *

SELECTION-SCREEN END OF BLOCK INTRO.

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
*----------------------------- TOP-OF-PAGE -----------------------------
top-of-page.
 write: /1 text-rpt, sy-repid, 40 text-001, 105 text-dte, sy-datum,
          text-amp, sy-uzeit.
write: / text-clt under text-rpt, sy-mandt under sy-repid, sy-sysid,
         text-pge under text-dte, sy-pagno under sy-datum.
uline.
write: /1 text-002, 8 text-003,  15 text-005,
       25 text-007, 70 text-008, 90 text-010,
      105 text-011.
uline.


initialization.
* loop at s_stoff.
   move 'IBT0                 ZZZ               ' to s_stoff.
   append s_stoff.
* endloop.


* The following will highlight the screens output for certain texts *
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.


*********************BEGINNING OF MAIN PROGRAM *************************

START-OF-SELECTION.

 SELECT * FROM ent1036
    where stoff in s_stoff
      and  spras = sy-langu.
    MOVE ent1036-MATNR+12(6) to MAT_TAB-MATNR.
    MOVE ent1036-MAKTX TO MAT_TAB-MAKTX.
    move ent1036-stoff to mat_tab-stoff.

    IF  S_PLANT  NE SPACE
    AND S_STORAG NE SPACE.
      SELECT * FROM MARD
      WHERE  MATNR = ent1036-MATNR AND
             WERKS IN S_PLANT      AND
             LGORT IN S_STORAG.
        MOVE ent1036-MATNR+12(6) TO MAT_TAB-MATNR.
        MOVE ent1036-MAKTX TO MAT_TAB-MAKTX.
        move ent1036-stoff to mat_tab-stoff.
        MOVE MARD-WERKS    TO MAT_TAB-WERKS.
        MOVE MARD-LGORT    TO MAT_TAB-LGORT.
        MOVE MARD-INSME    TO MAT_TAB-INSME.
        ADD  MARD-LABST    TO MAT_TAB-insme.
        MOVE MARD-LGPBE    TO MAT_TAB-LGPBE.
        APPEND MAT_TAB.
        CLEAR  MAT_TAB.
      ENDSELECT.
    ELSE.
      IF  S_PLANT NE SPACE.
        SELECT * FROM MARD
        WHERE  MATNR = ent1036-MATNR AND
               WERKS IN S_PLANT.
          MOVE ent1036-MATNR+12(6) TO MAT_TAB-MATNR.
          MOVE ent1036-MAKTX TO MAT_TAB-MAKTX.
          move ent1036-stoff to mat_tab-stoff.
          MOVE MARD-WERKS    TO MAT_TAB-WERKS.
          MOVE MARD-LGORT    TO MAT_TAB-LGORT.
          MOVE MARD-INSME    TO MAT_TAB-INSME.
          ADD  MARD-LABST    TO MAT_TAB-insme.
          MOVE MARD-LGPBE     TO MAT_TAB-LGPBE.
          concatenate '_' mat_tab-lgpbe '_' into mat_tab-lgpbe.
          if mat_tab-lgpbe = '__'.
             move space to mat_tab-lgpbe.
          endif.
          APPEND MAT_TAB.
          CLEAR  MAT_TAB.
        ENDSELECT.
      ELSE.
        IF  S_STORAG NE SPACE.
          SELECT * FROM MARD
          WHERE  MATNR = ent1036-MATNR
          AND    LGORT IN S_STORAG.
            MOVE ent1036-MATNR+12(6) TO MAT_TAB-MATNR.
            MOVE ent1036-MAKTX TO MAT_TAB-MAKTX.
            move ent1036-stoff to mat_tab-stoff.
            MOVE MARD-WERKS    TO MAT_TAB-WERKS.
            MOVE MARD-LGORT    TO MAT_TAB-LGORT.
            MOVE MARD-INSME    TO MAT_TAB-INSME.
            ADD  MARD-LABST    TO MAT_TAB-insme.
            MOVE MARD-LGPBE    TO MAT_TAB-LGPBE.
            concatenate '_' mat_tab-lgpbe '_' into mat_tab-lgpbe.
             if mat_tab-lgpbe = '__'.
             move space to mat_tab-lgpbe.
            endif.

            APPEND MAT_TAB.
            CLEAR  MAT_TAB.
          ENDSELECT.
        ELSE.
          APPEND MAT_TAB.
          CLEAR  MAT_TAB.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDSELECT.                                      "End of ENT1036 select
END-OF-SELECTION.

 SORT MAT_TAB BY WERKS LGORT MATNR.

* Process the table, looping thru and outputing the detail liness. *
if p_rprt = 'X'.                       "Print Report
   LOOP AT MAT_TAB.
     AT NEW WERKS.
        skip 2.
     endat.
     at new LGORT.
        skip 1.
     endat.

     write: / mat_tab-werks under text-002,
              mat_tab-lgort under text-003,
              MAT_TAB-MATNR      under text-005,
              MAT_TAB-MAKTX      under text-007,
              MAT_TAB-INSME      under text-008,
              MAT_TAB-LGPBE      under text-010,
              mat_tab-stoff      under text-011.
   ENDLOOP.
else.
if p_excl = 'X'.
*   perform display_alv.
   perform display_excel.
endif. endif.

form display_alv.
data:  fieldcat type slis_t_fieldcat_alv,
       fc_str   type slis_fieldcat_alv,
       layout   type slis_layout_alv,
       title    type lvc_title,
       repid    like sy-repid,
       variant like disvariant,
       sort     type slis_t_sortinfo_alv,
       sort_sir type slis_sortinfo_alv.

       Layout-colwidth_optimize = 'x'.

       fc_str-fieldname = 'WERKS'.   "Plant
       FC_str-KEY       = ' '.
       fc_str-seltext_l = text-002.
       fc_str-ddictxt   ='L'.
       append fc_str to fieldcat.
       clear fc_str.


       fc_str-fieldname = 'LGORT'.    "Storage Location
       FC_str-KEY       = ' '.
       fc_str-seltext_l = text-003.
       fc_str-ddictxt   ='L'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'MATNR'.    "Material
       FC_str-KEY       = ' '.
       fc_str-seltext_l = text-005.
       fc_str-ddictxt   ='R'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'MAKTX'.    "Material Description
       FC_str-KEY       = ' '.
       fc_str-seltext_l = text-007.
       fc_str-ddictxt   ='L'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'INSME'.      "Quantity on Hand
       FC_str-KEY       = ' '.
       fc_str-seltext_l = text-008.
       fc_str-ddictxt   ='L'.
       fc_str-just = 'R'.               "right justify
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'LGPBE'.     "Bin Location
       FC_str-KEY       = ' '.
       fc_str-seltext_l = text-010.
       fc_str-ddictxt   ='L'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'STOFF'.     "Hazardous Material Ind.
       FC_str-KEY       = ' '.
       fc_str-seltext_l = text-011.
       fc_str-ddictxt   ='L'.
       append fc_str to fieldcat.
       clear fc_str.

 call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
           I_CALLBACK_PROGRAM     =  sy-repid
    TABLES
           T_OUTTAB = MAT_TAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.



endform.

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: DATUM1(10).
  DATA: UZEIT1(10).
  data: W_HEAD01(132) type c.

   move 'Plant(s):  ' to w_head01.
   LOOP AT S_plant.
     IF S_plant+1(2) = 'EQ'.
        CONCATENATE W_HEAD01 S_plant+3(4) INTO W_HEAD01 SEPARATED BY
'  '.
     ELSEIF S_plant+1(2) = 'BT'.
        CONCATENATE W_HEAD01 S_plant+3(4) INTO W_HEAD01 SEPARATED BY
'  '.
        CONCATENATE W_HEAD01 S_plant+7(4) INTO W_HEAD01 SEPARATED BY
'-'.
     ENDIF.
   ENDLOOP.

IF W_HEAD01+28(1) = ';'.
   MOVE ' ' TO W_HEAD01+28(1).
ENDIF.

WRITE SY-DATUM TO DATUM1 DD/MM/YYYY.
WRITE SY-UZEIT TO UZEIT1 USING EDIT MASK '__:__:__'.
*CONCATENATE TEXT-CLT SY-SYSID SY-MANDT TEXT-DTE DATUM1 '@' UZEIT1
*            INTO W_HEAD02 SEPARATED BY SPACE.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- SELECTION LINE: TYPE S
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'S'.
  LS_LINE-KEY   = ' '.
  LS_LINE-INFO  = W_HEAD01.
*  CONCATENATE SY-SYSID SY-MANDT INTO LS_LINE-INFO
*              SEPARATED BY SPACE.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
*  ls_line-typ  = 'A'.
  LS_LINE-TYP   = 'S'.
  LS_LINE-KEY  = ' '.
*  LS_LINE-INFO  = W_HEAD02.
*  WRITE SY-DATUM TO DATUM1 DD/MM/YYYY.
*  WRITE SY-UZEIT TO UZEIT1 USING EDIT MASK '__:__:__'.
*  CONCATENATE DATUM1 '@' UZEIT1 INTO LS_LINE-INFO
*                     SEPARATED BY SPACE.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.

ENDFORM.                               " ALV_TOP_OF_PAGE

FORM DISPLAY_EXCEL.
    DATA: BEGIN OF LT_FNAMES OCCURS 0,
          TEXT(60) TYPE C,
          END OF LT_FNAMES.

        LT_FNAMES-TEXT = TEXT-002.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-003.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-005.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-007.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-008.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-010.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-011.
        APPEND LT_FNAMES.


  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
       EXPORTING
*           FILE_NAME                 = 'C:\SAPTEMP'"TR995
            FILE_NAME                 = P_FILE      "TR995
            CREATE_PIVOT              = 0
*           DATA_SHEET_NAME           = ' '
*           PIVOT_SHEET_NAME          = ' '
*           PASSWORD                  = ' '
*           PASSWORD_OPTION           = 0
       TABLES
*           PIVOT_FIELD_TAB           =
            DATA_TAB                  = MAT_TAB
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
