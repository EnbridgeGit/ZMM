REPORT ZMMMR131 LINE-SIZE 256 NO STANDARD PAGE HEADING LINE-COUNT 90.

******************************************************************
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YY/MM/DD - USERID - MOD# - DESCRIPTION                         *
* -------------------------------------------------------------- *
*  2009/10/28 mdemeest TR758 Rewrite according to DBossy specs   *
******************************************************************
*
*  ZMMMR131 - Material Status Report
*
*-----------------------------------------------------------------
type-pools: slis.
TABLES: ent1027,   "View of MARA & MAKT
        eina,      "Info Record
        lfa1,      "Vendor Name
        cabn,      "Characteristic code
        ausp,      "Characteristics
        T141T,     "Descriptions of Material Status from MM/PP View
        MARC,      "Material Master: C Segment
        MARD.      "Material Master: Storage Location/Batch Segment

DATA:  BEGIN OF EXCLTAB     OCCURS 50,               "material data
          matkl    like ent1027-matkl,            "Material Group
          matnr    like ent1027-matnr,            "Material Number
          maktx    like ent1027-maktx,            "Material Description
*          bukrs(4) type c,                          "fix
          werks    like marc-werks,               "Plant
          lgort    like mard-lgort,               "Storage Location
          qoh(15) type c,                           "fix
          dismm    like marc-dismm,               "MRP Type
          atwrt    like ausp-atwrt,               "NLA Characteristic
          mstae    like ent1027-mstae,            "X-plant Status
          MMSTA    LIKE MARC-MMSTA,               "MATERIAL STATUS
          lvormx   like ent1027-lvorm,            "Material Flag for Del
          lvormp   like marc-lvorm,               "Plant flag for del
          infnr(30) type c,                       "Info Record
          loekz(3)  type c,                       "Info Record status
          lifnr(75) type c,                       "Vendor#&Name

        END OF EXCLTAB.

data: w_head01(100) type c,
      w_head02(100) type c.


SELECTION-SCREEN begin of block general with frame title text-001.

select-options:
*   s_bukrs for mara-bukrs,                   "Company Code
   s_werks for marc-werks,                   "Plant
   s_lgort for mard-lgort.                   "Storage Location
selection-screen end of block general.

selection-screen begin of block general2 with frame.
select-options:
   s_matkl for ent1027-matkl,                   "Material Group
   s_matnr for ent1027-matnr,                   "Material Number
   s_dismm for marc-dismm,                      "MRP Type
   s_atwrt for ausp-atwrt no intervals.         "Characteristic NLA code
selection-screen end of block general2.

selection-screen begin of block statuses with frame.
select-options:
   s_mstae for ent1027-mstae,                   "Xplant status
   s_mmsta for marc-mmsta.                   "plant specific status
selection-screen end of block statuses.

selection-screen begin of block flags with frame..
parameters:
   p_lvormm like ent1027-lvorm,                  "Delete flag-material
   p_lvormp like marc-lvorm.                  "Delete flag-plant
selection-screen end of block flags.


SELECTION-SCREEN begin of block reports with frame title text-002.
parameter:  p_rept  radiobutton group RBRC,     "Report
            p_excel radiobutton group RBRC default 'X'.     "Excel
SELECTION-SCREEN end of block reports.




*********************BEGINNING OF MAIN PROGRAM *************************
TOP-OF-PAGE.
*  PERFORM TOP-TITLES.
*  PERFORM HEADERS.

START-OF-SELECTION.
  select * from ent1027
    where matnr in s_matnr
      and matkl in s_matkl
      and lvorm = p_lvormm
      and mstae in s_mstae.
    clear excltab.
    move-corresponding ent1027 to excltab.
    move ent1027-lvorm to excltab-lvormx.
    perform info_record_info.
    select single * from marc
       where werks in s_werks
         and matnr = ent1027-matnr
         and dismm in s_dismm
         and mmsta in s_mmsta
         and lvorm = p_lvormp.
    if sy-subrc <> '0'.
       append excltab.
    endif.
    select * from marc
       where werks in s_werks
         and matnr = ent1027-matnr
         and dismm in s_dismm
         and mmsta in s_mmsta
         and lvorm = p_lvormp..
         move-corresponding marc to excltab.
         move marc-lvorm         to excltab-lvormp.
         select single * from MARD
              where werks = marc-werks
                   and lgort in s_lgort
                   and matnr = marc-matnr.
         if sy-subrc = '4'.
            append excltab.
         endif.
         select * from MARD
             where werks = marc-werks
               and lgort in s_lgort
               and matnr = marc-matnr.
               move-corresponding mard to excltab.
               move mard-labst         to excltab-qoh.
               append excltab.
          endselect.              "End of MARD
    endselect.                    "End of MARC
 endselect.                      "End of ENT1027

  select single * from cabn
     where atnam = 'NLA_CODE'.
  if sy-subrc = '0'.
     loop at excltab.
        select single * from ausp
          where objek = excltab-matnr
            and atinn = cabn-atinn
            and atwrt in s_atwrt.
          if sy-subrc = '0'.
             move ausp-atwrt to excltab-atwrt.
             modify excltab.
          else.
*            remove any material that does not have
*            the NLA_Code as listed in variant
             if s_atwrt = ' '.
             else.
                 delete excltab.
             endif.
          endif.
     endloop.
  endif.


  if p_rept = 'X'.
     perform output_alv.
  else.
     perform output_excel.
  endif.

  perform print_selection_screen.

*---------------------------------------------------------------------

form INFO_RECORD_INFO.
  clear excltab-infnr.
  select * from eina
    where matnr = ent1027-matnr.
    if excltab-infnr = ' '.
       move eina-infnr to excltab-infnr.
       move eina-loekz to excltab-loekz.
       move eina-lifnr to excltab-lifnr.
       select single * from lfa1
         where lifnr = eina-lifnr.
       if sy-subrc  = '0'.
         concatenate excltab-lifnr lfa1-name1
           into excltab-lifnr separated by '_'.
       endif.
    else.
       concatenate excltab-infnr eina-infnr
              into excltab-infnr separated by '^'.

       concatenate excltab-loekz eina-loekz
              into excltab-loekz separated by '^'.

       concatenate excltab-lifnr eina-lifnr
              into excltab-lifnr separated by '^'.
       select single * from lfa1
         where lifnr = eina-lifnr.
       if sy-subrc  = '0'.
         concatenate excltab-lifnr lfa1-name1
           into excltab-lifnr separated by '_'.
       endif.

    endif.
  endselect.
endform.

FORM HEADERS.
   write: 1 text-101, 10 text-102, 15 text-103,
          20 text-104, 25 text-105, 30 text-106,
          35 text-107, 40 text-108, 45 text-109,
          50 text-110, 55 text-111, 60 text-112,
          65 text-113, 70 text-114, 75 text-115,
          80 text-116.
    uline.
    SKIP.

ENDFORM.

form print_report.
  perform headers.
  sort excltab by matkl.
endform.

FORM OUTPUT_EXCEL.
  DATA: BEGIN OF LT_FNAMES OCCURS 0,
        TEXT(60) TYPE C,
        END OF LT_FNAMES.


        LT_FNAMES-TEXT = TEXT-101.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-102.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-103.     "2007/02/13
        APPEND LT_FNAMES.
*        LT_FNAMES-TEXT = TEXT-104.
*        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-105.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-106.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-107.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-108.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-109.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-110.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-111.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-112.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-113.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-114.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-115.
        APPEND LT_FNAMES.
        LT_FNAMES-TEXT = TEXT-116.
        APPEND LT_FNAMES.

  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
       EXPORTING
            FILE_NAME                 = 'C:\SAPTEMP'
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

*---------------------------------------------------------------------*
*       FORM OUTPUT_ALV                                               *
*---------------------------------------------------------------------*
FORM OUTPUT_ALV.

DATA: WDATE1(10) TYPE C,
      WDATE2(10) TYPE C.



*  CONCATENATE SBUDAT-LOW+0(4) '/' SBUDAT-LOW+4(2) '/'
*              SBUDAT-LOW+6(2) INTO WDATE1.
*  CONCATENATE SBUDAT-HIGH+0(4) '/' SBUDAT-HIGH+4(2) '/'
*              SBUDAT-HIGH+6(2) INTO WDATE2.
*  CONCATENATE TEXT-002 WDATE1 TEXT-003 WDATE2 INTO W_HEAD01
*              SEPARATED BY SPACE.
  MOVE TEXT-DTE TO W_HEAD01+0(7).
  WRITE SY-DATUM TO W_HEAD01+8(10).
  MOVE TEXT-amp  TO W_HEAD01+19(5).
  WRITE SY-UZEIT TO W_HEAD01+25(10).


  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-MANDT  TO W_HEAD02+8(4).
  MOVE SY-SYSID  TO W_HEAD02+14(5).

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
     WHEN 'MATKL'.
          FC_STR-SELTEXT_L = TEXT-C01.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'MATNR'.
          FC_STR-SELTEXT_L = TEXT-C02.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'MAKTX'.
          FC_STR-SELTEXT_L = TEXT-C31.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'WERKS'.
          FC_STR-SELTEXT_L = TEXT-105.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'LGORT'.
          FC_STR-SELTEXT_L = TEXT-C05.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'QOH'.
          FC_STR-SELTEXT_L = TEXT-107.          " Alternative col header
          FC_STR-ddictxt = 'R'.
*          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'DISMM'.                              "MRP Type
          FC_STR-SELTEXT_L = TEXT-C07.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     when 'ATWRT'.
          FC_STR-SELTEXT_L = TEXT-109.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'MMSTA'.
          FC_STR-SELTEXT_L = TEXT-C08.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'MSTAE'.
          FC_STR-SELTEXT_L = TEXT-C09.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
     WHEN 'LVORMM'.
          FC_STR-SELTEXT_S = TEXT-C10.          " Alternative col header
          FC_STR-DDICTXT = 'S'.                 " Use small system text
     WHEN 'LVORMP'.
          FC_STR-SELTEXT_S = TEXT-C11.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'INFNR'.
          FC_STR-SELTEXT_L = TEXT-114.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'LOEKZ'.
          FC_STR-SELTEXT_L = TEXT-115.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'LIFNR'.
          FC_STR-KEY    = ' '.                " Key columns -not first
          FC_STR-SELTEXT_L = TEXT-116.        " Alternative col header
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
form print_selection_screen.
  data:  begin of t_scr_image occurs 0,
           line(120) type c,
          end of t_scr_image.
  call function 'PRINT_SELECTIONS'
          exporting
              mode = 'I'
              rname = sy-cprog
              rvariante = sy-slset
              tables
              infotab = t_scr_image.
  loop at t_scr_image.
    write : / t_scr_image-line+2.
  endloop.
endform.
