******************************************************************
*                                                                *
*   PROGRAM: ZMMR_CHANGE_DOCUMENT                                *
*   AUTHOR:  Larry Ritchie                                       *
*   CREATED: 2010/02/17                                          *
*                                                                *
*   DESCRIPTION: This report will list the change documents on   *
*                material masters.                               *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
* 2010/02/17 LRITCHIE TR 801 New program for DBossy              *
* 2012/01/10 SAHMAD   Copy of ZMMMR064 and Add Plant net Impact of
*                     Changed material
* 2012/05/01 M Khan   QR163 Calculation Correcetion.             *
*
* 2012/08/13 M Khan   TR995 Change C: drive to H: drive with     *
*                           directory, file selection using F4 & *
*                           move the hard-coded file path/name to*
*                           variant.                             *
* 2014/11/20 PANUSURI SDP77194 Include MRP type 'Z1'.            *
* 2019/06/01 AKMADASU CHG0148060-RTSK0074548 Adding material     *
*                     description to the output of tcode ZLMMR009*
*                     D30K929901                                 *
******************************************************************

REPORT zlmmr009_doa_material_change LINE-SIZE 255 NO STANDARD PAGE HEADING LINE-COUNT 65.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

TABLES: cdhdr,                          "Change document header
        cdpos,                          "Change document item
        dd02t,                          "Table text
        dd03l,                          "Table fields
        dd04t,                          "Field text
        mara,                           "Material master
        t001w.

TYPE-POOLS  slis.

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* change document header data
DATA: BEGIN OF tbl_cdhdr OCCURS 0,
        objectid   LIKE cdhdr-objectid,
        changenr   LIKE cdhdr-changenr,
        username   LIKE cdhdr-username,
        udate      LIKE cdhdr-udate,
        utime      LIKE cdhdr-utime,
        tcode      LIKE cdhdr-tcode,
      END OF tbl_cdhdr.

* change document item data
DATA: BEGIN OF tbl_cdpos OCCURS 0,
        objectid   LIKE cdpos-objectid,
        changenr   LIKE cdpos-changenr,
        tabname    LIKE cdpos-tabname,
        tabkey     LIKE cdpos-tabkey,
        fname      LIKE cdpos-fname,
        chngind    LIKE cdpos-chngind,
        value_new  LIKE cdpos-value_new,
        value_old  LIKE cdpos-value_old,
      END OF tbl_cdpos.
* table names
DATA: BEGIN OF tbl_dd02t OCCURS 16,
         tabname   LIKE dd02t-tabname,
         ddtext    LIKE dd02t-ddtext,
      END OF tbl_dd02t.

* temporary table to get table names
DATA: BEGIN OF tbl_tabname OCCURS 0,
        tabname    LIKE dd02t-tabname,
      END OF tbl_tabname.

* table & field combinations
DATA: BEGIN OF tbl_dd03l OCCURS 782,
         tabname   LIKE dd03l-tabname,
         fieldname LIKE dd03l-fieldname,
         rollname  LIKE dd03l-rollname,
      END OF tbl_dd03l.

* field names
DATA: BEGIN OF tbl_dd04t OCCURS 674,
         rollname  LIKE dd04t-rollname,
         ddtext    LIKE dd04t-ddtext,
      END OF tbl_dd04t.

* temporary table to get field names
DATA: BEGIN OF tbl_rollname OCCURS 0,
         rollname  LIKE dd04t-rollname,
      END OF tbl_rollname.

* report table
DATA: BEGIN OF tbl_report OCCURS 0,
        matnr      LIKE mara-matnr,
**--START OF CHANGES BY AKMADASU FOR CHG0148060
        MAKTX      LIKE MAKT-MAKTX,
**-- END OF CHANGES BY AKMADASU FOR CHG0148060
        changenr   LIKE cdhdr-changenr,
        username   LIKE cdhdr-username,
        udate      LIKE cdhdr-udate,
        utime      LIKE cdhdr-utime,
        tcode(10)        TYPE c,
        change_status(6) TYPE c,
        fdesc      LIKE dd02t-ddtext,
        value_old(50) TYPE c,
        value_new(50) TYPE c,
        tabkey(35)    TYPE c,
        tabname(10)   TYPE c,
        fname(15)     TYPE c,
        werks LIKE marc-werks,
        verpr LIKE mbew-verpr,
        netim LIKE mbew-vmsal,
       END OF tbl_report.
*data: begin of tbl_report1 occurs 0,
*        matnr      like mara-matnr,
*        changenr   like cdhdr-changenr,
*        username   like cdhdr-username,
*        udate      like cdhdr-udate,
*        utime      like cdhdr-utime,
*        tcode(10)        type c,
*        change_status(6) type c,
*        fdesc      like dd02t-ddtext,
*        value_new(50) type c,
*        value_old(50) type c,
*        werks like marc-werks,
*        VERPR like MBEW-VERPR,
*        NETIM like MBEW-VMSAL,
*      end of tbl_report1.

*  internal table for field catalog.
DATA : tbl_fieldtab TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       tbl_fieldcat TYPE slis_fieldcat_alv.

RANGES: r_table FOR cdpos-tabname,
        r_tabname FOR cdpos-tabname,
        r_rollname FOR dd03l-rollname.

* EXCEL table
*DATA: tbl_excel LIKE TABLE OF tbl_report WITH HEADER LINE.
DATA: BEGIN OF tbl_excel OCCURS 0,
        matnr      LIKE mara-matnr,
**--START OF CHANGES BY AKMADASU FOR CHG0148060
        MAKTX      LIKE MAKT-MAKTX,
**-- END OF CHANGES BY AKMADASU FOR CHG0148060
        changenr   LIKE cdhdr-changenr,
        username   LIKE cdhdr-username,
        udate      LIKE cdhdr-udate,
        utime      LIKE cdhdr-utime,
        tcode(10)        TYPE c,
        change_status(6) TYPE c,
        fdesc      LIKE dd02t-ddtext,
        value_old(50) TYPE c,
        value_new(50) TYPE c,
        tabkey(35)    TYPE c,
        tabname(10)   TYPE c,
        fname(15)     TYPE c,

      END OF tbl_excel.

DATA: BEGIN OF tbl_excel1 OCCURS 0,
        matnr      LIKE mara-matnr,
**--START OF CHANGES BY AKMADASU FOR CHG0148060
        MAKTX      LIKE MAKT-MAKTX,
**-- END OF CHANGES BY AKMADASU FOR CHG0148060
        changenr   LIKE cdhdr-changenr,
        username   LIKE cdhdr-username,
        udate      LIKE cdhdr-udate,
        utime      LIKE cdhdr-utime,
        tcode(10)        TYPE c,
        change_status(6) TYPE c,
        fdesc      LIKE dd02t-ddtext,
        value_old(50) TYPE c,
        value_new(50) TYPE c,
        werks LIKE marc-werks,
        verpr(14) TYPE c, " LIKE mbew-verpr,
        netim(14) TYPE c, " LIKE mbew-vmsal,
      END OF tbl_excel1.
* EXCEL table header
DATA:  BEGIN OF tbl_excel_header OCCURS 1,
         spaltenname(20)  TYPE c,
         ddic_table(5)    TYPE c,
         ddic_field(5)    TYPE c,
         key              TYPE c,
       END OF tbl_excel_header.

DATA: BEGIN OF gt_data OCCURS 0,
        objectid   LIKE cdpos-objectid,
        changenr   LIKE cdpos-changenr,
        tabname    LIKE cdpos-tabname,
        tabkey     LIKE cdpos-tabkey,
        fname      LIKE cdpos-fname,
        chngind    LIKE cdpos-chngind,
        value_old  LIKE cdpos-value_old,
        value_new  LIKE cdpos-value_new,
        fdesc      LIKE dd02t-ddtext,
      END OF gt_data.

DATA: gt_cdpos LIKE TABLE OF gt_data,
      gs_cdpos LIKE LINE OF gt_cdpos.
DATA: gv_netim1      TYPE mbew-salk3.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

DATA: v_error(1)      TYPE c,
      v_days          TYPE i,
      v_marm(1)       TYPE c,
      v_makt(1)       TYPE c,
      v_mlan(1)       TYPE c.

* ALV stuff
DATA: st_layout TYPE slis_layout_alv,
      st_sort  TYPE slis_sortinfo_alv OCCURS 0,
      st_events TYPE slis_t_event,
      v_repid LIKE sy-repid.

DATA: v_callback_subroutine TYPE slis_formname VALUE 'USER_COMMAND'.

DATA: st_line TYPE slis_listheader.
DATA: tbl_top_of_page TYPE slis_t_listheader.
DATA: v_head01(100) TYPE c,
      v_head02(100) TYPE c.

******************************************************************
*                   SELECTION SCREENS                            *
******************************************************************

* block 1
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-011.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS s_matnr FOR mara-matnr.               "material
SELECT-OPTIONS s_chgnr FOR cdhdr-changenr.           "change number
SELECT-OPTIONS s_user  FOR cdhdr-username.           "SAP user
SELECT-OPTIONS s_date  FOR cdhdr-udate OBLIGATORY    "Date
                                       DEFAULT sy-datum.
SELECT-OPTIONS s_utime FOR cdhdr-utime.
SELECT-OPTIONS s_tcode FOR cdhdr-tcode.              "Tcode
SELECTION-SCREEN END OF BLOCK b1.

* block 2
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-008.
*SELECTION-SCREEN SKIP 1.
*PARAMETERS p_all RADIOBUTTON GROUP r2.          "All change documents
*PARAMETERS p_choose RADIOBUTTON GROUP r2 DEFAULT 'X'.       "Specific change docs
*
SELECTION-SCREEN BEGIN OF BLOCK b2_sub WITH FRAME TITLE text-014.

**SELECTION-SCREEN BEGIN OF LINE.
**SELECTION-SCREEN COMMENT 1(5) text-001.         "spaces
*PARAMETERS p_mara AS CHECKBOX.                  "Basic material
**SELECTION-SCREEN COMMENT 10(40) text-002.
**SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(5) text-001.         "spaces
PARAMETERS p_marc RADIOBUTTON GROUP grp1. "CHECKBOX.                  "Plant
*SELECTION-SCREEN COMMENT 10(40) text-003.
*SELECTION-SCREEN COMMENT 3(20) text-003.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(5) text-001.         "spaces
PARAMETERS: p_pdni RADIOBUTTON GROUP grp1. "AS CHECKBOX.                  "Plant data net impact
*            p_werks type MARC-WERKS.
*SELECTION-SCREEN COMMENT 10(25) text-012.
SELECTION-SCREEN COMMENT 3(25) text-012.
SELECTION-SCREEN COMMENT 30(10) text-013.
SELECT-OPTIONS   s_werks FOR t001w-werks.
*selection-screen comment 55(20) text-013.
SELECTION-SCREEN END OF LINE.

**SELECTION-SCREEN BEGIN OF LINE.
**SELECTION-SCREEN COMMENT 1(5) text-001.         "spaces
*PARAMETERS p_mard AS CHECKBOX.                  "Storage Location
**SELECTION-SCREEN COMMENT 10(40) text-004.
**SELECTION-SCREEN END OF LINE.

**SELECTION-SCREEN BEGIN OF LINE.
**SELECTION-SCREEN COMMENT 1(5) text-001.         "spaces
*PARAMETERS p_mbew AS CHECKBOX.                  "Pricing
**SELECTION-SCREEN COMMENT 10(40) text-005.
**SELECTION-SCREEN END OF LINE.
*
**SELECTION-SCREEN BEGIN OF LINE.
**SELECTION-SCREEN COMMENT 1(5) text-001.         "spaces
*PARAMETERS p_makt AS CHECKBOX.                  "Material description
**SELECTION-SCREEN COMMENT 10(40) text-006.
**SELECTION-SCREEN END OF LINE.
*
**SELECTION-SCREEN BEGIN OF LINE.
**SELECTION-SCREEN COMMENT 1(5) text-001.         "spaces
*PARAMETERS p_marm AS CHECKBOX.                  "Unit of measure
**SELECTION-SCREEN COMMENT 10(40) text-007.
**SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2_sub.

SELECTION-SCREEN END OF BLOCK b2.

* block 3
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-009.
SELECTION-SCREEN SKIP 1.
PARAMETERS p_alv RADIOBUTTON GROUP r1.         "ALV Report
PARAMETERS p_excel RADIOBUTTON GROUP r1.       "Excel Spreadsheet
PARAMETERS p_file LIKE rlgrap-filename DEFAULT
                                   'H:\SAPTEMP\ZLMMR009'. "TR995

SELECTION-SCREEN END OF BLOCK b3.

******************************************************************
*                   INITIALIZATION                               *
******************************************************************
INITIALIZATION.


******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.                           "TR995
  PERFORM check_file_path.                               "TR995
*End of TR995 changes

******************************************************************
*                   START OF SELECTION                           *
******************************************************************
START-OF-SELECTION.


  PERFORM validate_screen_info.

  PERFORM read_change_document_header.

  IF NOT tbl_cdhdr[] IS INITIAL.
    PERFORM read_change_document_item.
  ENDIF.

  IF NOT tbl_cdpos[] IS INITIAL.
    PERFORM read_table_field_texts.
    IF p_pdni = 'X'.
      PERFORM create_report1.
    ELSE.
      PERFORM create_report.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

  CHECK v_error = ' '.
  IF tbl_report[] IS INITIAL.
    SKIP 5.
    WRITE:/15 'NO CHANGE DOCUMENT DATA FOUND'.
  ELSE.
    IF p_alv = 'X'.
      PERFORM build_fieldcat.
      PERFORM build_events USING st_events[].
      PERFORM display_grid.
    ENDIF.
    IF p_excel = 'X'.
      PERFORM build_excel_table.
    ENDIF.
  ENDIF.

******************************************************************
*                   SUBROUTINES                                  *
******************************************************************
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcat .

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MATNR'.
  tbl_fieldcat-seltext_s = 'Matl #'.
  tbl_fieldcat-seltext_m = 'Material Number'.
  tbl_fieldcat-seltext_l = 'Material Number'.
  tbl_fieldcat-reptext_ddic = 'Material Number'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-no_zero = 'X'.
  tbl_fieldcat-fix_column = 'X'.
  tbl_fieldcat-emphasize = 'C100'.
  tbl_fieldcat-hotspot = 'X'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

**--START OF CHNAGES BY AKMADASU FOR CHG0148060
  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MAKTX'.
  tbl_fieldcat-seltext_s = TEXT-100. "'Mat Desc'.
  tbl_fieldcat-seltext_m = TEXT-101. "'Material Desc'.
  tbl_fieldcat-seltext_l = TEXT-102. "'Material Descripion'.
  tbl_fieldcat-reptext_ddic = TEXT-103. "'Material Descripion'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.
**--END OF CHNAGES BY AKMADASU FOR CHG0148060
  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'CHANGENR'.
  tbl_fieldcat-seltext_s = 'Change #'.
  tbl_fieldcat-seltext_m = 'Change Number'.
  tbl_fieldcat-seltext_l = 'Change Number'.
  tbl_fieldcat-reptext_ddic = 'Change Number'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-no_zero = 'X'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'USERNAME'.
  tbl_fieldcat-seltext_s = 'SAP User'.
  tbl_fieldcat-seltext_m = 'SAP User'.
  tbl_fieldcat-seltext_l = 'SAP User'.
  tbl_fieldcat-reptext_ddic = 'SAP User'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'UDATE'.
  tbl_fieldcat-seltext_s = 'Date'.
  tbl_fieldcat-seltext_m = 'Change Date'.
  tbl_fieldcat-seltext_l = 'Change Date'.
  tbl_fieldcat-reptext_ddic = 'Change Date'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'UTIME'.
  tbl_fieldcat-seltext_s = 'Time'.
  tbl_fieldcat-seltext_m = 'Change Time'.
  tbl_fieldcat-seltext_l = 'Change Time'.
  tbl_fieldcat-reptext_ddic = 'Change Time'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'TCODE'.
  tbl_fieldcat-seltext_s = 'TCODE'.
  tbl_fieldcat-seltext_m = 'Transaction Code'.
  tbl_fieldcat-seltext_l = 'Transaction Code'.
  tbl_fieldcat-reptext_ddic = 'Transaction Code'.
  tbl_fieldcat-just = 'L'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'CHANGE_STATUS'.
  tbl_fieldcat-seltext_s = 'Type'.
  tbl_fieldcat-seltext_m = 'Change Type'.
  tbl_fieldcat-seltext_l = 'Change Type'.
  tbl_fieldcat-reptext_ddic = 'Change Type'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'FDESC'.
  tbl_fieldcat-seltext_s = 'Desc'.
  tbl_fieldcat-seltext_m = 'Description'.
  tbl_fieldcat-seltext_l = 'Field Description'.
  tbl_fieldcat-reptext_ddic = 'Field Description'.
  tbl_fieldcat-just = 'L'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'VALUE_OLD'.
  tbl_fieldcat-seltext_s = 'Old Value'.
  tbl_fieldcat-seltext_m = 'Old Value'.
  tbl_fieldcat-seltext_l = 'Old Value'.
  tbl_fieldcat-reptext_ddic = 'Old Value'.
  tbl_fieldcat-just = 'L'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'VALUE_NEW'.
  tbl_fieldcat-seltext_s = 'New Value'.
  tbl_fieldcat-seltext_m = 'New Value'.
  tbl_fieldcat-seltext_l = 'New Value'.
  tbl_fieldcat-reptext_ddic = 'New Value'.
  tbl_fieldcat-just = 'L'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  IF p_pdni = 'X'.

    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'WERKS'.
    tbl_fieldcat-seltext_s = 'Plant'.
    tbl_fieldcat-seltext_m = 'Plant'.
    tbl_fieldcat-seltext_l = 'Plant'.
    tbl_fieldcat-reptext_ddic = 'Plant'.
    tbl_fieldcat-just = 'L'.
    APPEND tbl_fieldcat TO tbl_fieldtab.
    CLEAR tbl_fieldcat.

    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'VERPR'.
    tbl_fieldcat-seltext_s = 'Avg. Unit Price'.
    tbl_fieldcat-seltext_m = 'Avg. Unit Price'.
    tbl_fieldcat-seltext_l = 'Avg. Unit Price'.
    tbl_fieldcat-reptext_ddic = 'Avg. Unit Price'.
    tbl_fieldcat-just = 'L'.
    APPEND tbl_fieldcat TO tbl_fieldtab.
    CLEAR tbl_fieldcat.

    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'NETIM'.
    tbl_fieldcat-seltext_s = 'Net Impact'.
    tbl_fieldcat-seltext_m = 'Net Impact'.
    tbl_fieldcat-seltext_l = 'Net Impact'.
    tbl_fieldcat-reptext_ddic = 'Net Impact'.
    tbl_fieldcat-just = 'L'.
    APPEND tbl_fieldcat TO tbl_fieldtab.
    CLEAR tbl_fieldcat.

  ELSE.

    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'TABKEY'.
    tbl_fieldcat-seltext_s = 'Table Key'.
    tbl_fieldcat-seltext_m = 'Table Key'.
    tbl_fieldcat-seltext_l = 'Table Key'.
    tbl_fieldcat-reptext_ddic = 'Table Key'.
    tbl_fieldcat-just = 'L'.
    APPEND tbl_fieldcat TO tbl_fieldtab.
    CLEAR tbl_fieldcat.
  ENDIF.
*  tbl_fieldcat-tabname   = 'TBL_REPORT'.
*  tbl_fieldcat-fieldname = 'TABNAME'.
*  tbl_fieldcat-seltext_s = 'SAP Table'.
*  tbl_fieldcat-seltext_m = 'SAP Table'.
*  tbl_fieldcat-seltext_l = 'SAP Table'.
*  tbl_fieldcat-reptext_ddic = 'SAP Table'.
*  tbl_fieldcat-just = 'C'.
*  append tbl_fieldcat to tbl_fieldtab.
*  clear tbl_fieldcat.
*
*  tbl_fieldcat-tabname   = 'TBL_REPORT'.
*  tbl_fieldcat-fieldname = 'FNAME'.
*  tbl_fieldcat-seltext_s = 'SAP Field'.
*  tbl_fieldcat-seltext_m = 'SAP Field'.
*  tbl_fieldcat-seltext_l = 'SAP Field'.
*  tbl_fieldcat-reptext_ddic = 'SAP Field'.
*  tbl_fieldcat-just = 'L'.
*  append tbl_fieldcat to tbl_fieldtab.
*  clear tbl_fieldcat.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_events USING rt_events TYPE slis_t_event.

* Get all the events into itab rt_events
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = rt_events.

ENDFORM.                    " BUILD_EVENTS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_grid .

* Populate the layout
  v_repid = sy-repid.
  st_layout-colwidth_optimize = 'X'.
  st_layout-detail_popup = 'X'.
*  st_layout-zebra = 'X'.
  st_layout-no_keyfix = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_repid
      i_callback_user_command = v_callback_subroutine
*     i_callback_top_of_page  = gv_top_of_page
      i_callback_top_of_page  = 'ALV_TOP_OF_PAGE'
      i_grid_title            = ''
      is_layout               = st_layout
      it_fieldcat             = tbl_fieldtab[]
      it_sort                 = st_sort
      i_save                  = 'A'
      i_default               = 'X'
    TABLES
      t_outtab                = tbl_report[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_GRID
*&---------------------------------------------------------------------*
*&      Form  read_change_document_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_change_document_header .

  REFRESH tbl_cdhdr.

  SELECT objectid changenr username udate utime tcode
           INTO TABLE tbl_cdhdr
           FROM cdhdr
            WHERE objectclas = 'MATERIAL'
             AND  objectid IN s_matnr
             AND  changenr IN s_chgnr
             AND  username IN s_user
             AND  udate IN s_date
             AND  utime IN s_utime
             AND  tcode IN s_tcode.

  COMMIT WORK.                         "reset time counter

  SORT tbl_cdhdr BY objectid changenr.

ENDFORM.                    " read_change_document_header
*&---------------------------------------------------------------------*
*&      Form  READ_CHANGE_DOCUMENT_ITEM
*&---------------------------------------------------------------------*
*       You cannot JOIN tables CDHDR & CDPOS
*----------------------------------------------------------------------*
FORM read_change_document_item .

*  IF p_mara = 'X'.
*    r_table-sign = 'I'.
*    r_table-option = 'EQ'.
*    r_table-low = 'MARA'.
*    APPEND r_table.
*  ENDIF.

  IF p_marc = 'X'.
    r_table-sign = 'I'.
    r_table-option = 'EQ'.
    r_table-low = 'MARC'.
    APPEND r_table.
  ENDIF.

  IF p_pdni = 'X'.
    r_table-sign = 'I'.
    r_table-option = 'EQ'.
    r_table-low = 'MARC'.
    APPEND r_table.
  ENDIF.
*  IF p_mard = 'X'.
*    r_table-sign = 'I'.
*    r_table-option = 'EQ'.
*    r_table-low = 'MARD'.
*    APPEND r_table.
*  ENDIF.

*  IF p_mbew = 'X'.
*    r_table-sign = 'I'.
*    r_table-option = 'EQ'.
*    r_table-low = 'MBEW'.
*    APPEND r_table.
*  ENDIF.
*
*  IF p_makt = 'X'.
*    r_table-sign = 'I'.
*    r_table-option = 'EQ'.
*    r_table-low = 'DMAKT'.
*    APPEND r_table.
*  ENDIF.
*
*  IF p_marm = 'X'.
*    r_table-sign = 'I'.
*    r_table-option = 'EQ'.
*    r_table-low = 'DMARM'.
*    APPEND r_table.
*  ENDIF.

  REFRESH tbl_cdpos.

  SELECT objectid changenr tabname tabkey
        fname chngind value_new value_old
        INTO TABLE tbl_cdpos
        FROM cdpos
        FOR ALL ENTRIES IN tbl_cdhdr
        WHERE objectclas = 'MATERIAL'
          AND objectid   = tbl_cdhdr-objectid
          AND changenr   = tbl_cdhdr-changenr
          AND tabname IN r_table.

  COMMIT WORK.                         "reset time counter

  SORT tbl_cdpos BY objectid changenr.

  FREE r_table.

ENDFORM.                    " READ_CHANGE_DOCUMENT_ITEM
*&---------------------------------------------------------------------*
*&      Form  CREATE_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_report.
**--START OF CHANGES BY AKMADASU CHG0148060
  Types:BEGIN OF ty_makt,
        matnr type matnr,
        maktx type maktx,
        END OF ty_makt,
        BEGIN OF ty_mara_temp,
        matnr type matnr,
        END OF ty_mara_temp.
  data: lt_makt type TABLE OF ty_makt,
        ls_makt type ty_makt,
        lt_mara_temp type TABLE OF ty_mara_temp,
        ls_mara_temp type ty_mara_temp.
  clear:lt_mara_temp[],lt_makt[].
  LOOP AT tbl_cdhdr.
    ls_mara_temp-matnr  = tbl_cdhdr-objectid(18).
    APPEND ls_mara_temp to lt_mara_temp.
    clear:ls_mara_temp.
  endloop.
  sort lt_mara_temp by matnr.
  delete ADJACENT DUPLICATES FROM lt_mara_temp COMPARING matnr.
  IF lt_mara_temp is not INITIAL.
    select matnr maktx from makt into TABLE lt_makt
                       FOR ALL ENTRIES IN lt_mara_temp
                       where matnr = lt_mara_temp-matnr
                       and   spras = sy-langu.
    IF sy-subrc is INITIAL.
      sort lt_makt by matnr.
    ENDIF.
  ENDIF.
**--END OF CHANGES BY AKMADASU CHG0148060

  REFRESH tbl_report.
  LOOP AT tbl_cdpos.

    CLEAR tbl_report.

    IF tbl_cdhdr-objectid <> tbl_cdpos-objectid OR
       tbl_cdhdr-changenr <> tbl_cdpos-changenr.
      READ TABLE tbl_cdhdr WITH KEY objectid    = tbl_cdpos-objectid
                                    changenr    = tbl_cdpos-changenr
                                    BINARY SEARCH.
    ENDIF.

    tbl_report-matnr = tbl_cdhdr-objectid(18).
**--START OF CHANGES BY AKMADASU CHG0148060
    READ TABLE LT_MAKT INTO LS_MAKT WITH KEY MATNR = tbl_report-matnr BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      tbl_report-MAKTX = LS_MAKT-MAKTX.
      CLEAR:LS_MARA_TEMP.
    ENDIF.
**--END OF CHANGES BY AKMADASU CHG0148060
    tbl_report-changenr = tbl_cdhdr-changenr.
    tbl_report-username = tbl_cdhdr-username.
    tbl_report-udate = tbl_cdhdr-udate.
    tbl_report-utime = tbl_cdhdr-utime.
    tbl_report-tcode = tbl_cdhdr-tcode.
    CASE tbl_cdpos-chngind.
      WHEN 'U'.
        tbl_report-change_status = 'Change'.
      WHEN 'E'.
        tbl_report-change_status = 'Delete'.
      WHEN 'D'.
        tbl_report-change_status = 'Delete'.
      WHEN 'J'.
        tbl_report-change_status = 'Entry'.
      WHEN 'I'.
        tbl_report-change_status = 'Entry'.
    ENDCASE.

    tbl_report-tabname = tbl_cdpos-tabname.
    tbl_report-tabkey = tbl_cdpos-tabkey.
    tbl_report-fname = tbl_cdpos-fname.

    IF tbl_cdpos-fname = 'KEY'.
      IF tbl_cdpos-tabname = 'DMARM'.
        tbl_cdpos-tabname = 'MARM'.
      ENDIF.
      IF tbl_cdpos-tabname = 'DMAKT'.
        tbl_cdpos-tabname = 'MAKT'.
      ENDIF.
      IF tbl_cdpos-tabname = 'DMLAN'.
        tbl_cdpos-tabname = 'MLAN'.
      ENDIF.

      IF tbl_dd02t-tabname <> tbl_cdpos-tabname.
        READ TABLE tbl_dd02t WITH KEY tabname = tbl_cdpos-tabname
                                      BINARY SEARCH.
      ENDIF.

      tbl_report-fdesc = tbl_dd02t-ddtext.
    ELSE.

      IF tbl_dd03l-tabname <> tbl_cdpos-tabname OR
         tbl_dd03l-fieldname <> tbl_cdpos-fname.
        READ TABLE tbl_dd03l WITH KEY tabname = tbl_cdpos-tabname
                                   fieldname = tbl_cdpos-fname
                                   BINARY SEARCH.
      ENDIF.

      IF tbl_dd04t-rollname <> tbl_dd03l-rollname.
        READ TABLE tbl_dd04t WITH KEY rollname = tbl_dd03l-rollname
                                      BINARY SEARCH.
      ENDIF.

      tbl_report-fdesc = tbl_dd04t-ddtext.
    ENDIF.

    SHIFT tbl_cdpos-value_new LEFT DELETING LEADING ' '.
    tbl_report-value_new = tbl_cdpos-value_new.
    SHIFT tbl_cdpos-value_old LEFT DELETING LEADING ' '.
    tbl_report-value_old = tbl_cdpos-value_old.

    APPEND tbl_report.

  ENDLOOP.

  SORT tbl_report BY matnr udate utime.

ENDFORM.                    " CREATE_REPORT

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command USING p_ucomm LIKE sy-ucomm
                        p_selfield TYPE slis_selfield.

  CASE p_ucomm.
    WHEN '&IC1'.                            "double click
      CASE p_selfield-fieldname.
        WHEN 'MATNR'.
          SET PARAMETER ID 'MAT' FIELD p_selfield-value.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ENDCASE.
  ENDCASE.

ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  READ_TABLE_FIELD_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_table_field_texts .

  CLEAR: v_marm, v_makt, v_mlan.
  REFRESH: tbl_tabname, tbl_rollname.

  LOOP AT tbl_cdpos.
    IF tbl_cdpos-fname = 'KEY'.
      tbl_tabname-tabname = tbl_cdpos-tabname.
      APPEND tbl_tabname.
      CASE tbl_cdpos-tabname.
        WHEN 'DMARM'.
          IF v_marm = ' '.
            tbl_tabname-tabname = 'MARM'.
            APPEND tbl_tabname.
            v_marm = 'X'.
          ENDIF.
        WHEN 'DMAKT'.
          IF v_makt = ' '.
            tbl_tabname-tabname = 'MAKT'.
            APPEND tbl_tabname.
            v_makt = 'X'.
          ENDIF.
        WHEN 'DMLAN'.
          IF v_mlan = ' '.
            tbl_tabname-tabname = 'MLAN'.
            APPEND tbl_tabname.
            v_mlan = 'X'.
          ENDIF.
      ENDCASE.
    ELSE.
      tbl_rollname-rollname = tbl_cdpos-fname.
      APPEND tbl_rollname.
      tbl_tabname-tabname = tbl_cdpos-tabname.
      APPEND tbl_tabname.
    ENDIF.
  ENDLOOP.

  SORT tbl_tabname.
  DELETE ADJACENT DUPLICATES FROM tbl_tabname.
  SORT tbl_rollname.
  DELETE ADJACENT DUPLICATES FROM tbl_rollname.

  r_tabname-sign = 'I'.
  r_tabname-option = 'EQ'.

  LOOP AT tbl_tabname.
    r_tabname-low = tbl_tabname-tabname.
    APPEND r_tabname.
  ENDLOOP.

  SELECT tabname ddtext
         INTO TABLE tbl_dd02t
         FROM dd02t
         WHERE tabname IN r_tabname
           AND ddlanguage = 'E'.

  SORT tbl_dd02t BY tabname.

  SELECT tabname fieldname rollname
         INTO TABLE tbl_dd03l
         FROM dd03l
         WHERE tabname IN r_tabname
           AND rollname > 'A'
           AND rollname < 'ZZZZZZZZZZ'.

  SORT tbl_dd03l BY tabname fieldname.

  REFRESH r_rollname.

  r_rollname-sign = 'I'.
  r_rollname-option = 'EQ'.

  LOOP AT tbl_dd03l.
    r_rollname-low = tbl_dd03l-rollname.
    APPEND r_rollname.
  ENDLOOP.

  SELECT rollname ddtext
         INTO TABLE tbl_dd04t
         FROM dd04t
         WHERE rollname IN r_rollname
           AND ddlanguage = 'E'.

  SORT tbl_dd04t BY rollname.

  FREE: tbl_tabname, tbl_rollname, r_rollname, r_tabname.

ENDFORM.                    " READ_TABLE_FIELD_TEXTS

*&---------------------------------------------------------------------*
*&      Form  alv_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM alv_top_of_page.

* if you are printing the ALV, do not setup the header again
  IF tbl_top_of_page[] IS INITIAL.

    MOVE text-dte TO v_head01+0(7).
    WRITE sy-datum TO v_head01+8(10).
    MOVE text-amp  TO v_head01+19(5).
    WRITE sy-uzeit TO v_head01+25(10).

    MOVE text-clt  TO v_head02+0(7).
    MOVE sy-mandt  TO v_head02+8(4).
    MOVE sy-sysid  TO v_head02+14(5).

*1- HEADING LINE: TYPE H
    CLEAR st_line.
    st_line-typ  = 'H'.
    st_line-info = sy-title.             "sy-title.
    APPEND st_line TO tbl_top_of_page.

*2- SELECTION LINE: TYPE S
    CLEAR st_line.
    st_line-typ   = 'A'.
    st_line-key   = ''.
    st_line-info  = v_head01.
    APPEND st_line TO tbl_top_of_page.

*3- ACTION LINE:  TYPE A
    CLEAR st_line.
    st_line-typ   = 'A'.
    st_line-key   = ''.
    st_line-info = v_head02.
    APPEND st_line TO tbl_top_of_page.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = tbl_top_of_page.

ENDFORM.                               " ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SCREEN_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validate_screen_info .

  CLEAR v_error.

*  IF p_all = 'X'.
**    IF p_mara = 'X' OR p_marc = 'X' OR p_mard = 'X' OR p_mbew = 'X' OR
**       p_makt = 'X' OR p_marm = 'X' OR p_pdni = 'X'.
*    IF  p_marc = 'X' OR p_pdni = 'X'.
*      SKIP 5.
*      WRITE:/10
*       'Do not choose specific change documents',
*       'and ALL MATERIAL DOCUMENTS'.
*      v_error = 'X'.
*      STOP.
*    ENDIF.
*  ENDIF.

*  IF p_choose = 'X'.
**    IF p_mara = ' ' AND p_marc = ' ' AND p_mard = ' ' AND p_mbew = ' ' AND
**       p_makt = ' ' AND p_marm = ' ' AND p_pdni = ' '.
*    IF p_marc = ' ' AND p_pdni = ' '.
*      SKIP 5.
*      WRITE:/10 'Choose a specific change document'.
*      v_error = 'X'.
*      STOP.
*    ENDIF.
*  ENDIF.

  IF NOT s_date-high IS INITIAL.
    v_days = s_date-high - s_date-low.
    IF v_days > 3650.
      SKIP 5.
      WRITE:/10 'Date range > 10 years'.
      v_error = 'X'.
      STOP.
    ENDIF.
  ENDIF.
  IF p_marc = 'X' AND s_werks IS NOT INITIAL.
    SKIP 5.
    WRITE:/10 'For Plant input is allowed for Plant Data Net Impact option'.
    v_error = 'X'.
    STOP.
  ENDIF.
  IF p_alv = 'X'.
    FREE: tbl_excel1, tbl_excel, tbl_excel_header.
  ENDIF.

ENDFORM.                    " VALIDATE_SCREEN_INFO
*&---------------------------------------------------------------------*
*&      Form  BUILD_EXCEL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_excel_table .

*  DATA: lv_filename TYPE rlgrap-filename.                       "TR995
*
*CONCATENATE 'C:\SAPTEMP\MM-' sy-datum sy-uzeit INTO lv_filename."TR995

  PERFORM setup_excel_header.

  LOOP AT tbl_report.
    IF p_pdni IS INITIAL.
      MOVE-CORRESPONDING tbl_report TO tbl_excel.
      CONCATENATE '''' tbl_excel-tabkey INTO tbl_excel-tabkey.  "prevent scientific notation
    ELSE.
      MOVE-CORRESPONDING tbl_report TO tbl_excel1.
    ENDIF.
    WRITE tbl_report-udate TO tbl_excel-udate.
    WRITE tbl_report-utime TO tbl_excel-utime.
    IF p_pdni IS INITIAL.
      APPEND tbl_excel.
    ELSE.
      APPEND tbl_excel1.
    ENDIF.
  ENDLOOP.

  IF p_pdni IS INITIAL.
    CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
      EXPORTING
*       file_name                 = lv_filename    "TR995
        file_name                 = p_file          "TR995
        create_pivot              = 0
      TABLES
        data_tab                  = tbl_excel
        fieldnames                = tbl_excel_header
      EXCEPTIONS
        file_not_exist            = 1
        filename_expected         = 2
        communication_error       = 3
        ole_object_method_error   = 4
        ole_object_property_error = 5
        invalid_pivot_fields      = 6
        download_problem          = 7
        OTHERS                    = 8.
  ELSE.
    CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
      EXPORTING
*       file_name                 = lv_filename   "TR995
        file_name                 = p_file         "TR995
        create_pivot              = 0
      TABLES
        data_tab                  = tbl_excel1
        fieldnames                = tbl_excel_header
      EXCEPTIONS
        file_not_exist            = 1
        filename_expected         = 2
        communication_error       = 3
        ole_object_method_error   = 4
        ole_object_property_error = 5
        invalid_pivot_fields      = 6
        download_problem          = 7
        OTHERS                    = 8.
  ENDIF.
  IF sy-subrc <> 0.
    WRITE: /1 'table download unsuccessful - reason = ', sy-subrc.
  ENDIF.

ENDFORM.                    " BUILD_EXCEL_TABLE
*&---------------------------------------------------------------------*
*&      Form  SETUP_EXCEL_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM setup_excel_header .

  MOVE 'Mtl #'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
**--START OF CHANGES BY AKMADASU CHG0148060
  MOVE 'Mtl Desc#'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
**--END OF CHANGES BY AKMADASU CHG0148060
  MOVE 'Change #'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'SAP User'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'Date'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'Time'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'TCODE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'Type'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'Field Description'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'Old Value'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'New Value'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  IF p_pdni = 'X'.
    MOVE 'Plant'  TO tbl_excel_header-spaltenname.
    APPEND tbl_excel_header.
    MOVE 'Avg. Unit Price'  TO tbl_excel_header-spaltenname.
    APPEND tbl_excel_header.
    MOVE 'Net Impact'  TO tbl_excel_header-spaltenname.
    APPEND tbl_excel_header.
  ELSE.
    MOVE 'Table Key'  TO tbl_excel_header-spaltenname.
    APPEND tbl_excel_header.
  ENDIF.
*  move 'SAP Table'  to tbl_excel_header-spaltenname.
*  append tbl_excel_header.
*  move 'SAP Field'  to tbl_excel_header-spaltenname.
*  append tbl_excel_header.

ENDFORM.                    " SETUP_EXCEL_HEADER
*&---------------------------------------------------------------------*
*&      Form  CREATE_REPORT1
*&---------------------------------------------------------------------*
*       Base on Plant Data Net Impact check Flag
*----------------------------------------------------------------------*
FORM create_report1 .
**--START OF CHANGES BY AKMADASU CHG0148060
  Types:BEGIN OF ty_makt,
        matnr type matnr,
        maktx type maktx,
        END OF ty_makt,
        BEGIN OF ty_mara_temp,
        matnr type matnr,
        END OF ty_mara_temp.
  data: lt_makt type TABLE OF ty_makt,
        ls_makt type ty_makt,
        lt_mara_temp type TABLE OF ty_mara_temp,
        ls_mara_temp type ty_mara_temp.
**--end OF CHANGES BY AKMADASU CHG0148060
  DATA: lv_fdesc LIKE dd02t-ddtext,
        lv_dismm LIKE marc-dismm,
        lv_no_mrp,
        lv_changenr LIKE cdpos-changenr,
        lv_no_append.

  CLEAR: gt_data, tbl_report[].

  SORT tbl_cdpos BY changenr.
  LOOP AT tbl_cdpos.
    IF tbl_cdpos-fname = 'KEY'.
      IF tbl_cdpos-tabname = 'DMARM'.
        tbl_cdpos-tabname = 'MARM'.
      ENDIF.
      IF tbl_cdpos-tabname = 'DMAKT'.
        tbl_cdpos-tabname = 'MAKT'.
      ENDIF.
      IF tbl_cdpos-tabname = 'DMLAN'.
        tbl_cdpos-tabname = 'MLAN'.
      ENDIF.
      IF tbl_dd02t-tabname <> tbl_cdpos-tabname.
        READ TABLE tbl_dd02t WITH KEY tabname = tbl_cdpos-tabname
                                  BINARY SEARCH.
      ENDIF.
      lv_fdesc = tbl_dd02t-ddtext.
    ELSE.
      IF tbl_dd03l-tabname <> tbl_cdpos-tabname OR
         tbl_dd03l-fieldname <> tbl_cdpos-fname.
        READ TABLE tbl_dd03l WITH KEY tabname = tbl_cdpos-tabname
                                      fieldname = tbl_cdpos-fname
                                  BINARY SEARCH.
      ENDIF.
      IF tbl_dd04t-rollname <> tbl_dd03l-rollname.
        READ TABLE tbl_dd04t WITH KEY rollname = tbl_dd03l-rollname
                                  BINARY SEARCH.
      ENDIF.
      lv_fdesc = tbl_dd04t-ddtext.
    ENDIF.
    IF lv_fdesc = 'MRP Type' OR
       lv_fdesc = 'Safety Stock' OR
       lv_fdesc = 'Maximum stock level' OR
       lv_fdesc = 'Reorder Point'.

      SHIFT tbl_cdpos-value_new LEFT DELETING LEADING ' '.
      tbl_report-value_new = tbl_cdpos-value_new.
      SHIFT tbl_cdpos-value_old LEFT DELETING LEADING ' '.
      tbl_report-value_old = tbl_cdpos-value_old.
      gt_data-objectid = tbl_cdpos-objectid.
      gt_data-changenr = tbl_cdpos-changenr.
      gt_data-tabname  = tbl_cdpos-tabname.
      gt_data-tabkey   = tbl_cdpos-tabkey.
      gt_data-fname    = tbl_cdpos-fname.
      gt_data-chngind  = tbl_cdpos-chngind.
      gt_data-value_new = tbl_cdpos-value_new.
      gt_data-value_old = tbl_cdpos-value_old.
      gt_data-fdesc = lv_fdesc.

      APPEND gt_data.

    ENDIF.
  ENDLOOP.
************************
  SORT gt_data BY changenr fdesc objectid.
  IF s_werks IS NOT INITIAL.
    DELETE gt_data WHERE tabkey+21(4) NOT IN s_werks.
  ENDIF.
  gt_cdpos[] = gt_data[].
************************
  CLEAR: lv_changenr,
         lv_no_mrp.
**--START OF CHANGES BY AKMADASU CHG0148060
  clear:lt_mara_temp[],lt_makt[].
  LOOP AT tbl_cdhdr.
    ls_mara_temp-matnr  = tbl_cdhdr-objectid(18).
    APPEND ls_mara_temp to lt_mara_temp.
    clear:ls_mara_temp.
  endloop.
  sort lt_mara_temp by matnr.
  delete ADJACENT DUPLICATES FROM lt_mara_temp COMPARING matnr.
  IF lt_mara_temp is not INITIAL.
    select matnr maktx from makt into TABLE lt_makt
                       FOR ALL ENTRIES IN lt_mara_temp
                       where matnr = lt_mara_temp-matnr
                       and   spras = sy-langu.
    IF sy-subrc is INITIAL.
      sort lt_makt by matnr.
    ENDIF.
  ENDIF.
**--END OF CHANGES BY AKMADASU CHG0148060
  LOOP AT gt_data.
    CLEAR tbl_report.
    tbl_report-werks = gt_data-tabkey+21(4).
*    IF p_werks IS NOT INITIAL.
*      CHECK tbl_report-werks = p_werks.
*    ENDIF.
    IF lv_changenr <> gt_data-changenr.
      lv_changenr = gt_data-changenr.
      READ TABLE gt_cdpos   WITH KEY changenr = lv_changenr
                                     fdesc    = 'MRP Type' TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        lv_no_mrp = 'X'.
      ELSE.
        CLEAR lv_no_mrp.
      ENDIF.
    ENDIF.
    IF tbl_cdhdr-objectid <> gt_data-objectid OR
       tbl_cdhdr-changenr <> gt_data-changenr.
      READ TABLE tbl_cdhdr WITH KEY objectid    = gt_data-objectid
                                    changenr    = gt_data-changenr
                                    BINARY SEARCH.
    ENDIF.
    tbl_report-matnr = tbl_cdhdr-objectid(18).
**--START OF CHANGES BY AKMADASU CHG0148060
    READ TABLE LT_MAKT INTO LS_MAKT WITH KEY MATNR = tbl_report-matnr BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      tbl_report-MAKTX = LS_MAKT-MAKTX.
      CLEAR:LS_MARA_TEMP.
    ENDIF.
**--END OF CHANGES BY AKMADASU CHG0148060
    tbl_report-changenr = tbl_cdhdr-changenr.
    tbl_report-username = tbl_cdhdr-username.
    tbl_report-udate = tbl_cdhdr-udate.
    tbl_report-utime = tbl_cdhdr-utime.
    tbl_report-tcode = tbl_cdhdr-tcode.
    tbl_report-value_old = gt_data-value_old.
    tbl_report-value_new = gt_data-value_new.
    tbl_report-fdesc = gt_data-fdesc.
    CASE gt_data-chngind.
      WHEN 'U'.
        tbl_report-change_status = 'Change'.
      WHEN 'E'.
        tbl_report-change_status = 'Delete'.
      WHEN 'D'.
        tbl_report-change_status = 'Delete'.
      WHEN 'J'.
        tbl_report-change_status = 'Entry'.
      WHEN 'I'.
        tbl_report-change_status = 'Entry'.
    ENDCASE.
*************************************************
    CLEAR: lv_no_append.
    IF gt_data-fdesc = 'Safety Stock' OR
       gt_data-fdesc = 'Maximum stock level' OR
       gt_data-fdesc = 'Reorder Point'.
      IF lv_no_mrp = 'X'.
        CLEAR: lv_dismm.
        SELECT SINGLE dismm INTO lv_dismm FROM marc  WHERE matnr = tbl_report-matnr
                                                       AND werks = tbl_report-werks.
        IF lv_dismm = 'VB' OR lv_dismm = 'VM' OR lv_dismm = 'V1'
                           OR lv_dismm = 'Z1'. "(+)PANUSURI Ticket 77194

          PERFORM process_calc2.
*              APPEND tbl_report.
*              lv_no_append = 'X'.
        ENDIF.
        IF lv_dismm = 'PD' OR lv_dismm = 'ND'.
          PERFORM process_calc3. "Net impact calculation
*              APPEND tbl_report.
*              lv_no_append = 'X'.
        ENDIF.
        APPEND tbl_report.
      ELSE.
        PERFORM process_calc1. "Net impact calculation
        APPEND tbl_report.
      ENDIF.
*      APPEND tbl_report.
    ELSEIF gt_data-fdesc = 'MRP Type'.
      CLEAR tbl_report-verpr.
      SELECT SINGLE verpr INTO tbl_report-verpr FROM mbew
                                               WHERE matnr = tbl_report-matnr
                                                 AND bwkey = tbl_report-werks.
      APPEND tbl_report.
    ENDIF.

  ENDLOOP.

  SORT tbl_report BY matnr changenr fdesc. "udate utime.
ENDFORM.                    " CREATE_REPORT1
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CALC1
*&---------------------------------------------------------------------*
*       Net impact calculation
*----------------------------------------------------------------------*
FORM process_calc1 .

  DATA: ls_cdpos LIKE LINE OF gt_cdpos,
        ls_cdpos1 LIKE LINE OF gt_cdpos,
        lv_tabix TYPE syst-tabix,
        lv_changenr TYPE cdpos-changenr.
  DATA: lv_sstock_old  TYPE marc-eisbe,
        lv_mslevel_new TYPE marc-mabst,
        lv_sstock_new  TYPE marc-eisbe,
        lv_mslevel_old TYPE marc-mabst,
        lv_1st_cond,
        lv_2nd_cond,
        lv_ss_old,
        lv_ms_new,
        lv_ms_old,
        lv_ss_new.
  DATA: lt_report LIKE TABLE OF tbl_report,
        ls_report LIKE LINE OF tbl_report,
        lv_no_netim.


  CLEAR: gv_netim1,
       lv_1st_cond,
       lv_2nd_cond,
       lv_ss_old,
       lv_ms_new,
       lv_ms_old,
       lv_ss_new,
       lv_sstock_old,
       lv_sstock_new,
       lv_mslevel_new,
       lv_mslevel_old.


  SELECT SINGLE verpr INTO tbl_report-verpr FROM mbew
                                                 WHERE matnr = tbl_report-matnr
                                                   AND bwkey = tbl_report-werks.
  lv_changenr = gt_data-changenr.
  lt_report = tbl_report[].
  LOOP AT lt_report INTO ls_report WHERE changenr = lv_changenr.
    IF ls_report-netim <> 0.
      lv_no_netim = 'X'.
    ENDIF.
  ENDLOOP.
  CHECK lv_no_netim IS INITIAL.

  READ TABLE gt_cdpos INTO ls_cdpos WITH KEY changenr = lv_changenr
                                             fdesc    = 'MRP Type'.
  CHECK sy-subrc = 0.
  IF ( ls_cdpos-value_old = 'PD' OR ls_cdpos-value_old = 'ND' ) AND
     ( ls_cdpos-value_new = 'VB' OR ls_cdpos-value_new = 'VM'  OR
       ls_cdpos-value_new = 'V1' OR ls_cdpos-value_new = 'Z1' ). "(+)PANUSURI Ticket 77194
    lv_1st_cond = 'X'.
    READ TABLE gt_cdpos INTO ls_cdpos1 WITH KEY changenr = lv_changenr
                                                fdesc    =  'Safety Stock'.
    IF sy-subrc = 0.
      lv_ss_old = 'X'.
      lv_sstock_old = ls_cdpos1-value_old.
    ENDIF.
    READ TABLE gt_cdpos INTO ls_cdpos1 WITH KEY changenr = lv_changenr
                                                fdesc    =  'Maximum stock level'.
    IF sy-subrc = 0.
      lv_ms_new = 'X'.
      lv_mslevel_new = ls_cdpos1-value_new.
    ENDIF.
  ENDIF.
  IF ( ls_cdpos-value_old = 'VB' OR ls_cdpos-value_old = 'VM' OR
       ls_cdpos-value_old = 'V1' OR ls_cdpos-value_old = 'Z1' ) AND "(+)PANUSURI Ticket 77194
     ( ls_cdpos-value_new = 'PD' OR ls_cdpos-value_new = 'ND' ).
    lv_2nd_cond = 'X'.
    READ TABLE gt_cdpos INTO ls_cdpos1 WITH KEY changenr = lv_changenr
                                                fdesc    =  'Maximum stock level'.
    IF sy-subrc = 0.
      lv_ms_old = 'X'.
      lv_mslevel_old = ls_cdpos1-value_old.
    ENDIF.
    READ TABLE gt_cdpos INTO ls_cdpos1 WITH KEY changenr = lv_changenr
                                                fdesc    =  'Safety Stock'.
    IF sy-subrc = 0.
      lv_ss_new = 'X'.
      lv_sstock_new = ls_cdpos1-value_new.
    ENDIF.
  ENDIF.
  IF lv_1st_cond = 'X'.
    IF lv_ss_old IS INITIAL.
      SELECT SINGLE eisbe INTO lv_sstock_old FROM marc
                                            WHERE matnr = tbl_report-matnr
                                              AND werks = tbl_report-werks.
    ENDIF.
    IF lv_ms_new IS INITIAL.
      SELECT SINGLE mabst INTO lv_mslevel_new FROM marc
                                             WHERE matnr = tbl_report-matnr
                                               AND werks = tbl_report-werks.
    ENDIF.
    tbl_report-netim = ( lv_mslevel_new - lv_sstock_old ) * tbl_report-verpr.
    CLEAR: lv_mslevel_old,
           lv_sstock_new,
           lv_2nd_cond,
           lv_ms_old,
           lv_ss_new.
  ENDIF.
  IF lv_2nd_cond = 'X'.
    IF lv_ms_old IS INITIAL.
      SELECT SINGLE mabst INTO lv_mslevel_old FROM marc
                                             WHERE matnr = tbl_report-matnr
                                               AND werks = tbl_report-werks.
    ENDIF.
    IF lv_ss_new IS INITIAL.
      SELECT SINGLE eisbe INTO lv_sstock_new FROM marc
                                            WHERE matnr = tbl_report-matnr
                                              AND werks = tbl_report-werks.
    ENDIF.
*   tbl_report-netim = ( lv_mslevel_old - lv_sstock_new ) * tbl_report-verpr."QR163
    tbl_report-netim = ( lv_sstock_new - lv_mslevel_old ) * tbl_report-verpr.
  ENDIF.
ENDFORM.                    " PROCESS_CALC1
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CALC2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM process_calc2 .
  DATA: ls_cdpos LIKE LINE OF gt_cdpos,
        lv_changenr TYPE cdpos-changenr.
  DATA: lv_mslevel_new TYPE marc-mabst,
        lv_mslevel_old TYPE marc-mabst,
        lt_report LIKE TABLE OF tbl_report,
        ls_report LIKE LINE OF tbl_report,
        lv_no_netim.

  CLEAR: gv_netim1,
         lv_mslevel_new,
         lv_mslevel_old.

  lv_changenr = gt_data-changenr.
  SELECT SINGLE verpr INTO tbl_report-verpr FROM mbew
                                           WHERE matnr = tbl_report-matnr
                                             AND bwkey = tbl_report-werks.
  READ TABLE gt_cdpos INTO ls_cdpos WITH KEY changenr = lv_changenr
                                             fdesc    = 'Maximum stock level'.
  CHECK sy-subrc = 0.

  lt_report = tbl_report[].
  LOOP AT lt_report INTO ls_report WHERE changenr = lv_changenr.
    IF ls_report-netim <> 0.
      lv_no_netim = 'X'.
    ENDIF.
  ENDLOOP.
  CHECK lv_no_netim IS INITIAL.

  lv_mslevel_old = ls_cdpos-value_old.
  lv_mslevel_new = ls_cdpos-value_new.
  tbl_report-netim = ( lv_mslevel_new - lv_mslevel_old ) * tbl_report-verpr.
ENDFORM.                    " PROCESS_CALC2
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CALC3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM process_calc3 .

  DATA: ls_cdpos LIKE LINE OF gt_cdpos,
        lv_changenr TYPE cdpos-changenr.
  DATA: lv_sstock_old  TYPE marc-eisbe,
        lv_sstock_new  TYPE marc-eisbe,
        lt_report LIKE TABLE OF tbl_report,
        ls_report LIKE LINE OF tbl_report,
        lv_no_netim.


  CLEAR: gv_netim1,
         lv_sstock_old,
         lv_sstock_new.

  lv_changenr = gt_data-changenr.
  SELECT SINGLE verpr INTO tbl_report-verpr FROM mbew
                                            WHERE matnr = tbl_report-matnr
                                              AND bwkey = tbl_report-werks.
  READ TABLE gt_cdpos INTO ls_cdpos WITH KEY changenr = lv_changenr
                                             fdesc    = 'Safety Stock'.
  CHECK sy-subrc = 0.
  lt_report = tbl_report[].
  LOOP AT lt_report INTO ls_report WHERE changenr = lv_changenr.
    IF ls_report-netim <> 0.
      lv_no_netim = 'X'.
    ENDIF.
  ENDLOOP.
  CHECK lv_no_netim IS INITIAL.

  lv_sstock_old = ls_cdpos-value_old.
  lv_sstock_new = ls_cdpos-value_new.
  tbl_report-netim = ( lv_sstock_new - lv_sstock_old ) * tbl_report-verpr.

ENDFORM.                    " PROCESS_CALC3

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995.
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*lv_dir = sep_path.
  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
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
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
