******************************************************************
*                                                                *
*   PROGRAM: ZMMR_INVENTORY_COUNT                                *
*   AUTHOR:  Larry Ritchie                                       *
*   CREATED: 2010/01/04                                          *
*                                                                *
*   DESCRIPTION: This material inventory count report will show  *
*                the last count date.  It is a newer version of  *
*                ZMMMR040.                                       *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
* 2010/01/04 LRITCHIE TR 779 New program for DBossy specs        *
*
* 2012/09/05 M Khan   TR995 Change C: drive to H: drive with dir-*
*                           ectory file selection using F4 & move*
*                           hard-coded file path/name to variant.*
*
******************************************************************

report zmmr_inventory_count line-size 170 no standard page heading line-count 65.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

tables: mara,                           "Material
        marc,                           "Material/plant
        mard,                           "Material/plant/storage loc
        makt.                           "Material description

type-pools  slis.

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* material - MARA
data: begin of tbl_mara occurs 0,
         matnr      like mara-matnr,
         matkl      like mara-matkl,
         maktx      like makt-maktx,
      end of tbl_mara.

* material/plant data - MARC
data: begin of tbl_marc occurs 0,
         matnr      like marc-matnr,
         werks      like marc-werks,
         maabc      like marc-maabc,          "ABC indicator
         abcin      like marc-abcin,          "physical inventory indicator
      end of tbl_marc.

* material/plant/storage location - MARD
data: begin of tbl_mard occurs 0,
         matnr      like mard-matnr,
         werks      like mard-werks,
         lgort      like mard-lgort,
         lgpbe      like mard-lgpbe,          "storage bin
         dlinl      like mard-dlinl,          "last count date
      end of tbl_mard.

* material description
data: begin of tbl_makt occurs 0,
         matnr      like makt-matnr,
         maktx      like makt-maktx,
      end of tbl_makt.

* temporary material number table
data: begin of tbl_matnr occurs 0,
         matnr      like mara-matnr,
      end of tbl_matnr.

* table to collect all the info for the output
data: begin of tbl_report_detail occurs 0,
         werks(5)   type c,
         lgort      like mard-lgort,
         matnr(18)  type c,
         maktx      like makt-maktx,
         matkl      like mara-matkl,
         maabc(3)   type c,
         abcin(18)  type c,
         dlinl(16)  type c,
         lgpbe(12)  type c,
       end of tbl_report_detail.

data: begin of tbl_report_summary occurs 0,
         year(4)    type c,
         month(5)   type c,
         count(17)  type c,
       end of tbl_report_summary.

* temporary table to setup the report summary table
data: begin of tbl_temp_summary occurs 0,
         dlinl(10)  type c,
         count(9)   type n,
      end of tbl_temp_summary.

* EXCEL table header
data:  begin of tbl_excel_header occurs 1,
         spaltenname(20)  type c,
         ddic_table(5)    type c,
         ddic_field(5)    type c,
         key              type c,
       end of tbl_excel_header.

*  internal table for field catalog.
data : tbl_fieldtab type slis_t_fieldcat_alv with header line,
       tbl_fieldcat type slis_fieldcat_alv.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

data: v_no_material_data(1)   type c,
      v_marc_index            like sy-index,
      v_mard_index            like sy-index
      .

* ALV stuff
data: st_layout type slis_layout_alv,
      st_sort  type slis_sortinfo_alv occurs 0,
      st_events type slis_t_event,
      v_repid like sy-repid.

data: st_line type slis_listheader.
data: tbl_top_of_page type slis_t_listheader.
data: v_head01(100) type c,
      v_head02(100) type c.

******************************************************************
*                   SELECTION SCREENS                            *
******************************************************************

selection-screen skip 1.
selection-screen begin of block box1 with frame title text-001.
selection-screen skip 1.
parameter p_werks like marc-werks obligatory.
selection-screen end of block box1.

selection-screen skip 1.
selection-screen begin of block box2 with frame title text-002.
selection-screen skip 1.
select-options s_lgort for mard-lgort.
select-options s_matkl for mara-matkl.
select-options s_matnr for mara-matnr.
select-options s_maabc for marc-maabc.            "ABC indicator
select-options s_abcin for marc-abcin.            "physical inventory indicator
select-options s_dlinl for mard-dlinl.
selection-screen end of block box2.

selection-screen skip 1.
selection-screen begin of block b3 with frame title text-003.
selection-screen skip 1.
parameters p_detail radiobutton group r1.         "Detail report
parameters p_sum radiobutton group r1.            "Summary report
selection-screen end of block b3.

selection-screen skip 1.
selection-screen begin of block b4 with frame title text-004.
selection-screen skip 1.
parameters p_alv radiobutton group r2.        "Print Report
parameters p_excel radiobutton group r2.       "Excel Spreadsheet
parameters p_file like rlgrap-filename default 'H:\SAPTEMP'. "TR995
selection-screen end of block b4.

selection-screen skip 2.
selection-screen comment 1(54) text-013.

******************************************************************
*                   INITIALIZATION                               *
******************************************************************
initialization.

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
******************************************************************
*                   START OF SELECTION                           *
******************************************************************
start-of-selection.

  perform load_material_data.

  if v_no_material_data = ' '.
    perform create_report_table.
  endif.

end-of-selection.

  if v_no_material_data = 'X'.
    skip 5.
    write:/15 'NO MATERIAL DATA SELECTED'.
    exit.
  endif.

  if p_alv = 'X'.
    if p_detail = 'X'.
      perform build_fieldcat_detail.
      perform build_events using st_events[].
      perform display_grid_detail.
    else.
      perform merge_counts.
      perform build_fieldcat_summary.
      perform build_events using st_events[].
      perform display_grid_summary.

    endif.
  endif.
  if p_excel = 'X'.
    if p_detail = 'X'.
      perform build_excel_table_detail.
    else.
      perform merge_counts.
      perform build_excel_table_summary.
    endif.
  endif.


******************************************************************
*                   SUBROUTINES                                  *
******************************************************************

*&---------------------------------------------------------------------*
*&      Form  LOAD_MATERIAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_material_data .

  refresh: tbl_mara, tbl_marc, tbl_mard, tbl_matnr.
  clear:   tbl_mara, tbl_marc, tbl_mard, tbl_matnr.
  clear:   v_no_material_data.

*  get the plant level material data - plant is mandatory

  select matnr werks maabc abcin
         into table tbl_marc
         from marc
         where matnr in s_matnr
           and werks = p_werks
           and maabc in s_maabc
           and abcin in s_abcin.

  sort tbl_marc by matnr werks.

  if tbl_marc[] is initial.
    v_no_material_data = 'X'.    "bail out if there is no data
    exit.
  endif.

* get the material group & description
  loop at tbl_marc.
    if tbl_marc-matnr <> tbl_matnr-matnr.
      tbl_matnr-matnr = tbl_marc-matnr.
      append tbl_matnr.
    endif.
  endloop.

  select mara~matnr mara~matkl makt~maktx
         into table tbl_mara
         from mara inner join makt
              on makt~matnr = mara~matnr
         where mara~matnr in s_matnr
           and mara~matkl in s_matkl
           and makt~spras = 'EN'.

  sort tbl_mara by matnr.

* if the material group is entered on the selection screen, we have to
* remove extra materials from tbl_marc

  if not s_matkl[] is initial.
    loop at tbl_marc.
      read table tbl_mara with key matnr = tbl_marc-matnr
                                   binary search.
      if sy-subrc <> 0.
        delete tbl_marc.
      endif.
    endloop.
  endif.

  if tbl_marc[] is initial.
    v_no_material_data = 'X'.    "bail out if there is no data
    exit.
  endif.

* get the material/plant/storage location data

  select matnr werks lgort lgpbe dlinl
         into table tbl_mard
         from mard
         for all entries in tbl_marc
         where matnr = tbl_marc-matnr
           and werks = tbl_marc-werks
           and lgort in s_lgort
           and dlinl in s_dlinl.
  sort tbl_mard by matnr werks lgort.

  if tbl_mard[] is initial.
    v_no_material_data = 'X'.    "bail out if there is no data
    exit.
  endif.

  free tbl_matnr.

endform.                    " LOAD_MATERIAL_DATA

*&---------------------------------------------------------------------*
*&      Form  CREATE_REPORT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_report_table .

  refresh tbl_report_detail.

  v_marc_index = 1.
  v_mard_index = 1.

  loop at tbl_mara.
    clear tbl_report_detail.

* plant level
    loop at tbl_marc from v_marc_index.
      if tbl_marc-matnr > tbl_mara-matnr.
        v_marc_index = sy-tabix.
        exit.
      endif.
      if tbl_marc-matnr < tbl_mara-matnr.
        continue.
      endif.

* storage location level
      loop at tbl_mard from v_mard_index.

        if tbl_mard-matnr > tbl_marc-matnr or
           ( tbl_mard-matnr = tbl_marc-matnr and
             tbl_mard-werks > tbl_marc-werks ).
          v_mard_index = sy-tabix.
          exit.
        endif.
        if tbl_mard-matnr < tbl_marc-matnr or
           ( tbl_mard-matnr = tbl_marc-matnr and tbl_mard-werks < tbl_marc-werks ).
          continue.
        endif.

        call function 'CONVERSION_EXIT_MATN1_OUTPUT'
          exporting
            input  = tbl_mara-matnr
          importing
            output = tbl_report_detail-matnr.

        tbl_report_detail-matkl = tbl_mara-matkl.
        translate tbl_mara-maktx using '" '.   "remove double quote for EXCEL
        translate tbl_mara-maktx using ', '.   "remove comma for EXCEL
        tbl_report_detail-maktx = tbl_mara-maktx.

        tbl_report_detail-werks = tbl_marc-werks.
        tbl_report_detail-abcin = tbl_marc-abcin.
        tbl_report_detail-maabc = tbl_marc-maabc.

        tbl_report_detail-lgort  = tbl_mard-lgort.
        write tbl_mard-dlinl to tbl_report_detail-dlinl.
        tbl_report_detail-lgpbe = tbl_mard-lgpbe.

        append tbl_report_detail.
        clear tbl_report_detail.

      endloop.            "MARD

    endloop.              "MARC

  endloop.                "MARA

  sort tbl_report_detail by werks lgort matnr.

endform.                    " CREATE_REPORT_TABLE

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_detail
*&---------------------------------------------------------------------*
*       field length L = 40, M = 20, S = 10
*----------------------------------------------------------------------*
form build_fieldcat_detail .

  tbl_fieldcat-tabname   = 'TBL_REPORT_DETAIL'.
  tbl_fieldcat-fieldname = 'WERKS'.
  tbl_fieldcat-seltext_s = 'Plnt'.
  tbl_fieldcat-seltext_m = 'Plnt'.
  tbl_fieldcat-seltext_l = 'Plnt'.
  tbl_fieldcat-reptext_ddic = 'Plnt'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_DETAIL'.
  tbl_fieldcat-fieldname = 'LGORT'.
  tbl_fieldcat-seltext_s = 'SLoc'.
  tbl_fieldcat-seltext_m = 'Storage Location'.
  tbl_fieldcat-seltext_l = 'Storage Location'.
  tbl_fieldcat-reptext_ddic = 'Storage Location'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_DETAIL'.
  tbl_fieldcat-fieldname = 'MATNR'.
  tbl_fieldcat-seltext_s = 'Mtl #'.
  tbl_fieldcat-seltext_m = 'Mtl #'.
  tbl_fieldcat-seltext_l = 'Mtl #'.
  tbl_fieldcat-reptext_ddic = 'Mtl #'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_DETAIL'.
  tbl_fieldcat-fieldname = 'MAKTX'.
  tbl_fieldcat-seltext_s = 'Matl Desc'.
  tbl_fieldcat-seltext_m = 'Material Description'.
  tbl_fieldcat-seltext_l = 'Material Description'.
  tbl_fieldcat-reptext_ddic = 'Material Description'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_DETAIL'.
  tbl_fieldcat-fieldname = 'MATKL'.
  tbl_fieldcat-seltext_s = 'Matl Grp'.
  tbl_fieldcat-seltext_m = 'Material Group'.
  tbl_fieldcat-seltext_l = 'Material Group'.
  tbl_fieldcat-reptext_ddic = 'Mtl Grp'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_DETAIL'.
  tbl_fieldcat-fieldname = 'MAABC'.
  tbl_fieldcat-seltext_s = 'ABC'.
  tbl_fieldcat-seltext_m = 'ABC Indicator'.
  tbl_fieldcat-seltext_l = 'ABC Indicator'.
  tbl_fieldcat-reptext_ddic = 'ABC Indicator'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_DETAIL'.
  tbl_fieldcat-fieldname = 'ABCIN'.
  tbl_fieldcat-seltext_s = 'CC PhysInv'.
  tbl_fieldcat-seltext_m = 'CC Physical Inv'.
  tbl_fieldcat-seltext_l = 'CC Physical Inventory Indicator'.
  tbl_fieldcat-reptext_ddic = 'CC Physical Inventory Indicator'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_DETAIL'.
  tbl_fieldcat-fieldname = 'DLINL'.
  tbl_fieldcat-seltext_s = 'Last Count'.
  tbl_fieldcat-seltext_m = 'Last Count Date'.
  tbl_fieldcat-seltext_l = 'Last Count Date'.
  tbl_fieldcat-reptext_ddic = 'Last Count Date'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_DETAIL'.
  tbl_fieldcat-fieldname = 'LGPBE'.
  tbl_fieldcat-seltext_s = 'Bin Location'.
  tbl_fieldcat-seltext_m = 'Bin Location'.
  tbl_fieldcat-seltext_l = 'Bin Location'.
  tbl_fieldcat-reptext_ddic = 'Bin Location'.
  tbl_fieldcat-just = 'L'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

endform.                    " build_fieldcat_detail
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_st_events[]  text
*----------------------------------------------------------------------*
form build_events using rt_events type slis_t_event.

* Get all the events into itab rt_events
  call function 'REUSE_ALV_EVENTS_GET'
    exporting
      i_list_type = 0
    importing
      et_events   = rt_events.

endform.                    " BUILD_EVENTS
*&---------------------------------------------------------------------*
*&      Form  display_grid_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form display_grid_detail .

* Populate the layout
  v_repid = sy-repid.
  st_layout-colwidth_optimize = 'X'.
  st_layout-detail_popup = 'X'.
  st_layout-no_keyfix = 'X'.

  call function 'REUSE_ALV_GRID_DISPLAY'
   exporting
     i_callback_program     = v_repid
*      i_callback_top_of_page = gv_top_of_page
     i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
     i_grid_title           = ''
     is_layout              = st_layout
     it_fieldcat            = tbl_fieldtab[]
     it_sort                = st_sort
     i_save                 = 'A'
     i_default              = 'X'
   tables
     t_outtab               = tbl_report_detail[]
   exceptions
     program_error          = 1
     others                 = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " display_grid_detail

*&---------------------------------------------------------------------*
*&      Form  alv_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

form alv_top_of_page.

* if you are printing the ALV, do not setup the header again
  if tbl_top_of_page[] is initial.

    move text-dte to v_head01+0(7).
    write sy-datum to v_head01+8(10).
    move text-amp  to v_head01+19(5).
    write sy-uzeit to v_head01+25(10).

    move text-clt  to v_head02+0(7).
    move sy-mandt  to v_head02+8(4).
    move sy-sysid  to v_head02+14(5).

*1- HEADING LINE: TYPE H
    clear st_line.
    st_line-typ  = 'H'.
    st_line-info = sy-title.             "sy-title.
    append st_line to tbl_top_of_page.

*2- SELECTION LINE: TYPE S
    clear st_line.
    st_line-typ   = 'A'.
    st_line-key   = ''.
    st_line-info  = v_head01.
    append st_line to tbl_top_of_page.

*3- ACTION LINE:  TYPE A
    clear st_line.
    st_line-typ   = 'A'.
    st_line-key   = ''.
    st_line-info = v_head02.
    append st_line to tbl_top_of_page.

  endif.

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = tbl_top_of_page.

endform.                               " ALV_TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  build_excel_table_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form build_excel_table_detail .

  perform setup_excel_header_detail.

  call function 'MS_EXCEL_OLE_STANDARD_DAT'
    exporting
*     file_name                 = 'C:\SAPTEMP' "tr995
      file_name                 = p_file       "tr995
      create_pivot              = 0
    tables
      data_tab                  = tbl_report_detail
      fieldnames                = tbl_excel_header
    exceptions
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_object_method_error   = 4
      ole_object_property_error = 5
      invalid_pivot_fields      = 6
      download_problem          = 7
      others                    = 8.

  if sy-subrc <> 0.
    write: /1 'table download unsuccessful - reason = ', sy-subrc.
  endif.


endform.                    " build_excel_table_detail
*&---------------------------------------------------------------------*
*&      Form  setup_excel_header_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_excel_header_detail .

  move 'PLNT'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'SLOC'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MTL #'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MATERIAL DESCRIPTION'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MTL GRP'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'ABC'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'CC PHYS INV IND'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'LAST COUNT'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'BIN LOCATION'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.

endform.                    " setup_excel_header_detail
*&---------------------------------------------------------------------*
*&      Form  MERGE_COUNTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form merge_counts .

  sort tbl_report_detail by dlinl.

  loop at tbl_report_detail.
    if sy-tabix = 1.
      tbl_temp_summary-dlinl = tbl_report_detail-dlinl.
      tbl_temp_summary-count = 1.
      continue.
    endif.
    if tbl_temp_summary-dlinl = tbl_report_detail-dlinl.
      tbl_temp_summary-count = tbl_temp_summary-count + 1.
    else.
      append tbl_temp_summary.
      tbl_temp_summary-dlinl = tbl_report_detail-dlinl.
      tbl_temp_summary-count = 1.
    endif.
  endloop.
  append tbl_temp_summary.

  loop at tbl_temp_summary.
    tbl_report_summary-count = tbl_temp_summary-count.
    tbl_report_summary-year = tbl_temp_summary-dlinl(4).
    tbl_report_summary-month = tbl_temp_summary-dlinl+5(2).
    append tbl_report_summary.
  endloop.

  sort tbl_report_summary by year month.

endform.                    " MERGE_COUNTS
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form build_fieldcat_summary .

  tbl_fieldcat-tabname   = 'TBL_REPORT_SUMMARY'.
  tbl_fieldcat-fieldname = 'YEAR'.
  tbl_fieldcat-seltext_s = 'YEAR'.
  tbl_fieldcat-seltext_m = 'YEAR'.
  tbl_fieldcat-seltext_l = 'YEAR'.
  tbl_fieldcat-reptext_ddic = 'YEAR'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_SUMMARY'.
  tbl_fieldcat-fieldname = 'MONTH'.
  tbl_fieldcat-seltext_s = 'MONTH'.
  tbl_fieldcat-seltext_m = 'MONTH'.
  tbl_fieldcat-seltext_l = 'MONTH'.
  tbl_fieldcat-reptext_ddic = 'MONTH'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT_SUMMARY'.
  tbl_fieldcat-fieldname = 'COUNT'.
  tbl_fieldcat-seltext_s = 'COUNT'.
  tbl_fieldcat-seltext_m = 'COUNT'.
  tbl_fieldcat-seltext_l = 'COUNT'.
  tbl_fieldcat-reptext_ddic = 'COUNT'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-no_zero = 'X'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

endform.                    " BUILD_FIELDCAT_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form display_grid_summary .

* Populate the layout
  v_repid = sy-repid.
*  st_layout-colwidth_optimize = 'X'.
  st_layout-detail_popup = 'X'.
  st_layout-no_keyfix = 'X'.

  call function 'REUSE_ALV_GRID_DISPLAY'
   exporting
     i_callback_program     = v_repid
*      i_callback_top_of_page = gv_top_of_page
     i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
     i_grid_title           = ''
     is_layout              = st_layout
     it_fieldcat            = tbl_fieldtab[]
     it_sort                = st_sort
     i_save                 = 'A'
     i_default              = 'X'
   tables
     t_outtab               = tbl_report_summary[]
   exceptions
     program_error          = 1
     others                 = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " DISPLAY_GRID_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  BUILD_EXCEL_TABLE_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form build_excel_table_summary .

  perform setup_excel_header_summary.

  call function 'MS_EXCEL_OLE_STANDARD_DAT'
    exporting
*     file_name                 = 'C:\SAPTEMP' "tr995
      file_name                 = p_file       "tr995
      create_pivot              = 0
    tables
      data_tab                  = tbl_report_summary
      fieldnames                = tbl_excel_header
    exceptions
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_object_method_error   = 4
      ole_object_property_error = 5
      invalid_pivot_fields      = 6
      download_problem          = 7
      others                    = 8.

  if sy-subrc <> 0.
    write: /1 'table download unsuccessful - reason = ', sy-subrc.
  endif.

endform.                    " BUILD_EXCEL_TABLE_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  SETUP_EXCEL_HEADER_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_excel_header_summary .

  move 'YEAR'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MONTH'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'COUNT'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.

endform.                    " SETUP_EXCEL_HEADER_SUMMARY

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
