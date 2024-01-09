******************************************************************
*                                                                *
*   PROGRAM: ZMMR_MATERIAL_STATUS                                *
*   AUTHOR:  Larry Ritchie                                       *
*   CREATED: 2009/11/24                                          *
*                                                                *
*   DESCRIPTION: This is a material status report listing all    *
*                the material data fields associated with        *
*                deleting a material.  It is a newer version of  *
*                program ZMMMR131.                               *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
* 2012/07/17 M Khan   TR995 Change C: drive to H: drive with     *
*                           directory and file selection using F4*
* 2010/01/25 LRITCHIE TR758 Show only exact matches when plant   *
*                           and/or storage location are entered  *
* 2010/01/11 LRITCHIE TR758 Remove " and , from text and drop    *
*                           decimals on quantity                 *
* 2009/11/24 LRITCHIE TR758 New program for DBossy specs         *
******************************************************************

report zmmr_material_status line-size 256 no standard page heading line-count 90.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

tables: ausp,                           "Material/characteristics
        cabn,                           "Characteristics
        mara,                           "Material
        marc,                           "Material/plant
        mard,                           "Material/plant/storage loc
        makt,                           "Material description
        t023t,                          "Material group description
        t141t.                          "Material status descriptions

type-pools  slis.

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* material date - MARA & MAKT
data: begin of tbl_mara occurs 0,
         matnr      like mara-matnr,
         vpsta      like mara-vpsta,
         lvorm      like mara-lvorm,
         matkl      like mara-matkl,
         mstae      like mara-mstae,
         maktx      like makt-maktx,
      end of tbl_mara.

* material/plant data - MARC
data: begin of tbl_marc occurs 0,
         matnr      like marc-matnr,
         werks      like marc-werks,
         lvorm      like marc-lvorm,
         mmsta      like marc-mmsta,
      end of tbl_marc.

* material/plant/storage location - MARD
data: begin of tbl_mard occurs 0,
         matnr      like mard-matnr,
         werks      like mard-werks,
         lgort      like mard-lgort,
         lvorm      like mard-lvorm,
         labst      like mard-labst,          "unrestricted qty
         umlme      like mard-umlme,          "in transfer qty
         insme      like mard-insme,          "quality inspection qty
         speme      like mard-speme,          "blocked qty
         retme      like mard-retme,          "return qty
      end of tbl_mard.

* material characteristics
data: begin of tbl_ausp occurs 0,
         objek      like ausp-objek,          "18 digit material number
         atwrt      like ausp-atwrt,          "characteristic value
      end of tbl_ausp.

* material status descriptions
data: begin of tbl_t141t occurs 9,
         mmsta      like t141t-mmsta,
         mtstb      like t141t-mtstb,
      end of tbl_t141t.

* material group description
data: begin of tbl_t023t occurs 200,
         matkl      like t023t-matkl,
         wgbez      like t023t-wgbez,
      end of tbl_t023t.

* temporary material number table
data: begin of tbl_matnr occurs 0,
         matnr      like mara-matnr,
      end of tbl_matnr.

* temporary material/plant table
data: begin of tbl_matnr_werks occurs 0,
         matnr     like mara-matnr,
         werks     like marc-werks,
      end of tbl_matnr_werks.

* table to collect all the info for the report
data: begin of tbl_report occurs 0,
         matkl      like mara-matkl,
         wgbez      like t023t-wgbez,
         matnr(18)  type c,
         maktx      like makt-maktx,
         werks(5)   type c,
         lgort      like mard-lgort,
         qoh(15)    type c,          "all quantities
         atwrt      like ausp-atwrt,
         mstae      like mara-mstae,          "material X-plant
         mtstb_mstae like t141t-mtstb,
         mmsta      like marc-mmsta,
         mtstb_mmsta like t141t-mtstb,
         lvorm_mara(8) type c,
         lvorm_marc(8) type c,
         lvorm_mard(8) type c,
         vpsta      like mara-vpsta,
       end of tbl_report.

* EXCEL table header
data:  begin of tbl_excel_header occurs 1,
         spaltenname(20)  type c,
         ddic_table(5)    type c,
         ddic_field(5)    type c,
         key              type c,
       end of tbl_excel_header.


* for printing the selection screen info at the end of the report
data:  begin of tbl_scr_image occurs 0,
          line(120) type c,
         end of tbl_scr_image.

*  internal table for field catalog.
data : tbl_fieldtab type slis_t_fieldcat_alv with header line,
       tbl_fieldcat type slis_fieldcat_alv.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

data: v_screen_matl_group(1)  type c,
      v_screen_matl_no(1)     type c,
      v_screen_plant(1)       type c,
      v_screen_storage_loc(1) type c,
      v_put_in_report(1) type c,
      v_no_material_data(1)   type c,
      v_no_mara_orphans(1)    type c,
      v_no_marc_orphans(1)    type c,
      v_labst(7)              type p,
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

DATA: LV_DIR TYPE STRING,                                   "TR995
      LV_BOL TYPE C,        "abap_bool.                     "TR995
      W_FILE(128) TYPE C,                                   "TR995
      W_PATH(128) TYPE C VALUE 'H:\SAPTEMP',                       "TR995
      WNAME TYPE STRING.                                    "TR995
******************************************************************
*                   SELECTION SCREENS                            *
******************************************************************

selection-screen begin of block b1 with frame title text-001.
select-options s_matkl for mara-matkl.         "material group
select-options s_matnr for mara-matnr.         "material
select-options s_werks for marc-werks.         "plant
select-options s_lgort for mard-lgort.         "storage location
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
parameters p_all radiobutton group r1.        "Report all entries
parameters p_status radiobutton group r1.        "Report selected status
selection-screen skip 1.
select-options s_atwrt for ausp-atwrt.         "NLA code
select-options s_mstae for mara-mstae.         "material X-plant
selection-screen: begin of line.
selection-screen comment 3(29) text-010.       "material deletion flag
parameter p_delm1 radiobutton group r5.
selection-screen comment 36(10) text-007.
parameter p_delm2 radiobutton group r5.
selection-screen comment 50(10) text-008.
parameter p_delm3 radiobutton group r5.
selection-screen comment 64(11) text-009.
selection-screen: end of line.
selection-screen skip 1.
select-options s_mmsta for marc-mmsta.         "plant X-plant
selection-screen: begin of line.
selection-screen comment 3(29) text-011.       "plant deletion flag
parameter p_delp1 radiobutton group r6.
selection-screen comment 36(10) text-007.
parameter p_delp2 radiobutton group r6.
selection-screen comment 50(10) text-008.
parameter p_delp3 radiobutton group r6.
selection-screen comment 64(11) text-009.
selection-screen: end of line.
selection-screen skip 1.
selection-screen: begin of line.
selection-screen comment 3(29) text-012.       "storage location deletion flag
parameter p_dels1 radiobutton group r7.
selection-screen comment 36(10) text-007.
parameter p_dels2 radiobutton group r7.
selection-screen comment 50(10) text-008.
parameter p_dels3 radiobutton group r7.
selection-screen comment 64(11) text-009.
selection-screen: end of line.
selection-screen end of block b2.

selection-screen begin of block b4 with frame title text-004.
parameters p_alv radiobutton group r2.        "Print Report
parameters p_excel radiobutton group r2.       "Excel Spreadsheet
parameter  p_file like rlgrap-filename default 'H:\SAPTEMP'. "Excel file path TR995
selection-screen end of block b4.

******************************************************************
*                   INITIALIZATION                               *
******************************************************************
initialization.
MOVE P_FILE TO WNAME.                                  "TR995

************************************************************************
*                        AT SELECTION-SCREEN. TR995                    *
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  DATA: TAB TYPE FILETABLE,
        WCOUNT TYPE I,
        LINE_TAB LIKE LINE OF TAB,
        GD_PATH  TYPE STRING.

*Slect Directory
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
    EXPORTING
      WINDOW_TITLE    = '*** SELECT DIRECTORY PATH ***'
      INITIAL_FOLDER  = WNAME
    CHANGING
      SELECTED_FOLDER = GD_PATH.

  CALL METHOD CL_GUI_CFW=>FLUSH.
  IF gd_path = space.
     p_file = W_PATH.
  ELSE.
     W_PATH = gd_path.

* Select File.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE      = '*** SELECT FILE ***'
      INITIAL_DIRECTORY = GD_PATH
    CHANGING
      FILE_TABLE        = TAB
      RC                = WCOUNT.

  LOOP AT TAB INTO LINE_TAB.
    MOVE LINE_TAB-FILENAME TO P_FILE.
  ENDLOOP.
  IF P_FILE = SPACE.
     P_FILE = W_PATH.
  ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  PERFORM CHECK_FILE_PATH.
*End Of TR995 changes
******************************************************************
*                   START OF SELECTION                           *
******************************************************************
start-of-selection.

  perform clear_variables.

  perform validate_screen_inputs.

  perform load_material_data.

  if v_no_material_data = ' '.
    perform load_material_group_desc.

    perform load_material_status_desc.

    perform load_nla_code_materials.

    perform create_report_table.
  endif.

end-of-selection.

  if tbl_report[] is initial.
    skip 5.
    if v_no_material_data = 'X'.
      write:/15 'NO MATERIAL DATA SELECTED'.
    else.
      write:/15 'NO DATA TO REPORT'.
    endif.
  else.
    if p_alv = 'X'.
      perform build_fieldcat.
      perform build_events using st_events[].
      perform display_grid.
    endif.
    if p_excel = 'X'.
      perform build_excel_table.
    endif.
  endif.

******************************************************************
*                   SUBROUTINES                                  *
******************************************************************
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SCREEN_INPUTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form validate_screen_inputs .

  if not s_matkl[] is initial.
    v_screen_matl_group = 'X'.
  endif.

  if not s_matnr[] is initial.
    v_screen_matl_no = 'X'.
  endif.

  if not s_werks[] is initial.
    v_screen_plant = 'X'.
    v_no_mara_orphans = 'X'.
  endif.

  if not s_lgort[] is initial.
    v_screen_storage_loc = 'X'.
    v_no_mara_orphans = 'X'.
    v_no_marc_orphans = 'X'.
  endif.

endform.                    " VALIDATE_SCREEN_INPUTS
*&---------------------------------------------------------------------*
*&      Form  LOAD_MATERIAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_material_data .

  refresh: tbl_mara, tbl_marc, tbl_mard, tbl_matnr, tbl_matnr_werks,
           tbl_ausp, tbl_t141t, tbl_t023t.

  clear:   tbl_mara, tbl_marc, tbl_mard, tbl_matnr, tbl_matnr_werks,
           tbl_ausp, tbl_t141t, tbl_t023t.

*******************************
*    USER ENTERED MATERIAL INFO
*******************************

  if v_screen_matl_group = 'X' or v_screen_matl_no = 'X'.

    select mara~matnr mara~vpsta mara~lvorm mara~matkl mara~mstae makt~maktx
           into table tbl_mara
           from mara inner join makt
                on makt~matnr = mara~matnr
           where mara~matnr in s_matnr
             and mara~matkl in s_matkl
             and makt~spras = 'EN'.

    sort tbl_mara by matnr.

    loop at tbl_mara.
      tbl_matnr-matnr = tbl_mara-matnr.
      append tbl_matnr.
    endloop.

    if not tbl_matnr[] is initial.
      select matnr werks lvorm mmsta
             into table tbl_marc
             from marc
             for all entries in tbl_matnr
             where matnr = tbl_matnr-matnr
               and werks in s_werks.
      sort tbl_marc by matnr werks.
    endif.

    loop at tbl_marc.
      tbl_matnr_werks-matnr = tbl_marc-matnr.
      tbl_matnr_werks-werks = tbl_marc-werks.
      append tbl_matnr_werks.
    endloop.

    if not tbl_matnr_werks[] is initial.
      select matnr werks lgort lvorm labst umlme insme speme retme
             into table tbl_mard
             from mard
             for all entries in tbl_matnr_werks
             where matnr = tbl_matnr_werks-matnr
               and werks = tbl_matnr_werks-werks
               and lgort in s_lgort.
      sort tbl_mard by matnr werks lgort.
    endif.
  endif.

************************************************************
*    USER DID NOT ENTER MATERIAL INFO BUT ENTERED PLANT INFO
************************************************************
  if v_screen_matl_group = ' ' and v_screen_matl_no = ' ' and
     v_screen_plant = 'X'.
    select matnr werks lvorm mmsta
          into table tbl_marc
          from marc
          where werks in s_werks.

    sort tbl_marc by matnr werks.

    loop at tbl_marc.
      tbl_matnr-matnr = tbl_marc-matnr.
      append tbl_matnr.
      tbl_matnr_werks-matnr = tbl_marc-matnr.
      tbl_matnr_werks-werks = tbl_marc-werks.
      append tbl_matnr_werks.
    endloop.

    sort tbl_matnr.
    delete adjacent duplicates from tbl_matnr.

    if not tbl_matnr[] is initial.
      select mara~matnr mara~vpsta mara~lvorm mara~matkl mara~mstae makt~maktx
             into table tbl_mara
             from mara inner join makt
                  on makt~matnr = mara~matnr
             for all entries in tbl_matnr
             where mara~matnr = tbl_matnr-matnr
               and makt~spras = 'EN'.
      sort tbl_mara by matnr.
    endif.

    if not tbl_matnr_werks[] is initial.
      select matnr werks lgort lvorm labst umlme insme speme retme
              into table tbl_mard
              from mard
              for all entries in tbl_matnr_werks
              where matnr = tbl_matnr_werks-matnr
                and werks = tbl_matnr_werks-werks
                and lgort in s_lgort.
      sort tbl_mard by matnr werks lgort.

    endif.
  endif.

********************************************************************************
*    USER DID NOT ENTER MATERIAL OR PLANT INFO BUT ENTERED STORAGE LOCATION DATA
********************************************************************************
  if v_screen_matl_group = ' ' and v_screen_matl_no = ' ' and
     v_screen_plant = ' ' and v_screen_storage_loc = 'X'.
    select matnr werks lgort lvorm labst umlme insme speme retme
          into table tbl_mard
          from mard
          where lgort in s_lgort.
    sort tbl_mard by matnr werks lgort.

    loop at tbl_mard.
      tbl_matnr-matnr = tbl_mard-matnr.
      append tbl_matnr.
      tbl_matnr_werks-matnr = tbl_mard-matnr.
      tbl_matnr_werks-werks = tbl_mard-werks.
      append tbl_matnr_werks.
    endloop.

    sort tbl_matnr.
    delete adjacent duplicates from tbl_matnr.

    sort tbl_matnr_werks.
    delete adjacent duplicates from tbl_matnr_werks.

    if not tbl_matnr[] is initial.
      select mara~matnr mara~vpsta mara~lvorm mara~matkl mara~mstae makt~maktx
          into table tbl_mara
          from mara inner join makt
               on makt~matnr = mara~matnr
          for all entries in tbl_matnr
          where mara~matnr = tbl_matnr-matnr
            and makt~spras = 'EN'.
      sort tbl_mara by matnr.
    endif.

    if not tbl_matnr_werks[] is initial.
      select matnr werks lvorm mmsta
            into table tbl_marc
            from marc
            for all entries in tbl_matnr_werks
            where matnr = tbl_matnr_werks-matnr
              and werks = tbl_matnr_werks-werks.
      sort tbl_marc by matnr werks.
    endif.

  endif.

*********************************************************************
*    THE USER DID NOT ENTER GROUP, MATERIAL, PLANT & STORAGE LOCATION
*********************************************************************
  if v_screen_matl_group = ' ' and v_screen_matl_no = ' ' and
     v_screen_plant = ' ' and v_screen_storage_loc = ' '.

    select mara~matnr mara~vpsta mara~lvorm mara~matkl mara~mstae makt~maktx
             into table tbl_mara
             from mara inner join makt
                  on makt~matnr = mara~matnr
             where makt~spras = 'EN'.

    sort tbl_mara by matnr.

    select matnr werks lvorm mmsta
            into table tbl_marc
            from marc.

    sort tbl_marc by matnr werks.

    select matnr werks lgort lvorm labst umlme insme speme retme
           into table tbl_mard
           from mard.

    sort tbl_mard by matnr werks lgort.

  endif.

***********************************************
*    USER INPUT DATA DID NOT FIND ANY MATERIALS
***********************************************
  if tbl_mara[] is initial.
    v_no_material_data = 'X'.
  endif.

  free: tbl_matnr, tbl_matnr_werks.

endform.                    " LOAD_MATERIAL_DATA
*&---------------------------------------------------------------------*
*&      Form  LOAD_MATERIAL_GROUP_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_material_group_desc .

  select matkl wgbez
         into table tbl_t023t
         from t023t
         where spras = 'EN'.

  sort tbl_t023t by matkl.

endform.                    " LOAD_MATERIAL_GROUP_DESC
*&---------------------------------------------------------------------*
*&      Form  LOAD_MATERIAL_STATUS_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_material_status_desc .

  select mmsta mtstb
           into table tbl_t141t
           from t141t
           where spras = 'EN'.

  sort tbl_t141t by mmsta.

endform.                    " LOAD_MATERIAL_STATUS_DESC
*&---------------------------------------------------------------------*
*&      Form  LOAD_NLA_CODE_MATERIALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_nla_code_materials .

* get the internal version of NLA_CODE
  select single atinn
         into cabn-atinn
         from cabn
         where atnam = 'NLA_CODE'.

* get any materials using it
  if sy-subrc = 0.
    select objek atwrt
           into table tbl_ausp
           from ausp
           where atinn = cabn-atinn
             and klart = '001'.

    sort tbl_ausp by objek atwrt.
  endif.

endform.                    " LOAD_NLA_CODE_MATERIALS
*&---------------------------------------------------------------------*
*&      Form  CREATE_REPORT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_report_table .

  refresh tbl_report.

* In order to be included in the report, all values of NLA_CODE, material
* status & deletion indicators must match EXACTLY.

  v_marc_index = 1.
  v_mard_index = 1.

  loop at tbl_mara.
    clear v_put_in_report.
    clear tbl_report.

    if p_all = 'X'.                     "include all entries in the report
      v_put_in_report = 'X'.
    endif.

* material level

    if v_put_in_report = ' '.

      if ( tbl_mara-lvorm = 'X' and p_delm3 = 'X' ) or        "looking for blank deletion flag
         ( tbl_mara-lvorm = ' ' and p_delm2 = 'X' ).          "looking for X deletion flag
        continue.
      endif.


      if not s_mstae[] is initial.
        if not tbl_mara-mstae in s_mstae[].
          continue.
        endif.
      endif.

    endif.

    clear tbl_ausp.
    read table tbl_ausp with key objek = tbl_mara-matnr
                                 binary search.

    if v_put_in_report = ' '.
      if not s_atwrt[] is initial.
        if not tbl_ausp-atwrt in s_atwrt[].    "NLA_CODE
          continue.
        endif.
      endif.
    endif.

    clear tbl_marc.
    read table tbl_marc with key matnr = tbl_mara-matnr
                                 binary search.
    if sy-subrc <> 0.
      if v_no_mara_orphans = ' '.                                         "keep orphaned MARA data
        perform setup_mara_report_info.
        append tbl_report.
        clear tbl_report.
      endif.
      continue.
    endif.

* plant level
    loop at tbl_marc from v_marc_index.
      if tbl_marc-matnr > tbl_mara-matnr.
        v_marc_index = sy-tabix.
        exit.
      endif.
      if tbl_marc-matnr < tbl_mara-matnr.
        continue.
      endif.

      if v_put_in_report = ' '.

        if ( tbl_marc-lvorm = 'X' and p_delp3 = 'X' ) or        "looking for blank deletion flag
           ( tbl_marc-lvorm = ' ' and p_delp2 = 'X' ).          "looking for X deletion flag
          continue.
        endif.

        if not s_mmsta[] is initial.
          if not tbl_marc-mmsta in s_mmsta[].
            continue.
          endif.
        endif.

      endif.

      read table tbl_mard with key matnr = tbl_marc-matnr
                                   werks = tbl_marc-werks
                                   binary search.
      if sy-subrc <> 0.
        if v_no_marc_orphans = ' '.                           "keep the orphaned MARC entry
          perform setup_mara_report_info.
          perform setup_marc_report_info.
          append tbl_report.
          clear tbl_report.
        endif.
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

        if v_put_in_report = ' '.
          if ( tbl_mard-lvorm = 'X' and p_dels3 = 'X' ) or        "looking for blank deletion flag
             ( tbl_mard-lvorm = ' ' and p_dels2 = 'X' ).          "looking for X deletion flag
            continue.
          endif.
        endif.

        perform setup_mara_report_info.
        perform setup_marc_report_info.
        perform setup_mard_report_info.
        append tbl_report.
        clear tbl_report.

      endloop.            "MARD

    endloop.              "MARC

  endloop.                "MARA

  sort tbl_report by matkl matnr werks lgort.

endform.                    " CREATE_REPORT_TABLE
*&---------------------------------------------------------------------*
*&      Form  SETUP_MARA_REPORT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_mara_report_info .

  call function 'CONVERSION_EXIT_MATN1_OUTPUT'
    exporting
      input  = tbl_mara-matnr
    importing
      output = tbl_report-matnr.

  tbl_report-vpsta = tbl_mara-vpsta.
  tbl_report-lvorm_mara = tbl_mara-lvorm.
  tbl_report-matkl = tbl_mara-matkl.
  tbl_report-mstae = tbl_mara-mstae.
  tbl_report-maktx = tbl_mara-maktx.
  translate tbl_report-maktx using '" '.   "remove double quote for EXCEL
  translate tbl_report-maktx using ', '.   "remove comma for EXCEL
  tbl_report-atwrt = tbl_ausp-atwrt.

  clear tbl_t141t.
  read table tbl_t141t with key mmsta = tbl_mara-mstae
                            binary search.
  tbl_report-mtstb_mstae = tbl_t141t-mtstb.

  clear tbl_t023t.
  read table tbl_t023t with key matkl = tbl_mara-matkl
                            binary search.
  tbl_report-wgbez = tbl_t023t-wgbez.
  translate tbl_report-wgbez using '" '.   "remove double quote for EXCEL
  translate tbl_report-wgbez using ', '.   "remove comma for EXCEL

endform.                    " SETUP_MARA_REPORT_INFO
*&---------------------------------------------------------------------*
*&      Form  SETUP_MARC_REPORT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_marc_report_info .

  tbl_report-werks = tbl_marc-werks.
  tbl_report-lvorm_marc = tbl_marc-lvorm.
  tbl_report-mmsta = tbl_marc-mmsta.

  clear tbl_t141t.
  read table tbl_t141t with key mmsta = tbl_marc-mmsta
                            binary search.
  tbl_report-mtstb_mmsta = tbl_t141t-mtstb.

endform.                    " SETUP_MARC_REPORT_INFO
*&---------------------------------------------------------------------*
*&      Form  SETUP_MARD_REPORT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_mard_report_info .

  tbl_report-lgort  = tbl_mard-lgort.
  tbl_report-lvorm_mard = tbl_mard-lvorm.
  v_labst = tbl_mard-labst + tbl_mard-umlme + tbl_mard-insme +
            tbl_mard-speme + tbl_mard-retme.
  write v_labst to tbl_report-qoh.

endform.                    " SETUP_MARD_REPORT_INFO
*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form clear_variables .

  clear: v_screen_matl_group, v_screen_matl_no, v_screen_plant,
         v_screen_storage_loc, v_no_material_data.

endform.                    " CLEAR_VARIABLES
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       field length L = 40, M = 20, S = 10
*----------------------------------------------------------------------*
form build_fieldcat .

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MATKL'.
  tbl_fieldcat-seltext_s = 'Matl Grp'.
  tbl_fieldcat-seltext_m = 'Material Group'.
  tbl_fieldcat-seltext_l = 'Material Group'.
  tbl_fieldcat-reptext_ddic = 'Mtl Grp'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-no_zero = 'X'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WGBEZ'.
  tbl_fieldcat-seltext_s = 'M Grp Desc'.
  tbl_fieldcat-seltext_m = 'Matl Group Desc'.
  tbl_fieldcat-seltext_l = 'Material Group Description'.
  tbl_fieldcat-reptext_ddic = 'Mtl Grp Desc'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MATNR'.
  tbl_fieldcat-seltext_s = 'Matl No'.
  tbl_fieldcat-seltext_m = 'Material Number'.
  tbl_fieldcat-seltext_l = 'Material Number'.
  tbl_fieldcat-reptext_ddic = 'Material Number'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MAKTX'.
  tbl_fieldcat-seltext_s = 'Matl Desc'.
  tbl_fieldcat-seltext_m = 'Material Description'.
  tbl_fieldcat-seltext_l = 'Material Description'.
  tbl_fieldcat-reptext_ddic = 'Material Description'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WERKS'.
  tbl_fieldcat-seltext_s = 'Plnt'.
  tbl_fieldcat-seltext_m = 'Plant'.
  tbl_fieldcat-seltext_l = 'Plant'.
  tbl_fieldcat-reptext_ddic = 'Plant'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LGORT'.
  tbl_fieldcat-seltext_s = 'SLoc'.
  tbl_fieldcat-seltext_m = 'Storage Location'.
  tbl_fieldcat-seltext_l = 'Storage Location'.
  tbl_fieldcat-reptext_ddic = 'Storage Location'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'QOH'.
  tbl_fieldcat-seltext_s = 'QOH'.
  tbl_fieldcat-seltext_m = 'Quantity on Hand'.
  tbl_fieldcat-seltext_l = 'Quantity on Hand'.
  tbl_fieldcat-reptext_ddic = 'Quantity on Hand'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'ATWRT'.
  tbl_fieldcat-seltext_s = 'NLA_Code'.
  tbl_fieldcat-seltext_m = 'NLA_Code'.
  tbl_fieldcat-seltext_l = 'NLA_Code'.
  tbl_fieldcat-reptext_ddic = 'NLA_Code'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MSTAE'.
  tbl_fieldcat-seltext_s = 'X-Plnt St'.
  tbl_fieldcat-seltext_m = 'X-Plant Status'.
  tbl_fieldcat-seltext_l = 'X-Plant Material Status'.
  tbl_fieldcat-reptext_ddic = 'X-Plant Material Status'.
  tbl_fieldcat-just = 'C'.
  tbl_fieldcat-no_zero = 'X'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MTSTB_MSTAE'.
  tbl_fieldcat-seltext_s = 'X-Plnt St'.
  tbl_fieldcat-seltext_m = 'X-Plant Status Desc'.
  tbl_fieldcat-seltext_l = 'X-Plant Material Status Description'.
  tbl_fieldcat-reptext_ddic = 'X-Plant Material Status Description'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MMSTA'.
  tbl_fieldcat-seltext_s = 'PltSp St'.
  tbl_fieldcat-seltext_m = 'Plnt-Specific Status'.
  tbl_fieldcat-seltext_l = 'Plant-Specific Material Status'.
  tbl_fieldcat-reptext_ddic = 'Plant-Specific Material Status'.
  tbl_fieldcat-just = 'C'.
  tbl_fieldcat-no_zero = 'X'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MTSTB_MMSTA'.
  tbl_fieldcat-seltext_s = 'PltSp St'.
  tbl_fieldcat-seltext_m = 'Plnt-Specific Status'.
  tbl_fieldcat-seltext_l = 'Plant-Specific Material Status Description'.
  tbl_fieldcat-reptext_ddic = 'Plant-Specific Material Status Description'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LVORM_MARA'.
  tbl_fieldcat-seltext_s = 'FFD-Matl'.
  tbl_fieldcat-seltext_m = 'Material Deletion'.
  tbl_fieldcat-seltext_l = 'Material Flagged For Deletion'.
  tbl_fieldcat-reptext_ddic = 'Material Flagged For Deletion'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LVORM_MARC'.
  tbl_fieldcat-seltext_s = 'FFD-Plant'.
  tbl_fieldcat-seltext_m = 'Plant Deletion'.
  tbl_fieldcat-seltext_l = 'Plant Flagged For Deletion'.
  tbl_fieldcat-reptext_ddic = 'Plant Flagged For Deletion'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LVORM_MARD'.
  tbl_fieldcat-seltext_s = 'FFD-SLOC'.
  tbl_fieldcat-seltext_m = 'Stor Loc Deletion'.
  tbl_fieldcat-seltext_l = 'Storage Location Flagged For Deletion'.
  tbl_fieldcat-reptext_ddic = 'Storage Location Flagged For Deletion'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'VPSTA'.
  tbl_fieldcat-seltext_s = 'Matl Views'.
  tbl_fieldcat-seltext_m = 'Material Views'.
  tbl_fieldcat-seltext_l = 'Material Views'.
  tbl_fieldcat-reptext_ddic = 'Material Views'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

endform.                    " BUILD_FIELDCAT
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
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form display_grid .

* Populate the layout
  v_repid = sy-repid.
  st_layout-colwidth_optimize = 'X'.
  st_layout-detail_popup = 'X'.
  st_layout-zebra = 'X'.
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
     t_outtab               = tbl_report[]
   exceptions
     program_error          = 1
     others                 = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " DISPLAY_GRID

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
*&      Form  BUILD_EXCEL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form build_excel_table .

  perform setup_excel_header.

  call function 'MS_EXCEL_OLE_STANDARD_DAT'
    exporting
      file_name                 = p_file   "'C:\SAPTEMP' TR995
      create_pivot              = 0
    tables
      data_tab                  = tbl_report
      fieldnames                = tbl_excel_header
    exceptions
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_ojbect_method_error   = 4
      ole_object_property_block = 5
      invalid_filename          = 6
      invalid_pivot_fields      = 7
      download_problem          = 8
      others                    = 9.

  if sy-subrc <> 0.
    write: /1 'table download unsuccessful - reason = ', sy-subrc.
  endif.


endform.                    " BUILD_EXCEL_TABLE
*&---------------------------------------------------------------------*
*&      Form  SETUP_EXCEL_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_excel_header .

  move 'MTL GRP'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MTL GRP DESC'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MATERIAL NUMBER'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MATERIAL DESC'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'PLANT'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'SLOC'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'QOH'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'NLA CODE'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'XP'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'X-PLNT ST'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'PS'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'PLNT-SP ST'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'FFD-MATL'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'FFD-PLNT'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'FFD-SLOC'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MATERIAL VIEWS'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.

endform.                    " SETUP_EXCEL_HEADER

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM CHECK_FILE_PATH.
DATA: sep_file type string,
      sep_path type string.
*Separate Path and file
     CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
       EXPORTING
         FULL_NAME           = p_file
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

lv_dir = sep_path.
*IF lv_dir+(1) = 'C' OR lv_dir+0(1) = 'c'.
IF lv_dir CS 'C:'.
   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
ELSE.
*Check if directory path exist or not.
CALL METHOD cl_gui_frontend_services=>directory_exist
  EXPORTING
    directory            = lv_dir
  RECEIVING
    result               = lv_bol
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    wrong_parameter      = 3
    not_supported_by_gui = 4
    OTHERS               = 5.
IF lv_bol IS INITIAL.
*   CONCATENATE TEXT-099 sep_path into w_error.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
   MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH TEXT-099 SEP_PATH.
ENDIF.
ENDIF.
ENDFORM.
