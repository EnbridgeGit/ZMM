******************************************************************
*                                                                *
*   PROGRAM: ZMMR_MATERIAL_PRICE_VARIANCE                        *
*   AUTHOR:  Larry Ritchie                                       *
*   CREATED: 2009/11/30                                          *
*                                                                *
*   DESCRIPTION: This report will list the quantity on hand &    *
*                prices for materials.  The user can search for  *
*                price variances.  This is a newer version of    *
*                program ZMMMI015.                               *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
* 2009/11/30 LRITCHIE TR773 New program for DBossy               *
*
* 2012/09/04 M Khan   TR995 Change C: drive to H: drive with dir-*
*                          ectory, file selection using F4 & move*
*                           hard-coded file path/name to variant.*
*
******************************************************************

report zmmr_material_price_variance line-size 255 no standard page heading line-count 65.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

tables: ausp,                           "Material/characteristics
        cabn,                           "Characteristics
        mara,                           "Material
        marc,                           "Material/plant
        makt,                           "Material description
        mbew,                           "Material valuation
        t001,                           "Companies
        t001k,                          "Valuation area
        t023t.                          "Material group description

type-pools  slis.

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* material data - MARA & MAKT
data: begin of tbl_mara occurs 0,
         matnr      like mara-matnr,
         lvorm      like mara-lvorm,
         matkl      like mara-matkl,
         meins      like mara-meins,
         maktx      like makt-maktx,
      end of tbl_mara.

* material/plant data - MARC
data: begin of tbl_marc occurs 0,
         matnr      like marc-matnr,
         werks      like marc-werks,
         lvorm      like marc-lvorm,
      end of tbl_marc.

* material characteristics
data: begin of tbl_ausp occurs 0,
         objek      like ausp-objek,          "18 digit material number
         atinn      like ausp-atinn,          "internal characteristic
         atwrt      like ausp-atwrt,          "characteristic value
      end of tbl_ausp.

* material group description
data: begin of tbl_t023t occurs 200,
         matkl      like t023t-matkl,
         wgbez      like t023t-wgbez,
      end of tbl_t023t.

* material valuation
data: begin of tbl_mbew occurs 0,
         matnr      like mbew-matnr,
         bwkey      like mbew-bwkey,
         lvorm      like mbew-lvorm,
         lbkum      like mbew-lbkum,            "quantity
         salk3      like mbew-salk3,            "value of the stock
         vprsv      like mbew-vprsv,            "price control S or V
         verpr      like mbew-verpr,            "moving average price
         stprs      like mbew-stprs,            "standard price
         peinh      like mbew-peinh,
         min        like mbew-verpr,
         max        like mbew-verpr,
         aup        like mbew-verpr,            "average unit price
         price_var  like mbew-verpr,            "price variance
         value_aup  like mbew-salk3,            "aup value
         value_var  like mbew-salk3,            "variance value
      end of tbl_mbew.

* valuations
data: begin of tbl_t001k occurs 200,
        bwkey       like t001k-bwkey,
        bukrs       like t001k-bukrs,
      end of tbl_t001k.

* company code
data: begin of tbl_t001 occurs 50,
        bukrs       like t001-bukrs,
        waers       like t001-waers,
      end of tbl_t001.

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
data: begin of tbl_materials occurs 0,
         matkl      like mara-matkl,
         wgbez      like t023t-wgbez,
         matnr      like mara-matnr,
         maktx      like makt-maktx,
         primary_desc   like ausp-atwrt,
         secondary_desc like ausp-atwrt,
         manufacturer(80) type c,
         model_number(80) type c,
         bukrs      like t001-bukrs,
         werks      like marc-werks,
         lbkum(7)   type p,             "total valuated stock
         meins      like mara-meins,
         verpr      like mbew-verpr,    "moving average price
         aup        like mbew-verpr,     "average unit price
         min        like mbew-verpr,
         max        like mbew-verpr,
         price_var  like mbew-verpr,     "price variance
         waers(5)   type c,
         peinh      like mbew-peinh,
         vprsv      like mbew-vprsv,
         salk3      like mbew-salk3,    "$value of stock
         value_aup  like mbew-salk3,            "aup value
         value_var  like mbew-salk3,            "variance value
         lvorm_mara(8) type c,
         lvorm_marc(8) type c,
         lvorm_mbew(8) type c,
         nla_code   like ausp-atwrt,
       end of tbl_materials.

* table to roll up the plant level data to either company or material level
data:  begin of tbl_roll occurs 0.
        include structure tbl_materials.
data:  end of tbl_roll.

** table to collect all the info for the report (plant level)
data: begin of tbl_report occurs 0,
         matkl      like mara-matkl,
         wgbez      like t023t-wgbez,
         matnr(18)  type c,
         maktx      like makt-maktx,
         primary_desc   like ausp-atwrt,
         secondary_desc like ausp-atwrt,
         manufacturer(80) type c,
         model_number(80) type c,
         bukrs      like t001-bukrs,
         werks(5)   type c,
         lbkum(15)  type c,          "all quantities
         meins      like mara-meins,
         verpr(15)  type c,
         aup(15)    type c,
         min(15)    type c,
         max(15)    type c,
         price_var(15) type c,
         waers(5)   type c,
         peinh(6)   type c,
         vprsv(13)  type c,
         salk3(15)     type c,        " $value of stock
         value_aup(15) type c,        " aup value
         value_var(15) type c,        " variance value
         lvorm_mara(8) type c,
         lvorm_marc(8) type c,
         lvorm_mbew(8) type c,
         nla_code   like ausp-atwrt,
       end of tbl_report.

** EXCEL report file for company level
data: begin of tbl_report_comp occurs 0,
         matkl      like mara-matkl,
         wgbez      like t023t-wgbez,
         matnr(18)  type c,
         maktx      like makt-maktx,
         primary_desc   like ausp-atwrt,
         secondary_desc like ausp-atwrt,
         manufacturer(80) type c,
         model_number(80) type c,
         bukrs      like t001-bukrs,
         lbkum(15)  type c,          "all quantities
         meins      like mara-meins,
         aup(15)    type c,
         min(15)    type c,
         max(15)    type c,
         waers(5)   type c,
         peinh(6)   type c,
         vprsv(13)  type c,
         salk3(15)     type c,        " $value of stock
         value_aup(15) type c,        " aup value
         lvorm_mara(8) type c,
         nla_code   like ausp-atwrt,
       end of tbl_report_comp.

** EXCEL report file for material level
data: begin of tbl_report_matl occurs 0,
         matkl      like mara-matkl,
         wgbez      like t023t-wgbez,
         matnr(18)  type c,
         maktx      like makt-maktx,
         primary_desc   like ausp-atwrt,
         secondary_desc like ausp-atwrt,
         manufacturer(80) type c,
         model_number(80) type c,
         lbkum(15)  type c,          "all quantities
         meins      like mara-meins,
         aup(15)    type c,
         min(15)    type c,
         max(15)    type c,
         waers(5)   type c,
         peinh(6)   type c,
         vprsv(13)  type c,
         salk3(15)     type c,        " $value of stock
         value_aup(15) type c,        " aup value
         lvorm_mara(8) type c,
         nla_code   like ausp-atwrt,
       end of tbl_report_matl.

* temporary table to hold prices to get max & min values
data:  begin of tbl_prices occurs 0,
         matnr    like mara-matnr,
         bukrs    like t001-bukrs,
         verpr    like mbew-verpr,
         lbkum    like mbew-lbkum,            "quantity
         salk3    like mbew-salk3,            "value
         peinh    like mbew-peinh,
         vprsv    like mbew-vprsv,
       end of tbl_prices.

* temporary table to hold min & max prices
data:  begin of tbl_min_max occurs 0,
         matnr    like mara-matnr,
         bukrs    like t001-bukrs,
         min      like mbew-verpr,
         max      like mbew-verpr,
         avg      like mbew-verpr,
       end of tbl_min_max.

* model classifications
data:  begin of tbl_model occurs 0,
         atwrt     like ausp-atwrt,
       end of tbl_model.

* manufacturer classifications
data:  begin of tbl_manu occurs 0,
         atwrt     like ausp-atwrt,
       end of tbl_manu.

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

ranges r_lvorm for mara-lvorm.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

data: v_screen_matl_group(1)  type c,
      v_screen_matl_no(1)     type c,
      v_screen_plant(1)       type c,
      v_screen_company(1)     type c,
      v_no_material_data(1)   type c,
      v_price_count           like sy-tabix,
      v_different_price(1)    type c,
      v_lbkum_total           like mbew-lbkum,
      v_salk3_total           like mbew-salk3,
      v_peinh                 like mbew-peinh,
      v_price_total           like mbew-verpr,
      v_atinn_nla_code        like cabn-atinn,
      v_atinn_manufacturer    like cabn-atinn,
      v_atinn_model_number    like cabn-atinn,
      v_atinn_primary_desc    like cabn-atinn,
      v_atinn_secondary_desc  like cabn-atinn,
      v_price_value_positive  like mbew-salk3,
      v_price_value_negative  like mbew-salk3,
      v_dollar_value_positive  like mbew-salk3,
      v_dollar_value_negative  like mbew-salk3,
      v_string1(30)           type c,
      v_string2(30)           type c,
      v_model_count           like sy-tabix,
      v_ausp_index            like sy-tabix,
      v_min_max_index         like sy-tabix,
      v_marc_index            like sy-tabix,
      v_mbew_index            like sy-tabix
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

selection-screen begin of block b1 with frame title text-001.
select-options s_matkl for mara-matkl.               "material group
select-options s_matnr for mara-matnr.               "material
select-options s_bukrs for t001-bukrs default 'UGL'. "company
select-options s_werks for marc-werks.               "plant
selection-screen end of block b1.

selection-screen begin of block b6 with frame title text-006.
parameters p_both radiobutton group r3.          "AUP based on qty then price count
parameters p_qty radiobutton group r3.          "AUP based on quantity
parameters p_count radiobutton group r3.        "AUP based on price count
selection-screen end of block b6.

selection-screen begin of block b4 with frame title text-004.
parameters p_plnt radiobutton group r1.        "Plant
parameters p_comp radiobutton group r1.        "Company
parameters p_matl radiobutton group r1.        "Material
selection-screen end of block b4.

selection-screen begin of block b2 with frame title text-002.
parameters p_lbkum as checkbox.
parameters p_lvorm as checkbox.
parameters p_delete as checkbox.
selection-screen end of block b2.

selection-screen begin of block b3 with frame title text-003.
parameters p_none radiobutton group r4.

selection-screen begin of line.
parameters p_pricev radiobutton group r4.
selection-screen comment 3(29) text-013.
parameters p_prival like mbew-verpr.                   "price variance value
selection-screen end of line.

selection-screen begin of line.
parameters p_pricep radiobutton group r4.
selection-screen comment 3(29) text-014.
parameters p_priper like mbew-peinh.                   "price variance percent
selection-screen end of line.

selection-screen begin of line.
parameters p_dollav radiobutton group r4.
selection-screen comment 3(29) text-015.
parameters p_dolval like mbew-salk3.                   "variance dollar value
selection-screen end of line.

selection-screen begin of line.
parameters p_dollap radiobutton group r4.
selection-screen comment 3(29) text-016.
parameters p_dolper like mbew-peinh.                   "variance dollar percent
selection-screen end of line.
selection-screen end of block b3.

selection-screen begin of block b5 with frame title text-005.
parameters p_alv radiobutton group r2.        "Print Report
parameters p_excel radiobutton group r2.       "Excel Spreadsheet
parameters p_file like rlgrap-filename default 'H:\SAPTEMP'. "TR995
selection-screen end of block b5.

selection-screen skip 1.
selection-screen comment 1(79) text-017.
selection-screen skip 1.
selection-screen comment 1(80) text-018.
selection-screen skip 1.
selection-screen comment 1(79) text-019.
selection-screen skip 2.
selection-screen comment 1(79) text-020.

select-options s_atinn for cabn-atinn no-display.
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

  perform clear_variables.

  perform load_valuations.

  perform validate_screen_inputs.

  perform load_material_data.
  check v_no_material_data = ' '.

  perform setup_pricing.
  check v_no_material_data = ' '.

  perform load_company_codes.
  perform load_material_group_desc.
  perform load_characteristics.

  perform merge_material_data.

  if p_comp = 'X' or p_matl = 'X'.
    perform roll_up_data.
  else.
    tbl_roll[] = tbl_materials[].
  endif.

  perform setup_report_table.

end-of-selection.

  if tbl_report[] is initial.
    skip 5.
    write:/15 'NO MATERIAL DATA SELECTED'.
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

  if not s_bukrs[] is initial.
    v_screen_company = 'X'.
  endif.

  if not s_werks[] is initial.
    v_screen_plant = 'X'.
  endif.

* if the user enters company and no plants, fill in the valid plants
  if not s_bukrs[] is initial and
         s_werks[] is initial.
    clear s_werks.
    loop at tbl_t001k.
      s_werks-sign = 'I'.
      s_werks-option = 'EQ'.
      s_werks-low = tbl_t001k-bwkey.
      append s_werks.
    endloop.
    sort s_werks.
    v_screen_plant = 'X'.
  endif.

* the user cannot use the variance options unless they are at a plant level
  if p_none = ' ' and p_plnt = ' '.
    skip 4.
    write:/10 'The variance options can only be used with Material/Company/Plant'.
    stop.
  endif.

endform.                    " VALIDATE_SCREEN_INPUTS
*&---------------------------------------------------------------------*
*&      Form  LOAD_MATERIAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_material_data .

  refresh r_lvorm.
  if p_delete = 'X'.
    r_lvorm-sign = 'I'.
    r_lvorm-option = 'EQ'.
    r_lvorm-low = ' '.
    append r_lvorm.
  endif.

  refresh: tbl_mara, tbl_marc, tbl_mbew, tbl_matnr, tbl_matnr_werks.

*******************************
*    USER ENTERED MATERIAL INFO
*******************************
  if v_screen_matl_group = 'X' or v_screen_matl_no = 'X'.

    select mara~matnr mara~lvorm mara~matkl mara~meins makt~maktx
           into table tbl_mara
           from mara inner join makt
                on makt~matnr = mara~matnr
           where mara~matnr in s_matnr
             and mara~lvorm in r_lvorm
             and mara~matkl in s_matkl
             and makt~spras = 'EN'.

    sort tbl_mara by matnr.

    loop at tbl_mara.
      tbl_matnr-matnr = tbl_mara-matnr.
      append tbl_matnr.
    endloop.

    if not tbl_matnr[] is initial.
      select matnr werks lvorm
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
      select matnr bwkey lvorm lbkum salk3 vprsv verpr stprs peinh
             into table tbl_mbew
             from mbew
             for all entries in tbl_matnr_werks
             where matnr = tbl_matnr_werks-matnr
               and bwkey = tbl_matnr_werks-werks.
      sort tbl_mbew by matnr bwkey.
    endif.
  endif.

************************************************************
*    USER DID NOT ENTER MATERIAL INFO BUT ENTERED PLANT INFO
************************************************************
  if v_screen_matl_group = ' ' and v_screen_matl_no = ' ' and
     v_screen_plant = 'X'.
    select matnr werks lvorm
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
      select mara~matnr mara~lvorm mara~matkl mara~meins makt~maktx
             into table tbl_mara
             from mara inner join makt
                  on makt~matnr = mara~matnr
             for all entries in tbl_matnr
             where mara~matnr = tbl_matnr-matnr
               and mara~lvorm in r_lvorm
               and makt~spras = 'EN'.
      sort tbl_mara by matnr.
    endif.

    if not tbl_matnr_werks[] is initial.
      select matnr bwkey lvorm lbkum salk3 vprsv verpr stprs peinh
                  into table tbl_mbew
                  from mbew
                  for all entries in tbl_matnr_werks
                  where matnr = tbl_matnr_werks-matnr
                    and bwkey = tbl_matnr_werks-werks.
      sort tbl_mbew by matnr bwkey.

    endif.
  endif.

***************************************************
*    THE USER DID NOT ENTER GROUP, MATERIAL & PLANT
***************************************************
  if v_screen_matl_group = ' ' and v_screen_matl_no = ' ' and
     v_screen_plant = ' '.

    select mara~matnr mara~lvorm mara~matkl mara~meins makt~maktx
             into table tbl_mara
             from mara inner join makt
                  on makt~matnr = mara~matnr
             where makt~spras = 'EN'
               and mara~lvorm in r_lvorm.

    sort tbl_mara by matnr.

    select matnr werks lvorm
            into table tbl_marc
            from marc.

    sort tbl_marc by matnr werks.

    select matnr bwkey lvorm lbkum salk3 vprsv verpr stprs peinh
            into table tbl_mbew
            from mbew.
    sort tbl_mbew by matnr bwkey.

  endif.

***********************************************
*    USER INPUT DATA DID NOT FIND ANY MATERIALS
***********************************************
  if tbl_mbew[] is initial.
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
*&      Form  load_characteristics
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_characteristics .

  s_atinn-sign = 'I'.
  s_atinn-option = 'EQ'.

* get the internal version of NLA_CODE
  select single atinn
         into v_atinn_nla_code
         from cabn
         where atnam = 'NLA_CODE'.

  if sy-subrc = 0.
    s_atinn-low = v_atinn_nla_code.
    append s_atinn.
  endif.

* get the internal version of MANUFACTURER
  select single atinn
         into v_atinn_manufacturer
         from cabn
         where atnam = 'MANUFACTURER_NAME'.

  if sy-subrc = 0.
    s_atinn-low = v_atinn_manufacturer.
    append s_atinn.
  endif.

* get the internal version of MODEL_NUMBER
  select single atinn
         into v_atinn_model_number
         from cabn
         where atnam = 'MODEL_NUMBER'.

  if sy-subrc = 0.
    s_atinn-low = v_atinn_model_number.
    append s_atinn.
  endif.

* get the internal version of PRIMARY_DESCRIPTION
  select single atinn
         into v_atinn_primary_desc
         from cabn
         where atnam = 'PRIMARY_DESCRIPTION'.

  if sy-subrc = 0.
    s_atinn-low = v_atinn_primary_desc.
    append s_atinn.
  endif.

* get the internal version of SECONDARY_DESCRIPTION
  select single atinn
         into v_atinn_secondary_desc
         from cabn
         where atnam = 'SECONDARY_DESCRIPTION'.

  if sy-subrc = 0.
    s_atinn-low = v_atinn_secondary_desc.
    append s_atinn.
  endif.

* get any materials using the above characteristics
  if not s_atinn[] is initial.

    refresh tbl_ausp.
    select objek atinn atwrt
           into table tbl_ausp
           from ausp
           where klart = '001'
             and atinn in s_atinn.

    sort tbl_ausp by objek atinn.

  endif.

endform.                    " load_characteristics
*&---------------------------------------------------------------------*
*&      Form  merge_material_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form merge_material_data .

  refresh tbl_materials.

  v_marc_index = 1.
  v_mbew_index = 1.

* material level
  loop at tbl_mara.
    clear tbl_materials.

* plant level
    loop at tbl_marc from v_marc_index.
      if tbl_marc-matnr > tbl_mara-matnr.
        v_marc_index = sy-tabix.
        exit.
      endif.
      if tbl_marc-matnr < tbl_mara-matnr.
        continue.
      endif.

* get the prices
      loop at tbl_mbew from v_mbew_index.

        if tbl_mbew-matnr > tbl_marc-matnr or
           ( tbl_mbew-matnr = tbl_marc-matnr and tbl_mbew-bwkey > tbl_marc-werks ).
          v_mbew_index = sy-tabix.
          exit.
        endif.
        if tbl_mbew-matnr < tbl_marc-matnr or
          ( tbl_mbew-matnr = tbl_marc-matnr and tbl_mbew-bwkey < tbl_marc-werks ).
          continue.
        endif.

        if p_lbkum = 'X' and tbl_mbew-lbkum = 0.
          continue.
        endif.
        if p_lvorm = 'X' and tbl_mbew-lvorm = 'X'.
          continue.
        endif.

        perform load_material_table_fields.

      endloop.            "MBEW

    endloop.              "MARC

  endloop.                "MARA

endform.                    " merge_material_data


*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form clear_variables .

  clear: v_screen_matl_group, v_screen_matl_no, v_screen_plant,
         v_no_material_data.

  if p_none = 'X'.
    clear: p_pricev, p_prival, p_pricep, p_priper, p_dollav, p_dolval, p_dollap, p_dolper.
  endif.

  if p_pricev = 'X'.
    clear: p_pricep, p_priper, p_dollav, p_dolval, p_dollap, p_dolper.
  endif.

  if p_pricep = 'X'.
    clear: p_pricev, p_prival, p_dollav, p_dolval, p_dollap, p_dolper.
  endif.

  if p_dollav = 'X'.
    clear: p_pricev, p_prival, p_pricep, p_priper, p_dollap, p_dolper.
  endif.

  if p_dollap = 'X'.
    clear: p_pricev, p_prival, p_pricep, p_priper, p_dollav, p_dolval.
  endif.

  if p_pricev = 'X' and p_prival = 0.
    skip 2.
    write:/10 'Enter price variance value'.
    stop.
  endif.

  if p_pricep = 'X' and p_priper = 0.
    skip 2.
    write:/10 'Enter price variance percent'.
    stop.
  endif.

  if p_dollav = 'X' and p_dolval = 0.
    skip 2.
    write:/10 'Enter dollar variance value'.
    stop.
  endif.

  if p_dollap = 'X' and p_dolper = 0.
    skip 2.
    write:/10 'Enter dollar variance percent'.
    stop.
  endif.


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
  tbl_fieldcat-seltext_s = 'Mtl #'.
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
  tbl_fieldcat-fieldname = 'PRIMARY_DESC'.
  tbl_fieldcat-seltext_s = 'Primary'.
  tbl_fieldcat-seltext_m = 'Primary Description'.
  tbl_fieldcat-seltext_l = 'Primary Description'.
  tbl_fieldcat-reptext_ddic = 'Primary Description'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'SECONDARY_DESC'.
  tbl_fieldcat-seltext_s = 'Secondary'.
  tbl_fieldcat-seltext_m = 'Secondary Description'.
  tbl_fieldcat-seltext_l = 'Secondary Description'.
  tbl_fieldcat-reptext_ddic = 'Secondary Description'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MANUFACTURER'.
  tbl_fieldcat-seltext_s = 'Manufactur'.
  tbl_fieldcat-seltext_m = 'Manufacturer'.
  tbl_fieldcat-seltext_l = 'Manufacturer'.
  tbl_fieldcat-reptext_ddic = 'Manufacturer'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MODEL_NUMBER'.
  tbl_fieldcat-seltext_s = 'Model'.
  tbl_fieldcat-seltext_m = 'Model Number'.
  tbl_fieldcat-seltext_l = 'Model Number'.
  tbl_fieldcat-reptext_ddic = 'Model Number'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  if p_plnt = 'X' or p_comp = 'X'.
    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'BUKRS'.
    tbl_fieldcat-seltext_s = 'Company'.
    tbl_fieldcat-seltext_m = 'Company Code'.
    tbl_fieldcat-seltext_l = 'Company Code'.
    tbl_fieldcat-reptext_ddic = 'Company'.
    tbl_fieldcat-just = 'L'.
    tbl_fieldcat-no_zero = 'X'.
    append tbl_fieldcat to tbl_fieldtab.
    clear tbl_fieldcat.
  endif.

  if p_plnt = 'X'.
    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'WERKS'.
    tbl_fieldcat-seltext_s = 'Plnt'.
    tbl_fieldcat-seltext_m = 'Plnt'.
    tbl_fieldcat-seltext_l = 'Plnt'.
    tbl_fieldcat-reptext_ddic = 'Plnt'.
    append tbl_fieldcat to tbl_fieldtab.
    clear tbl_fieldcat.
  endif.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LBKUM'.
  tbl_fieldcat-seltext_s = 'QOH'.
  tbl_fieldcat-seltext_m = 'Quantity on Hand'.
  tbl_fieldcat-seltext_l = 'Quantity on Hand'.
  tbl_fieldcat-reptext_ddic = 'Quantity on Hand'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MEINS'.
  tbl_fieldcat-seltext_s = 'UOM'.
  tbl_fieldcat-seltext_m = 'Unit of Measure'.
  tbl_fieldcat-seltext_l = 'Unit of Measure'.
  tbl_fieldcat-reptext_ddic = 'Unit of Measure'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  if p_plnt = 'X'.
    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'VERPR'.
    tbl_fieldcat-seltext_s = 'MM Price'.
    tbl_fieldcat-seltext_m = 'MM Price'.
    tbl_fieldcat-seltext_l = 'MM Price'.
    tbl_fieldcat-reptext_ddic = 'MM Price'.
    tbl_fieldcat-just = 'R'.
    append tbl_fieldcat to tbl_fieldtab.
    clear tbl_fieldcat.
  endif.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'AUP'.
  tbl_fieldcat-seltext_s = 'Avg Price'.
  tbl_fieldcat-seltext_m = 'Average Unit Price'.
  tbl_fieldcat-seltext_l = 'Average Unit Price'.
  tbl_fieldcat-reptext_ddic = 'Average Unit Price'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MIN'.
  tbl_fieldcat-seltext_s = 'Min Price'.
  tbl_fieldcat-seltext_m = 'Minimum Price'.
  tbl_fieldcat-seltext_l = 'Minimum Price'.
  tbl_fieldcat-reptext_ddic = 'Minimum Price'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MAX'.
  tbl_fieldcat-seltext_s = 'Max Price'.
  tbl_fieldcat-seltext_m = 'Maximum Price'.
  tbl_fieldcat-seltext_l = 'Maximum Price'.
  tbl_fieldcat-reptext_ddic = 'Maximum Price'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  if p_plnt = 'X'.
    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'PRICE_VAR'.
    tbl_fieldcat-seltext_s = 'Price Var'.
    tbl_fieldcat-seltext_m = 'Price Variance'.
    tbl_fieldcat-seltext_l = 'Price Variance'.
    tbl_fieldcat-reptext_ddic = 'Price Variance'.
    tbl_fieldcat-just = 'R'.
    append tbl_fieldcat to tbl_fieldtab.
    clear tbl_fieldcat.
  endif.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WAERS'.
  tbl_fieldcat-seltext_s = 'Currency'.
  tbl_fieldcat-seltext_m = 'Currency'.
  tbl_fieldcat-seltext_l = 'Currency'.
  tbl_fieldcat-reptext_ddic = 'Currency'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'PEINH'.
  tbl_fieldcat-seltext_s = 'Price Unit'.
  tbl_fieldcat-seltext_m = 'Price Unit'.
  tbl_fieldcat-seltext_l = 'Price Unit'.
  tbl_fieldcat-reptext_ddic = 'Price Unit'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'VPRSV'.
  tbl_fieldcat-seltext_s = 'Price Ctl'.
  tbl_fieldcat-seltext_m = 'Price Control'.
  tbl_fieldcat-seltext_l = 'Price Control Indicator'.
  tbl_fieldcat-reptext_ddic = 'Price Control Indicator'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'SALK3'.
  tbl_fieldcat-seltext_s = 'Stk Value'.
  tbl_fieldcat-seltext_m = 'Stock Value'.
  tbl_fieldcat-seltext_l = 'Stock Value'.
  tbl_fieldcat-reptext_ddic = 'Stock Value'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'VALUE_AUP'.
  tbl_fieldcat-seltext_s = 'Avg Pr Value'.
  tbl_fieldcat-seltext_m = 'Avg Price Value'.
  tbl_fieldcat-seltext_l = 'Average Price Value'.
  tbl_fieldcat-reptext_ddic = 'Average Price Value'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  if p_plnt = 'X'.
    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'VALUE_VAR'.
    tbl_fieldcat-seltext_s = 'Stock Var'.
    tbl_fieldcat-seltext_m = 'Stock Variance'.
    tbl_fieldcat-seltext_l = 'Stock Variance'.
    tbl_fieldcat-reptext_ddic = 'Stock Variance'.
    tbl_fieldcat-just = 'R'.
    append tbl_fieldcat to tbl_fieldtab.
    clear tbl_fieldcat.
  endif.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LVORM_MARA'.
  tbl_fieldcat-seltext_s = 'FFD-Matl'.
  tbl_fieldcat-seltext_m = 'Material Deletion'.
  tbl_fieldcat-seltext_l = 'Material Flagged For Deletion'.
  tbl_fieldcat-reptext_ddic = 'Material Flagged For Deletion'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  if p_plnt = 'X'.
    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'LVORM_MARC'.
    tbl_fieldcat-seltext_s = 'FFD-Plant'.
    tbl_fieldcat-seltext_m = 'Plant Deletion'.
    tbl_fieldcat-seltext_l = 'Plant Flagged For Deletion'.
    tbl_fieldcat-reptext_ddic = 'Plant Flagged For Deletion'.
    tbl_fieldcat-just = 'C'.
    append tbl_fieldcat to tbl_fieldtab.
    clear tbl_fieldcat.
  endif.

  if p_plnt = 'X'.
    tbl_fieldcat-tabname   = 'TBL_REPORT'.
    tbl_fieldcat-fieldname = 'LVORM_MBEW'.
    tbl_fieldcat-seltext_s = 'FFD-Price'.
    tbl_fieldcat-seltext_m = 'Price Deletion'.
    tbl_fieldcat-seltext_l = 'Price Flagged For Deletion'.
    tbl_fieldcat-reptext_ddic = 'Price Flagged For Deletion'.
    tbl_fieldcat-just = 'C'.
    append tbl_fieldcat to tbl_fieldtab.
    clear tbl_fieldcat.
  endif.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'NLA_CODE'.
  tbl_fieldcat-seltext_s = 'NLA_Code'.
  tbl_fieldcat-seltext_m = 'NLA_Code'.
  tbl_fieldcat-seltext_l = 'NLA_Code'.
  tbl_fieldcat-reptext_ddic = 'NLA_Code'.
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
*  st_layout-zebra = 'X'.
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

  if p_plnt = 'X'.
    call function 'MS_EXCEL_OLE_STANDARD_DAT'
      exporting
*       file_name                 = 'C:\SAPTEMP' "tr995
        file_name                 = p_file       "tr995
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
  endif.

  if p_comp = 'X'.
    call function 'MS_EXCEL_OLE_STANDARD_DAT'
      exporting
*       file_name                 = 'C:\SAPTEMP' "tr995
        file_name                 = p_file       "tr995
        create_pivot              = 0
      tables
        data_tab                  = tbl_report_comp
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
  endif.

  if p_matl = 'X'.
    call function 'MS_EXCEL_OLE_STANDARD_DAT'
      exporting
*       file_name                 = 'C:\SAPTEMP' "tr995
        file_name                 = p_file       "tr995
        create_pivot              = 0
      tables
        data_tab                  = tbl_report_matl
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
  endif.

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

  move 'Mtl Grp'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Mtl Grp Desc'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Mtl #'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Material Desc'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Primary Desc'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Secondary Desc'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Manufacturer'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Model'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  if p_comp = 'X' or p_plnt = 'X'.
    move 'Comp'  to tbl_excel_header-spaltenname.
    append tbl_excel_header.
  endif.
  if p_plnt = 'X'.
    move 'Plnt'  to tbl_excel_header-spaltenname.
    append tbl_excel_header.
  endif.
  move 'QOH'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'UOM'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  if p_plnt = 'X'.
    move 'MM Price'  to tbl_excel_header-spaltenname.
    append tbl_excel_header.
  endif.
  move 'Avg Price'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Min Pr'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Max Pr'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  if p_plnt = 'X'.
    move 'Price Var'  to tbl_excel_header-spaltenname.
    append tbl_excel_header.
  endif.
  move 'Curr'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'P Unit'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'P Ctrl'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Stock Value'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Avg Pr Value'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  if p_plnt = 'X'.
    move 'Stock Var'  to tbl_excel_header-spaltenname.
    append tbl_excel_header.
  endif.
  move 'FFD-Matl'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  if p_plnt = 'X'.
    move 'FFD-Plnt'  to tbl_excel_header-spaltenname.
    append tbl_excel_header.
    move 'FFD-Price'  to tbl_excel_header-spaltenname.
    append tbl_excel_header.
  endif.
  move 'NLA Code'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.

endform.                    " SETUP_EXCEL_HEADER

*&---------------------------------------------------------------------*
*&      Form  LOAD_VALUATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_valuations .

  select bwkey bukrs
         into table tbl_t001k
         from t001k
         where bukrs in s_bukrs.

  sort tbl_t001k by bwkey.

endform.                    " LOAD_VALUATIONS
*&---------------------------------------------------------------------*
*&      Form  LOAD_COMPANY_CODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_company_codes .

  select bukrs waers
         into table tbl_t001
         from t001.

  sort tbl_t001 by bukrs.

endform.                    " LOAD_COMPANY_CODES
*&---------------------------------------------------------------------*
*&      Form  load_material_table_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_material_table_fields .

**************** material fields *********************

  tbl_materials-matnr = tbl_mara-matnr.
  tbl_materials-lvorm_mara = tbl_mara-lvorm.
  tbl_materials-matkl = tbl_mara-matkl.
  tbl_materials-meins = tbl_mara-meins.
  tbl_materials-maktx = tbl_mara-maktx.

  clear tbl_t023t.
  read table tbl_t023t with key matkl = tbl_mara-matkl
                            binary search.
  tbl_materials-wgbez = tbl_t023t-wgbez.

* get the NLA_CODE value
  clear tbl_ausp.
  read table tbl_ausp with key objek = tbl_mara-matnr
                               atinn = v_atinn_nla_code
                               binary search.
  if sy-subrc = 0.
    tbl_materials-nla_code = tbl_ausp-atwrt.
  endif.

* get the PRIMARY_DESCRIPTION value
  clear tbl_ausp.
  read table tbl_ausp with key objek = tbl_mara-matnr
                               atinn = v_atinn_primary_desc
                               binary search.
  if sy-subrc = 0.
    tbl_materials-primary_desc = tbl_ausp-atwrt.
  endif.

* get the SECONDARY_DESCRIPTION value
  clear tbl_ausp.
  read table tbl_ausp with key objek = tbl_mara-matnr
                               atinn = v_atinn_secondary_desc
                               binary search.
  if sy-subrc = 0.
    tbl_materials-secondary_desc = tbl_ausp-atwrt.
  endif.

* figure out the model & manufacturers
  perform setup_models_manufacturers.

******************  plant fields **************************
  tbl_materials-werks = tbl_marc-werks.
  tbl_materials-lvorm_marc = tbl_marc-lvorm.

******************  valuation fields
  clear tbl_t001k.
  read table tbl_t001k with key bwkey = tbl_marc-werks
                                binary search.
  if sy-subrc = 0.
    clear tbl_t001.
    read table tbl_t001 with key bukrs = tbl_t001k-bukrs
                                 binary search.
    if sy-subrc = 0.
      tbl_materials-bukrs = tbl_t001-bukrs.
      tbl_materials-waers = tbl_t001-waers.
    endif.
  endif.

  if tbl_mbew-vprsv = 'V'.
    tbl_materials-verpr = tbl_mbew-verpr.
  else.
    tbl_materials-verpr = tbl_mbew-stprs.
  endif.
  tbl_materials-peinh = tbl_mbew-peinh.
  tbl_materials-vprsv = tbl_mbew-vprsv.
  tbl_materials-lvorm_mbew = tbl_mbew-lvorm.
  tbl_materials-salk3 = tbl_mbew-salk3.
  tbl_materials-lbkum = tbl_mbew-lbkum.
  tbl_materials-aup = tbl_mbew-aup.
  tbl_materials-price_var = tbl_mbew-price_var.
  tbl_materials-min = tbl_mbew-min.
  tbl_materials-max = tbl_mbew-max.
  tbl_materials-value_aup = tbl_mbew-value_aup.
  tbl_materials-value_var = tbl_mbew-value_var.

  append tbl_materials.
  clear tbl_materials.

endform.                    " load_material_table_fields
*&---------------------------------------------------------------------*
*&      Form  SETUP_PRICING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_pricing .

  clear: tbl_prices, tbl_t001k.
  loop at tbl_mbew.

* remove prices when the MBEW is marked for deletion and there is no quantity
 if p_delete = 'X'.                                 "TR773 12-04-2010
    if tbl_mbew-lvorm = 'X' and tbl_mbew-lbkum = 0.
      delete tbl_mbew.
      continue.
    endif.
 endif.                                             "TR773 12-04-2010

* remove prices when the user wants to exclude any of: quantity on hand = 0
* or price flagged for deletion
    if p_lbkum = 'X' and tbl_mbew-lbkum = 0.
      delete tbl_mbew.
      continue.
    endif.
    if p_lvorm = 'X' and tbl_mbew-lvorm = 'X'.
      delete tbl_mbew.
      continue.
    endif.

* get the company associated with this plant
    if tbl_t001k-bwkey <> tbl_mbew-bwkey and
       ( p_comp = 'X' or p_plnt = 'X' ).
      read table tbl_t001k with key bwkey = tbl_mbew-bwkey
                                    binary search.
    endif.

    clear: tbl_prices, tbl_min_max.

    if p_comp = 'X' or p_plnt = 'X'.
      tbl_prices-bukrs = tbl_t001k-bukrs.
    endif.

    if tbl_mbew-vprsv = 'V'.
      tbl_prices-verpr = tbl_mbew-verpr.       "moving average price
    else.
      tbl_prices-verpr = tbl_mbew-stprs.       "standard price
    endif.
    tbl_prices-matnr = tbl_mbew-matnr.
    tbl_prices-lbkum = tbl_mbew-lbkum.
    tbl_prices-salk3 = tbl_mbew-salk3.
    tbl_prices-peinh = tbl_mbew-peinh.
    tbl_prices-vprsv = tbl_mbew-vprsv.
    append tbl_prices.

  endloop.

  sort tbl_prices.

* setup the minimum & maximum price per material/company
  clear: tbl_min_max, v_price_count, v_price_total,
         v_lbkum_total, v_salk3_total.
  loop at tbl_prices.

    if sy-tabix = 1.
      tbl_min_max-matnr = tbl_prices-matnr.
      tbl_min_max-bukrs = tbl_prices-bukrs.
      tbl_min_max-min   = tbl_prices-verpr.
      tbl_min_max-max   = tbl_prices-verpr.
      v_price_total = tbl_prices-verpr.
      v_price_count = 1.
      v_lbkum_total = tbl_prices-lbkum.
      v_salk3_total = tbl_prices-salk3.
      v_peinh       = tbl_prices-peinh.
      continue.
    endif.

    if tbl_min_max-matnr <> tbl_prices-matnr or
       tbl_min_max-bukrs <> tbl_prices-bukrs.

      perform calculate_average_price.

      v_price_count = 1.
      v_price_total = tbl_prices-verpr.
      v_lbkum_total = tbl_prices-lbkum.
      v_salk3_total = tbl_prices-salk3.
      v_peinh       = tbl_prices-peinh.
      tbl_min_max-matnr = tbl_prices-matnr.
      tbl_min_max-bukrs = tbl_prices-bukrs.
      tbl_min_max-min   = tbl_prices-verpr.
      tbl_min_max-max   = tbl_prices-verpr.
      continue.
    endif.

    if tbl_prices-verpr < tbl_min_max-min.
      tbl_min_max-min   = tbl_prices-verpr.
    endif.
    if tbl_prices-verpr > tbl_min_max-max.
      tbl_min_max-max   = tbl_prices-verpr.
    endif.

    v_price_total = v_price_total + tbl_prices-verpr.
    v_price_count = v_price_count + 1.
    v_lbkum_total = v_lbkum_total + tbl_prices-lbkum.
    v_salk3_total = v_salk3_total + tbl_prices-salk3.

  endloop.

  perform calculate_average_price.

  free tbl_prices.

  clear: v_price_value_positive, v_price_value_negative,
         v_dollar_value_positive, v_dollar_value_negative.

  if p_pricev = 'X'.
    if p_prival <> 0.                       "price value
      if p_prival > 0.
        v_price_value_positive = p_prival.
        v_price_value_negative = p_prival * -1.
      else.
        v_price_value_negative = p_prival.
        v_price_value_positive = p_prival * -1.
      endif.
    endif.
  endif.

  if p_dollav = 'X'.
    if p_dolval <> 0.                       "dollar value
      if p_dolval > 0.
        v_dollar_value_positive = p_dolval.
        v_dollar_value_negative = p_dolval * -1.
      else.
        v_dollar_value_negative = p_dolval.
        v_dollar_value_positive = p_dolval * -1.
      endif.
    endif.
  endif.

* drop any valuations outside the selection screen range and
* add the additional price info to the MBEW table
  v_min_max_index = 1.
  loop at tbl_mbew.

    if tbl_t001k-bwkey <> tbl_mbew-bwkey.
      read table tbl_t001k with key bwkey = tbl_mbew-bwkey
                                binary search.
    endif.

    if p_comp = 'X' or p_plnt = 'X'.
      read table tbl_min_max with key matnr = tbl_mbew-matnr
                                      bukrs = tbl_t001k-bukrs
                                      binary search.
    else.
      read table tbl_min_max with key matnr = tbl_mbew-matnr
                                      binary search.
    endif.
    if sy-subrc <> 0.
      write:/ 'Error reading MIN MAX values for ',
      tbl_mbew-matnr, tbl_t001-bukrs.
      stop.
    endif.

    tbl_mbew-min = tbl_min_max-min.
    tbl_mbew-max = tbl_min_max-max.
    tbl_mbew-aup = tbl_min_max-avg.
    if tbl_mbew-vprsv = 'V'.
      tbl_mbew-price_var = tbl_mbew-verpr - tbl_min_max-avg.
    else.
      tbl_mbew-price_var = tbl_mbew-stprs - tbl_min_max-avg.
    endif.
    if tbl_mbew-peinh = 0.
      tbl_mbew-value_aup = tbl_mbew-lbkum * tbl_min_max-avg.
    else.
      tbl_mbew-value_aup = tbl_mbew-lbkum * tbl_min_max-avg /
                           tbl_mbew-peinh.
    endif.
    tbl_mbew-value_var = tbl_mbew-salk3 - tbl_mbew-value_aup.

* if the price variance amount is not within the values from the selection screen,
* delete the valuation (MBEW).

    if p_pricev = 'X'.
      if ( tbl_mbew-price_var >= v_price_value_positive or
           tbl_mbew-price_var <= v_price_value_negative ) and
           tbl_mbew-price_var <> 0.
      else.
        delete tbl_mbew.
        continue.
      endif.
    endif.

* if the price variance percent is not within the values from the selection screen,
* delete the valuation (MBEW).

    if p_pricep = 'X'.
      if p_priper <> 0.
        if p_priper < 0.
          p_priper = p_priper * -1.
        endif.
        v_price_value_positive = tbl_mbew-verpr * p_priper / 100.
        v_price_value_negative = v_price_value_positive * -1.
        if ( tbl_mbew-price_var >= v_price_value_positive or
             tbl_mbew-price_var <= v_price_value_negative ) and
             tbl_mbew-price_var <> 0.
        else.
          delete tbl_mbew.
          continue.
        endif.
      endif.
    endif.

* if the dollar variance amount is not within the values from the selection screen,
* delete the valuation (MBEW).

    if p_dollav = 'X'.
      if ( tbl_mbew-value_var >= v_dollar_value_positive or
           tbl_mbew-value_var <= v_dollar_value_negative ) and
           tbl_mbew-value_var <> 0 .
      else.
        delete tbl_mbew.
        continue.
      endif.
    endif.

* if the dollar variance percent is not within the values from the selection screen,
* delete the valuation (MBEW).

    if p_dollap = 'X'.
      if p_dolper <> 0.
        if p_dolper < 0.
          p_dolper = p_dolper * -1.
        endif.
        v_dollar_value_positive = tbl_mbew-salk3 * p_dolper / 100.
        v_dollar_value_negative = v_dollar_value_positive * -1.
        if ( tbl_mbew-value_var >= v_dollar_value_positive or
             tbl_mbew-value_var <= v_dollar_value_negative ) and
             tbl_mbew-value_var <> 0.
        else.
          delete tbl_mbew.
          continue.
        endif.
      endif.
    endif.

    modify tbl_mbew.

  endloop.

  if tbl_mbew[] is initial.
    v_no_material_data = 'X'.
  endif.


endform.                    " SETUP_PRICING
*&---------------------------------------------------------------------*
*&      Form  SETUP_REPORT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_report_table .

  loop at tbl_roll.

    tbl_report-matkl = tbl_roll-matkl.
    tbl_report-wgbez = tbl_roll-wgbez.

    call function 'CONVERSION_EXIT_MATN1_OUTPUT'
      exporting
        input  = tbl_roll-matnr
      importing
        output = tbl_report-matnr.

    translate tbl_roll-maktx using '" '.   "remove double quote for EXCEL
    translate tbl_roll-maktx using ', '.   "remove comma for EXCEL

    tbl_report-maktx = tbl_roll-maktx.
    tbl_report-meins = tbl_roll-meins.
    tbl_report-bukrs = tbl_roll-bukrs.
    tbl_report-werks = tbl_roll-werks.
    tbl_report-lvorm_mara = tbl_roll-lvorm_mara.
    tbl_report-lvorm_marc = tbl_roll-lvorm_marc.
    tbl_report-lvorm_mbew = tbl_roll-lvorm_mbew.

    if tbl_roll-waers is initial.                      "mixed currencies
      tbl_report-waers = '????'.
      tbl_report-verpr = '?????'.
      tbl_report-aup = '?????'.
      tbl_report-price_var = '?????'.
      tbl_report-min = '?????'.
      tbl_report-max = '?????'.
      tbl_report-salk3 = '?????'.
      tbl_report-value_aup = '?????'.
      tbl_report-value_var = '?????'.
    else.
      tbl_report-waers = tbl_roll-waers.
      write tbl_roll-verpr to tbl_report-verpr.
      write tbl_roll-aup to tbl_report-aup.
      write tbl_roll-price_var to tbl_report-price_var.
      write tbl_roll-min to tbl_report-min.
      write tbl_roll-max to tbl_report-max.
      write tbl_roll-salk3 to tbl_report-salk3.
      write tbl_roll-value_aup to tbl_report-value_aup.
      write tbl_roll-value_var to tbl_report-value_var.
    endif.

    if tbl_roll-peinh = 9999.                     "mixed price units
      tbl_report-peinh = '?????'.
      tbl_report-lbkum = '?????'.
    else.
      tbl_report-peinh = tbl_roll-peinh.
      write tbl_roll-lbkum to tbl_report-lbkum.
    endif.

    tbl_report-vprsv = tbl_roll-vprsv.
    tbl_report-nla_code = tbl_roll-nla_code.

    translate tbl_roll-primary_desc using '" '.   "remove double quote for EXCEL
    translate tbl_roll-primary_desc using ', '.   "remove comma for EXCEL

    tbl_report-primary_desc = tbl_roll-primary_desc.

    translate tbl_roll-secondary_desc using '" '.   "remove double quote for EXCEL
    translate tbl_roll-secondary_desc using ', '.   "remove comma for EXCEL

    tbl_report-secondary_desc = tbl_roll-secondary_desc.
    tbl_report-manufacturer = tbl_roll-manufacturer.
    tbl_report-model_number = tbl_roll-model_number.

    append tbl_report.

  endloop.

  sort tbl_report by matkl matnr bukrs werks.

* if the data has been rolled up to company or material level, use a different
* table with fewer columns
  if p_comp = 'X'.
    loop at tbl_report.
      move-corresponding tbl_report to tbl_report_comp.
      append tbl_report_comp.
    endloop.
  endif.

  if p_matl = 'X'.
    loop at tbl_report.
      move-corresponding tbl_report to tbl_report_matl.
      append tbl_report_matl.
    endloop.
  endif.

endform.                    " SETUP_REPORT_TABLE
*&---------------------------------------------------------------------*
*&      Form  SETUP_MODELS_MANUFACTURERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_models_manufacturers .

  refresh: tbl_model, tbl_manu.

* get all the manufacturers
  clear tbl_ausp.
  read table tbl_ausp with key objek = tbl_mara-matnr
                               atinn = v_atinn_manufacturer
                               binary search.
  if sy-subrc = 0.
    v_ausp_index = sy-tabix + 1.
    tbl_manu-atwrt = tbl_ausp-atwrt.
    append tbl_manu.
  endif.

  do.
    read table tbl_ausp index v_ausp_index.
    if tbl_ausp-objek = tbl_mara-matnr and
       tbl_ausp-atinn = v_atinn_manufacturer.
      v_ausp_index = sy-tabix + 1.
      tbl_manu-atwrt = tbl_ausp-atwrt.
      append tbl_manu.
    else.
      exit.
    endif.
  enddo.

* get all the model numbers
  clear tbl_ausp.
  read table tbl_ausp with key objek = tbl_mara-matnr
                               atinn = v_atinn_model_number
                               binary search.
  if sy-subrc = 0.
    v_ausp_index = sy-tabix + 1.
    tbl_model-atwrt = tbl_ausp-atwrt.
    append tbl_model.
  endif.

  do.
    read table tbl_ausp index v_ausp_index.
    if tbl_ausp-objek = tbl_mara-matnr and
       tbl_ausp-atinn = v_atinn_model_number.
      v_ausp_index = sy-tabix + 1.
      tbl_model-atwrt = tbl_ausp-atwrt.
      append tbl_model.
    else.
      exit.
    endif.
  enddo.

* when there is no model number, use the manufacturers as is
  if tbl_model[] is initial and not tbl_manu[] is initial.
    loop at tbl_manu.
      if sy-tabix = 1.
        tbl_materials-manufacturer = tbl_manu-atwrt.
      else.
        concatenate tbl_materials-manufacturer '^' tbl_manu-atwrt
           into tbl_materials-manufacturer.
      endif.
    endloop.
  endif.

* when there is only 1 model number, use the last manufacturer
  describe table tbl_model lines v_model_count.
  if v_model_count = 1.
    loop at tbl_model.
      split tbl_model-atwrt at ' ' into v_string1 v_string2.
      tbl_materials-model_number = v_string1.
    endloop.
    loop at tbl_manu.
      tbl_materials-manufacturer = tbl_manu-atwrt.
    endloop.
  endif.

* when there are multiple models, get the manufacturer from the second part of model
  if v_model_count > 1.
    loop at tbl_model.
      split tbl_model-atwrt at ' ' into v_string1 v_string2.
      if v_string1 <> ' '.
        if tbl_materials-model_number = ' '.
          tbl_materials-model_number = v_string1.
        else.
          concatenate v_string1 '^' tbl_materials-model_number
             into tbl_materials-model_number.
        endif.
      else.
        if tbl_materials-model_number = ' '.
          tbl_materials-model_number = 'N/A'.
        else.
          concatenate 'N/A' '^' tbl_materials-model_number
             into tbl_materials-model_number.
        endif.

      endif.
      if v_string2 <> ' '.
        if tbl_materials-manufacturer = ' '.
          tbl_materials-manufacturer = v_string2.
        else.
          concatenate v_string2 '^' tbl_materials-manufacturer
        into tbl_materials-manufacturer.
        endif.
      else.
        if tbl_materials-manufacturer = ' '.
          tbl_materials-manufacturer = 'N/A'.
        else.
          concatenate 'N/A' '^' tbl_materials-manufacturer
        into tbl_materials-manufacturer.
        endif.
      endif.
    endloop.
  endif.

endform.                    " SETUP_MODELS_MANUFACTURERS
*&---------------------------------------------------------------------*
*&      Form  ROLL_UP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form roll_up_data .

* The data original is at the plant level.  The user may request it be rolled up
* to the company level or higher to the material level.

  clear: tbl_roll, v_different_price.
  refresh tbl_roll.

  sort tbl_materials by matnr bukrs werks.

* summarize up to the company level
  if p_comp = 'X'.
    loop at tbl_materials.
      if ( tbl_materials-matnr <> tbl_roll-matnr or
           tbl_materials-bukrs <> tbl_roll-bukrs ) and
           sy-tabix <> 1.
        append tbl_roll.
        clear tbl_roll.
        clear v_different_price.
      endif.

      tbl_roll-matkl = tbl_materials-matkl.
      tbl_roll-wgbez = tbl_materials-wgbez.
      tbl_roll-matnr = tbl_materials-matnr.
      tbl_roll-maktx = tbl_materials-maktx.
      tbl_roll-meins = tbl_materials-meins.
      tbl_roll-werks = ' '.
      tbl_roll-bukrs = tbl_materials-bukrs.
      tbl_roll-lvorm_mara = tbl_materials-lvorm_mara.
      tbl_roll-lvorm_marc = ' '.
      tbl_roll-lvorm_mbew = ' '.
      tbl_roll-waers = tbl_materials-waers.
      if tbl_roll-peinh = ' '.
        tbl_roll-peinh = tbl_materials-peinh.
      else.
        if tbl_roll-peinh <> tbl_materials-peinh and  "CANNOT COMBINE DIFFERENT PRICE UNITS
           tbl_roll-peinh <> 9999.
          tbl_roll-peinh = 9999.
        endif.
      endif.
      if tbl_roll-vprsv = ' '.
        tbl_roll-vprsv = tbl_materials-vprsv.
      else.
        if tbl_roll-vprsv <> tbl_materials-vprsv.  "CANNOT COMBINE DIFFERENT PRICE CONTROL INDICATORS
          tbl_roll-vprsv = '?'.
        endif.
      endif.
      tbl_roll-lbkum = tbl_roll-lbkum + tbl_materials-lbkum.
      if tbl_roll-verpr = 0 and v_different_price = ' '.
        tbl_roll-verpr = tbl_materials-verpr.
      endif.
      if tbl_roll-verpr <> 0 and tbl_materials-verpr <> tbl_roll-verpr.
        v_different_price = 'X'.
      endif.
      tbl_roll-aup = tbl_materials-aup.
      tbl_roll-price_var = tbl_materials-price_var.
      if tbl_roll-min = 0 or tbl_roll-min > tbl_materials-min.
        tbl_roll-min = tbl_materials-min.
      endif.
      if tbl_roll-max = 0 or tbl_roll-max < tbl_materials-max.
        tbl_roll-max = tbl_materials-max.
      endif.
      tbl_roll-salk3 = tbl_roll-salk3 + tbl_materials-salk3.
      tbl_roll-value_aup = tbl_roll-value_aup + tbl_materials-value_aup.
      tbl_roll-value_var = tbl_materials-value_var.
      tbl_roll-nla_code = tbl_materials-nla_code.
      tbl_roll-primary_desc = tbl_materials-primary_desc.
      tbl_roll-secondary_desc = tbl_materials-secondary_desc.
      tbl_roll-manufacturer = tbl_materials-manufacturer.
      tbl_roll-model_number = tbl_materials-model_number.

* if the price is not the same. clear out all price related values
      if v_different_price = 'X'.
        tbl_roll-verpr = 0.
        tbl_roll-price_var = 0.
        tbl_roll-value_var = 0.
      endif.

    endloop.
    append tbl_roll.
  endif.

*  summarize up to the material level
  if p_matl = 'X'.
    loop at tbl_materials.
      if tbl_materials-matnr <> tbl_roll-matnr and
           sy-tabix <> 1.
        append tbl_roll.
        clear tbl_roll.
        clear v_different_price.
      endif.

      tbl_roll-matkl = tbl_materials-matkl.
      tbl_roll-wgbez = tbl_materials-wgbez.
      tbl_roll-matnr = tbl_materials-matnr.
      tbl_roll-maktx = tbl_materials-maktx.
      tbl_roll-meins = tbl_materials-meins.
      tbl_roll-werks = ' '.
      tbl_roll-bukrs = ' '.
      tbl_roll-lvorm_mara = tbl_materials-lvorm_mara.
      tbl_roll-lvorm_marc = ' '.
      tbl_roll-lvorm_mbew = ' '.

      if tbl_roll-waers = ' '.
        tbl_roll-waers = tbl_materials-waers.
      else.
        if tbl_roll-waers <> tbl_materials-waers and  "CANNOT COMBINE DIFFERENT CURRENCIES
           tbl_roll-waers <> 9999.
          tbl_roll-waers = 9999.
        endif.
      endif.

      if tbl_roll-peinh = ' '.
        tbl_roll-peinh = tbl_materials-peinh.
      else.
        if tbl_roll-peinh <> tbl_materials-peinh and  "CANNOT COMBINE DIFFERENT PRICE UNITS
           tbl_roll-peinh <> 9999.
          tbl_roll-peinh = 9999.
        endif.
      endif.

      if tbl_roll-vprsv = ' '.
        tbl_roll-vprsv = tbl_materials-vprsv.
      else.
        if tbl_roll-vprsv <> tbl_materials-vprsv.  "CANNOT COMBINE DIFFERENT PRICE CONTROL INDICATORS
          tbl_roll-vprsv = '?'.
        endif.
      endif.

      tbl_roll-lbkum = tbl_roll-lbkum + tbl_materials-lbkum.
      if tbl_roll-verpr = 0 and v_different_price = ' '.
        tbl_roll-verpr = tbl_materials-verpr.
      endif.
      if tbl_roll-verpr <> 0 and tbl_materials-verpr <> tbl_roll-verpr.
        v_different_price = 'X'.
      endif.
      tbl_roll-aup = tbl_materials-aup.
      tbl_roll-price_var = tbl_materials-price_var.
      if tbl_roll-min = 0 or tbl_roll-min > tbl_materials-min.
        tbl_roll-min = tbl_materials-min.
      endif.
      if tbl_roll-max = 0 or tbl_roll-max < tbl_materials-max.
        tbl_roll-max = tbl_materials-max.
      endif.
      tbl_roll-salk3 = tbl_roll-salk3 + tbl_materials-salk3.
      tbl_roll-value_aup = tbl_roll-value_aup + tbl_materials-value_aup.
      tbl_roll-value_var = tbl_materials-value_var.
      tbl_roll-nla_code = tbl_materials-nla_code.
      tbl_roll-primary_desc = tbl_materials-primary_desc.
      tbl_roll-secondary_desc = tbl_materials-secondary_desc.
      tbl_roll-manufacturer = tbl_materials-manufacturer.
      tbl_roll-model_number = tbl_materials-model_number.

* if the price is not the same. clear out all price related values
      if v_different_price = 'X'.
        tbl_roll-verpr = 0.
        tbl_roll-price_var = 0.
        tbl_roll-value_var = 0.
      endif.

    endloop.
    append tbl_roll.
  endif.

endform.                    " ROLL_UP_DATA
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_AVERAGE_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form calculate_average_price .

  if p_count = 'X'.
    tbl_min_max-avg = v_price_total / v_price_count.
  endif.
  if p_qty = 'X' and v_lbkum_total <> 0.
    tbl_min_max-avg = v_salk3_total / v_lbkum_total.
    if v_peinh <> 0.
      tbl_min_max-avg = tbl_min_max-avg * v_peinh.
    endif.
  endif.

  if p_both = 'X'.                           "AUP based on quantity first, then count
    if v_lbkum_total <> 0.
      tbl_min_max-avg = v_salk3_total / v_lbkum_total.
      if v_peinh <> 0.
        tbl_min_max-avg = tbl_min_max-avg * v_peinh.
      endif.
    else.
      tbl_min_max-avg = v_price_total / v_price_count.
    endif.
  endif.

  append tbl_min_max.
  clear tbl_min_max.

endform.                    " CALCULATE_AVERAGE_PRICE

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
