******************************************************************
*                                                                *
*   PROGRAM: ZMMR_SLOW_MOVING_STOCK                              *
*   AUTHOR:  Larry Ritchie                                       *
*   CREATED: 2009/12/29                                          *
*                                                                *
*   DESCRIPTION: This slow moving stock report will show the     *
*                quantities, dollars and last material document. *
*                It is a newer version of ZMINR003.              *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
* 2009/12/29 LRITCHIE TR774 New program for DBossy specs         *
* 2012/09/06 M Khan   TR995 Change C: drive to H: drive with dir-*
*                           ectory file selection using F4 & move*
*                           hard-coded file path/name to variant.*
*
******************************************************************

report zmmr_slow_moving_stock line-size 255 no standard page heading line-count 65.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

tables: ausp,                           "Material/characteristics
        cabn,                           "Characteristics
        mara,                           "Material
        marc,                           "Material/plant
        mard,                           "Material/plant/storage loc
        makt,                           "Material description
        mseg,                           "Material document
        resb,                           "Reservations
        t001,                           "Companies
        t001k,                          "Valuation area
        zmm_last_mseg.                  "Last material document

type-pools  slis.

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* material date - MARA & MAKT
data: begin of tbl_mara occurs 0,
         matnr      like mara-matnr,
         matkl      like mara-matkl,
         meins      like mara-meins,
         maktx      like makt-maktx,
      end of tbl_mara.

* material/plant data - MARC
data: begin of tbl_marc occurs 0,
         matnr      like marc-matnr,
         werks      like marc-werks,
         dismm      like marc-dismm,
         minbe      like marc-minbe,
         eisbe      like marc-eisbe,
         mabst      like marc-mabst,
      end of tbl_marc.

* material/plant/storage location - MARD
data: begin of tbl_mard occurs 0,
         matnr      like mard-matnr,
         werks      like mard-werks,
         lgort      like mard-lgort,
         labst      like mard-labst,          "unrestricted qty
         umlme      like mard-umlme,          "in transfer qty
         insme      like mard-insme,          "quality inspection qty
         speme      like mard-speme,          "blocked qty
         retme      like mard-retme,          "return qty
         lgpbe      like mard-lgpbe,          "bin location
      end of tbl_mard.

* material pricing - MBEW
data: begin of tbl_mbew occurs 0,
         matnr      like mbew-matnr,
         bwkey      like mbew-bwkey,          "plant
         vprsv      like mbew-vprsv,          "price control - S or V
         verpr      like mbew-verpr,          "moving average price
         stprs      like mbew-stprs,          "standard price
         peinh      like mbew-peinh,          "price unit
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

* material characteristics
data: begin of tbl_ausp occurs 0,
         objek      like ausp-objek,          "18 digit material number
         atwrt      like ausp-atwrt,          "characteristic value
      end of tbl_ausp.

* last material usage
data: begin of tbl_zmm_last_mseg occurs 0,
         matnr      like zmm_last_mseg-matnr,
         werks      like zmm_last_mseg-werks,
         lgort      like zmm_last_mseg-lgort,
         bwart      like zmm_last_mseg-bwart,
         sobkz      like zmm_last_mseg-sobkz,
         mblnr      like zmm_last_mseg-mblnr,
         mjahr      like zmm_last_mseg-mjahr,
         cpudt      like zmm_last_mseg-cpudt,
         budat      like zmm_last_mseg-budat,
      end of tbl_zmm_last_mseg.

* temporary material number table
data: begin of tbl_matnr occurs 0,
         matnr      like mara-matnr,
      end of tbl_matnr.

* temporary material/plant table
data: begin of tbl_matnr_werks occurs 0,
         matnr     like mara-matnr,
         werks     like marc-werks,
      end of tbl_matnr_werks.

* table to collect all the info for the output
data: begin of tbl_report occurs 0,
         matkl      like mara-matkl,
         matnr(18)  type c,
         maktx      like makt-maktx,
         atwrt      like ausp-atwrt,
         werks(5)   type c,
         dismm(8)   type c,
         minbe(10)  type c,
         mabst(10)  type c,
         eisbe(10)  type c,
         lgort      like mard-lgort,
         qoh(15)    type c,                   "all quantities
         meins      like mara-meins,
         verpr(15)  type c,
         peinh(10)  type c,
         salk3(15)  type c,
         waers(8)   type c,
         lgpbe(12)  type c,
         cpudt(10)  type c,
         mblnr(12)  type c,
         mjahr(4)   type c,
         bwart(9)   type c,
         sobkz(10)  type c,
       end of tbl_report.

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

data: v_screen_matl_group(1)  type c,
      v_screen_matl_no(1)     type c,
      v_screen_plant(1)       type c,
      v_screen_storage_loc(1) type c,
      v_no_material_data(1)   type c,
      v_labst(7)              type p,
      v_salk3                 like mbew-salk3,
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

selection-screen skip 2.
selection-screen begin of block b1 with frame title text-001.
selection-screen skip 1.
select-options s_matkl for mara-matkl.         "material group
select-options s_matnr for mara-matnr.         "material
select-options s_werks for marc-werks.         "plant
select-options s_lgort for mard-lgort.         "storage location
selection-screen end of block b1.

selection-screen skip 2.
selection-screen begin of block b2 with frame title text-002.
selection-screen skip 1.
selection-screen begin of line.
selection-screen comment 1(31) text-005.
parameter p_from like sy-datum obligatory default '19900101'.
selection-screen comment 52(20) text-006.
parameter p_to   like sy-datum obligatory default sy-datum.
selection-screen end of line.
select-options s_atwrt for ausp-atwrt.         "NLA code
select-options s_bwart for zmm_last_mseg-bwart.
select-options s_sobkz for zmm_last_mseg-sobkz.
selection-screen end of block b2.

selection-screen skip 2.
selection-screen begin of block b3 with frame title text-004.
selection-screen skip 1.
parameters p_alv radiobutton group r1.        "Print Report
parameters p_excel radiobutton group r1.       "Excel Spreadsheet
parameters p_file like rlgrap-filename default 'H:\SAPTEMP'. "TR995
selection-screen end of block b3.

selection-screen skip 2.
selection-screen comment 1(54) text-014.
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

  perform clear_variables.

  perform validate_screen_inputs.

  perform load_material_data.

  if v_no_material_data = ' '.
    perform load_valuations.
    perform load_companies.
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
  endif.

  if not s_lgort[] is initial.
    v_screen_storage_loc = 'X'.
  endif.

endform.                    " VALIDATE_SCREEN_INPUTS
*&---------------------------------------------------------------------*
*&      Form  LOAD_MATERIAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_material_data .

  refresh: tbl_mara, tbl_marc, tbl_mard, tbl_matnr, tbl_matnr_werks,
           tbl_ausp, tbl_mbew.

  clear:   tbl_mara, tbl_marc, tbl_mard, tbl_matnr, tbl_matnr_werks,
           tbl_ausp, tbl_mbew.

*******************************
*    USER ENTERED MATERIAL INFO
*******************************

  if v_screen_matl_group = 'X' or v_screen_matl_no = 'X'.

    select mara~matnr mara~matkl mara~meins makt~maktx
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
      select matnr werks dismm minbe eisbe mabst
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
      select matnr werks lgort labst umlme insme speme retme lgpbe
             into table tbl_mard
             from mard
             for all entries in tbl_matnr_werks
             where matnr = tbl_matnr_werks-matnr
               and werks = tbl_matnr_werks-werks
               and lgort in s_lgort
               and ( labst <> 0 or umlme <> 0 or insme <> 0 or         "make sure there is inventory
                     speme <> 0 or retme <> 0 ).
      sort tbl_mard by matnr werks lgort.
    endif.
  endif.

************************************************************
*    USER DID NOT ENTER MATERIAL INFO BUT ENTERED PLANT INFO
************************************************************
  if v_screen_matl_group = ' ' and v_screen_matl_no = ' ' and
     v_screen_plant = 'X'.
    select matnr werks dismm minbe eisbe mabst
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
      select mara~matnr mara~matkl mara~meins makt~maktx
             into table tbl_mara
             from mara inner join makt
                  on makt~matnr = mara~matnr
             for all entries in tbl_matnr
             where mara~matnr = tbl_matnr-matnr
               and makt~spras = 'EN'.
      sort tbl_mara by matnr.
    endif.

    if not tbl_matnr_werks[] is initial.
      select matnr werks lgort labst umlme insme speme retme lgpbe
              into table tbl_mard
              from mard
              for all entries in tbl_matnr_werks
              where matnr = tbl_matnr_werks-matnr
                and werks = tbl_matnr_werks-werks
                and lgort in s_lgort
                and ( labst <> 0 or umlme <> 0 or insme <> 0 or          "make sure there is inventory
                     speme <> 0 or retme <> 0 ).
      sort tbl_mard by matnr werks lgort.

    endif.
  endif.

********************************************************************************
*    USER DID NOT ENTER MATERIAL OR PLANT INFO BUT ENTERED STORAGE LOCATION DATA
********************************************************************************
  if v_screen_matl_group = ' ' and v_screen_matl_no = ' ' and
     v_screen_plant = ' ' and v_screen_storage_loc = 'X'.
    select matnr werks lgort labst umlme insme speme retme lgpbe
          into table tbl_mard
          from mard
          where lgort in s_lgort
            and ( labst <> 0 or umlme <> 0 or insme <> 0 or             "make sure there is inventory
                     speme <> 0 or retme <> 0 ).
    sort tbl_mard by matnr werks lgort.

    loop at tbl_mard.
      tbl_matnr-matnr = tbl_mard-matnr.
      append tbl_matnr.
      tbl_matnr_werks-matnr = tbl_mard-matnr.
      tbl_matnr_werks-werks = tbl_mard-werks.
      append tbl_matnr_werks.
    endloop.

    delete adjacent duplicates from tbl_matnr.

    delete adjacent duplicates from tbl_matnr_werks.

    if not tbl_matnr[] is initial.
      select mara~matnr mara~matkl mara~meins makt~maktx
          into table tbl_mara
          from mara inner join makt
               on makt~matnr = mara~matnr
          for all entries in tbl_matnr
          where mara~matnr = tbl_matnr-matnr
            and makt~spras = 'EN'.
      sort tbl_mara by matnr.
    endif.

    if not tbl_matnr_werks[] is initial.
      select matnr werks dismm minbe eisbe mabst
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

    select mara~matnr mara~matkl mara~meins makt~maktx
             into table tbl_mara
             from mara inner join makt
                  on makt~matnr = mara~matnr
             where makt~spras = 'EN'.

    sort tbl_mara by matnr.

    select matnr werks dismm minbe eisbe mabst
            into table tbl_marc
            from marc.

    sort tbl_marc by matnr werks.

    select matnr werks lgort labst umlme insme speme retme lgpbe
           into table tbl_mard
           from mard
           where ( labst <> 0 or umlme <> 0 or insme <> 0 or              "make sure there is inventory
                   speme <> 0 or retme <> 0 ).

    sort tbl_mard by matnr werks lgort.

  endif.

****************************
*    GET THE MATERIAL PRICES
****************************

  if not tbl_marc[] is initial.

    select matnr bwkey vprsv verpr stprs peinh
           into table tbl_mbew
           from mbew
           for all entries in tbl_marc
           where matnr = tbl_marc-matnr
             and bwkey = tbl_marc-werks
             and lbkum <> 0.                                             "make sure there is inventory
    sort tbl_mbew by matnr bwkey.

*******************************
*    GET LAST MATERIAL DOCUMENT
*******************************

    select matnr werks lgort bwart sobkz mblnr mjahr cpudt budat
           into table tbl_zmm_last_mseg
           from zmm_last_mseg
           for all entries in tbl_marc
           where matnr = tbl_marc-matnr
             and werks = tbl_marc-werks
             and bwart in s_bwart
             and sobkz in s_sobkz.

    sort tbl_zmm_last_mseg by matnr werks lgort ascending cpudt descending mblnr descending.

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

  v_marc_index = 1.
  v_mard_index = 1.

  loop at tbl_mara.
    clear tbl_report.

* material level

    clear tbl_ausp.
    read table tbl_ausp with key objek = tbl_mara-matnr
                                 binary search.

    if not s_atwrt[] is initial.
      if not tbl_ausp-atwrt in s_atwrt[].    "NLA_CODE
        continue.
      endif.
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

      clear tbl_mbew.
      read table tbl_mbew with key matnr = tbl_marc-matnr
                                   bwkey = tbl_marc-werks
                                   binary search.

      clear tbl_t001k.
      read table tbl_t001k with key bwkey = tbl_marc-werks
                                    binary search.
      if sy-subrc = 0.
        clear tbl_t001.
        read table tbl_t001 with key bukrs = tbl_t001k-bukrs
                                     binary search.
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

        clear tbl_zmm_last_mseg.
        read table tbl_zmm_last_mseg with key matnr = tbl_mard-matnr
                                              werks = tbl_mard-werks
                                              lgort = tbl_mard-lgort
                                              binary search.
        if sy-subrc = 0.
          if p_from <= tbl_zmm_last_mseg-cpudt and
             p_to >= tbl_zmm_last_mseg-cpudt.
            perform setup_mara_report_info.
            perform setup_marc_report_info.
            perform setup_mard_report_info.
            if p_excel = 'X'.
              translate tbl_report-maktx using ',_'.                   "remove comma for EXCEL
              translate tbl_report-maktx using '" '.                   "remove double quote for EXCEL
              translate tbl_report-lgpbe using ',_'.                   "remove comma for EXCEL
              translate tbl_report-lgpbe using '" '.                   "remove double quote for EXCEL
              concatenate '''' tbl_report-lgpbe into tbl_report-lgpbe.  "prevent scientific notation
            endif.
            append tbl_report.
            clear tbl_report.
          endif.
        endif.

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

  tbl_report-matkl = tbl_mara-matkl.
  tbl_report-maktx = tbl_mara-maktx.
  tbl_report-meins = tbl_mara-meins.
  tbl_report-atwrt = tbl_ausp-atwrt.

endform.                    " SETUP_MARA_REPORT_INFO
*&---------------------------------------------------------------------*
*&      Form  SETUP_MARC_REPORT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_marc_report_info .

  tbl_report-werks = tbl_marc-werks.
  tbl_report-dismm = tbl_marc-dismm.
  v_labst = tbl_marc-minbe.                  "drop decimals
  tbl_report-minbe = v_labst.
  v_labst = tbl_marc-eisbe.
  tbl_report-eisbe = v_labst.
  v_labst = tbl_marc-minbe.
  tbl_report-mabst = v_labst.
  tbl_report-waers = tbl_t001-waers.

  if tbl_mbew-vprsv = 'S'.
    write tbl_mbew-stprs to tbl_report-verpr.
  else.
    write tbl_mbew-verpr to tbl_report-verpr.
  endif.

  write tbl_mbew-peinh to tbl_report-peinh.

endform.                    " SETUP_MARC_REPORT_INFO
*&---------------------------------------------------------------------*
*&      Form  SETUP_MARD_REPORT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_mard_report_info .

  tbl_report-lgort  = tbl_mard-lgort.
  v_labst = tbl_mard-labst + tbl_mard-umlme + tbl_mard-insme +
            tbl_mard-speme + tbl_mard-retme.
  write v_labst to tbl_report-qoh.
  tbl_report-lgpbe = tbl_mard-lgpbe.

  if tbl_mbew-vprsv = 'S'.                  "standard price
    if tbl_mbew-peinh = ' '.
      v_salk3 = v_labst * tbl_mbew-stprs.
    else.
      v_salk3 = v_labst * tbl_mbew-stprs / tbl_mbew-peinh.
    endif.
  else.                                     "moving average price
    if tbl_mbew-peinh = ' '.
      v_salk3 = v_labst * tbl_mbew-verpr.
    else.
      v_salk3 = v_labst * tbl_mbew-verpr / tbl_mbew-peinh.
    endif.
  endif.

  write v_salk3 to tbl_report-salk3.

  tbl_report-mblnr = tbl_zmm_last_mseg-mblnr.
  tbl_report-mjahr = tbl_zmm_last_mseg-mjahr.
  write tbl_zmm_last_mseg-cpudt to tbl_report-cpudt.
  tbl_report-bwart = tbl_zmm_last_mseg-bwart.
  tbl_report-sobkz = tbl_zmm_last_mseg-sobkz.

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
  tbl_fieldcat-fieldname = 'MATNR'.
  tbl_fieldcat-seltext_s = 'Mtl#'.
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
  tbl_fieldcat-fieldname = 'ATWRT'.
  tbl_fieldcat-seltext_s = 'NLA_Code'.
  tbl_fieldcat-seltext_m = 'NLA_Code'.
  tbl_fieldcat-seltext_l = 'NLA_Code'.
  tbl_fieldcat-reptext_ddic = 'NLA_Code'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WERKS'.
  tbl_fieldcat-seltext_s = 'Plnt'.
  tbl_fieldcat-seltext_m = 'Plnt'.
  tbl_fieldcat-seltext_l = 'Plnt'.
  tbl_fieldcat-reptext_ddic = 'Plnt'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'DISMM'.
  tbl_fieldcat-seltext_s = 'MRP'.
  tbl_fieldcat-seltext_m = 'MRP Type'.
  tbl_fieldcat-seltext_l = 'MRP Type'.
  tbl_fieldcat-reptext_ddic = 'MRP Type'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'EISBE'.
  tbl_fieldcat-seltext_s = 'Safety'.
  tbl_fieldcat-seltext_m = 'Safety Stock'.
  tbl_fieldcat-seltext_l = 'Safety Stock'.
  tbl_fieldcat-reptext_ddic = 'Safety Stock'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MINBE'.
  tbl_fieldcat-seltext_s = 'Reorder'.
  tbl_fieldcat-seltext_m = 'Reorder Point'.
  tbl_fieldcat-seltext_l = 'Reorder Point'.
  tbl_fieldcat-reptext_ddic = 'Reorder Point'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MABST'.
  tbl_fieldcat-seltext_s = 'Maxlvl'.
  tbl_fieldcat-seltext_m = 'Maximum Stock Level'.
  tbl_fieldcat-seltext_l = 'Maximum Stock Level'.
  tbl_fieldcat-reptext_ddic = 'Maximum Stock Level'.
  tbl_fieldcat-just = 'R'.
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
  tbl_fieldcat-fieldname = 'MEINS'.
  tbl_fieldcat-seltext_s = 'UOM'.
  tbl_fieldcat-seltext_m = 'Unit of Measure'.
  tbl_fieldcat-seltext_l = 'Unit of Measure'.
  tbl_fieldcat-reptext_ddic = 'Unit of Measure'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'VERPR'.
  tbl_fieldcat-seltext_s = 'AUP'.
  tbl_fieldcat-seltext_m = 'AUP'.
  tbl_fieldcat-seltext_l = 'AUP'.
  tbl_fieldcat-reptext_ddic = 'AUP'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'PEINH'.
  tbl_fieldcat-seltext_s = 'AUP Un'.
  tbl_fieldcat-seltext_m = 'AUP Unit'.
  tbl_fieldcat-seltext_l = 'Average Unit Price'.
  tbl_fieldcat-reptext_ddic = 'Average Unit Price'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'SALK3'.
  tbl_fieldcat-seltext_s = '$Value'.
  tbl_fieldcat-seltext_m = '$ Total Value'.
  tbl_fieldcat-seltext_l = '$ Total Value'.
  tbl_fieldcat-reptext_ddic = '$ Total Value'.
  tbl_fieldcat-just = 'R'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WAERS'.
  tbl_fieldcat-seltext_s = 'Cur'.
  tbl_fieldcat-seltext_m = 'Currency'.
  tbl_fieldcat-seltext_l = 'Currency'.
  tbl_fieldcat-reptext_ddic = 'Currency'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LGPBE'.
  tbl_fieldcat-seltext_s = 'Bin Loc'.
  tbl_fieldcat-seltext_m = 'Bin Location'.
  tbl_fieldcat-seltext_l = 'Storage Bin Location'.
  tbl_fieldcat-reptext_ddic = 'Storage Bin Location'.
  tbl_fieldcat-just = 'L'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'CPUDT'.
  tbl_fieldcat-seltext_s = 'Last Used'.
  tbl_fieldcat-seltext_m = 'Material Last Used'.
  tbl_fieldcat-seltext_l = 'Material Last Used Date'.
  tbl_fieldcat-reptext_ddic = 'Material Last Used Date'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MBLNR'.
  tbl_fieldcat-seltext_s = 'Mat Doc'.
  tbl_fieldcat-seltext_m = 'Material Document'.
  tbl_fieldcat-seltext_l = 'Material Document'.
  tbl_fieldcat-reptext_ddic = 'Material Document'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'MJAHR'.
  tbl_fieldcat-seltext_s = 'M D Year'.
  tbl_fieldcat-seltext_m = 'Material Doc Year'.
  tbl_fieldcat-seltext_l = 'Material Document Year'.
  tbl_fieldcat-reptext_ddic = 'Material Document Year'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BWART'.
  tbl_fieldcat-seltext_s = 'MvT'.
  tbl_fieldcat-seltext_m = 'Movement Type'.
  tbl_fieldcat-seltext_l = 'Movement Type'.
  tbl_fieldcat-reptext_ddic = 'Movement Type'.
  tbl_fieldcat-just = 'C'.
  append tbl_fieldcat to tbl_fieldtab.
  clear tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'SOBKZ'.
  tbl_fieldcat-seltext_s = 'Sp Stk Ind'.
  tbl_fieldcat-seltext_m = 'Special Stock Ind'.
  tbl_fieldcat-seltext_l = 'Special Stock Indicator'.
  tbl_fieldcat-reptext_ddic = 'Special Stock Indicator'.
  tbl_fieldcat-just = 'C'.
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
*     file_name                 = 'C:\SAPTEMP' "TR995
      file_name                 = P_FILE       "TR995
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

  move 'Mtl Grp'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Mtl#'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Material Desc'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'NLA Code'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Plnt'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MRP'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Safety'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Reorder'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Maxlvl'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'SLoc'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'QOH'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'UOM'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'AUP'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'AUP Un'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move '$ Value'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Cur'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Bin Loc'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Last Used'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Mat Doc'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Year'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'MvT'  to tbl_excel_header-spaltenname.
  append tbl_excel_header.
  move 'Sp Stk Ind'  to tbl_excel_header-spaltenname.
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
          from t001k.

  sort tbl_t001k by bwkey.

endform.                    " LOAD_VALUATIONS
*&---------------------------------------------------------------------*
*&      Form  LOAD_COMPANIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_companies .

  select bukrs waers
          into table tbl_t001
          from t001.

  sort tbl_t001 by bukrs.

endform.                    " LOAD_COMPANIES

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
