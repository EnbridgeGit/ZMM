*&---------------------------------------------------------------------*
*& Report  ZLMMI042_PUR_CONTRACT_CREATE
*&
*&---------------------------------------------------------------------*
************************************************************************
*                            COGS                                      *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI042_PUR_CONTRACT_CREATE                  *
*& Author             :  Jaydeep Waychal/Prasanth Durbhaka             *
*& Creation Date      :  March 23, 2021                                *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program to upload file and update existing    *
*                                                                      *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*

REPORT  zlmmi042_pur_contract_create NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: ekko,zmmt_mastagree,konv.

* Types Declarations *
TYPES : BEGIN OF ty_upload,
           string TYPE string,
        END OF ty_upload,

        BEGIN OF ty_input_data,
            vendor      TYPE  elifn, " Header
            cont_typ    TYPE  bsart,
            agre_date    TYPE aedat,
            purch_org   TYPE  ekorg,
            pur_group   TYPE  bkgrp,
            comp_code   TYPE  bukrs,
            vper_start  TYPE  kdatb,
            vper_end    TYPE  kdate,
            pmnttrms    TYPE  dzterm,
            incoterms1  TYPE  inco1,
            zzcondayqty TYPE  z_condayqty,
            zzparty_agmt_id TYPE  zzparty,
            zzmsa       TYPE  zzmsa,
            zzoldealid  TYPE  zoldealid,
            zzekgrp     TYPE  ztrbuy,
            itm_cat     TYPE  pstyp,
            ass_cat     TYPE  knttp,
            currency    TYPE waers,
            plant       TYPE  werks_d,
            matl_group  TYPE  matkl,

            item_no     TYPE  ebelp,   " Item
            tax_code    TYPE  mwskz,
            itm_txt     TYPE  txz01,
            trackingno  TYPE  bednr,
            price_cat   TYPE  meprf,
            target_qty  TYPE  char13,

            ser_num   TYPE srvpos,
            price(16)   TYPE c,  "SBRTWR,
            qty_per  TYPE char13,
            uom  TYPE meins,
            price_unit TYPE d, "peinh,
            cost_center TYPE kostl,
            glaccount  TYPE sakto,
            userf2_num TYPE char17, "USERF2_NUM,
            userf1_txt TYPE userf1_txt,
        END OF ty_input_data,


        BEGIN OF ty_header,
            vendor      TYPE  elifn, " Header
            cont_typ    TYPE  bsart,
            agre_date    TYPE aedat,
            purch_org   TYPE  ekorg,
            pur_group   TYPE  bkgrp,
            comp_code   TYPE  bukrs,
            vper_start  TYPE  kdatb,
            vper_end    TYPE  kdate,
            pmnttrms    TYPE  dzterm,
            incoterms1  TYPE  inco1,
            zzcondayqty TYPE  z_condayqty,
            zzparty_agmt_id TYPE  zzparty,
            zzmsa       TYPE  zzmsa,
            verkf       TYPE  verkf,
            zzoldealid  TYPE  zoldealid,
            zzekgrp     TYPE  ztrbuy,
            itm_cat     TYPE  pstyp,
            ass_cat     TYPE  knttp,
            currency    TYPE waers,
            plant       TYPE  werks_d,
            matl_group  TYPE  matkl,
          END OF ty_header,

          BEGIN OF ty_item,
            zzoldealid  TYPE  zoldealid,
            item_no     TYPE  ebelp,   " Item
            tax_code    TYPE  mwskz,
            itm_txt     TYPE  txz01,
            trackingno  TYPE  bednr,
            price_cat   TYPE  meprf,
            target_qty  TYPE  ktmng,
          END OF ty_item,

          BEGIN OF ty_services,
            zzoldealid  TYPE  zoldealid,
            item_no     TYPE  ebelp,
            ser_num   TYPE srvpos,
            price   TYPE sbrtwr,
            qty_per  TYPE menge,
            uom  TYPE meins,
            price_unit TYPE peinh,
            cost_center TYPE kostl,
            glaccount  TYPE sakto,
            userf2_num TYPE userf2_num,
            userf1_txt TYPE userf1_txt,
          END OF ty_services.

TYPES:  BEGIN OF ty_final,
           vendor      TYPE  elifn, " Header
            cont_typ    TYPE  bsart,
            agre_date    TYPE aedat,
            purch_org   TYPE  ekorg,
            pur_group   TYPE  bkgrp,
            comp_code   TYPE  bukrs,
            vper_start  TYPE  kdatb,
            vper_end    TYPE  kdate,
            pmnttrms    TYPE  dzterm,
            incoterms1  TYPE  inco1,
            zzcondayqty TYPE  z_condayqty,
            zzparty_agmt_id TYPE  zzparty,
            zzmsa       TYPE  zzmsa,
            zzoldealid  TYPE  zoldealid,
            zzekgrp     TYPE  ztrbuy,
            itm_cat     TYPE  pstyp,
            ass_cat     TYPE  knttp,
            currency    TYPE waers,
            plant       TYPE  werks_d,
            matl_group  TYPE  matkl,

            item_no     TYPE  ebelp,   " Item
            tax_code    TYPE  mwskz,
            itm_txt     TYPE  txz01,
            trackingno  TYPE  bednr,
            price_cat   TYPE  meprf,
            target_qty  TYPE  ktmng,

           ser_num   TYPE srvpos,
          price   TYPE sbrtwr,
          qty_per  TYPE menge,
          uom  TYPE meins,
          price_unit TYPE peinh,
          cost_center TYPE kostl,
          glaccount  TYPE sakto,
          userf2_num TYPE userf2_num,
          userf1_txt TYPE userf1_txt,
            errmsg(2000)  TYPE c,
        END OF ty_final.
TYPES:  BEGIN OF ty_msgout,
            zzoldealid  TYPE  zoldealid,
            errmsg(2000) TYPE c,
            errtyp(1)    TYPE c,  "S - success , E-Error
        END OF ty_msgout.


*validation
TYPES: BEGIN OF ty_agmid,
            zzparty_agmt_id TYPE zzparty,
            zzcpid          TYPE zcpid,
       END OF ty_agmid.

TYPES: BEGIN OF ty_dealid,
            zzoldealid  TYPE  zoldealid,
       END OF ty_dealid.

TYPES: BEGIN OF ty_sapdeals,
  ebeln TYPE ebeln,
  zzoldealid TYPE zoldealid,
  verkf TYPE everk,
  END OF ty_sapdeals.
*Internal Table Declarations *.
DATA: lta_upload       TYPE STANDARD TABLE OF ty_upload,
      lta_input_data   TYPE STANDARD TABLE OF ty_input_data,
      lta_final        TYPE STANDARD TABLE OF ty_final,
      lta_msgout       TYPE STANDARD TABLE OF ty_msgout,
      lta_agmtid       TYPE STANDARD TABLE OF ty_agmid,
      lta_header       TYPE STANDARD TABLE OF ty_header,
      lta_header1       TYPE STANDARD TABLE OF ty_header,
      lta_item         TYPE STANDARD TABLE OF ty_item,
      lta_services      TYPE STANDARD TABLE OF ty_services,
      lta_dealid        TYPE STANDARD TABLE OF ty_dealid,
      lta_sapdeals      TYPE STANDARD TABLE OF ty_sapdeals.

DATA: git_bdcdata  TYPE TABLE OF bdcdata,                    " Internal table for BDCDATA
      gwa_bdcdata  LIKE LINE OF git_bdcdata,                 " Work area for BDCDATA
      git_messtab  TYPE TABLE OF bdcmsgcoll,                 " Internal table for BDC error msg
      git_messtab1 TYPE TABLE OF bdcmsgcoll,                 " Internal table for BDC error msg
      git_messtab2 TYPE TABLE OF bdcmsgcoll,                 " Internal table for BDC error msg
      gwa_messtab  LIKE LINE OF git_messtab,                 " Work area for BDC error msg
      git_olaid    TYPE STANDARD TABLE OF zcontract_olaid,   " Internal table for OLAID
      gwa_olaid    LIKE LINE OF git_olaid,                   " Work Area for it_olaid
      git_msg      TYPE STANDARD TABLE OF bapiret2,          " Internal table for Return parameter
      git_msg1     TYPE STANDARD TABLE OF bapiret2,          " Internal table for Return parameter
      git_msg2     TYPE STANDARD TABLE OF bapiret2,          " Internal table for Return parameter
      gwa_msg      LIKE LINE OF git_msg.

* Work Area Declarations *
DATA: lwa_upload       TYPE ty_upload,
      lwa_input_data   TYPE ty_input_data,
      lwa_final        TYPE ty_final,
      lwa_msgout       TYPE ty_msgout,
      lwa_agmtid       TYPE ty_agmid,
      lwa_header       TYPE ty_header,
      lwa_item         TYPE ty_item,
      lwa_services      TYPE ty_services,
      lwa_dealid       TYPE ty_dealid,
      lwa_sapdeals     TYPE ty_sapdeals.

* Variable Declarations *
DATA: lv_sep(1)      TYPE c VALUE ',',
      lv_tstflg      TYPE c,
      lv_errtype     TYPE c VALUE 'E',
      lv_cnt(5)      TYPE n,
      lv_path        TYPE btch0000-text80.
DATA: lv_date(10)     TYPE c, " Agriment creation date
      lv_start(10)    TYPE c, " Agriment start date
      lv_end(10)      TYPE c, " Agriment end date
      lv_qty(20)      TYPE c, " Quantity conversion
      lv_prc(15)      TYPE c, " Price conversion
      lv_count(2)     TYPE n,  " Service Count
      lv_itmcnt(2)    TYPE n,  " Item Count
      lv_page         TYPE c,  " PAge down flag
      lv_text(20)     TYPE c,  " Screen field
      lv_user2(17)    TYPE c,  " User field 2
      lv_pagedown(3)   TYPE n VALUE '10',  " Pagedown count
      lv_itmpage(3)    TYPE n VALUE '01'. "Iem page

DATA: lv_flag TYPE c,
      lv_msg(50) TYPE c,
      lv_matkl   TYPE matkl,
      lv_message(2000) TYPE c.

* Constant Declarations *
CONSTANTS: lc_doccat   TYPE c VALUE 'K',
           lc_gstrnspt TYPE c VALUE 'T',
           lc_gsstorge TYPE c VALUE 'S',
           lc_status   TYPE c VALUE 'A',
           lc_telphone1(3) TYPE c VALUE 'G00',
           lc_telphone2(3) TYPE c VALUE 'G99'.

** Declaration for ALV Grid **
DATA : gr_table TYPE REF TO cl_salv_table.
** Declarations for ALV Functions
DATA : gr_functions TYPE REF TO cl_salv_functions_list.

** declaration for ALV Columns
DATA : gr_columns    TYPE REF TO cl_salv_columns_table,
       gr_column     TYPE REF TO cl_salv_column_table,
       lt_column_ref TYPE salv_t_column_ref,
       ls_column_ref TYPE salv_s_column_ref.

** declaration for Layout Settings
DATA : gr_layout     TYPE REF TO cl_salv_layout,
       gr_layout_key TYPE salv_s_layout_key.
** Declaration for Global Display Settings
DATA : gr_display TYPE REF TO cl_salv_display_settings,
       lv_title   TYPE lvc_title.

** Declaration for Aggregate Function Settings
DATA : gr_aggr    TYPE REF TO cl_salv_aggregations.

** Declaration for Sort Function Settings
DATA : gr_sort    TYPE REF TO cl_salv_sorts.

** Declaration for Table Selection settings
DATA : gr_select  TYPE REF TO cl_salv_selections.

** Declaration for Top of List settings
DATA : gr_content TYPE REF TO cl_salv_form_element.

************************************************************************
* Selection screen *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETER: p_fname TYPE rlgrap-filename.
PARAMETER: p_deal(1) TYPE c  OBLIGATORY.
*PARAMETER: p_dl AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* AT SELECTION-SCREEN processes
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

*-- Form to Select a File From a Particular Location
  PERFORM get_filename_f4 .

************************************************************************
* initialization *
************************************************************************


************************************************************************
* Start-of-selection *
************************************************************************
START-OF-SELECTION.
*** To upload flat file data into the internal table.
  PERFORM file_upload.
*** Split data from input file into an internal table
  PERFORM split_data.
****Ccreate purchase contract
  PERFORM create_pur_contract.
***Display end result
  IF lta_msgout[] IS NOT INITIAL.
    PERFORM alv_grid_display.
*  ELSE.
*    MESSAGE 'File good to load' TYPE 'I'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_filename_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_filename_f4 .

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
    CHANGING
      file_name     = p_fname
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " get_filename_f4

*&---------------------------------------------------------------------*
*&      Form  file_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM file_upload .

  DATA : lv_file TYPE string.

  lv_file = p_fname.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file
*     filetype                = 'ASC'
*     has_field_separator     = 'X'
    TABLES
      data_tab                = lta_upload
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " file_upload
*&---------------------------------------------------------------------*
*&      Form  split_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM split_data .
**Moving data into internal table to validate
  IF lta_upload IS NOT INITIAL.
    LOOP AT lta_upload INTO lwa_upload.
      SPLIT lwa_upload-string AT lv_sep INTO
            lwa_input_data-vendor  " Header
            lwa_input_data-cont_typ
            lwa_input_data-agre_date
            lwa_input_data-purch_org
            lwa_input_data-pur_group
            lwa_input_data-comp_code
            lwa_input_data-vper_start
            lwa_input_data-vper_end
            lwa_input_data-pmnttrms
            lwa_input_data-incoterms1
            lwa_input_data-zzcondayqty
            lwa_input_data-zzparty_agmt_id
            lwa_input_data-zzmsa
            lwa_input_data-zzoldealid
            lwa_input_data-zzekgrp
            lwa_input_data-itm_cat
            lwa_input_data-ass_cat
            lwa_input_data-currency
            lwa_input_data-plant
            lwa_input_data-matl_group
            lwa_input_data-item_no
            lwa_input_data-tax_code
            lwa_input_data-itm_txt
            lwa_input_data-trackingno
            lwa_input_data-price_cat
           lwa_input_data-target_qty
           lwa_input_data-ser_num
          lwa_input_data-price
          lwa_input_data-qty_per
          lwa_input_data-uom
          lwa_input_data-price_unit
          lwa_input_data-cost_center
          lwa_input_data-glaccount
          lwa_input_data-userf2_num
          lwa_input_data-userf1_txt.
      APPEND lwa_input_data TO lta_input_data.
**Collect Old deal ID's.
      lwa_dealid-zzoldealid = lwa_input_data-zzoldealid.
      APPEND lwa_dealid TO lta_dealid.
      CLEAR : lwa_upload, lwa_dealid.
    ENDLOOP.
    DELETE lta_input_data INDEX 1.

**Prevalidation to eliminate data that is already created
    PERFORM check_oldealid.

    LOOP AT lta_input_data INTO lwa_input_data.
**Ignore the deals that have already been created in SAP
      READ TABLE lta_sapdeals INTO lwa_sapdeals WITH KEY zzoldealid = lwa_input_data-zzoldealid.
      IF sy-subrc EQ 0.
        CONCATENATE text-003 lwa_sapdeals-ebeln INTO lv_message.
        lwa_msgout-zzoldealid = lwa_input_data-zzoldealid.
        lwa_msgout-errmsg = lv_message.
        lwa_msgout-errtyp = 'E'.
        APPEND lwa_msgout TO lta_msgout.
        CLEAR: lv_message,
               lwa_msgout.
      ELSE.
        lwa_header-vendor = lwa_input_data-vendor.
        lwa_header-cont_typ   = lwa_input_data-cont_typ.
        lwa_header-agre_date  = lwa_input_data-agre_date.
        lwa_header-purch_org  = lwa_input_data-purch_org.
        lwa_header-pur_group  = lwa_input_data-pur_group.
        lwa_header-comp_code  = lwa_input_data-comp_code.
        lwa_header-vper_start = lwa_input_data-vper_start.
        lwa_header-vper_end   = lwa_input_data-vper_end.
        lwa_header-pmnttrms   = lwa_input_data-pmnttrms.
        lwa_header-incoterms1 = lwa_input_data-incoterms1.
        lwa_header-zzcondayqty = lwa_input_data-zzcondayqty.
        lwa_header-zzparty_agmt_id = lwa_input_data-zzparty_agmt_id.
        lwa_header-zzmsa       = lwa_input_data-zzmsa.
        lwa_header-zzoldealid  = lwa_input_data-zzoldealid.
*        lwa_header-inco2     =  ''."lwa_flheader-incoterms2.
        IF p_deal EQ 'Y'.
          lwa_header-verkf  =  lwa_input_data-zzoldealid.
        ENDIF.
        lwa_header-zzekgrp     = lwa_input_data-zzekgrp.
        lwa_header-itm_cat     = lwa_input_data-itm_cat.
        lwa_header-ass_cat     = lwa_input_data-ass_cat.
        lwa_header-currency    = lwa_input_data-currency.
        lwa_header-plant       = lwa_input_data-plant.
        lwa_header-matl_group  = lwa_input_data-matl_group.
        APPEND lwa_header TO lta_header.
        lwa_item-zzoldealid  = lwa_input_data-zzoldealid.
        lwa_item-item_no = lwa_input_data-item_no.
        lwa_item-tax_code = lwa_input_data-tax_code.
        lwa_item-itm_txt = lwa_input_data-itm_txt.
        lwa_item-trackingno  = lwa_input_data-trackingno .
        lwa_item-price_cat  = lwa_input_data-price_cat.
        lwa_item-target_qty = lwa_input_data-target_qty.
        APPEND lwa_item TO lta_item.


        lwa_services-zzoldealid  = lwa_input_data-zzoldealid.
        lwa_services-item_no = lwa_input_data-item_no.
        lwa_services-ser_num  = lwa_input_data-ser_num.
*            lwa_services-price   = lwa_input_data-price.
        MOVE lwa_input_data-price TO lwa_services-price.
        lwa_services-qty_per = lwa_input_data-qty_per.
        lwa_services-uom  = lwa_input_data-uom.
        lwa_services-price_unit = lwa_input_data-price_unit.
        lwa_services-cost_center = lwa_input_data-cost_center.
        lwa_services-glaccount = lwa_input_data-glaccount.
        lwa_services-userf2_num = lwa_input_data-userf2_num.
        lwa_services-userf1_txt = lwa_input_data-userf1_txt.

        APPEND lwa_services TO lta_services.
      ENDIF.
    ENDLOOP.
    SORT lta_header[] BY zzoldealid.
    SORT lta_item[] BY zzoldealid ASCENDING item_no ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lta_header COMPARING zzoldealid.
    DELETE ADJACENT DUPLICATES FROM lta_item COMPARING zzoldealid item_no.
*    DELETE ADJACENT DUPLICATES FROM lta_msgout COMPARING zzoldealid.
  ELSE.
    MESSAGE text-002 TYPE 'E'.
  ENDIF.
ENDFORM.                    " split_data
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid_display.
  IF lta_msgout[] IS NOT INITIAL.
    TRY.
        CALL METHOD cl_salv_table=>factory
*          EXPORTING
*            list_display = if_salv_c_bool_sap=>false
          IMPORTING
            r_salv_table = gr_table
          CHANGING
            t_table      = lta_msgout[].
      CATCH cx_salv_msg .
    ENDTRY.
  ENDIF.

  IF gr_table IS INITIAL.
    MESSAGE text-004 TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
** Get functions details
  gr_functions = gr_table->get_functions( ).

** Activate All Buttons in Tool Bar
  gr_functions->set_all( if_salv_c_bool_sap=>true ).

******* Layout Settings  *******
  CLEAR : gr_layout, gr_layout_key.
  MOVE sy-repid TO gr_layout_key-report.                        "Set Report ID as Layout Key"

  gr_layout = gr_table->get_layout( ).                          "Get Layout of Table"
  gr_layout->set_key( gr_layout_key ).                          "Set Report Id to Layout"
  gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ). "No Restriction to Save Layout"

******* Global Display Settings  *******
  CLEAR : gr_display.

  IF lta_msgout[] IS NOT INITIAL.
    CLEAR lv_title.
    MOVE text-005 TO lv_title.
  ENDIF.

  gr_display = gr_table->get_display_settings( ).               " Global Display settings"
  gr_display->set_striped_pattern( if_salv_c_bool_sap=>true ).  "Activate Strip Pattern"
  gr_display->set_list_header( lv_title ).                      "Report Header"

******* Aggregate Function Settings *******
  gr_aggr = gr_table->get_aggregations( ).                      "Get Aggregate Functions"

******* Sort Functions *******
  gr_sort = gr_table->get_sorts( ).

******* Table Selection Settings *******
  gr_select = gr_table->get_selections( ).
  IF gr_select IS NOT INITIAL.
    gr_select->set_selection_mode( if_salv_c_selection_mode=>row_column ). "Allow single row Selection"
  ENDIF.

******* Top of List settings *******
  PERFORM top_of_page CHANGING gr_content.
  gr_table->set_top_of_list( gr_content ).

******* Event Register settings *******

** Get the columns from ALV Table
  gr_columns = gr_table->get_columns( ).
  IF gr_columns IS NOT INITIAL.
    REFRESH : lt_column_ref.
    CLEAR   : ls_column_ref.
    lt_column_ref = gr_columns->get( ).

** Get columns properties
    gr_columns->set_optimize( if_salv_c_bool_sap=>true ).
    gr_columns->set_key_fixation( if_salv_c_bool_sap=>true ).

** Individual Column Properties.
    IF lta_msgout[] IS NOT INITIAL.
      PERFORM column_settings.
    ENDIF.

  ENDIF.
  CALL METHOD gr_table->display.
ENDFORM.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GR_CONTENT  text
*----------------------------------------------------------------------*
FORM top_of_page  CHANGING lr_content TYPE REF TO cl_salv_form_element.
  DATA : lr_grid TYPE REF TO cl_salv_form_layout_grid,
           lr_text  TYPE REF TO cl_salv_form_text,
           lr_label TYPE REF TO cl_salv_form_label,
           lr_head  TYPE string.

  IF lta_msgout IS NOT INITIAL.
    MOVE text-005 TO lr_head.
    CREATE OBJECT lr_grid.
    DESCRIBE TABLE lta_msgout LINES lv_cnt.
  ENDIF.

** Header of Top of Page **
  lr_grid->create_header_information( row     = 1
                                      column  = 1
                                      text    = lr_head
                                      tooltip = lr_head ).
** Add Row **
  lr_grid->add_row( ).

** Add Label in Grid **
  lr_label = lr_grid->create_label( row = 2
                                    column = 1
                                    text = text-006
                                    tooltip = text-006 ).
** Add Text in The Grid **
  lr_text = lr_grid->create_text( row = 2
                                  column = 2
                                  text = lv_cnt
                                  tooltip = lv_cnt ).
** Set Label and Text Link **
  lr_label->set_label_for( lr_text ).

** Move lr_grid to lr_content **
  lr_content = lr_grid.
ENDFORM.                    " TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  COLUMN_SETTINGS_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM column_settings.
  LOOP AT lt_column_ref INTO ls_column_ref.
    TRY.
        gr_column ?= gr_columns->get_column( ls_column_ref-columnname ).
      CATCH cx_salv_not_found.
    ENDTRY.

    IF gr_column IS NOT INITIAL.
      IF gr_column->get_columnname( ) = 'ZZOLDEALID'.
        gr_column->set_short_text( 'DEAL ID').
        gr_column->set_medium_text( 'DEAL ID').
        gr_column->set_long_text( 'OPEN LINK DEAL ID').
      ENDIF.
      IF gr_column->get_columnname( ) = 'ERRMSG'.
        gr_column->set_short_text( 'MESSAGE').
        gr_column->set_medium_text( 'MESSAGE').
        gr_column->set_long_text( 'MESSAGE').
      ENDIF.
      IF gr_column->get_columnname( ) = 'ERRTYP'.
        gr_column->set_short_text( 'MSG_TYPE').
        gr_column->set_medium_text( 'MESSAGE TYPE').
        gr_column->set_long_text( 'MESSAGE TYPE').
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " COLUMN_SETTINGS_PO
*&---------------------------------------------------------------------*
*&      Form  CREATE_PUR_CONTRACT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_pur_contract .


  DATA: opt TYPE ctu_params,
        lv_zzcondayqty TYPE char30.

  opt-dismode = 'N'.
  opt-updmode = 'A'.
  opt-defsize = 'X'.


  LOOP AT lta_header INTO lwa_header.
    CLEAR:lv_zzcondayqty.

***Build Header BDC
    PERFORM bdc_dynpro      USING 'SAPMM06E' '0200'.
    PERFORM bdc_field       USING 'BDC_CURSOR' 'EKKO-EKGRP'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
    PERFORM bdc_field       USING 'EKKO-LIFNR'  lwa_header-vendor. " '18418'. " Vendor
    PERFORM bdc_field       USING 'RM06E-EVART' lwa_header-cont_typ. "'ZK'."lwa_contract-doc_type. "Document Type
    CLEAR: lv_date.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = lwa_header-agre_date
      IMPORTING
        output = lv_date.

    PERFORM bdc_field       USING 'RM06E-VEDAT' lv_date."'12.05.2021'."lv_date. " Contract Creation date
    PERFORM bdc_field       USING 'EKKO-EKORG' lwa_header-purch_org. "'GASA'."lwa_contract-purch_org. " Purchase Org
    PERFORM bdc_field       USING 'EKKO-EKGRP' lwa_header-pur_group. "'MCN'."lwa_contract-pur_group. " Purchase Grp
    PERFORM bdc_field       USING 'RM06E-EPSTP' lwa_header-itm_cat. " 'D'. " ITem Cat
    PERFORM bdc_field       USING 'RM06E-KNTTP' lwa_header-ass_cat. "'K'. " ACASS
    PERFORM bdc_field       USING 'RM06E-WERKS' lwa_header-plant. "'GEGD'. " Plant
    PERFORM bdc_field       USING 'RM06E-LGORT' 'A001'. " Storage Loc
    PERFORM bdc_field       USING 'RM06E-MATKL' lwa_header-matl_group."'2001'. " Material Group

    PERFORM bdc_dynpro      USING 'SAPMM06E' '0201'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'EKKO-KTWRT'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
    CLEAR lv_start.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = lwa_header-vper_start
      IMPORTING
        output = lv_start.

    PERFORM bdc_field       USING 'EKKO-KDATB' lv_start. " Contract Start
    CLEAR lv_end.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = lwa_header-vper_end
      IMPORTING
        output = lv_end.

    PERFORM bdc_field       USING 'EKKO-KDATE' lv_end. " Contract end
    PERFORM bdc_field       USING 'EKKO-EKGRP' lwa_header-pur_group. "'MCN'.
*PERFORM bdc_field       USING 'EKKO-PINCR' '1'.
*PERFORM bdc_field       USING 'EKKO-SPRAS' 'EN'.
    PERFORM bdc_field       USING 'EKKO-ZTERM' lwa_header-pmnttrms. "'20TH'.
    PERFORM bdc_field       USING 'EKKO-WAERS' lwa_header-currency. "'CAD'.
*PERFORM bdc_field       USING 'EKKO-WKURS' '1'.
    PERFORM bdc_field       USING 'EKKO-INCO1' lwa_header-incoterms1. "'GTR'.
    PERFORM bdc_field       USING 'EKKO-INCO2' 'Transport'.
    IF p_deal EQ 'Y'.
      PERFORM bdc_field       USING 'EKKO-VERKF' lwa_header-verkf.     "ZZDEAL ID
    ENDIF.

    PERFORM bdc_field       USING 'BDC_SUBSCR' 'SAPLXM06                                0201CUSTSCR1'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'EKKO-ZZEKGRP'.

    IF ( lwa_header-currency EQ 'CAD' ).
      CONCATENATE  lwa_header-zzcondayqty 'GJ/Day' INTO lv_zzcondayqty SEPARATED BY space.
    ELSEIF ( lwa_header-currency EQ 'USD' ).
      CONCATENATE  lwa_header-zzcondayqty 'MMB/Day' INTO lv_zzcondayqty SEPARATED BY space.
    ENDIF.

    PERFORM bdc_field       USING 'EKKO-ZZCONDAYQTY' lv_zzcondayqty.             "'1000'.  "Condition Quantity
    PERFORM bdc_field       USING 'ZMMT_MASTAGREE-ZZMSA' lwa_header-zzmsa. "'CGY_PARK LOAN(C)_556'."
    PERFORM bdc_field       USING 'EKKO-ZZEKGRP' lwa_header-zzekgrp. "'G01'."l
    PERFORM bdc_field       USING 'EKKO-KDATE' lv_end. " Contract end
    PERFORM bdc_field      USING 'BDC_SUBSCR' 'SAPMM06E                                0250ISSCR1'.


**********************************************************************************
***************************ITEM SCREEN********************************************
    CLEAR: lv_itmcnt.
    LOOP AT lta_item INTO lwa_item WHERE zzoldealid = lwa_header-zzoldealid.
      lv_itmcnt = lv_itmcnt + 1.
      IF lv_itmcnt EQ 15.
        lv_itmpage = lv_itmcnt - 1.
        lv_itmcnt = 2.
      ENDIF.
***Build Item BDC
      CLEAR lv_prc.
      lv_prc =  lwa_item-target_qty.
      CONDENSE lv_prc.

      PERFORM bdc_dynpro      USING 'SAPMM06E' '0220'.
      PERFORM bdc_field       USING 'RM06E-EBELP'
                                     lv_itmpage.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '/00'.

      PERFORM bdc_dynpro      USING 'SAPMM06E' '0220'.
      CLEAR: lv_text.
      CONCATENATE 'RM06E-EVRTP(' lv_itmcnt ')' INTO lv_text.

      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    lv_text.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=DETA'.
      CLEAR: lv_text.
      CONCATENATE 'EKPO-TXZ01(' lv_itmcnt ')' INTO lv_text.

      PERFORM bdc_field       USING lv_text lwa_item-itm_txt. "'FTDEM_TCPL-60075'."lwa_contract-service_txt. "Material text

**  Tax code related
      PERFORM bdc_dynpro      USING 'SAPMM06E' '0211'.
      PERFORM bdc_field       USING 'BDC_CURSOR' 'EKPO-MWSKZ'.
      PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
      PERFORM bdc_field       USING 'EKPO-BEDNR' lwa_item-trackingno. "'202105'.
      PERFORM bdc_field       USING 'EKPO-PRSDR' 'X'.
      PERFORM bdc_field       USING 'EKPO-UEBTK' 'X'.
      PERFORM bdc_field       USING 'EKPO-MWSKZ' lwa_item-tax_code. "'T2'. " Tax code defaulted
**  End of Tax code


*****************************************************************************************************************
*************************SERVICE SCREENS*************************************************************************
**Check services count.

      CLEAR: lv_count.
      LOOP AT lta_services INTO lwa_services WHERE zzoldealid = lwa_item-zzoldealid
                                                AND item_no = lwa_item-item_no.
        CLEAR lv_page.
        lv_count = lv_count + 1.
        CLEAR: lv_page.
        IF lv_count = 15.
          lv_pagedown = ( lv_count * 10 ) - 10.
          lv_count = 2.
          lv_page = 'X'.

        ENDIF.


**PK
        PERFORM bdc_dynpro      USING 'SAPLMLSP' '0201'.
        PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                      lv_pagedown. "'10'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '/00'.
***EndPK

        CLEAR: lv_text.
        CONCATENATE 'ESLL-EXTROW(' lv_count ')' INTO lv_text.

        PERFORM bdc_dynpro      USING 'SAPLMLSP' '0201'.

*        IF lv_page IS NOT INITIAL.
*          PERFORM bdc_field     USING 'BDC_OKCODE' '=P+'.
*          PERFORM bdc_dynpro      USING 'SAPLMLSP' '0201'.
*        ELSE.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=CONT'.  " '=ESB'.
*        ENDIF.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      lv_text.

*        PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                      lv_pagedown.
        CLEAR: lv_text.
        CONCATENATE 'ESLL-SRVPOS(' lv_count ')' INTO lv_text.

        PERFORM bdc_field       USING lv_text lwa_services-ser_num . "'FTDEM_TCPL-60075'."lwa_contract-service_txt. "Service text

        CLEAR lv_qty.
        lv_qty =   lwa_services-qty_per. "'1'."lwa_contract-serv_qty.
        CONDENSE lv_qty.
        CLEAR: lv_text.
        CONCATENATE 'ESLL-MENGE(' lv_count ')' INTO lv_text.


        PERFORM bdc_field       USING lv_text lv_qty. "Quantity

        CLEAR: lv_text.
        CONCATENATE 'ESLL-MEINS(' lv_count ')' INTO lv_text.

        PERFORM bdc_field       USING lv_text lwa_services-uom. "'EA'."lwa_contract-uom. " Unit Of Mesure

        CLEAR lv_prc.
        WRITE lwa_services-price TO lv_prc . "'1'."lwa_contract-gross_price.
        CONDENSE lv_prc.
        CLEAR: lv_text.
        CONCATENATE 'ESLL-TBTWR(' lv_count ')' INTO lv_text.

        PERFORM bdc_field       USING lv_text lv_prc. "Gross price
        PERFORM bdc_field       USING 'RM11P-SELKZ(01)' 'X'.

        PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'ESKN-SAKTO(01)'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '/00'.
        PERFORM bdc_field       USING 'VRTKZ1'
                                      'X'.
        PERFORM bdc_field       USING 'RM11K-MKNTM(01)'
                                      '1'.
        PERFORM bdc_field       USING 'ESKN-KOSTL(01)'
                                      lwa_services-cost_center. "'77664'.
        PERFORM bdc_field       USING 'ESKN-SAKTO(01)'
                                      lwa_services-glaccount. "'302010'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.

        PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                     'ESKN-SAKTO(01)'.

        PERFORM bdc_field       USING 'VRTKZ1'
                                      'X'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=BACK'.

****PK1
*IF lv_page  IS INITIAL.
***21_09_2021
        PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                     'ESKN-SAKTO(01)'.

        PERFORM bdc_field       USING 'VRTKZ1'
                                      'X'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=BACK'.
***21_09_2021
*ENDIF.
**PK1 end
**PK
        PERFORM bdc_dynpro      USING 'SAPLMLSP' '0201'.
        PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                      lv_pagedown. "'10'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '/00'.
***EndPK
        PERFORM bdc_dynpro      USING 'SAPLMLSP' '0201'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=BSEL'.
        PERFORM bdc_field      USING 'BDC_SUBSCR' 'SAPLMLSP                                0400SERVICE'.
        CLEAR: lv_text.
        CONCATENATE 'ESLL-MENGE(' lv_count ')' INTO lv_text.

        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      lv_text.
*        PERFORM bdc_field       USING 'RM11P-NEW_ROW'
*                                      lv_pagedown. "'10'.
**NEW1
        PERFORM bdc_dynpro      USING 'SAPLMLSP' '0220'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=OK'.
*PERFORM bdc_field       USING 'ESLL-EXTROW'
*                              '10'.
****21_09_2021
*        PERFORM bdc_field       USING 'BDC_SUBSCR'
*                                      'SAPLMLSP                                0292SUSCREEN_DET1'.
*        PERFORM bdc_field       USING 'BDC_SUBSCR'
*                                      'SAPLMLSP                                0294SUSCREEN_DET2'.
****21_09_2021
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'ESLL-USERF1_TXT'.
        PERFORM bdc_field       USING 'ESLL-USERF1_NUM'
                                      ' '.

        CLEAR lv_user2.
        WRITE lwa_services-userf2_num TO lv_user2 . "'1'."lwa_contract-gross_price.
        CONDENSE lv_user2.
        PERFORM bdc_field       USING 'ESLL-USERF2_NUM'
                                      lv_user2. "'2'.
        PERFORM bdc_field       USING 'ESLL-USERF1_TXT'
                                      lwa_services-userf1_txt. "'3'.
****21_09_2021
*        PERFORM bdc_field       USING 'BDC_SUBSCR'
*                                      'SAPLMLSP                                0296SUSCREEN_DET3'.
*        PERFORM bdc_field       USING 'BDC_SUBSCR'
*                                      'SAPLMLSP                                0291SUSCREEN_DET4'.
*        PERFORM bdc_field       USING 'BDC_SUBSCR'
*                                      'SAPLMLSP                                0296SUSCREEN_DET5'.
*        PERFORM bdc_field       USING 'BDC_SUBSCR'
*                                      'SAPLMLSP                                0291SUSCREEN_DET6'.
*        PERFORM bdc_field       USING 'RM11P-NORMALPOS'
*                                      'X'.
*        PERFORM bdc_field       USING 'BDC_SUBSCR'
*                                      'SAPLMLSP                                0290SUSCREEN_DET'.
*
**          PERFORM bdc_field       USING 'ESLL-MATKL'  lv_matkl.
**                                      lwa_header-matl_group. "'2001'.
*
*        PERFORM bdc_dynpro      USING 'SAPLMLSK' '0200'.
*        PERFORM bdc_field       USING 'BDC_CURSOR'
*                                     'ESKN-SAKTO(01)'.
*
*        PERFORM bdc_field       USING 'VRTKZ1'
*                                      'X'.

*        PERFORM bdc_field       USING 'BDC_OKCODE'
*                                    '=BACK'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=OK'.

****21_09_2021


*      IF lv_page IS NOT INITIAL.
*        PERFORM bdc_dynpro      USING 'SAPLMLSP' '0201'.
*        PERFORM bdc_field     USING 'BDC_OKCODE' '=P+'.
**        PERFORM bdc_dynpro      USING 'SAPLMLSP' '0201'.
*      ENDIF.

      ENDLOOP.  " End of Service

**Last step of Service
      PERFORM bdc_dynpro      USING 'SAPLMLSP' '0201'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ESB'.
      PERFORM bdc_field      USING 'BDC_SUBSCR' 'SAPLMLSP                                0400SERVICE'.
      CLEAR: lv_text.
      CONCATENATE 'ESLL-KTEXT1(' lv_count ')' INTO lv_text.

      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    lv_text.
      PERFORM bdc_field       USING 'RM11P-NEW_ROW'
                                    '10'.

***ENd of Services

      PERFORM bdc_dynpro      USING 'SAPMM06E' '0220'.
      CLEAR: lv_text.
      CONCATENATE 'RM06E-EVRTP(' lv_itmcnt ')' INTO lv_text.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    lv_text. "'RM06E-EVRTP(01)'.
      PERFORM bdc_field       USING 'RM06E-EBELP'
                                    lv_itmpage.
      CLEAR: lv_text.
      CONCATENATE 'RM06E-TCSELFLAG(' lv_itmcnt ')' INTO lv_text.
      PERFORM bdc_field       USING lv_text                "'RM06E-TCSELFLAG(01)'
                                    'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=DETZ'.

      PERFORM bdc_dynpro      USING 'SAPMM06E' '0212'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RM06E-MEPTX'.
      PERFORM bdc_field       USING 'EKPO-MEPRF'
                                    lwa_item-price_cat. "'5'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=BACK'.


****End of Item
    ENDLOOP.   " End of Item details
****Save
    PERFORM bdc_dynpro      USING 'SAPMM06E' '0220'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                               'EKPO-MATKL(01)'.
    PERFORM bdc_field       USING 'RM06E-EBELP'
                                   '1'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
    DATA: lv_ebeln TYPE ebeln.
*          lv_zzoldealid_old TYPE zoldealid.
    IF git_bdcdata[] IS NOT INITIAL.
      CLEAR: git_messtab2, gwa_messtab.
** Call BDC transaction for OLA create in ECC
      CALL TRANSACTION 'ME31K' USING git_bdcdata
*                     MODE   'N'
*                     UPDATE 'A'
                       OPTIONS FROM opt
                       MESSAGES INTO git_messtab2.
      REFRESH git_bdcdata.
      LOOP AT git_messtab2 INTO gwa_messtab.

        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = gwa_messtab-msgid
            lang      = '-D'
            no        = gwa_messtab-msgnr
            v1        = gwa_messtab-msgv1
            v2        = gwa_messtab-msgv2
            v3        = gwa_messtab-msgv3
            v4        = gwa_messtab-msgv4
          IMPORTING
            msg       = lwa_msgout-errmsg
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc EQ 0.
          lwa_msgout-zzoldealid = lwa_header-zzoldealid.
          lwa_msgout-errtyp = gwa_messtab-msgtyp.
          lv_ebeln = gwa_messtab-msgv2.
*        lwa_msgout-errmsg = lv_output.
          APPEND lwa_msgout TO lta_msgout.
        ENDIF.
      ENDLOOP.
      IF p_deal  EQ 'N'.
***Update Deal id using FM
        PERFORM update_zzoldealid USING lv_ebeln
                                        lwa_header-zzoldealid.

      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CREATE_PUR_CONTRACT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZZOLDEALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_EBELN  text
*      -->P_LWA_HEADER_ZZOLDEALID  text
*----------------------------------------------------------------------*
FORM update_zzoldealid  USING    p_lv_ebeln
                                 p_lwa_header_zzoldealid.

  DATA: lta_oplk_dealid TYPE STANDARD TABLE OF zmms_oplk_dealid,
        lta_oplk_nodeal TYPE STANDARD TABLE OF zmms_oplk_dealid_return,
        lwa_oplk_dealid TYPE zmms_oplk_dealid.

  CALL FUNCTION 'ENQUEUE_EMEKKOE'
    EXPORTING
      mode_ekko      = 'E'
*     MODE_EKPO      = 'E'
      mandt          = sy-mandt
      ebeln          = p_lv_ebeln
      _wait          = '3'
*     _COLLECT       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    CALL FUNCTION 'DEQUEUE_EMEKKOE'
      EXPORTING
        mode_ekko = 'E'
*       MODE_EKPO = 'E'
        mandt     = sy-mandt
        ebeln     = p_lv_ebeln.
  ELSE.
    lwa_oplk_dealid-ebeln = p_lv_ebeln.
    lwa_oplk_dealid-zzoldealid = p_lwa_header_zzoldealid.
    APPEND  lwa_oplk_dealid TO lta_oplk_dealid.

    CALL FUNCTION 'ZLMMI039_OPLK_DEALID'
      TABLES
        tab_dealid = lta_oplk_dealid
        tab_nodeal = lta_oplk_nodeal.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    REFRESH: lta_oplk_dealid.
  ENDIF.
ENDFORM.                    " UPDATE_ZZOLDEALID
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR gwa_bdcdata.
  gwa_bdcdata-program  = program.
  gwa_bdcdata-dynpro   = dynpro.
  gwa_bdcdata-dynbegin = 'X'.
  APPEND gwa_bdcdata TO git_bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
*  IF fval <> nodata.
  CLEAR gwa_bdcdata.
  gwa_bdcdata-fnam = fnam.
  gwa_bdcdata-fval = fval.
  APPEND gwa_bdcdata TO git_bdcdata.
*  ENDIF.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  CHECK_OLDEALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_oldealid .

  IF lta_dealid IS NOT INITIAL.
    SELECT ebeln zzoldealid verkf INTO TABLE lta_sapdeals
      FROM ekko FOR ALL ENTRIES IN lta_dealid
      WHERE zzoldealid EQ lta_dealid-zzoldealid.        "#EC CI_NOFIELD
  ENDIF.
  SORT lta_sapdeals.
ENDFORM.                    " CHECK_OLDEALID
