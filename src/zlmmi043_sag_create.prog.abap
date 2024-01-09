*&---------------------------------------------------------------------*
*& Report  ZLMMI043_SAG_CREATE
*&---------------------------------------------------------------------*
************************************************************************
*                            COGS                                      *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI043_SAG_CREATE                           *
*& Author             :  Jaydeep Waychal/Prashan Durbhaka              *
*& Creation Date      :  May 03, 2021                                  *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program to Create Supply deal using Custom FM *
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

REPORT  zlmmi043_sag_create NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: ekko,ekpo,zmmt_mastagree,konv.

* Types Declarations*
TYPES : BEGIN OF ty_upload,
           string TYPE string,
        END OF ty_upload,

      BEGIN OF ty_input_data,
            doc_type    TYPE  esart,
            plant       TYPE  werks_d,
            stge_loc    TYPE  lgort_d,
            comp_code   TYPE  bukrs,
            lifnr     TYPE  elifn,
            pmnttrms    TYPE  dzterm,
            purch_org   TYPE  ekorg,
            pur_group   TYPE  bkgrp,
            vper_end    TYPE  kdate,
            vper_start  TYPE  kdatb,
            doc_date    TYPE  vedat,
            incoterms1  TYPE  inco1,
            incoterms2  TYPE  inco2,
            zzoldealid  TYPE  zoldealid,
            zzekgrp     TYPE  ztrbuy,
            zzmsa       TYPE  zzmsa,
            zzparty_agmt_id TYPE  zzparty,
            zztrloc1    TYPE  ztrloc,
            zztrloc2    TYPE  ztrloc2,
            zztrloc3    TYPE  ztrloc3,
            zztrloc4    TYPE  ztrloc4,
            zzparty     TYPE  zparty,
            zzcondayqty TYPE  z_condayqty,
            zzconprice  TYPE  z_conprice,
            currency    TYPE waers,
            item_no     TYPE  ebelp,           "Items
            target_qty  TYPE  char13,
            po_unit     TYPE  bstme,
            trackingno  TYPE  bednr,
            matl_group  TYPE  matkl,
            tax_code    TYPE  mwskz,
            material    TYPE  matnr,
            cond_type   TYPE kscha,
            cond_value  TYPE d, "bapikbetr1,
     END OF ty_input_data,

BEGIN OF ty_flheader,
            lifnr       TYPE  lifnr,
            plant       TYPE  werks_d,
            stge_loc    TYPE  lgort_d,
            doc_type    TYPE  evart,
            comp_code   TYPE  bukrs,
            pmnttrms    TYPE  dzterm,
            purch_org   TYPE  ekorg,
            pur_group   TYPE  bkgrp,
            vper_end    TYPE  kdate,
            vper_start  TYPE  kdatb,
            doc_date    TYPE  vedat,
            incoterms1  TYPE  inco1,
            incoterms2  TYPE  inco2,
            verkf       TYPE verkf,             "JWAYCHAL
            zzoldealid  TYPE  zoldealid,
            zzekgrp     TYPE  ztrbuy,
            zzmsa       TYPE  zzmsa,
            zzparty_agmt_id TYPE  zzparty,
            zztrloc1    TYPE  ztrloc,
            zztrloc2    TYPE  ztrloc2,
            zztrloc3    TYPE  ztrloc3,
            zztrloc4    TYPE  ztrloc4,
            zzparty     TYPE  zparty,
            zzcondayqty TYPE  z_condayqty,
*            zzconprice  TYPE z_conprice,
            zzconprice(16)  TYPE c,
            currency    TYPE waers,
  END OF ty_flheader,

  BEGIN OF ty_flitem,
            zzoldealid  TYPE  zoldealid,
            item_no     TYPE  ebelp,
            target_qty  TYPE  ktmng,
            po_unit     TYPE  bstme,
            trackingno  TYPE  bednr,
            matl_group  TYPE  matkl,
            tax_code    TYPE  mwskz,
            material    TYPE  matnr,
            sakto(10)   TYPE c,
            kostl       TYPE kostl,
  END OF ty_flitem,

  BEGIN OF ty_flitemcon,
            zzoldealid  TYPE  zoldealid,
            item_con    TYPE  ebelp,
            cond_type TYPE kscha,
            cond_value TYPE char11,
  END OF ty_flitemcon.

TYPES:  BEGIN OF ty_final,
            doc_type    TYPE  esart,
            comp_code   TYPE  bukrs,
            lifnr     TYPE  elifn,
            pmnttrms    TYPE  dzterm,
            purch_org   TYPE  ekorg,
            pur_group   TYPE  bkgrp,
            vper_end    TYPE  kdate,
            vper_start  TYPE  kdatb,
            doc_date    TYPE  vedat,
            incoterms1  TYPE  inco1,
            incoterms2  TYPE  inco2,
            verkf       TYPE  verkf,             "JWAYCHAL
            zzoldealid  TYPE  zoldealid,
            zzekgrp     TYPE  ztrbuy,
            zzmsa       TYPE  zzmsa,
            zzparty_agmt_id TYPE  zzparty,
            zztrloc1    TYPE  ztrloc,
            zztrloc2    TYPE  ztrloc2,
            zztrloc3    TYPE  ztrloc3,
            zztrloc4    TYPE  ztrloc4,
            zzparty     TYPE  zparty,
            zzcondayqty TYPE  z_condayqty,
            item_no     TYPE  ebelp,
            target_qty  TYPE  ktmng,
            po_unit     TYPE  bstme,
            plant       TYPE  werks_d,
            stge_loc    TYPE  lgort_d,
            trackingno  TYPE  bednr,
            matl_group  TYPE  matkl,
            tax_code    TYPE  mwskz,
            gr_ind      TYPE  wepos,
            ir_ind      TYPE  repos,
            gr_basediv  TYPE  webre,
            material    TYPE  matnr,
            item_con    TYPE  ebelp,
            serial_id   TYPE  meout_id_konh,
            cond_count  TYPE  kopos,
            cond_type   TYPE kscha,
            cond_value  TYPE char11,
            currency      TYPE waers,
            currency_iso  TYPE waers_iso,
            cond_p_unt    TYPE  char11, "kpein,
            cond_unit_iso TYPE kvmei_iso,
            errmsg(2000)  TYPE c,
        END OF ty_final,

        BEGIN OF ty_msgout,
            zzoldealid  TYPE  zoldealid,
            errmsg(2000) TYPE c,
            errtyp(1)    TYPE c,  "S - success , E-Error
        END OF ty_msgout.
*validation
TYPES: BEGIN OF ty_agmid,
            zzparty_agmt_id TYPE zzparty,
            zzcpid          TYPE zcpid,
       END OF ty_agmid,

       BEGIN OF ty_dealid,
            zzoldealid  TYPE  zoldealid,
       END OF ty_dealid,

       BEGIN OF ty_sapdeals,
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
      lta_flheader     TYPE STANDARD TABLE OF ty_flheader,
      lta_flitem       TYPE STANDARD TABLE OF ty_flitem,
      lta_flitemcon    TYPE STANDARD TABLE OF ty_flitemcon,
      lta_dealid       TYPE STANDARD TABLE OF ty_dealid,
      lta_sapdeals     TYPE STANDARD TABLE OF ty_sapdeals,
      lta_header       TYPE STANDARD TABLE OF zmm_sagheader,
      lta_itemsag      TYPE STANDARD TABLE OF zmm_itemsag,
      lta_pricond      TYPE STANDARD TABLE OF zmm_pricecondsag,
      lta_maintain     TYPE STANDARD TABLE OF zmm_sagmaintain,
      lta_return       TYPE STANDARD TABLE OF zmm_returnmsg,
      lta_output       TYPE STANDARD TABLE OF zmm_returnmsg.

* Work Area Declarations *
DATA: lwa_upload       TYPE ty_upload,
      lwa_input_data   TYPE ty_input_data,
      lwa_final        TYPE ty_final,
      lwa_msgout       TYPE ty_msgout,
      lwa_agmtid       TYPE ty_agmid,
      lwa_flheader     TYPE ty_flheader,
      lwa_flitem       TYPE ty_flitem,
      lwa_flitemcon    TYPE ty_flitemcon,
      lwa_dealid       TYPE ty_dealid,
      lwa_sapdeals     TYPE ty_sapdeals,
      lwa_header       TYPE zmm_sagheader_dm,
      lwa_itemsag      TYPE zmm_itemsag,
      lwa_pricond      TYPE zmm_pricecondsag,
      lwa_maintain     TYPE zmm_sagmaintain,
      lwa_return       TYPE zmm_returnmsg.

* Variable Declarations *
DATA: lv_sep(1)      TYPE c VALUE ',',
      lv_tstflg      TYPE c,
      lv_errtype     TYPE c,
      lv_cnt(5)      TYPE n,
      lv_message(2000) TYPE c,
      lv_path        TYPE btch0000-text80.

* Constant Declarations *
CONSTANTS: lc_lpein        TYPE c VALUE 'M',
           lc_doccat       TYPE c VALUE 'L',
           lc_gasply       TYPE c VALUE 'G',
           lc_gsstorge     TYPE c VALUE 'S',
           lc_status       TYPE c VALUE 'A',
           lc_telphone(12) TYPE c VALUE 'TRADINGBUYER'.

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
PARAMETER: p_sakto(10) TYPE c.
PARAMETER: p_kostl TYPE kostl.
PARAMETER: p_matnr  TYPE matnr_d.
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
***Check Deal status
  PERFORM upload_customdata.
  IF lta_msgout[] IS NOT INITIAL.
    PERFORM alv_grid_display.
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
                              lwa_input_data-doc_type
                              lwa_input_data-plant
                              lwa_input_data-stge_loc
                              lwa_input_data-comp_code
                              lwa_input_data-lifnr
                              lwa_input_data-pmnttrms
                              lwa_input_data-purch_org
                              lwa_input_data-pur_group
                              lwa_input_data-vper_end
                              lwa_input_data-vper_start
                              lwa_input_data-doc_date
                              lwa_input_data-incoterms1
                              lwa_input_data-incoterms2
                              lwa_input_data-zzoldealid
                              lwa_input_data-zzekgrp
                              lwa_input_data-zzmsa
                              lwa_input_data-zzparty_agmt_id
                              lwa_input_data-zztrloc1
                              lwa_input_data-zztrloc2
                              lwa_input_data-zztrloc3
                              lwa_input_data-zztrloc4
                              lwa_input_data-zzparty
                              lwa_input_data-zzcondayqty
                              lwa_input_data-zzconprice
                              lwa_input_data-currency
                              lwa_input_data-item_no          " Item
                              lwa_input_data-target_qty
                              lwa_input_data-po_unit
                              lwa_input_data-trackingno
                              lwa_input_data-matl_group
                              lwa_input_data-tax_code
                              lwa_input_data-material
                              lwa_input_data-cond_type            " Conditions
                              lwa_input_data-cond_value.
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
*Ignore the deals that have already been created in sap
      READ TABLE lta_sapdeals INTO lwa_sapdeals WITH KEY zzoldealid = lwa_input_data-zzoldealid.
      IF sy-subrc EQ 0.
        CONCATENATE text-004 lwa_sapdeals-ebeln INTO lv_message.
        lwa_msgout-zzoldealid = lwa_input_data-zzoldealid.
        lwa_msgout-errmsg = lv_message.
        lwa_msgout-errtyp = 'E'.
        APPEND lwa_msgout TO lta_msgout.
        CLEAR: lv_message,
               lwa_msgout.
      ELSE.
        lwa_flheader-doc_type   = lwa_input_data-doc_type.
        lwa_flheader-plant      = lwa_input_data-plant.
        lwa_flheader-stge_loc   = lwa_input_data-stge_loc.
        lwa_flheader-comp_code  = lwa_input_data-comp_code.
        lwa_flheader-lifnr      = lwa_input_data-lifnr.
        lwa_flheader-pmnttrms   = lwa_input_data-pmnttrms.
        lwa_flheader-purch_org  = lwa_input_data-purch_org.
        lwa_flheader-pur_group  = lwa_input_data-pur_group.
        lwa_flheader-vper_end   = lwa_input_data-vper_end.
        lwa_flheader-vper_start = lwa_input_data-vper_start.
        lwa_flheader-doc_date   = lwa_input_data-doc_date.
        lwa_flheader-incoterms1 = lwa_input_data-incoterms1.
        lwa_flheader-incoterms2 = lwa_input_data-incoterms2.
        lwa_flheader-zzoldealid = lwa_input_data-zzoldealid.
        lwa_flheader-zzekgrp    = lwa_input_data-zzekgrp.
        lwa_flheader-zzmsa      = lwa_input_data-zzmsa.
        lwa_flheader-zzparty_agmt_id = lwa_input_data-zzparty_agmt_id.
        lwa_flheader-zztrloc1        = lwa_input_data-zztrloc1.
        lwa_flheader-zztrloc2        = lwa_input_data-zztrloc2.
        lwa_flheader-zztrloc3        = lwa_input_data-zztrloc3.
        lwa_flheader-zztrloc4        = lwa_input_data-zztrloc4.
        lwa_flheader-zzparty         = lwa_input_data-zzparty.
        lwa_flheader-zzcondayqty     = lwa_input_data-zzcondayqty.
        lwa_flheader-zzconprice     = lwa_input_data-zzconprice.
        lwa_flheader-currency   = lwa_input_data-currency.
        APPEND lwa_flheader TO lta_flheader.

        lwa_flitem-zzoldealid =   lwa_input_data-zzoldealid.
        lwa_flitem-item_no    =   lwa_input_data-item_no.
        lwa_flitem-target_qty =   lwa_input_data-target_qty.
        lwa_flitem-po_unit    =   lwa_input_data-po_unit.
        lwa_flitem-trackingno =   lwa_input_data-trackingno.
        lwa_flitem-matl_group =   lwa_input_data-matl_group.
        lwa_flitem-tax_code   =   lwa_input_data-tax_code.
        IF lwa_input_data-material NE 'NATGAS'.
          lwa_flitem-material   =   p_matnr.
          lwa_flitem-sakto = p_sakto.
          lwa_flitem-kostl = p_kostl.
        ELSE.
          lwa_flitem-material   =   lwa_input_data-material.
        ENDIF.
        APPEND lwa_flitem TO lta_flitem.

        lwa_flitemcon-zzoldealid = lwa_input_data-zzoldealid.
        lwa_flitemcon-item_con   = lwa_input_data-item_no.       "Pass item number from Item
        lwa_flitemcon-cond_type  = lwa_input_data-cond_type.
        lwa_flitemcon-cond_value  = lwa_input_data-cond_value.
        APPEND lwa_flitemcon TO lta_flitemcon.
      ENDIF.
    ENDLOOP.
    SORT lta_flheader BY zzoldealid.
*    SORT lta_flitem ASCENDING BY item_no zzoldealid.                            .
    DELETE ADJACENT DUPLICATES FROM lta_flheader COMPARING zzoldealid.
*    DELETE ADJACENT DUPLICATES FROM lta_flitem COMPARING zzoldealid item_no.
  ELSE.
    MESSAGE text-002 TYPE 'E'.
  ENDIF.
ENDFORM.                    " split_data

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_CUSTOMDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_customdata.
*  Define local data
  DATA: lv_ebeln    TYPE ebeln,                             "#EC NEEDED
        lwa_ekko    TYPE ekko,
        lv_lines TYPE i,
        lv_zzoldealid_old TYPE zoldealid,
        lv_flag TYPE c,
        lv_count TYPE i.
  DATA: lv_num TYPE syst-subrc,
        lv_msg TYPE char100.

  IF lta_flheader IS NOT INITIAL.
    DESCRIBE TABLE lta_flheader LINES lv_lines.
****Header***
    LOOP AT lta_flheader INTO lwa_flheader.
      CLEAR:lv_ebeln.
**Logic for 1 SA
**CApture line count for processing.
      lv_count = sy-tabix.
**Set variable for multi processing.
      IF sy-tabix EQ 1.
        lv_zzoldealid_old = lwa_flheader-zzoldealid.
      ENDIF.
**Trigger FM in case of multiple SA'a.
      IF lwa_flheader-zzoldealid NE lv_zzoldealid_old.
*        lv_zzoldealid_old = lwa_flheader-zzoldealid.
***Call Function Module
        CALL FUNCTION 'ZMM_SAG_CREATE_DM'
          EXPORTING
            lv_mode        = 'N'
            lv_update      = 'S'
            header         = lwa_header
          IMPORTING
            lv_subrc       = lv_num
            lv_output      = lv_msg
          TABLES
            it_itemsag     = lta_itemsag
            it_pricondsag  = lta_pricond
            it_sagmaintain = lta_maintain
            it_return      = lta_return.

        IF lta_return IS NOT INITIAL.
          LOOP AT lta_return INTO lwa_return.
*            lwa_msgout-zzoldealid = lwa_flheader-zzoldealid.
            lwa_msgout-zzoldealid = lv_zzoldealid_old.
            lwa_msgout-errmsg = lwa_return-errormessage.
            lwa_msgout-errtyp = lwa_return-errortype.
            APPEND lwa_msgout TO lta_msgout.
          ENDLOOP.
          CLEAR : lv_ebeln.
          IF p_deal EQ 'N'.
            IF lv_msg IS NOT INITIAL.
              lv_msg =  reverse( lv_msg ).
              PERFORM reverse_string USING lv_msg CHANGING lv_ebeln.
***Update Deal id using FM
              PERFORM update_zzoldealid USING lv_ebeln
                                              lv_zzoldealid_old.
            ENDIF.
          ENDIF.
          lv_zzoldealid_old = lwa_flheader-zzoldealid.
        ENDIF.

        CLEAR:lv_ebeln,
              lwa_header,
               lv_num,
               lv_msg,
               lta_itemsag,
               lta_pricond,
               lta_maintain,
               lta_return.
      ENDIF.
      lwa_header-lifnr     =  lwa_flheader-lifnr.
      lwa_header-evart     =  lwa_flheader-doc_type.
      lwa_header-ekorg     =  lwa_flheader-purch_org.
      lwa_header-ekgrp     =  lwa_flheader-pur_group.
      lwa_header-werks     =  lwa_flheader-plant.
      lwa_header-lgort     =  lwa_flheader-stge_loc.

*      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
*        EXPORTING
*          input  = lwa_flheader-vper_start
*        IMPORTING
*          output = lwa_header-kdatb.
*
*      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
*        EXPORTING
*          input  = lwa_flheader-vper_end
*        IMPORTING
*          output = lwa_header-kdate.

      lwa_header-kdatb     =  lwa_flheader-vper_start.
      lwa_header-kdate     =  lwa_flheader-vper_end.
      lwa_header-vedat     =  lwa_flheader-doc_date.
      lwa_header-zterm     =  lwa_flheader-pmnttrms.
      lwa_header-waers     =  lwa_flheader-currency.
      lwa_header-inco1     =  lwa_flheader-incoterms1.
      lwa_header-inco2     =  lwa_flheader-incoterms2.
      IF p_deal EQ 'Y'.
        lwa_header-verkf  =  lwa_flheader-zzoldealid.
      ENDIF.
      lwa_header-zztrloc1  =  lwa_flheader-zztrloc1.
      lwa_header-zztrloc2  =  lwa_flheader-zztrloc2.
      lwa_header-zztrloc3  =  lwa_flheader-zztrloc3.
      lwa_header-zztrloc4  =  lwa_flheader-zztrloc4.
      lwa_header-zzcondayqty     =  lwa_flheader-zzcondayqty.
*      DATA:  lv_zzconprice TYPE string.
*      lwa_flheader-zzconprice = lwa_flheader-zzconprice / 1000.
*      IF lwa_flitem-po_unit EQ 'MMB'.
*        CONCATENATE lwa_flheader-zzconprice 'MMBTU' INTO lv_zzconprice SEPARATED BY space.
*        SHIFT lv_zzconprice LEFT DELETING LEADING space.
*        lwa_header-zzconprice      =  lv_zzconprice.
*      ELSE.
*        CONCATENATE lwa_flheader-zzconprice lwa_flitem-po_unit INTO lv_zzconprice SEPARATED BY space.
*        SHIFT lv_zzconprice LEFT DELETING LEADING space.
*        lwa_header-zzconprice      =  lv_zzconprice.
*      ENDIF.
      lwa_header-zzmsa     =  lwa_flheader-zzmsa.
      lwa_header-zzekgrp   =  lwa_flheader-zzekgrp.
      lwa_header-zzparty   =  lwa_flheader-zzparty.
      lwa_header-waers   =  lwa_flheader-currency.
****Item****
      LOOP AT lta_flitem INTO lwa_flitem WHERE zzoldealid = lwa_flheader-zzoldealid.
*        lwa_itemsag-ebelp     =   lwa_flitem-item_no.
        lwa_itemsag-ktmng     =   lwa_flitem-target_qty.

        lwa_itemsag-bednr     =   lwa_flitem-trackingno.
        lwa_itemsag-matkl     =   lwa_flitem-matl_group.
        lwa_itemsag-mwskz     =   lwa_flitem-tax_code.
        lwa_itemsag-evers     =   'FI'.

        lwa_itemsag-matnr     =   lwa_flitem-material.
        lwa_itemsag-sakto     =   lwa_flitem-sakto.
*        lwa_itemsag-kostl     =   lwa_flitem-aufnr.
        lwa_itemsag-kostl     =   lwa_flitem-kostl.
        IF lwa_flitem-material NE 'NATGAS'.
          lwa_itemsag-knttp = 'K'.
        ELSE.
          lwa_itemsag-knttp   =   ''.
          lwa_itemsag-meins   =   lwa_flitem-po_unit.
        ENDIF.
****pricing Condition****
        SORT lta_flitemcon[] ASCENDING BY zzoldealid.
        LOOP AT lta_flitemcon INTO lwa_flitemcon WHERE zzoldealid = lwa_flheader-zzoldealid
                                                    AND item_con = lwa_flitem-item_no.
          lwa_pricond-kschl         =   lwa_flitemcon-cond_type.
*          lwa_pricond-kbetr         =   lwa_flitemcon-cond_value.    "++JWAYCHAL as per previous
          IF lwa_flitem-material NE 'NATGAS'.                       "--JWAYCHAL
            lwa_itemsag-netpr = lwa_flitemcon-cond_value.
          ELSE.
            lwa_pricond-kbetr         =   lwa_flitemcon-cond_value.
          ENDIF.
***ZCONPrice New change
          IF ( lwa_flitemcon-cond_value NE 0 ) OR ( lwa_flitemcon-cond_value NE '' ).
            DATA:  lv_zzconprice TYPE string.
            lwa_flheader-zzconprice = lwa_flitemcon-cond_value / 1000.
            IF lwa_flitem-po_unit EQ 'MMB'.
              CONCATENATE lwa_flheader-currency '/MMBTU' INTO lv_zzconprice.
              CONCATENATE lwa_flheader-zzconprice lv_zzconprice INTO lv_zzconprice SEPARATED BY space.
              SHIFT lv_zzconprice LEFT DELETING LEADING space.
              lwa_header-zzconprice      =  lv_zzconprice.
            ELSE.
              CONCATENATE lwa_flheader-currency '/' lwa_flitem-po_unit INTO lv_zzconprice.
              CONCATENATE lwa_flheader-zzconprice lv_zzconprice INTO lv_zzconprice SEPARATED BY space.
              SHIFT lv_zzconprice LEFT DELETING LEADING space.
              lwa_header-zzconprice      =  lv_zzconprice.
            ENDIF.
          ELSE.
            lwa_flheader-zzconprice = ''.
          ENDIF.
***ZCONPrice New change
          APPEND lwa_itemsag TO lta_itemsag.
          APPEND lwa_pricond TO lta_pricond.
          CLEAR:lwa_pricond.
        ENDLOOP.
        DELETE ADJACENT DUPLICATES FROM lta_pricond.
        lwa_maintain-lpein    =   lc_lpein.
*        CONCATENATE lwa_flheader-vper_start+4(2) lwa_flheader-vper_start+0(4) INTO lwa_maintain-eeind.
        CONCATENATE lwa_flitem-trackingno+4(2) lwa_flitem-trackingno+0(4) INTO lwa_maintain-eeind.
*        lwa_sagmaintain-EEIND    =   lv_eeind.
        lwa_maintain-menge    =   lwa_flitem-target_qty.
        APPEND lwa_maintain TO lta_maintain.
        CLEAR: lwa_itemsag, lwa_maintain.
      ENDLOOP.

****SAG Maintain****
*      LOOP AT lta_flitemcon INTO lwa_flitemcon WHERE zzoldealid = lwa_flheader-zzoldealid.
**        lwa_sagmaintain-item_no      =   lwa_itemcon-item_con.
*        lwa_sagmaintain-lpein    =   lc_lpein.
*        lwa_sagmaintain-eeind    =   lwa_itemcon-cond_count.
*        lwa_sagmaintain-menge    =   lwa_itemcon-cond_type.
*        APPEND lwa_sagmaintain TO lta_sagmaintain.
*      ENDLOOP.

** Logic to trigger for 1 SA.
      IF lv_lines EQ lv_count.
***Call Function Module
        CALL FUNCTION 'ZMM_SAG_CREATE_DM'
          EXPORTING
            lv_mode        = 'N'
            lv_update      = 'S'
            header         = lwa_header   "lta_header
          IMPORTING
            lv_subrc       = lv_num
            lv_output      = lv_msg
          TABLES
            it_itemsag     = lta_itemsag
            it_pricondsag  = lta_pricond
            it_sagmaintain = lta_maintain
            it_return      = lta_return.

        IF lta_return IS NOT INITIAL.
*          APPEND LINES OF lta_return TO lta_output.
          LOOP AT lta_return INTO lwa_return.
            lwa_msgout-zzoldealid = lwa_flheader-zzoldealid.
            lwa_msgout-errmsg = lwa_return-errormessage.
            lwa_msgout-errtyp = lwa_return-errortype.
            APPEND lwa_msgout TO lta_msgout.
          ENDLOOP.
          CLEAR : lv_ebeln.
          IF p_deal EQ 'N'.
            IF lv_msg IS NOT INITIAL.
              lv_msg =  reverse( lv_msg ).
              PERFORM reverse_string USING lv_msg CHANGING lv_ebeln.
              PERFORM update_zzoldealid USING lv_ebeln
                                            lwa_flheader-zzoldealid.
            ENDIF.
          ENDIF.

        ENDIF.
        CLEAR:lv_ebeln,
              lwa_header,
              lv_num,
              lv_msg,
              lta_itemsag,
              lta_pricond,
              lta_maintain,
              lta_return.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " UPLOAD_CUSTOMDATA

*&---------------------------------------------------------------------*
*&         Form  fill_x_structure
*& ---------------------------------------------------------------------*
*       create x-bar per requested parameter
*       all fields will be marked with an 'X' -> change relevant
*----------------------------------------------------------------------*
FORM fill_x_structure  CHANGING cs_x_structure TYPE any.

  DATA: lr_struct TYPE REF TO cl_abap_structdescr,
        lr_field  TYPE REF TO cl_abap_elemdescr.

  FIELD-SYMBOLS: <comp> LIKE LINE OF cl_abap_structdescr=>components,
  <x>    TYPE any.

  lr_struct ?= cl_abap_typedescr=>describe_by_data( cs_x_structure ).
  CHECK lr_struct IS BOUND.
  LOOP AT lr_struct->components ASSIGNING <comp>.
    UNASSIGN <x>.
*  take only x fields into consideration, if unicode is active C1 takes 2 bytes
    CHECK <comp>-type_kind EQ lr_struct->typekind_char.
    ASSIGN COMPONENT <comp>-name OF STRUCTURE cs_x_structure TO <x>.
    CHECK <x> IS ASSIGNED.
*  get element description
    lr_field ?= cl_abap_typedescr=>describe_by_data( <x> ).
    CHECK lr_field->absolute_name EQ '\TYPE=BAPIUPDATE'.
    <x> = cl_mmpur_constants=>yes.
  ENDLOOP.

ENDFORM.                     " fill_x_structure

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
    MESSAGE text-007 TYPE 'I' DISPLAY LIKE 'E'.
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
    IF lta_msgout IS NOT INITIAL.
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
*    COMMIT WORK.
    ENDIF.

  ENDIF.
ENDFORM.                    " UPDATE_ZZOLDEALID
*&---------------------------------------------------------------------*
*&      Form  REVERSE_STRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_MSG  text
*      <--P_LV_EBELN  text
*----------------------------------------------------------------------*
FORM reverse_string  USING p_lv_msg
                     CHANGING p_lv_ebeln.

  DATA: lv_msg1(10) TYPE c,
        lv_string(10) TYPE c.

  lv_msg1 = p_lv_msg+8(10).

  CALL FUNCTION 'STRING_REVERSE'
    EXPORTING
      string    = lv_msg1
      lang      = 'E'
    IMPORTING
      rstring   = lv_string
    EXCEPTIONS
      too_small = 1
      OTHERS    = 2.
  IF sy-subrc EQ 0.
    p_lv_ebeln = lv_string.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " REVERSE_STRING
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
