*&---------------------------------------------------------------------*
*& Report  ZLMM_PURCONTRCT_VALID_RECON
*&---------------------------------------------------------------------*
************************************************************************
*                            COGS                                      *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMM_PURCONTRCT_VALID_RECON                   *
*& Author             :  Jaydeep Waychal/Prashant Durbhaka              *
*& Creation Date      :  Jun 26, 2021                                  *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program to Validate input file details to     *
*&                       create Purchase Contract and generate recon   *
*&                       report after creation of Purchase contract.   *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          :                                                      *
* Modified By   : P2P:CHG0216959 - DM08 Validation Program V1_0                                                      *
* Correction No :                                                      *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*

REPORT  zlmmi047_prcontrct_validation.

TABLES: ekko,ekpo,zmmt_mastagree,konv.

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
*Item
            item_no     TYPE  ebelp,   " Item
            tax_code    TYPE  mwskz,
            itm_txt     TYPE  txz01,
            trackingno  TYPE  bednr,
            price_cat   TYPE  meprf,
            target_qty  TYPE  ktmng,
*Service
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

*Internal Table Declarations *.
DATA: lta_upload       TYPE STANDARD TABLE OF ty_upload,
      lta_input_data   TYPE STANDARD TABLE OF ty_input_data,
      lta_final        TYPE STANDARD TABLE OF ty_final,
      lta_msgout       TYPE STANDARD TABLE OF ty_msgout,
*      lta_agmtid       TYPE STANDARD TABLE OF ty_agmid,
      lta_header       TYPE STANDARD TABLE OF ty_header,
*      lta_header1      TYPE STANDARD TABLE OF ty_header,
      lta_item         TYPE STANDARD TABLE OF ty_item,
      lta_services     TYPE STANDARD TABLE OF ty_services,
      lta_recon        TYPE STANDARD TABLE OF ty_final.

* Work Area Declarations *
DATA: lwa_upload       TYPE ty_upload,
      lwa_input_data   TYPE ty_input_data,
      lwa_final        TYPE ty_final,
      lwa_msgout       TYPE ty_msgout,
*      lwa_agmtid       TYPE ty_agmid,
      lwa_header       TYPE ty_header,
      lwa_item         TYPE ty_item,
      lwa_services     TYPE ty_services.

* Variable Declarations *
DATA: lv_message(2000) TYPE c,
      lv_sep(1)      TYPE c VALUE ',',
      lv_errtype     TYPE c VALUE 'E',
      lv_cnt(5)      TYPE n,
      lv_path        TYPE btch0000-text80.

DATA: lv_flag TYPE c,
      lv_msg(50) TYPE c,
      lv_matkl   TYPE matkl.

* Constant Declarations *
*CONSTANTS: lc_doccat   TYPE c VALUE 'K',
*           lc_gstrnspt TYPE c VALUE 'T',
*           lc_gsstorge TYPE c VALUE 'S',
*           lc_status   TYPE c VALUE 'A',
*           lc_telphone1(3) TYPE c VALUE 'G00',
*           lc_telphone2(3) TYPE c VALUE 'G99'.

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
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* AT SELECTION-SCREEN processes
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
*-- Form to Select a File From a Particular Location
  PERFORM get_filename_f4 USING p_fname.

************************************************************************
* Start-of-selection *
************************************************************************
START-OF-SELECTION.
*** To upload flat file data into the internal table.
  PERFORM file_upload.
*** Split data from input file into an internal table
  PERFORM split_data.
*** Validate input data
  PERFORM validate_data.
  IF lta_msgout[] IS NOT INITIAL.
    PERFORM alv_grid_display.
  ELSE.
    WRITE 'No error in the input file and its ready to load.' .
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  get_filename_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_filename_f4 USING p_fname.

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
*&      Form  FILE_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
ENDFORM.                    " FILE_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  SPLIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM split_data .

*Moving data into internal table to validate
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
      CLEAR lwa_upload.
    ENDLOOP.
    DELETE lta_input_data INDEX 1.
    LOOP AT lta_input_data INTO lwa_input_data.
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
      lwa_header-zzekgrp     = lwa_input_data-zzekgrp.
      lwa_header-itm_cat     = lwa_input_data-itm_cat.
      lwa_header-ass_cat     = lwa_input_data-ass_cat.
      lwa_header-currency    = lwa_input_data-currency.
      lwa_header-plant       = lwa_input_data-plant.
*      CLEAR: lv_matkl.
*      SELECT matkl
*         FROM asmd
*         INTO lv_matkl
*         WHERE asnum EQ lwa_input_data-itm_txt.
*      ENDSELECT.
*      IF sy-subrc EQ 0.
*        lwa_header-matl_group  = lv_matkl.
*      ENDIF.
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
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM lta_header COMPARING zzoldealid.
*    DELETE ADJACENT DUPLICATES FROM lta_item COMPARING item_no zzoldealid.
  ELSE.
    MESSAGE text-002 TYPE 'E'.
  ENDIF.

ENDFORM.                    " SPLIT_DATA
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_data .
  DATA:   lv_ekorg TYPE ekorg,
           lv_zzmsa TYPE zzmsa,
           lv_ekgrp TYPE ekgrp,
            lv_lifnr TYPE lifnr.

  IF lta_header IS NOT INITIAL.
***Validate input file data
*1.	Document Type (DOC_TYPE)
    DATA: lv_bsart TYPE bsart.
    LOOP AT lta_header INTO lwa_header.
      CLEAR:lv_bsart.
      SELECT bsart FROM t161 INTO lv_bsart
           WHERE  bsart EQ lwa_header-cont_typ
            AND bstyp EQ 'K'
            AND bsart EQ 'ZK'.
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE ' Invalid Document Type.' lwa_header-cont_typ INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ENDIF.
*2.	PLANT
      DATA: lv_werks TYPE werks,
            lv_bukrs TYPE bukrs.
      CLEAR:lv_werks,lv_bukrs.
      SELECT werks FROM t001w INTO lv_werks
           WHERE  werks EQ lwa_header-plant.
      ENDSELECT.
      IF sy-subrc EQ 0.
        SELECT  bukrs FROM t001k INTO lv_bukrs
           WHERE  bwkey EQ lv_werks
          AND bukrs EQ lwa_header-comp_code.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid Plant/Plant Company Code.' lwa_header-plant INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ELSE.
        CONCATENATE 'Invalid Plant code.' lwa_header-plant INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ENDIF.
***********************Check if required*************************
*3.	STGE_LOC
*#  Validate Storage location is A001 # Message #Storage location NE A001#
*#  Plant and Storage location exist in table T001L # Message #Invalid Plant/Storage location &PLANT &STGE_LOC#
*WERKS = PLANT and LGORT = STGE_LOC
*      IF lwa_header-stge_loc NE 'A001'.
*        CONCATENATE ' Storage location is not A001' lwa_header-stge_loc INTO lv_message SEPARATED BY space.
*        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
*      ELSE.
*        DATA: lv_lgort TYPE lgort_d.
*        CLEAR:lv_lgort.
*        SELECT lgort FROM t001l INTO lv_lgort
*             WHERE lgort EQ lwa_header-stge_loc
*             AND werks EQ lwa_header-plant.
*        ENDSELECT.
*        IF sy-subrc NE 0.
*          CONCATENATE ' Invalid Plant/Storage location' lwa_header-stge_loc '/' lwa_header-stge_loc INTO lv_message SEPARATED BY space.
*          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
*        ENDIF.
*      ENDIF.

*4.	COMP_CODE
      CLEAR:lv_bukrs.
      SELECT bukrs FROM t001 INTO lv_bukrs
           WHERE bukrs EQ lwa_header-comp_code.
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE ' Invalid Company code' lwa_header-comp_code INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ELSE.
        IF ( ( lwa_header-comp_code NE 'UGL' ) AND  ( lwa_header-comp_code NE 'EGD' ) ).
          CONCATENATE ' Company code ' lwa_header-comp_code ' is not UGL or EGD' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ENDIF.
*5.	VENDOR
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_header-vendor
        IMPORTING
          output = lwa_header-vendor.
      CLEAR:lv_lifnr.
      SELECT lifnr FROM lfa1 INTO lv_lifnr
           WHERE lifnr EQ lwa_header-vendor.
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE ' Invalid Vendor.' lwa_header-vendor INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ELSE.
*Vendor and Purchase org
        SELECT lifnr FROM lfm1 INTO lv_lifnr
           WHERE lifnr EQ lwa_header-vendor
          AND ekorg EQ lwa_header-purch_org.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Vendor' lwa_header-vendor 'not extended to Pur Org GASA' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
*Vendor and Company code
        SELECT lifnr FROM lfb1 INTO lv_lifnr
           WHERE lifnr EQ lwa_header-vendor
          AND bukrs EQ lwa_header-comp_code.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Vendor' lwa_header-vendor 'not extended to company code' lwa_header-comp_code INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
*Vendor and Care ID
        SELECT lifnr FROM lfm1 INTO lv_lifnr
           WHERE lifnr EQ lwa_header-vendor
          AND eikto NE ''.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Vendor' lwa_header-vendor 'CARE ID not populated' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ENDIF.

*6.	PMNTTRMS
      DATA: lv_zterm TYPE dzterm.
      CLEAR:lv_zterm.
      SELECT zterm FROM lfm1 INTO lv_zterm
           WHERE  zterm EQ lwa_header-pmnttrms.         "#EC CI_NOFIELD
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE ' Invalid Payment Terms' lwa_header-pmnttrms INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ELSE.
        CLEAR: lv_lifnr.
        SELECT lifnr FROM lfm1 INTO lv_lifnr
         WHERE lifnr EQ lwa_header-vendor
          AND ekorg EQ 'GASA'.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE ' Vendor Master' lwa_header-vendor lwa_header-purch_org 'payment term is different' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ENDIF.

*7.	PURCH_ORG
      IF lwa_header-purch_org EQ 'GASA'.
        SELECT ekorg FROM t024e INTO lv_ekorg
         WHERE ekorg EQ lwa_header-purch_org.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid Purchase Organization' lwa_header-purch_org INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ELSE.
        CONCATENATE 'Purchase Organization ' lwa_header-purch_org 'not equal to GASA.' INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ENDIF.

*8.	PURCH_GRP
      DATA: lv_ektel TYPE ektel.
      CLEAR: lv_ektel.
      SELECT ektel FROM t024 INTO lv_ektel
                     WHERE ekgrp EQ lwa_header-pur_group.
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE 'Invalid Purchasing Group' lwa_header-pur_group INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ELSE.
        IF ( lv_ektel NE 'TRANSPORT' ) .
          CONCATENATE 'Purchase group' lwa_header-pur_group 'not for Transport and Storage.' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ENDIF.

*9.	VPER_END
      DATA: lv_date TYPE string.
      PERFORM check_input_date USING lwa_header-vper_end CHANGING lv_date.
      IF lv_date IS NOT INITIAL.
        CONCATENATE lv_date 'for End of Validity Period ' lwa_header-vper_end INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ELSE.
*        DATA: lv_vperend TYPE  kdate.
*        PERFORM conversion_date USING lwa_header-vper_end CHANGING lv_vperend.
      ENDIF.

*10.  VPER_START
      CLEAR: lv_date.
      PERFORM check_input_date USING lwa_header-vper_start CHANGING lv_date.
      IF lv_date IS NOT INITIAL.
        CONCATENATE lv_date 'for Start of Validity Period' lwa_header-vper_start INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ELSE.
*        DATA: lv_vperstart TYPE  kdate.
*        PERFORM conversion_date USING lwa_header-vper_start CHANGING lv_vperstart.
      ENDIF.
***Compare End date and Start date
      IF  lwa_header-vper_end LT lwa_header-vper_start.
        CONCATENATE 'End date' lwa_header-vper_end 'must be greater or Equal to start date' lwa_header-vper_start
        INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ENDIF.


***********************Check if required*************************
*11.  DOC_DATE
*#  Valid date
*      CLEAR: lv_date.
*      PERFORM check_input_date USING lwa_header-doc_date CHANGING lv_date.
*      IF lv_date IS NOT INITIAL.
*        CONCATENATE lv_date 'Check System Date format' lwa_header-doc_date INTO lv_message SEPARATED BY space.
*        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
*      ELSE.
**        DATA: lv_docdate TYPE  kdate.
**        PERFORM conversion_date USING lwa_flheader-doc_date CHANGING lv_docdate.
*      ENDIF.
*
****Compare start dare and doc date
*      IF lwa_flheader-vper_start LT lwa_header-doc_date.
*        CONCATENATE 'Start date' lwa_header-vper_start 'must be greater or equal to Contract date' lwa_header-doc_date
*        INTO lv_message SEPARATED BY space.
*        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
*      ENDIF.


*12.  INCOTERMS1
      DATA: lv_inco1 TYPE inco1.
      CLEAR: lv_inco1.
      SELECT inco1 FROM tinc INTO lv_inco1
           WHERE inco1 EQ lwa_header-incoterms1.
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE 'Invalid Index' lwa_header-incoterms1 INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ENDIF.

*13.  ZZOLDEALID
      DATA: lv_zzoldealid TYPE zoldealid.
      CLEAR: lv_zzoldealid.
      IF lwa_header-zzoldealid EQ ''.
        CONCATENATE 'Openlink Deal ID is blank.' lwa_header-zzoldealid  INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ELSE.
        SELECT zzoldealid  INTO lv_zzoldealid
         FROM ekko
         WHERE zzoldealid EQ lwa_header-zzoldealid.     "#EC CI_NOFIELD
        ENDSELECT.
        IF sy-subrc EQ 0.
          CONCATENATE 'Openlink Deal ID is available in the system.' lwa_header-zzoldealid INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ENDIF.

*14.  ZZEKGRP
      IF ( ( lwa_header-zzekgrp(1) NE 'G') OR ( ( lwa_header-zzekgrp+1(2) < '01' ) OR ( lwa_header-zzekgrp+1(2) > '99' ) ) ).
        CONCATENATE 'Trading buyer' lwa_header-zzekgrp 'must have a value between G01 & G99.' INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ELSE.
        CLEAR: lv_ekgrp.
        SELECT ekgrp  INTO lv_ekgrp
         FROM t024
         WHERE ekgrp EQ lwa_header-zzekgrp.             "#EC CI_NOFIELD
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid trading buyer' lwa_header-zzekgrp INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ENDIF.
*15.  ZZMSA
      TYPES: BEGIN OF ty_zmmt_mastagree,
              zzmsa TYPE zmmt_mastagree-zzmsa,
              bukrs TYPE zmmt_mastagree-bukrs,
              zzfromdate TYPE zmmt_mastagree-zzfromdate,
              zztodate TYPE zmmt_mastagree-zztodate,
              zzstatus TYPE zmmt_mastagree-zzstatus,
              zzmsatype TYPE zmmt_mastagree-zzmsatype,
            END OF ty_zmmt_mastagree.

      DATA: lwa_zmmt_mastagree     TYPE ty_zmmt_mastagree,
            lta_zmmt_mastagree    LIKE TABLE OF lwa_zmmt_mastagree.

      IF lwa_header-zzmsa EQ ''.
        CONCATENATE 'Master Service Agreement is blank' lwa_header-zzmsa INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ELSE.
        SELECT zzmsa bukrs zzfromdate zztodate zzstatus zzmsatype  FROM zmmt_mastagree
          INTO CORRESPONDING FIELDS OF TABLE lta_zmmt_mastagree
          WHERE zzmsa EQ lwa_header-zzmsa.              "#EC CI_NOFIELD
        IF sy-subrc EQ 0.
          DESCRIBE TABLE lta_zmmt_mastagree LINES lv_cnt.
          IF lv_cnt > 1.
            CONCATENATE 'MSA Name is duplicated' lwa_header-zzmsa INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
          ELSE.
            LOOP AT lta_zmmt_mastagree INTO lwa_zmmt_mastagree.
              IF lwa_zmmt_mastagree-zzstatus NE 'A'.
                CONCATENATE 'MSA ' lwa_header-zzmsa 'is not active.' INTO lv_message SEPARATED BY space.
                PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
              ELSEIF ( ( lwa_zmmt_mastagree-zzmsatype NE 'T' ) AND ( lwa_zmmt_mastagree-zzmsatype NE 'S' ) ).
                CONCATENATE 'MSA ' lwa_header-zzmsa 'is not T/S MSA.' INTO lv_message SEPARATED BY space.
                PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
              ELSEIF ( ( ( lwa_zmmt_mastagree-zzfromdate => lwa_header-vper_start ) AND ( lwa_zmmt_mastagree-zztodate <= lwa_header-vper_start ) )
                OR ( ( lwa_zmmt_mastagree-zzfromdate => lwa_header-vper_end ) AND ( lwa_zmmt_mastagree-zztodate <= lwa_header-vper_end ) ) ).
                CONCATENATE 'MSA ' lwa_header-zzmsa 'T/S deals dates are incorrect' INTO lv_message SEPARATED BY space.
                PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
              ELSEIF ( lwa_header-plant NE lwa_zmmt_mastagree-bukrs ).
                DATA: lv_plant TYPE bukrs.
                SELECT bwkey FROM t001k INTO lv_plant
                  WHERE bwkey EQ lwa_header-plant
                  AND bukrs EQ lwa_zmmt_mastagree-bukrs.
                ENDSELECT.
                IF sy-subrc NE 0..
                  CONCATENATE 'MSA plant is different ' lwa_zmmt_mastagree-bukrs 'than input file' lwa_header-plant
              INTO lv_message SEPARATED BY space.
                  PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
*16.  CURRENCY
      IF ( lwa_header-currency EQ 'USD' ) OR ( lwa_header-currency EQ 'CAD' ).
        DATA: lv_waers TYPE waers.
        CLEAR: lv_waers.
        SELECT waers FROM tcurc INTO lv_waers
             WHERE waers EQ lwa_header-currency.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid currency' lwa_header-currency   INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ELSE.
        CONCATENATE 'Currency' lwa_header-currency  'not equal to USD/CAD.' INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
      ENDIF.
    ENDLOOP .
****************Item Level validation******************
    LOOP AT lta_item INTO lwa_item.
      READ TABLE lta_header INTO lwa_header WITH KEY zzoldealid = lwa_item-zzoldealid.
      IF sy-subrc EQ 0.
*18.  TRACKINGNO
        IF lwa_item-trackingno EQ ''.
          CONCATENATE 'Blank tracking number' lwa_item-trackingno INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
*19.  TAX_CODE
        DATA: lv_text1 TYPE text1_007s.
        CLEAR: lv_text1.
        SELECT  text1 FROM t007s INTO lv_text1
          WHERE spras EQ 'E'
          AND kalsm EQ 'TAXCA'
          AND mwskz EQ lwa_item-tax_code.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid tax code' lwa_item-tax_code   INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ELSE.
          IF lv_text1 CS '*NOT USED*'.
            CONCATENATE 'Tax text updated as NOT USED' lwa_item-tax_code INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*17.  PO_UNIT
*          DATA:lv_mseh3 TYPE mseh3,
*                          lv_isocode TYPE isocd_unit.
*          CLEAR: lv_mseh3, lv_isocode.
*          SELECT mseh3 FROM t006a INTO lv_mseh3
*               WHERE mseh3 EQ lwa_item-.       "#EC CI_GENBUFF
*          ENDSELECT.
*          IF sy-subrc NE 0.
*            CONCATENATE 'Invalid PO UOM' lwa_item-po_unit INTO lv_message SEPARATED BY space.
*            PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
*          ELSE.
*            IF ( ( lwa_item-po_unit NE 'MMB' ) AND ( lwa_item-po_unit NE 'GJ' ) ).
*              CONCATENATE 'UOM' lwa_item-po_unit 'is not MMB or GJ' INTO lv_message SEPARATED BY space.
*              PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
*            ELSE.
*              SELECT isocode FROM t006 INTO lv_isocode
*               WHERE isocode NE ''.
*              ENDSELECT.
*              IF sy-subrc EQ 0 .
*                CONCATENATE 'Missing ISO Code' lwa_item-po_unit INTO lv_message SEPARATED BY space.
*                PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
*              ENDIF.
*            ENDIF.
*          ENDIF.
***********************************************************
****************Services Validation************************
    LOOP AT lta_services INTO lwa_services.
      READ TABLE lta_header INTO lwa_header WITH KEY zzoldealid = lwa_services-zzoldealid.
      IF sy-subrc EQ 0.
*20.  ESLL-SRVPOS Service Number
        IF lwa_services-ser_num EQ ''.
          CONCATENATE 'Service Number is Blank' lwa_services-ser_num INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ELSE.
          DATA:lv_lvorm  TYPE lvorm.
          CLEAR: lv_lvorm.
          TRANSLATE lwa_services-ser_num TO UPPER CASE.
          SELECT lvorm FROM asmd INTO lv_lvorm
               WHERE asnum EQ lwa_services-ser_num.     "#EC CI_GENBUFF
          ENDSELECT.
          IF sy-subrc NE 0.
            CONCATENATE 'Service Number is Blank' lwa_services-ser_num INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
          ELSE.
            IF lv_lvorm NE ''.
              CONCATENATE 'Service Master' lwa_services-ser_num 'is deleted' INTO lv_message SEPARATED BY space.
              PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
            ENDIF.
          ENDIF.
        ENDIF.

*21.  ESLL-TBTWR  Price
        IF lwa_services-price EQ ''.
          CONCATENATE 'Price not populated' '' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.

********************************Check table***************************
*22.  ESLL-MEINS  Base UOM
*          DATA:lv_meins   TYPE meins.
*          CLEAR: lv_meins.
*          SELECT meins FROM t006 INTO lv_meins
*               WHERE meins EQ lwa_services-uom.         "#EC CI_GENBUFF
*          ENDSELECT.
*          IF sy-subrc NE 0.
*            CONCATENATE 'Invalid UOM' lwa_services-uom INTO lv_message SEPARATED BY space.
*            PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
*          ENDIF.

********************************Check table***************************
*23.  ESLH-WAERS  UOM
*#  Validate WAERS in table T006A - Message #Invalid UOM &SRVPOS#
*          DATA:lv_waers  TYPE waers.
*          CLEAR: lv_waers.
*          SELECT waers FROM t006 INTO lv_waers
*               WHERE waers EQ lwa_services-price.       "#EC CI_GENBUFF
*          ENDSELECT.
*          IF sy-subrc NE 0.
*
*          ENDIF.

*24.  KOSTL Cost Center
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lwa_services-cost_center
          IMPORTING
            output = lwa_services-cost_center.
        DATA:lv_kostl  TYPE kostl,
             date TYPE sy-datum.
        CLEAR: lv_kostl.
        SELECT kostl FROM csks INTO lv_kostl
             WHERE kostl EQ lwa_services-cost_center
          AND datab LE sy-datum
          AND datbi GE  date+2.                         "#EC CI_GENBUFF
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid cost center' lwa_services-cost_center 'Or Cost Center date.' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
*25.  SAKTO GL
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lwa_services-glaccount
          IMPORTING
            output = lwa_services-glaccount.

        DATA:lv_saknr  TYPE saknr.
        CLEAR: lv_saknr.
        SELECT saknr FROM ska1 INTO lv_saknr
             WHERE ktopl EQ 'COAT'
              AND saknr EQ lwa_services-glaccount.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid GL account' lwa_services-glaccount INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ELSE.
          SELECT saknr FROM skb1 INTO lv_saknr
            WHERE saknr EQ lwa_services-glaccount.      "#EC CI_GENBUFF
          ENDSELECT.
          IF sy-subrc NE 0.
            CONCATENATE 'GL account' lwa_services-glaccount 'not created in company code in company EGD.' INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
          ENDIF.
        ENDIF.

*26.  ESLL-USERF2_NUM User Field 2
        IF lwa_services-userf2_num EQ ''.
*            lwa_services-userf2_num
          CONCATENATE 'User field 2 is not populated' '' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.

*27.  ESLL -USERF1_TXT  User Field 3
        IF lwa_services-userf1_txt EQ ''.
          CONCATENATE 'User field 3 is not populated.' lwa_services-userf1_txt INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_header-zzoldealid.
        ENDIF.
      ENDIF.
    ENDLOOP.  "item Services
    SORT lta_msgout[] DESCENDING BY zzoldealid.
    DELETE ADJACENT DUPLICATES FROM lta_msgout COMPARING errmsg zzoldealid.
  ENDIF.
ENDFORM.                    " VALIDATE_DATA

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
    MESSAGE 'Error Creating ALV Grid ' TYPE 'I' DISPLAY LIKE 'E'.
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
    MOVE 'Validation for Purchase Contract Creation ' TO lv_title.
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
    MOVE 'Validation for Purchase Contract Creation' TO lr_head.
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
                                    text = 'No of Records :'
                                    tooltip = 'No of Records :' ).
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
*&      Form  ERROR_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_MESSAGE  text
*      -->P_LWA_FLHEADER_ZZOLDEALID  text
*----------------------------------------------------------------------*
FORM error_message_display  USING    p_lv_message
                                     p_lwa_flheader_zzoldealid.
  lwa_msgout-zzoldealid = p_lwa_flheader_zzoldealid.
  lwa_msgout-errmsg = p_lv_message.
  lwa_msgout-errtyp = 'E'.
  APPEND lwa_msgout TO lta_msgout.
  CLEAR: lwa_msgout.

ENDFORM.                    " ERROR_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_FLHEADER_DOC_DATE  text
*      <--P_LV_DATE  text
*----------------------------------------------------------------------*
FORM check_input_date  USING    p_lwa_flheader_doc_date
                       CHANGING p_lv_date.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                            = p_lwa_flheader_doc_date
*   EXCEPTIONS
*     PLAUSIBILITY_CHECK_FAILED       = 1
*     OTHERS                          = 2
            .
  IF sy-subrc <> 0.
    p_lv_date =  'Invalid date'.
*  ELSE.
*    p_lv_date = ''.
  ENDIF.
ENDFORM.                    " CHECK_INPUT_DATE
