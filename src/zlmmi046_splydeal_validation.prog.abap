*&---------------------------------------------------------------------*
*& Report  ZLMMI046_SPLYDEAL_VALIDATION
*&
*&---------------------------------------------------------------------*
************************************************************************
*                            COGS                                      *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI046_SPLYDEAL_VALIDATION                  *
*& Author             :  Jaydeep Waychal/Prashan Durbhaka              *
*& Creation Date      :  Jul 26, 2021                                  *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program to Validate input file details to     *
*&                       create  Supply deal and generate recon report *
*&                       after creation of supply deals.               *
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

REPORT  zlmmi046_splydeal_validation NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: ekko,ekpo,zmmt_mastagree,konv.

* Types Declarations *
TYPES : BEGIN OF ty_upload,
           string TYPE string,
        END OF ty_upload,

 BEGIN OF ty_input_data,
            doc_type    TYPE  esart,
            plant       TYPE  werks_d,
            stge_loc    TYPE  lgort_d,
            comp_code   TYPE  bukrs,
            lifnr       TYPE  elifn,
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
            currency    TYPE  waers,
            item_no     TYPE  ebelp,           "Items
            target_qty  TYPE  char13,
            po_unit     TYPE  bstme,
*            orderpr_un  TYPE  bbprm,
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
            zzconprice  TYPE z_conprice,
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
            aufnr       TYPE aufnr,
  END OF ty_flitem,

  BEGIN OF ty_flitemcon,
            zzoldealid  TYPE  zoldealid,
            item_con    TYPE  ebelp,
            cond_type   TYPE kscha,
            cond_value  TYPE d,
  END OF ty_flitemcon.

TYPES:  BEGIN OF ty_final,
          doc_type    TYPE  esart,
          comp_code   TYPE  bukrs,
          lifnr       TYPE  elifn,
          pmnttrms    TYPE  dzterm,
          purch_org   TYPE  ekorg,
          pur_group   TYPE  bkgrp,
          vper_end    TYPE  kdate,
          vper_start  TYPE  kdatb,
          doc_date    TYPE  vedat,
          incoterms1  TYPE  inco1,
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
*            orderpr_un  TYPE  bbprm,
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
          cond_value  TYPE d,
          currency      TYPE waers,
          currency_iso  TYPE waers_iso,
          cond_p_unt    TYPE  d, "kpein,
          cond_unit_iso TYPE kvmei_iso,
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


***Internal Table Declarations *.
DATA: lta_upload       TYPE STANDARD TABLE OF ty_upload,
      lta_input_data   TYPE STANDARD TABLE OF ty_input_data,
      lta_msgout       TYPE STANDARD TABLE OF ty_msgout,
      lta_agmtid       TYPE STANDARD TABLE OF ty_agmid,
      lta_flheader     TYPE STANDARD TABLE OF ty_flheader,
      lta_flitem       TYPE STANDARD TABLE OF ty_flitem,
      lta_flitemcon    TYPE STANDARD TABLE OF ty_flitemcon,
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
      lwa_flitemcon    TYPE ty_flitemcon.

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
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

PARAMETER: p_fname TYPE rlgrap-filename.

SELECTION-SCREEN END OF BLOCK b2.
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
    WRITE 'No error in the file and ready to load.' .
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
      CLEAR lwa_upload.
    ENDLOOP.
    DELETE lta_input_data INDEX 1.
    IF lta_input_data IS NOT INITIAL.
      LOOP AT lta_input_data INTO lwa_input_data.
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
*        IF lwa_input_data-material NE 'NATGAS'.
**        lwa_flitem-material   =   p_matnr.
*        ELSE.
          lwa_flitem-material   =   lwa_input_data-material.
*        ENDIF.
        APPEND lwa_flitem TO lta_flitem.

        lwa_flitemcon-zzoldealid = lwa_input_data-zzoldealid.
        lwa_flitemcon-item_con   = lwa_input_data-item_no.    "Pass item number from Item
        lwa_flitemcon-cond_type  = lwa_input_data-cond_type.
        lwa_flitemcon-cond_value  = lwa_input_data-cond_value.
        APPEND lwa_flitemcon TO lta_flitemcon.
      ENDLOOP.
      SORT lta_flheader[] DESCENDING BY zzoldealid.
      SORT lta_flitem[] DESCENDING BY zzoldealid.
      DELETE ADJACENT DUPLICATES FROM lta_flheader COMPARING zzoldealid.
*      DELETE ADJACENT DUPLICATES FROM lta_flitem COMPARING item_no.
    ENDIF.
  ELSE.
    MESSAGE text-002 TYPE 'E'.
  ENDIF.
ENDFORM.                    " split_data
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_data.

  DATA:  lv_ebeln TYPE ebeln,
         lv_ekkolifnr TYPE lifnr,
         lv_lfm1lifnr TYPE lifnr,
         lv_ekorg TYPE ekorg VALUE 'GASA',
         lv_zzmsa TYPE zzmsa,
         lv_ekgrp TYPE ekgrp,
         lv_zloc TYPE ztrloc,
         lv_lines TYPE i,
         lv_errflg TYPE c.


  DATA: lv_bsart TYPE bsart.

  IF lta_flheader[] IS NOT INITIAL.
***Validate input file data
    LOOP AT lta_flheader INTO lwa_flheader.
      "Document Type (DOC_TYPE)
      CLEAR:lv_bsart.
      SELECT bsart FROM t161 INTO lv_bsart
           WHERE  bsart EQ lwa_flheader-doc_type
            AND bstyp EQ 'L'
            AND ( ( bsart NE 'LP' ) OR ( bsart NE 'LU' ) OR ( bsart NE 'LPA' ) ).
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE ' Invalid Document Type.' lwa_flheader-doc_type INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ENDIF.
*2.	PLANT
      DATA: lv_werks TYPE werks,
            lv_bukrs TYPE bukrs.
      CLEAR:lv_werks,lv_bukrs.
      SELECT werks FROM t001w INTO lv_werks
           WHERE  werks EQ lwa_flheader-plant.
      ENDSELECT.
      IF sy-subrc EQ 0.
        SELECT  bukrs FROM t001k INTO lv_bukrs
           WHERE  bwkey EQ lv_werks
          AND bukrs EQ lwa_flheader-comp_code.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid Plant/Plant Company Code.' lwa_flheader-plant INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ELSE.
        CONCATENATE 'Invalid Plant code.' lwa_flheader-plant INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ENDIF.

*3.	STGE_LOC
      IF lwa_flheader-stge_loc NE 'A001'.
        CONCATENATE ' Storage location is not A001' lwa_flheader-stge_loc INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
        DATA: lv_lgort TYPE lgort_d.
        CLEAR:lv_lgort.
        SELECT lgort FROM t001l INTO lv_lgort
             WHERE lgort EQ lwa_flheader-stge_loc
             AND werks EQ lwa_flheader-plant.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE ' Invalid Plant/Storage location' lwa_flheader-stge_loc '/' lwa_flheader-stge_loc INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.

*4.	COMP_CODE
      CLEAR:lv_bukrs.
      SELECT bukrs FROM t001 INTO lv_bukrs
           WHERE bukrs EQ lwa_flheader-comp_code.
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE ' Invalid Company code' lwa_flheader-comp_code INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
        IF ( ( lwa_flheader-comp_code NE 'UGL' ) AND  ( lwa_flheader-comp_code NE 'EGD' ) ).
          CONCATENATE ' Company code ' lwa_flheader-comp_code ' is not UGL or EGD' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.

*5.	VENDOR
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_flheader-lifnr
        IMPORTING
          output = lwa_flheader-lifnr.

      DATA: lv_lifnr TYPE lifnr.
      CLEAR:lv_lifnr.
      SELECT lifnr FROM lfa1 INTO lv_lifnr
           WHERE lifnr EQ lwa_flheader-lifnr.
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE ' Invalid Vendor.' lwa_flheader-lifnr INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
*Vendor and Purchase org
        SELECT lifnr FROM lfm1 INTO lv_lifnr
           WHERE lifnr EQ lwa_flheader-lifnr
          AND ekorg EQ lwa_flheader-purch_org.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Vendor' lwa_flheader-lifnr 'not extended to Pur Org GASA' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
*Vendor and Company code
        SELECT lifnr FROM lfb1 INTO lv_lifnr
           WHERE lifnr EQ lwa_flheader-lifnr
          AND bukrs EQ lwa_flheader-comp_code.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Vendor' lwa_flheader-lifnr 'not extended to company code' lwa_flheader-comp_code INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
*Vendor and Care ID
        SELECT lifnr FROM lfm1 INTO lv_lifnr
           WHERE lifnr EQ lwa_flheader-lifnr
          AND eikto NE ''.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Vendor' lwa_flheader-lifnr 'CARE ID not populated' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.

*6.	PMNTTRMS
      DATA: lv_zterm TYPE dzterm.
      CLEAR:lv_zterm.
      SELECT zterm FROM lfm1 INTO lv_zterm
           WHERE  zterm EQ lwa_flheader-pmnttrms.   "#EC CI_NOFIELD
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE ' Invalid Payment Terms' lwa_flheader-pmnttrms INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
        CLEAR: lv_lifnr.
        SELECT lifnr FROM lfm1 INTO lv_lifnr
         WHERE  lifnr EQ lwa_flheader-lifnr
          AND ekorg EQ 'GASA'.            "zterm EQ lwa_flheader-pmnttrms
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE ' Vendor Master' lwa_flheader-lifnr lwa_flheader-purch_org 'payment term is different' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.

*7.	PURCH_ORG
      IF lwa_flheader-purch_org EQ 'GASA'.
        SELECT ekorg FROM t024e INTO lv_ekorg
         WHERE ekorg EQ lwa_flheader-purch_org.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid Purchase Organization' lwa_flheader-purch_org INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ELSE.
        CONCATENATE 'Purchase Organization ' lwa_flheader-purch_org 'not equal to GASA.' INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ENDIF.

*8.	PURCH_GRP
      SELECT ekgrp FROM t024 INTO lv_ekgrp
               WHERE ekgrp EQ lwa_flheader-pur_group.
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE 'Invalid Purchasing Group' lwa_flheader-pur_group INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
        IF ( ( lwa_flheader-pur_group NE 'GP1' ) AND ( lwa_flheader-pur_group NE 'GP2' ) ).
          CONCATENATE 'Purchase group' lwa_flheader-pur_group 'not equal to GP1 or GP2.' INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.
*9.	VPER_END

      DATA: lv_date TYPE string.
      PERFORM check_input_date USING lwa_flheader-vper_end CHANGING lv_date.
      IF lv_date IS NOT INITIAL.
        CONCATENATE lv_date 'for End of Validity Period ' lwa_flheader-vper_end INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
*        DATA: lv_vperend TYPE  kdate.
*        PERFORM conversion_date USING lwa_flheader-vper_end CHANGING lv_vperend.
      ENDIF.
*10.  VPER_START
      CLEAR: lv_date.
      PERFORM check_input_date USING lwa_flheader-vper_start CHANGING lv_date.
      IF lv_date IS NOT INITIAL.
        CONCATENATE lv_date 'for Start of Validity Period' lwa_flheader-vper_start INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
*        DATA: lv_vperstart TYPE  kdate.
*        PERFORM conversion_date USING lwa_flheader-vper_start CHANGING lv_vperstart.
      ENDIF.
***Compare End date and Start date
      IF  lwa_flheader-vper_end LT lwa_flheader-vper_start.
        CONCATENATE 'End date' lwa_flheader-vper_end 'must be greater or Equal to start date' lwa_flheader-vper_start
        INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ENDIF.

*11.  DOC_DATE
*#  Valid date
      CLEAR: lv_date.
      PERFORM check_input_date USING lwa_flheader-doc_date CHANGING lv_date.
      IF lv_date IS NOT INITIAL.
        CONCATENATE lv_date 'Check System Date format' lwa_flheader-doc_date INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
*        DATA: lv_docdate TYPE  kdate.
*        PERFORM conversion_date USING lwa_flheader-doc_date CHANGING lv_docdate.
      ENDIF.

***Compare start dare and doc date
      IF lwa_flheader-vper_start LT lwa_flheader-doc_date.
        CONCATENATE 'Start date' lwa_flheader-vper_start 'must be greater or equal to Contract date' lwa_flheader-doc_date
        INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ENDIF.
*12.  INCOTERMS1
      DATA: lv_inco1 TYPE inco1.
      CLEAR: lv_inco1.
      SELECT inco1 FROM tinc INTO lv_inco1
           WHERE inco1 EQ lwa_flheader-incoterms1.
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE 'Invalid Index' lwa_flheader-incoterms1 INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ENDIF.
*13.  ZZOLDEALID
      DATA: lv_zzoldealid TYPE zoldealid.
      CLEAR: lv_zzoldealid.
      IF lwa_flheader-zzoldealid EQ ''.
        CONCATENATE 'Openlink Deal ID is blank.' lwa_flheader-zzoldealid  INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
        SELECT zzoldealid  INTO lv_zzoldealid
         FROM ekko
         WHERE zzoldealid EQ lwa_flheader-zzoldealid.   "#EC CI_NOFIELD
        ENDSELECT.
        IF sy-subrc EQ 0.
          CONCATENATE 'Openlink Deal ID is available in the system.' lwa_flheader-zzoldealid INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.
*14.  ZZEKGRP
      IF ( ( lwa_flheader-zzekgrp(1) NE 'G') OR ( ( lwa_flheader-zzekgrp+1(2) < '01' ) OR ( lwa_flheader-zzekgrp+1(2) > '99' ) ) ).
        CONCATENATE 'Trading buyer' lwa_flheader-zzekgrp 'must have a value between G01 & G99.' INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
        CLEAR: lv_ekgrp.
        SELECT ekgrp  INTO lv_ekgrp
         FROM t024
         WHERE ekgrp EQ lwa_flheader-zzekgrp.           "#EC CI_NOFIELD
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid trading buyer' lwa_flheader-zzekgrp INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
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

      IF lwa_flheader-zzmsa EQ ''.
        CONCATENATE 'Master Service Agreement is blank' lwa_flheader-zzmsa INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ELSE.
        SELECT zzmsa bukrs zzfromdate zztodate zzstatus zzmsatype  FROM zmmt_mastagree
          INTO CORRESPONDING FIELDS OF TABLE lta_zmmt_mastagree
          WHERE zzmsa EQ lwa_flheader-zzmsa.       "#EC CI_NOFIELD
        IF sy-subrc EQ 0.
          DESCRIBE TABLE lta_zmmt_mastagree LINES lv_cnt.
          IF lv_cnt > 1.
            CONCATENATE 'MSA Name is duplicated' lwa_flheader-zzmsa INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
          ELSE.
            LOOP AT lta_zmmt_mastagree INTO lwa_zmmt_mastagree.
              IF lwa_zmmt_mastagree-zzstatus NE 'A'.
                CONCATENATE 'MSA ' lwa_flheader-zzmsa 'is not active.' INTO lv_message SEPARATED BY space.
                PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
              ELSEIF (  lwa_zmmt_mastagree-zzmsatype NE 'G' ) .
                CONCATENATE 'MSA ' lwa_flheader-zzmsa 'is not a Supply MSA.' INTO lv_message SEPARATED BY space.
                PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
              ELSEIF ( ( ( lwa_zmmt_mastagree-zzfromdate => lwa_flheader-vper_start ) AND ( lwa_zmmt_mastagree-zztodate <= lwa_flheader-vper_start ) )
                OR ( ( lwa_zmmt_mastagree-zzfromdate => lwa_flheader-vper_end ) AND ( lwa_zmmt_mastagree-zztodate <= lwa_flheader-vper_end ) ) ).
                CONCATENATE 'MSA ' lwa_flheader-zzmsa 'Supply deals dates are incorrect.' INTO lv_message SEPARATED BY space.
                PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
              ELSE.
                DATA: lv_plant TYPE bukrs.
                SELECT bwkey FROM t001k INTO lv_plant
                  WHERE bwkey EQ lwa_flheader-plant
                  AND bukrs EQ lwa_zmmt_mastagree-bukrs.
                ENDSELECT.
                IF sy-subrc NE 0..
                  CONCATENATE 'MSA plant is different ' lwa_zmmt_mastagree-bukrs 'than input file' lwa_flheader-plant
              INTO lv_message SEPARATED BY space.
                  PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
*16.  ZZPARTY_AGMT_ID
      DATA: lv_zzparty_agmt_id TYPE zzparty.
      CLEAR: lv_zzparty_agmt_id.
      SELECT zzparty_agmt_id  INTO lv_zzparty_agmt_id
       FROM zmmt_mastagree
       WHERE zzparty_agmt_id EQ lwa_flheader-zzparty_agmt_id. "#EC CI_NOFIELD
      ENDSELECT.
      IF sy-subrc NE 0.
        CONCATENATE 'Invalid AGMT Party ID' lwa_flheader-zzparty_agmt_id INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ENDIF.

*17.  ZZTRLOC1
      DATA: lv_ztrloc TYPE ztrloc.
      IF lwa_flheader-zztrloc1 NE ''.
        CLEAR: lv_ztrloc.
        SELECT zztrloc FROM zmmt_locmast INTO lv_ztrloc
             WHERE zztrloc EQ lwa_flheader-zztrloc1
             AND zzparty EQ lwa_flheader-zzparty.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid location/Pipeline' lwa_flheader-zztrloc1  lwa_flheader-zzparty INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.

*18.  ZZTRLOC2
      IF lwa_flheader-zztrloc2 NE ''.
        CLEAR: lv_ztrloc.
        SELECT zztrloc FROM zmmt_locmast INTO lv_ztrloc
             WHERE zztrloc EQ lwa_flheader-zztrloc2
             AND zzparty EQ lwa_flheader-zzparty.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid location/Pipeline' lwa_flheader-zztrloc2  lwa_flheader-zzparty INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.

*19.  ZZTRLOC3
      IF lwa_flheader-zztrloc3 NE ''.
        CLEAR: lv_ztrloc.
        SELECT zztrloc FROM zmmt_locmast INTO lv_ztrloc
             WHERE zztrloc EQ lwa_flheader-zztrloc3
             AND zzparty EQ lwa_flheader-zzparty.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid location/Pipeline' lwa_flheader-zztrloc3  lwa_flheader-zzparty INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.

*20.  ZZTRLOC4
      IF lwa_flheader-zztrloc4 NE ''.
        CLEAR: lv_ztrloc.
        SELECT zztrloc FROM zmmt_locmast INTO lv_ztrloc
             WHERE zztrloc EQ lwa_flheader-zztrloc4
             AND zzparty EQ lwa_flheader-zzparty.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid location/Pipeline' lwa_flheader-zztrloc4  lwa_flheader-zzparty INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ENDIF.

*21.  CURRENCY
      IF  ( lwa_flheader-currency EQ 'USD' ) OR ( lwa_flheader-currency EQ 'CAD' ) .
        DATA: lv_waers TYPE waers.
        CLEAR: lv_waers.
        SELECT waers FROM tcurc INTO lv_waers
             WHERE waers EQ lwa_flheader-currency.
        ENDSELECT.
        IF sy-subrc NE 0.
          CONCATENATE 'Invalid currency' lwa_flheader-currency   INTO lv_message SEPARATED BY space.
          PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
        ENDIF.
      ELSE.
        CONCATENATE 'Currency' lwa_flheader-currency  'not equal to USD/CAD.' INTO lv_message SEPARATED BY space.
        PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
      ENDIF.
      ENDLOOP. " Header
****************Item Level validation******************
      LOOP AT lta_flitem INTO lwa_flitem.
        READ TABLE lta_flheader INTO lwa_flheader WITH KEY zzoldealid = lwa_flitem-zzoldealid.
        IF sy-subrc EQ 0.
*22.  PO_UNIT
          DATA:lv_mseh3 TYPE mseh3,
                lv_isocode TYPE isocd_unit.
          CLEAR: lv_mseh3, lv_isocode.
          SELECT mseh3 FROM t006a INTO lv_mseh3
               WHERE mseh3 EQ lwa_flitem-po_unit.       "#EC CI_GENBUFF
          ENDSELECT.
          IF sy-subrc NE 0.
            CONCATENATE 'Invalid PO UOM' lwa_flitem-po_unit INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
          ELSE.
            IF ( ( lwa_flitem-po_unit NE 'MMB' ) AND ( lwa_flitem-po_unit NE 'GJ' ) ).
              CONCATENATE 'UOM' lwa_flitem-po_unit 'is not MMB or GJ' INTO lv_message SEPARATED BY space.
              PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
            ELSE.
              SELECT isocode FROM t006 INTO lv_isocode
               WHERE msehi EQ lwa_flitem-po_unit
                AND isocode NE ''.
              ENDSELECT.
              IF sy-subrc NE 0 .
                CONCATENATE 'Missing ISO Code' lwa_flitem-po_unit INTO lv_message SEPARATED BY space.
                PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
              ENDIF.
            ENDIF.
          ENDIF.
*23.  TRACKINGNO
          IF lwa_flitem-trackingno EQ ''.
            CONCATENATE 'Blank tracking number' lwa_flitem-trackingno INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
          ENDIF.
*24.  MATL_GROUP
          IF lwa_flitem-matl_group EQ ''.
            CONCATENATE 'Material group is blank.' lwa_flitem-matl_group INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
          ENDIF.

*25.  TAX_CODE
          DATA: lv_text1 TYPE text1_007s.
          SELECT  text1 FROM t007s INTO lv_text1
            WHERE spras EQ 'E'
            AND kalsm EQ 'TAXCA'
            AND mwskz EQ lwa_flitem-tax_code.
          ENDSELECT.
          IF sy-subrc NE 0.
            CONCATENATE 'Invalid tax code' lwa_flitem-tax_code   INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
          ELSE.
            IF lv_text1 CS '*NOT USED*'.
              CONCATENATE 'Tax text updated as NOT USED' lwa_flitem-tax_code INTO lv_message SEPARATED BY space.
              PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
            ENDIF.
          ENDIF.
*26.  MATERIAL
          IF ( ( lwa_flitem-material NE 'NATGAS' ) AND ( lwa_flitem-material NE 'PEAKFIXED' ) ) .
            CONCATENATE 'Supply material is not NATGAS' lwa_flitem-material INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
          ENDIF.
        ENDIF.
      ENDLOOP.
****************Item Condition validation******************
*27.  COND_TYPE
      LOOP AT lta_flitemcon INTO lwa_flitemcon.
        READ TABLE lta_flheader INTO lwa_flheader WITH KEY zzoldealid = lwa_flitemcon-zzoldealid.
        IF sy-subrc EQ 0.
         TRANSLATE lwa_flitemcon-cond_type TO UPPER CASE.
          IF ( ( lwa_flitemcon-cond_type NE 'ZC01' ) AND ( lwa_flitemcon-cond_type NE 'ZC00' ) AND ( lwa_flitemcon-cond_type NE 'PBXX' ) ).
            CONCATENATE 'Invalid Condition' lwa_flitemcon-cond_type INTO lv_message SEPARATED BY space.
            PERFORM error_message_display USING lv_message lwa_flheader-zzoldealid.
          ENDIF.
        ENDIF.
      ENDLOOP.  "item condition
    SORT lta_msgout[] DESCENDING by zzoldealid.
    DELETE ADJACENT DUPLICATES FROM lta_msgout COMPARING errmsg zzoldealid.
  ELSE.
    WRITE 'No records found in the file.' .
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
    MOVE 'Validation for Create Scheduling Agreement' TO lv_title.
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
    MOVE 'Validation For Create Scheduling Agreements.' TO lr_head.
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
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_FLHEADER_VPER_END  text
*      <--P_LV_VPEREND  text
*----------------------------------------------------------------------*
FORM conversion_date  USING    p_lwa_flheader_vper_end
                      CHANGING p_lv_vperend.


  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = p_lwa_flheader_vper_end
    IMPORTING
      output = p_lv_vperend.

ENDFORM.                    " CONVERSION_DATE
