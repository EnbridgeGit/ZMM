*&---------------------------------------------------------------------*
*& Report  zlmmi048_splydeal_recon
*&
*&---------------------------------------------------------------------*
************************************************************************
*                            cogs                                      *
*&---------------------------------------------------------------------*
*& program name       :  zlmmi048_splydeal_recon                       *
*& author             :  jaydeep waychal/prashant durbhaka             *
*& creation date      :  jul 26, 2021                                  *
*& object id          :                                                *
*& application area   :  mm                                            *
*& description        :  program to get input file details to generate *
*&                       reconciliation report                         *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           modification log                           *
*----------------------------------------------------------------------*
* version no    : 1.0                                                  *
* date          :                                                      *
* modified by   :                                                      *
* correction no :                                                      *
* description   : initial program development                          *
*----------------------------------------------------------------------*

REPORT  zlmmi048_splydeal_recon MESSAGE-ID zs NO STANDARD PAGE HEADING LINE-SIZE 255.

TABLES: ekko,ekpo,zmmt_mastagree,konv.

* Types declarations *
TYPES : BEGIN OF ty_upload,
           string TYPE string,
        END OF ty_upload,

 BEGIN OF ty_source,
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
            item_no     TYPE  ebelp,           "items
            target_qty  TYPE  char13,
            po_unit     TYPE  bstme,
*            orderpr_un  type  bbprm,
            trackingno  TYPE  bednr,
            matl_group  TYPE  matkl,
            tax_code    TYPE  mwskz,
            material    TYPE  matnr,
            cond_type   TYPE  kscha,
            cond_value  TYPE  char13,          "kbetr,
     END OF ty_source,
     BEGIN OF ty_headtarget,
          ebeln1 TYPE ebeln,
          bukrs1 TYPE bukrs,
          bsart1 TYPE bsart,
          lifnr1 TYPE lifnr,
          zterm1 TYPE dzterm,
          ekorg1 TYPE ekorg,
          ekgrp1 TYPE ekgrp,
          waers1 TYPE waers,
          bedat1 TYPE bedat,
          kdatb1 TYPE kdatb,
          kdate1 TYPE kdate,
          ihrez  TYPE ihrez,
          verkf TYPE verkf,
          inco11 TYPE inco1,
          inco21 TYPE inco2,
          knumv1 TYPE knumv,
          zzcondayqty1      TYPE z_condayqty,
          zzconprice1       TYPE z_conprice,
          zzparty_agmt_id1  TYPE zzparty,
          zztrloc11     TYPE ztrloc,
          zztrloc21     TYPE ztrloc2,
          zztrloc31     TYPE ztrloc3,
          zztrloc41     TYPE ztrloc4,
          zzparty1     TYPE zzparty,
          zzekgrp1      TYPE ztrbuy,
          zzoldealid1   TYPE zoldealid,
     END OF ty_headtarget,

     BEGIN OF ty_itemtarget,
          ebeln1 TYPE ebeln,
          ebelp1 TYPE ebelp,
          matnr1  TYPE matnr,
          werks1  TYPE werks,
          lgort1  TYPE lgort_d,
          bednr1  TYPE bednr,
          matkl1  TYPE matkl,
          ktmng1 TYPE ktmng,
          meins1  TYPE bstme,
          mwskz1  TYPE mwskz,
     END OF ty_itemtarget,

     BEGIN OF ty_itemcon_target,
            knumv TYPE  knumv,
            kposn TYPE kposn,
            kschl1  TYPE kscha,
            kbetr1  TYPE kbetr,
            waers1  TYPE waers,
     END OF ty_itemcon_target,

     BEGIN OF ty_mastagree_target,
       zzparty_agmt_id  TYPE zzparty,
       zzmsa TYPE zzmsa,
       END OF ty_mastagree_target,

     BEGIN OF ty_final,
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
*            verkf       TYPE verkf,
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
            item_no     TYPE  ebelp,           "items
            target_qty  TYPE  char13,
            po_unit     TYPE  bstme,
            trackingno  TYPE  bednr,
            matl_group  TYPE  matkl,
            tax_code    TYPE  mwskz,
            material    TYPE  matnr,
            cond_type   TYPE  kscha,            "condition type
            cond_value  TYPE  kbetr, "bapikbetr1,
*****target recors
            ebeln1 TYPE ebeln,
            bsart1 TYPE bsart,
            werks1 TYPE werks_d,    "item level data
            lgort1 TYPE lgort_d,    "item level data
            bukrs1 TYPE bukrs,
            lifnr1 TYPE lifnr,
            zterm1 TYPE dzterm,
            ekorg1 TYPE ekorg,
            ekgrp1 TYPE ekgrp,
            kdate1 TYPE kdate,
            kdatb1 TYPE kdatb,
            bedat1 TYPE bedat,
            ihrez  TYPE  ihrez,
            inco11 TYPE inco1,
            inco21 TYPE inco2,
            zzoldealid1   TYPE zoldealid,
            zzekgrp1      TYPE ztrbuy,
            zzmsa1        TYPE  zzmsa,
            zzparty_agmt_id1  TYPE zzparty,
            zztrloc11     TYPE ztrloc,
            zztrloc21     TYPE ztrloc2,
            zztrloc31     TYPE ztrloc3,
            zztrloc41     TYPE ztrloc4,
            zzparty1      TYPE zzparty,
            zzcondayqty1  TYPE z_condayqty,
            zzconprice1   TYPE z_conprice,
            waers1        TYPE waers,          "condition type
            ebelp1  TYPE ebelp,
            ktmng1  TYPE ktmng,
            meins1  TYPE bstme,
            bednr1  TYPE bednr,
            matkl1  TYPE matkl,
            mwskz1  TYPE mwskz,
            matnr1  TYPE matnr,
            kschl1  TYPE kscha,
            kbetr1  TYPE kbetr,
***recon status
            ebeln2 TYPE ebeln,
            bsart2 TYPE bsart,
            werks2 TYPE werks_d,      "item level data
            lgort2 TYPE lgort_d,    "item level data
            bukrs2 TYPE bukrs,
            lifnr2 TYPE lifnr,
            zterm2 TYPE dzterm,
            ekorg2 TYPE ekorg,
            ekgrp2 TYPE ekgrp,
            kdate2 TYPE char3,"kdate,
            kdatb2 TYPE char3,"kdatb,
            bedat2 TYPE char3,"bedat,
            inco12 TYPE inco1,
            inco22 TYPE inco2,
            zzoldealid2   TYPE zoldealid,
            zzekgrp2      TYPE ztrbuy,
            zzmsa2        TYPE  zzmsa,
            zzparty_agmt_id2  TYPE zzparty,
            zztrloc12    TYPE ztrloc,
            zztrloc22     TYPE ztrloc2,
            zztrloc32     TYPE ztrloc3,
            zztrloc42     TYPE ztrloc4,
            zzparty2      TYPE zzparty,
            zzcondayqty2  TYPE char3,
            zzconprice2   TYPE char3,
            waers2  TYPE char3,          "condition type
            ebelp2  TYPE char3,
            ktmng2  TYPE char3,
            meins2  TYPE char3,
            bednr2  TYPE char3,
            matkl2  TYPE char3,
            mwskz2  TYPE char3,
            matnr2  TYPE char3,
            kschl2  TYPE char3,
            kbetr2  TYPE char3,
            waers3  TYPE char3,
      END OF ty_final,

      BEGIN OF ty_msgout,
          zzoldealid  TYPE  zoldealid,
          errmsg(2000) TYPE c,
          errtyp(1)    TYPE c,  "s - success , e-error
      END OF ty_msgout,

*deal id to get ekko details
       BEGIN OF ty_dealid,
            zzoldealid  TYPE  zoldealid,
            item_no     TYPE  ebelp,
       END OF ty_dealid,

       BEGIN OF ty_sapdeals,
            ebeln TYPE ebeln,
            zzoldealid TYPE zoldealid,
            verkf TYPE everk,
       END OF ty_sapdeals,

      BEGIN OF ty_header,
        field1 TYPE c LENGTH 30,
      END OF ty_header.

***Internal table declarations *.
DATA: lta_upload       TYPE STANDARD TABLE OF ty_upload,
      lta_source       TYPE STANDARD TABLE OF ty_source,
      lta_head         TYPE STANDARD TABLE OF ty_headtarget,
      lta_item         TYPE STANDARD TABLE OF ty_itemtarget,
      lta_itemcon      TYPE STANDARD TABLE OF ty_itemcon_target,
      lta_mastagree    TYPE STANDARD TABLE OF ty_mastagree_target,
      lta_final        TYPE STANDARD TABLE OF ty_final,
*      lta_final_header TYPE STANDARD TABLE OF ty_final,
      lta_dealid       TYPE STANDARD TABLE OF ty_dealid,
      lta_sapdeals     TYPE STANDARD TABLE OF ty_sapdeals,
      lta_msgout       TYPE STANDARD TABLE OF ty_msgout,
      lta_final_header TYPE STANDARD TABLE OF ty_header.

* Work area declarations *
DATA: lwa_upload       TYPE ty_upload,
      lwa_source       TYPE ty_source,
      lwa_head         TYPE ty_headtarget,
      lwa_item         TYPE ty_itemtarget,
      lwa_itemcon      TYPE ty_itemcon_target,
      lwa_mastagree    TYPE ty_mastagree_target,
      lwa_target       TYPE ty_source,
      lwa_dealid       TYPE ty_dealid,
      lwa_sapdeals     TYPE ty_sapdeals,
      lwa_final        TYPE ty_final,
      lwa_msgout       TYPE ty_msgout,
      header           TYPE ty_header.

* Variable declarations *
DATA: lv_sep(1)      TYPE c VALUE ',',
      lv_tstflg      TYPE c,
      lv_errtype     TYPE c,
      lv_cnt(5)      TYPE n,
      lv_message(2000) TYPE c,
      lv_path        TYPE btch0000-text80.

************************************************************************
* Selection screen *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETER: p_recfl TYPE rlgrap-filename.
PARAMETER: p_outfl TYPE rlgrap-filename DEFAULT 'c:\saptemp\dm7_reconciliation_report.xls'.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* At selection-screen processes
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_recfl.
*-- Form to select a file from a particular location
  PERFORM get_filename_f4 USING p_recfl.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_outfl.
*-- Form to select a file from a particular location
  PERFORM get_filename_f4 USING p_outfl.

AT SELECTION-SCREEN ON p_outfl.
  IF p_outfl IS NOT INITIAL.
    PERFORM check_file_path.
  ENDIF.
************************************************************************
* Start-of-selection *
************************************************************************
START-OF-SELECTION.
  IF  p_recfl IS NOT INITIAL.
*** To upload flat file data into the internal table.
    PERFORM file_upload.
  ELSE.
    MESSAGE i019 WITH text-005.
  ENDIF.

  IF  p_outfl IS NOT INITIAL.
*** Split data from input file into an internal table
    PERFORM split_data.
    IF lta_source IS NOT INITIAL.
      PERFORM generate_report.
    ELSE.
      MESSAGE i019 WITH text-006.
    ENDIF.
    IF lta_final IS NOT INITIAL.
      PERFORM prepare_header.
      PERFORM download_recon.
    ENDIF.
  ELSE.
    MESSAGE i019 WITH text-007.
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
ENDFORM.                    " Get_filename_f4

*&---------------------------------------------------------------------*
*&      form  file_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM file_upload .

  DATA : lv_file TYPE string.

  lv_file = p_recfl.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file
*     filetype                = 'asc'
*     has_field_separator     = 'x'
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
ENDFORM.                    " File_upload
*&---------------------------------------------------------------------*
*&      form  split_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM split_data .
**Moving data into internal table to validate
  IF lta_upload IS NOT INITIAL.
    LOOP AT lta_upload INTO lwa_upload.
      SPLIT lwa_upload-string AT lv_sep INTO
                              lwa_source-doc_type
                              lwa_source-plant
                              lwa_source-stge_loc
                              lwa_source-comp_code
                              lwa_source-lifnr
                              lwa_source-pmnttrms
                              lwa_source-purch_org
                              lwa_source-pur_group
                              lwa_source-vper_end
                              lwa_source-vper_start
                              lwa_source-doc_date
                              lwa_source-incoterms1
                              lwa_source-incoterms2
                              lwa_source-zzoldealid
                              lwa_source-zzekgrp
                              lwa_source-zzmsa
                              lwa_source-zzparty_agmt_id
                              lwa_source-zztrloc1
                              lwa_source-zztrloc2
                              lwa_source-zztrloc3
                              lwa_source-zztrloc4
                              lwa_source-zzparty
                              lwa_source-zzcondayqty
                              lwa_source-zzconprice
                              lwa_source-currency
                              lwa_source-item_no          " item
                              lwa_source-target_qty
                              lwa_source-po_unit
*                              lwa_source-orderpr_un
                              lwa_source-trackingno
                              lwa_source-matl_group
                              lwa_source-tax_code
                              lwa_source-material
                              lwa_source-cond_type        " conditions
                              lwa_source-cond_value.
      APPEND lwa_source TO lta_source.
**Collect deal id's and item no.
      lwa_dealid-zzoldealid = lwa_source-zzoldealid.
      lwa_dealid-item_no = lwa_source-item_no.
      APPEND lwa_dealid TO lta_dealid.
      CLEAR : lwa_upload, lwa_dealid.
    ENDLOOP.
    DELETE lta_source INDEX 1.                             "Remove header line
  ENDIF.
ENDFORM.                    " Split_data
*&---------------------------------------------------------------------*
*&      form  check_oldealid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_lwa_source_zzoldealid  text
*      <--p_lta_target  text
*----------------------------------------------------------------------*
FORM check_oldealid  USING    p_lwa_source_zzoldealid
                              p_item_no
                     CHANGING p_lta_target
                              p_lta_itemtarget
                              p_lta_itemcon_target.

  DATA: item_no TYPE ebelp.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_item_no
    IMPORTING
      output = item_no.

***Get details from ekko, ekpo and konv table
  SELECT  ebeln
          bukrs
          bsart
          lifnr
          zterm
          ekorg
          ekgrp
          waers
          bedat
          kdatb
          kdate
          ihrez
          verkf
          inco1
          inco2
          knumv
          zzcondayqty
          zzconprice
          zzparty_agmt_id
          zztrloc1
          zztrloc2
          zztrloc3
          zztrloc4
          zzparty
          zzekgrp
          zzoldealid
    FROM ekko INTO TABLE lta_head
*    WHERE zzoldealid EQ p_lwa_source_zzoldealid.
    WHERE verkf EQ p_lwa_source_zzoldealid.        "#Ec ci_nofield
  IF sy-subrc = 0.
*    p_lta_target  = lta_head.
    SELECT  ebeln
            ebelp
            matnr
            werks
            lgort
            bednr
            matkl
            ktmng
            meins
            mwskz
      FROM ekpo INTO TABLE lta_item
      FOR ALL ENTRIES IN lta_head
      WHERE ebeln EQ lta_head-ebeln1
      AND  ebelp EQ item_no.
    IF sy-subrc = 0.
*      p_lta_itemtarget = lta_item.
      SELECT knumv kposn kschl kbetr waers
       FROM konv INTO TABLE lta_itemcon
        FOR ALL ENTRIES IN lta_head
        WHERE knumv EQ lta_head-knumv1
        AND kposn EQ item_no
        AND ( ( kschl EQ 'PBXX' ) OR ( kschl EQ 'ZC01' ) OR ( kschl EQ 'ZC00' ) )
        AND kbetr NE 0.
      IF sy-subrc = 0.
*        p_lta_itemcon_target = lta_itemcon.
*    Sort lt_target by ebeln ebelp.
      ENDIF.
    ENDIF.
    SELECT zzparty_agmt_id zzmsa FROM zmmt_mastagree
      INTO TABLE lta_mastagree
          FOR ALL ENTRIES IN lta_head
          WHERE zzparty_agmt_id EQ lta_head-zzparty_agmt_id1. "#EC CI_NOFIELD
    IF sy-subrc EQ 0.
*WRITE ://.
    ENDIF.
  ENDIF.
ENDFORM.                    " Check_oldealid
*&---------------------------------------------------------------------*
*&      form  download_recon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*2
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_recon .

  DATA:lv_flname TYPE string.

  lv_flname = p_outfl.

*download file in excel
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      bin_filesize        = ''
      filename            = lv_flname
      filetype            = 'DAT'
    TABLES
      data_tab            = lta_final
      fieldnames          = lta_final_header
    EXCEPTIONS
      file_open_error     = 1
      file_write_error    = 2
      invalid_filesize    = 3
      invalid_table_width = 4
      invalid_type        = 5
      no_batch            = 6
      unknown_error       = 7
      OTHERS              = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " Download_recon
*&---------------------------------------------------------------------*
*&      form  check_file_path
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_file_path .
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "Abap_bool.

*Separate path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_outfl
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Message id sy-msgid type sy-msgty number sy-msgno
*         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF sep_path CS 'c:' OR sep_path CS 'c:'.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
* Message id sy-msgid type sy-msgty number sy-msgno
*         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.                    " Check_file_path
*&---------------------------------------------------------------------*
*&      form  check_field_mapping
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_lwa_head  text
*      -->p_lwa_source  text
*      <--p_lv_match  text
*----------------------------------------------------------------------*
FORM check_field_mapping  USING    p_lwa_head
                                   p_lwa_source
                          CHANGING p_lv_match.
  IF p_lwa_head EQ p_lwa_source.
    p_lv_match = 'YES'.
  ELSE.
    p_lv_match = 'NO'.
  ENDIF.
ENDFORM.                    " Check_field_mapping
*&---------------------------------------------------------------------*
*&      form  generate_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_report .

  DATA: lv_match(10) TYPE c,
        lv_strpric  TYPE string,
        lv_strpric1 TYPE string.

  IF lta_source IS NOT INITIAL.
    LOOP AT lta_source INTO lwa_source.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_source-lifnr
        IMPORTING
          output = lwa_source-lifnr.
*Ignore the deals that have already been created in sap
      READ TABLE lta_dealid INTO lwa_dealid WITH KEY zzoldealid = lwa_source-zzoldealid
                                                     item_no    = lwa_source-item_no.
      IF sy-subrc EQ 0.
**Check all the deal id present in the ekko table and ekpo table
        PERFORM check_oldealid USING lwa_source-zzoldealid
                                     lwa_source-item_no CHANGING lta_head
                                                                 lta_item
                                                                 lta_itemcon.
        READ TABLE lta_head INTO lwa_head WITH KEY verkf = lwa_source-zzoldealid.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lwa_head TO   lwa_final.
          MOVE-CORRESPONDING lwa_source TO lwa_final.
          lwa_final-ebeln2 = lwa_head-ebeln1.
          PERFORM check_field_mapping USING lwa_head-bsart1 lwa_source-doc_type CHANGING lv_match.
          lwa_final-bsart2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-bukrs1 lwa_source-comp_code CHANGING lv_match.
          lwa_final-bukrs2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-lifnr1 lwa_source-lifnr CHANGING lv_match.
          lwa_final-lifnr2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-zterm1 lwa_source-pmnttrms CHANGING lv_match.
          lwa_final-zterm2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-ekorg1 lwa_source-purch_org CHANGING lv_match.
          lwa_final-ekorg2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-waers1 lwa_source-currency CHANGING lv_match.
          lwa_final-waers2  = lv_match.
          PERFORM check_field_mapping USING lwa_head-ekgrp1 lwa_source-pur_group CHANGING lv_match.
          lwa_final-ekgrp2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-kdate1 lwa_source-vper_end CHANGING lv_match.
          lwa_final-kdate2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-kdatb1 lwa_source-vper_start CHANGING lv_match.
          lwa_final-kdatb2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-bedat1 lwa_source-doc_date CHANGING lv_match.
          lwa_final-bedat2 = lv_match.

          PERFORM check_field_mapping USING lwa_head-ihrez '12345' CHANGING lv_match.
*          lwa_final-ihrez = lv_match.

          PERFORM check_field_mapping USING lwa_head-inco11 lwa_source-incoterms1 CHANGING lv_match.
          lwa_final-inco12 = lv_match.
          PERFORM check_field_mapping USING lwa_head-inco21 lwa_source-incoterms2 CHANGING lv_match.
          lwa_final-inco22 = lv_match.
          IF lwa_head-verkf IS NOT INITIAL.
            PERFORM check_field_mapping USING lwa_head-verkf lwa_source-zzoldealid CHANGING lv_match.
            lwa_final-zzoldealid1 = lwa_head-verkf.
            lwa_final-zzoldealid2 = lv_match.
          ENDIF.
          PERFORM check_field_mapping USING lwa_head-zzekgrp1 lwa_source-zzekgrp CHANGING lv_match.
          lwa_final-zzekgrp2 = lv_match.
          IF lta_mastagree IS NOT INITIAL.
            READ TABLE lta_mastagree INTO lwa_mastagree WITH KEY zzmsa = lwa_source-zzmsa.
            IF sy-subrc EQ 0.
              PERFORM check_field_mapping USING lwa_mastagree-zzmsa lwa_source-zzmsa CHANGING lv_match.
              lwa_final-zzmsa1 = lwa_mastagree-zzmsa.
              lwa_final-zzmsa2 = lv_match.
              PERFORM check_field_mapping USING lwa_head-zzparty_agmt_id1 lwa_source-zzparty_agmt_id CHANGING lv_match.
              lwa_final-zzparty_agmt_id2 = lv_match.
            ENDIF.
          ENDIF.
          PERFORM check_field_mapping USING lwa_head-zztrloc11 lwa_source-zztrloc1 CHANGING lv_match.
          lwa_final-zztrloc12 = lv_match.
          PERFORM check_field_mapping USING lwa_head-zztrloc21 lwa_source-zztrloc2 CHANGING lv_match.
          lwa_final-zztrloc22 = lv_match.
          PERFORM check_field_mapping USING lwa_head-zztrloc31 lwa_source-zztrloc3 CHANGING lv_match.
          lwa_final-zztrloc32 = lv_match.
          PERFORM check_field_mapping USING lwa_head-zztrloc41 lwa_source-zztrloc4 CHANGING lv_match.
          lwa_final-zztrloc42 = lv_match.
          PERFORM check_field_mapping USING lwa_head-zzparty1 lwa_source-zzparty CHANGING lv_match.
          lwa_final-zzparty2 = lv_match.
          TRANSLATE lwa_head-zzcondayqty1 TO UPPER CASE.
          TRANSLATE lwa_source-zzcondayqty TO UPPER CASE.
          PERFORM check_field_mapping USING lwa_head-zzcondayqty1 lwa_source-zzcondayqty CHANGING lv_match.
          lwa_final-zzcondayqty2 = lv_match.
          IF lwa_source-zzconprice IS NOT INITIAL.
            CLEAR: lv_strpric, lv_strpric1.
            SPLIT lwa_head-zzconprice1 AT space INTO lv_strpric lv_strpric1.
            PERFORM check_field_mapping USING lv_strpric lwa_source-zzconprice CHANGING lv_match.
            lwa_final-zzconprice2  = lv_match.
          ELSE.
            lwa_final-zzconprice2  = 'YES'.
          ENDIF.
**Item
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_source-item_no
            IMPORTING
              output = lwa_source-item_no.

          READ TABLE lta_item INTO lwa_item WITH KEY ebeln1  = lwa_head-ebeln1
                                                     ebelp1  = lwa_source-item_no.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING lwa_item TO   lwa_final.
            PERFORM check_field_mapping USING lwa_item-ebelp1 lwa_source-item_no CHANGING lv_match.
            lwa_final-ebelp2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-werks1 lwa_source-plant CHANGING lv_match.
            lwa_final-werks2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-lgort1 lwa_source-stge_loc CHANGING lv_match.
            lwa_final-lgort2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-ktmng1 lwa_source-target_qty CHANGING lv_match.
            lwa_final-ktmng2  = lv_match.
            IF lwa_item-meins1 EQ 'GJ1'.
              lwa_item-meins1 = 'GJ'.
            ENDIF.
            PERFORM check_field_mapping USING lwa_item-meins1 lwa_source-po_unit CHANGING lv_match.
            lwa_final-meins2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-bednr1 lwa_source-trackingno CHANGING lv_match.
            lwa_final-bednr2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-matkl1 lwa_source-matl_group CHANGING lv_match.
            lwa_final-matkl2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-mwskz1 lwa_source-tax_code CHANGING lv_match.
            lwa_final-mwskz2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-matnr1 lwa_source-material CHANGING lv_match.
            lwa_final-matnr2  = lv_match.
          ENDIF.
          READ TABLE lta_itemcon INTO lwa_itemcon WITH KEY knumv = lwa_head-knumv1
                                                           kposn = lwa_source-item_no.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING lwa_itemcon TO   lwa_final.
            PERFORM check_field_mapping USING lwa_itemcon-waers1 lwa_head-waers1 CHANGING lv_match.
            lwa_final-waers3  = lv_match.
            PERFORM check_field_mapping USING lwa_itemcon-kschl1 lwa_source-cond_type CHANGING lv_match.
            lwa_final-kschl2  = lv_match.
            PERFORM check_field_mapping USING lwa_itemcon-kbetr1 lwa_source-cond_value CHANGING lv_match.
            lwa_final-kbetr2  = lv_match.
          ELSE.
            MOVE-CORRESPONDING lwa_itemcon TO lwa_final.
            CLEAR : lv_match.
            IF ( lwa_source-cond_value EQ 0 OR lwa_source-cond_value IS INITIAL ).
              lv_match = 'YES'.
            ELSE.
              lv_match = 'NO'.
            ENDIF.
            lwa_final-waers3  = lv_match.
            lwa_final-kschl2  = lv_match.
            lwa_final-kbetr2  = lv_match.
          ENDIF.
          APPEND lwa_final TO lta_final.
        ELSE.
          WRITE:/ lwa_source-zzoldealid,  text-004.
          MOVE-CORRESPONDING lwa_source TO lwa_final.
          lv_match = 'Not in SAP'.
          lwa_final-ebeln1 = lv_match.
          lwa_final-ebeln2 = lv_match.
          APPEND lwa_final TO lta_final.
        ENDIF.
      ENDIF.
*        Refresh: lta_head, lta_item, lta_itemcon.
      CLEAR : lwa_source, lwa_dealid, lwa_final, lwa_head,lwa_item, lwa_itemcon.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " Generate_report
*&---------------------------------------------------------------------*
*&      Form  PREPARE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_header.

*  DATA: BEGIN OF header OCCURS 0,
*          field1 TYPE char30,
*        END OF header.

  header-field1 = 'DOC_TYPE'.    APPEND header TO lta_final_header.
  header-field1 = 'PLANT'.       APPEND header TO lta_final_header.
  header-field1 = 'STGE_LOC'.    APPEND header TO lta_final_header.
  header-field1 = 'COMP_CODE'.   APPEND header TO lta_final_header.
  header-field1 = 'LIFNR'.       APPEND header TO lta_final_header.
  header-field1 = 'PMNTTRMS'.    APPEND header TO lta_final_header.
  header-field1 = 'PURCH_ORG'.   APPEND header TO lta_final_header.
  header-field1 = 'PUR_GROUP'.   APPEND header TO lta_final_header.
  header-field1 = 'VPER_END'.    APPEND header TO lta_final_header.
  header-field1 = 'VPER_START'.  APPEND header TO lta_final_header.
  header-field1 = 'DOC_DATE'.    APPEND header TO lta_final_header.
  header-field1 = 'INCOTERMS1'.  APPEND header TO lta_final_header.
  header-field1 = 'INCOTERMS2'.  APPEND header TO lta_final_header.
  header-field1 = 'ZZOLDEALID'.  APPEND header TO lta_final_header.
  header-field1 = 'ZZEKGRP'.     APPEND header TO lta_final_header.
  header-field1 = 'ZZMSA'.       APPEND header TO lta_final_header.
  header-field1 = 'ZZPARTY_AGMT_ID'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC1'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC2'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC3'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC4'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZPARTY'.     APPEND header TO lta_final_header.
  header-field1 = 'ZZCONDAYQTY'. APPEND header TO lta_final_header.
  header-field1 = 'ZZCONPRICE'.  APPEND header TO lta_final_header.
  header-field1 = 'CURRENCY'.    APPEND header TO lta_final_header.
  header-field1 = 'ITEM_NO'.     APPEND header TO lta_final_header.
  header-field1 = 'TARGET_QTY'.  APPEND header TO lta_final_header.
  header-field1 = 'PO_UNIT'.     APPEND header TO lta_final_header.
  header-field1 = 'TRACKINGNO'.  APPEND header TO lta_final_header.
  header-field1 = 'MATL_GROUP'.  APPEND header TO lta_final_header.
  header-field1 = 'TAX_CODE'.    APPEND header TO lta_final_header.
  header-field1 = 'MATERIAL'.    APPEND header TO lta_final_header.
  header-field1 = 'COND_TYPE'.   APPEND header TO lta_final_header.
  header-field1 = 'COND_VALUE'.  APPEND header TO lta_final_header.

  header-field1 = 'SAG'.    APPEND header TO lta_final_header.
  header-field1 = 'DOC_TYPE'.    APPEND header TO lta_final_header.
  header-field1 = 'PLANT'.       APPEND header TO lta_final_header.
  header-field1 = 'STGE_LOC'.    APPEND header TO lta_final_header.
  header-field1 = 'COMP_CODE'.   APPEND header TO lta_final_header.
  header-field1 = 'LIFNR'.       APPEND header TO lta_final_header.
  header-field1 = 'PMNTTRMS'.    APPEND header TO lta_final_header.
  header-field1 = 'PURCH_ORG'.   APPEND header TO lta_final_header.
  header-field1 = 'PUR_GROUP'.   APPEND header TO lta_final_header.
  header-field1 = 'VPER_END'.    APPEND header TO lta_final_header.
  header-field1 = 'VPER_START'.  APPEND header TO lta_final_header.
  header-field1 = 'DOC_DATE'.    APPEND header TO lta_final_header.
  header-field1 = 'YOUR_REFER'.  APPEND header TO lta_final_header.
  header-field1 = 'INCOTERMS1'.  APPEND header TO lta_final_header.
  header-field1 = 'INCOTERMS2'.  APPEND header TO lta_final_header.
  header-field1 = 'VERKF/ZZOLDEALID'.  APPEND header TO lta_final_header.
  header-field1 = 'ZZEKGRP'.     APPEND header TO lta_final_header.
  header-field1 = 'ZZMSA'.       APPEND header TO lta_final_header.
  header-field1 = 'ZZPARTY_AGMT_ID'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC1'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC2'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC3'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC4'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZPARTY'.     APPEND header TO lta_final_header.
  header-field1 = 'ZZCONDAYQTY'. APPEND header TO lta_final_header.
  header-field1 = 'ZZCONPRICE'.  APPEND header TO lta_final_header.
  header-field1 = 'CURRENCY'.    APPEND header TO lta_final_header.
  header-field1 = 'ITEM_NO'.     APPEND header TO lta_final_header.
  header-field1 = 'TARGET_QTY'.  APPEND header TO lta_final_header.
  header-field1 = 'PO_UNIT'.     APPEND header TO lta_final_header.
  header-field1 = 'TRACKINGNO'.  APPEND header TO lta_final_header.
  header-field1 = 'MATL_GROUP'.  APPEND header TO lta_final_header.
  header-field1 = 'TAX_CODE'.    APPEND header TO lta_final_header.
  header-field1 = 'MATERIAL'.    APPEND header TO lta_final_header.
  header-field1 = 'COND_TYPE'.   APPEND header TO lta_final_header.
  header-field1 = 'COND_VALUE'.  APPEND header TO lta_final_header.

  header-field1 = 'SAG'.    APPEND header TO lta_final_header.
  header-field1 = 'DOC_TYPE'.    APPEND header TO lta_final_header.
  header-field1 = 'PLANT'.       APPEND header TO lta_final_header.
  header-field1 = 'STGE_LOC'.    APPEND header TO lta_final_header.
  header-field1 = 'COMP_CODE'.   APPEND header TO lta_final_header.
  header-field1 = 'LIFNR'.       APPEND header TO lta_final_header.
  header-field1 = 'PMNTTRMS'.    APPEND header TO lta_final_header.
  header-field1 = 'PURCH_ORG'.   APPEND header TO lta_final_header.
  header-field1 = 'PUR_GROUP'.   APPEND header TO lta_final_header.
  header-field1 = 'VPER_END'.    APPEND header TO lta_final_header.
  header-field1 = 'VPER_START'.  APPEND header TO lta_final_header.
  header-field1 = 'DOC_DATE'.    APPEND header TO lta_final_header.
*  header-field1 = 'YOUR_REFER'.  APPEND header TO lta_final_header.
  header-field1 = 'INCOTERMS1'.  APPEND header TO lta_final_header.
  header-field1 = 'INCOTERMS2'.  APPEND header TO lta_final_header.
  header-field1 = 'ZZOLDEALID'.  APPEND header TO lta_final_header.
  header-field1 = 'ZZEKGRP'.     APPEND header TO lta_final_header.
  header-field1 = 'ZZMSA'.       APPEND header TO lta_final_header.
  header-field1 = 'ZZPARTY_AGMT_ID'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC1'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC2'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC3'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZTRLOC4'.    APPEND header TO lta_final_header.
  header-field1 = 'ZZPARTY'.     APPEND header TO lta_final_header.
  header-field1 = 'ZZCONDAYQTY'. APPEND header TO lta_final_header.
  header-field1 = 'ZZCONPRICE'.  APPEND header TO lta_final_header.
  header-field1 = 'CURRENCY'.    APPEND header TO lta_final_header.
  header-field1 = 'ITEM_NO'.     APPEND header TO lta_final_header.
  header-field1 = 'TARGET_QTY'.  APPEND header TO lta_final_header.
  header-field1 = 'PO_UNIT'.     APPEND header TO lta_final_header.
  header-field1 = 'TRACKINGNO'.  APPEND header TO lta_final_header.
  header-field1 = 'MATL_GROUP'.  APPEND header TO lta_final_header.
  header-field1 = 'TAX_CODE'.    APPEND header TO lta_final_header.
  header-field1 = 'MATERIAL'.    APPEND header TO lta_final_header.
  header-field1 = 'COND_TYPE'.   APPEND header TO lta_final_header.
  header-field1 = 'COND_VALUE'.  APPEND header TO lta_final_header.
  header-field1 = 'CURRENCY'.    APPEND header TO lta_final_header.

ENDFORM.                    " PREPARE_HEADER
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_SOURCE_COND_VALUE  text
*      <--P_LWA_SOURCE_COND_VALUE  text
*----------------------------------------------------------------------*
FORM conversion_exist  USING    p_lwa_source_cond_value
                       CHANGING p_lwa_source_cond_value1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_lwa_source_cond_value
    IMPORTING
      output = p_lwa_source_cond_value1.

ENDFORM.                    " CONVERSION_EXIST
