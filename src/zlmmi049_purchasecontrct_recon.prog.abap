*&---------------------------------------------------------------------*
*& REPORT  ZLMM_PURCONTRCT_VALID_RECON
*&
*&---------------------------------------------------------------------*
************************************************************************
*                            COGS                                      *
*&---------------------------------------------------------------------*
*& PROGRAM NAME       :  ZLMM_PURCONTRCT_VALID_RECON                   *
*& AUTHOR             :  JAYDEEP WAYCHAL/PRASHANT DURBHAKA             *
*& CREATION DATE      :  JUN 26, 2021                                  *
*& OBJECT ID          :                                                *
*& APPLICATION AREA   :  MM                                            *
*& DESCRIPTION        :  PROGRAM TO READ INPUT FILE DETAILS TO FOR     *
*&                       PURCHASE CONTRACT AND GENERATE RECON REPORT   *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           MODIFICATION LOG                           *
*----------------------------------------------------------------------*
* VERSION NO    : 1.0                                                  *
* DATE          :                                                      *
* MODIFIED BY   :                                                      *
* CORRECTION NO :                                                      *
* DESCRIPTION   : INITIAL PROGRAM DEVELOPMENT                          *
*----------------------------------------------------------------------*

REPORT  zlmmi047_prcontrct_validation MESSAGE-ID zs.

TABLES: ekko,ekpo,zmmt_mastagree ,konv.

* TYPES DECLARATIONS *
TYPES : BEGIN OF ty_upload,
           string TYPE string,
        END OF ty_upload,

      BEGIN OF ty_source,
            vendor      TYPE  elifn, " HEADER
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
            item_no     TYPE  ebelp,   " ITEM
            tax_code    TYPE  mwskz,
            itm_txt     TYPE  txz01,
            trackingno  TYPE  bednr,
            price_cat   TYPE  meprf,
            target_qty  TYPE  char13,
            ser_num     TYPE srvpos,
            price(16)   TYPE c,  "SBRTWR,
            qty_per     TYPE char13,
            uom         TYPE meins,
            price_unit  TYPE char13, "PEINH,
            cost_center TYPE kostl,
            glaccount   TYPE sakto,
            userf2_num  TYPE char17, "USERF2_NUM,
            userf1_txt  TYPE userf1_txt,
        END OF ty_source,

        BEGIN OF ty_headtarget,
            ebeln1 TYPE  ebeln,
            bukrs1 TYPE  bukrs,
            bsart1 TYPE  bsart,
            lifnr1 TYPE  elifn,
            zterm1 TYPE  dzterm,
            ekorg1 TYPE  ekorg,
            ekgrp1 TYPE  bkgrp,
            waers1 TYPE  waers,
            bedat1 TYPE  aedat,
            kdatb1 TYPE  kdatb,
            kdate1 TYPE  kdate,
            verkf  TYPE  verkf,
            inco11 TYPE  inco1,
            zzcondayqty1 TYPE  z_condayqty,
            zzparty_agmt_id1 TYPE  zzparty,
            zzekgrp1     TYPE  ztrbuy,
            zzoldealid1  TYPE  zoldealid,
          END OF ty_headtarget,

          BEGIN OF ty_itemtarget,
            ebeln1 TYPE ebeln,
            ebelp1 TYPE ebelp,
            txz011 TYPE txz01,
            matnr1 TYPE matnr,
            werks1 TYPE werks,
            lgort1 TYPE lgort_d,
            bednr1 TYPE bednr,
            matkl1 TYPE matkl,
            ktmng1 TYPE ktmng,
            menge1 TYPE bstmg,
            mwskz1 TYPE mwskz,
            pstyp1 TYPE pstyp,
            knttp1  TYPE knttp,
            meprf1 TYPE meprf,
            packno TYPE packno,
          END OF ty_itemtarget,

          BEGIN OF ty_mastagree_target,
            zzparty_agmt_id  TYPE zzparty,
            zzmsa TYPE zzmsa,
          END OF ty_mastagree_target,

          BEGIN OF ty_eslltarget,
            packno     TYPE packno,
            introw     TYPE introw,
            srvpos     TYPE srvpos,
            sub_packno TYPE sub_packno,
*            qty_per1   TYPE menge,
*            meins1     TYPE meins,
*            price_unit1 TYPE peinh,
*            price1      TYPE brtwr,
*            userf2_num1 TYPE userf2_num,
*            userf1_txt1 TYPE userf1_txt,
          END OF ty_eslltarget,

          BEGIN OF ty_eslltarget1,
            packno     TYPE packno,
            introw     TYPE introw,
            extrow     TYPE extrow,
            srvpos     TYPE srvpos,
            sub_packno TYPE sub_packno,
            menge     TYPE mengev,
            meins     TYPE meins,
            peinh     TYPE peinh,
            brtwr     TYPE brtwr,
            userf2_num TYPE userf2_num,
            userf1_txt TYPE userf1_txt,
          END OF ty_eslltarget1,

          BEGIN OF ty_eskltarget,
            packno TYPE packno,
            introw TYPE introw,
            zekkn  TYPE dzekkn,
          END OF ty_eskltarget,

           BEGIN OF ty_ekkntarget,
             ebeln TYPE ebeln,
             ebelp TYPE ebelp,
             zekkn TYPE dzekkn,
             sakto TYPE sakto,  "glaccount
             kostl TYPE kostl,  "cost_center
          END OF ty_ekkntarget,

          BEGIN OF ty_header,
        field1 TYPE c LENGTH 30,
      END OF ty_header.

TYPES:  BEGIN OF ty_final,
            vendor      TYPE  elifn, " HEADER
            cont_typ    TYPE  bsart,
            agre_date   TYPE  aedat,
            purch_org   TYPE  ekorg,
            pur_group   TYPE  bkgrp,
            comp_code   TYPE  bukrs,
            vper_start  TYPE  kdatb,
            vper_end    TYPE  kdate,
            pmnttrms    TYPE  dzterm,
            incoterms1  TYPE  inco1,
*            verkf       TYPE  verkf,
            zzcondayqty TYPE  z_condayqty,
            zzparty_agmt_id TYPE  zzparty,
            zzmsa       TYPE  zzmsa,
            zzoldealid  TYPE  zoldealid,
            zzekgrp     TYPE  ztrbuy,
            itm_cat     TYPE  pstyp,
            ass_cat     TYPE  knttp,
            currency    TYPE  waers,
            plant       TYPE  werks_d,
            matl_group  TYPE  matkl,
            item_no     TYPE  ebelp,   " ITEM
            tax_code    TYPE  mwskz,
            itm_txt     TYPE  txz01,
            trackingno  TYPE  bednr,
            price_cat   TYPE  meprf,
            target_qty  TYPE  ktmng,
            ser_num     TYPE  srvpos,
            price(16)   TYPE  c,        "sbrtwr,
            qty_per     TYPE  char13,   "menge,
            uom         TYPE  meins,
            price_unit  TYPE  char13,        "peinh,
            cost_center TYPE  kostl,
            glaccount   TYPE  sakto,
            userf2_num  TYPE  char17,   "userf2_num,
            userf1_txt  TYPE  userf1_txt,
*** Target System
            ebeln1 TYPE  ebeln,
            lifnr1 TYPE  elifn,
            bsart1 TYPE  bsart,
            bedat1 TYPE  aedat,
            ekorg1 TYPE  ekorg,
            ekgrp1 TYPE  bkgrp,
            bukrs1 TYPE  bukrs,
            kdatb1 TYPE  kdatb,
            kdate1 TYPE  kdate,
            zterm1 TYPE  dzterm,
            inco11 TYPE  inco1,
            zzcondayqty1 TYPE z_condayqty,
            zzparty_agmt_id1 TYPE  zzparty,
            zzmsa1       TYPE  zzmsa,
            zzoldealid1  TYPE  zoldealid,
            zzekgrp1     TYPE  ztrbuy,
            pstyp1       TYPE  pstyp,
            knttp1       TYPE  knttp,
            waers1       TYPE  waers,
            werks1 TYPE werks_d,
            matkl1 TYPE matkl,
            ebelp1 TYPE ebelp,
            mwskz1 TYPE mwskz,
            txz011 TYPE txz01,
            bednr1 TYPE bednr,
            meprf1 TYPE meprf,
            ktmng1 TYPE ktmng,
            srvpos     TYPE  srvpos,
            brtwr      TYPE  char16,   "sbrtwr,
            menge      TYPE  char13,   "menge,
            meins      TYPE  meins,
            peinh      TYPE  char10,   "peinh,
            kostl1     TYPE  kostl,
            sakto1     TYPE  sakto,
            userf2_num1 TYPE  char17,   "userf2_num,
            userf1_txt1 TYPE  userf1_txt,
****Recon fields
            ebeln2 TYPE  ebeln,
            lifnr2 TYPE  elifn,
            bsart2 TYPE  bsart,
            bedat2 TYPE  char3,
            ekorg2 TYPE  ekorg,
            ekgrp2 TYPE  bkgrp,
            bukrs2 TYPE  bukrs,
            kdatb2 TYPE  char3,
            kdate2 TYPE  char3,
            zterm2 TYPE  dzterm,
            inco12 TYPE  inco1,
            zzcondayqty2 TYPE z_condayqty,
            zzparty_agmt_id2 TYPE  zzparty,
            zzmsa2 TYPE zzmsa,
            zzoldealid2  TYPE  zoldealid,
            zzekgrp2     TYPE  ztrbuy,
            pstyp2       TYPE  char3,
            knttp2        TYPE char3,
            waers2       TYPE  char3,
            werks2 TYPE char3,
            matkl2 TYPE char3,
            ebelp2 TYPE char3,
            mwskz2 TYPE char3,
            txz012 TYPE txz01,
            bednr2 TYPE char3,
            meprf2 TYPE char3,
            ktmng2 TYPE char3,
            srvpos2     TYPE  srvpos,
            price2      TYPE  char3,     "sbrtwr,
            qty_per2     TYPE  char13,   "menge,
            meins2       TYPE  char3,
            price_unit2  TYPE  char3,    "peinh,
            kostl2       TYPE  char3,
            sakto2       TYPE  char3,
            userf2_num2  TYPE  char17,   "userf2_num,
            userf1_txt2  TYPE  userf1_txt,
        END OF ty_final,
*DEAL ID TO GET EKKO DETAILS
       BEGIN OF ty_dealid,
            zzoldealid  TYPE  zoldealid,
            item_no     TYPE  ebelp,
       END OF ty_dealid,

       BEGIN OF ty_sapdeals,
            ebeln TYPE ebeln,
            zzoldealid TYPE zoldealid,
            verkf TYPE everk,
       END OF ty_sapdeals,

       BEGIN OF ty_msgout,
            zzoldealid   TYPE zoldealid,
            errmsg(2000) TYPE c,
            errtyp(1)    TYPE c,  "S - SUCCESS , E-ERROR
        END OF ty_msgout.

*INTERNAL TABLE DECLARATIONS *.
DATA: lta_upload     TYPE STANDARD TABLE OF ty_upload,
      lta_source     TYPE STANDARD TABLE OF ty_source,
      lta_dealid     TYPE STANDARD TABLE OF ty_dealid,
      lta_sapdeals   TYPE STANDARD TABLE OF ty_sapdeals,
      lta_final      TYPE STANDARD TABLE OF ty_final,
      lta_head       TYPE STANDARD TABLE OF ty_headtarget,
      lta_item       TYPE STANDARD TABLE OF ty_itemtarget,
      lta_mastagree  TYPE STANDARD TABLE OF ty_mastagree_target,
      lta_esll       TYPE STANDARD TABLE OF ty_eslltarget,
      lta_esll_1     TYPE STANDARD TABLE OF ty_eslltarget1,
      lta_eskl       TYPE STANDARD TABLE OF ty_eskltarget,
      lta_ekkn       TYPE STANDARD TABLE OF ty_ekkntarget,
      lta_recon      TYPE STANDARD TABLE OF ty_final,
      lta_final_header TYPE STANDARD TABLE OF ty_header.

* WORK AREA DECLARATIONS *
DATA: lwa_upload       TYPE ty_upload,
      lwa_source       TYPE ty_source,
      lwa_final        TYPE ty_final,
      lwa_dealid       TYPE ty_dealid,
      lwa_sapdeals     TYPE ty_sapdeals,
      lwa_mastagree    TYPE ty_mastagree_target,
      lwa_msgout       TYPE ty_msgout,
      lwa_head         TYPE ty_headtarget,
      lwa_item         TYPE ty_itemtarget,
      lwa_esll         TYPE ty_eslltarget,
      lwa_esll_1       TYPE ty_eslltarget1,
      lwa_eskl         TYPE ty_eskltarget,
      lwa_ekkn         TYPE ty_ekkntarget,
      header           TYPE ty_header.

* VARIABLE DECLARATIONS *
DATA: lv_sep(1)      TYPE c VALUE ',',
      lv_tstflg      TYPE c,
      lv_errtype     TYPE c VALUE 'E',
      lv_cnt(5)      TYPE n,
      lv_path        TYPE btch0000-text80.

************************************************************************
* SELECTION SCREEN *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETER: p_recfl TYPE rlgrap-filename.
PARAMETER: p_outfl TYPE rlgrap-filename DEFAULT 'C:\SAPTEMP\DM8_RECONCILIATION_REPORT.XLS'.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* AT SELECTION-SCREEN PROCESSES
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_recfl.
*-- FORM TO SELECT A FILE FROM A PARTICULAR LOCATION
  PERFORM get_filename_f4 USING p_recfl.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_outfl.
*-- FORM TO SELECT A FILE FROM A PARTICULAR LOCATION
  PERFORM get_filename_f4 USING p_outfl.

AT SELECTION-SCREEN ON p_outfl.
  IF p_outfl IS NOT INITIAL.
    PERFORM check_file_path.
  ENDIF.

************************************************************************
* START-OF-SELECTION *
************************************************************************
START-OF-SELECTION.
*** TO UPLOAD FLAT FILE DATA INTO THE INTERNAL TABLE.
  IF  p_recfl IS NOT INITIAL.
    PERFORM file_upload.
  ELSE.
    MESSAGE i019 WITH text-005.
  ENDIF.
  IF  p_outfl IS NOT INITIAL.
*** SPLIT DATA FROM INPUT FILE INTO AN INTERNAL TABLE
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
  ENDIF..
*&---------------------------------------------------------------------*
*&      FORM  GET_FILENAME_F4
*&---------------------------------------------------------------------*
*       TEXT
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
ENDFORM.                    " GET_FILENAME_F4
*&---------------------------------------------------------------------*
*&      FORM  CHECK_FILE_PATH
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM check_file_path .
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "ABAP_BOOL.

*SEPARATE PATH AND FILE
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
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'C:'.
  ELSE.
*CHECK IF DIRECTORY PATH EXIST OR NOT.
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
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_FILE_PATH
*&---------------------------------------------------------------------*
*&      FORM  FILE_UPLOAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM file_upload .

  DATA : lv_file TYPE string.

  lv_file = p_recfl.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file
*     FILETYPE                = 'ASC'
*     HAS_FIELD_SEPARATOR     = 'X'
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
*&      FORM  SPLIT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM split_data .
*MOVING DATA INTO INTERNAL TABLE TO VALIDATE
  IF lta_upload IS NOT INITIAL.
    LOOP AT lta_upload INTO lwa_upload.
      SPLIT lwa_upload-string AT lv_sep INTO
            lwa_source-vendor  " HEADER
            lwa_source-cont_typ
            lwa_source-agre_date
            lwa_source-purch_org
            lwa_source-pur_group
            lwa_source-comp_code
            lwa_source-vper_start
            lwa_source-vper_end
            lwa_source-pmnttrms
            lwa_source-incoterms1
            lwa_source-zzcondayqty
            lwa_source-zzparty_agmt_id
            lwa_source-zzmsa
            lwa_source-zzoldealid
            lwa_source-zzekgrp
            lwa_source-itm_cat
            lwa_source-ass_cat
            lwa_source-currency
            lwa_source-plant
            lwa_source-matl_group
            lwa_source-item_no
            lwa_source-tax_code
            lwa_source-itm_txt
            lwa_source-trackingno
            lwa_source-price_cat
            lwa_source-target_qty
            lwa_source-ser_num
            lwa_source-price
            lwa_source-qty_per
            lwa_source-uom
            lwa_source-price_unit
            lwa_source-cost_center
            lwa_source-glaccount
            lwa_source-userf2_num
            lwa_source-userf1_txt.
      APPEND lwa_source TO lta_source.
**Collect deal id's and item no.
      lwa_dealid-zzoldealid = lwa_source-zzoldealid.
      lwa_dealid-item_no = lwa_source-item_no.
      APPEND lwa_dealid TO lta_dealid.
      CLEAR : lwa_upload, lwa_dealid.
    ENDLOOP.
    DELETE lta_source INDEX 1.
  ENDIF.
ENDFORM.                    " SPLIT_DATA
*&---------------------------------------------------------------------*
*&      FORM  GENERATE_REPORT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM generate_report .

  DATA: lv_match(3) TYPE c,
        lv_strqant  TYPE string,
        lv_strqant1  TYPE string,
        lv_seritemcnt TYPE extrow,
        lv_ekkncnt   TYPE dzekkn,
        lastdealid  TYPE zoldealid.

  IF lta_source IS NOT INITIAL.
    LOOP AT lta_source INTO lwa_source.
*IGNORE THE DEALS THAT HAVE ALREADY BEEN CREATED IN SAP
      READ TABLE lta_dealid INTO lwa_dealid WITH KEY zzoldealid = lwa_source-zzoldealid
                                                     item_no    = lwa_source-item_no.
      IF sy-subrc EQ 0.
**CHECK ALL THE DEAL ID PRESENT IN THE EKKO TABLE AND EKPO TABLE
        PERFORM check_oldealid USING lwa_source-zzoldealid
                                     lwa_source-item_no CHANGING lta_head
                                                                 lta_item
                                                                 lta_esll
                                                                 lta_esll_1
                                                                 lta_eskl
                                                                 lta_ekkn.
        IF lastdealid NE lwa_source-zzoldealid.
          CLEAR: lv_seritemcnt, lv_ekkncnt.
        ENDIF.
        DESCRIBE TABLE lta_esll_1 LINES lv_cnt.
        IF lv_cnt > 1.
          lv_seritemcnt = lv_seritemcnt + 10.
          lv_ekkncnt  = lv_ekkncnt + 01.
        ENDIF.
        READ TABLE lta_head INTO lwa_head WITH KEY verkf = lwa_source-zzoldealid.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lwa_source TO lwa_final.
          MOVE-CORRESPONDING lwa_head   TO lwa_final.
          lwa_final-ebeln2 = 'NA'.
          SHIFT lwa_head-lifnr1 LEFT DELETING LEADING '0'.
          PERFORM check_field_mapping USING lwa_head-lifnr1 lwa_source-vendor CHANGING lv_match.
          lwa_final-lifnr2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-bsart1 lwa_source-cont_typ CHANGING lv_match.
          lwa_final-bsart2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-bedat1 lwa_source-agre_date CHANGING lv_match.
          lwa_final-bedat2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-ekorg1 lwa_source-purch_org CHANGING lv_match.
          lwa_final-ekorg2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-ekgrp1 lwa_source-pur_group CHANGING lv_match.
          lwa_final-ekgrp2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-bukrs1 lwa_source-comp_code CHANGING lv_match.
          lwa_final-bukrs2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-kdatb1 lwa_source-vper_start CHANGING lv_match.
          lwa_final-kdatb2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-kdate1 lwa_source-vper_end CHANGING lv_match.
          lwa_final-kdate2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-zterm1 lwa_source-pmnttrms CHANGING lv_match.
          lwa_final-zterm2 = lv_match.
          PERFORM check_field_mapping USING lwa_head-inco11 lwa_source-incoterms1 CHANGING lv_match.
          lwa_final-inco12 = lv_match.
          CLEAR: lv_strqant, lv_strqant1.
          SPLIT lwa_head-zzcondayqty1 AT space INTO lv_strqant lv_strqant1.
          PERFORM check_field_mapping USING lv_strqant lwa_source-zzcondayqty CHANGING lv_match.
          lwa_final-zzcondayqty2 = lv_match.
*          PERFORM check_field_mapping USING lwa_head-zzcondayqty1 lwa_source-zzcondayqty CHANGING lv_match.
*          lwa_final-zzcondayqty2 = lv_match.
*          EKKO-ZZPARTY_AGMT_ID ZMMT_MASTAGREE - ZZMSA  EKKO-OLDEALID
          IF lwa_head-verkf IS NOT INITIAL.
            PERFORM check_field_mapping USING lwa_head-verkf lwa_source-zzoldealid CHANGING lv_match.
            lwa_final-zzoldealid2 = lv_match.
          ELSE.
          ENDIF.
          IF lta_mastagree IS NOT INITIAL.
            READ TABLE lta_mastagree INTO lwa_mastagree WITH KEY zzmsa = lwa_source-zzmsa.
            IF sy-subrc EQ 0.
              CLEAR lv_match.
              PERFORM check_field_mapping USING lwa_mastagree-zzmsa lwa_source-zzmsa CHANGING lv_match.
              lwa_final-zzmsa1 = lwa_mastagree-zzmsa.
              lwa_final-zzmsa2 = lv_match.
              PERFORM check_field_mapping USING lwa_head-zzparty_agmt_id1 lwa_source-zzparty_agmt_id CHANGING lv_match.
              lwa_final-zzparty_agmt_id2 = lv_match.
            ENDIF.
          ENDIF.
          PERFORM check_field_mapping USING lwa_head-zzekgrp1 lwa_source-zzekgrp CHANGING lv_match.
          lwa_final-zzekgrp2 = lv_match.
*          RM06E-EPSTP  EKPO-KNTTP
          PERFORM check_field_mapping USING lwa_head-waers1 lwa_source-currency CHANGING lv_match.
          lwa_final-waers2 = lv_match.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lwa_source-item_no
            IMPORTING
              output = lwa_source-item_no.

          READ TABLE lta_item INTO lwa_item WITH KEY ebeln1  = lwa_head-ebeln1
                                                     ebelp1  = lwa_source-item_no.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING lwa_item TO lwa_final.
            PERFORM check_item_cat USING lwa_item-pstyp1 CHANGING lwa_item-pstyp1.
            lwa_final-pstyp1 = lwa_item-pstyp1.
            PERFORM check_field_mapping USING lwa_item-pstyp1 lwa_source-itm_cat CHANGING lv_match.
            lwa_final-pstyp2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-knttp1 lwa_source-ass_cat CHANGING lv_match.
            lwa_final-knttp2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-werks1 lwa_source-plant CHANGING lv_match.
            lwa_final-werks2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-matkl1 lwa_source-matl_group CHANGING lv_match.
            lwa_final-matkl2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-ebelp1 lwa_source-item_no CHANGING lv_match.
            lwa_final-ebelp2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-mwskz1 lwa_source-tax_code CHANGING lv_match.
            lwa_final-mwskz2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-txz011 lwa_source-itm_txt CHANGING lv_match.
            lwa_final-txz012  = lv_match.
            PERFORM check_field_mapping USING lwa_item-bednr1 lwa_source-trackingno CHANGING lv_match.
            lwa_final-bednr2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-meprf1 lwa_source-price_cat CHANGING lv_match.
            lwa_final-meprf2  = lv_match.
            PERFORM check_field_mapping USING lwa_item-ktmng1 lwa_source-target_qty CHANGING lv_match.
            lwa_final-ktmng2  = lv_match.
          ENDIF.
          READ TABLE lta_esll INTO lwa_esll WITH KEY packno = lwa_item-packno.
          IF sy-subrc EQ 0.
            READ TABLE lta_esll_1 INTO lwa_esll_1 WITH KEY packno = lwa_esll-sub_packno
                                                           extrow = lv_seritemcnt.
            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING lwa_esll_1 TO lwa_final.
              TRANSLATE lwa_source-ser_num TO UPPER CASE.
              PERFORM check_field_mapping USING lwa_esll_1-srvpos lwa_source-ser_num CHANGING lv_match.
              lwa_final-srvpos2  = lv_match.
              PERFORM check_field_mapping USING lwa_esll_1-brtwr lwa_source-price CHANGING lv_match.
              lwa_final-price2  = lv_match.
              PERFORM check_field_mapping USING lwa_esll_1-menge lwa_source-qty_per CHANGING lv_match.
              lwa_final-qty_per2  = lv_match.
              IF lwa_esll_1-meins EQ 'GJ1'.
                lwa_esll_1-meins = 'GJ'.
              ENDIF.
              PERFORM check_field_mapping USING lwa_esll_1-meins lwa_source-uom CHANGING lv_match.
              lwa_final-meins2  = lv_match.
*              PERFORM check_field_mapping USING lwa_esll_1-peinh lwa_source-price_unit CHANGING lv_match.
              lwa_final-price_unit2 = 'N/A'.
              PERFORM check_field_mapping USING lwa_esll_1-userf2_num lwa_source-userf2_num CHANGING lv_match.
              lwa_final-userf2_num2  = lv_match.
              lwa_final-userf2_num1 = lwa_esll_1-userf2_num.
              PERFORM check_field_mapping USING lwa_esll_1-userf1_txt lwa_source-userf1_txt CHANGING lv_match.
              lwa_final-userf1_txt2  = lv_match.
              lwa_final-userf1_txt1 = lwa_esll_1-userf1_txt.
            ENDIF.
***ekkn table
*            lwa_esll_1-introw = lwa_esll_1-introw - 1.
            READ TABLE lta_eskl INTO lwa_eskl WITH KEY packno = lwa_esll_1-packno
                                                       introw = lwa_esll_1-introw.
*            LOOP AT lta_eskl INTO lwa_eskl WHERE packno = lwa_esll-sub_packno.
            IF sy-subrc EQ 0.
              LOOP AT lta_ekkn INTO lwa_ekkn WHERE ebeln = lwa_item-ebeln1
                                              AND  zekkn = lwa_eskl-zekkn.
                IF sy-subrc EQ 0.
                  MOVE-CORRESPONDING lwa_ekkn TO lwa_final.
                  SHIFT lwa_ekkn-kostl LEFT DELETING LEADING '0'.
                  SHIFT lwa_ekkn-sakto LEFT DELETING LEADING '0'.
                  PERFORM check_field_mapping USING lwa_ekkn-kostl lwa_source-cost_center CHANGING lv_match.
                  lwa_final-kostl1  = lwa_ekkn-kostl.
                  lwa_final-kostl2  = lv_match.
                  PERFORM check_field_mapping USING lwa_ekkn-sakto lwa_source-glaccount CHANGING lv_match.
                  lwa_final-sakto1  = lwa_ekkn-sakto.
                  lwa_final-sakto2  = lv_match.
                  EXIT.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
*          ENDIF.  incase
          APPEND lwa_final TO lta_final.
        ELSE.
          WRITE:/ lwa_source-zzoldealid,  text-004.
          MOVE-CORRESPONDING lwa_source TO lwa_final.
          lv_match = 'Not in SAP'.
          lwa_final-ebeln1 = lv_match.
          lwa_final-ebeln2 = lv_match.
          APPEND lwa_final TO lta_final.
        ENDIF.
*        REFRESH: LTA_HEAD, LTA_ITEM, LTA_ITEMCON.
        CLEAR :  lwa_dealid, lwa_final, lwa_head,lwa_item, lwa_esll,lwa_esll_1, lwa_eskl, lwa_ekkn.
      ENDIF.
      lastdealid = lwa_source-zzoldealid.
      CLEAR: lwa_source.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GENERATE_REPORT
*&---------------------------------------------------------------------*
*&      FORM  CHECK_OLDEALID
*----------------------------------------------------------------------*
*      -->p_lwa_head  text
*      -->p_lwa_source  text
*      <--p_lv_match  text
*----------------------------------------------------------------------*
FORM check_field_mapping  USING    p_lwa_head
                                   p_lwa_source
                          CHANGING p_lv_match.
  CLEAR: p_lv_match.
  IF p_lwa_head EQ p_lwa_source.
    p_lv_match = 'YES'.
  ELSE.
    p_lv_match = 'NO'.
  ENDIF.
ENDFORM.                    " Check_field_mapping
*&---------------------------------------------------------------------*
*&      FORM  DOWNLOAD_RECON
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*2
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM download_recon .

  DATA:lv_flname TYPE string.

  lv_flname = p_outfl.lv_flname = p_outfl.

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

ENDFORM.                    " DOWNLOAD_RECON
*&---------------------------------------------------------------------*
*&      Form  CHECK_OLDEALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_SOURCE_ZZOLDEALID  text
*      -->P_LWA_SOURCE_ITEM_NO  text
*      <--P_LTA_HEAD  text
*      <--P_LTA_ITEM  text
*      <--P_LTA_ESLL  text
*      <--P_LTA_ESLH  text
*      <--P_LTA_ekkn  text
*----------------------------------------------------------------------*
FORM check_oldealid  USING    p_lwa_source_zzoldealid
                              p_lwa_source_item_no
                     CHANGING p_lta_head
                              p_lta_item
                              p_lta_esll
                              p_lta_esll_1
                              p_lta_eskl
                              p_lta_ekkn.

  DATA: item_no TYPE ebelp.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_lwa_source_item_no
    IMPORTING
      output = item_no.

***GET DETAILS FROM EKKO, EKPO AND KONV TABLE
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
          verkf
          inco1
*          inco2
*          knumv
          zzcondayqty
*          zzconprice
          zzparty_agmt_id
*          zzparty
          zzekgrp
          zzoldealid
    FROM ekko INTO TABLE lta_head
*    WHERE zzoldealid EQ lwa_source-zzoldealid.          "#EC CI_NOFIELD
    WHERE verkf EQ p_lwa_source_zzoldealid.
  IF sy-subrc = 0.
    p_lta_head = lta_head.
    SELECT  ebeln
            ebelp
            txz01
            matnr
            werks
            lgort
            bednr
            matkl
            ktmng
            menge
            mwskz
            pstyp
            knttp
            meprf
            packno
      FROM ekpo INTO TABLE lta_item
      FOR ALL ENTRIES IN lta_head
      WHERE ebeln EQ lta_head-ebeln1.
*       AND  ebelp EQ item_no.
    IF sy-subrc = 0.
      p_lta_item = lta_item.
      SELECT packno introw srvpos sub_packno "menge meins peinh brtwr userf2_num userf1_txt
        FROM esll INTO CORRESPONDING FIELDS OF TABLE lta_esll
        FOR ALL ENTRIES IN lta_item
        WHERE packno EQ lta_item-packno.     "check query
      IF sy-subrc = 0.
        p_lta_esll   = lta_esll.
        SELECT packno introw extrow srvpos sub_packno menge meins peinh brtwr userf2_num userf1_txt
        FROM esll INTO CORRESPONDING FIELDS OF TABLE lta_esll_1
        FOR ALL ENTRIES IN lta_esll
        WHERE packno EQ lta_esll-sub_packno.
        IF sy-subrc EQ 0.
          p_lta_esll_1 = lta_esll_1.
          SELECT ebeln ebelp zekkn sakto kostl
          FROM ekkn INTO TABLE lta_ekkn
          FOR ALL ENTRIES IN lta_item
          WHERE ebeln EQ lta_item-ebeln1
            AND ebelp EQ lta_item-ebelp1.               "#EC CI_GENBUFF
          IF sy-subrc = 0.
            SELECT packno introw zekkn
            FROM eskl INTO TABLE lta_eskl
             FOR ALL ENTRIES IN lta_esll
              WHERE packno EQ lta_esll-sub_packno.
            IF sy-subrc = 0.
              p_lta_eskl = lta_eskl.
            ENDIF.
            p_lta_ekkn = lta_ekkn.
          ENDIF.
        ENDIF.
      ENDIF.
      SELECT zzparty_agmt_id zzmsa FROM zmmt_mastagree
      INTO TABLE lta_mastagree
          FOR ALL ENTRIES IN lta_head
          WHERE zzparty_agmt_id EQ lta_head-zzparty_agmt_id1. "#EC CI_NOFIELD
      IF sy-subrc EQ 0.
**write //
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_OLDEALID
*&---------------------------------------------------------------------*
*&      Form  PREPARE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_header .

  header-field1 = 'EKKO-LIFNR'.  APPEND header TO lta_final_header.
  header-field1 = 'RM06E-EVART'. APPEND header TO lta_final_header.
  header-field1 = 'RM06E-VEDAT'. APPEND header TO lta_final_header.
  header-field1 = 'EKKO-EKORG'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-EKGRP'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-BUKRS'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-KDATB'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-KDATE'.   APPEND header TO lta_final_header.
  header-field1 = 'EKKO-ZTERM'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-INCO1'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-ZZCONDAYQTY'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-ZZPARTY_AGMT_ID'.  APPEND header TO lta_final_header.
  header-field1 = 'ZMMT_MASTAGREE - ZZMSA'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-OLDEALID'.  APPEND header TO lta_final_header.
  header-field1 = 'ZZEKGRP'.     APPEND header TO lta_final_header.
  header-field1 = 'RM06E-EPSTP'.       APPEND header TO lta_final_header.
  header-field1 = 'EKPO-KNTTP'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-WAERS'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-WERKS'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-MATKL'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-EBELN'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-MWSKZ'.     APPEND header TO lta_final_header.
  header-field1 = 'EKPO-TXZ01'. APPEND header TO lta_final_header.
  header-field1 = 'EKPO-BEDNR'.  APPEND header TO lta_final_header.
  header-field1 = 'EKPO-MEPRF'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-KTMNG'.     APPEND header TO lta_final_header.
  header-field1 = 'ESLL-SRVPOS'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLL-TBTWR'.     APPEND header TO lta_final_header.
  header-field1 = 'ESLL-MENGE'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLL-MEINS'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLH-PEINH'.    APPEND header TO lta_final_header.
  header-field1 = 'KOSTL'.    APPEND header TO lta_final_header.
  header-field1 = 'SAKTO'.   APPEND header TO lta_final_header.
  header-field1 = 'ESLL-USERF2_NUM'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLL -USERF1_TXT'.  APPEND header TO lta_final_header.

  header-field1 = 'CONTRACT'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-LIFNR'.  APPEND header TO lta_final_header.
  header-field1 = 'RM06E-EVART'. APPEND header TO lta_final_header.
  header-field1 = 'RM06E-VEDAT'. APPEND header TO lta_final_header.
  header-field1 = 'EKKO-EKORG'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-EKGRP'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-BUKRS'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-KDATB'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-KDATE'.   APPEND header TO lta_final_header.
  header-field1 = 'EKKO-ZTERM'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-INCO1'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-ZZCONDAYQTY'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-ZZPARTY_AGMT_ID'.  APPEND header TO lta_final_header.
  header-field1 = 'ZMMT_MASTAGREE - ZZMSA'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-OLDEALID'.  APPEND header TO lta_final_header.
  header-field1 = 'ZZEKGRP'.     APPEND header TO lta_final_header.
  header-field1 = 'RM06E-EPSTP'.       APPEND header TO lta_final_header.
  header-field1 = 'EKPO-KNTTP'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-WAERS'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-WERKS'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-MATKL'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-EBELN'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-MWSKZ'.     APPEND header TO lta_final_header.
  header-field1 = 'EKPO-TXZ01'. APPEND header TO lta_final_header.
  header-field1 = 'EKPO-BEDNR'.  APPEND header TO lta_final_header.
  header-field1 = 'EKPO-MEPRF'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-KTMNG'.     APPEND header TO lta_final_header.
  header-field1 = 'ESLL-SRVPOS'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLL-TBTWR'.     APPEND header TO lta_final_header.
  header-field1 = 'ESLL-MENGE'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLL-MEINS'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLH-PEINH'.    APPEND header TO lta_final_header.
  header-field1 = 'KOSTL'.    APPEND header TO lta_final_header.
  header-field1 = 'SAKTO'.   APPEND header TO lta_final_header.
  header-field1 = 'ESLL-USERF2_NUM'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLL -USERF1_TXT'.  APPEND header TO lta_final_header.


  header-field1 = 'CONTRACT'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-LIFNR'.  APPEND header TO lta_final_header.
  header-field1 = 'RM06E-EVART'. APPEND header TO lta_final_header.
  header-field1 = 'RM06E-VEDAT'. APPEND header TO lta_final_header.
  header-field1 = 'EKKO-EKORG'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-EKGRP'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-BUKRS'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-KDATB'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-KDATE'.   APPEND header TO lta_final_header.
  header-field1 = 'EKKO-ZTERM'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-INCO1'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-ZZCONDAYQTY'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-ZZPARTY_AGMT_ID'.  APPEND header TO lta_final_header.
  header-field1 = 'ZMMT_MASTAGREE - ZZMSA'.  APPEND header TO lta_final_header.
  header-field1 = 'EKKO-VERKF/OLDEALID'.  APPEND header TO lta_final_header.
  header-field1 = 'ZZEKGRP'.     APPEND header TO lta_final_header.
  header-field1 = 'RM06E-EPSTP'.       APPEND header TO lta_final_header.
  header-field1 = 'EKPO-KNTTP'.    APPEND header TO lta_final_header.
  header-field1 = 'EKKO-WAERS'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-WERKS'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-MATKL'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-EBELN'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-MWSKZ'.     APPEND header TO lta_final_header.
  header-field1 = 'EKPO-TXZ01'. APPEND header TO lta_final_header.
  header-field1 = 'EKPO-BEDNR'.  APPEND header TO lta_final_header.
  header-field1 = 'EKPO-MEPRF'.    APPEND header TO lta_final_header.
  header-field1 = 'EKPO-KTMNG'.     APPEND header TO lta_final_header.
  header-field1 = 'ESLL-SRVPOS'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLL-TBTWR'.     APPEND header TO lta_final_header.
  header-field1 = 'ESLL-MENGE'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLL-MEINS'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLH-PEINH'.    APPEND header TO lta_final_header.
  header-field1 = 'KOSTL'.    APPEND header TO lta_final_header.
  header-field1 = 'SAKTO'.   APPEND header TO lta_final_header.
  header-field1 = 'ESLL-USERF2_NUM'.  APPEND header TO lta_final_header.
  header-field1 = 'ESLL -USERF1_TXT'.  APPEND header TO lta_final_header.

ENDFORM.                    " PREPARE_HEADER
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITEM_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_ITEM_PSTYP1  text
*      <--P_LWA_ITEM_PSTYP1  text
*----------------------------------------------------------------------*
FORM check_item_cat  USING    p_lwa_item_pstyp1
                     CHANGING p_lwa_item_pstyp2.

  DATA: lv_itemcat TYPE epstp.

  SELECT epstp FROM t163y INTO lv_itemcat
    WHERE pstyp EQ p_lwa_item_pstyp1
    AND spras EQ 'E'.
    IF sy-subrc EQ 0.
      p_lwa_item_pstyp2 = lv_itemcat.
    ENDIF.
  ENDSELECT.
ENDFORM.                    " CHECK_ITEM_CAT
