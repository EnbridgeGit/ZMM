FUNCTION-POOL zcog                        . "MESSAGE-ID ...

************************************************************************
* The following data statements are common to all functions:
************************************************************************

DATA: w_sapbatchid       TYPE zzsapbatchid,
      w_per_year         TYPE zzperyear,
      w_per_mth          TYPE zzpermth,
      w_gas_day          TYPE zzgas_day,
      w_contractid       TYPE zzcontractno,
      w_contract_ref     TYPE zzconref,
      w_serv_type        TYPE zzservtype,
      w_rate_class       TYPE zzratecls,
      w_serv_class       TYPE zzservcls,
      w_offer_partyid    TYPE zzofferpartyid,
      w_offer_party      TYPE zzofferparty,
      w_from_tradelocid  TYPE zzfromtrlocid,
      w_from_tradeloc    TYPE zzfromtradeloc,
      w_to_tradelocid    TYPE zztotrlocid,
      w_to_tradeloc      TYPE zztotradeloc,
      w_serv_usage       TYPE zzservusage,
      w_serv_sub_type    TYPE zzsubtype,  "COG
      w_overrunind       TYPE zzoverind,
      w_quantity         TYPE zzquantity,
      w_uom              TYPE zzuom,
      w_sapdocid         TYPE zzsapdocid,
      w_matdocret        TYPE bapi2017_gm_head_ret,
      w_returnmsg        TYPE char200,
      w_zcarereference   LIKE zcarereference,
      w_zcareenb         LIKE zcareenb, "Added by KMB CHG0138279 COG CARE inbound change on 05.03.2019
      w_frye1            LIKE t001b-frye1,
      w_frpe1            LIKE t001b-frpe1,
      w_perclose_flag(1) TYPE c.

************************************************************************
* The following data statements apply to function:
* ZLMMI001_GASSUP_GOODSRECEIPT
************************************************************************
DATA: w_grgi_slip_no   LIKE lfm1-lifnr,
      w_entry_uom      LIKE ekpo-bprme,
      w_po_item        LIKE eket-ebelp,
      w_contractno     LIKE ekpo-ebeln,
      w_qty_num        TYPE p DECIMALS 4,
      w_sapcontract    LIKE zcarereference-zzsapcontract,
      w_move_type      TYPE bwart,
      yrmonth(6)       TYPE c,
      fromdate         LIKE sy-datum,
      todate           LIKE sy-datum,
      w_error_flag(1)  TYPE c,
      w_testrun        LIKE bapi2017_gm_gen-testrun VALUE IS INITIAL.

* BAPI tables
DATA: BEGIN OF tbl_bapi_gmhdr OCCURS 0.
        INCLUDE STRUCTURE bapi2017_gm_head_01.
DATA: END OF tbl_bapi_gmhdr.

DATA: BEGIN OF tbl_bapi_gmcode OCCURS 0.
        INCLUDE STRUCTURE bapi2017_gm_code.
DATA: END OF tbl_bapi_gmcode.

DATA: BEGIN OF tbl_bapi_gmitem OCCURS 0.
        INCLUDE STRUCTURE bapi2017_gm_item_create.
DATA: END OF tbl_bapi_gmitem.

DATA: BEGIN OF tbl_bapi_gmhdret OCCURS 0.
        INCLUDE STRUCTURE bapi2017_gm_head_ret.
DATA: END OF tbl_bapi_gmhdret.

DATA: BEGIN OF tbl_bapi_gmreturn OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
DATA: END OF tbl_bapi_gmreturn.

************************************************************************
************************************************************************
* The following data statements apply to function:
* ZLMMI004_TRANS_SERVENTRY
************************************************************************
DATA:  w_yearmo          LIKE ekpo-bednr,
       w_per_firstday    LIKE sy-datum,
       w_per_lastday     LIKE sy-datum,
       w_contid          TYPE zzcontractid,
       w_ebeln           LIKE ekpo-ebeln,
       w_ebelp           LIKE ekpo-ebeln,
       w_ekpo_packno     LIKE ekpo-packno,
       w_matkl           LIKE ekpo-matkl,
       w_nosrvcmatch(18) TYPE c VALUE 'NO_SERVICE_MATCH  ',
       w_esll_packno     LIKE esll-packno,
       w_sub_packno      LIKE esll-sub_packno,
       w_introw          LIKE esll-introw,
       w_srvpos          LIKE esll-srvpos,
       w_count           TYPE i,
       w_sapsrvcmat      LIKE zcaresrvcmat.

* BAPI tables
DATA: BEGIN OF tbl_bapi_eshdr OCCURS 0.
        INCLUDE STRUCTURE bapiessrc.
DATA: END OF tbl_bapi_eshdr.

DATA: BEGIN OF tbl_bapi_essrvcs OCCURS 0.
        INCLUDE STRUCTURE bapiesllc.
DATA: END OF tbl_bapi_essrvcs.

DATA: BEGIN OF tbl_bapi_esreturn OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
DATA: END OF tbl_bapi_esreturn.

DATA: w_entrysheetno LIKE bapiessr-sheet_no.
*****************************
DATA: BEGIN OF msg,
       ty LIKE syst-msgty,
       id LIKE syst-msgid,
       no LIKE syst-msgno,
       v1 LIKE syst-msgv1,
       v2 LIKE syst-msgv2,
       v3 LIKE syst-msgv3,
       v4 LIKE syst-msgv4,
    END OF msg.

*Start of change by DADIM for CHG0218759
DATA : gv_inco1 TYPE inco1,
       gv_zrate TYPE zrate,
       gs_a050  TYPE a050,
       gv_day   TYPE zzgas_day.
*End of change by DADIM for CHG0218759
