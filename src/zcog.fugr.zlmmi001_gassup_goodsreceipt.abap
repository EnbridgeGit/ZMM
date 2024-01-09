FUNCTION zlmmi001_gassup_goodsreceipt.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SAP_BATCH_ID) TYPE  ZZSAPBATCHID
*"     VALUE(PER_YEAR) TYPE  ZZPERYEAR OPTIONAL
*"     VALUE(PER_MTH) TYPE  ZZPERMTH OPTIONAL
*"     VALUE(GAS_DAY) TYPE  ZZGAS_DAY
*"     VALUE(CONTRACTID) TYPE  ZZCONTRACTNO
*"     VALUE(CONTRACT_REF) TYPE  ZZCONREF
*"     VALUE(SERV_TYPE) TYPE  ZZSERVTYPE
*"     VALUE(RATE_CLASS) TYPE  ZZRATECLS
*"     VALUE(SERV_CLASS) TYPE  ZZSERVCLS
*"     VALUE(OFFER_PARTYID) TYPE  ZZOFFERPARTYID
*"     VALUE(OFFER_PARTY) TYPE  ZZOFFERPARTY
*"     VALUE(FROM_TRADE_LOCID) TYPE  ZZFROMTRLOCID OPTIONAL
*"     VALUE(FROM_TRADE_LOC) TYPE  ZZFROMTRADELOC
*"     VALUE(TO_TRADE_LOCID) TYPE  ZZTOTRLOCID OPTIONAL
*"     VALUE(TO_TRADE_LOC) TYPE  ZZTOTRADELOC
*"     VALUE(SERV_SUB_TYPE) TYPE  ZZSUBTYPE OPTIONAL
*"     VALUE(SERV_USAGE) TYPE  ZZSERVUSAGE
*"     VALUE(OVERRUNIND) TYPE  ZZOVERIND OPTIONAL
*"     VALUE(QUANTITY) TYPE  ZZQUANTITY
*"     VALUE(UOM) TYPE  ZZUOM
*"     VALUE(SENT_IND) TYPE  ZZSENTIND OPTIONAL
*"     VALUE(SAP_DOC_ID) TYPE  ZZSAPDOCID OPTIONAL
*"     VALUE(TESTRUN) TYPE  CHAR0001 OPTIONAL
*"  EXPORTING
*"     VALUE(GOODSMVT_HEADRET) TYPE  BAPI2017_GM_HEAD_RET
*"  TABLES
*"      RETURNMSG STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 05-03-2019   KMB         D30K929675  COG CARE inbound changes        *
************************************************************************
* 07-10-2021   DADIM       D30K931758  CARE to SAP integration         *
************************************************************************
* 11-01-2022   DADIM       D30K931947  CARE to SAP integration         *
************************************************************************

  TABLES: zcarereference,
          lfm1,
          t006a,
          eket.

*"----------------------------------------------------------------------
*" All Global variables / BAPI tables defined in LZCOGTOP
*"----------------------------------------------------------------------

* Set Up Variables

  MOVE sap_batch_id   TO w_sapbatchid.
  MOVE per_year       TO w_per_year.
  MOVE per_mth        TO w_per_mth.
  MOVE gas_day        TO w_gas_day.
  MOVE contractid     TO w_contractid.
  MOVE contract_ref   TO w_contract_ref.
  MOVE serv_type      TO w_serv_type.
  MOVE rate_class     TO w_rate_class.
  MOVE serv_class     TO w_serv_class.
  MOVE offer_partyid  TO w_offer_partyid.
  MOVE offer_party    TO w_offer_party.
  MOVE from_trade_loc TO w_from_tradeloc.
  MOVE to_trade_loc   TO w_to_tradeloc.
  MOVE serv_usage     TO w_serv_usage.
  MOVE quantity       TO w_qty_num.
  MOVE uom            TO w_uom.
  MOVE sap_doc_id     TO w_sapdocid.
  MOVE testrun        TO w_testrun.

  w_error_flag = 'N'.

* Check posting period
  PERFORM check_posting_period.

* Determine Movement Type
  IF w_qty_num < 0.
    MOVE '122' TO w_move_type.
    w_qty_num = w_qty_num * -1.
  ELSE.
    MOVE '101' TO w_move_type.
  ENDIF.

* If Gas_Day is blank, use day 1 of Period Yr/Mth
  IF w_gas_day = '00000000'.
    CONCATENATE w_per_year w_per_mth '01' INTO w_gas_day.
  ENDIF.

* Validate purchasing document number. If the contract ID is blank
* skip this step. If not found on ZCAREREFERENCE, use the contract
* reference field value.
  IF w_contractid <> ' '.
    SELECT SINGLE * FROM zcarereference
      WHERE zzcareconid = w_contractid.

*    If found on ZCAREREFERENCE, use the purchase doc no
*    otherwise, just pass along the original contract ID

    IF sy-subrc = 0.
      MOVE zcarereference-zzsappurdoc TO w_contractno.
    ELSE.
      w_contractno = w_contract_ref(10).
    ENDIF.
  ELSE.
    w_contractno = w_contract_ref(10).
  ENDIF.

*BOC by KMB CHG0138279 COG CARE inbound change on 05.03.2019
  CLEAR: w_zcareenb, w_returnmsg.
  SELECT SINGLE * FROM zcareenb
    INTO w_zcareenb
   WHERE zzcareconid = w_contractid.
  IF sy-subrc = 0.
    MOVE 'I'         TO returnmsg-type.
    MOVE 'FM'        TO returnmsg-id.
    MOVE '999'       TO returnmsg-number.
    CONCATENATE: 'Contract'
                 contractid
                 'is identified as Enbridge and not processed'
                 INTO w_returnmsg SEPARATED BY space.
    MOVE w_returnmsg TO returnmsg-message.
    APPEND returnmsg.
    EXIT.
  ENDIF.
*EOC by KMB CHG0138279 COG CARE inbound change on 05.03.2019

* Validate Vendor number.
  IF w_offer_partyid <> ' '.
    SELECT SINGLE * FROM lfm1
     WHERE eikto = w_offer_partyid.

    IF sy-subrc = 0.
      MOVE w_offer_partyid TO w_grgi_slip_no.
    ELSE.
      MOVE 'E'         TO returnmsg-type.
      MOVE 'FM'        TO returnmsg-id.
      MOVE '001'       TO returnmsg-number.
      MOVE 'CARE Offered By Party ID does not match SAP Vendor'
           TO returnmsg-message.
      APPEND returnmsg.
      MOVE 'Y' TO w_error_flag.
    ENDIF.
  ENDIF.

* Validate Base Unit of Measure

  CLEAR w_entry_uom.

  SELECT SINGLE * FROM t006a
   WHERE mseh3 = w_uom.

  IF sy-subrc = 0.
    MOVE t006a-msehi TO w_entry_uom.
  ELSE.
    MOVE 'E'         TO returnmsg-type.
    MOVE 'FM'        TO returnmsg-id.
    MOVE '002'       TO returnmsg-number.
    MOVE 'CARE Unit of Measure does not match any valid SAP UOM.'
         TO returnmsg-message.
    APPEND returnmsg.
    MOVE 'Y' TO w_error_flag.
  ENDIF.


* Find item number of the purchasing document.
  yrmonth = w_gas_day+0(6).
  CONCATENATE yrmonth '01' INTO fromdate.

  CALL FUNCTION 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in            = fromdate
    IMPORTING
      last_day_of_month = todate.

  SELECT SINGLE * FROM eket
   WHERE ebeln = w_contractno
     AND eindt BETWEEN fromdate AND todate.

  IF sy-subrc = 0.
    MOVE eket-ebelp TO w_po_item.
  ELSE.
    MOVE 'E'         TO returnmsg-type.
    MOVE 'FM'        TO returnmsg-id.
    MOVE '003'       TO returnmsg-number.
    MOVE 'Cannot find purchasing document item no. using Gas Day.'
         TO returnmsg-message.
    APPEND returnmsg.
    MOVE 'Y' TO w_error_flag.
  ENDIF.

*Start of change by DADIM for CHG0218759
  CLEAR : gv_inco1, gv_zrate, gs_a050.
  SELECT SINGLE inco1 FROM ekko INTO gv_inco1
    WHERE ebeln = w_contract_ref.
  SELECT SINGLE zrate FROM zmprate INTO gv_zrate
    WHERE zinco1 = gv_inco1.
  IF sy-subrc = 0.
    IF gv_zrate = 'D'.
      SELECT SINGLE * FROM a050 INTO gs_a050
        WHERE kappl = 'M' AND
              kschl = 'MP01' AND
              ekorg = 'GASA' AND
              inco1 = gv_inco1 AND
*              datbi = w_gas_day AND                         "Commented for CHG0237630
            ( datbi =  w_gas_day OR datbi = '99991231' ) AND "Added by DADIM for "CHG0237630
              datab = w_gas_day.
    ELSEIF gv_zrate = 'M'.
      gv_day+0(6) = w_gas_day+0(6).
      gv_day+6(2) = '01'.
      SELECT SINGLE * FROM a050 INTO gs_a050
       WHERE kappl = 'M' AND
             kschl = 'MP01' AND
             ekorg = 'GASA' AND
             inco1 = gv_inco1 AND
             datab = gv_day.
    ENDIF.
    IF gs_a050 IS INITIAL.
      MOVE 'E' TO returnmsg-type.
      CONCATENATE 'Price missing for SA' w_contract_ref 'on' w_gas_day
           INTO returnmsg-message SEPARATED BY space.
      APPEND returnmsg.
      MOVE 'Y' TO w_error_flag.
    ENDIF.
  ENDIF.
*End of change by DADIM for CHG0218759

  IF w_error_flag = 'N'.
    PERFORM call_function_module.
    MOVE tbl_bapi_gmhdret TO goodsmvt_headret.
    LOOP AT tbl_bapi_gmreturn.
      MOVE tbl_bapi_gmreturn TO returnmsg.
      APPEND returnmsg.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
***********************************************************************
FORM check_posting_period.

  DATA:  v_year(4)  TYPE n,
         v_mth(3)   TYPE n.

  w_perclose_flag = 'N'.

  IF w_per_year <> '' AND
     w_per_mth <> ''.
    MOVE w_per_year TO v_year.
    MOVE w_per_mth TO v_mth.
  ELSE.
    MOVE w_gas_day+0(4) TO v_year.
    MOVE w_gas_day+4(2) TO v_mth.
  ENDIF.

  IF v_year <> sy-datum+0(4) OR
     v_mth <> sy-datum+4(2).

    SELECT frye1 frpe1 FROM t001b
      INTO (w_frye1, w_frpe1)
     WHERE rrcty = '0'
       AND bukrs = '0001'
       AND mkoar = 'M'.
    ENDSELECT.

    IF sy-subrc = 0.

      IF v_year = w_frye1.
        IF v_mth < w_frpe1.
          MOVE 'Y' TO w_perclose_flag.
        ENDIF.
      ELSEIF v_year < w_frye1.
        MOVE 'Y' TO w_perclose_flag.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    "check_posting_period
***********************************************************************
FORM call_function_module.

* Populate BAPI parameters

  REFRESH: tbl_bapi_gmhdr, tbl_bapi_gmcode, tbl_bapi_gmitem.
  CLEAR: tbl_bapi_gmhdr, tbl_bapi_gmcode, tbl_bapi_gmitem.

  IF w_perclose_flag = 'Y'.
    CONCATENATE w_frye1 w_frpe1+1(2) '01'
           INTO tbl_bapi_gmhdr-pstng_date.
  ELSE.
    MOVE w_gas_day TO tbl_bapi_gmhdr-pstng_date.
  ENDIF.

  tbl_bapi_gmhdr-doc_date      = w_gas_day.
  tbl_bapi_gmhdr-ref_doc_no    = w_rate_class.

  CONCATENATE w_serv_type '_' w_sapbatchid INTO
                       tbl_bapi_gmhdr-ref_doc_no.

  CONCATENATE w_rate_class '_' w_serv_class INTO
                       tbl_bapi_gmhdr-header_txt.

  IF w_serv_usage = 'F'.
    tbl_bapi_gmhdr-bill_of_lading = 'FUEL'.
  ELSEIF w_serv_usage = 'C'.
    tbl_bapi_gmhdr-bill_of_lading = 'COMM'.
  ENDIF.

  tbl_bapi_gmhdr-gr_gi_slip_no = w_grgi_slip_no.

  APPEND tbl_bapi_gmhdr.

  tbl_bapi_gmcode-gm_code ='01'.

  APPEND tbl_bapi_gmcode.

  tbl_bapi_gmitem-material  = 'NATGAS'.
  tbl_bapi_gmitem-move_type = w_move_type.
  IF w_move_type = '122'.
    tbl_bapi_gmitem-move_reas = '4'.
  ENDIF.
* Change quantity to avoid posting fractional cents.
  IF w_qty_num <= '0.004'.
    MOVE '0.004' TO w_qty_num.
  ENDIF.

  MOVE w_qty_num TO w_quantity.
  tbl_bapi_gmitem-entry_qnt = w_quantity.
  tbl_bapi_gmitem-entry_uom = w_entry_uom.
  tbl_bapi_gmitem-po_number = w_contractno.
  tbl_bapi_gmitem-po_item   = w_po_item.
  IF w_sapdocid <> ''.
    tbl_bapi_gmitem-ref_doc = w_sapdocid(10).
  ENDIF.
  tbl_bapi_gmitem-comp_ship = '01'.
  tbl_bapi_gmitem-mvt_ind   = 'B'.

  CONCATENATE w_offer_party '_' w_from_tradeloc '_' w_to_tradeloc
         INTO tbl_bapi_gmitem-item_text.

  APPEND tbl_bapi_gmitem.


* Create the Goods Receipt Document

  REFRESH: tbl_bapi_gmhdret, tbl_bapi_gmreturn.
  CLEAR: tbl_bapi_gmhdret, tbl_bapi_gmreturn.

  SET UPDATE TASK LOCAL.
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = tbl_bapi_gmhdr
      goodsmvt_code    = tbl_bapi_gmcode
      testrun          = w_testrun
    IMPORTING
      goodsmvt_headret = tbl_bapi_gmhdret
*     MATERIALDOCUMENT = tbl_bapi_gmhdret-mat_doc
*     MATDOCUMENTYEAR  = tbl_bapi_gmhdret-doc_year
    TABLES
      goodsmvt_item    = tbl_bapi_gmitem
      return           = tbl_bapi_gmreturn.

*  Commit the work or rollback the BAPI call.

  IF tbl_bapi_gmhdret-mat_doc <> ' '.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
*  wait up to 1 seconds.

ENDFORM.                    "call_function_module
