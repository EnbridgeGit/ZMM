function zlmmi004_trans_serventry.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SAP_BATCH_ID) TYPE  ZZSAPBATCHID
*"     VALUE(PER_YEAR) TYPE  ZZPERYEAR
*"     VALUE(PER_MTH) TYPE  ZZPERMTH
*"     VALUE(GAS_DAY) TYPE  ZZGAS_DAY OPTIONAL
*"     VALUE(CONTRACT_ID) TYPE  ZZCONTRACTID
*"     VALUE(CONTRACT_REF) TYPE  ZZCONREF
*"     VALUE(SERV_TYPE) TYPE  ZZSERVTYPE
*"     VALUE(RATE_CLASS) TYPE  ZZRATECLS
*"     VALUE(SERV_CLASS) TYPE  ZZSERVCLS OPTIONAL
*"     VALUE(OFFER_PARTY_ID) TYPE  ZZOFFERPARTYID OPTIONAL
*"     VALUE(OFFER_PARTY) TYPE  ZZOFFERPARTY OPTIONAL
*"     VALUE(FROM_TRADE_LOCID) TYPE  ZZFROMTRLOCID OPTIONAL
*"     VALUE(FROM_TRADE_LOC) TYPE  ZZFROMTRADELOC OPTIONAL
*"     VALUE(TO_TRADE_LOCID) TYPE  ZZTOTRLOCID OPTIONAL
*"     VALUE(TO_TRADE_LOC) TYPE  ZZTOTRADELOC OPTIONAL
*"     VALUE(SERV_SUB_TYPE) TYPE  ZZSUBTYPE OPTIONAL
*"     VALUE(SERV_USAGE) TYPE  ZZSERVUSAGE
*"     VALUE(OVERRUNIND) TYPE  ZZOVERIND
*"     VALUE(QUANTITY) TYPE  ZZQUANTITY
*"     VALUE(UOM) TYPE  ZZUOM
*"     VALUE(SENT_IND) TYPE  ZZSENTIND OPTIONAL
*"     VALUE(SAP_DOC_ID) TYPE  ZZSAPDOCID OPTIONAL
*"     VALUE(TESTRUN) TYPE  CHAR0001 OPTIONAL
*"  EXPORTING
*"     VALUE(ENTRYSHEETNO) TYPE  BAPIESSR-SHEET_NO
*"  TABLES
*"      RETURNMSG STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 05-03-2019   KMB         D30K929675  COG CARE inbound changes        *
* 07-09-2021   birudurd    D30K931557  COG change to make service      *
*              subtype as key in table ZCARESRVCMAT and use it to get  *
*              SAP Service master.                                     *
************************************************************************

  tables: zcaresrvcmat,
          ekko,
          ekpo,
          essr.

*"----------------------------------------------------------------------
*" All Global variables / BAPI tables defined in LZCOGTOP
*"----------------------------------------------------------------------

* Set up variables

  move sap_batch_id     to w_sapbatchid.
  move contract_id      to w_contid.
  move contract_ref     to w_contract_ref.
  move per_year         to w_per_year.
  move per_mth          to w_per_mth.
  move serv_type        to w_serv_type.
  move rate_class       to w_rate_class.
  move serv_class       to w_serv_class.
  move from_trade_locid to w_from_tradelocid.
  move from_trade_loc   to w_from_tradeloc.
  move to_trade_locid   to w_to_tradelocid.
  move to_trade_loc     to w_to_tradeloc.
  move serv_usage       to w_serv_usage.
  move serv_sub_type    to w_serv_sub_type. " COG
  move overrunind       to w_overrunind.
  move quantity         to w_qty_num.
  move uom              to w_uom.
  move sap_doc_id       to w_sapdocid.
  move testrun          to w_testrun.

  w_error_flag = 'N'.

  concatenate w_per_year w_per_mth into w_yearmo.
  concatenate w_per_year w_per_mth '01' into w_per_firstday.

  call function 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
    exporting
      day_in            = w_per_firstday
    importing
      last_day_of_month = w_per_lastday.

******Step 1a***********************************************************

* Check posting period
  perform check_posting_period.

******Step 1b***********************************************************
* If SAP_DOC_ID exists, execute BAPI: BBP_ENTRYSHEET_CANCEL

  if sap_doc_id <> ' '.

    select * from essr
     where lblni = sap_doc_id.
    endselect.

    if sy-subrc <> 0.
      move 'E'         to returnmsg-type.
      move 'FM'        to returnmsg-id.
      move '999'       to returnmsg-number.
      concatenate: 'Entry sheet: ' sap_doc_id
                   ' does not exist.  Please verify SAP document ID.'
                   into w_returnmsg.
      move w_returnmsg to returnmsg-message.
      append returnmsg.
      exit.
    else.
*     If the entrysheet has been deleted already then skip bapi
      if essr-loekz <> 'X'.
         perform delete_entrysheet using w_sapdocid
                                         essr-budat.

        if tbl_bapi_esreturn-message <> ''.
          loop at tbl_bapi_esreturn.
            if tbl_bapi_esreturn-type = 'E' or
               tbl_bapi_esreturn-type = 'S' or
               tbl_bapi_esreturn-type = 'A'.
              move tbl_bapi_esreturn to returnmsg.
              append returnmsg.
            endif.

*           Intercept 'Deleted' message and change msg type to 'S'

            if tbl_bapi_esreturn-type = 'I' and
               tbl_bapi_esreturn-id = 'SE' and
               tbl_bapi_esreturn-number = '110'.
               move 'S' to tbl_bapi_esreturn-type.
               move tbl_bapi_esreturn to returnmsg.
               append returnmsg.
            endif.
          endloop.
        endif.

*       If the input record has a zero quantity, then exit
*       the function after the SES cancel is performed.

        if w_qty_num = 0.
           exit.
        endif.

      endif.
    endif.
  endif.

* Convert/Validate CARE Data

******Step 2A***********************************************************

  select * from zcaresrvcmat
   where srvc_typ = w_serv_type
     and conid = w_contid
     and ratecls = w_rate_class
     and servcls = w_serv_class
     and overind = w_overrunind
     and fromtrlocid = w_from_tradelocid
     and totrlocid = w_to_tradelocid
     and servusage = w_serv_usage
     and srvsubtype = w_serv_sub_type . " COG
  endselect.

  if sy-subrc = 0.
    move zcaresrvcmat-sap_srvcmat to w_sapsrvcmat.
  else.
    move w_nosrvcmatch to w_sapsrvcmat.
  endif.


*****Step 2B************************************************************

  refresh tbl_bapi_esreturn.
  clear tbl_bapi_esreturn.

  select single * from zcarereference
    into w_zcarereference
   where zzcareconid = w_contid.

  if sy-subrc = 0.
*    move w_zcarereference-zzsappurdoc to w_contractno.          "TR804
    move w_zcarereference-zzsapcontract to w_sapcontract.
  else.
    move 'E'         to returnmsg-type.
    move 'FM'        to returnmsg-id.
    move '999'       to returnmsg-number.
    concatenate: 'Purchase order not found in ZCAREREFERENCE f'
                 'or contract: '
                 contract_id into w_returnmsg.
    move w_returnmsg to returnmsg-message.
    append returnmsg.
    exit.
  endif.

*BOC by KMB CHG0138279 COG CARE inbound change on 05.03.2019
  CLEAR : w_zcareenb, w_returnmsg.
  select single * from zcareenb
    into w_zcareenb
   where zzcareconid = w_contid.
  IF sy-subrc = 0.
    move 'I'         to returnmsg-type.
    move 'FM'        to returnmsg-id.
    move '999'       to returnmsg-number.
    concatenate: 'Contract'
                 contract_id
                 'is identified as Enbridge and not processed'
                 into w_returnmsg separated by space.
    move w_returnmsg to returnmsg-message.
    append returnmsg.
    exit.
  ENDIF.
*EOC by KMB CHG0138279 COG CARE inbound change on 05.03.2019

* Validate PO                                                    "TR804
*                                                                "TR804
*  select single * from ekko                                     "TR804
*   where ebeln = w_contractno.                                  "TR804
*                                                                "TR804
*  if sy-subrc <> 0.                                             "TR804
*    move 'E'         to returnmsg-type.                         "TR804
*    move 'FM'        to returnmsg-id.                           "TR804
*    move '999'       to returnmsg-number.                       "TR804
*    concatenate: 'ZCAREREFERENCE PO not found in EKKO f'        "TR804
*                 'or contract: '                                "TR804
*                 contract_id into w_returnmsg.                  "TR804
*    move w_returnmsg to returnmsg-message.                      "TR804
*    append returnmsg.                                           "TR804
*    exit.                                                       "TR804
*  else.                                                         "TR804
*    if ekko-loekz <> ' '.                                       "TR804
*      move 'E'         to returnmsg-type.                       "TR804
*      move 'FM'        to returnmsg-id.                         "TR804
*      move '999'       to returnmsg-number.                     "TR804
*      concatenate: 'PO delete flag not blank in EKKO f'         "TR804
*                   'or PO number: '                             "TR804
*                w_contractno into w_returnmsg.                  "TR804
*      move w_returnmsg to returnmsg-message.                    "TR804
*      append returnmsg.                                         "TR804
*      w_error_flag = 'Y'.                                       "TR804
*    endif.                                                      "TR804
*                                                                "TR804
*    if ekko-frgke <> 'Y'.                                       "TR804
*      move 'E'         to returnmsg-type.                       "TR804
*      move 'FM'        to returnmsg-id.                         "TR804
*      move '999'       to returnmsg-number.                     "TR804
*      concatenate: 'PO release indicator not set in EKKO f'     "TR804
*                   'or PO number: '                             "TR804
*                   w_contractno into w_returnmsg.               "TR804
*      move w_returnmsg to returnmsg-message.                    "TR804
*      append returnmsg.                                         "TR804
*      w_error_flag = 'Y'.                                       "TR804
*    endif.                                                      "TR804
*                                                                "TR804
*    if w_yearmo < ekko-kdatb(6) or                              "TR804
*       w_yearmo > ekko-kdate(6).                                "TR804
*      move 'E'         to returnmsg-type.                       "TR804
*      move 'FM'        to returnmsg-id.                         "TR804
*      move '999'       to returnmsg-number.                     "TR804
*      concatenate: 'CARE year/mo not within start/end dates f'  "TR804
*                   'or PO number: '                             "TR804
*                   w_contractno into w_returnmsg.               "TR804
*      move w_returnmsg to returnmsg-message.                    "TR804
*      append returnmsg.                                         "TR804
*      w_error_flag = 'Y'.                                       "TR804
*    endif.                                                      "TR804
*  endif.                                                        "TR804

  select count( * ) from ekpo
    into w_count
   where ebeln between '4100000000' and '4199999999'             "TR804
     and konnr = w_sapcontract                                   "TR804
     and bednr = w_yearmo
     and loekz = ' '.

  if sy-subrc <> 0.
    move 'E'         to returnmsg-type.
    move 'FM'        to returnmsg-id.
    move '999'       to returnmsg-number.
    concatenate: 'No line items found for PO number: '
                 w_contractno into w_returnmsg.

    move w_returnmsg to returnmsg-message.
    append returnmsg.
    w_error_flag = 'Y'.
  else.
    if w_count <> 1.
      move 'E'         to returnmsg-type.
      move 'FM'        to returnmsg-id.
      move '999'       to returnmsg-number.
      concatenate: 'Duplicate line items found for PO number: '
                   w_contractno into w_returnmsg.
      move w_returnmsg to returnmsg-message.
      append returnmsg.
      w_error_flag = 'Y'.
    else.
      select ebeln ebelp packno from ekpo
        into (w_ebeln, w_ebelp, w_ekpo_packno)
       where ebeln between '4100000000' and '4199999999'         "TR804
         and konnr = w_sapcontract                               "TR804
         and bednr = w_yearmo
         and loekz = ' '.
      endselect.
    endif.
  endif.

*****Step 2C************************************************************

  clear: w_sub_packno, w_esll_packno, w_introw, w_srvpos.

  select matkl from asmd into w_matkl
   where asnum = w_sapsrvcmat.

    if sy-subrc = 0.
      if w_matkl <> '2099'.
        select sub_packno from esll
          into w_sub_packno
         where packno = w_ekpo_packno.
        endselect.

        if sy-subrc = 0.
          select packno introw srvpos from esll
            into (w_esll_packno, w_introw, w_srvpos)
           where packno = w_sub_packno
             and srvpos = w_sapsrvcmat.
          endselect.
        endif.
      endif.
    endif.
  endselect.

  if w_error_flag = 'Y'.
    exit.
  endif.

*****Step 3*************************************************************

* Call BAPI: BAPI_ENTRYSHEET_CREATE

  perform create_entrysheet.

  move w_entrysheetno to entrysheetno.
  loop at tbl_bapi_esreturn.
    if tbl_bapi_esreturn-type = 'E' or
       tbl_bapi_esreturn-type = 'S' or
       tbl_bapi_esreturn-type = 'A'.
      move tbl_bapi_esreturn to returnmsg.
      append returnmsg.
    endif.
  endloop.
  wait up to 1 seconds.
endfunction.

***********************************************************************
*      Form  delete_entrysheet
***********************************************************************
*       text
***********************************************************************
*      -->W_SAPDOCID text
***********************************************************************
form delete_entrysheet using w_sapdocid
                             p_doc_date TYPE sy-datum.
* Cancel the previous Service Entry Sheet

  data: w_entrysheet type bapiessrc-sheet_no,
        lv_pdate TYPE sy-datum.

  move w_sapdocid to w_entrysheet.

  refresh tbl_bapi_esreturn.
  clear tbl_bapi_esreturn.
*********SAHMAD--- Posting date for cancelled document
  IF w_perclose_flag = 'Y'.
    CONCATENATE w_frye1 w_frpe1+1(2) '01'
           INTO lv_pdate.
  ELSE.
*    move w_per_lastday to lv_pdate.
    MOVE p_doc_date TO lv_pdate.
  ENDIF.
**********************
*  call function 'BBP_ENTRYSHEET_CANCEL'
*    exporting
*      entrysheet = w_entrysheet
*    tables
*      return     = tbl_bapi_esreturn.
CALL FUNCTION 'Z_BBP_ENTRYSHEET_CANCEL'
  EXPORTING
    entrysheet         = w_entrysheet
    POSTING_DATE       = lv_pdate
 TABLES
    RETURN             = tbl_bapi_esreturn.

*  Commit the work or rollback the BAPI call.

  if tbl_bapi_esreturn-type = 'E'.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  else.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  endif.
  wait up to 1 seconds.
endform.                    "delete_entrysheet

***********************************************************************
*      Form  create_entrysheet
***********************************************************************
form create_entrysheet.

* Populate BAPI parameters

  refresh: tbl_bapi_eshdr, tbl_bapi_essrvcs.
  clear: tbl_bapi_eshdr, tbl_bapi_essrvcs.

  concatenate 'CARE' w_sapbatchid into tbl_bapi_eshdr-ext_number.
  concatenate w_from_tradeloc '_' w_to_tradeloc
                                  into tbl_bapi_eshdr-location.
  tbl_bapi_eshdr-ref_date         = w_per_firstday.
  tbl_bapi_eshdr-begdate          = w_per_firstday.
  tbl_bapi_eshdr-enddate          = w_per_lastday.
  tbl_bapi_eshdr-pckg_no          = '0000000001'.

  if w_sapsrvcmat = w_nosrvcmatch.
    tbl_bapi_eshdr-short_text     = w_sapsrvcmat.
  else.
    tbl_bapi_eshdr-short_text     = w_srvpos.
  endif.

  tbl_bapi_eshdr-po_number        = w_ebeln.
  tbl_bapi_eshdr-po_item          = w_ebelp.
  if w_matkl = '2099'.
    tbl_bapi_eshdr-block_ind      = 'X'.
  else.
    tbl_bapi_eshdr-block_ind      = ' '.
  endif.
  tbl_bapi_eshdr-doc_date         = w_per_lastday.

  if w_perclose_flag = 'Y'.
    concatenate w_frye1 w_frpe1+1(2) '01'
           into tbl_bapi_eshdr-post_date.
  else.
    move w_per_lastday to tbl_bapi_eshdr-post_date.
  endif.

  concatenate w_serv_type '_' w_contid
                                  into tbl_bapi_eshdr-ref_doc_no.
  tbl_bapi_eshdr-accasscat        = 'K'.
  tbl_bapi_eshdr-user_field       = w_sapcontract.
  if w_matkl = '2099'.
    tbl_bapi_eshdr-acceptance       = ' '.
  else.
    tbl_bapi_eshdr-acceptance       = 'X'.
  endif.

  append tbl_bapi_eshdr.

* Create 1st detail record.

  tbl_bapi_essrvcs-pckg_no        = '0000000001'.
  tbl_bapi_essrvcs-line_no        = '0000000001'.
  tbl_bapi_essrvcs-subpckg_no     = '0000000002'.
  append tbl_bapi_essrvcs.

* Create 2nd detail record.
  clear tbl_bapi_essrvcs.

  tbl_bapi_essrvcs-pckg_no        = '0000000002'.
  tbl_bapi_essrvcs-line_no        = '0000000002'.
  tbl_bapi_essrvcs-ext_line       = '0000000010'.
  tbl_bapi_essrvcs-subpckg_no     = '0000000000'.

  if w_sapsrvcmat = w_nosrvcmatch.
    tbl_bapi_essrvcs-service     = w_sapsrvcmat.
  else.
    tbl_bapi_essrvcs-service     = w_srvpos.
  endif.
  if w_matkl = '2099'.
    tbl_bapi_essrvcs-service     = ' '.
  endif.

* Change quantity to positive amount.  Neg amts not allowed
  if w_qty_num < 0.
    w_qty_num = w_qty_num * -1.
  endif.

  if w_matkl = '2099'.
    tbl_bapi_essrvcs-quantity    = 0.
  else.
    tbl_bapi_essrvcs-quantity    = w_qty_num.
  endif.

  if w_sapsrvcmat <> w_nosrvcmatch.
    tbl_bapi_essrvcs-pln_pckg    = w_esll_packno.
  endif.
  if w_matkl = '2099'.
    tbl_bapi_essrvcs-pln_pckg  = 0.
  endif.

  if w_sapsrvcmat <> w_nosrvcmatch.
    tbl_bapi_essrvcs-pln_line  = w_introw.
  endif.
  if w_matkl = '2099'.
    tbl_bapi_essrvcs-pln_line  = 0.
  endif.

  if w_matkl = '2099'.
    tbl_bapi_essrvcs-short_text  = w_sapsrvcmat.
    tbl_bapi_essrvcs-inform      = 'X'.
  else.
    tbl_bapi_essrvcs-short_text  = ' '.
    tbl_bapi_essrvcs-inform      = ' '.
  endif.

  tbl_bapi_essrvcs-userf2_num     = w_qty_num.

  shift w_serv_type right deleting trailing space.
  shift w_rate_class right deleting trailing space.
  shift w_serv_class right deleting trailing space.

  concatenate w_serv_type w_contid w_rate_class w_serv_class
              w_overrunind w_from_tradelocid w_to_tradelocid
              w_serv_usage into tbl_bapi_essrvcs-userf1_txt.

  tbl_bapi_essrvcs-userf2_txt     = w_sapdocid.

  if w_sapsrvcmat = w_nosrvcmatch.
    tbl_bapi_essrvcs-gr_price     = '1.0000'.
  endif.
  append tbl_bapi_essrvcs.

* Create the Service Entry Sheet Document

  refresh tbl_bapi_esreturn.
  clear tbl_bapi_esreturn.

  call function 'BAPI_ENTRYSHEET_CREATE'
    exporting
      entrysheetheader   = tbl_bapi_eshdr
      testrun            = w_testrun
    importing
      entrysheet         = w_entrysheetno
    tables
      entrysheetservices = tbl_bapi_essrvcs
      return             = tbl_bapi_esreturn.

*  Commit the work or rollback the BAPI call.

  if w_entrysheetno <> ' '.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  endif.

endform.                    "create_entrysheet
