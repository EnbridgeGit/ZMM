report  zlmmi003_gassup_mthlypo message-id zs line-size 132
        line-count 65 no standard page heading.
************************************************************************
* Author      : Glenn Ymana                                            *
* Date Created: Feb 16, 2011                                           *
*----------------------------------------------------------------------*
* Description :                                                        *
* This program will run prior to the start of the month and will       *
* generate purchase orders with reference to the corresponding         *
* Transportation Services Contracts.                                   *
* This program will generate a new PO or new line item off an existing *
* PO for each contract number listed in the variant and release them   *
* if there are no errors. Once all POs have been released, a service   *
* entry sheet will be created for each PO.                             *
*                                                                      *
************************************************************************
* Date         Developer      Request #  Description                   *
************************************************************************
* 2011/09/30   GYMANA         TR804      Add Gross Price to PO Services
*
* 2012/09/13   GYMANA         TR804      Change program logic to allow
*                                        automated execution.
*                                        Search on '2012/09/13'
************************************************************************
type-pools:  slis.

tables:  tvarvc,
         zcarereference,
         ekko,
         ekpo,
         esll.

************************************************************************
*    SELECT OPTIONS                                                    *
************************************************************************
selection-screen begin of block box with frame title text-hdr.
select-options:  s_conid  for ekko-ebeln no intervals obligatory.
selection-screen begin of line.
*selection-screen comment 1(79) text-st1.                    "2012/09/13
selection-screen end   of line.
selection-screen skip.
parameters:      p_org    like ekko-ekorg obligatory default 'GASA',
                 p_sdate  like ekko-kdatb obligatory,
                 p_edate  like ekko-kdate obligatory,
                 p_frgke  like ekko-frgke obligatory default 'Y',
                 p_delind like ekko-loekz default ' '.
selection-screen skip.
selection-screen begin of block box2 with frame title text-hd1.
parameters:     p_newpo as checkbox default 'X'.     "Create New PO
parameters:     p_testrn as checkbox default 'X'.    "Test Run Only
selection-screen end of block box2.
selection-screen end of block box.
*----------------------------------------------------------------------*
*                  at selection-screen
*----------------------------------------------------------------------*
at selection-screen output.                                  "2012/09/13
                                                             "2012/09/13
  loop at screen.                                            "2012/09/13
* Grey out New PO checkbox                                   "2012/09/13
    if screen-name = 'P_NEWPO'.                              "2012/09/13
      screen-input = '0'.                                    "2012/09/13
      modify screen.                                         "2012/09/13
    endif.                                                   "2012/09/13
  endloop.                                                   "2012/09/13
************************************************************************
*                   INITIALIZATION                                     *
************************************************************************
initialization.

* Change parameter to use Current Date
  select * from tvarvc
   where name = 'ZMM_CURR_PER_FIRSTDAY'.                     "2012/09/13

    if sy-subrc = 0.
      p_sdate = tvarvc-low.
    endif.
  endselect.

  call function 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
    exporting
      day_in            = p_sdate
    importing
      last_day_of_month = p_edate.

  refresh s_conid.                                           "2012/09/13
  clear s_conid.                                             "2012/09/13
  move 'I' to s_conid-sign.                                  "2012/09/13
  move 'CP' to s_conid-option.                               "2012/09/13
  move '42*' to s_conid-low.                                 "2012/09/13
  append s_conid.                                            "2012/09/13

************************************************************************
*    VARIABLES                                                         *
************************************************************************

  data:   wa_contractid(10)   type n,      "contract ID
          wa_contract_ref(30) type c,      "contract reference
          wa_gas_day          type d,      "gas day
          wa_rate_class(8)    type c,      "rate class
          wa_offer_partyid(5) type n,      "offered by party ID
          wa_serv_usage(1)    type c,      "service usage
          wa_quantity(13)     type n,      "quantity
          wa_uom(8)           type c,      "unit of measure
          wa_lponr            like ekko-lponr,
          wa_next_line_item   like ekko-lponr,
          wa_packno           like esll-packno,
          wa_ponumber         like bapimepoheader-po_number,
          wa_entrysheetno     like bapiessr-sheet_no,
          wa_rel_status_new   like bapimmpara-rel_status,
          wa_rel_ind_new      like bapimmpara-po_rel_ind,
          wa_ret_code         like sy-subrc,
          infile(70),                      "input file with path
          inrec(100),                      "input record
          ln_cntr             type i value 99,
          fromdate            like sy-datum,
          todate              like sy-datum,
          w_err_flag          type c value 'N',
          w_batch_err_flag    type c value 'N',
          w_po_err_flag       type c value 'N',
          w_purchdoc          like ekko-ebeln,
*          w_sapcontract       like ekko-ebeln,              "2012/09/13
          w_currdate(6)       type c,
          w_mandt(5)          type c,
          msg(150)            type c.

* Selected Purchase Document internal table
  data: begin of itab_selpurdoc occurs 0,
          ebeln    like ekko-ebeln,
        end of itab_selpurdoc.

  data itab_selpurdoc2 like itab_selpurdoc occurs 0.

* purchasing data internal table
  data: begin of fs_purch,
          ebeln    like ekko-ebeln,
          bukrs    like ekko-bukrs,
          lponr    like ekko-lponr,
          ekorg    like ekko-ekorg,
          ekgrp    like ekko-ekgrp,
          zterm    like ekko-zterm,
          kdatb    like ekko-kdatb,
          kdate    like ekko-kdate,
          ebelp    like ekpo-ebelp,
          txz01    like ekpo-txz01,
          werks    like ekpo-werks,
          lgort    like ekpo-lgort,
          matkl    like ekpo-matkl,
          pstyp    like ekpo-pstyp,
          knttp    like ekpo-knttp,
          meins    like ekpo-meins, "BTBOUNDY
          bprme    like ekpo-bprme, "BTBOUNDY
          mwskz    like ekpo-mwskz,
          konnr    like ekpo-konnr,
          ktpnr    like ekpo-ktpnr,
          packno   like ekpo-packno,
          prdat    like ekpo-prdat,
        end of fs_purch.

*  data fs_po like fs_purch occurs 0 with header line.
  data fs_po like fs_purch.

  data: begin of itab_esll occurs 0,
          packno       like esll-packno,
          introw       like esll-introw,
          extrow       like esll-extrow,
          srvpos       like esll-srvpos,
          brtwr        like esll-brtwr,
          menge        like esll-menge,
          ktext1       like esll-ktext1,
          userf2_num   like esll-userf2_num,
          userf1_txt   like esll-userf1_txt,
          matkl        like esll-matkl,
        end of itab_esll.

  data: begin of det_rpttab occurs 0,
          po_number    like bapimepoheader-po_number,
          ses_number   like bapiessr-sheet_no,
          create_date  like sy-datum,
          valid_sdate  like sy-datum,
          valid_edate  like sy-datum,
          contractno   like ekko-ebeln,
        end of det_rpttab.

  data: begin of itab_con occurs 0,
          contractno   like ekko-ebeln,
          po_number    like ekko-ebeln,
        end of itab_con.

  data:   out_material_document like bapi2017_gm_head_ret-mat_doc,
          out_err_msg(200)    type c.

* BAPI tables
  data: begin of itab_bapi_poheader occurs 0.
          include structure bapimepoheader.
  data: end of itab_bapi_poheader.

  data: begin of itab_bapi_poheaderx occurs 0.
          include structure bapimepoheaderx.
  data: end of itab_bapi_poheaderx.

  data: begin of itab_bapi_poitem occurs 0.
          include structure bapimepoitem.
  data: end of itab_bapi_poitem.

  data: begin of itab_bapi_poitemx occurs 0.
          include structure bapimepoitemx.
  data: end of itab_bapi_poitemx.

  data: begin of itab_bapi_poaccountx occurs 0.
          include structure bapimepoaccountx.
  data: end of itab_bapi_poaccountx.

  data: begin of itab_bapi_poservices occurs 0.
          include structure bapiesllc.
  data: end of itab_bapi_poservices.

  data: begin of itab_bapi_poschedule occurs 0.
          include structure bapimeposchedule.
  data: end of itab_bapi_poschedule.

  data: begin of itab_bapi_poschedulx occurs 0.
          include structure bapimeposchedulx.
  data: end of itab_bapi_poschedulx.

  data: begin of itab_bapi_polimits occurs 0.
          include structure bapiesuhc.
  data: end of itab_bapi_polimits.

  data: begin of itab_bapi_return occurs 0.
          include structure bapiret2.
  data: end of itab_bapi_return.

  data: begin of itab_bapi_porelease occurs 0.
          include structure bapimmpara.
  data: end of itab_bapi_porelease.

  data: begin of itab_bapi_porel_ret occurs 0.
          include structure bapireturn.
  data: end of itab_bapi_porel_ret.

  data: begin of itab_bapi_eshdr occurs 0.
          include structure bapiessrc.
  data: end of itab_bapi_eshdr.

  data: begin of itab_bapi_essrvcs occurs 0.
          include structure bapiesllc.
  data: end of itab_bapi_essrvcs.

  data: begin of itab_bapi_esreturn occurs 0.
          include structure bapiret2.
  data: end of itab_bapi_esreturn.

************************************************************************
*    C O N S T A N T S                                                 *
************************************************************************
  data: c_doc_type(4)      type c value 'ZT',
        c_item_intvl(5)    type n value '00001',
        c_pckg_no1(10)     type n value '0000000001',
        c_pckg_no2(10)     type n value '0000000002',
        c_line_no(10)      type n value '0000000001',
        c_sched_lineno(4)  type n value '0001',
        c_del_date_cat(1)  type c value 'M'.

************************************************************************
*    START OF SELECTION (MAINLINE)                                     *
************************************************************************
start-of-selection.

  REFRESH ITAB_SELPURDOC.                                    "2012/09/13
  CLEAR   ITAB_SELPURDOC.                                    "2012/09/13

* Get list of purchasing docs from EKKO (Step 1A)            "2012/09/13
  select ebeln from ekko                                     "2012/09/13
    into itab_selpurdoc                                      "2012/09/13
   where ebeln in s_conid                                    "2012/09/13
     and loekz = p_delind                                    "2012/09/13
     and ekorg = p_org                                       "2012/09/13
     and frgke = p_frgke                                     "2012/09/13
     and kdatb <= p_sdate                                    "2012/09/13
     and kdate => p_edate                                    "2012/09/13
  order by ebeln.                                            "2012/09/13
                                                             "2012/09/13
* Check if purchasing doc on EKPO has a line item with no    "2012/09/13
* deletion flag set                                          "2012/09/13
     select * from ekpo                                      "2012/09/13
      where ebeln = itab_selpurdoc-ebeln                     "2012/09/13
        and loekz = ' '.                                     "2012/09/13
     endselect.                                              "2012/09/13
                                                             "2012/09/13
* If there is at least one line item in the document without "2012/09/13
* a delete flag, then keep the purchase document             "2012/09/13
     if sy-subrc = 0.                                        "2012/09/13
       append itab_selpurdoc.                                "2012/09/13
     endif.                                                  "2012/09/13
                                                             "2012/09/13
  endselect.                                                 "2012/09/13

  concatenate '(' sy-mandt ')' into w_mandt.
  perform generate_err_report_header.

  loop at itab_selpurdoc.                                    "2012/09/13

    if ln_cntr >= 55.
      perform generate_err_report_header.
    endif.

*    clear: w_sapcontract, w_purchdoc.
    move 'N' to w_err_flag.
    move 'N' to w_po_err_flag.

* Get data for PO using SAP contract number (Step 1B)
    if w_err_flag = 'N'.
      perform validate_purch_data.
    endif.

*   Retrieve Contract data to copy to PO (Step 1C)
    if w_err_flag = 'N'.
      perform select_purch_data.
    endif.

*   Execute BAPI routines if contract data was found (Step 2A).
    if w_err_flag = 'N'.
      perform execute_bapi_routines.
    endif.

    if w_err_flag = 'Y'.
      w_po_err_flag = 'Y'.
    endif.
  endloop.

* Generate all Service Entry Sheets using successfully released
* Purchase Orders.

  if p_testrn = ' '.
    loop at itab_con.
      perform create_new_ses.
    endloop.
  endif.

* Generate Successful Update Message if no errors

  if w_batch_err_flag = 'N' and
     w_po_err_flag = 'N' and
     p_testrn = ''.
    write: /41 '**** Update Run Successful ****'.
    add +1 to ln_cntr.
  endif.

  perform write_detail_report.

end-of-selection.

************************************************************************
*    FORMS                                                             *
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  validate_purch_data (1B)
*&---------------------------------------------------------------------*
form validate_purch_data.

* Get data for PO using SAP contract number                  "2012/09/13
                                                             "2012/09/13
  w_currdate = p_sdate(6).                                   "2012/09/13
                                                             "2012/09/13
  select * from ekpo                                         "2012/09/13
   where konnr = itab_selpurdoc-ebeln                        "2012/09/13
     and ebeln like '41%'                                    "2012/09/13
     and loekz = p_delind                                    "2012/09/13
     and bednr = w_currdate                                  "2012/09/13
   order by ebeln.                                           "2012/09/13
  endselect.                                                 "2012/09/13
                                                             "2012/09/13
  if sy-subrc = 0.                                           "2012/09/13
    w_err_flag = 'Y'.                                        "2012/09/13
    write: /3    'Release Order ',                           "2012/09/13
            17   ekpo-ebeln,                                 "2012/09/13
            28   'already exists for Contract ',             "2012/09/13
            56   itab_selpurdoc-ebeln.                       "2012/09/13
    skip.                                                    "2012/09/13
    add +2 to ln_cntr.                                       "2012/09/13
  endif.                                                     "2012/09/13

********** Commented Out - GYMANA - 2012/09/12 *************************
* Verify EKKO data for contract number
*
*  select * from ekko
*   where ebeln = s_conid.
*  endselect.
*
*  if sy-subrc <> 0.
*    w_err_flag = 'Y'.
*    write: /3    'Contract #',
*            16   s_conid,
*            28   ' not found on EKKO.'.
*    add +1 to ln_cntr.
*    exit.
*  endif.
*
* Check for existing Release order this Period (Step 1B)
*
*  if ekko-loekz <> p_delind.
*    w_err_flag = 'Y'.
*    write: /3    'Contract #',
*            16   s_conid,
*            28   ' Deletion indicator not blank on EKKO record'.
*    add +1 to ln_cntr.
*  endif.
*  if ekko-ekorg <> p_org.
*    w_err_flag = 'Y'.
*    write: /3    'Contract #',
*            16   s_conid,
*            28   ' Purchase Org on EKKO record does not match variant'.
*    add +1 to ln_cntr.
*  endif.
*  if ekko-kdatb > p_sdate or
*     ekko-kdate < p_edate.
*    w_err_flag = 'Y'.
*    write: /3    'Contract #',
*            16   s_conid,
*            28   'PO / Contract validity start/end dates falls',
*            73   'within the variant date range.'.
*    write: /28   'Adjust dates before proceeding.'.
*    add +2 to ln_cntr.
*  endif.
*  if ekko-frgke <> 'Y'.
*    w_err_flag = 'Y'.
*    write: /3    'Contract #',
*            16   s_conid,
*            28   ' Release indicator on EKKO record not set to "Y".'.
*    add +1 to ln_cntr.
*  endif.

* Verify EKKO data for purchase document

*  if w_purchdoc <> ''.
*    select * from ekko
*     where ebeln = w_purchdoc.
*    endselect.
*
*    if sy-subrc <> 0.
*      w_err_flag = 'Y'.
*      write: /3    'Purch Doc #',
*              16   w_purchdoc,
*              28   ' not found on EKKO.'.
*      add +1 to ln_cntr.
*      exit.
*    endif.
*
*    if ekko-loekz <> p_delind.
*      w_err_flag = 'Y'.
*      write: /3    'Purch Doc #',
*              16   w_purchdoc,
*              28   ' Deletion indicator not blank on EKKO record'.
*      add +1 to ln_cntr.
*    endif.
*    if ekko-ekorg <> p_org.
*      w_err_flag = 'Y'.
*      write: /3   'Purch Doc #',
*              16  w_purchdoc,
*              28  ' Purchase Org on EKKO record does not match
*variant'.
*      add +1 to ln_cntr.
*    endif.
*    if ekko-kdatb > p_sdate and
*       ekko-kdate < p_edate.
*      w_err_flag = 'Y'.
*      write: /3    'Purch Doc #',
*              16   w_purchdoc,
*              28   'PO / Contract validity start/end dates falls',
*              73   'within the variant date range.'.
*      write: /28   'Adjust dates before proceeding.'.
*      add +2 to ln_cntr.
*    endif.
*    if ekko-frgke <> 'Y'.
*      w_err_flag = 'Y'.
*      write: /3    'Purch Doc #',
*              16   w_purchdoc,
*              28   ' Release indicator on EKKO record not set to "Y".'.
*      add +1 to ln_cntr.
*    endif.
*    if w_err_flag = 'N'.
*      move ekko-lponr to wa_lponr.
*    endif.
*  endif.
*
*  if w_err_flag = 'Y'.
*    skip.
*    add +1 to ln_cntr.
*  endif.
*
endform.                    "validate_purch_data

*&---------------------------------------------------------------------*
*&      Form  select_purch_data (1B & 1C)
*&---------------------------------------------------------------------*
form select_purch_data.

* Get data for PO using SAP contract number
  select ekko~ebeln ekko~bukrs ekko~lponr ekko~ekorg ekko~ekgrp
         ekko~zterm ekko~kdatb ekko~kdate ekpo~ebelp ekpo~txz01
         ekpo~werks ekpo~lgort ekpo~matkl ekpo~pstyp ekpo~knttp
         ekpo~meins ekpo~bprme  "BTBOUNDY
         ekpo~mwskz ekpo~konnr ekpo~ktpnr ekpo~packno ekpo~prdat
    into fs_purch
    from ( ekko inner join ekpo
             on ekko~ebeln = ekpo~ebeln )
   where ekko~ebeln = itab_selpurdoc-ebeln                  "2012/09/13
     and ekko~loekz = p_delind
     and ekko~ekorg = p_org
     and ekko~kdatb <= p_sdate
     and ekko~kdate >= p_edate
     and ekko~frgke = 'Y'
   order by ekko~ebeln.
  endselect.

  if sy-subrc <> 0.
    w_err_flag = 'Y'.
  endif.
* Select ESLL Data

  clear: wa_ponumber, wa_entrysheetno.

  refresh itab_esll.
  clear itab_esll.

  select * from esll
   where packno = fs_purch-packno
     and package = 'X'.
  endselect.

  if sy-subrc = 0.
    move esll-sub_packno to wa_packno.
    select * from esll
     where packno = wa_packno
       and del <> 'X'.

      if sy-subrc = 0.
        move-corresponding esll to itab_esll.
        append itab_esll.
      else.
        w_err_flag = 'Y'.
      endif.
    endselect.
  else.
    w_err_flag = 'Y'.
  endif.

  if w_err_flag = 'Y'.
    write: /3    'Contract #',
            16   itab_selpurdoc-ebeln,                       "2012/09/13
            28   ' - not all data found on EKKO/EKPO/ESLL.'.
    skip.
    add +2 to ln_cntr.
  endif.

endform.                    "select_purch_data

*&---------------------------------------------------------------------*
*&      Form  execute_bapi_routines (2A - 2B)
*&---------------------------------------------------------------------*
form execute_bapi_routines.
  if p_newpo <> ' '.
    perform create_new_po.
  else.
*    perform add_line_item_to_po.                            "2012/09/13
  endif.

* Continue with PO release and SES create if the new PO has been
* created or line item was added to existing PO successfully.

  if w_err_flag = 'N' and
     p_testrn = ' '.

    select single * from ekko
     where ebeln = wa_ponumber.

*   check if the document has been released and execute BAPI
*   if it has not been released.
    if ekko-frgke = '' or
       ekko-frgke = 'X'.
      perform setup_po_release.
      perform call_fm_po_release.

      if wa_rel_ind_new <> 'Y'.
*         Write PO release errors to the error report.
        write: /3 'Purchase Doc: ',
               18 wa_ponumber.
        skip.
        add +2 to ln_cntr.
        loop at itab_bapi_porel_ret.
          write: /3  'PO Release Error:',
          23  itab_bapi_porel_ret-type,
          26  itab_bapi_porel_ret-code,
          35  itab_bapi_porel_ret-message.
          add +1 to ln_cntr.
        endloop.
        skip.
        add +1 to ln_cntr.
        w_err_flag = 'Y'.
      endif.
    endif.
  endif.

  if w_err_flag = 'N'.
    move itab_selpurdoc-ebeln to itab_con-contractno.        "2012/09/13
    move wa_ponumber  to itab_con-po_number.
    append itab_con.
  endif.

endform.                    "execute_bapi_routines

*&---------------------------------------------------------------------*
*&      Form  create_new_ses
*&---------------------------------------------------------------------*
form create_new_ses.

  move 'N' to w_err_flag.
  move itab_con-po_number to wa_ponumber.
  perform setup_ses_create.

*     Do not run SES create if Demand service not found on ESLL
  if w_err_flag = 'N'.
    perform call_fm_ses_create.

    if wa_entrysheetno = ' '.
*         Write SES create errors to the error report.
      write: /3 'P.O. Number: ',
             18 wa_ponumber.
      skip.
      add +2 to ln_cntr.
      loop at itab_bapi_esreturn.
        write: /3  'SES Create Error:',
        23  itab_bapi_esreturn-type,
        26  itab_bapi_esreturn-number,
        35  itab_bapi_esreturn-message.
        add +1 to ln_cntr.
      endloop.
      skip.
      add +1 to ln_cntr.
      w_err_flag = 'Y'.
      w_batch_err_flag = 'Y'.
    endif.
  endif.

  if w_err_flag = 'Y'.
    w_batch_err_flag = 'Y'.
  endif.

* Save detail data for the detail report
* Report is generate regardless of any SES errors.

  move itab_con-po_number   to det_rpttab-po_number.
  move wa_entrysheetno      to det_rpttab-ses_number.
  move sy-datum             to det_rpttab-create_date.
  move fs_po-kdatb          to det_rpttab-valid_sdate.
  move fs_po-kdate          to det_rpttab-valid_edate.
  move itab_con-contractno  to det_rpttab-contractno.
  append det_rpttab.

endform.                    "create_new_ses

*&---------------------------------------------------------------------*
*&      Form  create_new_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_new_po.

  perform setup_po_create.
  perform call_fm_po_create.

  if wa_ponumber = ' '.
*   Write PO create errors to the error report.
    write: /3 'Contract #: ',
           18 fs_purch-ebeln.
    skip.
    add +2 to ln_cntr.
    loop at itab_bapi_return.
      write: /3  'PO Create Error:',
             23  itab_bapi_return-type,
             26  itab_bapi_return-number,
             35  itab_bapi_return-message.
      add +1 to ln_cntr.
    endloop.
    skip.
    add +1 to ln_cntr.
  endif.

endform.                    "create_new_po


*&---------------------------------------------------------------------*
*&      Form  SETUP_PO_CREATE (Step 2A)
*&---------------------------------------------------------------------*
form setup_po_create.

  clear:   itab_bapi_poheader,
           itab_bapi_poheaderx,
           itab_bapi_poitem,
           itab_bapi_poitemx,
           itab_bapi_poservices,
           itab_bapi_poschedule,
           itab_bapi_poschedulx,
           itab_bapi_polimits,
           itab_bapi_poaccountx,
           itab_bapi_return.
  refresh: itab_bapi_poheader,
           itab_bapi_poheaderx,
           itab_bapi_poitem,
           itab_bapi_poitemx,
           itab_bapi_poservices,
           itab_bapi_poschedule,
           itab_bapi_poschedulx,
           itab_bapi_polimits,
           itab_bapi_poaccountx,
           itab_bapi_return.


* Setup PO Header
  move fs_purch-bukrs   to itab_bapi_poheader-comp_code.
  move c_doc_type       to itab_bapi_poheader-doc_type.
  move c_item_intvl     to itab_bapi_poheader-item_intvl.
  move fs_purch-ekorg   to itab_bapi_poheader-purch_org.
  move fs_purch-ekgrp   to itab_bapi_poheader-pur_group.
  move fs_purch-zterm   to itab_bapi_poheader-pmnttrms.
  move sy-datum         to itab_bapi_poheader-doc_date.
  move fs_purch-kdatb   to itab_bapi_poheader-vper_start.
  move fs_purch-kdate   to itab_bapi_poheader-vper_end.
  move fs_purch-ebeln   to itab_bapi_poheader-agreement.
  append itab_bapi_poheader.

* Setup PO HeaderX
  move 'X' to: itab_bapi_poheaderx-comp_code,
               itab_bapi_poheaderx-doc_type,
               itab_bapi_poheaderx-vendor,
               itab_bapi_poheaderx-pmnttrms,
               itab_bapi_poheaderx-purch_org,
               itab_bapi_poheaderx-pur_group,
               itab_bapi_poheaderx-currency,
               itab_bapi_poheaderx-doc_date,
               itab_bapi_poheaderx-vper_start,
               itab_bapi_poheaderx-vper_end,
               itab_bapi_poheaderx-agreement.
  append itab_bapi_poheaderx.

* Setup PO Item

  move fs_purch-ebelp to itab_bapi_poitem-po_item.
  move fs_purch-txz01 to itab_bapi_poitem-short_text.
  move fs_purch-werks to itab_bapi_poitem-plant.
  move 'A001'         to itab_bapi_poitem-stge_loc.
  move p_sdate(6)     to itab_bapi_poitem-trackingno.
  move fs_purch-matkl to itab_bapi_poitem-matl_group.
  move fs_purch-pstyp to itab_bapi_poitem-item_cat.
  move fs_purch-knttp to itab_bapi_poitem-acctasscat.
  move fs_purch-meins to itab_bapi_poitem-po_unit. "BTBOUNDY
  move fs_purch-bprme to itab_bapi_poitem-orderpr_un. "BTBOUNDY
  move fs_purch-mwskz to itab_bapi_poitem-tax_code.
  move fs_purch-ebeln to itab_bapi_poitem-agreement.
  move fs_purch-ebelp to itab_bapi_poitem-agmt_item.
  move c_pckg_no1     to itab_bapi_poitem-pckg_no.
  move p_sdate        to itab_bapi_poitem-price_date.
  append itab_bapi_poitem.

* Setup PO ItemX

  move fs_purch-ebelp to itab_bapi_poitemx-po_item.
  move 'X'            to: itab_bapi_poitemx-short_text,
                          itab_bapi_poitemx-plant,
                          itab_bapi_poitemx-stge_loc,
                          itab_bapi_poitemx-trackingno,
                          itab_bapi_poitemx-matl_group,
                          itab_bapi_poitemx-tax_code,
                          itab_bapi_poitemx-item_cat,
                          itab_bapi_poitemx-acctasscat,
                          itab_bapi_poitemx-agreement,
                          itab_bapi_poitemx-agmt_item,
                          itab_bapi_poitemx-pckg_no.
  append itab_bapi_poitemx.

* Setup PO Limits

  move c_pckg_no1 to itab_bapi_polimits-pckg_no.
  move 'X' to:       itab_bapi_polimits-no_limit,
                     itab_bapi_polimits-no_frlimit.
  append itab_bapi_polimits.

* Setup PO Services

  move c_pckg_no1 to itab_bapi_poservices-pckg_no.
  move c_line_no  to itab_bapi_poservices-line_no.
  move c_pckg_no2 to itab_bapi_poservices-subpckg_no.
  append itab_bapi_poservices.
  clear itab_bapi_poservices.

  loop at itab_esll.
    move c_pckg_no2       to itab_bapi_poservices-pckg_no.
    move itab_esll-introw to itab_bapi_poservices-line_no.
    move itab_esll-extrow to itab_bapi_poservices-ext_line.
    move itab_esll-srvpos to itab_bapi_poservices-service.
    move itab_esll-ktext1 to itab_bapi_poservices-short_text.
    if itab_esll-matkl = '2007'.
      move itab_esll-brtwr  to itab_bapi_poservices-gr_price.
    endif.
    if itab_esll-userf2_num <> 0.
      if itab_esll-matkl = '2002'.
        move itab_esll-userf2_num to itab_bapi_poservices-quantity.
        itab_bapi_poservices-quantity = itab_bapi_poservices-quantity
                                      * p_edate+6(2).
      else.
        move itab_esll-userf2_num to itab_bapi_poservices-quantity.
      endif.
    else.
      move 1   to itab_bapi_poservices-quantity.
    endif.
    move 'X'                  to itab_bapi_poservices-ovf_unlim.
    move 'X'                  to itab_bapi_poservices-price_chg.
    move itab_esll-userf2_num to itab_bapi_poservices-userf2_num.
    move itab_esll-userf1_txt to itab_bapi_poservices-userf1_txt.
    append itab_bapi_poservices.
    clear itab_bapi_poservices.
  endloop.

* Setup PO Schedule

  move fs_purch-ebelp      to itab_bapi_poschedule-po_item.
  move c_sched_lineno      to itab_bapi_poschedule-sched_line.
  move c_del_date_cat      to itab_bapi_poschedule-del_datcat_ext.
  concatenate p_sdate+4(2) p_sdate(4) into
                           itab_bapi_poschedule-delivery_date.
  append itab_bapi_poschedule.

* Setup PO Schedulx

  move fs_purch-ebelp      to itab_bapi_poschedulx-po_item.
  move c_sched_lineno      to itab_bapi_poschedulx-sched_line.
  move 'X'                 to: itab_bapi_poschedulx-po_itemx,
                               itab_bapi_poschedulx-sched_linex,
                               itab_bapi_poschedulx-del_datcat_ext,
                               itab_bapi_poschedulx-delivery_date.
  append itab_bapi_poschedulx.

* Setup PO AccountX

  move fs_purch-ebelp      to itab_bapi_poaccountx-po_item.
  move '01'                to itab_bapi_poaccountx-serial_no.
  move 'X' to: itab_bapi_poaccountx-quantity,
               itab_bapi_poaccountx-net_value,
               itab_bapi_poaccountx-gl_account,
               itab_bapi_poaccountx-costcenter,
               itab_bapi_poaccountx-co_area.
  append itab_bapi_poaccountx.

endform.                    "setup_po_create

*&---------------------------------------------------------------------*
*&      Form CALL_FM_PO_CREATE (Step 2A)
*&---------------------------------------------------------------------*

form call_fm_po_create.

  set update task local.
  call function 'BAPI_PO_CREATE1'
  exporting
       poheader           = itab_bapi_poheader
       poheaderx          = itab_bapi_poheaderx
       testrun            = p_testrn
  importing
       exppurchaseorder   = wa_ponumber
*"       EXPHEADER          = BAPIMEPOHEADER
*"       EXPPOEXPIMPHEADER  = BAPIEIKP
  tables
       return             = itab_bapi_return
       poitem             = itab_bapi_poitem
       poitemx            = itab_bapi_poitemx
       poschedule         = itab_bapi_poschedule
       poschedulex        = itab_bapi_poschedulx
       poaccountx         = itab_bapi_poaccountx
       polimits           = itab_bapi_polimits
       poservices         = itab_bapi_poservices.

*  Commit the work or rollback the BAPI call.

  if wa_ponumber <> ' '.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    move 'Y' to w_err_flag.
    move 'Y' to w_po_err_flag.
  endif.

endform.                    "call_fm_po_create

*&---------------------------------------------------------------------*
*&      Form  add_line_item_to_po
*&
*& **NOTE - This step has been disabled and will no longer be run**
*& **       GYMANA 2012/09/13
*&---------------------------------------------------------------------*

form add_line_item_to_po.

  perform setup_po_change.
  perform call_fm_po_change.

  if wa_ponumber = ' ' or
     p_testrn <> ' '.
*   Write PO change errors to the error report.
    write: /3 'Purchase Doc #: ',
           19 wa_ponumber.
    skip.
    add +2 to ln_cntr.
    loop at itab_bapi_return.
      write: /3  'PO Change Error:',
             23  itab_bapi_return-type,
             26  itab_bapi_return-number,
             35  itab_bapi_return-message.
      add +1 to ln_cntr.
    endloop.
    skip.
    add +1 to ln_cntr.
  endif.

endform.                    "add_line_item_to_po

*&---------------------------------------------------------------------*
*&      Form  SETUP_PO_CHANGE (Step 2B)
*&
*& **NOTE - This step has been disabled and will no longer be run**
*& **       GYMANA 2012/09/13
*&---------------------------------------------------------------------*
form setup_po_change.

  clear:   itab_bapi_poitem,
           itab_bapi_poitemx,
           itab_bapi_poservices,
           itab_bapi_poschedule,
           itab_bapi_poschedulx,
           itab_bapi_polimits,
           itab_bapi_return.
  refresh: itab_bapi_poitem,
           itab_bapi_poitemx,
           itab_bapi_poservices,
           itab_bapi_poschedule,
           itab_bapi_poschedulx,
           itab_bapi_polimits,
           itab_bapi_return.


* Setup PO Item
  wa_next_line_item = wa_lponr + 1.
  move wa_next_line_item     to itab_bapi_poitem-po_item.
  move fs_purch-txz01        to itab_bapi_poitem-short_text.
  move fs_purch-werks        to itab_bapi_poitem-plant.
  move 'A001'                to itab_bapi_poitem-stge_loc.
  move p_sdate(6)            to itab_bapi_poitem-trackingno.
  move fs_purch-matkl        to itab_bapi_poitem-matl_group.
  move fs_purch-pstyp        to itab_bapi_poitem-item_cat.
  move fs_purch-knttp        to itab_bapi_poitem-acctasscat.
  move fs_purch-meins        to itab_bapi_poitem-po_unit. "BTBOUNDY
  move fs_purch-bprme        to itab_bapi_poitem-orderpr_un. "BTBOUNDY
  move fs_purch-mwskz        to itab_bapi_poitem-tax_code.
  move itab_selpurdoc-ebeln  to itab_bapi_poitem-agreement.  "2012/09/13
  move fs_purch-ebelp        to itab_bapi_poitem-agmt_item.
*  MOVE wa_lponr              TO itab_bapi_poitem-agmt_item.
  move c_pckg_no1            to itab_bapi_poitem-pckg_no.
  move p_sdate               to itab_bapi_poitem-price_date.
  append itab_bapi_poitem.

* Setup PO ItemX

  itab_bapi_poitemx-po_item = wa_next_line_item.
  move 'X' to: itab_bapi_poitemx-po_itemx,
               itab_bapi_poitemx-short_text,
               itab_bapi_poitemx-plant,
               itab_bapi_poitemx-stge_loc,
               itab_bapi_poitemx-trackingno,
               itab_bapi_poitemx-matl_group,
               itab_bapi_poitemx-tax_code,
               itab_bapi_poitemx-item_cat,
               itab_bapi_poitemx-acctasscat,
               itab_bapi_poitemx-agreement,
               itab_bapi_poitemx-agmt_item,
               itab_bapi_poitemx-pckg_no.
  append itab_bapi_poitemx.

* Setup PO Limits

  move c_pckg_no1 to itab_bapi_polimits-pckg_no.
  move 'X' to:       itab_bapi_polimits-no_limit,
                     itab_bapi_polimits-no_frlimit.
  append itab_bapi_polimits.

* Setup PO Services

  move c_pckg_no1  to itab_bapi_poservices-pckg_no.
  move c_line_no   to itab_bapi_poservices-line_no.
  move c_pckg_no2  to itab_bapi_poservices-subpckg_no.
  append itab_bapi_poservices.
  clear itab_bapi_poservices.

  loop at itab_esll.
    move c_pckg_no2       to itab_bapi_poservices-pckg_no.
    move itab_esll-introw to itab_bapi_poservices-line_no.
    move itab_esll-extrow to itab_bapi_poservices-ext_line.
    move itab_esll-srvpos to itab_bapi_poservices-service.
    move itab_esll-ktext1 to itab_bapi_poservices-short_text.
    if itab_esll-matkl = '2007'.
      move itab_esll-brtwr  to itab_bapi_poservices-gr_price.
    endif.
    if itab_esll-userf2_num <> 0.
      if itab_esll-matkl = '2002'.
        move itab_esll-userf2_num to itab_bapi_poservices-quantity.
        itab_bapi_poservices-quantity = itab_bapi_poservices-quantity
                                      * p_edate+6(2).
      else.
        move itab_esll-userf2_num to itab_bapi_poservices-quantity.
      endif.
    else.
      move 1   to itab_bapi_poservices-quantity.
    endif.
    move 'X'                  to itab_bapi_poservices-ovf_unlim.
    move 'X'                  to itab_bapi_poservices-price_chg.
    move itab_esll-userf2_num to itab_bapi_poservices-userf2_num.
    move itab_esll-userf1_txt to itab_bapi_poservices-userf1_txt.
    append itab_bapi_poservices.
    clear itab_bapi_poservices.
  endloop.

* Setup PO Schedule

  move wa_next_line_item    to itab_bapi_poschedule-po_item.
  move c_sched_lineno       to itab_bapi_poschedule-sched_line.
  move c_del_date_cat       to itab_bapi_poschedule-del_datcat_ext.
  concatenate p_sdate+4(2) p_sdate(4) into
                           itab_bapi_poschedule-delivery_date.
  append itab_bapi_poschedule.

* Setup PO Schedulx

  move wa_next_line_item    to itab_bapi_poschedulx-po_item.
  move c_sched_lineno       to itab_bapi_poschedulx-sched_line.
  move 'X' to: itab_bapi_poschedulx-po_itemx,
               itab_bapi_poschedulx-sched_linex,
               itab_bapi_poschedulx-del_datcat_ext,
               itab_bapi_poschedulx-delivery_date.
  append itab_bapi_poschedulx.

endform.                    "setup_po_create

*&---------------------------------------------------------------------*
*&      Form CALL_FM_PO_CHANGE (Step 2B)
*&
*& **NOTE - This step has been disabled and will no longer be run**
*& **       GYMANA 2012/09/13
*&---------------------------------------------------------------------*

form call_fm_po_change.

  set update task local.
  call function 'BAPI_PO_CHANGE'
  exporting
       purchaseorder      = w_purchdoc
       testrun            = p_testrn
*  importing
  tables
       return             = itab_bapi_return
       poitem             = itab_bapi_poitem
       poitemx            = itab_bapi_poitemx
       poschedule         = itab_bapi_poschedule
       poschedulex        = itab_bapi_poschedulx
       polimits           = itab_bapi_polimits
       poservices         = itab_bapi_poservices.

*  Commit the work or rollback the BAPI call.
  loop at itab_bapi_return.
    if itab_bapi_return-type = 'E'.
      move 'Y' to w_err_flag.
      move 'Y' to w_po_err_flag.
    endif.
  endloop.

  if w_err_flag = 'Y'.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  else.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
    move w_purchdoc to wa_ponumber.
  endif.

endform.                    "call_fm_po_change

*&---------------------------------------------------------------------*
*&      Form  setup_po_release
*&---------------------------------------------------------------------*
form setup_po_release.

  refresh: itab_bapi_porelease.
  clear:   itab_bapi_porelease.

* Setup PO Release fields

  move wa_ponumber      to itab_bapi_porelease-po_number.
  move 'GS'             to itab_bapi_porelease-po_rel_cod.

endform.                    "setup_po_release

*&---------------------------------------------------------------------*
*&      Form  call_fm_po_release
*&---------------------------------------------------------------------*

form call_fm_po_release.

  set update task local.
  call function 'BAPI_PO_RELEASE'
    exporting
      purchaseorder     = itab_bapi_porelease-po_number
      po_rel_code       = itab_bapi_porelease-po_rel_cod
      use_exceptions    = ' '
      no_commit         = p_testrn
    importing
      rel_status_new    = wa_rel_status_new
      rel_indicator_new = wa_rel_ind_new
      ret_code          = wa_ret_code
    tables
      return            = itab_bapi_porel_ret.

*  Commit the work or rollback the BAPI call.

  if wa_rel_ind_new = 'Y'.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    move 'Y' to w_err_flag.
  endif.

*  WAIT UP TO 10 SECONDS.
endform.                    "call_fm_po_release

*&---------------------------------------------------------------------*
*&      Form  setup_ses_create
*&---------------------------------------------------------------------*
form setup_ses_create.

  refresh: itab_esll.
  clear: fs_po, itab_esll, wa_entrysheetno.

  select * from ekko
   where ebeln = wa_ponumber
     and loekz = p_delind.
  endselect.

  if sy-subrc = 0.
    move-corresponding ekko to fs_po.
  endif.

* Field LPONR will determine which item no of the PO the SES will be
* attached to. If New PO: first item no is used.
* If change PO, most recent item no is used.

  select * from ekpo
   where ebeln = wa_ponumber
     and ebelp = fs_po-lponr
     and loekz = p_delind.
  endselect.

  if sy-subrc = 0.
    move-corresponding ekpo to fs_po.
  endif.

  select * from esll
   where packno = fs_po-packno.
  endselect.

  if sy-subrc = 0.
    move esll-sub_packno to wa_packno.
    select * from esll
     where packno = wa_packno
       and ( matkl = '2001' OR matkl = '2007' ).

      if sy-subrc = 0.
        move-corresponding esll to itab_esll.
        append itab_esll.
      endif.
    endselect.

    if sy-subrc <> 0.
      write: /3    'P.O. #',
              16   wa_ponumber,
              28   '- Matl Group 2001 (Demand Service) not found.'.
      write: /28   '- Service Entry Sheet not created.'.
      skip.
      add +3 to ln_cntr.
      move 'Y' to w_err_flag.
    endif.
  endif.

* Populate BAPI parameters

  clear: itab_bapi_eshdr, itab_bapi_essrvcs.
  refresh: itab_bapi_eshdr, itab_bapi_essrvcs.


  itab_bapi_eshdr-ext_number       = 'Demand'.
  itab_bapi_eshdr-location         = fs_po-txz01.
  itab_bapi_eshdr-ref_date         = p_edate.
  itab_bapi_eshdr-begdate          = p_sdate.
  itab_bapi_eshdr-enddate          = p_edate.
  itab_bapi_eshdr-pckg_no          = '0000000001'.
  itab_bapi_eshdr-short_text       = itab_esll-srvpos.
  itab_bapi_eshdr-po_number        = fs_po-ebeln.
  itab_bapi_eshdr-po_item          = fs_po-lponr.
  itab_bapi_eshdr-doc_date         = sy-datum.
  itab_bapi_eshdr-post_date        = p_edate.
  itab_bapi_eshdr-ref_doc_no       = fs_po-matkl.
  itab_bapi_eshdr-accasscat        = 'K'.
  itab_bapi_eshdr-acceptance       = ' '.
  itab_bapi_eshdr-user_field       = fs_po-konnr.

  append itab_bapi_eshdr.

* Create 1st detail record.

  itab_bapi_essrvcs-pckg_no        = '0000000001'.
  itab_bapi_essrvcs-line_no        = '0000000001'.
  itab_bapi_essrvcs-subpckg_no     = '0000000002'.
  append itab_bapi_essrvcs.
  clear itab_bapi_essrvcs.

* Create additional detail records.

  loop at itab_esll.
    itab_bapi_essrvcs-pckg_no        = '0000000002'.
    itab_bapi_essrvcs-line_no        = itab_esll-introw.
    itab_bapi_essrvcs-ext_line       = itab_esll-extrow.
    itab_bapi_essrvcs-subpckg_no     = '0000000000'.
    itab_bapi_essrvcs-service        = itab_esll-srvpos.
    itab_bapi_essrvcs-quantity       = itab_esll-menge.
    itab_bapi_essrvcs-pln_pckg       = itab_esll-packno.
    itab_bapi_essrvcs-pln_line       = itab_esll-introw.
    itab_bapi_essrvcs-userf2_num     = itab_esll-userf2_num.
    itab_bapi_essrvcs-userf1_txt     = itab_esll-userf1_txt.
    itab_bapi_essrvcs-short_text     = itab_esll-ktext1.
    append itab_bapi_essrvcs.
    clear itab_bapi_essrvcs.
  endloop.

endform.                    "setup_ses_create

*&---------------------------------------------------------------------*
*&      Form  call_fm_ses_create
*&---------------------------------------------------------------------*

form call_fm_ses_create.

* Create the Service Entry Sheet Document

  refresh itab_bapi_esreturn.
  clear itab_bapi_esreturn.

  set update task local.
  call function 'BAPI_ENTRYSHEET_CREATE'
    exporting
      entrysheetheader   = itab_bapi_eshdr
      testrun            = p_testrn
    importing
      entrysheet         = wa_entrysheetno
    tables
      entrysheetservices = itab_bapi_essrvcs
      return             = itab_bapi_esreturn.

*  Commit the work or rollback the BAPI call.

  if wa_entrysheetno <> ' '.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    move 'Y' to w_err_flag.
  endif.


endform.                    "create_entrysheet

*&---------------------------------------------------------------------*
*&      Form  generate_err_report_header
*&---------------------------------------------------------------------*

form generate_err_report_header.
  new-page.
  clear ln_cntr.
  format intensified on.
  write: /1 text-001, 35 text-002.
  write: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  write: /1 sy-sysid, 5 w_mandt, 51 text-009.
  write: 121 text-pge, sy-pagno.
  write: /46 p_sdate, 57 '-', 59 p_edate.
  skip.

  move '4' to ln_cntr.
  format intensified off.

endform.                    "generate_err_report_header

*&---------------------------------------------------------------------*
*&      Form  write_detail_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form write_detail_report.

  ln_cntr = 55.
  sort det_rpttab by po_number.

  loop at det_rpttab.
    perform write_detail_report_lines.
  endloop.

endform.                    "write_detail_report

*&---------------------------------------------------------------------*
*&      Form  generate_detail_rpt_header
*&---------------------------------------------------------------------*

form generate_detail_rpt_header.
  new-page.
  clear ln_cntr.
  format intensified on.
  write: /1 text-001, 35 text-002.
  write: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  write: /1 sy-sysid, 5 w_mandt, 46 p_sdate, 57 '-', 59 p_edate.
  write: 121 text-pge, sy-pagno.
  skip.

  write: /3    text-003,        "Purch Order
          20   text-004,        "Serv. Entry
          37   text-005,        "Creation
          52   text-006,        "Validity
          68   text-007.        "Validity
  write: /3    text-03b,        "Number
          20   text-04b,        "Sheet
          37   text-05b,        "Date
          52   text-06b,        "Start Date
          68   text-07b,        "End Date
          82   text-008.        "Contract No.

  skip 2.
  move '7' to ln_cntr.
  format intensified off.

endform.                    "generate_detail_rpt_header

*&---------------------------------------------------------------------*
*&      Form  write_grcreate_rpt_detail
*&---------------------------------------------------------------------*
form write_detail_report_lines.

  if ln_cntr >= 55.
    perform generate_detail_rpt_header.
  endif.

  write det_rpttab-po_number   under text-03b.
  write det_rpttab-ses_number  under text-04b.
  write det_rpttab-create_date under text-05b.
  write det_rpttab-valid_sdate under text-06b.
  write det_rpttab-valid_edate under text-07b.
  write det_rpttab-contractno  under text-008.
  skip.
  add +2 to ln_cntr.

endform.                    "write_summary_rpt_detail
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ALV     *** NOT IN USE ***
*&---------------------------------------------------------------------*

form output_alv.

  sort det_rpttab by po_number.

  data: fieldcat type slis_t_fieldcat_alv,
        fc_str   type slis_fieldcat_alv,
        layout   type slis_layout_alv,
        title    type lvc_title,
        repid    like sy-repid,
        variant  like disvariant,
        sort     type slis_t_sortinfo_alv,
        sort_str type slis_sortinfo_alv.

  repid = sy-repid.
  layout-colwidth_optimize = 'X'.
  variant-report = repid.

  fc_str-fieldname = 'PO_NUMBER'.
  fc_str-key    = ' '.                " Key columns -not first
  fc_str-seltext_l = text-003.        " Alternative col header
  fc_str-ddictxt = 'L'.
  fc_str-do_sum  = 'X'.
  append fc_str to fieldcat.
  clear  fc_str.

  fc_str-fieldname = 'SES_NUMBER'.
  fc_str-key    = ' '.                " Key columns -notfirst
  fc_str-seltext_l = text-004.        " Alternative col header
  fc_str-ddictxt = 'L'.
  fc_str-do_sum  = 'X'.
  append fc_str to fieldcat.
  clear  fc_str.

  fc_str-fieldname = 'CREATE_DATE'.
  fc_str-key    = ' '.                " Key columns -not first
  fc_str-seltext_l = text-005.        " Alternative col header
  fc_str-ddictxt = 'L'.
  append fc_str to fieldcat.
  clear  fc_str.

  fc_str-fieldname = 'VALID_SDATE'.
  fc_str-seltext_l = text-006.        " Alternative col header
  fc_str-ddictxt = 'L'.
  append fc_str to fieldcat.
  clear  fc_str.

  fc_str-fieldname = 'VALID_EDATE'.
  fc_str-seltext_l = text-007.        " Alternative col header
  fc_str-ddictxt = 'L'.
  append fc_str to fieldcat.
  clear  fc_str.

  fc_str-fieldname = 'CONTRACTNO'.
  fc_str-seltext_l = text-008.        " Alternative col header
  fc_str-ddictxt = 'L'.               " Use Large system text
  append fc_str to fieldcat.
  clear  fc_str.

* Display ALV
  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
             it_fieldcat  = fieldcat
             is_layout    = layout
             i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
            i_callback_program      = repid
*          I_SAVE       = 'A'
*          IS_VARIANT   = variant
*          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
      tables
             t_outtab = det_rpttab
      exceptions
             program_error = 1
      others               = 2.

endform.                    "OUTPUT_ALV
*************************************************************

form alv_top_of_page.
  data: ls_line type slis_listheader.
  data: lt_top_of_page type slis_t_listheader.
  data: t_line like ls_line-info.
  data: i_count  type i.
  data: i_countc(10) type c.
  data: datum1(10).
  data: uzeit1(10).

*1- HEADING LINE: TYPE H
  clear ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = sy-title.             "sy-title.
  append ls_line to lt_top_of_page.

*2- SELECTION LINE: TYPE S
  clear ls_line.
  ls_line-typ   = 'A'.
  ls_line-key   = ''.
  write sy-datum to datum1 dd/mm/yyyy.
  write sy-uzeit to uzeit1 using edit mask '__:__:__'.
  concatenate 'CLIENT:' sy-sysid sy-mandt
              '  DATE:' datum1 '@' uzeit1
              into ls_line-info
              separated by space.
  append ls_line to lt_top_of_page.

  clear ls_line.
  ls_line-typ   = 'A'.
  ls_line-key   = ''.
  move '---' to ls_line-info.
  append ls_line to lt_top_of_page.

* Total No. of Records Selected
  describe table det_rpttab lines i_count.
  i_countc = i_count.
  concatenate 'Total No. of PO''s/SES''s  Created: ' i_countc
                    into t_line separated by space.
  ls_line-typ  = 'A'.
  ls_line-info = t_line.
  append ls_line to lt_top_of_page.
  clear: ls_line, t_line.


  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = lt_top_of_page.

endform.                               " ALV_TOP_OF_PAGE
