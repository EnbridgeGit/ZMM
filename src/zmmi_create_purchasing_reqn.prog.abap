************************************************************************
* Author      : Larry Ritchie                                          *
* Date Created: Nov. 30 2009                                           *
*----------------------------------------------------------------------*
* Description : This program will create purchase requisitions directly*
*               from an EXCEL file.                                    *
*                                                                      *
*               Please keep the East & West copies of this program     *
*               identical.                                             *
************************************************************************
* Date         Developer      Request #       Description              *
************************************************************************
* Nov 30 2009  L Ritchie      TR 658          Initial development      *
************************************************************************

report  zmmi_create_purchasing_reqn line-size 132 line-count 65
                                    no standard page heading.

************************************************************************
*    TABLES                                                            *
************************************************************************
tables: afvc,                     "Operation within an order
        aufk,                     "Order master
        caufv,                    "Order header
        csks,                     "Cost centers
        eban,                     "Purchase requisition
        eina,                     "Info record
        jest,                     "Order object status
        lfa1,                     "Vendors
        lfm1,                     "Vendor/purchasing org
        mara,                     "Material master
        marc,                     "Material/plant
        mard,                     "Material/plant/storage location
        mbew,                     "Material valuation
        prps,                     "WBS element master
        skb1,                     "G/L account master by company code
        t001,                     "Company code
        t001k,                    "Valuation area
        t001l,                    "Storage locations
        t001w,                    "Plants
        t006,                     "Units of measure
        t024e,                    "Purchasing organization
        t024w,                    "Purchasing organization & plant
        t024,                     "Purchasing group
        t100,                     "Error messages
        t141,                     "Material status
        t163k,                    "Account assignment category
        tcurc,                    "Currency
        tka02,                    "Controlling area assignment
        tvarvc                    "Table of variable info
        .

************************************************************************
*    SELECT OPTIONS                                                    *
************************************************************************
selection-screen skip 1.
selection-screen begin of block b1 with frame title text-001.
parameters: p_pcfile like rlgrap-filename obligatory memory id m01,
            begcol type i default 1 no-display,
            begrow type i default 1 no-display,
            endcol type i default 40 no-display,             "max EXCEL columns 40
            endrow type i default 20000 no-display.          "max EXCEL rows 20000
selection-screen comment /5(50) text-015.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-005.
selection-screen:begin of line, comment 1(30) text-006.
parameters s_valid radiobutton group rg01.      "validate only
selection-screen end of line.
selection-screen:begin of line, comment 1(30) text-007.
parameters s_load radiobutton group rg01.     "validate & load
selection-screen end of line.
selection-screen:begin of line, comment 1(30) text-008.
parameters s_errors radiobutton group rg01.       "validate,load,allow errors
selection-screen end of line.
selection-screen end of block b2.

selection-screen begin of block b4 with frame title text-022.
selection-screen:begin of line, comment 1(30) text-023.
parameters p_ymd_sl radiobutton group rg02.      "YYYY/MM/DD
selection-screen end of line.
selection-screen:begin of line, comment 1(30) text-024.
parameters p_ymd radiobutton group rg02.     "YYYYMMDD
selection-screen end of line.
selection-screen:begin of line, comment 1(30) text-025.
parameters p_mdy_sl radiobutton group rg02.       "MM/DD/YYYY
selection-screen end of line.
selection-screen:begin of line, comment 1(30) text-026.
parameters p_dmy_sl radiobutton group rg02.       "DD/MM/YYYY
selection-screen end of line.
selection-screen end of block b4.
selection-screen skip 1.

selection-screen begin of block b3 with frame title text-002.
selection-screen: begin of line, comment 1(79) text-018.
selection-screen end of line.
selection-screen: begin of line, comment 1(79) text-019.
selection-screen end of line.
selection-screen: begin of line, comment 1(79) text-020.
selection-screen end of line.

selection-screen skip 1.
selection-screen: begin of line, comment 1(60) text-009.
selection-screen end of line.
selection-screen: begin of line, comment 1(60) text-010.
selection-screen end of line.
selection-screen: begin of line, comment 1(60) text-016.
selection-screen end of line.

selection-screen skip 1.
selection-screen: begin of line, comment 1(60) text-011.
selection-screen end of line.
selection-screen: begin of line, comment 1(79) text-012.
selection-screen end of line.
selection-screen: begin of line, comment 1(79) text-013.
selection-screen end of line.
selection-screen skip 1.
selection-screen: begin of line, comment 1(79) text-017.
selection-screen end of line.
selection-screen end of block b3.

************************************************************************
*    INTERNAL TABLES                                                   *
************************************************************************

* input file from the PC - these lengths reflect the size of the EXCEL header
types: begin of ty_pc,
        badat(10)   type c,     "requisition request data also called order date
        afnam(13)   type c,     "requistioner
        werks(5)    type c,     "plant
        lgort(16)   type c,     "storage location
        menge(14)   type c,     "quantity
        meins(15)   type c,     "unit of measure
        matnr(18)   type c,     "material number
        txz01(40)   type c,     "material description
        round(14)   type c,     "rounding value, no edits, just push thru
        lead(9)     type c,     "lead time, no edits, just push thru
        lfdat(13)   type c,     "item delivery date
        ekgrp(16)   type c,     "purchasing group
        matkl(14)   type c,     "material group
        bednr(15)   type c,     "requirement tracking number
        preis(12)   type c,     "price
        waers(8)    type c,     "currency
        peinh(10)   type c,     "price units
        knttp(27)   type c,     "account assignment category
        sakto(11)   type c,     "g/l account
        poski(16)   type c,     "WBS element
        kostl(11)   type c,     "cost center
        aufnr(12)   type c,     "order
        nplnr(12)   type c,     "network
        vornr(8)    type c,     "activity
        ablad(25)   type c,     "unloading point
        wempf(15)   type c,     "goods recipient
        lifnr(14)   type c,     "desired vendor
        flief(12)   type c,     "fixed vendor
        ekorg(23)   type c,     "purchasing org
        infnr(10)   type c,     "info record number
        idnlf(35)   type c,     "vendor material number
        text1(200)  type c,
        text2(200)  type c,
        text3(200)  type c,
        text4(200)  type c,
        text5(200)  type c,
      end of ty_pc.

data: tbl_pc_data type ty_pc occurs 0 with header line.

data: tbl_pc_header type ty_pc occurs 1 with header line.

* copy PC data into true fields
data: begin of tbl_input occurs 0,
        badat         like eban-badat,
        afnam         like eban-afnam,
        werks         like eban-werks,
        lgort         like eban-lgort,
        menge         like eban-menge,
        meins         like eban-meins,
        matnr         like eban-matnr,
        txz01         like eban-txz01,
        round(9)      type c,                    "rounding value, no edits, just push thru
        lead(9)       type c,                    "lead time, no edits, just push thru
        lfdat         like eban-lfdat,
        ekgrp         like eban-ekgrp,
        matkl         like eban-matkl,
        bednr         like eban-bednr,
        preis         like eban-preis,
        waers         like eban-waers,
        peinh         like eban-peinh,
        knttp         like eban-knttp,
        sakto         like ebkn-sakto,
        poski         like prps-poski,               "WBS element
        kostl         like ebkn-kostl,
        aufnr         like ebkn-aufnr,               "order number
        nplnr         like ebkn-nplnr,               "network
        vornr         like afvc-vornr,               "operation or activity
        ablad         like ebkn-ablad,               "unloading point
        wempf         like ebkn-wempf,               "goods recipient
        lifnr         like eban-lifnr,               "desired vendor
        flief         like eban-flief,               "fixed vendor
        ekorg         like eban-ekorg,               "purchasing org
        infnr         like eina-infnr,               "info record number
        idnlf         like eban-idnlf,               "vendor material number
        text1(200)    type c,
        text2(200)    type c,
        text3(200)    type c,
        text4(200)    type c,
        text5(200)    type c,
      end of tbl_input.

* output EXCEL file
data: begin of tbl_excel_output occurs 0.
        include structure tbl_pc_data.
data:           banfn            like eban-banfn,         "purchase requisition number
                calc_lfdat       like eban-lfdat,         "delivery date using MRP lead time
                days_diff(6)     type n,                  "delivery days short
                bapi_message(150) type c,                  "BAPI error message
      end of tbl_excel_output.

* column titles on the EXCEL file
data: begin of tbl_column_names occurs 0,
          name(60) type c,
      end of tbl_column_names.

* use internal version of material
data: tbl_converted like tbl_input occurs 0 with header line.

* input data ready to be put in the input BAPI structures
data: tbl_load like tbl_input occurs 0 with header line.

data: tbl_filetable type filetable.

* material
data: begin of tbl_mara occurs 0,
        matnr          like mara-matnr,
        lvorm          like mara-lvorm,
        matkl          like mara-matkl,
        mstae          like mara-mstae,
      end of tbl_mara.

data: tbl_matnr like tbl_mara occurs 0 with header line.

* plant
data: begin of tbl_t001w occurs 200,
        werks          like t001w-werks,
      end of tbl_t001w.

data: tbl_werks like tbl_t001w occurs 0 with header line.

* material/plant
data: begin of tbl_marc occurs 0,
        matnr          like marc-matnr,
        werks          like marc-werks,
        lvorm          like marc-lvorm,
        mmsta          like marc-mmsta,
        plifz          like marc-plifz,
      end of tbl_marc.

data: begin of tbl_matnr_werks occurs 0,
        matnr          like marc-matnr,
        werks          like marc-werks,
      end of tbl_matnr_werks.

* storage location
data: begin of tbl_t001l occurs 0,
        werks          like t001l-werks,
        lgort          like t001l-lgort,
      end of tbl_t001l.

data: tbl_werks_lgort like tbl_t001l occurs 0 with header line.

* material/plant/storage location
data: begin of tbl_mard occurs 0,
        matnr          like mard-matnr,
        werks          like mard-werks,
        lgort          like mard-lgort,
        lvorm          like mard-lvorm,
      end of tbl_mard.

data: begin of tbl_matnr_werks_lgort occurs 0,
        matnr          like mard-matnr,
        werks          like mard-werks,
        lgort          like mard-lgort,
      end of tbl_matnr_werks_lgort.

* material valuation
data: begin of tbl_mbew occurs 0,
        matnr          like mbew-matnr,
        bwkey          like mbew-bwkey,
      end of tbl_mbew.

* purchasing group
data: begin of tbl_t024 occurs 25,
        ekgrp          like t024-ekgrp,
      end of tbl_t024.

* unit of measure
data: begin of tbl_t006 occurs 200,
        msehi          like t006-msehi,
        isocode        like t006-isocode,
      end of tbl_t006.

data: begin of tbl_t006_iso occurs 100,
        msehi          like t006-msehi,
        isocode        like t006-isocode,
      end of tbl_t006_iso.

* material group
data: begin of tbl_t023 occurs 100,
        matkl          like t023-matkl,
      end of tbl_t023.

* currency
data: begin of tbl_tcurc occurs 175,
        waers          like tcurc-waers,
      end of tbl_tcurc.

* account assignment category
data: begin of tbl_t163k occurs 15,
        knttp          like t163k-knttp,
      end of tbl_t163k.

* purchasing organization
data: begin of tbl_t024e occurs 2,
        ekorg          like t024e-ekorg,
      end of tbl_t024e.

* purchasing organization & plant
data: begin of tbl_t024w occurs 150,
        werks          like t024w-werks,
        ekorg          like t024w-ekorg,
      end of tbl_t024w.

* valuation area
data: begin of tbl_t001k occurs 200,
        bwkey          like t001k-bwkey,
        bukrs          like t001k-bukrs,
      end of tbl_t001k.

* company code
data: begin of tbl_t001 occurs 50,
        bukrs          like t001-bukrs,
        waers          like t001-waers,
        ktopl          like t001-ktopl,
      end of tbl_t001.

data: begin of tbl_bukrs occurs 0,
        bukrs          like t001-bukrs,
      end of tbl_bukrs.

* vendor
data: begin of tbl_lfa1 occurs 0,
        lifnr          like lfa1-lifnr,
        loevm          like lfa1-loevm,
        sperr          like lfa1-sperr,
        sperm          like lfa1-sperm,
      end of tbl_lfa1.

data: begin of tbl_lifnr occurs 0,
        lifnr          like lfa1-lifnr,
      end of tbl_lifnr.

* vendor/purchasing org
data: begin of tbl_lfm1 occurs 0,
        lifnr          like lfm1-lifnr,
        ekorg          like lfm1-ekorg,
        sperm          like lfm1-sperm,
        loevm          like lfm1-loevm,
      end of tbl_lfm1.

data: begin of tbl_lifnr_ekorg occurs 0,
        lifnr          like lfm1-lifnr,
        ekorg          like lfm1-ekorg,
      end of tbl_lifnr_ekorg.

* G/L account
data: begin of tbl_skb1 occurs 0,
        bukrs          like skb1-bukrs,
        saknr          like skb1-saknr,
      end of tbl_skb1.

data: begin of tbl_saknr occurs 0,
        saknr          like skb1-saknr,
      end of tbl_saknr.

* WBS element
data: begin of tbl_prps occurs 0,
        pspnr         like prps-pspnr,
        posid         like prps-posid,
        objnr         like prps-objnr,
        poski         like prps-poski,
        pbukr         like prps-pbukr,
end of tbl_prps.

data: begin of tbl_poski occurs 0,
        poski         like prps-poski,
      end of tbl_poski.

data: begin of tbl_posid occurs 0,
        posid         like prps-posid,
      end of tbl_posid.

* Controlling area assignment
data: begin of tbl_tka02 occurs 25,
         bukrs        like tka02-bukrs,
         kokrs        like tka02-kokrs,
      end of tbl_tka02.

* cost centers
data: begin of tbl_csks occurs 0,
         kokrs        like csks-kokrs,
         kostl        like csks-kostl,
      end of tbl_csks.

data: begin of tbl_kostl occurs 0,
        kostl         like csks-kostl,
      end of tbl_kostl.

* orders
data: begin of tbl_aufk occurs 0,
         aufnr        like aufk-aufnr,
         bukrs        like aufk-bukrs,
      end of tbl_aufk.

data: begin of tbl_aufnr occurs 0,
         aufnr        like aufk-aufnr,
      end of tbl_aufnr.

* info records - by material & vendor
data: begin of tbl_eina_matnr occurs 0,
        infnr         like eina-infnr,
        matnr         like eina-matnr,
        lifnr         like eina-lifnr,
      end of tbl_eina_matnr.

data: begin of tbl_matnr_flief occurs 0,
        matnr         like eina-matnr,
        flief         like eina-lifnr,
      end of tbl_matnr_flief.

* info records - by material group & vendor
data: begin of tbl_eina_matkl occurs 0,
        infnr         like eina-infnr,
        matkl         like eina-matkl,
        lifnr         like eina-lifnr,
      end of tbl_eina_matkl.

data: begin of tbl_matkl_flief occurs 0,
        matkl         like eina-matkl,
        flief         like eina-lifnr,
      end of tbl_matkl_flief.

* info records - by info record number
data: begin of tbl_eina_infnr occurs 0,
        infnr         like eina-infnr,
        matnr         like eina-matnr,
        matkl         like eina-matkl,
        lifnr         like eina-lifnr,
      end of tbl_eina_infnr.

data: begin of tbl_infnr occurs 0,
        infnr         like eina-lifnr,
      end of tbl_infnr.

* network
data: begin of tbl_caufv occurs 0,
        aufnr         like aufk-aufnr,
        bukrs         like aufk-bukrs,
        objnr         like aufk-objnr,
        aufpl         like afko-aufpl,
      end of tbl_caufv.

data: begin of tbl_aufnr_netw occurs 0,
        aufnr          like aufk-aufnr,
      end of tbl_aufnr_netw.

* operation or activity
data: begin of tbl_afvc occurs 0,
        aufpl          like afvc-aufpl,
        vornr          like afvc-vornr,
      end of tbl_afvc.

* order status
data: begin of tbl_jest occurs 0,
         objnr         like jest-objnr,
         stat          like jest-stat,
      end of tbl_jest.

data: begin of tbl_objnr occurs 0,
         objnr         like jest-objnr,
      end of tbl_objnr.

* material status
data: begin of tbl_t141 occurs 0,
         mmsta         like t141-mmsta,
      end of tbl_t141.

* delivery data to be added to the EXCEL output file
data: begin of tbl_delivery occurs 0,
          material(1)  type c,
          calc_lfdat   like eban-lfdat,
          days_diff    like sy-tabix,
      end of tbl_delivery.

* data from EXCEL in row, column & value format
data: begin of tbl_intern occurs 0.
        include structure  alsmex_tabline.
data: end of tbl_intern.

* BAPI tables
data: begin of tbl_bapi_items occurs 0.
        include structure bapiebanc.
data: end of tbl_bapi_items.

data: begin of tbl_bapi_text occurs 0.
        include structure bapiebantx.
data: end of tbl_bapi_text.

data: begin of tbl_bapi_return occurs 0.
        include structure bapireturn.
data: end of tbl_bapi_return.

data: begin of tbl_bapi_acct_assign occurs 0.
        include structure bapiebkn.
data: end of tbl_bapi_acct_assign.

************************************************************************
*    VARAIBLES                                                         *
************************************************************************

data:
        v_window_title   type string,
        v_user_action    type i,
        v_subrc          like sy-subrc,
        v_input_count(6) type p,
        v_bapi_err_count(6) type p,
        v_row_start_cnt  like sy-tabix,
        v_row_end_cnt    like sy-tabix,
        v_row_current_cnt like sy-tabix,
        v_row_next_cnt   like sy-tabix,
        v_current_load_cnt like sy-tabix,
        v_tabix          like sy-tabix,
        v_update_count(6) type p,
        v_date8(8)       type c,
        v_date           like sy-datum,
        v_numeric4(4)    type n,
        v_numeric10(10)  type n,
        v_numeric12(12)  type n,
        v_char3(3)       type c,
        v_char4(4)       type c,
        v_commit_count   type i,
        v_found_info(1)  type c,
        v_objnr          like jest-objnr,
        v_record_number  like sy-tabix,
        v_next_rec_num   like sy-tabix,
        v_prev_werks     like marc-werks,
        v_prev_afnam     like eban-afnam,
        v_prev_objnr     like jest-objnr,
        v_purch_req_item_cnt(5) type n,
        v_itemx10(5)     type n,
        v_part1          like rlgrap-filename,
        v_part2(8)       type c,
        v_error(1)       type c,
        v_error_count(6) type p,
        v_valid_mara(1)  type c,
        v_valid_marc(1)  type c,
        v_valid_mard(1)  type c,
        v_valid_t001w(1) type c,
        v_valid_t001l(1) type c,
        v_kokrs          like ebkn-kokrs,
        v_bnfpo          like eban-bnfpo,
        v_werks          like eban-werks,
        v_lgort          like eban-lgort,
        v_matnr          like eban-matnr,
        v_ekgrp          like eban-ekgrp,
        v_matkl          like eban-matkl,
        v_waers          like eban-waers,
        v_knttp          like eban-knttp,
        v_sakto          like ebkn-sakto,
        v_kostl          like ebkn-kostl,
        v_aufnr          like ebkn-aufnr,
        v_nplnr          like ebkn-nplnr,
        v_ekorg          like eban-ekorg,
        v_infnr          like eina-infnr,
        v_vornr          like afvc-vornr,
        v_bapi_reqn_number like bapiebanc-preq_no,
        v_output_filename200(200) type c,
        v_output_filename128  like rlgrap-filename,
        v_screen_error(1) type c.

data:
        v_filetable type file_table,
        v_pc_line type ty_pc.

************************************************************************
*    EVENTS                                                            *
************************************************************************

at selection-screen on value-request for p_pcfile.
  perform get_filename using p_pcfile.

************************************************************************
*    START OF SELECTION                                                *
************************************************************************

start-of-selection.

  perform upload_excel_file.

  perform load_tvarvc.

  perform validate_excel_numerics.

  perform convert_data_to_internal_form.

  perform load_validation_files.

  perform validate_individual_fields.

  perform create_purchasing_requisition.

  perform create_excel_file.

end-of-selection.

  check v_screen_error = ' '.

  skip 1.
  write:/ 'EXCEL rows excluding header: ', v_input_count.
  skip 1.
  write:/ 'EXCEL rows with errors:      ', v_error_count.
  skip 1.

  if v_bapi_err_count > 0.
    write:/ 'BAPI errors:                 ', v_bapi_err_count.
    skip 1.
  endif.

  write:/ 'New purchasing requisitions: ', v_update_count.

************************************************************************
*    FORMS                                                             *
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  get_filename
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_filename using p_pcfile.

  v_window_title = 'Select EXCEL file to be Uploaded'.

  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title            = v_window_title
    changing
      file_table              = tbl_filetable
      rc                      = v_subrc
      user_action             = v_user_action
    exceptions
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      others                  = 5.

  if v_user_action = cl_gui_frontend_services=>action_cancel or
     tbl_filetable[] is initial.
    write: / 'Action Canceled'.
    exit.
  endif.

  read table tbl_filetable into v_filetable index 1.
  p_pcfile = v_filetable.

endform.                    " get_filename

*&---------------------------------------------------------------------*
*&      Form  convert_data_to_internal_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form convert_data_to_internal_form .

  refresh tbl_converted.

  loop at tbl_input.
    tbl_converted = tbl_input.

* plant
    translate tbl_converted-werks to upper case.

* storage location
    translate tbl_converted-lgort to upper case.

* unit of measure
    translate tbl_converted-meins to upper case.

* material
    if tbl_input-matnr <> ' '.
      call function 'CONVERSION_EXIT_MATN1_INPUT'         "insert leading zero
        exporting
          input        = tbl_input-matnr
        importing
          output       = tbl_converted-matnr
        exceptions
          length_error = 1
          others       = 2.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.

* material group
    translate tbl_converted-matkl to upper case.

* currency
    translate tbl_converted-waers to upper case.

* account assignment
    translate tbl_converted-knttp to upper case.

* G/L account - it must have leading zeros
    if tbl_input-sakto <> ' '.
      if tbl_input-sakto co ' 0123456789'.
        v_numeric10 = tbl_input-sakto.
        tbl_converted-sakto = v_numeric10.
      endif.
    endif.

* cost center - it must have leading zeros
    if tbl_input-kostl <> ' '.
      if tbl_input-kostl co ' 0123456789'.
        v_numeric10 = tbl_input-kostl.
        tbl_converted-kostl = v_numeric10.
      endif.
    endif.

* order - it must have leading zeros
    if tbl_input-aufnr <> ' '.
      if tbl_input-aufnr co ' 0123456789'.
        v_numeric12 = tbl_input-aufnr.
        tbl_converted-aufnr = v_numeric12.
      endif.
    endif.

* desired vendor
    if tbl_input-lifnr <> ' '.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = tbl_input-lifnr
        importing
          output = tbl_converted-lifnr.
    endif.

* fixed vendor
    if tbl_input-flief <> ' '.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = tbl_input-flief
        importing
          output = tbl_converted-flief.
    endif.

* purchasing org
    translate tbl_converted-ekorg to upper case.

    append tbl_converted.

  endloop.

  free tbl_input.

endform.                    " convert_data_to_internal_form
*&---------------------------------------------------------------------*
*&      Form  validate_individual_fields
*&---------------------------------------------------------------------*
*       Make sure all the EXCEL input data is valid
*----------------------------------------------------------------------*
form validate_individual_fields.

  loop at tbl_converted.
    clear: v_valid_mara, v_valid_marc, v_valid_mard, v_valid_t001w,
           v_valid_t001l, v_error.
    v_record_number = sy-tabix.

* 1 - order date - already checked in the numeric section

* 2 - requistioner - required field for west

    if tbl_converted-afnam = ' ' and tvarvc-low = 'WEST'.
      perform input_error.
      write:/28 '**ERROR** Requisitioner is required'.
    endif.

* 3 - plant - if there is a material, the plant may be on MARC

    if tbl_converted-werks <> ' '.
      clear tbl_t001w.
      read table tbl_t001w with key werks = tbl_converted-werks
                                    binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Plant not valid:', tbl_converted-werks.
      else.
        v_valid_t001w = 'X'.
      endif.
    endif.

* 4 - storage location - If it is not entered, look on MARD for a single storage location.
*                        Otherwise default to A001 (east) or 0001 (west)

    if tbl_converted-lgort <> ' ' and v_valid_t001w = 'X'.
      clear tbl_t001l.
      read table tbl_t001l with key werks = tbl_converted-werks
                                    lgort = tbl_converted-lgort
                                    binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Storage location not valid:', tbl_converted-lgort,
                  ' for plant ', tbl_converted-werks.
      else.
        v_valid_t001l = 'X'.
      endif.
    else.
      if tbl_converted-matnr <> ' ' and tbl_converted-werks <> ' '.
        clear tbl_mard.
        read table tbl_mard with key matnr = tbl_converted-matnr       "is there 1 MARD entry
                                     werks = tbl_converted-werks
                                     binary search.
        if sy-subrc = 0.
          v_tabix = sy-tabix + 1.
          tbl_converted-lgort = tbl_mard-lgort.
          v_valid_t001l = 'X'.
          v_valid_mard = 'X'.
          clear tbl_mard.
          read table tbl_mard index v_tabix.
          if sy-subrc = 0.
            if tbl_marc-matnr = tbl_converted-matnr and  "2+ storage locations, cannot default using MARD
               tbl_marc-werks = tbl_converted-werks.
              clear: tbl_converted-lgort, v_valid_t001l, v_valid_mard.
            endif.
          endif.
        endif.
      endif.
    endif.

* since we could not find a unique storage location on MARD, use the east/west defaults
    if tbl_converted-lgort = ' ' and v_valid_t001w = 'X'.
      if tvarvc-low = 'EAST'.
        tbl_converted-lgort = 'A001'.
      else.
        tbl_converted-lgort = '0001'.
      endif.
      clear tbl_t001l.
      read table tbl_t001l with key werks = tbl_converted-werks
                                   lgort = tbl_converted-lgort
                                   binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Storage location not valid:', tbl_converted-lgort,
                  ' for plant ', tbl_converted-werks.
      else.
        v_valid_t001l = 'X'.
      endif.
    endif.

* 5 - quantity - This is a mandatory field

    if tbl_converted-menge = 0.
      perform input_error.
      write:/28 '**ERROR** Quantity is required'.
    endif.

* 6 - unit of measure

    if tbl_converted-meins <> ' '.
      clear tbl_t006.
      read table tbl_t006 with key msehi = tbl_converted-meins
                                   binary search.
      if sy-subrc <> 0.
        clear tbl_t006_iso.
        read table tbl_t006_iso with key isocode = tbl_converted-meins
                                         binary search.
        if sy-subrc <> 0.
          perform input_error.
          v_char3 = tbl_converted-meins.
          write:/28 '**ERROR** Unit of measure not valid: ', v_char3.
        else.
          tbl_converted-meins = tbl_t006_iso-msehi.      "send the internal version to the BAPI
        endif.
      endif.
    endif.

    if tbl_converted-meins = ' ' and tbl_converted-matnr = ' '.
      perform input_error.
      write:/28 '**ERROR** Unit of measure is required when there is no material'.
    endif.

* 7 - material - If the material has been entered check MARA, MARC, MARD & MBEW.

    if tbl_converted-matnr <> ' '.
      clear tbl_mara.
      read table tbl_mara with key matnr = tbl_converted-matnr
                                   binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Material not valid:', tbl_converted-matnr.
      else.
        if tbl_mara-lvorm = 'X'.
          perform input_error.
          write:/28 '**ERROR** Material is flagged for deletion:', tbl_converted-matnr.
        else.
          if tbl_mara-mstae <> ' '.
            read table tbl_t141 with key mmsta = tbl_mara-mstae    "holds only blocked status
                                     binary search.
            if sy-subrc <> 0.
              v_valid_mara = 'X'.
            else.
              perform input_error.
              write:/28 '**ERROR** Material status invalid:', tbl_mara-mstae,
                        tbl_converted-matnr.
            endif.
          else.
            v_valid_mara = 'X'.
          endif.
        endif.
      endif.
    endif.

* material & plant
    if v_valid_mara = 'X' and v_valid_t001w = 'X'.
      if v_valid_marc = ' '.
        clear tbl_marc.
        read table tbl_marc with key matnr = tbl_converted-matnr
                                     werks = tbl_converted-werks
                                     binary search.
        if sy-subrc <> 0.
          perform input_error.
          write:/28 '**ERROR** Material/plant not valid:', tbl_converted-matnr,
                  tbl_converted-werks.
        else.
          if tbl_marc-lvorm = 'X'.
            perform input_error.
            write:/28 '**ERROR** Material/plant flagged for deletion:',
            tbl_converted-matnr, tbl_converted-werks.
          else.
            if tbl_marc-mmsta <> ' '.
              read table tbl_t141 with key mmsta = tbl_marc-mmsta
                                       binary search.
              if sy-subrc <> 0.
                v_valid_marc = 'X'.
              else.
                perform input_error.
                write:/28 '**ERROR** Material/plant status invalid:', tbl_marc-mmsta,
                          tbl_converted-matnr, tbl_converted-werks.
              endif.
            else.
              v_valid_marc = 'X'.
            endif.
          endif.
        endif.
      endif.
    endif.

* material valuation
    if v_valid_marc = 'X'.
      clear tbl_mbew.
      read table tbl_mbew with key matnr = tbl_converted-matnr
                                   bwkey = tbl_converted-werks
                                   binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Missing material valuation for material/plant:',
                  tbl_converted-matnr, tbl_converted-werks.
      endif.
    endif.

* material & plant & storage location
    if v_valid_marc = 'X' and v_valid_t001l = 'X'.
      if v_valid_mard = ' '.
        clear tbl_mard.
        read table tbl_mard with key matnr = tbl_converted-matnr
                                     werks = tbl_converted-werks
                                     lgort = tbl_converted-lgort
                                     binary search.
        if sy-subrc <> 0.
          perform input_error.
          write:/28 '**ERROR** Material/plant/storage location not valid:', tbl_converted-matnr,
                  tbl_converted-werks, tbl_converted-lgort.
        else.
          if tbl_mard-lvorm = 'X'.
            perform input_error.
            write:/28 '**ERROR** Material/plant/SLoc flagged for deletion:',
            tbl_converted-matnr, tbl_converted-werks, tbl_converted-lgort.
          endif.
        endif.
      endif.
    endif.

* material is mandatory when account assignment category is blank
    if tbl_converted-matnr = ' ' and tbl_converted-knttp = ' '.
      perform input_error.
      write:/28 '**ERROR** Material is required when the account assignment category is blank'.
    endif.

* 8 - material description

    if tbl_converted-matnr = ' ' and tbl_converted-txz01 = ' '.
      perform input_error.
      write:/28 '**ERROR** Material description is required when there is no material number'.
    endif.

* 9  - rounding value - pass thru field

* 10 - lead time - pass thru field

* 11 - delivery date - mandatory if no material number

    if tbl_converted-lfdat = ' '.
      if tbl_converted-matnr = ' '.
        perform input_error.
        write:/28 '**ERROR** Delivery date is required when there is no material number'.
      else.
        if v_valid_t001w = 'X'.
          clear tbl_marc.
          read table tbl_marc with key matnr = tbl_converted-matnr
                                       werks = tbl_converted-werks
                                       binary search.
          if sy-subrc = 0.
            tbl_converted-lfdat = sy-datum + tbl_marc-plifz.
          endif.
        else.
          perform input_error.
          write:/28 '**ERROR** Delivery date is required'.
        endif.
      endif.
    endif.

* 12 - purchasing group

    if tbl_converted-ekgrp <> ' '.
      clear tbl_t024.
      read table tbl_t024 with key ekgrp = tbl_converted-ekgrp
                               binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid purchasing group: ', tbl_converted-ekgrp.
      endif.
    endif.

* 13 - material group - use the MARA material group if material number exists.  Otherwise the
*                       material group is required.  Note, the material group is the first 4
*                       characters with leading zeros of a 9 character field

    if tbl_converted-matnr <> ' '.
      clear tbl_mara.
      read table tbl_mara with key matnr = tbl_converted-matnr
                                   binary search.
      if sy-subrc = 0.
        tbl_converted-matkl = tbl_mara-matkl.
      endif.
    else.
      if tbl_converted-matkl <> ' '.
        if tvarvc-low = 'EAST'.
          v_numeric4 = tbl_converted-matkl.
          v_char4 = v_numeric4.
          tbl_converted-matkl = v_char4.
        endif.
        clear tbl_t023.
        read table tbl_t023 with key matkl = tbl_converted-matkl
                                     binary search.
        if sy-subrc <> 0.
          perform input_error.
          write:/28 '**ERROR** Invalid material group: ', tbl_converted-matkl.
        endif.
      endif.
    endif.

* 14 - tracking number - no validation

* 15 - price - already checked in the numeric section

    if tbl_converted-matnr = ' ' and tbl_converted-preis = 0.
      perform input_error.
      write:/28 '**ERROR** Price is required when there is no material number'.
    endif.

* 16 - currency

    if tbl_converted-waers <> ' '.
      clear tbl_tcurc.
      read table tbl_tcurc with key waers = tbl_converted-waers
                                binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid currency: ', tbl_converted-waers.
      endif.
    else.
* if there is no currency, get it based on the plant
      clear tbl_t001k.
      read table tbl_t001k with key bwkey = tbl_converted-werks
                                    binary search.
      if sy-subrc = 0.
        clear tbl_t001.
        read table tbl_t001 with key bukrs = tbl_t001k-bukrs
                                 binary search.
        if sy-subrc = 0.
          tbl_converted-waers = tbl_t001-waers.
        endif.
      endif.
    endif.

* 17 - price unit - already checked in the numeric section

* 18 - account assignment category

    if tbl_converted-knttp <> ' '.
      clear tbl_t163k.
      read table tbl_t163k with key knttp = tbl_converted-knttp
                                binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid account assignment category: ', tbl_converted-knttp.
      else.
        if tbl_converted-matnr = ' '.
          if tbl_converted-sakto = ' '.
            perform input_error.
            write:/28 '**ERROR** G/L account required'.
          endif.
        endif.
        if tbl_converted-poski = ' ' and tbl_converted-kostl = ' ' and
           tbl_converted-aufnr = ' ' and tbl_converted-nplnr = ' '.
          perform input_error.
          write:/28 '**ERROR** One of WBS element or cost center or order or network is required'.
        endif.
      endif.
    endif.

* 19 - g/l account

    if tbl_converted-sakto <> ' '.
      clear tbl_t001k.
      read table tbl_t001k with key bwkey = tbl_converted-werks
                                  binary search.
      if sy-subrc = 0.
        clear tbl_skb1.
        read table tbl_skb1 with key bukrs = tbl_t001k-bukrs
                                     saknr = tbl_converted-sakto
                                     binary search.
        if sy-subrc <> 0.
          perform input_error.
          write:/28 '**ERROR** Invalid g/l account: ', tbl_converted-sakto.
        endif.
      endif.
    endif.

* 20 - WBS element

    if tbl_converted-poski <> ' '.
      clear tbl_prps.
      if tbl_converted-poski cs '-'.
        read table tbl_prps with key poski = tbl_converted-poski
                                     binary search.
      else.
        read table tbl_prps with key posid = tbl_converted-poski
                                     binary search.
      endif.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid WBS element: ', tbl_converted-poski.
      else.
        perform check_status.
        clear tbl_t001k.
        read table tbl_t001k with key bwkey = tbl_converted-werks
                                      binary search.
        if sy-subrc = 0 and tbl_prps-pbukr <> tbl_t001k-bukrs.
          perform input_error.
          write:/28 '**ERROR** Invalid WBS element company: ', tbl_converted-poski,
                tbl_prps-pbukr.
        endif.
      endif.
    endif.

* 21 - cost center

    if tbl_converted-kostl <> ' '.
      clear tbl_t001k.
      read table tbl_t001k with key bwkey = tbl_converted-werks
                                    binary search.
      if sy-subrc = 0.
        clear tbl_tka02.
        read table tbl_tka02 with key bukrs = tbl_t001k-bukrs
                                      binary search.
        if sy-subrc = 0.
          clear tbl_csks.
          read table tbl_csks with key kokrs = tbl_tka02-kokrs
                                       kostl = tbl_converted-kostl
                                       binary search.
          if sy-subrc <> 0.
            perform input_error.
            write:/28 '**ERROR** Invalid cost center: ', tbl_converted-kostl.
          endif.
        endif.
      endif.
    endif.

* 22 - order

    if tbl_converted-aufnr <> ' '.
      clear tbl_aufk.
      read table tbl_aufk with key aufnr = tbl_converted-aufnr
                                   binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid order: ', tbl_converted-aufnr.
      else.
        concatenate 'OR' tbl_converted-aufnr into v_objnr.
        clear tbl_jest.
        read table tbl_jest with key objnr = v_objnr
                                     binary search.
        if sy-subrc = 0.
          case tbl_jest-stat.
            when 'I0001'.
              perform input_error.
              write:/28 '**ERROR** Internal order not yet released: ', tbl_converted-aufnr.
            when 'I0002'.
            when 'I0013'.
              perform input_error.
              write:/28 '**ERROR** Internal order is deleted: ', tbl_converted-aufnr.
            when 'I0045'.
              perform input_error.
              write:/28 '**ERROR** Internal order technically complete: ', tbl_converted-aufnr.
            when 'I0046'.
              perform input_error.
              write:/28 '**ERROR** Internal order closed: ', tbl_converted-aufnr.
            when 'I0076'.
              perform input_error.
              write:/28 '**ERROR** Internal order is deleted: ', tbl_converted-aufnr.
          endcase.
        endif.
        clear tbl_t001k.
        read table tbl_t001k with key bwkey = tbl_converted-werks
                                      binary search.
        if sy-subrc = 0 and tbl_t001k-bukrs <> tbl_aufk-bukrs.
          perform input_error.
          write:/28 '**ERROR** Internal order company invalid: ', tbl_converted-aufnr, tbl_aufk-bukrs.
        endif.
      endif.
    endif.

* 23 & 24 - network & activity

    if tbl_converted-nplnr <> ' '.
      clear tbl_aufnr_netw.
      read table tbl_caufv with key aufnr = tbl_converted-nplnr
                                    binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid network: ', tbl_converted-nplnr.
      else.
        clear tbl_t001k.
        read table tbl_t001k with key bwkey = tbl_converted-werks
                                      binary search.
        if sy-subrc = 0 and tbl_t001k-bukrs <> tbl_caufv-bukrs.
          perform input_error.
          write:/28 '**ERROR** Network company invalid: ', tbl_converted-nplnr, tbl_caufv-bukrs.
        endif.
        if tbl_converted-vornr = ' '.
          perform input_error.
          write:/28 '**ERROR** Activity number required'.
        else.
          clear tbl_afvc.
          read table tbl_afvc with key aufpl = tbl_caufv-aufpl
                                       vornr = tbl_converted-vornr
                                       binary search.
          if sy-subrc <> 0.
            perform input_error.
            write:/28 '**ERROR** Invalid activity: ', tbl_converted-vornr.
          endif.
        endif.

        concatenate 'NP' tbl_converted-nplnr into v_objnr.
        clear tbl_jest.
        read table tbl_jest with key objnr = v_objnr
                                     binary search.
        if sy-subrc = 0.                                    "check the order status
          case tbl_jest-stat.
            when 'I0001'.
              perform input_error.
              write:/28 '**ERROR** Network order not yet released: ', tbl_converted-nplnr.
            when 'I0002'.
            when 'I0013'.
              perform input_error.
              write:/28 '**ERROR** Network order is deleted: ', tbl_converted-nplnr.
            when 'I0045'.
              perform input_error.
              write:/28 '**ERROR** Network order technically complete: ', tbl_converted-nplnr.
            when 'I0046'.
              perform input_error.
              write:/28 '**ERROR** Network order closed: ', tbl_converted-nplnr.
            when 'I0076'.
              perform input_error.
              write:/28 '**ERROR** Network order is deleted: ', tbl_converted-nplnr.
          endcase.
        endif.

      endif.
    endif.

* 25 - unloading point - no validation required

* 26 - goods recipient - no validation required

* 27 - desired vendor

    if tbl_converted-lifnr <>  ' '.
      clear tbl_lfa1.
      read table tbl_lfa1 with key lifnr = tbl_converted-lifnr
                                   binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid desired vendor: ', tbl_converted-lifnr.
      else.
        if tbl_lfa1-loevm = 'X'.
          perform input_error.
          write:/28 '**ERROR** Vendor is marked for deletion: ', tbl_converted-lifnr.
        else.
          if tbl_lfa1-sperm = 'X' or tbl_lfa1-sperr = 'X'.
            perform input_error.
            write:/28 '**ERROR** Vendor is blocked: ', tbl_converted-lifnr.
          endif.
        endif.
        if tbl_converted-ekorg <> ' '.
          clear tbl_lfm1.
          read table tbl_lfm1 with key lifnr = tbl_converted-lifnr
                                       ekorg = tbl_converted-ekorg
                                       binary search.
          if sy-subrc <> 0.
            perform input_error.
            write:/28 '**ERROR** Invalid desired vendor/purchasing org: ', tbl_converted-lifnr,
                      tbl_converted-ekorg.
          else.
            if tbl_lfm1-loevm = 'X'.
              perform input_error.
              write:/28 '**ERROR** Vendor is marked for deletion: ', tbl_converted-lifnr, tbl_converted-ekorg.
            else.
              if tbl_lfm1-sperm = 'X'.
                perform input_error.
                write:/28 '**ERROR** Vendor is blocked: ', tbl_converted-lifnr, tbl_converted-ekorg.
              endif.
            endif.

          endif.
        endif.
      endif.
    endif.

* 28 - fixed vendor - requires info record (it may default in)

    if tbl_converted-flief <>  ' '.
      clear v_found_info.
      clear tbl_lfa1.
      read table tbl_lfa1 with key lifnr = tbl_converted-flief
                                   binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid fixed vendor: ', tbl_converted-flief.
      else.
        if tbl_lfa1-loevm = 'X'.
          perform input_error.
          write:/28 '**ERROR** Vendor is marked for deletion: ', tbl_converted-flief.
        else.
          if tbl_lfa1-sperm = 'X' or tbl_lfa1-sperr = 'X'.
            perform input_error.
            write:/28 '**ERROR** Vendor is blocked: ', tbl_converted-flief.
          endif.
        endif.
        if tbl_converted-ekorg <> ' '.
          clear tbl_lfm1.
          read table tbl_lfm1 with key lifnr = tbl_converted-flief
                                       ekorg = tbl_converted-ekorg
                                       binary search.
          if sy-subrc <> 0.
            perform input_error.
            write:/28 '**ERROR** Invalid fixed vendor/purchasing org: ', tbl_converted-flief,
                      tbl_converted-ekorg.
            if tbl_lfm1-loevm = 'X'.
              perform input_error.
              write:/28 '**ERROR** Vendor is marked for deletion: ', tbl_converted-flief, tbl_converted-ekorg.
            else.
              if tbl_lfm1-sperm = 'X'.
                perform input_error.
                write:/28 '**ERROR** Vendor is blocked: ', tbl_converted-flief, tbl_converted-ekorg.
              endif.
            endif.
          endif.
        endif.

        " If there is no info record number entered, it could default in. So check the fixed vendor
        " by both material (if there is one) and/or material group

        if tbl_converted-infnr = ' '.
          if tbl_converted-matnr <> ' '.
            clear tbl_eina_matnr.
            read table tbl_eina_matnr with key matnr = tbl_converted-matnr
                                               lifnr = tbl_converted-flief
                                               binary search.
            if sy-subrc = 0.
              v_found_info = 'X'.
            endif.
          endif.
          if tbl_converted-matkl <> ' '.
            clear tbl_eina_matkl.
            read table tbl_eina_matkl with key matkl = tbl_converted-matkl
                                               lifnr = tbl_converted-flief
                                               binary search.
            if sy-subrc = 0.
              v_found_info = 'X'.
            endif.
          endif.
          if v_found_info = ' '.
            perform input_error.
            write:/28 '**ERROR** Missing info record for fixed vendor:', tbl_converted-flief.
          endif.
        endif.
      endif.
    endif.

* 29 - purchasing organization

    if tbl_converted-ekorg <> ' '.
      clear tbl_t024e.
      read table tbl_t024e with key ekorg = tbl_converted-ekorg
                                    binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid purchasing organization: ', tbl_converted-ekorg.
      else.
        if v_valid_t001w = 'X'.
          clear tbl_t024w.
          read table tbl_t024w with key werks = tbl_converted-werks
                                        ekorg = tbl_converted-ekorg
                                        binary search.
          if sy-subrc <> 0.
            perform input_error.
            write:/28 '**ERROR** Invalid plant/purchasing organization: ', tbl_converted-werks,
                      tbl_converted-ekorg.
          endif.
        endif.
      endif.
    endif.

    if tbl_converted-ekorg = ' ' and tbl_converted-flief <> ' '.
      perform input_error.
      write:/28 '**ERROR** Purchasing organization is required with fixed vendor'.
    endif.

* 30 - info record - the material or material group must match

    if tbl_converted-infnr <> ' '.
      clear tbl_eina_infnr.
      read table tbl_eina_infnr with key infnr = tbl_converted-infnr
                                         binary search.
      if sy-subrc <> 0.
        perform input_error.
        write:/28 '**ERROR** Invalid info record number: ', tbl_converted-infnr.
      else.
        if tbl_converted-matnr <> ' ' and tbl_eina_infnr-matkl = ' '.             "match on material
          if  tbl_converted-matnr <> tbl_eina_infnr-matnr or tbl_converted-flief <> tbl_eina_infnr-lifnr.
            perform input_error.
            write:/28 '**ERROR** Invalid info record number: ', tbl_converted-infnr,
                      tbl_eina_infnr-matnr, tbl_eina_infnr-lifnr.
          endif.
        endif.
        if tbl_converted-matkl <> ' ' and tbl_eina_infnr-matnr = ' '.             "match on material group
          if tbl_converted-matkl <> tbl_eina_infnr-matkl or tbl_converted-flief <> tbl_eina_infnr-lifnr.
            perform input_error.
            write:/28 '**ERROR** Invalid info record number: ', tbl_converted-infnr,
                      tbl_eina_infnr-matkl, tbl_eina_infnr-lifnr.
          endif.
        endif.
      endif.
    endif.

* 31 - vendor material - no validation required

    if v_error = 'X'.
      v_error_count = v_error_count + 1.
    endif.
    tbl_load = tbl_converted.
    append tbl_load.

  endloop.

  free: tbl_mara, tbl_mard, tbl_t001w, tbl_t001l, tbl_lfa1, tbl_lfm1,
        tbl_csks, tbl_tka02, tbl_aufk, tbl_skb1, tbl_converted.

endform.                    " validate_individual_fields
*&---------------------------------------------------------------------*
*&      Form  load_validation_files
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_validation_files .

  clear: tbl_matnr, tbl_werks, tbl_matnr_werks, tbl_matnr_werks_lgort, tbl_bukrs,
         tbl_lifnr, tbl_lifnr_ekorg, tbl_poski, tbl_kostl, tbl_aufnr, tbl_saknr,
         tbl_matnr_flief, tbl_matkl_flief, tbl_infnr, tbl_aufnr_netw, tbl_objnr,
         tbl_posid.

  refresh: tbl_matnr, tbl_werks, tbl_matnr_werks, tbl_matnr_werks_lgort, tbl_bukrs,
           tbl_lifnr, tbl_lifnr_ekorg, tbl_poski, tbl_kostl, tbl_aufnr, tbl_saknr,
           tbl_matnr_flief, tbl_matkl_flief, tbl_infnr, tbl_aufnr_netw, tbl_objnr,
           tbl_posid.

  refresh: tbl_mara, tbl_marc, tbl_mard, tbl_t001w, tbl_t001l, tbl_lfa1, tbl_lfm1,
           tbl_prps, tbl_csks, tbl_tka02, tbl_aufk, tbl_skb1, tbl_eina_matnr,
           tbl_eina_matkl, tbl_eina_infnr, tbl_caufv, tbl_afvc, tbl_jest,
           tbl_t006, tbl_t006_iso, tbl_mbew, tbl_t141.

  loop at tbl_converted.
    if tbl_converted-matnr <> ' ' and tbl_matnr-matnr <> tbl_converted-matnr.
      tbl_matnr-matnr = tbl_converted-matnr.
      append tbl_matnr.
    endif.
    if tbl_converted-werks <> ' ' and tbl_werks-werks <> tbl_converted-werks.
      tbl_werks-werks = tbl_converted-werks.
      append tbl_werks.
    endif.
    if tbl_converted-matnr <> ' ' and tbl_converted-werks <> ' ' and
       ( tbl_converted-matnr <> tbl_matnr_werks-matnr or
         tbl_converted-werks <> tbl_matnr_werks-werks ).
      tbl_matnr_werks-matnr = tbl_converted-matnr.
      tbl_matnr_werks-werks = tbl_converted-werks.
      append tbl_matnr_werks.
    endif.
    if tbl_converted-matnr <> ' ' and tbl_converted-werks <> ' ' and tbl_converted-lgort <> ' ' and
       ( tbl_converted-matnr <> tbl_matnr_werks_lgort-matnr or
         tbl_converted-werks <> tbl_matnr_werks_lgort-werks or
         tbl_converted-lgort <> tbl_matnr_werks_lgort-lgort ).
      tbl_matnr_werks_lgort-matnr = tbl_converted-matnr.
      tbl_matnr_werks_lgort-werks = tbl_converted-werks.
      tbl_matnr_werks_lgort-lgort = tbl_converted-lgort.
      append tbl_matnr_werks_lgort.
    endif.
    if tbl_converted-werks <> ' ' and tbl_converted-lgort <> ' ' and
       ( tbl_converted-werks <> tbl_werks_lgort-werks or
         tbl_converted-lgort <> tbl_werks_lgort-lgort ).
      tbl_werks_lgort-werks = tbl_converted-werks.
      tbl_werks_lgort-lgort = tbl_converted-lgort.
      append tbl_werks_lgort.
    endif.
    if tbl_converted-lifnr <> ' ' and tbl_converted-lifnr <> tbl_lifnr-lifnr.
      tbl_lifnr-lifnr = tbl_converted-lifnr.
      append tbl_lifnr.
    endif.
    if tbl_converted-flief <> ' ' and tbl_converted-flief <> tbl_lifnr-lifnr.
      tbl_lifnr-lifnr = tbl_converted-flief.
      append tbl_lifnr.
    endif.
    if tbl_converted-lifnr <> ' ' and tbl_converted-ekorg <> ' ' and
       ( tbl_converted-lifnr <> tbl_lifnr_ekorg-lifnr or
         tbl_converted-ekorg <> tbl_lifnr_ekorg-ekorg ).
      tbl_lifnr_ekorg-lifnr = tbl_converted-lifnr.
      tbl_lifnr_ekorg-ekorg = tbl_converted-ekorg.
      append tbl_lifnr_ekorg.
    endif.
    if tbl_converted-flief <> ' ' and tbl_converted-ekorg <> ' ' and
       ( tbl_converted-flief <> tbl_lifnr_ekorg-lifnr or
         tbl_converted-ekorg <> tbl_lifnr_ekorg-ekorg ).
      tbl_lifnr_ekorg-lifnr = tbl_converted-flief.
      tbl_lifnr_ekorg-ekorg = tbl_converted-ekorg.
      append tbl_lifnr_ekorg.
    endif.
    if tbl_converted-poski <> ' ' and tbl_converted-poski <> tbl_poski-poski.
      if tbl_converted-poski cs '-'.
        tbl_poski-poski = tbl_converted-poski.
        append tbl_poski.
      else.
        tbl_posid-posid = tbl_converted-poski.
        append tbl_posid.
      endif.
    endif.
    if tbl_converted-kostl <> ' ' and tbl_converted-kostl <> tbl_kostl-kostl.
      tbl_kostl-kostl = tbl_converted-kostl.
      append tbl_kostl.
    endif.
    if tbl_converted-aufnr <> ' ' and tbl_converted-aufnr <> tbl_aufnr-aufnr.
      tbl_aufnr-aufnr = tbl_converted-aufnr.
      append tbl_aufnr.
      concatenate 'OR' tbl_converted-aufnr into tbl_objnr-objnr.
      append tbl_objnr.
    endif.
    if tbl_converted-sakto <> ' ' and tbl_converted-sakto <> tbl_saknr-saknr.
      tbl_saknr-saknr = tbl_converted-sakto.
      append tbl_saknr.
    endif.
    if tbl_converted-matnr <> ' ' and tbl_converted-flief <> ' ' and
       ( tbl_converted-matnr <> tbl_matnr_flief-matnr or
         tbl_converted-flief <> tbl_matnr_flief-flief ).
      tbl_matnr_flief-matnr = tbl_converted-matnr.
      tbl_matnr_flief-flief = tbl_converted-flief.
      append tbl_matnr_flief.
    endif.
    if tbl_converted-matkl <> ' ' and tbl_converted-flief <> ' ' and
      ( tbl_converted-matkl <> tbl_matkl_flief-matkl or
        tbl_converted-flief <> tbl_matkl_flief-flief ).
      tbl_matkl_flief-matkl = tbl_converted-matkl.
      tbl_matkl_flief-flief = tbl_converted-flief.
      append tbl_matkl_flief.
    endif.
    if tbl_converted-infnr <> ' ' and tbl_converted-infnr <> tbl_infnr-infnr.
      tbl_infnr-infnr = tbl_converted-infnr.
      append tbl_infnr.
    endif.
    if tbl_converted-nplnr <> ' ' and tbl_converted-nplnr <> tbl_aufnr_netw.
      tbl_aufnr_netw-aufnr = tbl_converted-nplnr.
      append tbl_aufnr_netw.
    endif.

  endloop.

  sort tbl_matnr.
  delete adjacent duplicates from tbl_matnr.
  sort tbl_werks.
  delete adjacent duplicates from tbl_werks.
  sort tbl_matnr_werks.
  delete adjacent duplicates from tbl_matnr_werks.
  sort tbl_matnr_werks_lgort.
  delete adjacent duplicates from tbl_matnr_werks_lgort.
  sort tbl_werks_lgort.
  delete adjacent duplicates from tbl_werks_lgort.
  sort tbl_lifnr.
  delete adjacent duplicates from tbl_lifnr.
  sort tbl_lifnr_ekorg.
  delete adjacent duplicates from tbl_lifnr_ekorg.
  sort tbl_poski.
  delete adjacent duplicates from tbl_poski.
  sort tbl_kostl.
  delete adjacent duplicates from tbl_kostl.
  sort tbl_aufnr.
  delete adjacent duplicates from tbl_aufnr.
  sort tbl_saknr.
  delete adjacent duplicates from tbl_saknr.
  sort tbl_matnr_flief by matnr flief.
  delete adjacent duplicates from tbl_matnr_flief.
  sort tbl_matkl_flief by matkl flief.
  delete adjacent duplicates from tbl_matkl_flief.
  sort tbl_infnr.
  delete adjacent duplicates from tbl_infnr.
  sort tbl_aufnr_netw.
  delete adjacent duplicates from tbl_aufnr_netw.
  sort tbl_objnr.
  delete adjacent duplicates from tbl_objnr.

* materials
  if not tbl_matnr[] is initial.
    select matnr lvorm matkl mstae
           into table tbl_mara
           from mara
           for all entries in tbl_matnr
           where matnr = tbl_matnr-matnr.
    sort tbl_mara by matnr.
  endif.

* materials & plants
  if not tbl_matnr_werks[] is initial.
    select matnr werks lvorm mmsta plifz
           into table tbl_marc
           from marc
           for all entries in tbl_matnr_werks
           where matnr = tbl_matnr_werks-matnr
             and werks = tbl_matnr_werks-werks
             and lvorm = ' '.
    sort tbl_marc by matnr werks.
  endif.

* storage locations
  if not tbl_werks[] is initial.
    select werks lgort
           into table tbl_t001l
           from t001l
           for all entries in tbl_werks
           where werks = tbl_werks-werks.      " get all SLOCs
    sort tbl_t001l.
  endif.

* materials & plants & storage locations - get all storage locations
  if not tbl_matnr_werks[] is initial.
    select matnr werks lgort lvorm
           into table tbl_mard
           from mard
           for all entries in tbl_matnr_werks
           where matnr = tbl_matnr_werks-matnr
             and werks = tbl_matnr_werks-werks
             and lvorm = ' '.
    sort tbl_mard by matnr werks lgort.
  endif.

* plants
  if not tbl_werks[] is initial.
    select werks
           into table tbl_t001w
           from t001w
           for all entries in tbl_werks
           where werks = tbl_werks-werks.
    sort tbl_t001w by werks.
  endif.

* material valuation
  if not tbl_matnr_werks[] is initial.
    select matnr bwkey
           into table tbl_mbew
           from mbew
           for all entries in tbl_matnr_werks
           where matnr = tbl_matnr_werks-matnr
             and bwkey = tbl_matnr_werks-werks
             and lvorm = ' '.
    sort tbl_mbew by matnr bwkey.
  endif.

* units of measure
  select msehi isocode
         into table tbl_t006
         from t006.
  sort tbl_t006.

  loop at tbl_t006.
    if tbl_t006-isocode <> ' ' and
       tbl_t006-isocode <> tbl_t006-msehi.
      tbl_t006_iso = tbl_t006.
      append tbl_t006_iso.
    endif.
  endloop.
  sort tbl_t006_iso by isocode.

* purchasing group
  select ekgrp
         into table tbl_t024
         from t024.
  sort tbl_t024.

* material group
  select matkl
         into table tbl_t023
         from t023.
  sort tbl_t023.

* currency
  select waers
         into table tbl_tcurc
         from tcurc.
  sort tbl_tcurc.

* account assignment category
  select knttp
         into table tbl_t163k
         from t163k.
  sort tbl_t163k.

* purchasing organization
  select ekorg
         into table tbl_t024e
         from t024e.
  sort tbl_t024e.

  select werks ekorg
         into table tbl_t024w
         from t024w.
  sort tbl_t024w by werks ekorg.

* valuation area
  if not tbl_werks[] is initial.
    select bwkey bukrs
           into table tbl_t001k
           from t001k
           for all entries in tbl_werks
           where bwkey = tbl_werks-werks.
    sort tbl_t001k by bwkey.
  endif.

  loop at tbl_t001k.
    tbl_bukrs-bukrs = tbl_t001k-bukrs.
    append tbl_bukrs.
  endloop.
  sort tbl_bukrs.
  delete adjacent duplicates from tbl_bukrs.

* company code
  if not tbl_bukrs[] is initial.
    select bukrs waers ktopl
           into table tbl_t001
           from t001
           for all entries in tbl_bukrs
           where bukrs = tbl_bukrs-bukrs.
    sort tbl_t001 by bukrs.
  endif.

* vendors
  if not tbl_lifnr[] is initial.
    select lifnr loevm sperr sperm
           into table tbl_lfa1
           from lfa1
           for all entries in tbl_lifnr
           where lifnr = tbl_lifnr-lifnr.
    sort tbl_lfa1 by lifnr.
  endif.

* vendors/purchasing org
  if not tbl_lifnr_ekorg[] is initial.
    select lifnr ekorg sperm loevm
           into table tbl_lfm1
           from lfm1
           for all entries in tbl_lifnr_ekorg
           where lifnr = tbl_lifnr_ekorg-lifnr
             and ekorg = tbl_lifnr_ekorg-ekorg.
    sort tbl_lfm1 by lifnr ekorg.
  endif.

* WBS element
  if not tbl_poski[] is initial.
    select pspnr posid objnr poski pbukr
           into table tbl_prps
           from prps
           for all entries in tbl_poski
           where poski = tbl_poski-poski.
  endif.

  if not tbl_posid[] is initial.
    select pspnr posid objnr poski pbukr
         appending table tbl_prps
         from prps
         for all entries in tbl_posid
         where posid = tbl_posid-posid.
  endif.
  sort tbl_prps by poski.

* WBS status - save technically complete and closed values (see TJ02T)
  if not tbl_prps[] is initial.
    select objnr stat
           into table tbl_jest
           from jest
           for all entries in tbl_prps
           where objnr = tbl_prps-objnr
             and inact <> 'X'               "only active
             and ( stat = 'I0001' or              "open
                   stat = 'I0002' or              "released
                   stat = 'I0013' or              "deleted
                   stat = 'I0076' or              "deleted
                   stat = 'I0045' or              "technical complete
                   stat = 'I0046' ).              "closed
    sort tbl_jest by objnr ascending stat descending.
  endif.

* controlling area assignment
  select bukrs kokrs
         into table tbl_tka02
         from tka02.
  sort tbl_tka02 by bukrs.

* cost center
  if not tbl_kostl[] is initial.
    select kokrs kostl
           into table tbl_csks
           from csks
           for all entries in tbl_kostl
           where kostl = tbl_kostl-kostl
             and datbi > sy-datum.
    sort tbl_csks by kokrs kostl.
  endif.

* orders
  if not tbl_aufnr[] is initial.
    select aufnr bukrs
           into table tbl_aufk
           from aufk
           for all entries in tbl_aufnr
           where aufnr = tbl_aufnr-aufnr.
    sort tbl_aufk.
  endif.

* order status - save technically complete and closed values (see TJ02T)
  if not tbl_objnr[] is initial.
    select objnr stat
           appending table tbl_jest
           from jest
           for all entries in tbl_objnr
           where objnr = tbl_objnr-objnr
             and inact <> 'X'               "only active
             and ( stat = 'I0001' or              "open
                   stat = 'I0002' or              "released
                   stat = 'I0013' or              "deleted
                   stat = 'I0076' or              "deleted
                   stat = 'I0045' or              "technical complete
                   stat = 'I0046' ).              "closed
    sort tbl_jest by objnr ascending stat descending.
  endif.

* g/l account
  if not tbl_saknr[] is initial.
    select bukrs saknr
           into table tbl_skb1
           from skb1
           for all entries in tbl_saknr
           where saknr = tbl_saknr-saknr.
    sort tbl_skb1 by bukrs saknr.
  endif.

* info records

  if not tbl_matnr_flief[] is initial.
    select infnr matnr lifnr
           into table tbl_eina_matnr
           from eina
           for all entries in tbl_matnr_flief
           where lifnr = tbl_matnr_flief-flief
             and matnr = tbl_matnr_flief-matnr.
    sort tbl_eina_matnr by matnr lifnr.
  endif.

  if not tbl_matkl_flief[] is initial.
    select infnr matkl lifnr
           into table tbl_eina_matkl
           from eina
           for all entries in tbl_matkl_flief
           where lifnr = tbl_matkl_flief-flief
             and matkl = tbl_matkl_flief-matkl.
    sort tbl_eina_matkl by matkl lifnr.
  endif.

  if not tbl_infnr[] is initial.
    select infnr matnr matkl lifnr
           into table tbl_eina_infnr
           from eina
           for all entries in tbl_infnr
           where infnr = tbl_infnr-infnr.
    sort tbl_eina_infnr by infnr.
  endif.

* network and activity

  if not tbl_aufnr_netw[] is initial.
    select aufnr bukrs objnr aufpl
           into table tbl_caufv
           from caufv
           for all entries in tbl_aufnr_netw
           where aufnr =  tbl_aufnr_netw-aufnr
             and auart = 'NETW'.
    sort tbl_caufv.

    if not tbl_caufv[] is initial.
      select aufpl vornr
             into table tbl_afvc
             from afvc
             for all entries in tbl_caufv
             where aufpl = tbl_caufv-aufpl.
      sort tbl_afvc by aufpl vornr.
    endif.

* order status - save technically complete and closed values (see TJ02T)
    if not tbl_caufv[] is initial.
      select objnr stat
             appending table tbl_jest
             from jest
             for all entries in tbl_caufv
             where objnr = tbl_caufv-objnr
               and inact <> 'X'               "only active
               and ( stat = 'I0001' or              "open
                     stat = 'I0002' or              "released
                     stat = 'I0013' or              "deleted
                     stat = 'I0076' or              "deleted
                     stat = 'I0045' or              "technical complete
                     stat = 'I0046' ).              "closed
      sort tbl_jest by objnr ascending stat descending.
    endif.

* keep the highest stat value, only I0002 is valid but we must not have only I0001
    loop at tbl_jest.
      if tbl_jest-objnr = v_prev_objnr.
        delete tbl_jest.
      endif.
      v_prev_objnr = tbl_jest-objnr.
    endloop.

  endif.

* material status - only keep the ones that are to be blocked
  select mmsta
         into table tbl_t141
         from t141
         where deink = 'B'.

  sort tbl_t141.

  free: tbl_matnr, tbl_werks, tbl_matnr_werks, tbl_matnr_werks_lgort,
        tbl_werks_lgort, tbl_bukrs, tbl_lifnr, tbl_lifnr_ekorg, tbl_poski,
        tbl_kostl, tbl_aufnr, tbl_saknr, tbl_matnr_flief, tbl_infnr,
        tbl_aufnr_netw, tbl_matkl_flief, tbl_posid.

endform.                    " load_validation_files
*&---------------------------------------------------------------------*
*&      Form  input_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form input_error .

  if v_error = ' '.
    v_next_rec_num = v_record_number + 1.
    read table tbl_pc_data index v_next_rec_num.
    skip 1.
    format reset.
    format color col_normal.
    write:/ ' ROW', v_next_rec_num, '==>',
            tbl_pc_data(111).
    format reset.
  endif.
  v_error = 'X'.

endform.                    " input_error
*&---------------------------------------------------------------------*
*&      Form  create_purchasing_requisition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_purchasing_requisition.

  if s_valid = 'X'.                             "only validate
    stop.
  endif.

  if s_load = 'X' and v_error_count > 0.        "cannot load errors
    stop.
  endif.

  commit work.                                    "reset the time clock

  clear: v_prev_werks, v_prev_afnam, v_current_load_cnt, v_commit_count.
  refresh: tbl_bapi_items, tbl_bapi_acct_assign, tbl_bapi_text, tbl_delivery.

  loop at tbl_load.

    v_current_load_cnt = sy-tabix.

    if ( v_prev_afnam <> tbl_load-afnam or
         v_prev_werks <> tbl_load-werks or
         v_purch_req_item_cnt > 999 ) and       "max 999 items in a purchasin requisition
       sy-tabix <> 1.
      perform execute_bapi.
    else.
      if sy-tabix = 1.
        v_purch_req_item_cnt = 1.
        v_row_start_cnt = 1.
        v_row_end_cnt = 1.
      else.
        v_row_end_cnt = v_row_end_cnt + 1.
        v_purch_req_item_cnt = v_purch_req_item_cnt + 1.
      endif.
    endif.

    v_prev_afnam = tbl_load-afnam.
    v_prev_werks = tbl_load-werks.

* get the planned delivery days from the MRP 2 screen
    clear tbl_marc.
    read table tbl_marc with key matnr = tbl_load-matnr
                                 werks = tbl_load-werks
                                 binary search.
    v_date = sy-datum + tbl_marc-plifz.

    clear: tbl_bapi_items, tbl_bapi_acct_assign, tbl_bapi_text.

* setup the item

    v_itemx10 = v_purch_req_item_cnt * 10.
    tbl_bapi_items-preq_item = v_itemx10.
    tbl_bapi_items-doc_type = 'NB'.
    tbl_bapi_items-created_by = sy-uname.
    tbl_bapi_items-preq_date = tbl_load-badat.
    tbl_bapi_items-preq_name = tbl_load-afnam.
    tbl_bapi_items-plant = tbl_load-werks.
    tbl_bapi_items-store_loc = tbl_load-lgort.
    tbl_bapi_items-quantity = tbl_load-menge.
    tbl_bapi_items-unit = tbl_load-meins.
    tbl_bapi_items-material = tbl_load-matnr.
    tbl_bapi_items-short_text = tbl_load-txz01.
    tbl_bapi_items-deliv_date = tbl_load-lfdat.
    tbl_bapi_items-pur_group = tbl_load-ekgrp.
    tbl_bapi_items-mat_grp = tbl_load-matkl.
    tbl_bapi_items-trackingno = tbl_load-bednr.
    tbl_bapi_items-c_amt_bapi = tbl_load-preis.
    tbl_bapi_items-currency = tbl_load-waers.
    tbl_bapi_items-price_unit = tbl_load-peinh.
    tbl_bapi_items-acctasscat = tbl_load-knttp.
    tbl_bapi_items-des_vendor = tbl_load-lifnr.
    tbl_bapi_items-fixed_vend = tbl_load-flief.
    tbl_bapi_items-purch_org = tbl_load-ekorg.
    tbl_bapi_items-info_rec = tbl_load-infnr.
    tbl_bapi_items-vend_mat = tbl_load-idnlf.

    append tbl_bapi_items.

* setup the account assignment

    tbl_bapi_acct_assign-preq_item = v_itemx10.
    tbl_bapi_acct_assign-g_l_acct = tbl_load-sakto.

    if tbl_load-poski <> ' '.
      clear tbl_prps.
      read table tbl_prps with key poski = tbl_load-poski
                                   binary search.
      if sy-subrc = 0.
        tbl_bapi_acct_assign-wbs_elem = tbl_prps-pspnr.
      endif.
    endif.

    tbl_bapi_acct_assign-cost_ctr = tbl_load-kostl.
    tbl_bapi_acct_assign-order_no = tbl_load-aufnr.
    tbl_bapi_acct_assign-network = tbl_load-nplnr.
    tbl_bapi_acct_assign-activity = tbl_load-vornr.
    tbl_bapi_acct_assign-unload_pt = tbl_load-ablad.
    tbl_bapi_acct_assign-gr_rcpt = tbl_load-wempf.

    append tbl_bapi_acct_assign.

* setup the text differently between East & West
* the East uses 3 texts & the West uses 5

    if tvarvc-low = 'EAST'.
      if tbl_load-text1 <> ' '.
        tbl_bapi_text-preq_item = v_itemx10.
        tbl_bapi_text-text_id = 'B01'.
        tbl_bapi_text-text_line = tbl_load-text1.
        append tbl_bapi_text.
      endif.
      if tbl_load-text2 <> ' '.
        tbl_bapi_text-preq_item = v_itemx10.
        tbl_bapi_text-text_id = 'B02'.
        tbl_bapi_text-text_line = tbl_load-text2.
        append tbl_bapi_text.
      endif.
      if tbl_load-text3 <> ' '.
        tbl_bapi_text-preq_item = v_itemx10.
        tbl_bapi_text-text_id = 'B03'.
        tbl_bapi_text-text_line = tbl_load-text3.
        append tbl_bapi_text.
      endif.
    else.
      if tbl_load-text1 <> ' '.
        tbl_bapi_text-preq_item = v_itemx10.
        tbl_bapi_text-text_id = 'B01'.
        tbl_bapi_text-text_line = tbl_load-text1.
        append tbl_bapi_text.
      endif.
      if tbl_load-text2 <> ' '.
        tbl_bapi_text-preq_item = v_itemx10.
        tbl_bapi_text-text_id = 'B02'.
        tbl_bapi_text-text_line = tbl_load-text2.
        append tbl_bapi_text.
      endif.
      if tbl_load-text3 <> ' '.
        tbl_bapi_text-preq_item = v_itemx10.
        tbl_bapi_text-text_id = 'B03'.
        tbl_bapi_text-text_line = tbl_load-text3.
        append tbl_bapi_text.
      endif.
      if tbl_load-text4 <> ' '.
        tbl_bapi_text-preq_item = v_itemx10.
        tbl_bapi_text-text_id = 'B06'.
        tbl_bapi_text-text_line = tbl_load-text4.
        append tbl_bapi_text.
      endif.
      if tbl_load-text5 <> ' '.
        tbl_bapi_text-preq_item = v_itemx10.
        tbl_bapi_text-text_id = 'B08'.
        tbl_bapi_text-text_line = tbl_load-text5.
        append tbl_bapi_text.
      endif.
    endif.

* setup the delivery date & difference - to be added to the EXCEL output file

    clear: tbl_marc, tbl_delivery.

    if tbl_load-matnr <> ' '.
      tbl_delivery-material = 'Y'.
      read table tbl_marc with key matnr = tbl_load-matnr
                                werks = tbl_load-werks
                                binary search.
      if sy-subrc <> 0.
        write:/8 'Program error- material/plant:', tbl_load-matnr,
                  tbl_load-werks.
        if s_errors is initial.
          stop.
        endif.
      endif.

      v_date = sy-datum + tbl_marc-plifz.
      tbl_delivery-calc_lfdat = v_date.
      if v_date > tbl_load-lfdat.
        tbl_delivery-days_diff = v_date - tbl_load-lfdat.
      endif.
    else.
      tbl_delivery-material = 'N'.    "place holder if items have mixed material & material desc
    endif.

    append tbl_delivery.

  endloop.

  perform execute_bapi.

  commit work.

endform.                    " create_purchasing_requisition

*&---------------------------------------------------------------------*
*&      Form  upload_excel_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

form upload_excel_file .

  refresh: tbl_pc_data, tbl_intern.

* create internal table with row, column & value
  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = p_pcfile
      i_begin_col             = begcol
      i_begin_row             = begrow
      i_end_col               = endcol
      i_end_row               = endrow
    tables
      intern                  = tbl_intern
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.

  if sy-subrc <> 0.
    write:/ 'Upload Error ', sy-subrc, '  EXCEL file formatting error'.
  endif.

* make sure the data is in row & column sequence
  sort tbl_intern by row col.
* move the EXCEL data into specific SAP data fields
  loop at tbl_intern.
    case tbl_intern-col.
      when 1.
        tbl_pc_data-badat = tbl_intern-value.
      when 2.
        tbl_pc_data-afnam = tbl_intern-value.
      when 3.
        tbl_pc_data-werks = tbl_intern-value.
      when 4.
        tbl_pc_data-lgort = tbl_intern-value.
      when 5.
        tbl_pc_data-menge = tbl_intern-value.
      when 6.
        tbl_pc_data-meins = tbl_intern-value.
      when 7.
        tbl_pc_data-matnr = tbl_intern-value.
      when 8.
        tbl_pc_data-txz01 = tbl_intern-value.
      when 9.
        tbl_pc_data-round = tbl_intern-value.
      when 10.
        tbl_pc_data-lead = tbl_intern-value.
      when 11.
        tbl_pc_data-lfdat = tbl_intern-value.
      when 12.
        tbl_pc_data-ekgrp = tbl_intern-value.
      when 13.
        tbl_pc_data-matkl = tbl_intern-value.
      when 14.
        tbl_pc_data-bednr = tbl_intern-value.
      when 15.
        tbl_pc_data-preis = tbl_intern-value.
      when 16.
        tbl_pc_data-waers = tbl_intern-value.
      when 17.
        tbl_pc_data-peinh = tbl_intern-value.
      when 18.
        tbl_pc_data-knttp = tbl_intern-value.
      when 19.
        tbl_pc_data-sakto = tbl_intern-value.
      when 20.
        tbl_pc_data-poski = tbl_intern-value.
      when 21.
        tbl_pc_data-kostl = tbl_intern-value.
      when 22.
        tbl_pc_data-aufnr = tbl_intern-value.
      when 23.
        tbl_pc_data-nplnr = tbl_intern-value.
      when 24.
        tbl_pc_data-vornr = tbl_intern-value.
      when 25.
        tbl_pc_data-ablad = tbl_intern-value.
      when 26.
        tbl_pc_data-wempf = tbl_intern-value.
      when 27.
        tbl_pc_data-lifnr = tbl_intern-value.
      when 28.
        tbl_pc_data-flief = tbl_intern-value.
      when 29.
        tbl_pc_data-ekorg = tbl_intern-value.
      when 30.
        tbl_pc_data-infnr = tbl_intern-value.
      when 31.
        tbl_pc_data-idnlf = tbl_intern-value.
      when 32.
        tbl_pc_data-text1 = tbl_intern-value.
      when 33.
        tbl_pc_data-text2 = tbl_intern-value.
      when 34.
        tbl_pc_data-text3 = tbl_intern-value.
      when 35.
        tbl_pc_data-text4 = tbl_intern-value.
      when 36.
        tbl_pc_data-text5 = tbl_intern-value.
      when others.
        write:/ 'Too many columns in EXCEL file in row', tbl_intern-row.
        stop.
    endcase.
    at end of row.
      append tbl_pc_data.
      clear tbl_pc_data.
    endat.
  endloop.

  free tbl_intern.

endform.                    " upload_excel_file
*&---------------------------------------------------------------------*
*&      Form  validate_excel_numerics
*&---------------------------------------------------------------------*
*       All the SAP fields expecting numeric, must be valid or else
*       the program will abend.
*----------------------------------------------------------------------*
form validate_excel_numerics .

  refresh tbl_input.

  loop at tbl_pc_data into v_pc_line.
* bypass the first line, it is a column header
    if sy-tabix = 1.
      move-corresponding v_pc_line to tbl_pc_header.
      continue.
    endif.

    v_record_number = sy-tabix.

* order date check
    if v_pc_line-badat <> ' '.
      if p_ymd_sl = 'X'.
        concatenate v_pc_line-badat(4) v_pc_line-badat+5(2)       "YYYY/MM/DD
                v_pc_line-badat+8(2) into v_date8.
      endif.
      if p_mdy_sl = 'X'.
        concatenate v_pc_line-badat+6(4) v_pc_line-badat(2)       "MM/DD/YYYY
                    v_pc_line-badat+3(2) into v_date8.
      endif.
      if p_dmy_sl = 'X'.
        concatenate v_pc_line-badat+6(4) v_pc_line-badat+3(2)     "DD/MM/YYYY
                    v_pc_line-badat(2) into v_date8.
      endif.
      if p_ymd = 'X'.
        v_date8 = v_pc_line-badat.                                "YYYYMMDD
      endif.

      v_date = v_date8.

      call function 'DATE_CHECK_PLAUSIBILITY'
        exporting
          date                      = v_date
        exceptions
          plausibility_check_failed = 1
          others                    = 2.

      if sy-subrc <> 0.
        format color col_negative.
        write:/ 'INVALID FORMAT ON ORDER DATE - VALUE:',
                 v_pc_line-badat.
        format reset.
        format color col_normal.
        write:/ ' ROW', v_record_number, '==>', v_pc_line(111).
        stop.
      else.
        v_pc_line-badat = v_date.
      endif.
    else.
      v_pc_line-badat = sy-datum.                              "default in current date
    endif.

* quantity check
    if v_pc_line-menge <> ' '.
      if v_pc_line-menge co ' .,0123456789' and
         v_pc_line-menge ca '0123456789'.
        tbl_input-menge = v_pc_line-menge.
      else.
        format color col_negative.
        write:/ 'INVALID FORMAT ON QUANTITY - VALUE:',
                 v_pc_line-menge.
        format reset.
        format color col_normal.
        write:/ ' ROW', v_record_number, '==>', v_pc_line(111).
        stop.
      endif.
    endif.

* delivery date check
    if v_pc_line-lfdat <> ' '.
      if p_ymd_sl = 'X'.
        concatenate v_pc_line-lfdat(4) v_pc_line-lfdat+5(2)       "YYYY/MM/DD
                v_pc_line-lfdat+8(2) into v_date8.
      endif.
      if p_dmy_sl = 'X'.
        concatenate v_pc_line-lfdat+6(4) v_pc_line-lfdat+3(2)     "DD/MM/YYYY
                    v_pc_line-lfdat(2) into v_date8.
      endif.
      if p_mdy_sl = 'X'.
        concatenate v_pc_line-lfdat+6(4) v_pc_line-lfdat(2)       "MM/DD/YYYY
                    v_pc_line-lfdat+3(2) into v_date8.
      endif.
      if p_ymd = 'X'.
        v_date8 = v_pc_line-lfdat.                                "YYYYMMDD
      endif.

      v_date = v_date8.
      call function 'DATE_CHECK_PLAUSIBILITY'
        exporting
          date                      = v_date
        exceptions
          plausibility_check_failed = 1
          others                    = 2.

      if sy-subrc <> 0.
        format color col_negative.
        write:/ 'INVALID FORMAT ON DELIVERY DATE - VALUE:',
                 v_pc_line-lfdat.
        format reset.
        format color col_normal.
        write:/ ' ROW', v_record_number, '==>', v_pc_line(111).
        stop.
      else.
        v_pc_line-lfdat = v_date.
      endif.
    endif.

* material group
    if v_pc_line-matkl <> ' '.
      if v_pc_line-matkl co ' 0123456789'.
      else.
        if tvarvc-low = 'EAST'.
          format color col_negative.
          write:/ 'INVALID FORMAT ON MATERIAL GROUP - VALUE:',
                      v_pc_line-matkl.
          format reset.
          format color col_normal.
          write:/ ' ROW', v_record_number, '==>', v_pc_line(111).
          stop.
        endif.
      endif.
    endif.

* price check
    if v_pc_line-preis <> ' '.
      if v_pc_line-preis co ' .,0123456789' and
         v_pc_line-preis ca '0123456789'.
        tbl_input-preis = v_pc_line-preis.
      else.
        format color col_negative.
        write:/ 'INVALID FORMAT ON PRICE - VALUE:',
                 v_pc_line-preis.
        format reset.
        format color col_normal.
        write:/ ' ROW', v_record_number, '==>', v_pc_line(111).
        stop.
      endif.
    endif.

* price units
    if v_pc_line-peinh <> ' '.
      if v_pc_line-peinh co ' 0123456789'.
      else.
        format color col_negative.
        write:/ 'INVALID FORMAT ON PRICE UNITS - VALUE:',
                 v_pc_line-peinh.
        format reset.
        format color col_normal.
        write:/ ' ROW', v_record_number, '==>', v_pc_line(111).
        stop.
      endif.
    else.
      if tbl_input-preis <> 0.
        v_pc_line-peinh = 1.             "default in 1 if there is a price
      endif.
    endif.

    move-corresponding v_pc_line to tbl_input.

    append tbl_input.
  endloop.

  describe table tbl_input lines v_input_count.

endform.                    " validate_excel_numerics

*&---------------------------------------------------------------------*
*&      Form  CREATE_EXCEL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form create_excel_file .

  perform setup_excel_title.

  split p_pcfile at '.' into v_part1 v_part2.
  concatenate v_part1 '_RESULTS_' sy-datum sy-uzeit '.xls' into v_output_filename200.

  condense v_output_filename200.
  v_output_filename128 = v_output_filename200.

  concatenate v_part1 '_RESULTS_' sy-datum sy-uzeit '.xlsx' into v_output_filename200.

  data vt_output_file type string.
  vt_output_file = v_output_filename128.

  call function 'GUI_DOWNLOAD'
   exporting
*     BIN_FILESIZE                    =
     filename                        = vt_output_file
     filetype                        = 'ASC'
*     APPEND                          = ' '
     write_field_separator           = 'X'
*     HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS            = ABAP_TRUE
*   IMPORTING
*     FILELENGTH                      =
   tables
     data_tab                        = tbl_excel_output
     fieldnames                      = tbl_column_names
  exceptions
    file_write_error                = 1
    no_batch                        = 2
    gui_refuse_filetransfer         = 3
    invalid_type                    = 4
    no_authority                    = 5
    unknown_error                   = 6
    header_not_allowed              = 7
    separator_not_allowed           = 8
    filesize_not_allowed            = 9
    header_too_long                 = 10
    dp_error_create                 = 11
    dp_error_send                   = 12
    dp_error_write                  = 13
    unknown_dp_error                = 14
    access_denied                   = 15
    dp_out_of_memory                = 16
    disk_full                       = 17
    dp_timeout                      = 18
    file_not_found                  = 19
    dataprovider_exception          = 20
    control_flush_error             = 21
    others                          = 22
    .

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " CREATE_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  SETUP_EXCEL_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form setup_excel_title .

  tbl_column_names-name = tbl_pc_header-badat.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-afnam.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-werks.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-lgort.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-menge.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-meins.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-matnr.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-txz01.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-round.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-lead.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-lfdat.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-ekgrp.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-matkl.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-bednr.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-preis.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-waers.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-peinh.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-knttp.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-sakto.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-poski.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-kostl.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-aufnr.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-nplnr.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-vornr.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-ablad.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-wempf.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-lifnr.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-flief.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-ekorg.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-infnr.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-idnlf.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-text1.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-text2.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-text3.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-text4.
  append tbl_column_names.
  tbl_column_names-name = tbl_pc_header-text5.
  append tbl_column_names.
  tbl_column_names-name = 'New Purch Reqn'.
  append tbl_column_names.
  tbl_column_names-name = 'Anticipated Delivery Date'.
  append tbl_column_names.
  tbl_column_names-name = 'Delivery Discrepancy'.
  append tbl_column_names.
  tbl_column_names-name = 'BAPI error message'.
  append tbl_column_names.

endform.                    " SETUP_EXCEL_TITLE
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form execute_bapi.

  refresh tbl_bapi_return.

  perform clear_sap_memory.

  call function 'BAPI_REQUISITION_CREATE'
    exporting
*       SKIP_ITEMS_WITH_ERROR                =
      automatic_source                     = 'X'
    importing
      number                               = v_bapi_reqn_number
    tables
      requisition_items                    = tbl_bapi_items
      requisition_account_assignment       = tbl_bapi_acct_assign
      requisition_item_text                = tbl_bapi_text
*       REQUISITION_LIMITS                   =
*       REQUISITION_CONTRACT_LIMITS          =
*       REQUISITION_SERVICES                 =
*       REQUISITION_SRV_ACCASS_VALUES        =
      return                               = tbl_bapi_return
*       REQUISITION_SERVICES_TEXT            =
*       REQUISITION_ADDRDELIVERY             =
*       EXTENSIONIN                          =
            .


* match up the delivery data, BAPI returned info and orignal PC data
* to create the EXCEL output file

  v_row_current_cnt = v_row_start_cnt.

  while v_row_current_cnt <= v_row_end_cnt.

    v_row_next_cnt = v_row_current_cnt + 1.           "add 1 to skip by the header line
    clear: tbl_excel_output, tbl_delivery, tbl_pc_data.

    read table tbl_pc_data index v_row_next_cnt.
    move-corresponding tbl_pc_data to tbl_excel_output.

    read table tbl_delivery index 1.
    tbl_excel_output-calc_lfdat = tbl_delivery-calc_lfdat.
    tbl_excel_output-days_diff = tbl_delivery-days_diff.

    loop at tbl_bapi_return.
      if tbl_bapi_return-type = 'E'.
        if v_row_end_cnt = v_row_current_cnt.         "update counts by purchasing reqn not item
          v_bapi_err_count = v_bapi_err_count + 1.
        endif.
        concatenate tbl_bapi_return-message tbl_bapi_return-message_v1
                    tbl_bapi_return-message_v2 tbl_bapi_return-message_v3
                    tbl_bapi_return-message_v4 into tbl_excel_output-bapi_message separated by ' '.
      else.
        if v_row_end_cnt = v_row_current_cnt.         "update counts by purchasing reqn not item
          v_update_count = v_update_count + 1.
        endif.
        tbl_excel_output-banfn = v_bapi_reqn_number.
      endif.
    endloop.

    append tbl_excel_output.

    delete table tbl_delivery.
    v_row_current_cnt = v_row_current_cnt + 1.

  endwhile.

  v_purch_req_item_cnt = 1.
  v_row_start_cnt = v_current_load_cnt.
  v_row_end_cnt = v_current_load_cnt.

  v_commit_count = v_commit_count + 1.
  if v_commit_count > 50.
    commit work.
    v_commit_count = 0.
  endif.

  refresh: tbl_bapi_items, tbl_bapi_acct_assign, tbl_bapi_text, tbl_delivery.

endform.                    " EXECUTE_BAPI
*&---------------------------------------------------------------------*
*&      Form  CLEAR_SAP_MEMORY
*&---------------------------------------------------------------------*
*       Ensure previous online activities do no default in
*----------------------------------------------------------------------*
form clear_sap_memory .

  clear v_kokrs.
  set parameter id 'CAC' field v_kokrs.

  clear v_bnfpo.
  set parameter id 'BAP' field v_bnfpo.

  clear v_werks.
  set parameter id 'WRK' field v_werks.

  clear v_lgort.
  set parameter id 'LAG' field v_lgort.

  clear v_matnr.
  set parameter id 'MAT' field v_matnr.

  clear v_ekgrp.
  set parameter id 'EKG' field v_ekgrp.

  clear v_matkl.
  set parameter id 'MKL' field v_matkl.

  clear v_waers.
  set parameter id 'FWS' field v_waers.

  clear v_knttp.
  set parameter id 'KNT' field v_knttp.

  clear v_sakto.
  set parameter id 'SAK' field v_sakto.

  clear v_kostl.
  set parameter id 'KOS' field v_kostl.

  clear v_aufnr.
  set parameter id 'ANR' field v_aufnr.

  clear v_nplnr.
  set parameter id 'NET' field v_nplnr.

  clear v_ekorg.
  set parameter id 'EKO' field v_ekorg.

  clear v_infnr.
  set parameter id 'INF' field v_infnr.

  clear v_vornr.
  set parameter id 'VGN' field v_vornr.

endform.                    " CLEAR_SAP_MEMORY
*&---------------------------------------------------------------------*
*&      Form  LOAD_tvarvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form load_tvarvc .

  select single
         low into tvarvc-low
         from tvarvc
         where name = 'ZMM_EAST_WEST_IDENTIFIER'
           and type = 'P'
           and numb = '0000'.
  if sy-subrc <> 0.
    write:/ 'MISSING tvarvc ENTRY FOR: ZMM_EAST_WEST_IDENTIFIER'.
    stop.
  endif.

endform.                    " LOAD_tvarvc
*&---------------------------------------------------------------------*
*&      Form  CHECK_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form check_status .

  clear tbl_jest.
  read table tbl_jest with key objnr = tbl_prps-objnr
                               binary search.
  if sy-subrc = 0.
    case tbl_jest-stat.
      when 'I0001'.
        perform input_error.
        write:/28 '**ERROR** WBS element not yet released: ', tbl_converted-poski.
      when 'I0002'.
      when 'I0013'.
        perform input_error.
        write:/28 '**ERROR** WBS element is deleted: ', tbl_converted-poski.
      when 'I0045'.
        perform input_error.
        write:/28 '**ERROR** WBS element technically complete: ', tbl_converted-poski.
      when 'I0046'.
        perform input_error.
        write:/28 '**ERROR** WBS elment closed: ', tbl_converted-poski.
      when 'I0076'.
        perform input_error.
        write:/28 '**ERROR** WBS element is deleted: ', tbl_converted-poski.
    endcase.
  endif.

endform.                    " CHECK_STATUS
