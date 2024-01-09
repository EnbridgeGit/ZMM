FUNCTION z_mm_idoc_input_zarbinv.                           "D30K916283
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT_METHOD) LIKE  BDWFAP_PAR-INPUTMETHD
*"     VALUE(MASS_PROCESSING) LIKE  BDWFAP_PAR-MASS_PROC
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) LIKE  BDWF_PARAM-RESULT
*"     VALUE(APPLICATION_VARIABLE) LIKE  BDWF_PARAM-APPL_VAR
*"     VALUE(IN_UPDATE_TASK) LIKE  BDWFAP_PAR-UPDATETASK
*"     VALUE(CALL_TRANSACTION_DONE) LIKE  BDWFAP_PAR-CALLTRANS
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"  EXCEPTIONS
*"      WRONG_FUNCTION_CALLED
*"----------------------------------------------------------------------

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Function:  Z_MM_IDOC_INPUT_ZARBINV                                  *
*  Author:    John Hartung                                             *
*  Date:      February 25, 2011                                        *
*  Track #:   TR872 Release 2                                          *
*                                                                      *
*  Description:                                                        *
*     - MM IDOC Input ARIBA Invoice - create Invoice via BAPI          *
*                                                                      *
*     This function module "Z_MM_IDOC_INPUT_ZARBINV" was copied from   *
*     generated function module "ZIDOC_INPUT_ZINVCRT".  After this     *
*     function module was created, it was enhanced with additional     *
*     requirements.                                                    *
*                                                                      *
*     If SAP requires that the original function be regenerated, then  *
*     this function may require changes.  An analysis/comparison will  *
*     need to be performed between the newly generated function and    *
*     this function to determine the necessary changes.                *
*                                                                      *
*     The original function was created via transaction "BDBG"         *
*     (Generate ALE Interface for BAPI) for Object "BUS2081" and       *
*     Method "CREATEFROMDATA".                                         *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 02/25/11 0872  JRHARTU  D30K916283 - Initial program development     *
* 06/23/11 0872  BTBOUNDY D30K9##### - Add ZARIST01 IDOC sending       *
* 18/06/12 QR137 SAHMAD   C11K925175 - Split Accounting PO Error       *
* 22/03/12 28770 PANUSURI D30K921700 - Populate Item Text              *
* 06/03/14 60131 PANUSURI D30K923166 - Service PO with Split is Parked *
*                         as Complete.Material PO with Split is Posted.*
*----------------------------------------------------------------------*
* 02/04/15 78045 SKAPSE   D30K925189 - Holdback functionality for Ariba*
*                                      Invoices.                       *
* 09/06/16 1125  PANUSURI D30K926920 - If Invoice doesn't exist in ECC,*
*                         a 'RECEIVED' status sent back to Ariba.      *
* 05/03/19 CHG0132705 AKMADASU C11K935251 Update with holding tax      *
*                                         in invoice- 04               *
************************************************************************


  DATA:
      z1bp_incinv_create_header
                         LIKE z1bp_incinv_create_header,
      z1bp_incinv_addchrgs_hdr                              "D30K916283
                         LIKE z1bp_incinv_addchrgs_hdr,     "D30K916283
      z1bp_incinv_create_addressd
                         LIKE z1bp_incinv_create_addressd,
      z1bp_incinv_create_item
                         LIKE z1bp_incinv_create_item,
      z1bp_incinv_addchrgs_item                             "D30K916283
                         LIKE z1bp_incinv_addchrgs_item,    "D30K916283
      z1bp_incinv_create_tax_item                           "D30K916283
                         LIKE z1bp_incinv_create_tax_item,  "D30K916283
      z1bp_e1edpt1                                          "D30K916283
                         LIKE z1bp_e1edpt1,                 "D30K916283
      z1bp_e1edpt2                                          "D30K916283
                         LIKE z1bp_e1edpt2,                 "D30K916283
      z1bp_incinv_create_account
                         LIKE z1bp_incinv_create_account,
      z1bp_incinv_create_gl_accou
                         LIKE z1bp_incinv_create_gl_accou,
      z1bp_incinv_create_material
                         LIKE z1bp_incinv_create_material,
      z1bp_incinv_create_tax
                         LIKE z1bp_incinv_create_tax,
      z1bp_incinv_create_withtax
                         LIKE z1bp_incinv_create_withtax,
      z1bp_incinv_create_vendorsp
                         LIKE z1bp_incinv_create_vendorsp,
      z1bp_e1edkt1                                          "D30K916283
                         LIKE z1bp_e1edkt1,                 "D30K916283
      z1bp_e1edkt2                                          "D30K916283
                         LIKE z1bp_e1edkt2,                 "D30K916283

      invoicedocnumber LIKE
        bapi_incinv_fld-inv_doc_no,
      fiscalyear LIKE
        bapi_incinv_fld-fisc_year,
      headerdata LIKE
        bapi_incinv_create_header,
      addressdata LIKE
        bapi_incinv_create_addressdata,

      itemdata LIKE bapi_incinv_create_item
                  OCCURS 0 WITH HEADER LINE,
      accountingdata LIKE bapi_incinv_create_account
                  OCCURS 0 WITH HEADER LINE,
      glaccountdata LIKE bapi_incinv_create_gl_account
                  OCCURS 0 WITH HEADER LINE,
      materialdata LIKE bapi_incinv_create_material
                  OCCURS 0 WITH HEADER LINE,
      taxdata LIKE bapi_incinv_create_tax
                  OCCURS 0 WITH HEADER LINE,
      withtaxdata LIKE bapi_incinv_create_withtax
                  OCCURS 0 WITH HEADER LINE,
      vendoritemsplitdata LIKE bapi_incinv_create_vendorsplit
                  OCCURS 0 WITH HEADER LINE,
      return LIKE bapiret2
                  OCCURS 0 WITH HEADER LINE,

      t_edidd  LIKE edidd OCCURS 0 WITH HEADER LINE,
      bapi_retn_info  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
**--Begin Of Changes CHG0132705 by AKMADASU
      TYPES : BEGIN OF ty_wyt3,
              LIFNR type LIFNR,
              LIFN2 type LIFN2,
              END OF ty_wyt3.
      data: lt_wyt3 TYPE TABLE OF ty_wyt3,
            lw_wyt3 type ty_wyt3.
**-- end Of Changes CHG0132705 by AKMADASU
  DATA: error_flag,
        bapi_idoc_status LIKE bdidocstat-status.

  TYPES: BEGIN OF ty_afvc,
          aufpl TYPE co_aufpl,
          aplzl TYPE co_aplzl,
          vornr TYPE vornr,
          END OF ty_afvc.

  DATA: lt_afvc TYPE STANDARD TABLE OF ty_afvc,
        lwa_afvc TYPE ty_afvc.

*BTBOUNDY Added D30K9#####
  DATA:  lv_im_vnd_no        LIKE ekko-lifnr,
         lv_im_arib_com_sup  LIKE lfa1-emnfr,
         lv_im_sup_inv_no    TYPE xblnr,
         lv_awkey            LIKE bkpf-awkey,
         ls_bkpf             TYPE bkpf,
         lv_zlspr            LIKE bseg-zlspr,
         lv_vrtkz            TYPE ekpo-vrtkz,
         lv_mwskz            TYPE mwskz, "(+)PANUSURI Ticket SDP64418
         lv_meins            TYPE bstme, "(+)PANUSURI Ticket SDP64418
         lv_menge            TYPE bstmg,
         lv_netwr            TYPE bwert,
         lv_per_total        TYPE i,
         lv_split_acct,
         lv_lifnr            TYPE ekko-lifnr,
         lv_po_number        TYPE ekko-ebeln,
**-- START OF CHANGES BY AKMADASU
         lv_ekko_vnd_wth          LIKE ekko-lifnr,
         lv_wyt3_vnd_wth          LIKE wyt3-lifn2,
         lv_lfbw_vnd_wth          LIKE wyt3-lifn2,
         lv_lfbw_witht_wth        LIKE lfbw-witht,
         lv_lfbw_wt_withcd_wth    LIKE lfbw-wt_withcd,
**-- END OF CHANGES BY AKMADASU
*BTBOUNDY D30K9#####
*Ticket 22498 ADD
         lv_zwels     TYPE lfb1-zwels,
         lv_bukr      TYPE ekpo-bukrs,
         lv_lifn2     TYPE ekko-lifnr.

*BOI by PANUSURI ticket 60131
  DATA: lv_srv             TYPE c,
        lv_inv_status      TYPE c,
        lv_pstyp           TYPE pstyp,
        lv_inv_qty         TYPE bstmg,
        lv_line            TYPE sy-tfill,
        lv_lines           TYPE sy-tfill,
        lv_inv_totamt      TYPE bwert,
        lv_inv_amount      TYPE bwert,
        lwa_itemdata       LIKE LINE OF itemdata,
        lwa_accountingdata LIKE LINE OF accountingdata,
        lt_ekkn            TYPE STANDARD TABLE OF ekkn,
        lwa_ekkn           TYPE ekkn,
*EOI by PANUSURI ticket 60131
*BOI SKAPSE Ticket 78045
        lt_zvar            TYPE STANDARD TABLE OF zvar,
        lwa_zvar           TYPE zvar,
        lv_zterm           TYPE dzterm.

  CONSTANTS: lc_programm(23)     TYPE c VALUE 'Z_MM_IDOC_INPUT_ZARBINV',
             lc_hold_payterm(12) TYPE c VALUE 'HOLD_PAYTERM'.
*EOI SKAPSE Ticket 78045


  in_update_task = 'X'.
  CLEAR call_transaction_done.
* check if the function is called correctly                            *
  READ TABLE idoc_contrl INDEX 1.
  IF sy-subrc <> 0.
    EXIT.
  ELSEIF idoc_contrl-mestyp <> 'ZARBINV'.                   "D30K916283
    RAISE wrong_function_called.
  ENDIF.

* go through all IDocs                                                 *
  LOOP AT idoc_contrl.
*   select segments belonging to one IDoc                              *
    REFRESH t_edidd.
    LOOP AT idoc_data WHERE docnum = idoc_contrl-docnum.
      APPEND idoc_data TO t_edidd.
    ENDLOOP.

* Begin changes - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

    CLEAR    gt_ekpo_key[].
    CLEAR    gt_ekpo_ekko[].
    CLEAR    gt_text_lines_header[].
    CLEAR    gt_text_lines_items[].
    CLEAR    gt_text_lines_addchrgs[].
    CLEAR    gt_text_lines_errors[].

    CLEAR    gv_bukrs.
    CLEAR    gv_waers.
    CLEAR    gv_kalsm.
    CLEAR    gv_ebeln.
    CLEAR    gv_mwskz.
    CLEAR    gv_ref_doc_no.
    CLEAR    gv_gross_amount.
    CLEAR    gv_amount_addchrgs.
    CLEAR    gv_amount_addchrgs_c.
*{   INSERT         S01K900044                                        1
    CLEAR    gv_amount_addchrgs_tol.                        "S01K900044
*}   INSERT
    CLEAR    gv_tol_amt.
    CLEAR    gv_tol_rate.
    CLEAR    gv_tdname_miro.
    CLEAR    gv_tdspras_miro.
    CLEAR    gv_tdfuser_miro.
    CLEAR    gv_subrc_bapi.
    CLEAR    gv_subrc_conv.
    CLEAR    gv_flag_post.
    CLEAR    gv_flag_park.
    CLEAR    gv_segnam_error.
    CLEAR    gs_flags_invalid.
    CLEAR    lv_split_acct.
    CLEAR    lv_srv.        "(+)PANUSURI ticket 60131
    CLEAR    lv_inv_status. "(+)PANUSURI ticket 60131
* Retrieve the company code, currency key, and tolerance values

    PERFORM  f_retrieve_codes      TABLES   t_edidd.

* Total the additional header and line item charges

    PERFORM  f_pre_process_idoc    TABLES   t_edidd
                                            gt_text_lines_addchrgs
                                   CHANGING gv_gross_amount
                                            gv_amount_addchrgs
*{   REPLACE        S01K900044                                        2
*\                                            gv_amount_addchrgs_c.
                                            gv_amount_addchrgs_c
                                            gv_amount_addchrgs_tol.
*}   REPLACE

* Perform the delivery costs tolerance check

*{   REPLACE        S01K900044                                        3
*\    PERFORM  f_check_del_costs_tol    USING gv_gross_amount
*\                                            gv_amount_addchrgs.
    PERFORM  f_check_del_costs_tol    USING gv_gross_amount
                                            gv_amount_addchrgs_tol.
*}   REPLACE

* End changes   - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

*   through all segments of this IDoc                                  *
    CLEAR error_flag.
    REFRESH bapi_retn_info.
    CATCH SYSTEM-EXCEPTIONS conversion_errors = 1.
      LOOP AT t_edidd INTO idoc_data.

        IF ( gv_subrc_conv EQ 1 ).                          "D30K916283
          EXIT. "conversion error                           "D30K916283
        ENDIF.                                              "D30K916283

        CASE idoc_data-segnam.

          WHEN 'Z1BP_INCINV_CREATE_HEADER'.

            z1bp_incinv_create_header = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_header
               TO headerdata.                               "#EC ENHOK

*           BOI PANUSURI Ticket 28770
            IF z1bp_incinv_create_header-po_ref_no IS NOT INITIAL.
              headerdata-item_text = z1bp_incinv_create_header-po_ref_no.
*             BOI SKAPSE Ticket 78045
              CLEAR lv_zterm.
              SELECT SINGLE zterm
                 FROM ekko
                 INTO lv_zterm
                 WHERE ebeln EQ z1bp_incinv_create_header-po_ref_no.
              IF sy-subrc = 0.
                SELECT *
                  FROM zvar
                  INTO TABLE lt_zvar
                  WHERE programm = lc_programm
                  AND   varname  = lc_hold_payterm.
                IF sy-subrc = 0.
                  READ TABLE lt_zvar INTO lwa_zvar WITH KEY value1 = lv_zterm.
                  IF sy-subrc = 0.
                    headerdata-doc_type = 'ZN'.
                  ELSE.
                    headerdata-doc_type = 'ZR'.
                  ENDIF.
                ELSE.
*                  headerdata-doc_type = 'ZR'.
                ENDIF.
              ENDIF.
*             EOI SKAPSE Ticket 78045

            ENDIF.
*           EOI PANUSURI Ticket 28770
            IF z1bp_incinv_create_header-doc_date
               IS INITIAL.
              CLEAR headerdata-doc_date.
            ENDIF.
            IF z1bp_incinv_create_header-pstng_date
               IS INITIAL.
              CLEAR headerdata-pstng_date.
            ENDIF.
            IF z1bp_incinv_create_header-bline_date
               IS INITIAL.
              CLEAR headerdata-bline_date.
            ENDIF.
            IF z1bp_incinv_create_header-inv_rec_date
               IS INITIAL.
              CLEAR headerdata-inv_rec_date.
            ENDIF.
            IF z1bp_incinv_create_header-planning_date
               IS INITIAL.
              CLEAR headerdata-planning_date.
            ENDIF.
*Ticket 22498: copy from system DEC: BOI Ajonnalagedd 14Aug12
            CLEAR : lv_zwels, lv_bukr, lv_lifn2.
            IF z1bp_incinv_create_header-pymt_meth IS INITIAL.
              SELECT SINGLE bukrs
                 FROM ekko
                 INTO lv_bukr
                 WHERE ebeln EQ z1bp_incinv_create_header-po_ref_no.
              IF sy-subrc = 0.
                SELECT SINGLE lifnr
                   FROM ekko
                   INTO lv_lifn2
                   WHERE ebeln EQ z1bp_incinv_create_header-po_ref_no.
                IF sy-subrc = 0.
                  SELECT SINGLE zwels
                     FROM lfb1
                     INTO lv_zwels
                     WHERE lifnr EQ lv_lifn2
                       AND bukrs EQ lv_bukr.
                  IF sy-subrc = 0.
                    headerdata-pymt_meth = lv_zwels+0(1).
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              headerdata-pymt_meth = z1bp_incinv_create_header-pymt_meth.
            ENDIF.   "z1bp_incinv_create_header-pymt_meth IS INITIAL.
*Ticket 22498: copy from system DEC: EOI Ajonnalagedd 14Aug12.


* Begin changes - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

            CLEAR                      gv_levl.
            MOVE     'HDR'(011)     TO gv_levl. "set for header texts

            CLEAR                      gv_ref_doc_no.
            MOVE     z1bp_incinv_create_header-ref_doc_no
                                    TO gv_ref_doc_no.

            IF     ( gv_bukrs              IS NOT INITIAL ).
              CLEAR                    headerdata-comp_code.
              MOVE   gv_bukrs       TO headerdata-comp_code.
            ENDIF.

            IF     ( gv_waers             IS NOT INITIAL ).
              CLEAR                    headerdata-currency.
              MOVE   gv_waers       TO headerdata-currency.
            ENDIF.

            IF     ( gv_amount_addchrgs_c  IS NOT INITIAL ).
              CLEAR                    headerdata-del_costs.
              MOVE   gv_amount_addchrgs_c
                                    TO headerdata-del_costs.
            ENDIF.

* End changes   - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

          WHEN 'Z1BP_INCINV_CREATE_ADDRESSD'.

            z1bp_incinv_create_addressd = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_addressd
               TO addressdata.                              "#EC ENHOK


          WHEN 'Z1BP_INCINV_CREATE_ITEM'.

            z1bp_incinv_create_item = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_item
               TO itemdata.                                 "#EC ENHOK

            APPEND itemdata.

* Begin changes - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

            CLEAR                      gv_item_inv.
            MOVE     z1bp_incinv_create_item-invoice_doc_item
                                    TO gv_item_inv.
            CLEAR                      gv_item_po. "SPACE"
            CLEAR                      gv_levl. "set for item texts

            PERFORM  f_format_level
                              USING    gv_item_inv
                                       gv_item_po
                              CHANGING gv_levl
                                       gv_subrc_conv.

            IF ( gv_subrc_conv EQ 1 ).
              CLEAR                    gv_segnam_error.
              MOVE   idoc_data-segnam
                                    TO gv_segnam_error.
              EXIT. "conversion error
            ENDIF.

          WHEN 'Z1BP_INCINV_CREATE_TAX_ITEM'.

            z1bp_incinv_create_tax_item = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_tax_item
               TO taxdata.                                  "#EC ENHOK

*           APPEND TAXDATA. "do not create this segment     "D30K916283

* End changes   - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

          WHEN 'Z1BP_INCINV_CREATE_ACCOUNT'.

            z1bp_incinv_create_account = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_account
               TO accountingdata.                           "#EC ENHOK

            IF z1bp_incinv_create_account-ref_date
               IS INITIAL.
              CLEAR accountingdata-ref_date.
            ENDIF.
            APPEND accountingdata.

          WHEN 'Z1BP_INCINV_CREATE_GL_ACCOU'.

            z1bp_incinv_create_gl_accou = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_gl_accou
               TO glaccountdata.                            "#EC ENHOK

            IF z1bp_incinv_create_gl_accou-ref_date
               IS INITIAL.
              CLEAR glaccountdata-ref_date.
            ENDIF.
            APPEND glaccountdata.

          WHEN 'Z1BP_INCINV_CREATE_MATERIAL'.

            z1bp_incinv_create_material = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_material
               TO materialdata.                             "#EC ENHOK

            APPEND materialdata.

          WHEN 'Z1BP_INCINV_CREATE_TAX'.

            z1bp_incinv_create_tax = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_tax
               TO taxdata.                                  "#EC ENHOK

*           APPEND TAXDATA. "do not create this segment     "D30K916283

          WHEN 'Z1BP_INCINV_CREATE_WITHTAX'.

            z1bp_incinv_create_withtax = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_withtax
               TO withtaxdata.                              "#EC ENHOK

*           APPEND WITHTAXDATA. "do not create this segment "D30K916283

          WHEN 'Z1BP_INCINV_CREATE_VENDORSP'.

            z1bp_incinv_create_vendorsp = idoc_data-sdata.
            MOVE-CORRESPONDING z1bp_incinv_create_vendorsp
               TO vendoritemsplitdata.                      "#EC ENHOK

            APPEND vendoritemsplitdata.

* Begin changes - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

          WHEN 'Z1BP_E1EDKT1'.

            z1bp_e1edkt1 = idoc_data-sdata.

            PERFORM  f_set_text_language
                              USING    z1bp_e1edkt1-tsspras
                              CHANGING gv_tdspras_miro
                                       gv_subrc_conv.

            IF ( gv_subrc_conv EQ 1 ).
              CLEAR                    gv_segnam_error.
              MOVE   idoc_data-segnam
                                    TO gv_segnam_error.
              EXIT. "conversion error
            ENDIF.

          WHEN 'Z1BP_E1EDKT2'.

            z1bp_e1edkt2 = idoc_data-sdata.

            CLEAR                      gs_text_lines_header.
            MOVE     'HDR'(011)     TO gs_text_lines_header-levl.
            MOVE     z1bp_e1edkt2-tdformat
                                    TO gs_text_lines_header-tdformat.
            MOVE     z1bp_e1edkt2-tdline
                                    TO gs_text_lines_header-tdline.
            APPEND   gs_text_lines_header
                                    TO gt_text_lines_header.

          WHEN 'Z1BP_E1EDPT1'.

            z1bp_e1edpt1 = idoc_data-sdata.

            PERFORM  f_set_text_language
                              USING    z1bp_e1edpt1-tsspras
                              CHANGING gv_tdspras_miro
                                       gv_subrc_conv.

            IF ( gv_subrc_conv EQ 1 ).
              CLEAR                    gv_segnam_error.
              MOVE   idoc_data-segnam
                                    TO gv_segnam_error.
              EXIT. "conversion error
            ENDIF.

          WHEN 'Z1BP_E1EDPT2'.

            z1bp_e1edpt2 = idoc_data-sdata.

            CLEAR                      gs_text_lines_items.
            MOVE     gv_levl        TO gs_text_lines_items-levl.
            MOVE     z1bp_e1edpt2-tdformat
                                    TO gs_text_lines_items-tdformat.
            MOVE     z1bp_e1edpt2-tdline
                                    TO gs_text_lines_items-tdline.
            APPEND   gs_text_lines_items
                                    TO gt_text_lines_items.

* End changes   - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

        ENDCASE.

      ENDLOOP.
    ENDCATCH.
*   IF SY-SUBRC = 1.                                        "D30K916283
    IF ( ( sy-subrc EQ 1 ) OR ( gv_subrc_conv EQ 1 ) ).     "D30K916283
      IF ( sy-subrc EQ 1 ).                                 "D30K916283
        CLEAR                          gv_segnam_error.     "D30K916283
        MOVE     idoc_data-segnam   TO gv_segnam_error.     "D30K916283
      ENDIF.                                                "D30K916283
*     write IDoc status-record as error and continue                   *
      CLEAR bapi_retn_info.
      bapi_retn_info-type   = 'E'.
      bapi_retn_info-id     = 'B1'.
      bapi_retn_info-number = '527'.
*     BAPI_RETN_INFO-MESSAGE_V1 = IDOC_DATA-SEGNAM.         "D30K916283
      bapi_retn_info-message_v1 = gv_segnam_error.          "D30K916283
      bapi_idoc_status      = '51'.
      PERFORM zidoc_status_zinvcrt
              TABLES t_edidd
                     idoc_status
                     return_variables
               USING idoc_contrl
                     bapi_retn_info
                     bapi_idoc_status
                     workflow_result.
      CONTINUE.
    ENDIF.
*Split Accounting Changes
*BOC by PANUSURI ticket 60131
*if any record in idoc has distribution indicator for account
* assignment then parked the invoice
*    LOOP AT itemdata.
*      CLEAR lv_vrtkz.
*      SELECT SINGLE vrtkz FROM ekpo INTO lv_vrtkz
*        WHERE ebeln = itemdata-po_number
*          AND ebelp = itemdata-po_item.
*      IF lv_vrtkz IS NOT INITIAL.
*        gs_flags_invalid = gc_x.
*        lv_split_acct = 'X'.  "Split accounting Flag
*      ENDIF.
*    ENDLOOP.
*EOC by PANUSURI ticket 60131
*BOI by PANUSURI ticket 60131
    IF accountingdata[] IS INITIAL.
      LOOP AT itemdata INTO lwa_itemdata.
        CLEAR: lv_pstyp,
          lv_vrtkz,
          lv_mwskz, "(+)PANUSURI Ticket SDP64418
          lv_meins, "(+)PANUSURI Ticket SDP64418
          lv_menge,
          lv_netwr,
          lv_srv.
*      SELECT SINGLE pstyp vrtkz FROM ekpo INTO (lv_pstyp, lv_vrtkz)  "(-)PANUSURI Ticket SDP64418
        SELECT SINGLE menge meins netwr mwskz pstyp vrtkz FROM ekpo
          INTO (lv_menge, lv_meins, lv_netwr, lv_mwskz, lv_pstyp, lv_vrtkz)    "(+)PANUSURI Ticket SDP64418
          WHERE ebeln = lwa_itemdata-po_number
          AND   ebelp = lwa_itemdata-po_item.
*     Service PO
        IF lv_pstyp = '1' OR lv_pstyp = '9'.
          lv_srv = 'X'.
        ENDIF.
*     Distribution indicator for Mulitple account assignment
*        IF lv_vrtkz IS NOT INITIAL. "(-)PANUSURI
*        IF accountingdata[] IS INITIAL.
        SELECT * FROM ekkn INTO TABLE lt_ekkn
          WHERE ebeln = lwa_itemdata-po_number
          AND   ebelp = lwa_itemdata-po_item
          AND   loekz = ' '.
        IF lt_ekkn IS NOT INITIAL.
          SELECT aufpl aplzl vornr
                 FROM afvc
                 INTO TABLE lt_afvc
                 FOR ALL ENTRIES IN lt_ekkn
                 WHERE aufpl = lt_ekkn-aufpl
                 AND   aplzl = lt_ekkn-aplzl.

          CLEAR: lv_per_total,
                 lv_lines, lv_line.       "Balaji dt 8/15/2014 SDP 60131
          LOOP AT lt_ekkn INTO lwa_ekkn.
            lv_per_total = lv_per_total + lwa_ekkn-vproz.
          ENDLOOP.
          IF lv_per_total EQ 100.
            DELETE lt_ekkn WHERE vproz EQ 0.
          ELSE.
            DELETE lt_ekkn WHERE menge EQ 0.
          ENDIF.
          DESCRIBE TABLE lt_ekkn LINES lv_lines.    "Balaji dt 8/15/2014 SDP 60131
          CLEAR: lv_inv_totamt,
                 lv_line.               "Balaji dt 8/15/2014 SDP 60131
          LOOP AT lt_ekkn INTO lwa_ekkn.
            CLEAR: lwa_accountingdata, lv_inv_qty, lv_inv_amount.
            lv_line = lv_line + 1.      "Balaji dt 8/15/2014 SDP 60131
            IF lv_per_total = '100'.
              IF lwa_itemdata-item_amount IS NOT INITIAL.
                CLEAR lv_inv_amount.
                lv_inv_amount = ( lwa_itemdata-item_amount * ( lwa_ekkn-vproz / 100 ) ).
                lwa_accountingdata-item_amount = lv_inv_amount.
              ENDIF.
              IF lv_srv IS INITIAL.
                IF lwa_itemdata-quantity IS NOT INITIAL.
                  CLEAR lv_inv_qty.
                  lv_inv_qty = ( lwa_itemdata-quantity * ( lwa_ekkn-vproz / 100 ) ).
                  lwa_accountingdata-quantity = lv_inv_qty.
                ENDIF.
*                    IF lwa_itemdata-po_unit IS NOT INITIAL. "(+)PANUSURI Ticket SDP64418
*                      lwa_accountingdata-po_unit = lwa_itemdata-po_unit.
** BOI by PANUSURI Ticket SDP64418
*                    ELSEIF lv_meins IS NOT INITIAL.
*                      lwa_accountingdata-po_unit = lv_meins.
*                    ENDIF.
** EOI by PANUSURI Ticket SDP64418
              ENDIF.
            ELSE. "Quantity Based Split
* BOI by panusuri ticket sdp64418
              IF lv_srv IS INITIAL.
                IF lwa_itemdata-quantity IS NOT INITIAL AND lwa_ekkn-menge IS NOT INITIAL.
                  CLEAR lv_inv_qty.
                  lv_inv_qty = ( lwa_itemdata-quantity / lv_menge ) *  lwa_ekkn-menge .
                  lwa_accountingdata-quantity = lv_inv_qty.
                ENDIF.
              ELSE.
                IF lwa_itemdata-quantity IS INITIAL.
                  lwa_itemdata-quantity = 1.
                ENDIF.
              ENDIF.
*- Begin of Change by Balaji for Material Invoice Issue Diff Amt
*              IF lwa_itemdata-item_amount IS NOT INITIAL AND lwa_itemdata-quantity IS NOT INITIAL.
*                CLEAR lv_inv_amount.
*                lv_inv_amount = ( lwa_itemdata-quantity / lv_menge ) * lwa_itemdata-item_amount.
*                lwa_accountingdata-item_amount = lv_inv_amount.
*              ENDIF.
              CLEAR lv_inv_amount.
              lv_inv_amount = ( lwa_ekkn-menge / lv_menge ) * lwa_itemdata-item_amount.
              lwa_accountingdata-item_amount = lv_inv_amount.
*- End of Change by Balaji for Material Invoice Issue Diff Amt
            ENDIF.
*- Begin of Change by Balaji dt 8/15/2014 SDP 60131
            IF lv_line EQ lv_lines.
              lwa_accountingdata-item_amount = lwa_itemdata-item_amount - lv_inv_totamt.
            ENDIF.
            lv_inv_totamt = lv_inv_totamt + lv_inv_amount.
*- End of Change by Balaji dt 8/15/2014 SDP 60131
            IF lv_srv IS INITIAL.
              IF lwa_itemdata-po_unit IS NOT INITIAL.
                lwa_accountingdata-po_unit = lwa_itemdata-po_unit.
              ELSEIF lv_meins IS NOT INITIAL.
                lwa_accountingdata-po_unit = lv_meins.
              ENDIF.
            ENDIF.
* EOI by PANUSURI Ticket SDP64418
            lwa_accountingdata-invoice_doc_item = lwa_itemdata-invoice_doc_item.
            lwa_accountingdata-serial_no = lwa_ekkn-zekkn.
            lwa_accountingdata-gl_account = lwa_ekkn-sakto.
            lwa_accountingdata-costcenter = lwa_ekkn-kostl.
            lwa_accountingdata-wbs_elem = lwa_ekkn-ps_psp_pnr.
            lwa_accountingdata-profit_ctr = lwa_ekkn-prctr.
            lwa_accountingdata-co_area = lwa_ekkn-kokrs.
            lwa_accountingdata-ref_date = lwa_ekkn-dabrz.
* BOI by PANUSURI Ticket SDP64418
            lwa_accountingdata-orderid = lwa_ekkn-aufnr.
            IF taxdata-tax_code IS NOT INITIAL.
              lwa_accountingdata-tax_code = taxdata-tax_code.
            ELSEIF lv_mwskz IS NOT INITIAL.
              lwa_accountingdata-tax_code = lv_mwskz.
            ENDIF.
            lwa_accountingdata-network = lwa_ekkn-nplnr.
            CLEAR lwa_afvc.
            READ TABLE lt_afvc INTO lwa_afvc WITH KEY aufpl = lwa_ekkn-aufpl
                                                      aplzl = lwa_ekkn-aplzl.
            IF sy-subrc = 0.
              lwa_accountingdata-activity = lwa_afvc-vornr.
            ENDIF.
* EOI by PANUSURI Ticket SDP64418
            IF lv_inv_amount IS NOT INITIAL.
              IF lv_srv IS INITIAL AND lv_inv_qty IS INITIAL.
                CONTINUE.
              ENDIF.
              APPEND lwa_accountingdata TO accountingdata.
            ENDIF.
            CLEAR lwa_accountingdata.
            CLEAR lwa_ekkn.
          ENDLOOP.
          REFRESH lt_afvc.
        ENDIF.
        REFRESH lt_ekkn.
*        ENDIF.
*        ENDIF. "(-)PANUSURI
        CLEAR lwa_itemdata.
      ENDLOOP.
    ENDIF.
*EOI by PANUSURI ticket 60131
*End of Split Accounting Changes.

*eject
* Begin changes - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

*   call BAPI-function in this system                                  *

* If there are no validation errors, then post the invoice; else, park
    IF ( gs_flags_invalid IS INITIAL ).
      gv_flag_post = gc_x.
    ELSE.
      gv_flag_park = gc_x.
    ENDIF.

    IF ( ( gv_flag_post  IS NOT INITIAL ) AND
         ( gv_subrc_bapi     IS INITIAL )     ).

*BOC by PANUSURI ticket 60131
*      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
*        EXPORTING
*          headerdata          = headerdata
*          addressdata         = addressdata
*        IMPORTING
*          invoicedocnumber    = invoicedocnumber
*          fiscalyear          = fiscalyear
*        TABLES
*          itemdata            = itemdata
*          accountingdata      = accountingdata
*          glaccountdata       = glaccountdata
*          materialdata        = materialdata
*          taxdata             = taxdata
*          withtaxdata         = withtaxdata
*          vendoritemsplitdata = vendoritemsplitdata
*          return              = return
*        EXCEPTIONS
*          OTHERS              = 1.
*EOC by PANUSURI ticket 60131
**-- Begin Of Changes CHG0132705 by AKMADASU
clear: lt_wyt3[],lv_ekko_vnd_wth.
      IF withtaxdata IS INITIAL.
        SELECT SINGLE lifnr
          FROM ekko
          INTO lv_ekko_vnd_wth
          WHERE ebeln EQ headerdata-po_ref_no AND bukrs EQ headerdata-comp_code.
        IF sy-subrc EQ 0.
          SELECT LIFNR lifn2 from wyt3 into TABLE lt_wyt3 where lifnr = lv_ekko_vnd_wth.
          IF sy-subrc is INITIAL.
            LOOP AT lt_wyt3 into lw_wyt3.
              IF lw_wyt3-lifn2(1) EQ '2'.
                lv_lfbw_vnd_wth = lw_wyt3-lifn2.
                exit.
              ENDIF.
              clear:lw_wyt3.
            ENDLOOP.
          ENDIF.
          IF lv_lfbw_vnd_wth IS NOT INITIAL.
            SELECT SINGLE witht wt_withcd FROM lfbw
                      INTO (lv_lfbw_witht_wth,lv_lfbw_wt_withcd_wth)
                      WHERE lifnr EQ lv_lfbw_vnd_wth AND bukrs EQ headerdata-comp_code AND wt_subjct = 'X'.
            IF sy-subrc EQ 0.
              z1bp_incinv_create_withtax-SPLIT_KEY   = '000001'.
              z1bp_incinv_create_withtax-wi_tax_type = lv_lfbw_witht_wth.
              z1bp_incinv_create_withtax-wi_tax_code = lv_lfbw_wt_withcd_wth.
              z1bp_incinv_create_withtax-wi_tax_base = headerdata-gross_amount.
              MOVE-CORRESPONDING z1bp_incinv_create_withtax
                 TO withtaxdata.
              APPEND withtaxdata.
              clear:withtaxdata,lv_lfbw_witht_wth,lv_lfbw_wt_withcd_wth.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
**-- End Of Changes CHG0132705 by AKMADASU

*BOI by PANUSURI ticket 60131
      IF lv_srv = 'X'.
        lv_inv_status = 'B'. "Parked as complete
      ELSE.
        lv_inv_status = '5'. "Posted
      ENDIF.

      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE1'
        EXPORTING
          headerdata          = headerdata
          addressdata         = addressdata
          invoicestatus       = lv_inv_status
        IMPORTING
          invoicedocnumber    = invoicedocnumber
          fiscalyear          = fiscalyear
        TABLES
          itemdata            = itemdata
          accountingdata      = accountingdata
          glaccountdata       = glaccountdata
          materialdata        = materialdata
          taxdata             = taxdata
          withtaxdata         = withtaxdata
          vendoritemsplitdata = vendoritemsplitdata
          return              = return.
*EOI by PANUSURI ticket 60131

      gv_subrc_bapi = sy-subrc.

* Check if there was an invoice balance error
      CLEAR          return.
      READ     TABLE return WITH KEY type   = 'E'
                                     id     = 'M8'
                                     number = '534'.
      IF ( sy-subrc EQ 0 ).
        CLEAR                     gs_text_lines_errors.
        MOVE     'HDR'(011)    TO gs_text_lines_errors-levl.
        MOVE     return-message+0(132)
                               TO gs_text_lines_errors-tdline+0(132).
        INSERT   gs_text_lines_errors
                             INTO gt_text_lines_errors
                            INDEX 1.
        CLEAR:   return[],        return.
        CLEAR    gv_subrc_bapi.
        CLEAR    gv_flag_post.
        gv_flag_park = gc_x.
      ENDIF.

* Check if the reversal quantity was greater than quantity inv-to-date
      CLEAR          return.
      READ     TABLE return WITH KEY type   = 'E'
                                     id     = 'M8'
                                     number = '080'.
      IF ( sy-subrc EQ 0 ).

* Check if the document is a credit memo

        IF ( headerdata-invoice_ind IS INITIAL ).

          CLEAR                   gs_text_lines_errors.
          MOVE     'HDR'(011)  TO gs_text_lines_errors-levl.
          MOVE     return-message+0(132)
                               TO gs_text_lines_errors-tdline+0(132).
          INSERT   gs_text_lines_errors
                             INTO gt_text_lines_errors
                            INDEX 1.
          CLEAR:   return[],      return.
          CLEAR    gv_subrc_bapi.
          CLEAR    gv_flag_post.
          gv_flag_park = gc_x.

        ENDIF.

      ENDIF.

    ENDIF.

* If there are validation errors, then park invoice
    IF ( ( gv_flag_park  IS NOT INITIAL ) AND
         ( gv_subrc_bapi     IS INITIAL )     ).

*BOC by PANUSURI ticket 60131
* then keep only header data and parked document.
*      IF lv_split_acct IS NOT INITIAL.
*        CLEAR: lv_lifnr,
*               lv_po_number.
*        IF itemdata[] IS NOT INITIAL.
*          READ TABLE itemdata INDEX 1.
*          SELECT SINGLE lifnr INTO lv_lifnr FROM ekko
*          WHERE ebeln =  itemdata-po_number.
*          lv_po_number = itemdata-po_number.
*        ELSE.
*          SELECT SINGLE lifnr INTO lv_lifnr FROM ekko
*             WHERE ebeln =  headerdata-po_ref_no.
*          lv_po_number = headerdata-po_ref_no.
*        ENDIF.
*        headerdata-diff_inv  = lv_lifnr.
*        REFRESH :  itemdata,
*                   taxdata,
*                   accountingdata,
*                   glaccountdata,
*                   materialdata,
*                   taxdata,
*                   withtaxdata,
*                   vendoritemsplitdata.
*        CLEAR :    itemdata,
*                   taxdata,
*                   accountingdata,
*                   glaccountdata,
*                   materialdata,
*                   taxdata,
*                   withtaxdata,
*                   vendoritemsplitdata.
*      ENDIF. "end of split acct IF
*EOC by PANUSURI ticket 60131
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
        EXPORTING
          headerdata          = headerdata
          addressdata         = addressdata
        IMPORTING
          invoicedocnumber    = invoicedocnumber
          fiscalyear          = fiscalyear
        TABLES
          itemdata            = itemdata
          accountingdata      = accountingdata
          glaccountdata       = glaccountdata
          materialdata        = materialdata
          taxdata             = taxdata
          withtaxdata         = withtaxdata
          vendoritemsplitdata = vendoritemsplitdata
          return              = return
        EXCEPTIONS
          OTHERS              = 1.

      gv_subrc_bapi = sy-subrc.

    ENDIF.

* End changes   - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

*   IF SY-SUBRC <> 0.                                       "D30K916283
    IF ( gv_subrc_bapi NE 0 ).                              "D30K916283
*     write IDoc status-record as error                                *
      CLEAR bapi_retn_info.
      bapi_retn_info-type       = 'E'.
      bapi_retn_info-id         = sy-msgid.
      bapi_retn_info-number     = sy-msgno.
      bapi_retn_info-message_v1 = sy-msgv1.
      bapi_retn_info-message_v2 = sy-msgv2.
      bapi_retn_info-message_v3 = sy-msgv3.
      bapi_retn_info-message_v4 = sy-msgv4.
      bapi_idoc_status          = '51'.
      PERFORM zidoc_status_zinvcrt
              TABLES t_edidd
                     idoc_status
                     return_variables
               USING idoc_contrl
                     bapi_retn_info
                     bapi_idoc_status
                     workflow_result.
    ELSE.
      LOOP AT return.
        IF NOT return IS INITIAL.
          CLEAR bapi_retn_info.
          MOVE-CORRESPONDING return TO bapi_retn_info.      "#EC ENHOK
          IF return-type = 'A' OR return-type = 'E'.
            error_flag = 'X'.
          ENDIF.
          APPEND bapi_retn_info.
        ENDIF.
      ENDLOOP.

* Begin changes - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

* Attach the note to the invoice if the document was created
      IF ( ( error_flag           IS INITIAL ) AND
           ( invoicedocnumber IS NOT INITIAL ) AND
           ( fiscalyear       IS NOT INITIAL )     ).

        CLEAR                           gv_tdname_miro.
        CONCATENATE    invoicedocnumber fiscalyear
                                   INTO gv_tdname_miro.

        IF     ( gv_tdspras_miro     IS INITIAL ).
          MOVE   gc_spras_dflt       TO gv_tdspras_miro.
        ENDIF.

        CLEAR                           gv_tdfuser_miro.
        MOVE     idoc_contrl-sndprn  TO gv_tdfuser_miro.

        PERFORM  f_attach_note TABLES   return
                               USING    gc_tdobject_miro
                                        gv_tdname_miro
                                        gc_tdid_miro
                                        gv_tdspras_miro
                                        gc_tdtitle_miro
                                        gc_tdversion_miro
                                        gv_tdfuser_miro.

        LOOP AT  return.
          IF   ( return IS NOT INITIAL ).
            CLEAR                           bapi_retn_info.
            MOVE-CORRESPONDING return    TO bapi_retn_info.
            IF ( return-type EQ 'A' ) OR
               ( return-type EQ 'E' ).
              error_flag = 'X'.
            ENDIF.
            APPEND   bapi_retn_info.
          ENDIF.
        ENDLOOP.

*BOC by PANUSURI ticket 60131
*Message for Split account
*        IF lv_split_acct IS NOT INITIAL.
*          CLEAR          return.
*          CLEAR                     gs_text_lines_errors.
*          MOVE     'HDR'(011)         TO gs_text_lines_errors-levl.
*          CONCATENATE 'PO' lv_po_number
*         'has split acct, header data parked please complete Item details'
*                                   INTO gs_text_lines_errors-tdline+0(132)
*                                   SEPARATED BY space.
*          INSERT   gs_text_lines_errors
*                                    INTO gt_text_lines_errors
*                                   INDEX 1.
*          CLEAR:   return[],        return.
*          CLEAR    gv_subrc_bapi.
**       Update Note tab
*          PERFORM update_note_tab USING invoicedocnumber
*                                        fiscalyear
*                                        lv_po_number.
*        ENDIF.
*end of Message for Split Account
*EOC by PANUSURI ticket 60131
      ENDIF.

* End changes   - insert code   JRHARTUNG  02/25/11  TR0872  D30K916283

      LOOP AT bapi_retn_info.
*       write IDoc status-record                                       *
        IF error_flag IS INITIAL.
          bapi_idoc_status = '53'.
        ELSE.
          bapi_idoc_status = '51'.
          IF bapi_retn_info-type = 'S'.
            CONTINUE.
          ENDIF.
        ENDIF.
        PERFORM zidoc_status_zinvcrt
                TABLES t_edidd
                       idoc_status
                       return_variables
                 USING idoc_contrl
                       bapi_retn_info
                       bapi_idoc_status
                       workflow_result.
      ENDLOOP.
      IF sy-subrc <> 0.
*      'RETURN' is empty write idoc status-record as successful        *
        CLEAR bapi_retn_info.
        bapi_retn_info-type       = 'S'.
        bapi_retn_info-id         = 'B1'.
        bapi_retn_info-number     = '501'.
        bapi_retn_info-message_v1 = 'CREATEFROMDATA'.
        bapi_idoc_status          = '53'.
        PERFORM zidoc_status_zinvcrt
                TABLES t_edidd
                       idoc_status
                       return_variables
                 USING idoc_contrl
                       bapi_retn_info
                       bapi_idoc_status
                       workflow_result.
      ENDIF.
      IF error_flag IS INITIAL.
*       write linked object keys                                       *
        CLEAR return_variables.
        return_variables-wf_param = 'Appl_Objects'.
        READ TABLE return_variables WITH KEY wf_param = 'Appl_Objects'.
        MOVE invoicedocnumber
          TO return_variables-doc_number+00.
        IF sy-subrc <> 0.
          APPEND return_variables.
        ELSE.
          MODIFY return_variables INDEX sy-tabix.
        ENDIF.
        READ TABLE return_variables WITH KEY wf_param = 'Appl_Objects'.
        MOVE fiscalyear
          TO return_variables-doc_number+10.
        IF sy-subrc <> 0.
          APPEND return_variables.
        ELSE.
          MODIFY return_variables INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.



*BTBOUNDY added ZARBIST01 idoc sending. D30K9#####

    COMMIT WORK.

    SELECT SINGLE lifnr
      FROM ekko
      INTO lv_im_vnd_no
      WHERE ebeln = headerdata-po_ref_no
    .

    SELECT SINGLE emnfr
      FROM lfa1
      INTO lv_im_arib_com_sup
      WHERE lifnr = lv_im_vnd_no
    .

    lv_im_sup_inv_no = headerdata-ref_doc_no_long.

    CONCATENATE invoicedocnumber fiscalyear INTO lv_awkey.

    SELECT SINGLE bstat belnr gjahr
      INTO CORRESPONDING FIELDS OF ls_bkpf
      FROM bkpf
      WHERE awkey = lv_awkey.



    IF ls_bkpf-bstat = 'V'.

      CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
        EXPORTING
*         im_doc_typ                   = 'ZR'
          im_doc_typ                   = headerdata-doc_type  "'ZR' SKAPSE 78045 2/4/2015
          im_inv_doc_no                = invoicedocnumber
          im_vnd_no                    = lv_im_vnd_no
          im_arib_com_sup              = lv_im_arib_com_sup
          im_sup_inv_no                = lv_im_sup_inv_no
          im_inv_status                = 'RECEIVED'
          im_ztext                     = ''
          im_inv_amt                   = ''
          im_year                      = fiscalyear
          im_port                      = 'SONIC'
          im_partno                    = 'SONIC'
          im_parttype                  = 'LS'
        EXCEPTIONS
          error_idoc_control           = 1
          error_idoc_status            = 2
          error_idoc_data              = 3
          error_logical_system_unknown = 4
          error_other                  = 5
          OTHERS                       = 6.

    ELSEIF  ls_bkpf-bstat = ''.

      SELECT SINGLE zlspr
        FROM bseg
        INTO lv_zlspr
        WHERE belnr = ls_bkpf-belnr
          AND gjahr = ls_bkpf-gjahr
          AND buzei = '1'.
      IF sy-subrc EQ 0. "(+)PANUSURI Ticket ACR-1125
        IF lv_zlspr = ''.
          CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
            EXPORTING
*             im_doc_typ                   = 'ZR'
              im_doc_typ                   = headerdata-doc_type  "'ZR' SKAPSE 78045 2/4/2015
              im_inv_doc_no                = invoicedocnumber
              im_vnd_no                    = lv_im_vnd_no
              im_arib_com_sup              = lv_im_arib_com_sup
              im_sup_inv_no                = lv_im_sup_inv_no
              im_inv_status                = 'OPEN'
              im_ztext                     = ''
              im_inv_amt                   = ''
              im_year                      = fiscalyear
              im_port                      = 'SONIC'
              im_partno                    = 'SONIC'
              im_parttype                  = 'LS'
            EXCEPTIONS
              error_idoc_control           = 1
              error_idoc_status            = 2
              error_idoc_data              = 3
              error_logical_system_unknown = 4
              error_other                  = 5
              OTHERS                       = 6.
        ELSE.
          CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
            EXPORTING
*             im_doc_typ                   = 'ZR'
              im_doc_typ                   = headerdata-doc_type  "'ZR' SKAPSE 78045 2/4/2015
              im_inv_doc_no                = invoicedocnumber
              im_vnd_no                    = lv_im_vnd_no
              im_arib_com_sup              = lv_im_arib_com_sup
              im_sup_inv_no                = lv_im_sup_inv_no
              im_inv_status                = 'RECEIVED'
              im_ztext                     = ''
              im_inv_amt                   = ''
              im_year                      = fiscalyear
              im_port                      = 'SONIC'
              im_partno                    = 'SONIC'
              im_parttype                  = 'LS'
            EXCEPTIONS
              error_idoc_control           = 1
              error_idoc_status            = 2
              error_idoc_data              = 3
              error_logical_system_unknown = 4
              error_other                  = 5
              OTHERS                       = 6.
        ENDIF.
*     BOI by PANUSURI Ticket ACR-1125
      ELSE.
        CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
          EXPORTING
*           im_doc_typ                   = 'ZR'
            im_doc_typ                   = headerdata-doc_type  "'ZR' SKAPSE 78045 2/4/2015
            im_inv_doc_no                = invoicedocnumber
            im_vnd_no                    = lv_im_vnd_no
            im_arib_com_sup              = lv_im_arib_com_sup
            im_sup_inv_no                = lv_im_sup_inv_no
            im_inv_status                = 'RECEIVED'
            im_ztext                     = ''
            im_inv_amt                   = ''
            im_year                      = fiscalyear
            im_port                      = 'SONIC'
            im_partno                    = 'SONIC'
            im_parttype                  = 'LS'
          EXCEPTIONS
            error_idoc_control           = 1
            error_idoc_status            = 2
            error_idoc_data              = 3
            error_logical_system_unknown = 4
            error_other                  = 5
            OTHERS                       = 6.
      ENDIF.
*     EOI by PANUSURI Ticket ACR-1125
    ENDIF.

*BTBOUNDY END INSERT D30K9#####


  ENDLOOP.                             " idoc_contrl






ENDFUNCTION.


* subroutine writing IDoc status-record                                *
FORM zidoc_status_zinvcrt
     TABLES idoc_data    STRUCTURE  edidd
            idoc_status  STRUCTURE  bdidocstat
            r_variables  STRUCTURE  bdwfretvar
      USING idoc_contrl  LIKE  edidc
            value(retn_info) LIKE   bapiret2
            status       LIKE  bdidocstat-status
            wf_result    LIKE  bdwf_param-result.

  CLEAR idoc_status.
  idoc_status-docnum   = idoc_contrl-docnum.
  idoc_status-msgty    = retn_info-type.
  idoc_status-msgid    = retn_info-id.
  idoc_status-msgno    = retn_info-number.
  idoc_status-appl_log = retn_info-log_no.
  idoc_status-msgv1    = retn_info-message_v1.
  idoc_status-msgv2    = retn_info-message_v2.
  idoc_status-msgv3    = retn_info-message_v3.
  idoc_status-msgv4    = retn_info-message_v4.
  idoc_status-repid    = sy-repid.
  idoc_status-status   = status.

  CASE retn_info-parameter.
    WHEN 'ACCOUNTINGDATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1BP_INCINV_CREATE_ACCOUNT'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'ADDRESSDATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1BP_INCINV_CREATE_ADDRESSD'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'GLACCOUNTDATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1BP_INCINV_CREATE_GL_ACCOU'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'HEADERDATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1BP_INCINV_CREATE_HEADER'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'ITEMDATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1BP_INCINV_CREATE_ITEM'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'MATERIALDATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1BP_INCINV_CREATE_MATERIAL'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'TAXDATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1BP_INCINV_CREATE_TAX'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'VENDORITEMSPLITDATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1BP_INCINV_CREATE_VENDORSP'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN 'WITHTAXDATA'
         .
      LOOP AT idoc_data WHERE
                        segnam = 'Z1BP_INCINV_CREATE_WITHTAX'.
        retn_info-row = retn_info-row - 1.
        IF retn_info-row <= 0.
          idoc_status-segnum = idoc_data-segnum.
          idoc_status-segfld = retn_info-field.
          EXIT.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.

  ENDCASE.

  INSERT idoc_status INDEX 1.

  IF idoc_status-status = '51'.
    wf_result = '99999'.
    r_variables-wf_param   = 'Error_IDOCs'.
    r_variables-doc_number = idoc_contrl-docnum.
    READ TABLE r_variables FROM r_variables.
    IF sy-subrc <> 0.
      APPEND r_variables.
    ENDIF.
  ELSEIF idoc_status-status = '53'.
    CLEAR wf_result.
    r_variables-wf_param = 'Processed_IDOCs'.
    r_variables-doc_number = idoc_contrl-docnum.
    READ TABLE r_variables FROM r_variables.
    IF sy-subrc <> 0.
      APPEND r_variables.
    ENDIF.
    r_variables-wf_param = 'Appl_Object_Type'.
    r_variables-doc_number = 'BUS2081'.
    READ TABLE r_variables FROM r_variables.
    IF sy-subrc <> 0.
      APPEND r_variables.
    ENDIF.
  ENDIF.

ENDFORM.                               " ZIDOC_STATUS_ZINVCRT
