class ZCL_IM_MEOUT_BAPI_CUST definition
  public
  final
  create public .

*"* public components of class ZCL_IM_MEOUT_BAPI_CUST
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_BAPI_PO_CREATE_02 .
*"* protected components of class ZCL_IM_MEOUT_BAPI_CUST
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_MEOUT_BAPI_CUST
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_MEOUT_BAPI_CUST IMPLEMENTATION.


method IF_EX_ME_BAPI_PO_CREATE_02~EXTENSIONIN.
endmethod.


method IF_EX_ME_BAPI_PO_CREATE_02~EXTENSIONOUT.
endmethod.


METHOD if_ex_me_bapi_po_create_02~inbound.

  DATA: ls_item     LIKE LINE OF ch_item,
        ls_itemx    LIKE LINE OF ch_itemx,
        ls_account  TYPE bapimepoaccount,
        ls_accountx TYPE bapimepoaccountx.

  DATA: lt_zvar     TYPE STANDARD TABLE OF  zvar,
        ls_zvar     LIKE LINE OF            lt_zvar,

        lt_zvarsys  TYPE STANDARD TABLE OF zvarsys,
        ls_zvarsys  LIKE LINE OF           lt_zvarsys.

  DATA: lv_uname    LIKE sy-uname,
        lv_index  TYPE sy-tabix,

        lv_estkz  TYPE estkz,
        lv_frgdt  TYPE frgdt,
        lv_lfdat  TYPE lfdat.
*{   INSERT         D30K926762                                        6
* BOI by PANUSURI Ticket 91677
  DATA: lv_po_from    TYPE ebeln,
        lv_po_to      TYPE ebeln,
        lv_ecatalog   TYPE c.

  FIELD-SYMBOLS: <lfs_zvar> LIKE LINE OF lt_zvar.
* EOI by PANUSURI Ticket 91677
*}   INSERT

  CONSTANTS:  c_programm TYPE programm VALUE 'BAPI_PO_CREATE_CUST'.
*{   INSERT         D30K926762                                        7
* BOI by PANUSURI Ticket 91677
* Get data from ZVAR table
  SELECT * FROM zvar
         INTO TABLE lt_zvar
         WHERE programm = c_programm.
  IF sy-subrc EQ 0.
    SORT lt_zvar BY varname.
  ENDIF.

  CLEAR: lv_po_from,
         lv_po_to,
         lv_ecatalog.

* Check for eCatalog PO based on Purchase Order number coming from SRM
  READ TABLE lt_zvar ASSIGNING <lfs_zvar> WITH KEY varname = 'DOC_RANGE' BINARY SEARCH.
  IF sy-subrc EQ 0.
    lv_po_from = <lfs_zvar>-value1.
    lv_po_to   = <lfs_zvar>-value2.
    IF ch_poheader-po_number GE lv_po_from AND ch_poheader-po_number LE lv_po_to.
      lv_ecatalog = 'X'.
      "Set the flag to Park the Document to blank
      ch_memory_complete = ' '.
    ENDIF.
  ENDIF.

  IF lv_ecatalog = space.
*EOI by PANUSURI Ticket 91677
*}   INSERT

  SELECT SINGLE value1
    FROM zvarsys
    INTO lv_uname
    WHERE programm = 'USERNAME'
      AND varname  = 'SRM_RFC'
      AND value1   = sy-uname.
  .

  "Only DO if PO is created from SRM user
  IF sy-subrc = 0.
****HEADER ENHANCMENTS
****PO Hold.
    SELECT SINGLE *
      FROM zvar
      INTO CORRESPONDING FIELDS OF ls_zvar
      WHERE programm = c_programm
        AND varname  = 'HOLD_DOC_TYPE'
        AND ( value1   = ch_poheader-doc_type
          OR  value1   = '*' ) .

    "Do only for specific doc types
    IF sy-subrc = 0.
      "Set the flag to Park the Document
      ch_memory_complete = 'X'.
    ENDIF.


****LINE ITEM ENHANCMENTS
    LOOP AT ch_item INTO ls_item.
      READ TABLE ch_itemx     INTO ls_itemx     WITH KEY po_item = ls_item-po_item.
      READ TABLE ch_account   INTO ls_account   WITH KEY po_item = ls_item-po_item.
      READ TABLE ch_accountx  INTO ls_accountx  WITH KEY po_item = ls_item-po_item.

*******SW ONLY LOGIC
*      "Only do for SRO
*      IF ch_poheader-doc_type = 'ZF'.
*        "Change Account Type to any
*        ls_item-acctasscat  = 'X'.
*        ls_itemx-acctasscat = 'X'.
*
      "PM Operation Dates
      "Check if it came from a PR
      IF ls_item-preq_no IS NOT INITIAL AND ls_item-preq_item IS NOT INITIAL.
        SELECT SINGLE estkz frgdt lfdat
          FROM eban
          INTO (lv_estkz, lv_frgdt, lv_lfdat)
          WHERE banfn = ls_item-preq_no
            AND bnfpo = ls_item-preq_item
        .
        "Check if req is from pm
        IF lv_estkz = 'F'.
          IF sy-tabix = 1.
            "Set the first date to first item.
            ch_poheader-vper_start = lv_frgdt.
            ch_poheader-vper_end   = lv_lfdat.
          ELSE.
            "Update the dates if needed
            IF ch_poheader-vper_start > lv_frgdt.
              ch_poheader-vper_start = lv_frgdt.
            ENDIF.

            IF ch_poheader-vper_end < lv_lfdat.
              ch_poheader-vper_start = lv_lfdat.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "Do an update here since this is just SW stuff
      MODIFY ch_item     FROM ls_item     TRANSPORTING acctasscat.
*        MODIFY ch_itemx    FROM ls_itemx    TRANSPORTING acctasscat.
*      ENDIF.
*******SW ONLY LOGIC [END]


******Populate Vendor Material Number from agreement or inforec.
      "Get all material master items that do NOT have a vendor material assigned
      IF ls_item-material IS NOT INITIAL AND ls_item-vend_mat IS INITIAL.
        "Check if agreement is on the line item
        IF ls_item-agreement IS NOT INITIAL AND ls_item-agmt_item IS NOT INITIAL.
          "If agreement is used get vendor material from agreement
          SELECT SINGLE idnlf
            FROM ekpo
            INTO ls_item-vend_mat
            WHERE ebeln = ls_item-agreement
              AND ebelp = ls_item-agmt_item
          .
        ENDIF.
        "if vend_mat is still blank check the info record
        IF ls_item-vend_mat IS INITIAL.
          SELECT SINGLE idnlf
            INTO ls_item-vend_mat
            FROM eina
            WHERE matnr = ls_item-material
              AND lifnr = ch_poheader-vendor
          .
        ENDIF.
        "Update the change structure if required
        IF ls_item-vend_mat IS NOT INITIAL.
          "Update the change table.
          ls_itemx-vend_mat = 'X'.
        ENDIF.
      ENDIF.

******Populate Goods Recepient on PO from Purchase Req
*{   DELETE         D30K926463                                        4
*\      CLEAR: ls_account-gr_rcpt, ls_accountx-gr_rcpt.
*}   DELETE
      IF ls_item-preq_no IS NOT INITIAL AND ls_item-preq_item IS NOT INITIAL.
*{   INSERT         D30K926463                                        5
        CLEAR: ls_account-gr_rcpt, ls_accountx-gr_rcpt. "(+)PANUSURI Ticket ACR-229
*}   INSERT
        SELECT SINGLE wempf
          INTO ls_account-gr_rcpt
          FROM ebkn
          WHERE banfn = ls_item-preq_no
            AND bnfpo = ls_item-preq_item.
        IF sy-subrc = 0 AND ls_account-gr_rcpt IS NOT INITIAL.
          ls_accountx-gr_rcpt = 'X'.
        ENDIF.
      ENDIF.


******Begin of GR/IR Flag removal
      "Only for Service Items
      IF ls_item-item_cat = 1 OR ls_item-item_cat = 9.
        "Clear Goods/Invoice Receipts
        ls_item-gr_ind = ''.
        ls_item-ir_ind = ''.
        "Update itemx
        ls_itemx-gr_ind = 'X'.
        ls_itemx-ir_ind = ''.
      ENDIF.

******UPDATE all the changed data
      MODIFY ch_item     FROM ls_item     TRANSPORTING vend_mat gr_ind ir_ind.
      "WHERE <CURRENT LOOP>
      MODIFY ch_itemx    FROM ls_itemx    TRANSPORTING vend_mat gr_ind ir_ind
        WHERE po_item = ls_account-po_item.
      MODIFY ch_accountx FROM ls_accountx TRANSPORTING gr_rcpt
        WHERE po_item = ls_account-po_item.
      MODIFY ch_account  FROM  ls_account TRANSPORTING gr_rcpt
        WHERE po_item = ls_account-po_item.
    ENDLOOP.
  ENDIF.
*{   INSERT         D30K926762                                        8
 ENDIF. "(+)PANUSURI Ticket 91677
*}   INSERT
ENDMETHOD.


method IF_EX_ME_BAPI_PO_CREATE_02~MAP2E_EXTENSIONOUT.
endmethod.


method IF_EX_ME_BAPI_PO_CREATE_02~MAP2I_EXTENSIONIN.
endmethod.


method IF_EX_ME_BAPI_PO_CREATE_02~OUTBOUND.
endmethod.


method IF_EX_ME_BAPI_PO_CREATE_02~PARTNERS_ON_ITEM_ACTIVE.
endmethod.


method IF_EX_ME_BAPI_PO_CREATE_02~TEXT_OUTPUT.
endmethod.


method IF_EX_ME_BAPI_PO_CREATE_02~TOGGLE_ORDER_UNIT.
endmethod.
ENDCLASS.
