class ZCL_IM_ME_PROCESS_PO_CUST definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_ME_PROCESS_PO_CUST
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_PROCESS_PO_CUST .

  class-data CURRENT_REFERENCE type ref to IF_PURCHASE_ORDER_MM .
*"* protected components of class ZCL_IM_ME_PROCESS_PO_CUST
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_ME_PROCESS_PO_CUST
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_ME_PROCESS_PO_CUST IMPLEMENTATION.


METHOD IF_EX_ME_PROCESS_PO_CUST~CHECK.
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  BADI Impl: Z_ME_PROCESS_PO_CUST                                     *
*  BADI Def:  ME_PROCESS_PO_CUST                                       *
*  Interface: IF_EX_ME_PROCESS_PO_CUST                                 *
*  Class Int: ZCL_IM__ME_PROCESS_PO_CUST                               *
*  Method:    CHECK                                                    *
*  Author:    Brian Boundy                                             *
*  Date:      October, 2011                                            *
*  Track #:   P2P                                                      *
*                                                                      *
*  Description: Enhanced Purchase Order Processing                     *
*                                                                      *
************************************************************************
************************************************************************

  DATA: lv_cl_message_mm TYPE REF TO cl_message_mm.

  DATA: lt_item_obj      TYPE purchase_order_items,
        ls_item_obj      TYPE purchase_order_item,
        lc_if_item_obj   TYPE REF TO if_purchase_order_item_mm.

  DATA: ls_ekko          TYPE mepoheader,
        ls_ekko_old      TYPE mepoheader,
        ls_ekpo          TYPE mepoitem,
        ls_ekpo_old      TYPE mepoitem,
        lv_adrnum        LIKE ls_ekpo-adrn2,
        ls_adrc          TYPE adrc,
        lv_isnqual       TYPE z_isnqual,
        lv_zzisnvdr      TYPE z_isnvdr,
        lv_msgtyp        TYPE char01,
        lv_msg           TYPE symsgv,
        lv_lifnr         TYPE symsgv,
        lv_emnfr         TYPE emnfr,

        lv_uname         LIKE sy-uname,
        lv_item          TYPE symsgv,
        lv_srmsys        TYPE symsgv,
        lv_eprofile      TYPE meprofile.



  "Get the PO header data
  CALL METHOD im_header->get_data
    RECEIVING
      re_data = ls_ekko.

  "Get previous data.
  CALL METHOD im_header->get_persistent_data
    IMPORTING
      ex_data = ls_ekko_old
    EXCEPTIONS
      no_data = 1.

  IF sy-subrc = 1.
    CLEAR ls_ekko_old.
  ENDIF.




*********************************
*** Ariba Implimentation.
*********************************
  "Code is only performed for a Service Purchase Order "SRO"
  IF ls_ekko-bstyp = 'F' AND ls_ekko-bsart = 'ZF'.
*    All Service PO no need to have  confirmer and validation dates
*    "Ariba Vendor required for Service Based PO's
*    CLEAR    lv_emnfr.
*    SELECT   SINGLE emnfr
*      INTO   lv_emnfr
*      FROM   lfa1
*     WHERE   lifnr = ls_ekko-lifnr.
*    IF ( sy-subrc NE 0 ).
*      CLEAR   lv_emnfr.
*    ENDIF.
*
*
*    IF lv_emnfr IS NOT INITIAL.

    "Service Confirmer required for Service Based PO's
    IF ls_ekko-zzariba_approver IS INITIAL.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ME'
          im_msgty         = 'E'
          im_msgno         = '303'
          im_msgv1         = text-001
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.

      ch_failed = 'X'.
    ENDIF.


    "Validity Dates required for Service Based PO's
    IF ls_ekko-kdatb IS INITIAL OR ls_ekko-kdate IS INITIAL.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ME'
          im_msgty         = 'E'
          im_msgno         = '303'
          im_msgv1         = text-002
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.

      ch_failed = 'X'.
    ENDIF.
  ENDIF.


*********************************
*** S2C Implimentation.
*********************************
  "Get the table of interface objects which reference PO items
  CLEAR: lt_item_obj, ls_item_obj.
  CALL METHOD im_header->get_items
    RECEIVING
      re_items = lt_item_obj.


  "Loop at the referenced PO items and get the PO data
  LOOP AT lt_item_obj INTO ls_item_obj.
    lc_if_item_obj = ls_item_obj-item.

    CALL METHOD lc_if_item_obj->get_data
      RECEIVING
        re_data = ls_ekpo.

    "ensure this is services and not marked for deletion
    IF ls_ekpo-pstyp = '9' AND ls_ekpo-loekz IS INITIAL.

      "Get ISN Vendor
      CLEAR lv_zzisnvdr.
      SELECT SINGLE zzisnvdr
        FROM lfa1
        INTO lv_zzisnvdr
        WHERE lifnr = ls_ekko-lifnr
      .

      "Skip this line if no ISN vendor is suplied
      IF lv_zzisnvdr IS INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR lv_adrnum.

      IF ls_ekpo-adrn2 IS NOT INITIAL.
        "Number of Delivery Address
        lv_adrnum = ls_ekpo-adrn2.

      ELSEIF ls_ekpo-lgort IS NOT INITIAL.
        "PO Item Storage Location
        SELECT SINGLE adrnr
          FROM twlad
          INTO lv_adrnum
          WHERE lgort = ls_ekpo-lgort
            AND werks = ls_ekpo-werks
        .

        IF sy-subrc <> 0.
          "PO Item Plant
          SELECT SINGLE adrnr
            FROM t001w
            INTO lv_adrnum
            WHERE werks = ls_ekpo-werks
          .
        ENDIF.


      ELSE.
        "PO Item Plant
        SELECT SINGLE adrnr
          FROM t001w
          INTO lv_adrnum
          WHERE werks = ls_ekpo-werks
        .
      ENDIF.


      "Get coutry and region from the address
      SELECT SINGLE country region
        FROM adrc
        INTO CORRESPONDING FIELDS OF ls_adrc
        WHERE addrnumber = lv_adrnum
          AND langu = 'E'
      .


      "Get the isn qualification for country and region
      CLEAR lv_isnqual.
      SELECT SINGLE isnqual
        FROM zisnqual
        INTO lv_isnqual
        WHERE isnvdr  = lv_zzisnvdr
          AND land1   = ls_adrc-country
          AND regio   = ls_adrc-region
        .


      CLEAR: lv_msgtyp, lv_msg.
      IF lv_isnqual IS INITIAL.
        "Look for blank entry in Value 1
        SELECT SINGLE value2
          FROM zvar
          INTO lv_msgtyp
          WHERE programm  = 'ZME_PROCESS_PO_CUST'
            AND varname   = 'ISNQUAL'
              AND value1    = ''
        .

        SELECT SINGLE value2
          FROM zvar
          INTO lv_msg
          WHERE programm  = 'ZME_PROCESS_PO_CUST'
            AND varname   = 'ISNQUAL_M'
              AND value1    = ''
          .
      ELSE.
        "Look for specific entry of Value 1
        SELECT SINGLE value2
          FROM zvar
          INTO lv_msgtyp
          WHERE programm  = 'ZME_PROCESS_PO_CUST'
            AND varname   = 'ISNQUAL'
            AND value1    = lv_isnqual
      .

        SELECT SINGLE value2
          FROM zvar
          INTO lv_msg
          WHERE programm  = 'ZME_PROCESS_PO_CUST'
            AND varname   = 'ISNQUAL_M'
            AND value1    = lv_isnqual
        .
      ENDIF.

      "If no msgtyp was found then exit without creating msg
      CHECK lv_msgtyp IS NOT INITIAL.

      lv_lifnr = ls_ekko-lifnr.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ME'
          im_msgty         = lv_msgtyp
          im_msgno         = '303'
          im_msgv1         = 'Vendor '
          im_msgv2         = lv_lifnr
          im_msgv3         = ' qualification status '
          im_msgv4         = lv_msg
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.

      IF lv_msgtyp = 'E'.
        ch_failed = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.



*********************************
*** SRM Implimentation
*********************************
  "Get the table of interface objects which reference PO items
  CLEAR: lt_item_obj, ls_item_obj.
  CALL METHOD im_header->get_items
    RECEIVING
      re_items = lt_item_obj.


  "Loop at the referenced PO items and get the PO data
  CLEAR ls_item_obj.
  LOOP AT lt_item_obj INTO ls_item_obj.
    lc_if_item_obj = ls_item_obj-item.

    "Get the SRM user
    SELECT SINGLE value1
      FROM zvarsys
      INTO lv_uname
      WHERE programm = 'USERNAME'
        AND varname  = 'SRM_RFC'.

    "If the user is SRM then exit the loop
    IF sy-uname = lv_uname.
      EXIT.
    ENDIF.

    "Get current and old version of the line data
    CALL METHOD lc_if_item_obj->get_data
      RECEIVING
        re_data = ls_ekpo.

    CALL METHOD lc_if_item_obj->get_persistent_data
      IMPORTING
        ex_data = ls_ekpo_old
      EXCEPTIONS
        no_data = 1.

    IF sy-subrc = 1.
      CLEAR ls_ekpo_old.
    ENDIF.

    "Assign Item
    lv_item = ls_ekpo-ebelp.

    SHIFT lv_item LEFT DELETING LEADING '0'.

    "Skip items that are not from PR
    IF ls_ekpo-banfn IS INITIAL OR ls_ekpo-bnfpo IS INITIAL.
      CONTINUE.
    ENDIF.

    "Get eprofile from the req
    SELECT SINGLE eprofile
      FROM eban
      INTO lv_eprofile
      WHERE banfn = ls_ekpo-banfn
        AND bnfpo = ls_ekpo-bnfpo
    .

    "Check if not found
    IF sy-subrc  <> 0 OR lv_eprofile IS INITIAL.
      CONTINUE.
    ELSE.
      "Get system name for srm
      SELECT SINGLE logsys
        FROM t160pr
        INTO lv_srmsys
        WHERE eprofile = lv_eprofile
      .

      IF sy-subrc <> 0.
        "If eprofile is blank
        lv_eprofile = 'SRM'.
      ENDIF.
    ENDIF.

    "Check header fields
    IF ls_ekko-ekorg <> ls_ekko_old-ekorg.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'P Org'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    "Check line fields
    IF ls_ekpo-pstyp <> ls_ekpo_old-pstyp.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Item Cat'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-knttp <> ls_ekpo_old-knttp.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Acc. Assignment'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-matnr <> ls_ekpo_old-matnr.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Material Num'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-banfn <> ls_ekpo_old-banfn.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Purchase Req'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-bnfpo <> ls_ekpo_old-bnfpo.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Purchase Req Line'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-konnr <> ls_ekpo_old-konnr.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Contract'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-ktpnr <> ls_ekpo_old-ktpnr.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Contract Line'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-werks <> ls_ekpo_old-werks.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Plant'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-matkl <> ls_ekpo_old-matkl.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Material Group'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-arsnr <> ls_ekpo_old-arsnr.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Settlement Reservation'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.

    IF ls_ekpo-arsps <> ls_ekpo_old-arsps.
      CALL METHOD cl_message_mm=>create
        EXPORTING
          im_msgid         = 'ZMM_MESSAGE'
          im_msgty         = 'E'
          im_msgno         = '017'
          im_msgv1         = lv_item
          im_msgv2         = 'Settlement Reservation Line'
          im_msgv3         = lv_srmsys
          im_msgv4         = ''
          im_force_collect = 'X'
        IMPORTING
          ex_message       = lv_cl_message_mm.
      ch_failed = 'X'.
    ENDIF.
  ENDLOOP.
ENDMETHOD.


method IF_EX_ME_PROCESS_PO_CUST~CLOSE.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_HEADER_REFKEYS.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~FIELDSELECTION_ITEM_REFKEYS.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~INITIALIZE.
endmethod.


METHOD IF_EX_ME_PROCESS_PO_CUST~OPEN.
ENDMETHOD.


method IF_EX_ME_PROCESS_PO_CUST~POST.
endmethod.


method IF_EX_ME_PROCESS_PO_CUST~PROCESS_ACCOUNT.
endmethod.


METHOD IF_EX_ME_PROCESS_PO_CUST~PROCESS_HEADER.
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  BADI Impl: Z_ME_PROCESS_PO_CUST                                     *
*  BADI Def:  ME_PROCESS_PO_CUST                                       *
*  Interface: IF_EX_ME_PROCESS_PO_CUST                                 *
*  Class Int: ZCL_IM__ME_PROCESS_PO_CUST                               *
*  Method:    PROCESS_HEADER                                           *
*  Author:    Boundy Brian                                             *
*  Date:      October, 2011                                            *
*  Track #:   P2P                                                      *
*                                                                      *
*  Description: Enhanced Purchase Order Processing                     *
*                                                                      *
*               Method PROCESS_HEADER - assign the Service Confirmer   *
*               to the PO header                                       *
*                                                                      *
************************************************************************

  DATA: lt_item_obj      TYPE purchase_order_items,
        ls_item_obj      TYPE purchase_order_item,
        lc_if_item_obj   TYPE REF TO if_purchase_order_item_mm.

  DATA: ls_ekko          TYPE mepoheader,
        ls_ekpo          TYPE mepoitem,
        lv_svr_found(1)  TYPE c.

  TYPE-POOLS abap.

  "Get the table of interface objects which reference PO items
  CALL METHOD im_header->get_items
    RECEIVING
      re_items = lt_item_obj.

  CALL METHOD im_header->get_data
    RECEIVING
      re_data = ls_ekko.

  "Clear the Service Confirmer.
  "Don't clear it, leave the old one, but replace it from line items
  "CLEAR ls_ekko-zzariba_approver.

  IF ls_ekko-zzariba_approver IS INITIAL.

    CLEAR lv_svr_found.

    "Loop at the referenced PO items and get the PO data
    LOOP AT  lt_item_obj INTO ls_item_obj.
      IF lv_svr_found IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      lc_if_item_obj = ls_item_obj-item.
      CALL METHOD lc_if_item_obj->get_data
        RECEIVING
          re_data = ls_ekpo.

      "Find the Service Confirmer item with lowest item number
      IF ls_ekpo-loekz IS INITIAL AND ls_ekpo-zzariba_approver IS NOT INITIAL.
        ls_ekko-zzariba_approver = ls_ekpo-zzariba_approver.
        lv_svr_found = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CALL METHOD im_header->set_data
    EXPORTING
      im_data = ls_ekko.

ENDMETHOD.


METHOD if_ex_me_process_po_cust~process_item.
  DATA: lc_if_ex_me_proc_po_cust TYPE REF TO if_ex_me_process_po_cust,
        lc_header TYPE REF TO if_purchase_order_mm.
*BOI PANUSURI Ticket 42467
  DATA: lwa_mepoitem TYPE mepoitem.
  DATA: lwa_mepoheader TYPE mepoheader.
  DATA: lv_emnfr TYPE emnfr.
  DATA: lv_doc_type TYPE z_varvaluel.
  DATA: lt_zvar TYPE STANDARD TABLE OF zvar,
        lwa_zvar LIKE LINE OF lt_zvar.
  CONSTANTS: lc_zme_process_po_cust(19) TYPE c VALUE 'ZME_PROCESS_PO_CUST',
             lc_doc_type_ers(12) TYPE c VALUE 'DOC_TYPE_ERS',
*             lc_DOC_TYPE_GR(11) type c value 'DOC_TYPE_GR'. "(+) PANUSURI Ticket 49399  "(-) PANUSURI Ticket 55438
*EOI PANUSURI Ticket 42467
*BOI PANUSURI Ticket 55438
             lc_doc_type_gr_on(14)  TYPE c VALUE 'DOC_TYPE_GR_ON',
             lc_doc_type_gr_off(15) TYPE c VALUE 'DOC_TYPE_GR_OFF',
             lc_doc_type_gr_on_new(18)  TYPE c VALUE 'DOC_TYPE_GR_ON_NEW',
             lc_doc_type_gr_off_new(19) TYPE c VALUE 'DOC_TYPE_GR_OFF_NEW',
             lc_tol_ovr(7)              TYPE c VALUE 'TOL_OVR', "(+)PANUSURI Ticket 78091
             lc_tol_und(7)              TYPE c VALUE 'TOL_UND'. "(+)PANUSURI Ticket 78091
  DATA : lwa_mepoitem_old TYPE mepoitem.
*EOI PANUSURI Ticket 55438
*BOI PANUSURI Ticket 91677
  DATA : lv_po_from    TYPE ekko-ebeln,
         lv_po_to      TYPE ekko-ebeln,
         lv_ecatalog   TYPE c,
         lt_mepoitems  TYPE purchase_order_items,
         lwa_mepoitems TYPE purchase_order_item,
         lv_total_val  TYPE bbwert,
         lref_item     TYPE REF TO if_purchase_order_item_mm,
         lwa_item      TYPE mepoitem.
*EOI PANUSURI Ticket 91677

  CALL METHOD im_item->get_header
    RECEIVING
      re_header = lc_header.

*BOI PANUSURI Ticket 42467
*Get Header data
  CLEAR lwa_mepoheader.
  CALL METHOD lc_header->get_data
    RECEIVING
      re_data = lwa_mepoheader.

* BOI PANUSURI Ticket 91677
* Get the table of interface objects which reference PO items
  REFRESH lt_mepoitems.
  CALL METHOD lc_header->get_items
    RECEIVING
      re_items = lt_mepoitems.

* Get Item data
  CLEAR lwa_mepoitem.
  CALL METHOD im_item->get_data
    RECEIVING
      re_data = lwa_mepoitem.

* Get document type and Item category from ZVAR table
  REFRESH lt_zvar.
  SELECT *
         FROM zvar
         INTO TABLE lt_zvar
         WHERE programm = lc_zme_process_po_cust.

  CLEAR: lv_po_from,
         lv_po_to,
         lv_ecatalog,
         lwa_zvar,
         lv_total_val.
* Check for eCatalog PO based on Purchase Order number coming from SRM
  READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = 'DOC_RANGE'.
  IF sy-subrc EQ 0.
    lv_po_from = lwa_zvar-value1.
    lv_po_to   = lwa_zvar-value2.
    IF lwa_mepoheader-ebeln GE lv_po_from AND lwa_mepoheader-ebeln LE lv_po_to.
      lv_ecatalog = 'X'.
      CLEAR: lwa_zvar,
             lv_total_val.
*     Check eCatalog PO Vendor from list of Vendors that need to have total PO value checked.
      READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = 'DOC_LIFNR_VAL'
                                                value1 = lwa_mepoheader-lifnr.
      IF sy-subrc = 0.
*       Calculate PO total value.
        LOOP AT lt_mepoitems INTO lwa_mepoitems.
          lref_item = lwa_mepoitems-item.
          CLEAR: lwa_item.
          CALL METHOD lref_item->get_data
            RECEIVING
              re_data = lwa_item.
          IF lwa_item-loekz <> 'L'.
            lv_total_val = lwa_item-brtwr + lv_total_val.
          ENDIF.
          CLEAR lwa_item.
        ENDLOOP.
*       Check PO total value to see how the GR Flag should be set.
        CLEAR lwa_zvar.
        READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = 'DOC_TOTAL_VAL'.
        IF sy-subrc = 0.
          IF lv_total_val GE lwa_zvar-value1.
*           Set GR flag to ON.
            IF lwa_mepoitem-wepos = space.
              lwa_mepoitem-wepos = 'X'.
              CALL METHOD im_item->set_data
                EXPORTING
                  im_data = lwa_mepoitem.
            ENDIF.
          ELSE.
*           Set GR flag to OFF.
            IF lwa_mepoitem-wepos = 'X'.
              lwa_mepoitem-wepos = space.
            ENDIF.
*           Set GR Non-Valuated flag to OFF.
            IF lwa_mepoitem-weunb = 'X'.
              lwa_mepoitem-weunb = space.
            ENDIF.
            CALL METHOD im_item->set_data
              EXPORTING
                im_data = lwa_mepoitem.
          ENDIF.
        ENDIF.
      ELSE.
*       Set GR flag to OFF.
        IF lwa_mepoitem-wepos = 'X'.
          lwa_mepoitem-wepos = space.
        ENDIF.
*       Set GR Non-Valuated flag to OFF.
        IF lwa_mepoitem-weunb = 'X'.
          lwa_mepoitem-weunb = space.
        ENDIF.
        CALL METHOD im_item->set_data
          EXPORTING
            im_data = lwa_mepoitem.
      ENDIF.
    ENDIF.
  ENDIF.
  IF lv_ecatalog = ' '.
*EOI PANUSURI Ticket 91677
    IF lwa_mepoheader IS NOT INITIAL.
*   Get Ariba number for the Vendor number
      CLEAR lv_emnfr.
      SELECT SINGLE emnfr
             FROM lfa1
             INTO lv_emnfr
             WHERE lifnr = lwa_mepoheader-lifnr.
      IF sy-subrc = 0 AND lv_emnfr IS NOT INITIAL.
        CLEAR lv_doc_type.
*     Get document type from ZVAR table
        SELECT SINGLE value1
               FROM zvar
               INTO lv_doc_type
               WHERE programm = lc_zme_process_po_cust
               AND   varname = lc_doc_type_ers
               AND   value1 = lwa_mepoheader-bsart.
        IF sy-subrc = 0.
          CLEAR lwa_mepoitem.
*       Get Item data
          CALL METHOD im_item->get_data
            RECEIVING
              re_data = lwa_mepoitem.
          IF lwa_mepoitem-xersy IS NOT INITIAL.
*         Set ERS to blank.
            lwa_mepoitem-xersy = ' '.
            CALL METHOD im_item->set_data
              EXPORTING
                im_data = lwa_mepoitem.
          ENDIF.
        ENDIF.
      ENDIF.
*BOI PANUSURI Ticket 49399
      IF lwa_mepoitem IS INITIAL.
*     Get Item data
        CALL METHOD im_item->get_data
          RECEIVING
            re_data = lwa_mepoitem.
      ENDIF.

*BOI by PANUSURI Ticket 55438
*   Get document type and Item category from ZVAR table
      REFRESH lt_zvar.
      SELECT *
             FROM zvar
             INTO TABLE lt_zvar
             WHERE programm = lc_zme_process_po_cust.

*   Get existing data from database
      CLEAR lwa_mepoitem_old.
      CALL METHOD im_item->get_persistent_data
        IMPORTING
          ex_data = lwa_mepoitem_old
        EXCEPTIONS
          no_data = 1.

      IF sy-subrc = 1.
*   If new item
*EOI PANUSURI Ticket 55438
*BOC PANUSURI Ticket 55438
*    if lwa_mepoitem is not INITIAL and lwa_mepoitem-wepos is initial.
*      clear lv_doc_type.
**     Get document type from ZVAR table
*      select single value1
*             from zvar
*             into lv_doc_type
*             where programm = lc_ZME_PROCESS_PO_CUST
*             and   varname = lc_DOC_TYPE_GR
*             and   value1 = lwa_mepoheader-bsart
*             and   value2 = lwa_mepoitem-pstyp.
*      if sy-subrc = 0.
**       Set GR flag.
*        lwa_mepoitem-wepos = 'X'.
*        CALL METHOD im_item->SET_DATA
*          EXPORTING
*            IM_DATA = lwa_mepoitem.
*      endif.
*    endif.
*EOC PANUSURI Ticket 55438
*BOI PANUSURI Ticket 55438
        IF lwa_mepoitem IS NOT INITIAL.
          IF lwa_mepoitem-wepos IS INITIAL.
            READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_doc_type_gr_on_new
                                                      value1 = lwa_mepoheader-bsart
                                                      value2 = lwa_mepoitem-pstyp.
            IF sy-subrc = 0.
*           Set GR flag.
              lwa_mepoitem-wepos = 'X'.
              CALL METHOD im_item->set_data
                EXPORTING
                  im_data = lwa_mepoitem.
            ENDIF.
          ELSE.
            READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_doc_type_gr_off_new
                                                      value1 = lwa_mepoheader-bsart
                                                      value2 = lwa_mepoitem-pstyp.
            IF sy-subrc = 0.
*           Clear GR flag.
              lwa_mepoitem-wepos = ' '.
              CALL METHOD im_item->set_data
                EXPORTING
                  im_data = lwa_mepoitem.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSEIF sy-subrc = 0.
*     If existing item
        IF lwa_mepoitem_old IS NOT INITIAL.
          IF lwa_mepoitem-wepos IS INITIAL.
            READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_doc_type_gr_on
                                                      value1 = lwa_mepoheader-bsart
                                                      value2 = lwa_mepoitem-pstyp.
            IF sy-subrc = 0.
*           Set GR flag.
              lwa_mepoitem-wepos = 'X'.
              CALL METHOD im_item->set_data
                EXPORTING
                  im_data = lwa_mepoitem.
            ENDIF.
          ELSE.
            READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_doc_type_gr_off
                                                      value1 = lwa_mepoheader-bsart
                                                      value2 = lwa_mepoitem-pstyp.
            IF sy-subrc = 0.
*           Clear GR flag.
              lwa_mepoitem-wepos = ' '.
              CALL METHOD im_item->set_data
                EXPORTING
                  im_data = lwa_mepoitem.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*EOI PANUSURI Ticket 55438
*    CLEAR lwa_mepoitem.
*EOI PANUSURI Ticket 49399
*  ENDIF.
*EOI PANUSURI Ticket 42467
*BOI PANUSURI Ticket 78091
* Check for Material PO
      IF lwa_mepoitem-pstyp = '0'.
        CLEAR lwa_zvar.
        READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_tol_ovr
                                                  value1 = lwa_mepoheader-bsart.
        IF sy-subrc = 0.
          IF lwa_mepoitem-uebto <> lwa_zvar-value2.
            lwa_mepoitem-uebto = lwa_zvar-value2.
            CALL METHOD im_item->set_data
              EXPORTING
                im_data = lwa_mepoitem.
          ENDIF.
        ENDIF.
        CLEAR lwa_zvar.
        READ TABLE lt_zvar INTO lwa_zvar WITH KEY varname = lc_tol_und
                                                  value1 = lwa_mepoheader-bsart.
        IF sy-subrc = 0.
          IF lwa_mepoitem-untto <> lwa_zvar-value2.
            lwa_mepoitem-untto = lwa_zvar-value2.
            CALL METHOD im_item->set_data
              EXPORTING
                im_data = lwa_mepoitem.
          ENDIF.
        ENDIF.
      ENDIF.
*EOI PANUSURI Ticket 78091
    ENDIF.
  ENDIF.

  lc_if_ex_me_proc_po_cust ?= cl_badi_mm=>get_instance('ME_PROCESS_PO_CUST' ).

  CALL METHOD lc_if_ex_me_proc_po_cust->process_header
    EXPORTING
      im_header = lc_header.

ENDMETHOD.


method IF_EX_ME_PROCESS_PO_CUST~PROCESS_SCHEDULE.
endmethod.
ENDCLASS.
