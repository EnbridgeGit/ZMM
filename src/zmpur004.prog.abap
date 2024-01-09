REPORT zmpur004 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 65.
*-------------------------------------------------------------------*
* DESCRIPTION                                                       *
* Outstanding Commitments to Vendors                                *
* NOTE -------------------------------------------------------------*
* In the report, some fields have been combined into 1 columnn.     *
* eg. Vendor & Vendor Name; Material & Material Description,        *
*     PO & Item Number, Plant and Storage Location.                 *
* This is because the excel spreadsheet is limited to a maximum of  *
* 20 columns.
*
* NOTE 2:  INITIALIZATION of Document Type different between EAST & WEST
*          EAST: FO to NB
*          WEST: FO to ZS
*          Outstanding Commitment EAST: FO WEST: FO & ZF
*          Default of S_EKORG - EAST: MATL;  WEST: P001
*          If copying between EAST & WEST for consistency, redo
*          these 2 changes.
*-------------------------------------------------------------------*
* CHANGES                                                           *
* 2017/01/19 CKUMAR ACR-2369- Add field 'Network' on selection      *
*                         screen.                                   *
* 2015/07/23 PANUSURI SDP87240- Add field 'S R Confirmer(Header)' on*
*                         report output. Change label of output field
*                         'S R Confirmer' to 'S R Confirmer(Item)'. *
*                         Add 'Overall Limit' on report output.     *
* 2014/12/02 PANUSURI SDP76880- Add WBS,PM Work Order Number and    *
*                            Cost center on selection screen.       *
*                            Add hyperlink to the PO.               *
*                            Logic change for Outstanding commitment.
* 2013/12/02 G Ymana SDP51588 - Remove SDP51588 code - No longer    *
*                               needed.                             *
* 2013/08/22 G Ymana SDP51588 - Add #PO Created By# Column in report*
* 2012/09/06 M Khan   TR995 Change C: drive to H: drive with direct-*
*                           ory file selection using F4 & move      *
*                           hard-coded file path/name to variant.   *
*                                                                   *
* 2012/05/03 M Khan   TR926 - Add Add new columns (Purchase Agrement*
*                           Requisitioner, Service Receipt Confirmer*
*                           CC, G/L Acct, Order, WBS & Account      *
*                           Assignment Category).                   *
* 2011/07/21 BTBOUNDY TR872 - Add Ariba CS ID                       *
* 2010/11/11 btboundy TR789 - Split columns to individual instead of*
*                              combined.                            *
* 2008/04/04 mdemeest TR277 - Outstanding commitment calculation    *
*                             exclude vgabe = 3 (Subsequent debit)  *
* 2006/08/01 mdemeest TR277 - Radiobuttons, delivery date fix and   *
*                             net price calculation.                *
* 2006/03/08 mdemeest #---- Formatted dates with yyyy/mm/dd         *
* 2005/04/22 mdemeest #1070 New Report                              *
*-------------------------------------------------------------------*
TABLES:
       lfa1,
       ekbe,                       "PO History
       eket,                       "Scheduling Details
       esuh,                       "Service Data
       prps,                       "(+) D30K925231 SDP 81665
       ekko,                       "Purchase Order
       ekpo,                       "Purchase Order Detail
       cooi,                       "Commitments Management: Line Items
       ekkn.              "TR926 Account Assignment in Purchasing Doc
*- Begin of Change D30K925231 SDP 81665 dt 10-Feb-2015
TYPES: BEGIN OF ty_prps,
         pspnr TYPE prps-pspnr,
         posid TYPE prps-posid,
       END OF ty_prps.

DATA: it_prps TYPE STANDARD TABLE OF ty_prps,
      wa_prps LIKE LINE OF it_prps,

      it_wbs  TYPE RANGE OF ekkn-ps_psp_pnr,
      wa_wbs  LIKE LINE OF it_wbs.
*- End of Change D30K925231 SDP 81665 dt 10-Feb-2015

DATA:
    BEGIN OF wa OCCURS 0,
      bsart          LIKE ekko-bsart,
      lifnr          LIKE ekko-lifnr,                       "TR789
      name           LIKE lfa1-name1,                       "TR789
      emnfr          LIKE lfa1-emnfr,                       "TR872
      ebeln          LIKE ekko-ebeln,                       "TR789
      ebelp(5)       TYPE c,        "LIKE ekpo-ebelp,       "TR789
      banfn          LIKE eket-banfn,
      labnr          LIKE ekpo-labnr,                       "TR789
      bednr          LIKE ekpo-bednr,                       "TR789
      ekgrp          LIKE ekko-ekgrp,                       "TR789
      matkl          LIKE ekpo-matkl,                       "TR789
      dc(2)          TYPE c,        "LIKE ekpo-elikz,       "TR789
      fi(2)          TYPE c,        "LIKE ekpo-erekz,       "TR789
      aedatekko(10)  TYPE c,                 "Order date
      eindt(10)      TYPE c,                 "Delivery date
      kdatb(10)      TYPE c,                 "Validity Start Date
      kdate(10)      TYPE c,                 "Validity End Date
      werks          LIKE ekpo-werks,                       "TR789
      lgort          LIKE ekpo-lgort,                       "TR789
      ematn          LIKE ekpo-ematn,                       "TR789
      txz01          LIKE ekpo-txz01,                       "TR789
      konnr          LIKE ekpo-konnr,                       "TR926
      afnam          LIKE ekpo-afnam,                       "TR926
      zzariba_appr_hdr(21) TYPE c,         "(+)PANUSURI Ticket 87240
      zzariba_approver LIKE ekpo-zzariba_approver,          "TR926
      kostl          LIKE ekkn-kostl,                       "TR926
      sakto          LIKE ekkn-sakto,                       "TR926
      aufnr          LIKE ekkn-aufnr,                       "TR926
*- Begin of Change for SDP76880 dt 1/29/2015
*     ps_psp_pnr(14) TYPE c,                                 "TR926
     ps_psp_pnr     LIKE prps-posid,
*- End of Change for SDP76880 dt 1/29/2015
*     ps_psp_pnr     LIKE ekkn-ps_psp_pnr,                  "TR926
      knttp          LIKE ekpo-knttp,                       "TR926
      meins          LIKE ekpo-meins,                       "TR789
      peinh(5)       TYPE c,        "LIKE ekpo-peinh,       "TR789
      bprme          LIKE ekpo-bprme,                       "TR789

      netpr(13)      TYPE c,                "like ekpo-netpr,
      waers          LIKE ekko-waers,
      menge(15)      TYPE c,                "like ekpo-menge,
      netwr(15)      TYPE c,            "(+) SDP 76880 1/29/2015

      wemng(15)      TYPE c,                "like ekbez-wemng,
      remng(15)      TYPE c,                "like ekpo-menge,
      commit(15)     TYPE c,                "like ekpo-netpr,
      sumlimit(15)   TYPE c,            "(+)PANUSURI Ticket 87240
    END OF wa.

*-----------------------------------------------------------------------
DATA:  w_option(12)  TYPE c VALUE 'START_EXCEL'.
DATA:  w_head01(132) TYPE c.
DATA:  w_head02(120) TYPE c.
DATA:  retcode       LIKE sy-subrc.
DATA:  w_repttl      LIKE sy-title.
DATA:  wa_ekgrp(4)   TYPE c.
*BOI by PANUSURI Ticket 76880
DATA:  lv_whgbtr      TYPE whgxxx,
       lwa_listheader TYPE slis_listheader,
       lt_listheader  TYPE slis_t_listheader,
       lt_fieldcat    TYPE slis_t_fieldcat_alv,
       lwa_layout     TYPE slis_layout_alv.
*EOI by PANUSURI Ticket 76880
DATA: lv_sumlimit TYPE sumlimit.  "(+)PANUSURI Ticket 87240

DATA:
      BEGIN OF prot_header OCCURS 1,
        spaltenname(20) TYPE c,
        ddic_table(5)   TYPE c,
        ddic_field(5)   TYPE c,
        key             TYPE c,
      END OF prot_header.

DATA:  errortab LIKE hrerror   OCCURS 0 WITH HEADER LINE.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) text-001.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.
SELECT-OPTIONS:
           s_lifnr FOR ekko-lifnr,                    "Vendor
           s_ekorg FOR ekko-ekorg  NO INTERVALS OBLIGATORY
                                  DEFAULT 'MATL'.
SELECT-OPTIONS:
           s_bsart FOR ekko-bsart OBLIGATORY,
           s_ekgrp FOR ekko-ekgrp,
           s_matkl FOR ekpo-matkl,                    "Material Group
           s_werks FOR ekpo-werks,                    "Plant
           s_ebelp FOR ekpo-ebelp,                    "Item Number
           s_knttp FOR ekpo-knttp,
           s_eindt FOR eket-eindt,
           s_kdate FOR ekko-kdate,
           s_ebeln FOR ekko-ebeln,
           s_ematn FOR ekpo-ematn,
           s_bedat FOR ekko-bedat,
*          BOI by PANUSURI Ticket 76880
           s_wbs   FOR prps-posid,                "(+)D30K925231 SDP 81665
*           s_wbs   FOR ekkn-ps_psp_pnr,          "(-)D30K925231 SDP 81665
           s_aufnr FOR ekkn-aufnr,
           s_kostl FOR ekkn-kostl,
           s_nplnr FOR ekkn-nplnr.                "(+)CKUMAR Ticket ACR-2369
*          EOI by PANUSURI Ticket 76880
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-003.

* DELIVERY
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-200.
SELECTION-SCREEN COMMENT 20(13) text-201 FOR FIELD p_delall.
PARAMETER:       p_delall  RADIOBUTTON                     GROUP delr.
SELECTION-SCREEN COMMENT 45(22) text-202 FOR FIELD p_delvry.
PARAMETER:       p_delvry  RADIOBUTTON                     GROUP delr.
SELECTION-SCREEN COMMENT 79(23) text-203 FOR FIELD p_undelr.
PARAMETER:       p_undelr  RADIOBUTTON                     GROUP delr.
SELECTION-SCREEN END OF LINE.

* INVOICES
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-205.
SELECTION-SCREEN COMMENT 20(13) text-201 FOR FIELD p_finall.
PARAMETER:       p_finall  RADIOBUTTON                     GROUP finl.
SELECTION-SCREEN COMMENT 45(22) text-207 FOR FIELD p_fin.
PARAMETER:       p_fin     RADIOBUTTON                     GROUP finl.
SELECTION-SCREEN COMMENT 79(23) text-208 FOR FIELD p_nonfin.
PARAMETER:       p_nonfin  RADIOBUTTON                     GROUP finl.
SELECTION-SCREEN END OF LINE.

* DELETED
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-210.
SELECTION-SCREEN COMMENT 20(13) text-201 FOR FIELD p_dltall.
PARAMETER:       p_dltall  RADIOBUTTON                     GROUP delt.
SELECTION-SCREEN COMMENT 45(22) text-211 FOR FIELD p_delt.
PARAMETER:       p_delt     RADIOBUTTON                    GROUP delt.
SELECTION-SCREEN COMMENT 79(23) text-212 FOR FIELD p_nondel.
PARAMETER:       p_nondel  RADIOBUTTON                     GROUP delt.
SELECTION-SCREEN END OF LINE.

* GOODS RECEIPT
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-215.
SELECTION-SCREEN COMMENT 20(13) text-201 FOR FIELD p_grall.
PARAMETER:       p_grall  RADIOBUTTON                       GROUP grin.
SELECTION-SCREEN COMMENT 45(22) text-216 FOR FIELD p_gron.
PARAMETER:       p_gron     RADIOBUTTON                     GROUP grin.
SELECTION-SCREEN COMMENT 79(23) text-217 FOR FIELD p_groff.
PARAMETER:       p_groff  RADIOBUTTON                       GROUP grin.
SELECTION-SCREEN END OF LINE.

* OUTSTANDING COMMITMENT
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-220.
SELECTION-SCREEN COMMENT 20(13) text-201 FOR FIELD p_comall.
PARAMETER:       p_comall  RADIOBUTTON                      GROUP comm.
SELECTION-SCREEN COMMENT 45(22) text-221 FOR FIELD p_compos.
PARAMETER:       p_compos  RADIOBUTTON                      GROUP comm.
SELECTION-SCREEN COMMENT 79(23) text-222 FOR FIELD p_comneg.
PARAMETER:       p_comneg  RADIOBUTTON                      GROUP comm.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETERS:      p_rprt RADIOBUTTON GROUP rbcr,     "Print report
                 p_excl RADIOBUTTON GROUP rbcr.     "Excel spreadsheet
PARAMETERS       p_file LIKE rlgrap-filename DEFAULT 'H:\SAPTEMP'. "TR995
SELECTION-SCREEN END OF BLOCK box2.
*-----------------------------------------------------------------------
INITIALIZATION.
  MOVE 'IBTFO  ZF' TO s_bsart.
  APPEND s_bsart.

******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  PERFORM check_file_path.
*End of TR995 changes
*-----------------------------------------------------------------------
START-OF-SELECTION.
*- Begin of Change D30K925231 SDP 81665 dt 10-Feb-2015
  CLEAR: it_wbs[].
  IF s_wbs[] IS NOT INITIAL.
    SELECT pspnr
           posid
      FROM prps INTO TABLE it_prps
      WHERE posid IN s_wbs.
    IF sy-subrc EQ 0.
      SORT it_prps BY pspnr.
      LOOP AT it_prps INTO wa_prps.
        CLEAR: wa_wbs.
        wa_wbs-sign = 'I'.
        wa_wbs-option = 'EQ'.
        wa_wbs-low = wa_prps-pspnr.
        APPEND wa_wbs TO it_wbs.
      ENDLOOP.
    ENDIF.
    CLEAR: it_prps[].
  ENDIF.
*- End of Change D30K925231 SDP 81665 dt 10-Feb-2015

*-----------------------------------------------------------------------
* Select all PO's and the PO detail for report
*-----------------------------------------------------------------------
  SELECT * FROM ekko
      WHERE ebeln IN s_ebeln
        AND bsart IN s_bsart
        AND ekgrp IN s_ekgrp
        AND ekorg IN s_ekorg
        AND lifnr IN s_lifnr
        AND bedat IN s_bedat
        AND loekz = space.                 "Non-deleted records
    CLEAR wa.
    IF ekko-bsart = 'FO'.
      IF ekko-kdate < s_kdate+3(8).
        EXIT.
      ENDIF.
    ENDIF.
*-----------------------------------------------------------------------
*  fields being moved: bsart,  waers
*-----------------------------------------------------------------------
    DATA:  tmp_bsart LIKE ekko-bsart.

    MOVE-CORRESPONDING ekko TO wa.
    MOVE ekko-zzariba_approver TO wa-zzariba_appr_hdr.  "(+)PANUSURI Ticket 87240
    CLEAR wa-zzariba_approver.                          "(+)PANUSURI Ticket 87240
    MOVE ekko-bsart TO tmp_bsart.

    CONCATENATE ekko-aedat(4) '/'
                ekko-aedat+4(2) '/'
                ekko-aedat+6(2)
                INTO wa-aedatekko.

    CONCATENATE ekko-kdatb(4) '/'
                ekko-kdatb+4(2) '/'
                ekko-kdatb+6(2)
                INTO wa-kdatb.

    CONCATENATE ekko-kdate(4) '/'
                ekko-kdate+4(2) '/'
                ekko-kdate+6(2)
                INTO wa-kdate.

    SELECT * FROM ekpo
      WHERE ebeln = ekko-ebeln
        AND ebelp IN s_ebelp
        AND werks IN s_werks
        AND knttp IN s_knttp
        AND ematn IN s_ematn
        AND matkl IN s_matkl.
*        and loekz = space                "Non-deleted records
*        or erekz = p_erekz                 "final invoice
*        or loekz = p_loekz ).              "flagged for deletion

* Delivery options - all items, completed items, undelivered items
      IF p_delall = 'X'.                           "All
      ELSEIF p_delvry = 'X' AND ekpo-elikz = 'X'. "Del complete
      ELSEIF p_undelr = 'X' AND ekpo-elikz = ' '.  "Undelivered
      ELSE.
        CONTINUE.
      ENDIF.

* Final invoices - all items, final invoicess, non-final invoices
      IF p_finall = 'X'.                         "All
      ELSEIF p_fin = 'X' AND ekpo-erekz = 'X'.   "Final Invoices
      ELSEIF p_nonfin = 'X' AND ekpo-erekz = ' '. "Undelivered
      ELSE.
        CONTINUE.
      ENDIF.

* Deleted Items - all items, deleted items, non-deleted items
      IF p_dltall = 'X'.                          "All
      ELSEIF p_delt = 'X' AND ekpo-loekz = 'L'.   "Deleted items
      ELSEIF p_nondel = 'X' AND ekpo-loekz = ' '. "Non-deleted
      ELSE.
        CONTINUE.
      ENDIF.

* Goods Receipts - all items, received, no receipts items
      IF p_grall = 'X'.                          "All
      ELSEIF p_gron = 'X' AND ekpo-wepos = 'X'.  "Received items
      ELSEIF p_groff = 'X' AND ekpo-wepos = ' '. "No receipts
      ELSE.
        CONTINUE.
      ENDIF.


* Outstanding Commitment - all items, positive amts, zero/negative amts.
      MOVE-CORRESPONDING ekpo TO wa.                        "2008/04/28

*Fix field format                                         TR789
      DATA: tmpvar(5) TYPE c.
      SPLIT wa-menge AT '.' INTO wa-menge tmpvar.
      wa-ematn = wa-ematn+12(6).

      CLEAR wa-commit.
*BOC by PANUSURI Ticket 76880
*      IF ekko-bsart = 'NB' OR ekko-bsart = 'ZF'.
*        PERFORM po_calculations.
*      ELSEIF ekko-bsart = 'FO'.
*        SELECT SINGLE * FROM esuh
*            WHERE packno = ekpo-packno.
*        IF sy-subrc = '0'.
*          wa-commit = esuh-sumlimit - esuh-actvalue.
*        ENDIF.
*      ENDIF.
*EOC by PANUSURI Ticket 76880
*BOI by PANUSURI Ticket 76880 1/29/2015
      IF ekko-bsart = 'NB' OR ekko-bsart = 'ZF'.
        PERFORM po_calculations.
      ENDIF.
*EOI by PANUSURI Ticket 76880 1/29/2015
*BOI by PANUSURI Ticket 87240
      CLEAR lv_sumlimit.
      SELECT SINGLE sumlimit FROM esuh
        INTO lv_sumlimit WHERE packno = ekpo-packno.
      IF sy-subrc = 0.
        wa-sumlimit = lv_sumlimit.
      ELSE.
        CLEAR wa-sumlimit.
      ENDIF.
*EOI by PANUSURI Ticket 87240

      IF p_comall = 'X'.                            "All
      ELSEIF p_compos = 'X' .                 "Pos amts
        IF wa-commit > 0.
        ELSE.
          CONTINUE.
        ENDIF.
      ELSEIF p_comneg = 'X'.
        IF wa-commit > 0.
          CONTINUE.
        ENDIF.
      ENDIF.

*-----------------------------------------------------------------------
*  fields being moved: labnr, matkl, meins, netpr, menge
*-----------------------------------------------------------------------
*             move-corresponding ekpo to wa.  "2008/04/28
      MOVE ekpo-elikz TO wa-dc.
      MOVE ekpo-erekz TO wa-fi.

      IF ekko-bstyp = 'B'.
        MOVE wa-ebeln TO wa-banfn.
        CLEAR wa-ebeln.
      ENDIF.

* flag for deletion is concatenated after the PO type in column 1
      IF ekpo-loekz <> ' '.
        CONCATENATE tmp_bsart '_' ekpo-loekz INTO wa-bsart.
      ELSE.
        MOVE tmp_bsart TO wa-bsart.
      ENDIF.

      SELECT SINGLE * FROM eket
        WHERE ebeln = ekpo-ebeln
          AND ebelp = ekpo-ebelp
          AND eindt IN s_eindt.

      CLEAR:  wa-eindt, wa-banfn.
      IF sy-subrc = '0'.
        CONCATENATE eket-eindt(4) '/'
                    eket-eindt+4(2) '/'
                    eket-eindt+6(2)
                    INTO wa-eindt.
        MOVE eket-banfn TO wa-banfn.
      ELSE.
        CONTINUE.
      ENDIF.
*              clear wa-commit.
*              if ekko-bsart = 'NB'.
*                 perform po_calculations.
*              elseif ekko-bsart = 'FO'.
*                 select single * from esuh
*                     where packno = ekpo-packno.
*                 if sy-subrc = '0'.
*                    wa-commit = esuh-sumlimit - esuh-actvalue.
*                 endif.
*              endif.

*-----------------------------------------------------------------------
*
*            perform PO_Calculations.
*-----------------------------------------------------------------------

      PERFORM move_lfa1_fields.
*Start of TR926 Changes.
*      IF ekpo-vrtkz = space.       "(-) SDP81665
      SELECT SINGLE kostl sakto aufnr ps_psp_pnr
        INTO (ekkn-kostl, ekkn-sakto, ekkn-aufnr, ekkn-ps_psp_pnr)
        FROM ekkn
       WHERE ebeln = ekpo-ebeln
         AND ebelp = ekpo-ebelp
*          BOI by PANUSURI Ticket 76880
         AND kostl IN s_kostl
         AND aufnr IN s_aufnr
*- Begin of Change D30K925231 SDP 81665 dt 10-Feb-2015
*         and ps_psp_pnr in s_wbs.
         AND ps_psp_pnr IN it_wbs
         AND nplnr IN s_nplnr.                          "(+)CKUMAR Ticket ACR-2369
*- End of Change D30K925231 SDP 81665 dt 10-Feb-2015
*          EOI by PANUSURI Ticket 76880
      IF sy-subrc = 0.
*            MOVE-CORRESPONDING ekkn TO wa.
        wa-kostl = ekkn-kostl.
        wa-sakto = ekkn-sakto.
        wa-aufnr = ekkn-aufnr.
        wa-ps_psp_pnr = ekkn-ps_psp_pnr.
        IF ekkn-ps_psp_pnr = '00000000'.
          wa-ps_psp_pnr   = space.
        ELSE.
          WRITE ekkn-ps_psp_pnr TO wa-ps_psp_pnr.
        ENDIF.
*        ENDIF.
*      ENDIF.
*End of TR926 Changes.
*- Begin of Change D30K925231 SDP 81665 dt 10-Feb-2015
        IF s_wbs[] IS NOT INITIAL AND it_wbs[] IS INITIAL.
          CONTINUE.
        ENDIF.
*- End of Change D30K925231 SDP 81665 dt 10-Feb-2015
        APPEND wa.
*- Begin of Change  SDP 81665 dt 16-Feb-2015
      ELSE.
        IF s_wbs[]   IS NOT INITIAL OR
           s_kostl[] IS NOT INITIAL OR
           s_aufnr[] IS NOT INITIAL OR
           s_nplnr[] IS NOT INITIAL.              "(+)CKUMAR Ticket ACR-2369
          CONTINUE.
        ENDIF.
        APPEND wa.
*- End of Change  SDP 81665 dt 16-Feb-2015
      ENDIF.
*      ENDIF.                       "(-) SDP81665
    ENDSELECT.
  ENDSELECT.

  SORT wa BY bsart lifnr ebeln ebelp.                       "TR789

*sort wa by bsart lifnr_name ebeln_ebelp. "TR789
  IF p_rprt = 'X'.
    PERFORM create_output_report.
  ELSE.
    PERFORM create_spreadsheet.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  create_output_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_output_report.
  IF wa[] IS NOT INITIAL. "(+)PANUSURI Ticket 76880
    MOVE sy-repid  TO w_repttl.
    MOVE '-'       TO w_repttl+10(1).
    MOVE text-ttl  TO w_repttl+12(34).

    CONCATENATE text-dte sy-datum text-clt sy-mandt sy-sysid
                 INTO w_head01 SEPARATED BY space.
    IF p_delall = 'X'.
    ELSEIF p_delvry = 'X'.
      MOVE 'Delivery Completed' TO w_head02.
    ELSE.
      MOVE 'Undelivered Items'  TO w_head02.
    ENDIF.
    IF p_finall = 'X'.
    ELSEIF p_fin = 'X'.
      CONCATENATE w_head02 '  Final Invoices' INTO w_head02.
    ELSEIF p_nonfin = 'X'.
      CONCATENATE w_head02 ' Open Invoices' INTO w_head02.
    ENDIF.
    IF p_dltall = 'X'.
    ELSEIF p_delt = 'X'.
      CONCATENATE w_head02 '  Deleted Line Items' INTO w_head02.
    ELSEIF p_nondel = 'X'.
      CONCATENATE w_head02 '  Non Deleted Line Items' INTO w_head02.
    ENDIF.

*BOC by PANUSURI Ticket 76880
*  PERFORM prot_header.
*  IF p_rprt = 'X'.
*    CLEAR w_option.
*    IF sy-batch = 'X'.
*      w_option = 'LINESELMOD:1'.
*    ENDIF.
*  ENDIF.
*
*  CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
*    EXPORTING
*      basic_list_title    = w_repttl
*      file_name           = sy-cprog
*      head_line1          = w_head01
*      head_line2          = w_head02
*      additional_options  = w_option
*    IMPORTING
*      return_code         = retcode
*    TABLES
*      data_tab            = wa
*      fieldname_tab       = prot_header
*      error_tab           = errortab
*    EXCEPTIONS
*      download_problem    = 1
*      no_data_tab_entries = 2
*      table_mismatch      = 3
*      print_problems      = 4
*      OTHERS              = 5.
*
*  IF sy-subrc <> 0.
*    WRITE: /1 'table download unsuccessful - reason = ', sy-subrc.
*    WRITE: /1 'No data selected'.
*  ENDIF.
*EOC by PANUSURI Ticket 76880
*BOI by PANUSURI Ticket 76880
*   Build fieldcatalog
    PERFORM build_fieldcatalog.

    lwa_layout-colwidth_optimize = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = 'ZMPUR004'
        i_callback_user_command = 'ALV_USER_COMMAND'
        i_callback_top_of_page  = 'DISPLAY_TOP_OF_PAGE'
        is_layout               = lwa_layout
        it_fieldcat             = lt_fieldcat[]
        i_default               = 'X'
        i_save                  = 'A'
      TABLES
        t_outtab                = wa[]
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.
  ELSE.
    WRITE: /1 'No data selected'.
  ENDIF.
*EOI by PANUSURI Ticket 76880

ENDFORM.                    "create_output_report



*&---------------------------------------------------------------------*
*&      Form  prot_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prot_header.
  MOVE text-100        TO prot_header-spaltenname.
  APPEND prot_header.
*  MOVE text-101        TO prot_header-spaltenname.
*  APPEND prot_header.
  MOVE text-151        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-152        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-102        TO prot_header-spaltenname.          "TR872
  APPEND prot_header.                                       "TR872
*  MOVE text-103        TO prot_header-spaltenname.
*  APPEND prot_header.
  MOVE text-153        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-154        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-104        TO prot_header-spaltenname.
  APPEND prot_header.
*  MOVE text-105        TO prot_header-spaltenname.
*  APPEND prot_header.
  MOVE text-155        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-156        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
*  MOVE text-106        TO prot_header-spaltenname.
*  APPEND prot_header.
  MOVE text-157        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-158        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
*  MOVE text-107        TO prot_header-spaltenname.
*  APPEND prot_header.
  MOVE text-159        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-160        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-108        TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-109        TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-110        TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-111        TO prot_header-spaltenname.
  APPEND prot_header.
*  MOVE text-112        TO prot_header-spaltenname.
*  APPEND prot_header.
  MOVE text-161        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-162        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
*  MOVE text-115        TO prot_header-spaltenname.
*  APPEND prot_header.
  MOVE text-163        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-164        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-168        TO prot_header-spaltenname.          "TR926
  APPEND prot_header.                                       "TR926
  MOVE text-169        TO prot_header-spaltenname.          "TR926
  APPEND prot_header.                                       "TR926
  MOVE text-170        TO prot_header-spaltenname.          "TR926
  APPEND prot_header.                                       "TR926
  MOVE text-171        TO prot_header-spaltenname.          "TR926
  APPEND prot_header.                                       "TR926
  MOVE text-172        TO prot_header-spaltenname.          "TR926
  APPEND prot_header.                                       "TR926
  MOVE text-173        TO prot_header-spaltenname.          "TR926
  APPEND prot_header.                                       "TR926
  MOVE text-174        TO prot_header-spaltenname.          "TR926
  APPEND prot_header.                                       "TR926
  MOVE text-175        TO prot_header-spaltenname.          "TR926
  APPEND prot_header.                                       "TR926
*  MOVE text-117        TO prot_header-spaltenname.
*  APPEND prot_header.
  MOVE text-165        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-166        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-167        TO prot_header-spaltenname.          "TR789
  APPEND prot_header.                                       "TR789
  MOVE text-118        TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-119        TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-120        TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-121        TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-122        TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-123        TO prot_header-spaltenname.
  APPEND prot_header.
ENDFORM.                    "prot_header

*-----------------------  MOVE_LFA1_FIELDS  ---------------------------
*  This routine is used to get the vendor's name from the master file
*-----------------------------------------------------------------------
FORM move_lfa1_fields.                                      "TR789
  SELECT SINGLE * FROM lfa1    "For each PO, get vendor name
     WHERE lifnr = ekko-lifnr.
  IF sy-subrc = '0'.
    MOVE lfa1-name1 TO wa-name.                             "TR789
    MOVE lfa1-emnfr TO wa-emnfr.                            "TR872
  ENDIF.
ENDFORM.                    "move_other_fields

*-------------------------  PO_Calculations  ---------------------------
* PO History is used to calculate "Still to be Delivered", "Still to be
* Invoiced" and "Outstanding Commitments".  Using the quantity ordered,
* the quantity from each identified record (Receipts --> VGABE = 1),
* (Invoices --> VGABE = 2) from the quantity ordered.  This results in
* the "Still to be Delivered" and "Still to be Invoiced" values.
* "Outstanding is calculated be multiplying the "Still to be Invoiced"
* by the "Net Price" (NETPR) from the Purchase Order.
* All other values of VGABE except 1 and 2 are ignored in this program.
* Must also consider the value of EKBE-SHKZG (S--> +, H --> - )
*-----------------------------------------------------------------------
FORM po_calculations.
*-----------------------------------------------------------------------
  DATA: rflag(1) TYPE c,              "indicates receipts
        iflag(1) TYPE c,              "indicates invoices
        itotal LIKE ekbe-dmbtr,       "indicates total amt of invoices
        wacommit LIKE ekpo-netwr.

  CLEAR: rflag, iflag, itotal.
  IF ekko-bsart EQ 'NB'.              "(+) SDP 76880 1/29/2015
    MOVE wa-menge TO: wa-wemng, wa-remng.
  ENDIF.                              "(+) SDP 76880 1/29/2015
  MOVE ekpo-netwr TO wacommit.
  SELECT * FROM ekbe
     WHERE ebeln = ekpo-ebeln
       AND ebelp = ekpo-ebelp
       AND vgabe IN (1, 2, 3).
    IF ekbe-shkzg = 'H'.
      COMPUTE ekbe-menge = ekbe-menge * -1.
      COMPUTE ekbe-dmbtr = ekbe-dmbtr * -1.
      COMPUTE ekbe-rewrb = ekbe-rewrb * -1.         "(+)SDP76880 1/29/2015
      COMPUTE ekbe-wrbtr = ekbe-wrbtr * -1.         "(+)SDP76880 1/29/2015
    ENDIF.
    CASE ekbe-vgabe.
      WHEN '1'.
*** for Material POs, the commitments from GR already done
        IF ekko-bsart = 'NB'.                       "(+)SDP76880 1/29/2015
          COMPUTE wa-wemng = wa-wemng - ekbe-menge.
          COMPUTE wacommit = wacommit - ekbe-wrbtr. "(+)SDP76880 1/29/2015
        ENDIF.                                      "(+)SDP76880 1/29/2015
*- Begin of Change SDP76880 1/29/2015
*      WHEN '2'.
*        COMPUTE wa-remng = wa-remng - ekbe-menge.
*        COMPUTE wacommit = wacommit - ekbe-dmbtr.
*      WHEN '3'.
*        COMPUTE wacommit = wacommit - ekbe-dmbtr.
      WHEN '2'.
        IF ekko-bsart = 'NB'.
          COMPUTE wa-remng = wa-remng - ekbe-menge.
        ELSE.   "For Service PO, the Commitments from Invoice
          COMPUTE wacommit = wacommit - ekbe-rewrb.
        ENDIF.
      WHEN '3'.
        IF ekko-bsart NE 'NB'.      " for Service Commitmnets from Invoice
          COMPUTE wacommit = wacommit - ekbe-rewrb.
        ENDIF.
*- End of Change SDP76880 1/29/2015
    ENDCASE.
  ENDSELECT.
*      if ekbe-shkzg = 'H'.
*         compute ekbe-menge = ekbe-menge * -1.
*         compute ekbe-dmbtr = ekbe-dmbtr * -1.
*      endif.
*      case ekbe-vgabe.
*      when '1'.                "receipts
*         move 'X' to rflag.
*         compute wa-wemng = wa-wemng - ekbe-menge.
*      when '2'.                "invoices
*         move 'X' to iflag.
*         compute wa-remng = wa-remng - ekbe-menge.
*         compute itotal = itotal + ekbe-dmbtr.
*      when '3'.                "invoices
*         move 'X' to iflag.
*         compute wa-remng = wa-remng - ekbe-menge.
*         compute itotal = itotal + ekbe-dmbtr.
*      endcase.
*   endselect.

*  This code executes only if there are invoices
*   if iflag = 'X'.
*      if rflag = 'X'.
*         compute wacommit = ( wa-remng - wa-wemng )
*                          * ( wa-netpr / ekpo-peinh ).
*      else.
*         compute wacommit = wacommit - itotal.
*      endif.
*   else.
*      compute wacommit = wa-remng * ( wa-netpr / ekpo-peinh ).
*   endif.
*- Begin of Change SDP76880 1/29/2015
  IF wacommit LT 0.   " if Commitment determined is less than 0, mark them as 0
    wacommit = 0.
  ENDIF.
  IF wa-remng LT 0.
    CLEAR: wa-remng.
  ENDIF.
  IF wa-wemng LT 0.
    CLEAR: wa-wemng.
  ENDIF.
*- End of Change SDP76880 1/29/2015
  MOVE wacommit TO wa-commit.
ENDFORM.                    "po_calculations

*&---------------------------------------------------------------------*
*&      Form  create_spreadsheet
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_spreadsheet.

  DATA:  BEGIN OF lt_fnames OCCURS 0,
         text(60) TYPE c,
         END OF lt_fnames.

*TR789 Built to match Form  prot_header
  lt_fnames-text = text-100.
  APPEND lt_fnames.
  lt_fnames-text = text-151.
  APPEND lt_fnames.
  lt_fnames-text = text-152.
  APPEND lt_fnames.
  lt_fnames-text = text-102.                                "TR872
  APPEND lt_fnames.                                         "TR872
  lt_fnames-text = text-153.
  APPEND lt_fnames.
  lt_fnames-text = text-154.
  APPEND lt_fnames.
  lt_fnames-text = text-104.
  APPEND lt_fnames.
  lt_fnames-text = text-155.
  APPEND lt_fnames.
  lt_fnames-text = text-156.
  APPEND lt_fnames.
  lt_fnames-text = text-157.
  APPEND lt_fnames.
  lt_fnames-text = text-158.
  APPEND lt_fnames.
  lt_fnames-text = text-159.
  APPEND lt_fnames.
  lt_fnames-text = text-160.
  APPEND lt_fnames.
  lt_fnames-text = text-108.
  APPEND lt_fnames.
  lt_fnames-text = text-109.
  APPEND lt_fnames.
  lt_fnames-text = text-110.
  APPEND lt_fnames.
  lt_fnames-text = text-111.
  APPEND lt_fnames.
  lt_fnames-text = text-161.
  APPEND lt_fnames.
  lt_fnames-text = text-162.
  APPEND lt_fnames.
  lt_fnames-text = text-163.
  APPEND lt_fnames.
  lt_fnames-text = text-164.
  APPEND lt_fnames.
  lt_fnames-text = text-168.                                "TR926
  APPEND lt_fnames.                                         "TR926
  lt_fnames-text = text-169.                                "TR926
  APPEND lt_fnames.                                         "TR926
* BOI by PANUSURI Ticket 87240
*  lt_fnames-text = text-170.                                "TR926
*  APPEND lt_fnames.                                         "TR926
  lt_fnames-text = text-010.
  APPEND lt_fnames.
  lt_fnames-text = text-011.
  APPEND lt_fnames.
* EOI by PANUSURI Ticket 87240
  lt_fnames-text = text-171.                                "TR926
  APPEND lt_fnames.                                         "TR926
  lt_fnames-text = text-172.                                "TR926
  APPEND lt_fnames.                                         "TR926
  lt_fnames-text = text-173.                                "TR926
  APPEND lt_fnames.                                         "TR926
  lt_fnames-text = text-174.                                "TR926
  APPEND lt_fnames.                                         "TR926
  lt_fnames-text = text-175.                                "TR926
  APPEND lt_fnames.                                         "TR926
  lt_fnames-text = text-165.
  APPEND lt_fnames.
  lt_fnames-text = text-166.
  APPEND lt_fnames.
  lt_fnames-text = text-167.
  APPEND lt_fnames.
  lt_fnames-text = text-118.
  APPEND lt_fnames.
  lt_fnames-text = text-119.
  APPEND lt_fnames.
  lt_fnames-text = text-120.
  APPEND lt_fnames.
  lt_fnames-text = 'PO NetValue'.       "(+)SDP76880 1/29/2015
  APPEND lt_fnames.                     "(+)SDP76880 1/29/2015
  lt_fnames-text = text-121.
  APPEND lt_fnames.
  lt_fnames-text = text-122.
  APPEND lt_fnames.
  lt_fnames-text = text-123.
  APPEND lt_fnames.
* BOI by PANUSURI Ticket 87240
  lt_fnames-text = text-008.
  APPEND lt_fnames.
* EOI by PANUSURI Ticket 87240

  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
    EXPORTING
*     file_name                 = 'c:\saptemp' "TR995
      file_name                 = p_file       "TR995
      create_pivot              = '0'
    TABLES
      data_tab                  = wa
      fieldnames                = lt_fnames
    EXCEPTIONS
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_object_method_error   = 4
      ole_object_property_error = 5
      invalid_filename          = 6
      invalid_pivot_fields      = 7
      download_problem          = 8
      OTHERS                    = 9.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    "create_spreadsheet

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
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

*lv_dir = sep_path.
  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path      "lv_dir
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
*&---------------------------------------------------------------------*
*&      Form  alv_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_user_command USING lv_ucomm     LIKE sy-ucomm
                            ls_selfield TYPE slis_selfield.

  IF ls_selfield-tabindex LE 0 OR ls_selfield-sumindex > 0.
    EXIT.
  ENDIF.

  READ TABLE wa INTO wa INDEX ls_selfield-tabindex.

  CASE lv_ucomm.
    WHEN '&IC1'.
      CASE ls_selfield-fieldname.
        WHEN 'EBELN'.
          IF ls_selfield-value IS NOT INITIAL.
            SET PARAMETER ID 'BES' FIELD wa-ebeln.
            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
  ENDCASE.

ENDFORM.                    "alv_user_command
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       Display Top of page
*----------------------------------------------------------------------*
FORM display_top_of_page .
  DATA: lv_lines      TYPE i,
        lv_linesc(10) TYPE c,
        lv_text       TYPE string.
  IF lt_listheader[] IS INITIAL.
    lwa_listheader-typ               ='H'.
    lwa_listheader-info             = w_repttl.
    APPEND lwa_listheader TO lt_listheader.
    CLEAR lwa_listheader.

    lwa_listheader-typ               ='S'.
    lwa_listheader-info             = w_head01.
    APPEND lwa_listheader TO lt_listheader.
    CLEAR lwa_listheader.

    lwa_listheader-typ               ='S'.
    lwa_listheader-info             = w_head02.
    APPEND lwa_listheader TO lt_listheader.
    CLEAR lwa_listheader.

*   Write List header
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = lt_listheader.
  ENDIF.

ENDFORM.                    " DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM build_fieldcatalog .
  PERFORM create_catalog USING : 'BSART'  'Ty'.
  PERFORM create_catalog USING : 'LIFNR'  'Vendor#'.
  PERFORM create_catalog USING : 'NAME'  'Name'.
  PERFORM create_catalog USING : 'EMNFR'  'Ariba CS ID'.
  PERFORM create_catalog USING : 'EBELN'  'PO#'.
  PERFORM create_catalog USING : 'EBELP'  'Item#'.
  PERFORM create_catalog USING : 'BANFN'  'PurReq'.
  PERFORM create_catalog USING : 'LABNR'  'Ackn'.
  PERFORM create_catalog USING : 'BEDNR'  'Track#'.
  PERFORM create_catalog USING : 'EKGRP'  'PGrp'.
  PERFORM create_catalog USING : 'MATKL'  'MGrp'.
  PERFORM create_catalog USING : 'DC'  'DC'.
  PERFORM create_catalog USING : 'FI'  'IC'.
  PERFORM create_catalog USING : 'AEDATEKKO'  'Order Date'.
  PERFORM create_catalog USING : 'EINDT'  'Delivry Dt'.
  PERFORM create_catalog USING : 'KDATB'  'Start Date'.
  PERFORM create_catalog USING : 'KDATE'  'End Date'.
  PERFORM create_catalog USING : 'WERKS'  'Plnt'.
  PERFORM create_catalog USING : 'LGORT'  'Sloc'.
  PERFORM create_catalog USING : 'EMATN'  'Material#'.
  PERFORM create_catalog USING : 'TXZ01'  'Description'.
  PERFORM create_catalog USING : 'KONNR'  'P P Agreement'.
  PERFORM create_catalog USING : 'AFNAM'  'Requisitioner'.
  PERFORM create_catalog USING : 'ZZARIBA_APPR_HDR'  'S R Confirmer(Header)'."(+)PANUSURI Ticket 87240
  PERFORM create_catalog USING : 'ZZARIBA_APPROVER'  'S R Confirmer(Item)'.  "(+)PANUSURI Ticket 87240
*  PERFORM create_catalog USING : 'ZZARIBA_APPROVER'  'S R Confirmer'.       "(-)PANUSURI Ticket 87240
  PERFORM create_catalog USING : 'KOSTL'  'Cost Center'.
  PERFORM create_catalog USING : 'SAKTO'  'G/L Account'.
  PERFORM create_catalog USING : 'AUFNR'  'Order #'.
  PERFORM create_catalog USING : 'PS_PSP_PNR'  'WBS'.
  PERFORM create_catalog USING : 'KNTTP'  'A A Category'.
  PERFORM create_catalog USING : 'MEINS'  'UoM'.
  PERFORM create_catalog USING : 'PEINH'  'Per'.
  PERFORM create_catalog USING : 'BPRME'  'OPU'.
  PERFORM create_catalog USING : 'NETPR'  'Net Price'.
  PERFORM create_catalog USING : 'WAERS'  'PO Cur'.
  PERFORM create_catalog USING : 'MENGE'  'PO Qty'.
  PERFORM create_catalog USING : 'NETWR'  'PO NetValue'.  "(+) SDP76880  1/29/2015
  PERFORM create_catalog USING : 'WEMNG'  'ToBeDeliv'.
  PERFORM create_catalog USING : 'REMNG'  'ToBeInv'.
  PERFORM create_catalog USING : 'COMMIT'  'OutstCom-POCut'.
  PERFORM create_catalog USING : 'SUMLIMIT'  'Overall Limit'. "(+)PANUSURI Ticket 87240

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  CREATE_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_catalog  USING    iv_fieldname TYPE slis_fieldname
                              iv_seltext   TYPE scrtext_l.
  DATA: lwa_fieldcat TYPE slis_fieldcat_alv.

  CLEAR lwa_fieldcat.
  lwa_fieldcat-fieldname = iv_fieldname.
  lwa_fieldcat-seltext_l = iv_seltext.
  APPEND lwa_fieldcat TO lt_fieldcat.

ENDFORM.                    " CREATE_CATALOG
