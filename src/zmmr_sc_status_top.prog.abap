*&---------------------------------------------------------------------*
*&  Include           ZMMR_SC_STATUS_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZMMR_SC_STATUS_TOP                             *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 18-Sep-2014                                    *
*& Object ID          : SDP70149: PM-PR to SRM reference field         *
*& Application Area   : MM                                             *
*& Description        : Report displays Shopping cart status in ECC    *
*&---------------------------------------------------------------------*
TABLES: aufk,
        resb,
        eban,
        iloa,
        afih,
        ekko,
        ekpo,
        ekkn,
        crmd_orderadm_h.

************************************************************************
* DATA DECLARATION
************************************************************************
TYPES: BEGIN OF ty_viaufkst,
       aufnr      TYPE aufnr,
       iwerk      TYPE iwerk,
       auart      TYPE aufart,
       ernam      TYPE auferfnam,
       erdat      TYPE auferfdat,
       loekz      TYPE aufloekz,  "(+)PANUSURI Ticket ACR-56
*       zzdue_date TYPE z_due_date,
       swerk      TYPE swerk,
       END OF ty_viaufkst,
       BEGIN OF ty_resb,
       rsnum      TYPE rsnum,
       rspos      TYPE rspos,
       matnr      TYPE matnr,
       bdmng      TYPE bdmng,
       END OF ty_resb,
       BEGIN OF ty_ebkn,
       banfn      TYPE banfn,
       bnfpo      TYPE bnfpo,
       aufnr      TYPE aufnr,
       END OF ty_ebkn,
       BEGIN OF ty_eban,
       banfn      TYPE banfn,
       bnfpo      TYPE bnfpo,
       loekz      TYPE eloek,"(+)PANUSURI Ticket ACR-56
       ernam      TYPE ernam,"(+)PANUSURI Ticket 75531
       afnam      TYPE afnam,"(+)PANUSURI Ticket 75531
       txz01      TYPE txz01,
       matnr      TYPE matnr,
       bednr      TYPE bednr,"(+)PANUSURI Ticket 75531
       menge      TYPE bamng,
       badat      TYPE badat,
       lfdat      TYPE eindt,"(+)PANUSURI Ticket 75531
       preis      TYPE bapre,
       ebeln      TYPE bstnr,"(+)PANUSURI Ticket 75531
       ebelp      TYPE bstpo,"(+)PANUSURI Ticket 75531
       arsnr      TYPE arsnr,
       arsps      TYPE arsps,
       idnlf       TYPE idnlf,    "(+)PANUSURI Ticket ACR-56
       END OF ty_eban,
       BEGIN OF ty_cskt,
       kokrs      TYPE kokrs,
       kostl      TYPE kostl,
       ktext      TYPE ktext,
       END OF ty_cskt,
       BEGIN OF ty_ekko_ekpo_ekkn,
       ebeln      TYPE ebeln,
       aedat      TYPE erdat,
       ernam      TYPE ernam,
       lifnr      TYPE elifn,
       ekgrp      TYPE bkgrp,
       waers      TYPE waers,
       zzariba_approver TYPE z_ariba_approver,
       ebelp      TYPE ebelp,
       loekz      TYPE eloek, "(+)PANUSURI Ticket ACR-56
       txz01      TYPE txz01,
       matnr      TYPE matnr,
       bednr      TYPE bednr,
       menge      TYPE bstmg,
       meins      TYPE bstme,
       netpr      TYPE bprei,
       knttp      TYPE knttp,
       banfn      TYPE banfn,
       bnfpo      TYPE bnfpo,
       afnam      TYPE afnam,
       sakto      TYPE saknr,
       kostl      TYPE kostl,
       kokrs      TYPE kokrs,
       ps_psp_pnr TYPE ps_psp_pnr,
       END OF ty_ekko_ekpo_ekkn,
       BEGIN OF ty_lfa1,
       lifnr      TYPE lifnr,
       name1      TYPE name1_gp,
       END OF ty_lfa1,
       BEGIN OF ty_sc_details.
        INCLUDE STRUCTURE zsc_details.
TYPES: END OF ty_sc_details,
       BEGIN OF ty_sc_pr_tmp,
       ext_demid     TYPE banfn,
       ext_dem_posid TYPE bnfpo,
       be_obj_item   TYPE ebelp,
       be_object_id  TYPE ebeln,
       END OF ty_sc_pr_tmp,
       BEGIN OF ty_sc_po_tmp,
       be_object_id  TYPE ebeln,
       be_obj_item   TYPE ebelp,
       END OF ty_sc_po_tmp,
       BEGIN OF ty_output,
       aufnr      TYPE aufnr,
       erdat      TYPE auferfdat,
       loekz_wo   TYPE aufloekz,  "(+)PANUSURI Ticket ACR-56
*       zzdue_date TYPE z_due_date,
       auart      TYPE aufart,
       wo_mat     TYPE matnr,
       ernam      TYPE auferfnam,
       swerk      TYPE swerk,
       iwerk      TYPE iwerk,
       banfn      TYPE banfn,
       bnfpo      TYPE bnfpo,
       pr_mat     TYPE matnr,
       pr_txt     TYPE txz01,
       badat      TYPE badat,
*BOI by PANUSURI Ticket 75531
       lfdat      TYPE eindt,
       prnam      TYPE afnam,
       bednr      TYPE bednr,
       prcrt      TYPE ernam,
*EOI by PANUSURI Ticket 75531
*BOI by PANUSURI Ticket ACR-56
       idnlf       TYPE idnlf,
       loekz       TYPE eloek,
*EOI by PANUSURI Ticket ACR-56
       bdmng      TYPE bdmng,
       srm_qty    TYPE bstmg,
       pr_qty     TYPE bamng,
       po_qty     TYPE bstmg,
       meins      TYPE bstme,
       gross_price TYPE bbwert,
       preis      TYPE bapre,
       netpr      TYPE bprei,
       waers      TYPE waers,
       ebeln      TYPE ebeln,
       ebelp      TYPE ebelp,
       loekz_po   TYPE eloek, "(+)PANUSURI Ticket ACR-56
       aedat      TYPE erdat,
       po_mat     TYPE matnr,
       po_txt     TYPE txz01,
       knttp      TYPE knttp,
       kostl      TYPE kostl,
       ktext      TYPE ktext,
       sakto      TYPE saknr,
       wbs        TYPE ps_psp_pnr,
       ekgrp      TYPE ekgrp,
       zzariba_approver TYPE z_ariba_approver,
       afnam      TYPE afnam,
**--START OF CHANGES BY AKMADASU FOR CHG0116780
       NAME_TEXT  type adrp-NAME_TEXT,
       SMTP_ADDR  type adr6-SMTP_ADDR,
**--end OF CHANGES BY AKMADASU FOR CHG0116780
       lifnr      TYPE elifn,
       name1      TYPE name1_gp,
       object_id  TYPE crmt_object_id_db,
       number_int TYPE crmt_item_no,
       stat       TYPE char30,
       crt_date   TYPE crmt_posting_date,
       crt_by     TYPE crmt_created_by,
       cur_appr   TYPE char12,
       next_appr  TYPE char12,
       END OF ty_output.
TYPES: ty_date_range TYPE RANGE OF sy-datum.
DATA: lt_ucomm       TYPE RANGE OF sy-ucomm,
      lwa_ucomm      LIKE LINE OF lt_ucomm,
      lwa_viaufkst   TYPE ty_viaufkst,
      lt_viaufkst    TYPE STANDARD TABLE OF ty_viaufkst,
      lwa_resb       TYPE ty_resb,
      lt_resb        TYPE STANDARD TABLE OF ty_resb,
      lwa_ebkn       TYPE ty_ebkn,
      lt_ebkn        TYPE STANDARD TABLE OF ty_ebkn,
      lwa_eban       TYPE ty_eban,
      lt_eban        TYPE STANDARD TABLE OF ty_eban,
      lwa_cskt       TYPE ty_cskt,
      lt_cskt        TYPE STANDARD TABLE OF ty_cskt,
      lwa_ekko_ekpo_ekkn  TYPE ty_ekko_ekpo_ekkn,
      lt_ekko_ekpo_ekkn   TYPE STANDARD TABLE OF ty_ekko_ekpo_ekkn,
      lwa_lfa1       TYPE ty_lfa1,
      lt_lfa1        TYPE STANDARD TABLE OF ty_lfa1,
      lwa_output     TYPE ty_output,
      lt_output      TYPE STANDARD TABLE OF ty_output,
      lwa_sc_details TYPE ty_sc_details,
      lt_sc_details  TYPE STANDARD TABLE OF ty_sc_details,
      lwa_sc_pr_tmp  TYPE ty_sc_pr_tmp,
      lt_sc_pr_tmp   TYPE STANDARD TABLE OF ty_sc_pr_tmp,
      lwa_sc_po_tmp  TYPE ty_sc_po_tmp,
      lt_sc_po_tmp   TYPE STANDARD TABLE OF ty_sc_po_tmp,
      lv_wo          TYPE char1,
      lv_pr          TYPE char1,
      lv_po          TYPE char1,
      lv_sc          TYPE char1,
      lv_rfcdest     TYPE rfcdisplay-rfcdest,
      lv_object_id   TYPE char10,
      lv_logsys      TYPE tbdls-logsys,
      lwa_listheader TYPE slis_listheader,
      lt_listheader  TYPE slis_t_listheader,
      lt_fieldcat    TYPE slis_t_fieldcat_alv,
      lwa_layout     TYPE slis_layout_alv,
      lwa_variant    TYPE disvariant.

CONSTANTS: lc_prosrm   TYPE zvarsys-programm VALUE 'LOGSYS',
           lc_varsrm   TYPE zvarsys-varname  VALUE 'SRM_RFC',
           lc_varnum   TYPE zvarsys-varnum   VALUE '1'.

************************************************************************
*SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_aufnr  FOR aufk-aufnr MODIF ID m1. "Order Number
SELECT-OPTIONS: s_erdat  FOR aufk-erdat NO-EXTENSION. "Created on
*SELECT-OPTIONS: s_ddate  FOR aufk-zzdue_date MODIF ID m1. "Due date field
SELECT-OPTIONS: s_auart  FOR aufk-auart  MODIF ID m1. "Order Type
SELECT-OPTIONS: s_womat  FOR resb-matnr  MODIF ID m1. "Material number
SELECT-OPTIONS: s_ernam  FOR aufk-ernam MODIF ID m1. "Entered by
SELECT-OPTIONS: s_swerk  FOR iloa-swerk MODIF ID m1. "Maintenance plant
SELECT-OPTIONS: s_iwerk  FOR afih-iwerk MODIF ID m1. "Maintenance Planning Plant
SELECTION-SCREEN: END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-005.
SELECT-OPTIONS: s_banfn  FOR eban-banfn MODIF ID m1. "Purchase Requisition Number
SELECT-OPTIONS: s_badat  FOR eban-badat NO-EXTENSION. "Requisition (Request) Date
*BOI by PANUSURI Ticket 75531
SELECT-OPTIONS: s_prnam  FOR eban-afnam MODIF ID m1. "Name of Requisitioner/Requester
SELECT-OPTIONS: s_bednr  FOR eban-bednr MODIF ID m1. "Requirement Tracking Number
SELECT-OPTIONS: s_prcrt  FOR eban-ernam MODIF ID m1. "Name of Person who Created the Object
*EOI by PANUSURI Ticket 75531
SELECTION-SCREEN: END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-006.
SELECT-OPTIONS: s_ebeln  FOR ekko-ebeln MODIF ID m1.  "Purchasing Document Number
SELECT-OPTIONS: s_aedat  FOR ekko-aedat NO-EXTENSION.  "Date on Which Record Was Created
SELECT-OPTIONS: s_crnam  FOR ekko-ernam MODIF ID m1.  "Name of Person who Created the Object
SELECT-OPTIONS: s_matnr  FOR ekpo-matnr MODIF ID m1.  "Material Number
SELECT-OPTIONS: s_wbs    FOR ekkn-ps_psp_pnr MODIF ID m1."Work Breakdown Structure Element (WBS Element)
SELECT-OPTIONS: s_kostl  FOR ekkn-kostl MODIF ID m1.  "Cost Center
SELECT-OPTIONS: s_knttp  FOR ekpo-knttp MODIF ID m1.  "Account Assignment Category
SELECT-OPTIONS: s_ekgrp  FOR ekko-ekgrp MODIF ID m1.  "Purchasing Group
SELECT-OPTIONS: s_arbap  FOR ekko-zzariba_approver MODIF ID m1. "Service Confirmer
SELECT-OPTIONS: s_afnam  FOR ekpo-afnam MODIF ID m1.  "Name of Requisitioner/Requester
SELECT-OPTIONS: s_lifnr  FOR ekko-lifnr MODIF ID m1.  "Vendor Account Number
SELECTION-SCREEN: END OF BLOCK b3.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-007.
SELECT-OPTIONS: s_objid  FOR crmd_orderadm_h-object_id MODIF ID m1. "Transaction Number
SELECT-OPTIONS: s_cdate  FOR crmd_orderadm_h-posting_date NO-EXTENSION."Posting Date for a Business Transaction
SELECT-OPTIONS: s_crtby  FOR crmd_orderadm_h-created_by MODIF ID m1."User that Created the Transaction
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-t01.
PARAMETERS: p_layout TYPE disvariant-variant.  "Select layout
SELECTION-SCREEN END OF BLOCK b5 .
