*&---------------------------------------------------------------------*
*&  Include           ZLMMR002_SRO_SPEND_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLMMR002_SRO_SPEND_TOP                         *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 08-Mar-2016                                    *
*& Object ID          : ACR-159                                        *
*& Application Area   : MM                                             *
*& Description        : Send daily email notification at 80% SRO spend *
*&                      to Requestor, Service Confirmer and Buyer.     *
*&---------------------------------------------------------------------*
************************************************************************
* TYPES DECLARATION
************************************************************************
TYPES: BEGIN OF ty_ekko_ekpo,
         ebeln            TYPE ebeln,
         lifnr            TYPE elifn,
         ekgrp            TYPE bkgrp,
         zzariba_approver TYPE z_ariba_approver,
         ebelp            TYPE ebelp,
         netwr            TYPE bwert,
         erekz            TYPE erekz, "(+)PANUSURI Ticket ACR-1375
         afnam            TYPE afnam,
       END OF ty_ekko_ekpo,
*BOC by PANUSURI Ticket ACR-1375
*       BEGIN OF ty_rseg,
*         belnr            TYPE belnr_d,
*         gjahr            TYPE gjahr,
*         buzei            TYPE rblgp,
*         ebeln            TYPE ebeln,
*         ebelp            TYPE ebelp,
*         wrbtr            TYPE wrbtr,
*         shkzg            TYPE shkzg,
*       END OF ty_rseg,
*EOC by PANUSURI Ticket ACR-1375
*BOI by PANUSURI Ticket ACR-1375
        BEGIN OF ty_ekbe,
         ebeln            TYPE ebeln,
         ebelp            TYPE ebelp,
         gjahr            TYPE mjahr,
         belnr            TYPE mblnr,
         buzei            TYPE mblpo,
         wrbtr            TYPE wrbtr,
         shkzg            TYPE shkzg,
         END OF ty_ekbe,
*EOI by PANUSURI Ticket ACR-1375
       BEGIN OF ty_lfa1,
         lifnr            TYPE lifnr,
         name1            TYPE name1_gp,
       END OF ty_lfa1,
       BEGIN OF ty_po_amt,
         ebeln            TYPE ebeln,
         lifnr            TYPE elifn,
         po_amt           TYPE bwert,
       END OF ty_po_amt,
       BEGIN OF ty_inv_amt,
         ebeln            TYPE ebeln,
         inv_amt          TYPE wrbtr,
       END OF ty_inv_amt,
       BEGIN OF ty_recipients,
         ebeln            TYPE ebeln,
         usrid            TYPE char12,
       END OF ty_recipients,
       BEGIN OF ty_pgrp,
         ebeln             TYPE ebeln,
         ekgrp             TYPE ekgrp,
       END OF ty_pgrp,
       BEGIN OF ty_pgrp_parid,
         ekgrp            TYPE ekgrp,
       END OF ty_pgrp_parid,
       BEGIN OF ty_pgrp_email,
         ekgrp            TYPE ekgrp,
         smtp_addr        TYPE ad_smtpadr,
       END OF ty_pgrp_email,
       BEGIN OF ty_rec_parid,
         bname            TYPE xubname,
       END OF ty_rec_parid,
       BEGIN OF ty_usr21,
         bname            TYPE xubname,
         persnumber       TYPE ad_persnum,
         addrnumber       TYPE ad_addrnum,
       END OF ty_usr21,
       BEGIN OF ty_rec_email,
         addrnumber       TYPE ad_addrnum,
         persnumber       TYPE ad_persnum,
         smtp_addr        TYPE ad_smtpadr,
       END OF ty_rec_email,
       BEGIN OF ty_rec_final,
         ebeln            TYPE ebeln,
         usrid            TYPE char12,
         smtp_addr        TYPE ad_smtpadr,
       END OF ty_rec_final,
       BEGIN OF ty_sro_final,
         ebeln            TYPE ebeln,
         lifnr            TYPE elifn,
         name1            TYPE name1_gp,
         smtp_addr        TYPE ad_smtpadr,
       END OF ty_sro_final.

************************************************************************
* DATA DECLARATION
************************************************************************
DATA: wa_ekko_ekpo        TYPE ty_ekko_ekpo,                "#EC NEEDED
      ta_ekko_ekpo        TYPE STANDARD TABLE OF ty_ekko_ekpo, "#EC NEEDED
      ta_ekko_ekpo_finv   TYPE STANDARD TABLE OF ty_ekko_ekpo,  "(+)PANUSURI Ticket ACR-1375
*      wa_rseg             TYPE ty_rseg,                     "#EC NEEDED
*      ta_rseg             TYPE STANDARD TABLE OF ty_rseg,   "#EC NEEDED
      ta_ekbe             TYPE STANDARD TABLE OF ty_ekbe,   "(+)PANUSURI Ticket ACR-1375
      wa_ekbe             TYPE ty_ekbe,                     "(+)PANUSURI Ticket ACR-1375
      wa_lfa1             TYPE ty_lfa1,                     "#EC NEEDED
      ta_lfa1             TYPE STANDARD TABLE OF ty_lfa1,   "#EC NEEDED
      wa_po_amt           TYPE ty_po_amt,                   "#EC NEEDED
      ta_po_amt           TYPE STANDARD TABLE OF ty_po_amt, "#EC NEEDED
      wa_mul_inv_amt      TYPE ty_inv_amt,                  "#EC NEEDED
      ta_mul_inv_amt      TYPE STANDARD TABLE OF ty_inv_amt, "#EC NEEDED
      wa_inv_amt          TYPE ty_inv_amt,                  "#EC NEEDED
      ta_inv_amt          TYPE STANDARD TABLE OF ty_inv_amt, "#EC NEEDED
      wa_recipients       TYPE ty_recipients,               "#EC NEEDED
      ta_recipients       TYPE STANDARD TABLE OF ty_recipients, "#EC NEEDED
      wa_pgrp             TYPE ty_pgrp,                     "#EC NEEDED
      ta_pgrp             TYPE STANDARD TABLE OF ty_pgrp,   "#EC NEEDED
      wa_pgrp_parid       TYPE ty_pgrp_parid,               "#EC NEEDED
      ta_pgrp_parid       TYPE STANDARD TABLE OF ty_pgrp_parid, "#EC NEEDED
      wa_pgrp_email       TYPE ty_pgrp_email,               "#EC NEEDED
      ta_pgrp_email       TYPE STANDARD TABLE OF ty_pgrp_email, "#EC NEEDED
      wa_rec_parid        TYPE ty_rec_parid,                "#EC NEEDED
      ta_rec_parid        TYPE STANDARD TABLE OF ty_rec_parid, "#EC NEEDED
      wa_usr21            TYPE ty_usr21,                    "#EC NEEDED
      ta_usr21            TYPE STANDARD TABLE OF ty_usr21,  "#EC NEEDED
      wa_rec_email        TYPE ty_rec_email,                "#EC NEEDED
      ta_rec_email        TYPE STANDARD TABLE OF ty_rec_email, "#EC NEEDED
      wa_rec_final        TYPE ty_rec_final,                "#EC NEEDED
      ta_rec_final        TYPE STANDARD TABLE OF ty_rec_final, "#EC NEEDED
      wa_sro_final        TYPE ty_sro_final,                "#EC NEEDED
      ta_sro_final        TYPE STANDARD TABLE OF ty_sro_final. "#EC NEEDED

************************************************************************
* CONSTANTS DECLARATION
************************************************************************
CONSTANTS: co_parid  TYPE memoryid VALUE 'Z_NO_SRO_EM'.
