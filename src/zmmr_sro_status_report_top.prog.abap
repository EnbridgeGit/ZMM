*&---------------------------------------------------------------------*
*&  Include            ZMMR_SRO_STATUS_REPORT_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZMMR_SRO_STATUS_REPORT_TOP                     *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 20-Dec-2013                                    *
*& Object ID          : R_PTP_MM_0001_SRO Status Report                *
*& Application Area   : MM                                             *
*& Description        : Display SRO data retrieved from cross functional
*                       modules like PS,MM,AP in a single report.      *
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.
TABLES: ekko,
        ekpo,
        proj,
        ekkn.

************************************************************************
* DATA DECLARATION
************************************************************************
TYPES: BEGIN OF ty_ekko,
       ebeln            TYPE ebeln,
       bsart            TYPE esart,
       lifnr            TYPE elifn,
       ekgrp            TYPE bkgrp,
       kdatb            TYPE kdatb,
       kdate            TYPE kdate,
       zzariba_approver TYPE z_ariba_approver,
       END OF ty_ekko,
       BEGIN OF ty_lfa1,
       lifnr TYPE lifnr,
       name1 TYPE name1_gp,
       END OF ty_lfa1,
       BEGIN OF ty_ekpo,
       ebeln  TYPE ebeln,
       ebelp  TYPE ebelp,
       loekz  TYPE eloek,
       txz01  TYPE txz01,
       bukrs  TYPE bukrs,
       bednr  TYPE bednr,
       netwr  TYPE bwert,
       elikz  TYPE elikz,
       erekz  TYPE erekz,
       wepos  TYPE wepos,
       packno	TYPE packno,
       afnam  TYPE afnam,
       END OF ty_ekpo,
       BEGIN OF ty_esuh,
       packno     TYPE packno,
       sumlimit   TYPE sumlimit,
       commitment TYPE commitment,
       actvalue   TYPE actvalue,
       END OF ty_esuh,
       BEGIN OF ty_ekkn,
       ebeln      TYPE ebeln,
       ebelp      TYPE ebelp,
       sakto      TYPE saknr,
       kostl      TYPE kostl,
       ps_psp_pnr	TYPE ps_psp_pnr,
       END OF ty_ekkn,
       BEGIN OF ty_prps,
       pspnr    TYPE ps_posnr,
       posid    TYPE ps_posid,
       psphi    TYPE ps_psphi,
       pspnr_pr TYPE ps_intnr,
       post1    TYPE ps_post1,
       END OF ty_prps,
       BEGIN OF ty_ekbe,
       ebeln    TYPE ebeln,
       ebelp    TYPE ebelp,
       vgabe    TYPE vgabe,
       gjahr    TYPE mjahr,
       belnr    TYPE mblnr,
       buzei    TYPE mblpo,
       END OF ty_ekbe,
       BEGIN OF ty_ekbe_tmp,
       ebeln    TYPE ebeln,
       ebelp    TYPE ebelp,
       vgabe    TYPE vgabe,
       gjahr    TYPE mjahr,
       belnr    TYPE mblnr,
       buzei    TYPE rblgp,
       END OF ty_ekbe_tmp,
       BEGIN OF ty_rbco,
       belnr      TYPE belnr_d,
       gjahr      TYPE gjahr,
       buzei      TYPE rblgp,
       kostl      TYPE kostl,
       ps_psp_pnr TYPE ps_psp_pnr,
       saknr      TYPE saknr,
       wrbtr      TYPE rbco-wrbtr,
       shkzg      TYPE rbco-shkzg,
       xunpl      TYPE rbco-xunpl,
       END OF ty_rbco,
       BEGIN OF ty_rbkp,
       belnr TYPE re_belnr,
       gjahr TYPE gjahr,
       blart TYPE blart,
       xblnr TYPE xblnr1,
       END OF ty_rbkp,
       BEGIN OF ty_rseg,
       belnr TYPE belnr_d,
       gjahr TYPE gjahr,
       buzei TYPE rblgp,
       wrbtr TYPE wrbtr,
       shkzg TYPE shkzg,
       END OF ty_rseg,
       BEGIN OF ty_output,
       afnam             TYPE afnam,
       ekgrp             TYPE bkgrp,
       ebeln             TYPE ebeln,
       txz01             TYPE txz01,
       name1             TYPE name1_gp,
       bukrs             TYPE bukrs,
       pspnr_pr(24)      TYPE c,"ps_intnr,
       post1             TYPE ps_post1,
       ps_psp_pnr(24)    TYPE c," ps_psp_pnr,
       sakto             TYPE saknr,
       kostl             TYPE kostl,
       ps_psp_pnr_in(24) TYPE c,"ps_psp_pnr,
       saknr             TYPE saknr,
       kostl_in          TYPE kostl,
       kdatb(16)         TYPE c,"kdatb,
       kdate(14)         TYPE c,"kdate,
       zzariba_approver  TYPE z_ariba_approver,
       belnr             TYPE xblnr1,
       erekz             TYPE erekz,
       blart             TYPE blart,
       xblnr             TYPE xblnr1,
       netwr             TYPE bwert,
       wrbtr             TYPE wrbtr,
       rem_bal           TYPE wrbtr,
       per_spent         TYPE i,
       sumlimit          TYPE sumlimit,
       commitment        TYPE commitment,
       loekz(10)         TYPE c,"eloek,
       bednr             TYPE bednr,
       END OF ty_output.

DATA: lt_ekko         TYPE STANDARD TABLE OF ty_ekko,
      lwa_ekko        TYPE ty_ekko,
      lt_lfa1         TYPE STANDARD TABLE OF ty_lfa1,
      lwa_lfa1        TYPE ty_lfa1,
      lt_ekpo         TYPE STANDARD TABLE OF ty_ekpo,
      lwa_ekpo        TYPE ty_ekpo,
      lt_esuh         TYPE STANDARD TABLE OF ty_esuh,
      lwa_esuh        TYPE ty_esuh,
      lt_ekkn         TYPE STANDARD TABLE OF ty_ekkn,
      lwa_ekkn        TYPE ty_ekkn,
      lt_prps         TYPE STANDARD TABLE OF ty_prps,
      lwa_prps        TYPE ty_prps,
      lt_ekbe         TYPE STANDARD TABLE OF ty_ekbe,
      lwa_ekbe        TYPE ty_ekbe,
      lt_ekbe_tmp     TYPE STANDARD TABLE OF ty_ekbe_tmp,
      lwa_ekbe_tmp    TYPE ty_ekbe_tmp,
      lt_rbco         TYPE STANDARD TABLE OF ty_rbco,
      lwa_rbco        TYPE ty_rbco,
      lt_rbkp         TYPE STANDARD TABLE OF ty_rbkp,
      lwa_rbkp        TYPE ty_rbkp,
      lt_rseg         TYPE STANDARD TABLE OF ty_rseg,
      lwa_rseg        TYPE ty_rseg,
      lt_output       TYPE STANDARD TABLE OF ty_output,
      lwa_output      TYPE ty_output,
      lwa_output1     TYPE ty_output,
      lwa_listheader  TYPE slis_listheader,
      lt_listheader   TYPE slis_t_listheader,
      lt_fieldcat     TYPE slis_t_fieldcat_alv,
      lt_fieldcat_exl TYPE lvc_t_fcat,
      lit_sort        TYPE slis_t_sortinfo_alv,
      lwa_sort        TYPE slis_sortinfo_alv,
      lwa_layout      TYPE slis_layout_alv.

************************************************************************
*SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_lifnr FOR ekko-lifnr,
                s_bukrs FOR ekko-bukrs NO INTERVALS,
                s_bsart FOR ekko-bsart OBLIGATORY DEFAULT 'ZF',
                s_ekgrp FOR ekko-ekgrp,
                s_pspnr FOR proj-pspnr,
                s_wbs   FOR ekkn-ps_psp_pnr,
                s_kostl FOR ekkn-kostl,
                s_knttp FOR ekpo-knttp,
                s_sakto FOR ekkn-sakto,
                s_kdate FOR ekko-kdate,
                s_ebeln FOR ekko-ebeln,
                s_bedat FOR ekko-bedat.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
* DELIVERY
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-003.
SELECTION-SCREEN COMMENT 20(13) text-004 FOR FIELD p_delall.
PARAMETER:       p_delall  RADIOBUTTON                     GROUP delr.
SELECTION-SCREEN COMMENT 45(22) text-005 FOR FIELD p_delvry.
PARAMETER:       p_delvry  RADIOBUTTON                     GROUP delr.
SELECTION-SCREEN COMMENT 79(23) text-006 FOR FIELD p_undelr.
PARAMETER:       p_undelr  RADIOBUTTON                     GROUP delr.
SELECTION-SCREEN END OF LINE.

* INVOICES
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-007.
SELECTION-SCREEN COMMENT 20(13) text-004 FOR FIELD p_finall.
PARAMETER:       p_finall  RADIOBUTTON                     GROUP finl.
SELECTION-SCREEN COMMENT 45(22) text-008 FOR FIELD p_fin.
PARAMETER:       p_fin     RADIOBUTTON                     GROUP finl.
SELECTION-SCREEN COMMENT 79(23) text-009 FOR FIELD p_nonfin.
PARAMETER:       p_nonfin  RADIOBUTTON                     GROUP finl.
SELECTION-SCREEN END OF LINE.

* DELETED
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-010.
SELECTION-SCREEN COMMENT 20(13) text-004 FOR FIELD p_dltall.
PARAMETER:       p_dltall  RADIOBUTTON                     GROUP delt.
SELECTION-SCREEN COMMENT 45(22) text-011 FOR FIELD p_delt.
PARAMETER:       p_delt     RADIOBUTTON                    GROUP delt.
SELECTION-SCREEN COMMENT 79(23) text-012 FOR FIELD p_nondel.
PARAMETER:       p_nondel  RADIOBUTTON                     GROUP delt.
SELECTION-SCREEN END OF LINE.

* GOODS RECEIPT
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-013.
SELECTION-SCREEN COMMENT 20(13) text-004 FOR FIELD p_grall.
PARAMETER:       p_grall  RADIOBUTTON                       GROUP grin.
SELECTION-SCREEN COMMENT 45(22) text-014 FOR FIELD p_gron.
PARAMETER:       p_gron     RADIOBUTTON                     GROUP grin.
SELECTION-SCREEN COMMENT 79(23) text-015 FOR FIELD p_groff.
PARAMETER:       p_groff  RADIOBUTTON                       GROUP grin.
SELECTION-SCREEN END OF LINE.

* OUTSTANDING COMMITMENT
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(15) text-016.
SELECTION-SCREEN COMMENT 20(13) text-004 FOR FIELD p_comall.
PARAMETER:       p_comall  RADIOBUTTON                      GROUP comm.
SELECTION-SCREEN COMMENT 45(22) text-017 FOR FIELD p_compos.
PARAMETER:       p_compos  RADIOBUTTON                      GROUP comm.
SELECTION-SCREEN COMMENT 79(23) text-018 FOR FIELD p_comneg.
PARAMETER:       p_comneg  RADIOBUTTON                      GROUP comm.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-019.
PARAMETERS:      p_rprt RADIOBUTTON GROUP rbcr,     "Print report
                 p_excl RADIOBUTTON GROUP rbcr.     "Excel spreadsheet
PARAMETER: p_file TYPE  rlgrap-filename MODIF ID clr.
SELECTION-SCREEN END OF BLOCK b3.
