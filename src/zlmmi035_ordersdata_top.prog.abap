*&---------------------------------------------------------------------*
*&  Include           ZMMORDERSDATA_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :   ZMMORDERSDATA                                 *
* Include            :   ZMMORDERSDATA_TOP                             *
* Author             :   Rajeshwar Reddy                               *
* Date               :   23-Jan-2020                                   *
* Technical Contact  :   Rajeshwar Reddy                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Procurement performance report                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 23-Jan-2020  JOOKONTR  D30K930414 CHG0172306   Initial               *
* 28-Feb-2020  JOOKONTR  D30K930458 CHG0176546 consider only Document  *
*                        Types 'NB','ZF','RP' and Change the file name *
*                        as requested,Change output date format to     *
*                        MM/DD/YYYY                                    *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* TABLES                                                               *
*&---------------------------------------------------------------------*
TABLES: ekko,ekpo,eket,ekbe,sscrfields.

*&---------------------------------------------------------------------*
* GLOBAL TYPES                                                         *
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gty_eket,
        ebeln             TYPE eket-ebeln,
        ebelp             TYPE eket-ebelp,
        eindt             TYPE eket-eindt,
        slfdt             TYPE eket-slfdt,
      END OF gty_eket,
      BEGIN OF gty_ekbe,
        ebeln TYPE ekbe-ebeln,
        ebelp TYPE ekbe-ebelp,
        gjahr TYPE ekbe-gjahr,
        vgabe TYPE ekbe-vgabe,
        belnr TYPE ekbe-belnr,
        buzei TYPE ekbe-buzei,
        bwart TYPE ekbe-bwart,
      END OF gty_ekbe,
      BEGIN OF gty_mkpf,
        mblnr TYPE mkpf-mblnr,
        mjahr TYPE mkpf-mjahr,
        bldat TYPE mkpf-bldat,
        usnam TYPE mkpf-usnam,
      END OF gty_mkpf,
      BEGIN OF gty_mseg,
        mblnr             TYPE mseg-mblnr,
        mjahr             TYPE mseg-mjahr,
        zeile             TYPE mseg-zeile,
        ebeln             TYPE mseg-ebeln,
        ebelp             TYPE mseg-ebelp,
        erfmg             TYPE mseg-erfmg,
        budat_mkpf        TYPE mseg-budat_mkpf,
        cpudt_mkpf        TYPE mseg-cpudt_mkpf,
        usnam_mkpf        TYPE mseg-usnam_mkpf,
      END OF gty_mseg,
      BEGIN OF gty_lfa1,
        lifnr             TYPE lfa1-lifnr,
        name1             TYPE lfa1-name1,
      END OF gty_lfa1.

TYPES: BEGIN OF gty_data,
        ebeln             TYPE ekko-ebeln,
        bsart             TYPE ekko-bsart,
        ernam             TYPE ekko-ernam,
        lifnr             TYPE ekko-lifnr,
        ekgrp             TYPE ekko-ekgrp,
        waers             TYPE ekko-waers,
        bedat             TYPE ekko-bedat,
        submi             TYPE ekko-submi,
        inco1             TYPE ekko-inco1,
        zzariba_approver  TYPE ekko-zzariba_approver,
        ebelp             TYPE ekpo-ebelp,
        txz01             TYPE ekpo-txz01,
        matnr             TYPE ekpo-matnr,
        werks             TYPE ekpo-werks,
        lgort             TYPE ekpo-lgort,
        matkl             TYPE ekpo-matkl,
        menge             TYPE ekpo-menge,
        meins             TYPE ekpo-meins,
        netpr             TYPE ekpo-netpr,
        bednr             TYPE ekpo-bednr,
        knttp             TYPE ekpo-knttp,
        konnr             TYPE ekpo-konnr,
        afnam             TYPE ekpo-afnam,
        zekkn             TYPE ekkn-zekkn,
        sakto             TYPE ekkn-sakto,
        kostl             TYPE ekkn-kostl,
        aufnr             TYPE ekkn-aufnr,
        wempf             TYPE ekkn-wempf,
        ps_psp_pnr        TYPE ekkn-ps_psp_pnr,
        nplnr             TYPE ekkn-nplnr,
      END OF gty_data.
TYPES:BEGIN OF gty_orders,
        ebeln             TYPE ekko-ebeln,      "PO#
        ebelp             TYPE ekpo-ebelp,      "Line Item#
*        bedat             TYPE ekko-bedat,     "Order Date "Commented by JOOKONTR for CHG0176546
        bedat(10)         TYPE c,               "Order Date "Added by JOOKONTR for  CHG0176546
        ekgrp             TYPE ekko-ekgrp,      "PGroup
        konnr             TYPE ekpo-konnr,      "Outline Agreement
        name1             TYPE lfa1-name1,      "Vendor Name
        txz01             TYPE ekpo-txz01,      "Short Text
        werks             TYPE ekpo-werks,      "Plant
        netpr             TYPE ekpo-netpr,      "Net Value
        waers             TYPE ekko-waers,      "Currency
        bednr             TYPE ekpo-bednr,      "Shopping Cart Number
        submi             TYPE ekko-submi,      "Collective Number
        matnr             TYPE ekpo-matnr,      "Material #
        matkl             TYPE ekpo-matkl,      "Material Group
        menge             TYPE ekpo-menge,      "PO Qty
        meins             TYPE ekpo-meins,      "Order Unit
        bsart             TYPE ekko-bsart,      "Document Type
        knttp             TYPE ekpo-knttp,      "A A Category
        sakto             TYPE ekkn-sakto,      "G/L Account
        ps_psp_pnr(24)    TYPE c,               "WBS
        kostl             TYPE ekkn-kostl,      "Cost Center
        aufnr             TYPE ekkn-aufnr,      "Order #
        afnam             TYPE ekpo-afnam,      "Requisitioner
        ernam             TYPE ekko-ernam,      "SC Created By
      END OF gty_orders,
      BEGIN OF gty_ontime_del,
        ebeln             TYPE ekko-ebeln,            "PO#
        ebelp             TYPE ekpo-ebelp,            "Line Item#
        ekgrp             TYPE ekko-ekgrp,            "PGroup
*BOC changed by JOOKONTR for  CHG0176546
*        bedat             TYPE ekko-bedat,            "Order Date
*        slfdt             TYPE eket-slfdt,            "Stat Rel Del Date
*        eindt             TYPE eket-eindt,            "Delivery
*        bldat             TYPE mkpf-bldat,            "GR Document Date
*        budat             TYPE mseg-budat_mkpf,       "GR Posting Date
        bedat(10)         TYPE c,                     "Order Date
        slfdt(10)         TYPE c,                     "Stat Rel Del Date
        eindt(10)         TYPE c,                     "Delivery
        bldat(10)         TYPE c,                     "GR Document Date
        budat(10)         TYPE c,                     "GR Posting Date
*EOC changed by JOOKONTR for  CHG0176546
        knttp             TYPE ekpo-knttp,            "A A Category
        ps_psp_pnr(24)    TYPE c,                     "WBS
        werks             TYPE ekpo-werks,            "Plant
        konnr             TYPE ekpo-konnr,            "Outline Agreement
        bsart             TYPE ekko-bsart,            "Document Type
        submi             TYPE ekko-submi,            "Collective Number
        name1             TYPE lfa1-name1,            "Vendor Name
        txz01             TYPE ekpo-txz01,            "Short Text
        menge             TYPE ekpo-menge,            "PO Qty
        erfmg             TYPE mseg-erfmg,            "Received Qty
        meins             TYPE ekpo-meins,            "Order Unit
        usnam             TYPE mkpf-usnam,            "GR Entered By
        bednr             TYPE ekpo-bednr,            "Shopping Cart Number
        matnr             TYPE ekpo-matnr,            "Material #
        matkl             TYPE ekpo-matkl,            "Material Group
        lgort             TYPE ekpo-lgort,            "Storage Location
        netpr             TYPE ekpo-netpr,            "Net Value
        waers             TYPE ekko-waers,            "Currency
        inco1             TYPE ekko-inco1,            "Freight Terms
        afnam             TYPE ekpo-afnam,            "Requisitioner
        wempf             TYPE ekkn-wempf,            "Goods Recipient
        zzariba_approver  TYPE ekko-zzariba_approver, "S R Confirmer(Item)
      END OF gty_ontime_del.

*&---------------------------------------------------------------------*
* GLOBAL INTERNAL TABLES                                               *
*&---------------------------------------------------------------------*
DATA: gt_eket   TYPE TABLE OF gty_eket,
      gt_mseg   TYPE TABLE OF gty_mseg,
      gt_ekbe   TYPE TABLE OF gty_ekbe,
      gt_mkpf   TYPE TABLE OF gty_mkpf,
      gt_lfa1   TYPE TABLE OF gty_lfa1,
      gt_data   TYPE TABLE OF gty_data,
      gt_orders TYPE TABLE OF gty_orders,
      gt_otd    TYPE TABLE OF gty_ontime_del,
      gt_fcat   TYPE slis_t_fieldcat_alv.

*&---------------------------------------------------------------------*
* GLOBAL Variables                                                     *
*&---------------------------------------------------------------------*
DATA: gv_flag     TYPE c,
      gv_path     TYPE string,
      gv_string   TYPE string.

*&---------------------------------------------------------------------*
* GLOBAL constants                                                     *
*&---------------------------------------------------------------------*
CONSTANTS : gc_x            TYPE c VALUE 'X',
            gc_ucomm_onli   TYPE sscrfields-ucomm VALUE 'ONLI',
            gc_m1(2)        TYPE c VALUE 'M1',
            gc_m2(2)        TYPE c VALUE 'M2',
            gc_xlsx(4)      TYPE c VALUE 'XLSX',
            gc_xls(4)      TYPE c VALUE 'XLS'.    "Added by JOOKONTR for CHG0176546
