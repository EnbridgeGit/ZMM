*&---------------------------------------------------------------------*
*&  Include           ZLMMR012_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLMMR012_TOP                                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 19-DEC-2013                                    *
*& Object ID          : FS59177 Report: Vendor Delivery Assessment     *
*& Application Area   : MM                                             *
*& Description        : This report provides both the PO Delivery date *
*&                      with the Goods Receipt date for tracking Vendor*
*&                      delivery performance.                          *
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.
TABLES: eket,
        lfa1,
        ekpo.

************************************************************************
* DATA DECLARATION
************************************************************************
TYPES: BEGIN OF ty_ekko,
       ebeln TYPE ebeln,
       lifnr TYPE elifn,
       END OF ty_ekko,
       BEGIN OF ty_lfa1,
       lifnr TYPE lifnr,
       name1 TYPE name1_gp,
       END OF ty_lfa1,
       BEGIN OF ty_ekpo,
       ebeln TYPE ebeln,
       ebelp TYPE ebelp,
       matnr TYPE matnr,  "(+)PANUSURI Ticket 88758
       werks TYPE ewerk,
       END OF ty_ekpo,
       BEGIN OF ty_eket,
       ebeln TYPE ebeln,
       ebelp TYPE ebelp,
       eindt TYPE eindt,
       END OF ty_eket,
       BEGIN OF ty_ekbe,
       ebeln TYPE ebeln,
       ebelp TYPE ebelp,
       budat TYPE budat,
       END OF ty_ekbe,
       BEGIN OF ty_output,
       lifnr TYPE elifn,
       name1 TYPE name1_gp,
       ebeln TYPE ebeln,
       ebelp TYPE ebelp,
       eindt TYPE eindt,
       budat TYPE budat,
       matnr TYPE matnr,  "(+)PANUSURI Ticket 88758
       werks TYPE ewerk,  " COG
       END OF ty_output.

DATA: lt_ekko        TYPE STANDARD TABLE OF ty_ekko,
      lwa_ekko       TYPE ty_ekko,
      lt_lfa1        TYPE STANDARD TABLE OF ty_lfa1,
      lwa_lfa1       TYPE ty_lfa1,
      lt_ekpo        TYPE STANDARD TABLE OF ty_ekpo,
      lwa_ekpo       TYPE ty_ekpo,
      lt_eket        TYPE STANDARD TABLE OF ty_eket,
      lwa_eket       TYPE ty_eket,
      lt_ekbe        TYPE STANDARD TABLE OF ty_ekbe,
      lwa_ekbe       TYPE ty_ekbe,
      lt_output      TYPE STANDARD TABLE OF ty_output,
      lwa_output     TYPE ty_output,
      lwa_listheader TYPE slis_listheader,
      lt_listheader  TYPE slis_t_listheader,
      lt_fieldcat    TYPE slis_t_fieldcat_alv,
      lit_sort       TYPE slis_t_sortinfo_alv,
      lwa_sort       TYPE slis_sortinfo_alv,
      lwa_layout     TYPE slis_layout_alv.

************************************************************************
*SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_ddate FOR eket-eindt OBLIGATORY. "Delivey Date
SELECT-OPTIONS: s_vnum  FOR lfa1-lifnr.            "Vendor number
SELECT-OPTIONS: s_werks FOR ekpo-werks.            "Plant
SELECTION-SCREEN: END OF BLOCK b1.
