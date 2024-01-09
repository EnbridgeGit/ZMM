*&---------------------------------------------------------------------*
*&  Include           ZLMMR011_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLMMR011_TOP                                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 20-DEC-2013                                    *
*& Object ID          : 59211: Report Storage Location MRP Information *
*& Application Area   : MM                                             *
*& Description        : Maintain/monitor the MRP settings at the storage
*&                      location level                                 *
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.
TABLES: mard.

************************************************************************
* DATA DECLARATION
************************************************************************
TYPES: BEGIN OF ty_mard,
       matnr TYPE matnr,
       werks TYPE werks_d,
       lgort TYPE lgort_d,
       labst TYPE labst,
       diskz TYPE diskz,
       lminb TYPE lminb,
       lbstf TYPE lbstf,
       lgpbe TYPE lgpbe,
       END OF ty_mard,
       BEGIN OF ty_mara,
       matnr TYPE matnr,
       meins TYPE meins,
       END OF ty_mara,
*BOI by PANUSURI ticket 59211
       BEGIN OF ty_t006a,
       msehi TYPE msehi,
       mseh3 TYPE mseh3,
       END OF ty_t006a,
*EOI by PANUSURI ticket 59211
       BEGIN OF ty_makt,
       matnr TYPE matnr,
       maktg TYPE maktg,
       END OF ty_makt,
       BEGIN OF ty_output,
       werks TYPE werks_d,
       lgort TYPE lgort_d,
       matnr TYPE matnr,
       maktg TYPE maktg,
       labst TYPE labst,
*       meins TYPE meins, "(-)PANUSURI ticket 59211
       mseh3 TYPE mseh3,  "(+)PANUSURI ticket 59211
       diskz TYPE diskz,
       lminb TYPE lminb,
       lbstf TYPE lbstf,
       lgpbe TYPE lgpbe,
       END OF ty_output.

DATA: lt_mard        TYPE STANDARD TABLE OF ty_mard,
      lwa_mard       TYPE ty_mard,
      lt_mara        TYPE STANDARD TABLE OF ty_mara,
      lwa_mara       TYPE ty_mara,
*BOI by PANUSURI ticket 59211
      lt_t006a       TYPE STANDARD TABLE OF ty_t006a,"
      lwa_t006a      TYPE ty_t006a,
*EOI by PANUSURI ticket 59211
      lt_makt        TYPE STANDARD TABLE OF ty_makt,
      lwa_makt       TYPE ty_makt,
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
SELECT-OPTIONS: s_werks  FOR mard-werks. "Plant
SELECT-OPTIONS: s_lgort  FOR mard-lgort. "Storage Location
SELECT-OPTIONS: s_matnr  FOR mard-matnr. "Material Number
SELECTION-SCREEN: END OF BLOCK b1.
