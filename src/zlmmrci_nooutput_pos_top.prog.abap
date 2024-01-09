*&---------------------------------------------------------------------*
*&  Include           ZLMMRCI_NOOUTPUT_POS_TOP
*&---------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 11-Oct-2018                                          *
* Created By    : SKAKUMANU                                            *
* Correction No : D30K929174                                           *
* Object ID     : CI Item                                              *
* Description   : Report to display POs with no output triggered       *
*----------------------------------------------------------------------*
***************************************************************************
*                         Global declarations                             *
***************************************************************************
TABLES: ekko.

TYPES:BEGIN OF ty_ekko,
  ebeln TYPE  na_objkey,  "Purchasing Document Number
  bukrs TYPE  bukrs,  "Company Code
  bsart TYPE  esart,  "Purchasing Document Type
  aedat	TYPE  erdat,  "Date on Which Record Was Created
  ernam TYPE  ernam,  "Name of Person who Created the Object
  lifnr TYPE  elifn,  "Vendor Account Number
  ekorg TYPE  ekorg,  "Purchasing Organization
  ekgrp TYPE  bkgrp,  "Purchasing Group
  END OF ty_ekko,

  BEGIN OF ty_nast,
    objky	TYPE na_objkey,   "Object key
    END OF ty_nast,

  BEGIN OF ty_final,
    ebeln TYPE  na_objkey,  "Purchasing Document Number
    bukrs TYPE  bukrs,  "Company Code
    bsart TYPE  esart,  "Purchasing Document Type
    ernam TYPE  ernam,  "Name of Person who Created the Object
    lifnr TYPE  elifn,  "Vendor Account Number
    ekorg TYPE  ekorg,  "Purchasing Organization
    ekgrp TYPE  bkgrp,  "Purchasing Group
    aedat	TYPE  erdat,  "Date on Which Record Was Created
    END OF ty_final.

DATA: gt_ekko TYPE TABLE OF ty_ekko,
      gs_ekko LIKE LINE OF gt_ekko,

      gt_nast TYPE TABLE OF ty_nast,
      gs_nast LIKE LINE OF gt_nast,

      gt_final TYPE TABLE OF ty_final,
      gs_final LIKE LINE OF gt_final,

     go_alv TYPE REF TO cl_salv_table,
     go_columns TYPE REF TO cl_salv_columns_table,
     go_column TYPE REF TO cl_salv_column_table.


***************************************************************************
*                         Selection screen                                *
***************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_ekorg FOR ekko-ekorg,
                s_aedat FOR ekko-aedat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
