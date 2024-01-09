*&---------------------------------------------------------------------*
*&  Include           ZMMI001_PO_TO_SALESFORCE_SEL
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZMMI001_PO_TO_SALESFORCE                      *
*& Include Name       :  ZMMI001_PO_TO_SALESFORCE_SEL                  *
*& Author             :  Tawfeeq Ahmad                                 *
*& Creation Date      :  18/12/2019                                    *
*& Change Request     :  CHG0164927                                    *
*& Description        :  SAP PO Data to Sales Force                    *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 18-Dec-2019  AHMADT        D30K930342 CHG0164927 Initial Development *
*                            D30K930336                                *
*                            D30K930452                                *
*                            D30K930545                                *
*                            D30K930541                                *
*&---------------------------------------------------------------------*

TABLES: ekko.

* Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_ebeln  FOR ekko-ebeln,
                s_bsart  FOR ekko-bsart,
                s_bukrs  FOR ekko-bukrs,
                s_ekorg  FOR ekko-ekorg,
                s_aedat  FOR ekko-aedat.
PARAMETERS    : p_bstyp  TYPE bstyp.
PARAMETERS    : r_delta RADIOBUTTON GROUP r1,
                r_full  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF block b1.
