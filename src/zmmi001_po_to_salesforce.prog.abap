*&---------------------------------------------------------------------*
*& Report  ZMMI001_PO_TO_SALESFORCE
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZMMI001_PO_TO_SALESFORCE                      *
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

REPORT  ZMMI001_PO_TO_SALESFORCE.

INCLUDE ZMMI001_PO_TO_SALESFORCE_SEL.
INCLUDE ZMMI001_PO_TO_SALESFORCE_TOP.
INCLUDE ZMMI001_PO_TO_SALESFORCE_MAIN.
