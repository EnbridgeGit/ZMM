*&---------------------------------------------------------------------*
*& Report  ZMMORDERSDATA
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :   ZMMORDERSDATA                                 *
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
* Changed On   Changed By  CTS                Description              *
* ---------------------------------------------------------------------*
* 23-Jan-2020  JOOKONTR  D30K930414 CHG0172306   Initial               *
* 28-Feb-2020  JOOKONTR  D30K930458 CHG0176546 consider only Document  *
*                        Types 'NB','ZF','RP' and Change the file name *
*                        as requested,Change output date format to     *
*                        MM/DD/YYYY                                    *
*&---------------------------------------------------------------------*

REPORT  ZLMMI035_ORDERSDATA.

*&--Data Declarations
INCLUDE ZLMMI035_ORDERSDATA_TOP.
*&-Selection Screen
INCLUDE ZLMMI035_ORDERSDATA_SCR.
*&-Main Logic
INCLUDE ZLMMI035_ORDERSDATA_LGC.
*&-Subroutines forms
INCLUDE ZLMMI035_ORDERSDATA_F01.
