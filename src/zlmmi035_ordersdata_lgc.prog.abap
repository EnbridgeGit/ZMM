*&---------------------------------------------------------------------*
*&  Include           ZMMORDERSDATA_LGC
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :   ZMMORDERSDATA                                 *
* Include            :   ZMMORDERSDATA_LGC                             *
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
*&---------------------------------------------------------------------*

START-OF-SELECTION.

*&---------------------------------------------------------------------*
* GET PO DATA                                                          *
*&---------------------------------------------------------------------*
  PERFORM get_data.

*&---------------------------------------------------------------------*
* PREPARE DATA                                                         *
*&---------------------------------------------------------------------*
  PERFORM prep_data.

*&---------------------------------------------------------------------*
* SAVE DATA                                                            *
*&---------------------------------------------------------------------*
  IF rb_app EQ abap_true.
*&-Save data to Application Server
    PERFORM save_to_app.
  ELSEIF rb_prs EQ abap_true.
*&-Save data to Local system
    IF gt_orders[] IS NOT INITIAL.
      PERFORM f_transfer_data_pres USING gt_orders[] p_prs.
    ELSEIF gt_otd[] IS NOT INITIAL.
      PERFORM f_transfer_data_pres USING gt_otd[] p_prs.
    ENDIF.

  ELSEIF rb_alv EQ abap_true.
*&-Display ALV Output
    PERFORM alv_output.
  ENDIF.
