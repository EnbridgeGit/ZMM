*----------------------------------------------------------------------*
* Report Name: ZLMMI_INVOICE_ATTCHMNT_F01
* Author:	     KBANERJEE-Kaushiki Banerjee
* Date:	       September 4th,2018
*
* Logical Database: NA
* SAPScript name:   NA
* Application Area: MM
* Description:  This tool automates the process of downloading
*               archived or non-archived attachments of invoices
*               Invoices are created through goods recipt if PO.
*----------------------------------------------------------------------*
* Modifications
* Version No:  Date:            Modified By:  Ticket:      Correction:
* Ver. 01
* Description:
* Ver. 02
* Description:
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_FILE_F4
*&---------------------------------------------------------------------*
*      Get F4 help for browsing the location to download invoices
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_file_f4 .
  DATA : lv_folder TYPE string,
         lv_title  TYPE string.
  lv_title = text-t02.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = lv_title
      initial_folder  = 'C:'
    CHANGING
      selected_folder = lv_folder.

  p_lpath = lv_folder .
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.                    " F_FILE_F4
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_DATA
*&---------------------------------------------------------------------*
*  Create instance of class ZLMM_INVOICE_ATTACH_DWNLD to dwnload
* invoice attachments
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_process_data .
  CREATE OBJECT o_invoice_attach_dwnld.
  DATA:lr_invoices TYPE idfiwt_t_belnr,
       lwa_invoices TYPE idfiwt_s_belnr.
  DATA:lv_rcode TYPE subrc.

  REFRESH lr_invoices.
*Get all invoice sentered in selection screen  in a range table
  IF s_invce IS NOT INITIAL.
    LOOP AT s_invce.
      lwa_invoices-low    = s_invce-low.
      lwa_invoices-high   = s_invce-high.
      lwa_invoices-sign   = s_invce-sign.
      lwa_invoices-option = s_invce-option.
      APPEND lwa_invoices TO lr_invoices.
      CLEAR lwa_invoices.
    ENDLOOP.
  ENDIF.
*Validate the invoice sentered by user
  CALL METHOD o_invoice_attach_dwnld->validations
    EXPORTING
      invoices    = lr_invoices
      fiscal_yr   = p_fscyr
    IMPORTING
      return_code = lv_rcode.
  IF lv_rcode IS INITIAL.
*Call method to download attachments
    CALL METHOD o_invoice_attach_dwnld->dwnld_attachments
      EXPORTING
        filepath = p_lpath.
  ENDIF.
ENDFORM.                    " F_PROCESS_DATA
