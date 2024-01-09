*----------------------------------------------------------------------*
* Report Name: ZLMMI_INVOICE_ATTCHMNT_SCR
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
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS:s_invce FOR gv_belnr MATCHCODE OBJECT fikr_elm_belnr OBLIGATORY.
PARAMETERS: p_fscyr   TYPE vbrk-gjahr OBLIGATORY.
PARAMETERS: p_lpath LIKE rlgrap-filename DEFAULT 'C:'.
SELECTION-SCREEN END OF BLOCK blk1.
