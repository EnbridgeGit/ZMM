*----------------------------------------------------------------------*
* Report Name: ZLMMR13_INVOICE_ATTCHMNT_DWNLD
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
REPORT  zlmmr13_invoice_attchmnt_dwnld MESSAGE-ID zmm_message NO STANDARD PAGE HEADING.
INCLUDE:zlmmi_invoice_attchmnt_top,
        zlmmi_invoice_attchmnt_scr,
        zlmmi_invoice_attchmnt_f01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lpath.
  PERFORM f_file_f4.

START-OF-SELECTION.
  PERFORM f_process_data.
