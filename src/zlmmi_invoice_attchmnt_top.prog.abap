*----------------------------------------------------------------------*
* Report Name: ZLMMI_INVOICE_ATTCHMNT_TOP
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
DATA:gv_belnr  TYPE bsik-belnr.
TYPES:BEGIN OF gty_rbkp,
         belnr TYPE re_belnr,
         gjahr TYPE gjahr,
         objkey TYPE sibfboriid,
       END OF gty_rbkp.
DATA:gt_rbkp TYPE STANDARD TABLE OF gty_rbkp INITIAL SIZE 0.
DATA:wa_rbkp TYPE gty_rbkp.
DATA:o_invoice_attach_dwnld TYPE REF TO zlmm_invoice_attach_dwnld.
CONSTANTS:gc_set TYPE flag VALUE 'X'.
