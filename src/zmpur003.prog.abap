 REPORT ZMPUR002 NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65.
*-------------------------------------------------------------------*
* DESCRIPTION                                                       *
* Purchase Order Report with Split Accounting Assigned              *
*-------------------------------------------------------------------*
* CHANGES                                                           *
* 2003/03/28 mdemeest 0943 New Report                               *
*-------------------------------------------------------------------*
 TABLES:
        EKKO,                       "Purchase Order
        EKPO,                       "Purchase Order Detail
        EKKN.                       "Split Accounting Assignment

 DATA:
     BEGIN OF INFO_TABLE OCCURS 0,
        ebeln          like ekko-ebeln,       "Purchase Order Number
        ebelp          like ekpo-ebelp,       "PO Line Item #
        MATNR          LIKE EKPO-MATNR,       "Material Number
        VRTKZ          like ekpo-vrtkz,       "Distrib Ind. for Assign.
        ps_psp_pnr     like ekkn-ps_psp_pnr,  "Project
        kostl          like ekkn-kostl,       "Cost Centre
        aufnr          like ekkn-aufnr,       "Order Number

 END OF INFO_TABLE.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
 parameters:     p_bukrs like ekko-bukrs obligatory memory id BUK.
                                                      "Company Code
 SELECT-OPTIONS:
                 s_EBELN for ekko-ebeln,              "Purchase Order
                 s_aedat for ekko-aedat.              "Date Created
SELECTION-SCREEN END OF BLOCK BOX1.


 START-OF-SELECTION.
*-----------------------------------------------------------------------
* Print all data entered in the variant
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
* Select all PO's and the PO detail for report
*-----------------------------------------------------------------------
   SELECT * from ekko
       where ebeln in s_ebeln             "PO number
         and aedat in s_aedat             "PO date
         and loekz = ' '.                 "Active PO's
*      ----------------------------------------------------------------
*      Line Items that are still outstanding - either Invoicing or
*      Goods Receipt still required.
*      -----------------------------------------------------------------
       select * from ekpo
          where ebeln = ekko-ebeln        "Match PO's
            and vrtkz <> ' '              "--> Mult. Accounts Assigned
            and elikz = ' '               "Delivery Completed Ind OFF
            and erekz = ' '.              "Invoice Completed Ind OFF

          select * from ekkn
             where ebeln = ekko-ebeln     "Match PO's
               and ebelp = ekpo-ebelp.    "Match item number

             move ekko-ebeln        to info_table-ebeln.
             move ekpo-matnr        to info_table-matnr.
             move ekpo-ebelp        to info_table-ebelp.
             move ekpo-vrtkz        to info_table-vrtkz.
             move ekkn-ps_psp_pnr   to info_table-ps_psp_pnr.
             move ekkn-kostl        to info_table-kostl.
             move ekkn-aufnr        to info_table-aufnr.
             append info_table.
          endselect.                      "End of EKKN select
       endselect.                         "End of EKPO select
   ENDSELECT.                             "End of EKKO select

   SORT info_TABLE BY ebeln ebelp.
   PERFORM DISPLAY_TABLE.
   WRITE: /.
   WRITE: / TEXT-END UNDER TEXT-TTL.

  new-page.
  Perform print_variant.

*----------------  DISPLAY_TABLE  --------------------------------------
 FORM DISPLAY_TABLE.
   new-page.
   LOOP AT info_TABLE.

     write: / info_table-ebeln      under text-003,
              info_table-matnr      under text-004,
              info_table-ebelp      under text-009,
              info_table-kostl      under text-005,
              info_table-ps_psp_pnr under text-007,  "WBS
              info_table-aufnr      under text-010.  "I/O


   ENDLOOP.
 ENDFORM.

* EVENT -------------  TOP-OF-PAGE  ------------------------------------
 TOP-OF-PAGE.
   WRITE: /1 TEXT-RPT, SY-REPID, 54 TEXT-DTE, SY-DATUM,
             TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, sy-sysid,
            22 tEXT-TTL,
            TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
   WRITE: /.
   WRITE: /1 TEXT-003, 13 TEXT-009, 25 text-004, 35 TEXT-005,
          45 text-010, 58 text-007.
ULINE.

form print_variant.

data: begin of t_scr_image occurs 0,
      line(120) type c,
      end of t_scr_image.

call function 'PRINT_SELECTIONS'
    exporting
          mode      = '1'
          rname     = sy-cprog
          rvariante = sy-slset
    tables
          infotab   = t_scr_image.

loop at t_scr_image.
  write: / t_scr_image-line+2.
endloop.

endform.
