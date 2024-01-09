 REPORT ZMPUR002 NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65.
*-------------------------------------------------------------------*
* DESCRIPTION                                                       *
* Purchase Order Status Report listing Goods Receipt Indicator and  *
* Invoice Receipt Indicator                                         *
*-------------------------------------------------------------------*
* CHANGES                                                           *
* 2003/03/26 mdemeest 0943 New Report                               *
*-------------------------------------------------------------------*
 TABLES:
        EKKO,                       "Purchase Order
        EKPO,                       "Purchase Order Detail
        EKBE.                       "PO History

 DATA:
     BEGIN OF INFO_TABLE OCCURS 0,
        LIFNR          LIKE EKKO-LIFNR,       "Vendor Number
        ebeln          like ekko-ebeln,       "Purchase Order Number
        ebelp          like ekpo-ebelp,       "PO Line Item #
        MATNR          LIKE EKPO-MATNR,       "Material Number
        WEPOS          LIKE EKPO-WEPOS,       "Goods Receipt Indicator
        REPOS          LIKE EKPO-REPOS,       "Invoice Receipt Indicator

 END OF INFO_TABLE.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
 parameters:     p_bukrs like ekko-bukrs obligatory memory id BUK.
                                                      "Company Code
 SELECT-OPTIONS: s_lifnr for ekko-lifnr,              "Vendor
                 s_EBELN for ekko-ebeln,              "Purchase Order
                 s_aedat for ekko-aedat,              "Date Created
                 s_wepos for ekpo-wepos NO INTERVALS, "GR Indicator
                 s_repos for ekpo-repos NO INTERVALS. "IR Indicator
SELECTION-SCREEN END OF BLOCK BOX1.


 START-OF-SELECTION.
*-----------------------------------------------------------------------
* Select all PO's and the PO detail for report
*-----------------------------------------------------------------------
   SELECT * from ekko
       where ebeln in s_ebeln             "PO number
         and aedat in s_aedat             "PO date
         and lifnr in s_lifnr             "Vendor
         and loekz = ' '.                 "Active PO's
       select * from ekpo
          where ebeln = ekko-ebeln        "Match PO's
            and repos in s_repos          "Invoice Indicator
            and wepos in s_wepos.         "Goods Receipt Indicator

          select single * from ekbe
            where ebeln = ekpo-ebeln
              and ebelp = ekpo-ebelp
              and bewtp = 'R'.             "--> no Invoice Receipts
          if sy-subrc <> '0'.
             move ekko-lifnr        to info_table-lifnr.
             move ekko-ebeln        to info_table-ebeln.
             move ekpo-matnr        to info_table-matnr.
             move ekpo-repos        to info_table-repos.
             move ekpo-wepos        to info_table-wepos.
             move ekpo-ebelp        to info_table-ebelp.
             append info_table.
          endif.
       endselect.                         "End of EKPO select
   ENDSELECT.                             "End of EKKO select

   SORT info_TABLE BY LIFNR ebeln ebelp.
   PERFORM DISPLAY_TABLE.
   WRITE: /.
   WRITE: / TEXT-END UNDER TEXT-TTL.
*-----------------------------------------------------------------------
* Print all data entered in the variant
*-----------------------------------------------------------------------
  new-page.
  Perform print_variant.


*----------------  DISPLAY_TABLE  --------------------------------------
 FORM DISPLAY_TABLE.
   new-page.
   LOOP AT info_TABLE.
     AT NEW LIFNR.
       write: /.
       WRITE: / info_TABLE-LIFNR UNDER TEXT-005.
     ENDAT.

     write: / info_table-ebeln under text-003,
              info_table-matnr under text-004,
              info_table-repos under text-007,
              info_table-wepos under text-008,
              info_table-ebelp under text-009.

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
   WRITE: /1 TEXT-005, 10 TEXT-003, 25 text-009, 35 TEXT-004,
          45 text-007, 55 text-008.
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










