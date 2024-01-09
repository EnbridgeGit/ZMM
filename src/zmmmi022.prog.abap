REPORT ZMMMI022 MESSAGE-ID ZS.

************************************************************************
*  Author:    M L De Meester.
*
*  Date:      January 2006.
*
*  Issue Log: TR182
*
*  Description:
*    The purpose of this program is to create the Spend Analysis files.
*    This abap is for both the East and the West.
*
*    The client will execute this abap, filling in the appropriate
*    values in the variant.
*
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
* CHANGES                                                              *
*                                                                      *
*27/11/2006 TR198 Mohammad  Add new fields to output file (Ref. Doc,   *
*                           Line item text, Group key, Purchase Order#,*
*                           Acknow.#, Plant, Material#, Material Descr,*
*                           Material Group, Cost center, Order#,       *
*                           Network(West), Operation Number(Weat),     *
*                           WBS Number(East).                          *
*                           The logic to extract data from BSEG Table  *
*                           has been developed after discussing with ME*
*                           There could be multiple records from BSEG  *
*                           that qualify for the Query, but this prog. *
*                           needs only one of them. Following criteria *
*                           has been used to pick the required record  *
*                           from the bseg table:                       *
*                           1- If there is data in BSEG field cost cntr*
*                              or in  WBS # (East) or in order number, *
*                              pick the record and use it.             *
*                           2- If criteria 1 is not met, then use this:*
*                              If BSEG field sgtxt has data, pick the  *
*                              record and use it.                      *
*                           3- If criteria 1 or 2 are not met, use this*
*                              If BSEG field Purchasing Document Number*
*                              has data, pick the record and use it.   *
*                           4- If none of the above criteria meets,    *
*                              then use line item number one.          *
*       *IMPORTANT NOTE: This program on East system and West system   *
*                        has a number of differences, it would be easy *
*                        to maintain them separately in future.        *
*                                                                      *
************************************************************************
* NOTE:  Differences between EAST & WEST - to find all the differences
*        do a find on DIFFERENCES
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
*                EAST              |           WEST
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
*                                  |
*                                  |
*                                  |
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
* ...
*
************************************************************************
tables:  BSAK,                  "Cleared Items
         BSIK,                  "Open items
         BKPF,                  "Header Table
         EKKO,                  "Purchasing Document Header     TR198
         EKPO,                  "Purchasing Document Item       TR198
         BSEG,                  "Accounting Document Segment    TR198
         LFA1.                  "Vendor Master

data: inrec(400).
data: OUTREC(2000).

data:  wa_outfile(300)        type c.
data:  vendor_name(50)        type c.
data:  wa_dmbtr(15)           type c,
       wa_wrbtr(15)           type c.                           "TR198
*data:  wa_dmbtr(15)  decimals 2.
data:  header_txt  like bkpf-bktxt.

data:  begin of wa         occurs 0,
         bukrs         like bsak-bukrs,   "Company code
         gjahrs        like bsak-gjahr,   "Fiscal year
         lifnr         like bsak-lifnr,   "Vendor Number
         name1         like lfa1-name1,   "Vendor Name
         belnr         like bsak-belnr,   "Document number
         budat         like bsak-budat,   "Posting date
         blart         like bsak-blart,   "Document type
         bschl         like bsak-bschl,   "Posting key
         dmbtr         like bsak-dmbtr,   "Amount local currency
         mwskz         like bsak-mwskz,   "Tax code
         waers         like bsak-waers,   "Currency
         bktxt         like bkpf-bktxt,   "Document header text
         xblnr         like bkpf-xblnr,   "Reference Doc.    TR198
         sgtxt         like bseg-sgtxt,   "Line item text    TR198
         konzs         like lfa1-konzs,   "Group Key         TR198
         ebeln         like ekpo-ebeln,   "Purchasing Doc no TR198
         labnr         like ekpo-labnr,   "Acknowledge No.   TR198
         werks         like ekpo-werks,   "Plant             TR198
         ematn         like ekpo-ematn,   "Material No.      TR198
         txz01         like ekpo-txz01,   "Material Descript TR198
         matkl         like ekpo-matkl,   "Material Group    TR198
         kostl         like bseg-kostl,   "Cost Center       TR198
         aufnr         like bseg-aufnr,   "Order Number      TR198
         projk         like bseg-projk,   "WBS Number        TR198 EAST
*        nplnr         like bsak-nplnr,   "Network           TR198 WEST
*        aufpl         like bsak-aufpl,   "Operation Number  TR198 WEST
       end of wa.
*-----------------------------------------------------------------------
*field-symbols: <F1>.
*data:    char(21)    type c,
*         nodata(1)   value '/'.
*data: wa_lifnr like lfa1-lifnr.
*-----------------------------------------------------------------------
* Input file name with path
data: infile(70).
* Output file name with path
DATA: OUTFILE(70).

*-----------------------------------------------------------------------
* Dates Data
DATA:   TBLDAT TYPE D,
        TBUDAT TYPE D,
        ZBLDAT(10),                        "date in user format
        ZBUDAT(10),
        FLENGTH TYPE I,
        COUNT3(2) TYPE N,
        W_PROJK(14) TYPE C,
        W_CHANGE(2) TYPE C VALUE ',-',    "To replace commas with dashes
        W_TXZ01 LIKE EKPO-TXZ01,
        WSPOSID LIKE COBL-PS_POSID.        "23/05/02 CHANGES

*=======================================================================
PARAMETERS:
    p_flout like filename-fileextern         obligatory
      default '/usr/sap/interfaces/P01/XXXXXXX/bsik.csv'.
select-options:
    s_bukrs   for bsak-bukrs     obligatory,
    s_lifnr   for bsak-lifnr,
    s_belnr   for bsak-belnr,
    s_budat   for bsak-budat,
    s_blart   for bsak-blart      no intervals,
    s_bschl   for bsak-bschl      no intervals.


*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================
INITIALIZATION.
*  move sy-sysid to p_fileot+20(3).
*  move sy-sysid to p_filein+20(3).

START-OF-SELECTION.
AT SELECTION-SCREEN.
  PERFORM OPEN_FILES.
  concatenate  'Company Code' ','
               'Vendor' ',' 'Vendor Name' ','
               'Document' ','
               'Posting Date' ','
               'Doc Type' ','
               'Posting Key' ','
               'Amount' ','
               'Tax Code' ','
               'Currency' ','
               'Header Text' ','
               'Reference Doc'  ','       "TR198
               'Line item text' ','       "TR198
               'Group Key'      ','       "TR198
               'Purchasing Doc' ','       "TR198
               'Acknowledge No' ','       "TR198
               'Plant'          ','       "TR198
               'Material No'    ','       "TR198
               'Material Des'   ','       "TR198
               'Material Group' ','       "TR198
               'Cost Center'    ','       "TR198
               'Order Number'   ','       "TR198
               'WBS Number'     ','       "TR198 EAST
*              'Network'        ','       "TR198 WEST
*              'Operation No'   ','       "TR198 WEST

               into wa_outfile.
         transfer wa_outfile to P_FLOUT length 300.


*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.
  perform select_bsak..
  perform select_bsik.
  close dataset p_flout.
*------------------------  INPUT_FILE_TO_WA  ---------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields.
*-----------------------------------------------------------------------
form SELECT_BSAK.
   select * from BSAK
       where bukrs in s_bukrs
         and lifnr in s_lifnr
         and belnr in s_belnr
         and budat in s_budat
         and blart in s_blart
         and bschl in s_bschl.
         clear: vendor_name, lfa1-konzs.
      select single name1 konzs from lfa1 into (vendor_name, lfa1-konzs)
             where lifnr = bsak-lifnr.
         replace ',' with ' ' into vendor_name.

       clear: header_txt, bkpf-xblnr.
       select single bktxt xblnr from bkpf into (header_txt, bkpf-xblnr)
             where bukrs = bsak-bukrs
               and belnr = bsak-belnr
               and gjahr = bsak-gjahr.

       clear: bseg.
       select single kostl projk aufnr ebeln sgtxt
       into (bseg-kostl, bseg-projk, bseg-aufnr, bseg-ebeln, bseg-sgtxt)
         from bseg
        where bukrs = bsak-bukrs
          and belnr = bsak-belnr
          and gjahr = bsak-gjahr
          and ( kostl > space or projk > 0 or aufnr > space ).

       if sy-subrc <> 0.
       select single kostl projk aufnr ebeln sgtxt
       into (bseg-kostl, bseg-projk, bseg-aufnr, bseg-ebeln, bseg-sgtxt)
         from bseg
        where bukrs = bsak-bukrs
          and belnr = bsak-belnr
          and gjahr = bsak-gjahr
          and sgtxt <> space.
       endif.

       if sy-subrc <> 0.
       select single kostl projk aufnr ebeln sgtxt
       into (bseg-kostl, bseg-projk, bseg-aufnr, bseg-ebeln, bseg-sgtxt)
         from bseg
        where bukrs = bsak-bukrs
          and belnr = bsak-belnr
          and gjahr = bsak-gjahr
          and ebeln <> space.
       endif.

       if sy-subrc <> 0.
       select single kostl projk aufnr ebeln sgtxt
       into (bseg-kostl, bseg-projk, bseg-aufnr, bseg-ebeln, bseg-sgtxt)
         from bseg
        where bukrs = bsak-bukrs
          and belnr = bsak-belnr
          and gjahr = bsak-gjahr
          and buzei = 1.
       endif.

       clear ekpo.
       select single * from ekpo
        where ebeln = bseg-ebeln.

*         move bsak-dmbtr to wa_dmbtr.
         move bsak-wrbtr to wa_wrbtr.                  "TR198
         if bsak-shkzg = 'H'.
*             concatenate: '-' wa_dmbtr into wa_dmbtr.
             concatenate: '-' wa_wrbtr into wa_wrbtr.  "TR198
*            compute wa_dmbtr = wa_dmbtr * -1.
         endif.

write bseg-projk to w_projk.
translate: ekpo-txz01  using w_change, header_txt using w_change,
           bseg-sgtxt  using w_change, bkpf-xblnr using w_change,
           vendor_name using w_change.
         concatenate bsak-bukrs ','
                     bsak-lifnr ',' vendor_name ','
                     bsak-belnr ','
                     bsak-budat ','
                     bsak-blart ','
                     bsak-bschl ','
*                     wa_dmbtr ','
                     wa_wrbtr ','                   "TR198
                     bsak-mwskz ','
                     bsak-waers ','
                     header_txt ','
                     bkpf-xblnr ','                 "TR198
                     bseg-sgtxt ','                 "TR198
                     lfa1-konzs ','                 "TR198
                     ekpo-ebeln ','                 "TR198
                     ekpo-labnr ','                 "TR198
                     ekpo-werks ','                 "TR198
                     ekpo-ematn ','                 "TR198
                     ekpo-txz01 ','                 "TR198
                     ekpo-matkl ','                 "TR198
                     bseg-kostl ','                 "TR198
                     bseg-aufnr ','                 "TR198
                     w_projk    ','                 "TR198 EAST
*                    bsak-nplnr ','                 "TR198 WEST
*                    bsak-aufpl ','                 "TR198 WEST

                 into wa_outfile.
         transfer wa_outfile to P_FLOUT length 300.
      endselect.

endform.

form SELECT_BSiK.
   select * from BSIK
       where bukrs in s_bukrs
         and lifnr in s_lifnr
         and belnr in s_belnr
         and budat in s_budat
         and blart in s_blart
         and bschl in s_bschl.
         clear: vendor_name, header_txt.
      select single name1 konzs from lfa1 into (vendor_name, lfa1-konzs)
             where lifnr = bsik-lifnr.
       select single bktxt xblnr from bkpf into (header_txt, bkpf-xblnr)
             where bukrs = bsik-bukrs
               and belnr = bsik-belnr
               and gjahr = bsik-gjahr.

       clear: bseg.
       select single kostl projk aufnr ebeln sgtxt
       into (bseg-kostl, bseg-projk, bseg-aufnr, bseg-ebeln, bseg-sgtxt)
        from  bseg
        where bukrs = bsik-bukrs
          and belnr = bsik-belnr
          and gjahr = bsik-gjahr
          and ( kostl > space or projk > 0 or aufnr > space ).

       if sy-subrc <> 0.
       select single kostl projk aufnr ebeln sgtxt
       into (bseg-kostl, bseg-projk, bseg-aufnr, bseg-ebeln, bseg-sgtxt)
         from bseg
        where bukrs = bsik-bukrs
          and belnr = bsik-belnr
          and gjahr = bsik-gjahr
          and sgtxt <> space.
       endif.

       if sy-subrc <> 0.
       select single kostl projk aufnr ebeln sgtxt
       into (bseg-kostl, bseg-projk, bseg-aufnr, bseg-ebeln, bseg-sgtxt)
         from bseg
        where bukrs = bsik-bukrs
          and belnr = bsik-belnr
          and gjahr = bsik-gjahr
          and ebeln <> space.
       endif.

       if sy-subrc <> 0.
       select single kostl projk aufnr ebeln sgtxt
       into (bseg-kostl, bseg-projk, bseg-aufnr, bseg-ebeln, bseg-sgtxt)
         from bseg
        where bukrs = bsik-bukrs
          and belnr = bsik-belnr
          and gjahr = bsik-gjahr
          and buzei = 1.
       endif.

       select single * from ekpo
        where ebeln = bseg-ebeln.

*         move bsik-dmbtr to wa_dmbtr.                 "TR198
         move bsik-wrbtr to wa_wrbtr.                  "TR198
         if bsik-shkzg = 'H'.
*             concatenate: '-' wa_dmbtr into wa_dmbtr. "TR198
             concatenate: '-' wa_wrbtr into wa_wrbtr.  "TR198
*            compute wa_dmbtr = wa_dmbtr * -1.
         endif.

write bseg-projk to w_projk.
translate: ekpo-txz01  using w_change, header_txt using w_change,
           bseg-sgtxt  using w_change, bkpf-xblnr using w_change,
           vendor_name using w_change.
         concatenate:bsik-bukrs ','
                     bsik-lifnr ',' vendor_name ','
                     bsik-belnr ','
                     bsik-budat ','
                     bsik-blart ','
                     bsik-bschl ','
*                     wa_dmbtr   ','
                     wa_wrbtr   ','                 "TR198
                     bsik-mwskz ','
                     bsik-waers ','
                     header_txt ','
                     bkpf-xblnr ','                 "TR198
                     bsik-sgtxt ','                 "TR198
                     lfa1-konzs ','                 "TR198
                     ekpo-ebeln ','                 "TR198
                     ekpo-labnr ','                 "TR198
                     ekpo-werks ','                 "TR198
                     ekpo-ematn ','                 "TR198
                     ekpo-txz01 ','                 "TR198
                     ekpo-matkl ','                 "TR198
                     bseg-kostl ','                 "TR198
                     bseg-aufnr ','                 "TR198
                     w_projk    ','                 "TR198
*                    bsik-nplnr ','                 "TR198 For-West-Only
*                    bsik-aufpl ','                 "TR198 For-West-Only
                 into wa_outfile.
         transfer wa_outfile to P_FLOUT length 200.
      endselect.

endform.


*==========================  OPEN_FILE =================================
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*=======================================================================
FORM OPEN_FILES.

  DATA: MSG(100).
*-----------------------------------------------------------------------
  OPEN DATASET P_FLOut FOR OUTPUT IN TEXT MODE MESSAGE MSG.
  if ( sy-subrc <> 0 ).
    message E002 with outFILE msg.
  endif.

ENDFORM.
