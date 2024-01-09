REPORT ZMMMR055 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65
               MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMR055 - MM: PO - Counts & Amounts
*    Programmer  :  M DeMeester
*    Date        :  March 25,2002
*
*    This ABAP will retrieve all Purchase Orders for a specific time
*    frame, either at a detail or summary level.
*    requested materials.
************************************************************************
*
************************************************************************

*****************************  TABLES   ********************************

TABLES: EKKO,        "Purchase Order Header
        EKPO.        "Purchase Order Items

**************************  DATA ELEMENTS  *****************************
*-----------------------------------------------------------------------

DATA: BEGIN OF PO_TABLE OCCURS 0,
       netwr            like ekpo-netwr,       "Amount
       ebeln            like ekpo-ebeln,       "PO Number
       END OF PO_TABLE.

DATA: BEGIN OF out_TABLE OCCURS 0,
       range(40) type c,
       netwr            like ekpo-netwr,       "Amount
       ebeln            like ekpo-ebeln,       "PO Number
       cnt              type i,
       END OF out_TABLE.

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-014.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS: S_aedat FOR Ekpo-aedat obligatory,  "PO Creation Date
                s_netwr for ekpo-netwr obligatory.  "Amount Groups
selection-screen: skip 2.
parameter:      p_det radiobutton group rpt,
                p_sum radiobutton group rpt default 'X'.
SELECTION-SCREEN END OF BLOCK BOX1.


*--------------------  TOP-OF-PAGE  ------------------------------------
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_GROUP,
       50 TEXT-HDG,                                             "Title
      102 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT,
         SY-MANDT UNDER SY-REPID, SY-SYSID,
         'for' under text-hdg, s_aedat+3(8), '-',
         s_aedat+11(8),
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
WRITE: /.
FORMAT COLOR COL_NORMAL.
ULINE.
write: /35 text-005, 54 text-006.
***************************  MAIN ROUTINE  *****************************



START-OF-SELECTION.
*----------------------------------------------------------------------
*  select all po's in time frame and create record with total amounts
*  for po.
*----------------------------------------------------------------------
    select * from ekko
        where aedat in s_aedat.
        select * from ekpo
          where ebeln = ekko-ebeln.
            move ekpo-ebeln to po_table-ebeln.
            move ekpo-netwr to po_table-netwr.
            collect po_table.
        endselect.
    endselect.

    sort po_table by netwr.



*---------------------------------------------------------------------
* Categorize according to grouping entered in variant
*---------------------------------------------------------------------

   loop at s_netwr.
      loop at po_table.
         if po_table-netwr between s_netwr-low and s_netwr-high.
            move s_netwr-low             to out_table-range+0(15).
            move '- '                    to out_table-range+15(2).
            move s_netwr-high            to out_table-range+17(15).
            move 1                       to out_table-cnt.
            condense out_table-range.
            move po_table-ebeln          to out_table-ebeln.
            move po_table-netwr          to out_table-netwr.
            append out_table.
         endif.

        endloop.
   endloop.

*-----------------------------------------------------------------------
*  Print Summary or Detail Report based on selection radio button
*-----------------------------------------------------------------------
   if p_det = 'X'.
      perform detail_report.
   else.
      perform summary_report.
   endif.

*-------------------------- DETAIL_REPORT --------------
 form detail_report.
    loop at out_table.
       at new range.
          write: /10 out_table-range.
       endat.

       write: / out_table-ebeln under text-005,
                out_table-netwr under text-006.

       at end of range.
          sum.
          uline.
          write: / out_table-cnt under text-005, out_table-cnt,
                   out_table-netwr under text-006.
       endat.

    endloop.
 endform.
*-------------------------- SUMMARY_REPORT --------------
 form summary_report.
    loop at out_table.
       at end of range.
          sum.
          write: /10 out_table-range,
                     out_table-cnt under text-005,
                     out_table-netwr under text-006.
       endat.

    endloop.
 endform.


***************************  END OF PROGRAM  ***************************
