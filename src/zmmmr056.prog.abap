REPORT ZMMMR056 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65
               MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMR056 - MM: PO - Counts & Amounts
*    Programmer  :  M DeMeester
*    Date        :  March 25,2002
*
*    Statistics for materials
*
*
************************************************************************
* 2002/10/31 mdemeest Added selection criteria for MATERIAL TYPE
************************************************************************

*****************************  TABLES   ********************************

TABLES: MARA,        "Material Number
        marc,        "Plant
        MARD.        "Storage Locations

**************************  DATA ELEMENTS  *****************************
*-----------------------------------------------------------------------

DATA: BEGIN OF TABLE1 OCCURS 0,

       werks            like mard-werks,
       mtart            like mara-mtart,
       matnr            like mara-matnr,       "Material
       maincnt type i,
       othercnt type i,
       END OF TABLE1.

DATA: BEGIN OF table2 OCCURS 0,
        mtart            like mara-mtart,
        matnr            like mara-matnr,

        matnrcnt type i,
      END OF TABLE2.

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-014.

select-options: s_mtart for mara-mtart no intervals.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
selection-screen:  skip.
selection-screen comment 1(77) text-015.
selection-screen:  skip.
parameter:      p_mat radiobutton group mat.
select-options: s_matkl for mara-matkl
                            no intervals.
selection-screen:  skip.
parameter: p_plt radiobutton  group mat default 'X'.

SELECT-OPTIONS: S_werks FOR mard-werks,  "Plant
                s_lgort for mard-lgort,  "Storage Location
                s_mmsta for marc-mmsta  no intervals.

selection-screen: skip 2.
SELECTION-SCREEN COMMENT 1(79) TEXT-016.
SELECTION-SCREEN:  SKIP.
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
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
WRITE: /.
FORMAT COLOR COL_NORMAL.
ULINE.
write: /25 text-010, 35 text-001, 60 text-002.
***************************  MAIN ROUTINE  *****************************



START-OF-SELECTION.
*----------------------------------------------------------------------
*  select all materials
*  for po.
*----------------------------------------------------------------------
     if p_plt = 'X'.
        select * from mard
           where werks in s_werks
             and lgort in s_lgort
             and lvorm = ' '.           "Active
           clear table1.
           move mard-matnr to table1-matnr.
           move mard-werks to table1-werks.
           select single * from mara
               where matnr = mard-matnr
                 and mtart in s_mtart.                    .
           if sy-subrc = '0'.
              move mara-mtart to table1-mtart.
              if  mard-lgort = 'A001'.
                  move 1 to table1-maincnt.
               else.
                  move 1 to table1-othercnt.
               endif.
               collect table1.
           endif.
        endselect.

       loop at table1.

         select * from marc
            where matnr = table1-matnr
              and werks = table1-werks
              and mmsta not in s_mmsta.
            if sy-subrc = '0'.
               delete table1.
            endif.
         endselect.
       endloop.
    endif.

    if p_mat = 'X'.
       select * from mara
          where lvorm = space
            and matkl in s_matkl
            and mtart in s_mtart.             .
          clear table2.
          if sy-subrc = '0'.
             move mara-matnr to table2-matnr.
             move mara-mtart to table2-mtart.
             move 1          to table2-matnrcnt.
             append table2.
          endif.
       endselect.

       if  p_det = 'X'.
           perform material_detail_print.
       endif.
       perform material_summary_print.
    endif.


*---------------------------------------------------------------------

  if p_plt = 'X'.
     sort table1 by mtart werks.
     if p_det = 'X'.
        perform plant_detail_print.
     else.
        perform plant_summary_print.
     endif.
  endif.

skip 2.
write: / text-end under text-hdg.             "END of REPORT

*-----------------------------------------------------------------------

form plant_detail_print.
     loop at table1.
             write: / 'Plant', table1-werks,
                          table1-matnr,
                          table1-mtart    under text-010,
                          table1-maincnt  under text-001,
                          table1-othercnt under text-002.
     endloop.
endform.

form plant_summary_print.
   loop at table1.
     at new mtart.
        sum.
        write: / table1-mtart under text-010,
                 table1-maincnt under text-001,
                 table1-othercnt under text-002.
     endat.

     at new werks.
        sum.
        write: / 'Number of Materials for ',
                                      table1-werks,
                                      table1-maincnt under text-001,
                                      table1-othercnt under text-002.
        write: /.
     endat.
   endloop.
endform.


form material_detail_print.
   sort table2 by mtart matnr.

   loop at table2.
      at new mtart.
         write: /.
         write: / table2-mtart under text-010.
      endat.
      write: / table2-matnr under text-001.
   endloop.
endform.

form material_summary_print.
   sort table2 by mtart matnr.

   loop at table2.
     at end of mtart.
        sum.
        write: / table2-mtart under text-010,
               table2-matnrcnt under text-001.
     endat.
     at last.
        sum.
        write: / 'Number of Materials for Company ',
                                      table2-matnrcnt under text-001.
     endat.
   endloop.
endform.


*-------------------------- DETAIL_REPORT --------------
 form detail_report.
*    loop at out_table.
*       at new range.
*          write: /10 out_table-range.
*       endat.

*       write: / out_table-ebeln under text-005,
*                out_table-netwr under text-006.

*      at end of range.
*          sum.
*          uline.
*          write: / out_table-cnt under text-005, out_table-cnt,
*                   out_table-netwr under text-006.
*       endat.

*   endloop.
 endform.
*-------------------------- SUMMARY_REPORT --------------
 form summary_report.
*    loop at out_table.
*       at end of range.
*          sum.
*          write: /10 out_table-range,
*                     out_table-cnt under text-005,
*                     out_table-netwr under text-006.
*       endat.

*    endloop.
 endform.


***************************  END OF PROGRAM  ***************************
