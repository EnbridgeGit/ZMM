REPORT ZMMMR053 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132
                MESSAGE-ID ZM.
************************************************************************
*  Author:      M DeMeester
*  Date:        May 2001
*  Description:
*     - The purpose of this program is to create a Source List by Vendor
*       Report
************************************************************************
* Changes:
*
*
* 2001/05/07 mdemeest #876 New Report
************************************************************************

TABLES: EORD,                          "Purchasing Source List
        MAKT.                          "Material Descriptions

INCLUDE <SYMBOL>.
DATA:
    BEGIN OF SAVE_TABLE OCCURS 10000,
       srt1(15)     type c,            "Sort field 1
       srt2(15)     type c,            "Sort field 2
       srt3(15)     type c,            "Sort field 3
       lifnr        like eord-lifnr,
       MATNR        LIKE S012-MATNR,   "Material
       WERKS        LIKE S012-WERKS,   "Plant
       vdatu        like eord-vdatu,   "Valid From date
       bdatu        like eord-bdatu,   "Valid To date
   END OF SAVE_TABLE.

data: records_selected(50) type c value 'Selecting Record '.
data: wa_count(10)  type p.
data: wa_title2(35) type c.

*-----------------------------------------------------------------------
*    Start of the selection screen
*-----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
selection-screen skip 2.
 SELECT-OPTIONS:
           S_LIFNR for eord-LIFNR,
           S_MATNR for eord-matnr,
           s_werks for eord-werks.
selection-screen skip 2.
 parameters:
           p_chk1 as checkbox,
           p_chk2 as checkbox,
           p_chk3 as checkbox,
           p_chk4 as checkbox,
           p_chk5 as checkbox,
           p_chk6 as checkbox.
SELECTION-SCREEN end of block box.

*---------------------  TOP-OF-PAGE  -----------------------------------
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
*         58 T001-BUTXT COLOR 4 INTENSIFIED ON,
        100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.

  WRITE: /1 TEXT-CLT, SY-MANDT, SY-SYSID,
         50 TEXT-TTL COLOR 4 INTENSIFIED ON,
          TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.

  WRITE: / wa_title2 under text-ttl.

  WRITE: /.

  ULINE.

  if p_chk1 = 'X'.
   WRITE: /1 text-001, text-002, text-003, text-004, text-005, text-006.
  elseif p_chk2 = 'X'.
   WRITE: /1 text-001, text-003, text-002, text-004, text-005, text-006.
  elseif p_chk3 = 'X'.
   WRITE: /1 text-002, text-001, text-003, text-004, text-005, text-006.
  elseif p_chk4 = 'X'.
   WRITE: /1 text-002, text-003, text-001, text-004, text-005, text-006.
  elseif p_chk5 = 'X'.
   WRITE: /1 text-003, text-002, text-001, text-004, text-005, text-006.
  elseif p_chk6 = 'X'.
   WRITE: /1 text-003, text-001, text-002, text-004, text-005, text-006.
  endif.

*-------------------  START-OF-SELECTION  ------------------------------
START-OF-SELECTION.
  if ( p_chk1 = 'X' and p_chk2 = ' ' and p_chk3 = ' ' and p_chk4 = ' '
                                    and p_chk5 = ' ' and p_chk6 = ' ' ).
       move 'Sorted by Vendor, Material, Plant' to wa_title2.
  elseif ( p_chk1 = ' ' and p_chk2 = 'X' and p_chk3 = ' ' and
           p_chk4 = ' ' and p_chk5 = ' ' and p_chk6 = ' ' ).
           move 'Sorted by Vendor, Plant, Material' to wa_title2.
  elseif ( p_chk1 = ' ' and p_chk2 = ' ' and p_chk3 = 'X' and
           p_chk4 = ' ' and p_chk5 = ' ' and p_chk6 = ' ' ).
           move 'Sorted by Material, Vendor, Plant' to wa_title2.
  elseif ( p_chk1 = ' ' and p_chk2 = ' ' and p_chk3 = ' ' and
           p_chk4 = 'X' and p_chk5 = ' ' and p_chk6 = ' ' ).
           move 'Sorted by Material, Plant, Vendor' to wa_title2.
  elseif ( p_chk1 = ' ' and p_chk2 = ' ' and p_chk3 = ' ' and
           p_chk4 = ' ' and p_chk5 = 'X' and p_chk6 = ' ' ).
           move 'Sorted by Plant, Material, Vendor' to wa_title2.
  elseif ( p_chk1 = ' ' and p_chk2 = ' ' and p_chk3 = ' ' and
           p_chk4 = ' ' and p_chk5 = ' ' and p_chk6 = 'X' ).
           move 'Sorted by Plant, Vendor, Material' to wa_title2.
  ELSE.
    message E100 with 'PLEASE REQUEST ONLY ONE REPORT'.
  endif.
*-----------------------------------------------------------------------
* select all records from EORD table that satisfy selection criteria
*-----------------------------------------------------------------------

  select * from eord
      where lifnr in s_lifnr
        and matnr in s_matnr
        and werks in s_werks.
*-----------------------------------------------------------------------
* Counts number of records read & displays message on bottom of screen
*-----------------------------------------------------------------------
  compute wa_count = wa_count + 1.
  if wa_count =  5000 or wa_count = 10000 or wa_count = 15000 or
     wa_count = 20000 or wa_count = 25000 or wa_count = 30000 or
     wa_count = 35000 or wa_count = 40000 or wa_Count = 45000 or
     wa_count = 50000 or wa_count = 55000 or wa_count = 60000.
     write wa_count to records_selected+20(10).
     CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
               TEXT   = records_selected
          EXCEPTIONS
               OTHERS = 1.
  endif.
*-----------------------------------------------------------------------


    move eord-lifnr to save_table-lifnr.
    move eord-matnr to save_table-matnr.
    move eord-werks to save_table-werks.
    move eord-vdatu to save_table-vdatu.
    move eord-bdatu to save_table-bdatu.

    if p_chk1 = 'X'.                      "Sort by Vendor,Material,Plant
       move eord-lifnr+5(5)  to save_table-srt1.
       move eord-matnr+12(6) to save_table-srt2.
       move eord-werks       to save_table-srt3.
    elseif p_chk2 = 'X'.                  "Sort by Vendor,Plant,Material
       move eord-lifnr+5(5)  to save_table-srt1.
       move eord-werks       to save_table-srt2.
       move eord-matnr+12(6) to save_table-srt3.
    elseif p_chk3 = 'X'.                  "Sort by Material,Vendor,Plant
       move eord-matnr+12(6) to save_table-srt1.
       move eord-lifnr+5(5)  to save_table-srt2.
       move eord-werks       to save_table-srt3.
    elseif p_chk4 = 'X'.                  "Sort by Material,Plant,Vendor
       move eord-matnr+12(6) to save_table-srt1.
       move eord-werks       to save_table-srt2.
       move eord-lifnr+5(5)  to save_table-srt3.
    elseif p_chk5 = 'X'.                  "Sort by Plant,Material,Vendor
       move eord-werks       to save_table-srt1.
       move eord-matnr+12(6) to save_table-srt2.
       move eord-lifnr+5(5)  to save_table-srt3.
    elseif p_chk6 = 'X'.                  "Sort by Plant,Vendor,Material
       move eord-werks       to save_table-srt1.
       move eord-lifnr+5(5)  to save_table-srt2.
       move eord-matnr+12(6) to save_table-srt3.
    endif.

    append save_table.

  endselect.
*-----------------------------------------------------------------------

  sort save_table by srt1 srt2 srt3.

*-----------------------------------------------------------------------
* Produce report
*-----------------------------------------------------------------------

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Report Being Generated'
       EXCEPTIONS
            OTHERS = 1.

*-----------------------------------------------------------------------
  if p_chk1 = 'X'.
     loop at save_table.
       write: / save_table-srt1 under text-001,
                save_table-srt2 under text-002,
                save_table-srt3 under text-003.
       perform write_rest_of_line.
     endloop.
  endif.
*-----------------------------------------------------------------------
  if p_chk2 = 'X'.
     loop at save_table.
       write: / save_table-srt1 under text-001,
                save_table-srt2 under text-003,
                save_table-srt3 under text-002.
       perform write_rest_of_line.
     endloop.
  endif.
*-----------------------------------------------------------------------
  if p_chk3 = 'X'.
     loop at save_table.
       write: / save_table-srt1 under text-002,
                save_table-srt2 under text-001,
                save_table-srt3 under text-003.
       perform write_rest_of_line.
     endloop.
  endif.
*-----------------------------------------------------------------------
  if p_chk4 = 'X'.
     loop at save_table.
       write: / save_table-srt1 under text-002,
                save_table-srt2 under text-003,
                save_table-srt3 under text-001.
       perform write_rest_of_line.
     endloop.
  endif.
*-----------------------------------------------------------------------
  if p_chk5 = 'X'.
     loop at save_table.
       write: / save_table-srt1 under text-003,
                save_table-srt2 under text-002,
                save_table-srt3 under text-001.
       perform write_rest_of_line.
     endloop.
  endif.
*-----------------------------------------------------------------------
  if p_chk6 = 'X'.
     loop at save_table.
       write: / save_table-srt1 under text-003,
                save_table-srt2 under text-001,
                save_table-srt3 under text-002.
       perform write_rest_of_line.
     endloop.
  endif.

*---------------  WRITE_REST_OF_LINE  ----------------------------------
form write_rest_of_line.
  write:  save_table-vdatu under text-004,
          save_table-bdatu under text-005.
  select single * from makt                           "Get Description
    where matnr = save_table-matnr
      and spras = sy-langu.

  if sy-subrc = '0'.
     write:  makt-maktx under text-006.
  endif.
endform.




