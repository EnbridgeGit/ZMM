REPORT ZZMLTST no standard page heading line-size 132 line-count 65.
*-----------------------------------------------------------------------
* Developer:  ML DeMeester
* Date:       May 10,2001
* Purpose:    Test program to try out the GoodsIssue BAPI.  Information
*             will be hardcoded so that the developer can get a handle
*             on what is required for the ActiveWorks product.
*----------------------------------------------------------------------
tables: usr02, usr21,
        adrp.

data:  number_of_days(3) type c.
data:  begin of wa       occurs 0,
          bname like usr02-bname,
          erdat like usr02-erdat,
          gltgb like usr02-gltgb,
          trdat like usr02-trdat,
          aname like usr02-aname,
          name_first like adrp-name_first,
          name_last  like adrp-name_last,
       end of wa.

select-options: s_class for usr02-class memory id xug no intervals,
                s_ustyp for usr02-ustyp.

parameters:     p_erdat like usr02-erdat,
                p_gltgb like usr02-gltgb,
                p_trdat like usr02-trdat.
*
compute number_of_days = sy-datum - p_erdat.

top-of-page.
write: /1 text-rpt, sy-repid, 50 text-ttl,
      100 text-dte, sy-datum, text-amp, sy-uzeit,
      75  number_of_days.
write: / text-clt under text-rpt, sy-mandt under sy-repid,
       text-pge under text-dte, sy-pagno  under sy-datum.
write: /.
write: /1 text-001, 15 text-002, 30 text-003,
       45 text-004, 57 text-005, 68 text-006,
       80 text-007.
uline.
write: /.


start-of-selection.
select bname erdat gltgb trdat aname
       into  (wa-bname, wa-erdat, wa-gltgb, wa-trdat, wa-aname)
       from  usr02
   where class in s_class
     and erdat < p_erdat
     and gltgb > p_gltgb
     and trdat < p_trdat
     and ustyp in s_ustyp.
     select single * from usr21
        where bname = wa-bname.
     if sy-subrc = '0'.
        select single * from adrp
           where persnumber = usr21-persnumber.
           move adrp-name_first to wa-name_first.
           move adrp-name_last  to wa-name_last.
     endif.
  append wa.
  clear wa.
endselect.

sort wa by bname.


  loop at wa.
    write: / wa-bname          under text-001,
             wa-name_first     under text-002,
             wa-name_last      under text-003,
             wa-trdat          under text-004,
             wa-aname          under text-005,
             wa-erdat          under text-006,
             wa-gltgb          under text-007.
  endloop.                                                     .
*endselect.
