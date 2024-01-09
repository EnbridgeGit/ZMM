"Name: \PR:RSNASTED\FO:READ_TEDE1\SE:BEGIN\EI
ENHANCEMENT 0 Z_NASTEDF0.
*Custom Enhancement Point.
*BTBOUNDY Jan 04/2011

  "Check if PO Has been canceled
  data: lv_count type I value 0.
  select count( * ) into lv_count from ekpo
    WHERE ebeln = nast-objky
      and loekz <> 'L'.

  if lv_count = 0.
    edp12-evcoda = 'ME11'.
  endif.
ENDENHANCEMENT.
