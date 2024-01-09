"Name: \PR:RSNASTED\FO:EDI_PROCESSING\SE:BEGIN\EI
ENHANCEMENT 0 Z_NASTEDF0.
*Custom Enhancement Point.
*BTBOUNDY Jan 04/2011
  data: wa_lfa1 type lfa1.

  if RC = '990'.
    if nast-aende = 'X'.
      nast-aende = ''.
    endif.
  endif.


  select single emnfr from lfa1 into CORRESPONDING FIELDS OF wa_lfa1
    where lifnr = nast-parnr.

  if wa_lfa1-emnfr = '' and nast-KSCHL = 'zari'.
    RC = 0.
    exit.
  endif.
ENDENHANCEMENT.
