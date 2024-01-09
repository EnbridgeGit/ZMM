"Name: \PR:SAPLEINM\FO:MAINTAIN_DATA_ORDRSP\SE:BEGIN\EI
ENHANCEMENT 0 Z_LEINMF2O.
*Custom Enhancement Point.
*BTBOUNDY Jan 18/2011

data: st_ekpo like ekpo,
      lv_ebeln like ekpo-ebeln.

      lv_ebeln = EKKO-EBELN.

  loop at fekpo.
    select matnr from ekpo into CORRESPONDING FIELDS OF st_ekpo
      where ebeln = lv_ebeln
        and ebelp = fekpo-ebelp
    .
      fekpo-matnr = st_ekpo-matnr.
      modify fekpo.
    ENDSELECT.
  endloop.
ENDENHANCEMENT.
