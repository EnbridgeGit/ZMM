"Name: \PR:SAPLEINN\FO:BLAORD_FILL_BATCH_INPUT_TABLE\SE:BEGIN\EI
ENHANCEMENT 0 Z_LEINNF01.
*btboundy
*CLM TR835 - May 2011

  LOOP AT t_ekpo.
    IF t_ekpo-pstyp = 'A'.
      t_ekpo-pstyp = '9'.
      doenhancement = c_true.
    ELSE.
      doenhancement = c_false.
    ENDIF.
    MODIFY t_ekpo.
  ENDLOOP.
ENDENHANCEMENT.
