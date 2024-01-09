"Name: \PR:SAPLEINN\FO:USER_EXIT_003_CALL\SE:END\EI
ENHANCEMENT 0 Z_LEINNF03.
  "Set maximum possible min line number
  minlinenum = 99999.
  LOOP AT t_ekpo.
    IF t_ekpo-ebelp < minlinenum.
      minlinenum = t_ekpo-ebelp.
    ENDIF.
  ENDLOOP.
ENDENHANCEMENT.
