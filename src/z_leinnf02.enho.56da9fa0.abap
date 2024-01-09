"Name: \PR:SAPLEINN\FO:BLAOCH_FILL_BATCH_INPUT_TABLE\SE:BEGIN\EI
ENHANCEMENT 0 Z_LEINNF02.
*btboundy
*CLM TR835 - May 2011
  DATA: ls_ekpo TYPE ekpo.

  doenhancement = c_false.
  LOOP AT t_ekpo.
      IF t_ekpo-loekz = 'L' OR t_ekpo-loekz = 'D'.
        CLEAR ls_ekpo.
        SELECT SINGLE *
          FROM ekpo
          INTO CORRESPONDING FIELDS OF ls_ekpo
          WHERE ebeln = f_ekko-ebeln
            AND ebelp = t_ekpo-ebelp
        .

        IF ls_ekpo-pstyp = '9'.
          MOVE t_ekpo-loekz to ls_ekpo-loekz.
          ls_ekpo-pstyp = 'A'.
          MOVE-CORRESPONDING ls_ekpo to t_ekpo.
        ENDIF.
      ENDIF.


    IF t_ekpo-pstyp = 'A'.
      t_ekpo-pstyp = '9'.
      doenhancement = c_true.
    ENDIF.

    MODIFY t_ekpo.
  ENDLOOP.
ENDENHANCEMENT.
