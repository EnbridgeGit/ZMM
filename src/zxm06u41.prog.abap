*&---------------------------------------------------------------------*
*&  Include           ZXM06U41
*&---------------------------------------------------------------------*
*Move from DB to *EKPO

* Create data
IF ( ( i_aktyp EQ 'H' ) OR ( i_aktyp EQ 'V' ) ). " create i_aktyp eq 'H' or change i_aktyp eq 'V'

  IF ( ( ( sy-tcode                       EQ 'ME21N' )    ) AND
       ( ( i_ci_ekpo-zzariba_approver     IS INITIAL )    ) AND
       ( ( i_reban-banfn              IS NOT INITIAL )    ) AND
       ( ( i_reban-zzariba_approver   IS NOT INITIAL )    )     ).

    zekpo-zzariba_approver = i_ci_ekpo-zzariba_approver.
    ekpo-zzariba_approver  = i_reban-zzariba_approver.

  ELSE.

    zekpo-zzariba_approver = i_ci_ekpo-zzariba_approver.
    ekpo-zzariba_approver  = i_ci_ekpo-zzariba_approver.

  ENDIF.

* Display data
ELSE. " display i_aktyp eq 'A'

  zekpo-zzariba_approver = i_ci_ekpo-zzariba_approver.
  ekpo-zzariba_approver  = i_ci_ekpo-zzariba_approver.

ENDIF.

*Modify or Display.
gf_aktyp = i_aktyp.
