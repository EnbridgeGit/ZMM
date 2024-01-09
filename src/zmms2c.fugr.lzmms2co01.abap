************************************************************************
***INCLUDE LZMMS2CO01 .
************************************************************************


*  MODULE import_isnvdr OUTPUT
MODULE import_isnvdr OUTPUT.

  CLEAR: gv_isnvdr.

  IMPORT act TO lv_aktyp    FROM MEMORY ID 'VENDORADDDATACS_S2C-ZZACT'.
  IMPORT act TO gv_isnvdr   FROM MEMORY ID 'VENDORADDDATACS_S2C-ZZISNVDR'.

  ASSIGN (c_field_zzisnvdr) TO <isnvdr>.
  <isnvdr> = gv_isnvdr.

  PERFORM update_display.
ENDMODULE.                    "import_isnvdr OUTPUT


*  MODULE TC_ZISNQUAL_INIT OUTPUT
MODULE tc_zisnqual_init OUTPUT.
  IF lv_zisnqual_copied = abap_false.
    CLEAR tc_zisnqual-lines.
    SELECT * FROM zisnqual
       INTO CORRESPONDING FIELDS
       OF TABLE lt_zisnqual
       WHERE isnvdr = gv_isnvdr.
    lv_zisnqual_copied  = abap_true.
    lv_tblchanged       = abap_false.
  ENDIF.
ENDMODULE.                    "TC_ZISNQUAL_INIT OUTPUT


*  MODULE TC_ZISNQUAL_MOVE OUTPUT
MODULE tc_zisnqual_move OUTPUT.
  MOVE-CORRESPONDING ls_zisnqual TO zisnqual.
ENDMODULE.                    "TC_ZISNQUAL_MOVE OUTPUT
