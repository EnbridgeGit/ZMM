*----------------------------------------------------------------------*
***INCLUDE ZXM06I12 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZZTRLOC4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zztrloc4 INPUT.
  DATA: BEGIN OF ls_zztrloc4,
            zztrloc TYPE zmmt_locmast-zztrloc,
            zztrlocalias TYPE zmmt_locmast-zztrlocalias,
       END OF ls_zztrloc4.
  IF ekko-zztrloc2 is INITIAL or ekko-zztrloc3 is INITIAL.
    CLEAR ekko-zztrloc4.
    MESSAGE 'LOCATION4 cannot be entered without LOCATION2 or LOCATION3' TYPE 'E'.
  else.
    SELECT SINGLE zztrloc zztrlocalias
             FROM zmmt_locmast
             INTO ls_zztrloc4
            WHERE zztrloc = ekko-zztrloc4
              AND zzparty = ekko-zzparty.

    IF sy-subrc NE 0.
      MESSAGE 'Invalid combination of LOCATION4 and PARTY' TYPE 'E'.
    ELSE.
      zztrlocalias4 = ls_zztrloc4-zztrlocalias.
      IF ekko-zztrloc4 = ekko-zztrloc1 OR
         ekko-zztrloc4 = ekko-zztrloc2 OR
         ekko-zztrloc4 = ekko-zztrloc3.
        MESSAGE 'Location4 should not be same with other location' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ZZTRLOC4  INPUT
