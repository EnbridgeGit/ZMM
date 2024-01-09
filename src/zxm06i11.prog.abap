*----------------------------------------------------------------------*
***INCLUDE ZXM06I11 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZZTRLOC3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zztrloc3 INPUT.
  DATA: BEGIN OF ls_zztrloc3,
          zztrloc TYPE zmmt_locmast-zztrloc,
          zztrlocalias TYPE zmmt_locmast-zztrlocalias,
     END OF ls_zztrloc3.
  IF ekko-zztrloc2 is INITIAL.
    CLEAR ekko-zztrloc3.
    MESSAGE 'LOCATION3 cannot be entered without LOCATION2' TYPE 'E'.
  else.
    SELECT SINGLE zztrloc zztrlocalias
             FROM zmmt_locmast
             INTO ls_zztrloc3
            WHERE zztrloc = ekko-zztrloc3
              AND zzparty = ekko-zzparty.

    IF sy-subrc NE 0.
      MESSAGE 'Invalid combination of LOCATION3 and PARTY' TYPE 'E'.
    ELSE.
      zztrlocalias3 = ls_zztrloc3-zztrlocalias.
      IF ekko-zztrloc3 = ekko-zztrloc1 OR
         ekko-zztrloc3 = ekko-zztrloc2 OR
         ekko-zztrloc3 = ekko-zztrloc4.
        MESSAGE 'Location3 should not be same with other location' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ZZTRLOC3  INPUT
