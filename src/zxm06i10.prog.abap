*----------------------------------------------------------------------*
***INCLUDE ZXM06I10 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZZTRLOC2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zztrloc2 INPUT.

  DATA: BEGIN OF ls_zztrloc2,
          zztrloc TYPE zmmt_locmast-zztrloc,
          zztrlocalias TYPE zmmt_locmast-zztrlocalias,
     END OF ls_zztrloc2.

  SELECT SINGLE zztrloc zztrlocalias
           FROM zmmt_locmast
           INTO ls_zztrloc2
           WHERE zztrloc = ekko-zztrloc2
             AND zzparty = ekko-zzparty.

  IF sy-subrc NE 0.
    MESSAGE 'Invalid combination of LOCATION2 and PARTY' TYPE 'E'.
  ELSE.
    zztrlocalias2 = ls_zztrloc2-zztrlocalias.
    IF ekko-zztrloc2 = ekko-zztrloc1 OR
       ekko-zztrloc2 = ekko-zztrloc3 OR
      ekko-zztrloc2 = ekko-zztrloc4.
      MESSAGE 'Location2 should not be same with other location' TYPE 'E'.
    ENDIF.
  ENDIF.


ENDMODULE.                 " CHECK_ZZTRLOC2  INPUT
