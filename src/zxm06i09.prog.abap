*----------------------------------------------------------------------*
***INCLUDE ZXM06I09 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZZTRLOC1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zztrloc1 INPUT.
  DATA: BEGIN OF ls_zztrloc1,
        zztrloc TYPE zmmt_locmast-zztrloc,
        zzparty TYPE zmmt_locmast-zzparty,
        END OF ls_zztrloc1.

  SELECT SINGLE zztrloc zzparty
           FROM zmmt_locmast
           INTO ls_zztrloc1
           WHERE zztrloc = ekko-zztrloc1
             AND zzparty = ekko-zzparty.

  IF sy-subrc NE 0.
    MESSAGE 'Location1 does not exist or not related to Pipeline' TYPE 'E'.
  ELSE.
*    ekko-zzparty = ls_zztrloc1-zzparty.
    IF ekko-zztrloc1 = ekko-zztrloc2 OR
       ekko-zztrloc1 = ekko-zztrloc3 OR
       ekko-zztrloc1 = ekko-zztrloc4.
      MESSAGE 'Location1 should not be same with other location' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_ZZTRLOC1  INPUT
