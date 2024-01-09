*----------------------------------------------------------------------*
***INCLUDE ZXM06I14 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZZPARTY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_ZZPARTY INPUT.
  DATA: BEGIN OF ls_zzparty,
        zztrloc TYPE zmmt_locmast-zztrloc,
        zzparty TYPE zmmt_locmast-zzparty,
        END OF ls_zzparty.

  SELECT SINGLE zztrloc zzparty
           FROM zmmt_locmast
           INTO ls_zztrloc1
           WHERE zzparty = ekko-zzparty.

  IF sy-subrc NE 0.
    MESSAGE 'Please enter valid Pipeline' TYPE 'E'.
  endif.
ENDMODULE.                 " CHECK_ZZPARTY  INPUT
