*----------------------------------------------------------------------*
***INCLUDE ZXM06I05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_ZZTRLOC1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_zztrloc1 INPUT.

  DATA: lt_locmast LIKE TABLE OF gs_locmast .
  refresh lt_locmast.
 if ekko-zzparty is not INITIAL.
    SELECT zztrloc
           zztrlocalias
           zzconvndid
           zzparty
           zzactflg
           zzfrequsdflg
      FROM zmmt_locmast
      INTO TABLE lt_locmast WHERE zzactflg = 'Y'
                              and zzparty = ekko-zzparty.
 else.
   SELECT zztrloc
           zztrlocalias
           zzconvndid
           zzparty
           zzactflg
           zzfrequsdflg
      FROM zmmt_locmast
      INTO TABLE lt_locmast WHERE zzactflg = 'Y'.
 endif.

sort lt_locmast by zzfrequsdflg DESCENDING zztrloc ASCENDING.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'ZZTRLOC'
        dynpprog    = sy-cprog
        dynpnr      = sy-dynnr
        dynprofield = 'ZZTRLOC'
        value       = ' '
        value_org   = 'S'
      TABLES
        value_tab   = lt_locmast.


ENDMODULE.                 " F4_ZZTRLOC1  INPUT
