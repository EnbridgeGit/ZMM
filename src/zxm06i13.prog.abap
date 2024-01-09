*----------------------------------------------------------------------*
***INCLUDE ZXM06I13 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_ZZPARTY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE F4_ZZPARTY INPUT.

  DATA: lt_locmast_p LIKE TABLE OF gs_locmast.
  refresh lt_locmast_p.
  SELECT zztrloc
         zztrlocalias
         zzconvndid
         zzparty
         zzactflg
         zzfrequsdflg
    FROM zmmt_locmast
    INTO TABLE lt_locmast_p WHERE zzactflg = 'Y'.
*                            and zzparty = ekko-zzparty.
*                              AND zzfrequsdflg = 'X'.
  IF SY-SUBRC = 0.
    sort lt_locmast_p by zzfrequsdflg DESCENDING zztrloc ASCENDING.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'ZZPARTY'
        dynpprog    = sy-cprog
        dynpnr      = sy-dynnr
        dynprofield = 'ZZTRLOC'
        value       = ' '
        value_org   = 'S'
      TABLES
        value_tab   = lt_locmast_p.
  ENDIF.



ENDMODULE.                 " F4_ZZPARTY  INPUT
