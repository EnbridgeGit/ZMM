*----------------------------------------------------------------------*
***INCLUDE ZXM06I06 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_ZZTRLOC2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_zztrloc2 INPUT.
  DATA: lt_locmast2 LIKE TABLE OF gs_locmast.
IF ekko-zztrloc1 IS INITIAL AND SY-TCODE = 'ME31L'.

     SELECT zztrloc
           zztrlocalias
           zzconvndid
           zzparty
           zzactflg
           zzfrequsdflg
      FROM zmmt_locmast
      INTO TABLE lt_locmast2 WHERE zzactflg = 'Y'.

ELSE.
  IF ekko-zztrloc1 IS NOT INITIAL .
    SELECT zztrloc
           zztrlocalias
           zzconvndid
           zzparty
           zzactflg
           zzfrequsdflg
      FROM zmmt_locmast
      INTO TABLE lt_locmast2 WHERE zztrloc NE ekko-zztrloc1
                               AND zzparty = ekko-zzparty
                               AND zzactflg = 'Y'.
*                               AND zzfrequsdflg = 'X'.
  ENDIF.


ENDIF.
sort lt_locmast2 by zzfrequsdflg DESCENDING zztrloc ASCENDING.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'ZZTRLOC'
        dynpprog    = sy-cprog
        dynpnr      = sy-dynnr
        dynprofield = 'ZZTRLOC'
        value       = ' '
        value_org   = 'S'
      TABLES
        value_tab   = lt_locmast2.

ENDMODULE.                 " F4_ZZTRLOC2  INPUT
