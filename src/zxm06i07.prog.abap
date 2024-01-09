*----------------------------------------------------------------------*
***INCLUDE ZXM06I07 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_ZZTRLOC3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_zztrloc3 INPUT.
  DATA:lt_locmast3 LIKE TABLE OF gs_locmast .
  IF ( ekko-zztrloc1 IS INITIAL OR ekko-zztrloc2 IS INITIAL )
    AND SY-TCODE = 'ME31L'.

    SELECT zztrloc
          zztrlocalias
          zzconvndid
          zzparty
          zzactflg
          zzfrequsdflg
     FROM zmmt_locmast
     INTO TABLE lt_locmast3 WHERE zzactflg = 'Y'.

  ELSE.
    IF ekko-zztrloc1 IS NOT INITIAL AND
       ekko-zztrloc2 IS NOT INITIAL .
      SELECT zztrloc
             zztrlocalias
             zzconvndid
             zzparty
             zzactflg
             zzfrequsdflg
        FROM zmmt_locmast
        INTO TABLE lt_locmast3 WHERE zztrloc NE ekko-zztrloc1
                                 AND zztrloc NE ekko-zztrloc2
                                 AND zzparty = ekko-zzparty
                                 AND zzactflg = 'Y'.
*                               AND zzfrequsdflg = 'X'.
    ENDIF.

  ENDIF.
  sort lt_locmast3 by zzfrequsdflg DESCENDING zztrloc ASCENDING.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZZTRLOC'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'ZZTRLOC'
      value       = ' '
      value_org   = 'S'
    TABLES
      value_tab   = lt_locmast3.

ENDMODULE.                 " F4_ZZTRLOC3  INPUT
