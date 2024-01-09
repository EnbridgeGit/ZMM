*----------------------------------------------------------------------*
***INCLUDE ZXM06I04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_ZZEKGRP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_zzekgrp INPUT.
  DATA: BEGIN OF ls_ekgrp1,
        ekgrp TYPE ekgrp,
        eknam TYPE eknam,
        ektel TYPE ektel,
         END OF ls_ekgrp1,
         lt_t024 LIKE TABLE OF ls_ekgrp1 .
  DATA: lr_ekgrp TYPE RANGE OF ekgrp,
        ls_ekgrp LIKE LINE OF lr_ekgrp.


  ls_ekgrp-sign = 'I'.
  ls_ekgrp-option = 'BT'.
  ls_ekgrp-low = 'G01'.
  ls_ekgrp-high = 'G99'.
  APPEND ls_ekgrp TO lr_ekgrp.
  CLEAR ls_ekgrp.

  SELECT ekgrp
         eknam
         ektel
     FROM t024
     INTO TABLE lt_t024 WHERE ekgrp IN lr_ekgrp.
  IF sy-subrc = 0.
    SORT lt_t024 BY ekgrp.
  ENDIF.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'EKGRP'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'EKGRP'
      value       = ' '
      value_org   = 'S'
    TABLES
      value_tab   = lt_t024.
ENDMODULE.                 " F4_ZZEKGRP  INPUT
