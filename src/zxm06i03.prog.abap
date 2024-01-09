*----------------------------------------------------------------------*
***INCLUDE ZXM06I03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_ZZEKGRP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zzekgrp INPUT.
  DATA: ls_t024 TYPE ekgrp.
  CLEAR ls_t024.
  SELECT SINGLE ekgrp
           FROM t024
           INTO ls_t024
          WHERE ekgrp = ekko-zzekgrp.
*            AND EKTEL = 'TRADINGBUYER'.

  IF sy-subrc NE 0.
    MESSAGE 'Purchasing group is Invalid' TYPE 'E'.
  ELSEIF ls_t024 NOT BETWEEN 'G01' AND 'G99'.
    MESSAGE 'Purchasing group is Invalid' TYPE 'E'.
  ENDIF.
*Start of change By DADIM for CHG0246647
  IF sy-tcode = 'ME31K'.
    CLEAR : gt_zvar, gs_zvar.
    SELECT * FROM zvar INTO TABLE gt_zvar
           WHERE programm = gc_program AND
                 varname  = gc_varname.
    READ TABLE gt_zvar INTO gs_zvar WITH KEY value1 = gf_ekgrp.
    IF sy-subrc = 0.
      READ TABLE gt_zvar INTO gs_zvar WITH KEY value1 = ekko-zzekgrp.
      IF sy-subrc = 0.
        LOOP AT SCREEN.
          IF screen-group2 = 'GRP'.
            screen-required = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT SCREEN.
          IF screen-group2 = 'GRP'.
            screen-required = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
        MESSAGE 'Fill in all required entry fields' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
*End of change By DADIM for CHG0246647
ENDMODULE.                 " CHECK_ZZEKGRP  INPUT
