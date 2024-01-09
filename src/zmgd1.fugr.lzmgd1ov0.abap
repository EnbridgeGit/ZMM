*----------------------------------------------------------------------*
***INCLUDE LMGD1OV0 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_MATERIAL_FIXED  OUTPUT
*&---------------------------------------------------------------------*
* TF 4.6C Materialfixierung
*----------------------------------------------------------------------*
MODULE set_material_fixed OUTPUT.

*Fixationflag
  IF mara-matfi = 'X'.
    gv_icon_name = icon_locked.
    gv_icon_info = text-500.
    gv_add_stdinf = 'X'.
    CALL FUNCTION 'ICON_CREATE'
         EXPORTING
              name                  = gv_icon_name
              text                  = ' '
              info                  = gv_icon_info
              add_stdinf            = gv_add_stdinf
         IMPORTING
              result                = material_fixed
         EXCEPTIONS
              icon_not_found        = 1
              outputfield_too_short = 2
              OTHERS                = 3.
  ENDIF.

ENDMODULE.                             " SET_MATERIAL_FIXED  OUTPUT
ENHANCEMENT-POINT LMGD1OV0_01 SPOTS ES_LMGD1OV0 STATIC INCLUDE BOUND.

ENHANCEMENT-POINT LMGD1OV0_02 SPOTS ES_LMGD1OV0 STATIC INCLUDE BOUND.

ENHANCEMENT-POINT LMGD1OV0_03 SPOTS ES_LMGD1OV0 STATIC INCLUDE BOUND.

ENHANCEMENT-POINT LMGD1OV0_04 SPOTS ES_LMGD1OV0 STATIC INCLUDE BOUND.

ENHANCEMENT-POINT LMGD1OV0_05 SPOTS ES_LMGD1OV0 STATIC INCLUDE BOUND.


ENHANCEMENT-POINT LMGD1OV0_06 SPOTS ES_LMGD1OV0 STATIC INCLUDE BOUND.
