*----------------------------------------------------------------------*
***INCLUDE LZMM_LOCMASTO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  DISABLE_ICONS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISABLE_ICONS OUTPUT.
  excl_cua_funct-function = 'DELE'.
  APPEND excl_cua_funct.
  excl_cua_funct-function = 'NEWL'.
  APPEND excl_cua_funct.
  excl_cua_funct-function = 'KOPE'.
  APPEND excl_cua_funct.

ENDMODULE.                 " DISABLE_ICONS  OUTPUT
