*&---------------------------------------------------------------------*
*&  Include           ZXMG0U03
*&---------------------------------------------------------------------*

*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(MMUE2) LIKE  MMUE2 STRUCTURE  MMUE2
*"       CHANGING
*"             VALUE(MATNR) LIKE  MARA-MATNR
*"----------------------------------------------------------------------

"BTBOUNDY
"MDM

AUTHORITY-CHECK OBJECT 'Z_MATE_NEW'
         ID 'MTART' FIELD mmue2-mtart.

IF sy-subrc <> 0.
  "Not authorized to create new material number.
  MESSAGE ID 'S#' TYPE 'E' NUMBER '077'
    WITH 'create new number'.
ENDIF.
