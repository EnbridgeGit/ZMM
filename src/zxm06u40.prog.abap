*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_UCOMM) LIKE  SY-UCOMM
*"  EXPORTING
*"     VALUE(E_CI_UPDATE) LIKE  SY-CALLD
*"     VALUE(E_UCOMM) LIKE  SY-UCOMM
*"  CHANGING
*"     VALUE(E_CI_EKPO) LIKE  EKPO_CI STRUCTURE  EKPO_CI OPTIONAL
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&  Include           ZXM06U40
*&---------------------------------------------------------------------*
IF ekpo-zzariba_approver NE zekpo-zzariba_approver.
  e_ci_ekpo-zzariba_approver = ekpo-zzariba_approver.
  e_ci_update = 'X'.
ENDIF.
