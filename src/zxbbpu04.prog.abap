*&---------------------------------------------------------------------*
*&  Include           ZXBBPU04
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IM_BBPDECDATA) LIKE  BBPDECDATA STRUCTURE  BBPDECDATA
*"     VALUE(IV_MANU_PROF) TYPE  MANPRC OPTIONAL
*"  CHANGING
*"     VALUE(CH_PROFILE) LIKE  T160EX-EPROFILE
*"----------------------------------------------------------------------

"Exclude MRP from sending to SRM.
IF im_bbpdecdata-estkz = 'B'.
  CLEAR ch_profile.
ENDIF.
