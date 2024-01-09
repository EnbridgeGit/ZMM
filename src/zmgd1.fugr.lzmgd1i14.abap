*-------------------------------------------------------------------
***INCLUDE LMGD1I14 .
*-------------------------------------------------------------------

  INCLUDE LMGD1I6K .  " CHECK_EAN_ZUS


  INCLUDE LMGD1I6J .  " OKCODE_EAN_ZUS


  INCLUDE LMGD1I6I .  " CHECK_HPEAN


  INCLUDE LMGD1I6H .  " OKCODE_EADE


  INCLUDE LMGD1I6G .  " DUB_DEL_EAN_ZUS


  INCLUDE LMGD1I6F .  " BILDFLAG_BLAETTERN_EAN_ZUS


  INCLUDE LMGD1I6E .  " PREPARE_EAN_ZUS


  INCLUDE LMGD1I6D .  " CHECK_NEW_MEINH


  INCLUDE LMGD1I6C .  " UPDATE_HPEAN


  INCLUDE LMGD1I6B .  " ZEILE_ERMITTELN


  INCLUDE LMGD1I6A .  " INIT_EAN


  INCLUDE LMGD1I69 .  " CLEAN_MEINH


  INCLUDE LMGD1I68 .  " BELEGEN_MEAN_ME_TAB


  INCLUDE LMGD1I67 .  " SORT


  INCLUDE LMGD1I66 .  " MLEA_LFEAN


  INCLUDE LMGD1I65 .  " CHECK_LFEAN


*&---------------------------------------------------------------------*
*&      Module  CHECK_MEINH  INPUT
*&---------------------------------------------------------------------*
*       Testet, ob die eingegebene Mengeneinheit zum Material gehört.
*----------------------------------------------------------------------*

* AHE: 19.07.96  - neues Modul ! !

  INCLUDE LMGD1I64 .  " CHECK_MEINH
