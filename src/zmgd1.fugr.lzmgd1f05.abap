*-------------------------------------------------------------------
***INCLUDE LMGD1F05 .
*-------------------------------------------------------------------

*---------------------------------------------------------------------*
*       FORM MEAN_ME_TAB_AKT
*---------------------------------------------------------------------
*   Aktualisieren der int. Tabelle MEAN_ME_TAB wg. Benutzereingabe
*---------------------------------------------------------------------
*   keine USING-Parameter                                             *
*---------------------------------------------------------------------*

  INCLUDE LMGD1F1O .  " MEAN_ME_TAB_AKT


  INCLUDE LMGD1F1N .  " TMLEA_AKT


  INCLUDE LMGD1F1M .  " TMLEA_AKT_MEINH


  INCLUDE LMGD1F1L .  " OK_CODE_EAN_ZUS


  INCLUDE LMGD1F1K .  " EAN_SET_ZEILE


  INCLUDE LMGD1F1J .  " EAN_SET_ZEILE_LFEAN


  INCLUDE LMGD1F1I .  " SET_UPDATE_TAB


  INCLUDE LMGD1F1H .  " DEL_EAN_LIEF


  INCLUDE LMGD1F1G .  " DEL_EAN_LIEF_MEINH


  INCLUDE LMGD1F1F .  " UPD_EAN_LIEF


  INCLUDE LMGD1F1E .  " UPD_EAN_LIEF_MEINH


  INCLUDE LMGD1F1D .  " SET_SCREEN_FIELD_VALUE

* Start: EAN.UCC Functionality
  INCLUDE EAN_UCC_ROUTINES IF FOUND. " EAN.UCC Functionality
* End:
