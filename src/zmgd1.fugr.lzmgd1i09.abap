*-------------------------------------------------------------------
*  INCLUDE LMGD1I03 .
*    PAI-Module für Kurztexthandling
*-------------------------------------------------------------------

  INCLUDE LMGD1I5C .  " BILDFLAG_BLAETTERN

*&---------------------------------------------------------------------*
*&      Module  SKTEXT-MAKTX  INPUT
*&---------------------------------------------------------------------*
*Pruefen der Eintraege auf lokale Fehler, wie
*-Falscher Sprachenschluessel
*-keinen Sprachenschluessel zum Text
*-keinen Text zum Sprachenschluessel.
*Uebernehmen der Eintraege in die interne Tabelle.
*Setzen Bearbeitungsflag abhaengig davon, ob es sich um einen neuen
*oder geaenderten Eintrag handelt.
*Setzen Loeschflag, falls der Eintrag zurueckgenommen wurde.
*------------------------------------------------------------------
* //br050396 für RT 1.1 .. Einzeltext für Anzeige im Kopf
* - Handling bei SA / Einzelartikel wie bisher
* - bei Varianten steht in MAKT der Text zum SA, der Text zur Variante
*   steht in RMMW1_BEZ-VARTX
* (integrated here from d2i09 to 4.0 wk
*------------------------------------------------------------------

  INCLUDE LMGD1I5B .  " SKTEXT-MAKTX

  INCLUDE LMGD1I5A .  " OK_CODE_KTEXT_I

  INCLUDE LMGD1I59 .  " OK_CODE_KTEXT_II

  INCLUDE LMGD1I58 .  " SETZEN_NACHRICHT
