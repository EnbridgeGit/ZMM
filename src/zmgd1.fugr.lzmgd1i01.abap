  INCLUDE LMGD1I1B .                   " MAKT-MAKTX

  INCLUDE LMGD1I1A .                   " MARA-BSTME

  INCLUDE LMGD1I19 .                   " MARA-EAN11

  INCLUDE LMGD1I18 .                   " MARA-EKWSL

  INCLUDE LMGD1I0Z .                   " MARA-ERGEW

  INCLUDE LMGD1I0Y .                   " MARA-ERVOL

  INCLUDE LMGD1I0X .                   " MARA-ETIFO

  INCLUDE LMGD1I0W .                   " MARA-GEWEI


*------------------------------------------------------------------

  INCLUDE LMGD1I0V .                   " MARA-MATKL


  INCLUDE LMGD1I0U .                   " MARA-MEABM

*------------------------------------------------------------------
*    Module MARA-MEINS.
*Allgemeine Prüfungen beim Pflegen der Basismengeneinheit:
*- neue Basis-ME darf nicht bereits als Alternativ-ME vorkommen
*Prüfungen beim Anlegen des Materials (Basis-ME nur bei Neuanlage
*eingabebereit):
*- Beim Anlegen erfolgt eine Warnung, wenn die Basis-ME nachträglich
*  geändert wurde, aber bereits Alternativ-ME's vorhanden waren.
*- Beim Anlegen wird generell die Basis-ME in die Tabelle MEINH
*  eingestellt - wurde die Basis-ME geändert wird der alte Eintrag
*  in den neuen Eintrag kopiert (es erfolgt eine Warnung, wenn
*  zur alten Basis-ME EAN- bzw. Abpackungsdaten vorhanden waren). Der
*  alte Eintrag wird gelöscht
*Prüfungen beim Ändern des Materials:
*- Sperren des kompletten Materialstamms vorab.
*- Prüfen, ob abhängige Objekte vorhanden sind
*  ab 2.0 auch Prüfung, ob das Material in Arbeitsplänen verwendet wird
*  ab 2.0 Prüfung bzgl. Kundenkonsi- und Kundenleergutbestände
*  ab 2.1 Prüfung bzgl. Lieferantenbeistellungs- und Kundenauftragsbest.
*- Führen die Prüfungen zu einem Fehler, wird die Sperre zurückgenommen
*- Die EAN-Spez. Daten der bisherigen Basis-ME werden, falls vorhanden,
*  mit einer entsprechenden Warnung in die neue Basis-ME übernommen.
*  Die alte Basis-ME wird aus der Tabelle MEINH gelöscht.
*Achtung:
*Modul läuft jetzt nicht mehr on chain-request ab    mk/20.02.93
*(stattdessen Vergleich mit ret_meins eingeführt).
*(dadurch auch kein Sonderhandling mehr für BTCI dadurch, daß Basis-Me
*auf jedem Bild versorgt wird)
*ab 2.1B Prüfung, ob eine kaufmännische Einheit eingegeben wurde
*ab 2.2 - Setzen des KZ_MEINS_DIMLESS

  INCLUDE LMGD1I0T .                   " MARA-MEINS


  INCLUDE LMGD1I0S .                   " MARA-MHDRZ


  INCLUDE LMGD1I0R .                   " MARA-SPART

  INCLUDE LMGD1I0Q .                   " MARA-KUNNR

  INCLUDE LMGD1I0P .                   " MARA-KZKFG

  INCLUDE LMGD1I0O .                   " MARA-PRDHA

  INCLUDE LMGD1I0N .                   " MARA-SATNR

  INCLUDE LMGD1I0M .                   " MARA-STOFF

  INCLUDE LMGD1I0L .                   " MARA-VOLEH

  INCLUDE LMGD1I0K .                   " DATENUEBERNAHME_BME


  INCLUDE LMGD1I0J .                   " DATEN_WERTESCHL

  INCLUDE LMGD1I0I .                   " GEWICHTE

  INCLUDE LMGD1I0H .                   " BERECHTIGUNG_MATNR


  INCLUDE LMGD1I0G .                   " INIT_FLAG
  INCLUDE LMGD1I0F .                   " MARA_WRKST
  INCLUDE LMGD1I0E .                   " MARA_BMATN

  INCLUDE LMGD1I0D .                   " MARA-KZGVH

  INCLUDE LMGD1I0C .                   " MARA-XGCHP

  INCLUDE LMGD1I0B .                   " MARA-MSTDV

  INCLUDE LMGD1I0A .                   " MARA-MSTDE

  INCLUDE LMGD1I7U .                   " MARA-RMATP

* AHE: 07.04.99 - A (4.6a)
*&---------------------------------------------------------------------*
*&      Module  MARA-EXTWG  INPUT
*&---------------------------------------------------------------------*
*       Check Existenz MARA-EXTWG mit Customizbarer Meldung
*----------------------------------------------------------------------*
  MODULE MARA-EXTWG INPUT.

    CHECK BILDFLAG = SPACE.
    CHECK T130M-AKTYP NE AKTYPA AND T130M-AKTYP NE AKTYPZ.

    CALL FUNCTION 'MARA_EXTWG'
         EXPORTING
              P_MARA_EXTWG  = MARA-EXTWG
              P_LMARA_EXTWG = LMARA-EXTWG.
*             P_MESSAGE      = ' '
*        EXCEPTIONS
*             ERR_MARA_EXTWG = 1
*             OTHERS         = 2.

  ENDMODULE.                           " MARA-EXTWG  INPUT
* AHE: 07.04.99 - E


* AHE: 26.05.99 - A (4.6a) HW 154217
* komplett neues Modul
* AHE: 07.09.99 <<<<<<<<<<<<<<< HW deaktiviert, da nicht durchgängig
*                               im System
*----------------------------------------------------------------------*
*       Aufruf der speziellen Eingabehilfe für MARC-SOBSL              *
*----------------------------------------------------------------------*
* MODULE MARC-SOBSL_HELP.
*
*   PERFORM SET_DISPLAY.
*
*   CALL FUNCTION 'MARC_SOBSL_HELP'
*        EXPORTING
*             WERK    = MARC-WERKS
*             DISPLAY = DISPLAY
*        IMPORTING
*             SOBSL   = MARC-SOBSL.
*
* ENDMODULE.

* AHE: 26.05.99 - E
