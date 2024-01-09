  INCLUDE LMGD1O0S .  " SONFAUSW_MEGRP

*&---------------------------------------------------------------------*
*&      Module  ANZEIGEN_MEINH  OUTPUT
*&---------------------------------------------------------------------*
*   Ermitteln Anzahl Einträge.
*   Fuellen der Loop-Zeile mit den Daten aus der internen Tabelle
*   Bildschirmmodifikation für vorhandene ME's:
*   -  Die komplette Zeile ist nicht eingabebereit
*      - für die Zeile zur Basis-ME (außer EAN, Abmessungen)
*      - im Anzeigefall immer
*   - Beim Anlegen/Ändern/Erweitern ist
*     nur die alternative ME nicht eingabebereit
*   - Im Fehlerfalle wird die Zeile helleuchtend dargestellt
*   Bildschirmmodifikation für neue Zeilen (bisher leer)
*   -  Die komplette Zeile ist im Anzeige
*      nicht eingabebereit, der Step-Loop wird verlassen
*----------------------------------------------------------------------*

  INCLUDE LMGD1O0R .  " ANZEIGEN_MEINH

  INCLUDE LMGD1O0Q .  " ME_EINTRAEGE_ERMITTELN

  INCLUDE LMGD1O0P .  " ME_INITIALISIERUNG

  INCLUDE LMGD1O0O .  " RM03E-ZEANS

  INCLUDE LMGD1O1H.   " me_anzahl_sub_berechnen, Rel. 4.6A JW

  include lmgd1O1I.   " MEGRP_SET_VALUES
ENHANCEMENT-POINT LMGD1O04_01 SPOTS ES_LMGD1O04 STATIC INCLUDE BOUND.
