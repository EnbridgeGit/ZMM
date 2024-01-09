*----------------------------------------------------------------------*
*   INCLUDE LMGD1O05                                                   *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  GET_PROW_SUB  OUTPUT
*&---------------------------------------------------------------------*
*       Analog GET_DATEN_SUB aber speziell für Prognosewerte         *
*----------------------------------------------------------------------*
MODULE GET_PROW_SUB OUTPUT.

  CHECK NOT ANZ_SUBSCREENS IS INITIAL.

  IF NOT KZ_EIN_PROGRAMM IS INITIAL.
    IF NOT KZ_BILDBEGINN IS INITIAL.
      CLEAR SUB_ZAEHLER.
*     IF BILDFLAG IS INITIAL OR NOT BILDTAB-KZPRO IS INITIAL
* AHE: 19.02.96 bildtab-kzpro darf nicht mehr benutzt werden (3.0D)
      IF BILDFLAG IS INITIAL OR NOT RMMZU-BILDPROZ IS INITIAL.
        PERFORM ZUSATZDATEN_GET_SUB.
      ENDIF.
    ENDIF.

* Achtung:
* Spezielles Lesen der Prognose-Daten und wir befinden uns NICHT auf
* dem ersten Bildbaustein des Trägerdynpros  ==> es muß unabhängig von
* KZ_BILDBEGINN gelesen werden (und nicht direkt hinter dem Form
* ZUSATZDATEN_GET_SUB).
    CALL FUNCTION 'PROW_GET_SUB'
         TABLES
              WPROWM = TPROWF
              XPROWM = DTPROWF
              YPROWM = LTPROWF.

    CALL FUNCTION 'MPOP_GET_SUB'
         IMPORTING
              WMPOP = MPOP
              XMPOP = *MPOP
              YMPOP = LMPOP.
* AHE: 20.11.95 - A
    CALL FUNCTION 'MARC_GET_SUB'
         IMPORTING
              WMARC = MARC
              XMARC = *MARC
              YMARC = LMARC.

    CALL FUNCTION 'T001W_SINGLE_READ'
         EXPORTING
              KZRFB       = KZRFB
              T001W_WERKS = MARC-WERKS
         IMPORTING
              WT001W      = T001W
         EXCEPTIONS
              NOT_FOUND   = 1
              OTHERS      = 2.
* AHE: 20.11.95 - E

  ELSE.
*   IF BILDFLAG IS INITIAL OR NOT BILDTAB-KZPRO IS INITIAL
* AHE: 19.02.96 bildtab-kzpro darf nicht mehr benutzt werden (3.0D)
    IF BILDFLAG IS INITIAL OR NOT RMMZU-BILDPROZ IS INITIAL.
      PERFORM ZUSATZDATEN_GET_SUB.
      CALL FUNCTION 'PROW_GET_SUB'
           TABLES
                WPROWM = TPROWF
                XPROWM = DTPROWF
                YPROWM = LTPROWF.

      CALL FUNCTION 'MPOP_GET_SUB'
           IMPORTING
                WMPOP = MPOP
                XMPOP = *MPOP
                YMPOP = LMPOP.

* AHE: 20.11.95 - A
      CALL FUNCTION 'MARC_GET_SUB'
           IMPORTING
                WMARC = MARC
                XMARC = *MARC
                YMARC = LMARC.

      CALL FUNCTION 'T001W_SINGLE_READ'
           EXPORTING
                KZRFB       = KZRFB
                T001W_WERKS = MARC-WERKS
           IMPORTING
                WT001W      = T001W
           EXCEPTIONS
                NOT_FOUND   = 1
                OTHERS      = 2.
* AHE: 20.11.95 - E

    ENDIF.
  ENDIF.

ENDMODULE.                             " GET_PROW_SUB  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  SET_PROW_SUB  OUTPUT
*&---------------------------------------------------------------------*
*       Analog SET_DATEN_SUB aber speziell für Prognosewerte         *
*----------------------------------------------------------------------*
MODULE SET_PROW_SUB OUTPUT.

*mk/3.0E Setzen Kz. 'Status-Update am Ende des Bildes erforderlich',
*falls auf dem Bild Felder zu statusrelevanten Tabellen vorhanden
*sind
  IF RMMZU-KZSTAT_UPD IS INITIAL.
    LOOP AT SUB_PTAB WHERE NOT KZSTA IS INITIAL.
      RMMZU-KZSTAT_UPD = X.
    ENDLOOP.
  ENDIF.
  IF ANZ_SUBSCREENS IS INITIAL.
* Keine Bildbausteine auf dem Bild vorhanden
    CALL FUNCTION 'MAIN_PARAMETER_SET_REFTAB'
          EXPORTING
              RMMZU_KZSTAT_UPD = RMMZU-KZSTAT_UPD
         TABLES
              REFTAB = REFTAB.
  ELSEIF NOT KZ_EIN_PROGRAMM IS INITIAL.
* Bildbausteine auf dem Bild vorhanden, alle aus einheitlichem Programm
    CLEAR KZ_BILDBEGINN.
    SUB_ZAEHLER = SUB_ZAEHLER + 1.
    IF SUB_ZAEHLER EQ ANZ_SUBSCREENS.
      KZ_BILDBEGINN = X.               "für PAI notwendig
      CALL FUNCTION 'MAIN_PARAMETER_SET_REFTAB'
          EXPORTING
              RMMZU_KZSTAT_UPD = RMMZU-KZSTAT_UPD
           TABLES
                REFTAB = REFTAB.
    ENDIF.
  ELSE.
* Bildbausteine auf dem Bild vorhanden, aus unterschiedlichen Programmen
    PERFORM ZUSATZDATEN_SET_SUB.

    CALL FUNCTION 'PROW_SET_SUB'
         EXPORTING
              MATNR  = RMMG1-MATNR
              WERKS  = RMMG1-WERKS
         TABLES
              WPROWM = TPROWF.

    MPOP-MANDT = SY-MANDT.
    MPOP-MATNR = RMMG1-MATNR.
    MPOP-WERKS = RMMG1-WERKS.
    CALL FUNCTION 'MPOP_SET_SUB'
         EXPORTING
              WMPOP = MPOP.

    CALL FUNCTION 'MAIN_PARAMETER_SET_REFTAB'
          EXPORTING
              RMMZU_KZSTAT_UPD = RMMZU-KZSTAT_UPD
         TABLES
              REFTAB = REFTAB.
  ENDIF.

ENDMODULE.                             " SET_PROW_SUB  OUTPUT


*&---------------------------------------------------------------------*
*&      Module    TPROWF_ANZEIGEN  OUTPUT
*&---------------------------------------------------------------------*
*       Anzeigen der Prognosewerte.
*----------------------------------------------------------------------*
MODULE TPROWF_ANZEIGEN OUTPUT.

  IF SY-STEPL = 1.
    PW_ZLEPROSEITE = SY-LOOPC.
  ENDIF.

  PW_AKT_ZEILE = PW_ERSTE_ZEILE + SY-STEPL.
  READ TABLE TPROWF INDEX PW_AKT_ZEILE.
  IF SY-SUBRC NE 0.
    IF T130M-AKTYP = AKTYPA OR
       T130M-AKTYP = AKTYPZ.
      EXIT FROM STEP-LOOP.
    ELSE.
      CLEAR T009B_ERROR.
      NO_T009B_ABEND = X.
      PERFORM TPROWF_ERWEITERN.
      CLEAR NO_T009B_ABEND.
      IF T009B_ERROR = X.
        EXIT FROM STEP-LOOP.
      ENDIF.
      MOVE-CORRESPONDING TPROWF TO RM03M.
      PERFORM DATUMSAUFBEREITUNG USING TPROWF-ERTAG RM03M-PRIOD.
*     RM03M-ANTEI = 100.   " Wert aus altem Mat-Stamm; heißt: 1.00
      RM03M-ANTEI = 1.                 " wegen Fließkommaarithmetik
    ENDIF.
  ELSE.
    MOVE-CORRESPONDING TPROWF TO RM03M.
    PERFORM DATUMSAUFBEREITUNG USING TPROWF-ERTAG RM03M-PRIOD.
    IF RM03M-PRWRT NE 0.
*--- Feldüberlauf abgefragt;
*     RECHFELD = RM03M-KOPRW * 100 / RM03M-PRWRT.  " raus wg. Fließkomm.
      RECHFELD = RM03M-KOPRW / RM03M-PRWRT.
      IF RECHFELD GT MAX_ANTEI.
        CLEAR RM03M-ANTEI.
      ELSE.
*       RM03M-ANTEI = RM03M-KOPRW * 100 / RM03M-PRWRT. "raus wg. Fließko
        RM03M-ANTEI = RM03M-KOPRW / RM03M-PRWRT.
      ENDIF.
    ELSE.
      IF RM03M-KOPRW = 0.
*       RM03M-ANTEI = 100.   " Wert aus altem Mat-Stamm heißt: 1.00
        RM03M-ANTEI = 1.
      ELSE.
        CLEAR RM03M-ANTEI.      "hier müßte Anteil unendlich sein !
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                             " TPROWF_ANZEIGEN  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  PW_INITIALISIERUNG  OUTPUT
*&---------------------------------------------------------------------*
*       Initialisieren Daten für Prognosewerte
*----------------------------------------------------------------------*
MODULE PW_INITIALISIERUNG OUTPUT.

* erster Aufruf
  IF RMMZU-PWINIT IS INITIAL.
*   Initflag setzen
    RMMZU-PWINIT = X.
    CLEAR PW_ERSTE_ZEILE.
  ENDIF.

  PERKZ = MARC-PERKZ.  " Zur Aufbereitung der Periodenanzeige
  PERIV = MARC-PERIV.                  " auf Subscreen notwendig !
  RMMG2-FLGPROGW = X.


* ------- Ermitteln aktuelle Anzahl Prognoseeinträge
  DESCRIBE TABLE TPROWF LINES PW_LINES.

ENDMODULE.                             " PW_INITIALISIERUNG  OUTPUT
