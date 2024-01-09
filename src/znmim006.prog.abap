************************************************************************
*        Performroutinen für Selektionsparameter                       *
************************************************************************
*eject.
*----------------------------------------------------------------------*
*  Anforderungsbild bearbeiten                                         *
*----------------------------------------------------------------------*
FORM ANFORDERUNGSBILD USING ANF_BSTYF ANF_BSTYK ANF_BSTYL ANF_BSTYA.

PERFORM T160B_LESEN USING SY-TCODE.
CHECK SY-SUBRC EQ 0.
PERFORM T160B_ZUWEISEN USING ANF_BSTYF ANF_BSTYK ANF_BSTYL ANF_BSTYA.

ENDFORM.

*----------------------------------------------------------------------*
*  Lesen der Tabelle T160B                                             *
*----------------------------------------------------------------------*
FORM T160B_LESEN USING TCODE LIKE SY-TCODE.

MOVE TEXT-KEY TO SSCRFIELDS-FUNCTXT_01.

IMPORT BUEB FROM MEMORY.
IMPORT EUEB FROM MEMORY.
IMPORT WUEB-CALKZ WUEB-NRCFD WUEB FROM MEMORY.

SELECT SINGLE * FROM T160B WHERE TCODE = TCODE.
IF SY-SUBRC NE 0.
   IF BUEB EQ SPACE AND
      EUEB EQ SPACE.
      MESSAGE A207 WITH TCODE.
   ENDIF.
ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  Werte aus der Tabelle T106B an Parameter zuweisen                   *
*----------------------------------------------------------------------*

FORM T160B_ZUWEISEN USING ANF_BSTYF ANF_BSTYK ANF_BSTYL ANF_BSTYA.

LISTU = T160B-LISTU.

REFRESH SELPA.
CLEAR   SELPA.
IF T160B-SELPA NE SPACE.
   SELPA-SIGN = 'I'.
   SELPA-OPTION = 'EQ'.
   SELPA-LOW = T160B-SELPA.
   APPEND SELPA.
ENDIF.

ANF_BSTYF = T160B-BSTYF.
ANF_BSTYK = T160B-BSTYK.
ANF_BSTYL = T160B-BSTYL.
ANF_BSTYA = T160B-BSTYA.
EKDY-SELKF = T160B-BSTYF.
EKDY-SELKK = T160B-BSTYK.
EKDY-SELKL = T160B-BSTYL.
EKDY-SELKA = T160B-BSTYA.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Modifizieren Anforderungsbild                                       *
*----------------------------------------------------------------------*
FORM MODIFY_ANFO USING MAN_SELTYP.

LOOP AT SCREEN.
   IF SCREEN-NAME CS 'P_GULDT'.
      IF ( EKDY-SELKK EQ SPACE AND EKDY-SELKL EQ SPACE ) OR
           MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'P_RWEIT'.
      IF ( EKDY-SELKK EQ SPACE AND EKDY-SELKL EQ SPACE ) OR
           MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_ANGDT'.
      IF EKDY-SELKA EQ SPACE.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_BSART'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_KNTTP'.
      IF ( EKDY-SELKF EQ SPACE AND
           EKDY-SELKK EQ SPACE AND
           EKDY-SELKL EQ SPACE     ) OR
         MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_PSTYP'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_EINDT'.
      IF EKDY-SELKF EQ SPACE   AND
         EKDY-SELKA EQ SPACE   AND
         EKDY-SELKL EQ SPACE.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_EBELN'.
      IF MAN_SELTYP = 'N' OR MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_LIFNR'.
      IF MAN_SELTYP = 'L'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_RESWK'.
      IF MAN_SELTYP = 'L' OR MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_MATNR'.
      IF MAN_SELTYP = 'M'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_MATKL'.
      IF MAN_SELTYP = 'C' OR MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT     = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_BEDAT'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_EAN11'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_IDNLF'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_LTSNR'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_AKTNR'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_SAISO'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_SAISJ'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'S_SAISJ'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'P_TXZ01'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
   IF SCREEN-NAME CS 'P_NAME1'.
      IF MAN_SELTYP = 'W'.
         SCREEN-INVISIBLE = 1.
         SCREEN-INPUT = 0.
         MODIFY SCREEN.
      ENDIF.
   ENDIF.
ENDLOOP.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*  Einkaufsbelegtyp über POP-UP bestimmen                              *
*----------------------------------------------------------------------*
FORM BSTYP_BESTIMMEN USING BSB_BSTYF BSB_BSTYK BSB_BSTYL BSB_BSTYA.

EKDY-SELKF = BSB_BSTYF.
EKDY-SELKK = BSB_BSTYK.
EKDY-SELKL = BSB_BSTYL.
EKDY-SELKA = BSB_BSTYA.
  IF SSCRFIELDS-UCOMM = 'FC01'.
    CALL FUNCTION 'RM_SENDEN_BILD_EXM'
         EXPORTING
              E0_SELKA = EKDY-SELKA
              E0_SELKB = EKDY-SELKF
              E0_SELKK = EKDY-SELKK
              E0_SELKL = EKDY-SELKL
         IMPORTING
              E0_SELKA = EKDY-SELKA
              E0_SELKB = EKDY-SELKF
              E0_SELKK = EKDY-SELKK
              E0_SELKL = EKDY-SELKL.
     BSB_BSTYF = EKDY-SELKF.
     BSB_BSTYK = EKDY-SELKK.
     BSB_BSTYL = EKDY-SELKL.
     BSB_BSTYA = EKDY-SELKA.
  ENDIF.
  REFRESH R_BSTYP.
  R_BSTYP-SIGN   = 'I'.
  R_BSTYP-OPTION = 'EQ'.
  IF EKDY-SELKF NE SPACE.
    R_BSTYP-LOW    = 'F'.
    APPEND R_BSTYP.
  ENDIF.
  IF EKDY-SELKK NE SPACE.
    R_BSTYP-LOW    = 'K'.
    APPEND R_BSTYP.
  ENDIF.
  IF EKDY-SELKL NE SPACE.
    R_BSTYP-LOW    = 'L'.
    APPEND R_BSTYP.
  ENDIF.
  IF EKDY-SELKA NE SPACE.
    R_BSTYP-LOW    = 'A'.
    APPEND R_BSTYP.
  ENDIF.
  DESCRIBE TABLE R_BSTYP LINES SY-TFILL.
  IF SY-TFILL = 0.
    MESSAGE E002(DM).
  ENDIF.
ENDFORM.

*eject
*----------------------------------------------------------------------*
* Preisanzeigeberechtigung setzen aus Hilfsreports                     *
*----------------------------------------------------------------------*
FORM PREISANZ_SETZEN USING PAS_XFELD.

PREISANZ = PAS_XFELD.

ENDFORM.

*eject
*----------------------------------------------------------------------*
* Preisanzeigeberechtigung Kopf                                        *
*----------------------------------------------------------------------*
FORM PREISBER_KOPF.

PREISANZ = 'X'.
AENDANZ = 'X'.
CLEAR PREISPOS.
CLEAR AENDPOS.
*- Objekttext bestimmen -----------------------------------------------*
CASE EKKO-BSTYP.
   WHEN 'A'.
      XOBJEKT = 'M_ANFR_'.
   WHEN 'F'.
      XOBJEKT = 'M_BEST_'.
   WHEN 'L'.
      XOBJEKT = 'M_RAHM_'.
   WHEN 'K'.
      XOBJEKT = 'M_RAHM_'.
ENDCASE.

*- Einkaufsorganisation -----------------------------------------------*
XOBJEKT+7(3) = 'EKO'.
AUTHORITY-CHECK OBJECT XOBJEKT
     ID 'ACTVT' FIELD '09'
     ID 'EKORG' FIELD EKKO-EKORG.
IF SY-SUBRC NE 0.
   CLEAR PREISANZ.
   EXIT.
ENDIF.
AUTHORITY-CHECK OBJECT XOBJEKT
     ID 'ACTVT' FIELD '08'
     ID 'EKORG' FIELD EKKO-EKORG.
IF SY-SUBRC NE 0.
   CLEAR AENDANZ.
   EXIT.
ENDIF.

*- Einkäufergruppe ----------------------------------------------------*
XOBJEKT+7(3) = 'EKG'.
AUTHORITY-CHECK OBJECT XOBJEKT
     ID 'ACTVT' FIELD '09'
     ID 'EKGRP' FIELD EKKO-EKGRP.
IF SY-SUBRC NE 0.
   CLEAR PREISANZ.
   EXIT.
ENDIF.
AUTHORITY-CHECK OBJECT XOBJEKT
     ID 'ACTVT' FIELD '08'
     ID 'EKGRP' FIELD EKKO-EKGRP.
IF SY-SUBRC NE 0.
   CLEAR AENDANZ.
   EXIT.
ENDIF.

*- Belegart -----------------------------------------------------------*
XOBJEKT+7(3) = 'BSA'.
AUTHORITY-CHECK OBJECT XOBJEKT
     ID 'ACTVT' FIELD '09'
     ID 'BSART' FIELD EKKO-BSART.
IF SY-SUBRC NE 0.
   CLEAR PREISANZ.
   EXIT.
ENDIF.
AUTHORITY-CHECK OBJECT XOBJEKT
     ID 'ACTVT' FIELD '08'
     ID 'BSART' FIELD EKKO-BSART.
IF SY-SUBRC NE 0.
   CLEAR AENDANZ.
   EXIT.
ENDIF.

ENDFORM.

*eject
*----------------------------------------------------------------------*
* Preisanzeigeberechtigung Position                                    *
*----------------------------------------------------------------------*
FORM PREISBER_POS.

IF PREISPOS EQ 'X'.
   PREISANZ = 'X'.
ENDIF.
CHECK PREISANZ NE SPACE.
PREISPOS = 'X'.
IF AENDPOS EQ 'X'.
   AENDANZ = 'X'.
ENDIF.
CHECK AENDANZ NE SPACE.
AENDPOS = 'X'.
*- WERK ---------------------------------------------------------------*
IF EKPO-WERKS NE SPACE.
   XOBJEKT+7(3) = 'WRK'.
   AUTHORITY-CHECK OBJECT XOBJEKT
        ID 'ACTVT' FIELD '09'
        ID 'WERKS' FIELD EKPO-WERKS.
   IF SY-SUBRC NE 0.
      CLEAR PREISANZ.
      EXIT.
   ENDIF.
   AUTHORITY-CHECK OBJECT XOBJEKT
        ID 'ACTVT' FIELD '08'
        ID 'WERKS' FIELD EKPO-WERKS.
   IF SY-SUBRC NE 0.
      CLEAR AENDANZ.
      EXIT.
   ENDIF.
ENDIF.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Lesen Tabelle T160S - Selektionsparameter                           *
*----------------------------------------------------------------------*
FORM SELPA_ANALYSE.

REFRESH XT160S.
VARANZ = 'X'.        "Dummybelegung
LOOP AT SELPA.
   IF SELPA-OPTION NE 'EQ' OR
      SELPA-SIGN   NE 'I'.
      MESSAGE E205.
   ENDIF.
   SELECT SINGLE * FROM T160S WHERE SELPA = SELPA-LOW.
   IF SY-SUBRC NE 0.
      MESSAGE E206 WITH SELPA-LOW.
   ENDIF.
   XT160S = T160S.
   APPEND XT160S.
   CASE VARANZ.
*- initial: setzen aus T160S ------------------------------------------*
     WHEN 'X'.
       VARANZ = T160S-VARANZ.
*- nur Sammelartikel --------------------------------------------------*
     WHEN '1'.
*- wenn nichts spezielles gewünscht -> nichts spezielles = ' ' --------*
       IF T160S-VARANZ EQ SPACE.
          VARANZ = SPACE.
       ENDIF.
*- Wenn Varianten gewünscht -> alles = '3' ----------------------------*
       IF T160S-VARANZ EQ '2' OR
          T160S-VARANZ EQ '3'.
          VARANZ = '3'.         "alle anzeigen
       ENDIF.
*- nur Varianten ------------------------------------------------------*
     WHEN '2'.
*- Wenn keine Varianten gewünscht -> alles = '3' ----------------------*
       IF T160S-VARANZ NE '2'.
          VARANZ = '3'.
       ENDIF.
*- alles -> bleibt so -------------------------------------------------*
     WHEN '3'.
*- keine spezielle Variantenselektion----------------------------------*
     WHEN SPACE.
*- Wenn Varianten gewünscht -> alles = '3' ----------------------------*
       IF T160S-VARANZ EQ '2' OR
          T160S-VARANZ EQ '3'.
          VARANZ = '3'.
       ENDIF.
   ENDCASE.
ENDLOOP.
IF VARANZ EQ 'X'.
   CLEAR VARANZ.
ENDIF.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Pruefen Selektion Kopf                                              *
*----------------------------------------------------------------------*
FORM SELPA_CHECK_KOPF.

REJECT = 'X'.
*- Allgemeine Select-options ------------------------------------------*
CHECK: S_BSART,
       S_EKGRP,
       S_EBELN,
       S_LIFNR,
       S_RESWK,
       S_BEDAT.

*- Angebotsdatum ------------------------------------------------------*
IF EKKO-BSTYP EQ BSTYP-ANFR.
   CHECK S_ANGDT.
ENDIF.

*- Gültigkeitsstichtag ------------------------------------------------*
IF EKKO-BSTYP EQ 'K' OR
   EKKO-BSTYP EQ 'L'.
   IF P_GULDT NE 0.
      IF EKKO-KDATB > P_GULDT OR
         EKKO-KDATE < P_GULDT.
         EXIT.
      ENDIF.
   ENDIF.
ENDIF.

*- Lieferantenname ----------------------------------------------------*
PERFORM EKKO_ANSCHRIFT.
IF P_NAME1 NE SPACE.
   IF EKAN-NAME1 NP P_NAME1.
      EXIT.
   ENDIF.
ENDIF.

CLEAR REJECT.

*- Preisanzeigeberechtigung -------------------------------------------*
PERFORM PREISBER_KOPF.

*- Abrufdoku besorgen -------------------------------------------------*
IF EKKO-BSTYP EQ BSTYP-KONT.
   IF ARCHIV EQ SPACE.
      IF EKABKS NE SPACE OR
         EKABKA NE SPACE.
         CALL FUNCTION 'ME_READ_RELEASE_DOCU'
              EXPORTING
                   EBELN = EKKO-EBELN
                   BUKRS = EKKO-BUKRS
                   WAERS = EKKO-WAERS
                   WKURS = EKKO-WKURS
              IMPORTING
                   SUWRT = EKABS_SUWRT
              TABLES
                   XADO = ADO
                   XADOS = ADOS.
      ENDIF.
   ENDIF.
ENDIF.

*- Selektionsparameter aus T160S prüfen -------------------------------*
READ TABLE XT160S INDEX 1.
IF SY-SUBRC EQ 0.
   REJECT = 'X'.
ENDIF.
LOOP AT XT160S.
   T160S = XT160S.
   IF REJECT EQ SPACE.
      EXIT.
   ENDIF.
   CLEAR REJECT.
*- Kontrakt/Lieferplan ------------------------------------------------*
   IF EKKO-BSTYP EQ BSTYP-KONT OR
      EKKO-BSTYP EQ BSTYP-LFPL.
      PERFORM SELPA_CHECK_KOPF_RAHM.
   ENDIF.
ENDLOOP.


ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Pruefen Selektion für Kontrakte/Lieferpläne Kopf                    *
*----------------------------------------------------------------------*
FORM SELPA_CHECK_KOPF_RAHM.

CLEAR H-WERT.
IF EKKO-BSTYP EQ BSTYP-LFPL.
*  H-WERT = EKKO-NBWRT.
ELSE.
   H-WERT = EKABS_SUWRT.
ENDIF.

*- Zielwert <-> abgerufen/eingeteilt-----------------------------------*
IF T160S-ZWBW0 NE SPACE.
   VFELD1 = EKKO-KTWRT.
   F1 = EKKO-KTWRT * T160S-ZWBWT / 1000.
   VFELD2 = H-WERT + F1.
   PERFORM SELPA_VERGLEICH USING T160S-ZWBW0.
ENDIF.

*- laufzeit abgelaufen ------------------------------------------------*
IF T160S-LFZAB EQ SPACE AND
   EKKO-KDATE NE 0.
   IF SY-DATUM > EKKO-KDATE.
      REJECT = 'X'.
   ENDIF.
ENDIF.

ENDFORM.


*eject.
*----------------------------------------------------------------------*
*  Pruefen Selektion Position                                          *
*----------------------------------------------------------------------*
FORM SELPA_CHECK_POS.

REJECT = 'X'.
*- Allgemeine Select-options ------------------------------------------*
CHECK EKPO-PSTYP IN R_PSTYP.

*- Varianten ----------------------------------------------------------*
*- Nur Standardartikel anzeigen oder keine Variantenselektion ---------*
IF VARANZ EQ SPACE OR
   VARANZ EQ '1'.
   CHECK: S_MATNR.
*- Variantenpositionen (mit) anzeigen ---------------------------------*
ELSE.
   IF EKPO-ATTYP NE ATTYP-VAR.   "keine Variante
      CHECK: S_MATNR.
      IF EKPO-ATTYP EQ ATTYP-SAM.
         CHECK VARANZ NE '2'.  "keine Standardartikel
      ENDIF.
   ELSE.
      CHECK EKPO-MATNR IN S_MATNR OR EKPO-SATNR IN S_MATNR.
   ENDIF.
ENDIF.

CHECK: S_MATKL,
       S_WERKS,
       S_EAN11,
       S_IDNLF,
       S_LTSNR,
       S_AKTNR,
       S_SAISO,
       S_SAISJ.

*- Kontierungstyp -----------------------------------------------------*
IF EKKO-BSTYP NE BSTYP-ANFR.
   CHECK S_KNTTP.
ENDIF.

*- Kurztext -----------------------------------------------------------*
IF P_TXZ01 NE SPACE.
   IF EKPO-TXZ01 NP P_TXZ01.
      EXIT.
   ENDIF.
ENDIF.

*- Abrufdoku lesen ----------------------------------------------------*
IF EKKO-BSTYP EQ BSTYP-KONT.
   IF ARCHIV EQ SPACE.
      IF EKABKS EQ SPACE AND
         EKABKA EQ SPACE AND
       ( EKABPS NE SPACE OR
         EKABPA NE SPACE ).
         CALL FUNCTION 'ME_READ_RELEASE_DOCU'
              EXPORTING
                   EBELN = EKKO-EBELN
                   EBELP = EKPO-EBELP
                   BUKRS = EKKO-BUKRS
                   WAERS = EKKO-WAERS
                   WKURS = EKKO-WKURS
              TABLES
                   XADO = ADO
                   XADOS = ADOS.
      ENDIF.
   ENDIF.
ENDIF.
*- Reichweite ---------------------------------------------------------*
IF P_RWEIT NE 0.
   IF EKKO-BSTYP EQ BSTYP-KONT.
      CHECK EKPO-KTMNG NE 0.
      PERFORM REICHWEITE_KONT.
      IF EXITFLAG NE SPACE.
         EXIT.
      ENDIF.
   ENDIF.
   IF EKKO-BSTYP EQ BSTYP-LFPL.
      CHECK EKPO-KTMNG NE 0.
      PERFORM REICHWEITE_LFPL.
      IF EXITFLAG NE SPACE.
         EXIT.
      ENDIF.
   ENDIF.
ENDIF.

*- Lieferdatum --------------------------------------------------------*
IF EKKO-BSTYP NE BSTYP-KONT.
   READ TABLE S_EINDT INDEX 1.
   IF SY-SUBRC EQ 0.
      PERFORM EKET_LESEN.
      LOOP AT ETT.
         EKET-EINDT = ETT-EINDT.
         CHECK S_EINDT.
         CLEAR REJECT.
         EXIT.
      ENDLOOP.
      CHECK REJECT EQ SPACE.
   ENDIF.
ENDIF.

CLEAR REJECT.

*- Preisanzeigeberechtigung -------------------------------------------*
PERFORM PREISBER_POS.

*- WE-RE-Daten besorgen -----------------------------------------------*
IF EKKO-BSTYP NE BSTYP-KONT AND
   EKKO-BSTYP NE BSTYP-ANFR.
   IF ARCHIV EQ SPACE.
      REFRESH: BET, BZT, BETS, BETZ, XEKBNK.
      CLEAR:   BET, BZT, BETS, BETZ, XEKBNK.
      IF EKBEPS NE SPACE OR
         EKBEPA NE SPACE.
         IF EKPO-ATTYP NE ATTYP-SAM OR EKPO-UPVOR EQ SPACE.
            IF EKPO-MENGE NE 0.
               CALL FUNCTION 'ME_READ_HISTORY'
                    EXPORTING
                         EBELN = EKPO-EBELN
                         EBELP = EKPO-EBELP
                         WEBRE = EKPO-WEBRE
                    TABLES
                         XEKBE = BET
                         XEKBZ = BZT
                         XEKBES = BETS
                         XEKBEZ = BETZ
                         XEKBNK = XEKBNK.
               READ TABLE BETS INDEX 1.
               SORT BZT BY EBELP VGABE BEWTP GJAHR BELNR BUZEI.
               SORT BET BY EBELP VGABE BEWTP GJAHR BELNR BUZEI.
            ENDIF.
         ELSE.
            PERFORM EKBE_SAMMELARTIKEL.
         ENDIF.
      ENDIF.
   ELSE.
      CLEAR BETS.
      READ TABLE BETS INDEX 1.
      SORT BZT BY EBELP VGABE BEWTP GJAHR BELNR BUZEI.
      SORT BET BY EBELP VGABE BEWTP GJAHR BELNR BUZEI.
   ENDIF.
ENDIF.

*- Selektionsparameter aus T160S prüfen -------------------------------*
READ TABLE XT160S INDEX 1.
IF SY-SUBRC EQ 0.
   REJECT = 'X'.
ENDIF.
LOOP AT XT160S.
   T160S = XT160S.
   IF REJECT EQ SPACE.
      EXIT.
   ENDIF.
   CLEAR REJECT.
*- Allgemeine Kriterien -----------------------------------------------*
*- Löschkennzeichen ---------------------------------------------------*
*  if ekpo-loekz ne space.          " Prüfung in Zugriff_Ermitteln
*     if t160s-sloek ne space.      " *- Löschkennzeichen
*        exit.                      " ...
*     else.
*        reject = 'X'.
*        check reject eq space.
*     endif.
*  endif.
* Variantenpositionen
   CASE T160S-VARANZ.
     WHEN '1'. "nur Sammelartikel
       IF  EKPO-ATTYP EQ ATTYP-VAR.
           REJECT = 'X'.
           CHECK REJECT EQ SPACE.
       ENDIF.
     WHEN '2'. "nur Varianten
       IF  EKPO-ATTYP EQ ATTYP-SAM.
           REJECT = 'X'.
           CHECK REJECT EQ SPACE.
       ENDIF.
   ENDCASE.
*- Kriterien für Bestellungen und Lieferpläne -------------------------*
   IF EKKO-BSTYP EQ BSTYP-BEST OR
      EKKO-BSTYP EQ BSTYP-LFPL.
      PERFORM SELPA_CHECK_POS_BEST.
      CHECK REJECT NE SPACE.
   ENDIF.
*- Kriterien für Kontrakte und Lieferpläne ----------------------------*
   IF EKKO-BSTYP EQ BSTYP-KONT OR
      EKKO-BSTYP EQ BSTYP-LFPL.
      PERFORM SELPA_CHECK_POS_RAHM.
      CHECK REJECT NE SPACE.
   ENDIF.
*- Kriterien für Anfragen ---------------------------------------------*
   IF EKKO-BSTYP EQ BSTYP-ANFR.
      PERFORM SELPA_CHECK_POS_ANFR.
   ENDIF.

ENDLOOP.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Pruefen Selektion für Bestellungen/Lieferpläne                      *
*----------------------------------------------------------------------*
FORM SELPA_CHECK_POS_BEST.

*- WE-Kennzeichen -----------------------------------------------------*
   IF T160S-SWEPO EQ '+' AND
      EKPO-WEPOS  EQ SPACE.
      REJECT = 'X'.
   ENDIF.
   IF T160S-SWEPO EQ '-' AND
      EKPO-WEPOS  NE SPACE.
      REJECT = 'X'.
   ENDIF.

*- RE-Kennzeichen -----------------------------------------------------*
   IF T160S-SREPO EQ '+' AND
      EKPO-REPOS  EQ SPACE.
      REJECT = 'X'.
   ENDIF.
   IF T160S-SREPO EQ '-' AND
      EKPO-REPOS  NE SPACE.
      REJECT = 'X'.
   ENDIF.

*- gelieferte Menge <-> 0 ---------------------------------------------*
   IF T160S-WE000 NE SPACE.
      PERFORM SELPA_VFELD_WE
              USING VFELD1 T160S-WE00E T160S-WE00S SPACE.
      VFELD2 = 0.
      PERFORM SELPA_VERGLEICH USING T160S-WE000.
   ENDIF.

*- gelieferte Menge <-> bestellte Menge -------------------------------*
   IF T160S-WEBE0 NE SPACE.
      PERFORM SELPA_VFELD_WE
              USING VFELD1 T160S-WEBEE T160S-WEBES T160S-WEBET.
      VFELD2 = EKPO-MENGE.
      PERFORM SELPA_VERGLEICH USING T160S-WEBE0.
   ENDIF.

*- gelieferte Menge <-> berechnete Menge ------------------------------*
   IF T160S-WERE0 NE SPACE.
      PERFORM SELPA_VFELD_WE
              USING VFELD1 T160S-WEREE T160S-WERES T160S-WEREE.
      PERFORM SELPA_VFELD_RE USING VFELD2 T160S-WEREP.
      PERFORM SELPA_VERGLEICH USING T160S-WERE0.
   ENDIF.

*- berechnete Menge <-> 0 ---------------------------------------------*
   IF T160S-RE000 NE SPACE.
      PERFORM SELPA_VFELD_RE
              USING VFELD1 0.
      VFELD2 = 0.
      PERFORM SELPA_VERGLEICH USING T160S-RE000.
   ENDIF.

*- berechnete Menge <-> bestellte Menge -------------------------------*
   IF T160S-REBE0 NE SPACE.
      PERFORM SELPA_VFELD_RE
              USING VFELD1 T160S-REBEP.
      VFELD2 = EKPO-MENGE.
      PERFORM SELPA_VERGLEICH USING T160S-REBE0.
   ENDIF.

*- Anzahlungswert <-> 0 -----------------------------------------------*
   IF T160S-AN000 NE SPACE.
      VFELD1 = BETS-ANZAL.
      VFELD2 = 0.
      PERFORM SELPA_VERGLEICH USING T160S-AN000.
   ENDIF.

*- WE-Sperrbestandsmenge <-> 0 ----------------------------------------*
   IF T160S-WS000 NE SPACE.
      VFELD1 = BETS-WESBS.
      VFELD2 = 0.
      PERFORM SELPA_VERGLEICH USING T160S-WS000.
   ENDIF.

*- WA-Menge -----------------------------------------------------------*
   IF EKPO-PSTYP EQ PSTYP-UMLG.

*- WA-Menge <-> 0 -----------------------------------------------------*
      IF T160S-WA000 NE SPACE.
         VFELD1 = BETS-WAMNG.
         VFELD2 = 0.
         PERFORM SELPA_VERGLEICH USING T160S-WA000.
      ENDIF.

*- WA-Menge <-> bestellte Menge ---------------------------------------*
      IF T160S-WABE0 NE SPACE.
         VFELD1 = BETS-WAMNG.
         VFELD2 = EKPO-MENGE.
         PERFORM SELPA_VERGLEICH USING T160S-WABE0.
      ENDIF.

*- WA-Menge <-> gelieferte Menge --------------------------------------*
      IF T160S-WAWE0 NE SPACE.
         VFELD1 = BETS-WAMNG.
         VFELD2 = BETS-WEMNG.
         PERFORM SELPA_VERGLEICH USING T160S-WAWE0.
      ENDIF.

   ENDIF.

*- Avisierte Menge ----------------------------------------------------*
   IF T160S-AV000 NE SPACE OR
      T160S-AVBE0 NE SPACE OR
      T160S-AVWE0 NE SPACE.
      IF EKPO-BSTAE EQ SPACE.
         REJECT = 'X'.
      ELSE.
         CALL FUNCTION 'ME_CONFIRMATION_MAINTAIN'
              EXPORTING
                   I_BSTAE    = EKPO-BSTAE
                   I_EBELN    = EKPO-EBELN
                   I_EBELP    = EKPO-EBELP
                   I_FUNKT    = 'A'
                   I_WERKS    = EKPO-WERKS
              IMPORTING
                   E_EKESOK   = H-AVIRL
                   E_LAMNG    = H-AVIMG
              TABLES
                   XEKET      = ETT
                   YEKET      = YEKET.
         IF H-AVIRL EQ SPACE.
            REJECT = 'X'.
         ELSE.
*- avisiert <-> 0 -----------------------------------------------------*
            IF T160S-AV000 NE SPACE.
               VFELD1 = H-AVIMG.
               VFELD2 = 0.
               PERFORM SELPA_VERGLEICH USING T160S-AV000.
            ENDIF.

*- avisiert <-> bestellte Menge ---------------------------------------*
            IF T160S-AVBE0 NE SPACE.
               VFELD1 = H-AVIMG.
               VFELD2 = EKPO-MENGE.
               PERFORM SELPA_VERGLEICH USING T160S-AVBE0.
            ENDIF.

*- avisiert <-> gelieferte Menge --------------------------------------*
            IF T160S-AVWE0 NE SPACE.
               VFELD1 = H-AVIMG.
               VFELD2 = BETS-WEMNG.
               PERFORM SELPA_VERGLEICH USING T160S-WAWE0.
            ENDIF.
         ENDIF.
      ENDIF.
   ENDIF.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Pruefen Selektion für Kontrakte/Lieferpläne                         *
*----------------------------------------------------------------------*
FORM SELPA_CHECK_POS_RAHM.

CLEAR H-MENGE.
IF EKKO-BSTYP EQ BSTYP-LFPL.
   H-MENGE = EKPO-MENGE.
ELSE.
   READ TABLE ADOS WITH KEY EKPO-EBELP BINARY SEARCH.
   IF SY-SUBRC EQ 0.
      H-MENGE = ADOS-MENGE.
   ENDIF.
ENDIF.

*- abgerufen/eingeteilt  <-> 0 ----------------------------------------*
IF T160S-BM000 NE SPACE.
   VFELD1 = H-MENGE.
   VFELD2 = 0.
   PERFORM SELPA_VERGLEICH USING T160S-BM000.
ENDIF.

*- Zielmenge <-> abgerufen/eingeteilt ---------------------------------*
IF T160S-ZMBM0 NE SPACE.
   VFELD1 = EKPO-KTMNG.
   F1 = EKPO-KTMNG * T160S-ZMBMT / 1000.
   VFELD2 = H-MENGE + F1.
   PERFORM SELPA_VERGLEICH USING T160S-ZMBM0.
ENDIF.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Pruefen Selektion für Anfragen                                      *
*----------------------------------------------------------------------*
FORM SELPA_CHECK_POS_ANFR.

*- Angebot vorhanden --------------------------------------------------*
CASE T160S-ANGEB.
   WHEN '+'.
      IF EKPO-STATU NE 'A'.
         REJECT = 'X'.
      ENDIF.
   WHEN '-'.
      IF EKPO-STATU EQ 'A'.
         REJECT = 'X'.
      ENDIF.
ENDCASE.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Vergleichsoperation ausfuehren                                      *
*----------------------------------------------------------------------*
FORM SELPA_VERGLEICH USING SVG_OPER.

CASE SVG_OPER.
   WHEN '< '.
      IF VFELD1 < VFELD2.
      ELSE.
         REJECT = 'X'.
      ENDIF.
   WHEN '<='.
      IF VFELD1 <= VFELD2.
      ELSE.
         REJECT = 'X'.
      ENDIF.
   WHEN '> '.
      IF VFELD1 > VFELD2.
      ELSE.
         REJECT = 'X'.
      ENDIF.
   WHEN '>='.
      IF VFELD1 >= VFELD2.
      ELSE.
         REJECT = 'X'.
      ENDIF.
   WHEN '<>'.
      IF VFELD1 <> VFELD2.
      ELSE.
         REJECT = 'X'.
      ENDIF.
   WHEN '= '.
      IF VFELD1 = VFELD2.
      ELSE.
         REJECT = 'X'.
      ENDIF.
ENDCASE.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Vergleichsfeld gelieferte Menge berechnen                           *
*----------------------------------------------------------------------*
FORM SELPA_VFELD_WE USING SVW_VFELD SVW_ELIKZ SVW_WESPB SVW_UNTTO.

*- WE = - -> WE-Menge = 0 ---------------------------------------------*
IF EKPO-WEPOS EQ SPACE.
   SVW_VFELD = 0.
   EXIT.
ENDIF.

*- WE-Menge setzen ----------------------------------------------------*
SVW_VFELD = BETS-WEMNG.

*- Endlieferungskennzeichen berücksichtigen ---------------------------*
IF SVW_ELIKZ NE SPACE AND EKPO-ELIKZ NE SPACE.
   IF BETS-WEMNG < EKPO-MENGE.
      SVW_VFELD = EKPO-MENGE.
   ENDIF.
   EXIT.
ENDIF.

*- WE-Sperrbestand berücksichtigen ------------------------------------*
IF SVW_WESPB NE SPACE.
   SVW_VFELD = SVW_VFELD + BETS-WESBS.
ENDIF.

*- Unterlieferungstoleranz berücksichtigen ----------------------------*
IF SVW_UNTTO NE SPACE.
   F1 = EKPO-MENGE * EKPO-UNTTO / 1000.
   SVW_VFELD = SVW_VFELD + F1.
ENDIF.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Vergleichsfeld berechnete Menge berechnen                           *
*----------------------------------------------------------------------*
FORM SELPA_VFELD_RE USING SVR_VFELD SVR_UNTTO.

*- RE = - -> RE-Menge = 0 ---------------------------------------------*
IF EKPO-REPOS EQ SPACE.
   SVR_VFELD = 0.
   EXIT.
ENDIF.

*- RE-Menge setzen ----------------------------------------------------*
SVR_VFELD = BETS-REMNG.

*- Rechnungstoleranz berücksichtigen ----------------------------------*
IF SVR_UNTTO NE SPACE.
   F1 = BETS-REMNG * SVR_UNTTO / 1000.
   SVR_VFELD = SVR_VFELD + F1.
ENDIF.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*  Zugriffsreihenfolge ermitteln                                       *
*----------------------------------------------------------------------*
FORM ZUGRIFF_ERMITTELN.

CLEAR: EKABKS,
      EKABKA,
      EKABPS,
      EKABPA,
      EKETPS,
      EKETPA,
      EKBEPS,
      EKBEPA,
      LFA1KS,
      LFA1KA,
      SELK,
      SELP.

*- Zusatzdaten wegen Selectoptions / Parameter ------------------------*
LFA1KA = 'X'.
IF P_NAME1 NE SPACE.
   LFA1KS = 'X'.
ENDIF.

IF P_RWEIT NE 0.
   EKETPS = 'X'.
   EKABPS = 'X'.
ENDIF.

READ TABLE S_EINDT INDEX 1.
IF SY-SUBRC EQ 0.
   EKETPS = 'X'.
ENDIF.

*- Ermitteln Prüfungen aufgrund T160S ---------------------------------*
REFRESH: R_WEPOS, R_REPOS, R_LOEKZ, R_MATNR.
*- Materialnummer für DB-Selektion übernehmen -------------------------*
LOOP AT S_MATNR.
   MOVE S_MATNR TO R_MATNR.
   APPEND R_MATNR.
ENDLOOP.

R_WEPOS-SIGN = 'I'.
R_REPOS-SIGN = 'I'.
R_LOEKZ-SIGN = 'I'.
LOOP AT XT160S.
   T160S = XT160S.
*- Prüfungen mit EKBE-Daten -------------------------------------------*
   IF T160S-WE000 NE SPACE OR
      T160S-WEBE0 NE SPACE OR
      T160S-WERE0 NE SPACE OR
      T160S-RE000 NE SPACE OR
      T160S-AN000 NE SPACE OR
      T160S-WS000 NE SPACE OR
      T160S-WAWE0 NE SPACE OR
      T160S-WABE0 NE SPACE OR
      T160S-AVWE0 NE SPACE.
      EKBEPS = 'X'.
   ENDIF.

*- Prüfungen mit EKAB-Daten -------------------------------------------*
   IF T160S-ZWBW0 NE SPACE.
      EKABKS = 'X'.
   ENDIF.
   IF T160S-ZMBM0 NE SPACE OR
      T160S-BM000 NE SPACE.
      EKABPS = 'X'.
   ENDIF.

*- Prüfungen mit Kopfdaten --------------------------------------------*
   IF T160S-LFZAB NE SPACE.
      SELK = 'X'.
   ENDIF.

*- Prüfungen mit Positionsdaten ---------------------------------------*
   IF T160S-ZMBM0 NE SPACE OR
      T160S-BM000 NE SPACE OR
      T160S-ANGEB NE SPACE OR
      T160S-AV000 NE SPACE OR
      T160S-AVBE0 NE SPACE OR
      T160S-AVWE0 NE SPACE.
      SELP = 'X'.
   ENDIF.

*- WE-Kennzeichen ----------------------------------------------------*
   IF R_WEPOS-SIGN NE SPACE.
     CASE T160S-SWEPO.
       WHEN '+'.
         CASE R_WEPOS-OPTION.
           WHEN 'EQ'.  CLEAR R_WEPOS.
           WHEN SPACE. R_WEPOS-OPTION = 'NE'.
         ENDCASE.
       WHEN '-'.
         CASE R_WEPOS-OPTION.
           WHEN 'NE'.  CLEAR R_WEPOS.
           WHEN SPACE. R_WEPOS-OPTION = 'EQ'.
         ENDCASE.
       WHEN SPACE.
         CLEAR R_WEPOS.
     ENDCASE.
   ENDIF.

*- RE-Kennzeichen ----------------------------------------------------*
   IF R_REPOS-SIGN NE SPACE.
     CASE T160S-SREPO.
       WHEN '+'.
         CASE R_REPOS-OPTION.
           WHEN 'EQ'.  CLEAR R_REPOS.
           WHEN SPACE. R_REPOS-OPTION = 'NE'.
         ENDCASE.
       WHEN '-'.
         CASE R_REPOS-OPTION.
           WHEN 'NE'.  CLEAR R_REPOS.
           WHEN SPACE. R_REPOS-OPTION = 'EQ'.
         ENDCASE.
       WHEN SPACE.
         CLEAR R_REPOS.
     ENDCASE.
   ENDIF.

*- Löschkennzeichen --------------------------------------------------*
   IF R_LOEKZ-SIGN NE SPACE.
     IF T160S-SLOEK EQ SPACE.
       R_LOEKZ-OPTION = 'EQ'.
     ELSE.
       CLEAR R_LOEKZ.
     ENDIF.
   ENDIF.
*- Materialnummer ----------------------------------------------------*
   IF T160S-VARANZ NE '1' AND
      T160S-VARANZ NE SPACE.
     REFRESH R_MATNR.
   ENDIF.

ENDLOOP.
IF R_WEPOS-OPTION NE SPACE AND
   EKDY-SELKK EQ SPACE AND
   EKDY-SELKA EQ SPACE.
  APPEND R_WEPOS.
  SELP = 'X'.
ENDIF.
IF R_REPOS-OPTION NE SPACE AND
   EKDY-SELKK EQ SPACE AND
   EKDY-SELKA EQ SPACE.
  APPEND R_REPOS.
  SELP = 'X'.
ENDIF.
IF R_LOEKZ-OPTION NE SPACE.
  APPEND R_LOEKZ.
  SELP = 'X'.
ENDIF.

*- Ermitteln Zusatzdaten aufgrund T160L--------------------------------*
*- EKBE für Grundliste                      ---------------------------*
IF T160L-SOWRS NE SPACE OR
   T160L-SOWRO NE SPACE OR
   T160L-SOUML NE SPACE OR
   T160L-INEKB NE SPACE.
   EKBEPA = 'X'.
ENDIF.

*- EKAB für Grundliste ------------------------------------------------*
IF T160L-SOZWS NE SPACE OR
   T160L-SOZWO NE SPACE.
   EKABKA = 'X'.
ENDIF.
IF T160L-SOKTO NE SPACE OR
   T160L-SORSU NE SPACE OR
   T160L-INEKA NE SPACE OR
   T160L-SORWE NE SPACE.
   EKABPA = 'X'.
ENDIF.

*- EKET für Grundliste ------------------------------------------------*
IF T160L-INEKE NE SPACE.
   EKETPA = 'X'.
ENDIF.

*- Ermitteln Prüfungen aufgrund Selectoptions/Parameter----------------*
PERFORM SELK_ERMITTELN.
PERFORM SELP_ERMITTELN.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*  Selektivität bezüglich Positionsdaten ermitteln                     *
*----------------------------------------------------------------------*
FORM SELP_ERMITTELN.

CHECK SELP EQ SPACE.
READ TABLE R_MATNR INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE S_WERKS INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE R_PSTYP INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE S_MATKL INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE S_EAN11 INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE S_IDNLF INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE S_LTSNR INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE S_AKTNR INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE S_SAISO INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE S_SAISJ INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
IF P_TXZ01 NE SPACE.
  SELP = 'X'.
ENDIF.
CHECK SELP EQ SPACE.
READ TABLE S_KNTTP INDEX 1.
IF SY-SUBRC EQ 0.
  SELP = 'X'.
ENDIF.
ENDFORM.
*eject
*----------------------------------------------------------------------*
*  Selektivität bezüglich Kopfdaten ermitteln                          *
*----------------------------------------------------------------------*
FORM SELK_ERMITTELN.

CHECK SELK EQ SPACE.
READ TABLE S_BSART INDEX 1.
IF SY-SUBRC EQ 0.
  SELK = 'X'.
ENDIF.
CHECK SELK EQ SPACE.
READ TABLE S_EKGRP INDEX 1.
IF SY-SUBRC EQ 0.
  SELK = 'X'.
ENDIF.
CHECK SELK EQ SPACE.
READ TABLE S_EBELN INDEX 1.
IF SY-SUBRC EQ 0.
  SELK = 'X'.
ENDIF.
CHECK SELK EQ SPACE.
READ TABLE S_LIFNR INDEX 1.
IF SY-SUBRC EQ 0.
  SELK = 'X'.
ENDIF.
CHECK SELK EQ SPACE.
READ TABLE S_RESWK INDEX 1.
IF SY-SUBRC EQ 0.
  SELK = 'X'.
ENDIF.
CHECK SELK EQ SPACE.
READ TABLE S_BEDAT INDEX 1.
IF SY-SUBRC EQ 0.
  SELK = 'X'.
ENDIF.
CHECK SELK EQ SPACE.
READ TABLE S_ANGDT INDEX 1.
IF SY-SUBRC EQ 0.
  SELK = 'X'.
ENDIF.
CHECK SELK EQ SPACE.
IF P_GULDT NE 0.
  SELK = 'X'.
ENDIF.

ENDFORM.

*eject
*----------------------------------------------------------------------*
*  Zugriff auf Zusatzdaten - gesamtbelegorientiert                     *
*----------------------------------------------------------------------*
FORM ZUGRIFF_KOPF.

*- Lieferanten --------------------------------------------------------*
READ TABLE IT_LFA1 INDEX 1.
IF SY-SUBRC EQ 0.
   IF LFA1KA NE SPACE OR
      LFA1KS NE SPACE.
      SELECT * FROM LFA1 APPENDING TABLE LLFA1
                    FOR ALL ENTRIES IN IT_LFA1
                    WHERE LIFNR EQ IT_LFA1-LIFNR.
      SORT LLFA1.
      REFRESH IT_LFA1.
   ENDIF.
ENDIF.

*- Abrufdoku bei Kontrakten -------------------------------------------*
IF GRBSTYP EQ BSTYP-KONT.
   IF EKABKS NE SPACE OR
      EKABKA NE SPACE OR
      ( SELP EQ SPACE AND
        EKABPS NE SPACE ) OR
      ( SELP EQ SPACE AND
        EKABPA NE SPACE ).
      CALL FUNCTION 'ME_PREFETCH_RELDOCU'
           TABLES
                SEL_TAB = IT_EKKO.
   ENDIF.
ENDIF.

CHECK SELP EQ SPACE.
*- Bestellentwicklung bei Bestellungen und Lieferplänen ---------------*
IF GRBSTYP EQ BSTYP-BEST OR
   GRBSTYP EQ BSTYP-LFPL.
   IF EKBEPS NE SPACE OR
      EKBEPA NE SPACE.
      CALL FUNCTION 'ME_PREFETCH_HISTORY'
           TABLES
                SEL_TAB = IT_EKKO.
   ENDIF.
ENDIF.


*- Einteilungen bei Anfragen, Bestellungen und Lieferplänen -----------*
IF GRBSTYP NE BSTYP-KONT.
   IF EKETPS NE SPACE OR
      EKETPA NE SPACE.
      CALL FUNCTION 'ME_PREFETCH_SCHED'
           TABLES
                SEL_TAB = IT_EKKO.
   ENDIF.
ENDIF.

ENDFORM.

*eject.
*---------------------------------------------------------------------*
*       FORM ZUGRIFF_POS                                              *
*---------------------------------------------------------------------*
FORM ZUGRIFF_POS.

CHECK SELP NE SPACE.
READ TABLE IT_EKPO INDEX 1.
CHECK SY-SUBRC EQ 0.

*- Bestellentwicklung bei Bestellungen und Lieferplänen ---------------*
IF GRBSTYP EQ BSTYP-BEST OR
   GRBSTYP EQ BSTYP-LFPL.
   IF EKBEPS NE SPACE OR
      EKBEPA NE SPACE.
      CALL FUNCTION 'ME_PREFETCH_HISTORY'
           TABLES
              SEL_TAB = IT_EKPO.
   ENDIF.
ENDIF.

*- Abrufdoku bei Kontrakten -------------------------------------------*
IF GRBSTYP EQ BSTYP-KONT.
   IF ( EKABKS EQ SPACE AND
        EKABKA EQ SPACE ) AND
      ( EKABPS NE SPACE OR
        EKABPA NE SPACE ).
      CALL FUNCTION 'ME_PREFETCH_RELDOCU'
           TABLES
                SEL_TAB = IT_EKPO.
   ENDIF.
ENDIF.

*- Einteilungen bei Anfragen, Bestellungen und Lieferplänen -----------*
IF GRBSTYP NE BSTYP-KONT.
   IF EKETPS NE SPACE OR
      EKETPA NE SPACE.
      CALL FUNCTION 'ME_PREFETCH_SCHED'
           TABLES
                SEL_TAB = IT_EKPO.
   ENDIF.
ENDIF.

ENDFORM.

*eject
*----------------------------------------------------------------------*
*  Kopfdaten in interner Tabelle merken                                *
*----------------------------------------------------------------------*
FORM EKKO_MERKEN.

PERFORM BERECHTIGUNGEN_KOPF(SAPFM06D).
IF SY-SUBRC NE 0.
  XAUTH = 'X'.
  CHECK SY-SUBRC EQ 0.             "nächster EKKO
ENDIF.

*- gruppenwechsel belegtyp --------------------------------------------*
IF ( GRBSTYP NE SPACE ) AND ( EKKO-BSTYP NE GRBSTYP ).

  SAVEEKKO = EKKO.
  PERFORM ZUGRIFF_KOPF.
  PERFORM POS_NACH_KOPF.
  REFRESH IT_EKKO.
  EKKO = SAVEEKKO.
ENDIF.

GRBSTYP = EKKO-BSTYP.
LEKKO = EKKO.
LEKKO-Z_NETWR = 0.                                    "JLEE 11/18/1996
SELECT * FROM EKPO WHERE EBELN = EKKO-EBELN.          "JLEE 11/18/1996
   LEKKO-Z_NETWR = LEKKO-Z_NETWR + EKPO-NETWR.        "JLEE 11/18/1996
ENDSELECT.                                            "JLEE 11/18/1996
APPEND LEKKO.
IT_EKKO-EBELN = EKKO-EBELN.
IT_EKKO-Z_NETWR = LEKKO-Z_NETWR.                      "JLEE 11/18/1996
APPEND IT_EKKO.
IF EKKO-LIFNR <> SPACE.
  MOVE-CORRESPONDING EKKO TO LFA1KEY.
  READ TABLE LLFA1 WITH KEY LFA1KEY BINARY SEARCH.
  IF SY-SUBRC NE 0.
     READ TABLE IT_LFA1 WITH KEY EKKO-LIFNR BINARY SEARCH.
     IF SY-SUBRC NE 0.
        IT_LFA1-LIFNR = EKKO-LIFNR.
        INSERT IT_LFA1 INDEX SY-TABIX.
     ENDIF.
  ENDIF.
ENDIF.

ENDFORM.

*eject
*----------------------------------------------------------------------*
*  Positionsdaetne in interner Tabelle merken                          *
*----------------------------------------------------------------------*
FORM EKPO_MERKEN.

PERFORM BERECHTIGUNGEN_POS(SAPFM06D).
IF SY-SUBRC NE 0.
  XAUTH = 'X'.
  CHECK SY-SUBRC EQ 0.             "nächster EKPO
ENDIF.

*- Gruppenwechsel belegtyp -------------------------------------------*
IF ( GRBSTYP NE SPACE ) AND ( EKPO-BSTYP NE GRBSTYP ).

  SAVEEKPO = EKPO.
  PERFORM KOPF_NACH_POS.
  EKPO = SAVEEKPO.
ENDIF.

GRBSTYP = EKPO-BSTYP.
LEKPO = EKPO.
APPEND LEKPO.
READ TABLE IT_EKKO WITH KEY EKPO-EBELN BINARY SEARCH.
IF SY-SUBRC NE 0.
   IT_EKKO-EBELN = EKPO-EBELN.
   INSERT IT_EKKO INDEX SY-TABIX.
ENDIF.
IT_EKPO-EBELN = EKPO-EBELN.
IT_EKPO-EBELP = EKPO-EBELP.
APPEND IT_EKPO.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*  Positionsdaten lesen                                                *
*----------------------------------------------------------------------*
FORM POS_NACH_KOPF.

READ TABLE IT_EKKO INDEX 1.
CHECK SY-SUBRC EQ 0.
*- Positionen lesen ---------------------------------------------------*
  CLEAR IT_EKPO.

  SELECT * FROM EKPO
    FOR ALL ENTRIES IN IT_EKKO
    WHERE EBELN = IT_EKKO-EBELN
      AND MATNR IN R_MATNR
      AND WERKS IN S_WERKS
      AND BPRME IN P_BPRME            "JLEE 11/18/1996
      AND WEPOS IN R_WEPOS
      AND REPOS IN R_REPOS
      AND LOEKZ IN R_LOEKZ
      AND PSTYP IN R_PSTYP
      AND KNTTP IN S_KNTTP
      AND MATKL IN S_MATKL
      AND EAN11 IN S_EAN11
      AND IDNLF IN S_IDNLF
      AND LTSNR IN S_LTSNR
      AND AKTNR IN S_AKTNR
      AND SAISO IN S_SAISO
      AND SAISJ IN S_SAISJ.

    PERFORM BERECHTIGUNGEN_POS(SAPFM06D).
    IF SY-SUBRC NE 0.
      XAUTH = 'X'.
      CHECK SY-SUBRC EQ 0.             "nächster EKPO
    ENDIF.

    IT_EKPO-EBELN = EKPO-EBELN.
    IT_EKPO-EBELP = EKPO-EBELP.
    APPEND IT_EKPO.
    LEKPO = EKPO.
    APPEND LEKPO.
  ENDSELECT.

*- Zugriff für letzte Belegnummer -------------------------------------*
  PERFORM ZUGRIFF_POS.
  REFRESH IT_EKPO.

*- Daten ausgeben -----------------------------------------------------*
  "sort the output in ascend / descend order or in doc no. order
  IF P_ASCEND = 'X'.                    "JLEE 11/18/1996
     SORT LEKKO BY Z_NETWR ASCENDING.   "JLEE 11/18/1996
  ELSEIF P_DESCED = 'X'.                "JLEE 11/18/1996
     SORT LEKKO BY Z_NETWR DESCENDING.  "JLEE 11/18/1996
  ENDIF.                                "JLEE 11/18/1996

  LOOP AT LEKKO.
    EKKO = LEKKO.
    PERFORM SELPA_CHECK_KOPF.
    CHECK REJECT EQ SPACE.
    LOOP AT LEKPO WHERE EBELN = EKKO-EBELN.
      EKPO = LEKPO.
      PERFORM SELPA_CHECK_POS.
      CHECK REJECT EQ SPACE.
      ON CHANGE OF EKPO-EBELN.
        PERFORM EKKO_AUSGABE.
      ENDON.
      PERFORM EKPO_AUSGABE.
    ENDLOOP.
  ENDLOOP.
  REFRESH LEKKO.
  REFRESH LEKPO.
ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Kopfdaten zu selektierten Positionen lesen                          *
*----------------------------------------------------------------------*
FORM KOPF_NACH_POS.

DATA: RCODE.

PERFORM KOPF_NACH_POS_LESEN.

*- Daten ausgeben -----------------------------------------------------*
LOOP AT LEKPO.

   PERFORM KOPF_NACH_POS_PRUEFEN CHANGING RCODE.
   IF RCODE = 'O'.  "OK
      PERFORM KOPF_NACH_POS_AUSGEBEN.
   ENDIF.

ENDLOOP.
REFRESH LEKKO.
REFRESH LEKPO.

ENDFORM.

*----------------------------------------------------------------------*
*  Kopfdaten zu den Positionen lesen                                   *
*----------------------------------------------------------------------*
FORM KOPF_NACH_POS_LESEN.

DATA: BEGIN OF KKEY,
         MANDT LIKE EKKO-MANDT,
         EBELN LIKE EKKO-EBELN,
      END OF KKEY.

READ TABLE IT_EKKO INDEX 1.
CHECK SY-SUBRC EQ 0.

*- Köpfe zu Positionen lesen ------------------------------------------*
SELECT * FROM EKKO
   FOR ALL ENTRIES IN IT_EKKO
   WHERE EBELN = IT_EKKO-EBELN
     AND LIFNR IN S_LIFNR
     AND EKORG IN R_EKORG
     AND BSART IN S_BSART
     AND EKGRP IN S_EKGRP
     AND RESWK IN S_RESWK
     AND BEDAT IN S_BEDAT.

   PERFORM BERECHTIGUNGEN_KOPF(SAPFM06D).
   IF SY-SUBRC NE 0.
      SELK = 'X'.
      XAUTH = 'X'.
      CHECK SY-SUBRC EQ 0.
   ENDIF.
   LEKKO = EKKO.
   APPEND LEKKO.
   MOVE-CORRESPONDING EKKO TO LFA1KEY.
   READ TABLE LLFA1 WITH KEY LFA1KEY BINARY SEARCH.
   IF SY-SUBRC NE 0.
      READ TABLE IT_LFA1 WITH KEY EKKO-LIFNR BINARY SEARCH.
      IF SY-SUBRC NE 0.
         IT_LFA1-LIFNR = EKKO-LIFNR.
         INSERT IT_LFA1 INDEX SY-TABIX.
      ENDIF.
   ENDIF.
ENDSELECT.
SORT LEKKO.

*- Interne Tabellen um nicht selektierte Köpfe bereinigen -------------*
IF SELK NE SPACE.
   LOOP AT IT_EKPO.
      KKEY-MANDT = SY-MANDT.
      KKEY-EBELN = IT_EKPO-EBELN.
      READ TABLE LEKKO WITH KEY KKEY BINARY SEARCH.
      IF SY-SUBRC NE 0.
         DELETE IT_EKPO.
         READ TABLE IT_EKKO WITH KEY KKEY-EBELN BINARY SEARCH.
         IF SY-SUBRC EQ 0.
            DELETE IT_EKKO INDEX SY-TABIX.
         ENDIF.
      ENDIF.
   ENDLOOP.
ENDIF.

PERFORM ZUGRIFF_KOPF.
PERFORM ZUGRIFF_POS.
REFRESH IT_EKKO.
REFRESH IT_EKPO.

ENDFORM.

*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
FORM KOPF_NACH_POS_PRUEFEN CHANGING RCODE.

DATA: BEGIN OF KKEY,
         MANDT LIKE EKKO-MANDT,
         EBELN LIKE EKKO-EBELN,
      END OF KKEY.

RCODE = 'E'.

EKPO = LEKPO.
MOVE-CORRESPONDING EKPO TO KKEY.
READ TABLE LEKKO WITH KEY KKEY BINARY SEARCH.
CHECK SY-SUBRC EQ 0.
EKKO = LEKKO.
PERFORM SELPA_CHECK_KOPF.
CHECK REJECT EQ SPACE.
PERFORM SELPA_CHECK_POS.
CHECK REJECT EQ SPACE.

RCODE = 'O'.

ENDFORM.

*----------------------------------------------------------------------*
*  Daten auf dem Bildschirm ausgeben                                   *
*----------------------------------------------------------------------*
FORM KOPF_NACH_POS_AUSGEBEN.

ON CHANGE OF EKPO-EBELN.
   PERFORM EKKO_AUSGABE.
ENDON.
PERFORM EKPO_AUSGABE.

ENDFORM.

*eject.
*----------------------------------------------------------------------*
*  Meldungen nach Ende der Selektion ausgeben                          *
*----------------------------------------------------------------------*
FORM END_OF_SELECTION.

IF XAUTH NE SPACE.
  PERFORM ENACO(SAPFM06D) USING 'ME' '235'.
  IF SY-SUBRC NE 0.
    MESSAGE S235(ME).
  ENDIF.
ENDIF.
IF NOT_FOUND NE SPACE.
  MESSAGE S260.
  IF SY-CALLD NE SPACE.
    LEAVE.
  ELSE.
    LEAVE TO TRANSACTION SY-TCODE.
  ENDIF.
ENDIF.

ENDFORM.
