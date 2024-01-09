* 2000/07/21 mdemeest 4.6B All changes identified with "UGL"
*                          Copied FM06PF01_POS_ZUSATZDATUN to
*                          ZNMPU002_FM06PF01_POS_ZUSATZDA and added
*                          Info Record info
*----------------------------------------------------------------------
*eject
*
* 109695 40B 13.07.1998 PH: HTN-Bestelltext drucken
* 163613 40B 21.07.1999 SH :HTN: MARA-MFRPN wird in MEDRUCK falsch
*eject
*----------------------------------------------------------------------*
* Positionszusatzdaten lesen
*----------------------------------------------------------------------*
FORM POS_ZUSATZDATEN.

  DATA: TEXT_FLAG(1) TYPE P.

* Lesen Versandvorschrift --------------------------------------------*
  CLEAR T027B.
  SELECT SINGLE * FROM T027A WHERE EVERS EQ EKPO-EVERS.
  IF SY-SUBRC EQ 0 AND
     T027A-EVDRU NE SPACE.
    SELECT SINGLE * FROM T027B WHERE SPRAS EQ EKKO-SPRAS
                               AND   EVERS EQ EKPO-EVERS.
  ENDIF.

* Lesen Langtext Mengeneinheit ----------------------------------------*
  CLEAR T006A.
  SELECT SINGLE * FROM T006A WHERE SPRAS EQ EKKO-SPRAS
                             AND   MSEHI EQ EKPO-MEINS.
  MOVE T006A TO *T006A.
  IF EKPO-BPRME NE EKPO-MEINS.
    SELECT SINGLE * FROM T006A INTO *T006A WHERE SPRAS EQ EKKO-SPRAS
                                           AND   MSEHI EQ EKPO-BPRME.
  ENDIF.

* Ermitteln Nachkommastellen ------------------------------------------*
  CLEAR T006.
  SELECT SINGLE * FROM T006 WHERE MSEHI EQ EKPO-MEINS.
  MOVE T006 TO *T006.
  IF EKPO-BPRME NE EKPO-MEINS.
    SELECT SINGLE * FROM T006 INTO *T006 WHERE MSEHI EQ EKPO-BPRME.
  ENDIF.

* Aufbereiten Preiseinheit --------------------------------------------*
  CLEAR RM06P-PRPEI.
  IF EKPO-PEINH NE 1.
    RM06P-PRPEI(1) = '/'.
    WRITE EKPO-PEINH NO-SIGN TO RM06P-PRPEI+1(6).
    DO.
      IF RM06P-PRPEI+1(1) NE SPACE.
        RM06P-PRPEI(1) = '/'.
        EXIT.
      ENDIF.
      SHIFT RM06P-PRPEI LEFT.
    ENDDO.
  ENDIF.

* Lesen Werksanschrift pro Position -----------------------------------*
  IF PEKKO-WERKS EQ SPACE.
    IF EKPO-ADRNR EQ SPACE AND
       EKPO-EMLIF EQ SPACE AND
       EKPO-ADRN2 EQ SPACE AND
       EKPO-KUNNR EQ SPACE.
*     PERFORM WERKSANSCHRIFT USING EKPO-WERKS.                  "126150
      PERFORM GET_PLANT_ADDRESS                             "126150
                  USING                                     "126150
                     EKPO-WERKS                             "126150
                  CHANGING                                  "126150
                     EKPO-ADRNR                             "126150
                     SADR.                                  "126150
    ENDIF.
  ENDIF.

* Lesen Kundenanschrift pro Position ----------------------------------*
  IF PEKKO-KUNNR EQ SPACE.
*   PERFORM KUNDEN_ANSCHRIFT USING EKPO-KUNNR.                  "126150
    PERFORM GET_CUSTOMER_ADDRESS                            "126150
                USING                                       "126150
                   EKPO-KUNNR                               "126150
                CHANGING                                    "126150
                   EKPO-ADRNR.                              "126150
  ENDIF.

* Lesen Anlieferungsanschrift pro Position ----------------------------*
  IF PEKKO-ADRNR EQ SPACE.
*   PERFORM ANLIEF_ANSCHRIFT USING EKPO-ADRNR.                  "126150
  ENDIF.

* Lesen Adressnummer pro Position -------------------------------------*
  IF PEKKO-ADRN2 EQ SPACE.
    IF NOT EKPO-ADRN2 IS INITIAL.                           "126150
      EKPO-ADRNR = EKPO-ADRN2.                              "126150
    ENDIF.                                                  "126150
*   PERFORM LESEN_ADRESSNUMMER USING EKPO-ADRN2.                "126150
  ENDIF.

* Lesen Lieferantenanschrift pro Position -----------------------------*
  IF PEKKO-EMLIF EQ SPACE.
*   PERFORM LESEN_LIEFERANT3 USING EKPO-EMLIF.                  "126150
    PERFORM GET_VENDOR_ADDRESS                              "126150
                USING                                       "126150
                   EKPO-EMLIF                               "126150
                CHANGING                                    "126150
                   EKPO-ADRNR.                              "126150
  ENDIF.

* Auftragsbestätigung einheitlich -------------------------------------*
  IF PEKKO-LABNR NE SPACE.
    CLEAR EKPO-LABNR.
  ENDIF.

  IF EKPO-UPTYP NE SPACE.
    PERFORM TMSI2_LESEN USING EKPO-SIKGR.
  ENDIF.

  IF EKPO-UPTYP EQ SPACE OR TMSI2-SITXD NE SPACE.
* Lesen Positionstextheader--------------------------------------------*
    THEAD-TDOBJECT = 'EKPO'.
    THEAD-TDSPRAS = EKKO-SPRAS.
    THEAD-TDNAME = EKKO-EBELN.
    THEAD-TDNAME+10(5) = EKPO-EBELP.
    THEAD-TDID = '*'.
    MOVE-CORRESPONDING THEAD TO XTHEADKEY.

    CALL FUNCTION 'SELECT_TEXT'
         EXPORTING
              ID         = THEAD-TDID
              LANGUAGE   = THEAD-TDSPRAS
              NAME       = THEAD-TDNAME
              OBJECT     = THEAD-TDOBJECT
         IMPORTING
              ENTRIES    = ENTRIES
         TABLES
              SELECTIONS = XTHEAD.
    SORT XTHEAD BY TDID.

* Lesen Positionstexte ------------------------------------------------*
    REFRESH XT166P.
    CLEAR XT166P.
    SELECT * FROM T166P WHERE DRUVO = XDRUVO
                        AND   BSTYP = EKKO-BSTYP
                        AND   BSART = EKKO-BSART
                        AND   PSTYP = EKPO-PSTYP.
      MOVE T166P TO XT166P.
      IF T166P-TDOBJECT = THEAD-TDOBJECT.
        XTHEADKEY-TDID = T166P-TDID.
        READ TABLE XTHEAD WITH KEY XTHEADKEY BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*       im Änderungsdruck prüfen, ob Kopftextänderungen vorhanden sind
*         IF XDRUVO EQ AEND OR XDRUVO EQ LPAE.
*           LOOP AT XAETX WHERE EBELP EQ EKPO-EBELP.
*             CLEAR EKKEY.
*             EKKEY-EBELP = EKPO-EBELP.
*             T166C-CTXNR = 'S4'.
*             PERFORM FUELLEN_XAEND.
*           ENDLOOP.
*           IF SY-SUBRC = 0.
*             APPEND XT166P.
*           ENDIF.
*         ELSE.
          APPEND XT166P.
*         ENDIF.
        ENDIF.
      ELSE.
        CASE T166P-TDOBJECT.   "Unnötige Zugriffe auf STXH sparen
          WHEN 'MATERIAL'.
            IF EKPO-MATNR NE SPACE.
              APPEND XT166P.
            ENDIF.
          WHEN 'EINA'.
            IF EKPO-INFNR NE SPACE.
              APPEND XT166P.
            ENDIF.
          WHEN 'EINE'.
            IF EKPO-INFNR NE SPACE.
              APPEND XT166P.
            ENDIF.
          WHEN 'VBBP'.
            LOOP AT XEKET WHERE EBELN EQ EKPO-EBELN
                          AND   EBELP EQ EKPO-EBELP
                          AND   BANFN NE SPACE.
              EXIT.
            ENDLOOP.
            IF SY-SUBRC EQ 0 AND       "nur bei Positionen mit Banfbezug
               EKPO-KNTTP NE SPACE.
              SELECT SINGLE * FROM EKKN WHERE EBELN EQ EKPO-EBELN
                                        AND   EBELP EQ EKPO-EBELP
                                        AND   ZEKKN EQ 01.
              IF SY-SUBRC EQ 0 AND EKKN-VBELN NE SPACE.
                APPEND XT166P.
              ENDIF.
            ENDIF.
          WHEN OTHERS.
            APPEND XT166P.
        ENDCASE.
      ENDIF.
    ENDSELECT.
    SORT XT166P BY DRFLG DRPRI.

* When no text changes have been made, remove item texts and all texts
* with lower priority
    IF XDRUVO EQ AEND OR XDRUVO EQ LPAE.
      LOOP AT XAETX WHERE EBELP EQ EKPO-EBELP.
        CLEAR EKKEY.
        EKKEY-EBELP = EKPO-EBELP.
        T166C-CTXNR = 'S4'.
        PERFORM FUELLEN_XAEND.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        DO.
          READ TABLE XT166P WITH KEY DRUVO    = XDRUVO
                                     BSTYP    = EKKO-BSTYP
                                     BSART    = EKKO-BSART
                                     PSTYP    = EKPO-PSTYP
                                     TDOBJECT = 'EKPO'.
          IF SY-SUBRC EQ 0.
            DELETE XT166P WHERE DRUVO  = XDRUVO
                          AND   BSTYP  = EKKO-BSTYP
                          AND   BSART  = EKKO-BSART
                          AND   PSTYP  = EKPO-PSTYP
                          AND   DRFLG  = XT166P-DRFLG
                          AND   DRPRI GE XT166P-DRPRI.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.

    CLEAR XDRFLG.
* XT166P ergänzen/bereinigen ------------------------------------------*
    LOOP AT XT166P.
* Kein weiterer Text mit gleichem Reihenfolge-Kennzeichen -------------*
* außer bei Infobestelltext, da er evtl. den Materialbestelltext ------*
* noch verdrängen kann                                           ------*
      IF XT166P-DRFLG EQ XDRFLG AND
         ( XT166P-TDOBJECT NE 'EINE' OR XT166P-TDID NE 'BT' ).
        DELETE XT166P.
      ELSE.
        IF XT166P-TDOBJECT EQ 'EKPO' OR
           XT166P-TDOBJECT EQ 'TEXT'.
          XDRFLG = XT166P-DRFLG.
        ELSE.
* Lesen Textheader zu MAterial/Infosatz-Texten-------------------------*
          THEAD-TDOBJECT = XT166P-TDOBJECT.
          THEAD-TDSPRAS = EKKO-SPRAS.
          THEAD-TDID = XT166P-TDID.
          CASE XT166P-TDOBJECT.
            WHEN 'MATERIAL'.
*
              IF NOT EKPO-MPROF IS INITIAL.                 "109695
                CALL FUNCTION 'MB_READ_TMPPF'               "109695
                     EXPORTING                              "109695
                          PROFILE        = EKPO-MPROF       "109695
                     IMPORTING                              "109695
                          MPN_PARAMETERS = TMPPF.           "109695
              ENDIF.                                        "109695
              IF TMPPF-MPBTX IS INITIAL.                    "109695
*... Text aus best. gef. Material ....................................*
                THEAD-TDNAME = EKPO-MATNR.
              ELSE.                                         "109695
*... Text aus HTN ....................................................*
                THEAD-TDNAME = EKPO-EMATN.                  "109695
              ENDIF.                                        "109695
*
            WHEN 'EINE'.
              DATA: H_MEICO LIKE MEICO,"82403 Anfang
                    H_EINE  LIKE EINE.
              H_MEICO-INFNR = EKPO-INFNR.
              H_MEICO-EKORG = EKKO-EKORG.
              IF EKPO-PSTYP EQ PSTYP-LOHN.
                H_MEICO-ESOKZ = ESOKZ-LOHN.
              elseif ekpo-pstyp eq pstyp-kons.            "145834
                h_meico-esokz = esokz-konsi.              "145834
              ELSE.
                H_MEICO-ESOKZ = ESOKZ-NORM.
              ENDIF.
              H_MEICO-WERKS = EKPO-WERKS.
              CALL FUNCTION 'ME_READ_INFORECORD'
                   EXPORTING
                        INCOM     = H_MEICO
                   IMPORTING
                        EINEDATEN = H_EINE
                   EXCEPTIONS
                        OTHERS    = 1.
              THEAD-TDNAME       = EKPO-INFNR.
              THEAD-TDNAME+10(4) = EKKO-EKORG.
              THEAD-TDNAME+14(1) = H_MEICO-ESOKZ.
              THEAD-TDNAME+15(4) = H_EINE-WERKS.          "82403 Ende
            WHEN 'VBBP'.
              THEAD-TDNAME       = EKKN-VBELN.
              THEAD-TDNAME+10(6) = EKKN-VBELP.
            WHEN 'ESLL'.
              THEAD-TDNAME       = EKKN-VBELN.
              THEAD-TDNAME+10(6) = EKKN-VBELP.
          ENDCASE.
          MOVE-CORRESPONDING THEAD TO XTHEADKEY.
          CALL FUNCTION 'SELECT_TEXT'
               EXPORTING
                    ID         = THEAD-TDID
                    LANGUAGE   = THEAD-TDSPRAS
                    NAME       = THEAD-TDNAME
                    OBJECT     = THEAD-TDOBJECT
               IMPORTING
                    ENTRIES    = ENTRIES
               TABLES
                    SELECTIONS = XTHEAD.
          READ TABLE XTHEAD INDEX 1.
          IF SY-SUBRC EQ 0.
            XT166P-TXNAM = XTHEAD-TDNAME.
            MODIFY XT166P.
            XDRFLG = XT166P-DRFLG.
            IF THEAD-TDOBJECT EQ 'MATERIAL' OR
               ( THEAD-TDOBJECT EQ 'EINE' AND THEAD-TDID EQ 'BT' ).
              TEXT_FLAG = TEXT_FLAG + 1.
            ENDIF.
          ELSE.
            DELETE XT166P.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

*--------------------------- UGL Change ---------------------------- UGL
* Get Info Record info                                               UGL
  select single * from eina where infnr = ekpo-infnr.               "UGL
*                                                                    UGL
* Get the Quote Information on the purchasing documents              UGL
  select * from eine where infnr eq ekpo-infnr                      "UGL
                       and ekorg eq ekko-ekorg                      "UGL
                       and esokz eq '0'.                            "UGL
  endselect.                                                        "UGL
*-------------------------- End of UGL Change ---------------------  UGL

* Infobestelltext rausnehmen, wenn er durch die erste Bestellung
* erzeugt wurde und dies die erste Bestellung ist
    IF ( XDRUVO EQ NEU OR XDRUVO EQ AEND ) AND EKPO-SPINF NE SPACE AND
       EKPO-BSTYP NE BSTYP-ANFR.
      LOOP AT XT166P WHERE TDOBJECT EQ 'EINE'
                     AND   TDID     EQ 'BT'.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
        LOOP AT XT165P WHERE VORGA    EQ EKPO-BSTYP
                       AND   TDOBJECT EQ XT166P-TDOBJECT
                       AND   KOOBJECT EQ 'EKPO'.
          LOOP AT XT166P WHERE TDOBJECT EQ 'EKPO'
                         AND   TDID     EQ XT165P-KOID.
            EXIT.
          ENDLOOP.
          EXIT.
        ENDLOOP.
        IF XT166P-TDOBJECT EQ 'EKPO' AND XT166P-TDID EQ XT165P-KOID.
* der Positionstext, aus dem der Bestelltext des Infosatzes erstellt
* wurde, wird ebenfalls gedruckt --> kann den Infobestelltext
* bei der Erstbestellung rausnehmen
          LOOP AT XT166P WHERE TDOBJECT EQ 'EINE'
                         AND   TDID     EQ 'BT'.
            EINE-INFNR = XT166P-TXNAM(10).
            EINE-EKORG = XT166P-TXNAM+10(4).
            EINE-ESOKZ = XT166P-TXNAM+14(1).
            EINE-WERKS = XT166P-TXNAM+15(4).
            SELECT SINGLE * FROM EINE WHERE INFNR EQ EINE-INFNR
                                      AND   EKORG EQ EINE-EKORG
                                      AND   ESOKZ EQ EINE-ESOKZ
                                      AND   WERKS EQ EINE-WERKS.
            IF SY-SUBRC EQ 0.
              MOVE EINE TO *EINE.      "für nächsten Schritt
              IF EINE-NETPR EQ 0 AND
                 EINE-ERDAT EQ EKPO-AEDAT AND
                 EINE-EBELN EQ EKPO-EBELN.
*    Infosatz wurde mit großer Wahrscheinlichkeit durch die Bestellung
*    erzeugt (wenn am Tag der ersten Bestellung auch gleich noch eine
*    zweite erfaßt wird, greift diese Abfrage natürlich nicht )
                TEXT_FLAG = 1.
                DELETE XT166P.
              ENDIF.
            ENDIF.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

* XT166P bereinigen (2. Runde wegen Ausschluß Materialbestelltext) ----*
    IF TEXT_FLAG GT 1.
*  Sowohl Material- als auch Infobestelltext sind vorhanden
      LOOP AT XT166P WHERE TDOBJECT EQ 'EINE'
                     AND   TDID     EQ 'BT'.
        EINE-INFNR = XT166P-TXNAM(10).
        EINE-EKORG = XT166P-TXNAM+10(4).
        EINE-ESOKZ = XT166P-TXNAM+14(1).
        EINE-WERKS = XT166P-TXNAM+15(4).
        IF EINE-INFNR EQ *EINE-INFNR AND EINE-EKORG EQ *EINE-EKORG AND
           EINE-ESOKZ EQ *EINE-ESOKZ AND EINE-WERKS EQ *EINE-WERKS.
          MOVE *EINE TO EINE.  "kann oben schon mal gelesen worden sein
          SY-SUBRC = 0.
        ELSE.
          SELECT SINGLE * FROM EINE WHERE INFNR EQ EINE-INFNR
                                    AND   EKORG EQ EINE-EKORG
                                    AND   ESOKZ EQ EINE-ESOKZ
                                    AND   WERKS EQ EINE-WERKS.
        ENDIF.
        IF SY-SUBRC EQ 0 AND EINE-MTXNO NE SPACE.
*       Materialbestelltext laut Infosatz nicht relevant
          TEXT_FLAG = 9.
          DELETE XT166P.
        ENDIF.
        EXIT.
      ENDLOOP.
    ENDIF.

    IF TEXT_FLAG EQ 9.
*  Materialbestelltext soll nicht gedruckt werden
*  ---> er wird ersetzt durch Infobestelltext
*  ---> Reihenfolge wird von Materialbestelltext genommen
      LOOP AT XT166P WHERE TDOBJECT EQ 'MATERIAL'
                     AND   TDID     EQ 'BEST'.
        XT166P-TDOBJECT     = 'EINE'.
        XT166P-TDID         = 'BT'.
        XT166P-TXNAM        = EINE-INFNR.
        XT166P-TXNAM+10(4)  = EINE-EKORG.
        XT166P-TXNAM+14(1)  = EINE-ESOKZ.
        XT166P-TXNAM+15(4)  = EINE-WERKS.
        MODIFY XT166P.
        EXIT.
      ENDLOOP.
    ENDIF.

    CLEAR XDRFLG.
* XT166P bereinigen ---------------------------------------------------*
    LOOP AT XT166P.
* Kein weiterer Text mit gleichem Reihenfolge-Kennzeichen -------------*
* wegen Verdrängung Materialbestelltext durch Infobestelltext ---------*
      IF XT166P-DRFLG EQ XDRFLG.
        DELETE XT166P.
      ELSE.
        XDRFLG = XT166P-DRFLG.
      ENDIF.
    ENDLOOP.

    PERFORM LESEN_QM_DOCUMENTS.
  ENDIF.

*- Bereitstellen Kanban-Daten zur Position ----------------------------*
  CLEAR CPKME.
  IF EKPO-KANBA NE SPACE AND EKKO-BSTYP EQ BSTYP-BEST.
    CALL FUNCTION 'PK_GET_DATA_FOR_PURCHASE_ORDER'
         EXPORTING
              IEBELN             = EKPO-EBELN
              IEBELP             = EKPO-EBELP
         IMPORTING
              ECPKME             = CPKME
         EXCEPTIONS
              INSUFFICIENT_INPUT = 01
              HEADER_NOT_FOUND   = 02
              PVB_NOT_FOUND      = 03.
  ENDIF.

* Lesen der Konfiguration
  IF NOT EKPO-CUOBJ IS INITIAL.   "im Standard nur, wenn konfiguriert
* --> Zeile aussternen --> auch Klassifizierung von normalem Material
*     wird angedruckt !! (Disney-Anforderung)
    IF EKPO-ATTYP NE ATTYP-SAM AND "vorläufig, damit die Klassifizierung
       EKPO-ATTYP NE ATTYP-LOT.        "auch gedruckt wird
      CALL FUNCTION 'ME_VAR_GET_CLASSIFICATION'      "auch gedruckt wird
           EXPORTING
                I_MATNR    = EKPO-EMATN
                I_SPRAS    = EKKO-SPRAS
                I_EBELP    = EKPO-EBELP
                I_CUOBJ    = EKPO-CUOBJ
                I_DATE     = EKKO-BEDAT
                I_LIFNR    = EKKO-LIFNR
                I_UPTYP    = EKPO-UPTYP
           TABLES
                T_CONF_OUT =  TCONF_OUT
           EXCEPTIONS
                OTHERS     = 1.
    ENDIF.
  ENDIF.

  IF M_FLAG NE SPACE AND Z >= 1.
* lesen, wenn Sammelartikel auf abweichende Konditionen

    IF EKPO-ATTYP EQ ATTYP-SAM AND EKPO-UPVOR NE SPACE.
      LOOP AT XPEKPOV WHERE UEBPO EQ EKPO-EBELP AND EBELP IS INITIAL.
        MOVE XPEKPOV-NETPR TO SAMPR.
      ENDLOOP.

      LOOP AT XPEKPOV WHERE UEBPO EQ EKPO-EBELP AND EBELP NE SPACE.
        MOVE XPEKPOV-NETPR TO VARPR.
        EXIT.
      ENDLOOP.

      S = 0.
      LOOP AT XPEKPOV WHERE UEBPO EQ EKPO-EBELP AND EBELP NE SPACE.
        IF XPEKPOV-NETPR NE SAMPR.
          S = 1.
          EXIT.
        ENDIF.
      ENDLOOP.

      V = 0.
      LOOP AT XPEKPOV WHERE UEBPO EQ EKPO-EBELP AND EBELP NE SPACE.
        IF XPEKPOV-NETPR NE VARPR.
          V = 1.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Lesen Konditionen ---------------------------------------------------*
  IF EKPO-UEBPO EQ SPACE OR PEKPOV-XKONDV NE SPACE.
    IF EKKO-BSTYP NE BSTYP-KONT AND EKKO-STAKO EQ SPACE.
      IF ( EKPO-ATTYP EQ ATTYP-SAM OR EKPO-ATTYP EQ ATTYP-LOT )
         AND EKPO-UPVOR NE SPACE AND M_FLAG NE SPACE AND Z >= 1.
        LOOP AT XEKPO WHERE EBELP EQ EKPO-EBELP OR
                            UEBPO EQ EKPO-EBELP.
          MOVE XEKPO TO EKPO.
          REFRESH TKOMVD.
          CHECK XLPET EQ SPACE.
          CHECK EKKO-KNUMV NE SPACE.
          CHECK EKPO-NETPR NE 0.
          CHECK EKKO-BSTYP EQ BSTYP-LFPL OR
                EKKO-BSTYP EQ BSTYP-BEST.
*     IF EKKO-BSTYP EQ BSTYP-BEST.  "wegen statistischen Konditionen
*        CHECK EKPO-NETWR NE EKPO-BRTWR.  "nicht mehr haltbar
*     ENDIF.
*     IF EKKO-BSTYP EQ BSTYP-LFPL.
*        CHECK EKPO-ZWERT NE EKPO-BRTWR.
*     ENDIF.
          CHECK EKPO-PRSDR NE SPACE.
          CLEAR: KOMP.

          KOMK-MANDT = EKKO-MANDT.
          IF EKKO-KALSM NE SPACE.
            KOMK-KALSM = EKKO-KALSM.
          ELSE.
            KOMK-KALSM = 'RM0000'.
          ENDIF.
          KOMK-KAPPL = 'M'.
          KOMK-WAERK = EKKO-WAERS.
          KOMK-KNUMV = EKKO-KNUMV.
          KOMK-LIFNR = EKKO-LIFNR.     "WDUK
          KOMP-KPOSN = EKPO-EBELP.
          KOMP-MATNR = EKPO-MATNR.
          KOMP-WERKS = EKPO-WERKS.
          KOMP-MATKL = EKPO-MATKL.
          KOMP-INFNR = EKPO-INFNR.
          KOMP-EVRTN = EKPO-KONNR.
          KOMP-EVRTP = EKPO-KTPNR.

*- Mengen richtig fuellen ---------------------------------------------*
          IF EKKO-BSTYP EQ BSTYP-BEST.
            KOMP-MGLME = EKPO-MENGE.
          ELSE.
            KOMP-MGLME = EKPO-KTMNG.
          ENDIF.
          IF KOMP-MGLME EQ 0.
            KOMP-MGLME = 1000.  "z.B. bestellte Banf nochmal bestellt
          ENDIF.
*- Menge umrechnen bei abweichender Preiseinheit ----------------------*
          IF EKPO-MEINS NE EKPO-BPRME.
            IF EKPO-BPUMN NE 0.
              KOMP-MGAME = KOMP-MGLME * EKPO-BPUMZ / EKPO-BPUMN.
            ELSE.
              KOMP-MGAME = KOMP-MGLME.
            ENDIF.
            KOMP-UMVKZ = EKPO-BPUMZ.
            KOMP-UMVKN = EKPO-BPUMN.
            KOMP-VRKME = EKPO-BPRME.
          ELSE.
            KOMP-MGAME = KOMP-MGLME.
            KOMP-VRKME = EKPO-MEINS.
          ENDIF.
          KOMP-MEINS = EKPO-LMEIN.
          KOMP-LGUMZ = EKPO-UMREZ.
          KOMP-LGUMN = EKPO-UMREN.
          KOMP-LAGME = EKPO-MEINS.
          KOMP-KURSK = EKKO-WKURS.
          KOMP-IX_KOMK = 1.

          CALL FUNCTION 'RV_PRICE_PRINT_ITEM'
               EXPORTING
                    COMM_HEAD_I = KOMK
                    COMM_ITEM_I = KOMP
                    LANGUAGE    = EKKO-SPRAS
               IMPORTING
                    COMM_HEAD_E = KOMK
                    COMM_ITEM_E = KOMP
               TABLES
                    TKOMV       = TKOMV
                    TKOMVD      = TKOMVD.
          DESCRIBE TABLE TKOMVD LINES SY-TFILL.
          IF SY-TFILL EQ 1.            "Brutto = Netto
            READ TABLE TKOMVD INDEX 1.
            IF TKOMVD-KSCHL IS INITIAL.
              REFRESH TKOMVD.
            ELSE.
              READ TABLE TKOMV WITH KEY KSCHL = TKOMVD-KSCHL.
              IF SY-SUBRC EQ 0 AND TKOMV-KNTYP EQ 'H'.
                REFRESH TKOMVD.
              ENDIF.
            ENDIF.
          ENDIF.
*          IF M_FLAG NE SPACE AND EKPO-UPTYP EQ UPTYP-VAR.

* fuellen der internen Tabelle mit Konditionssatz
          LOOP AT TKOMVD.
            MOVE TKOMVD TO KOND.
            APPEND KOND.
          ENDLOOP.

          IF S = 0.
            EXIT.
          ENDIF.

          IF EKPO-ATTYP EQ ATTYP-SAM OR
             EKPO-ATTYP EQ ATTYP-LOT.
            LOOP AT XPEKPOV WHERE UEBPO EQ EKPO-EBELP AND
                                  EBELP IS INITIAL.
              MOVE XPEKPOV TO PEKPOV.
            ENDLOOP.
          ELSE.
            LOOP AT XPEKPOV WHERE EBELP EQ EKPO-EBELP.
              MOVE XPEKPOV TO PEKPOV.
            ENDLOOP.
          ENDIF.
*          ENDIF.
          IF PEKPOV-MENGE GT 0 AND PEKPOV-MENGE NE KOMP-MGLME.
*        umrechnen des Konditionswertes auf die Menge, für die diese
*        Kondition gültig sind
            LOOP AT TKOMVD.
              TKOMVD-KWERT = TKOMVD-KWERT * PEKPOV-MENGE / KOMP-MGLME.
              MODIFY TKOMVD.
            ENDLOOP.
            PERFORM MENGE_AUSGEBEN USING
                    PEKPOV-MENGE T006-ANDEC PEKPOV-PRMNG.
          ENDIF.
        ENDLOOP.
      ELSE.
        REFRESH TKOMVD.
        IF XLPET EQ SPACE             and
           EKKO-KNUMV NE SPACE        and
           EKPO-NETPR NE 0            and
           ( EKKO-BSTYP EQ BSTYP-LFPL OR
           EKKO-BSTYP EQ BSTYP-BEST ) and
           EKPO-PRSDR NE SPACE.
*        CHECK XLPET EQ SPACE.
*        CHECK EKKO-KNUMV NE SPACE.
*        CHECK EKPO-NETPR NE 0.
*        CHECK EKKO-BSTYP EQ BSTYP-LFPL OR
*              EKKO-BSTYP EQ BSTYP-BEST.
*     IF EKKO-BSTYP EQ BSTYP-BEST.  "wegen statistischen Konditionen
*        CHECK EKPO-NETWR NE EKPO-BRTWR.  "nicht mehr haltbar
*     ENDIF.
*     IF EKKO-BSTYP EQ BSTYP-LFPL.
*        CHECK EKPO-ZWERT NE EKPO-BRTWR.
*     ENDIF.
*        CHECK EKPO-PRSDR NE SPACE.
          CLEAR: KOMP.

          KOMK-MANDT = EKKO-MANDT.
          IF EKKO-KALSM NE SPACE.
            KOMK-KALSM = EKKO-KALSM.
          ELSE.
            KOMK-KALSM = 'RM0000'.
          ENDIF.
          KOMK-KAPPL = 'M'.
          KOMK-WAERK = EKKO-WAERS.
          KOMK-KNUMV = EKKO-KNUMV.
          KOMK-LIFNR = EKKO-LIFNR.     "WDUK
          KOMP-KPOSN = EKPO-EBELP.
          KOMP-MATNR = EKPO-MATNR.
          KOMP-WERKS = EKPO-WERKS.
          KOMP-MATKL = EKPO-MATKL.
          KOMP-INFNR = EKPO-INFNR.
          KOMP-EVRTN = EKPO-KONNR.
          KOMP-EVRTP = EKPO-KTPNR.

*- Mengen richtig fuellen ---------------------------------------------*
          IF EKKO-BSTYP EQ BSTYP-BEST.
            KOMP-MGLME = EKPO-MENGE.
          ELSE.
            KOMP-MGLME = EKPO-KTMNG.
          ENDIF.
        IF KOMP-MGLME EQ 0.  "falls keine Menge gesetzt --> auf 1 setzen
            KOMP-MGLME = 1000.  "z.B. bestellte Banf nochmal bestellt
          ENDIF.
*- Menge umrechnen bei abweichender Preiseinheit ----------------------*
          IF EKPO-MEINS NE EKPO-BPRME.
            IF EKPO-BPUMN NE 0.
              KOMP-MGAME = KOMP-MGLME * EKPO-BPUMZ / EKPO-BPUMN.
            ELSE.
              KOMP-MGAME = KOMP-MGLME.
            ENDIF.
            KOMP-UMVKZ = EKPO-BPUMZ.
            KOMP-UMVKN = EKPO-BPUMN.
            KOMP-VRKME = EKPO-BPRME.
          ELSE.
            KOMP-MGAME = KOMP-MGLME.
            KOMP-VRKME = EKPO-MEINS.
          ENDIF.
          KOMP-MEINS = EKPO-LMEIN.
          KOMP-LGUMZ = EKPO-UMREZ.
          KOMP-LGUMN = EKPO-UMREN.
          KOMP-LAGME = EKPO-MEINS.
          KOMP-KURSK = EKKO-WKURS.
          KOMP-IX_KOMK = 1.

          CALL FUNCTION 'RV_PRICE_PRINT_ITEM'
               EXPORTING
                    COMM_HEAD_I = KOMK
                    COMM_ITEM_I = KOMP
                    LANGUAGE    = EKKO-SPRAS
               IMPORTING
                    COMM_HEAD_E = KOMK
                    COMM_ITEM_E = KOMP
               TABLES
                    TKOMV       = TKOMV
                    TKOMVD      = TKOMVD.
          DESCRIBE TABLE TKOMVD LINES SY-TFILL.
          IF SY-TFILL EQ 1 AND KOPFKOND EQ SPACE. "Brutto = Netto #46992
            READ TABLE TKOMVD INDEX 1.
            IF TKOMVD-KSCHL IS INITIAL.
              REFRESH TKOMVD.
            ELSE.
              READ TABLE TKOMV WITH KEY KSCHL = TKOMVD-KSCHL.
              IF SY-SUBRC EQ 0 AND TKOMV-KNTYP EQ 'H'.
                REFRESH TKOMVD.
              ENDIF.
            ENDIF.
          ENDIF.
          IF PEKPOV-MENGE GT 0 AND PEKPOV-MENGE NE KOMP-MGLME.
*        umrechnen des Konditionswertes auf die Menge, für die diese
*        Kondition gültig sind
            LOOP AT TKOMVD.
              TKOMVD-KWERT = TKOMVD-KWERT * PEKPOV-MENGE / KOMP-MGLME.
              MODIFY TKOMVD.
            ENDLOOP.
            PERFORM MENGE_AUSGEBEN USING
                    PEKPOV-MENGE T006-ANDEC PEKPOV-PRMNG.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      REFRESH TEKOMD.
      IF EKKO-BSTYP NE BSTYP-ANFR AND
*        EKPO-NETPR NE 0 AND
         ( EKPO-NETPR NE 0 OR EKPO-PSTYP EQ PSTYP-WAGR ) AND
         EKPO-PRSDR NE SPACE.
        CALL FUNCTION 'ME_PRINT_CONTRACT_CONDITIONS'
             EXPORTING
                  POSITION = EKPO-EBELP
             TABLES
                  TEKOMD   = TEKOMD.
      ENDIF.
    ENDIF.
  ELSE.
    REFRESH: TKOMVD, TEKOMD.
  ENDIF.

*-- Lesen des Verkaufspreises und der zugehörigen EAN -- (IS-R) -------*
  CLEAR EKVKP.
  PERFORM VKP_ERMITTELN USING    EKPO-EBELP
                                 EKPO-MATNR
                                 EKPO-WERKS
                        CHANGING EKVKP.

*--- HTN-Abwicklung
  REFRESH: HTNMAT, HTNAMP.                                  "109229
  IF EKPO-MFRPN IS INITIAL AND NOT EKPO-MPROF IS INITIAL.
*   refresh: htnmat, htnamp.                              "109229
    CALL FUNCTION 'MB_SEARCH_HTN_MATERIAL'
         EXPORTING
*              I_EMATN          =
              I_BMATN          = EKPO-MATNR
              I_MPROF          = EKPO-MPROF
*              I_WERKS          =
*              I_DATUV          =
*              I_DATUB          =
*              I_REVLV          =
*              I_MFRNR          =
         TABLES
              E_HTNMAT         = HTNMAT
              E_HTNAMP         = HTNAMP
         EXCEPTIONS
              NO_RECORDS_FOUND = 1
              OTHERS           = 2.
  ELSEIF NOT EKPO-MFRPN IS INITIAL.

    CALL FUNCTION 'MB_READ_TMPPF'
         EXPORTING
              PROFILE        = EKPO-MPROF
         IMPORTING
              MPN_PARAMETERS = TMPPF
         EXCEPTIONS
              NO_ENTRY       = 1
              OTHERS         = 2.

    IF NOT TMPPF-MPAMP IS INITIAL.
      CALL FUNCTION 'MB_SEARCH_HTN_MATERIAL'
           EXPORTING
                I_EMATN          = EKPO-EMATN
                I_BMATN          = EKPO-MATNR
                I_MPROF          = EKPO-MPROF
                I_WERKS          = EKPO-WERKS
                I_DATUV          = SY-DATUM
*              I_DATUB          =
                 I_REVLV          = EKPO-REVLV
*              I_MFRNR          =
           TABLES
                E_HTNMAT         = HTNMAT
                E_HTNAMP         = HTNAMP
           EXCEPTIONS
                NO_RECORDS_FOUND = 1
                OTHERS           = 2.
    ENDIF.
    REFRESH: HTNMAT. CLEAR: HTNMAT.
    IF NOT HTNAMP[] IS INITIAL.
      LOOP AT HTNAMP.
        HTNMAT-MFRPN = EKPO-MFRPN.
        IF NOT EKPO-EMNFR IS INITIAL.
          HTNMAT-EMNFR = EKPO-EMNFR.
        ELSE.
          HTNMAT-EMNFR = EKPO-MFRNR.
        ENDIF.
        HTNMAT-MFRNR = HTNAMP-MFRNR.   "Herstellerwerk
        HTNMAT-REVLV = HTNAMP-REVLV.   "Revisionslevel
        htnmat-matnr = ekpo-ematn.
        APPEND HTNMAT.
      ENDLOOP.
      CLEAR HTNAMP. REFRESH HTNAMP.
    ELSE.
      HTNMAT-MFRPN = EKPO-MFRPN.
      IF NOT EKPO-EMNFR IS INITIAL.
        HTNMAT-EMNFR = EKPO-EMNFR.
      ELSE.
        HTNMAT-EMNFR = EKPO-MFRNR.
      ENDIF.
      HTNMAT-REVLV = EKPO-REVLV.       "Revisionslevel
      htnmat-matnr = ekpo-ematn.
      APPEND HTNMAT.
    ENDIF.
  ENDIF.
*
  DATA: L_VORGA LIKE T160-VORGA,
         L_MTCOM LIKE MTCOM,
         L_MT06E LIKE MT06E,
         L_MTCOR LIKE MTCOR.

  IF NOT HTNMAT[] IS INITIAL.
    LOOP AT HTNMAT.
      CLEAR: L_MTCOM, L_MT06E, L_MTCOR, L_VORGA.

      l_mtcom-kenng = 'MT06E'.
      L_MTCOM-MATNR = HTNMAT-MATNR.
      L_MTCOM-PSTAT = 'E'.
      L_MTCOM-SPRAS = EKKO-SPRAS.
      l_mtcom-kzspr = 'X'.

      CALL FUNCTION 'MEX_CHECK_MATERIAL'
           EXPORTING
                IM_MTCOM = L_MTCOM
                IM_VORGA = SPACE
           IMPORTING
                EX_MT06E = L_MT06E
                EX_MTCOR = L_MTCOR
           EXCEPTIONS
                OTHERS   = 1.
      IF SY-SUBRC NE 0.
        DELETE HTNMAT.
        CONTINUE.
      ENDIF.
      CALL FUNCTION 'MEX_CHECK_MT06E_STATUS'
           EXPORTING
                IM_MTCOM = L_MTCOM
                IM_MTCOR = L_MTCOR
                IM_MT06E = L_MT06E
           EXCEPTIONS
                OTHERS   = 1.
      IF SY-SUBRC NE 0.
        DELETE HTNMAT.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF NOT HTNAMP[] IS INITIAL.
    LOOP AT HTNAMP.
      CLEAR: L_MTCOM, L_MT06E, L_MTCOR, L_VORGA.

      l_mtcom-kenng = 'MT06E'.
      L_MTCOM-MATNR = HTNAMP-MATNR.
      L_MTCOM-PSTAT = 'E'.
      L_MTCOM-SPRAS = EKKO-SPRAS.
      l_mtcom-kzspr = 'X'.

      CALL FUNCTION 'MEX_CHECK_MATERIAL'
           EXPORTING
                IM_MTCOM = L_MTCOM
                IM_VORGA = SPACE
           IMPORTING
                EX_MT06E = L_MT06E
                EX_MTCOR = L_MTCOR
           EXCEPTIONS
                OTHERS   = 1.
      IF SY-SUBRC NE 0.
        DELETE HTNAMP.
        CONTINUE.
      ENDIF.
      CALL FUNCTION 'MEX_CHECK_MT06E_STATUS'
           EXPORTING
                IM_MTCOM = L_MTCOM
                IM_MTCOR = L_MTCOR
                IM_MT06E = L_MT06E
           EXCEPTIONS
                OTHERS   = 1.
      IF SY-SUBRC NE 0.
        DELETE HTNAMP.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.
*
*- Materialstammdaten dazulesen ---------------------------------------*
  CLEAR: MARA, MARC, MT06E.                               "171485
  IF NOT EKPO-MATNR IS INITIAL.
    CALL FUNCTION 'MARA_SINGLE_READ'
         EXPORTING
              MATNR  = EKPO-MATNR
         IMPORTING
              WMARA  = MARA
         EXCEPTIONS
              OTHERS = 5.
    MOVE-CORRESPONDING MARA TO MT06E.
  ENDIF.
  IF EKPO-MATNR NE SPACE AND NOT EKPO-WERKS IS INITIAL.
    CALL FUNCTION 'MARC_SINGLE_READ'
         EXPORTING
              MATNR  = EKPO-MATNR
              WERKS  = EKPO-WERKS
         IMPORTING
              WMARC  = MARC
         EXCEPTIONS
              OTHERS = 1.
    MOVE-CORRESPONDING MARC TO MT06E.
  ENDIF.

*- im Streckenfall Auftragsdaten nachlesen ----------------------------*
  IF EKPO-STATU EQ 'V'.                "P.O. item generated from SD
*- read sales order + item No from account ----------------------------*
    SELECT SINGLE * FROM EKKN
             WHERE EBELN = EKPO-EBELN AND
                   EBELP = EKPO-EBELP AND
                   ZEKKN = '01'.
    H_VBELN = EKKN-VBELN.
    H_VBELP = EKKN-VBELP.

    IF NOT H_VBELN IS INITIAL.
*- read sales order header VBAK ---------------------------------------*
      CLEAR VBAK.
      SELECT SINGLE * FROM VBAK WHERE VBELN = H_VBELN.
* read sales order header VBKD ----------------------------------------*
      CLEAR *VBKD.
      SELECT SINGLE * FROM VBKD INTO *VBKD
                      WHERE VBELN = H_VBELN AND POSNR = '000000' .
*- read sales order item VBAP -----------------------------------------*
      CLEAR VBAP.
      SELECT SINGLE * FROM VBAP
                      WHERE VBELN = H_VBELN AND POSNR = H_VBELP.
*- read sales order item VBKD -----------------------------------------*
      CLEAR VBKD.
      SELECT SINGLE * FROM VBKD
                      WHERE VBELN = H_VBELN AND POSNR = H_VBELP.
    ENDIF.
  ENDIF.

*- Buying and additionals ---------------------------------------------*
  IF NOT EKKO-ADDNR IS INITIAL.
    CALL FUNCTION 'WTAD_BUYING_PRINT'
         EXPORTING
              ADDIBELNR                    = EKKO-ADDNR
              FI_EKPO                      = EKPO
*         LANGUAGE                     = SY-LANGU
         TABLES
              FET_ADDIS_IN_ORDERS          = L_ADDIS_IN_ORDERS
         EXCEPTIONS
              ADDIS_NOT_ACTIVE             = 1
              NO_ADDIS_FOR_BUYING_POSITION = 2
              OTHERS                       = 3.
    IF SY-SUBRC <> 0.
      REFRESH L_ADDIS_IN_ORDERS.
    ENDIF.
  ENDIF.

*- Absagegrund nachlesen
  IF XDRUVO EQ ABSA  AND EKKO-BSTYP EQ BSTYP-ANFR.
    SELECT SINGLE * FROM TMAMT INTO TMAMT WHERE SPRAS EQ EKKO-SPRAS
                                          AND   AGMEM EQ EKPO-AGMEM.
  ENDIF.

*- read documents -----------------------------------------------------*
  DATA: OBJKY LIKE DRAD-OBJKY.

  REFRESH DOKTAB.
  OBJKY    = EKPO-EBELN.
  OBJKY+10 = EKPO-EBELP.

  SELECT * FROM DRAD
           WHERE DOKOB = 'EKPO' AND
                 OBJKY = OBJKY.
    IF SY-SUBRC = 0.
      CLEAR DOKTAB.
      MOVE-CORRESPONDING DRAD TO DOKTAB.
*
      SELECT SINGLE * FROM DRAT
         WHERE DOKAR = DRAD-DOKAR AND
               DOKNR = DRAD-DOKNR AND
               DOKVR = DRAD-DOKVR AND
               DOKTL = DRAD-DOKTL AND
               LANGU = SY-LANGU.
      IF SY-SUBRC = 0.
        DOKTAB-DKTXT = DRAT-DKTXT.
      ENDIF.
      APPEND DOKTAB.
    ENDIF.

  ENDSELECT.

ENDFORM.
