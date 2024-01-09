* 2000/07/27 mdemeest 4.6B Union Gas changes identified by "UGL"
*                          Retrieve TERMS OF PAYMENT description
*-----------------------------------------------------------------------
*eject
*  72307  26.10.1998  CF  Hinweis wurde eingebaut, aber ausgesternt
* 125349  13.11.1998  MK  Druck Umlagerbest.: Falsche Adresse Lieferwerk
* 134882  20.01.1999  CF  Nullwerte bei Konditionen
* 165521  03.09.1999  CF  Falsche WE-Menge angedruckt
*----------------------------------------------------------------------*
* Daten lesen
*----------------------------------------------------------------------*
FORM LESEN USING UNAST STRUCTURE NAST.

  DATA: H_PARVW   LIKE EKPA-PARVW,
        LES_EBELN LIKE EKKO-EBELN.
  LES_EBELN = UNAST-OBJKY.
* Zurücksetzen Tabellen -----------------------------------------------*
  PERFORM CLEAR_TAB.
*  PERFORM MATRIXDRUCK.                                         "UGL

* Lesen Kopfdaten -----------------------------------------------------*
  SELECT SINGLE * FROM EKKO WHERE EBELN EQ LES_EBELN.

*-------------------------- UGL Change ----------------------------- UGL
* The following statement was added to retrieve the description      UGL
* for the terms of payment.                                          UGL
*                                                                    UGL
   select single * from t052u where zterm eq ekko-zterm.            "UGL
*                                                                    UGL
*-------------------------- End of UGL Change -----------------------UGL
* Zurücksetzen Konditionen --------------------------------------------*
  IF EKKO-BSTYP NE BSTYP-KONT AND EKKO-STAKO EQ SPACE.
    CALL FUNCTION 'PRICING_REFRESH'
         TABLES
              TKOMK = TKOMK
              TKOMV = TKOMV.
    CALL FUNCTION 'RV_PRICE_PRINT_REFRESH'                  "134882
         TABLES                                             "134882
              TKOMV   = TKOMV.                              "134882
  ELSE.
    CLEAR: KONTRAKT_PREIS, KOMK.
    CALL FUNCTION 'ME_RESET_CONTRACT_CONDITIONS'.
  ENDIF.

* Lesen Änderungsbelege bei Änderungsdruck ----------------------------*
  IF XDRUVO EQ AEND OR
     XDRUVO EQ LPAE.
    PERFORM LESEN_AENDERUNGEN.
    IF XDRUVO EQ LPAE AND RETCO NE 0.
* Lieferplanänderung --> keine Änderungsbelege aus Dialog (ME38) da
      XNOAEND = 'X'.
    ENDIF.
    IF XDRUVO EQ AEND AND RETCO NE 0.
* Bestelländerung --> keine Änderungsbelege --> erfolgreich verarbeitet
      PERFORM PROTOCOL_UPDATE USING '140' EKKO-EBELN SPACE SPACE SPACE.
      RETCO = 0.     "damit Nachrichtenstatus auf '1' gesetzt wird
      EXIT.
    ENDIF.
  ENDIF.

* Lesen Überschrifttabellen -------------------------------------------*
  SELECT SINGLE * FROM T166U WHERE SPRAS EQ EKKO-SPRAS
                             AND   DRUVO EQ XDRUVO
                             AND   BSTYP EQ EKKO-BSTYP
                             AND   BSART EQ EKKO-BSART.

* Lesen Lieferantendaten ----------------------------------------------*
* IF EKKO-LIFNR NE SPACE.
*--- Abweichende Bestell- bzw. Vertragsadresse vorhanden? -------------*
  SELECT SINGLE * FROM  T161N
         WHERE  KVEWE       = 'B'
         AND    KAPPL       = UNAST-KAPPL    .
*--- Altes Verfahren: Lies EKPA mit 'BA' bzw. 'VA'
  IF SY-SUBRC NE 0 OR T161N-NEUPR IS INITIAL.
    CASE EKKO-BSTYP.
      WHEN BSTYP-KONT.
        H_PARVW = 'VA'.
      WHEN BSTYP-LFPL.
        IF XDRUVO CA '1237'.
          H_PARVW = 'VA'.
        ELSE.
          H_PARVW = 'BA'.
        ENDIF.
      WHEN OTHERS.
        H_PARVW = 'BA'.
    ENDCASE.
  ELSE.
*--- Neues Verfahren: Lies EKPA mit der Partnerrolle aus der NAST
    H_PARVW = UNAST-PARVW.
  ENDIF.
  CLEAR EKPA.
  IF H_PARVW NE SPACE.
    SELECT * FROM EKPA WHERE EBELN = EKKO-EBELN
                         AND PARVW = H_PARVW
                         AND LIFN2 = UNAST-PARNR.
      EXIT.
    ENDSELECT.
  ENDIF.
  IF EKPA-LIFN2 NE SPACE AND EKPA-LIFN2 NE EKKO-LIFNR.      "72307
    CLEAR: EKKO-VERKF, EKKO-IHREZ.
    SELECT SINGLE * FROM LFA1 WHERE LIFNR = EKPA-LIFN2.
    TABLES: LFM1.                                           "72307
    SELECT SINGLE VERKF FROM  LFM1 INTO (EKKO-VERKF)        "72307
           WHERE  LIFNR       = EKPA-LIFN2                  "72307
           AND    EKORG       = EKPA-EKORG     .            "72307
    EKKO-ADRNR = LFA1-ADRNR.                                "126150
  ELSE.
    CALL FUNCTION 'MM_ADDRESS_GET'
         EXPORTING
              I_EKKO = EKKO
         IMPORTING
              E_SADR = SADR
         EXCEPTIONS
              OTHERS = 1.
    MOVE-CORRESPONDING SADR TO LFA1.
    IF EKKO-ADRNR IS INITIAL.                               "126150
      EKKO-ADRNR = LFA1-ADRNR.                              "126150
    ENDIF.                                                  "126150
  ENDIF.
* ENDIF.
* Lesen Lieferwerk ----------------------------------------------------*
* IF EKKO-RESWK NE SPACE.
*   SELECT SINGLE * FROM T001W WHERE WERKS EQ EKKO-RESWK.
*   IF T001W-ADRNR IS INITIAL.                              "125349
*     MOVE-CORRESPONDING T001W TO LFA1.
*   ELSE.                                                   "125349
*     CLEAR ADDR1_SEL.                                      "125349
*     ADDR1_SEL-ADDRNUMBER = T001W-ADRNR.                   "125349
*     CALL FUNCTION 'ADDR_GET'                              "125349
*          EXPORTING                                        "125349
*               ADDRESS_SELECTION       = ADDR1_SEL         "125349
*          IMPORTING                                        "125349
*               SADR                    = SADR              "125349
*          EXCEPTIONS                                       "125349
*               OTHERS                  = 1.                "125349
*     IF SY-SUBRC EQ 0.                                     "125349
*       MOVE-CORRESPONDING SADR TO LFA1.                    "125349
*     ELSE.                                                 "125349
*       MOVE-CORRESPONDING T001W TO LFA1.                   "125349
*     ENDIF.                                                "125349
*     EKKO-ADRNR = T001W-ADRNR.                                 "126150
*   ENDIF.                                                  "125349
*   CLEAR T001W.
* ENDIF.

* Lesen Einkäufergruppe -----------------------------------------------*
  SELECT SINGLE * FROM T024 WHERE EKGRP EQ EKKO-EKGRP.
  SELECT SINGLE * FROM TINCT WHERE SPRAS EQ EKKO-SPRAS
                             AND   INCO1 EQ EKKO-INCO1.
  SELECT SINGLE * FROM T001 WHERE BUKRS EQ EKKO-BUKRS.
  PERFORM LESEN_KOPF_DRUCK.

* Lesen Absagegrund
  IF EKKO-ABSGR NE SPACE.
    SELECT SINGLE * FROM T165M WHERE SPRAS EQ EKKO-SPRAS
                               AND   ABSGR EQ EKKO-ABSGR.
  ENDIF.


* Lesen Positionsdaten ------------------------------------------------*
  SELECT * FROM EKPO WHERE EBELN = EKKO-EBELN.
    IF EKPO-KTPNR EQ SPACE.
      CLEAR EKPO-KTPNR.
    ENDIF.
    IF EKPO-UPTYP NE SPACE.            "Unterposition zu drucken?
      PERFORM TMSI2_LESEN USING EKPO-SIKGR.
      CHECK TMSI2-SIDRU EQ SPACE.
    ENDIF.
    IF XDRUVO NE AEND.
      IF EKPO-LOEKZ NE SPACE.
        HLOEP = 'X'.
      ENDIF.
      CHECK EKPO-LOEKZ EQ SPACE.
    ENDIF.
    IF EKPO-EMATN EQ SPACE.
      EKPO-EMATN = EKPO-MATNR.
    ENDIF.
    IF EKPO-RETPO NE SPACE.
*  ekpo-NETPR = KOMP-NETPR * -1.
      CALL FUNCTION 'ME_CHANGE_SIGNS_RETURN_ITEM'
           IMPORTING
                P_EKPO = ekpo.
* VORZEICHENWECHSEL BEI RETOURENPOSITIONEN AUSFUEHREN
* (SYNCHRONISATION ZWISCHEN BESTELLUNG UND NACHTRAEGLICHER ABRECHNUNG)
    ENDIF.
    IF ( EKKO-STAKO NE SPACE OR EKKO-BSTYP EQ BSTYP-KONT ) AND
         EKPO-KTMNG EQ 0.
      CLEAR: EKPO-ZWERT,
             EKPO-BRTWR,
             EKPO-EFFWR,
             EKPO-BONBA,
             EKPO-KZWI1,
             EKPO-KZWI2,
             EKPO-KZWI3,
             EKPO-KZWI4,
             EKPO-KZWI5,
             EKPO-KZWI6.
    ENDIF.
    IF EKPO-STAPO NE SPACE.
      HSTAP = 'X'.
*     IF EKPO-ATTYP EQ '01'.
*        HSAMM = 'X'.
*     ENDIF.
    ENDIF.
    IF EKPO-UPTYP NE SPACE.   "Unterpositionszuordnung druckrelevant
      HSAMM = 'X'.
    ENDIF.
    IF EKPO-ETDRK EQ '1' AND
       ( XDRUVO EQ LPET OR XDRUVO EQ LPAE ).
* Position mit druckrelevanten Einteilungen aus der Disposition
      XETDRK = 'X'.
    ENDIF.
* Dazulesen der letzten WE-Menge mit Lieferscheinnummer
*   select * from ekbe into table xekbe
*             where ebeln  = ekpo-ebeln and
*                   ebelp  = ekpo-ebelp.

    CALL FUNCTION 'ME_READ_LAST_GR'
         EXPORTING
              I_EBELN = EKPO-EBELN
              I_EBELP = EKPO-EBELP
         TABLES
              E_PEKPO = XPEKPO
         EXCEPTIONS
              OTHERS  = 1.

*   call function 'ME_READ_HISTORY'
*        exporting
*             ebeln  = ekpo-ebeln
*             ebelp  = ekpo-ebelp
*             webre  = ekpo-webre
*        tables
*             xekbe  = xekbe
*             xekbes = tekbes
*             xekbez = xekbez
*             xekbnk = xekbnk
*             xekbz  = xekbz.
*   sort xekbe descending by budat cpudt cputm.
*   loop at xekbe where bewtp eq we.   "erste Zeile lesen und raus
*     select single * from ekes where ebeln = xekbe-ebeln
*                               and   ebelp = xekbe-ebelp
*                               and   etens = xekbe-etens.
*     if ekes-vbeln is initial.
*       select single * from mkpf where mblnr = xekbe-belnr
*                                 and   mjahr = xekbe-gjahr.
*       xpekpo-lfdat = mkpf-bldat.
*     else.
*       select single * from likp where vbeln = ekes-vbeln.
*       xpekpo-lfdat = likp-lfdat.
*     endif.
*     xpekpo-xblnr = xekbe-xblnr.      "likp-verur.
*     xpekpo-ebelp = ekpo-ebelp.
*     xpekpo-lwemg = xekbe-menge.
*     xpekpo-budat = xekbe-budat.
*     append xpekpo.
*     exit.
*   endloop.
    MOVE-CORRESPONDING EKPO TO XEKPO.
    CLEAR AB-MENGE.
    IF XDRUVO EQ AUFB AND EKPO-BSTAE NE SPACE.
      SELECT SINGLE * FROM T163D WHERE IBTYP EQ '1'.
      SELECT SUM( MENGE ) INTO (AB-MENGE) FROM EKES
                   WHERE EBELN = EKPO-EBELN
                     AND EBELP = EKPO-EBELP
                     AND EBTYP = T163D-EBTYP.
    ENDIF.
    XEKPO-BSMNG = AB-MENGE.
    APPEND XEKPO.
  ENDSELECT.

  IF XDRUVO EQ LPAE.
* Lieferplanänderung
* --> entweder Änderungsbelege aus Dialog (ME38)
* --> oder     Kennzeichen in Position aus Disposition
    IF XNOAEND NE SPACE AND XETDRK EQ SPACE.
      PERFORM PROTOCOL_UPDATE USING '140' EKKO-EBELN SPACE SPACE SPACE.
      RETCO = 1.
      EXIT.
    ENDIF.
  ENDIF.

  SORT XEKPO BY EBELN EBELP.

* Lesen Einteilungen --------------------------------------------------*
  SELECT * FROM EKET WHERE EBELN = EKKO-EBELN.
    MOVE-CORRESPONDING EKET TO HEKETKEY.
    READ TABLE HEKET WITH KEY HEKETKEY.
    H_SUBRC = SY-SUBRC.
    IF H_SUBRC = 0.
      MOVE-CORRESPONDING EKET TO XEKET.
      IF NOT HEKET-EINDT IS INITIAL.
        XEKET-EINDT = HEKET-EINDT.
      ENDIF.
      IF NOT HEKET-LPEIN IS INITIAL.
        XEKET-LPEIN = HEKET-LPEIN.
      ENDIF.
      IF NOT HEKET-TFLAG IS INITIAL.
        XEKET-UZEIT = HEKET-UZEIT.
      ENDIF.
      IF HEKET-MENGE IS INITIAL.
        XEKET-AMENG = XEKET-MENGE.
      ELSE.
        XEKET-AMENG = HEKET-MENGE.
      ENDIF.
      CLEAR XEKET-MENGE.
      CLEAR XEKET-MAHNZ.
      XEKET-EINDT = EKET-ALTDT.
      CLEAR XEKET-WEMNG.                                       "165521
      APPEND XEKET.
    ENDIF.
    MOVE-CORRESPONDING EKET TO XEKET.
* neu zu 4.0: wenn die dispo das Einteilungsdatum ändert, wird nur ein
* update gemacht und das Feld eket-altdt gesetzt. Um bei der Ausgabe
* die gewohnte form zu erhalten, muß eine zusätzliche Einteilungszeile
* mit Menge 0 und altem Einteilungsdatum erzeugt werden.
    IF XDRUVO EQ AEND OR XDRUVO EQ LPAE.
      IF EKET-EINDT NE EKET-ALTDT AND EKKO-BSTYP EQ BSTYP-LFPL
                                  AND NOT EKET-ALTDT IS INITIAL
                                  AND H_SUBRC NE 0.
        MOVE-CORRESPONDING EKET TO ZEKET.
        CLEAR XEKET-AMENG.
        CLEAR ZEKET-MENGE.
        ZEKET-EINDT = EKET-ALTDT.
        APPEND ZEKET TO XEKET.
      ENDIF.
    ENDIF.
    IF H_SUBRC = 0.
      CLEAR XEKET-AMENG.
    ENDIF.
    APPEND XEKET.
  ENDSELECT.

  IF XLMAHN NE SPACE AND ( XDRUVO EQ MAHN OR XDRUVO EQ LPMA ).
    PERFORM EKET_EKES_ABMISCHEN.
  ENDIF.

  DESCRIBE TABLE XEKET LINES SY-TFILL.
  IF SY-TFILL EQ 0 AND XDRUVO EQ LPET.
* Lieferplaneinteilung (Neudruck)
* --> keine Einteilungen vorhanden
* --> Nachricht bleibt weiter in der NAST mit Status 0 (unverarbeitet)
* --> Einteilungen werden erst später im Dialog erfaßt oder aus
* --> der Disposition erstellt
    PERFORM PROTOCOL_UPDATE USING '141' EKKO-EBELN SPACE SPACE SPACE.
    RETCO = 0.
    EXIT.
  ENDIF.
  SORT XEKET BY EBELN EBELP EINDT UZEIT ETENR.
  CLEAR XPEKPO.

* Feststellen, ob einheitliches Lieferdatum ---------------------------*
  LOOP AT XEKET.
    IF XPEKPO-EBELP NE XEKET-EBELP.
      IF XPEKPO-EBELP NE 0.
        IF H-IND NE 0.
          MODIFY XPEKPO INDEX H-IND.
        ELSE.
          APPEND XPEKPO.
        ENDIF.
      ENDIF.
      READ TABLE XPEKPO WITH KEY XEKET-EBELP.
      IF SY-SUBRC NE SPACE.
        H-IND = 0.
        XPEKPO-EBELP = XEKET-EBELP.
      ELSE.
        H-IND = SY-TABIX.
      ENDIF.
      XPEKPO-LPEIN = XEKET-LPEIN.
      XPEKPO-EINDT = XEKET-EINDT.
      XPEKPO-UZEIT = XEKET-UZEIT.
      XPEKPO-LICHA = XEKET-LICHA.
      CLEAR XPEKPO-WEMNG.
      CLEAR XPEKPO-MAHNZ.
      CLEAR FZFLAG.
      XEKPOKEY-MANDT = EKKO-MANDT.
      XEKPOKEY-EBELN = EKKO-EBELN.
      XEKPOKEY-EBELP = XEKET-EBELP.
      READ TABLE XEKPO WITH KEY XEKPOKEY BINARY SEARCH.
      XPEKPO-MAHNZ = XEKPO-MAHNZ.                           "PN
    ENDIF.
* WE-Menge aufaddieren ------------------------------------------------*
    IF FZFLAG EQ SPACE.
*   if xekpo-abdat ne 0 and
      IF NOT XEKPO-ABDAT IS INITIAL   AND
         XEKPO-ABFTZ =< XPEKPO-WEMNG  AND
*      xekpo-abdat =< sy-datlo      and
         XEKPO-ABDAT =< SY-DATLO.
*      fzflag eq space.
        FZFLAG = 'X'.
        XPEKPO-WEMNG = XPEKPO-WEMNG - XEKPO-ABFTZ + XEKET-WEMNG.
      ELSEIF XEKPO-ABDAT IS INITIAL.
        FZFLAG = 'X'.
        XPEKPO-WEMNG = XPEKPO-WEMNG + XEKET-WEMNG + XEKPO-ABFTZ.
      ELSE.
        XPEKPO-WEMNG = XPEKPO-WEMNG + XEKET-WEMNG.
      ENDIF.
    ELSE.
      XPEKPO-WEMNG = XPEKPO-WEMNG + XEKET-WEMNG.
    ENDIF.
* Mahnung prüfen ------------------------------------------------------*
    IF XLMAHN NE SPACE.
      PERFORM PRUEFEN_MAHNUNG.
      IF XEKET-MAHNZ NE HMAHNZ.
        XEKET-MAHNZ = HMAHNZ.
        MODIFY XEKET.
      ENDIF.
      IF XPEKPO-MAHNZ EQ 0 AND EKKO-LPHIS IS INITIAL.
        XPEKPO-MAHNZ = XEKET-MAHNZ.
      ENDIF.
    ENDIF.
* Mehrere Termine pro Position ----------------------------------------*
    IF XPEKPO-EINDT NE XEKET-EINDT OR
       XPEKPO-UZEIT NE XEKET-UZEIT OR
       XPEKPO-LPEIN NE XEKET-LPEIN.
      CLEAR: XPEKPO-EINDT, XPEKPO-LPEIN.
    ENDIF.
    IF PEKKO-LPEIN EQ SPACE.
      MOVE XEKET-EINDT TO PEKKO-EINDT.
      MOVE XEKET-LPEIN TO PEKKO-LPEIN.
    ELSE.
* Lieferdatum nicht für alle Positionen gleich ------------------------*
      IF PEKKO-EINDT NE XEKET-EINDT OR
         XPEKPO-UZEIT NE XEKET-UZEIT OR
         PEKKO-LPEIN NE XEKET-LPEIN.
        CLEAR PEKKO-EINDT.
        PEKKO-LPEIN = '*'.
      ENDIF.
    ENDIF.
    IF NOT XEKET-UZEIT IS INITIAL.
      CLEAR PEKKO-EINDT.
      PEKKO-LPEIN = '*'.
    ENDIF.
* Änderung der Menge berechnen ----------------------------------------*
    IF XLPET NE SPACE.
      XEKET-AMENG = XEKET-MENGE - XEKET-AMENG.
    ENDIF.
* nicht zu druckende
    PERFORM XEKET_BEREINIGEN.          "nicht zu druckende löschen
  ENDLOOP.
  IF H-IND NE 0.
    MODIFY XPEKPO INDEX H-IND.
  ELSE.
    APPEND XPEKPO.
  ENDIF.

  CLEAR XPEKPO-EBELP.
  IF XLPET NE SPACE AND XFZ NE SPACE.
    LOOP AT XEKET.
      PERFORM EKET_FZ.
*    perform eket_lieferplan.
    ENDLOOP.
  ENDIF.

* Wenn Lieferdatum nicht für alle Positionen gleich auch LPEIN clearen *
  IF PEKKO-LPEIN EQ '*'.
    CLEAR PEKKO-LPEIN.
  ELSE.
* Bei Mahnung Lieferdatum immer auf Positionsebene drucken ------------*
    IF XDRUVO EQ MAHN.
      CLEAR: PEKKO-LPEIN, PEKKO-EINDT.
    ENDIF.
  ENDIF.
* Bei Lieferplanänderung stört PEKKO-EINDT, falls es sitzt ------------*
  IF EKKO-BSTYP EQ BSTYP-LFPL AND ( XDRUVO EQ AEND OR       "67154
                                    XDRUVO EQ AUFB ).       "67154
    CLEAR: PEKKO-LPEIN, PEKKO-EINDT.                        "67154
  ENDIF.                                                    "67154
* Lieferdatum in Druckdarstellung -------------------------------------*
  CLEAR PEKKO-LFDAT.
  IF XLPET EQ SPACE AND
     PEKKO-EINDT NE 0.
    PERFORM AUFBEREITEN_LIEFERDATUM USING PEKKO-EINDT PEKKO-LPEIN
                                          PEKKO-LFDAT PEKKO-LPEIN
                                          PEKKO-PRITX.
* keine Lieferdatumsänderung auf Position ausgeben --------------------*
*(del) perform xaend_bereinigen using '01' 'S1-1'.            "#88301
  ENDIF.

  CLEAR PEKKO-NETWR.
  IF XDRUVO EQ NEU OR XDRUVO EQ AEND.
    PREISDRUCK = 'X'.
  ELSE.
    CLEAR PREISDRUCK.
  ENDIF.

  LOOP AT XEKPO.
* Feststellen, ob einheitliches Werk ----------------------------------*
    IF SY-TABIX EQ 1.
      MOVE XEKPO-WERKS TO PEKKO-WERKS.
    ELSE.
      IF PEKKO-WERKS NE XEKPO-WERKS.
        PEKKO-WERKS = '****'.
      ENDIF.
    ENDIF.
    IF XEKPO-ADRNR NE SPACE OR
       XEKPO-ADRN2 NE SPACE OR
       XEKPO-EMLIF NE SPACE OR
       XEKPO-KUNNR NE SPACE.
      PEKKO-WERKS = '****'.
    ENDIF.
* Feststellen, ob einheitliche Anlieferungsanschrift ------------------*
    IF SY-TABIX EQ 1.
      MOVE XEKPO-ADRNR TO PEKKO-ADRNR.
    ELSE.
      IF PEKKO-ADRNR NE XEKPO-ADRNR.
        PEKKO-ADRNR = '**********'.
      ENDIF.
    ENDIF.
* Feststellen, ob einheitliche Adressnummer ---------------------------*
    IF SY-TABIX EQ 1.
      MOVE XEKPO-ADRN2 TO PEKKO-ADRN2.
    ELSE.
      IF PEKKO-ADRN2 NE XEKPO-ADRN2.
        PEKKO-ADRN2 = '**********'.
      ENDIF.
    ENDIF.
* Feststellen, ob einheitliche Kundennummer ---------------------------*
    IF SY-TABIX EQ 1.
      MOVE XEKPO-KUNNR TO PEKKO-KUNNR.
    ELSE.
      IF PEKKO-KUNNR NE XEKPO-KUNNR.
        PEKKO-KUNNR = '**********'.
      ENDIF.
    ENDIF.
* Feststellen, ob einheitliche Lieferantennummer ----------------------*
    IF SY-TABIX EQ 1.
      MOVE XEKPO-EMLIF TO PEKKO-EMLIF.
    ELSE.
      IF PEKKO-EMLIF NE XEKPO-EMLIF.
        PEKKO-EMLIF = '**********'.
      ENDIF.
    ENDIF.
* Feststellen, ob einheitliche Auftragsbestätigung --------------------*
    IF SY-TABIX EQ 1.
      MOVE XEKPO-LABNR TO PEKKO-LABNR.
    ELSE.
      IF PEKKO-LABNR NE XEKPO-LABNR.
        PEKKO-LABNR = '****'.
      ENDIF.
    ENDIF.
* Feststellen, ob Auftragsbestätigungspflicht in jeder Position -------*
    IF SY-TABIX EQ 1.
      MOVE XEKPO-KZABS TO PEKKO-KZABS.
    ELSE.
      IF PEKKO-KZABS NE XEKPO-KZABS.
        PEKKO-KZABS = '*'.
      ENDIF.
    ENDIF.
* Feststellen, ob einheitliches Angebotsdatum -------------------------*
    IF EKKO-BSTYP EQ BSTYP-ANFR.
      IF XEKPO-AGDAT NE EKKO-ANGDT.
        CLEAR EKKO-ANGDT.
      ENDIF.
    ENDIF.

* Rest nur bei Positionen ohne Standardartikel
    CHECK XEKPO-STAPO EQ SPACE.
* Nettowert aufaddieren -----------------------------------------------*
    IF XEKPO-LOEKZ EQ SPACE.
      PEKKO-NETWR = PEKKO-NETWR + XEKPO-NETWR.
    ENDIF.
    IF XEKPO-PRSDR EQ SPACE.
      CLEAR PREISDRUCK.
    ELSE.
      IF EKKO-BSTYP EQ BSTYP-KONT OR EKKO-STAKO NE SPACE.
        KONTRAKT_PREIS = 'X'.
      ENDIF.
    ENDIF.
    IF EKKO-BSTYP EQ BSTYP-KONT OR EKKO-STAKO NE SPACE.
      IF XEKPO-LOEKZ NE 'L'.
        KOMK-SUPOS = KOMK-SUPOS + XEKPO-ZWERT.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Are incoterms the same for all items? -------------------------------*
  IF EKKO-INCO1 NE SPACE.
    PEKKO-INCO1 = EKKO-INCO1.
    PEKKO-INCO2 = EKKO-INCO2.
* Some item has deviating incoterms -> clear PEKKO-INCO1 and INCO2 ----*
    LOOP AT XEKPO.
      CHECK XEKPO-STAPO EQ SPACE.
      IF XEKPO-INCO1 NE SPACE.
        IF XEKPO-INCO1 NE PEKKO-INCO1 OR XEKPO-INCO2 NE PEKKO-INCO2.
          CLEAR PEKKO-INCO1.
          CLEAR PEKKO-INCO2.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
* Copy header incoterms to those items without incoterms --------------*
    IF PEKKO-INCO1 EQ SPACE.
      LOOP AT XEKPO.
        CHECK XEKPO-STAPO EQ SPACE.
        IF XEKPO-INCO1 EQ SPACE AND XEKPO-INCO2 EQ SPACE.
          XEKPO-INCO1 = EKKO-INCO1.
          XEKPO-INCO2 = EKKO-INCO2.
          MODIFY XEKPO.
        ENDIF.
      ENDLOOP.
      CLEAR EKKO-INCO1.
      CLEAR EKKO-INCO2.
    ENDIF.
  ENDIF.
* Check if all incoterms on the item level coincide -------------------*
  LOOP AT XEKPO.
    IF SY-TABIX EQ 1.
      PEKKO-INCO1 = XEKPO-INCO1.
      PEKKO-INCO2 = XEKPO-INCO2.
    ELSE.
      IF XEKPO-INCO1 NE PEKKO-INCO1 OR XEKPO-INCO2 NE PEKKO-INCO2.
        CLEAR PEKKO-INCO1.
        CLEAR PEKKO-INCO2.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF PEKKO-INCO1 NE SPACE.
    EKKO-INCO1 = PEKKO-INCO1.
    EKKO-INCO2 = PEKKO-INCO2.
    LOOP AT XEKPO.
      CLEAR XEKPO-INCO1.
      CLEAR XEKPO-INCO2.
      MODIFY XEKPO.
    ENDLOOP.
  ENDIF.

  IF EKKO-BSTYP EQ BSTYP-ANFR AND
     EKKO-ANGDT NE 0.
* erst mal die Kopfänderung rausschmeissen, kommt wieder rein ---------*
    LOOP AT XAEND WHERE CTXNR EQ 'K4'.
      DELETE XAEND.
    ENDLOOP.
* keine Angebotsfriständerung auf Position ausgeben -------------------*
    PERFORM XAEND_BEREINIGEN USING '05' 'K4'.
  ENDIF.

* Stammkonditionen Kontrakt ----------- (verlagert 4.6A )--------------*
* IF ( EKKO-BSTYP EQ BSTYP-KONT OR EKKO-STAKO NE SPACE ) AND
*    EKKO-BSTYP NE BSTYP-ANFR.
*   CALL FUNCTION 'ME_GET_CONTRACT_CONDITIONS'
*        EXPORTING
*             AUCH_POSITIONEN = KONTRAKT_PREIS
*             BELEG           = EKKO-EBELN
*             SCHEMA          = EKKO-KALSM
*             SPRACHE         = EKKO-SPRAS.
* ENDIF.

* Kein einheitliches Werk bei allen Positionen ------------------------*
  IF PEKKO-WERKS EQ '****'.
    CLEAR PEKKO-WERKS.
  ENDIF.

* Keine einheitliche Kundennummer bei allen Positionen ----------------*
  IF PEKKO-KUNNR EQ '**********'.
    CLEAR PEKKO-KUNNR.
  ENDIF.

* Keine einheitliche Lieferantennummer bei allen Positionen -----------*
  IF PEKKO-EMLIF EQ '**********'.
    CLEAR PEKKO-EMLIF.
  ENDIF.

* Keine einheitliche Adressnummer bei allen Positionen ----------------*
  IF PEKKO-ADRN2 EQ '**********'.
    CLEAR PEKKO-ADRN2.
  ENDIF.

* Keine einheitliche Anlieferungsanschrift bei allen Positionen -------*
  IF PEKKO-ADRNR EQ '**********'.
    CLEAR PEKKO-ADRNR.
  ENDIF.

* Keine einheitliche Auftragsbestaetigung bei allen Positionen --------*
  IF PEKKO-LABNR EQ '****'.
    CLEAR PEKKO-KZABS.
    CLEAR PEKKO-LABNR.
  ENDIF.

* Nicht in jeder Position Auftragsbestätigungspflicht -----------------*
  IF PEKKO-KZABS EQ '*'.
    CLEAR PEKKO-KZABS.
  ENDIF.

* Lesen Werksanschrift ------------------------------------------------*
  IF PEKKO-WERKS NE SPACE.
*   PERFORM WERKSANSCHRIFT USING PEKKO-WERKS.
    PERFORM GET_PLANT_ADDRESS
      USING    PEKKO-WERKS
      CHANGING PEKKO-ADRNR
               SADR.
  ENDIF.

* Lesen Kundenanschrift -----------------------------------------------*
  IF PEKKO-KUNNR NE SPACE.
*   PERFORM KUNDEN_ANSCHRIFT USING PEKKO-KUNNR.
    PERFORM GET_CUSTOMER_ADDRESS
      USING    PEKKO-KUNNR
      CHANGING PEKKO-ADRNR.
    PERFORM CHECK_ADDRESS_CHANGES.
  ENDIF.

* Lesen Anlieferungsanschrift -----------------------------------------*
  IF PEKKO-ADRNR NE SPACE.
*   PERFORM ANLIEF_ANSCHRIFT USING PEKKO-ADRNR.
    PERFORM CHECK_ADDRESS_CHANGES.
  ENDIF.

* Lesen Anschrift zur Adressnummer ------------------------------------*
  IF PEKKO-ADRN2 NE SPACE.
*   PERFORM LESEN_ADRESSNUMMER USING PEKKO-ADRN2.
    PEKKO-ADRNR = PEKKO-ADRN2.
    PERFORM CHECK_ADDRESS_CHANGES.
  ENDIF.

* Lesen Lieferantenanschrift ------------------------------------------*
  IF PEKKO-EMLIF NE SPACE.
*   PERFORM LESEN_LIEFERANT3 USING PEKKO-EMLIF.
    PERFORM GET_VENDOR_ADDRESS
      USING    PEKKO-EMLIF
      CHANGING PEKKO-ADRNR.
    PERFORM CHECK_ADDRESS_CHANGES.
  ENDIF.
  CLEAR AENDERNSRV.
* Positionstabelle bereinigen -----------------------------------------*
  PERFORM XEKPO_BEREINIGEN.
*-- Feststellen, ob Änderungen für Druck vorhanden sind ---------------*
  IF XDRUVO EQ AEND AND AENDERNSRV EQ SPACE.
    READ TABLE XAEND INDEX 1.
    IF SY-SUBRC NE 0.
      READ TABLE XAETX INDEX 1.
      IF SY-SUBRC NE 0.
        READ TABLE XADRNR INDEX 1.
        IF SY-SUBRC NE 0.
* Bestelländerung --> keine Änderungsbelege --> erfolgreich verarbeitet
          PERFORM PROTOCOL_UPDATE USING '140'
                         EKKO-EBELN SPACE SPACE SPACE.
          RETCO = 0.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* Wenn XEKPO leer, dann nicht Auftragsbestätigungstext bringen
  IF XEKPO[] IS INITIAL.
    CLEAR PEKKO-KZABS.
  ENDIF.

* Stammkonditionen Kontrakt -------------- (verlagert 4.6A) -----------*
  IF ( EKKO-BSTYP EQ BSTYP-KONT OR EKKO-STAKO NE SPACE ) AND
     EKKO-BSTYP NE BSTYP-ANFR.
    data: begin of lt_ebelp occurs 0.
            include structure ekpo_key.
    data: end of lt_Ebelp.
    if xdruvo eq aend.
      loop at xekpo.
        move-corresponding xekpo to lt_ebelp.
        append lt_ebelp.
      endloop.
    endif.
    CALL FUNCTION 'ME_GET_CONTRACT_CONDITIONS'
         EXPORTING
              AUCH_POSITIONEN = KONTRAKT_PREIS
              BELEG           = EKKO-EBELN
              SCHEMA          = EKKO-KALSM
              SPRACHE         = EKKO-SPRAS
         tables
              t_ebelp         = lt_ebelp.
  ENDIF.

  IF EKKO-LPHIS NE SPACE AND XDRUVO EQ LPHE.
* Lesen Historien -----------------------------------------------------*
    REFRESH: XEKEK,PEKEK,XEKEH,XEKPOABR.
    CLEAR: XEKEK,PEKEK,XEKEH,XEKPOABR.
* Schleife über alle Positionen des Belegs ----------------------------*
    LOOP AT XEKPO.
      H-IND = SY-TABIX.
*---- Lesen des aktuellen Lieferabrufkopfes der Position ------------*
      SELECT SINGLE * FROM EKEK WHERE EBELN = XEKPO-EBELN AND
                                      EBELP = XEKPO-EBELP AND
                                      ABART = '1'         AND
                                      ABRDT = '00000000'.        "neuPN
      IF SY-SUBRC EQ 0 AND EKEK-ABRDT IS INITIAL.
        MOVE-CORRESPONDING XEKPO TO XEKPOABR.
        APPEND XEKPOABR.
        MOVE-CORRESPONDING EKEK TO XEKEK.
        APPEND XEKEK.
*-------- Lesen der Abrufeinteilungen zum Lieferabrufkopf ---------*
        SELECT * FROM EKEH APPENDING TABLE XEKEH
                           WHERE EBELN = XEKEK-EBELN AND
                                 EBELP = XEKEK-EBELP AND
                                 ABART = XEKEK-ABART AND
                                 ABRUF = XEKEK-ABRUF.
        XEKPO-MENGE = XEKEK-ABEFZ.
        LOOP AT XEKEH WHERE EBELN = XEKEK-EBELN AND
                            EBELP = XEKEK-EBELP AND
                            ABART = XEKEK-ABART AND
                            ABRUF = XEKEK-ABRUF.
          XEKPO-MENGE = XEKPO-MENGE + XEKEH-MENGE - XEKEH-WEMNG.
        ENDLOOP.
        MODIFY XEKPO INDEX H-IND.      "und was für eine Mng?
*-------- Lesen alten Abruf wegen Datum alt -------------------------*
        XABRUF = XEKEK-ABRUF - 1.
        SELECT SINGLE * FROM EKEK WHERE EBELN = XEKPO-EBELN AND
                                        EBELP = XEKPO-EBELP AND
                                        ABART = '1'         AND
                                        ABRUF = XABRUF.
        IF SY-SUBRC EQ 0.
          MOVE-CORRESPONDING EKEK TO PEKEK.
          APPEND PEKEK.
        ENDIF.
      ELSE.
        DELETE XEKPO.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF EKKO-LPHIS NE SPACE AND XDRUVO EQ LPMA.
    REFRESH: XEKEH, XEKEK.
    LOOP AT XEKPO.
      CLEAR EKEK.
      REFRESH TEKEH.
      CLEAR HDATUM.
      CASE XEKPO-MAHNZ.
        WHEN 1.
          IF XEKPO-MAHN1 NE 0.
            HDATUM = SY-DATLO - XEKPO-MAHN1.
          ENDIF.
        WHEN 2.
          IF XEKPO-MAHN2 NE 0.
            HDATUM = SY-DATLO - XEKPO-MAHN2.
          ENDIF.
        WHEN 3.
          IF XEKPO-MAHN3 NE 0.
            HDATUM = SY-DATLO - XEKPO-MAHN3.
          ENDIF.
      ENDCASE.
      CALL FUNCTION 'ME_READ_LAST_RELEASE'
           EXPORTING
                I_EBELN    = XEKPO-EBELN
                I_EBELP    = XEKPO-EBELP
                I_FABKZ    = XEKPO-FABKZ
                I_WEKZ     = 'X'
           IMPORTING
                E_EKEK     = EKEK
           TABLES
                E_EKEH     = TEKEH
           EXCEPTIONS
                NO_RELEASE = 1
                OTHERS     = 2.
      IF SY-SUBRC <> 0.
        DELETE XEKPO.
        CONTINUE.
      ELSE.
        APPEND EKEK TO XEKEK.
        LOOP AT TEKEH WHERE EINDT LE HDATUM.
          APPEND TEKEH TO XEKEH.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
* Nicht drucken, wenn keine Positionen da sind ------------------------*
  IF XDRUVO EQ MAHN OR XDRUVO EQ LPMA OR XDRUVO EQ AUFB.
    READ TABLE XEKPO INDEX 1.
    IF SY-SUBRC NE 0.
      RETCO = 0.                       "erfolgreich verarbeitet
      EXIT.
    ELSE.
      IF HSTAP NE SPACE.
        LOOP AT XEKPO WHERE STAPO EQ SPACE.
          EXIT.
        ENDLOOP.
        IF SY-SUBRC NE 0.
* Nur noch Positionen mit Standardartikel vorhanden
          RETCO = 0.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF XDRUVO NE AEND.
    READ TABLE XEKPO INDEX 1.
    IF SY-SUBRC NE 0.
      PERFORM PROTOCOL_UPDATE USING '140' EKKO-EBELN SPACE SPACE SPACE.
      RETCO = 1.                       "fehlerhaft verarbeitet
      EXIT.
    ELSE.
      IF HSTAP NE SPACE.
        LOOP AT XEKPO WHERE STAPO EQ SPACE.
          EXIT.
        ENDLOOP.
        IF SY-SUBRC NE 0.
* Nur noch Positionen mit Standardartikel vorhanden
          PERFORM PROTOCOL_UPDATE USING
                                  '140' EKKO-EBELN SPACE SPACE SPACE.
          RETCO = 1.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM VARIANTEN_DATEN.

*----------------------------------------------------------------------*
* Ausgabe ansteuern
*----------------------------------------------------------------------*
* Ausgabe Kopf --------------------------------------------------------*
  PERFORM AUSGABE_KOPF.
  CHECK RETCO EQ 0.

  IF XLPET EQ SPACE.
* Ausgabe Positionsüberschrift ----------------------------------------*
    READ TABLE XEKPO INDEX 1.
    IF SY-SUBRC EQ 0.
      PERFORM AUSGABE_POS_UEB.
    ENDIF.
  ENDIF.

* Prüfen ob Kopfkonditionen vorhanden sind -----------------------
  CLEAR KOPFKOND.
  IF PREISDRUCK NE SPACE.    "beim Beleg nur, wenn alle Pos mit Prei
    IF EKKO-BSTYP NE BSTYP-KONT AND EKKO-STAKO EQ SPACE AND
       EKKO-BSTYP NE BSTYP-ANFR.
      KOMK-MANDT = EKKO-MANDT.
      IF EKKO-KALSM NE SPACE.
        KOMK-KALSM = EKKO-KALSM.
      ELSE.
        KOMK-KALSM = 'RM0000'.
      ENDIF.
      KOMK-KAPPL = 'M'.
      KOMK-WAERK = EKKO-WAERS.
      KOMK-KNUMV = EKKO-KNUMV.
      KOMK-BUKRS = EKKO-BUKRS.
      CALL FUNCTION 'RV_PRICE_PRINT_HEAD'
           EXPORTING
                COMM_HEAD_I = KOMK
                LANGUAGE    = EKKO-SPRAS
           IMPORTING
                COMM_HEAD_E = KOMK
           TABLES
                TKOMV       = TKOMV
                TKOMVD      = TKOMVD.
      DESCRIBE TABLE TKOMVD LINES SY-TFILL.
      IF SY-TFILL GE 1.
        KOPFKOND = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.


* Ausgabe Positionszeilen ---------------------------------------------*
  IF HSAMM EQ SPACE.                   "wie bisher
    LOOP AT XEKPO.
*    Füllen aus Historie
*     if not ekko-lphis is initial.
*       xekpokey-ebelp = xekpo-ebelp.
*       read table xekpoabr with key xekpokey.
*       if sy-subrc ne 0.
*         continue.
*       endif.
*     endif.
      IF EKKO-LPHIS NE SPACE AND XDRUVO EQ LPMA.
        READ TABLE XEKEK WITH KEY EBELP = XEKPO-EBELP.
      ELSE.
        READ TABLE XEKEK WITH KEY EBELP = XEKPO-EBELP
                                  ABRUF = XEKPO-DRUNR.
      ENDIF.
      MOVE XEKEK TO EKEK.
      PERFORM POS_AUSGEBEN.
    ENDLOOP.
  ELSE.                                "Beleg mit Unterpositionen
    Z = 0.
    LOOP AT XPEKPOV WHERE UPTYP EQ 1 OR UPTYP EQ UPTYP-LOT.
      LOOP AT TCONF_OUT WHERE EBELP EQ XPEKPOV-EBELP.
        Z = Z + 1.
      ENDLOOP.
      EXIT.
    ENDLOOP.
    IF M_FLAG EQ SPACE OR Z < 1.
*-- Varianten/Lot-bestellung ohne Matrix ausgeben
      MOVE-CORRESPONDING EKKO TO XEKPOKEY.
      LOOP AT XPEKPOV.
        MOVE XPEKPOV TO PEKPOV.
        IF XPEKPOV-EBELP NE SPACE.
          IF NOT XPEKPOV-UEBPO IS INITIAL AND
             NOT XPEKPOV-UEBPO EQ SEKPO-EBELP.
*          HAUPTPOSITION BEREITSTELLEN
            CLEAR SEKPO.
            MOVE XPEKPOV-UEBPO TO XEKPOKEY-EBELP.
            READ TABLE XEKPO WITH KEY XEKPOKEY.
            IF SY-SUBRC EQ 0.
              MOVE XEKPO TO SEKPO.
            ENDIF.
          ENDIF.
          MOVE XPEKPOV-EBELP TO XEKPOKEY-EBELP.
        ELSE.
          MOVE XPEKPOV-UEBPO TO XEKPOKEY-EBELP.
        ENDIF.
*    AUSZUGEBENDE POSITION BEREITSTELLEN
        READ TABLE XEKPO WITH KEY XEKPOKEY.
        PERFORM POS_AUSGEBEN.
      ENDLOOP.
    ELSE.
* Varianten/Lot-positionen mit Matrix ausgeben
*   auch für ein variantenbildendes Merkmal
      REFRESH KOND.
      MOVE-CORRESPONDING EKKO TO XEKPOKEY.
      LOOP AT XPEKPOV.
        MOVE XPEKPOV TO PEKPOV.
        IF XPEKPOV-EBELP NE SPACE.
          IF NOT XPEKPOV-UEBPO IS INITIAL AND
             NOT XPEKPOV-UEBPO EQ SEKPO-EBELP.
*          HAUPTPOSITION BEREITSTELLEN
            CLEAR SEKPO.
            MOVE XPEKPOV-UEBPO TO XEKPOKEY-EBELP.
            READ TABLE XEKPO WITH KEY XEKPOKEY.
            IF SY-SUBRC EQ 0.
              MOVE XEKPO TO SEKPO.
            ENDIF.
          ENDIF.
          MOVE XPEKPOV-EBELP TO XEKPOKEY-EBELP.
        ELSE.
          MOVE XPEKPOV-UEBPO TO XEKPOKEY-EBELP.
        ENDIF.
*    AUSZUGEBENDE POSITION BEREITSTELLEN
        READ TABLE XEKPO WITH KEY XEKPOKEY.
*    Ausgabe auf Hauptposition
        ON CHANGE OF XEKPO-UEBPO.
          IF XEKPO-UPTYP IS INITIAL.
            PERFORM VARIANTEN.
            PERFORM M_AUSGEBEN.
            PERFORM ABWEICHUNG.
            PERFORM AENDERUNGEN.
          ENDIF.
        ENDON.

        IF ( XEKPO-UPTYP NE UPTYP-VAR ) AND
           ( XEKPO-UPTYP NE UPTYP-LOT ).
          PERFORM POS_AUSGEBEN.
        ELSE.
          MOVE XEKPO TO EKPO.
          PERFORM SAMMEL.
        ENDIF.
      ENDLOOP.
*     Ausgabe auf die Unterposition
      IF ( XEKPO-UPTYP EQ UPTYP-VAR ) OR ( XEKPO-UPTYP EQ UPTYP-LOT ).
        PERFORM VARIANTEN.
        PERFORM M_AUSGEBEN.
        PERFORM ABWEICHUNG.
        PERFORM AENDERUNGEN.
      ENDIF.
    ENDIF.
  ENDIF.

* Kopf-Konditionen berücksichtigen ------------------------------------
IF PREISDRUCK NE SPACE.    "beim Beleg nur, wenn alle Pos mit Preisdruck
    IF EKKO-BSTYP NE BSTYP-KONT AND EKKO-STAKO EQ SPACE AND
       EKKO-BSTYP NE BSTYP-ANFR.
      KOMK-MANDT = EKKO-MANDT.
      IF EKKO-KALSM NE SPACE.
        KOMK-KALSM = EKKO-KALSM.
      ELSE.
        KOMK-KALSM = 'RM0000'.
      ENDIF.
      KOMK-KAPPL = 'M'.
      KOMK-WAERK = EKKO-WAERS.
      KOMK-KNUMV = EKKO-KNUMV.
      KOMK-BUKRS = EKKO-BUKRS.
      KOMK-LIFNR = EKKO-LIFNR.         "WDUK
      CALL FUNCTION 'RV_PRICE_PRINT_HEAD'
           EXPORTING
                COMM_HEAD_I = KOMK
                LANGUAGE    = EKKO-SPRAS
           IMPORTING
                COMM_HEAD_E = KOMK
           TABLES
                TKOMV       = TKOMV
                TKOMVD      = TKOMVD.
      IF ( XDRUVO NE AEND AND HLOEP NE SPACE ) OR HSTAP NE SPACE.
*  Neudruck und gelöschte Positionen
        CLEAR XEKPOKEY.
        LOOP AT TKOMV.
          IF TKOMV-KPOSN NE XEKPOKEY-EBELP.
            XEKPOKEY-MANDT = EKKO-MANDT.
            XEKPOKEY-EBELN = EKKO-EBELN.
            XEKPOKEY-EBELP = TKOMV-KPOSN.
            READ TABLE XEKPO WITH KEY XEKPOKEY BINARY SEARCH.
            IF SY-SUBRC NE 0.
              HKPOS = 'X'.
            ELSE.
              IF HSTAP EQ SPACE OR XEKPO-STAPO EQ SPACE.
                CLEAR HKPOS.
              ELSE.
                HKPOS ='X'.
              ENDIF.
            ENDIF.
          ENDIF.
          IF HKPOS NE SPACE.
*  Konditionen zu gelöschten Positionen löschen
            DELETE TKOMV.
          ENDIF.
        ENDLOOP.
*  Neu bewerten
        CALL FUNCTION 'RV_PRICE_PRINT_HEAD'
             EXPORTING
                  COMM_HEAD_I = KOMK
                  LANGUAGE    = EKKO-SPRAS
             IMPORTING
                  COMM_HEAD_E = KOMK
             TABLES
                  TKOMV       = TKOMV
                  TKOMVD      = TKOMVD.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ( EKKO-BSTYP EQ BSTYP-KONT OR EKKO-STAKO NE SPACE ) AND
     EKKO-BSTYP NE BSTYP-ANFR.
    REFRESH TEKOMD.
    EKPO-EBELP = '00000'.
    CALL FUNCTION 'ME_PRINT_CONTRACT_CONDITIONS'
         EXPORTING
              POSITION = EKPO-EBELP
         TABLES
              TEKOMD   = TEKOMD.
  ENDIF.

* Ausgabe Anhang und Gesamtsumme --------------------------------------
  PERFORM AUSGABE_ANHANG.

* Ende Formular -------------------------------------------------------*
  PERFORM ENDE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_ADDRESS_CHANGES
*&---------------------------------------------------------------------*
FORM CHECK_ADDRESS_CHANGES.

* keine Anschriftenänderung auf Position ausgeben ---------------------*
* Einträge suchen, die geändert sind, aber nicht neu hinzugefügt ------*
  LOOP AT XAEND WHERE ROUNR EQ '04' AND
                      INSERT EQ SPACE.
    H-IND = SY-TABIX.
* Prüfen, ob Eintrag 'neu' zusätzl. da für diese Position -------------*
    LOOP AT XAEND WHERE EBELP EQ XAEND-EBELP
                  AND   INSERT = 'X'.
      EXIT.
    ENDLOOP.
*- Wenn ja, Eintrag für Änderung aus XAEND löschen --------------------*
    IF SY-SUBRC EQ 0.
      DELETE XAEND INDEX H-IND.
      CHECK 1 EQ 2.
    ENDIF.
*- Wenn nicht, Änderung auf Kopfebene ausgeben ------------------------*
    CLEAR XAEND.
    XAEND-CTXNR = 'S5-1'.
    MODIFY XAEND INDEX H-IND.
    EXIT.
  ENDLOOP.
* Alle sonstigen Einträge mit dieser Änderung aus XAEND rausschmeissen *
  LOOP AT XAEND WHERE ROUNR EQ '04'
                AND   INSERT EQ SPACE.
    DELETE XAEND.
  ENDLOOP.

ENDFORM.                               " CHECK_ADDRESS_CHANGES
