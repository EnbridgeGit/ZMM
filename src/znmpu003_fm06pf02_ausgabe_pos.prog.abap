* 2000/07/27 mdemeest 4.6B All changes indicated by "UGL"            UGL
*                          Added code to calculate QTY OVERDUE,      UGL
*                          QTY OUTSTANDING, invoice to address       UGL
*-----------------------------------------------------------------------

*175075 24.09.1999 4.6B KH Druck Infobestelltext, Materialbestelltext
*----------------------------------------------------------------------*
* Ausgabe Positionen
*----------------------------------------------------------------------*
FORM AUSGABE_POS.

* Positionszeilen - In den Positionszeilen kein Seitenumbruch ---------*
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'PROTECT'
       EXCEPTIONS
            OTHERS  = 01.
  CLEAR SY-SUBRC.

*F EKPO-STAPO NE SPACE.
*  CALL FUNCTION 'WRITE_FORM'
*       EXPORTING ELEMENT = 'ITEM_HEADER_SATNR_1'
*       EXCEPTIONS OTHERS = 01.
*NDIF.
  IF EKPO-UEBPO NE SPACE.
    IF SEKPO-EBELP EQ EKPO-UEBPO AND SEKPO-FIRST_VARPOS NE EKPO-UPTYP.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_HEADER_VATNR_1'
           EXCEPTIONS
                OTHERS  = 01.
      SEKPO-FIRST_VARPOS = EKPO-UPTYP.
    ENDIF.
  ENDIF.

* Positionszeilen Anfrage ---------------------------------------------*
  IF EKKO-BSTYP EQ BSTYP-ANFR.
    PERFORM MENGE_AUSGEBEN USING EKPO-KTMNG T006-ANDEC RM06P-PRMG1.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_LINE_A'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ELSE.
* positionieren für Matrixdruck ---------------------------------------*
    IF EKPO-UPTYP EQ UPTYP-VAR or ekpo-uptyp eq uptyp-lot and
                                  M_FLAG NE SPACE AND Z >= 1.
      LOOP AT XEKPO WHERE EBELP EQ EKPO-UEBPO.
        MOVE XEKPO TO EKPO.
      ENDLOOP.

      LOOP AT XPEKPOV WHERE UEBPO EQ EKPO-EBELP AND EBELP IS INITIAL.
        MOVE XPEKPOV TO PEKPOV.
      ENDLOOP.

    ENDIF.

* Erste Positionszeile bei Bestellung und Rahmenvertrag ---------------*
    IF EKPO-UPTYP NE UPTYP-VAR.
* ------------------------- UGL Change  ---------------------------  UGL
      if tnapr-kschl = 'LPMA'.                                      "UGL
          "processing scheduling doc reminder                        UGL
          "calculate QUANTITY OVERDUE                                UGL
          eket-dabmg = rm06p-prmg2 - eket-wemng.                    "UGL
      else.                                                         "UGL
          "calculate QUANTITY OUTSTANDING                            UGL
          eket-dabmg = ekpo-menge - eket-wemng.                     "UGL
      endif.                                                        "UGL
      select single * from eket where ebeln = ekpo-ebeln            "UGL
                                  and ebelp = ekpo-ebelp            "UGL
                                  and etenr = '0001'.               "UGL
                                                                    "UGL
      "find out invoice to address                                   UGL
      select single * from t001w where werks = 'P112'.              "UGL
      call function 'WRITE_FORM'                                    "UGL
           exporting element = 'ADDRESS'                            "UGL
                     window  = 'INVTO'                              "UGL
           exceptions others = 01.                                  "UGL
* ----------------------- End of UGL Change -----------------------  UGL

      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_LINE_1'
           EXCEPTIONS
                OTHERS  = 01.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_LINE_1_VAR'
           EXCEPTIONS
                OTHERS  = 01.
      CLEAR SY-SUBRC.
      CLEAR ECONF_OUT.
      LOOP AT TCONF_OUT WHERE EBELP EQ EKPO-EBELP.
        MOVE TCONF_OUT TO ECONF_OUT.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_LINE_1_VAR_CONF_OUT'
             EXCEPTIONS
                  OTHERS  = 01.
        CLEAR SY-SUBRC.
      ENDLOOP.
    ENDIF.
    CLEAR SY-SUBRC.
* Positionszeile ohne Preis -------------------------------------------*
    IF EKPO-MEINS NE SPACE.
      IF EKPO-NETPR EQ 0 OR
         EKPO-PRSDR EQ SPACE OR
         EKPO-BPRME NE EKPO-MEINS.
        IF EKKO-BSTYP EQ BSTYP-BEST.
          PERFORM MENGE_AUSGEBEN USING EKPO-MENGE T006-ANDEC
                                       RM06P-PRMG1.
* Zusatz für Bestätigungen
          CLEAR RM06P-PRMG2.
          IF XDRUVO EQ AUFB AND XEKPO-BSMNG LT EKPO-MENGE
                            AND XEKPO-BSMNG GT 0.
            PERFORM MENGE_AUSGEBEN USING XEKPO-BSMNG *T006-ANDEC
                                      rm06p-prmg2.
          ENDIF.
* Ende Zusatz

          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'ITEM_LINE_2F'
               EXCEPTIONS
                    OTHERS  = 01.
          CLEAR SY-SUBRC.
        ELSE.
          PERFORM MENGE_AUSGEBEN USING EKPO-KTMNG T006-ANDEC
                                       RM06P-PRMG1.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'ITEM_LINE_2R'
               EXCEPTIONS
                    OTHERS  = 01.
          CLEAR SY-SUBRC.
        ENDIF.
      ENDIF.
    ENDIF.
* Positionszeile mit Preis --------------------------------------------*
    IF EKPO-NETPR NE 0 AND
       EKPO-PRSDR NE SPACE.
* Menge in Bestellpreismengeneinheit ----------------------------------*
      IF EKPO-BPRME NE EKPO-MEINS.
        IF EKKO-BSTYP EQ BSTYP-BEST.
          EKPO-MENGE = F1 = EKPO-MENGE * EKPO-BPUMZ / EKPO-BPUMN.
        ELSE.
          EKPO-KTMNG = F1 = EKPO-KTMNG * EKPO-BPUMZ / EKPO-BPUMN.
        ENDIF.
      ENDIF.
      IF EKKO-BSTYP EQ BSTYP-BEST.
        PERFORM MENGE_AUSGEBEN USING EKPO-MENGE *T006-ANDEC
                                     RM06P-PRMG1.
* Zusatz für Bestätigungen
        CLEAR RM06P-PRMG2.
        IF XDRUVO EQ AUFB AND XEKPO-BSMNG LT EKPO-MENGE
                          AND XEKPO-BSMNG GT 0.
          PERFORM MENGE_AUSGEBEN USING XEKPO-BSMNG *T006-ANDEC
                                    RM06P-PRMG2.
        ENDIF.
* Ende Zusatz
        READ TABLE TKOMVD INDEX 1.
        IF SY-SUBRC EQ 0 OR
           ( EKPO-UEBPO NE SPACE AND PEKPOV-XKONDV EQ SPACE ) OR
           EKPO-UPTYP EQ UPTYP-LOT.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'ITEM_LINE_3F'
               EXCEPTIONS
                    OTHERS  = 01.
          CLEAR SY-SUBRC.
        ELSE.
          IF ( EKPO-ATTYP NE ATTYP-SAM AND EKPO-ATTYP NE ATTYP-LOT )
             OR PEKPOV-MENGE EQ 0 OR
             EKPO-UPVOR EQ SPACE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_LINE_3F_PRICE'
                 EXCEPTIONS
                      OTHERS  = 01.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_LINE_3F'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
* Menge in Bestellpreismengeneinheit ----------------------------------*
            if ekpo-bprme ne ekpo-meins.                "INS  "TK 4.6B
* pekpov-menge in bprme, da ekpo-menge auch in bprme    "INS  "TK 4.6B
              pekpov-menge = f1 =                       "INS  "TK 4.6B
               pekpov-menge * ekpo-bpumz / ekpo-bpumn.  "INS  "TK 4.6B
            endif.                                      "INS  "TK 4.6B
            PERFORM MENGE_AUSGEBEN USING
                    PEKPOV-MENGE T006-ANDEC PEKPOV-PRMNG.
            RM06P-PRMG1 = PEKPOV-PRMNG.
            EKPO-NETWR = EKPO-NETWR * PEKPOV-MENGE / EKPO-MENGE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_LINE_3F_PRICE'
                 EXCEPTIONS
                      OTHERS  = 01.
          ENDIF.
          CLEAR SY-SUBRC.
        ENDIF.
      ELSE.
        PERFORM MENGE_AUSGEBEN USING EKPO-KTMNG *T006-ANDEC
                                     RM06P-PRMG1.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_LINE_3R'
             EXCEPTIONS
                  OTHERS  = 01.
        CLEAR SY-SUBRC.
      ENDIF.
    ENDIF.
  ENDIF.

* Ausgeben Lieferdatum pro Position -----------------------------------*
  IF PEKPO-EINDT NE 0 AND
     EKKO-BSTYP NE BSTYP-ANFR AND
     EKKO-BSTYP NE BSTYP-LFPL.
    IF HDATUM NE 0.                    "#88301
      IF XLMAHN NE SPACE and ekpo-upvor is initial. "#182583
        PEKPO-WEMNG = XEKPO-MENGE - PEKPO-WEMNG.
        PERFORM MENGE_AUSGEBEN USING PEKPO-WEMNG T006-ANDEC RM06P-PRMG2.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_INFO_WEMNG'
             EXCEPTIONS
                  OTHERS  = 01.
        CLEAR SY-SUBRC.
      ENDIF.                           "#88301
    ENDIF.
    PERFORM SET_TIMEFLAG CHANGING RM06P-PHTX2.
    EKET-UZEIT = XEKET-UZEIT.
    PERFORM TIME_INTO_PRINTFORM.
    PERFORM MAHNTEXT USING XPEKPO-MAHNZ.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_INFO_DATE'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Lieferdatum zusammen mit den Positionszeilen drucken-----------------*
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ENDPROTECT'
       EXCEPTIONS
            OTHERS  = 01.
  CLEAR SY-SUBRC.

* MPN-Abwicklung
  IF NOT HTNMAT[] IS INITIAL OR NOT HTNAMP[] IS INITIAL.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_MPN_LISTHEADER'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.

*... externe Herstellernummer besorgen ...............................*
    DATA T_HERST LIKE LFA1_KEY OCCURS 0 WITH HEADER LINE.
    DATA T_LFA1  LIKE LFA1 OCCURS 0 WITH HEADER LINE.
    DATA: BEGIN OF T_HTNM OCCURS 0.
            INCLUDE STRUCTURE V_HTNM.
    DATA:    REVLV LIKE RAMPL-REVLV,
          END OF T_HTNM.
    REFRESH: T_HERST, T_HTNM.
    IF NOT HTNAMP[] IS INITIAL.
      LOOP AT HTNAMP.
        MOVE-CORRESPONDING HTNAMP TO T_HTNM.
*... Nun steht in t_htnm-mfrnr das Herstellerwerk aus der LZHT .......*
*
*... Hersteller besorgen .............................................*
        CALL FUNCTION 'MARA_SINGLE_READ'
             EXPORTING
                  MATNR  = HTNAMP-MATNR
             IMPORTING
                  WMARA  = *MARA
             EXCEPTIONS
                  OTHERS = 5.
        T_HERST-LIFNR = *MARA-MFRNR.
        APPEND T_HERST.
*
*... zunächst externen Hersteller auf internen Hersteller setzen .....*
        T_HTNM-EMNFR = *MARA-MFRNR.
        T_HTNM-REVLV = HTNAMP-REVLV.
        APPEND T_HTNM.
      ENDLOOP.
    ELSEIF NOT HTNMAT[] IS INITIAL AND
       EKPO-MFRPN IS INITIAL.          "Kein HTN in der Bestellung
      LOOP AT HTNMAT.                  "xxxxxx
        T_HERST-LIFNR = HTNMAT-MFRNR.  "xxxxxx
        APPEND T_HERST.                "xxxxxx
      ENDLOOP.                         "xxxxxx
    ENDIF.
    IF NOT T_HERST[] IS INITIAL.
      CALL FUNCTION 'WY_LFA1_ARRAY_READ'
*       EXPORTING
*            REFRESH_BUFFER           =
           TABLES
                PTI_LFA1_KEYTAB          = T_HERST
                PTO_LFA1                 = T_LFA1
           EXCEPTIONS
                ERR_NO_RECORDS_REQUESTED = 1
                ERR_NO_RECORDS_FOUND     = 2
                OTHERS                   = 3.
    ENDIF.
    IF NOT HTNAMP[] IS INITIAL.
*... LZHT-Verwaltung ist aktiv .......................................*
      LOOP AT T_HTNM.
        MOVE-CORRESPONDING T_HTNM TO V_HTNM.
*... Nun steht in v_htnm-mfrnr das Herstellerwerk aus der LZHT .......*
*... Revisionslevel setzen ...........................................*
        RAMPL-REVLV = T_HTNM-REVLV.
*
*... externe Herstellernummer setzen .................................*
        READ TABLE T_LFA1 WITH KEY LIFNR = T_HTNM-EMNFR.
        IF SY-SUBRC EQ 0 AND
           NOT T_LFA1-EMNFR IS INITIAL.
          V_HTNM-EMNFR = T_LFA1-EMNFR.
        ELSE.
          V_HTNM-EMNFR = T_HTNM-EMNFR.
        ENDIF.

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_MPN_LISTLINE'
             EXCEPTIONS
                  OTHERS  = 01.
      ENDLOOP.
    ELSE.
*... LZHT-Verwaltung ist NICHT aktiv .................................*
      LOOP AT HTNMAT.
        MOVE-CORRESPONDING HTNMAT TO V_HTNM.
        IF V_HTNM-EMNFR IS INITIAL AND
           EKPO-MFRPN IS INITIAL.      "Nur falls keine HTn in Best.
*... externe Herstellernummer setzen .................................*
          READ TABLE T_LFA1 WITH KEY LIFNR = HTNMAT-MFRNR.     "xxxxxx
          IF SY-SUBRC EQ 0 AND
            NOT T_LFA1-EMNFR IS INITIAL.
            V_HTNM-EMNFR = T_LFA1-EMNFR.
          ELSE.
            V_HTNM-EMNFR = HTNMAT-MFRNR.                     "xxxxxx
          ENDIF.
        ENDIF.
        IF NOT EKPO-REVLV IS INITIAL.
*... Revisionslevel aus der Bestellung setzen ........................*
          RAMPL-REVLV = EKPO-REVLV.
        ELSE.
          RAMPL-REVLV = T_HTNM-REVLV.
        ENDIF.

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_MPN_LISTLINE'
             EXCEPTIONS
                  OTHERS  = 01.
      ENDLOOP.
    ENDIF.
  ENDIF.

*F NOT EKPO-CUOBJ IS INITIAL.            "Retail immer vorläufig
  CLEAR ECONF_OUT.
  LOOP AT TCONF_OUT WHERE EBELP EQ EKPO-EBELP.
    MOVE TCONF_OUT TO ECONF_OUT.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_INFO_CONF_OUT'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDLOOP.
*NDIF.

* Ausgeben Positionsinfo-Fenster -------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'ITEM_INFO_2'
       EXCEPTIONS
            OTHERS  = 01.
  CLEAR SY-SUBRC.

  IF NOT EKVKP IS INITIAL.
* Ausgeben Fenster mit VKP-Informationen (IS-R)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_INFO_VKP'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDIF.

  IF EKPO-UPTYP NE UPTYP-LOT.
* Positionstexte ausgeben --------------------------------------------*
* bei Änderungsdruck nur, wenn Texte auch geändert wurden ------------*
    IF XDRUVO EQ AEND OR XDRUVO EQ LPAE.                    "175075
      READ TABLE XAETX WITH KEY EBELP = EKPO-EBELP.         "175075
    ENDIF.                                                  "175075
    LOOP AT XT166P.
      IF SY-SUBRC NE 0.                                     "175075
        CLEAR SY-SUBRC.                                     "175075
        EXIT.                                               "175075
      ENDIF.                                                "175075
      MOVE XT166P TO T166P.
      IF T166P-TDOBJECT EQ 'EKPO'.
        T166P-TXNAM(10) = EKKO-EBELN.
        T166P-TXNAM+10(5) = EKPO-EBELP.
      ENDIF.
     PERFORM LESEN_TTXIT USING XT166P-TITDR XT166P-TDOBJECT XT166P-TDID.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_TEXT'
           EXCEPTIONS
                OTHERS  = 01.
      CLEAR SY-SUBRC.
    ENDLOOP.

* QM-Texte       ausgeben --------------------------------------------*
    LOOP AT QM_TEXT_KEY.
      THEAD-TDSPRAS  = EKKO-SPRAS.
      THEAD-TDNAME   = QM_TEXT_KEY-TDNAME.
      THEAD-TDOBJECT = QM_TEXT_KEY-TDOBJECT.
      THEAD-TDID     = QM_TEXT_KEY-TDID.
      TTXIT-TDTEXT   = QM_TEXT_KEY-TDTEXT.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'QM_TEXT'
           EXCEPTIONS
                OTHERS  = 01.
      CLEAR SY-SUBRC.
    ENDLOOP.

* Ausgeben Hinweis, daß zu jeder Lieferung ein Zeugnis erwartet wird--*
    IF EKPO-ZGTYP NE SPACE AND ZG_KZ NE SPACE AND
       ( XDRUVO EQ NEU OR
         XDRUVO EQ AEND ).
*  Interpretieren Zeugnistyp
      SELECT SINGLE * FROM TQ05 WHERE ZGTYP EQ EKPO-ZGTYP.
      IF SY-SUBRC EQ 0.
        SELECT SINGLE * FROM TQ05T WHERE SPRACHE EQ EKKO-SPRAS
                                   AND   ZGTYP   EQ EKPO-ZGTYP.
        IF TQ05-ZGLIEF NE SPACE.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'ITEM_INFO_QM_1'
               EXCEPTIONS
                    OTHERS  = 01.
        ELSE.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'ITEM_INFO_QM_2'
               EXCEPTIONS
                    OTHERS  = 01.
        ENDIF.
      ENDIF.
      CLEAR SY-SUBRC.
    ENDIF.

* Ausgeben Mehrere Liefertermine -------------------------------------*
    H_SUBRC = 0.                                            "J4S
    IF EKKO-BSTYP = BSTYP-KONT.                             "J4S
      CALL FUNCTION 'ME4S_CONTRACT_TYPE_WITH_SCHED'         "J4S
           EXPORTING                                        "J4S
                KTYPE  = EKKO-BSART                         "J4S
           IMPORTING                                        "J4S
                RETURN = H_SUBRC.                           "J4S
    ENDIF.                                                  "J4S
    IF H_SUBRC = 0 AND                                      "J4S
*(del)if ekko-bstyp ne bstyp-kont and                               "J4S
             EKKO-BSTYP NE BSTYP-LFPL AND
             PEKKO-EINDT EQ 0.
      READ TABLE XEKET WITH KEY EBELN = EKPO-EBELN
                                EBELP = EKPO-EBELP.
      IF PEKPO-EINDT EQ 0 AND EKPO-PSTYP NE PSTYP-TEXT
                          AND SY-SUBRC EQ 0.
        IF XDRUVO EQ NEU OR
           EKKO-BSTYP EQ BSTYP-ANFR.
          ELEMENTN = 'ITEM_SCHEDULE_HEADER_NEW'.
        ELSE.
          ELEMENTN = 'ITEM_SCHEDULE_HEADER'.
        ENDIF.
* Muß Text 'Uhrzeit' ausgegeben werden? -------------------------------*
        PERFORM SET_TIMEFLAG CHANGING RM06P-PHTXT.
* kein Seitenumbruch bei Einteilungen ---------------------------------*
        XPROTECT = 'X'.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'PROTECT'
             EXCEPTIONS
                  OTHERS  = 01.
        CLEAR SY-SUBRC.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = ELEMENTN
             EXCEPTIONS
                  OTHERS  = 01.
        CLEAR SY-SUBRC.

        SORT XEKET BY EBELP EINDT ETENR.
        LOOP AT XEKET WHERE EBELN EQ EKPO-EBELN AND
                            EBELP EQ EKPO-EBELP.
          MOVE XEKET TO EKET.
         PERFORM MENGE_AUSGEBEN USING EKET-MENGE T006-ANDEC RM06P-PRMG1.
* Mahntext ------------------------------------------------------------*
          PERFORM MAHNTEXT USING EKET-MAHNZ.
          PERFORM TIME_INTO_PRINTFORM.
          IF TIMEFLAG NE SPACE.
            SHIFT RM06P-PHTXT RIGHT BY 7 PLACES.
            RM06P-PHTXT(5) = PEKPO-TPRIN.
          ENDIF.
* Lieferdatum in Druckdarstellung -------------------------------------*
          CLEAR: PEKPO-LPEIN, RM06P-LFDAT, RM06P-PRITX.
          PERFORM AUFBEREITEN_LIEFERDATUM USING EKET-EINDT EKET-LPEIN
                                              RM06P-LFDAT PEKPO-LPEIN
                                                RM06P-PRITX.
* Lieferdatum in Wochen- oder Monatsdarstellung -----------------------*
          IF RM06P-PRITX NE SPACE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_SCHEDULE'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
* Lieferdatum tagesgenau ----------------------------------------------*
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_SCHEDULE_DAY'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          ENDIF.
* Seitenumbruch nach 1. Einteilung möglich ----------------------------*
          IF XPROTECT NE SPACE.
            CLEAR XPROTECT.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ENDPROTECT'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
* Überschrift Einteilungen - Folgeseiten -----------------------------*
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT  = ELEMENTN
                      TYPE     = 'TOP'
                      FUNCTION = 'APPEND'
                 EXCEPTIONS
                      OTHERS   = 01.
            CLEAR SY-SUBRC.
          ENDIF.

        ENDLOOP.
* Überschrift Einteilungen - Folgeseiten löschen ---------------------*
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT  = ELEMENTN
                  FUNCTION = 'DELETE'
                  TYPE     = 'TOP'
             EXCEPTIONS
                  OTHERS   = 01.
        CLEAR SY-SUBRC.
      ENDIF.
    ENDIF.

* Ausgeben Komponenten -----------------------------------------------*
    IF EKKO-BSTYP EQ BSTYP-BEST AND
       EKPO-PSTYP EQ PSTYP-LOHN.
      PERFORM AUSGABE_COMP.
    ENDIF.

* Ausgeben Konditionen -----------------------------------------------*
    IF EKKO-BSTYP NE BSTYP-KONT AND EKKO-STAKO EQ SPACE.
      IF ( EKPO-ATTYP EQ ATTYP-SAM OR EKPO-ATTYP EQ ATTYP-LOT ) AND
         M_FLAG NE SPACE AND Z >= 1 AND EKPO-UPVOR NE SPACE.
*       loop at xekpo where uebpo eq ekpo-ebelp and ebelp ne space.
        LOOP AT XEKPO WHERE EBELP EQ EKPO-EBELP AND ATTYP EQ ATTYP-SAM.
          LOOP AT TKOMVD.
            IF SY-TABIX EQ 1.
              IF EKPO-UPTYP EQ UPTYP-VAR.
                CALL FUNCTION 'WRITE_FORM'
                     EXPORTING
                          ELEMENT = 'ITEM_INFO_VAR_COND'
                     EXCEPTIONS
                          OTHERS  = 01.
              ELSE.
                IF EKPO-ATTYP EQ ATTYP-SAM AND EKPO-UPVOR NE SPACE.
                  CALL FUNCTION 'WRITE_FORM'
                       EXPORTING
                            ELEMENT = 'ITEM_INFO_SATNR_COND'
                       EXCEPTIONS
                            OTHERS  = 01.
                ENDIF.
              ENDIF.
              CLEAR SY-SUBRC.
            ENDIF.
            KOMVD = TKOMVD.
            IF KOMVD-KPEIN IS INITIAL.
              CALL FUNCTION 'WRITE_FORM'
                   EXPORTING
                        ELEMENT = 'ITEM_CONDITIONS'
                   EXCEPTIONS
                        OTHERS  = 01.
              CLEAR SY-SUBRC.
            ELSE.
              CALL FUNCTION 'WRITE_FORM'
                   EXPORTING
                        ELEMENT = 'ITEM_CONDITIONS_UNIT'
                   EXCEPTIONS
                        OTHERS  = 01.
              CLEAR SY-SUBRC.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ELSE.
*              move xekpo to ekpo.
        LOOP AT TKOMVD.
          IF SY-TABIX EQ 1.
            IF EKPO-UPTYP EQ UPTYP-VAR.
              CALL FUNCTION 'WRITE_FORM'
                   EXPORTING
                        ELEMENT = 'ITEM_INFO_VAR_COND'
                   EXCEPTIONS
                        OTHERS  = 01.
            ELSE.
              IF EKPO-ATTYP EQ ATTYP-SAM AND EKPO-UPVOR NE SPACE.
                CALL FUNCTION 'WRITE_FORM'
                     EXPORTING
                          ELEMENT = 'ITEM_INFO_SATNR_COND'
                     EXCEPTIONS
                          OTHERS  = 01.
              ENDIF.
            ENDIF.
            CLEAR SY-SUBRC.
          ENDIF.
          KOMVD = TKOMVD.
          IF KOMVD-KPEIN IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_CONDITIONS'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_CONDITIONS_UNIT'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      PERFORM AUSGABE_STAMMKONDITIONEN.
    ENDIF.

* Ausgeben Auftragsbestätigungspflicht, wenn gesetzt -----------------*
    IF EKPO-KZABS NE SPACE AND
       EKPO-LABNR EQ SPACE AND
       PEKKO-KZABS EQ SPACE AND
       XDRUVO NE AUFB.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_INFO_ACKNOW'
           EXCEPTIONS
                OTHERS  = 01.
      CLEAR SY-SUBRC.
    ENDIF.

* Ausgeben Werksanschrift pro Position -------------------------------*
*   IF PEKKO-WERKS EQ SPACE AND                                 "126150
*      EKPO-WERKS NE SPACE.                                     "126150
*     IF EKPO-KUNNR EQ SPACE AND                                "126150
*        EKPO-EMLIF EQ SPACE AND                                "126150
*        EKPO-ADRN2 EQ SPACE AND                                "126150
*        EKPO-ADRNR EQ SPACE.                                   "126150
*       CALL FUNCTION 'WRITE_FORM'                              "126150
*            EXPORTING                                          "126150
*                 ELEMENT = 'ITEM_DELADDRESS'                   "126150
*            EXCEPTIONS                                         "126150
*                 OTHERS  = 01.                                 "126150
*       CLEAR SY-SUBRC.                                         "126150
*     ENDIF.                                                    "126150
*   ENDIF.                                                      "126150
*                                                               "126150
* Ausgeben Kundenanschrift pro Position ------------------------------*
*   IF PEKKO-KUNNR EQ SPACE AND                                 "126150
*      EKPO-KUNNR NE SPACE.                                     "126150
*     CALL FUNCTION 'WRITE_FORM'                                "126150
*          EXPORTING                                            "126150
*               ELEMENT = 'ITEM_DELADDRESS'                     "126150
*          EXCEPTIONS                                           "126150
*               OTHERS  = 01.                                   "126150
*     CLEAR SY-SUBRC.                                           "126150
*   ENDIF.                                                      "126150
*                                                               "126150
* Ausgeben Anschrift zur Adressnummer pro Position --------------------*
*   IF PEKKO-ADRN2 EQ SPACE AND                                 "126150
*      EKPO-ADRN2 NE SPACE.                                     "126150
*     CALL FUNCTION 'WRITE_FORM'                                "126150
*          EXPORTING                                            "126150
*               ELEMENT = 'ITEM_DELADDRESS'                     "126150
*          EXCEPTIONS                                           "126150
*               OTHERS  = 01.                                   "126150
*     CLEAR SY-SUBRC.                                           "126150
*   ENDIF.                                                      "126150
*                                                               "126150
* Ausgeben Lieferantenanschrift pro Position --------------------------*
*   IF PEKKO-EMLIF EQ SPACE AND                                 "126150
*      EKPO-EMLIF NE SPACE.                                     "126150
*     CALL FUNCTION 'WRITE_FORM'                                "126150
*          EXPORTING                                            "126150
*               ELEMENT = 'ITEM_DELADDRESS'                     "126150
*          EXCEPTIONS                                           "126150
*               OTHERS  = 01.                                   "126150
*     CLEAR SY-SUBRC.                                           "126150
*   ENDIF.                                                      "126150
*                                                               "126150
* Ausgeben manuelle Anschrift pro Position ---------------------------*
*   IF PEKKO-ADRNR EQ SPACE AND                                 "126150
*      EKPO-ADRNR NE SPACE.                                     "126150
*     CALL FUNCTION 'WRITE_FORM'                                "126150
*          EXPORTING                                            "126150
*               ELEMENT = 'ITEM_DELADDRESS'                     "126150
*          EXCEPTIONS                                           "126150
*               OTHERS  = 01.                                   "126150
*     CLEAR SY-SUBRC.                                           "126150
*   ENDIF.                                                      "126150
                                                                "126150
* Print delivery address per line item                          "126150
    IF PEKKO-ADRNR IS INITIAL AND                               "126150
       PEKKO-ADRN2 IS INITIAL AND                               "126150
       PEKKO-EMLIF IS INITIAL AND                               "126150
       PEKKO-KUNNR IS INITIAL AND                               "126150
       PEKKO-WERKS IS INITIAL.                                  "126150
      CALL FUNCTION 'WRITE_FORM'                                "126150
           EXPORTING                                            "126150
                ELEMENT = 'ITEM_DELADDRESS'                     "126150
           EXCEPTIONS                                           "126150
                OTHERS  = 01.                                   "126150
      CLEAR SY-SUBRC.                                           "126150
    ENDIF.                                                      "126150

  ENDIF.                               "UPTYP-LOT

* Änderungshinweise ausgeben ------------------------------------------*
  PERFORM ERGAENZEN_XAEND.
  LOOP AT XAEND WHERE EBELP EQ EKPO-EBELP.
    SELECT SINGLE * FROM T166T WHERE SPRAS = EKKO-SPRAS
                                 AND CTXNR = XAEND-CTXNR.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'CHANGE_REMARKS'
           EXCEPTIONS
                OTHERS  = 01.
      CLEAR SY-SUBRC.
    ENDIF.
  ENDLOOP.

* Dienstleistungspaket ausgeben
  IF EKPO-PSTYP EQ PSTYP-DIEN OR
    ( EKPO-PSTYP EQ PSTYP-LOHN AND NOT EKPO-PACKNO IS INITIAL ).
    PERFORM SELECT_SERVICES.
    CALL FUNCTION 'PLAN_READ_FOR_PURCHASE_ORDER'
         EXPORTING
              EBELN               = EKKO-EBELN
              EBELP               = EKPO-EBELP
         TABLES
              MPOS_TAB            = MPOS_TAB
              ZYKL_TAB            = ZYKL_TAB
         EXCEPTIONS
              NO_MAINTENANCE_ITEM = 1
              OTHERS              = 2.
    IF SY-SUBRC EQ 0.
      PERFORM PRINT_MAINTANCE_SCHEDULE.
    ENDIF.
  ENDIF.

* Ausgabe des Rechnungsplans
  IF NOT EKPO-FPLNR IS INITIAL.
    PERFORM SELECT_INVOICING_SCHEDULE.
    PERFORM PRINT_INVOICING_SCHEDULE.
  ENDIF.

* Ausgabe zugeordneter Dokumente
  DATA XFLAG.
  LOOP AT DOKTAB.
    IF DOKTAB-OBJKY+10(5) = EKPO-EBELP.
      IF XFLAG IS INITIAL.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_DVS_HEADER'
             EXCEPTIONS
                  OTHERS  = 01.
        XFLAG = 'X'.
      ENDIF.
      MOVE-CORRESPONDING DOKTAB TO DRAD.
      DRAT-DKTXT = DOKTAB-DKTXT.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_DVS_DOCUMENT'
           EXCEPTIONS
                OTHERS  = 01.
    ENDIF.
  ENDLOOP.

*  Ausgabe von Additionals (Verkaufshilfsmitteln).
*  Die Dokumente müssen durch den Kunden angepasst werden.
  LOOP AT L_ADDIS_IN_ORDERS.
    MOVE-CORRESPONDING L_ADDIS_IN_ORDERS TO WTAD_BUYING_PRINT_ADDI.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'ITEM_ADDI_INFO'
         EXCEPTIONS
              OTHERS  = 01.
    LOOP AT L_ADDIS_IN_ORDERS-ADDI_BUYING_EXTRA_TEXT_INFO
                       INTO WTAD_BUYING_PRINT_EXTRA_TEXT.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_ADDI_EXTRA_TEXT'
           EXCEPTIONS
                OTHERS  = 01.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
