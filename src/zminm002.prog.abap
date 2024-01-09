REPORT RM07MMBL MESSAGE-ID M7 NO STANDARD PAGE HEADING.

TABLES: T159B, T156.
******* CHANGE LOG *****************************************************
* 08/22/96 - NOTE - all additions/changes to the original code have
*            been marked with the comment "ENHANCEMENT TO RM07MMBL"
* 03/24/97 - Lee Haire
*          - Wrong OK_CODE being called when adding serial numbers for
*            reservations.  This was causing an incorrect screen
*            sequence.  Changes made to FORM ADD_SERIAL_NUMBER.
************************************************************************
*******  ENHANCEMENT TO RM07MMBL - LH   08/22/96 ***********************
DATA: OLD_GERNR LIKE ZBMSEG-GERNR,
      OLD_DYNPRO LIKE SY-DYNNR,
      OLD_PROGRAM LIKE TSTC-PGMNA.
DATA: BEGIN OF XWEWA.
        INCLUDE STRUCTURE ZBMSEG.
DATA: END OF XWEWA.
*DATA: BEGIN OF XWEWA.
*        INCLUDE STRUCTURE BMSEG.
*DATA: END OF XWEWA.

*DATA: DSNAME LIKE FILENAME-FILEINTERN VALUE
*                 'MM_INVENTORY_MANAGEMENT_GOODS_MOVEMENT'.
DATA: DSNAME LIKE FILENAME-FILEINTERN VALUE
                 'ZMINM002_?'.

************************************************************************
DATA: DATUM LIKE SY-DATUM.
DATA: GESAMT TYPE I.

DATA: BEGIN OF YWEWA OCCURS 50.
        INCLUDE STRUCTURE ZBMSEG.
DATA: END OF YWEWA.

DATA: BEGIN OF WEWA_HEADER,
        TCODE LIKE MKPF-TCODE,
        BLDAT LIKE MKPF-BLDAT,
        BUDAT LIKE MKPF-BUDAT,
        BWART LIKE MSEG-BWART,
        XBLNR LIKE MKPF-XBLNR,
        FRBNR LIKE MKPF-FRBNR,
        BKTXT LIKE MKPF-BKTXT,
        WEVER LIKE MKPF-WEVER,
        LDEST LIKE RM07M-LDEST,
      END OF WEWA_HEADER.

DATA: BEGIN OF WEWA_HEADER_OLD,
        TCODE LIKE MKPF-TCODE,
        BLDAT LIKE MKPF-BLDAT,
        BUDAT LIKE MKPF-BUDAT,
        BWART LIKE MSEG-BWART,
        XBLNR LIKE MKPF-XBLNR,
        FRBNR LIKE MKPF-FRBNR,
        BKTXT LIKE MKPF-BKTXT,
        WEVER LIKE MKPF-WEVER,
        LDEST LIKE RM07M-LDEST,
      END OF WEWA_HEADER_OLD.

DATA: BEGIN OF MAPPEN OCCURS 10,
        MAPPE LIKE AM07M-MAPPE,
        ANZBE TYPE I,
        ANZPO TYPE I,
      END OF MAPPEN.

DATA: BEGIN OF BDCDATA OCCURS 10.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: ALTER_TCODE LIKE SY-TCODE,
      ANZAHL     TYPE I,
      BEZ_ART(3) TYPE C,               "Bezugsart für MB11/MB01
      BEZ_NEU(3) TYPE C,
      EOF        TYPE C.

INCLUDE MM07MABC.
INCLUDE RM07DATA.
INCLUDE RM07MUSR.
INCLUDE RM07MEND.
INCLUDE RM07MBDC.
INCLUDE RM07MMBS.
INCLUDE RM07DSET.
INCLUDE RM07MMBT.
INCLUDE RM07GRID.

*--- Parameterdeklaration
SELECTION-SCREEN SKIP 2.
PARAMETERS: DS_NAME LIKE FILENAME-FILEINTERN DEFAULT DSNAME.
PARAMETERS: MAXPO LIKE AM07M-MAXPO.
PARAMETERS: P_UNAME LIKE USR07-BNAME DEFAULT SY-UNAME.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT /1(79) TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XPROT LIKE AM07M-XPROT.
SELECTION-SCREEN COMMENT 3(28) TEXT-001.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XKEEP LIKE AM07M-XSELK DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(28) TEXT-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: XTEST LIKE AM07M-XTEST DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(28) TEXT-002.
SELECTION-SCREEN END OF LINE.

*---------------------------------------------------------------------*
*       INITIALIZATION                                                *
*---------------------------------------------------------------------*
INITIALIZATION.
  SELECT SINGLE * FROM T159B WHERE REPID = SYST-REPID.
  IF SY-SUBRC IS INITIAL.
    MAXPO  = T159B-MAXPO.
    XPROT  = T159B-XPROT.
  ELSE.
    XPROT  = X.
  ENDIF.
  IF MAXPO IS INITIAL.
    MAXPO  = 20.
  ELSEIF MAXPO > 300.
    MAXPO = 300.
  ENDIF.

*---------------------------------------------------------------------*
*       AT SELECTION-SCREEN                                           *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF NOT XTEST IS INITIAL.
    PERFORM READ_PHYSICAL_NAME USING X DS_NAME DS_PHY_NAME.
    MESSAGE W852 WITH DS_PHY_NAME.
  ENDIF.
  IF MAXPO IS INITIAL.
    MAXPO  = 20.
  ELSEIF MAXPO > 300.
    MAXPO = 300.
  ENDIF.

*---------------------------------------------------------------------*
*       START-OF-SELECTION                                            *
*---------------------------------------------------------------------*
START-OF-SELECTION.

  EXPORT P_UNAME TO MEMORY ID 'MBBM_UNAME'.
  IMPORT YWEWA FROM MEMORY ID 'CPIC_YWEWA'.
  IF NOT SY-SUBRC IS INITIAL.
    KEEP = XKEEP.
    PERFORM READ_PHYSICAL_NAME USING X DS_NAME DS_PHY_NAME.
    IF NOT XTEST IS INITIAL.
      PERFORM TESTDATEN_AUFBAUEN.
    ENDIF.
    OPEN DATASET DS_PHY_NAME FOR INPUT IN TEXT MODE.
    IF NOT SY-SUBRC IS INITIAL.
      MESSAGE E850 WITH DS_PHY_NAME.
    ENDIF.
    CLEAR YWEWA.
    READ DATASET DS_PHY_NAME INTO YWEWA.
    IF NOT SY-SUBRC IS INITIAL.
*******  ENHANCEMENT TO RM07MMBL - LH   01/28/97 ***********************
* - stop program when file is empty but do not issue an ERROR message.
*      MESSAGE E851 WITH DS_PHY_NAME.              "-->delete
      MESSAGE I851 WITH DS_PHY_NAME.               "<--insert
      MESSAGE I028(ZS).                            "<--insert
      STOP.                                        "<--insert
************************************************************************
    ENDIF.
* an dieser Stelle muß nochmals ein OPEN DATASET ... erfolgen, da sonst
* der erste Satz des DATASETS verloren geht.
    OPEN DATASET DS_PHY_NAME FOR INPUT IN TEXT MODE.
    WHILE EOF IS INITIAL.
      DO.
        CLEAR YWEWA.
        READ DATASET DS_PHY_NAME INTO YWEWA.
        IF NOT SY-SUBRC IS INITIAL.
          EOF = X.
          EXIT.
        ELSE.
          IF YWEWA-TCODE EQ MB1A OR
             YWEWA-TCODE EQ MB1B OR
             YWEWA-TCODE EQ MB1C.
            YWEWA-TCODE = MB11.
          ENDIF.
*******  ENHANCEMENT TO RM07MMBL - LH   08/22/96 ***********************
          PERFORM RESERVATION_CHECK.
************************************************************************
          APPEND YWEWA.
        ENDIF.
      ENDDO.
    ENDWHILE.
  ENDIF.

*---------------------------------------------------------------------*
*       END-OF-SELECTION                                              *
*---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM STEUERTABELLEN_AUFBAUEN.

  CLEAR: FLG_MAPPE_OFFEN,
         FLG_BELEG_OFFEN,
         FLG_BELEG_VOLL,
         EOF.

* Interne Tabelle mit allen zu verarbeitenden Sätzen
  LOOP AT YWEWA.

*-- Mappe erstellen
    PERFORM MAPPE_AUFBAUEN.
  ENDLOOP.

* Beleg schließen, wenn noch offen
  IF NOT FLG_BELEG_OFFEN IS INITIAL.
    PERFORM BELEG_SCHLIESSEN.
  ENDIF.

* Mappe schließen, wenn noch offen
  IF NOT FLG_MAPPE_OFFEN IS INITIAL.
    APPEND MAPPEN.
    PERFORM MAPPE_SCHLIESSEN.
  ENDIF.

* Ausgabe des Protokolls über die Mappen
  DESCRIBE TABLE MAPPEN LINES GESAMT.
  IF GESAMT IS INITIAL.
    MESSAGE S801.
  ELSE.
    IF GESAMT > 1.
      MESSAGE S835 WITH GESAMT.
    ELSE.
      MESSAGE S832 WITH MAPPEN-MAPPE.
    ENDIF.
    IF XPROT IS INITIAL.
      PERFORM ANFORDERUNGSBILD.
    ELSE.
      SET PF-STATUS 'MMBL'.
      LOOP AT MAPPEN.
        PERFORM NEXT_ROW USING 2 SPACE.
        PERFORM SET_FORMAT USING 4 X SPACE.
        WRITE:02 MAPPEN-MAPPE NO-GAP, SPACE.
        PERFORM SET_FORMAT USING 2 SPACE SPACE.
        WRITE:15 SY-VLINE NO-GAP, 19 MAPPEN-ANZBE,
              30 SY-VLINE NO-GAP, 38 MAPPEN-ANZPO.
        HIDE MAPPEN-MAPPE.
      ENDLOOP.
      PERFORM CLOSE_GRID.
    ENDIF.
  ENDIF.

*---------------------------------------------------------------------*
*       TOP-OF-PAGE                                                   *
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM OPEN_GRID USING 49 1 X.
  WRITE:02 TEXT-005.
  WRITE:15 SY-VLINE.
  WRITE:30 SY-VLINE.
  PERFORM SEP_GRID USING 2 SPACE.

*---------------------------------------------------------------------*
*       FORM MAPPE_AUFBAUEN                                           *
*---------------------------------------------------------------------*
*       Aufbauen der Batch-Input-Mappe aus der internen Tabelle       *
*---------------------------------------------------------------------*
FORM MAPPE_AUFBAUEN.

  ALTER_TCODE = AKTUELLER_TCODE.
  AKTUELLER_TCODE = YWEWA-TCODE.
  IF ALTER_TCODE IS INITIAL.
    ALTER_TCODE = AKTUELLER_TCODE.
  ENDIF.

* Neuer Mappenname ?
  ON CHANGE OF YWEWA-MAPPE.
    IF NOT FLG_BELEG_OFFEN IS INITIAL.
      PERFORM BELEG_SCHLIESSEN.
      PERFORM MAPPE_SCHLIESSEN.
      APPEND MAPPEN.
      CLEAR: FLG_MAPPE_OFFEN,
             FLG_BELEG_OFFEN,
             FLG_BELEG_VOLL.
    ENDIF.
  ENDON.

* Neuer Transaktionscode ?
  ON CHANGE OF YWEWA-TCODE.
    IF NOT FLG_BELEG_OFFEN IS INITIAL.
      PERFORM BELEG_SCHLIESSEN.
      CLEAR: FLG_BELEG_OFFEN,
             FLG_BELEG_VOLL.
    ENDIF.
  ENDON.

* Neue Bewegungsart ?
  ON CHANGE OF YWEWA-BWART.
    PERFORM PRUEFEN_UB.
  ENDON.

  MOVE-CORRESPONDING YWEWA TO WEWA_HEADER.
*******  ENHANCEMENT TO RM07MMBL - LH   08/22/96 ***********************
*  OLD_GERNR = YWEWA-GERNR.
************************************************************************

* Mappe öffnen, wenn noch keine geöffnet wurde
  IF FLG_MAPPE_OFFEN IS INITIAL.
    CLEAR MAPPEN.
    PERFORM MAPPE_OEFFNEN USING YWEWA-MAPPE.
    MAPPEN-MAPPE = YWEWA-MAPPE.
  ENDIF.

* Beleg öffnen, wenn noch keiner geöffnet wurde
  IF FLG_BELEG_OFFEN IS INITIAL.
    PERFORM BELEG_OEFFNEN.
  ELSE.

*-- Beleg ist voll, neuer Beleg muß geöffnet werden
    IF NOT FLG_BELEG_VOLL IS INITIAL.
      PERFORM BELEG_SCHLIESSEN.
      PERFORM BELEG_OEFFNEN.
    ELSE.

*---- Kopfdaten haben sich geändert, neuer Beleg muß geöffnet werden
      PERFORM BEZUG_PRUEFEN.
      IF WEWA_HEADER NE WEWA_HEADER_OLD OR BEZ_ART NE BEZ_NEU.
*******  ENHANCEMENT TO RM07MMBL - LH   08/22/96 ***********************
*        OLD_GERNR = YWEWA-GERNR.
************************************************************************
        PERFORM BELEG_SCHLIESSEN.
        PERFORM BELEG_OEFFNEN.
      ENDIF.
    ENDIF.
  ENDIF.

* Mappe in Abhängigkeit der Transaktion aufbauen
  CASE AKTUELLER_TCODE.
    WHEN MB01.
      MAPPEN-ANZPO = MAPPEN-ANZPO + 1.
      PERFORM POSITION_MB01.
    WHEN MB11.
      MAPPEN-ANZPO = MAPPEN-ANZPO + 1.
      PERFORM POSITION_MB11.
    WHEN MB31.
      MAPPEN-ANZPO = MAPPEN-ANZPO + 1.
      PERFORM POSITION_MB31.
  ENDCASE.
  MOVE-CORRESPONDING YWEWA TO WEWA_HEADER_OLD.
*******  ENHANCEMENT TO RM07MMBL - LH   08/22/96 ***********************
*  OLD_GERNR = YWEWA-GERNR.
  PERFORM CHECK_FOR_SERIAL_NUMBER.
************************************************************************
ENDFORM.

*----------------------------------------------------------------------*
*       FORM BEZUG_PRUEFEN
*----------------------------------------------------------------------*
FORM BEZUG_PRUEFEN.
  CHECK AKTUELLER_TCODE EQ MB11.
  IF NOT YWEWA-RSNUM IS INITIAL.
    BEZ_NEU = 'RES'.
  ELSEIF T156-KZBWA EQ '01'.
    BEZ_NEU = 'ULB'.
  ELSE.
    CLEAR BEZ_NEU.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BELEG_OEFFNEN                                            *
*---------------------------------------------------------------------*
*       Öffnen eines Beleges. Es ist sicher, dass keiner offen ist    *
*       und mindestens noch eine Position kommt.                      *
*       Das Einstiegsbild der aufzurufenden Transaktion wird          *
*       aufgebaut und mit den notwendigen Kopfdaten gefüllt.          *
*       Der letzte OK-CODE ruft ein POPUP für Vorschlagswerte auf.    *
*---------------------------------------------------------------------*
FORM BELEG_OEFFNEN.

  CHECK EOF IS INITIAL.
  PERFORM XT158_LESEN USING YWEWA-TCODE.
  PERFORM DYNPRO_AUFBAUEN USING DYNPRO.
  FLG_BELEG_OFFEN = X.
  CLEAR FLG_BELEG_VOLL.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM POSTION_MB01                                             *
*---------------------------------------------------------------------*
*       Erzeugen einer Positon für Wareneingang zu Bestellung.        *
*---------------------------------------------------------------------*
FORM POSITION_MB01.

* Dynpro-Nummer aus OK-Code bestimmen
  PERFORM DYNPRO_BESTIMMEN USING NFBL.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
  PERFORM ADD_SERIAL_NUMBER.
************************************************************************

* Dynpro laden
  PERFORM BDCDYNPRO USING PROGRAMM
                          DYNPRO.

* Dynpro füllen
  PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

* Dynpro abschließen
  PERFORM BDCDATEN USING 'BDC_OKCODE'
                          SP.

* Fullscreen Kontierungsblock durchlaufen
* Abfrage, ob das Dynpro prozessiert werden soll, muß noch modif. werden
  IF X = Y.
    PERFORM BDCDYNPRO USING 'SAPLKACB'
                            '0002'.
    PERFORM DYNPRO_AUFBAUEN USING '0002'.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
*    PERFORM CHECK_FOR_SERIAL_NUMBER.
************************************************************************
  ENDIF.

* Nächste Position vorbereiten
  PERFORM DYNPRO_BESTIMMEN USING NP_A.

  PERFORM BDCDYNPRO USING PROGRAMM
                          DYNPRO.

  PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

  PERFORM BDCDATEN USING 'BDC_OKCODE'
                          NFBL.

  ANZAHL = ANZAHL + 1.
  IF ANZAHL = MAXPO.
    FLG_BELEG_VOLL = X.
    CLEAR ANZAHL.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM POSTION_MB11                                             *
*---------------------------------------------------------------------*
*       Erzeugen einer Positon Warenausgabe / Umbuchung / Sonstige    *
*       Warenbewegungen.                                              *
*       Zuerst wird das POPUP mit den Vorschlagswerten aufgebaut      *
*       und gefüllt. Anschließend wird das Dynpro der Einzelerfassung *
*       aufgebaut und gefüllt.                                        *
*       Der OK-Code für die Übernahme der eingegebenen Daten kommt    *
*       entweder von der nächsten Position, oder von der Formroutine  *
*       BELEG_SCHLIESSEN.                                             *
*---------------------------------------------------------------------*
FORM POSITION_MB11.

* Warenbewegung mit Bezug zur Reservierung
  IF BEZ_ART EQ 'RES'.
    PERFORM DYNPRO_BESTIMMEN USING NFRL.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
    PERFORM ADD_SERIAL_NUMBER.
************************************************************************

    PERFORM BDCDYNPRO USING PROGRAMM
                            DYNPRO.

    PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

    PERFORM BDCDATEN USING 'BDC_OKCODE'
                            SP.

*-- Materialeinzelerfassungsdynpro
    PERFORM DYNPRO_BESTIMMEN USING NP_A.

    PERFORM BDCDYNPRO USING PROGRAMM
                            DYNPRO.

    PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

    PERFORM BDCDATEN USING 'BDC_OKCODE'
                            NFRL.

* Umlagerungsbestellung
  ELSEIF BEZ_ART EQ 'ULB'.
    PERFORM DYNPRO_BESTIMMEN USING NFUL.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
    PERFORM ADD_SERIAL_NUMBER.
************************************************************************

    PERFORM BDCDYNPRO USING PROGRAMM
                            DYNPRO.

    PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

    PERFORM BDCDATEN USING 'BDC_OKCODE'
                            SP.

*-- Materialeinzelerfassungsdynpro
    PERFORM DYNPRO_BESTIMMEN USING NP_A.

    PERFORM BDCDYNPRO USING PROGRAMM
                            DYNPRO.

    PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

    PERFORM BDCDATEN USING 'BDC_OKCODE'
                            NFUL.

* alle anderen Fälle
  ELSE.
    PERFORM DYNPRO_BESTIMMEN USING VOR_A.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
    PERFORM ADD_SERIAL_NUMBER.
************************************************************************

    PERFORM BDCDYNPRO USING PROGRAMM
                            DYNPRO.

    PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

    PERFORM BDCDATEN USING 'BDC_OKCODE'
                            NP.

*-- Materialeinzelerfassungsdynpro
    PERFORM DYNPRO_BESTIMMEN USING NP_A.

    PERFORM BDCDYNPRO USING PROGRAMM
                            DYNPRO.

    PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

    PERFORM BDCDATEN USING 'BDC_OKCODE'
                            VOR_A.
  ENDIF.

* Fullscreen Kontierungsblock durchlaufen
* Abfrage, ob das Dynpro prozessiert werden soll, muß noch modif. werden
  IF X = X.
    PERFORM BDCDYNPRO USING 'SAPLKACB'
                            '0002'.
    PERFORM DYNPRO_AUFBAUEN USING '0002'.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
*  PERFORM CHECK_FOR_SERIAL_NUMBER.
************************************************************************
  ENDIF.
*  PERFORM CHECK_FOR_SERIAL_NUMBER.

  ANZAHL = ANZAHL + 1.
  IF ANZAHL = MAXPO.
    FLG_BELEG_VOLL = X.
    CLEAR ANZAHL.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM POSTION_MB31                                             *
*---------------------------------------------------------------------*
*       Erzeugen einer Positon für Wareneingang zum Fertigungsauftrag *
*---------------------------------------------------------------------*
FORM POSITION_MB31.

* Dynpro-Nummer aus OK-Code bestimmen
  PERFORM DYNPRO_BESTIMMEN USING NFPL.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
  PERFORM ADD_SERIAL_NUMBER.
************************************************************************

* Dynpro laden
  PERFORM BDCDYNPRO USING PROGRAMM
                          DYNPRO.

* Dynpro füllen
  PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

* Dynpro abschließen
  PERFORM BDCDATEN USING 'BDC_OKCODE'
                          SP.

* Fullscreen Kontierungsblock durchlaufen
* Abfrage, ob das Dynpro prozessiert werden soll, muß noch modif. werden
  IF X = Y.
    PERFORM BDCDYNPRO USING 'SAPLKACB'
                            '0002'.
    PERFORM DYNPRO_AUFBAUEN USING '0002'.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
*    PERFORM CHECK_FOR_SERIAL_NUMBER.
************************************************************************
  ENDIF.

* Nächste Position vorbereiten
  PERFORM DYNPRO_BESTIMMEN USING NP_A.

  PERFORM BDCDYNPRO USING PROGRAMM
                          DYNPRO.

  PERFORM DYNPRO_AUFBAUEN USING DYNPRO.

  PERFORM BDCDATEN USING 'BDC_OKCODE'
                          NFPL.

  ANZAHL = ANZAHL + 1.
  IF ANZAHL = MAXPO.
    FLG_BELEG_VOLL = X.
    CLEAR ANZAHL.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DYNPRO_AUFBAUEN                                          *
*---------------------------------------------------------------------*
*       Aufbauen der Dynpro's.                                        *
*---------------------------------------------------------------------*
FORM DYNPRO_AUFBAUEN USING D_DYNPRO.

  CASE D_DYNPRO.

*-- Einstiegsdynpro MB01
    WHEN '0200'.
      REFRESH BDCDATA.
      CLEAR BDCDATA.
      PERFORM BDCDYNPRO USING PROGRAMM
                              D_DYNPRO.
      WRITE YWEWA-BUDAT TO DATUM.
      PERFORM BDCDATEN USING 'MKPF-BUDAT'
                              DATUM.
      WRITE YWEWA-BLDAT TO DATUM.
      PERFORM BDCDATEN USING 'MKPF-BLDAT'
                              DATUM.
      PERFORM BDCDATEN USING 'RM07M-LFSNR'
                              YWEWA-XBLNR.
      PERFORM BDCDATEN USING 'MKPF-FRBNR'
                              YWEWA-FRBNR.
      PERFORM BDCDATEN USING 'MKPF-BKTXT'
                              YWEWA-BKTXT.
      PERFORM BDCDATEN USING 'BDC_OKCODE'
                              NFBL.
    WHEN '0210'.
      PERFORM BDCDATEN USING 'MSEG-LGORT'
                              YWEWA-LGORT.
      PERFORM BDCDATEN USING 'MSEG-CHARG'
                              YWEWA-CHARG.
      PERFORM BDCDATEN USING 'MSEG-ERFMG'
                              YWEWA-ERFMG.
      PERFORM BDCDATEN USING 'MSEG-ERFME'
                              YWEWA-ERFME.
      PERFORM BDCDATEN USING 'MSEG-INSMK'
                              YWEWA-INSMK.
      PERFORM BDCDATEN USING 'MSEG-ZUSCH'
                              YWEWA-ZUSCH.
      PERFORM BDCDATEN USING 'MSEG-BPMNG'
                              YWEWA-BPMNG.
      PERFORM BDCDATEN USING 'MSEG-BPRME'
                              YWEWA-BPRME.
      PERFORM BDCDATEN USING 'MSEG-ELIKZ'
                              YWEWA-ELIKZ.
      PERFORM BDCDATEN USING 'MSEG-WEANZ'
                              YWEWA-WEANZ.
      PERFORM BDCDATEN USING 'MSEG-ABLAD'
                              YWEWA-ABLAD.
      PERFORM BDCDATEN USING 'MSEG-WEMPF'
                              YWEWA-WEMPF.
      PERFORM BDCDATEN USING 'MSEG-GRUND'
                              YWEWA-GRUND.
      PERFORM BDCDATEN USING 'MSEG-SGTXT'
                              YWEWA-SGTXT.
      PERFORM BDCDATEN USING 'DM07M-XZGVH'
                              YWEWA-XZGVH.

*-- Einstiegsdynpro MB31
    WHEN '0300'.
      REFRESH BDCDATA.
      CLEAR BDCDATA.
      PERFORM BDCDYNPRO USING PROGRAMM
                              D_DYNPRO.
      WRITE YWEWA-BUDAT TO DATUM.
      PERFORM BDCDATEN USING 'MKPF-BUDAT'
                              DATUM.
      WRITE YWEWA-BLDAT TO DATUM.
      PERFORM BDCDATEN USING 'MKPF-BLDAT'
                              DATUM.
      PERFORM BDCDATEN USING 'RM07M-LFSNR'
                              YWEWA-XBLNR.
      PERFORM BDCDATEN USING 'MKPF-BKTXT'
                              YWEWA-BKTXT.
      PERFORM BDCDATEN USING 'BDC_OKCODE'
                              NFPL.
    WHEN '0310'.
      PERFORM BDCDATEN USING 'MSEG-LGORT'
                              YWEWA-LGORT.
      PERFORM BDCDATEN USING 'MSEG-CHARG'
                              YWEWA-CHARG.
      PERFORM BDCDATEN USING 'MSEG-ERFMG'
                              YWEWA-ERFMG.
      PERFORM BDCDATEN USING 'MSEG-ERFME'
                              YWEWA-ERFME.
      PERFORM BDCDATEN USING 'MSEG-INSMK'
                              YWEWA-INSMK.
      PERFORM BDCDATEN USING 'MSEG-ZUSCH'
                              YWEWA-ZUSCH.
      PERFORM BDCDATEN USING 'MSEG-BPMNG'
                              YWEWA-BPMNG.
      PERFORM BDCDATEN USING 'MSEG-BPRME'
                              YWEWA-BPRME.
      PERFORM BDCDATEN USING 'MSEG-ELIKZ'
                              YWEWA-ELIKZ.
      PERFORM BDCDATEN USING 'MSEG-WEANZ'
                              YWEWA-WEANZ.
      PERFORM BDCDATEN USING 'MSEG-ABLAD'
                              YWEWA-ABLAD.
      PERFORM BDCDATEN USING 'MSEG-WEMPF'
                              YWEWA-WEMPF.
      PERFORM BDCDATEN USING 'MSEG-GRUND'
                              YWEWA-GRUND.
      PERFORM BDCDATEN USING 'MSEG-SGTXT'
                              YWEWA-SGTXT.
      PERFORM BDCDATEN USING 'DM07M-XZGVH'
                              YWEWA-XZGVH.

*-- Einstiegsdynpro MB11
    WHEN '0400'.
      REFRESH BDCDATA.
      CLEAR BDCDATA.
      PERFORM BDCDYNPRO USING PROGRAMM
                              D_DYNPRO.
      WRITE YWEWA-BUDAT TO DATUM.
      PERFORM BDCDATEN USING 'MKPF-BUDAT'
                              DATUM.
      WRITE YWEWA-BLDAT TO DATUM.
      PERFORM BDCDATEN USING 'MKPF-BLDAT'
                              DATUM.
      PERFORM BDCDATEN USING 'RM07M-MTSNR'
                              YWEWA-XBLNR.
      PERFORM BDCDATEN USING 'MKPF-BKTXT'
                              YWEWA-BKTXT.

*---- Warenbewegung mit Bezug zur Reservierung
      IF NOT YWEWA-RSNUM IS INITIAL.
        BEZ_ART = 'RES'.
        PERFORM BDCDATEN USING 'BDC_OKCODE'
                                NFRL.

*---- Warenbewegung mit Bezug zur Bestellung (bei Umlagerungsbestellung)
      ELSEIF T156-KZBWA EQ '01'.
        BEZ_ART = 'ULB'.
        PERFORM BDCDATEN USING 'BDC_OKCODE'
                                NFUL.

*---- sonstige Warenbewegungen
      ELSE.
        CLEAR BEZ_ART.
        PERFORM BDCDATEN USING 'BDC_OKCODE'
                                VOR_A.
      ENDIF.

    WHEN '0410'.

      PERFORM BDCDATEN USING 'MSEG-MATNR'
                              YWEWA-MATNR.
      PERFORM BDCDATEN USING 'MSEG-WERKS'
                              YWEWA-WERKS.
      PERFORM BDCDATEN USING 'MSEG-LGORT'
                              YWEWA-LGORT.
      PERFORM BDCDATEN USING 'MSEG-CHARG'
                              YWEWA-CHARG.
      PERFORM BDCDATEN USING 'MSEG-ERFMG'
                              YWEWA-ERFMG.
      PERFORM BDCDATEN USING 'MSEG-ERFME'
                              YWEWA-ERFME.
      PERFORM BDCDATEN USING 'MSEG-ZUSCH'
                              YWEWA-ZUSCH.
      PERFORM BDCDATEN USING 'MSEG-LIFNR'
                              YWEWA-LIFNR.
      PERFORM BDCDATEN USING 'MSEG-KUNNR'
                              YWEWA-KUNNR.
      PERFORM BDCDATEN USING 'MSEG-EBELN'
                              YWEWA-EBELN.
      PERFORM BDCDATEN USING 'MSEG-EBELP'
                              YWEWA-EBELP.
      PERFORM BDCDATEN USING 'MSEG-SGTXT'
                              YWEWA-SGTXT.
      PERFORM BDCDATEN USING 'MSEG-WEMPF'
                              YWEWA-WEMPF.
      PERFORM BDCDATEN USING 'MSEG-KZEAR'
                              YWEWA-KZEAR.
      PERFORM BDCDATEN USING 'MSEG-UMMAT'
                              YWEWA-UMMAT.
      PERFORM BDCDATEN USING 'MSEG-UMWRK'
                              YWEWA-UMWRK.
      PERFORM BDCDATEN USING 'MSEG-UMLGO'
                              YWEWA-UMLGO.
      PERFORM BDCDATEN USING 'MSEG-UMCHA'
                              YWEWA-UMCHA.
      PERFORM BDCDATEN USING 'MSEG-UMZUS'
                              YWEWA-UMZUS.
      PERFORM BDCDATEN USING 'MSEG-GRUND'
                              YWEWA-GRUND.
      PERFORM BDCDATEN USING 'MSEG-LGPLA'
                              YWEWA-LGPLA.
      PERFORM BDCDATEN USING 'MSEG-LGTYP'
                              YWEWA-LGTYP.
      PERFORM BDCDATEN USING 'DM07M-KONTO'
                              YWEWA-KONTO.
*
*     PERFORM BDCDATEN USING 'MSEG-EXWRT'
*                             YWEWA-EXWRT.
     PERFORM BDCDATEN USING 'MSEG-EXBWR'
                             YWEWA-EXWRT.
      PERFORM BDCDATEN USING 'MSEG-EXVKW'
                              YWEWA-EXVKW.
      IF YWEWA-MHDAT NE SPACE.
        WRITE YWEWA-MHDAT TO DATUM.
        PERFORM BDCDATEN USING 'DM07M-MHDAT'
                                DATUM.
      ENDIF.
*---- KDAUF und KDPOS müssen wegen vorgelagertem Zeitpunkt sowohl
*     im MSEG als auch im COBL enthalten sein
      PERFORM BDCDATEN USING 'MSEG-KDAUF'
                              YWEWA-KDAUF.
      PERFORM BDCDATEN USING 'MSEG-KDPOS'
                              YWEWA-KDPOS.

*---- Eingabefelder des Kontierungblocks
      IF X = Y.
        PERFORM CO_BLOCK_DATEN.
      ENDIF.

* Eingabe-POPUP'S
    WHEN '1201'.
      PERFORM BDCDATEN USING 'RM07M-BWARTWE'
                              YWEWA-BWART.
      PERFORM BDCDATEN USING 'RM07M-EBELN(01)'
                              YWEWA-EBELN.
      PERFORM BDCDATEN USING 'RM07M-EBELP(01)'
                              YWEWA-EBELP.

    WHEN '1301'.
      PERFORM BDCDATEN USING 'RM07M-BWARTWE'
                              YWEWA-BWART.
      PERFORM BDCDATEN USING 'RM07M-LGORT'
                              YWEWA-LGORT.
      PERFORM BDCDATEN USING 'RM07M-AUFNR(01)'
                              YWEWA-AUFNR.
      PERFORM BDCDATEN USING 'RM07M-WERKS(01)'
                              YWEWA-WERKS.

    WHEN '1401'.
      PERFORM BDCDATEN USING 'RM07M-RSNUM'
                              YWEWA-RSNUM.
      PERFORM BDCDATEN USING 'RM07M-RSPOS'
                              YWEWA-RSPOS.

    WHEN '1403'.
      PERFORM BDCDATEN USING 'RM07M-BWARTWA'
                              YWEWA-BWART.
      PERFORM BDCDATEN USING 'RM07M-WERKS'
                              YWEWA-WERKS.
      PERFORM BDCDATEN USING 'RM07M-LGORT'
                              YWEWA-LGORT.
      PERFORM BDCDATEN USING 'RM07M-SOBKZ'
                              YWEWA-SOBKZ.
      PERFORM BDCDATEN USING 'RM07M-SOBKZ'
                              YWEWA-SOBKZ.

    WHEN '1404'.
      PERFORM BDCDATEN USING 'RM07M-BWARTWA'
                              YWEWA-BWART.
      PERFORM BDCDATEN USING 'RM07M-WERKS'
                              YWEWA-WERKS.
      PERFORM BDCDATEN USING 'RM07M-LGORT'
                              YWEWA-LGORT.
      PERFORM BDCDATEN USING 'RM07M-EBELN(01)'
                              YWEWA-EBELN.
      PERFORM BDCDATEN USING 'RM07M-EBELP(01)'
                              YWEWA-EBELP.

*-- Kontierungsbild, wenn kompletter Co-Block prozessiert wird
    WHEN '0002'.
      PERFORM CO_BLOCK_DATEN.
      PERFORM BDCDATEN USING 'BDC_OKCODE'       "Weiter mit ENTER
                              OK_08.

  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CO_BLOCK_DATEN                                           *
*---------------------------------------------------------------------*
FORM CO_BLOCK_DATEN.
  PERFORM BDCDATEN USING 'COBL-AUFNR'
                          YWEWA-AUFNR.
  PERFORM BDCDATEN USING 'COBL-ANLN1'
                          YWEWA-ANLN1.
  PERFORM BDCDATEN USING 'COBL-ANLN2'
                          YWEWA-ANLN2.
  PERFORM BDCDATEN USING 'COBL-KOSTL'
                          YWEWA-KOSTL.
  PERFORM BDCDATEN USING 'COBL-KDPOS'
                          YWEWA-KDPOS.
  PERFORM BDCDATEN USING 'COBL-KDEIN'
                          YWEWA-KDEIN.
  PERFORM BDCDATEN USING 'COBL-KDAUF'
                          YWEWA-KDAUF.
  PERFORM BDCDATEN USING 'COBL-KSTRG'
                          YWEWA-KSTRG.
  PERFORM BDCDATEN USING 'COBL-PAOBJNR'
                          YWEWA-PAOBJNR.
  PERFORM BDCDATEN USING 'COBL-PRCTR'
                          YWEWA-PRCTR.
  PERFORM BDCDATEN USING 'COBL-PS_PSP_PNR'
                          YWEWA-PS_PSP_PNR.
  PERFORM BDCDATEN USING 'COBL-NPLNR'
                          YWEWA-NPLNR.
  PERFORM BDCDATEN USING 'COBL-AUFPL'
                          YWEWA-AUFPL.
  PERFORM BDCDATEN USING 'COBL-APLZL'
                          YWEWA-APLZL.
  PERFORM BDCDATEN USING 'COBL-AUFPS'
                          YWEWA-AUFPS.
  PERFORM BDCDATEN USING 'COBL-VPTNR'
                          YWEWA-VPTNR.
  PERFORM BDCDATEN USING 'COBL-FIPOS'
                          YWEWA-FIPOS.
  PERFORM BDCDATEN USING 'COBL-GSBER'
                          YWEWA-GSBER.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BELEG_SCHLIESSEN                                         *
*---------------------------------------------------------------------*
*       Schließen eines Beleges. Es ist sicher, dass einer            *
*       offen ist.                                                    *
*       Die letzte Eingabe wird mit Aufruf des POPUP's für            *
*       Vorschlagswerte übernommen. Danach erfolgt ein Abbruch        *
*       der Eingabe für Vorschlagswerte und das Buchen.               *
*---------------------------------------------------------------------*
FORM BELEG_SCHLIESSEN.

  MAPPEN-ANZBE = MAPPEN-ANZBE + 1.
  CLEAR: FLG_BELEG_OFFEN.

  CASE ALTER_TCODE.

*-- WE zur Bestellung
    WHEN MB01.
      PERFORM DYNPRO_BESTIMMEN USING NFBL.
      PERFORM BDCDYNPRO USING PROGRAMM
                              DYNPRO.
      PERFORM BDCDATEN USING 'BDC_OKCODE'
                              EX.

      PERFORM DYNPRO_BESTIMMEN USING KP_A.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
      PERFORM ADD_LAST_SERIAL_NUM USING PROGRAMM DYNPRO.     "<--insert
************************************************************************
      PERFORM BDCDYNPRO USING PROGRAMM
                              DYNPRO.
      PERFORM BDCDATEN USING 'BDC_OKCODE'
                              EX.

*-- Andere Warenbewegungen
    WHEN MB11.

*---- Reservierung
      IF BEZ_ART EQ 'RES'.
        PERFORM DYNPRO_BESTIMMEN USING NFRL.

*---- Umlagerungsbestellung
      ELSEIF BEZ_ART EQ 'ULB'.
        PERFORM DYNPRO_BESTIMMEN USING NFUL.

*---- sonstige Warenbewegungen
      ELSE.
        PERFORM DYNPRO_BESTIMMEN USING VOR_A.
      ENDIF.
      CLEAR BEZ_ART.
      CLEAR BEZ_NEU.

      PERFORM BDCDYNPRO USING PROGRAMM
                              DYNPRO.
      PERFORM BDCDATEN USING 'BDC_OKCODE'
                              EX.
      PERFORM DYNPRO_BESTIMMEN USING NP_A.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
      PERFORM ADD_LAST_SERIAL_NUM USING PROGRAMM DYNPRO.     "<--insert
************************************************************************
      PERFORM BDCDYNPRO USING PROGRAMM
                              DYNPRO.
      PERFORM BDCDATEN USING 'BDC_OKCODE'
                              EX.

*-- WE zum Fertigungsauftrag
    WHEN MB31.
      PERFORM DYNPRO_BESTIMMEN USING NFPL.
      PERFORM BDCDYNPRO USING PROGRAMM
                              DYNPRO.
      PERFORM BDCDATEN USING 'BDC_OKCODE'
                              EX.

      PERFORM DYNPRO_BESTIMMEN USING KP_A.
*******  ENHANCEMENT TO RM07MMBL - LH   08/13/96 ***********************
      PERFORM ADD_LAST_SERIAL_NUM USING PROGRAMM DYNPRO.     "<--insert
************************************************************************
      PERFORM BDCDYNPRO USING PROGRAMM
                              DYNPRO.
      PERFORM BDCDATEN USING 'BDC_OKCODE'
                              EX.

  ENDCASE.

* Buchen
  PERFORM DYNPRO_BESTIMMEN USING AB_A.
  PERFORM BDCDYNPRO USING PROGRAMM
                          DYNPRO.
  PERFORM BDCDATEN USING 'BDC_OKCODE'
                          BU.

* BDC-Objekt hinzufügen
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = ALTER_TCODE
       TABLES
            DYNPROTAB = BDCDATA.
  ALTER_TCODE = AKTUELLER_TCODE.
  CLEAR ANZAHL.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM PRUEFEN_UB                                               *
*---------------------------------------------------------------------*
*       Umlagerungsbestellung                                         *
*---------------------------------------------------------------------*
FORM PRUEFEN_UB.

  CHECK NOT YWEWA-BWART IS INITIAL.
  SELECT SINGLE * FROM T156 WHERE BWART EQ YWEWA-BWART.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E001 WITH 'T156' YWEWA-BWART.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       AT USER-COMMAND                                               *
*---------------------------------------------------------------------*
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN UCOM-BACK.
      PERFORM ANFORDERUNGSBILD.
    WHEN UCOM-ENDE.
      PERFORM BEENDEN.
    WHEN 'SM35'.
      SET PARAMETER ID 'MPN' FIELD MAPPEN-MAPPE.
      CALL TRANSACTION 'SM35' AND SKIP FIRST SCREEN.
  ENDCASE.

*---------------------------------------------------------------------*
*       FORM CPIC_MMBL                                                *
*---------------------------------------------------------------------*
*  -->  YWEWA , übergebene Struktur gemäß BMSEG                       *
*---------------------------------------------------------------------*
FORM CPIC_MMBL TABLES YWEWA.
  EXPORT YWEWA TO MEMORY ID 'CPIC_YWEWA'.
  CALL TRANSACTION 'MBBM' AND SKIP FIRST SCREEN.
ENDFORM.
*eject

*******  ENHANCEMENT TO RM07MMBL - LH   08/22/96 ***********************
* NEW FORMS ADDED
************************************************************************
*-----------------------------------------------------------------------
*     FORM CHECK_FOR_SERIAL_NUMBER
*-----------------------------------------------------------------------
* - A little routine which checks if there is a serial number to add
* and then sets some global variables.
*-----------------------------------------------------------------------
FORM CHECK_FOR_SERIAL_NUMBER.
  CHECK YWEWA-GERNR <> SPACE AND YWEWA-GERNR <> '/'.
  OLD_GERNR = YWEWA-GERNR.
  OLD_DYNPRO = DYNPRO.
  OLD_PROGRAM = PROGRAMM.
*  PERFORM BDCDYNPRO USING 'SAPMM07M' '1403'.
*  PERFORM BDCDYNPRO USING PROGRAMM DYNPRO.
*  PERFORM BDCDATEN USING 'BDC_OKCODE' '/12'.
ENDFORM.


*-----------------------------------------------------------------------
*     FORM ADD_SERIAL_NUMBER
*-----------------------------------------------------------------------
* - This is the added BDC mapping for adding a serial number to a
* material document.
*-----------------------------------------------------------------------
FORM ADD_SERIAL_NUMBER.

  CHECK OLD_GERNR <> SPACE AND OLD_GERNR <> '/'.
  PERFORM BDCDYNPRO USING PROGRAMM DYNPRO.
  PERFORM BDCDATEN USING 'BDC_OKCODE' '/12'.
  PERFORM BDCDYNPRO USING OLD_PROGRAM OLD_DYNPRO.
  PERFORM BDCDATEN USING 'BDC_OKCODE' 'NSER'.
  PERFORM BDCDYNPRO USING 'SAPLIPW0' '0200'.
  PERFORM BDCDATEN USING 'RIPW0-SERNR(1)' OLD_GERNR.
  PERFORM BDCDATEN USING 'BDC_OKCODE' '/8'.
  IF ( ALTER_TCODE = MB11 ).
    PERFORM BDCDYNPRO USING 'SAPLKACB' '0002'.
    PERFORM BDCDATEN USING 'BDC_OKCODE' '/8'.
  ENDIF.
  PERFORM BDCDYNPRO USING OLD_PROGRAM OLD_DYNPRO.
  IF ( ALTER_TCODE = MB01 ).
    PERFORM BDCDATEN USING 'BDC_OKCODE' NFBL.
  ENDIF.
  IF ( ALTER_TCODE = MB11 ).                           "<-- 03/24/97 LH
    IF BEZ_ART EQ 'RES'.                               "<-- 03/24/97 LH
      PERFORM BDCDATEN USING 'BDC_OKCODE' NFRL.        "<-- 03/24/97 LH
    ELSE.                                              "<-- 03/24/97 LH
      PERFORM BDCDATEN USING 'BDC_OKCODE' 'VOR'.
    ENDIF.                                             "<-- 03/24/97 LH
    PERFORM BDCDYNPRO USING 'SAPLKACB' '0002'.
    PERFORM BDCDATEN USING 'BDC_OKCODE' '/8'.
  ENDIF.
  CLEAR: OLD_GERNR, OLD_DYNPRO, OLD_PROGRAM.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM ADD_LAST_SERIAL_NUM
*-----------------------------------------------------------------------
* - This routine was added as an enhancement to the standard SAP
*   program RM07MMBL.  This will add serial numbers (if needed) to
*   materials in the goods movement document.  It is assumed that
*   each line item will only ever contain a quantity of 1 for a given
*   material.  Only certain movement types will have serial numbers
*   for materials.  However, instead of checking for these movement
*   types, the program will attempt to add serial numbers only if a
*   serial number is present in the input file.  Consequently, it
*   will be completely up the source program to determine whether a
*   material should have a serial number.
*
*  Parameters:
*     --> D_PROGRAM - branch from screen (program)
*         D_DYNPRO  - branch from screen number
*-----------------------------------------------------------------------
FORM ADD_LAST_SERIAL_NUM USING D_PROGRAM D_DYNPRO.

  CHECK OLD_GERNR <> SPACE AND OLD_GERNR <> '/'.
  PERFORM BDCDYNPRO USING D_PROGRAM D_DYNPRO.
  PERFORM BDCDATEN USING 'BDC_OKCODE' 'NSER'.
  PERFORM BDCDYNPRO USING 'SAPLIPW0' '0200'.
  PERFORM BDCDATEN USING 'RIPW0-SERNR(1)' OLD_GERNR.
  PERFORM BDCDATEN USING 'BDC_OKCODE' '/8'.
  IF ( ALTER_TCODE = MB11 ).
    PERFORM BDCDYNPRO USING 'SAPLKACB' '0002'.
    PERFORM BDCDATEN USING 'BDC_OKCODE' '/8'.
  ENDIF.
  CLEAR: OLD_GERNR, OLD_DYNPRO, OLD_PROGRAM.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM RESERVATION_CHECK
*-----------------------------------------------------------------------
* - If the record contains a reservation number this means that the
* transaction ( goods issue for example) is being done with reference
* to a reservation.  We have to ensure that certain data from the
* record in the file matches the data from the referenced reservation
* line item in SAP.  The following data is checked:
* 1. Reservation header:
*     A.  Order
*     B.  Cost Center
*     C.  Project
* 2. Reservation line item:
*     A.  Material
*     B.  Plant
*     C.  Storage Location
*
* - If the data matches, the value is cleared from the structure YWEWA
* so that is does not pass through to the BDC session.  If the values
* do not match the value from the file is left so that it IS passed
* onto the BDC session.  This will force an error in the BDC because
* all of the above mentioned fields will not be available for input
* in the BDC transaction.  This will indicate that the values did not
* match.
*-----------------------------------------------------------------------
FORM RESERVATION_CHECK.

  TABLES: RKPF,
          RESB.
* SAP internal format
  DATA: T_MATNR(18),
        T_AUFNR(12),
        T_KOSTL(10).
* working variables from RKPF.
  DATA: BEGIN OF W_RKPF,
          AUFNR LIKE RKPF-AUFNR,                          "order
          KOSTL LIKE RKPF-KOSTL,                          "cost ctr.
          PROJN LIKE RKPF-PROJN,                          "project
        END OF W_RKPF.
* working variables from RESB.
  DATA: BEGIN OF W_RESB,
          MATNR LIKE RESB-MATNR,                          "material
          WERKS LIKE RESB-WERKS,                          "plant
          LGORT LIKE RESB-LGORT,                          "storage loc.
        END OF W_RESB.

  CHECK YWEWA-RSNUM <> SPACE.
* convert to SAP internal format
  CALL 'CONVERSION_EXIT_ALPHA_INPUT'  ID 'INPUT'  FIELD YWEWA-AUFNR
                                      ID 'OUTPUT' FIELD T_AUFNR.
  CALL 'CONVERSION_EXIT_ALPHA_INPUT'  ID 'INPUT'  FIELD YWEWA-KOSTL
                                      ID 'OUTPUT' FIELD T_KOSTL.
* validate information in reservation header
  SELECT SINGLE AUFNR KOSTL PROJN INTO W_RKPF FROM RKPF
                WHERE RSNUM = YWEWA-RSNUM.
  IF SY-SUBRC = 0.
    IF W_RKPF-AUFNR = T_AUFNR.
      CLEAR YWEWA-AUFNR.
    ENDIF.
    IF W_RKPF-KOSTL = T_KOSTL.
      CLEAR YWEWA-KOSTL.
    ENDIF.
    IF W_RKPF-PROJN = YWEWA-PROJN.
      CLEAR YWEWA-PROJN.
    ENDIF.
  ENDIF.

* convert to SAP internal format
  CALL 'CONVERSION_EXIT_ALPHA_INPUT'  ID 'INPUT'  FIELD YWEWA-MATNR
                                      ID 'OUTPUT' FIELD T_MATNR.
* validate information in reservation line item
  SELECT SINGLE MATNR WERKS LGORT INTO W_RESB FROM RESB
                WHERE RSNUM = YWEWA-RSNUM AND
                      RSPOS = YWEWA-RSPOS AND
                      RSART = SPACE.
  CHECK SY-SUBRC = 0.
  IF W_RESB-MATNR = T_MATNR.
    CLEAR YWEWA-MATNR.
  ENDIF.
  IF W_RESB-WERKS = YWEWA-WERKS.
    CLEAR YWEWA-WERKS.
  ENDIF.
  IF W_RESB-LGORT = YWEWA-LGORT.
    CLEAR YWEWA-LGORT.
  ENDIF.

ENDFORM.
