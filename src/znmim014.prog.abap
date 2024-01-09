***INCLUDE FM06LFFR.
************************************************************************
*        Performroutinen für Freigabeverfahren                         *
************************************************************************

DATA: UPD_ICDTXT_EINKBELEG  TYPE C.
DATA: BEGIN OF ICDTXT_EINKBELEG  OCCURS 0.
        INCLUDE STRUCTURE CDTXT.
DATA: END OF ICDTXT_EINKBELEG .

TABLES: *EKAN, *EKKO.
DATA: UPD_EKAN      .

DATA: BEGIN OF XEKES       OCCURS 0.
        INCLUDE STRUCTURE UEKES.
DATA: END OF XEKES.

DATA: BEGIN OF YEKES       OCCURS 0.
        INCLUDE STRUCTURE UEKES      .
DATA: END OF YEKES      .

DATA: UPD_EKES      .


DATA: BEGIN OF XEKET       OCCURS 0.
        INCLUDE STRUCTURE UEKET      .
DATA: END OF XEKET      .

DATA: UPD_EKET      .


DATA: BEGIN OF XEKKN       OCCURS 0.
        INCLUDE STRUCTURE UEKKN      .
DATA: END OF XEKKN      .

DATA: BEGIN OF YEKKN       OCCURS 0.
        INCLUDE STRUCTURE UEKKN      .
DATA: END OF YEKKN      .

DATA: UPD_EKKN      .

DATA: UPD_EKKO      .


DATA: BEGIN OF XEKPO       OCCURS 0.
        INCLUDE STRUCTURE UEKPO      .
DATA: END OF XEKPO      .

DATA: BEGIN OF YEKPO       OCCURS 0.
        INCLUDE STRUCTURE UEKPO      .
DATA: END OF YEKPO      .

DATA: UPD_EKPO      .


DATA: BEGIN OF XSADR       OCCURS 0.
        INCLUDE STRUCTURE USADR      .
DATA: END OF XSADR      .

DATA: BEGIN OF YSADR       OCCURS 0.
        INCLUDE STRUCTURE USADR      .
DATA: END OF YSADR      .

DATA: UPD_SADR      .
*-----------------------------------------------------------------------
*include fm06ecdc.                         "1999/06/01 upgrade mdemeest
INCLUDE ZNMIM015.                        "1999/06/01 upgrade mdemeest
*-----------------------------------------------------------------------

TABLES: RM06B, T16FE, T16FT, NAST.
DATA: FEKKOIND LIKE SY-TABIX.
DATA: BEGIN OF FEKKO OCCURS 50,
         EBELN LIKE EKKO-EBELN,
         FRGGR LIKE EKKO-FRGGR,
         FRGSX LIKE EKKO-FRGSX,
         FRGZU LIKE EKKO-FRGZU,
         FRGKE LIKE EKKO-FRGKE,
         FRGRL LIKE EKKO-FRGRL,
         FRGPO LIKE SY-FDPOS,
         FRGLI LIKE SY-LINNO,
         FRGPG LIKE SY-PAGNO,
         FRGDR,
         UPDKZ,
      END OF FEKKO.
DATA: BEGIN OF YEKKO       OCCURS 50.
        INCLUDE STRUCTURE EKKO.
DATA: END OF YEKKO.
DATA: BEGIN OF KKEY,
         MANDT LIKE EKKO-MANDT,
         EBELN LIKE EKKO-EBELN,
      END OF KKEY.
INCLUDE FM06LCFR.

*----------------------------------------------------------------------*
* Sperren und prüfen Einkaufsbeleg auf Änderung                        *
*----------------------------------------------------------------------*
FORM FRG_CHECK_UPDATE.

CHECK FEKKO-UPDKZ NE 'U'.
EKKO-BSTYP = HIDK-BSTYP.
PERFORM TITLE_TEXT.
*- Zwischenzeitliche Änderung prüfen ----------------------------------*
IF FEKKO-UPDKZ EQ 'A'.
   MESSAGE E176 WITH TEXT FEKKO-EBELN.
ENDIF.

*- Einkaufsbeleg sperren ----------------------------------------------*
CALL FUNCTION 'ENQUEUE_EMEKKOE'
  EXPORTING
     EBELN       = FEKKO-EBELN
  EXCEPTIONS
     FOREIGN_LOCK   = 2
     SYSTEM_FAILURE = 3.
IF SY-SUBRC NE 0.
   MESSAGE E006 WITH TEXT FEKKO-EBELN.
ENDIF.
*- Einkaufsbelegkopf neu lesen ----------------------------------------*
SELECT SINGLE * FROM EKKO WHERE EBELN EQ FEKKO-EBELN.
*- Zwischenzeitliche Änderung prüfen ----------------------------------*
MOVE-CORRESPONDING EKKO TO KKEY.
READ TABLE XEKKO WITH KEY KKEY BINARY SEARCH.
IF EKKO NE XEKKO.
   FEKKO-UPDKZ = 'A'.
   MODIFY FEKKO INDEX FEKKOIND.
   MESSAGE E176 WITH TEXT FEKKO-EBELN.
ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
* Tabelle der Zusatzinformationen für Freigabe aufbauen                *
*----------------------------------------------------------------------*
FORM FRG_FEKKO_AUFBAUEN USING FEA_FRGPO.

MOVE-CORRESPONDING EKKO TO FEKKO.
FEKKO-FRGPO = FEA_FRGPO.
APPEND FEKKO.

ENDFORM.

*eject
*----------------------------------------------------------------------*
*  Freigabe-Info                                                       *
*----------------------------------------------------------------------*
FORM FRG_INFO.
DATA: HTITEL(30).
PERFORM CHECK_KOPFZEILE.
CHECK EXITFLAG EQ SPACE.
READ TABLE FEKKO WITH KEY HIDK-EBELN BINARY SEARCH.
CHECK SY-SUBRC EQ 0.
CLEAR HTITEL.
EKKO-BSTYP = HIDK-BSTYP.
PERFORM TITLE_TEXT.
HTITEL = TEXT.
WRITE HIDK-EBELN TO HTITEL+15(10).
CONDENSE HTITEL.
CALL FUNCTION 'ME_REL_INFO'
     EXPORTING
          I_TITLE    = HTITEL
          I_FRGCO    = XFRGCO
          I_FRGKZ    = FEKKO-FRGKE
          I_FRGGR    = FEKKO-FRGGR
          I_FRGST    = FEKKO-FRGSX
          I_FRGZU    = FEKKO-FRGZU
          I_FRGOT    = '2'
     EXCEPTIONS
          NOT_ACTIVE = 01.

CLEAR: HIDK, HIDP.
ENDFORM.
*----------------------------------------------------------------------*
* Bestätigungen - line-selection
*----------------------------------------------------------------------*
FORM FRG_INIT USING FRI_FRGCO FRI_MITPOS.

XFRGCO = FRI_FRGCO.
CLEAR XFRGOP.
IF FRI_MITPOS EQ SPACE.
   XFRGOP = 'X'.
ENDIF.
PREISANZ = 'X'.
AENDANZ = 'X'.
ENDFORM.

*eject
*----------------------------------------------------------------------*
*  Freigabe zurücksetzen                                               *
*----------------------------------------------------------------------*
FORM FRG_RESET.

DATA: GEDRUCKT.
PERFORM CHECK_KOPFZEILE.
CHECK EXITFLAG EQ SPACE.
READ TABLE FEKKO WITH KEY HIDK-EBELN BINARY SEARCH.
FEKKOIND = SY-TABIX.
CHECK SY-SUBRC EQ 0.
PERFORM FRG_CHECK_UPDATE.
*- Prüfen ob Beleg bereits gedruckt ist -------------------------------*
IF FEKKO-FRGDR EQ SPACE.
   CASE EKKO-BSTYP.
     WHEN BSTYP-BEST.
       NACHRAPPL = 'EF'.
     WHEN BSTYP-ANFR.
       NACHRAPPL = 'EA'.
     WHEN BSTYP-KONT.
       NACHRAPPL = 'EV'.
     WHEN BSTYP-LFPL.
       NACHRAPPL = 'EV'.
   ENDCASE.
   SELECT * FROM NAST UP TO 1 ROWS
                      WHERE KAPPL EQ NACHRAPPL
                        AND OBJKY EQ HIDK-EBELN
                        AND VSTAT NE '0'.
   ENDSELECT.
   IF SY-SUBRC EQ 0.
      FEKKO-FRGDR = '1'.
   ELSE.
      FEKKO-FRGDR = '0'.
   ENDIF.
   MODIFY FEKKO INDEX FEKKOIND.
ENDIF.
CLEAR GEDRUCKT.
IF FEKKO-FRGDR EQ '1'.
   GEDRUCKT = 'X'.
ENDIF.

CALL FUNCTION 'ME_REL_RESET'
     EXPORTING
          I_TITLE                  = ' '
          I_DIALOG                 = ' '
          I_FRGRL                  = FEKKO-FRGRL
          I_FRGCO                  = XFRGCO
          I_FRGKZ                  = FEKKO-FRGKE
          I_FRGGR                  = FEKKO-FRGGR
          I_FRGST                  = FEKKO-FRGSX
          I_FRGZU                  = FEKKO-FRGZU
          I_STATU                  = GEDRUCKT
          I_FRGOT                  = '2'
     IMPORTING
          E_FRGKZ                  = FEKKO-FRGKE
          E_FRGZU                  = FEKKO-FRGZU
          E_FRGRL                  = FEKKO-FRGRL.
*    EXCEPTIONS
*         NO_NEW_RELEASE_INDICATOR = 01
*         NO_RELEASE_ALREADY       = 02
*         PO_EXISTS                = 03
*         REQ_EXISTS               = 04
*         RESPONSIBILITY_FAIL      = 05
*         PO_OUTPUT                = 06.
FEKKO-UPDKZ = 'U'.
MODIFY FEKKO INDEX FEKKOIND.
PERFORM FRG_ZEILE_UPDATE USING SPACE.
CLEAR: HIDK, HIDP.

ENDFORM.

*eject
*----------------------------------------------------------------------*
*  Sicherungsabfrage Freigabe                                          *
*----------------------------------------------------------------------*
FORM FRG_SAVE_CHECK.

CHECK XFRGCO NE SPACE.
LOOP AT FEKKO WHERE UPDKZ EQ 'U'.
   EXIT.
ENDLOOP.
CHECK SY-SUBRC EQ 0.

CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
     EXPORTING
          TEXTLINE1 = TEXT-F05
          TEXTLINE2 = TEXT-F06
          TITEL = TEXT-500
     IMPORTING
          ANSWER = ANSWER.
CASE ANSWER.
  WHEN 'J'.
    PERFORM FRG_UPDATE.
  WHEN 'A'.
    EXITFLAG = 'X'.
    EXIT.
ENDCASE.

ENDFORM.

*eject
*----------------------------------------------------------------------*
*  Freigabe setzen                                                     *
*----------------------------------------------------------------------*
FORM FRG_SET.

PERFORM CHECK_KOPFZEILE.
CHECK EXITFLAG EQ SPACE.
READ TABLE FEKKO WITH KEY HIDK-EBELN BINARY SEARCH.
FEKKOIND = SY-TABIX.
CHECK SY-SUBRC EQ 0.
PERFORM FRG_CHECK_UPDATE.

CALL FUNCTION 'ME_REL_SET'
     EXPORTING
          I_TITLE                = ' '
          I_DIALOG               = ' '
          I_FRGRL                = FEKKO-FRGRL
          I_FRGCO                = XFRGCO
          I_FRGKZ                = FEKKO-FRGKE
          I_FRGGR                = FEKKO-FRGGR
          I_FRGST                = FEKKO-FRGSX
          I_FRGZU                = FEKKO-FRGZU
          I_FRGOT                = '2'
     IMPORTING
          E_FRGKZ                = FEKKO-FRGKE
          E_FRGZU                = FEKKO-FRGZU
          E_FRGRL                = FEKKO-FRGRL.
*    EXCEPTIONS
*         PREREQUISITE_FAIL      = 01
*         RELEASE_ALREADY_POSTED = 02
*         RESPONSIBILITY_FAIL    = 03.

FEKKO-UPDKZ = 'U'.
MODIFY FEKKO INDEX FEKKOIND.
PERFORM FRG_ZEILE_UPDATE USING 'X'.
CLEAR: HIDK, HIDP.
ENDFORM.

*eject
*----------------------------------------------------------------------*
*  Freigabe sichern                                                    *
*----------------------------------------------------------------------*
FORM FRG_UPDATE.

*- Prüfen Änderung ----------------------------------------------------*
LOOP AT FEKKO WHERE UPDKZ EQ 'U'.
   EXIT.
ENDLOOP.
IF SY-SUBRC NE 0.
   MESSAGE S022(06).
   EXIT.
ENDIF.
*- Unveränderte entfernen ---------------------------------------------*
LOOP AT FEKKO WHERE UPDKZ NE 'U'.
   DELETE FEKKO.
ENDLOOP.
*- Zwischenzeitliche Änderung prüfen ----------------------------------*
LOOP AT XEKKO.
   READ TABLE FEKKO WITH KEY XEKKO-EBELN BINARY SEARCH.
   IF SY-SUBRC NE 0.
      DELETE XEKKO.
   ELSE.
      MOVE XEKKO TO *EKKO.
      XEKKO-FRGZU = FEKKO-FRGZU.
      XEKKO-FRGKE = FEKKO-FRGKE.
      XEKKO-FRGRL = FEKKO-FRGRL.
      MODIFY XEKKO INDEX SY-TABIX.
*- Änderungerungsbelege verbuchen -------------------------------------*
      EKKO = XEKKO.
      OBJECTID(10) = EKKO-EBELN.
      TCODE        = SY-TCODE.
      UTIME        = SY-UZEIT.
      UDATE        = SY-DATUM.
      USERNAME     = SY-UNAME.
      UPD_EKAN     = 'U'.
      UPD_EKKO     = 'U'.
      UPD_EKPO     = 'U'.
      UPD_EKKN     = 'U'.
      UPD_EKET     = 'U'.
      UPD_SADR     = 'U'.
      UPD_EKES     = 'U'.
      UPD_ICDTXT_EINKBELEG = 'U'.
      PERFORM CD_CALL_EINKBELEG.
   ENDIF.
ENDLOOP.
*- Nachrichten aktivieren ---------------------------------------------*
PERFORM FRG_UPDATE_NACHRICHTEN.
*- Änderungen sichern -------------------------------------------------*
READ TABLE XEKKO INDEX 1.
IF SY-SUBRC EQ 0.
*- Bestellköpfe verbuchen ---------------------------------------------*
   CALL FUNCTION 'ME_UPDATE_RELEASE'
        TABLES
             T_EKKO = XEKKO.
   COMMIT WORK.
ENDIF.
MESSAGE S177.

IF SY-CALLD NE SPACE.
   LEAVE.
ELSE.
   LEAVE TO TRANSACTION SY-TCODE.
ENDIF.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*  Nachrichten aktivieren beim Sichern der Freigaben                   *
*----------------------------------------------------------------------*
FORM FRG_UPDATE_NACHRICHTEN.

LOOP AT XEKKO.
*- Bestimmen Applikation ----------------------------------------------*
   CASE EKKO-BSTYP.
     WHEN BSTYP-BEST.
       NACHRAPPL = 'EF'.
     WHEN BSTYP-ANFR.
       NACHRAPPL = 'EA'.
     WHEN BSTYP-KONT.
       NACHRAPPL = 'EV'.
     WHEN BSTYP-LFPL.
       NACHRAPPL = 'EV'.
   ENDCASE.
*- Lesen Nachrichten --------------------------------------------------*
   CALL FUNCTION 'RV_MESSAGES_READ'
        EXPORTING
             MSG_KAPPL = NACHRAPPL
             MSG_OBJKY = XEKKO-EBELN.
*- Holen Nachrichten --------------------------------------------------*
   XOBJKY = XEKKO-EBELN.
   CALL FUNCTION 'RV_MESSAGES_GET'
        EXPORTING
           MSG_OBJKY_FROM = XOBJKY        "wegen externer Nummernvergabe
             MSG_OBJKY_TO   = XOBJKY
        TABLES
             TAB_XNAST = XNAST
             TAB_YNAST = YNAST.

*- Ändern Nachrichten -------------------------------------------------*
   LOOP AT XNAST WHERE VSTAT EQ '0'.
*- Freigabe gesetzt: aktivieren ---------------------------------------*
      IF XEKKO-FRGRL EQ SPACE AND
         XNAST-AKTIV NE SPACE.
         XNAST-AKTIV = SPACE.
         MODIFY XNAST.
      ELSE.
*- Freigabe zurückgesetzt: deaktivieren -------------------------------*
         IF XEKKO-FRGRL NE SPACE AND
            XNAST-AKTIV EQ SPACE.
            XNAST-AKTIV = 'X'.
            MODIFY XNAST.
         ENDIF.
      ENDIF.
   ENDLOOP.
*- Nachrichten ändern -------------------------------------------------*
   IF SY-SUBRC EQ 0.
      CALL FUNCTION 'RV_MESSAGES_MODIFY'
           TABLES
              TAB_XNAST = XNAST.
   ENDIF.
ENDLOOP.
*- Nachrichten verbuchen ----------------------------------------------*
IF SY-SUBRC EQ 0.
   CALL FUNCTION 'RV_MESSAGES_UPDATE'.
ENDIF.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*  Freigabeinfo-zeile ausgeben                                         *
*----------------------------------------------------------------------*
FORM FRG_ZEILE.
CHECK XFRGCO NE SPACE.

READ TABLE FEKKO WITH KEY HIDK-EBELN BINARY SEARCH.
FEKKOIND = SY-TABIX.
CHECK SY-SUBRC EQ 0.
SELECT SINGLE * FROM T16FT WHERE SPRAS = SY-LANGU
                             AND FRGGR = EKKO-FRGGR
                             AND FRGSX = EKKO-FRGSX.
CALL FUNCTION 'ME_REL_INFO_STATUS'
     EXPORTING
          I_FRGOT  = '2'
          I_FRGKZ  = EKKO-FRGKE
     IMPORTING
          E_STATUS = T16FE-FRGET.
CALL FUNCTION 'ME_REL_CHECK'
     EXPORTING
          I_FRGAB    = XFRGCO
          I_FRGGR    = EKKO-FRGGR
          I_FRGST    = EKKO-FRGSX
          I_FRGZU    = EKKO-FRGZU
          I_TEXT     = 'X'
     IMPORTING
          E_ANZFM    = RM06B-ANZFM
     EXCEPTIONS
          NO_RELEASE = 01.
WRITE: / SY-VLINE,
       2 EKKO-FRGGR,
       4 '/',
       5 EKKO-FRGSX,
         T16FT-FRGXT,
      29 EKKO-FRGKE,
         T16FE-FRGET,
      52 RM06B-ANZFM,
      81 SY-VLINE.
HIDE HIDK.
CLEAR HIDP.
FEKKO-FRGLI = SY-LINNO.
FEKKO-FRGPG = SY-PAGNO.
MODIFY FEKKO INDEX FEKKOIND.
ENDFORM.

*eject
*----------------------------------------------------------------------*
*  Freigabeinfo-zeile modifizieren                                     *
*----------------------------------------------------------------------*
FORM FRG_ZEILE_UPDATE USING FRG_SETKZ.

CHECK FEKKO-FRGLI NE 0.
READ LINE FEKKO-FRGLI OF PAGE FEKKO-FRGPG.
CHECK SY-SUBRC EQ 0.
CALL FUNCTION 'ME_REL_INFO_STATUS'
     EXPORTING
          I_FRGOT  = '2'
          I_FRGKZ  = FEKKO-FRGKE
     IMPORTING
          E_STATUS = T16FE-FRGET.

MOVE FEKKO-FRGKE TO SY-LISEL+28(1).
MOVE T16FE-FRGET TO SY-LISEL+30(20).
IF FRG_SETKZ NE SPACE.
   MOVE TEXT-F01 TO SY-LISEL+51(28).
ELSE.
   MOVE TEXT-F02 TO SY-LISEL+51(28).
ENDIF.
MODIFY LINE FEKKO-FRGLI OF PAGE FEKKO-FRGPG
                LINE FORMAT COLOR COL_POSITIVE.
ENDFORM.
