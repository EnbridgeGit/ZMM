* 2000/07/27 mdemeest 4.6b All changes indicated by 'UGL'           UGL
*------------------------------------------------------------------ UGL
*eject
*----------------------------------------------------------------------*
* Ausgabe Belegkopf     oder include  RVADOPFO verwenden !!
*----------------------------------------------------------------------*
FORM AUSGABE_KOPF.

  CLEAR RETCO.
  SET LANGUAGE EKKO-SPRAS.
  SET COUNTRY  LFA1-LAND1.

*- Formular festlegen -------------------------------------------------*
  XFORMULAR = TNAPR-FONAM.
  IF XFORMULAR EQ SPACE.
    XFORMULAR = 'MEDRUCK'.
  ENDIF.

  CLEAR XDIALOG.
  CLEAR XDEVICE.
  CLEAR ITCPO.
  IF SY-UCOMM EQ 'DRPR'.   "Probedruck                           "89494
    NAST-TDARMOD = '1'.   "nur Druck, kein Archivieren          "89494
  ENDIF.                                                    "89494
  MOVE-CORRESPONDING NAST TO ITCPO.
  ITCPO-TDTITLE = NAST-TDCOVTITLE.
  ITCPO-TDFAXUSER = NAST-USNAM.
*- Ausgabemedium festlegen --------------------------------------------*
  CASE NAST-NACHA.
    WHEN '2'.
      XDEVICE = 'TELEFAX'.
      IF NAST-TELFX EQ SPACE.
        XDIALOG = 'X'.
      ELSE.
        ITCPO-TDTELENUM  = NAST-TELFX.
        IF NAST-TLAND IS INITIAL.
          ITCPO-TDTELELAND = LFA1-LAND1.
        ELSE.
          ITCPO-TDTELELAND = NAST-TLAND.
        ENDIF.
      ENDIF.
    WHEN '3'.
      XDEVICE = 'TELETEX'.
      IF NAST-TELTX EQ SPACE.
        XDIALOG = 'X'.
      ELSE.
        ITCPO-TDTELENUM  = NAST-TELTX.
        ITCPO-TDTELELAND = LFA1-LAND1.
      ENDIF.
    WHEN '4'.
      XDEVICE = 'TELEX'.
      IF NAST-TELX1 EQ SPACE.
        XDIALOG = 'X'.
      ELSE.
        ITCPO-TDTELENUM  = NAST-TELX1.
        ITCPO-TDTELELAND = LFA1-LAND1.
      ENDIF.
    WHEN '5'.
      DATA:  LVS_COMM_TYPE     TYPE   AD_COMM,
             LVS_COMM_VALUES   TYPE   SZADR_COMM_VALUES.
*   ... use stratagy to get communication type
    CALL FUNCTION 'ADDR_GET_NEXT_COMM_TYPE'
         EXPORTING
              STRATEGY           = NAST-TCODE
*             ADDRESS_TYPE       =
*             ADDRESS_NUMBER     = VBDKA-ADRNR
*             PERSON_NUMBER      = VBDKA-ADRNP
              ADDRESS_NUMBER     = LFA1-ADRNR
*             person_number      = addr_key-persnumber
         IMPORTING
              COMM_TYPE          = LVS_COMM_TYPE
              COMM_VALUES        = LVS_COMM_VALUES
*        TABLES
*             STRATEGY_TABLE     =
         EXCEPTIONS
              ADDRESS_NOT_EXIST  = 1
              PERSON_NOT_EXIST   = 2
              NO_COMM_TYPE_FOUND = 3
              INTERNAL_ERROR     = 4
              PARAMETER_ERROR    = 5
              OTHERS             = 6.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
* convert communication data
      MOVE-CORRESPONDING NAST TO INTNAST.
      MOVE SY-REPID           TO XPROGRAM.
      CALL FUNCTION 'CONVERT_COMM_TYPE_DATA'
           EXPORTING
                PI_COMM_TYPE              = 'INT'
                PI_COMM_VALUES            = LVS_COMM_VALUES
*               pi_screen                 = us_screen
*           PI_NEWID                  =
                PI_COUNTRY                = LFA1-LAND1
                PI_REPID                  = XPROGRAM
                PI_SNAST                  = INTNAST
           IMPORTING
                PE_ITCPO                  = ITCPO
                PE_DEVICE                 = XDEVICE
                PE_MAIL_RECIPIENT         = LVS_RECIPIENT
                PE_MAIL_SENDER            = LVS_SENDER
           EXCEPTIONS
                COMM_TYPE_NOT_SUPPORTED   = 1
                RECIPIENT_CREATION_FAILED = 2
                SENDER_CREATION_FAILED    = 3
                OTHERS                    = 4.
      IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.



    WHEN OTHERS.
      XDEVICE = 'PRINTER'.
      IF NAST-LDEST EQ SPACE.
        XDIALOG = 'X'.
      ELSE.
        ITCPO-TDDEST   = NAST-LDEST.
      ENDIF.
  ENDCASE.
*- Testausgabe --------------------------------------------------------*
  IF XSCREEN NE SPACE.
*- Testausgabe auf Bildschirm -----------------------------------------*
*   IF NAST-TCODE EQ 'XTST'.                               "#211668
      ITCPO-TDPREVIEW = 'X'.
*   ENDIF.
  ENDIF.

  ITCPO-TDNOPRINT  = 'X'.
  ITCPO-TDCOVER    = NAST-TDOCOVER.
  ITCPO-TDCOPIES   = NAST-ANZAL.
  IF SY-UCOMM EQ 'DRPR'.                                    "91419
    ITCPO-TDCOPIES = 1.                                     "91419
  ENDIF.                                                    "91419
  ITCPO-TDDATASET  = NAST-DSNAM.
  ITCPO-TDSUFFIX1  = NAST-DSUF1.
*-------------------------------- UGL Change ----------------------- UGL
*  ITCPO-TDSUFFIX2  = NAST-DSUF2.                                    UGL
*  Place the PO number into the spool request name so that if        UGL
*  a PO number is referenced in a daily error report it checks       UGL
* the spool table for errors when the PO is faxed                    UGL
*                                                                    UGL
  concatenate '*' ekko-ebeln '*' into itcpo-tdsuffix2.              "UGL
*                                                                    UGL
*------------------------------- End of UGL Change ----------------- UGL
  ITCPO-TDIMMED    = NAST-DIMME.
  ITCPO-TDDELETE   = NAST-DELET.
  ITCPO-TDSENDDATE = NAST-VSDAT.
  ITCPO-TDSENDTIME = NAST-VSURA.
  ITCPO-TDPROGRAM  = SY-REPID.
  ITCPO-TDNEWID    = 'X'.                                   "99282

* Formular festlegen -------------------------------------------------*
  CALL FUNCTION 'OPEN_FORM'
       EXPORTING FORM = XFORMULAR
                 LANGUAGE = EKKO-SPRAS
                 OPTIONS = ITCPO
                 ARCHIVE_INDEX  = TOA_DARA
                 ARCHIVE_PARAMS = ARC_PARAMS
*              ARCHIVE_PARAMS = ALARC_PAR1
                 DEVICE = XDEVICE
                 DIALOG = XDIALOG
            MAIL_SENDER        = LVS_SENDER
            MAIL_RECIPIENT     = LVS_RECIPIENT
       EXCEPTIONS CANCELED = 01.
  IF SY-SUBRC NE 0.
    PERFORM PROTOCOL_UPDATE USING '142' EKKO-EBELN SPACE SPACE SPACE.
    RETCO = SY-SUBRC.
    EXIT.
  ENDIF.

* Folgeseitenzaehler -------------------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'NEXTPAGE'
            WINDOW  = 'NEXTPAGE'
       EXCEPTIONS
            OTHERS  = 01.
  CLEAR SY-SUBRC.

* Referenzangaben nur wenn gefüllt -----------------------------------*
  IF EKKO-ANGNR NE SPACE AND
     XLPET EQ SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'REFERENCE'
              WINDOW  = 'REFERENC'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Werksanschrift, wenn in allen Positionen gleich ---------------------*
  IF PEKKO-WERKS NE SPACE OR
     PEKKO-KUNNR NE SPACE OR
     PEKKO-ADRN2 NE SPACE OR
     PEKKO-EMLIF NE SPACE OR
     PEKKO-ADRNR NE SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'HEADER_DELADDRESS'
              WINDOW  = 'CONSGNEE'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Termine -------------------------------------------------------------*
  IF XLPET EQ SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = 'HEADER_DATES'
              WINDOW   = 'DELDATE'
              FUNCTION = 'APPEND'
         EXCEPTIONS
              OTHERS   = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Lieferdatum, wenn in allen Positionen gleich ------------------------*
  IF PEKKO-LFDAT NE SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = 'DELDATE'
              WINDOW   = 'DELDATE'
              FUNCTION = 'APPEND'
         EXCEPTIONS
              OTHERS   = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Bedingungen ---------------------------------------------------------*
  IF XLPET EQ SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'TERMS'
              WINDOW  = 'TERMS'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Unser Zeichen, wenn vorhanden ---------------------------------------*
  IF EKKO-UNSEZ NE SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = 'OUR_SIGN'
              WINDOW   = 'SIGN'
              FUNCTION = 'APPEND'
         EXCEPTIONS
              OTHERS   = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Main-Fenster --------------------------------------------------------*
* Bedingungen ---------------------------------------------------------*
  IF XLPET EQ SPACE.                   "für neues Formular
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'TERMS'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDIF.
* Zahlungsbedingungen -------------------------------------------------*
  IF XLPET EQ SPACE.
    CLEAR RM06P-PHTXT.
    LOOP AT ZBTXT.
      RM06P-PHTXT+14(50) = ZBTXT-LINE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TERMS_OF_PAYMENT'
           EXCEPTIONS
                OTHERS  = 01.
      CLEAR SY-SUBRC.
    ENDLOOP.
  ENDIF.

* Kopfzusatzinformationen ausgeben ------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'HEADER_INFO'
       EXCEPTIONS
            OTHERS  = 01.
  CLEAR SY-SUBRC.

* Zielwert, wenn vorhanden --------------------------------------------*
  IF EKKO-KTWRT NE 0 AND
     XLPET EQ SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'HEADER_TARGET_VALUE'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Kopftexte ausgeben -------------------------------------------------*
  LOOP AT XT166K.
    MOVE XT166K TO T166K.
    CASE T166K-TDOBJECT.
      WHEN 'EKKO'.
        T166K-TXNAM(10)   = EKKO-EBELN.
      WHEN 'LFA1'.
        T166K-TXNAM(10)   = EKKO-LIFNR.
      WHEN 'LFM1'.
        T166K-TXNAM(10)   = EKKO-LIFNR.
        T166K-TXNAM+10(4) = EKKO-EKORG.
    ENDCASE.
    PERFORM LESEN_TTXIT USING XT166K-TITDR XT166K-TDOBJECT XT166K-TDID.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'HEADER_TEXT'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDLOOP.

* Änderungshinweise ausgeben -----------------------------------------*
  LOOP AT XAEND WHERE EBELP EQ '00000'.
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

* Auftragsbestätigungspflicht, wenn in allen Positionen gleich --------*
  IF PEKKO-KZABS NE SPACE AND
     PEKKO-LABNR EQ SPACE AND
     XDRUVO NE AUFB.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'HEADER_INFO_ACKNOW'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Absagegrund ausgeben

  IF EKKO-ABSGR NE SPACE.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'REASON_FOR_REJECTION'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDIF.


ENDFORM.
