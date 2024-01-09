**********************************************************************
*                                                                    *
*     Formroutinenpool für Reports der Bestandsführung, Reservierung *
*     und Inventur.                                                  *
*                                                                    *
**********************************************************************
TABLES: T001,
        T001W,
        T001K,
        T064B,
        T064T,
        V134W,
        T156,
        T156T,
        T418,
        MARA,
        MARAV,
        MARV,
        MARC,
        MARD,
        MAKT,
        MABDR,
        MBEFU,
        MKPF,
        MKOL,
        MKOP,
        MSKU,
        MSLB,
        MSEG,
        IKPF,
        RKPF,
        RESB,
        MTCOR,
        MTCOM,
        EKBE,
        V_MMIM_MKP.

DATA: CONV_BUD    LIKE MKPF-BUDAT,
      ABGANG,
      KZ_SELECT,
      INDEX_T LIKE SY-TABIX,
      RMAX LIKE SY-TABIX VALUE 20,
      ZAEHLER LIKE SY-TABIX,
      MAX_ZAEHLER LIKE SY-TABIX.

DATA: BEGIN OF DUMMY OCCURS 1.
DATA:   DUMMY.
DATA: END OF DUMMY.

DATA: BEGIN OF YISEG OCCURS 5.
        INCLUDE STRUCTURE ISEG.
DATA: BTEXT LIKE T064B-BTEXT.          "Bestandsartentext
DATA: BUPER LIKE AM07M-BUPEV.          "Buchungsperiode
DATA: BWKEY LIKE VM07I-BWKEY.          "Bewertungsebene
DATA: BWTAR LIKE VM07I-BWTAR.          "Bewertungsart
DATA: DIFMG LIKE VM07I-DIFMG.          "Differenzmenge
DATA: DSTAT LIKE IKPF-DSTAT.           "Ausbuchstatus
DATA: GIDAT LIKE IKPF-GIDAT.           "Geplantes AufDat
DATA: MAKTX LIKE MAKT-MAKTX.           "Materialkurztext
DATA: MONAT LIKE IKPF-MONAT.
DATA: STEXT LIKE T064T-STEXT.          "Status d. Position
DATA: VGART LIKE IKPF-VGART.           "Vorgangsart
DATA: VGARI LIKE IKPF-VGART.           "Vorgangsart
DATA: XRUEM.
DATA: XDELE.
DATA: ADIFF LIKE ISEG-DMBTR.           "absolute difference
DATA: END OF YISEG.

DATA: BEGIN OF IMKPF OCCURS 100.
        INCLUDE STRUCTURE MKPF.
DATA: END OF IMKPF.

DATA: BEGIN OF XMKPF OCCURS 0.
        INCLUDE STRUCTURE V_MMIM_MKP.
DATA: END OF XMKPF.

DATA: BEGIN OF IMSEG OCCURS 100.
        INCLUDE STRUCTURE MSEG.
DATA: END OF IMSEG.

DATA: BEGIN OF IIKPF OCCURS 100.
        INCLUDE STRUCTURE IKPF.
DATA: END OF IIKPF.

DATA: BEGIN OF IRKPF OCCURS 100.
        INCLUDE STRUCTURE RKPF.
DATA: END OF IRKPF.

DATA: BEGIN OF IMAKT OCCURS 100.
        INCLUDE STRUCTURE MAKT.
DATA: END OF IMAKT.

DATA: BEGIN OF IMARA OCCURS 100.
        INCLUDE STRUCTURE MARA.
DATA: END OF IMARA.

DATA: BEGIN OF IMARAV OCCURS 100.
        INCLUDE STRUCTURE MARAV.
DATA: END OF IMARAV.

DATA: BEGIN OF IMARC OCCURS 100.
        INCLUDE STRUCTURE MARC.
DATA: END OF IMARC.

DATA: BEGIN OF IMARD OCCURS 100.
        INCLUDE STRUCTURE MARD.
DATA: END OF IMARD.

DATA: BEGIN OF IMSKU OCCURS 100.
        INCLUDE STRUCTURE MSKU.
DATA: END OF IMSKU.

DATA: BEGIN OF IMSLB OCCURS 100.
        INCLUDE STRUCTURE MSLB.
DATA: END OF IMSLB.

DATA: BEGIN OF IMKOL OCCURS 100.
        INCLUDE STRUCTURE MKOL.
DATA: END OF IMKOL.

DATA: BEGIN OF IMKOP OCCURS 100.
        INCLUDE STRUCTURE MKOP.
DATA: END OF IMKOP.

DATA: BEGIN OF IEKBE OCCURS 100.
        INCLUDE STRUCTURE EKBE.
DATA: END OF IEKBE.

DATA: BEGIN OF XKOPF OCCURS 100,
        BELNR LIKE MKPF-MBLNR,
        HIGH  LIKE MKPF-MBLNR,
        MJAHR LIKE MSEG-MJAHR,
      END OF XKOPF.

DATA: BEGIN OF XPOS  OCCURS 100,
        BELNR LIKE MKPF-MBLNR,
        HIGH  LIKE MKPF-MBLNR,
      END OF XPOS.

DATA: BEGIN OF XRKOPF OCCURS 100,
        RSNUM LIKE RKPF-RSNUM,
      END OF XRKOPF.

DATA: BEGIN OF XMATN OCCURS 100,
        MATNR LIKE MSEG-MATNR,
      END OF XMATN.

DATA:BEGIN OF IRESB OCCURS 0.
        INCLUDE STRUCTURE RESB.
DATA: END OF IRESB.

DATA:    BEGIN OF YDM07I OCCURS 0.
        INCLUDE STRUCTURE DM07I.
DATA:    END OF YDM07I.
DATA:   OPTION(2)       TYPE C.
DATA:   RVGART LIKE MKPF-VGART.
RANGES: IMBLNR FOR  MKPF-MBLNR.
RANGES: IMBLNS FOR  MSEG-MBLNR.
RANGES: IMATNR FOR  MSEG-MATNR.
RANGES: IRSNUM FOR  RKPF-RSNUM.
RANGES: IEBELN FOR  EKBE-EBELN.
RANGES: IEBELX FOR  EKBE-EBELN.
RANGES  RVGARX FOR  IKPF-VGART.
RANGES: RMATNR FOR  RESB-MATNR.
RANGES: RWERKS  FOR  RESB-MATNR.
RANGES: RBDTER  FOR  RESB-BDTER.

*----  SQL-Routinen fuer Auswertungsreports in der Bestandsfuehrung ---*

*---------- Lesen Materialkurztext (Massenverarbeitung) --------------*
FORM IMAKT_FUELLEN.
  SELECT * FROM MAKT APPENDING TABLE IMAKT WHERE MATNR IN IMATNR
                                           AND   SPRAS EQ SY-LANGU.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM KURZTEXT_LESEN_ARRAY                                     *
*---------------------------------------------------------------------*
FORM KURZTEXT_LESEN_ARRAY.
  IMATNR-SIGN = 'I'.
  IMATNR-OPTION = 'EQ'.
  DESCRIBE TABLE XMATN LINES INDEX_T.
  LOOP AT XMATN.
    IMATNR-LOW = XMATN-MATNR.
    APPEND IMATNR.
    ZAEHLER = ZAEHLER + 1.
    MAX_ZAEHLER = MAX_ZAEHLER + 1.
    IF ZAEHLER = RMAX.
      PERFORM IMAKT_FUELLEN.
      REFRESH IMATNR.
      CLEAR ZAEHLER.
      IF MAX_ZAEHLER = INDEX_T.
        CLEAR MAX_ZAEHLER.
        CLEAR INDEX_T.
        EXIT.
      ENDIF.
    ENDIF.
    IF MAX_ZAEHLER = INDEX_T.
      PERFORM IMAKT_FUELLEN.
      REFRESH IMATNR.
      CLEAR ZAEHLER.
      CLEAR MAX_ZAEHLER.
      CLEAR INDEX_T.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------- Lesen des Reservierungsbelegkopf (Mengenverarbeitung) ----
FORM IRKPF_FUELLEN.
  SELECT * FROM RKPF APPENDING TABLE IRKPF WHERE RSNUM IN IRSNUM
                               ORDER BY PRIMARY KEY.
ENDFORM.
*---------------------------------------------------------------------

FORM RBELEGKOPF_LESEN_ARRAY.
  IRSNUM-SIGN = 'I'.
  IRSNUM-OPTION = 'EQ'.
  DESCRIBE TABLE XRKOPF LINES INDEX_T.
  LOOP AT XRKOPF.
    IRSNUM-LOW = XRKOPF-RSNUM.
    APPEND IRSNUM.
    ZAEHLER = ZAEHLER + 1.
    MAX_ZAEHLER = MAX_ZAEHLER + 1.
    IF ZAEHLER = RMAX.
      PERFORM IRKPF_FUELLEN.
      REFRESH IRSNUM.
      CLEAR ZAEHLER.
      IF MAX_ZAEHLER = INDEX_T.
        CLEAR MAX_ZAEHLER.
        CLEAR INDEX_T.
        EXIT.
      ENDIF.
    ENDIF.
    IF MAX_ZAEHLER = INDEX_T.
      PERFORM IRKPF_FUELLEN.
      REFRESH IRSNUM.
      CLEAR ZAEHLER.
      CLEAR MAX_ZAEHLER.
      CLEAR INDEX_T.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.
*---------- Lesen des Materialbelegkopf_Views  -----------------------
FORM VMKPF_FUELLEN.
  SELECT * FROM V_MMIM_MKP APPENDING TABLE XMKPF WHERE MBLNR IN IMBLNR
                               order by mblnr mjahr.             "4.6B
*                              ORDER BY PRIMARY KEY.             "4.6B
ENDFORM.

*---------- Lesen des Materialbelegkopf (Mengenverarbeitung)  --------
FORM IMKPF_FUELLEN.
  SELECT * FROM MKPF APPENDING TABLE IMKPF WHERE MBLNR IN IMBLNR
                               ORDER BY PRIMARY KEY.
ENDFORM.

*---------- Lesen des Materialbelegkopf mit Vorgangsart      ---------
FORM RMKPF_FUELLEN.
  SELECT * FROM MKPF APPENDING TABLE IMKPF WHERE MBLNR IN IMBLNR
                                             AND VGART IN RVGARX
                               ORDER BY PRIMARY KEY.
ENDFORM.

*---------- Lesen des Inventurbelegkopf (Mengenverarbeitung) ---------
FORM IIKPF_FUELLEN.
  SELECT * FROM IKPF APPENDING TABLE IIKPF WHERE IBLNR IN IMBLNR
                               ORDER BY PRIMARY KEY.
ENDFORM.

*-------- Rangetabelle zum Lesen der Belegkoepfe fuellen -------------
FORM BELEGKOPF_LESEN_ARRAY USING INT_TAB.
  IMBLNR-SIGN = 'I'.
  IF INT_TAB = 'RMKPF' OR INT_TAB = 'XMKPF'.
    IMBLNR-OPTION = OPTION.
  ELSE.
    IMBLNR-OPTION = 'EQ'.
  ENDIF.
  DESCRIBE TABLE XKOPF LINES INDEX_T.
  LOOP AT XKOPF.
    IMBLNR-LOW = XKOPF-BELNR.
    IF INT_TAB = 'RMKPF' OR INT_TAB = 'XMKPF'.
      IMBLNR-HIGH = XKOPF-HIGH.
    ENDIF.
    APPEND IMBLNR.
    ZAEHLER = ZAEHLER + 1.
    MAX_ZAEHLER = MAX_ZAEHLER + 1.
    IF ZAEHLER = RMAX.
      CASE INT_TAB.
        WHEN 'IMKPF'.
          PERFORM IMKPF_FUELLEN.
        WHEN 'RMKPF'.
          PERFORM RMKPF_FUELLEN.
        WHEN 'IIKPF'.
          PERFORM IIKPF_FUELLEN.
        WHEN 'VMKPF'.
          PERFORM VMKPF_FUELLEN.
        WHEN 'XMKPF'.
          PERFORM VMKPF_FUELLEN.
      ENDCASE.
      REFRESH IMBLNR.
      CLEAR ZAEHLER.
      IF MAX_ZAEHLER = INDEX_T.
        CLEAR MAX_ZAEHLER.
        CLEAR INDEX_T.
        EXIT.
      ENDIF.
    ENDIF.
    IF MAX_ZAEHLER = INDEX_T.
      CASE INT_TAB.
        WHEN 'IMKPF'.
          PERFORM IMKPF_FUELLEN.
        WHEN 'RMKPF'.
          PERFORM RMKPF_FUELLEN.
        WHEN 'IIKPF'.
          PERFORM IIKPF_FUELLEN.
        WHEN 'VMKPF'.
          PERFORM VMKPF_FUELLEN.
        WHEN 'XMKPF'.
          PERFORM VMKPF_FUELLEN.
      ENDCASE.
      REFRESH IMBLNR.
      CLEAR ZAEHLER.
      CLEAR MAX_ZAEHLER.
      CLEAR INDEX_T.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.

*----  Lesen des Textes zur Bewegungsart                            ---*
FORM BWART_TEXT USING LSB_BWART
                      LSB_SOBKZ
                      LSB_KZBEW
                      LSB_KZZUG
                      LSB_KZVBR.
  SELECT SINGLE * FROM T156 WHERE BWART = LSB_BWART.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E001 WITH '156' LSB_BWART.
  ENDIF.
  SELECT SINGLE * FROM T156T WHERE SPRAS = SY-LANGU
                             AND   BWART = LSB_BWART
                             AND   SOBKZ = LSB_SOBKZ
                             AND   KZBEW = LSB_KZBEW
                             AND   KZZUG = LSB_KZZUG
                             AND   KZVBR = LSB_KZVBR.
ENDFORM.

*----  Lesen des Bestandsartentextes für die Inventurpositionen     ---*
FORM BSTAR_LESEN USING L_BSTAR.
  SELECT SINGLE * FROM T064B WHERE SPRAS = SY-LANGU
                             AND   BSTAR = L_BSTAR.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E001 WITH '064B' L_BSTAR.
  ENDIF.
ENDFORM.

*----  Lesen des Inventurstatustextes für die Inventurpositionen    ---*
FORM T064T_LESEN.
  SELECT SINGLE * FROM T064T WHERE SPRAS = SY-LANGU
                             AND   XZAEL = YISEG-XZAEL
                             AND   XDIFF = YISEG-XDIFF
                             AND   XNZAE = YISEG-XNZAE
                             AND   XLOEK = YISEG-XLOEK.
ENDFORM.

*----  Lesen des Materialkurztextes                                 ---*
FORM KURZTEXT_LESEN USING LSK_MATNR.
  CLEAR MTCOM.
  MTCOM-KENNG = 'MAKT'.
  MTCOM-MATNR = LSK_MATNR.
  MTCOM-SPRAS = SY-LANGU.
  MTCOM-NOMUS = 'X'.
  CALL FUNCTION 'MATERIAL_READ'
       EXPORTING
            SCHLUESSEL = MTCOM
       IMPORTING
            MATDATEN   = MAKT
            RETURN     = MTCOR
       TABLES
            SEQMAT01   = DUMMY.
ENDFORM.

*---- Lesen d. Matktxt. u. d. Lagerplzbeschr. f.Invzählbelegdruck   ---*
FORM MATERIALINFO_LESEN USING LSK_MATNR LSK_WERKS LSK_LGORT.
  CLEAR MTCOM.
  MTCOM-KENNG = 'MABDR'.
  MTCOM-MATNR = LSK_MATNR.
  MTCOM-WERKS = LSK_WERKS.
  MTCOM-LGORT = LSK_LGORT.
  MTCOM-SPRAS = SY-LANGU.
  MTCOM-NOMUS = 'X'.
  CALL FUNCTION 'MATERIAL_READ'
       EXPORTING
            SCHLUESSEL = MTCOM
       IMPORTING
            MATDATEN   = MABDR
            RETURN     = MTCOR
       TABLES
            SEQMAT01   = DUMMY.
ENDFORM.

*----  Lesen der Werkstabelle zum ermitteln des Werkstextes         ---*
FORM WERK_LESEN USING LSW_WERKS.
  SELECT SINGLE * FROM T001W WHERE WERKS = LSW_WERKS.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E001 WITH '001W' LSW_WERKS.
  ENDIF.
  PERFORM BWKEY_LESEN USING LSW_WERKS.
ENDFORM.

*----  Lesen des Bewertungs- und Buchungskreises                    ---*
FORM BWKEY_LESEN USING LSW_WERKS.
  SELECT SINGLE * FROM T001K WHERE BWKEY = T001W-BWKEY.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E001 WITH '001K' T001W-BWKEY.
  ENDIF.
  SELECT SINGLE * FROM T001 WHERE BUKRS = T001K-BUKRS.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E001 WITH '001' T001K-BWKEY.
  ENDIF.
ENDFORM.

*----  Lesen des Materialverwaltungssatzes                          ---*
FORM MARV_LESEN USING MARVLS_BUKRS.
  SELECT SINGLE * FROM MARV WHERE BUKRS = MARVLS_BUKRS.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E001 WITH 'MARV' MARVLS_BUKRS.
  ENDIF.
ENDFORM.

*----  Lesen des Materialbewertungssatzes                           ---*
FORM MBEW_LESEN USING L_MATNR L_BWKEY L_BWTAR.
  CLEAR MTCOM.
  MTCOM-KENNG = 'MBEFU'.
  MTCOM-MATNR = L_MATNR.
  MTCOM-BWKEY = L_BWKEY.
  MTCOM-BWTAR = L_BWTAR.
  MTCOM-NOMUS = X.
  CALL FUNCTION 'MATERIAL_READ'
       EXPORTING
            SCHLUESSEL = MTCOM
       IMPORTING
            MATDATEN   = MBEFU
            RETURN     = MTCOR
       TABLES
            SEQMAT01   = DUMMY.
  IF MTCOR-RMBEW NE SPACE.
    MESSAGE E001 WITH 'MBEW' L_MATNR L_BWKEY.
  ENDIF.
ENDFORM.

*--------------- Lesen Materialbewertungssatz mit Messagehandling --*
FORM MBEW_LESEN_RC USING L_MATNR L_BWKEY L_BWTAR L_RC.
  CLEAR MTCOM.
  MTCOM-KENNG = 'MBEFU'.
  MTCOM-MATNR = L_MATNR.
  MTCOM-BWKEY = L_BWKEY.
  MTCOM-BWTAR = L_BWTAR.
  MTCOM-NOMUS = X.
  CALL FUNCTION 'MATERIAL_READ'
       EXPORTING
            SCHLUESSEL = MTCOM
       IMPORTING
            MATDATEN   = MBEFU
            RETURN     = MTCOR
       TABLES
            SEQMAT01   = DUMMY.
  IF MTCOR-RMBEW NE SPACE.
    L_RC = 4.
  ENDIF.
ENDFORM.

*----  Ermitteln des Inventurdifferenzwertes über SAPLMBGB          ---*
FORM WERT_ERMITTELN.
  CASE MBEFU-VPRSV.
    WHEN S.
      IF YISEG-MENGE < YISEG-BUCHM.
        IF YISEG-XRUEM IS INITIAL.
          PERFORM WERTERMITTLUNG_S_ABGANG(SAPLMBGB) USING
                                                    MBEFU-SALK3
                                                    MBEFU-STPRS
                                                    MBEFU-LBKUM
                                                    MBEFU-PEINH
                                                    YISEG-DIFMG
                                                    YISEG-DMBTR.
        ELSE.
          PERFORM WERTERMITTLUNG_S_ABGANG(SAPLMBGB) USING
                                                        MBEFU-VMSAL
                                                        MBEFU-VMSTP
                                                        MBEFU-VMKUM
                                                        MBEFU-VMPEI
                                                        YISEG-DIFMG
                                                        YISEG-DMBTR.
        ENDIF.
      ELSE.
        IF YISEG-XRUEM IS INITIAL.
          PERFORM WERTERMITTLUNG_S_ZUGANG(SAPLMBGB) USING
                                                    MBEFU-STPRS
                                                    MBEFU-PEINH
                                                    YISEG-DIFMG
                                                    YISEG-DMBTR.
        ELSE.
          PERFORM WERTERMITTLUNG_S_ZUGANG(SAPLMBGB) USING
                                                    MBEFU-VMSTP
                                                    MBEFU-VMPEI
                                                    YISEG-DIFMG
                                                    YISEG-DMBTR.
        ENDIF.
      ENDIF.
    WHEN V.
      IF YISEG-XRUEM IS INITIAL.
        PERFORM WA_V_ERRECHNEN(SAPLMBGB) USING MBEFU-SALK3
                                               MBEFU-VERPR
                                               MBEFU-LBKUM
                                               MBEFU-PEINH
                                               YISEG-DIFMG
                                               YISEG-DMBTR.
      ELSE.
        PERFORM WA_V_ERRECHNEN(SAPLMBGB) USING MBEFU-VMSAL
                                               MBEFU-VMVER
                                               MBEFU-VMKUM
                                               MBEFU-VMPEI
                                               YISEG-DIFMG
                                               YISEG-DMBTR.
      ENDIF.
  ENDCASE.
ENDFORM.

*----------------- Ermitteln Bestandswerte ---------------------------*
FORM WERT_ERMITTELN1 USING X_BEST
                           X_WERT.
  CASE MBEFU-VPRSV.
    WHEN S.
      PERFORM WERTERMITTLUNG_S_ABGANG(SAPLMBGB) USING
                                                MBEFU-SALK3
                                                MBEFU-STPRS
                                                MBEFU-LBKUM
                                                MBEFU-PEINH
                                                     X_BEST
                                                     X_WERT.
    WHEN V.
      PERFORM WA_V_ERRECHNEN(SAPLMBGB) USING MBEFU-SALK3
                                             MBEFU-VERPR
                                             MBEFU-LBKUM
                                             MBEFU-PEINH
                                                  X_BEST
                                                  X_WERT.
  ENDCASE.
ENDFORM.

*-------------------- Ermitteln Materialart ---------------------------*
FORM MTART_ERMITTELN USING MATNR WERKS.
  SELECT SINGLE * FROM MARA WHERE MATNR = MATNR.
  SELECT SINGLE * FROM V134W WHERE WERKS = WERKS
                             AND   MTART = MARA-MTART.
ENDFORM.

*-------------------- Positionstyp in Reservierungsposition prüfen ----*
FORM RES_POSTYP_PRUEFEN USING POSTP
                              KZBSF.
  SELECT SINGLE * FROM T418 WHERE POSTP = POSTP.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE E001 WITH '418' POSTP.
  ENDIF.
  KZBSF = T418-KZBSF.
ENDFORM.

*eject

* &002 Begin:  CSP.Meldung bearbeitet: Datum der letzen Inventur wurde
*              nicht gefüllt. Material muß gelesen werden


*---------------------------------------------------------------------*
*       FORM MATERIAL_LESEN                                           *
*---------------------------------------------------------------------*
*       Der Materialstamm wird gelesen mit allen abhängigen Segmenten.*
*---------------------------------------------------------------------*
FORM MATERIAL_LESEN .

  CLEAR MTCOM.
  MOVE: YISEG-MATNR TO MTCOM-MATNR,
        'MBEFU'    TO MTCOM-KENNG,
        SY-LANGU   TO MTCOM-SPRAS,
        IKPF-WERKS TO MTCOM-WERKS,
        IKPF-LGORT TO MTCOM-LGORT.
  IF NOT YISEG-CHARG IS INITIAL.
    MOVE YISEG-CHARG TO MTCOM-CHARG.
  ENDIF.
  CASE IKPF-SOBKZ.
    WHEN E.
      MTCOM-SOBKZ = IKPF-SOBKZ.
      MTCOM-VBELN = YISEG-KDAUF.
      MTCOM-POSNR = YISEG-KDPOS.
    WHEN K.
      MTCOM-SOBKZ = IKPF-SOBKZ.
      MTCOM-LIFNR = YISEG-LIFNR.
    WHEN M.
      MTCOM-SOBKZ = IKPF-SOBKZ.
      MTCOM-LIFNR = YISEG-LIFNR.
    WHEN O.
      MTCOM-SOBKZ = IKPF-SOBKZ.
      MTCOM-LIFNR = YISEG-LIFNR.
    WHEN Q.
      MTCOM-SOBKZ = IKPF-SOBKZ.
      MTCOM-PSPNR = YISEG-PS_PSP_PNR.
    WHEN V.
      MTCOM-SOBKZ = IKPF-SOBKZ.
      MTCOM-KUNNR = YISEG-KUNNR.
    WHEN W.
      MTCOM-SOBKZ = IKPF-SOBKZ.
      MTCOM-KUNNR = YISEG-KUNNR.
  ENDCASE.
  MTCOM-XVKBW = T001K-XVKBW.


  CALL FUNCTION 'MATERIAL_READ'
       EXPORTING  SCHLUESSEL = MTCOM
       IMPORTING  MATDATEN   = MBEFU
                  RETURN     = MTCOR
       TABLES     SEQMAT01   = DUMMY.

ENDFORM.

*---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  DATUM_LETZTE_INV
*&---------------------------------------------------------------------*
FORM DATUM_LETZTE_INV.
  CASE IKPF-SOBKZ.
    WHEN E.
      IF NOT MBEFU-KADLL IS INITIAL AND
         MBEFU-KADLL NE SPACE.
        YDM07I-DLINL = MBEFU-KADLL.
      ENDIF.
    WHEN K.
      IF NOT MBEFU-KODLL IS INITIAL AND
         MBEFU-KODLL NE SPACE.
        YDM07I-DLINL = MBEFU-KODLL.
      ENDIF.
    WHEN M.
      IF NOT MBEFU-KODLL IS INITIAL AND
         MBEFU-KODLL NE SPACE.
        YDM07I-DLINL = MBEFU-KODLL.
      ENDIF.
    WHEN O.
      IF NOT MBEFU-LBDLL IS INITIAL AND
         MBEFU-LBDLL NE SPACE.
        YDM07I-DLINL = MBEFU-LBDLL.
      ENDIF.
    WHEN Q.
      IF NOT MBEFU-PRDLL IS INITIAL AND
         MBEFU-PRDLL NE SPACE.
        YDM07I-DLINL = MBEFU-PRDLL.
      ENDIF.
    WHEN V.
      IF NOT MBEFU-KUDLL IS INITIAL AND
         MBEFU-KUDLL NE SPACE.
        YDM07I-DLINL = MBEFU-KUDLL.
      ENDIF.
    WHEN W.
      IF NOT MBEFU-KUDLL IS INITIAL AND
         MBEFU-KUDLL NE SPACE.
        YDM07I-DLINL = MBEFU-KUDLL.
      ENDIF.
    WHEN OTHERS.
      IF YISEG-CHARG IS INITIAL.
        IF NOT MBEFU-DLINL IS INITIAL AND
           MBEFU-DLINL NE SPACE.
          YDM07I-DLINL = MBEFU-DLINL.
*       ELSE.
*         IF NOT YISEG-KWART IS INITIAL AND
*            NOT MBEFU-WLINL IS INITIAL AND
*            MBEFU-WLINL NE SPACE.
*             YDM07I-DLINL = MBEFU-WLINL.
*          ENDIF.
        ENDIF.
      ELSE.
        IF NOT MBEFU-CHDLL IS INITIAL AND
           MBEFU-CHDLL NE SPACE.
          YDM07I-DLINL = MBEFU-CHDLL.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    " DATUM_LETZTE_INV
* &002 end
