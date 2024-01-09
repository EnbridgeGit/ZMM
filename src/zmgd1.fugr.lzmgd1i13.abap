*----------------------------------------------------------------------*
*   INCLUDE LMGD1I13                                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_PROW_SUB  INPUT
*&---------------------------------------------------------------------*
*       Analog GET_DATEN_SUB aber speziell für Prognosewerte         *
*----------------------------------------------------------------------*
MODULE GET_PROW_SUB INPUT.

  CHECK NOT ANZ_SUBSCREENS IS INITIAL.

  IF NOT KZ_EIN_PROGRAMM IS INITIAL.
    IF NOT KZ_BILDBEGINN IS INITIAL.
      CALL FUNCTION 'MAIN_PARAMETER_GET_BILDPAI_SUB'
           IMPORTING
                RMMZU_OKCODE  = RMMZU-OKCODE
                BILDFLAG      = BILDFLAG
                RMMG2_VB_KLAS = RMMG2-VB_KLAS.
      CLEAR SUB_ZAEHLER.
      CLEAR KZ_BILDBEGINN.
    ENDIF.
    SUB_ZAEHLER = SUB_ZAEHLER + 1.
  ENDIF.

  CHECK KZ_EIN_PROGRAMM IS INITIAL.

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

ENDMODULE.                             " GET_PROW_SUB  INPUT


*&---------------------------------------------------------------------*
*&      Module  SET_PROW_SUB  INPUT
*&---------------------------------------------------------------------*
*       Analog SET_DATEN_SUB aber speziell für Prognosewerte         *
*----------------------------------------------------------------------*
MODULE SET_PROW_SUB INPUT.

  IF ANZ_SUBSCREENS IS INITIAL.
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

  ELSEIF NOT KZ_EIN_PROGRAMM IS INITIAL.
    IF SUB_ZAEHLER EQ ANZ_SUBSCREENS.
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
    ENDIF.
  ELSE.
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
  ENDIF.

ENDMODULE.                             " SET_PROW_SUB  INPUT

*&---------------------------------------------------------------------*
*&      Module  TPROWF_UEBERNEHMEN  INPUT
*&---------------------------------------------------------------------*
*       Übernahme der eingegebenen Werte in die interne Tabelle TPROWF
*----------------------------------------------------------------------*
MODULE TPROWF_UEBERNEHMEN INPUT.

  IF SY-STEPL = 1.
    PW_BILDFLAG_OLD = BILDFLAG.
  ENDIF.

  PW_AKT_ZEILE = PW_ERSTE_ZEILE + SY-STEPL.
  READ TABLE TPROWF INDEX PW_AKT_ZEILE.
  MOVE RM03M-PRWRT TO TPROWF-PRWRT.
  MOVE RM03M-SAIIN TO TPROWF-SAIIN.
  MOVE RM03M-FIXKZ TO TPROWF-FIXKZ.
  IF ( RM03M-KOPRW = TPROWF-KOPRW ) AND
     ( ( NOT RM03M-ANTEI IS INITIAL ) OR
       ( NOT TPROWF-PRWRT IS INITIAL ) ).
*   TPROWF-KOPRW = TPROWF-PRWRT * RM03M-ANTEI / 100.
*     "/ 100" fällt weg wegen Fließkommaarithmetik
    TPROWF-KOPRW = TPROWF-PRWRT * RM03M-ANTEI.
  ELSE.
    TPROWF-KOPRW = RM03M-KOPRW.
  ENDIF.

  MODIFY TPROWF INDEX PW_AKT_ZEILE.  " alt: SY-TABIX

ENDMODULE.                             " TPROWF_UEBERNEHMEN  INPUT


*&---------------------------------------------------------------------*
*&      Module  ANZAHL_EINTRAEGE_PW  INPUT
*&---------------------------------------------------------------------*
MODULE ANZAHL_EINTRAEGE_PW INPUT.

* ------ Ermitteln Anzahl Prognoseeinträge ------------------------
* ------ hier zur Auswertung der Blätter-FCodes benötigt

    DESCRIBE TABLE TPROWF LINES PW_LINES.

ENDMODULE.                 " ANZAHL_EINTRAEGE_PW  INPUT


*&---------------------------------------------------------------------*
*&      Module  OKCODE_PROGNOSE  INPUT
*&---------------------------------------------------------------------*
MODULE OKCODE_PROGNOSE INPUT.

  IF NOT PW_BILDFLAG_OLD IS INITIAL AND
     ( RMMZU-OKCODE = FCODE_PWFP OR
       RMMZU-OKCODE = FCODE_PWPP OR
       RMMZU-OKCODE = FCODE_PWNP OR
       RMMZU-OKCODE = FCODE_PWLP  ).
    CLEAR RMMZU-OKCODE.
  ENDIF.

  PERFORM OK_CODE_PROGNOSE.

ENDMODULE.                 " OKCODE_PROGNOSE  INPUT

*----------------------------------------------------------------------*
*       Module  PRDAT_VORSCHLAGEN
*
* Ermittlung eines Default-Prognosedatums, falls Prognosewerte gepflegt
* wurden und MPOP-PRDAT aber noch initial ist.
*                           (neu zu 2.1D / K11K067178 / 17.01.94 / CH)
*mk/24.04.95: Die Ermittlung erfolgt wie bisher zum PAI des Prognose-
*wertebildes
*----------------------------------------------------------------------*
MODULE PRDAT_VORSCHLAGEN.

  PERFORM PRDAT_ERMITTELN.

ENDMODULE.                              "PRDAT_VORSCHLAGEN

