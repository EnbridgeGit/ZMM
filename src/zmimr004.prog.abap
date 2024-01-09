REPORT RM06EL00 NO STANDARD PAGE HEADING MESSAGE-ID ME.
************************************************************************
*        Anzeigen Einkaufsbelege zum Lieferanten                       *
************************************************************************
*----------------------------------------------------------------------*
*  Tabellen                                                            *
*----------------------------------------------------------------------*
INCLUDE FM06LTO1.

*----------------------------------------------------------------------*
*  Parameter und Select-Options                                        *
*----------------------------------------------------------------------*
SELECT-OPTIONS:
  EL_LIFNR  FOR  EKKO-LIFNR MEMORY ID LIF MATCHCODE OBJECT KRED,
  EL_EKORG  FOR  EKKO-EKORG MEMORY ID EKO.
SELECTION-SCREEN FUNCTION KEY 1.
PARAMETERS:
  EL_SELKB  LIKE EKKO-BSTYP NO-DISPLAY DEFAULT 'X',
  EL_SELKK  LIKE EKKO-BSTYP NO-DISPLAY DEFAULT 'X',
  EL_SELKL  LIKE EKKO-BSTYP NO-DISPLAY DEFAULT 'X',
  EL_SELKA  LIKE EKKO-BSTYP NO-DISPLAY DEFAULT 'X'.
INCLUDE FM06LCS1.
INCLUDE FM06LCS3.
INCLUDE FM06LCS4.
INCLUDE FM06LCS2.
INCLUDE ZNMIM008.                                         "JLEE 11/18/96
DATA:           Z_NETWR LIKE EKPO-NETWR.                  "JLEE 11/18/96

*----------------------------------------------------------------------*
*  Hilfsfelder                                                         *
*----------------------------------------------------------------------*
INCLUDE FM06LCEK.

*----------------------------------------------------------------------*
*  Intitialisierung                                                    *
*----------------------------------------------------------------------*
INITIALIZATION.
SET TITLEBAR '001'.
*erform anforderungsbild(sapfm06l) using el_selkb el_selkk "JLEE 11/18
*                                        el_selkl el_selka."JLEE 11/18
PERFORM ANFORDERUNGSBILD(ZNMIM007) USING EL_SELKB EL_SELKK "JLEE 11/18
                                         EL_SELKL EL_SELKA."JLEE 11/18

*----------------------------------------------------------------------*
*  Selektionsbild / Selection screen                                   *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON SELPA.
*  perform selpa_analyse(sapfm06l).                     "JLEE 11/18/96
   PERFORM SELPA_ANALYSE(ZNMIM007).                     "JLEE 11/18/96
AT SELECTION-SCREEN ON LISTU.
*  perform listumfang(sapfm06l) using listu.            "JLEE 11/18/96
   PERFORM LISTUMFANG(ZNMIM007) USING LISTU.            "JLEE 11/18/96

AT SELECTION-SCREEN.
   CALL FUNCTION 'ME_ITEM_CATEGORY_SELOPT_INPUT'
        TABLES
             EXT_PSTYP = S_PSTYP
             INT_PSTYP = R_PSTYP.
*  perform bstyp_bestimmen(sapfm06l) using el_selkb el_selkk "JLEE 11/18
*                                       el_selkl el_selka.   "JLEE 11/18
   PERFORM BSTYP_BESTIMMEN(ZNMIM007) USING EL_SELKB EL_SELKK "JLEE 11/18
                                        EL_SELKL EL_SELKA.   "JLEE 11/18

AT SELECTION-SCREEN OUTPUT.
*  perform modify_anfo(sapfm06l) using 'L'.             "JLEE 11/18/96
   PERFORM MODIFY_ANFO(ZNMIM007) USING 'L'.             "JLEE 11/18/96

*----------------------------------------------------------------------*
*  F4 auf dem Selektionsbild / F4 on the selection screen              *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PSTYP-LOW.

   CALL FUNCTION 'HELP_VALUES_EPSTP'
        EXPORTING
             PROGRAM = SY-CPROG
             DYNNR   = SY-DYNNR
             FIELDNAME = 'S_PSTYP-LOW'
*            BSART   =
*            BSTYP   =
        IMPORTING
             EPSTP   = S_PSTYP-LOW
        EXCEPTIONS
             OTHERS  = 1.

*----------------------------------------------------------------------*
*  Beginn der Selektion                                                *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*erform pf_status(sapfm06l) using el_selkb el_selkk     "JLEE 11/18/96
*                                 el_selkl el_selka.    "JLEE 11/18/96
PERFORM PF_STATUS(ZNMIM007) USING EL_SELKB EL_SELKK     "JLEE 11/18/96
                                  EL_SELKL EL_SELKA.    "JLEE 11/18/96
SET TITLEBAR '001'.
NOT_FOUND = 'X'.

*- Lesen Belegköpfe --------------------------------------------------*
SELECT * FROM EKKO
   WHERE LIFNR IN EL_LIFNR
     AND EKORG IN EL_EKORG
     AND BSTYP IN R_BSTYP
     AND BSART IN S_BSART
     AND EKGRP IN S_EKGRP
     AND EBELN IN S_EBELN
     AND RESWK IN S_RESWK
     AND BEDAT IN S_BEDAT
     AND ERNAM IN P_ERNAM                               "JLEE 11/18/96
   ORDER BY LIFNR BSTYP EBELN.

   Z_NETWR = 0.                                         "JLEE 11/18/96
   SELECT * FROM EKPO WHERE EBELN = EKKO-EBELN.         "JLEE 11/18/96
      Z_NETWR = Z_NETWR + EKPO-NETWR.                   "JLEE 11/18/96
   ENDSELECT.                                           "JLEE 11/18/96

   IF Z_NETWR IN P_NETWR.                               "JLEE 11/18/96
*  perform ekko_merken(sapfm06l).                       "JLEE 11/18/96
      PERFORM EKKO_MERKEN(ZNMIM007).                    "JLEE 11/18/96
   ENDIF.                                               "JLEE 11/18/96
ENDSELECT.

*- Köpfe verarbeiten -------------------------------------------------*
*- Köpfe verabeiten für letztes Belegnummernintervall ----------------*
*erform zugriff_kopf(sapfm06l).                         "JLEE 11/18/96
PERFORM ZUGRIFF_KOPF(ZNMIM007).                         "JLEE 11/18/96
*erform pos_nach_kopf(sapfm06l).                        "JLEE 11/18/96
PERFORM POS_NACH_KOPF(ZNMIM007).                        "JLEE 11/18/96

*----------------------------------------------------------------------*
*  Ende der Selektion                                                  *
*----------------------------------------------------------------------*
END-OF-SELECTION.

*erform end_of_selection(sapfm06l).                     "JLEE 11/18/96
PERFORM END_OF_SELECTION(ZNMIM007).                     "JLEE 11/18/96

WRITE /(81) SY-ULINE.

*----------------------------------------------------------------------*
*  OK-Code-Eingabe                                                     *
*----------------------------------------------------------------------*
AT USER-COMMAND.
*erform user_command(sapfm06l).                         "JLEE 11/18/96
PERFORM USER_COMMAND(ZNMIM007).                         "JLEE 11/18/96

*----------------------------------------------------------------------*
*  Seitenueberschrift                                                  *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
*erform top(sapfm06l).                                  "JLEE 11/18/96
PERFORM TOP(ZNMIM007).                                  "JLEE 11/18/96

TOP-OF-PAGE DURING LINE-SELECTION.
IF SY-PFKEY = 'EKAB'.
* perform ekab_top(sapfm06l).                           "JLEE 11/18/96
  PERFORM EKAB_TOP(ZNMIM007).                           "JLEE 11/18/96
ELSE.
* perform top(sapfm06l).                                "JLEE 11/18/96
  PERFORM TOP(ZNMIM007).                                "JLEE 11/18/96
ENDIF.
