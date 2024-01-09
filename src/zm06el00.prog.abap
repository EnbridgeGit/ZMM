REPORT ZM06EL00 NO STANDARD PAGE HEADING MESSAGE-ID ME.
************************************************************************
*        Anzeigen Einkaufsbelege zum Lieferanten                       *
************************************************************************
*----------------------------------------------------------------------*
*  Tables                                                              *
*----------------------------------------------------------------------*
INCLUDE ZM06LT01.

*----------------------------------------------------------------------*
*  Parameter and Select-Options                                        *
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
INCLUDE ZM06LCS1.
INCLUDE ZM06LCS3.
INCLUDE ZM06LCS4.
INCLUDE ZM06LCS2.

*----------------------------------------------------------------------*
*  Auxialiary fields                                     *
*----------------------------------------------------------------------*
INCLUDE ZM06LCEK.

*----------------------------------------------------------------------*
*  Intitialization                                                     *
*----------------------------------------------------------------------*
INITIALIZATION.
SET TITLEBAR '001'.
PERFORM ANFORDERUNGSBILD(SAPFM06L) USING EL_SELKB EL_SELKK
                                         EL_SELKL EL_SELKA.

*----------------------------------------------------------------------*
*  Selektionsbild / Selection screen                                   *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON SELPA.
   PERFORM SELPA_ANALYSE(SAPFM06L).
AT SELECTION-SCREEN ON LISTU.
   PERFORM LISTUMFANG(SAPFM06L) USING LISTU.

AT SELECTION-SCREEN.
   CALL FUNCTION 'ME_ITEM_CATEGORY_SELOPT_INPUT'
        TABLES
             EXT_PSTYP = S_PSTYP
             INT_PSTYP = R_PSTYP.
   PERFORM BSTYP_BESTIMMEN(SAPFM06L) USING EL_SELKB EL_SELKK
                                        EL_SELKL EL_SELKA.

AT SELECTION-SCREEN OUTPUT.
   PERFORM MODIFY_ANFO(SAPFM06L) USING 'L'.   "modify screen for input

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
*  Beginn der Selektion/Start of Selection                             *
*----------------------------------------------------------------------*
START-OF-SELECTION.

PERFORM PF_STATUS(SAPFM06L) USING EL_SELKB EL_SELKK
                                  EL_SELKL EL_SELKA.
SET TITLEBAR '001'.
NOT_FOUND = 'X'.

*- Lesen Belegköpfe/Read Document Header -----------------------------*
SELECT * FROM EKKO
   WHERE LIFNR IN EL_LIFNR
     AND EKORG IN EL_EKORG
     AND BSTYP IN R_BSTYP
     AND BSART IN S_BSART
     AND EKGRP IN S_EKGRP
     AND EBELN IN S_EBELN
     AND RESWK IN S_RESWK
     AND BEDAT IN S_BEDAT
   ORDER BY LIFNR BSTYP EBELN.

*- Köpfe verarbeiten/Process the header-------------------------------*
   PERFORM EKKO_MERKEN(SAPFM06L).

ENDSELECT.

*- Köpfe verabeiten für letztes Belegnummernintervall ----------------*
PERFORM ZUGRIFF_KOPF(SAPFM06L).
PERFORM POS_NACH_KOPF(SAPFM06L).

*----------------------------------------------------------------------*
*  Ende der Selektion/end of selection                               *
*----------------------------------------------------------------------*
END-OF-SELECTION.

PERFORM END_OF_SELECTION(SAPFM06L).

WRITE /(81) SY-ULINE.

*----------------------------------------------------------------------*
*  OK-Code-Eingabe/ Enter OK-code                                      *
*----------------------------------------------------------------------*
AT USER-COMMAND.
PERFORM USER_COMMAND(SAPFM06L).

*----------------------------------------------------------------------*
*  Seitenueberschrift                                                  *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
PERFORM TOP(SAPFM06L).

TOP-OF-PAGE DURING LINE-SELECTION.
IF SY-PFKEY = 'EKAB'.
  PERFORM EKAB_TOP(SAPFM06L).
ELSE.
  PERFORM TOP(SAPFM06L).
ENDIF.
