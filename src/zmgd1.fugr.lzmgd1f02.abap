************************************************************************
* Include LMGD1F01 - Formroutinen Steuerabwicklung
************************************************************************

*----------------------------------------------------------------------*
*       Form  OK_CODE_STEUERN                                          *
*----------------------------------------------------------------------*
FORM OK_CODE_STEUERN.

  CASE RMMZU-OKCODE.
*----- Verlassen des Bildes ------------------------------------------
       WHEN FCODE_BABA.
            CLEAR RMMG2-FLGSTEUER.     "Zurücksetzen Steuerflag
*----- Erste Seite - Steuern First Page ------------------------------
       WHEN FCODE_STFP.
            PERFORM FIRST_PAGE USING ST_ERSTE_ZEILE.
*----- Seite vor - Steuern Next Page ---------------------------------
       WHEN FCODE_STNP.
           PERFORM NEXT_PAGE USING ST_ERSTE_ZEILE ST_ZLEPROSEITE
                                   ST_LINES.
*----- Seite zurueck - Steuern Previous Page -------------------------
       WHEN FCODE_STPP.
            PERFORM PREV_PAGE USING ST_ERSTE_ZEILE ST_ZLEPROSEITE.
*----- Bottom - Steuern Last Page ------------------------------------
       WHEN FCODE_STLP.
           PERFORM LAST_PAGE USING ST_ERSTE_ZEILE ST_LINES
                                   ST_ZLEPROSEITE SPACE.
*----- SPACE - Enter -------------------------------------------------
       WHEN FCODE_SPACE.
            CLEAR RMMG2-FLGSTEUER.     "Zurücksetzen Steuerflag
* ---- Sonstige Funktionen wie Springen etc. --------------------------
       WHEN OTHERS.
            CLEAR RMMG2-FLGSTEUER.     "Zurücksetzen Steuerflag
  ENDCASE.

ENDFORM.                    " OK_CODE_STEUERN



