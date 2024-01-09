*------------------------------------------------------------------
* Module Bestell_ltext_init.

* Initialisieren der Textparameter (auch für Änderungsbelege) für
* die Einkaufsbestelltexte

* Achtung: Dieser Modul wird nur durchlaufen, wenn noch gar kein
* Bild mit Langtexten prozessiert wurde, oder wenn vorher ein
* anderes Bild mit Langtexten bearbeitet wurde
* (Füllen der für alle Texte gemeinsamen Strukturen HTEXT, ICDTXT,
* LANGTEXTNAME, TEXT_TITLE etc., sowie der für alle Inlinetexte
* gemeinsamen Strukturen XTHEAD und ITHEAD)

* MK/1.2B (bisher Bestelltext in Retail-Standard-Sequenz nicht pflegbar)
* Da in der Artikelpflege auch die Materialnummer wechseln kann,
* muß auch bei Änderung der Materialnr die Initialisierung durchgeführt
*------------------------------------------------------------------
MODULE BESTELL_LTEXT_INIT OUTPUT.

  DATA: LVB_ISOLD LIKE SY-BINPT.    " note 593869
  DATA: LVB_AKTYP LIKE T130M-AKTYP. " note 593869
  DATA: LVB_RESET LIKE SY-BINPT.    " note 593869



  DATA : HELP_REF_MATNR LIKE MARA-MATNR.

* CHECK LANGTEXTBILD NE BESTELLTEXT_BILD.  mk/1.2B
  CHECK LANGTEXTBILD   NE BESTELLTEXT_BILD OR
        LANGTEXT_MATNR_BEST NE RMMG1-MATNR.

  IF LANGTEXT_MATNR_BEST NE RMMG1-MATNR.                         "160836
    REFRESH_TEXTEDIT_CONTROL = 'X'.
  ENDIF.
  LANGTEXT_MATNR_BEST = RMMG1-MATNR.
  LANGTEXTBILD = BESTELLTEXT_BILD.

* REFRESH_TEXTEDIT_CONTROL = 'X'.
  LONGTEXTCONTAINER = LONGTEXT_CONTAINER_BESTELL.
  HELP_REF_MATNR = RMMG1_REF-MATNR.
  READ TABLE PTAB WITH KEY TBNAM = 'MARA'.
  IF SY-SUBRC = 0.
    IF PTAB-BISTSTAT CA 'E'.
      CLEAR RMMG1_REF-MATNR.
    ENDIF.
  ENDIF.

* Materialfixierung für den alten Langtexteditor, note 593869
  CALL FUNCTION 'LTEXT_STEPLOOP'
    IMPORTING
      E_ISOLD = LVB_ISOLD.

  IF NOT LVB_ISOLD IS INITIAL.

    READ TABLE FAUSWTAB WITH KEY FNAME = 'RM03M-LTEX1'.

    IF SY-SUBRC = 0.
      IF FAUSWTAB-KZINP = 0.
        LVB_AKTYP = T130M-AKTYP.
        T130M-AKTYP = AKTYPA.
        LVB_RESET = 'X'.

        CALL FUNCTION 'UPDATE_AKTYP_MG19'
          EXPORTING
            P_AKTYP = AKTYPA.
      ENDIF.
    ENDIF.

  ENDIF.


  CALL FUNCTION 'LANGTEXT_INIT'
       EXPORTING
*           P_MATNR      = RMMG1-MATNR
             P_MATNR      = LANGTEXT_MATNR_BEST      "mk/1.2B
             P_AKTYP      = T130M-AKTYP
             P_TDID       = BESTELLTEXT_TDID
             LANGTEXTBILD = LANGTEXTBILD
             P_KZPROZ     = KZ_BEST_PROZ
             P_REF_MATNR  = RMMG1_REF-MATNR
             FLGNUMINT    = FLGNUMINT
             NEUFLAG      = NEUFLAG
             RMMG2_FLG_RETAIL = RMMG2-FLG_RETAIL            "mk/1.2B
      IMPORTING
           P_KZPROZ       = KZ_BEST_PROZ
           P_KZ_LANGTEXT  = RMMZU-MG_LANGTEX.
  RMMG1_REF-MATNR = HELP_REF_MATNR.

* Zurücksetzen des Aktyps auf den alten Wert, note 593869

  IF NOT LVB_RESET IS INITIAL.
    T130M-AKTYP = LVB_AKTYP.
    CLEAR LVB_RESET.
  ENDIF.


ENDMODULE.                    "BESTELL_LTEXT_INIT OUTPUT

*------------------------------------------------------------------
* Module Grundd_ltext_init.

* Initialisieren der Textparameter (auch für Änderungsbelege) für
* den Grunddatentext
* Achtung: Dieser Modul wird nur durchlaufen, wenn noch gar kein
* Bild mit Langtexten prozessiert wurde, oder wenn vorher ein
* anderes Bild mit Langtexten bearbeitet wurde
* (Füllen der für alle Texte gemeinsamen Strukturen HTEXT, ICDTXT,
* LANGTEXTNAME, TEXT_TITLE etc., sowie der für alle Inlinetexte
* gemeinsamen Strukturen XTHEAD und ITHEAD)

* MK/1.2B separates Retail-Modul für Grunddatentext in LMGD2O03
* wieder gelöscht, stattdessen Retail-Besonderheit direkt integriert.
* Da in der Artikelpflege auch die Materialnummer wechseln kann,
* muß auch bei Änderung der Materialnr die Initialisierung durchgeführt
*------------------------------------------------------------------
MODULE GRUNDD_LTEXT_INIT OUTPUT.

  DATA: LVG_ISOLD LIKE SY-BINPT.    " note 593869
  DATA: LVG_AKTYP LIKE T130M-AKTYP. " note 593869
  DATA: LVG_RESET LIKE SY-BINPT.    " note 593869

* CHECK LANGTEXTBILD NE GRUNDDTEXT_BILD.   mk/1.2B
  CHECK LANGTEXTBILD   NE GRUNDDTEXT_BILD OR
        LANGTEXT_MATNR_GRUN NE RMMG1-MATNR.
  IF LANGTEXT_MATNR_GRUN NE RMMG1-MATNR.
    REFRESH_TEXTEDIT_CONTROL = 'X'.
  ENDIF.
  LANGTEXT_MATNR_GRUN = RMMG1-MATNR.
  LANGTEXTBILD   = GRUNDDTEXT_BILD.

* REFRESH_TEXTEDIT_CONTROL = 'X'.
  LONGTEXTCONTAINER = LONGTEXT_CONTAINER_GRUNDD.

* Materialfixierung für den alten Langtexteditor, note 593869
  CALL FUNCTION 'LTEXT_STEPLOOP'
    IMPORTING
      E_ISOLD = LVG_ISOLD.

  IF NOT LVG_ISOLD IS INITIAL.

    READ TABLE FAUSWTAB WITH KEY FNAME = 'RM03M-LTEX1'.

    IF SY-SUBRC = 0.
      IF FAUSWTAB-KZINP = 0.
        LVG_AKTYP = T130M-AKTYP.
        T130M-AKTYP = AKTYPA.
        LVG_RESET = 'X'.

        CALL FUNCTION 'UPDATE_AKTYP_MG19'
          EXPORTING
            P_AKTYP = AKTYPA.
      ENDIF.
    ENDIF.

  ENDIF.
  CALL FUNCTION 'LANGTEXT_INIT'
       EXPORTING
*            P_MATNR      = RMMG1-MATNR          mk/1.2B
             P_MATNR      = LANGTEXT_MATNR_GRUN      "mk/1.2B
             P_AKTYP      = T130M-AKTYP
             P_TDID       = GRUNDDTEXT_TDID
             LANGTEXTBILD = LANGTEXTBILD
             P_KZPROZ     = KZ_GRUN_PROZ
             P_REF_MATNR  = RMMG1_REF-MATNR
             FLGNUMINT    = FLGNUMINT
             NEUFLAG      = NEUFLAG
            RMMG2_FLG_RETAIL = RMMG2-FLG_RETAIL             "mk/1.2B
       IMPORTING
             P_KZPROZ     = KZ_GRUN_PROZ
             P_KZ_LANGTEXT  = RMMZU-MG_LANGTEX.

* Zurücksetzen des Aktyps auf den alten Wert, note 593869

  IF NOT LVG_RESET IS INITIAL.
    T130M-AKTYP = LVG_AKTYP.
    CLEAR LVG_RESET.
  ENDIF.


ENDMODULE.                    "GRUNDD_LTEXT_INIT OUTPUT

*------------------------------------------------------------------
* Module Iverm_ltext_init.

* Initialisieren der Textparameter (auch für Änderungsbelege) für
* den Internen Vermerk
* Achtung: Dieser Modul wird nur durchlaufen, wenn noch gar kein
* Bild mit Langtexten prozessiert wurde, oder wenn vorher ein
* anderes Bild mit Langtexten bearbeitet wurde
* (Füllen der für alle Texte gemeinsamen Strukturen HTEXT, ICDTXT,
* LANGTEXTNAME, TEXT_TITLE etc., sowie der für alle Inlinetexte
* gemeinsamen Strukturen XTHEAD und ITHEAD)

* MK/1.2B (bisher Int.Vermerk in Retail-Standard-Sequenz nicht pflegbar)
* Da in der Artikelpflege auch die Materialnummer wechseln kann,
* muß auch bei Änderung der Materialnr die Initialisierung durchgeführt
*------------------------------------------------------------------
MODULE IVERM_LTEXT_INIT OUTPUT.

  DATA: LVI_ISOLD LIKE SY-BINPT.    " note 593869
  DATA: LVI_AKTYP LIKE T130M-AKTYP. " note 593869
  DATA: LVI_RESET LIKE SY-BINPT.    " note 593869


* CHECK LANGTEXTBILD NE IVERMTEXT_BILD.       "mk/1.2B
  CHECK LANGTEXTBILD   NE IVERMTEXT_BILD OR
        LANGTEXT_MATNR_IVER NE RMMG1-MATNR.
  IF LANGTEXT_MATNR_IVER NE RMMG1-MATNR.
    REFRESH_TEXTEDIT_CONTROL = 'X'.
  ENDIF.
  LANGTEXT_MATNR_IVER = RMMG1-MATNR.
  LANGTEXTBILD = IVERMTEXT_BILD.

* REFRESH_TEXTEDIT_CONTROL = 'X'.
  LONGTEXTCONTAINER = LONGTEXT_CONTAINER_IVERM.

* Materialfixierung für den alten Langtexteditor, note 593869
  CALL FUNCTION 'LTEXT_STEPLOOP'
    IMPORTING
      E_ISOLD = LVI_ISOLD.

  IF NOT LVI_ISOLD IS INITIAL.

    READ TABLE FAUSWTAB WITH KEY FNAME = 'RM03M-LTEX1'.

    IF SY-SUBRC = 0.
      IF FAUSWTAB-KZINP = 0.
        LVI_AKTYP = T130M-AKTYP.
        T130M-AKTYP = AKTYPA.
        LVI_RESET = 'X'.

        CALL FUNCTION 'UPDATE_AKTYP_MG19'
          EXPORTING
            P_AKTYP = AKTYPA.
      ENDIF.
    ENDIF.

  ENDIF.


  CALL FUNCTION 'LANGTEXT_INIT'
       EXPORTING
*            P_MATNR      = RMMG1-MATNR
             P_MATNR      = LANGTEXT_MATNR_IVER      "mk/1.2B
             P_AKTYP      = T130M-AKTYP
             P_TDID       = IVERMTEXT_TDID
             LANGTEXTBILD = LANGTEXTBILD
             P_KZPROZ     = KZ_IVER_PROZ
             P_REF_MATNR  = RMMG1_REF-MATNR
             FLGNUMINT    = FLGNUMINT
             NEUFLAG      = NEUFLAG
            RMMG2_FLG_RETAIL = RMMG2-FLG_RETAIL             "mk/1.2B
       IMPORTING
             P_KZPROZ     = KZ_IVER_PROZ
             P_KZ_LANGTEXT  = RMMZU-MG_LANGTEX.

* Zurücksetzen des Aktyps auf den alten Wert, note 593869

  IF NOT LVI_RESET IS INITIAL.
    T130M-AKTYP = LVI_AKTYP.
    CLEAR LVI_RESET.
  ENDIF.


ENDMODULE.                    "IVERM_LTEXT_INIT OUTPUT

*---------------------------------------------------------------------*
*  Module LTEXT_AUSGEBEN                                              *
*---------------------------------------------------------------------*
*       Falls die Sprache der aktuellen Zeile ungefüllt ist,          *
*       werden alle Bildschirmfelder initialisiert.                   *
*       Ansonsten lesen des Textes und versorgen der Bildschirmfelder.*
*       Setzen des Langtextkennzeichens, wenn mehr Textzeilen         *
*       als Inline-Zeilen vorhanden sind oder die ersten Textzeilen   *
*       nur teilweise in den Inline-Feldern ausgegeben werden können  *
*---------------------------------------------------------------------*
MODULE LTEXT_AUSGEBEN OUTPUT.
  CALL FUNCTION 'LTEXT_AUSGEBEN'
    IMPORTING
      P_SPRAS      = RM03M-SPRAS
      P_KZLTX      = RM03M-KZLTX
      P_LTEX1      = RM03M-LTEX1
      P_LTEX2      = RM03M-LTEX2
      P_LTEX3      = RM03M-LTEX3
      P_LTEX4      = RM03M-LTEX4
    TABLES                                                  "TF 4.6A
      TLINETAB_OUT = TLINETAB.                              "TF 4.6A

* Falls keine Berechtigung für zentrale Felder -> anzeigen oder ausbl.
*<<<<BEGIN OF INSERTION NOTE 216547<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  CALL FUNCTION 'LTEXT_STEPLOOP'
    IMPORTING
      E_ISOLD = GV_ISOLD.
  CHECK GV_ISOLD = 'X'.
*<<<<END OF INSERTION NOTE 216547<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  IF  RMMG2-MANBR NE SPACE             "neu zu 3.0F /Rt 1.2
  AND LANGTEXTBILD NE VERTRIEBSTEXT_BILD.
    IF RMMG2-MANBR = MANBR1.
      LOOP AT SCREEN.
        SCREEN-INPUT     = 0.
        SCREEN-REQUIRED  = 0.
        MODIFY SCREEN.
      ENDLOOP.
    ELSE.
      LOOP AT SCREEN.
        SCREEN-INVISIBLE = 1.
        SCREEN-ACTIVE    = 0.
        SCREEN-OUTPUT    = 0.
        SCREEN-INPUT     = 0.
        SCREEN-REQUIRED  = 0.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMODULE.                    "LTEXT_AUSGEBEN OUTPUT

*------------------------------------------------------------------
* Module Pruef_ltext_init.

* Initialisieren der Textparameter (auch für Änderungsbelege) für
* den Prüftext         (umgestellt zu 1.2)
* Achtung: Dieser Modul wird nur durchlaufen, wenn noch gar kein
* Bild mit Langtexten prozessiert wurde, oder wenn vorher ein
* anderes Bild mit Langtexten bearbeitet wurde
* (Füllen der für alle Texte gemeinsamen Strukturen HTEXT, ICDTXT,
* LANGTEXTNAME, TEXT_TITLE etc., sowie der für alle Inlinetexte
* gemeinsamen Strukturen XTHEAD und ITHEAD)

* MK/1.2B (bisher Prüftext in Retail-Standard-Sequenz nicht pflegbar)
* Da in der Artikelpflege auch die Materialnummer wechseln kann,
* muß auch bei Änderung der Materialnr die Initialisierung durchgeführt
*------------------------------------------------------------------
MODULE PRUEF_LTEXT_INIT OUTPUT.

  DATA: LVP_ISOLD LIKE SY-BINPT.    " note 593869
  DATA: LVP_AKTYP LIKE T130M-AKTYP. " note 593869
  DATA: LVP_RESET LIKE SY-BINPT.    " note 593869


* CHECK LANGTEXTBILD NE PRUEFTEXT_BILD.  mk/1.2B
  CHECK LANGTEXTBILD   NE PRUEFTEXT_BILD OR
        LANGTEXT_MATNR_PRUE NE RMMG1-MATNR.
  IF LANGTEXT_MATNR_PRUE NE RMMG1-MATNR.
    REFRESH_TEXTEDIT_CONTROL = 'X'.
  ENDIF.
  LANGTEXT_MATNR_PRUE = RMMG1-MATNR.
  LANGTEXTBILD = PRUEFTEXT_BILD.

* REFRESH_TEXTEDIT_CONTROL = 'X'.
  LONGTEXTCONTAINER = LONGTEXT_CONTAINER_PRUEF.

* Materialfixierung für den alten Langtexteditor, note 593869
  CALL FUNCTION 'LTEXT_STEPLOOP'
    IMPORTING
      E_ISOLD = LVP_ISOLD.

  IF NOT LVP_ISOLD IS INITIAL.

    READ TABLE FAUSWTAB WITH KEY FNAME = 'RM03M-LTEX1'.

    IF SY-SUBRC = 0.
      IF FAUSWTAB-KZINP = 0.
        LVP_AKTYP = T130M-AKTYP.
        T130M-AKTYP = AKTYPA.
        LVP_RESET = 'X'.

        CALL FUNCTION 'UPDATE_AKTYP_MG19'
          EXPORTING
            P_AKTYP = AKTYPA.
      ENDIF.
    ENDIF.

  ENDIF.


  CALL FUNCTION 'LANGTEXT_INIT'
       EXPORTING
*            P_MATNR      = RMMG1-MATNR
             P_MATNR      = LANGTEXT_MATNR_PRUE      "mk/1.2B
             P_AKTYP      = T130M-AKTYP
             P_TDID       = PRUEFTEXT_TDID
             LANGTEXTBILD = LANGTEXTBILD
             P_KZPROZ     = KZ_PRUE_PROZ
             P_REF_MATNR  = RMMG1_REF-MATNR
             FLGNUMINT    = FLGNUMINT
             NEUFLAG      = NEUFLAG
            RMMG2_FLG_RETAIL = RMMG2-FLG_RETAIL             "mk/1.2B
       IMPORTING
             P_KZPROZ     = KZ_PRUE_PROZ
             P_KZ_LANGTEXT  = RMMZU-MG_LANGTEX.

* Zurücksetzen des Aktyps auf den alten Wert, note 593869

  IF NOT LVP_RESET IS INITIAL.
    T130M-AKTYP = LVP_AKTYP.
    CLEAR LVP_RESET.
  ENDIF.

ENDMODULE.                    "PRUEF_LTEXT_INIT OUTPUT

*------------------------------------------------------------------
* Module Vertriebs_ltext_init.

* Initialisieren der Textparameter (auch für Änderungsbelege) für
* die Vertriebslangtexte

* Achtung: Dieser Modul wird nur durchlaufen, wenn noch gar kein
* Bild mit Langtexten prozessiert wurde, oder wenn vorher ein
* anderes Bild mit Langtexten bearbeitet wurde
* oder wenn vorher ein Org.ebenen-Wechsel stattgefunden hat. "BE/261095
* (Füllen der für alle Texte gemeinsamen Strukturen HTEXT, ICDTXT,
* LANGTEXTNAME, TEXT_TITLE etc., sowie der für alle Inlinetexte
* gemeinsamen Strukturen XTHEAD und ITHEAD)

* MK/1.2B (bisher Vertr.texte im Retail-Standard-Sequenz nicht pflegbar)
* Da in der Artikelpflege auch die Materialnummer wechseln kann,
* muß auch bei Änderung der Materialnr die Initialisierung durchgeführt
*------------------------------------------------------------------
MODULE VERTRIEBS_LTEXT_INIT OUTPUT.

  DATA: LVV_ISOLD LIKE SY-BINPT.    " note 593869
  DATA: LVV_AKTYP LIKE T130M-AKTYP. " note 593869
  DATA: LVV_RESET LIKE SY-BINPT.    " note 593869


*mk/1.2B Logik für VTL vereinheitlicht mit Logik für Matnr
*  IF LANGTXTVKORG NE RMMG1-VKORG OR    "Bei Wechsel der      "BE/261095
*    LANGTXTVTWEG NE RMMG1-VTWEG.      "Organisationsebene   "BE/261095
*    CLEAR LANGTEXTBILD.               "muß Vertriebstext    "BE/261095
* ENDIF.                               "neu gelesen werden   "BE/261095

* CHECK LANGTEXTBILD NE VERTRIEBSTEXT_BILD.         mk/1.2B
  CHECK LANGTEXTBILD   NE VERTRIEBSTEXT_BILD OR
        LANGTEXT_MATNR_VERT NE RMMG1-MATNR        OR
        LANGTXTVKORG NE RMMG1-VKORG OR
        LANGTXTVTWEG NE RMMG1-VTWEG.
  IF LANGTEXT_MATNR_VERT NE RMMG1-MATNR OR
     LANGTXTVKORG NE RMMG1-VKORG OR
     LANGTXTVTWEG NE RMMG1-VTWEG.
    REFRESH_TEXTEDIT_CONTROL = 'X'.
  ENDIF.

  LANGTEXT_MATNR_VERT = RMMG1-MATNR.        "mk/1.2B
  LANGTXTVKORG = RMMG1-VKORG.          "Zwischenspeichern    "BE/261095
  LANGTXTVTWEG = RMMG1-VTWEG.          "aktuellen Stand      "BE/261095
  LANGTEXTBILD = VERTRIEBSTEXT_BILD.

* REFRESH_TEXTEDIT_CONTROL = 'X'.
* Gleichbehandlung wie Bestelltexte, note 591778
  LONGTEXTCONTAINER = LONGTEXT_CONTAINER_VERTRIEBS.
  HELP_REF_MATNR = RMMG1_REF-MATNR.
  READ TABLE PTAB WITH KEY TBNAM = 'MVKE'.
  IF SY-SUBRC = 0.
    IF PTAB-BISTSTAT CA 'V'.
      CLEAR RMMG1_REF-MATNR.
    ENDIF.
  ENDIF.

* Materialfixierung für den alten Langtexteditor, note 593869
  CALL FUNCTION 'LTEXT_STEPLOOP'
    IMPORTING
      E_ISOLD = LVV_ISOLD.

  IF NOT LVV_ISOLD IS INITIAL.

    READ TABLE FAUSWTAB WITH KEY FNAME = 'RM03M-LTEX1'.

    IF SY-SUBRC = 0.
      IF FAUSWTAB-KZINP = 0.
        LVV_AKTYP = T130M-AKTYP.
        T130M-AKTYP = AKTYPA.
        LVV_RESET = 'X'.

        CALL FUNCTION 'UPDATE_AKTYP_MG19'
          EXPORTING
            P_AKTYP = AKTYPA.
      ENDIF.
    ENDIF.
  ENDIF.

* note 638371
  IF RMMG2-FLG_RETAIL = 'X'.
    CALL FUNCTION 'LANGTEXT_INIT'
      EXPORTING
        P_MATNR          = RMMG1-MATNR
        P_VKORG          = RMMG1-VKORG
        P_VTWEG          = RMMG1-VTWEG
        P_AKTYP          = T130M-AKTYP
        P_TDID           = VERTRIEBSTEXT_TDID
        LANGTEXTBILD     = LANGTEXTBILD
        P_KZPROZ         = KZ_VERT_PROZ
        P_REF_MATNR      = RMMG1_REF-MATNR
        P_REF_VKORG      = RMMG1-VKORG
        P_REF_VTWEG      = RMMG1-VTWEG
        FLGNUMINT        = FLGNUMINT
        NEUFLAG          = NEUFLAG
        RMMG2_FLG_RETAIL = RMMG2-FLG_RETAIL                 "mk/1.2B
      IMPORTING
        P_KZPROZ         = KZ_VERT_PROZ
        P_KZ_LANGTEXT    = RMMZU-MG_LANGTEX.
  ELSE.
    CALL FUNCTION 'LANGTEXT_INIT'
      EXPORTING
        P_MATNR          = RMMG1-MATNR
        P_VKORG          = RMMG1-VKORG
        P_VTWEG          = RMMG1-VTWEG
        P_AKTYP          = T130M-AKTYP
        P_TDID           = VERTRIEBSTEXT_TDID
        LANGTEXTBILD     = LANGTEXTBILD
        P_KZPROZ         = KZ_VERT_PROZ
        P_REF_MATNR      = RMMG1_REF-MATNR
        P_REF_VKORG      = RMMG1_REF-VKORG
        P_REF_VTWEG      = RMMG1_REF-VTWEG
        FLGNUMINT        = FLGNUMINT
        NEUFLAG          = NEUFLAG
        RMMG2_FLG_RETAIL = RMMG2-FLG_RETAIL                 "mk/1.2B
      IMPORTING
        P_KZPROZ         = KZ_VERT_PROZ
        P_KZ_LANGTEXT    = RMMZU-MG_LANGTEX.

  ENDIF.


* Gleichbehandlung wie Bestelltexte, note 591778
  RMMG1_REF-MATNR = HELP_REF_MATNR.

* Zurücksetzen des Aktyps auf den alten Wert, note 593869

  IF NOT LVV_RESET IS INITIAL.
    T130M-AKTYP = LVV_AKTYP.
    CLEAR LVV_RESET.
  ENDIF.

ENDMODULE.                    "VERTRIEBS_LTEXT_INIT OUTPUT

*------------------------------------------------------------------
* XTHEAD_AUFSETZEN
* Je Seite wird in der Tabelle XTHEAD der Langtexte aufgesetzt
*------------------------------------------------------------------
MODULE XTHEAD_AUFSETZEN OUTPUT.
  CALL FUNCTION 'XTHEAD_AUFSETZEN'.
ENDMODULE.                    "XTHEAD_AUFSETZEN OUTPUT

*---------------------------------------------------------------------*
*    XTHEAD_LESEN                                                     *
*---------------------------------------------------------------------*
*  Die interne Tabelle XTHEAD wird zur Anzeige am Bildschirm gelesen. *
*---------------------------------------------------------------------*
MODULE XTHEAD_LESEN OUTPUT.
  CALL FUNCTION 'XTHEAD_LESEN'
    IMPORTING
      P_KZSE1 = RM03M-KZSE1.
ENDMODULE.                    "XTHEAD_LESEN OUTPUT


*---------------------------------------------------------------------*
*    xthead_mehrere_screens
*---------------------------------------------------------------------*
*  Die interne Tabelle XTHEAD wird zur Anzeige am Bildschirm gelesen. *
*---------------------------------------------------------------------*
MODULE XTHEAD_MEHRERE_SCREENS OUTPUT.
  CALL FUNCTION 'XTHEAD_MEHRERE_SCREENS'
    IMPORTING
      P_SCREENS = RMMG2-KZLTXW.
ENDMODULE.                    "XTHEAD_MEHRERE_SCREENS OUTPUT


*---------------------------------------------------------------------*
*    XTHEAD_LEEREINTRAG                                               *
*---------------------------------------------------------------------*
*       Für den Fall, daß die Tabelle XTHEAD keine echten Einträge    *
*       (aktive Textheader) enthält, wird sichergestellt, daß         *
*       zumindest ein Leereintrag vorhanden ist.                      *
*       (notwendig beim Anlegen und Ändern, um Texte hinzuzufügen)    *
*       Ist bereits ein Leereintrag in der Tabelle XTHEAD vorhanden,  *
*       der aber als gelöscht markiert ist, wird lediglich das        *
*       Update-Kennz. geändert, ansonsten wird ein Eintrag in der     *
*       Tabelle XTHEAD ergänzt. Zusätzlich muß ein Eintrag in der     *
*       Tabelle ITHEAD ergänzt werden.                                *
*---------------------------------------------------------------------*
MODULE XTHEAD_LEEREINTRAG OUTPUT.

  CALL FUNCTION 'XTHEAD_LEEREINTRAG'
    IMPORTING
      TEXT_ANZAHL_OUT = ANZ_SPRACHEN.                       "TF 4.6A

ENDMODULE.                    "XTHEAD_LEEREINTRAG OUTPUT

*---------------------------------------------------------------------*
*       MODULE TEXTCTRL_PBO OUTPUT                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE TEXTCTRL_PBO OUTPUT.

*=Langtextpflege per Feldauswahl auf 'Ausgeblendet' gesetzt ?===========
  IF NOT LTEXT_INVISIBLE = 1.
*   Nein
*   TextEdit-Control-Objekt und entsprechendes Container-Objekt erzeugen
    IF <EDITOR_OBJ> IS INITIAL.
      CREATE OBJECT <TEXTEDIT_CUSTOM_CONTAINER>
          EXPORTING
              CONTAINER_NAME = LONGTEXTCONTAINER
*              LIFETIME       = cl_gui_control=>lifetime_dynpro
          EXCEPTIONS
              CNTL_ERROR = 1
              CNTL_SYSTEM_ERROR = 2
              CREATE_ERROR = 3
              LIFETIME_ERROR = 4
              LIFETIME_DYNPRO_DYNPRO_LINK = 5.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF ( LANGTEXTBILD = VERTRIEBSTEXT_BILD ).
        CALL FUNCTION 'CHECK_TEXT_OBJECT'
          EXPORTING
            OBJECT      = VERTRIEBSTEXT_OBJ
          IMPORTING
            OBJECT_INFO = OBJECT_INFO
          EXCEPTIONS
            OBJECT      = 1
            OTHERS      = 2.
        IF SY-SUBRC <> 0.
          WORDWRAP_POSITION = 72.
        ELSE.
          WORDWRAP_POSITION = OBJECT_INFO-TDLINESIZE.
        ENDIF.
      ELSE.
        CALL FUNCTION 'CHECK_TEXT_OBJECT'
          EXPORTING
            OBJECT      = LANGTEXT_OBJEKTID
          IMPORTING
            OBJECT_INFO = OBJECT_INFO
          EXCEPTIONS
            OBJECT      = 1
            OTHERS      = 2.
        IF SY-SUBRC <> 0.
          WORDWRAP_POSITION = 72.
        ELSE.
          WORDWRAP_POSITION = OBJECT_INFO-TDLINESIZE.
        ENDIF.
      ENDIF.

      CREATE OBJECT <EDITOR_OBJ>
            EXPORTING
             PARENT = <TEXTEDIT_CUSTOM_CONTAINER>
*             LIFETIME       =  CL_GUI_control=>LIFETIME_DYNPRO
            WORDWRAP_MODE = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
             WORDWRAP_POSITION = WORDWRAP_POSITION
             WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE.

      CALL FUNCTION 'APPEND_LANGTEXT_REFS'
        EXPORTING
          EDITOR_OBJ                = <EDITOR_OBJ>
          TEXTEDIT_CUSTOM_CONTAINER = <TEXTEDIT_CUSTOM_CONTAINER>.
      REFRESH_TEXTEDIT_CONTROL = 'X'.
    ENDIF.

*=Status der Dynproelemente setzen======================================
    CLEAR STATUSTEXT.
    IF ANZ_SPRACHEN > 0.
*===Es sind schon Sprachen vorhanden====================================
      IF T130M-AKTYP = AKTYPA OR T130M-AKTYP = AKTYPZ.
        TOOLBARMODE = <EDITOR_OBJ>->TRUE.
        READONLYMODE = <EDITOR_OBJ>->TRUE.
        LOOP AT SCREEN.
          CASE SCREEN-NAME.
            WHEN DESC_LANGU_LISTBOX.
              SCREEN-INPUT = 1.
              MODIFY SCREEN.
            WHEN 'TEAN'.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            WHEN 'LTEX'.
              SCREEN-INPUT = 1.
              MODIFY SCREEN.
            WHEN 'TELO'.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            WHEN 'NEXT_PAGE'.
              IF ANZ_SPRACHEN = 1.
                SCREEN-INPUT = 0.
              ELSE.
                SCREEN-INPUT = 1.
              ENDIF.
              MODIFY SCREEN.
            WHEN 'PREVIOUS_PAGE'.
              IF ANZ_SPRACHEN = 1.
                SCREEN-INPUT = 0.
              ELSE.
                SCREEN-INPUT = 1.
              ENDIF.
              MODIFY SCREEN.
          ENDCASE.
        ENDLOOP.
      ELSE.
        TOOLBARMODE = <EDITOR_OBJ>->TRUE.
        READONLYMODE = <EDITOR_OBJ>->FALSE.
        LOOP AT SCREEN.
          CASE SCREEN-NAME.
            WHEN DESC_LANGU_LISTBOX.
              SCREEN-INPUT = 1.
              MODIFY SCREEN.
            WHEN 'TEAN'.
              SCREEN-INPUT = 1.
              MODIFY SCREEN.
            WHEN 'LTEX'.
              SCREEN-INPUT = 1.
              MODIFY SCREEN.
            WHEN 'TELO'.
              SCREEN-INPUT = 1.
              MODIFY SCREEN.
            WHEN 'NEXT_PAGE'.
              IF ANZ_SPRACHEN = 1.
                SCREEN-INPUT = 0.
              ELSE.
                SCREEN-INPUT = 1.
              ENDIF.
              MODIFY SCREEN.
            WHEN 'PREVIOUS_PAGE'.
              IF ANZ_SPRACHEN = 1.
                SCREEN-INPUT = 0.
              ELSE.
                SCREEN-INPUT = 1.
              ENDIF.
              MODIFY SCREEN.
          ENDCASE.
        ENDLOOP.
        CALL METHOD <EDITOR_OBJ>->SET_FOCUS
          EXPORTING
            CONTROL = <EDITOR_OBJ>.
      ENDIF.
*=Es sind noch keine Sprachen vorhanden=================================
    ELSE.
      TOOLBARMODE = <EDITOR_OBJ>->FALSE.
      READONLYMODE = <EDITOR_OBJ>->TRUE.
      LOOP AT SCREEN.
        CASE SCREEN-NAME.
          WHEN DESC_LANGU_LISTBOX.
            SCREEN-INPUT = 0.
            MODIFY SCREEN.
          WHEN 'TEAN'.
            IF T130M-AKTYP = AKTYPA OR T130M-AKTYP = AKTYPZ.
              SCREEN-INPUT = 0.
            ELSE.
              SCREEN-INPUT = 1.
            ENDIF.
            MODIFY SCREEN.
          WHEN 'LTEX'.
            IF T130M-AKTYP = AKTYPA OR T130M-AKTYP = AKTYPZ.
              SCREEN-INPUT = 0.
            ELSE.
              SCREEN-INPUT = 1.
            ENDIF.
            MODIFY SCREEN.
          WHEN 'TELO'.
            SCREEN-INPUT = 0.
            MODIFY SCREEN.
          WHEN 'NEXT_PAGE'.
            SCREEN-INPUT = 0.
            MODIFY SCREEN.
          WHEN 'PREVIOUS_PAGE'.
            SCREEN-INPUT = 0.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
      IF NOT ( T130M-AKTYP = AKTYPA OR T130M-AKTYP = AKTYPZ ).
        SET CURSOR FIELD 'TEAN'.
      ENDIF.
    ENDIF.
    INCLUDE RSTXSCAD.
    DATA l_token type itf_token. CLEAR l_token.
    perform init_itf_scanner(rstxscan) using c_with_command_scan
                                             c_form_text.
    perform set_itf_text(rstxscan) using TLINETAB[].
    perform read_next_itf_token(rstxscan) changing l_token.
    WHILE l_token-code <> c_token_text_end.
      IF l_token-code <> c_token_element_begin AND
         l_token-code <> c_token_element_end AND
         NOT ( ( l_token-code = c_token_paragraph_begin OR
                 l_token-code = c_token_paragraph_end OR
                 l_token-code = c_token_line_begin OR
                 l_token-code = c_token_line_end ) AND
               ( l_token-string = '*' OR
                 l_token-string IS INITIAL ) ) AND
         l_token-code <> c_token_string.
        TOOLBARMODE = <EDITOR_OBJ>->TRUE.
        READONLYMODE = <EDITOR_OBJ>->TRUE.
        STATUSTEXT   = TEXT-076.
        EXIT.
      ENDIF.
      perform read_next_itf_token(rstxscan) changing l_token.
    ENDWHILE.

*   note 1148033 replace line format /: (command) and /* (comment) with
*   line format * (default paragraph) to enable display (no change!!)
*   in the textedit control.
    DATA: tlinetab_help LIKE tline OCCURS 0 WITH HEADER LINE.
    DATA: lv_last_format type TDFORMAT.
    CLEAR lv_last_format.
    tlinetab_help[] = TLINETAB[].

    LOOP AT tlinetab_help.
      IF tlinetab_help-tdformat = '/:' OR
         tlinetab_help-tdformat = '/*'.
        lv_last_format = tlinetab_help-tdformat.
        tlinetab_help-tdformat = '* '.
        MODIFY tlinetab_help.
        CONTINUE.
      ENDIF.
*     Set line format * also for the following line to get a line break.
*     This is only necessary for the line formats "  " and  "( " and "= "
      IF ( tlinetab_help-tdformat = '  ' OR
           tlinetab_help-tdformat = '( ' OR
           tlinetab_help-tdformat = '= ' )
      AND lv_last_format IS NOT INITIAL.
        tlinetab_help-tdformat = '* '.
        MODIFY tlinetab_help.
      ENDIF.
      CLEAR lv_last_format.
    ENDLOOP.

    DATA lt_x_datatab TYPE  TDTAB_X256.
    DATA lv_x_size TYPE  I.

    IF LINES( tlinetab_help ) > 0 AND READONLYMODE = <EDITOR_OBJ>->TRUE.
      CALL FUNCTION 'CONVERT_ITF_TO_ASCII'
        EXPORTING
          FORMATWIDTH             = 0
          LANGUAGE                = RM03M-SPRAS
          TABLETYPE               = 'BIN'
          REPLACE_SYMBOLS         = ' '
          REPLACE_SAPCHARS        = ' '
        IMPORTING
          X_DATATAB               = lt_x_datatab
          X_SIZE                  = lv_x_size
        TABLES
          ITF_LINES               = tlinetab_help
        EXCEPTIONS
          INVALID_TABLETYPE       = 1
          OTHERS                  = 2.

      CALL FUNCTION 'CONVERT_ASCII_TO_ITF'
        EXPORTING
          TABLETYPE               = 'BIN'
          X_DATATAB               = lt_x_datatab
          X_SIZE                  = lv_x_size
          LANGUAGE                = RM03M-SPRAS
        TABLES
          ITF_LINES               = tlinetab_help
        EXCEPTIONS
          INVALID_TABLETYPE       = 1
          MISSING_SIZE            = 2
          INVALID_SIZE            = 3
          OTHERS                  = 4.
    ENDIF.

*=Text enthält Formatinformation ?======================================
    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
         EXPORTING
              LANGUAGE    = RM03M-SPRAS
      TABLES
              ITF_TEXT    = tlinetab_help                   "note1148033
        TEXT_STREAM = TEXTSTREAM.

*=Attribute des TextEdit-Controls setzen================================
    CALL METHOD <EDITOR_OBJ>->SET_STATUS_TEXT
      EXPORTING
        STATUS_TEXT = STATUSTEXT.
    CALL METHOD <EDITOR_OBJ>->SET_TOOLBAR_MODE
      EXPORTING
        TOOLBAR_MODE = TOOLBARMODE.
    CALL METHOD <EDITOR_OBJ>->SET_READONLY_MODE
      EXPORTING
        READONLY_MODE = READONLYMODE.

*=Überprüfung, ob im Langtexteditor Änderungen vorgenommen wurden=======
    CALL FUNCTION 'GET_REFRESH_TEXTEDIT_CONTROL'
      CHANGING
        C_REFRESH_TEXTEDIT_CONTROL = REFRESH_TEXTEDIT_CONTROL.

*=Text an TextEdit-Control übergeben====================================
    IF REFRESH_TEXTEDIT_CONTROL   = 'X'.

      CALL METHOD <EDITOR_OBJ>->SET_TEXT_AS_STREAM
        EXPORTING
          TEXT            = TEXTSTREAM
        EXCEPTIONS
          ERROR_DP        = 1
          ERROR_DP_CREATE = 2
          OTHERS          = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CLEAR REFRESH_TEXTEDIT_CONTROL.
    ENDIF.


*=Flush ausführen=======================================================
* Flush nicht erforderlich, da vom System am Ende von PBO ausgeführt
*=Flush ausführen=======================================================

  ELSE.
*   Ja, LAngtextpflege per Feldauswahl ausgelendet
    LOOP AT SCREEN.
      SCREEN-INPUT = 0.
      SCREEN-REQUIRED = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.                             " TEXTCTRL_PBO  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  RM03M-KZSE1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE RM03M-KZSE1 OUTPUT.
  CALL FUNCTION 'LANGTEXT_KZSE1'
    EXPORTING
      P_KZSE1 = X.
ENDMODULE.                             " RM03M-KZSE1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DESC_LANGU_VOR_SETZEN  OUTPUT
*&---------------------------------------------------------------------*
*       Aufbau der Sprachtabellen, Initialisieren der ausgewählten
*       Sprache, Setzen der Anzeigeleiste für die Sprache über dem
*       TextControl
*----------------------------------------------------------------------*
MODULE DESC_LANGU_VOR_SETZEN OUTPUT.

*=Aufbau der Sprachtabellen und initialisieren der angewählten Sprache==
  CALL FUNCTION 'DESC_LANGU_SETZEN_VOR'
    EXPORTING
      DESC_LANGU_LISTBOX = DESC_LANGU_LISTBOX
    IMPORTING
      ANZ_SPRACHEN       = ANZ_SPRACHEN
      ACTIONCODE_OUT     = ACTIONCODE
    TABLES
      LANG_TC_TAB_OUT    = LANG_TC_TAB
    CHANGING
      DESC_LANGU         = <DESC_LANGU>.

*=Setzen der Anzeigeleiste für die Sprache über dem Textcontrol=========
  CLEAR LONGTEXT_MAINTAINED_TEXT.
  REFRESH LANG_TC_TAB_TC.
  LOOP AT LANG_TC_TAB.
    CLEAR LANG_TC_TAB_TC.
    MOVE-CORRESPONDING LANG_TC_TAB TO LANG_TC_TAB_TC.
    IF LANG_TC_TAB-SPRSL = <DESC_LANGU>.
      LANG_TC_TAB_TC-MARK = 'X'.
      TC_LONGTEXT_MARKEDLINE = SY-TABIX.
*     TEXTPOSI = 28 - ( STRLEN( LANG_TC_TAB-SPTXT ) - 1 ) / 2. "834476
      TEXTPOSI = 22 - ( STRLEN( LANG_TC_TAB-SPTXT ) - 1 ) / 2. "834476
      LONGTEXT_MAINTAINED_TEXT+TEXTPOSI = LANG_TC_TAB-SPTXT.
    ENDIF.
    APPEND LANG_TC_TAB_TC.
  ENDLOOP.

*=Refresh für das TextEdit-Contro aktivieren============================
  IF <RM03M_SPRAS> <> <DESC_LANGU>.
    REFRESH_TEXTEDIT_CONTROL = 'X'.
  ENDIF.

*=Merken der ausgewählten Sprache=======================================
  RM03M-SPRAS = <DESC_LANGU>.
  <RM03M_SPRAS> = <DESC_LANGU>.

ENDMODULE.                             " DESC_LANGU_VOR_SETZEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_TC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  INIT_TC_LONGTEXT  OUTPUT
*&---------------------------------------------------------------------*
*  Init TableControl for longtext screens
*----------------------------------------------------------------------*
MODULE INIT_TC_LONGTEXT OUTPUT.
  PERFORM INIT_TC_LONGTEXT.
ENDMODULE.                 " INIT_DESC_LANGU_TC_GDTXT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STARTEINTRAG  OUTPUT
*&---------------------------------------------------------------------*
*       TF 4.6A
*       Anlegen eines Eintrages in der Anmeldesprache, wenn beim
*       Betreten der Langtextpflege (Ändern, Anlegen) noch kein
*       Text vorhanden ist.
*----------------------------------------------------------------------*
MODULE STARTEINTRAG OUTPUT.
  IF ANZ_SPRACHEN = 0.
    IF T130M-AKTYP = AKTYPN OR
       T130M-AKTYP = AKTYPH OR
       T130M-AKTYP = AKTYPV.
      CALL FUNCTION 'GET_ACTIONCODE'
        IMPORTING
          ACTIONCODE_OUT = ACTIONCODE.
      IF NOT ACTIONCODE = OKCODE_TELO.
        CALL FUNCTION 'OKCODE_TEAN'
          EXPORTING
            DESC_LANGU_NEW_IN = SY-LANGU
            AUTOCREATE        = 'X'.
        ANZ_SPRACHEN = ANZ_SPRACHEN + 1.
        IF SY_LANGU_SPTXT IS INITIAL.
          SELECT SINGLE SPTXT FROM T002T INTO SY_LANGU_SPTXT
                                     WHERE SPRAS = SY-LANGU AND
                                     SPRSL = SY-LANGU.
        ENDIF.
        MESSAGE S630(MM) WITH SY_LANGU_SPTXT.
      ELSE.
        MESSAGE S629(MM).
      ENDIF.
    ENDIF.
  ENDIF.


ENDMODULE.                             " STARTEINTRAG  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_DYNPROPARAMS_GDTXT  OUTPUT
*&---------------------------------------------------------------------*
*       Setzen der dynprospezifischen Parameter
*----------------------------------------------------------------------*
MODULE SET_DYNPROPARAMS_GDTXT OUTPUT.
  DESC_LANGU_LISTBOX = DESC_LANGU_LISTBOX_GDTXT.
  ASSIGN DESC_LANGU_GDTXT TO <DESC_LANGU>.
  ASSIGN EDITOR_OBJ_GD TO <EDITOR_OBJ>.
  ASSIGN TEXTEDIT_CUSTOM_CONTAINER_GD TO <TEXTEDIT_CUSTOM_CONTAINER>.
  ASSIGN RM03M_SPRAS_GRUNDD TO <RM03M_SPRAS>.
ENDMODULE.                 " SET_LONGTEXTDYNPRO_PARAMS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DYNPROPARAMS_PRTXT  OUTPUT
*&---------------------------------------------------------------------*
*      Setzen der dynprospezifischen Parameter
*----------------------------------------------------------------------*
MODULE SET_DYNPROPARAMS_PRTXT OUTPUT.
  DESC_LANGU_LISTBOX = DESC_LANGU_LISTBOX_PRTXT.
  ASSIGN DESC_LANGU_PRTXT TO <DESC_LANGU>.
  ASSIGN EDITOR_OBJ_PR TO <EDITOR_OBJ>.
  ASSIGN TEXTEDIT_CUSTOM_CONTAINER_PR TO <TEXTEDIT_CUSTOM_CONTAINER>.
  ASSIGN RM03M_SPRAS_PRUEF TO <RM03M_SPRAS>.
ENDMODULE.                 " SET_LONGTEXTDYNPRO_PARAMS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DYNPROPARAMS_VERTRIEBS  OUTPUT
*&---------------------------------------------------------------------*
*       Setzen der dynprospezifischen Parameter
*----------------------------------------------------------------------*
MODULE SET_DYNPROPARAMS_VERTRIEBS OUTPUT.
  DESC_LANGU_LISTBOX = DESC_LANGU_LISTBOX_VERTRIEBS.
  ASSIGN DESC_LANGU_VERTRIEBS TO <DESC_LANGU>.
  ASSIGN EDITOR_OBJ_VE TO <EDITOR_OBJ>.
  ASSIGN TEXTEDIT_CUSTOM_CONTAINER_VE TO <TEXTEDIT_CUSTOM_CONTAINER>.
  ASSIGN RM03M_SPRAS_VERTRIEBS TO <RM03M_SPRAS>.
ENDMODULE.                 " SET_LONGTEXTDYNPRO_PARAMS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DYNPROPARAMS_BESTELL  OUTPUT
*&---------------------------------------------------------------------*
*       Setzen der dynprospezifischen Parameter
*----------------------------------------------------------------------*
MODULE SET_DYNPROPARAMS_BESTELL OUTPUT.
  DESC_LANGU_LISTBOX = DESC_LANGU_LISTBOX_BESTELL.
  ASSIGN DESC_LANGU_BESTELL TO <DESC_LANGU>.
  ASSIGN EDITOR_OBJ_BE TO <EDITOR_OBJ>.
  ASSIGN TEXTEDIT_CUSTOM_CONTAINER_BE TO <TEXTEDIT_CUSTOM_CONTAINER>.
  ASSIGN RM03M_SPRAS_BESTELL TO <RM03M_SPRAS>.
ENDMODULE.                 " SET_LONGTEXTDYNPRO_PARAMS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DYNPROPARAMS_IVERM  OUTPUT
*&---------------------------------------------------------------------*
*       Setzen der dynprospezifischen Parameter
*----------------------------------------------------------------------*
MODULE SET_DYNPROPARAMS_IVERM OUTPUT.
  DESC_LANGU_LISTBOX = DESC_LANGU_LISTBOX_IVERM.
  ASSIGN DESC_LANGU_IVERM TO <DESC_LANGU>.
  ASSIGN EDITOR_OBJ_IV TO <EDITOR_OBJ>.
  ASSIGN TEXTEDIT_CUSTOM_CONTAINER_IV TO <TEXTEDIT_CUSTOM_CONTAINER>.
  ASSIGN RM03M_SPRAS_IVERM TO <RM03M_SPRAS>.
ENDMODULE.                 " SET_LONGTEXTDYNPRO_PARAMS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_LTEXT_FELAUSWAHL_GRUNDD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_LTEXT_FELDAUSWAHL_GRUNDD OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  GRUNDDTEXT_BILD.
ENDMODULE.                    "SET_LTEXT_FELDAUSWAHL_GRUNDD OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_LTEXT_FELDAUSWAHL_BESTELL OUTPUT                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE SET_LTEXT_FELDAUSWAHL_BESTELL OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  BESTELLTEXT_BILD.
ENDMODULE.                    "SET_LTEXT_FELDAUSWAHL_BESTELL OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_LTEXT_FELDAUSWAHL_VERTRIEB OUTPUT                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE SET_LTEXT_FELDAUSWAHL_VERTRIEB OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  VERTRIEBSTEXT_BILD.
ENDMODULE.                    "SET_LTEXT_FELDAUSWAHL_VERTRIEB OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_LTEXT_FELDAUSWAHL_PRUEF OUTPUT                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE SET_LTEXT_FELDAUSWAHL_PRUEF OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  PRUEFTEXT_BILD.
ENDMODULE.                    "SET_LTEXT_FELDAUSWAHL_PRUEF OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_LTEXT_FELDAUSWAHL_IVERM OUTPUT                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE SET_LTEXT_FELDAUSWAHL_IVERM OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  IVERMTEXT_BILD.
ENDMODULE.                    "SET_LTEXT_FELDAUSWAHL_IVERM OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_LTXTE_FELAUSWAHL_GRUNDD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_LTXTE_FELDAUSWAHL_GRUNDD OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  GRUNDDTXTE_BILD.
ENDMODULE.                    "SET_LTXTE_FELDAUSWAHL_GRUNDD OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_LTXTE_FELDAUSWAHL_BESTELL OUTPUT                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE SET_LTXTE_FELDAUSWAHL_BESTELL OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  BESTELLTXTE_BILD.
ENDMODULE.                    "SET_LTXTE_FELDAUSWAHL_BESTELL OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_LTXTE_FELDAUSWAHL_VERTRIEB OUTPUT                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE SET_LTXTE_FELDAUSWAHL_VERTRIEB OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  VERTRIEBSTXTE_BILD.
ENDMODULE.                    "SET_LTXTE_FELDAUSWAHL_VERTRIEB OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_LTXTE_FELDAUSWAHL_PRUEF OUTPUT                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE SET_LTXTE_FELDAUSWAHL_PRUEF OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  PRUEFTXTE_BILD.
ENDMODULE.                    "SET_LTXTE_FELDAUSWAHL_PRUEF OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_LTXTE_FELDAUSWAHL_IVERM OUTPUT                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE SET_LTXTE_FELDAUSWAHL_IVERM OUTPUT.
  LANGTEXTBILD_FELDAUSWAHL =  IVERMTXTE_BILD.
ENDMODULE.                    "SET_LTXTE_FELDAUSWAHL_IVERM OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CLEAR_LANGTEXTBILD_FELDAUSWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CLEAR_LANGTEXTBILD_FELDAUSWAHL OUTPUT.
**<<<<BEGIN OF INSERTION NOTE216547<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  IF  RMMG2-MANBR NE SPACE
  AND ( LANGTEXTBILD_FELDAUSWAHL NE VERTRIEBSTEXT_BILD AND
        LANGTEXTBILD_FELDAUSWAHL NE VERTRIEBSTXTE_BILD ).
    IF RMMG2-MANBR = MANBR1.
      LTEXT_INPUT = 0.
      LTEXT_REQUIRED = 0.
    ELSE.
      LTEXT_INVISIBLE = 1.
      LTEXT_INPUT = 0.
      LTEXT_REQUIRED = 0.
    ENDIF.
  ENDIF.
*<<<<END OF INSERTION NOTE 216547<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  CLEAR LANGTEXTBILD_FELDAUSWAHL.


*Test Cursorsetzen
  IF T130M-AKTYP = AKTYPA OR T130M-AKTYP = AKTYPZ.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'ANZ_SPRACHEN'.
*        SET CURSOR FIELD 'ANZ_SPRACHEN'. wg02.05.02
        SET CURSOR FIELD 'MAKT-MAKTX'.
* Korrektes Scroll-Verhalten siehe Hinweis 516889
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " CLEAR_LANGTEXTBILD_FELDAUSWAHL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_LANGTEXTBILD_T130M_AKTYP  OUTPUT
*&---------------------------------------------------------------------*
*       TF 4.6A
*----------------------------------------------------------------------*
MODULE SET_LANGTEXTBILD_T130M_AKTYP OUTPUT.
  IF LTEXT_INPUT = 0.
    LANGTEXT_T130M_AKTYP_SAVE = T130M-AKTYP.
    T130M-AKTYP = AKTYPA.
  ENDIF.
* Aktualisieren des Aktyps in FUGR MG19, note 585507
  CALL FUNCTION 'UPDATE_AKTYP_MG19'
    EXPORTING
      P_AKTYP = T130M-AKTYP.
ENDMODULE.                 " SET_LANGTEXTBILD_T130M_AKTYP  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  RESET_LANGTEXTBILD_T130M_AKTYP  OUTPUT
*&---------------------------------------------------------------------*
*       TF 4.6A
*----------------------------------------------------------------------*
MODULE RESET_LANGTEXTBILD_T130M_AKTYP OUTPUT.
  IF NOT LANGTEXT_T130M_AKTYP_SAVE IS INITIAL.
    T130M-AKTYP = LANGTEXT_T130M_AKTYP_SAVE.
    CLEAR LANGTEXT_T130M_AKTYP_SAVE.
  ENDIF.
ENDMODULE.                 " CLEAR_LANGTEXTBILD_T130M_AKTYP  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LTEXT_STEPLOOP  OUTPUT
*&---------------------------------------------------------------------*
*       TF 4.6A
*----------------------------------------------------------------------*
MODULE LTEXT_STEPLOOP OUTPUT.

  CALL FUNCTION 'LTEXT_STEPLOOP'
    EXPORTING
      I_OLD   = 'X'
      I_WRITE = 'X'.

* Updaten des Aktyps wegen Materialfixierung, note 585507

  READ TABLE FAUSWTAB WITH KEY FNAME = 'RM03M-LTEX1'.

  IF SY-SUBRC = 0.
    IF FAUSWTAB-KZINP = 0.
      CALL FUNCTION 'UPDATE_AKTYP_MG19'
        EXPORTING
          P_AKTYP = AKTYPA.
    ELSE.                                  "note 593869
      CALL FUNCTION 'UPDATE_AKTYP_MG19'
        EXPORTING
          P_AKTYP = T130M-AKTYP.
    ENDIF.
  ENDIF.

ENDMODULE.                             " LTEXT_STEPLOOP  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LTEXT_TEXTEDIT_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*   TF 4.6A
*----------------------------------------------------------------------*
MODULE LTEXT_TEXTEDIT_CONTROL OUTPUT.
  CALL FUNCTION 'LTEXT_STEPLOOP'
    EXPORTING
      I_OLD   = ' '
      I_WRITE = 'X'.
ENDMODULE.                             " LTEXT_TEXTEDIT_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ANZEIGEN_OB_VORHANDEN  OUTPUT
*&---------------------------------------------------------------------*
*       TF 4.6A
*----------------------------------------------------------------------*
MODULE ANZEIGEN_OB_VORHANDEN OUTPUT.
  CALL FUNCTION 'MD_MATERIALNOTIZ'
    EXPORTING
      EMATNR    = RMMG1-MATNR
      EWERKS    = RMMG1-WERKS
      REF_MATNR = REF_MATNR
      REF_WERKS = REF_WERKS
      EMODUS    = 'C'
      NO_SAVE   = 'X'
    IMPORTING
      IEXIST    = NOTE_EXIST
    EXCEPTIONS
      OTHERS    = 1.
ENDMODULE.                             " ANZEIGEN_OB_VORHANDEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LTEXT_INVISIBLE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LTEXT_INVISIBLE OUTPUT.
  IF LTEXT_INVISIBLE = 1.
    LOOP AT SCREEN.
      SCREEN-INPUT = 0.
      SCREEN-REQUIRED = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.                             " LTEXT_INVISIBLE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_GRUN_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_GRUN_PROZ_SAVE OUTPUT.
  KZ_GRUN_PROZ_SAV = KZ_GRUN_PROZ.
ENDMODULE.                             " KZ_GRUN_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_GRUN_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_GRUN_PROZ_RESET OUTPUT.
  KZ_GRUN_PROZ = KZ_GRUN_PROZ_SAV.
ENDMODULE.                             " KZ_GRUN_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GRUNDD_LTEXT_INIT_FORCE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GRUNDD_LTEXT_INIT_FORCE OUTPUT.
  IF KZ_GRUN_PROZ IS INITIAL.
    CLEAR LANGTEXTBILD.
  ENDIF.
ENDMODULE.                             " GRUNDD_LTEXT_INIT_FORCE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_PRUE_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_PRUE_PROZ_SAVE OUTPUT.
  KZ_PRUE_PROZ_SAV = KZ_PRUE_PROZ.
ENDMODULE.                             " KZ_PRUE_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_PRUE_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_PRUE_PROZ_RESET OUTPUT.
  KZ_PRUE_PROZ = KZ_PRUE_PROZ_SAV.
ENDMODULE.                             " KZ_PRUE_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PRUEF_LTEXT_INIT_FORCE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PRUEF_LTEXT_INIT_FORCE OUTPUT.
  IF KZ_PRUE_PROZ IS INITIAL.
    CLEAR LANGTEXTBILD.
  ENDIF.
ENDMODULE.                             " PRUEF_LTEXT_INIT_FORCE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_VERT_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_VERT_PROZ_SAVE OUTPUT.
  KZ_VERT_PROZ_SAV = KZ_VERT_PROZ.
ENDMODULE.                             " KZ_VERT_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_VERT_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_VERT_PROZ_RESET OUTPUT.
  KZ_VERT_PROZ = KZ_VERT_PROZ_SAV.
ENDMODULE.                             " KZ_VERT_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GRUNDD_VERTRIEBS_INIT_FORCE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERTRIEBS_LTEXT_INIT_FORCE OUTPUT.
  IF KZ_VERT_PROZ IS INITIAL.
    CLEAR LANGTEXTBILD.
  ENDIF.
ENDMODULE.                 " VERTRIEBS_LTEXT_INIT_FORCE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_BEST_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_BEST_PROZ_SAVE OUTPUT.
  KZ_BEST_PROZ_SAV = KZ_BEST_PROZ.
ENDMODULE.                             " KZ_GRUN_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_BEST_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_BEST_PROZ_RESET OUTPUT.
  KZ_BEST_PROZ = KZ_BEST_PROZ_SAV.
ENDMODULE.                             " KZ_BEST_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  BESTELL_LTEXT_INIT_FORCE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BESTELL_LTEXT_INIT_FORCE OUTPUT.
  IF KZ_BEST_PROZ IS INITIAL.
    CLEAR LANGTEXTBILD.
  ENDIF.
ENDMODULE.                 " BESTELL_LTEXT_INIT_FORCE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_IVER_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_IVER_PROZ_SAVE OUTPUT.
  KZ_IVER_PROZ_SAV = KZ_IVER_PROZ.
ENDMODULE.                             " KZ_GRUN_PROZ_SAVE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  KZ_IVER_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE KZ_IVER_PROZ_RESET OUTPUT.
  KZ_IVER_PROZ = KZ_IVER_PROZ_SAV.
ENDMODULE.                             " KZ_IVER_PROZ_RESET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IVERM_LTEXT_INIT_FORCE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IVERM_LTEXT_INIT_FORCE OUTPUT.
  IF KZ_IVER_PROZ IS INITIAL.
    CLEAR LANGTEXTBILD.
  ENDIF.
ENDMODULE.                             " IVERM_LTEXT_INIT_FORCE  OUTPUT
