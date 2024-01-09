* 2000/10/10 mdemeest 4.6B Copied M07DRAUS to ZNMIM005              "UGL
* 2013/01/28 btboundy 600 to 605 Upgrade, re-coppied "UGL" code into SAP Standard
***INCLUDE M07DRAUS .
*
* 111277 4.5B 27.07.1008 PH: HTN - Fehlende Initialisierung
*
*----------------------------------------------------------------------*
*------------------ Ausgaberoutinen -----------------------------------*
*----------------------------------------------------------------------*
*------------- Wareneingangsschein Version 1 --------------------------*
FORM ausgabe_we01.
  PERFORM open_form.
  IF NOT t159p-xmehr IS INITIAL.
    IF mseg-weanz GT 0.
      anzahl = mseg-weanz.
    ELSE.
      anzahl = 1.
    ENDIF.
  ELSE.
    anzahl = 1.
  ENDIF.
  DO anzahl TIMES.
    PERFORM we01_druck.
  ENDDO.
  PERFORM close_form.
ENDFORM.                    "ausgabe_we01
*eject.
*------------- Wareneingangsschein Version 2 --------------------------*
FORM ausgabe_we02.
  PERFORM open_form.
  IF NOT t159p-xmehr IS INITIAL.
    IF mseg-weanz GT 0.
      anzahl = mseg-weanz.
    ELSE.
      anzahl = 1.
    ENDIF.
  ELSE.
    anzahl = 1.
  ENDIF.
  DO anzahl TIMES.
    PERFORM we02_druck.
  ENDDO.
  PERFORM close_form.
ENDFORM.                    "ausgabe_we02
*------------- Kanbankarte bei WE -------------------------------------*
FORM ausgabe_wek1.
  TABLES pkps.
  IF NOT mseg-aufnr IS INITIAL.
    pkps-aufnr = mseg-aufnr.
  ELSEIF NOT mseg-ebeln IS INITIAL.
    pkps-ebeln = mseg-ebeln.
    pkps-ebelp = mseg-ebelp.
  ELSEIF NOT mseg-rsnum IS INITIAL.
    pkps-rsnum = mseg-rsnum.
  ENDIF.
  PERFORM itcpo_fuellen.
  CALL FUNCTION 'PK_PRINT_KANBAN_GR'
    EXPORTING
      ipkps   = pkps
      iitcpo  = itcpo
      itdform = tnapr-fonam
      imblnr  = mseg-mblnr
      imblpo  = mseg-zeile.
ENDFORM.                    "ausgabe_wek1
*eject.
*------------- Warenausgangsschein Version 1 --------------------------*
FORM ausgabe_wa01.
  PERFORM open_form.
  PERFORM wa01_druck.
  PERFORM close_form.
ENDFORM.                    "ausgabe_wa01
*eject.
*------------- Warenausgangsschein Version 2 --------------------------*
FORM ausgabe_wa02.
  PERFORM open_form.
  PERFORM wa02_druck.
  PERFORM close_form.
ENDFORM.                    "ausgabe_wa02
*eject.
*------------- Lesen und Ausgabe für WE-Sammelschein ------------------*
FORM lesen_wes USING objky lgortsplit.
  REFRESH traptab.
  CLEAR retco.
  CLEAR: xkopfdr, new_page.
  nast_key = objky.
  SELECT SINGLE * FROM mkpf WHERE mblnr = nast_key-mblnr
                            AND   mjahr = nast_key-mjahr.
  MOVE-CORRESPONDING mkpf TO traptab.               "note 205937
  zaehler_m = 1.
  SELECT * FROM mseg WHERE mblnr = mkpf-mblnr
                     AND   mjahr = mkpf-mjahr.
    IF sy-subrc NE 0.
      retco = sy-subrc.
      EXIT.
    ENDIF.
    IF zaehler_m = 1.
      CLEAR zaehler_m.
      PERFORM tab001w_lesen.
    ENDIF.
    MOVE-CORRESPONDING mseg TO traptab.
    APPEND traptab.
  ENDSELECT.
  PERFORM open_form_sammel.
* ------------------------------- UGL Change  ----------------------UGL
*  IF NOT lgortsplit IS INITIAL.                       "431555 UGL
*    SORT traptab BY werks lgort zeile.                "431555 UGL
*  ELSE.                                               "431555 UGL
*    SORT traptab BY werks zeile.                      "431555 UGL
*  ENDIF.                                              "431555 UGL
  LOOP AT traptab.                                                 "UGL
    SELECT SINGLE * FROM mard WHERE matnr = traptab-matnr          "UGL
                                AND werks = traptab-werks          "UGL
                                AND lgort = traptab-lgort.         "UGL
    MOVE mard-lgpbe TO traptab-lgpbe.                              "UGL
    MODIFY traptab.                                                "UGL
  ENDLOOP.                                                         "UGL

  SORT traptab BY werks lgpbe.                                     "UGL
*---------------------------- End of UGL Change ------------------ "UGL

  LOOP AT traptab.
    MOVE-CORRESPONDING traptab TO mkpf.
    MOVE-CORRESPONDING traptab TO mseg.
    PERFORM tab156_lesen.
    CHECK NOT t156-kzdru IS INITIAL.
    xskkz = t156-rstyp.
    IF NOT mseg-evers IS INITIAL.         "Versandvorschriften lesen.
      PERFORM t027_lesen.
    ENDIF.
    PERFORM bestellung_lesen.
    IF NOT ekpo-knttp IS INITIAL AND NOT
          ekpo-weunb IS INITIAL.
      PERFORM kontierung_lesen.     "Für WE unbewertet
    ENDIF.
    IF NOT mseg-ematn IS INITIAL.
      PERFORM lesen_htn.
    ELSE.                                                   "111277/PH
      CLEAR am07m-mfrpn.                                    "111277/PH
    ENDIF.
    IF mseg-matnr IS INITIAL.
      mseg-menge = mseg-bpmng.
      mseg-meins = mseg-bprme.
      PERFORM bestelltext_lesen.
      CLEAR mabdr.
    ELSE.
      PERFORM material_lesen.
    ENDIF.
    PERFORM tab024_lesen.
    PERFORM t064b_lesen.
    PERFORM tab001w_lesen_2.
    PERFORM ladr_lesen.
    PERFORM helpdata1.
*   Read Advanced Returns data
    IF cl_ops_switch_check=>ops_sfws_sc_advret1( ) NE cl_ops_switch_check=>switch_active_true.
      PERFORM read_msr_data.
    ENDIF.
    PERFORM we03_ausgabe USING lgortsplit.
    PERFORM helpdata2.
  ENDLOOP.
  PERFORM close_form.
ENDFORM.                    "lesen_wes
*eject.
*-------------- Lesen und  Ausgabe Warenausgangssammelschein ----------*
FORM lesen_was USING objky lgortsplit.
  REFRESH traptab.
  nast_key = objky.
  CLEAR retco.
  CLEAR: xkopfdr, new_page.
  SELECT SINGLE * FROM mkpf WHERE mblnr = nast_key-mblnr
                            AND   mjahr = nast_key-mjahr.
  MOVE-CORRESPONDING mkpf TO traptab.
  zaehler_m = 1.
  SELECT * FROM mseg WHERE mblnr = mkpf-mblnr
                     AND   mjahr = mkpf-mjahr.
    IF sy-subrc NE 0.
      retco = sy-subrc.
      EXIT.
    ENDIF.
    CHECK mseg-xauto IS INITIAL.
    IF zaehler_m = 1.
      CLEAR zaehler_m.
      PERFORM tab001w_lesen.
    ENDIF.
    MOVE-CORRESPONDING mseg TO traptab.
    APPEND traptab.
  ENDSELECT.
  PERFORM open_form_sammel.
* ------------------------------- UGL Change  ----------------------UGL
*  IF NOT lgortsplit IS INITIAL.                       "431555 UGL
*    SORT traptab BY werks lgort zeile.                "431555 UGL
*  ELSE.                                               "431555 UGL
*    SORT traptab BY werks zeile.                      "431555 UGL
*  ENDIF.                                              "431555 UGL
  LOOP AT traptab.                                                 "UGL
    SELECT SINGLE * FROM mard WHERE matnr = traptab-matnr          "UGL
                                AND werks = traptab-werks          "UGL
                                AND lgort = traptab-lgort.         "UGL
    MOVE mard-lgpbe TO traptab-lgpbe.                              "UGL
*BOI by PANUSURI ticket 63176
    SELECT SINGLE * FROM mard WHERE matnr = traptab-matnr          "UGL
                                AND werks = traptab-umwrk          "UGL
                                AND lgort = traptab-umlgo.         "UGL
    MOVE mard-lgpbe TO traptab-lgpbe_rcv.                          "UGL
*EOI by PANUSURI ticket 63176
    MODIFY traptab.                                                "UGL
  ENDLOOP.                                                         "UGL

  SORT traptab BY werks lgpbe.                                     "UGL
*---------------------------- End of UGL Change ------------------ "UGL
  LOOP AT traptab.
    MOVE-CORRESPONDING traptab TO mkpf.
    MOVE-CORRESPONDING traptab TO mseg.
    MOVE traptab-lgpbe_rcv TO mard-lgpbe."(+) PANUSURI ticket 63176
    PERFORM tab156_lesen.
    CHECK NOT t156-kzdru IS INITIAL.                        " 108942
    xskkz = t156-rstyp.
    PERFORM tab001w_lesen_2.
    IF NOT mseg-matnr IS INITIAL.
      PERFORM material_lesen.
    ENDIF.
    PERFORM ladr_lesen.
    PERFORM helpdata1.
*   Read Advanced Returns data
    IF cl_ops_switch_check=>ops_sfws_sc_advret1( ) NE cl_ops_switch_check=>switch_active_true.
      PERFORM read_msr_data.
    ENDIF.
    PERFORM wa03_ausgabe USING lgortsplit.
    PERFORM helpdata2.
  ENDLOOP.
  PERFORM close_form.
ENDFORM.                    "lesen_was
*eject.
*------------------- Ausgabe Etiketten --------------------------------*
FORM ausgabe_eti.
  PERFORM open_form.
  PERFORM eti_druck.
  PERFORM close_form.
ENDFORM.                    "ausgabe_eti
*eject.
*------------- Etikettendruck bei Version 3 Wareneingang --------------*
FORM lesen_wese USING objky.
  CLEAR retco.
  nast_key = objky.
  zaehler_m = 1.
  SELECT SINGLE * FROM mkpf WHERE mblnr = nast_key-mblnr
                            AND   mjahr = nast_key-mjahr.
  SELECT * FROM mseg WHERE mblnr = mkpf-mblnr
                     AND   mjahr = mkpf-mjahr.
    IF sy-subrc NE 0.
      retco = sy-subrc.
      EXIT.
    ENDIF.
    IF zaehler_m = 1.
      CLEAR zaehler_m.
      PERFORM tab001w_lesen.
      PERFORM open_form.
    ENDIF.
    PERFORM tab156_lesen.
    CHECK NOT t156-kzdru IS INITIAL.
    xskkz = t156-rstyp.
    IF NOT mseg-evers IS INITIAL.         "Versandvorschriften lesen.
      PERFORM t027_lesen.
    ENDIF.
    PERFORM bestellung_lesen.
    IF mseg-matnr IS INITIAL.
      mseg-menge = mseg-bpmng.
      mseg-meins = mseg-bprme.
      PERFORM bestelltext_lesen.
    ELSE.
      PERFORM material_lesen.
    ENDIF.
    PERFORM tab024_lesen.
    PERFORM tab001w_lesen_2.
    PERFORM helpdata1.
    PERFORM eti_druck.
    PERFORM helpdata2.
  ENDSELECT.
  PERFORM close_form.
ENDFORM.                    "lesen_wese
*eject.
*------------ Etikettendruck Warenausgang Version 3 -------------------*
FORM lesen_wase USING objky.
  nast_key = objky.
  CLEAR retco.
  zaehler_m = 1.
  SELECT SINGLE * FROM mkpf WHERE mblnr = nast_key-mblnr
                            AND   mjahr = nast_key-mjahr.
  SELECT * FROM mseg WHERE mblnr = mkpf-mblnr
                     AND   mjahr = mkpf-mjahr.

    IF sy-subrc NE 0.
      retco = sy-subrc.
      EXIT.
    ENDIF.
    IF zaehler_m = 1.
      CLEAR zaehler_m.
      PERFORM tab001w_lesen.
      PERFORM open_form.
    ENDIF.
    PERFORM tab156_lesen.
    CHECK NOT t156-kzdru IS INITIAL.                        " 108942
    xskkz = t156-rstyp.
    PERFORM tab001w_lesen_2.
    IF NOT mseg-matnr IS INITIAL.
      PERFORM material_lesen.
    ENDIF.
    PERFORM helpdata1.
    PERFORM eti_druck.
    PERFORM helpdata2.
  ENDSELECT.
  PERFORM close_form.
ENDFORM.                    "lesen_wase
*eject.
*------------- Warenausgangsschein LB Version 1 -----------------------*
FORM ausgabe_wlb1.
  PERFORM open_form.
  PERFORM wa01_druck.
  PERFORM close_form.
ENDFORM.                    "ausgabe_wlb1
*eject.
*------------- Warenausgangsschein LB Version 2 -----------------------*
FORM ausgabe_wlb2.
  PERFORM open_form.
  PERFORM wa02_druck.
  PERFORM close_form.
ENDFORM.                    "ausgabe_wlb2
*eject.
*------- Lesen und Ausgabe Warenausgangssammelsch. LB -----------------*
FORM lesen_wlbs USING objky.
  nast_key = objky.
  CLEAR retco.
  CLEAR: xkopfdr, new_page.
  zaehler_m = 1.
  SELECT SINGLE * FROM mkpf WHERE mblnr = nast_key-mblnr
                            AND   mjahr = nast_key-mjahr.
  SELECT * FROM mseg INTO TABLE xmseg
                     WHERE mblnr = mkpf-mblnr
                     AND   mjahr = mkpf-mjahr.
  LOOP AT xmseg.
    mseg = xmseg.
    CHECK mseg-sobkz IS INITIAL OR
          ( mseg-sobkz = o AND mseg-xauto IS INITIAL ).
    IF sy-subrc NE 0.
      retco = sy-subrc.
      EXIT.
    ENDIF.
    IF zaehler_m = 1.
      CLEAR zaehler_m.
      PERFORM tab001w_lesen.
      PERFORM open_form_sammel.
    ENDIF.
    PERFORM tab156_lesen.
    xskkz = t156-rstyp.
    IF NOT mseg-matnr IS INITIAL.
      PERFORM material_lesen.
    ENDIF.
    ON CHANGE OF mseg-lifnr.
      PERFORM read_address.
    ENDON.
    PERFORM tab001w_lesen_2.
    PERFORM helpdata1.
*   Read Advanced Returns data
    IF cl_ops_switch_check=>ops_sfws_sc_advret1( ) NE cl_ops_switch_check=>switch_active_true.
      PERFORM read_msr_data.
    ENDIF.
    PERFORM lb03_ausgabe.
    PERFORM helpdata2.
  ENDLOOP.
  PERFORM close_form.
ENDFORM.                    "lesen_wlbs
*eject.
*----------------------------------------------------------------------*
*------------------ Ausgaberoutinen -----------------------------------*
*----------------------------------------------------------------------*
*------------- WE Schein Für Fert.Auftr Vers. 1 -----------------------*
FORM ausgabe_wf01.
  PERFORM open_form.
  IF NOT t159p-xmehr IS INITIAL.
    IF mseg-weanz GT 0.
      anzahl = mseg-weanz.
    ELSE.
      anzahl = 1.
    ENDIF.
  ELSE.
    anzahl = 1.
  ENDIF.
  DO anzahl TIMES.
    PERFORM wf01_druck.
  ENDDO.
  PERFORM close_form.
ENDFORM.                    "ausgabe_wf01
*eject.
*------------- WE Schein für Fert.Auftrag Vers 2.----------------------*
FORM ausgabe_wf02.
  PERFORM open_form.
  IF NOT t159p-xmehr IS INITIAL.
    IF mseg-weanz GT 0.
      anzahl = mseg-weanz.
    ELSE.
      anzahl = 1.
    ENDIF.
  ELSE.
    anzahl = 1.
  ENDIF.
  DO anzahl TIMES.
    PERFORM wf02_druck.
  ENDDO.
  PERFORM close_form.
ENDFORM.                    "ausgabe_wf02
