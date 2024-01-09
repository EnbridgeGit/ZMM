* 2000/10/10 mdemeest 4.6B Copied M07DRA03 to ZNMIM002              UGL
* 2013/01/28 btboundy 600 to 605 Upgrade, re-coppied "UGL" code into SAP Standard
*--------------Drucken WA-Ueberschrift---------------------------------*
FORM wa03_ausgabe USING lgortsplit.
  IF lgortsplit IS INITIAL.
    ON CHANGE OF mkpf-mblnr OR mseg-ebeln OR mseg-bwart OR mseg-werks.
      SELECT SINGLE * FROM t001l WHERE werks = mseg-umwrk        "UGL
                                   AND lgort = mseg-umlgo.       "UGL
      CLEAR xkopfdr.
    ENDON.
  ELSE.
    ON CHANGE OF mkpf-mblnr OR mseg-ebeln OR mseg-bwart OR mseg-werks
                            OR mseg-lgort.
      SELECT SINGLE * FROM t001l WHERE werks = mseg-umwrk        "UGL
                                   AND lgort = mseg-umlgo.       "UGL
      CLEAR xkopfdr.
    ENDON.
  ENDIF.
  IF xkopfdr IS INITIAL.
    xkopfdr = x.
    IF new_page = x.
      CALL FUNCTION 'CONTROL_FORM'
        EXPORTING
          command = 'NEW-PAGE'.
    ENDIF.
    IF NOT t159p-bacod IS INITIAL.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'BACOKOPF'
          window  = 'KOPF'.
      new_page = x.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'KOPF'
          window  = 'KOPF'.
      new_page = x.
    ENDIF.
  ENDIF.
*--------------Drucken WA-Positionen-----------------------------------*
  CLEAR: am07m-kontierung.
  am07m-rstyp = xskkz.
  CASE xskkz.                               "Kontierungsarten ?
    WHEN xfert.                            "Fertigungsauftrag
      am07m-kontierung = mseg-aufnr.
    WHEN xanlage.                          "auf Anlage kontiert?
      MOVE space TO anlage.
      anlage-anln1 = mseg-anln1.
      anlage-anln2 = mseg-anln2.
      MOVE space TO am07m-kontierung.
      CONDENSE anlage NO-GAPS.
      am07m-kontierung = anlage.          "Anlagennummer
    WHEN   xvbelg.                       "Kundenauftrag ?
      MOVE space TO kunde.
      kunde-kdauf = mseg-kdauf.
      kunde-kdpos = mseg-kdpos.
      kunde-kdein = mseg-kdein.
      MOVE space TO am07m-kontierung.
      CONDENSE kunde NO-GAPS.
      am07m-kontierung = kunde.           "Kundennummer
    WHEN   xkostl.                        "auf Kostenstelle kontiert?
      am07m-kontierung = mseg-kostl.      "Anlagennummer
    WHEN   xprojn OR xnplan.              "auf Projekt/Netzplan?
      IF mseg-nplnr IS INITIAL.
        PERFORM psp_convert USING mseg-ps_psp_pnr.
      ELSE.
        am07m-kontierung = mseg-nplnr.
        PERFORM nw_vorgang_lesen USING mseg-aufpl mseg-aplzl.
        IF NOT n_vornr IS INITIAL.
          MOVE '/'     TO am07m-kontierung+12.
          MOVE n_vornr TO am07m-kontierung+13.
        ENDIF.
      ENDIF.
    WHEN xumlag.                         "Umlagerung
      IF mseg-matnr NE mseg-ummat.
        MOVE mseg-ummat TO am07m-kontierung.
      ELSE.
        MOVE mseg-umwrk TO am07m-kontierung.
        MOVE '/'        TO am07m-kontierung+4.
        MOVE mseg-umlgo TO am07m-kontierung+5.
      ENDIF.
    WHEN OTHERS.                          "keine Kontierung ?
      IF NOT mseg-ummat IS INITIAL.
        IF mseg-matnr NE mseg-ummat.
          MOVE mseg-ummat TO am07m-kontierung.
        ELSEIF mseg-werks NE mseg-umwrk.
          MOVE mseg-umwrk TO am07m-kontierung.
          MOVE '/'        TO am07m-kontierung+4.
          MOVE mseg-umlgo TO am07m-kontierung+5.
        ELSEIF mseg-lgort NE mseg-umlgo.
          MOVE mseg-umwrk TO am07m-kontierung.
          MOVE '/'        TO am07m-kontierung+4.
          MOVE mseg-umlgo TO am07m-kontierung+5.
        ENDIF.
      ENDIF.
  ENDCASE.

* User-Exit über Erweiterung MBCF0005
  CALL CUSTOMER-FUNCTION '001'
       EXPORTING
            i_mkpf  = mkpf
            i_mseg  = mseg
            i_nast  = nast
            i_tnapr = tnapr
       CHANGING
            c_am07m = am07m
       EXCEPTIONS
            OTHERS  = 0.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'POS_ZEILE'.

*--------------------------  UGL Change  ----------------------------UGL
* Precondition in case of transfer:                                 "UGL
* 1.  The transfer item is expanded at the time of saving into      "UGL
*     the corresponding issuing and receiving transactions.         "UGL
* 2.  The serial information is only available at the item line     "UGL
*     indicated with "+" in the sign of material posting field      "UGL
  "UGL
* get the serial number and output it                               "UGL
  "UGL
* temporary variable - input to function module                     "UGL
  "UGL
  DATA: tmp_zeile LIKE mseg-zeile.                                    "UGL
  IF mseg-bwart = '301' OR mseg-bwart = '302' OR                      "UGL
     mseg-bwart = '303' OR mseg-bwart = '304' OR                      "UGL
     mseg-bwart = '305' OR mseg-bwart = '306' OR                      "UGL
     mseg-bwart = '311' OR mseg-bwart = '312'.                        "UGL
    tmp_zeile = mseg-zeile + 1.                                      "UGL
  ELSE.                                                               "UGL
    tmp_zeile = mseg-zeile.                                          "UGL
  ENDIF.                                                              "UGL
  "UGL
  CLEAR zequi.                                                        "UGL
  REFRESH zequi.                                                      "UGL
  CALL FUNCTION 'Z_MM_GET_SERIAL_NUMBER'                              "UGL
       EXPORTING                                                      "UGL
            p_mblnr = mseg-mblnr                                      "UGL
            p_mjahr = mseg-mjahr                                      "UGL
            p_zeile = tmp_zeile                                       "UGL
       TABLES                                                         "UGL
            zequi   = zequi                                           "UGL
       exceptions   = 1.                                              "UGL
  "UGL
* calls the write form function only when there is output           "UGL
  "UGL
  IF sy-subrc = 0.                                                    "UGL
    DESCRIBE TABLE zequi.                                            "UGL
    IF sy-tfill > 0 AND mseg-bwart <> '311' AND mseg-bwart <> '312'. "UGL
      LOOP AT zequi.                                                "UGL
        MOVE-CORRESPONDING zequi TO equi.                           "UGL
        CALL FUNCTION 'WRITE_FORM'                                  "UGL
             EXPORTING element = 'SERIAL'.                          "UGL
      ENDLOOP.                                                      "UGL
    ENDIF.                                                           "UGL
  ENDIF.
  "UGL
*-------------------  End of UGL Change --------------------------- UGL


*--------------Drucken WA-Seitenfuss-----------------------------------*

  PERFORM tab156t_lesen.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element  = 'FUSS'
      window   = 'FUSS'
      function = 'APPEND'.


* get the vendor address                                             UGL
  SELECT SINGLE * FROM lfa1 WHERE lifnr = mseg-lifnr.               "UGL
* get the plant address                                              UGL
  SELECT SINGLE * FROM t001w WHERE werks = mseg-umwrk.              "UGL
ENDFORM.                    "WA03_AUSGABE

*&---------------------------------------------------------------------*
*&      Form  wa03_ausgabe_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LGORTSPLIT  text
*----------------------------------------------------------------------*
FORM wa03_ausgabe_pdf  USING    p_lgortsplit.


  IF p_lgortsplit IS INITIAL.
    ON CHANGE OF mkpf-mblnr OR mseg-ebeln OR mseg-bwart OR mseg-werks.
      CLEAR xkopfdr.
    ENDON.
  ELSE.
    ON CHANGE OF mkpf-mblnr OR mseg-ebeln OR mseg-bwart OR mseg-werks
                            OR mseg-lgort.
      CLEAR xkopfdr.
    ENDON.
  ENDIF.
  IF xkopfdr IS INITIAL.
    xkopfdr = x.
    IF new_page = x.

    ENDIF.

    IF NOT t159p-bacod IS INITIAL.
      MOVE:mkpf-mblnr   TO  gs_mat_document-mblnr,
           mkpf-budat   TO  gs_mat_document-budat,
           mkpf-cpudt   TO  gs_mat_document-cpudt,
           t001w-werks  TO  gs_plants,
           t001w-name1  TO  gs_name1,
           t159p-bacod  TO  gs_barcode-bacod.
      new_page = x.
    ELSE.
      MOVE: mkpf-mblnr   TO  gs_mat_document-mblnr,
            mkpf-budat   TO  gs_mat_document-budat,
            mkpf-cpudt   TO  gs_mat_document-cpudt,
            t001w-werks  TO  gs_plants,
            t001w-name1  TO  gs_name1,
            mseg-lgort   TO  gs_stor_location.

      new_page = x.
    ENDIF.
  ENDIF.
*--------------Drucken WA-Positionen-----------------------------------*
  CLEAR: am07m-kontierung.
  am07m-rstyp = xskkz.
  CASE xskkz.                               "Kontierungsarten ?
    WHEN xfert.                            "Fertigungsauftrag
      am07m-kontierung = mseg-aufnr.
    WHEN xanlage.                          "auf Anlage kontiert?
      MOVE space TO anlage.
      anlage-anln1 = mseg-anln1.
      anlage-anln2 = mseg-anln2.
      MOVE space TO am07m-kontierung.
      CONDENSE anlage NO-GAPS.
      am07m-kontierung = anlage.          "Anlagennummer
    WHEN   xvbelg.                       "Kundenauftrag ?
      MOVE space TO kunde.
      kunde-kdauf = mseg-kdauf.
      kunde-kdpos = mseg-kdpos.
      kunde-kdein = mseg-kdein.
      MOVE space TO am07m-kontierung.
      CONDENSE kunde NO-GAPS.
      am07m-kontierung = kunde.           "Kundennummer
    WHEN   xkostl.                        "auf Kostenstelle kontiert?
      am07m-kontierung = mseg-kostl.      "Anlagennummer
    WHEN   xprojn OR xnplan.              "auf Projekt/Netzplan?
      IF mseg-nplnr IS INITIAL.
        PERFORM psp_convert USING mseg-ps_psp_pnr.
      ELSE.
        am07m-kontierung = mseg-nplnr.
        PERFORM nw_vorgang_lesen USING mseg-aufpl mseg-aplzl.
        IF NOT n_vornr IS INITIAL.
          MOVE '/'     TO am07m-kontierung+12.
          MOVE n_vornr TO am07m-kontierung+13.
        ENDIF.
      ENDIF.
    WHEN xumlag.                         "Umlagerung
      IF mseg-matnr NE mseg-ummat.
        MOVE mseg-ummat TO am07m-kontierung.
      ELSE.
        MOVE mseg-umwrk TO am07m-kontierung.
        MOVE '/'        TO am07m-kontierung+4.
        MOVE mseg-umlgo TO am07m-kontierung+5.
      ENDIF.
    WHEN OTHERS.                          "keine Kontierung ?
      IF NOT mseg-ummat IS INITIAL.
        IF mseg-matnr NE mseg-ummat.
          MOVE mseg-ummat TO am07m-kontierung.
        ELSEIF mseg-werks NE mseg-umwrk.
          MOVE mseg-umwrk TO am07m-kontierung.
          MOVE '/'        TO am07m-kontierung+4.
          MOVE mseg-umlgo TO am07m-kontierung+5.
        ELSEIF mseg-lgort NE mseg-umlgo.
          MOVE mseg-umwrk TO am07m-kontierung.
          MOVE '/'        TO am07m-kontierung+4.
          MOVE mseg-umlgo TO am07m-kontierung+5.
        ENDIF.
      ENDIF.
  ENDCASE.

* User-Exit über Erweiterung MBCF0005
  CALL CUSTOMER-FUNCTION '001'
       EXPORTING
            i_mkpf  = mkpf
            i_mseg  = mseg
            i_nast  = nast
            i_tnapr = tnapr
       CHANGING
            c_am07m = am07m
       EXCEPTIONS
            OTHERS  = 0.

  MOVE: mseg-zeile        TO  gs_items-zeile,
        mseg-matnr        TO  gs_items-matnr,
        mseg-charg        TO  gs_items-charg,
        am07m-rstyp       TO  gs_items-rstyp,
        am07m-kontierung  TO  gs_items-kontierung,
        mseg-lgort        TO  gs_items-lgort,
        mabdr-lgpbe       TO  gs_items-lgpbe,
        mabdr-maktx       TO  gs_items-maktx,
        mseg-menge        TO  gs_items-menge,
        mseg-meins        TO  gs_items-meins.

  APPEND gs_items  TO gt_items.
  CLEAR gs_items.
*--------------Drucken WA-Seitenfuss-----------------------------------*

  PERFORM tab156t_lesen.
  MOVE: t156t-bwart  TO  gs_mvt_type-bwart,
        t156t-btext  TO  gs_mvt_type-btext,
        mkpf-usnam   TO  gs_mat_document-usnam.
ENDFORM.                    " wa03_ausgabe_PDF

*&---------------------------------------------------------------------*
*&      Form  lesen_was_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAST_OBJKY  text
*      -->P_LGORTSPLIT  text
*----------------------------------------------------------------------*
FORM lesen_was_pdf  USING    p_nast_objky
                             p_lgortsplit.
  REFRESH traptab.
  nast_key = p_nast_objky.
  CLEAR retco.
  CLEAR: xkopfdr, new_page.
  SELECT SINGLE * FROM mkpf WHERE mblnr = nast_key-mblnr
                            AND   mjahr = nast_key-mjahr.
  MOVE-CORRESPONDING mkpf TO traptab.
  zaehler_m = cv_first_time.
  SELECT * FROM mseg WHERE mblnr = mkpf-mblnr
                     AND   mjahr = mkpf-mjahr.
    IF sy-subrc NE 0.
      retco = sy-subrc.
      EXIT.
    ENDIF.
    CHECK mseg-xauto IS INITIAL.
    IF zaehler_m = cv_first_time. "do it only once
      CLEAR zaehler_m.
      PERFORM tab001w_lesen.
    ENDIF.
    MOVE-CORRESPONDING mseg TO traptab.
    APPEND traptab.
  ENDSELECT.

*  PERFORM open_form_sammel_pdf. "UGL
  PERFORM open_form_sammel_pdf(m07drson_pdf). "UGL


  IF NOT lgortsplit IS INITIAL.                             "431555
    SORT traptab BY werks lgort zeile.                      "431555
  ELSE.                                                     "431555
    SORT traptab BY werks zeile.                            "431555
  ENDIF.                                                    "431555

  REFRESH: gt_items.
  CLEAR : gt_items.

  LOOP AT traptab.
    MOVE-CORRESPONDING traptab TO mkpf.
    MOVE-CORRESPONDING traptab TO mseg.
    PERFORM tab156_lesen.
    CHECK NOT t156-kzdru IS INITIAL.                        " 108942
    xskkz = t156-rstyp.
    PERFORM tab001w_lesen_2.
    IF NOT mseg-matnr IS INITIAL.
      PERFORM material_lesen.
    ENDIF.
    PERFORM ladr_lesen.
    PERFORM helpdata1.
    PERFORM wa03_ausgabe_pdf USING lgortsplit.
    PERFORM helpdata2.
  ENDLOOP.
*  PERFORM print_pdf. "UGL
  PERFORM print_pdf(m07drson_pdf). "UGL
*  PERFORM close_form_pdf.  "UGL
  PERFORM close_form_pdf(m07drson_pdf).  "UGL

ENDFORM.                    " lesen_was_PDF

*&---------------------------------------------------------------------*
*&      Form  LESEN_WES_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAST_OBJKY  text
*      -->P_LGORTSPLIT  text
*----------------------------------------------------------------------*
FORM lesen_wes_pdf  USING objky lgortsplit.
  CLEAR retco.
  CLEAR: xkopfdr, new_page.
  REFRESH: gt_mseg.                                         "n1593510

  nast_key = objky.
  SELECT SINGLE * FROM mkpf INTO gs_mkpf WHERE mblnr = nast_key-mblnr
                            AND   mjahr = nast_key-mjahr.
  zaehler_m = 1.

  SELECT * FROM mseg INTO gs_mseg WHERE mblnr = gs_mkpf-mblnr
                    AND   mjahr = gs_mkpf-mjahr.
    IF sy-subrc NE 0.
      retco = sy-subrc.
      EXIT.
    ENDIF.
    IF zaehler_m = 1.
      CLEAR zaehler_m.
      PERFORM tab001w_lesen_pdf.
    ENDIF.
    APPEND gs_mseg TO gt_mseg.
  ENDSELECT.

*  PERFORM open_form_sammel_pdf. "UGL
  PERFORM open_form_sammel_pdf(m07drson_pdf). "UGL
  IF NOT lgortsplit IS INITIAL.                             "431555
    SORT gt_mseg BY werks lgort zeile.                      "431555
  ELSE.                                                     "431555
    SORT gt_mseg BY werks zeile.                            "431555
  ENDIF.                                                    "431555
  LOOP AT gt_mseg INTO gs_mseg.
    PERFORM tab156_lesen_pdf.
    CHECK NOT gs_t156-kzdru IS INITIAL.
    xskkz = gs_t156-rstyp.
    IF NOT gs_mseg-evers IS INITIAL.         "Versandvorschriften lesen.
      PERFORM t027_lesen_pdf.
    ENDIF.
    PERFORM bestellung_lesen_pdf.
    IF NOT ekpo-knttp IS INITIAL AND NOT
          ekpo-weunb IS INITIAL.
      PERFORM kontierung_lesen.     "Für WE unbewertet
    ENDIF.
    IF NOT gs_mseg-ematn IS INITIAL.
      PERFORM lesen_htn_pdf.
    ELSE.                                                   "111277/PH
      CLEAR gs_am07m-mfrpn.                                 "111277/PH
    ENDIF.
    IF gs_mseg-matnr IS INITIAL.
      gs_mseg-menge = gs_mseg-bpmng.
      gs_mseg-meins = gs_mseg-bprme.
      PERFORM bestelltext_lesen_pdf.
      CLEAR mabdr.
    ELSE.
      PERFORM material_lesen_pdf.
    ENDIF.
    PERFORM tab024_lesen.
    PERFORM t064b_lesen_pdf.
    PERFORM tab001w_lesen_2_pdf.
    PERFORM ladr_lesen_pdf.
    PERFORM helpdata1.
*   Read Advanced Returns data
    IF cl_ops_switch_check=>ops_sfws_sc_advret1( ) NE cl_ops_switch_check=>switch_active_true.
      PERFORM read_msr_data_pdf.
    ENDIF.
    PERFORM we03_ausgabe_pdf USING lgortsplit.
    PERFORM helpdata2.
  ENDLOOP.
  APPEND gs_we03_details TO gt_we03_details.
  CLEAR gs_we03_details.
* Print forms.
  IF tnapr-formtype = '2' AND NOT tnapr-sform IS INITIAL.
    MOVE tnapr-sform TO pdf_form.
  ELSE.
    MOVE tnapr-fonam TO pdf_form.
  ENDIF.
*  PERFORM call_form_we03_pdf USING pdf_form. "UGL
  PERFORM call_form_we03_pdf(m07drson_pdf) USING pdf_form. "UGL
*  PERFORM close_form_pdf. "UGL
  PERFORM close_form_pdf(m07drson_pdf). "UGL
ENDFORM.                    " LESEN_WES_PDF
