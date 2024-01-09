*&---------------------------------------------------------------------*
*&  Include           ZLMMR012_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLMMR012_F01                                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 19-DEC-2013                                    *
*& Object ID          : FS59177 Report: Vendor Delivery Assessment     *
*& Application Area   : MM                                             *
*& Description        : This report provides both the PO Delivery date *
*&                      with the Goods Receipt date for tracking Vendor*
*&                      delivery performance.                          *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DETAILS
*&---------------------------------------------------------------------*
*       Get data from EKKO, LFA1, EKPO, EKET, EKBE tables
*----------------------------------------------------------------------*
FORM get_details .
  DATA: lt_eket_temp TYPE STANDARD TABLE OF ty_eket.

* Get data from Scheduling Agreement Schedule Lines
  SELECT ebeln
         ebelp
         eindt
         FROM eket
         INTO TABLE lt_eket
         WHERE eindt IN s_ddate.
  IF sy-subrc = 0.
    SORT lt_eket BY ebeln ebelp.
  ELSE.
    MESSAGE 'No data selected'(002) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  IF lt_eket IS NOT INITIAL.
    lt_eket_temp[] = lt_eket[].
    DELETE ADJACENT DUPLICATES FROM lt_eket_temp COMPARING ebeln.
*   Get data from Purchasing Document Header
    SELECT ebeln
           lifnr
           FROM ekko
           INTO TABLE lt_ekko
           FOR ALL ENTRIES IN lt_eket_temp
           WHERE ebeln = lt_eket_temp-ebeln
           AND lifnr IN s_vnum.
    IF sy-subrc = 0.
      SORT lt_ekko BY ebeln.
    ELSE.
      MESSAGE 'No data selected'(002) TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    CLEAR lt_eket_temp.
    REFRESH lt_eket_temp.
  ENDIF.

  IF lt_ekko IS NOT INITIAL.
*   Get data from Vendor Master (General Section)
    SELECT lifnr
           name1
           FROM lfa1
           INTO TABLE lt_lfa1
           FOR ALL ENTRIES IN lt_ekko
           WHERE lifnr = lt_ekko-lifnr.
    IF sy-subrc = 0.
      SORT lt_lfa1 BY lifnr.
    ENDIF.

*   Get data from Purchasing Document Item
    SELECT ebeln
           ebelp
           matnr  "(+)PANUSURI Ticket 88758
           werks  " COG
           FROM ekpo
           INTO TABLE lt_ekpo
           FOR ALL ENTRIES IN lt_ekko
           WHERE ebeln = lt_ekko-ebeln
           AND   loekz <> 'L'
           AND   werks in s_werks.
    IF sy-subrc = 0.
      SORT lt_ekpo BY ebeln ebelp.
    ENDIF.
  ENDIF.

  IF lt_eket IS NOT INITIAL.
    lt_eket_temp[] = lt_eket[].
    DELETE ADJACENT DUPLICATES FROM lt_eket_temp COMPARING ebeln ebelp.
*   Get data from History per Purchasing Document
    SELECT ebeln
           ebelp
           budat
           FROM ekbe
           INTO TABLE lt_ekbe
           FOR ALL ENTRIES IN lt_eket_temp
           WHERE ebeln = lt_eket_temp-ebeln
           AND   ebelp = lt_eket_temp-ebelp
           AND   vgabe = '1'.

    IF sy-subrc = 0.
      SORT lt_ekbe BY ebeln ebelp.
    ENDIF.
    CLEAR lt_eket_temp.
    REFRESH lt_eket_temp.
  ENDIF.

ENDFORM.                    " GET_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       Build output data
*----------------------------------------------------------------------*
FORM get_output_data .
  DATA: lv_ekbe_tabix TYPE sy-tabix.

  LOOP AT lt_eket INTO lwa_eket.
    CLEAR: lwa_output,
           lwa_ekko,
           lwa_lfa1,
           lwa_ekpo,
           lwa_ekbe.
    lwa_output-ebeln = lwa_eket-ebeln.
    lwa_output-ebelp = lwa_eket-ebelp.
    lwa_output-eindt = lwa_eket-eindt.
*   Read purchasing document header data
    READ TABLE lt_ekko INTO lwa_ekko WITH KEY ebeln = lwa_eket-ebeln
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_output-lifnr = lwa_ekko-lifnr.
*     Read vendor data
      READ TABLE lt_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_ekko-lifnr
                                                BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_output-name1 = lwa_lfa1-name1.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.
*   Read purchasing document item data not marked for deletion
    READ TABLE lt_ekpo INTO lwa_ekpo WITH KEY ebeln = lwa_eket-ebeln
                                              ebelp = lwa_eket-ebelp
                                              BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ELSE.
      lwa_output-matnr = lwa_ekpo-matnr.  "(+)PANUSURI Ticket 88758
      lwa_output-werks = lwa_ekpo-werks.  " COG
    ENDIF.
*   Read PO history data
    READ TABLE lt_ekbe INTO lwa_ekbe WITH KEY ebeln = lwa_eket-ebeln
                                              ebelp = lwa_eket-ebelp
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      lv_ekbe_tabix = sy-tabix.
      LOOP AT lt_ekbe INTO lwa_ekbe FROM lv_ekbe_tabix.
        IF ( lwa_ekbe-ebeln <> lwa_eket-ebeln OR lwa_ekbe-ebelp <> lwa_eket-ebelp ).
          EXIT.
        ENDIF.
        lwa_output-budat = lwa_ekbe-budat.
        APPEND lwa_output TO lt_output.
*        CLEAR: lwa_output.
        CLEAR: lwa_ekbe.
      ENDLOOP.
      CLEAR lv_ekbe_tabix.
    ELSE.
      APPEND lwa_output TO lt_output.
*      CLEAR: lwa_output.
    ENDIF.

    CLEAR: lwa_output,
           lwa_eket,
           lwa_ekko,
           lwa_ekpo,
           lwa_lfa1,
           lwa_ekbe.
  ENDLOOP.
  REFRESH: lt_eket,
           lt_ekko,
           lt_lfa1,
           lt_ekbe,
           lt_ekpo.
  SORT lt_output BY lifnr.

ENDFORM.                    " GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       Display report using ALV
*----------------------------------------------------------------------*
FORM display_report .
* Build fieldcatalog
  PERFORM build_fieldcatalog.

* Sort by Vendor number
  CLEAR lwa_sort.
  lwa_sort-fieldname = 'LIFNR'.
  APPEND lwa_sort TO lit_sort.

  lwa_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = 'ZLMMR012'
      i_callback_top_of_page = 'DISPLAY_TOP_OF_PAGE'
      is_layout              = lwa_layout
      it_fieldcat            = lt_fieldcat[]
      it_sort                = lit_sort
    TABLES
      t_outtab               = lt_output[]
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.                    " DISPLAY_REPORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Build fieldcatalog
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  PERFORM create_catalog USING : 'LIFNR' 'Vendor'(003).
  PERFORM create_catalog USING : 'NAME1' 'Name'(004).
  PERFORM create_catalog USING : 'EBELN' 'Purch.Doc.'(005).
  PERFORM create_catalog USING : 'EBELP' 'Item'(006).
  PERFORM create_catalog USING : 'EINDT' 'Del.Date'(007).
  PERFORM create_catalog USING : 'BUDAT' 'GR Date'(008).
  PERFORM create_catalog USING : 'MATNR' 'Material'(009). "(+)PANUSURI Ticket 88758
  PERFORM create_catalog USING : 'WERKS' 'Plant'(010). "

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  CREATE_CATALOG
*&---------------------------------------------------------------------*
*       Create catalog
*----------------------------------------------------------------------*
FORM create_catalog  USING    iv_fieldname TYPE slis_fieldname
                              iv_seltext   TYPE scrtext_m.
  DATA: lwa_fieldcat TYPE slis_fieldcat_alv.

  CLEAR lwa_fieldcat.

  lwa_fieldcat-fieldname = iv_fieldname.
* BOI by PANUSURI Ticket 88758
  IF lwa_fieldcat-fieldname = 'MATNR'.
    lwa_fieldcat-no_zero = 'X'.
  ENDIF.
* EOI by PANUSURI Ticket 88758
  lwa_fieldcat-seltext_m = iv_seltext.
  APPEND lwa_fieldcat TO lt_fieldcat.

ENDFORM.                    " CREATE_CATALOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       Display Top of page
*----------------------------------------------------------------------*
FORM display_top_of_page .
  IF lt_listheader[] IS INITIAL.
    lwa_listheader-typ               ='H'.
    lwa_listheader-info             = 'Vendor Delivery Assessment'.
    APPEND lwa_listheader TO lt_listheader.
    CLEAR lwa_listheader.
*   Write List header
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = lt_listheader.
  ENDIF.

ENDFORM.                    " DISPLAY_TOP_OF_PAGE
