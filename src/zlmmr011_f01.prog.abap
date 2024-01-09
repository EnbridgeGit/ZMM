*&---------------------------------------------------------------------*
*&  Include           ZLMMR011_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLMMR011_F01                                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 20-DEC-2013                                    *
*& Object ID          : 59211: Report Storage Location MRP Information *
*& Application Area   : MM                                             *
*& Description        : Maintain/monitor the MRP settings at the storage
*&                      location level                                 *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DETAILS
*&---------------------------------------------------------------------*
*       Get data from MARD, MARA, MAKT tables
*----------------------------------------------------------------------*
FORM get_details .
* Get data from Storage Location Data for Material
  SELECT matnr
         werks
         lgort
         labst
         diskz
         lminb
         lbstf
         lgpbe
         FROM mard
         INTO TABLE lt_mard
         WHERE matnr IN s_matnr
         AND   werks IN s_werks
         AND   lgort IN s_lgort.

  IF lt_mard IS NOT INITIAL.
*   Get data from General Material Data
    SELECT matnr
           meins
           FROM mara
           INTO TABLE lt_mara
           FOR ALL ENTRIES IN lt_mard
           WHERE matnr = lt_mard-matnr.
*BOI by PANUSURI ticket 59211
    IF lt_mara IS NOT INITIAL.
*     Get data for Unit of Measure
      SELECT msehi
             mseh3
             FROM t006a
             INTO TABLE lt_t006a
             FOR ALL ENTRIES IN lt_mara
             WHERE spras = 'EN'
             AND   msehi = lt_mara-meins.
    ENDIF.
*EOI by PANUSURI ticket 59211

*   Get data from Material Descriptions
    SELECT matnr
           maktg
           FROM makt
           INTO TABLE lt_makt
           FOR ALL ENTRIES IN lt_mard
           WHERE matnr = lt_mard-matnr.

  ELSE.
    MESSAGE 'No data selected'(002) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " GET_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       Build output data
*----------------------------------------------------------------------*
FORM get_output_data .

  LOOP AT lt_mard INTO lwa_mard.
    READ TABLE lt_mara INTO lwa_mara WITH KEY matnr = lwa_mard-matnr.
    IF sy-subrc = 0.
*BOI by PANUSURI ticket 59211
      READ TABLE lt_t006a INTO lwa_t006a WITH KEY msehi = lwa_mara-meins.
      IF sy-subrc = 0.
        lwa_output-mseh3 = lwa_t006a-mseh3.
      ENDIF.
*EOI by PANUSURI ticket 59211
*      lwa_output-meins = lwa_mara-meins. "(-)PANUSURI ticket 59211
    ENDIF.
    READ TABLE lt_makt INTO lwa_makt WITH KEY matnr = lwa_mard-matnr.
    IF sy-subrc = 0.
      lwa_output-maktg = lwa_makt-maktg.
    ENDIF.
    lwa_output-matnr = lwa_mard-matnr.
    lwa_output-werks = lwa_mard-werks.
    lwa_output-lgort = lwa_mard-lgort.
    lwa_output-labst = lwa_mard-labst.
    lwa_output-diskz = lwa_mard-diskz.
    lwa_output-lminb = lwa_mard-lminb.
    lwa_output-lbstf = lwa_mard-lbstf.
    lwa_output-lgpbe = lwa_mard-lgpbe.

    APPEND lwa_output TO lt_output.
    CLEAR: lwa_output,
           lwa_mard,
           lwa_mara,
           lwa_t006a, "(+)PANUSURI ticket 59211
           lwa_makt.
  ENDLOOP.
  REFRESH: lt_mard,
           lt_mara,
           lt_t006a, "(+)PANUSURI ticket 59211
           lt_makt.

  SORT lt_output BY werks lgort matnr.

ENDFORM.                    " GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       Display report using ALV
*----------------------------------------------------------------------*
FORM display_report .
* Build fieldcatalog
  PERFORM build_fieldcatalog.

* Sort by Plant
  CLEAR lwa_sort.
  lwa_sort-fieldname = 'WERKS'.
  APPEND lwa_sort TO lit_sort.

  lwa_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = 'ZLMMR011'
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
  PERFORM create_catalog USING : 'WERKS' 'Plnt'(003).
  PERFORM create_catalog USING : 'LGORT' 'SLoc'(004).
  PERFORM create_catalog USING : 'MATNR' 'Material'(005).
  PERFORM create_catalog USING : 'MAKTG' 'Description'(006).
  PERFORM create_catalog USING : 'LABST' 'QOH'(007).
*  PERFORM create_catalog USING : 'MEINS' 'UoM'(008). "(-)PANUSURI ticket 59211
  PERFORM create_catalog USING : 'MSEH3' 'UoM'(008).  "(+)PANUSURI ticket 59211
  PERFORM create_catalog USING : 'DISKZ' 'MRP Ind.'(009).
  PERFORM create_catalog USING : 'LMINB' 'ReorderPnt'(010).
  PERFORM create_catalog USING : 'LBSTF' 'ReplenshQt'(011).
  PERFORM create_catalog USING : 'LGPBE' 'Bin'(012).

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
*BOI by PANUSURI ticket 59211
  IF iv_fieldname = 'MATNR'.
    lwa_fieldcat-no_zero = abap_true.
  ENDIF.
*EOI by PANUSURI ticket 59211
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
    lwa_listheader-info             = 'Storage Location MRP Information'(013).
    APPEND lwa_listheader TO lt_listheader.
    CLEAR lwa_listheader.
*   Write List header
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = lt_listheader.
  ENDIF.

ENDFORM.                    " DISPLAY_TOP_OF_PAGE
