*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZMMMV_T001W.....................................*
FORM GET_DATA_ZMMMV_T001W.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM T001W WHERE
    VLFKZ EQ ' ' AND
(VIM_WHERETAB) .
    CLEAR ZMMMV_T001W .
ZMMMV_T001W-MANDT =
T001W-MANDT .
ZMMMV_T001W-WERKS =
T001W-WERKS .
ZMMMV_T001W-NAME1 =
T001W-NAME1 .
ZMMMV_T001W-NAME2 =
T001W-NAME2 .
ZMMMV_T001W-STRAS =
T001W-STRAS .
ZMMMV_T001W-PFACH =
T001W-PFACH .
ZMMMV_T001W-PSTLZ =
T001W-PSTLZ .
ZMMMV_T001W-ORT01 =
T001W-ORT01 .
ZMMMV_T001W-LAND1 =
T001W-LAND1 .
ZMMMV_T001W-REGIO =
T001W-REGIO .
ZMMMV_T001W-COUNC =
T001W-COUNC .
ZMMMV_T001W-CITYC =
T001W-CITYC .
ZMMMV_T001W-ADRNR =
T001W-ADRNR .
ZMMMV_T001W-SPRAS =
T001W-SPRAS .
ZMMMV_T001W-FABKL =
T001W-FABKL .
ZMMMV_T001W-TXJCD =
T001W-TXJCD .
ZMMMV_T001W-ZZTAX_CODE =
T001W-ZZTAX_CODE .
ZMMMV_T001W-ZZ_GOODS_REC =
T001W-ZZ_GOODS_REC .
    SELECT SINGLE * FROM T005 WHERE
LAND1 = T001W-LAND1 .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005T WHERE
LAND1 = T005-LAND1 AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-LANDX =
T005T-LANDX .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM T002 WHERE
SPRAS = T001W-SPRAS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T002T WHERE
SPRSL = T002-SPRAS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-SPTXT =
T002T-SPTXT .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM T005S WHERE
LAND1 = T001W-LAND1 AND
BLAND = T001W-REGIO .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005U WHERE
LAND1 = T005S-LAND1 AND
BLAND = T005S-BLAND AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-BEZEI =
T005U-BEZEI .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM T005G WHERE
LAND1 = T001W-LAND1 AND
REGIO = T001W-REGIO AND
CITYC = T001W-CITYC .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005H WHERE
LAND1 = T005G-LAND1 AND
REGIO = T005G-REGIO AND
CITYC = T005G-CITYC AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-HBEZEI =
T005H-BEZEI .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM T005E WHERE
LAND1 = T001W-LAND1 AND
REGIO = T001W-REGIO AND
COUNC = T001W-COUNC .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005F WHERE
LAND1 = T005E-LAND1 AND
REGIO = T005E-REGIO AND
COUNC = T005E-COUNC AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-FBEZEI =
T005F-BEZEI .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM TFACD WHERE
IDENT = T001W-FABKL .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM TFACT WHERE
IDENT = TFACD-IDENT AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-LTEXT =
TFACT-LTEXT .
      ENDIF.
    ENDIF.
<VIM_TOTAL_STRUC> = ZMMMV_T001W.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZMMMV_T001W .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZMMMV_T001W.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZMMMV_T001W-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM T001W WHERE
  WERKS = ZMMMV_T001W-WERKS .
    IF SY-SUBRC = 0.
    DELETE T001W .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM T001W WHERE
  WERKS = ZMMMV_T001W-WERKS .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR T001W.
    ENDIF.
T001W-MANDT =
ZMMMV_T001W-MANDT .
T001W-WERKS =
ZMMMV_T001W-WERKS .
T001W-NAME1 =
ZMMMV_T001W-NAME1 .
T001W-NAME2 =
ZMMMV_T001W-NAME2 .
T001W-STRAS =
ZMMMV_T001W-STRAS .
T001W-PFACH =
ZMMMV_T001W-PFACH .
T001W-PSTLZ =
ZMMMV_T001W-PSTLZ .
T001W-ORT01 =
ZMMMV_T001W-ORT01 .
T001W-LAND1 =
ZMMMV_T001W-LAND1 .
T001W-REGIO =
ZMMMV_T001W-REGIO .
T001W-COUNC =
ZMMMV_T001W-COUNC .
T001W-CITYC =
ZMMMV_T001W-CITYC .
T001W-ADRNR =
ZMMMV_T001W-ADRNR .
T001W-SPRAS =
ZMMMV_T001W-SPRAS .
T001W-FABKL =
ZMMMV_T001W-FABKL .
T001W-TXJCD =
ZMMMV_T001W-TXJCD .
T001W-ZZTAX_CODE =
ZMMMV_T001W-ZZTAX_CODE .
T001W-ZZ_GOODS_REC =
ZMMMV_T001W-ZZ_GOODS_REC .
    IF SY-SUBRC = 0.
    UPDATE T001W .
    ELSE.
    INSERT T001W .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZMMMV_T001W-UPD_FLAG,
STATUS_ZMMMV_T001W-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZMMMV_T001W.
  SELECT SINGLE * FROM T001W WHERE
WERKS = ZMMMV_T001W-WERKS .
ZMMMV_T001W-MANDT =
T001W-MANDT .
ZMMMV_T001W-WERKS =
T001W-WERKS .
ZMMMV_T001W-NAME1 =
T001W-NAME1 .
ZMMMV_T001W-NAME2 =
T001W-NAME2 .
ZMMMV_T001W-STRAS =
T001W-STRAS .
ZMMMV_T001W-PFACH =
T001W-PFACH .
ZMMMV_T001W-PSTLZ =
T001W-PSTLZ .
ZMMMV_T001W-ORT01 =
T001W-ORT01 .
ZMMMV_T001W-LAND1 =
T001W-LAND1 .
ZMMMV_T001W-REGIO =
T001W-REGIO .
ZMMMV_T001W-COUNC =
T001W-COUNC .
ZMMMV_T001W-CITYC =
T001W-CITYC .
ZMMMV_T001W-ADRNR =
T001W-ADRNR .
ZMMMV_T001W-SPRAS =
T001W-SPRAS .
ZMMMV_T001W-FABKL =
T001W-FABKL .
ZMMMV_T001W-TXJCD =
T001W-TXJCD .
ZMMMV_T001W-ZZTAX_CODE =
T001W-ZZTAX_CODE .
ZMMMV_T001W-ZZ_GOODS_REC =
T001W-ZZ_GOODS_REC .
    SELECT SINGLE * FROM T005 WHERE
LAND1 = T001W-LAND1 .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005T WHERE
LAND1 = T005-LAND1 AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-LANDX =
T005T-LANDX .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-LANDX .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-LANDX .
    ENDIF.
    SELECT SINGLE * FROM T002 WHERE
SPRAS = T001W-SPRAS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T002T WHERE
SPRSL = T002-SPRAS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-SPTXT =
T002T-SPTXT .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-SPTXT .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-SPTXT .
    ENDIF.
    SELECT SINGLE * FROM T005S WHERE
LAND1 = T001W-LAND1 AND
BLAND = T001W-REGIO .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005U WHERE
LAND1 = T005S-LAND1 AND
BLAND = T005S-BLAND AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-BEZEI =
T005U-BEZEI .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-BEZEI .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-BEZEI .
    ENDIF.
    SELECT SINGLE * FROM T005G WHERE
LAND1 = T001W-LAND1 AND
REGIO = T001W-REGIO AND
CITYC = T001W-CITYC .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005H WHERE
LAND1 = T005G-LAND1 AND
REGIO = T005G-REGIO AND
CITYC = T005G-CITYC AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-HBEZEI =
T005H-BEZEI .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-HBEZEI .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-HBEZEI .
    ENDIF.
    SELECT SINGLE * FROM T005E WHERE
LAND1 = T001W-LAND1 AND
REGIO = T001W-REGIO AND
COUNC = T001W-COUNC .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005F WHERE
LAND1 = T005E-LAND1 AND
REGIO = T005E-REGIO AND
COUNC = T005E-COUNC AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-FBEZEI =
T005F-BEZEI .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-FBEZEI .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-FBEZEI .
    ENDIF.
    SELECT SINGLE * FROM TFACD WHERE
IDENT = T001W-FABKL .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM TFACT WHERE
IDENT = TFACD-IDENT AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-LTEXT =
TFACT-LTEXT .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-LTEXT .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-LTEXT .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZMMMV_T001W USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZMMMV_T001W-WERKS TO
T001W-WERKS .
MOVE ZMMMV_T001W-MANDT TO
T001W-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'T001W'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN T001W TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'T001W'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZMMMV_T001W USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
T001W-MANDT =
ZMMMV_T001W-MANDT .
T001W-WERKS =
ZMMMV_T001W-WERKS .
T001W-NAME1 =
ZMMMV_T001W-NAME1 .
T001W-NAME2 =
ZMMMV_T001W-NAME2 .
T001W-STRAS =
ZMMMV_T001W-STRAS .
T001W-PFACH =
ZMMMV_T001W-PFACH .
T001W-PSTLZ =
ZMMMV_T001W-PSTLZ .
T001W-ORT01 =
ZMMMV_T001W-ORT01 .
T001W-LAND1 =
ZMMMV_T001W-LAND1 .
T001W-REGIO =
ZMMMV_T001W-REGIO .
T001W-COUNC =
ZMMMV_T001W-COUNC .
T001W-CITYC =
ZMMMV_T001W-CITYC .
T001W-ADRNR =
ZMMMV_T001W-ADRNR .
T001W-SPRAS =
ZMMMV_T001W-SPRAS .
T001W-FABKL =
ZMMMV_T001W-FABKL .
T001W-TXJCD =
ZMMMV_T001W-TXJCD .
T001W-ZZTAX_CODE =
ZMMMV_T001W-ZZTAX_CODE .
T001W-ZZ_GOODS_REC =
ZMMMV_T001W-ZZ_GOODS_REC .
    SELECT SINGLE * FROM T005 WHERE
LAND1 = T001W-LAND1 .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005T WHERE
LAND1 = T005-LAND1 AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-LANDX =
T005T-LANDX .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-LANDX .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-LANDX .
    ENDIF.
    SELECT SINGLE * FROM T002 WHERE
SPRAS = T001W-SPRAS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T002T WHERE
SPRSL = T002-SPRAS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-SPTXT =
T002T-SPTXT .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-SPTXT .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-SPTXT .
    ENDIF.
    SELECT SINGLE * FROM T005S WHERE
LAND1 = T001W-LAND1 AND
BLAND = T001W-REGIO .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005U WHERE
LAND1 = T005S-LAND1 AND
BLAND = T005S-BLAND AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-BEZEI =
T005U-BEZEI .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-BEZEI .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-BEZEI .
    ENDIF.
    SELECT SINGLE * FROM T005G WHERE
LAND1 = T001W-LAND1 AND
REGIO = T001W-REGIO AND
CITYC = T001W-CITYC .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005H WHERE
LAND1 = T005G-LAND1 AND
REGIO = T005G-REGIO AND
CITYC = T005G-CITYC AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-HBEZEI =
T005H-BEZEI .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-HBEZEI .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-HBEZEI .
    ENDIF.
    SELECT SINGLE * FROM T005E WHERE
LAND1 = T001W-LAND1 AND
REGIO = T001W-REGIO AND
COUNC = T001W-COUNC .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T005F WHERE
LAND1 = T005E-LAND1 AND
REGIO = T005E-REGIO AND
COUNC = T005E-COUNC AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-FBEZEI =
T005F-BEZEI .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-FBEZEI .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-FBEZEI .
    ENDIF.
    SELECT SINGLE * FROM TFACD WHERE
IDENT = T001W-FABKL .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM TFACT WHERE
IDENT = TFACD-IDENT AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZMMMV_T001W-LTEXT =
TFACT-LTEXT .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZMMMV_T001W-LTEXT .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZMMMV_T001W-LTEXT .
    ENDIF.
ENDFORM.