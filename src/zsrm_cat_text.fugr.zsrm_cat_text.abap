FUNCTION ZSRM_CAT_TEXT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      OUTPUT_TAB TYPE  ZSRM_T_CAT_TEXT
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
* Function Name      :  ZSRM_CAT_TEXT                                  *
* Author             :  AKMADASU                                       *
* Date               :  28-May-2020                                    *
* Technical Contact  :  Ashok Madasu                                   *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  To Fetch Material document text and send to    *
*                    :  SRM System                                     *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 28-May-2020  AKMADASU    D30K930572 CHG0180139   Initial Development *
*                                                                      *
*&---------------------------------------------------------------------*
  TYPES: BEGIN OF LTY_T023T,
         MATKL        TYPE MATKL,
         WGBEZ        TYPE WGBEZ,
         END OF LTY_T023T.
  DATA:  LT_T023T     TYPE TABLE OF LTY_T023T,
         LS_T023T     TYPE LTY_T023T.
  FIELD-SYMBOLS : <LS_OUTPUT_TAB> TYPE ZSRM_CAT_TEXT.
  CLEAR:LT_T023T[].
  SELECT MATKL WGBEZ from T023T
                       into TABLE lt_T023T
                       FOR ALL ENTRIES IN OUTPUT_TAB
                       where matkl = output_tab-matkl.
  IF sy-subrc is INITIAL.
    sort lt_t023t by matkl.
  ENDIF.
  LOOP AT OUTPUT_TAB ASSIGNING <LS_OUTPUT_TAB>.
    READ TABLE LT_T023T INTO LS_T023T WITH KEY matkl =
<LS_OUTPUT_TAB>-matkl BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      <LS_OUTPUT_TAB>-WGBEZ = LS_T023T-WGBEZ.
      CLEAR:LS_T023T.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
