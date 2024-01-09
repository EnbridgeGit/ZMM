*&---------------------------------------------------------------------*
*& Subroutine Pool   ZMPUM001_CLM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       : ZMPUM001_CLM                                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 25-Apr-2014                                    *
*& Object ID          : SDP57610 Modify Purchase Order form to include *
*&                      CLM Contract Number                            *
*& Application Area   : MM                                             *
*& Description        : External subroutine to get CLM Contract Number *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*Modification Log(Latest Version on Top)                               *
*----------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 11/18/2015                                           *
* Modified By   : Praveena Anusuri                                     *
* Object ID     : SDP88669                                             *
* Description   : Modify PO form to include Ariba/CLM Contract Number. *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 07/03/2015                                           *
* Modified By   : Praveena Anusuri                                     *
* Object ID     : SDP86610                                             *
* Description   : Modify PO form to include Ariba Contract Number.     *
*----------------------------------------------------------------------*

PROGRAM  zmpum001_clm.
*&---------------------------------------------------------------------*
*&      Form  GET_CLM_NUMBER
*&---------------------------------------------------------------------*
*&      Get CLM Contract Number
*----------------------------------------------------------------------*
FORM get_clm_number TABLES lt_intab STRUCTURE itcsy
                           lt_outtab STRUCTURE itcsy .
* BOI by PANUSURI Ticket 88669
  TYPES: BEGIN OF lty_ekpo,
           ebeln TYPE ebeln,
           ebelp TYPE ebelp,
         END OF lty_ekpo.
* EOI by PANUSURI Ticket 88669
  DATA: lv_konnr      TYPE ekpo-konnr,
        lv_ktpnr      TYPE ekpo-ktpnr,
        lv_clm_number TYPE char255,
        lv_temp1      TYPE string,
        lv_temp2      TYPE string,
        lv_name       TYPE thead-tdname.
  DATA: lwa_intab     TYPE itcsy,
        lwa_outtab    TYPE itcsy,
        lt_text       TYPE STANDARD TABLE OF tline,
        lwa_text      TYPE tline,
        lt_ekpo       TYPE STANDARD TABLE OF lty_ekpo,  "(+)PANUSURI Ticket 88669
        lwa_ekpo      TYPE lty_ekpo.                    "(+)PANUSURI Ticket 88669

  SORT lt_intab BY name.
  CLEAR lwa_intab.
* BOC by PANUSURI Ticket 88669
*  READ TABLE lt_intab INTO lwa_intab WITH KEY name = 'GWA_EKPO-KONNR'
*                                              BINARY SEARCH.
*  IF sy-subrc = 0.
*    lv_konnr = lwa_intab-value.
*  ENDIF.
*
*  CLEAR lwa_intab.
*  READ TABLE lt_intab INTO lwa_intab WITH KEY name = 'GWA_EKPO-KTPNR'
*                                              BINARY SEARCH.
*  IF sy-subrc = 0.
*    lv_ktpnr = lwa_intab-value.
*  ENDIF.
* EOC by PANUSURI Ticket 88669
* BOI by PANUSURI Ticket 88669
  READ TABLE lt_intab INTO lwa_intab WITH KEY name = 'EKKO-KONNR'
                                              BINARY SEARCH.
  IF sy-subrc = 0.
    SELECT ebeln
           ebelp
           FROM ekpo
           INTO TABLE lt_ekpo
           WHERE ebeln = lwa_intab-value
           AND   loekz = space.
    IF sy-subrc = 0.
      SORT lt_ekpo BY ebeln ebelp.
    ENDIF.
  ENDIF.
  READ TABLE lt_ekpo INTO lwa_ekpo INDEX 1.
  IF sy-subrc = 0.
    lv_konnr = lwa_ekpo-ebeln.
    lv_ktpnr = lwa_ekpo-ebelp.
  ELSE.
    CLEAR: lv_konnr,
           lv_ktpnr.
  ENDIF.
* EOI by PANUSURI Ticket 88669

  CONCATENATE lv_konnr lv_ktpnr INTO lv_name.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'K06'
      language                = 'E'
      name                    = lv_name
      object                  = 'EKPO'
    TABLES
      lines                   = lt_text
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc = 0.
    CLEAR lwa_text.
*   BOI by PANUSURI Ticket 86610
    READ TABLE lt_text INTO lwa_text INDEX 1.
    SHIFT lwa_text-tdline LEFT DELETING LEADING space.
    IF lwa_text-tdline = '0000000000 / 00000'. "CLM Contract Number
*   EOI by PANUSURI Ticket 86610
      READ TABLE lt_text INTO lwa_text INDEX 2.
      IF sy-subrc = 0.
        CLEAR : lv_temp1,
                lv_temp2,
                lv_clm_number.
        SPLIT lwa_text-tdline AT ':' INTO lv_temp1 lv_temp2.
        CLEAR lv_temp1.
        SPLIT lv_temp2 AT '/' INTO lv_clm_number lv_temp1.
        SHIFT lv_clm_number LEFT DELETING LEADING space.
      ENDIF.
*   BOI by PANUSURI Ticket 86610
    ELSE.
      lv_clm_number = lwa_text-tdline. "Ariba Contract Number
    ENDIF.
*   EOI by PANUSURI Ticket 86610
  ENDIF.
  SORT lt_outtab BY name.
  CLEAR lwa_outtab.
  READ TABLE lt_outtab INTO lwa_outtab WITH KEY name = 'LV_CLM_NUMBER'
                                                        BINARY SEARCH.
  lwa_outtab-value = lv_clm_number.
  MODIFY lt_outtab FROM lwa_outtab INDEX sy-tabix TRANSPORTING value.

  CLEAR: lv_konnr,
         lv_ktpnr,
         lv_temp1,
         lv_temp2,
         lv_clm_number.
  REFRESH: lt_text.

ENDFORM.                    " GET_CLM_NUMBER
