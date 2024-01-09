*----------------------------------------------------------------------*
***INCLUDE ZXM06I02 .
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 13-APR-2022 DADIM        D30K932132 CHG0246647 - MSA Field Optional  *
*                                     Requirement for G05 only         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  CHECK_ZZMSA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zzmsa INPUT.
  DATA: ls_mastagree TYPE zmmt_mastagree,
        ls_mastagree1 TYPE zmmt_mastagree,
        lr_agreetyp TYPE RANGE OF zmmt_mastagree-zzmsatype,
        ls_agreetyp LIKE LINE OF lr_agreetyp.
  CLEAR: ls_mastagree, ls_mastagree1, ls_agreetyp, lr_agreetyp.
  REFRESH lr_agreetyp.
*Start of changes by DADIM for CHG0246647
*  CHECK zmmt_mastagree-zzmsa IS NOT INITIAL.
  IF zmmt_mastagree-zzmsa IS NOT  INITIAL.
*End of changes by DADIM for CHG0246647
    IF sy-tcode = 'ME31L' OR sy-tcode = 'ME32L' .
      ls_agreetyp-sign = 'I'.
      ls_agreetyp-option = 'EQ'.
      ls_agreetyp-low = 'G'.
      APPEND ls_agreetyp TO lr_agreetyp.
      CLEAR ls_agreetyp.
    ELSEIF sy-tcode = 'ME31K' OR sy-tcode = 'ME32K'.
      ls_agreetyp-sign = 'I'.
      ls_agreetyp-option = 'EQ'.
      ls_agreetyp-low = 'T'.
      APPEND ls_agreetyp TO lr_agreetyp.
      CLEAR ls_agreetyp.

      ls_agreetyp-sign = 'I'.
      ls_agreetyp-option = 'EQ'.
      ls_agreetyp-low = 'S'.
      APPEND ls_agreetyp TO lr_agreetyp.
      CLEAR ls_agreetyp.
    ENDIF.
*Begin of Change NAGIRIR
    REFRESH: t_dynfld.
    t_dynfld-fieldname = 'EKKO-KDATB'.  " From Date
    APPEND t_dynfld.
    t_dynfld-fieldname = 'EKKO-KDATE'.  " To Date
    APPEND t_dynfld.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = 'SAPMM06E'
        dynumb     = '0201'
      TABLES
        dynpfields = t_dynfld.
    READ TABLE t_dynfld WITH KEY fieldname = 'EKKO-KDATB'.
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
        EXPORTING
          input        = t_dynfld-fieldvalue
        IMPORTING
          output       = gf_fromdate
        EXCEPTIONS
          invalid_date = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
    READ TABLE t_dynfld WITH KEY fieldname = 'EKKO-KDATE'.
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
        EXPORTING
          input        = t_dynfld-fieldvalue
        IMPORTING
          output       = gf_todate
        EXCEPTIONS
          invalid_date = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
*End of Change NAGIRIR
    SELECT SINGLE *
      FROM zmmt_mastagree
      INTO ls_mastagree
      WHERE zzmsa = zmmt_mastagree-zzmsa
        AND bukrs = gf_bukrs
        AND zzstatus = 'A'
        AND zzmsatype IN lr_agreetyp
        AND zzfromdate LE gf_fromdate
        AND zztodate GE gf_todate
        AND zzcpid = gf_eikto.
    IF sy-subrc NE 0.
      SELECT SINGLE *
    FROM zmmt_mastagree
    INTO ls_mastagree1
    WHERE zzmsa = zmmt_mastagree-zzmsa.
      IF sy-subrc = 0.
        IF ls_mastagree1-bukrs NE gf_bukrs.
          MESSAGE 'Invalid Company code  & ZZMSA' TYPE 'E'.
        ELSEIF ls_mastagree1-zzcpid NE gf_eikto.
          MESSAGE 'Incorrect Counter Party ID' TYPE 'E'.
        ELSEIF gf_fromdate LT ls_mastagree-zzfromdate.
          MESSAGE 'Agreement validity dates are outside of MSA agreement dates' TYPE 'E'.
        ELSEIF gf_todate GT ls_mastagree-zztodate.
          MESSAGE 'Agreement validity dates are outside of MSA agreement dates' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Master Service Agreement is Invalid' TYPE 'E'.
      ENDIF.
    ELSE.
      ekko-zzparty_agmt_id = ls_mastagree-zzparty_agmt_id.
    ENDIF.
*Start of changes by DADIM for CHG0246647
  ELSE.
    ekko-zzparty_agmt_id = ''.
  ENDIF.
*End of changes by DADIM for CHG0246647

ENDMODULE.                 " CHECK_ZZMSA  INPUT
