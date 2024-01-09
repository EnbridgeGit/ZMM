*----------------------------------------------------------------------*
***INCLUDE ZXM06I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_ZZMSA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_zzmsa INPUT.
  DATA: BEGIN OF ls_zzmsa,
           zzparty_agmt_id TYPE zmmt_mastagree-zzparty_agmt_id,
           zzmsa TYPE zmmt_mastagree-zzmsa,
           bukrs TYPE bukrs,
           zzdescp TYPE zmmt_mastagree-zzdescp,
           zzfromdate TYPE zmmt_mastagree-zztodate,
           zztodate TYPE zmmt_mastagree-zztodate,
           zzstatus TYPE zmmt_mastagree-zzstatus,
           zzmsatype TYPE zmmt_mastagree-zzmsatype,
           zzcpid TYPE zmmt_mastagree-zzcpid,
        END OF ls_zzmsa,
         BEGIN OF ls_zzmsa_f4,
           zzparty_agmt_id TYPE zmmt_mastagree-zzparty_agmt_id,
           zzmsa TYPE zmmt_mastagree-zzmsa,
           bukrs TYPE bukrs,
           zzdescp TYPE zmmt_mastagree-zzdescp,
           zzfromdate TYPE zmmt_mastagree-zztodate,
           zztodate TYPE zmmt_mastagree-zztodate,
           zzmsatype TYPE zmmt_mastagree-zzmsatype,
           lifnr     TYPE lfm1-lifnr,
           name1     TYPE lfa1-name1,
           name2 type lfa1-name2,       "Added for change D30K932602
           regio type ZMMS_ZZMSA-regio,  "Added for change D30K932602
           land1 type lfa1-land1,        "Added for change D30K932602

        END OF ls_zzmsa_f4,
        BEGIN OF ls_lfm1,
          lifnr TYPE lfm1-lifnr,
          eikto TYPE lfm1-eikto,
        END OF ls_lfm1,
        lt_lfm1 LIKE TABLE OF ls_lfm1,
        BEGIN OF ls_lfa1,
          lifnr TYPE lfm1-lifnr,
          name1 TYPE lfa1-name1,
          name2 type lfa1-name2,  "Added for change D30K932602
          regio type lfa1-regio,  "Added for change D30K932602
          land1 type lfa1-land1,  "Added for change D30K932602
        END OF ls_lfa1,
        lt_lfa1 LIKE TABLE OF ls_lfa1,
        lt_zzmsa LIKE TABLE OF ls_zzmsa,
        lt_zzmsa_f4 LIKE TABLE OF ls_zzmsa_f4.

  DATA: lr_agreetype TYPE RANGE OF zmmt_mastagree-zzmsatype,
        ls_agreetype LIKE LINE OF lr_agreetype.
  REFRESH : lt_zzmsa_f4, lt_zzmsa,lr_agreetype.
  IF sy-tcode = 'ME31L' OR sy-tcode = 'ME32L' OR sy-tcode = 'ME33L'.
    ls_agreetype-sign = 'I'.
    ls_agreetype-option = 'EQ'.
    ls_agreetype-low = 'G'.
    APPEND ls_agreetype TO lr_agreetype.
    CLEAR ls_agreetype.
  ELSEIF sy-tcode = 'ME31K' OR sy-tcode = 'ME32K' OR sy-tcode = 'ME33K'.
    ls_agreetype-sign = 'I'.
    ls_agreetype-option = 'EQ'.
    ls_agreetype-low = 'T'.
    APPEND ls_agreetype TO lr_agreetype.
    CLEAR ls_agreetype.

    ls_agreetype-sign = 'I'.
    ls_agreetype-option = 'EQ'.
    ls_agreetype-low = 'S'.
    APPEND ls_agreetype TO lr_agreetype.
    CLEAR ls_agreetype.
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
        input             = t_dynfld-fieldvalue
     IMPORTING
       OUTPUT             = gf_fromdate
     EXCEPTIONS
       INVALID_DATE       = 1
       OTHERS             = 2              .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.
  READ TABLE t_dynfld WITH KEY fieldname = 'EKKO-KDATE'.
  IF sy-subrc = 0.
    CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
      EXPORTING
        input             = t_dynfld-fieldvalue
     IMPORTING
       OUTPUT             = gf_todate
     EXCEPTIONS
       INVALID_DATE       = 1
       OTHERS             = 2              .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.
*End of Change NAGIRIR
  SELECT zzparty_agmt_id
         zzmsa
         bukrs
         zzdescp
         zzfromdate
         zztodate
         zzstatus
         zzmsatype
         zzcpid
    FROM zmmt_mastagree
    INTO TABLE lt_zzmsa WHERE bukrs = gf_bukrs
                         AND zzmsatype IN lr_agreetype
                         AND zzstatus = 'A'
                         AND zzfromdate LE gf_fromdate
                         AND zztodate GE gf_todate
                         AND zzcpid = gf_eikto.
  IF lt_zzmsa IS NOT INITIAL.
    SELECT lifnr eikto FROM lfm1
      INTO TABLE lt_lfm1
      FOR ALL ENTRIES IN lt_zzmsa
      WHERE eikto = lt_zzmsa-zzcpid
       AND ekorg = 'GASA'.
    IF lt_lfm1 IS NOT INITIAL.
      SELECT lifnr name1 name2 regio land1
        FROM lfa1
        INTO TABLE lt_lfa1
        FOR ALL ENTRIES IN lt_lfm1
        WHERE lifnr = lt_lfm1-lifnr.
    ENDIF.

  ENDIF.
  LOOP AT lt_zzmsa INTO ls_zzmsa.
    ls_zzmsa_f4-zzparty_agmt_id = ls_zzmsa-zzparty_agmt_id.
    ls_zzmsa_f4-zzmsa = ls_zzmsa-zzmsa.
    ls_zzmsa_f4-bukrs = ls_zzmsa-bukrs.
    ls_zzmsa_f4-zzdescp = ls_zzmsa-zzdescp+0(60).
    ls_zzmsa_f4-zzfromdate = ls_zzmsa-zzfromdate.
    ls_zzmsa_f4-zztodate = ls_zzmsa-zztodate.
    ls_zzmsa_f4-zzmsatype = ls_zzmsa-zzmsatype.
    READ TABLE lt_lfm1 INTO ls_lfm1 WITH KEY eikto = ls_zzmsa-zzcpid.
    IF sy-subrc = 0.
      ls_zzmsa_f4-lifnr = ls_lfm1-lifnr.
    ENDIF.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_lfm1-lifnr.
    IF sy-subrc = 0.
      ls_zzmsa_f4-name1 = ls_lfa1-name1.
      ls_zzmsa_f4-name2 = ls_lfa1-name2. "Added for change D30K932602
      ls_zzmsa_f4-regio = ls_lfa1-regio. "Added for change D30K932602
      ls_zzmsa_f4-land1 = ls_lfa1-land1. "Added for change D30K932602
    ENDIF.
    APPEND ls_zzmsa_f4 TO lt_zzmsa_f4.
    CLEAR : ls_zzmsa_f4,ls_zzmsa,ls_lfa1,ls_lfm1.
  ENDLOOP.

  IF sy-subrc = 0.
    CLEAR: lr_agreetype[].
  ENDIF.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZZMSA'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      dynprofield = 'ZZMSA'
      value       = ' '
      value_org   = 'S'
    TABLES
      value_tab   = lt_zzmsa_f4.

  CLEAR: lt_zzmsa[] ,lt_zzmsa_f4[],lt_lfm1[],lt_lfa1[].
ENDMODULE.                 " F4_ZZMSA  INPUT
