*&---------------------------------------------------------------------*
*& Report  ZMOVE_INBEL_CCBEL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMOVE_INBEL_CCBEL.
TABLES: ptrv_head,
        ptrv_perio,
        pcl1.

INCLUDE rpc1te00.

PARAMETERS:     pnr LIKE ptrv_head-pernr OBLIGATORY,
                guid_ccc  LIKE ptk34-guid_ccc OBLIGATORY.


DATA:   letzt TYPE ptk34_letzt,
        ccbel TYPE TABLE OF ptk34,
        inbel TYPE TABLE OF ptk34_inbel,
        trans TYPE TABLE OF ptk34_trans,
        pdvrs LIKE ptrv_perio-pdvrs.

DATA: wa_ccbel LIKE ptk34,
      wa_inbel LIKE ptk34_inbel.

DATA: rp_imp_tc_subrc LIKE sy-subrc,
      rp_exp_tc_subrc LIKE sy-subrc.

DATA:   lock_key LIKE pcl1-srtfd.

MOVE pnr TO lock_key.

CALL FUNCTION 'ENQUEUE_EPCLTC'
  EXPORTING
    relid          = 'TC'
    srtfd          = lock_key
    _wait          = 'X'
  EXCEPTIONS
    foreign_lock   = 1
    system_failure = 2
    OTHERS         = 3.

IF sy-subrc IS NOT INITIAL.
  WRITE: 'error: employee locked'.
  STOP.
ENDIF.

CALL FUNCTION 'PTRM_UTIL_IMPORT_TC_CLUSTER'
  EXPORTING
    i_pernr  = pnr
  IMPORTING
    et_ccbel = ccbel
    et_inbel = inbel
    et_trans = trans
    e_letzt  = letzt
    e_subrc  = rp_imp_tc_subrc.

IF rp_imp_tc_subrc IS INITIAL.

  LOOP AT inbel INTO wa_inbel WHERE
       guid_ccc = guid_ccc.
    CLEAR: wa_inbel-reinr, wa_inbel-belnr.
    MOVE-CORRESPONDING wa_inbel TO wa_ccbel.
    DELETE inbel INDEX sy-tabix.
    APPEND wa_ccbel TO ccbel.
    EXIT.
  ENDLOOP.

  IF sy-subrc IS NOT INITIAL.

    WRITE: 'error: this Guid_ccc is not existing in INBEL.'.

  ENDIF.

ELSE.

  WRITE: 'error: error when imporing credit card cluster.'.

ENDIF.

IF ( sy-subrc IS INITIAL ) AND ( rp_imp_tc_subrc IS INITIAL ).

  CALL FUNCTION 'PTRM_UTIL_EXPORT_TC_CLUSTER'
    EXPORTING
      i_pernr  = pnr
      it_ccbel = ccbel
      it_inbel = inbel
      it_trans = trans
      i_letzt  = letzt
    IMPORTING
      e_subrc  = rp_exp_tc_subrc.

  IF rp_exp_tc_subrc IS NOT INITIAL.

    WRITE: 'error: error when exporting credit card cluster.'.

  ELSE.

    WRITE: 'ok. No errors.'.

  ENDIF.

ELSE.

  WRITE: 'not changed'.

ENDIF.


CALL FUNCTION 'DEQUEUE_EPCLTC'
  EXPORTING
    relid  = 'TC'
    srtfd  = lock_key
  EXCEPTIONS
    OTHERS = 1.
