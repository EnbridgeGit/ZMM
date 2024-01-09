FUNCTION z_idoc_create_zarbrem01 .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_INV_DOC_NO) TYPE  RE_BELNR OPTIONAL
*"     REFERENCE(IM_VND_NO) TYPE  LIFNR OPTIONAL
*"     REFERENCE(IM_VND_NAME1) TYPE  NAME1_GP OPTIONAL
*"     REFERENCE(IM_SUP_INV_NO) TYPE  XBLNR OPTIONAL
*"     REFERENCE(IM_ARIB_COM_SUP) TYPE  EMNFR OPTIONAL
*"     REFERENCE(IM_PO) TYPE  ESRRE OPTIONAL
*"     REFERENCE(IM_GROSS_AMT) TYPE  DMBTR OPTIONAL
*"     REFERENCE(IM_PAID_AMT) TYPE  RWBTR OPTIONAL
*"     REFERENCE(IM_PYMT_DATE) TYPE  BUDAT OPTIONAL
*"     REFERENCE(IM_DISC_AMT) TYPE  SKNTO OPTIONAL
*"     REFERENCE(IM_PYMT_REF) TYPE  FM_UMART OPTIONAL
*"     REFERENCE(IM_PYMT_TYPE) TYPE  CHECF OPTIONAL
*"     REFERENCE(IM_DOC_DATE) TYPE  BUDAT OPTIONAL
*"     REFERENCE(IM_PYMT_ID) TYPE  VBLNR OPTIONAL
*"     REFERENCE(IM_PORT) TYPE  EDI_RCVPOR DEFAULT 'SONIC'
*"     REFERENCE(IM_PARTNO) TYPE  EDI_RCVPRN DEFAULT 'SONIC'
*"     REFERENCE(IM_PARTTYPE) TYPE  EDI_RCVPRT DEFAULT 'LS'
*"  EXCEPTIONS
*"      ERROR_IDOC_CONTROL
*"      ERROR_IDOC_STATUS
*"      ERROR_IDOC_DATA
*"      ERROR_LOGICAL_SYSTEM_UNKNOWN
*"      ERROR_OTHER
*"----------------------------------------------------------------------
  DATA: ls_ctr_rec  TYPE edidc,
        lt_ctr_rec  LIKE TABLE OF ls_ctr_rec,
        ls_idocdata TYPE edidd,
        lt_idocdata LIKE TABLE OF ls_idocdata,
        lv_logsys   TYPE tbdls-logsys.

  DATA: ls_inv_h    TYPE z1bp_incinv_hdr_rem_adv.




  CLEAR: lt_ctr_rec,lt_idocdata, lv_logsys.


*****************
** Control Record
*****************
  CLEAR: ls_ctr_rec.

* Specify Name of Current Logged-on System (client in R/3 System)
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = lv_logsys
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 0.

  IF sy-subrc = 1.
    RAISE error_logical_system_unknown.
  ENDIF.

  ls_ctr_rec-mestyp = 'ZARBREM'.           "Message Type
  ls_ctr_rec-idoctp = 'ZARBREM01'.         "Basic type
  "ls_ctr_rec-cimtyp = ''.                  "Extension

  ls_ctr_rec-sndpor = space.            "Sender port-SAP System
  ls_ctr_rec-sndprn = lv_logsys.                "Partner # of Sender
  ls_ctr_rec-sndprt = 'LS'.                 "Partner type of sender

  ls_ctr_rec-rcvpor = im_port.              "Receiver port-SAP System
  ls_ctr_rec-rcvprn = im_partno.            "Partner # of Receiver
  ls_ctr_rec-rcvprt = im_parttype.          "Partner Type of Receiver



*****************
** Data Record
*****************
  CLEAR: ls_inv_h, ls_idocdata.

  ls_inv_h-zinv_doc_no    = im_inv_doc_no.
  ls_inv_h-zvnd_no        = im_vnd_no.
  ls_inv_h-zvnd_name1     = im_vnd_name1.
  ls_inv_h-zsup_inv_no    = im_sup_inv_no.
  ls_inv_h-zarib_com_sup  = im_arib_com_sup.
  ls_inv_h-zpo            = im_po.
  ls_inv_h-zgross_amt     = im_gross_amt.
  ls_inv_h-zpaid_amt      = im_paid_amt.
  ls_inv_h-zpymt_date     = im_pymt_date.
  ls_inv_h-zdisc_amt      = im_disc_amt.
  ls_inv_h-zpymt_ref      = im_pymt_ref.
  ls_inv_h-zpymt_type     = im_pymt_type.
  ls_inv_h-zdoc_date      = im_doc_date.
  ls_inv_h-zpymt_id       = im_pymt_id. "(+) PANUSURI Ticket 46414

  ls_idocdata-segnam = 'Z1BP_INCINV_HDR_REM_ADV'.
  ls_idocdata-sdata = ls_inv_h.

  APPEND ls_idocdata TO lt_idocdata.



*****************
** Send Idoc
*****************
  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
    EXPORTING
      master_idoc_control            = ls_ctr_rec
    TABLES
      communication_idoc_control     = lt_ctr_rec
      master_idoc_data               = lt_idocdata
    EXCEPTIONS
      error_in_idoc_control          = 1
      error_writing_idoc_status      = 2
      error_in_idoc_data             = 3
      sending_logical_system_unknown = 4
      OTHERS                         = 5.

  CASE sy-subrc.
    WHEN 0.
      "Do nothing.
      "CALL FUNCTION 'DB_COMMIT'.
      COMMIT WORK.
      CALL FUNCTION 'DEQUEUE_ALL'.

    WHEN 1.
      ROLLBACK WORK.
      RAISE error_idoc_control.

    WHEN 2.
      ROLLBACK WORK.
      RAISE error_idoc_status.
    WHEN 3.
      ROLLBACK WORK.
      RAISE error_idoc_data.
    WHEN 4.
      ROLLBACK WORK.
      RAISE error_logical_system_unknown.
    WHEN OTHERS.
      ROLLBACK WORK.
      RAISE error_other.
  ENDCASE.

ENDFUNCTION.
