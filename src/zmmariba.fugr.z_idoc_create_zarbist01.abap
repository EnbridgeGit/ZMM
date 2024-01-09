FUNCTION z_idoc_create_zarbist01 .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_DOC_TYP) TYPE  BLART OPTIONAL
*"     REFERENCE(IM_INV_DOC_NO) TYPE  RE_BELNR OPTIONAL
*"     REFERENCE(IM_VND_NO) TYPE  LIFNR OPTIONAL
*"     REFERENCE(IM_ARIB_COM_SUP) TYPE  EMNFR OPTIONAL
*"     REFERENCE(IM_SUP_INV_NO) TYPE  XBLNR OPTIONAL
*"     REFERENCE(IM_INV_STATUS) TYPE  BKTXT OPTIONAL
*"     REFERENCE(IM_ZTEXT) TYPE  CHAR255 OPTIONAL
*"     REFERENCE(IM_INV_AMT) TYPE  WRBTR OPTIONAL
*"     REFERENCE(IM_YEAR) TYPE  GJAHR OPTIONAL
*"     REFERENCE(IM_PORT) TYPE  EDI_RCVPOR DEFAULT 'SONIC'
*"     REFERENCE(IM_PARTNO) TYPE  EDI_RCVPRN DEFAULT 'SONIC'
*"     REFERENCE(IM_PARTTYPE) TYPE  EDI_RCVPRT DEFAULT 'LS'
*"     REFERENCE(IM_REJ_REASON) TYPE  CHAR255 OPTIONAL
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

  DATA: ls_inv_h    TYPE z1bp_incinv_header_status.




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

  ls_ctr_rec-mestyp = 'ZARBIST'.           "Message Type
  ls_ctr_rec-idoctp = 'ZARBIST01'.         "Basic type
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

  ls_inv_h-zdoc_typ       = im_doc_typ.
  ls_inv_h-zinv_doc_no    = im_inv_doc_no.
  ls_inv_h-zvnd_no        = im_vnd_no.
  ls_inv_h-zarib_com_sup  = im_arib_com_sup.
  ls_inv_h-zsup_inv_no    = im_sup_inv_no.
  ls_inv_h-zinv_status    = im_inv_status.
  ls_inv_h-ztext          = im_ztext.
  ls_inv_h-zinv_amt       = im_inv_amt.
  ls_inv_h-zyear          = im_year.

  ls_inv_h-zzrej_reason   = im_rej_reason.    "(+) SDP 60131

  ls_idocdata-segnam = 'Z1BP_INCINV_HEADER_STATUS'.
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
* Begin of Changes by <Chaya> <17/05/2023> <D30K932464> "Update Task
*      COMMIT WORK,
* End of Changes by <Chaya> <17/05/2023> <D30K932464> "Update Task
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
