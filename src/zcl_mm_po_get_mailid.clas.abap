class ZCL_MM_PO_GET_MAILID definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_MM_PO_GET_MAILID
*"* do not include other source files here!!!

  interfaces BI_OBJECT .
  interfaces BI_PERSISTENT .
  interfaces IF_WORKFLOW .

  class-methods CLASS_CONSTRUCTOR .
  class-methods NOTIFY .
  class-methods GET_MAIL_ID .
protected section.
*"* protected components of class ZCL_MM_PO_GET_MAILID
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_MM_PO_GET_MAILID
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_MM_PO_GET_MAILID IMPLEMENTATION.


method BI_OBJECT~DEFAULT_ATTRIBUTE_VALUE.
endmethod.


method BI_OBJECT~EXECUTE_DEFAULT_METHOD.
endmethod.


method BI_OBJECT~RELEASE.
endmethod.


method BI_PERSISTENT~FIND_BY_LPOR.
endmethod.


method BI_PERSISTENT~LPOR.
endmethod.


method BI_PERSISTENT~REFRESH.
endmethod.


method CLASS_CONSTRUCTOR.
endmethod.


METHOD get_mail_id.

  TYPES : BEGIN OF lty_email_rec,
          banfn TYPE banfn,
          bnfpo TYPE bnfpo,
          type TYPE char1,
          email TYPE string,
          END OF lty_email_rec.

  DATA: lit_orders TYPE TABLE OF ekpo,
        lv_email_addr TYPE bapiaddr3,
        lit_req TYPE TABLE OF eban,
        lwa_req TYPE eban,
        lv_username TYPE bapibname-bapibname,
        lit_param TYPE TABLE OF bapiparam,
        lit_return TYPE TABLE OF bapiret2,
        lwa_param TYPE bapiparam,
        lwa_emailid TYPE zmms_mail_recipient,
        lit_email_rec TYPE TABLE OF lty_email_rec,
        lwa_email_rec TYPE lty_email_rec.
*
*  SELECT *
*   FROM ekpo
*   INTO TABLE lit_orders
*   WHERE ebeln = iv_po_num.
*  IF sy-subrc = 0.
*    SELECT *
*      FROM eban
*      INTO TABLE lit_req
*      FOR ALL ENTRIES IN lit_orders
*           WHERE banfn = lit_orders-banfn AND
*            bnfpo = lit_orders-bnfpo AND
*            estkz <> 'B'.
*    IF sy-subrc = 0.
*      LOOP AT lit_req INTO lwa_req.
*        lv_username = lwa_req-ernam.
*        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*          EXPORTING
*            username  = lv_username
*          IMPORTING
*            address   = lv_email_addr
*          TABLES
*            parameter = lit_param
*            return    = lit_return.
*        IF lit_param IS NOT INITIAL.
**          READ TABLE lit_param INTO lwa_param WITH KEY parid = 'Z_NO_CRT_EM_PO'.
**          IF sy-subrc = 0.
**            IF lwa_param-parva = 'X'.
**              CONTINUE.
**            ELSE.
*          lwa_emailid-smtp_addr = lv_email_addr-e_mail.
**          APPEND lwa_emailid TO et_emailid.
*
*                    lwa_emailid-smtp_addr = 'Kavya.mb@enbridge.com'.
**          APPEND lwa_emailid TO et_emailid.
**            ENDIF.
**          ENDIF.
*        ENDIF.
*      ENDLOOP.
**      SORT et_emailid.
**      DELETE ADJACENT DUPLICATES FROM et_emailid. "If the Requistioner is same in more than one line item,only one mail should trigger
*
*   ENDIF.
*  ENDIF.

* * Data Declarations
  DATA: lt_mailsubject     TYPE sodocchgi1.
  DATA: lt_mailrecipients  TYPE STANDARD TABLE OF somlrec90.
  DATA: lwa_mailrecipients  TYPE somlrec90.
  DATA: lt_mailtxt         TYPE STANDARD TABLE OF soli.
  DATA: lwa_mailtxt         TYPE soli.

** Recipients
**      LOOP AT lit_mailid INTO lwa_mailid.
*  lwa_mailrecipients-rec_type  = 'U'.
*  lwa_mailrecipients-receiver = 'Kavya.mb@enbridge.com'.
**      lwa_mailrecipients-receiver = 'KAVYA.MB@ENBRIDGE.COM'.
*  APPEND lwa_mailrecipients TO lt_mailrecipients .
*  CLEAR lwa_mailrecipients .
**      ENDLOOP.
*
** Subject.
*  lt_mailsubject-obj_name = 'PO/SRO created from Requisition'.
*  lt_mailsubject-obj_langu = sy-langu.
*  lt_mailsubject-obj_descr = 'PO/SRO created from Requisition'.
*
** Mail Contents
*  CONCATENATE 'Requisition'  'created the following Purchase order'  INTO lwa_mailtxt SEPARATED BY space.
*  APPEND lwa_mailtxt TO lt_mailtxt.
*  CLEAR lwa_mailtxt.
** Send Mail
*  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
*    EXPORTING
*      document_data              = lt_mailsubject
*      put_in_outbox              = 'X'
*    TABLES
*      object_content             = lt_mailtxt
*      receivers                  = lt_mailrecipients
*    EXCEPTIONS
*      too_many_receivers         = 1
*      document_not_sent          = 2
*      document_type_not_exist    = 3
*      operation_no_authorization = 4
*      parameter_error            = 5
*      x_error                    = 6
*      enqueue_error              = 7
*      OTHERS                     = 8.
*  IF sy-subrc EQ 0.
*    COMMIT WORK.
**   Push mail out from SAP outbox
**        SUBMIT rsconn01 WITH mode = 'INT' AND RETURN.
*  ENDIF.

ENDMETHOD.


method NOTIFY.
endmethod.
ENDCLASS.
