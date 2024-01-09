class ZCL_MM_PO_SEND_MAIL_NEW definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_MM_PO_SEND_MAIL_NEW
*"* do not include other source files here!!!

  interfaces BI_OBJECT .
  interfaces BI_PERSISTENT .
  interfaces IF_WORKFLOW .

  class-methods CLASS_CONSTRUCTOR .
  class-methods NOTIFY .
  class-methods SEND_MAIL
    importing
      !IV_PO_NUM type EKKO-EBELN .
protected section.
*"* protected components of class ZCL_MM_PO_SEND_MAIL_NEW
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_MM_PO_SEND_MAIL_NEW
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_MM_PO_SEND_MAIL_NEW IMPLEMENTATION.


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


method NOTIFY.
endmethod.


METHOD send_mail.
***********************************Change log***************************************************************
************************************************************************************************************
*Developer     Date      Details
*kmb           5/3/18    CHG0102802 PO creation email changes WF 05.03.18
************************************************************************************************************
* data declarations
*Variables
  DATA:  lv_po TYPE ekpo-ebeln,
         lv_email_addr TYPE bapiaddr3,
         lv_username TYPE bapibname-bapibname,
         lv_req TYPE banfn,
*BOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
         lv_order TYPE aufnr,
         lv_bsart TYPE bsart,
*EOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
         lv_ebeln TYPE ebeln.

*Internal tables and workareas
  DATA : lit_req TYPE TABLE OF eban,
         lwa_req TYPE eban,
         lit_param TYPE TABLE OF bapiparam,
         lit_return TYPE TABLE OF bapiret2,
         lwa_param TYPE bapiparam,
         lit_mailid TYPE TABLE OF zmms_mail_recipient_new,
         lwa_mailid TYPE zmms_mail_recipient_new,
         lit_orders TYPE TABLE OF ekpo,
         lwa_orders TYPE ekpo,
*BOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
         lit_ekkn TYPE TABLE OF ekkn,
         lwa_ekkn TYPE ekkn,
*EOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
         lt_mailsubject TYPE sodocchgi1,
         lt_mailrecipients TYPE STANDARD TABLE OF somlrec90,
         lwa_mailrecipients TYPE somlrec90,
         lt_mailtxt  TYPE STANDARD TABLE OF soli,
         lwa_mailtxt TYPE soli.

*Constants
  CONSTANTS : lc_zf(2) TYPE c VALUE 'ZF',
         lc_nb(2) TYPE c VALUE 'NB',
         lc_b TYPE c VALUE 'B',
         lc_u TYPE c VALUE 'U',
         lc_x TYPE c VALUE 'X',
         lc_x1 TYPE c VALUE 'x'.

*BOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
*  SELECT SINGLE ebeln
*    FROM ekko
*    INTO lv_ebeln
*    WHERE ebeln = iv_po_num.
  SELECT SINGLE ebeln bsart
    FROM ekko
    INTO (lv_ebeln,lv_bsart)
    WHERE ebeln = iv_po_num.
*EOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
  IF sy-subrc = 0.
    SELECT *
     FROM ekpo
     INTO TABLE lit_orders
     WHERE ebeln = iv_po_num.
    IF sy-subrc = 0 AND lit_orders IS NOT INITIAL.
*BOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
      SELECT *
        FROM ekkn
        INTO TABLE lit_ekkn
        FOR ALL ENTRIES IN lit_orders
        WHERE ebeln = lit_orders-ebeln AND
              ebelp = lit_orders-ebelp.
      IF sy-subrc <> 0.
        CLEAR lit_ekkn.
      ENDIF.
*EOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
      SELECT *
        FROM eban
        INTO TABLE lit_req
        FOR ALL ENTRIES IN lit_orders
             WHERE banfn = lit_orders-banfn AND
              bnfpo = lit_orders-bnfpo AND
              estkz <> lc_b.
      IF sy-subrc = 0.
        SORT lit_req BY ernam.
        DELETE ADJACENT DUPLICATES FROM lit_req COMPARING ernam.
        LOOP AT lit_req INTO lwa_req.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = lwa_req-banfn
            IMPORTING
              output = lv_req.

          lv_username = lwa_req-ernam.

          CALL FUNCTION 'BAPI_USER_GET_DETAIL'
            EXPORTING
              username  = lv_username
            IMPORTING
              address   = lv_email_addr
            TABLES
              parameter = lit_param
              return    = lit_return.

          IF sy-subrc = 0.
            READ TABLE lit_param INTO lwa_param WITH KEY parid = 'Z_NO_CRT_EM_PO'.
            IF sy-subrc = 0.
              IF lwa_param-parva = lc_x OR lwa_param-parva = lc_x1.
                CONTINUE.
              ENDIF.
            ENDIF.
*BOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
            IF lv_bsart = lc_nb.
              READ TABLE lit_param INTO lwa_param WITH KEY parid = 'Z_NO_CRT_PO_NB'.
              IF sy-subrc = 0.
                IF lwa_param-parva = lc_x OR lwa_param-parva = lc_x1.
                  CONTINUE.
                ENDIF.
              ENDIF.
            ELSEIF lv_bsart = lc_zf.
              READ TABLE lit_param INTO lwa_param WITH KEY parid = 'Z_NO_CRT_PO_ZF'.
              IF sy-subrc = 0.
                IF lwa_param-parva = lc_x OR lwa_param-parva = lc_x1.
                  CONTINUE.
                ENDIF.
              ENDIF.
            ENDIF.
*EOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18

            lwa_mailid-smtp_addr = lv_email_addr-e_mail.
            APPEND lwa_mailid TO lit_mailid.
          ENDIF.

*BOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
          READ TABLE lit_ekkn INTO lwa_ekkn WITH KEY ebeln = lwa_req-ebeln ebelp = lwa_req-ebelp.
          IF sy-subrc = 0 AND lwa_ekkn-aufnr IS NOT INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = lwa_ekkn-aufnr
              IMPORTING
                output = lv_order.
          ENDIF.
*EOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18

*Recipients
          LOOP AT lit_mailid INTO lwa_mailid.
            lwa_mailrecipients-rec_type  = lc_u.
            lwa_mailrecipients-receiver = lwa_mailid-smtp_addr.
            APPEND lwa_mailrecipients TO lt_mailrecipients .
            CLEAR lwa_mailrecipients .
          ENDLOOP.

* Subject.
          lt_mailsubject-obj_name = text-t01.
          lt_mailsubject-obj_langu = sy-langu.
          lt_mailsubject-obj_descr = text-t01.

* Mail Contents
*BOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
          IF lv_order IS INITIAL.
            CONCATENATE text-t02 lv_req text-t03 iv_po_num INTO lwa_mailtxt SEPARATED BY space.
          ELSE.
            CONCATENATE text-t02 lv_req text-t03 iv_po_num text-t04 lv_order INTO lwa_mailtxt SEPARATED BY space.
          ENDIF.
*EOC by kmb on 5/3/18 CHG0102802 PO creation email changes WF 05.03.18
          APPEND lwa_mailtxt TO lt_mailtxt.
          CLEAR lwa_mailtxt.
* Send Mail
          CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
            EXPORTING
              document_data              = lt_mailsubject
              put_in_outbox              = lc_x
            TABLES
              object_content             = lt_mailtxt
              receivers                  = lt_mailrecipients
            EXCEPTIONS
              too_many_receivers         = 1
              document_not_sent          = 2
              document_type_not_exist    = 3
              operation_no_authorization = 4
              parameter_error            = 5
              x_error                    = 6
              enqueue_error              = 7
              OTHERS                     = 8.
          IF sy-subrc EQ 0.
            COMMIT WORK.
*   Push mail out from SAP outbox
*        SUBMIT rsconn01 WITH mode = 'INT' AND RETURN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.
ENDCLASS.
