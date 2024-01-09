FUNCTION ZSENDMAIL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_EMAILID) TYPE  ZMMTT_MAIL_RECIPIENT
*"----------------------------------------------------------------------
* Data Declarations
      DATA: lt_mailsubject     TYPE sodocchgi1.
      DATA: lt_mailrecipients  TYPE STANDARD TABLE OF somlrec90.
      DATA: lwa_mailrecipients  TYPE somlrec90.
      DATA: lt_mailtxt         TYPE STANDARD TABLE OF soli.
      DATA: lwa_mailtxt         TYPE soli.
* Recipients
      lwa_mailrecipients-rec_type  = 'U'.
      lwa_mailrecipients-receiver = 'KAVYA.MB@ENBRIDGE.COM'.
      APPEND lwa_mailrecipients TO lt_mailrecipients .
      CLEAR lwa_mailrecipients .

      lwa_mailrecipients-rec_type  = 'U'.
      lwa_mailrecipients-receiver = 'kavya.mb@enbridge.com'.
      APPEND lwa_mailrecipients TO lt_mailrecipients .
      CLEAR lwa_mailrecipients .
* Subject.
      lt_mailsubject-obj_name = 'PO/SRO created from Requisition'.
      lt_mailsubject-obj_langu = sy-langu.
      lt_mailsubject-obj_descr = 'PO/SRO created from Requisition'.
* Mail Contents
      CONCATENATE 'Requisition' 'xxxxxx' 'created the following Purchase order' INTO lwa_mailtxt SEPARATED BY space.
      APPEND lwa_mailtxt TO lt_mailtxt.
      CLEAR lwa_mailtxt.
* Send Mail
      CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
        EXPORTING
          document_data              = lt_mailsubject
          put_in_outbox = 'X'
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
ENDFUNCTION.
