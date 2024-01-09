FUNCTION zmm_email_send.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IW_SUBJECT) TYPE  STRING
*"     REFERENCE(IT_EMAIL_DATA) TYPE  SOLI_TAB
*"     REFERENCE(IT_RECEIVERS) TYPE  ZMMTT_MAIL_RECIPIENT
*"  TABLES
*"      IT_RETURN TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Program Name       : ZBUS2012                                       *
*& Author             : Snehasis Dutta / sdutta                        *
*& Creation Date      : 09-MAY-2016                                    *
*& Object ID          : ACR-159                                        *
*& Application Area   : SC-MM                                          *
*& Description        : Depending on a Purchase Order creation a       *
*& notification mail will be sent to requisitioner & goods recepient   *
*&---------------------------------------------------------------------*
*  CONSTANTS: gc_htm    TYPE so_obj_tp  VALUE 'HTM',
*             gc_x(1)   TYPE c          VALUE 'X',
*             gc_e(1)   TYPE c          VALUE 'E',
*             gc_s(1)   TYPE c          VALUE 'S'.
*  DATA: g_recipient              TYPE REF TO if_recipient_bcs,
*        g_sender                 TYPE REF TO cl_sapuser_bcs,
*        g_send_request           TYPE REF TO cl_bcs,
*        g_bcs_exception          TYPE REF TO cx_bcs,
*        g_document               TYPE REF TO cl_document_bcs,
*        g_cnt                    TYPE i,
*        g_length                 TYPE so_obj_len,
*        g_us_subject             TYPE so_obj_des,
*        g_email_subject          TYPE string,
*        g_sender_id              TYPE uname,
*        g_lang                   TYPE thead-tdspras,
*        g_count                  TYPE i VALUE 24,
*        g_receiver_email_address TYPE adr6-smtp_addr,
*        g_sent_to_all            TYPE os_boolean,
*        g_sender_email           TYPE adr6-smtp_addr,
*        g_adrnr                  TYPE ad_addrnum,
*        g_pernr                  TYPE ad_persnum,
*        g_smtp_addr              TYPE adr6-smtp_addr,
*        wa_return                TYPE bapiret2.
*  FIELD-SYMBOLS: <fs_email_data> TYPE soli,
*                 <fs_receivers>  TYPE zmms_mail_recipient.
*
** Send Email notification
*  TRY.
**     Create persistent send request
*      MOVE cl_bcs=>create_persistent( ) TO g_send_request.
*
**     Create document from internal table with text
*      CLEAR: g_cnt,
*             g_length.
*      DESCRIBE TABLE it_email_data LINES g_cnt.
*      READ TABLE it_email_data ASSIGNING <fs_email_data>
*        INDEX g_cnt.
*      IF sy-subrc EQ 0.
**       Total size of Email Body
*        g_length   = ( g_cnt - 1 ) * 255 + strlen( <fs_email_data> ).
*      ENDIF.
**     Create Email Body
*      MOVE iw_subject TO g_us_subject.
*      MOVE cl_document_bcs=>create_document(
*                     i_type     = gc_htm
*                     i_text     = it_email_data[]
*                     i_length   = g_length
*                     i_subject  = g_us_subject
*                     i_language = g_lang )
*          TO g_document.
*      TRY .
*          CALL METHOD g_send_request->set_message_subject
*            EXPORTING
*              ip_subject = iw_subject.
*        CATCH cx_sy_dyn_call_illegal_method .
*      ENDTRY.
*
**     Add document to send request
*      CALL METHOD g_send_request->set_document( g_document ).
*
**     Set sender
*      MOVE sy-uname TO g_sender_id.
*      MOVE cl_sapuser_bcs=>create( g_sender_id ) TO g_sender.
*      CALL METHOD g_send_request->set_sender
*        EXPORTING
*          i_sender = g_sender.
*
*      LOOP AT it_receivers ASSIGNING <fs_receivers>.
*        IF  <fs_receivers> IS ASSIGNED.
*          MOVE <fs_receivers> TO g_receiver_email_address.
**         Create recipient object
*          MOVE cl_cam_address_bcs=>create_internet_address(
*                             g_receiver_email_address )
*               TO g_recipient.
**         Add recipient with its respective attributes to send request
*          CALL METHOD g_send_request->add_recipient
*            EXPORTING
*              i_recipient = g_recipient
*              i_express   = gc_x.
*        ENDIF.
**       Add Cc recipient (e-mail address)
*        MOVE <fs_receivers> TO g_receiver_email_address.
*        MOVE cl_cam_address_bcs=>create_internet_address(
*                           g_receiver_email_address )
*             TO g_recipient.
*
**       Add recipient with its respective attributes to send request
*        CALL METHOD g_send_request->add_recipient
*          EXPORTING
*            i_recipient = g_recipient
*            i_express   = gc_x
*            i_copy      = gc_x.
*      ENDLOOP.
*
**     Set sending immediately
*      CALL METHOD g_send_request->set_send_immediately
*        EXPORTING
*          i_send_immediately = gc_x.
*
**     Send document
*      CALL METHOD g_send_request->send(
*        EXPORTING
*          i_with_error_screen = gc_x
*        RECEIVING
*          result              = g_sent_to_all ).
*
*      IF g_sent_to_all NE gc_x.
*        MOVE gc_e TO wa_return.
*        MOVE 'ERROR' TO wa_return-message.
*        APPEND wa_return TO it_return.
*        CLEAR wa_return.
*      ELSE.
*        MOVE gc_s TO wa_return.
*        MOVE 'success' TO wa_return-message.
*        APPEND wa_return TO it_return.
*        CLEAR wa_return.
*      ENDIF.
*
*    CATCH cx_bcs INTO g_bcs_exception.
*      MOVE gc_e TO wa_return-type.
*      MOVE g_bcs_exception->get_text( )
*           TO wa_return-message.
*      CONCATENATE text-003 wa_return-message
*         INTO wa_return-message
*         SEPARATED BY space.
*      APPEND wa_return TO it_return.
*  ENDTRY.

ENDFUNCTION.
