REPORT  zlmmr008_delarfcerror.
*----------------------------------------------------------------------*
* Report Name       : ZLMMR008_DELARFCERROR                            *
* Author            : PAGRAWAL  Parul Agrawal                         *
* Date              : 17/09/2011                                       *
* Application Area  : P2P                                              *
* Description       : Report to delete error queue (EE0037)            *
*----------------------------------------------------------------------*
* BTBOUNDY - 01/28/2012 - Updated to re0activate que, and only use one *
*                         program                                      *
*----------------------------------------------------------------------*
TYPES abap.

* Types declaration
TYPES : BEGIN OF ty_address,
          member_adr  TYPE sodlienti1-member_adr,
        END OF ty_address,

        BEGIN OF ty_usr21,
          persnumber  TYPE usr21-persnumber,
          addrnumber  TYPE usr21-addrnumber,
        END OF ty_usr21,

        gta_address TYPE STANDARD TABLE OF ty_address.


*Variables
DATA: gv_queempty(1) TYPE c.


*Data Declaration for sending EMAIL**
CLASS cl_bcs DEFINITION LOAD.

* Define Constants
CONSTANTS : co_x      TYPE char1 VALUE 'X',
            co_none   TYPE char4 VALUE 'NONE',
            co_ready  TYPE char5 VALUE 'READY',
            co_run(7) TYPE c     VALUE 'RUNNING'.



*********************
*** Selection Screen
*********************
PARAMETERS: pe_qname TYPE trfcqnam      OBLIGATORY DEFAULT 'BBP_EXTREQ_TRANS',
            pe_qdest TYPE rfcdest       OBLIGATORY DEFAULT '#RMCLNT100',
            pe_dist  TYPE soodd-objnam OBLIGATORY.

************************
*** Start of Selection
************************

START-OF-SELECTION.
  gv_queempty = abap_false.

  WHILE gv_queempty = abap_false.
    gv_queempty = abap_false.

    CALL FUNCTION 'ENQUE_SLEEP'
      EXPORTING
        seconds = 5.

* Perform to delete the Queue Entry
    PERFORM delete_queue.

    CALL FUNCTION 'TRFC_QOUT_ACTIVATE'
      EXPORTING
        qname                      = pe_qname
        dest                       = pe_qdest
*   FORCE                      = ' '
* IMPORTING
*   EXELUW                     =
*   ASTATE                     =
     EXCEPTIONS
       invalid_parameter          = 1
       system_failed              = 2
       communication_failed       = 3
       OTHERS                     = 4
              .
    IF sy-subrc = 0.
      WRITE: / 'Que Activated.'.
    ELSEIF sy-subrc = 1.
      WRITE: / 'Que Did Not Activate, invalid parameters.'.
    ELSE.
      WRITE: / 'Que Activated with oustanding issues.'.
    ENDIF.

  ENDWHILE.






*************
*** Methods
*************
*---------------------------------------------------------------------*
*    Form  SEND_EMAIL_ERR_MESG_NOTE
*---------------------------------------------------------------------*
FORM send_email_err_mesg_note USING p_lwa_trfcqout TYPE trfcqout.


* Define Internal Table
  DATA : lta_address    TYPE STANDARD TABLE OF ty_address.

* Define WorkArea
  DATA :  lwa_address   TYPE ty_address,
          lwa_zmm_queue TYPE zmmt_queue,
          lwa_eban      TYPE eban.

* Definne Varaibles
  DATA: lv_sdata            TYPE string,
        lv_subject          TYPE so_obj_des,
        lv_tmpstring        TYPE string,
        l_send              TYPE adr6-smtp_addr,
        lv_preis            TYPE char11,
        lrf_document        TYPE REF TO cl_document_bcs  VALUE IS INITIAL,
        lrf_sender          TYPE REF TO if_sender_bcs    VALUE IS INITIAL,  "create sender
        lrf_recipient       TYPE REF TO if_recipient_bcs VALUE IS INITIAL, "create recipient
        lrf_send_request    TYPE REF TO cl_bcs           VALUE IS INITIAL,
        lv_sent_to_all      TYPE char1                   VALUE IS INITIAL,
        lt_message_body     TYPE bcsy_text               VALUE IS INITIAL,   "Message body and subject
        lrf_send_exc        TYPE REF TO cx_send_req_bcs,   "Variable for catching exception
        lrf_addr_exc        TYPE REF TO cx_address_bcs,
        lrf_send_email_mess TYPE REF TO cx_document_bcs.

  TRY.

      lrf_send_request = cl_bcs=>create_persistent(  ).

      lv_subject = text-001.

      APPEND text-002 TO lt_message_body.
      APPEND text-003 TO lt_message_body.
      APPEND text-004 TO lt_message_body.
      APPEND ''       TO lt_message_body.
      APPEND text-005 TO lt_message_body.

* Select PR Number and Queue Details from table zmmt_queue
      SELECT *
          FROM zmmt_queue
          UP TO 1 ROWS
          INTO lwa_zmm_queue
          WHERE arfcipid  EQ p_lwa_trfcqout-arfcipid
          AND  arfcpid    EQ p_lwa_trfcqout-arfcpid
          AND  arfctime   EQ p_lwa_trfcqout-arfctime
          AND  arfctidcnt EQ p_lwa_trfcqout-arfctidcnt.
      ENDSELECT.
      IF sy-subrc = 0.
* Select PR Details from table EBAN
        SELECT SINGLE *
           FROM eban
           INTO lwa_eban
           WHERE banfn = lwa_zmm_queue-banfn.

        IF sy-subrc = 0.

          IF NOT lwa_eban-banfn IS INITIAL.
            CLEAR lv_tmpstring.

            CONCATENATE text-006
                        lwa_eban-banfn
                   INTO lv_tmpstring
                   SEPARATED BY space.

            APPEND lv_tmpstring TO lt_message_body.
          ENDIF.

          IF NOT lwa_eban-preis IS INITIAL.
            lv_preis = lwa_eban-preis.
            CLEAR lv_tmpstring.

            CONCATENATE text-007
                        lv_preis
                    INTO lv_tmpstring
                    SEPARATED BY space.

            APPEND lv_tmpstring TO lt_message_body.
          ENDIF.

          IF NOT lwa_eban-afnam IS INITIAL.
            CLEAR lv_tmpstring.

            CONCATENATE text-008
                        lwa_eban-afnam
                   INTO lv_tmpstring
                   SEPARATED BY space.

            APPEND lv_tmpstring TO lt_message_body.
          ENDIF.

          IF NOT lwa_eban-ekorg IS INITIAL.
            CLEAR lv_tmpstring.

            CONCATENATE text-009
                        lwa_eban-ekorg
                   INTO lv_tmpstring
                   SEPARATED BY space.

            APPEND lv_tmpstring TO lt_message_body.
          ENDIF.

        ENDIF.
      ENDIF.

      IF NOT p_lwa_trfcqout-errmess IS INITIAL.
        CLEAR lv_tmpstring.

        CONCATENATE text-010
                    p_lwa_trfcqout-errmess
               INTO lv_tmpstring
               SEPARATED BY space.

        APPEND lv_tmpstring TO lt_message_body.
      ENDIF.

      IF NOT p_lwa_trfcqout-qname IS INITIAL.
        CLEAR lv_tmpstring.

        CONCATENATE text-011
                    p_lwa_trfcqout-qname
               INTO lv_tmpstring
               SEPARATED BY space.

        APPEND lv_tmpstring TO lt_message_body.
      ENDIF.

      IF NOT p_lwa_trfcqout-qstate IS INITIAL.
        CLEAR lv_tmpstring.

        CONCATENATE text-012
                    p_lwa_trfcqout-qstate
               INTO lv_tmpstring
               SEPARATED BY space.

        APPEND lv_tmpstring TO lt_message_body.
      ENDIF.

      IF NOT p_lwa_trfcqout-qrfcdatum IS INITIAL.
        CLEAR lv_tmpstring.

        CONCATENATE text-013
                    p_lwa_trfcqout-qrfcdatum
               INTO lv_tmpstring
               SEPARATED BY space.

        APPEND lv_tmpstring TO lt_message_body.
      ENDIF.

      IF NOT p_lwa_trfcqout-qrfcuzeit IS INITIAL.
        CLEAR lv_tmpstring.

        CONCATENATE text-014
                    p_lwa_trfcqout-qrfcuzeit
               INTO lv_tmpstring
               SEPARATED BY space.

        APPEND lv_tmpstring TO lt_message_body.
      ENDIF.

      IF NOT p_lwa_trfcqout-dest IS INITIAL.
        CLEAR lv_tmpstring.

        CONCATENATE text-015
                    p_lwa_trfcqout-dest
               INTO lv_tmpstring
               SEPARATED BY space.

        APPEND lv_tmpstring TO lt_message_body.
      ENDIF.

      IF NOT p_lwa_trfcqout-qrfcfnam IS INITIAL.
        CLEAR lv_tmpstring.

        CONCATENATE text-016
                    p_lwa_trfcqout-qrfcfnam
               INTO lv_tmpstring
               SEPARATED BY space.

        APPEND lv_tmpstring TO lt_message_body.
      ENDIF.

      APPEND  lv_sdata TO lt_message_body.
      APPEND ' '       TO lt_message_body.
      APPEND text-017  TO lt_message_body.

      lrf_document = cl_document_bcs=>create_document(
      i_type = 'RAW'
      i_text = lt_message_body
      i_subject = lv_subject ).

      "Pass the document to send request
      lrf_send_request->set_document( lrf_document ).

      "Pass Sender name
      lrf_sender = cl_sapuser_bcs=>create( sy-uname ).

      "Set sender
      lrf_send_request->set_sender(
      EXPORTING
      i_sender = lrf_sender ).

      PERFORM get_adrress CHANGING lta_address.

      LOOP AT lta_address INTO lwa_address.

        l_send  = lwa_address-member_adr.
        lrf_recipient = cl_cam_address_bcs=>create_internet_address( l_send ).

        "Set recipient
        lrf_send_request->add_recipient(
        EXPORTING
        i_recipient = lrf_recipient
        i_express = co_x ).


      ENDLOOP.

      "Send email
      lrf_send_request->send(
      EXPORTING
      i_with_error_screen = co_x
      RECEIVING
      result = lv_sent_to_all ).

    CATCH cx_send_req_bcs INTO lrf_send_exc.

    CATCH cx_address_bcs  INTO lrf_addr_exc.

    CATCH cx_document_bcs INTO lrf_send_email_mess.

  ENDTRY.

  COMMIT WORK.

  MESSAGE i005(zmm_message) WITH lwa_eban-banfn .


ENDFORM.                    " SEND_EMAIL_ERR_MESG_NOTE
*---------------------------------------------------------------------*
*    Form  GET_ADRRESS
*---------------------------------------------------------------------*
FORM get_adrress  CHANGING lt_address TYPE gta_address.

  DATA: BEGIN OF ls_users,
          sobid TYPE hrp1001-sobid,
          persnumber  TYPE usr21-persnumber,
          addrnumber  TYPE usr21-addrnumber,
        END OF ls_users,
        lt_users LIKE TABLE OF ls_users.

  DATA: lt_member   LIKE TABLE OF sodm1,
        lt_objpara  LIKE TABLE OF selc,
        lt_objparb  LIKE TABLE OF soop1,
        ls_member   LIKE LINE OF lt_member,

        ls_address  LIKE LINE OF lt_address.

  "Get distribution list
  CALL FUNCTION 'SO_DLI_READ'
    EXPORTING
      distributionlist           = pe_dist
      system_dli                 = 'C'
    TABLES
      member                     = lt_member
      objpara                    = lt_objpara
      objparb                    = lt_objparb
    EXCEPTIONS
      active_user_not_exist      = 1
      communication_failure      = 2
      component_not_available    = 3
      dl_name_not_exist          = 4
      folder_not_exist           = 5
      folder_no_authorization    = 6
      forwarder_not_exist        = 7
      object_not_exist           = 8
      object_no_authorization    = 9
      operation_no_authorization = 10
      owner_not_exist            = 11
      parameter_error            = 12
      substitute_not_active      = 13
      substitute_not_defined     = 14
      system_failure             = 15
      user_not_exist             = 16
      x_error                    = 17
      OTHERS                     = 18.

  IF sy-subrc <> 0.
    MESSAGE e008(zmm_message) .
  ELSE.

    "Add the users to a table.
    LOOP AT lt_member INTO ls_member WHERE memtp = 'USR'.
      CLEAR ls_users.
      ls_users-sobid = ls_member-memnam.
      APPEND ls_users TO lt_users.
    ENDLOOP.

    LOOP AT lt_users INTO ls_users.
      SELECT SINGLE persnumber addrnumber
        FROM usr21
        INTO CORRESPONDING FIELDS OF ls_users
        WHERE bname = ls_users-sobid
      .
      MODIFY lt_users FROM ls_users.
    ENDLOOP.
    IF sy-subrc = 0.
      SELECT  smtp_addr
        FROM adr6
        INTO TABLE lt_address
        FOR ALL ENTRIES IN lt_users
        WHERE addrnumber = lt_users-addrnumber
          AND   persnumber = lt_users-persnumber
          AND   date_from  =< sy-datum.
    ENDIF.

    "Add the internet addresses to the ouput table
    LOOP AT lt_member INTO ls_member WHERE memtp = 'ADR'.
      CLEAR ls_address.
      ls_address-member_adr = ls_member-address.
      APPEND ls_address TO lt_address.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " GET_ADRRESS
*---------------------------------------------------------------------*
*    Form  DELETE_QUEUE
*---------------------------------------------------------------------*
FORM delete_queue .

  "Define Internal table
  DATA: lta_arfcsstate  TYPE STANDARD TABLE OF arfcsstate.

  "Define Work area
  DATA : lwa_trfcqout   TYPE trfcqout,
         lwa_arfcsstate TYPE arfcsstate.

  "if blank destination put value 'NONE'
  IF pe_qdest IS INITIAL.
    pe_qdest = co_none.
  ENDIF.

  "Fetch tRFC Queue details based on 'tRFC Queue Name' and 'Destination'
  SELECT SINGLE * FROM trfcqout
    INTO lwa_trfcqout
    WHERE qname  EQ pe_qname
    AND   dest   EQ pe_qdest
    AND   qstate NE co_ready
    AND   qstate NE co_run.

  IF lwa_trfcqout IS NOT INITIAL.

    "Fetch ARFC Status details based on tRFC Queue details
    SELECT SINGLE * FROM arfcsstate
      INTO lwa_arfcsstate
      WHERE arfcipid   EQ lwa_trfcqout-arfcipid
       AND  arfcpid    EQ lwa_trfcqout-arfcpid
       AND  arfctime   EQ lwa_trfcqout-arfctime
       AND  arfcluwcnt EQ lwa_trfcqout-qluwcnt
       AND  arfcdest   EQ lwa_trfcqout-dest.

    IF lwa_arfcsstate IS NOT INITIAL.

      APPEND lwa_arfcsstate TO lta_arfcsstate.
      "Call the FM To delete the error qRFC
      CALL FUNCTION 'TRFC_QRCVTID_DELETE'
        EXPORTING
          no_commit = co_x
        TABLES
          state     = lta_arfcsstate.

      MESSAGE i006(zmm_message).

      PERFORM send_email_err_mesg_note USING lwa_trfcqout.

      "Delete the Z table Entry for the corresponding Queue
      DELETE FROM zmmt_queue
               WHERE arfcipid  EQ lwa_trfcqout-arfcipid
               AND  arfcpid    EQ lwa_trfcqout-arfcpid
               AND  arfctime   EQ lwa_trfcqout-arfctime
               AND  arfctidcnt EQ lwa_trfcqout-arfctidcnt.

    ENDIF.

  ELSE.
    MESSAGE i007(zmm_message).

    "Check que remaining items
    SELECT SINGLE * FROM trfcqout
      INTO lwa_trfcqout
      WHERE qname  EQ pe_qname
      AND   dest   EQ pe_qdest.

    IF sy-subrc = 0.
      "Que exists, loop again.
    ELSE.
      "Que Empty
      gv_queempty = abap_true.
      DELETE FROM zmmt_queue WHERE qname = pe_qname AND dest = pe_qdest .
    ENDIF.

  ENDIF.

ENDFORM.                    " DELETE_QUEUE
