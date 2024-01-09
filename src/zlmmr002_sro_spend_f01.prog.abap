*&---------------------------------------------------------------------*
*&  Include           ZLMMR002_SRO_SPEND_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLMMR002_SRO_SPEND_F01                         *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 08-Mar-2016                                    *
*& Object ID          : ACR-159                                        *
*& Application Area   : MM                                             *
*& Description        : Send daily email notification at 80% SRO spend *
*&                      to Requestor, Service confirmer and Buyer.     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_SRO_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_sro_pos .
  DATA: lta_ekko_ekpo_tmp TYPE STANDARD TABLE OF ty_ekko_ekpo,
        ltp_prv_date TYPE erdat.
* BOI by PANUSURI Ticket ACR-1375
* Get last 3 years SRO's
  ltp_prv_date = sy-datum.
  ltp_prv_date+0(4) = sy-datum+0(4) - 3.
* EOI by PANUSURI Ticket ACR-1375

* Get SRO Purchase Orders
  SELECT a~ebeln
         a~lifnr
         a~ekgrp
         a~zzariba_approver
         b~ebelp
         b~netwr
         b~erekz  "(+)PANUSURI Ticket ACR-1375
         b~afnam
         FROM ekko AS a
         INNER JOIN ekpo AS b
         ON  a~ebeln EQ b~ebeln
         INTO TABLE ta_ekko_ekpo
         WHERE a~bsart EQ 'ZF'
         AND   a~loekz EQ space
         AND   a~aedat GE ltp_prv_date  "(+)PANUSURI Ticket ACR-1375
         AND   a~aedat LE sy-datum      "(+)PANUSURI Ticket ACR-1375
         AND   b~loekz EQ space.
  IF sy-subrc = 0.
    SORT ta_ekko_ekpo BY ebeln ebelp.
* BOI by PANUSURI Ticket ACR-1375
    ta_ekko_ekpo_finv[] = ta_ekko_ekpo[].
    SORT ta_ekko_ekpo_finv BY ebeln erekz DESCENDING.
    DELETE ADJACENT DUPLICATES FROM ta_ekko_ekpo_finv COMPARING ebeln.
    LOOP AT ta_ekko_ekpo_finv INTO wa_ekko_ekpo WHERE erekz = 'X'.
      DELETE ta_ekko_ekpo WHERE ebeln = wa_ekko_ekpo-ebeln.
    ENDLOOP.
* EOI by PANUSURI Ticket ACR-1375
  ENDIF.
  IF ta_ekko_ekpo[] IS NOT INITIAL.
*   Get Invoices
* BOC by PANUSURI Ticket ACR-1375
*    SELECT belnr
*           gjahr
*           buzei
*           ebeln
*           ebelp
*           wrbtr
*           shkzg
*           FROM rseg
*           INTO TABLE ta_rseg
*           FOR ALL ENTRIES IN ta_ekko_ekpo
*           WHERE ebeln EQ ta_ekko_ekpo-ebeln
*           AND   ebelp EQ ta_ekko_ekpo-ebelp.
*    IF sy-subrc = 0.
*      SORT ta_rseg BY belnr gjahr buzei ebeln ebelp.
*    ENDIF.
* EOC by PANUSURI Ticket ACR-1375
* BOI by PANUSURI Ticket ACR-1375
    SELECT ebeln
           ebelp
           gjahr
           belnr
           buzei
           wrbtr
           shkzg
           FROM ekbe
           INTO TABLE ta_ekbe
           FOR ALL ENTRIES IN ta_ekko_ekpo
           WHERE ebeln EQ ta_ekko_ekpo-ebeln
           AND   ebelp EQ ta_ekko_ekpo-ebelp
           AND   vgabe IN ('2', 'P').
    IF sy-subrc = 0.
      SORT ta_ekbe BY ebeln ebelp.
    ENDIF.
* EOI by PANUSURI Ticket ACR-1375
*   Get Vendor name
    lta_ekko_ekpo_tmp[] = ta_ekko_ekpo[].
    SORT lta_ekko_ekpo_tmp BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lta_ekko_ekpo_tmp COMPARING lifnr.
    SELECT lifnr
           name1
           FROM lfa1
           INTO TABLE ta_lfa1
           FOR ALL ENTRIES IN lta_ekko_ekpo_tmp
           WHERE lifnr = lta_ekko_ekpo_tmp-lifnr.
    IF sy-subrc = 0.
      SORT ta_lfa1 BY lifnr name1.
    ENDIF.
  ENDIF.
* Get Recipients
  LOOP AT ta_ekko_ekpo INTO wa_ekko_ekpo.
    wa_pgrp-ebeln = wa_ekko_ekpo-ebeln.
    wa_pgrp-ekgrp = wa_ekko_ekpo-ekgrp.
    APPEND wa_pgrp TO ta_pgrp.
    CLEAR wa_pgrp.
    IF wa_ekko_ekpo-zzariba_approver IS NOT INITIAL.
      wa_recipients-ebeln = wa_ekko_ekpo-ebeln.
      wa_recipients-usrid = wa_ekko_ekpo-zzariba_approver.
      APPEND wa_recipients TO ta_recipients.
      CLEAR wa_recipients.
    ENDIF.
    IF wa_ekko_ekpo-afnam IS NOT INITIAL.
      wa_recipients-ebeln = wa_ekko_ekpo-ebeln.
      wa_recipients-usrid = wa_ekko_ekpo-afnam.
      APPEND wa_recipients TO ta_recipients.
      CLEAR wa_recipients.
    ENDIF.
  ENDLOOP.
  SORT ta_pgrp BY ebeln ekgrp.
  DELETE ADJACENT DUPLICATES FROM ta_pgrp COMPARING ebeln ekgrp.

  DELETE ta_recipients WHERE usrid EQ space.
  SORT ta_recipients BY ebeln usrid.
  DELETE ADJACENT DUPLICATES FROM ta_recipients COMPARING ebeln usrid.

  IF ta_pgrp[] IS NOT INITIAL.
*   Get Parameter ID of Purchasing group
    SELECT ekgrp
           FROM zmmt_pgrp
           INTO TABLE ta_pgrp_parid
           FOR ALL ENTRIES IN ta_pgrp
           WHERE ekgrp = ta_pgrp-ekgrp
           AND   zmm_no_email_sro = 'X'.
    IF sy-subrc = 0.
      SORT ta_pgrp_parid BY ekgrp.
      LOOP AT ta_pgrp_parid INTO wa_pgrp_parid.
        DELETE ta_pgrp WHERE ekgrp = wa_pgrp_parid-ekgrp.
        CLEAR wa_pgrp_parid.
      ENDLOOP.
    ENDIF.
*   Get Email address of Purchasing group
    SELECT ekgrp
           smtp_addr
           FROM t024
           INTO TABLE ta_pgrp_email
           FOR ALL ENTRIES IN ta_pgrp
           WHERE ekgrp = ta_pgrp-ekgrp.
    IF sy-subrc = 0.
      SORT ta_pgrp_email BY ekgrp smtp_addr.
    ENDIF.
  ENDIF.
  IF ta_recipients[] IS NOT INITIAL.
*   Get Parameter ID of Service Confirmer and Requestor
    SELECT bname
           FROM usr05
           INTO TABLE ta_rec_parid
           FOR ALL ENTRIES IN ta_recipients
           WHERE bname = ta_recipients-usrid
           AND   parid = co_parid.
    IF sy-subrc = 0.
      SORT ta_rec_parid BY bname.
      LOOP AT ta_rec_parid INTO wa_rec_parid.
        DELETE ta_recipients WHERE usrid = wa_rec_parid-bname.
        CLEAR wa_rec_parid.
      ENDLOOP.
    ENDIF.
*   Get Email address of Service Confirmer and Requestor
    SELECT bname
           persnumber
           addrnumber
           FROM usr21
           INTO TABLE ta_usr21
           FOR ALL ENTRIES IN ta_recipients
           WHERE bname = ta_recipients-usrid.
    IF sy-subrc = 0.
      SORT ta_usr21 BY bname persnumber addrnumber.
    ENDIF.
    IF ta_usr21[] IS NOT INITIAL.
      SELECT addrnumber
             persnumber
             smtp_addr
             FROM adr6
             INTO TABLE ta_rec_email
             FOR ALL ENTRIES IN ta_usr21
             WHERE addrnumber = ta_usr21-addrnumber
             AND   persnumber = ta_usr21-persnumber.
      IF sy-subrc = 0.
        SORT ta_rec_email BY addrnumber persnumber smtp_addr.
        DELETE ADJACENT DUPLICATES FROM ta_rec_email COMPARING ALL FIELDS.
      ENDIF.
    ENDIF.
  ENDIF.
* Build final Recipients list with Email address
  LOOP AT ta_pgrp INTO wa_pgrp.
    READ TABLE ta_pgrp_email INTO wa_pgrp_email WITH KEY ekgrp = wa_pgrp-ekgrp.
    IF sy-subrc = 0.
      wa_rec_final-ebeln = wa_pgrp-ebeln.
      wa_rec_final-usrid = wa_pgrp-ekgrp.
      wa_rec_final-smtp_addr = wa_pgrp_email-smtp_addr.
      APPEND wa_rec_final TO ta_rec_final.
      CLEAR wa_rec_final.
    ENDIF.
    CLEAR wa_pgrp.
  ENDLOOP.
  LOOP AT ta_recipients INTO wa_recipients.
    READ TABLE ta_usr21 INTO wa_usr21 WITH KEY bname = wa_recipients-usrid.
    IF sy-subrc = 0.
      READ TABLE ta_rec_email INTO wa_rec_email WITH KEY addrnumber = wa_usr21-addrnumber
                                                         persnumber = wa_usr21-persnumber.
      IF sy-subrc = 0.
        wa_rec_final-ebeln = wa_recipients-ebeln.
        wa_rec_final-usrid = wa_recipients-usrid.
        wa_rec_final-smtp_addr = wa_rec_email-smtp_addr.
        APPEND wa_rec_final TO ta_rec_final.
        CLEAR wa_rec_final.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_SRO_POS
*&---------------------------------------------------------------------*
*&      Form  GET_SRO_SPEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_sro_spend .
  DATA: ltp_po_amt      TYPE bwert,
        ltp_inv_amt     TYPE wrbtr,
        ltp_sro_spend   TYPE i,
        lwa_ekko_ekpo   TYPE ty_ekko_ekpo,
*        lwa_rseg        TYPE ty_rseg,  "(-)PANUSURI Ticket ACR-1375
        lwa_ekbe        TYPE ty_ekbe,   "(+)PANUSURI Ticket ACR-1375
        lwa_mul_inv_amt TYPE ty_inv_amt,
        lwa_rec_final   TYPE ty_rec_final.
* Get PO total value
  LOOP AT ta_ekko_ekpo INTO wa_ekko_ekpo.
    lwa_ekko_ekpo = wa_ekko_ekpo.
    ltp_po_amt = lwa_ekko_ekpo-netwr + ltp_po_amt.
    AT END OF ebeln.
      wa_po_amt-po_amt = ltp_po_amt.
      wa_po_amt-ebeln = lwa_ekko_ekpo-ebeln.
      wa_po_amt-lifnr = lwa_ekko_ekpo-lifnr.
      APPEND wa_po_amt TO ta_po_amt.
      CLEAR: wa_po_amt,
             ltp_po_amt.
    ENDAT.
    CLEAR: lwa_ekko_ekpo,
           wa_ekko_ekpo.
  ENDLOOP.
* Get Multiple invoice value
*BOC by PANUSURI Ticket ACR-1375
*  LOOP AT ta_rseg INTO wa_rseg.
*    lwa_rseg = wa_rseg.
*    IF lwa_rseg-shkzg = 'H'.
*      lwa_rseg-wrbtr = lwa_rseg-wrbtr * -1.
*    ENDIF.
*    ltp_inv_amt = lwa_rseg-wrbtr + ltp_inv_amt.
*    AT END OF belnr.
*      wa_mul_inv_amt-inv_amt = ltp_inv_amt.
*      wa_mul_inv_amt-ebeln = lwa_rseg-ebeln.
*      APPEND wa_mul_inv_amt TO ta_mul_inv_amt.
*      CLEAR: wa_mul_inv_amt,
*             ltp_inv_amt.
*    ENDAT.
*    CLEAR: lwa_rseg,
*           wa_rseg.
*  ENDLOOP.
*  CLEAR ltp_inv_amt.
*EOC by PANUSURI Ticket ACR-1375
*BOI by PANUSURI Ticket ACR-1375
* Get Multiple invoice value
  LOOP AT ta_ekbe INTO wa_ekbe.
    lwa_ekbe = wa_ekbe.
    IF lwa_ekbe-shkzg = 'H'.
      lwa_ekbe-wrbtr = lwa_ekbe-wrbtr * -1.
    ENDIF.
    ltp_inv_amt = lwa_ekbe-wrbtr + ltp_inv_amt.
    AT END OF belnr.
      wa_mul_inv_amt-inv_amt = ltp_inv_amt.
      wa_mul_inv_amt-ebeln = lwa_ekbe-ebeln.
      APPEND wa_mul_inv_amt TO ta_mul_inv_amt.
      CLEAR: wa_mul_inv_amt,
             ltp_inv_amt.
    ENDAT.
    CLEAR: lwa_ekbe,
           wa_ekbe.
  ENDLOOP.
  CLEAR ltp_inv_amt.
*EOI by PANUSURI Ticket ACR-1375
* Get Invoice total value
  LOOP AT ta_mul_inv_amt INTO wa_mul_inv_amt.
    lwa_mul_inv_amt = wa_mul_inv_amt.
    ltp_inv_amt = lwa_mul_inv_amt-inv_amt + ltp_inv_amt.
    AT END OF ebeln.
      wa_inv_amt-inv_amt = ltp_inv_amt.
      wa_inv_amt-ebeln = lwa_mul_inv_amt-ebeln.
      APPEND wa_inv_amt TO ta_inv_amt.
      CLEAR: wa_inv_amt,
             ltp_inv_amt.
    ENDAT.
    CLEAR: lwa_mul_inv_amt,
           wa_mul_inv_amt.
  ENDLOOP.
  LOOP AT ta_rec_final INTO wa_rec_final.
    CLEAR: ltp_sro_spend.        "(+)PANUSURI Ticket ACR-1375
    lwa_rec_final = wa_rec_final.
    AT NEW ebeln.
      CLEAR wa_po_amt.
      READ TABLE ta_po_amt INTO wa_po_amt WITH KEY ebeln = lwa_rec_final-ebeln.
      IF sy-subrc = 0.
        CLEAR wa_inv_amt.
        READ TABLE ta_inv_amt INTO wa_inv_amt WITH KEY ebeln = wa_po_amt-ebeln.
*        IF sy-subrc = 0. "(-)PANUSURI Ticket ACR-4243
        IF sy-subrc = 0 AND wa_po_amt-po_amt IS NOT INITIAL.  "(+)PANUSURI Ticket ACR-4243
          ltp_sro_spend = ( wa_inv_amt-inv_amt * 100 ) / wa_po_amt-po_amt.
        ELSE.
          CLEAR ltp_sro_spend.  "(+)PANUSURI Ticket ACR-1375
        ENDIF.
      ENDIF.
    ENDAT.
    IF ltp_sro_spend >= 80.
      wa_sro_final-ebeln = wa_po_amt-ebeln.
      wa_sro_final-lifnr = wa_po_amt-lifnr.
      wa_sro_final-smtp_addr = lwa_rec_final-smtp_addr.
      READ TABLE ta_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_po_amt-lifnr.
      IF sy-subrc = 0.
        wa_sro_final-name1 = wa_lfa1-name1.
      ELSE.
        CLEAR wa_sro_final-name1.
      ENDIF.
      APPEND wa_sro_final TO ta_sro_final.
      CLEAR: wa_sro_final,
             wa_lfa1,
             ltp_sro_spend.
    ENDIF.
    CLEAR: lwa_rec_final,
           wa_rec_final,
           ltp_sro_spend.
  ENDLOOP.

  SORT ta_sro_final BY ebeln lifnr smtp_addr.
  DELETE ta_sro_final WHERE smtp_addr EQ space.

ENDFORM.                    " GET_SRO_SPEND
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_email .
  DATA: ltp_subject      TYPE so_obj_des,
        lta_text         TYPE soli_tab,
        lwa_text         TYPE soli,
        lcl_sendrequest  TYPE REF TO cl_bcs,
        lcl_document     TYPE REF TO cl_document_bcs,
        lcl_sender       TYPE REF TO cl_sapuser_bcs,
        lif_recipient    TYPE REF TO if_recipient_bcs,
        lcl_bcsexception TYPE REF TO cx_bcs,                "#EC NEEDED
        ltp_senttoall    TYPE os_boolean.                   "#EC NEEDED

  DATA: lta_final        TYPE STANDARD TABLE OF ty_sro_final,
        ltp_index        TYPE sy-tabix.

  FIELD-SYMBOLS: <lfs_final> LIKE LINE OF lta_final.

* Build Subject
  ltp_subject = 'SRO approaching overall limit'(006).

  SORT ta_sro_final BY ebeln smtp_addr .
  DELETE ADJACENT DUPLICATES FROM ta_sro_final COMPARING ebeln smtp_addr.

  lta_final[] = ta_sro_final[].
  DELETE ADJACENT DUPLICATES FROM ta_sro_final COMPARING ebeln.

  LOOP AT ta_sro_final INTO wa_sro_final.
    READ TABLE lta_final ASSIGNING <lfs_final> WITH KEY ebeln = wa_sro_final-ebeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      ltp_index = sy-tabix.

*     Build Object content
      lwa_text = text-003.
      APPEND lwa_text TO lta_text.
      APPEND INITIAL LINE TO lta_text.
      CONCATENATE 'SRO'(007) wa_sro_final-ebeln 'with'(008) wa_sro_final-name1 '/'(009) wa_sro_final-lifnr
                    text-001 text-002 INTO lwa_text SEPARATED BY space.
      APPEND lwa_text TO lta_text.
      CLEAR lwa_text.
      TRY.
*         Create persistent send request
          lcl_sendrequest = cl_bcs=>create_persistent( ).

          lcl_document = cl_document_bcs=>create_document(
                           i_type         = 'RAW'
                           i_text         = lta_text[]
                           i_subject      = ltp_subject ).
          REFRESH lta_text.

*         Add document object to send request
          CALL METHOD lcl_sendrequest->set_document( lcl_document ).

*         Set sender
          lcl_sender = cl_sapuser_bcs=>create( sy-uname ).

          CALL METHOD lcl_sendrequest->set_sender
            EXPORTING
              i_sender = lcl_sender.

          LOOP AT lta_final ASSIGNING <lfs_final> FROM ltp_index.
            IF wa_sro_final-ebeln EQ <lfs_final>-ebeln.
*             Create recipient object
              lif_recipient = cl_cam_address_bcs=>create_internet_address( <lfs_final>-smtp_addr ).
*             Add recipient object to send request
              CALL METHOD lcl_sendrequest->add_recipient
                EXPORTING
                  i_recipient = lif_recipient
                  i_express   = 'X'.
            ELSE.
              CLEAR: ltp_index.
              EXIT.
            ENDIF.
          ENDLOOP.

*         Send document
          CALL METHOD lcl_sendrequest->send(
            EXPORTING
              i_with_error_screen = 'X'
            RECEIVING
              result              = ltp_senttoall ).

          COMMIT WORK AND WAIT.
          MESSAGE s023(zmm_message) WITH 'Emails sent out for SRO'(005) wa_sro_final-ebeln.
          WRITE:/ 'Emails sent out for SRO'(005), wa_sro_final-ebeln.
        CATCH cx_bcs INTO lcl_bcsexception.             "#EC NO_HANDLER
      ENDTRY.
    ENDIF.
    CLEAR: wa_sro_final.
  ENDLOOP.

ENDFORM.                    " SEND_EMAIL
