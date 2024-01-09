class ZCL_IM_MB_DOCUMENT_BADI definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_MB_DOCUMENT_BADI
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
*"* protected components of class ZCL_IM_MB_DOCUMENT_BADI
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_MB_DOCUMENT_BADI
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_MB_DOCUMENT_BADI IMPLEMENTATION.


METHOD if_ex_mb_document_badi~mb_document_before_update.
*&---------------------------------------------------------------------*
*& Program Name       : ZMB_DOCUMENT_BADI                              *
*& Author             : PANUSURI                                       *
*& Creation Date      : 06-May-2016                                    *
*& Object ID          : ACR-159                                        *
*& Application Area   : MM                                             *
*& Description        : When GR is done, E-Mail notification is sent to*
*&                      Buyer, Requestor and Goods Recipient.          *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*Modification Log(Latest Version on Top)                               *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 06-May-2016                                          *
* Modified By   : PANUSURI                                             *
* Correction No : D30K926833                                           *
* Object ID     : ACR-159                                              *
* Description   : Initial Version                                      *
*&---------------------------------------------------------------------*
*  TYPES: BEGIN OF lty_mseg_gr,
*           mblnr TYPE mblnr,
*           matnr TYPE matnr,
*           erfmg TYPE erfmg,
*           erfme TYPE erfme,
*           ebeln TYPE bstnr,
*           ebelp TYPE ebelp,
*         END OF lty_mseg_gr,
*         BEGIN OF lty_ekko_ekpo,
*           ebeln TYPE ebeln,
*           bsart TYPE esart,
*           ekgrp TYPE bkgrp,
*           ebelp TYPE ebelp,
*           txz01 TYPE txz01,
*           afnam TYPE afnam,
*         END OF lty_ekko_ekpo,
*         BEGIN OF lty_ekkn,
*           ebeln TYPE ebeln,
*           ebelp TYPE ebelp,
*           wempf TYPE wempf,
*         END OF lty_ekkn,
*         BEGIN OF lty_ekgrp,
*           ebeln TYPE ebeln,
*           ebelp TYPE ebelp,
*           ekgrp TYPE bkgrp,
*         END OF lty_ekgrp,
*         BEGIN OF lty_ekgrp_parid,
*           ekgrp TYPE bkgrp,
*         END OF lty_ekgrp_parid,
*         BEGIN OF lty_ekgrp_email,
*           ekgrp     TYPE bkgrp,
*           smtp_addr TYPE ad_smtpadr,
*         END OF lty_ekgrp_email,
*         BEGIN OF lty_recipients,
*           ebeln TYPE ebeln,
*           ebelp TYPE ebelp,
*           usrid TYPE char12,
*         END OF lty_recipients,
*         BEGIN OF lty_rec_email,
*           usrid     TYPE char12,
*           smtp_addr TYPE ad_smtpadr,
*         END OF lty_rec_email,
*         BEGIN OF lty_rec_final,
*           ebeln     TYPE ebeln,
*           ebelp     TYPE ebelp,
*           usrid     TYPE char12,
*           smtp_addr TYPE ad_smtpadr,
*         END OF lty_rec_final,
*         BEGIN OF lty_final_gr,
*           ebeln     TYPE bstnr,
*           usrid     TYPE char12,
*           ebelp     TYPE ebelp,
*           mblnr     TYPE mblnr,
*           matnr     TYPE matnr,
*           erfmg     TYPE erfmg,
*           erfme     TYPE erfme,
*           txz01     TYPE txz01,
*           smtp_addr TYPE ad_smtpadr,
*         END OF lty_final_gr.
*  DATA: lta_mkpf         TYPE ty_t_mkpf,
*        lwa_mkpf         TYPE mkpf,
*        lta_mseg         TYPE ty_t_mseg,
*        lwa_mseg         TYPE mseg,
*        lta_mseg_gr      TYPE STANDARD TABLE OF lty_mseg_gr,
*        lwa_mseg_gr      TYPE lty_mseg_gr,
*        lta_ekko_ekpo    TYPE STANDARD TABLE OF lty_ekko_ekpo,
*        lwa_ekko_ekpo    TYPE lty_ekko_ekpo,
*        lta_ekkn         TYPE STANDARD TABLE OF lty_ekkn,
*        lwa_ekkn         TYPE lty_ekkn,
*        lta_ekgrp        TYPE STANDARD TABLE OF lty_ekgrp,
*        lwa_ekgrp        TYPE lty_ekgrp,
*        lta_ekgrp_tmp    TYPE STANDARD TABLE OF lty_ekgrp,
*        lwa_ekgrp_tmp    TYPE lty_ekgrp,
*        lta_ekgrp_parid  TYPE STANDARD TABLE OF lty_ekgrp_parid,
*        lwa_ekgrp_parid  TYPE lty_ekgrp_parid,
*        lta_ekgrp_email  TYPE STANDARD TABLE OF lty_ekgrp_email,
*        lwa_ekgrp_email  TYPE lty_ekgrp_email,
*        lta_recipients   TYPE STANDARD TABLE OF lty_recipients,
*        lwa_recipients   TYPE lty_recipients,
*        lta_rec_tmp      TYPE STANDARD TABLE OF lty_recipients,
*        lwa_rec_tmp      TYPE lty_recipients,
*        lta_rec_parid    TYPE STANDARD TABLE OF bapiparam,
*        lwa_rec_parid    TYPE bapiparam,
*        lta_return       TYPE STANDARD TABLE OF bapiret2,
*        lta_rec_addsmtp  TYPE STANDARD TABLE OF bapiadsmtp,
*        lwa_rec_addsmtp  TYPE bapiadsmtp,
*        lta_rec_email    TYPE STANDARD TABLE OF lty_rec_email,
*        lwa_rec_email    TYPE lty_rec_email,
*        lta_rec_final    TYPE STANDARD TABLE OF lty_rec_final,
*        lwa_rec_final    TYPE lty_rec_final,
*        lta_final_gr     TYPE STANDARD TABLE OF lty_final_gr,
*        lwa_final_gr     TYPE lty_final_gr,
*        lta_final_po     TYPE STANDARD TABLE OF lty_final_gr,
*        lwa_final_po     TYPE lty_final_gr,
*        lwa_final_tmp    TYPE lty_final_gr,
*        ltp_tabix        TYPE sy-tabix,
*        ltp_menge(17)    TYPE c,
*        ltp_subject      TYPE so_obj_des,
*        lta_text         TYPE soli_tab,
*        lwa_text         TYPE soli,
*        lcl_sendrequest  TYPE REF TO cl_bcs,
*        lcl_document     TYPE REF TO cl_document_bcs,
*        lcl_sender       TYPE REF TO cl_sapuser_bcs,
*        lif_recipient    TYPE REF TO if_recipient_bcs,
*        lcl_bcsexception TYPE REF TO cx_bcs,
*        ltp_senttoall    TYPE os_boolean.
*  CONSTANTS: lco_parid   TYPE memoryid VALUE 'Z_NO_GR_EM'.
*
*  lta_mkpf[] = xmkpf[].
*  lta_mseg[] = xmseg[].
*  SORT lta_mkpf BY mblnr mjahr.
*  SORT lta_mseg BY mblnr mjahr.
*
** Get Goods Receipt for Purchase Order
*  LOOP AT lta_mkpf INTO lwa_mkpf WHERE vgart = 'WE'.
*    READ TABLE lta_mseg INTO lwa_mseg WITH KEY mblnr = lwa_mkpf-mblnr
*                                               mjahr = lwa_mkpf-mjahr.
*    IF sy-subrc = 0.
*      ltp_tabix = sy-tabix.
*      LOOP AT lta_mseg INTO lwa_mseg FROM ltp_tabix.
*        IF lwa_mseg-mblnr NE lwa_mkpf-mblnr OR lwa_mseg-mjahr NE lwa_mkpf-mjahr.
*          EXIT.
*        ENDIF.
*        lwa_mseg_gr-mblnr = lwa_mseg-mblnr.
*        lwa_mseg_gr-matnr = lwa_mseg-matnr.
*        lwa_mseg_gr-erfmg = lwa_mseg-erfmg.
*        lwa_mseg_gr-erfme = lwa_mseg-erfme.
*        lwa_mseg_gr-ebeln = lwa_mseg-ebeln.
*        lwa_mseg_gr-ebelp = lwa_mseg-ebelp.
*        APPEND lwa_mseg_gr TO lta_mseg_gr.
*        CLEAR: lwa_mseg_gr,
*               lwa_mseg.
*      ENDLOOP.
*      CLEAR: ltp_tabix,
*             lwa_mseg.
*    ENDIF.
*    CLEAR: lwa_mkpf.
*  ENDLOOP.
*  SORT lta_mseg_gr BY ebeln ebelp.
*
*  IF lta_mseg_gr[] IS NOT INITIAL.
**   Get Purchase Order data
*    SELECT a~ebeln
*           a~bsart
*           a~ekgrp
*           b~ebelp
*           b~txz01
*           b~afnam
*           FROM ekko AS a
*           INNER JOIN ekpo AS b
*           ON a~ebeln EQ b~ebeln
*           INTO TABLE lta_ekko_ekpo
*           FOR ALL ENTRIES IN lta_mseg_gr
*           WHERE a~ebeln EQ lta_mseg_gr-ebeln
*           AND   a~bsart EQ 'NB'
*           AND   b~ebelp EQ lta_mseg_gr-ebelp.
*    IF sy-subrc = 0.
*      SORT lta_ekko_ekpo BY ebeln ebelp.
*    ENDIF.
*    IF lta_ekko_ekpo[] IS NOT INITIAL.
**     Get PO Account assignment data
*      SELECT ebeln
*             ebelp
*             wempf
*             FROM ekkn
*             INTO TABLE lta_ekkn
*             FOR ALL ENTRIES IN lta_ekko_ekpo
*             WHERE ebeln EQ lta_ekko_ekpo-ebeln
*             AND   ebelp EQ lta_ekko_ekpo-ebelp.
*      IF sy-subrc = 0.
*        SORT lta_ekkn BY ebeln ebelp wempf.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  LOOP AT lta_ekko_ekpo INTO lwa_ekko_ekpo.
**   Get Buyer
*    lwa_ekgrp-ebeln = lwa_ekko_ekpo-ebeln.
*    lwa_ekgrp-ebelp = lwa_ekko_ekpo-ebelp.
*    lwa_ekgrp-ekgrp = lwa_ekko_ekpo-ekgrp.
*    APPEND lwa_ekgrp TO lta_ekgrp.
*    CLEAR lwa_ekgrp.
**   Get Requestor
*    IF lwa_ekko_ekpo-afnam IS NOT INITIAL.
*      lwa_recipients-ebeln = lwa_ekko_ekpo-ebeln.
*      lwa_recipients-ebelp = lwa_ekko_ekpo-ebelp.
*      lwa_recipients-usrid = lwa_ekko_ekpo-afnam.
*      APPEND lwa_recipients TO lta_recipients.
*      CLEAR lwa_recipients.
*    ENDIF.
*    READ TABLE lta_ekkn INTO lwa_ekkn WITH KEY ebeln = lwa_ekko_ekpo-ebeln
*                                               ebelp = lwa_ekko_ekpo-ebelp.
*    IF sy-subrc = 0.
**     Get Goods Recipient
*      IF lwa_ekkn-wempf IS NOT INITIAL.
*        lwa_recipients-ebeln = lwa_ekkn-ebeln.
*        lwa_recipients-ebelp = lwa_ekkn-ebelp.
*        lwa_recipients-usrid = lwa_ekkn-wempf.
*        APPEND lwa_recipients TO lta_recipients.
*        CLEAR lwa_recipients.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  lta_ekgrp_tmp[] = lta_ekgrp[].
*  SORT lta_ekgrp_tmp BY ekgrp.
*  DELETE ADJACENT DUPLICATES FROM lta_ekgrp_tmp COMPARING ekgrp.
*  IF lta_ekgrp_tmp[] IS NOT INITIAL.
**   Get Parameter ID of Buyer
*    SELECT ekgrp
*           FROM zmmt_pgrp
*           INTO TABLE lta_ekgrp_parid
*           FOR ALL ENTRIES IN lta_ekgrp_tmp
*           WHERE ekgrp = lta_ekgrp_tmp-ekgrp
*           AND   zmm_no_email_gr = 'X'.
*    IF sy-subrc = 0.
*      SORT lta_ekgrp_parid BY ekgrp.
*      LOOP AT lta_ekgrp_parid INTO lwa_ekgrp_parid.
*        DELETE lta_ekgrp_tmp WHERE ekgrp = lwa_ekgrp_parid-ekgrp.
*        CLEAR lwa_ekgrp_parid.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
*  IF lta_ekgrp_tmp[] IS NOT INITIAL.
**   Get Email address of Buyer
*    SELECT ekgrp
*           smtp_addr
*           FROM t024
*           INTO TABLE lta_ekgrp_email
*           FOR ALL ENTRIES IN lta_ekgrp_tmp
*           WHERE ekgrp = lta_ekgrp_tmp-ekgrp.
*    IF sy-subrc = 0.
*      SORT lta_ekgrp_email BY ekgrp smtp_addr.
*    ENDIF.
*  ENDIF.
*  LOOP AT lta_ekgrp INTO lwa_ekgrp.
*    READ TABLE lta_ekgrp_tmp INTO lwa_ekgrp_tmp WITH KEY ekgrp = lwa_ekgrp-ekgrp.
*    IF sy-subrc = 0.
*      READ TABLE lta_ekgrp_email INTO lwa_ekgrp_email WITH KEY ekgrp = lwa_ekgrp_tmp-ekgrp.
*      IF sy-subrc = 0.
*        lwa_rec_final-ebeln = lwa_ekgrp-ebeln.
*        lwa_rec_final-ebelp = lwa_ekgrp-ebelp.
*        lwa_rec_final-usrid = lwa_ekgrp-ekgrp.
*        TRANSLATE lwa_rec_final-usrid TO UPPER CASE.
*        lwa_rec_final-smtp_addr = lwa_ekgrp_email-smtp_addr.
*        APPEND lwa_rec_final TO lta_rec_final.
*        CLEAR: lwa_rec_final,
*               lwa_ekgrp_email.
*      ENDIF.
*      CLEAR lwa_ekgrp_tmp.
*    ENDIF.
*    CLEAR lwa_ekgrp.
*  ENDLOOP.
*
*  lta_rec_tmp[] = lta_recipients[].
*  SORT lta_rec_tmp BY usrid.
*  DELETE ADJACENT DUPLICATES FROM lta_rec_tmp COMPARING usrid.
*  LOOP AT lta_rec_tmp INTO lwa_rec_tmp.
**   Get Parameter ID and Email address of Requestor and Goods Recipient
*    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*      EXPORTING
*        username  = lwa_rec_tmp-usrid
*      TABLES
*        parameter = lta_rec_parid
*        return    = lta_return
*        addsmtp   = lta_rec_addsmtp.
*    READ TABLE lta_rec_parid INTO lwa_rec_parid WITH KEY parid = lco_parid.
*    IF sy-subrc NE 0.
*      READ TABLE lta_rec_addsmtp INTO lwa_rec_addsmtp INDEX 1.
*      IF sy-subrc = 0.
*        lwa_rec_email-usrid = lwa_rec_tmp-usrid.
*        lwa_rec_email-smtp_addr = lwa_rec_addsmtp-e_mail.
*        APPEND lwa_rec_email TO lta_rec_email.
*        CLEAR: lwa_rec_addsmtp,
*               lwa_rec_email.
*      ENDIF.
*      CLEAR lwa_rec_parid.
*    ENDIF.
*    CLEAR lwa_rec_tmp.
*  ENDLOOP.
*  LOOP AT lta_recipients INTO lwa_recipients.
*    READ TABLE lta_rec_email INTO lwa_rec_email WITH KEY usrid = lwa_recipients-usrid.
*    IF sy-subrc = 0.
*      lwa_rec_final-ebeln = lwa_recipients-ebeln.
*      lwa_rec_final-ebelp = lwa_recipients-ebelp.
*      lwa_rec_final-usrid = lwa_recipients-usrid.
*      TRANSLATE lwa_rec_final-usrid TO UPPER CASE.
*      lwa_rec_final-smtp_addr = lwa_rec_email-smtp_addr.
*      APPEND lwa_rec_final TO lta_rec_final.
*      CLEAR: lwa_rec_final,
*             lwa_rec_email.
*    ENDIF.
*    CLEAR lwa_recipients.
*  ENDLOOP.
*  SORT lta_rec_final BY ebeln ebelp usrid.
*  DELETE ADJACENT DUPLICATES FROM lta_rec_final COMPARING ebeln ebelp usrid.
*
*  LOOP AT lta_mseg_gr INTO lwa_mseg_gr.
*    READ TABLE lta_rec_final INTO lwa_rec_final WITH KEY ebeln = lwa_mseg_gr-ebeln
*                                                         ebelp = lwa_mseg_gr-ebelp.
*    IF sy-subrc = 0.
*      ltp_tabix = sy-tabix.
*      LOOP AT lta_rec_final INTO lwa_rec_final FROM ltp_tabix.
*        IF lwa_rec_final-ebeln NE lwa_mseg_gr-ebeln OR lwa_rec_final-ebelp NE lwa_mseg_gr-ebelp.
*          EXIT.
*        ENDIF.
*        READ TABLE lta_ekko_ekpo INTO lwa_ekko_ekpo WITH KEY ebeln = lwa_rec_final-ebeln
*                                                             ebelp = lwa_rec_final-ebelp.
*        IF sy-subrc = 0.
*          lwa_final_gr-txz01 = lwa_ekko_ekpo-txz01.
*        ENDIF.
*        lwa_final_gr-ebeln = lwa_mseg_gr-ebeln.
*        lwa_final_gr-ebelp = lwa_mseg_gr-ebelp.
*        lwa_final_gr-mblnr = lwa_mseg_gr-mblnr.
*        lwa_final_gr-matnr = lwa_mseg_gr-matnr.
*        lwa_final_gr-erfmg = lwa_mseg_gr-erfmg.
*        lwa_final_gr-erfme = lwa_mseg_gr-erfme.
*        lwa_final_gr-usrid = lwa_rec_final-usrid.
*        lwa_final_gr-smtp_addr = lwa_rec_final-smtp_addr.
*        APPEND lwa_final_gr TO lta_final_gr.
*        CLEAR: lwa_final_gr,
*               lwa_rec_final.
*      ENDLOOP.
*      CLEAR: ltp_tabix,
*             lwa_rec_final.
*    ENDIF.
*    CLEAR lwa_mseg_gr.
*  ENDLOOP.
*  SORT lta_final_gr BY ebeln usrid.
*
*  lta_final_po[] = lta_final_gr[].
*  SORT lta_final_po BY ebeln.
*  DELETE ADJACENT DUPLICATES FROM lta_final_po COMPARING ebeln.
*
** Send Email notification to Buyer, Requestor and Goods Recipient
*  LOOP AT lta_final_po INTO lwa_final_po.
**   Build Subject
*    CONCATENATE 'Goods Receipt notification for PO'(001) lwa_final_po-ebeln INTO ltp_subject SEPARATED BY space.
*    READ TABLE lta_final_gr INTO lwa_final_gr WITH KEY ebeln = lwa_final_po-ebeln.
*    IF sy-subrc = 0.
*      ltp_tabix = sy-tabix.
*      LOOP AT lta_final_gr INTO lwa_final_gr FROM sy-tabix.
*        IF lwa_final_gr-ebeln NE lwa_final_po-ebeln.
*          EXIT.
*        ENDIF.
*        lwa_final_tmp = lwa_final_gr.
**       Build Object content
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = lwa_final_tmp-matnr
*          IMPORTING
*            output = lwa_final_tmp-matnr.
*
*        ltp_menge = lwa_final_tmp-erfmg.
*        SHIFT ltp_menge LEFT DELETING LEADING space.
*        CONCATENATE 'Goods Receipt' lwa_final_tmp-mblnr 'has been completed for Purchase Order' lwa_final_tmp-ebeln
*                    'PO Item' lwa_final_tmp-ebelp 'Material' lwa_final_tmp-matnr lwa_final_tmp-txz01
*                    'quantity' ltp_menge lwa_final_tmp-erfme INTO lwa_text SEPARATED BY space.
*        APPEND lwa_text TO lta_text.
*        CLEAR lwa_text.
*
*        AT END OF usrid.
*          TRY.
**             Create persistent send request
*              lcl_sendrequest = cl_bcs=>create_persistent( ).
*
*              lcl_document = cl_document_bcs=>create_document(
*                               i_type         = 'RAW'
*                               i_text         = lta_text[]
*                               i_subject      = ltp_subject ).
*              REFRESH lta_text.
*
**             Add document object to send request
*              CALL METHOD lcl_sendrequest->set_document( lcl_document ).
*
**             Set sender
*              lcl_sender = cl_sapuser_bcs=>create( sy-uname ).
*
*              CALL METHOD lcl_sendrequest->set_sender
*                EXPORTING
*                  i_sender = lcl_sender.
*
**             Create recipient object
*              lif_recipient = cl_cam_address_bcs=>create_internet_address( lwa_final_tmp-smtp_addr ).
*
**             Add recipient object to send request
*              CALL METHOD lcl_sendrequest->add_recipient
*                EXPORTING
*                  i_recipient = lif_recipient
*                  i_express   = 'X'.
*
**             Send document
*              CALL METHOD lcl_sendrequest->send(
*                EXPORTING
*                  i_with_error_screen = 'X'
*                RECEIVING
*                  result              = ltp_senttoall ).
*
*            CATCH cx_bcs INTO lcl_bcsexception.
*          ENDTRY.
*        ENDAT.
*        CLEAR: ltp_menge,
*               lwa_text,
*               lwa_final_gr,
*               lwa_final_tmp.
*      ENDLOOP.
*      REFRESH: lta_text.
*      CLEAR: ltp_tabix,
*             lwa_final_gr.
*    ENDIF.
*    CLEAR: ltp_subject,
*           lwa_final_po.
*  ENDLOOP.

ENDMETHOD.


method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_UPDATE.
endmethod.
ENDCLASS.
