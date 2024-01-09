"Name: \FU:IDOC_INPUT_ORDRSP\SE:END\EI
ENHANCEMENT 0 Z_MMIMP_PO_CON_EMAIL.
*&---------------------------------------------------------------------*
*& Program Name       : Z_MMIMP_PO_CON_EMAIL                           *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 06-May-2016                                    *
*& Object ID          : ACR-159                                        *
*& Application Area   : MM                                             *
*& Description        : Send PO confirmation email notification to     *
*&                      Requisitioner and Goods Recipient.             *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 10/28/2016                                           *
* Modified By   : Praveena Anusuri                                     *
* Object ID     : ACR-2445                                             *
* Description   : Changed the content of the Email Header and Body.    *
*----------------------------------------------------------------------*
    types: begin of lty_recipients,
           ebeln        type ebeln,
           ebelp        type ebelp,
           usrid        type char12,
         end of lty_recipients,
         begin of lty_conf,
           ebeln        type ebeln,
           ebelp        type char5,
           confno       type thead-tdname,
         end of lty_conf,
         begin of lty_conf_txt,
           ebeln        type ebeln,
           ebelp        type char5,
           confno       type thead-tdname,
           conftxt      type char255,
         end of lty_conf_txt,
         begin of lty_conf_final,
           ebeln        type ebeln,
           usrid        type char12,
           ebelp        type char5,
           confno       type thead-tdname,
           conftxt      type char255,
         end of lty_conf_final.
  data: ltp_ebeln          type ebeln,
        ltp_ebelp          type e1edp01-posex,
        ltp_count          type i,
        ltp_segnum_p01     type edidd-segnum,
        ltp_segnum_pt1     type edidd-segnum,
        ltp_conftxt        type string,
        lwa_idoc_data      type edidd,
        lwa_e1edk02        type e1edk02,
        lwa_e1edp01        type e1edp01,
        lwa_e1edp02        type e1edp02,
        lwa_e1edpt1        type e1edpt1,
        lwa_e1edpt2        type e1edpt2,
        lta_recipients     type table of lty_recipients,
        lwa_recipients     type lty_recipients,
        lta_conf           type standard table of lty_conf,
        lwa_conf           type lty_conf,
        lwa_conf_tmp       type lty_conf,
        lta_conf_txt       type standard table of lty_conf_txt,
        lwa_conf_txt       type lty_conf_txt,
        lta_conf_txt_tmp   type standard table of lty_conf_txt,
        lwa_conf_txt_tmp   type lty_conf_txt,
        lta_conf_final     type standard table of lty_conf_final,
        lwa_conf_final     type lty_conf_final,
        lwa_conf_final_tmp type lty_conf_final,
        ltp_tabix          type sy-tabix,
        ltp_subject        type so_obj_des,
        lta_text           type soli_tab,
        lwa_text           type soli,
        lta_parameter      type standard table of bapiparam,
        lwa_parameter      type bapiparam,
        lta_return         type standard table of bapiret2,
        lta_addsmtp        type standard table of bapiadsmtp,
        lwa_addsmtp        type bapiadsmtp,
        lcl_sendrequest    type ref to cl_bcs,
        lcl_document       type ref to cl_document_bcs,
        lcl_sender         type ref to cl_sapuser_bcs,
        lif_recipient      type ref to if_recipient_bcs,
        lcl_bcsexception   type ref to cx_bcs,
        ltp_senttoall      type os_boolean,
*BOI by PANUSURI Ticket ACR-2445
        ltp_hdr_text(12)   TYPE c,
        ltp_bdy_text(10)   TYPE c,
        ltp_ven_name       TYPE name1_gp,
        ltp_lifnr          TYPE elifn.
*EOI by PANUSURI Ticket ACR-2445

  constants: lco_ordrsp    type edidc-mestyp value 'ORDRSP',
             lco_parid     type memoryid     value 'Z_NO_CON_EM',
*BOI by PANUSURI Ticket ACR-2445
             lco_con_text_hdr(12) TYPE c VALUE 'Confirmation',
             lco_rej_text_hdr(9)  TYPE c VALUE 'Rejection',
             lco_con_text_bdy(10) TYPE c VALUE 'confirmed.',
             lco_rej_text_bdy(9)  TYPE c VALUE 'rejected.'.
*EOI by PANUSURI Ticket ACR-2445

  loop at idoc_contrl where mestyp eq lco_ordrsp.
    loop at idoc_data into lwa_idoc_data where docnum eq idoc_contrl-docnum.
      clear: lwa_e1edk02,
             lwa_e1edp01,
             lwa_e1edp02,
             lwa_e1edpt1,
             lwa_e1edpt2.
      case lwa_idoc_data-segnam.
        when 'E1EDK02'.
          clear lwa_e1edk02.
          lwa_e1edk02 = lwa_idoc_data-sdata.
          if lwa_e1edk02-qualf = '001'.
            ltp_ebeln = lwa_e1edk02-belnr.
            lwa_conf-ebeln = ltp_ebeln.
          endif.

        when 'E1EDP01'.
          clear lwa_e1edp01.
          lwa_e1edp01 = lwa_idoc_data-sdata.
          ltp_ebelp = lwa_e1edp01-posex.
          shift ltp_ebelp left deleting leading '0'.
          ltp_count = strlen( ltp_ebelp ).
            ltp_count = 5 - ltp_count.
            do ltp_count times.
              concatenate '0' ltp_ebelp into ltp_ebelp.
            enddo.
          lwa_conf-ebelp = ltp_ebelp.
          ltp_segnum_p01 = lwa_idoc_data-segnum.

        when 'E1EDP02'.
          clear lwa_e1edp02.
          lwa_e1edp02 = lwa_idoc_data-sdata.
          if lwa_idoc_data-psgnum = ltp_segnum_p01.
*           Get Confirmation number
            if lwa_e1edp02-qualf = '002'.
              lwa_conf-confno = lwa_e1edp02-belnr.
              if lwa_conf-confno is not initial.
*               Get Requisitioner
                select ebeln
                       ebelp
                       afnam
                       from ekpo
                       appending table lta_recipients
                       where ebeln eq ltp_ebeln
                       and   ebelp eq lwa_conf-ebelp.
                if sy-subrc = 0.
                  sort lta_recipients by ebeln ebelp usrid.
                endif.
*               Get Goods Recipient
                select ebeln
                       ebelp
                       wempf
                       from ekkn
                       appending table lta_recipients
                       where ebeln eq ltp_ebeln
                       and   ebelp eq lwa_conf-ebelp.
                if sy-subrc = 0.
                  sort lta_recipients by ebeln ebelp usrid.
                endif.
                append lwa_conf to lta_conf.
*BOI by PANUSURI Ticket ACR-2445
                IF lwa_conf-confno CS 'RJT'.
                  ltp_hdr_text = lco_rej_text_hdr.
                  ltp_bdy_text = lco_rej_text_bdy.
                ELSE.
                  ltp_hdr_text = lco_con_text_hdr.
                  ltp_bdy_text = lco_con_text_bdy.
                ENDIF.
*EOI by PANUSURI Ticket ACR-2445
              endif.
            endif.
          endif.
*BOC by PANUSURI Ticket ACR-2445
*        when 'E1EDPT1'.
*          clear lwa_e1edpt1.
*          lwa_e1edpt1 = lwa_idoc_data-sdata.
*          if lwa_idoc_data-psgnum = ltp_segnum_p01.
*            if lwa_e1edpt1-tdid = 'F06'.
*              ltp_segnum_pt1 = lwa_idoc_data-segnum.
*            endif.
*          endif.
*
*        when 'E1EDPT2'.
*          clear lwa_e1edpt2.
*          lwa_e1edpt2 = lwa_idoc_data-sdata.
*          if lwa_idoc_data-psgnum = ltp_segnum_pt1.
**           Get Confirmation text for sending Emails
*            if lwa_e1edpt2-tdline is not initial.
*              lwa_conf_txt-conftxt = lwa_e1edpt2-tdline.
*              lwa_conf_txt-ebeln = lwa_conf-ebeln.
*              lwa_conf_txt-ebelp = lwa_conf-ebelp.
*              lwa_conf_txt-confno = lwa_conf-confno.
*              append lwa_conf_txt to lta_conf_txt.
*              clear lwa_conf_txt.
*            endif.
*          endif.
*EOC by PANUSURI Ticket ACR-2445
      endcase.
    endloop.
    clear lwa_conf.
*BOC by PANUSURI Ticket ACR-2445
*    loop at lta_conf into lwa_conf.
*      lwa_conf_tmp = lwa_conf.
*      clear: lwa_conf_txt,
*             ltp_conftxt.
*      read table lta_conf_txt into lwa_conf_txt with key ebeln = lwa_conf_tmp-ebeln
*                                                         ebelp = lwa_conf_tmp-ebelp.
*      if sy-subrc = 0.
*        ltp_tabix = sy-tabix.
*        loop at lta_conf_txt into lwa_conf_txt from ltp_tabix.
*          if lwa_conf_txt-ebeln ne lwa_conf_tmp-ebeln or lwa_conf_txt-ebelp ne lwa_conf_tmp-ebelp.
*            exit.
*          endif.
*          if ltp_conftxt is initial.
*            ltp_conftxt = lwa_conf_txt-conftxt.
*          else.
*            concatenate ltp_conftxt space lwa_conf_txt-conftxt into ltp_conftxt
*                    separated by space.
*          endif.
*        endloop.
*      endif.
*      at end of ebelp.
*        lwa_conf_txt_tmp-conftxt = ltp_conftxt.
*        lwa_conf_txt_tmp-ebeln = lwa_conf_tmp-ebeln.
*        lwa_conf_txt_tmp-ebelp = lwa_conf_tmp-ebelp.
*        lwa_conf_txt_tmp-confno = lwa_conf_tmp-confno.
*        append lwa_conf_txt_tmp to lta_conf_txt_tmp.
*        clear: lwa_conf_txt_tmp,
*               ltp_conftxt.
*      endat.
*      clear: lwa_conf,
*             lwa_conf_tmp.
*    endloop.
*EOC by PANUSURI Ticket ACR-2445
    loop at lta_recipients into lwa_recipients.
*BOC by PANUSURI Ticket ACR-2445
*      clear lwa_conf_txt_tmp.
*      read table lta_conf_txt_tmp into lwa_conf_txt_tmp with key ebeln = lwa_recipients-ebeln
*                                                                 ebelp = lwa_recipients-ebelp.
*      if sy-subrc = 0.
*        if lwa_conf_txt_tmp-conftxt is not initial.
*          lwa_conf_final-conftxt = lwa_conf_txt_tmp-conftxt.
*        else.
*          clear lwa_conf_final-conftxt.
*        endif.
*        lwa_conf_final-ebeln = lwa_conf_txt_tmp-ebeln.
*        lwa_conf_final-ebelp = lwa_conf_txt_tmp-ebelp.
*        lwa_conf_final-confno = lwa_conf_txt_tmp-confno.
*        lwa_conf_final-usrid = lwa_recipients-usrid.
*        translate lwa_conf_final-usrid to upper case.
*        append lwa_conf_final to lta_conf_final.
*        clear lwa_conf_final.
*      endif.
*EOC by PANUSURI Ticket ACR-2445
*BOI by PANUSURI Ticket ACR-2445
      lwa_conf_final-ebeln = lwa_recipients-ebeln.
      lwa_conf_final-usrid = lwa_recipients-usrid.
      TRANSLATE lwa_conf_final-usrid TO UPPER CASE.
      APPEND lwa_conf_final TO lta_conf_final.
      CLEAR: lwa_conf_final,
             lwa_recipients.
*EOI by PANUSURI Ticket ACR-2445
    endloop.
*BOI by PANUSURI Ticket ACR-2445
*   Get Vendor name
    SELECT SINGLE lifnr
           FROM ekko
           INTO ltp_lifnr
           WHERE ebeln = ltp_ebeln.
    IF sy-subrc EQ 0.
      IF ltp_lifnr IS NOT INITIAL.
        SELECT SINGLE name1
               FROM lfa1
               INTO ltp_ven_name
               WHERE lifnr = ltp_lifnr.
      ENDIF.
    ENDIF.
*EOI by PANUSURI Ticket ACR-2445

*   Build Subject
*BOC by PANUSURI Ticket ACR-2445
*    concatenate 'Ariba Confirmation' space '-' space 'Order' space ltp_ebeln into ltp_subject separated by space
*                respecting blanks.
*    sort lta_conf_final by ebeln usrid ebelp.
*    delete adjacent duplicates from lta_conf_final comparing ebeln usrid ebelp.
*EOC by PANUSURI Ticket ACR-2445
*BOI by PANUSURI Ticket ACR-2445
     CONCATENATE 'Ariba Order' space ltp_hdr_text space '-' space ltp_ebeln INTO ltp_subject SEPARATED BY space
               RESPECTING BLANKS.
     SORT lta_conf_final BY ebeln usrid.
     DELETE ADJACENT DUPLICATES FROM lta_conf_final COMPARING ebeln usrid.
*EOI by PANUSURI Ticket ACR-2445
* Send Email
    loop at lta_conf_final into lwa_conf_final.
      lwa_conf_final_tmp = lwa_conf_final.
*     Build Object content
*      concatenate 'Item' lwa_conf_final_tmp-ebelp lwa_conf_final_tmp-confno lwa_conf_final_tmp-conftxt into lwa_text
*                             separated by space. "(-)PANUSURI Ticket ACR-2445
      CONCATENATE 'PO/SRO' lwa_conf_final_tmp-ebeln 'sent to supplier' ltp_ven_name 'has been' ltp_bdy_text INTO lwa_text
                           SEPARATED BY space.  "(+)PANUSURI Ticket ACR-2445
      append lwa_text to lta_text.
      clear lwa_text.

*      at end of usrid. "(-)PANUSURI Ticket ACR-2445
        call function 'BAPI_USER_GET_DETAIL'
          exporting
            username             = lwa_conf_final_tmp-usrid
          tables
            parameter            = lta_parameter
            return               = lta_return
            addsmtp              = lta_addsmtp.
        read table lta_parameter into lwa_parameter with key parid = lco_parid.
        if sy-subrc ne 0.
          try.
*           Create persistent send request
            lcl_sendrequest = cl_bcs=>create_persistent( ).

            lcl_document = cl_document_bcs=>create_document(
                             i_type         = 'RAW'
                             i_text         = lta_text[]
                             i_subject      = ltp_subject ).
            refresh lta_text.

*           Add document object to send request
            call method lcl_sendrequest->set_document( lcl_document ).

*           Set sender
            lcl_sender = cl_sapuser_bcs=>create( sy-uname ).

            call method lcl_sendrequest->set_sender
              exporting
                i_sender = lcl_sender.

*           Create recipient object
            clear lwa_addsmtp.
            read table lta_addsmtp into lwa_addsmtp index 1.
            if sy-subrc = 0.
              lif_recipient = cl_cam_address_bcs=>create_internet_address( lwa_addsmtp-e_mail ).
            endif.

*           Add recipient object to send request
            call method lcl_sendrequest->add_recipient
              exporting
                i_recipient = lif_recipient
                i_express   = 'X'.

*           Send document
            call method lcl_sendrequest->send(
              exporting
                i_with_error_screen = 'X'
              receiving
                result              = ltp_senttoall ).

            commit work.
            catch cx_bcs into lcl_bcsexception.
          endtry.
        endif.
*      endat. "(-)PANUSURI Ticket ACR-2445
      clear: lwa_conf_final,
             lwa_conf_final_tmp.
    endloop.
  endloop.

ENDENHANCEMENT.
