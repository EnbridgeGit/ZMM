*&---------------------------------------------------------------------*
*& Report  ZLMME002_IDOC_ORDRSP_PO_UNBLCK
*&
*&---------------------------------------------------------------------*
************************************************************************
*                                                                      *
*  Client:    Spectra Energy                                           *
*  Author:    John Hartung                                             *
*  Date:      April 07, 2011                                           *
*  Track #:   TR872 Release 2                                          *
*                                                                      *
*  Description:                                                        *
*       Enhancement to function module IDOC_INPUT_ORDRSP.  When        *
*       an IDOC for PO Confirmations (message type ORDRSP) is          *
*       processed; the form in this program will be called to          *
*       check if any PO items are blocked.  If they are blocked,       *
*       then the PO items within the IDoc will be unblocked.           *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
* 04/07/11 0872 JRHARTU D30K916441 - Initial program development       *
*----------------------------------------------------------------------*
************************************************************************
REPORT  ZLMME002_IDOC_ORDRSP_PO_UNBLCK.

*eject
*&---------------------------------------------------------------------*
*&      Form  f_unblock_po_items
*&---------------------------------------------------------------------*
*       Unblock PO Items
*----------------------------------------------------------------------*
FORM f_unblock_po_items
  TABLES   it_idoc_contrl      STRUCTURE edidc
           it_idoc_data        STRUCTURE edidd
           ct_idoc_status      STRUCTURE bdidocstat
           ct_return_variables STRUCTURE bdwfretvar
  USING    iv_mass_processing  TYPE mass_proc
  CHANGING cv_workflow_result  TYPE wf_result
           cv_exitflag         TYPE char1.

  DATA:    lt_idoc_status      TYPE STANDARD TABLE OF bdidocstat,
           lt_return_variables TYPE STANDARD TABLE OF bdwfretvar,
           lv_workflow_result  TYPE wf_result,
           lv_exitflag         TYPE char1.

  DATA:    ls_idoc_contrl      TYPE edidc,
           ls_idoc_data        TYPE edidd,
           ls_return_variables TYPE bdwfretvar,
           ls_e1edk02          TYPE e1edk02,
           ls_e1edp01          TYPE e1edp01,
           ls_e1edp02          TYPE e1edp02.

  DATA:    lv_docnum           TYPE edi_docnum,
           lv_segnum           TYPE idocdsgnum,
           lv_ebeln_hdr        TYPE ebeln,
           lv_ebeln_item       TYPE ebeln,
           lv_ebelp            TYPE ebelp,
           lv_itemno           TYPE i.

  DATA:    BEGIN OF ls_ekpo_key,
             ebeln             TYPE ebeln,
             ebelp             TYPE ebelp,
           END   OF ls_ekpo_key.

  DATA:    lt_ekpo_key         LIKE STANDARD TABLE OF ls_ekpo_key.

*eject
  CLEAR    lv_exitflag.

* Check if all the IDoc's have the correct basic type
  CLEAR                                ls_idoc_contrl.
  LOOP AT  it_idoc_contrl         INTO ls_idoc_contrl.

    CLEAR                              lv_docnum.
    MOVE     ls_idoc_contrl-docnum  TO lv_docnum.

    IF   ( ls_idoc_contrl-idoctp(6) NE 'ORDERS' ).

      PERFORM f_idoc_status_fill
                                TABLES lt_idoc_status
                                USING  '51'
                                       lv_docnum
                                       'E0'
                                       '029'
                                       ls_idoc_contrl-idoctp
                                       ' '
                                       'IDOC_INPUT_ORDRSP'
                                       ' '
                                       'IDOC_INPUT_ORDRSP_UNBLOCK'
                                       0
                                       'IDOCTP'.

* dummy message to make the message appear in the where-used list
      IF 1 EQ 2.
        MESSAGE E029(E0) WITH ls_idoc_contrl-idoctp
                              ' ' 'IDOC_INPUT_ORDRSP'.
      ENDIF.

      CLEAR                            lv_workflow_result.
      MOVE     '99999'              TO lv_workflow_result.

      IF ( iv_mass_processing   IS NOT INITIAL ).
        CLEAR                ls_return_variables.
        MOVE   'Error_IDOCs'
                          TO ls_return_variables-wf_param.
        MOVE   lv_docnum
                          TO ls_return_variables-doc_number.
        APPEND ls_return_variables
                          TO lt_return_variables.
      ENDIF.

      lv_exitflag  = 'X'.

      RETURN.

    ENDIF.

    CLEAR  ls_idoc_contrl.
  ENDLOOP.

*eject
* Process each IDoc
  CLEAR                                ls_idoc_contrl.
  LOOP AT  it_idoc_contrl         INTO ls_idoc_contrl.

    CLEAR                              lv_docnum.
    MOVE     ls_idoc_contrl-docnum  TO lv_docnum.

    CLEAR    ls_e1edk02.
    CLEAR    ls_e1edp01.
    CLEAR    ls_e1edp02.

    CLEAR    lt_ekpo_key[].

    CLEAR    lv_exitflag.

    CLEAR                              ls_idoc_data.
    LOOP AT  it_idoc_data         INTO ls_idoc_data
                                 WHERE docnum = ls_idoc_contrl-docnum.

      IF   ( lv_exitflag IS NOT INITIAL ).
        EXIT.
      ENDIF.

      CASE   ls_idoc_data-segnam.

*eject
* Process the document header reference data
        WHEN 'E1EDK02'.

          CLEAR                             ls_e1edk02.
          MOVE     ls_idoc_data-sdata    TO ls_e1edk02.

* Convert the PO document number
          IF     ( ls_e1edk02-qualf      EQ '001' ).

            PERFORM  f_fieldlength   TABLES lt_idoc_status
                                            lt_return_variables
                                      USING iv_mass_processing
                                            lv_docnum
                                            ls_e1edk02-belnr
                                            '10'
                                            'E1EDK02-BELNR'
                                            'IDOC_INPUT_ORDRSP_UNBLOCK'
                                            'E1EDK02'
                                            ls_idoc_data-segnum
                                   CHANGING lv_workflow_result
                                            lv_exitflag.

            CHECK  ( lv_exitflag IS INITIAL ).

            CLEAR    lv_ebeln_hdr.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = ls_e1edk02-belnr(10)
              IMPORTING
                OUTPUT = lv_ebeln_hdr.

          ENDIF.

*eject
* Process the document item general data
        WHEN 'E1EDP01'.

          CLEAR                             ls_e1edp01.
          MOVE     ls_idoc_data-sdata    TO ls_e1edp01.

* Process the document item reference data
        WHEN 'E1EDP02'.

          IF     ( ls_e1edp01-uepos      IS INITIAL ).

            CLEAR                           ls_e1edp02.
            MOVE   ls_idoc_data-sdata    TO ls_e1edp02.

* Convert the PO document number
            IF   ( ls_e1edp02-qualf      EQ '001' ).

              PERFORM  f_fieldlength TABLES lt_idoc_status
                                            lt_return_variables
                                      USING iv_mass_processing
                                            lv_docnum
                                            ls_e1edp02-belnr
                                            '10'
                                            'E1EDP02-BELNR'
                                            'IDOC_INPUT_ORDRSP_UNBLOCK'
                                            'E1EDP02'
                                            ls_idoc_data-segnum
                                   CHANGING lv_workflow_result
                                            lv_exitflag.

              CHECK  ( lv_exitflag IS INITIAL ).

              CLEAR    lv_ebeln_item.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  INPUT  = ls_e1edp02-belnr(10)
                IMPORTING
                  OUTPUT = lv_ebeln_item.

*eject
* Compare the PO document numbers
              IF ( lv_ebeln_hdr NE lv_ebeln_item ).

                PERFORM f_idoc_status_fill
                                TABLES lt_idoc_status
                                USING  '51'
                                       lv_docnum
                                       'ME'
                                       '788'
                                       lv_ebeln_item
                                       lv_ebeln_hdr
                                       ' '
                                       ' '
                                       'IDOC_INPUT_ORDRSP_UNBLOCK'
                                       ls_idoc_data-segnum
                                       'BELNR'.

* dummy message to make the message appear in the where-used list
                IF 1 EQ 2.
                  MESSAGE S788(ME) WITH lv_ebeln_item lv_ebeln_hdr.
                ENDIF.

                CLEAR                            lv_workflow_result.
                MOVE     '99999'              TO lv_workflow_result.

                IF ( iv_mass_processing   IS NOT INITIAL ).
                  CLEAR                ls_return_variables.
                  MOVE   'Error_IDOCs'
                                    TO ls_return_variables-wf_param.
                  MOVE   lv_docnum
                                    TO ls_return_variables-doc_number.
                  APPEND ls_return_variables
                                    TO lt_return_variables.
                ENDIF.

                lv_exitflag  = 'X'.

                EXIT.

              ENDIF.

*eject
* Convert item number from IDoc 6 digits to PO 5 digits
              CLEAR    lv_ebelp.

              lv_itemno = ls_e1edp02-zeile.

              IF ( lv_itemno GT 99999 ).

                PERFORM  f_fieldlength
                                TABLES lt_idoc_status
                                       lt_return_variables
                                 USING iv_mass_processing
                                       lv_docnum
                                       ls_e1edp02-zeile
                                       '5'
                                       'E1EDP02-ZEILE'
                                       'IDOC_INPUT_ORDRSP_UNBLOCK'
                                       'E1EDP02'
                                       ls_idoc_data-segnum
                              CHANGING lv_workflow_result
                                       lv_exitflag.

                CHECK  ( lv_exitflag IS INITIAL ).

              ELSE.

                lv_ebelp = lv_itemno.

              ENDIF.

* Append the PO document number and item number to the item key table
              CLEAR                         ls_ekpo_key.
              MOVE     lv_ebeln_item     TO ls_ekpo_key-ebeln.
              MOVE     lv_ebelp          TO ls_ekpo_key-ebelp.
              APPEND   ls_ekpo_key       TO lt_ekpo_key.

            ENDIF.

          ENDIF.

      ENDCASE.

      CLEAR  ls_idoc_data.
    ENDLOOP.

*eject
    CHECK  ( lv_exitflag IS INITIAL ).

* Unblock the PO items
    IF ( lt_ekpo_key[] IS NOT INITIAL ).

      PERFORM  f_update_items_unblock   TABLES   lt_ekpo_key
                                                 lt_idoc_status
                                                 lt_return_variables
                                        USING    iv_mass_processing
                                                 lv_docnum
                                        CHANGING lv_workflow_result
                                                 lv_exitflag.

    ENDIF.

    CLEAR  ls_idoc_contrl.
  ENDLOOP.

ENDFORM.                    " f_unblock_po_items
*eject
*&---------------------------------------------------------------------*
*&      Form  f_fieldlength
*&---------------------------------------------------------------------*
*       Determine and check the fieldlength
*----------------------------------------------------------------------*
FORM f_fieldlength
  TABLES   ct_idoc_status      STRUCTURE bdidocstat
           ct_return_variables STRUCTURE bdwfretvar
  USING    iv_mass_processing  TYPE mass_proc
           iv_doc_number       TYPE edi_docnum
           fld_field
           fld_length
           fld_name
           fld_rout
           fld_segn
           segnum              LIKE edidd-segnum
  CHANGING cv_workflow_result  TYPE wf_result
           cv_exitflag         TYPE char1.

  DATA:  ls_return_variables   TYPE bdwfretvar.

* Determine the field length - set system variable FDPOS
  IF ( fld_field CP '*# ' ).
  ENDIF.

* Check the field length
  IF ( sy-fdpos GT fld_length ).

    PERFORM f_idoc_status_fill  TABLES ct_idoc_status
                                 USING '51'
                                       iv_doc_number
                                       'ME'
                                       '759'
                                       FLD_NAME
                                       ' '
                                       ' '
                                       ' '
                                       FLD_ROUT
                                       SEGNUM
                                       FLD_NAME.

*eject
* dummy message to make the message appear in the where-used list
    IF 1 EQ 2.
      MESSAGE S759(ME) WITH fld_name.
    ENDIF.

    CLEAR                              cv_workflow_result.
    MOVE     '99999'                TO cv_workflow_result.

    IF ( iv_mass_processing     IS NOT INITIAL ).
      CLEAR                            ls_return_variables.
      MOVE     'Error_IDOCs'        TO ls_return_variables-wf_param.
      MOVE     iv_doc_number        TO ls_return_variables-doc_number.
      APPEND   ls_return_variables  TO ct_return_variables.
    ENDIF.

    cv_exitflag  = 'X'.

  ENDIF.

ENDFORM.                    " f_fieldlength
*eject
*&---------------------------------------------------------------------*
*&      Form  f_idoc_status_fill
*&---------------------------------------------------------------------*
*       Fill IDoc Status
*----------------------------------------------------------------------*
FORM f_idoc_status_fill
  TABLES ct_idoc_status STRUCTURE bdidocstat
  USING  value(status)
         value(docnum)
         value(msgid)
         value(msgno)
         value(msgv1)
         value(msgv2)
         value(msgv3)
         value(msgv4)
         value(routid)
         value(segnum)
         value(segfld).

  DATA:  ls_idoc_status TYPE bdidocstat.

  CLEAR                                ls_idoc_status.
  MOVE     sy-uname                 TO ls_idoc_status-uname.
  MOVE     status                   TO ls_idoc_status-status.
  MOVE     docnum                   TO ls_idoc_status-docnum.
  IF     ( status                   EQ '53' ).
    MOVE   'S'                      TO ls_idoc_status-msgty.
  ELSEIF ( status                   EQ '52' ).
    MOVE   'E'                      TO ls_idoc_status-msgty.
  ELSEIF ( status                   EQ '51' ).
    MOVE   'E'                      TO ls_idoc_status-msgty.
  ENDIF.
  MOVE     msgid                    TO ls_idoc_status-msgid.
  MOVE     msgno                    TO ls_idoc_status-msgno.
  MOVE     msgv1                    TO ls_idoc_status-msgv1.
  MOVE     msgv2                    TO ls_idoc_status-msgv2.
  MOVE     msgv3                    TO ls_idoc_status-msgv3.
  MOVE     msgv4                    TO ls_idoc_status-msgv4.
  MOVE     segnum                   TO ls_idoc_status-segnum.
  MOVE     segfld                   TO ls_idoc_status-segfld.
  MOVE     'SAPLEINM'               TO ls_idoc_status-repid.
  MOVE     routid                   TO ls_idoc_status-routid.
  APPEND   ls_idoc_status           TO ct_idoc_status.

ENDFORM.                    " f_idoc_status_fill
*eject
*&---------------------------------------------------------------------*
*&      Form  f_update_items_unblock
*&---------------------------------------------------------------------*
*       Unblock the PO items
*----------------------------------------------------------------------*
FORM f_update_items_unblock
  TABLES   it_ekpo_key
           ct_idoc_status      STRUCTURE bdidocstat
           ct_return_variables STRUCTURE bdwfretvar
  USING    iv_mass_processing  TYPE mass_proc
           iv_doc_number       TYPE edi_docnum
  CHANGING cv_workflow_result  TYPE wf_result
           cv_exitflag         TYPE char1.

  DATA:    lv_subrc            TYPE sysubrc,
           lv_flag_commit      TYPE flag.

  DATA:    BEGIN OF ls_ekko,
             ebeln             TYPE ebeln,
             loekz             TYPE eloek,
           END   OF ls_ekko.

  DATA:    BEGIN OF ls_ekpo,
             ebeln             TYPE ebeln,
             ebelp             TYPE ebelp,
             loekz             TYPE eloek,
           END   OF ls_ekpo.

  DATA:    lt_ekpo             LIKE STANDARD TABLE OF ls_ekpo.

  DATA:    BEGIN OF ls_ekpo_key,
             ebeln             TYPE ebeln,
             ebelp             TYPE ebelp,
           END   OF ls_ekpo_key.

  DATA:    lt_ekpo_key         LIKE STANDARD TABLE OF ls_ekpo_key.

  DATA:    ls_return_variables TYPE bdwfretvar.

  DATA:    lt_return           TYPE STANDARD TABLE OF bapiret2,
           ls_return           LIKE LINE OF lt_return,
           lt_poitem           TYPE STANDARD TABLE OF bapimepoitem,
           ls_poitem           LIKE LINE OF lt_poitem,
           lt_poitemx          TYPE STANDARD TABLE OF bapimepoitemx,
           ls_poitemx          LIKE LINE OF lt_poitemx.

*eject
  CHECK  ( it_ekpo_key[]   IS NOT INITIAL ).

  CLEAR:   lt_ekpo_key[],         ls_ekpo_key.
  LOOP AT  it_ekpo_key       INTO ls_ekpo_key.
    APPEND ls_ekpo_key         TO lt_ekpo_key.
    CLEAR  ls_ekpo_key.
  ENDLOOP.

  CLEAR          ls_ekpo_key.
  READ     TABLE lt_ekpo_key
            INTO ls_ekpo_key
           INDEX 1.

* Select the PO header
  CLEAR    ls_ekko.
  SELECT   SINGLE ebeln loekz
    INTO   ls_ekko
    FROM   ekko
   WHERE   ebeln = ls_ekpo_key-ebeln.
  IF ( sy-subrc NE 0 ).
    CLEAR  ls_ekko.
    RETURN.
  ENDIF.

* Select the PO items
  CLEAR    lt_ekpo[].
  SELECT   ebeln ebelp loekz
    INTO   TABLE lt_ekpo
    FROM   ekpo FOR ALL ENTRIES IN lt_ekpo_key
   WHERE   ebeln = lt_ekpo_key-ebeln
     AND   ebelp = lt_ekpo_key-ebelp.
  IF ( sy-subrc NE 0 ).
    CLEAR  lt_ekpo[].
    RETURN.
  ENDIF.

  SORT     lt_ekpo ASCENDING BY ebeln ebelp.
  DELETE   ADJACENT DUPLICATES FROM lt_ekpo COMPARING ebeln ebelp.

*eject
* Check if the PO header is deleted
  IF ( ls_ekko-loekz EQ 'L' ).
    RETURN.
  ENDIF.

* Check if any PO item is deleted
  CLEAR          ls_ekpo.
  READ     TABLE lt_ekpo
            INTO ls_ekpo
        WITH KEY loekz = 'L'.
  IF ( sy-subrc EQ 0 ).
    CLEAR        ls_ekpo.
    RETURN.
  ENDIF.

* Identify the blocked PO items and build the BAPI change data
  CLEAR                 ls_ekpo.
  LOOP AT  lt_ekpo INTO ls_ekpo.
    IF       ( ls_ekpo-loekz        EQ 'S' ). "Blocked
      CLEAR                            ls_poitem.
      MOVE     ls_ekpo-ebelp        TO ls_poitem-po_item.
      MOVE     SPACE                TO ls_poitem-delete_ind. "Unblock
      APPEND   ls_poitem            TO lt_poitem.
      CLEAR                            ls_poitemx.
      MOVE     ls_ekpo-ebelp        TO ls_poitemx-po_item.
      MOVE     'X'                  TO ls_poitemx-po_itemx.
      MOVE     'X'                  TO ls_poitemx-delete_ind.
      APPEND   ls_poitemx           TO lt_poitemx.
    ENDIF.
    CLEAR  ls_ekpo.
  ENDLOOP.

*eject
* Check if the PO is enqueued
  IF     ( lt_poitem[] IS INITIAL ).
    RETURN.
  ENDIF.

  CALL FUNCTION 'MM_ENQUEUE_DOCUMENT'
    EXPORTING
      I_EBELN         = ls_ekko-ebeln
      I_BSTYP         = 'F'
      I_WAIT          = 'X'
    EXCEPTIONS
      DOCUMENT_LOCKED = 1
      PARAMETER_ERROR = 2
      OTHERS          = 3.

  IF ( sy-subrc NE 0 ).

    PERFORM f_idoc_status_fill  TABLES ct_idoc_status
                                USING  '51'
                                       iv_doc_number
                                       sy-msgid
                                       sy-msgno
                                       sy-msgv1
                                       sy-msgv2
                                       sy-msgv3
                                       sy-msgv4
                                       'IDOC_INPUT_ORDRSP_UNBLOCK'
                                       0
                                       ' '.

    CLEAR                              cv_workflow_result.
    MOVE     '99999'                TO cv_workflow_result.

    IF ( iv_mass_processing         IS NOT INITIAL ).
      CLEAR                      ls_return_variables.
      MOVE   'Error_IDOCs'    TO ls_return_variables-wf_param.
      MOVE   iv_doc_number    TO ls_return_variables-doc_number.
      APPEND ls_return_variables
                              TO ct_return_variables.
    ENDIF.

    cv_exitflag = 'X'.

    RETURN.

  ENDIF.

  CALL FUNCTION 'MM_DEQUEUE_DOCUMENT'
    EXPORTING
      I_EBELN = ls_ekko-ebeln
      I_BSTYP = 'F'.

*eject
* Unblock the PO items via BAPI_PO_CHANGE
  CLEAR                                lv_flag_commit.
  MOVE     'X'                      TO lv_flag_commit.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      PURCHASEORDER                = ls_ekko-ebeln
*     POHEADER                     =
*     POHEADERX                    =
    TABLES
      RETURN                       = lt_return
      POITEM                       = lt_poitem
      POITEMX                      = lt_poitemx.

  IF ( sy-subrc NE 0 ).

    CLEAR                              lv_flag_commit.

    PERFORM f_idoc_status_fill  TABLES ct_idoc_status
                                USING  '51'
                                       iv_doc_number
                                       sy-msgid
                                       sy-msgno
                                       sy-msgv1
                                       sy-msgv2
                                       sy-msgv3
                                       sy-msgv4
                                       'IDOC_INPUT_ORDRSP_UNBLOCK'
                                       0
                                       ' '.

    CLEAR                                  cv_workflow_result.
    MOVE           '99999'              TO cv_workflow_result.

    IF ( iv_mass_processing         IS NOT INITIAL ).
      CLEAR                      ls_return_variables.
      MOVE   'Error_IDOCs'    TO ls_return_variables-wf_param.
      MOVE   iv_doc_number    TO ls_return_variables-doc_number.
      APPEND ls_return_variables
                              TO ct_return_variables.
    ENDIF.

    cv_exitflag = 'X'.

*eject
  ELSE.

    CLEAR                              ls_return.
    LOOP AT  lt_return            INTO ls_return.

      IF   ( ls_return          IS NOT INITIAL  ).
        IF ( ls_return-type         EQ 'A' ) OR
           ( ls_return-type         EQ 'E' ).

          CLEAR                        lv_flag_commit.

          PERFORM f_idoc_status_fill
                                TABLES ct_idoc_status
                                USING  '51'
                                       iv_doc_number
                                       ls_return-id
                                       ls_return-number
                                       ls_return-message_v1
                                       ls_return-message_v2
                                       ls_return-message_v3
                                       ls_return-message_v4
                                       'IDOC_INPUT_ORDRSP_UNBLOCK'
                                       0
                                       ' '.

          CLEAR                            cv_workflow_result.
          MOVE     '99999'              TO cv_workflow_result.

          IF ( iv_mass_processing   IS NOT INITIAL ).
            CLEAR                ls_return_variables.
            MOVE   'Error_IDOCs'
                              TO ls_return_variables-wf_param.
            MOVE   iv_doc_number
                              TO ls_return_variables-doc_number.
            APPEND ls_return_variables
                              TO ct_return_variables.
          ENDIF.

          cv_exitflag = 'X'.

          EXIT.

        ENDIF.
      ENDIF.

      CLEAR  ls_return.
    ENDLOOP.

  ENDIF.

*eject
* Commit or rollback changes
  IF ( lv_flag_commit IS NOT INITIAL ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ENDIF.

ENDFORM.                    " f_update_items_unblock
