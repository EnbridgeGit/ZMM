FUNCTION ZMM_CONTRACT_CREATE_JRH.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      CONTRACT_DATA STRUCTURE  ZCONTRACTDATA_UG
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

************************************************************************
*                           Internal Tables                            *
************************************************************************
  DATA: it_contract TYPE STANDARD TABLE OF zcontractdata_ug,  " Internal table for Inbound data
        wa_contract LIKE LINE OF it_contract.                 " Work Area for it_contract

  REFRESH: git_messtab,
           git_messtab1,
           git_msg,
           git_msg1,
           git_bdcdata,
           git_olaid.
**  Move inbound data to internal table
  it_contract[] = contract_data[].

**Dummy code for testing
*  MODIFY zcontractdata_ug FROM TABLE it_contract.
*  IF sy-subrc EQ 0.
*    COMMIT WORK.
*  ENDIF.

** Loop for all internal table records
  LOOP AT it_contract INTO wa_contract.
**  Check whether to Create New/Update contract
    IF wa_contract-ola_id IS NOT INITIAL. " If true then Update Contract in ECC
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_contract-ola_id
        IMPORTING
          output = wa_contract-ola_id.

      SELECT SINGLE ebeln FROM ekko INTO gv_olaid WHERE ebeln EQ wa_contract-ola_id.
    ENDIF.

** Service Contracts Creation/Updation using BDC
    IF wa_contract-item_cat EQ 'D' AND wa_contract-acctasscat EQ 'U'.
      IF gv_olaid IS INITIAL.
** Create New Contract
        PERFORM f_srv_contractcreate USING wa_contract.
      ELSE.
**   Change Existing OLA in ECC
        PERFORM f_srv_contractchange USING wa_contract.
      ENDIF.

    ELSE.
** BAPI for Creation/Updation of Material/Quantity contracts
      IF gv_olaid IS INITIAL.
**     Create New contract.
        PERFORM f_mat_contractcreate USING wa_contract.
      ELSE.
**     Change existing Contract in ECC
        PERFORM f_mat_contractchange USING wa_contract.
      ENDIF.
    ENDIF.

    IF git_messtab2[] IS NOT INITIAL.
      APPEND LINES OF git_messtab2 TO git_messtab.
      REFRESH git_messtab2.
    ENDIF.

    IF git_msg2[] IS NOT INITIAL.
      APPEND LINES OF git_msg2 TO git_msg.
      REFRESH git_msg2.
    ENDIF.

    CLEAR wa_contract.
  ENDLOOP.

**Insert OLAID into custom table zcontract_olaid
  IF git_olaid[] IS NOT INITIAL.
    MODIFY zcontract_olaid FROM TABLE git_olaid.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.

  IF git_messtab1[] IS NOT INITIAL.
    APPEND LINES OF git_messtab1 TO git_messtab.
    REFRESH git_messtab1.
  ENDIF.

** BDC Error messages
  IF git_messtab[] IS NOT INITIAL.
    LOOP AT git_messtab INTO gwa_messtab.
      gwa_msg-type     = gwa_messtab-msgtyp.
      gwa_msg-id       = gwa_messtab-msgid.
      gwa_msg-number   = gwa_messtab-msgnr.

      IF gwa_messtab-msgtyp EQ 'S' AND
         gwa_messtab-msgid  = '06' AND
         gwa_messtab-msgnr  = '017'. " Created status
        CONCATENATE gwa_messtab-msgv1
                    gwa_messtab-msgv2
                    'Created'
                    INTO gwa_msg-message SEPARATED BY space.
      ENDIF.

      IF gwa_messtab-msgtyp EQ 'S' AND
         gwa_messtab-msgid  = '06' AND
         gwa_messtab-msgnr  = '023'. " Changed status
        CONCATENATE gwa_messtab-msgv1
                    gwa_messtab-msgv2
                    'Changed'
                    INTO gwa_msg-message SEPARATED BY space.
      ENDIF.
      gwa_msg-message_v1 = gwa_messtab-msgv1.
      gwa_msg-message_v2 = gwa_messtab-msgv2.
      gwa_msg-message_v3 = gwa_messtab-msgv3.
      gwa_msg-message_v4 = gwa_messtab-msgv4.
      APPEND gwa_msg TO git_msg.
      CLEAR:gwa_msg, gwa_messtab.
    ENDLOOP.
    REFRESH git_messtab.
  ENDIF.

**Appending It_msg1  lines to It_msg internal table
  IF git_msg1[] IS NOT INITIAL.
    APPEND LINES OF git_msg1 TO git_msg.
  ENDIF.

**Status Messages
  IF git_msg[] IS NOT INITIAL.
    DELETE git_msg WHERE type EQ 'W'.
    return[] = git_msg[].
  ENDIF.

ENDFUNCTION.
