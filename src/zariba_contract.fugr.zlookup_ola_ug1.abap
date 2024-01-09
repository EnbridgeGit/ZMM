FUNCTION zlookup_ola_ug1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OLA_ID) TYPE  EBELN OPTIONAL
*"     VALUE(CONTRACT_ID) TYPE  KONNR OPTIONAL
*"  EXPORTING
*"     VALUE(STATUS_FLAG) TYPE  CHAR5
*"----------------------------------------------------------------------

  DATA: lv_ola      TYPE ebeln,
        lv_contract TYPE konnr,
        lv_rfcid    TYPE ebeln.

**  Check for OLA ID
  IF ola_id IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ola_id
      IMPORTING
        output = lv_rfcid.
    SELECT SINGLE ola_id FROM zcontract_olaid INTO lv_ola WHERE ola_id EQ lv_rfcid.
    IF sy-subrc EQ 0.
      status_flag = 'X'. "OLA exist in table
    ELSE.
      status_flag = ''.  "OLA not exist in table
    ENDIF.
  ENDIF.

**  Check for Contract ID
  IF contract_id IS NOT INITIAL.
    SELECT SINGLE contract_id FROM zcontract_olaid INTO lv_contract WHERE contract_id EQ contract_id.
    IF sy-subrc EQ 0.
      status_flag = 'FALSE'. "Contract exist in table
    ELSE.
      status_flag = 'TRUE'.  "Contract not exist in table
    ENDIF.
  ENDIF.

ENDFUNCTION.
