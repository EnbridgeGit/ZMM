FUNCTION ZUPLOAD_OLA_UG .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      OLA_DATA STRUCTURE  ZCONTRACT_OLAID
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: it_ola    TYPE TABLE OF zcontract_olaid,
        it_return TYPE TABLE OF bapiret2,
        wa_return TYPE bapiret2.

  it_ola[] = ola_data[].
  IF it_ola[] IS NOT INITIAL.
    MODIFY zcontract_olaid FROM TABLE it_ola.
    IF sy-subrc EQ 0.  " Loaded data successfully
      COMMIT WORK AND WAIT.
      wa_return-type   = 'S'.
      wa_return-id    = '06'.
      wa_return-message_v1 = 'Success'.
      wa_return-message = 'Records Successfully uploaded'.
      APPEND wa_return TO it_return.
    ELSE.
      wa_return-type   = 'E'.
      wa_return-id    = '06'.
      wa_return-message_v1 = 'Failed'.
      wa_return-message   = 'Failed Upload'.
      APPEND wa_return TO it_return.
    ENDIF.
  ENDIF.
  return[] = it_return[].

ENDFUNCTION.
