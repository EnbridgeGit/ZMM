FUNCTION Z_BBP_ENTRYSHEET_CANCEL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ENTRYSHEET) TYPE  BAPIESSRC-SHEET_NO
*"     VALUE(POSTING_DATE) TYPE  SY-DATUM OPTIONAL
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
   DATA: lt_essr LIKE essr OCCURS 1 WITH HEADER LINE,
         lt_message_tab LIKE eessr OCCURS 0 WITH HEADER LINE,
         lv_transaction_id LIKE arfctid.

  CLEAR:   return, lt_essr, lt_message_tab.
  REFRESH: return, lt_essr, lt_message_tab.

* get transaction ID
  CLEAR lv_transaction_id.
  CALL FUNCTION 'TRANSACTION_BEGIN'
    IMPORTING
      transaction_id = lv_transaction_id
    EXCEPTIONS
      OTHERS         = 1.

* Delete entry sheet
  lt_essr-lblni = entrysheet.
  APPEND lt_essr.

  CALL FUNCTION 'MS_DELETE_SERVICE_ENTRY_MULTI'
    EXPORTING
      i_authority_check   = 'X'
      i_revoke_acceptance = 'X'
      i_budatum           = posting_date "lv_datum
    TABLES
      t_essr              = lt_essr
      t_messr             = lt_message_tab.

* Fill return table
  LOOP AT lt_message_tab.
    PERFORM fill_bapireturn2 TABLES return
                             USING  lt_message_tab-msgty
                                    lt_message_tab-msgid
                                    lt_message_tab-msgno
                                    lt_message_tab-msgv1
                                    lt_message_tab-msgv2
                                    lt_message_tab-msgv3
                                    lt_message_tab-msgv4.
  ENDLOOP.

* Commit work
  LOOP AT return WHERE type = 'E'.
    EXIT.
  ENDLOOP.
  IF sy-subrc <> 0.
    CALL FUNCTION 'TRANSACTION_END'
      EXPORTING
        transaction_id = lv_transaction_id
      EXCEPTIONS
        OTHERS         = 1.
  ELSE.
    CALL FUNCTION 'TRANSACTION_ABORT'
      EXPORTING
        transaction_id = lv_transaction_id
      EXCEPTIONS
        OTHERS         = 1.
  ENDIF.

ENDFUNCTION.
