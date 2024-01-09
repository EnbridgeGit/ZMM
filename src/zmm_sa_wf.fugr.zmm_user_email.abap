FUNCTION zmm_user_email.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(USERID) TYPE  WFSYST-INITIATOR
*"  EXPORTING
*"     VALUE(USER_EMAIL) TYPE  ADR6-SMTP_ADDR
*"  EXCEPTIONS
*"      NO_EMAIL
*"----------------------------------------------------------------------

 INCLUDE <cntn01>.

  DATA: ls_usr21 TYPE usr21,
        lv_bname type usr21-bname.

  lv_bname = userid+2(12).
  SELECT SINGLE * FROM usr21 INTO ls_usr21
    WHERE bname = lv_bname.

  IF sy-subrc = 0.
    SELECT SINGLE smtp_addr FROM adr6 INTO user_email
      WHERE addrnumber = ls_usr21-addrnumber
        AND persnumber = ls_usr21-persnumber
        AND smtp_addr <> space.
  ENDIF.
IF user_email is INITIAL.
   raise no_email.
ENDIF.

ENDFUNCTION.
