*&---------------------------------------------------------------------*
*&  Include           ZXM06U47
*&---------------------------------------------------------------------*
*&Changes:
*& 2011/07/20 - gymana - TR804 COG
*&              implemented user exit to use document date for instead
*&              of posting date for COG goods movements
*&---------------------------------------------------------------------*

* Use Document Date (BLDAT) instead of Posting Date.
IF i_ekpo-matnr = 'NATGAS'.
  MOVE i_bldat TO c_datum.
ENDIF.
