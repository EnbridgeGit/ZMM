FUNCTION zmm_schdagr_change.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_EBELN) TYPE  EKKO-EBELN
*"----------------------------------------------------------------------
  INCLUDE <cntn01>.

  DATA: lv_answer.

  gv_ebeln = iv_ebeln.

break sahmad.
  SET PARAMETER ID 'SAG' FIELD gv_ebeln.
  CALL TRANSACTION 'ME32L' AND SKIP FIRST SCREEN.

BREAK sahmad.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
     titlebar                    = 'Schedule Agreement Release for Approval '
*   DIAGNOSE_OBJECT             = ' '
     text_question               = 'Is Schedule Agreement ready to Release for Approval?'
     text_button_1               = 'Yes'
*   ICON_BUTTON_1               = ' '
     text_button_2               = 'No'
*   ICON_BUTTON_2               = ' '
*   DEFAULT_BUTTON              = '2'
     display_cancel_button       = ' '
*   USERDEFINED_F1_HELP         = ' '
*   START_COLUMN                = 25
*   START_ROW                   = 6
*   POPUP_TYPE                  =
*   IV_QUICKINFO_BUTTON_1       = ' '
*   IV_QUICKINFO_BUTTON_2       = ' '
   IMPORTING
     answer                      = lv_answer
* TABLES
*   PARAMETER                   =
   EXCEPTIONS
     text_not_found              = 1
     OTHERS                      = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  IF lv_answer = '1'.
    PERFORM raise_event USING 'ZReleaseForApproval'.
    WAIT UP TO 3 SECONDS.
  ENDIF.

ENDFUNCTION.
