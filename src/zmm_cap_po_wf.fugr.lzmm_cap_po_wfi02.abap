*----------------------------------------------------------------------*
***INCLUDE LZMM_CAP_PO_WFI02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9002 INPUT.

  break sahmad.
  gv_okcode = ok_code.
  CLEAR ok_code.
  CASE gv_okcode.
    WHEN 'ME22N'.
      PERFORM me22n_po.
    WHEN 'DISP'.
      PERFORM display_po.
    WHEN 'SNDREL'.
      "clear gv_comments.
      PERFORM send_for_release_po.
      PERFORM set_screen.
    WHEN 'SETDEL'.
      PERFORM set_for_delete_po.
    WHEN 'CANCEL' OR 'BACK' OR
          'EXIT' OR 'KEEPIT'.
      CLEAR: gv_decision.
      PERFORM set_screen.
    WHEN OTHERS.
*     PERFORM set_screen.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9003 INPUT.

  DATA: lv_answer(1),
        lv_dncmp type xfeld.

  break sahmad.
  gv_okcode = ok_code.
  CLEAR ok_code.
  CASE gv_okcode.
    WHEN 'DISP'.
      PERFORM display_po.
    WHEN 'ME2DP'.
      "clear gv_comments.
      PERFORM me2dp_po.
    WHEN 'DNCMP'.
      PERFORM get_downpay_info CHANGING lv_dncmp.
      if lv_dncmp is INITIAL.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
*           TITLEBAR                    = ' '
*           DIAGNOSE_OBJECT             = ' '
            text_question               = 'Purchase Order has NO down-payment, Do you want to proceed?'
*           TEXT_BUTTON_1               = 'Ja'(001)
*           ICON_BUTTON_1               = ' '
*           TEXT_BUTTON_2               = 'Nein'(002)
*           ICON_BUTTON_2               = ' '
             default_button              = '2'
             display_cancel_button       = ' '
*           USERDEFINED_F1_HELP         = ' '
*           START_COLUMN                = 25
*           START_ROW                   = 6
*           POPUP_TYPE                  =
*           IV_QUICKINFO_BUTTON_1       = ' '
*           IV_QUICKINFO_BUTTON_2       = ' '
           IMPORTING
             answer                      = lv_answer
*         TABLES
*           PARAMETER                   =
           EXCEPTIONS
             text_not_found              = 1
             OTHERS                      = 2 .
          IF sy-subrc <> 0.
*     Implement suitable error handling here
          ENDIF.
          IF lv_answer = '1'.
            PERFORM raise_event USING 'ZDownPayCompleted'.
            PERFORM set_screen.
          ENDIF.
      ELSE.
          PERFORM raise_event USING 'ZDownPayCompleted'.
          PERFORM set_screen.
      ENDIF.
    WHEN 'CANCEL' OR 'BACK' OR
        'EXIT' OR 'KEEPIT'.
      CLEAR: gv_decision.
      PERFORM set_screen.
    WHEN OTHERS.
*     PERFORM set_screen.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9003  INPUT
