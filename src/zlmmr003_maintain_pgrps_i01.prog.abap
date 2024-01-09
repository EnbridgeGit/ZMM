*&---------------------------------------------------------------------*
*&  Include           ZLMMR003_MAINTAIN_PGRPS_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE tp_okcode.
    WHEN 'BACK' OR 'EXIT'.
      IF tp_msg = 'X'.
        PERFORM display_msg.
      ELSE.
        CALL FUNCTION 'DEQUEUE_E_TABLE'
          EXPORTING
            mode_rstable = 'E'
            tabname      = 'T024'.
        REFRESH CONTROL 'TC_PGRPS' FROM SCREEN '0100'.
        CLEAR: tp_okcode,
               tp_lock,
               tp_msg.
        LEAVE PROGRAM.
      ENDIF.
    WHEN 'CANCEL'.
      tp_okcode = 'CHNG'.
    WHEN 'DISP'.
      tp_disp = 'X'.
      CLEAR: tp_chng.
    WHEN 'CHNG'.
      IF tp_lock IS INITIAL.
        CALL FUNCTION 'ENQUEUE_E_TABLE'
          EXPORTING
            mode_rstable = 'E'
            tabname      = 'T024'.
        tp_lock = 'X'.
      ENDIF.
      tp_chng = 'X'.
      CLEAR: tp_disp.
    WHEN 'NENTRY'.
      REFRESH: ta_pgrps,
               ta_pgrps_tmp.
      CLEAR: tp_okcode,
             tp_msg.
      CALL SCREEN '0101'.
    WHEN 'COPY'.
      IF ta_cpy_pgrps IS INITIAL.
        MESSAGE s023(zmm_message) WITH 'Select entries before performing the function'(011).
      ELSE.
        CLEAR: tp_msg.
        LOOP AT ta_cpy_pgrps INTO wa_cpy_pgrps WHERE zmm_sel = 'X'.
          DELETE ta_t024_tmp WHERE ekgrp = wa_cpy_pgrps-ekgrp.
        ENDLOOP.
        DELETE ta_pgrps_tmp WHERE zmm_sel = 'X'.
        CALL SCREEN '0102'.
      ENDIF.
    WHEN 'DELE'.
      PERFORM delete_data.
      CLEAR tp_msg.
    WHEN 'SAVE'.
      PERFORM save_data.
      CLEAR tp_msg.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DATA_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data_0100 INPUT.
  IF wa_pgrps NE zmms_maintain_pgrps.
    wa_pgrps = zmms_maintain_pgrps.
    MODIFY ta_pgrps FROM wa_pgrps INDEX tc_pgrps-current_line.
    APPEND wa_pgrps TO ta_pgrps_tmp.

    wa_t024_tmp-ekgrp = wa_pgrps-ekgrp.
    wa_t024_tmp-eknam = wa_pgrps-eknam.
    wa_t024_tmp-ektel = wa_pgrps-ektel.
    wa_t024_tmp-telfx = wa_pgrps-telfx.
    wa_t024_tmp-smtp_addr = wa_pgrps-smtp_addr.
    APPEND wa_t024_tmp TO ta_t024_tmp.
    CLEAR: wa_t024_tmp,
           wa_pgrps.

    tp_msg = 'X'.
  ENDIF.
  IF tp_okcode = 'COPY'.
    IF zmms_maintain_pgrps-zmm_sel = 'X'.
      wa_cpy_pgrps = zmms_maintain_pgrps.
      APPEND wa_cpy_pgrps TO ta_cpy_pgrps.
      CLEAR wa_cpy_pgrps.
    ENDIF.
  ENDIF.
  IF tp_okcode = 'DELE'.
    IF zmms_maintain_pgrps-zmm_sel = 'X'.
      wa_cpy_pgrps = zmms_maintain_pgrps.

      wa_t024_tmp-ekgrp = wa_cpy_pgrps-ekgrp.
      wa_t024_tmp-eknam = wa_cpy_pgrps-eknam.
      wa_t024_tmp-ektel = wa_cpy_pgrps-ektel.
      wa_t024_tmp-telfx = wa_cpy_pgrps-telfx.
      wa_t024_tmp-smtp_addr = wa_cpy_pgrps-smtp_addr.
      APPEND wa_t024_tmp TO ta_del_t024.
      CLEAR: wa_cpy_pgrps,
             wa_t024_tmp.
    ENDIF.
  ENDIF.

ENDMODULE.                 " MODIFY_DATA_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE tp_new_okcode.
    WHEN 'BACK'.
      tp_okcode = tp_new_okcode.
      CLEAR: tp_new_okcode.
      LEAVE TO SCREEN 100.
    WHEN 'EXIT'.
      REFRESH: ta_new_pgrps.
      tp_okcode = tp_new_okcode.
      CLEAR: tp_new_okcode.
      LEAVE TO SCREEN 100.
    WHEN 'SAVE'.
      CLEAR: tp_new_okcode.
      PERFORM save_data.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DATA_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data_0101 INPUT.
  IF zmms_maintain_pgrps IS NOT INITIAL.
    wa_new_pgrps = zmms_maintain_pgrps.
    APPEND wa_new_pgrps TO ta_new_pgrps.

    wa_t024_tmp-ekgrp = wa_new_pgrps-ekgrp.
    wa_t024_tmp-eknam = wa_new_pgrps-eknam.
    wa_t024_tmp-ektel = wa_new_pgrps-ektel.
    wa_t024_tmp-telfx = wa_new_pgrps-telfx.
    wa_t024_tmp-smtp_addr = wa_new_pgrps-smtp_addr.
    APPEND wa_t024_tmp TO ta_t024_tmp.
    CLEAR: wa_t024_tmp,
           wa_new_pgrps.
  ENDIF.

ENDMODULE.                 " MODIFY_DATA_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_EKGRP_0101  INPUT
*&---------------------------------------------------------------------*
MODULE validate_ekgrp_0101 INPUT.
  IF tp_new_okcode NE 'CANCEL' OR tp_new_okcode NE 'EXIT'.
    CLEAR tp_ekgrp.
    SELECT SINGLE ekgrp
           FROM t024
           INTO tp_ekgrp
           WHERE ekgrp = zmms_maintain_pgrps-ekgrp.
    IF sy-subrc = 0.
      MOVE zmms_maintain_pgrps TO wa_err_pgrps.
      APPEND wa_err_pgrps TO ta_err_pgrps.
      CLEAR wa_err_pgrps.
    ENDIF.
  ENDIF.

ENDMODULE.                 " VALIDATE_EKGRP_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  CASE tp_cpy_okcode.
    WHEN 'CANCEL'.
      CLEAR: tp_cpy_okcode.
      IF tp_modify EQ 'X'.
        IF tp_error = 'X'.
          PERFORM display_msg_cpy.
          CLEAR tp_error.
        ELSE.
          PERFORM save_data_cpy.
          CLEAR: tp_cpy_okcode,
                 tp_modify,
                 tp_error.
          REFRESH: ta_cpy_pgrps.
          LEAVE TO SCREEN 0.
        ENDIF.
      ELSE.
        REFRESH: ta_cpy_pgrps.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN OTHERS.
      PERFORM save_data_cpy.
      CLEAR: tp_cpy_okcode,
             tp_modify.
      REFRESH: ta_cpy_pgrps.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DATA_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data_0102 INPUT.
  IF wa_cpy_pgrps NE zmms_maintain_pgrps.
    wa_cpy_pgrps = zmms_maintain_pgrps.
    wa_cpy_pgrps-zmm_sel = ' '.
    MODIFY ta_cpy_pgrps FROM wa_cpy_pgrps INDEX tc_pgrps_cpy-current_line.
    CLEAR: wa_cpy_pgrps.
    tp_modify = 'X'.
  ENDIF.

ENDMODULE.                 " MODIFY_DATA_0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_EKGRP_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validate_ekgrp_0102 INPUT.

  CLEAR tp_ekgrp.

  SELECT SINGLE ekgrp
           FROM t024
           INTO tp_ekgrp
           WHERE ekgrp = zmms_maintain_pgrps-ekgrp.
  IF sy-subrc = 0.
    IF tp_cpy_okcode EQ 'CANCEL'.
      tp_error = 'X'.
    ELSE.
      MESSAGE e023(zmm_message) WITH 'Target key must be different from source key.'(001).
    ENDIF.
  ENDIF.

ENDMODULE.                 " VALIDATE_EKGRP_0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  REFRESH_DATA_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_data_0100 INPUT.
  IF ( sy-ucomm EQ tp_okcode ).
    REFRESH: ta_t024_tmp,
             ta_t160ex_tmp,
             ta_pgrps_tmp,
             ta_cpy_pgrps,
             ta_del_t024.
  ENDIF.

ENDMODULE.                 " REFRESH_DATA_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.
  CASE tp_err_okcode.
    WHEN 'CANCEL'.
      CLEAR tp_answer.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Cancel Maintenance'(005)
          text_question         = 'Your changes will be lost. Cancel?'(006)
          text_button_1         = 'Yes'(007)
          text_button_2         = 'No'(008)
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = tp_answer.
      IF tp_answer = '1'.
        REFRESH ta_err_pgrps.
        CLEAR tp_err_okcode.
        LEAVE TO SCREEN '0101'.
      ELSE.
        CLEAR tp_err_okcode.
      ENDIF.
    WHEN OTHERS.
      REFRESH ta_err_pgrps.
      CLEAR tp_err_okcode.
      LEAVE TO SCREEN '0101'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  ERROR_SCREEN_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE error_screen_0101 INPUT.
  IF ta_err_pgrps IS NOT INITIAL.
    LOOP AT ta_err_pgrps INTO wa_err_pgrps.
      DELETE TABLE ta_new_pgrps FROM wa_err_pgrps.
    ENDLOOP.
    CALL SCREEN '0103'.
  ENDIF.

ENDMODULE.                 " ERROR_SCREEN_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DATA_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data_0103 INPUT.
  IF tp_err_okcode NE 'CANCEL'.
    wa_err_pgrps = zmms_maintain_pgrps.
    APPEND wa_err_pgrps TO ta_new_pgrps.
    CLEAR wa_err_pgrps.
  ENDIF.

ENDMODULE.                 " MODIFY_DATA_0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_EKGRP_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validate_ekgrp_0103 INPUT.
  IF tp_err_okcode NE 'CANCEL'.
    CLEAR tp_ekgrp.
    SELECT SINGLE ekgrp
               FROM t024
               INTO tp_ekgrp
               WHERE ekgrp = zmms_maintain_pgrps-ekgrp.
    IF sy-subrc = 0.
      MESSAGE e023(zmm_message) WITH 'An entry already exists with the same key.'(002).
    ENDIF.
  ENDIF.

ENDMODULE.                 " VALIDATE_EKGRP_0103  INPUT
