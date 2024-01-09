*&---------------------------------------------------------------------*
*&  Include           ZLMMR003_MAINTAIN_PGRPS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data .
  IF sy-ucomm EQ tp_okcode.
    REFRESH ta_pgrps.
    SELECT ekgrp
             eknam
             ektel
             telfx
             smtp_addr
             FROM t024
             INTO TABLE ta_t024.
    IF sy-subrc = 0.
      SORT ta_t024 BY ekgrp.
      DELETE ta_t024 WHERE ekgrp = space.
    ENDIF.
    IF ta_t024[] IS NOT INITIAL.
      SELECT ekgrp
             FROM t160ex
             INTO TABLE ta_t160ex
             FOR ALL ENTRIES IN ta_t024
             WHERE ekgrp = ta_t024-ekgrp.
      IF sy-subrc = 0.
        SORT ta_t160ex BY ekgrp.
      ENDIF.
    ENDIF.
    LOOP AT ta_t024 INTO wa_t024.
      wa_pgrps-ekgrp = wa_t024-ekgrp.
      wa_pgrps-eknam = wa_t024-eknam.
      wa_pgrps-ektel = wa_t024-ektel.
      wa_pgrps-telfx = wa_t024-telfx.
      wa_pgrps-smtp_addr = wa_t024-smtp_addr.
      READ TABLE ta_t160ex INTO wa_t160ex WITH KEY ekgrp = wa_t024-ekgrp.
      IF sy-subrc = 0.
        wa_pgrps-zmm_srm = 'X'.
      ELSE.
        wa_pgrps-zmm_srm = ' '.
      ENDIF.
      APPEND wa_pgrps TO ta_pgrps.
      CLEAR: wa_pgrps,
             wa_t024.
    ENDLOOP.
  ENDIF.

  IF ta_new_pgrps IS NOT INITIAL.
    APPEND LINES OF ta_new_pgrps TO ta_pgrps.
    APPEND LINES OF ta_new_pgrps TO ta_pgrps_tmp.
    REFRESH ta_new_pgrps.
    tp_msg = 'X'.
  ENDIF.
  SORT ta_pgrps BY ekgrp.
  DESCRIBE TABLE ta_pgrps LINES tc_pgrps-lines.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_msg .
  CLEAR tp_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Exit maintenance'(009)
      text_question         = 'Data was changed. Save changes first?'(010)
      text_button_1         = 'Yes'(007)
      text_button_2         = 'No'(008)
      default_button        = '1'
      display_cancel_button = 'X'
    IMPORTING
      answer                = tp_answer.
  IF tp_answer = '1'.
    CLEAR tp_msg.
    PERFORM save_data.
  ELSEIF tp_answer = '2'.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'T024'.
    CLEAR: tp_lock,
           tp_msg.
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.                    " DISPLAY_MSG
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_data .
  IF ta_t024_tmp[] IS NOT INITIAL.
    MODIFY t024 FROM TABLE ta_t024_tmp.
    IF sy-subrc = 0.
      COMMIT WORK.
      ta_pgrps_del[] = ta_pgrps_tmp[].
      ta_pgrps_ins[] = ta_pgrps_tmp[].
      DELETE ta_pgrps_ins WHERE zmm_srm = ' '.
      DELETE ta_pgrps_del WHERE zmm_srm = 'X'.
      IF ta_pgrps_ins IS NOT INITIAL.
        LOOP AT ta_pgrps_ins INTO wa_pgrps_ins.
          wa_t160ex_tmp-mandt = sy-mandt.
          wa_t160ex_tmp-matkl = '*'.
          wa_t160ex_tmp-ekgrp = wa_pgrps_ins-ekgrp.
          wa_t160ex_tmp-eprofile = '01'.
          wa_t160ex_tmp-manual = ' '.
          APPEND wa_t160ex_tmp TO ta_t160ex_tmp.
          CLEAR wa_t160ex_tmp.
        ENDLOOP.
        MODIFY t160ex FROM TABLE ta_t160ex_tmp.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
        REFRESH: ta_t160ex_tmp,
                 ta_pgrps_ins.
      ENDIF.
      IF ta_pgrps_del IS NOT INITIAL.
        LOOP AT ta_pgrps_del INTO wa_pgrps_del.
          wa_t160ex_tmp-mandt = sy-mandt.
          wa_t160ex_tmp-matkl = '*'.
          wa_t160ex_tmp-ekgrp = wa_pgrps_del-ekgrp.
          wa_t160ex_tmp-eprofile = '01'.
          wa_t160ex_tmp-manual = ' '.
          APPEND wa_t160ex_tmp TO ta_t160ex_tmp.
          CLEAR wa_t160ex_tmp.
        ENDLOOP.
        DELETE t160ex FROM TABLE ta_t160ex_tmp.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
        REFRESH: ta_t160ex_tmp,
                 ta_pgrps_del.
      ENDIF.
      REFRESH ta_t024_tmp.
      MESSAGE s023(zmm_message) WITH 'Data saved.'(003).
    ENDIF.
  ELSE.
    MESSAGE s023(zmm_message) WITH 'Data already saved.'(004).
  ENDIF.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_data .
  IF ta_del_t024 IS NOT INITIAL.
    DELETE t024 FROM TABLE ta_del_t024.
    IF sy-subrc = 0.
      COMMIT WORK.
      DELETE ta_pgrps_tmp WHERE zmm_srm = ' '.
      IF ta_pgrps_tmp IS NOT INITIAL.
        LOOP AT ta_pgrps_tmp INTO wa_pgrps_tmp.
          wa_t160ex_tmp-mandt = sy-mandt.
          wa_t160ex_tmp-matkl = '*'.
          wa_t160ex_tmp-ekgrp = wa_pgrps_tmp-ekgrp.
          wa_t160ex_tmp-eprofile = '01'.
          wa_t160ex_tmp-manual = ' '.
          APPEND wa_t160ex_tmp TO ta_t160ex_tmp.
          CLEAR wa_t160ex_tmp.
        ENDLOOP.
        DELETE t160ex FROM TABLE ta_t160ex_tmp.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
        REFRESH ta_t160ex_tmp.
      ENDIF.
      REFRESH ta_del_t024.
    ENDIF.
    REFRESH ta_del_t024.
  ENDIF.

ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MSG_CPY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_msg_cpy .
  FIELD-SYMBOLS: <fs_pgrps> TYPE zmms_maintain_pgrps.

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
    REFRESH: ta_cpy_pgrps.
    CLEAR tp_modify.
    LOOP AT ta_pgrps ASSIGNING <fs_pgrps> WHERE zmm_sel = 'X'.
      CLEAR <fs_pgrps>-zmm_sel.
    ENDLOOP.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " DISPLAY_MSG_CPY
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_CPY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_data_cpy .
  FIELD-SYMBOLS: <fs_pgrps> TYPE zmms_maintain_pgrps.

  LOOP AT ta_pgrps ASSIGNING <fs_pgrps> WHERE zmm_sel = 'X'.
    CLEAR <fs_pgrps>-zmm_sel.
  ENDLOOP.
  APPEND LINES OF ta_cpy_pgrps TO ta_pgrps.
  SORT ta_pgrps BY ekgrp.
  APPEND LINES OF ta_cpy_pgrps TO ta_pgrps_tmp.
  LOOP AT ta_cpy_pgrps INTO wa_cpy_pgrps.
    wa_t024_tmp-ekgrp = wa_cpy_pgrps-ekgrp.
    wa_t024_tmp-eknam = wa_cpy_pgrps-eknam.
    wa_t024_tmp-ektel = wa_cpy_pgrps-ektel.
    wa_t024_tmp-telfx = wa_cpy_pgrps-telfx.
    wa_t024_tmp-smtp_addr = wa_cpy_pgrps-smtp_addr.
    APPEND wa_t024_tmp TO ta_t024_tmp.
    CLEAR wa_t024_tmp.
  ENDLOOP.

ENDFORM.                    " SAVE_DATA_CPY
