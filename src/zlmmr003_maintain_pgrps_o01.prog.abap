*&---------------------------------------------------------------------*
*&  Include           ZLMMR003_MAINTAIN_PGRPS_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF tp_chng = ' ' OR tp_disp = 'X'.
    SET PF-STATUS 'ZPGRPS_DIS_100'.
    SET TITLEBAR 'ZPGPRS_DIS_TITLE'.
  ELSEIF tp_chng = 'X' OR tp_disp = ' '.
    SET TITLEBAR 'ZPGPRS_TITLE'.
    AUTHORITY-CHECK OBJECT 'Z_PGRP'
    ID 'ACTVT' FIELD '06'.
    IF sy-subrc = 0.
      SET PF-STATUS 'ZPGRPS_100'.
    ELSE.
      SET PF-STATUS 'ZPGRPS_CHG_100'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  POPULATE_SCREEN_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE populate_screen_0100 OUTPUT.

  READ TABLE ta_pgrps INTO zmms_maintain_pgrps INDEX tc_pgrps-current_line.

  IF tp_chng = ' ' OR tp_disp = 'X'.
    LOOP AT SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " POPULATE_SCREEN_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_data_0100 OUTPUT.
  PERFORM get_data.

ENDMODULE.                 " GET_DATA_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'ZPGRPS_101'.
  SET TITLEBAR 'ZPGPRS_NEW_TITLE'.

ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_data_0101 OUTPUT.
  READ TABLE ta_new_pgrps INTO zmms_maintain_pgrps INDEX tc_pgrps_new-current_line.

ENDMODULE.                 " GET_DATA_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  REFRESH_DATA_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_data_0101 OUTPUT.
  REFRESH: ta_new_pgrps,
           ta_pgrps_tmp,
           ta_t024_tmp,
           ta_t160ex_tmp.
  CLEAR tp_new_okcode.

ENDMODULE.                 " REFRESH_DATA_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CLEAR_DATA_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_data_0100 OUTPUT.
  IF tp_okcode = 'COPY' OR tp_okcode = 'DELE' OR tp_okcode = 'SAVE' OR
     tp_okcode = 'BACK' OR tp_okcode = 'EXIT' OR tp_okcode = 'CANCEL'.
    CLEAR: tp_okcode.
    tp_okcode = 'CHNG'.
  ENDIF.

ENDMODULE.                 " CLEAR_DATA_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.
  SET PF-STATUS 'ZPGRPS_102'.
  SET TITLEBAR 'ZPGPRS_CPY_TITLE'.
  REFRESH CONTROL 'TC_PGRPS_CPY' FROM SCREEN '0102'.

ENDMODULE.                 " STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  POPULATE_SCREEN_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE populate_screen_0102 OUTPUT.
  READ TABLE ta_cpy_pgrps INTO zmms_maintain_pgrps INDEX tc_pgrps_cpy-current_line.

ENDMODULE.                 " POPULATE_SCREEN_0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  POPULATE_SCREEN_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE populate_screen_0103 OUTPUT.
  READ TABLE ta_err_pgrps INTO zmms_maintain_pgrps INDEX tc_pgrps_nerr-current_line.

ENDMODULE.                 " POPULATE_SCREEN_0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.
  SET PF-STATUS 'ZPGRPS_103'.
  SET TITLEBAR 'ZPGPRS_NEW_TITLE'.

ENDMODULE.                 " STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  POPULATE_ERROR_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE populate_error_0103 OUTPUT.
  MESSAGE s023(zmm_message) WITH 'An entry already exists with the same key.'(002) DISPLAY LIKE 'E'.

ENDMODULE.                 " POPULATE_ERROR_0103  OUTPUT
