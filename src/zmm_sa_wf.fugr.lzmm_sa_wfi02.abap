*----------------------------------------------------------------------*
***INCLUDE LZMM_SA_WFI02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9002 INPUT.
break sahmad.
gv_okcode = ok_code.
clear ok_code.
CASE gv_okcode.
  WHEN 'ME38'.
       PERFORM ME38_SA.
  WHEN 'DISP'.
       PERFORM display_sa.
  WHEN 'CANCEL' or 'BACK' or
       'EXIT' or 'KEEPIT'.
     clear: gv_decision.
     PERFORM set_screen.
  WHEN OTHERS.
*     PERFORM set_screen.
ENDCASE.
ENDMODULE.                 " USER_COMMAND_9002  INPUT
