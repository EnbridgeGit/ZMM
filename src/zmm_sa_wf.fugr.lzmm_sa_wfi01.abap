*----------------------------------------------------------------------*
***INCLUDE LZMM_SA_WFI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  break sahmad.
gv_okcode = ok_code.
clear ok_code.
IF gv_okcode = 'REJECT'
   AND gv_comments is INITIAL.
   MESSAGE e017(zfi01) WITH 'Comments are required at REJECT Release'.
ENDIF.
CASE gv_okcode.
  WHEN 'REJECT'.
       PERFORM reject_SA.
  WHEN 'RELEASE'.
       clear gv_comments.
       PERFORM release_SA.
  WHEN 'DISP'.
       PERFORM display_sa.
  WHEN 'CANCEL' or 'BACK' or
       'EXIT' or 'KEEPIT'.
     clear: gv_decision,
            gv_comments.
     PERFORM set_screen.
  WHEN OTHERS.
*     PERFORM set_screen.
ENDCASE.

ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_COMMENTS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validate_comments INPUT.

break sahmad.
  IF gv_okcode = 'REJECT' AND
     gv_comments IS INITIAL.
    MESSAGE e017(zfi01) WITH 'Comments are required at REJECT Release'.
  ENDIF.

ENDMODULE.                 " VALIDATE_COMMENTS  INPUT
