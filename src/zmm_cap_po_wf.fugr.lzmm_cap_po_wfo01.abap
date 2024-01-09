*----------------------------------------------------------------------*
***INCLUDE LZMM_CAP_PO_WFO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
 SET PF-STATUS 'STATUS_01'.
  SET TITLEBAR 'T01'.
ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  data: lv_text TYPE CHAR0256,
        lv_error TYPE xfeld.

break sahmad.
gv_okcode = ok_code.
clear ok_code.
IF gv_okcode = 'REJECT'
   AND gv_comments is INITIAL.
   MESSAGE e017(zfi01) WITH 'Comments are required at REJECT Release'.
ENDIF.
CASE gv_okcode.
  WHEN 'REJECT'.
       PERFORM reject_PO.
       PERFORM set_screen.
  WHEN 'RELEASE'.
       clear gv_comments.
       PERFORM release_PO USING lv_error
                                lv_text.
       if lv_error is INITIAL.
          PERFORM set_screen.
       endif.
  WHEN 'DISP'.
       PERFORM display_PO.
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
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9002 OUTPUT.
  SET PF-STATUS 'STATUS_01'.
  SET TITLEBAR 'T02'.

ENDMODULE.                 " STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9003 OUTPUT.

  SET PF-STATUS 'STATUS_01'.
  SET TITLEBAR 'T03'.


ENDMODULE.                 " STATUS_9003  OUTPUT
