*----------------------------------------------------------------------*
***INCLUDE LZCOGF01 .
*----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
*&      Form  FILL_BAPIRETURN2
*&---------------------------------------------------------------------*
*        Fill return table
*----------------------------------------------------------------------*
FORM FILL_BAPIRETURN2 TABLES I_RETURN STRUCTURE BAPIRET2
                      USING  I_MSTYP I_MSGID I_MSGNO
                             I_MSGV1 I_MSGV2 I_MSGV3 I_MSGV4.

DATA: BAPIRETURN1 LIKE BAPIRETURN1.

* Convert type
CLEAR MSG.
MOVE: I_MSTYP TO MSG-TY,
      I_MSGID TO MSG-ID,
      I_MSGNO TO MSG-NO,
      I_MSGV1 TO MSG-V1,
      I_MSGV2 TO MSG-V2,
      I_MSGV3 TO MSG-V3,
      I_MSGV4 TO MSG-V4.

* Fill return
CALL FUNCTION 'BALW_BAPIRETURN_GET1'
     EXPORTING
          TYPE       = MSG-TY
          CL         = MSG-ID
          NUMBER     = MSG-NO
          PAR1       = MSG-V1
          PAR2       = MSG-V2
          PAR3       = MSG-V3
          PAR4       = MSG-V4
*         LOG_NO     = ' '
*         LOG_MSG_NO = ' '
     IMPORTING
          BAPIRETURN = BAPIRETURN1
     EXCEPTIONS
          OTHERS     = 0.

CLEAR I_RETURN.
MOVE-CORRESPONDING BAPIRETURN1 TO I_RETURN.
APPEND I_RETURN.

ENDFORM.                    " FILL_BAPIRETURN
