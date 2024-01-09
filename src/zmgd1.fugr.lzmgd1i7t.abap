*----------------------------------------------------------------------*
***INCLUDE LMGD1I7T .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MARA-CMREL  INPUT
*&---------------------------------------------------------------------*
* TF 4.6C Materialfixierung
*----------------------------------------------------------------------*
MODULE mara-cmrel INPUT.
  CALL FUNCTION 'MARA_CMREL'
    EXPORTING
      if_wmara                = lmara
      if_lmara                = mara
    EXCEPTIONS
      CHANGE_NOT_ALLOWED      = 1
      OTHERS                  = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    bildflag = 'X'.
  ENDIF.

ENDMODULE.                 " MARA-CMREL  INPUT
ENHANCEMENT-POINT LMGD1I7T_01 SPOTS ES_LMGD1I7T STATIC INCLUDE BOUND.
