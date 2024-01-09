*----------------------------------------------------------------------*
*   INCLUDE LMGD1I7O                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*    Close_Sub_Cl                             ch/4.6
* Nachbereitung des Subscreens zur Merkmalsbewertung
*----------------------------------------------------------------------*
MODULE CLOSE_SUB_CL.

  DATA: FLG_INCOMPLETE.

  IF CLO0_IS_OPEN NE SPACE.
    IF  RMMZU-OKCODE NE FCODE_PAGP
    AND RMMZU-OKCODE NE FCODE_PAG1
    AND RMMZU-OKCODE NE FCODE_PAGN
    AND RMMZU-OKCODE NE FCODE_PAGL.

*   Mußmerkmale müssen vorab geprüft werden
      CALL FUNCTION 'CLO0_DDB_CHECK'   "H: 327936
           EXPORTING
                IMP_SIMULATE_FREE = 'X'
           EXCEPTIONS
                INCONSISTENCY     = 1
                INCOMPLETE        = 2
                VERIFICATION      = 3
                NOT_ASSIGNED      = 4
                ANOTHER_OBJECT    = 5
                OTHER_OBJECTS     = 6
                REQU_CHAR_EXCL    = 7
                OTHERS            = 8    .
      IF SY-SUBRC <> 0.
        BILDFLAG = X.
        MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        EXIT.
      ENDIF.

      CALL FUNCTION 'CLO0_DDB_OBJ_VALUATION_CLOSE'
           EXCEPTIONS
                ERROR_AT_TMP_SAVE = 1
                OTHERS            = 2.
      IF SY-SUBRC <> 0.
        MESSAGE E317(MM) WITH 'CLO0_DDB_OBJ_VALUATION_OPEN'.
      ENDIF.
      CLEAR CLO0_IS_OPEN.
*   Prüfen ob eine Änderung stattgefunden hat.
      CALL FUNCTION 'CLO0_DDB_HAS_CHANGES'
*        TABLES
*             OBJECTS               =
           EXCEPTIONS
                INVALID_TABLE_OBJECTS = 1
                NO_CHANGES            = 2
                OTHERS                = 3
                .
      IF SY-SUBRC = 0.                 "d.h. Änderung hat stattgefunden
* note 549774: in der MGMI ist für Subscreens die *RMMG2 aktuell,
* hier wird aber die RMMG2 geholt
* die *RMMG2 wird jeweils aktuell in die MGD1 repliziert
*        CALL FUNCTION 'MAIN_PARAMETER_GET_RMMG2'
*             IMPORTING
*                  WRMMG2 = RMMG2.
*     Im Verbuchungsteil muß erkannt werden, ob lediglich eine
*     Klassizizierung durch den Subscreen stattgefunden hat.
        IF RMMG2-VB_KLAS IS INITIAL.
          RMMG2-VB_KLAS_SUB = X.    "Kennz. wird wieder zurückgesetzt in
                                       "Form Ansprung_Klassenzuo
        ENDIF.
        RMMG2-VB_KLAS = X.
*        CALL FUNCTION 'MAIN_PARAMETER_SET_RMMG2'
*             EXPORTING
*                  WRMMG2 = RMMG2.

      ENDIF.                           "Änderung hat stattgefunden

*     Noch Mußmerkmale prüfen (Vorlage: LMGMWFO0)
*     CLEAR: FLG_INCOMPLETE.
*     CALL FUNCTION 'CTMS_DDB_EXECUTE_FUNCTION'
*          EXPORTING
*               OKCODE           = 'GOON'
*          IMPORTING
*               RAISE_INCOMPLETE = FLG_INCOMPLETE
*          EXCEPTIONS
*               OTHERS           = 1.
*     IF SY-SUBRC NE 0 OR NOT FLG_INCOMPLETE IS INITIAL.
*       CALL FUNCTION 'MAIN_PARAMETER_GET_BILDPAI_SUB'
*            IMPORTING
*                 RMMZU_OKCODE = RMMZU-OKCODE
*                 BILDFLAG     = BILDFLAG
*            EXCEPTIONS
*                 OTHERS       = 1.
*       BILDFLAG = X.
*       CALL FUNCTION 'MAIN_PARAMETER_SET_BILDPAI_SUB'
*            EXPORTING
*                 RMMZU_OKCODE = RMMZU-OKCODE
*                 BILDFLAG     = BILDFLAG
*            EXCEPTIONS
*                 OTHERS       = 1.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*     ENDIF.                           "not flg_incomplete is initial.
    ELSE.                              "d.h. es wurde geblättert
      CLEAR BILDFLAG.
*   Umschlüsseln, falls geblättert wurde
*   (P* usw. ist bereits für Langtexte vergeben)
      CASE RMMZU-OKCODE.
        WHEN FCODE_PAGP.               "P-
          RMMZU-OKCODE = FCODE_CLPP.
        WHEN FCODE_PAG1.               "P--
          RMMZU-OKCODE = FCODE_CLFP.
        WHEN FCODE_PAGN.               "P+
          RMMZU-OKCODE = FCODE_CLNP.
        WHEN FCODE_PAGL.               "P++
          RMMZU-OKCODE = FCODE_CLLP.
      ENDCASE.
    ENDIF.
  ENDIF.

ENDMODULE.
