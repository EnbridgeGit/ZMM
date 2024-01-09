* 2000/07/27 mdemeest 4.6B All changes indicated by "UGL"           UGL
*                          Condition value for non-deductible tax   UGL
*-----------------------------------------------------------------------
*eject
*----------------------------------------------------------------------*
* Ausgabe von Stammkonditionen
*----------------------------------------------------------------------*
FORM AUSGABE_STAMMKONDITIONEN.

  KOMK-WAERK = EKKO-WAERS.             "1.2
  IF EKPO-EBELP EQ '00000'.            "Kopfkonditionen
    READ TABLE TEKOMD INDEX 1.
    IF SY-SUBRC NE 0.
      IF PREISDRUCK NE SPACE.
        KOMK-FKWRT = KOMK-SUPOS.
        KOMK-WAERK = EKKO-WAERS.
*------------------------- UGL Change ------------------------------ UGL
* find out condition value for non-deductible tax                    UGL
        komvd-kwert = 0.                                            "UGL
        loop at tkomv.                                              "UGL
          if tkomv-kschl = 'NAVS'.                                  "UGL
             komvd-kwert = komvd-kwert + tkomv-kwert.               "UGL
          endif.                                                    "UGL
        endloop.                                                    "UGL
*----------------------- End of UGL Change ------------------------  UGL

        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'TOTAL_AMOUNT'
             EXCEPTIONS
                  OTHERS  = 01.
        CLEAR SY-SUBRC.
      ENDIF.
      EXIT.
    ELSE.
      IF PREISDRUCK NE SPACE.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'TOTAL_AMOUNT_ITEMS'
             EXCEPTIONS
                  OTHERS  = 01.
        CLEAR SY-SUBRC.
      ENDIF.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'HEAD_MCONDITIONS'
           EXCEPTIONS
                OTHERS  = 01.
      CLEAR SY-SUBRC.
    ENDIF.
  ENDIF.
  CLEAR KZBZG.
  LOOP AT TEKOMD WHERE EBELP EQ EKPO-EBELP.
    MOVE TEKOMD TO EKOMD.
    ON CHANGE OF TEKOMD-WERKS.
      IF NOT TEKOMD-WERKS IS INITIAL.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'ITEM_MCONDITIONS_PLANT'
             EXCEPTIONS
                  OTHERS  = 01.
        CLEAR SY-SUBRC.
      ENDIF.
    ENDON.
    IF NOT TEKOMD-DATAB IS INITIAL.
*       Zeile mit Konditionsgültigkeitszeitraum
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'ITEM_MCONDITIONS_VALID'
           EXCEPTIONS
                OTHERS  = 01.
      CLEAR SY-SUBRC.
    ENDIF.
    IF TEKOMD-VTEXT NE SPACE.
      KZBZG = TEKOMD-KZBZG.
      IF TEKOMD-KZBZG EQ SPACE.
*          Zeile ohne Staffel
        IF TEKOMD-KPEIN IS INITIAL.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'ITEM_MCONDITIONS'
               EXCEPTIONS
                    OTHERS  = 01.
          CLEAR SY-SUBRC.
        ELSE.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    ELEMENT = 'ITEM_MCONDITIONS_UNIT'
               EXCEPTIONS
                    OTHERS  = 01.
          CLEAR SY-SUBRC.
        ENDIF.
      ELSE.
*          Zeile mit Konditionsbezeichnung und Staffelbezeichnung
        CASE TEKOMD-STFKZ.
          WHEN 'C'.                    "Ab_Staffel gesteigert
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_MCONDITIONS_SCALE_FROM_I'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          WHEN 'D'.                    "Bis_Staffel gesteigert
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_MCONDITIONS_SCALE_TO_I'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          WHEN 'B'.                    "Bis-Staffel
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_MCONDITIONS_SCALE_TO'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          WHEN OTHERS.                 "bei 'A' und ' ' --> Ab-Staffel
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_MCONDITIONS_SCALE_FROM'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
        ENDCASE.
      ENDIF.
    ELSE.
      CASE KZBZG.
        WHEN 'B'.
*           Zeile für Wertstaffel
          IF TEKOMD-KPEIN IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_MCONDITIONS_VALUE_SCALE'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_MCONDITIONS_VALUE_SCALE_UNIT'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          ENDIF.
        WHEN OTHERS.
*           Zeile für Mengenstaffel, Gewichte und Volumenstaffel
          IF TEKOMD-KPEIN IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_MCONDITIONS_QUANT_SCALE'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_MCONDITIONS_QUANT_SCALE_UNIT'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDLOOP.

ENDFORM.
