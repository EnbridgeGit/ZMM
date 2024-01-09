* 2000/07/27 mdemeest 4.6B All changes identified with "UGL"
*                          added condition value for non-deductible tax
*-----------------------------------------------------------------------
*eject
*----------------------------------------------------------------------*
* Ausgabe Anhang
*----------------------------------------------------------------------*
FORM AUSGABE_ANHANG.

* Euroumrechnung
 IF  EKKO-BEDAT > '19990101' AND
     ( EKKO-WAERS = 'DEM' OR EKKO-WAERS = 'FRF' OR EKKO-WAERS = 'ATS'
    OR EKKO-WAERS = 'BEF' OR EKKO-WAERS = 'FIM' OR EKKO-WAERS = 'IEP'
    OR EKKO-WAERS = 'ITL' OR EKKO-WAERS = 'LUF' OR EKKO-WAERS = 'NLG'
    OR EKKO-WAERS = 'PTE' OR EKKO-WAERS = 'ESP' ).

  komk-waerk_euro = 'EUR'.                                  "302203
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
       EXPORTING
            DATE             =  EKKO-BEDAT
            FOREIGN_AMOUNT   =  komk-fkwrt
            FOREIGN_CURRENCY =  EKKO-WAERS
*            LOCAL_CURRENCY   =  'EUR'                      "302203
            LOCAL_CURRENCY   =  komk-waerk_euro             "302203
       IMPORTING
            LOCAL_AMOUNT     =  EURO-PRICE
       EXCEPTIONS
            NO_RATE_FOUND    = 1
            OVERFLOW         = 2
            NO_FACTORS_FOUND = 3
            NO_SPREAD_FOUND  = 4
            OTHERS           = 5.

  SUM-EURO-PRICE = EURO-PRICE.
ELSE.
  sum-EURO-PRICE = 0.
ENDIF.
* Gesamtsumme ausgeben ------------------------------------------------*
  IF PREISDRUCK NE SPACE.
    KOMK-WAERK = EKKO-WAERS.           "1.2
    IF EKKO-BSTYP NE BSTYP-KONT AND EKKO-STAKO EQ SPACE.
      READ TABLE TKOMVD INDEX 1.
      IF SY-SUBRC EQ 0.    "nur, wenn noch Kopfrabatte kommen
* IF KOMK-SUPOS NE KOMK-FKWRT.     "geht nicht wegen FKWRT-Rundung!!
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = 'TOTAL_AMOUNT_ITEMS'
             EXCEPTIONS
                  OTHERS  = 01.
        CLEAR SY-SUBRC.
* Ausgeben Kopf-Konditionen ------------------------------------------*
        LOOP AT TKOMVD.
          KOMVD = TKOMVD.
          IF KOMVD-KPEIN IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_CONDITIONS'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = 'ITEM_CONDITIONS_UNIT'
                 EXCEPTIONS
                      OTHERS  = 01.
            CLEAR SY-SUBRC.
          ENDIF.
        ENDLOOP.
      ENDIF.

*------------------------------  UGL Change -----------------------  UGL
* Find out the condition value for non-deductible tax                UGL
      komvd-kwert = 0.                                              "UGL
      loop at tkomv.                                                "UGL
        if tkomv-kschl = 'NAVS'.                                    "UGL
           komvd-kwert = komvd-kwert = tkomv-kwert.                 "UGL
        endif.                                                      "UGL
      endloop.                                                      "UGL
*----------------------------- End of UGL Change ------------------  UGL
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = 'TOTAL_AMOUNT'
           EXCEPTIONS
                OTHERS  = 01.
      CLEAR SY-SUBRC.
    ENDIF.
  ENDIF.

IF EKKO-BSTYP EQ BSTYP-KONT OR EKKO-STAKO NE SPACE. "beim Kontrakt immer
    EKPO-EBELP = '00000'.
    PERFORM AUSGABE_STAMMKONDITIONEN.
  ENDIF.

  IF XLPET EQ SPACE.
* Ueberschrift-Element ermitteln --------------------------------------*
    CASE EKKO-BSTYP.
      WHEN BSTYP-BEST. ELEMENTN = 'ITEM_HEADER_F'.
      WHEN BSTYP-ANFR. ELEMENTN = 'ITEM_HEADER_A'.
      WHEN BSTYP-KONT. ELEMENTN = 'ITEM_HEADER_R'.
      WHEN BSTYP-LFPL. ELEMENTN = 'ITEM_HEADER_R'.
    ENDCASE.

* Keine Positionsüberschrift mehr -------------------------------------*
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = ELEMENTN
              FUNCTION = 'DELETE'
              TYPE     = 'TOP'
         EXCEPTIONS
              OTHERS   = 01.
    CLEAR SY-SUBRC.
  ENDIF.

* Anhangstexte ausgeben ----------------------------------------------*
  LOOP AT XT166A.
    MOVE XT166A TO T166A.
    CASE T166A-TDOBJECT.
      WHEN 'EKKO'.
        T166A-TXNAM(10) = EKKO-EBELN.
    ENDCASE.
    PERFORM LESEN_TTXIT USING XT166A-TITDR XT166A-TDOBJECT XT166A-TDID.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'SUPPL_TEXT'
         EXCEPTIONS
              OTHERS  = 01.
    CLEAR SY-SUBRC.
  ENDLOOP.


ENDFORM.
