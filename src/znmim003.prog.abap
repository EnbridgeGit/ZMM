* 2000/10/10 mdemeest 4.6B Copied M07DRA01 to ZNMIM003            "UGL
* 2013/01/28 btboundy 600 to 605 Upgrade, re-coppied "UGL" code into SAP Standard


FORM WA01_DRUCK.

*-----------------------  UGL Changes ------------------------------ UGL
* Precondition: The user is single-threaded in running transactions  UGL
*               ZM01.  ie. there will be be only 1 entry per user    UGL
*               sitting in table INDX.  INDX is where this  print    UGL
*               program picks up the warranty data that is           UGL
*               uncommitted at the time the print program runs.      UGL

data: key(22) type c.         "Key to cluster table INDX             UGL
"get the warranty data                                              "UGL
move: 'ZWARRANTY'          to key+0(9),                             "UGL
      mkpf-usnam+0(8)      to key+9(8).                             "UGL
import zmwaps zmwap-wreid from database indx(zs) id key.            "UGL
if sy-subrc = 0 and zmwap-wreid <> '00000'.                         "UGL
*      initial printing of document                                 "UGL
   move-corresponding zmwaps to zmwap.                              "UGL
   clear zmwaps.                                                    "UGL
   export zmwaps '00000' to database indx(zw) id key.               "UGL
else.                                                               "UGL
*     reprinting of document                                        "UGL
   select * from zmwap where belnr = mseg-mblnr                     "UGL
                         and wtrno = '001'.                         "UGL
   endselect.                                                       "UGL
endif.                                                              "UGL

* get the vendor address                                            "UGL
  select single * from lfa1 where lifnr = mseg-lifnr.               "UGL

* get the plant address                                              UGL
  select single * from t001w where werks = mseg-werks.              "UGL

*------------------------- End of UGL Changes ---------------------- UGL

  CALL FUNCTION 'START_FORM'
       EXPORTING FORM = TNAPR-FONAM
             LANGUAGE = LANGUAGE.
    IF NOT T159P-BACOD IS INITIAL.
      BELPOS-MBLNR = MKPF-MBLNR.
      BELPOS-ZEILE = MSEG-ZEILE.
      CONDENSE BELPOS NO-GAPS.
      AM07M-BELPOS = BELPOS.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING ELEMENT = 'BACOKOPF'.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING ELEMENT = 'KOPF'.
    ENDIF.
*------------- Drucken Belegposition ---------------------------------*
  CLEAR: AM07M-KOTXT, AM07M-KONTIERUNG.
  CASE XSKKZ.
    WHEN XFERT.                            "Fertigungsauftrag
      AM07M-KOTXT = TEXT-062.
      AM07M-KONTIERUNG = MSEG-AUFNR.
*   when xkdanr.
    WHEN XVBELG.
      MOVE SPACE TO KUNDE.
      AM07M-KOTXT = TEXT-030.
      KUNDE-KDAUF = MSEG-KDAUF.
      KUNDE-KDPOS = MSEG-KDPOS.
      KUNDE-KDEIN = MSEG-KDEIN.
      MOVE SPACE TO AM07M-KONTIERUNG.
      CONDENSE KUNDE NO-GAPS.
      AM07M-KONTIERUNG = KUNDE.
    WHEN XANLAGE.                            "Anlage
      MOVE SPACE TO ANLAGE.
      AM07M-KOTXT = TEXT-040.
      ANLAGE-ANLN1 = MSEG-ANLN1.
      ANLAGE-ANLN2 = MSEG-ANLN2.
      MOVE SPACE TO AM07M-KONTIERUNG.
      CONDENSE ANLAGE NO-GAPS.
      AM07M-KONTIERUNG = ANLAGE.
    WHEN XKOSTL.                             "Kostenstelle
      AM07M-KOTXT = TEXT-050.
      AM07M-KONTIERUNG = MSEG-KOSTL.
    WHEN XPROJN OR XNPLAN.                   "Projekt/Netzplan
      IF MSEG-NPLNR IS INITIAL.
        AM07M-KOTXT = TEXT-060.
        PERFORM PSP_CONVERT USING MSEG-PS_PSP_PNR.
      ELSE.
        AM07M-KOTXT = TEXT-061.
        AM07M-KONTIERUNG = MSEG-NPLNR.
        PERFORM NW_VORGANG_LESEN USING MSEG-AUFPL MSEG-APLZL.
        IF NOT N_VORNR IS INITIAL.
          MOVE '/'     TO AM07M-KONTIERUNG+12.
          MOVE N_VORNR TO AM07M-KONTIERUNG+13.
        ENDIF.
      ENDIF.
  ENDCASE.
* User-Exit über Erweiterung MBCF0005
  CALL CUSTOMER-FUNCTION '001'
       EXPORTING
            I_MKPF  = MKPF
            I_MSEG  = MSEG
            I_NAST  = NAST
            I_TNAPR = TNAPR
       CHANGING
            C_AM07M = AM07M
       EXCEPTIONS
            OTHERS  = 0.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING ELEMENT = 'POS_ZEILE'.

*--------------- Drucken Belegfuß -------------------------------------*
  PERFORM TAB156T_LESEN.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING ELEMENT  = 'FUSS'
                 WINDOW   = 'FUSS'
                 FUNCTION = 'APPEND'.

  CALL FUNCTION 'END_FORM'.

ENDFORM.
