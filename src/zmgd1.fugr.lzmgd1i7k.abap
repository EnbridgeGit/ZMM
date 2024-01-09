*----------------------------------------------------------------------*
***INCLUDE LMGD1I7K .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MARC-VERKZ  INPUT
*&---------------------------------------------------------------------*
*       Im Dialigfall soll eine Warnung hochkommen, wenn die
*       Fertigungsversionen nicht vom Vorlagematerial übernommen
*       wurden. "cfo/4.0C
*----------------------------------------------------------------------*
MODULE MARC-VERKZ INPUT.

 CHECK BILDFLAG IS INITIAL.
 CHECK T130M-AKTYP NE AKTYPA AND T130M-AKTYP NE AKTYPZ.
 CHECK  NOT RMMG1_REF-MATNR IS INITIAL.
 CHECK MARC-VERKZ IS INITIAL AND NOT RMMZU-REF_VERKZ IS INITIAL.
 CALL FUNCTION 'T133D_ARRAY_READ'
      EXPORTING
           BILDSEQUENZ = BILDSEQUENZ
      TABLES
           TT133D      =  TT133D
      EXCEPTIONS
           WRONG_CALL  = 01.
 CLEAR FLAG1.
 LOOP AT TT133D WHERE ROUTN = FORM_MKAL.
   IF RMMZU-OKCODE EQ TT133D-FCODE.
     FLAG1 = X.
     EXIT.
   ENDIF.
 ENDLOOP.
 CHECK  FLAG1 IS INITIAL.

 IF RMMZU-PS_VERKZ IS INITIAL.
   CALL FUNCTION 'MARC_VERKZ'
        EXPORTING
             P_MARC_VERKZ = MARC-VERKZ
             REF_VERKZ    = RMMZU-REF_VERKZ
             HERKUNFT     = HERKUNFT_DIAL
             REF_MATNR    = RMMG1_REF-MATNR
             P_MESSAGE    = ' '
        IMPORTING
             P_PS_VERKZ   = RMMZU-PS_VERKZ.
   IF NOT RMMZU-PS_VERKZ IS INITIAL.
     BILDFLAG = X.
     MESSAGE S069(MM).
   ENDIF.
 ENDIF.

ENDMODULE.                 " MARC-VERKZ  INPUT
