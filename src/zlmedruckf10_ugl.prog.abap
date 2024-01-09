*&---------------------------------------------------------------------*
*&      Form  MENGE_AUSGEBEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EKET_MENGE  text
*      -->P_T006_DECAN  text
*      -->P_RM06P_PRMG1  text
*----------------------------------------------------------------------*
form menge_ausgeben using mga_menge mga_decan mga_ameng.

*  data: h-ameng like rm06p-prmg1.

*  write mga_menge no-sign to h-ameng.
  write mga_menge no-sign decimals mga_decan to mga_ameng.  "569326
*  if mga_andec eq 0 and
*     h-ameng+14(3) eq '000'.
*    shift h-ameng right by 4 places.
*  endif.
*  mga_ameng = h-ameng.


ENDFORM.                               " MENGE_AUSGEBEN
