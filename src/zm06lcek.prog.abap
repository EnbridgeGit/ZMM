*eject
*----------------------------------------------------------------------*
*             COMMON DATA                                              *
*----------------------------------------------------------------------*
*             Datenfelder für die Listen der Einkaufsbelege            *
*----------------------------------------------------------------------*

DATA: BEGIN OF COMMON PART ZM06LCEK.

*------------ Überschrift ---------------------------------------------*
DATA: BEGIN OF UEB,
          1(80),
          2(80),
          3(80),
          4(80),
      END OF UEB.

*------------ Hilfsfelder ---------------------------------------------*
DATA: REJECT.
DATA: LEERFLG.
DATA: NOT_FOUND.

DATA: END OF COMMON PART.
