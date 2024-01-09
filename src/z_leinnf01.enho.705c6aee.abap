"Name: \PR:SAPLEINN\FO:BDC_DYNPRO\SE:BEGIN\EI
ENHANCEMENT 0 Z_LEINNF01.
*btboundy
*CLM TR835 - May 2011

  "If this is a service, fill out the curprogram and curdynpro
  "If NOT SERVICE, clear these variables
  IF curpstyp = '9' and doenhancement = c_true.
    curprogram = program.
    curdynpro  = dynpro.
  ELSE.
    CLEAR: curprogram, curdynpro.
  ENDIF.
ENDENHANCEMENT.
