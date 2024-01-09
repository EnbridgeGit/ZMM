"Name: \PR:SAPLEINN\IC:LEINNTOP\SE:END\EI
ENHANCEMENT 0 Z_LEINNTOP.
*btboundy
*CLM TR835 - May 2011

  DATA: curprogram  LIKE bdcdata-program,
        curdynpro   LIKE bdcdata-dynpro,
        curpstyp    LIKE ekpo-pstyp,
        newekpoline TYPE integer.

  DATA: curktext1         LIKE ekpo-txz01,
        curmenge(19)      TYPE  c,
        curmeins          LIKE ekpo-meins,
        curtbtwr(16)      TYPE c,
        curpeinh(7)       TYPE c,
        curmatkl          LIKE ekpo-matkl,
        curlinenum(10)    TYPE i,
        minlinenum(10)    TYPE i,
        doenhancement(1)  TYPE c.
ENDENHANCEMENT.
