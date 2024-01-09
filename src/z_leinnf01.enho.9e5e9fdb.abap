"Name: \PR:SAPLEINN\FO:SET_FIELDS_BLAORD_DYNPRO_220\SE:BEGIN\EI
ENHANCEMENT 0 Z_LEINNF01.
*btboundy
*CLM TR835 - May 2011

  "Mark that we have a new line.
  newekpoline = 1.
  curpstyp    = t_ekpo-pstyp.

  "Fill the fields we will need later.
  curktext1   = t_ekpo-txz01.
  curmenge    = t_ekpo-ktmng.
  curmeins    = t_ekpo-meins.
  curtbtwr    = t_ekpo-netpr.
  curpeinh    = t_ekpo-peinh.
  curmatkl    = t_ekpo-matkl.
  curlinenum  = t_ekpo-ebelp.
ENDENHANCEMENT.
