"Name: \PR:SAPLEINN\FO:BDC_FIELD\SE:BEGIN\EI
ENHANCEMENT 0 Z_LEINNF01.
*btboundy
*CLM TR835 - May 2011

  "New Line Insert
  IF curprogram   = 'SAPMM06E'    AND
     curdynpro    = DYNPRO_211    AND
     FNAM         = 'BDC_OKCODE'  AND
     FVAL         = '=DETZ'       AND
     newekpoline  = 1
  .

    "Set that we already processed this line.
    CLEAR newekpoline.

    "Go to services
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'BDC_OKCODE'
                   '=DIEN'.
    "Service Screen
    PERFORM BDC_DYNPRO
            TABLES T_BDCDATA
            USING  'SAPLMLSP'
                   DYNPRO_201.

    "Insert new line
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'BDC_OKCODE'
                   '=NP'.

    "Service Screen (Needed second DynPro line)
    PERFORM BDC_DYNPRO
            TABLES T_BDCDATA
            USING  'SAPLMLSP'
                   DYNPRO_201.

    "Short Text
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'ESLL-KTEXT1(01)'
                   curktext1.

    "Target Quantity
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'ESLL-MENGE(01)'
                   curmenge.

    SELECT SINGLE isocode
      FROM T006
      INTO curmeins
      WHERE msehi = curmeins
    .

    "Unit of Measure
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'ESLL-MEINS(01)'
                   curmeins.

    "Gross Price
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'ESLL-TBTWR(01)'
                   curtbtwr.

    "Price Unit
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'ESLL-PEINH(01)'
                   curpeinh.

    "Material Group
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'ESLL-MATKL(01)'
                   curmatkl.

    "Back to Main Screen
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'BDC_OKCODE'
                   '/EESB'.

    "Main screen
    PERFORM BDC_DYNPRO
            TABLES T_BDCDATA
            USING  'SAPMM06E'
                   DYNPRO_220.
    "Specify which line to continue using.
    "This will always be line 2 except for the first item.
    IF curlinenum = minlinenum.
      PERFORM BDC_FIELD
              TABLES T_BDCDATA
              USING  'BDC_CURSOR'
              'RM06E-EVRTP(001)'.
    ELSE.
      PERFORM BDC_FIELD
              TABLES T_BDCDATA
              USING  'BDC_CURSOR'
              'RM06E-EVRTP(002)'.
    ENDIF.

    "Continue running DETZ aka Additional Data
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'BDC_OKCODE'
                   '=DETZ'.

    "Exit function
    EXIT.
  ENDIF.


  "Change existing Line
  IF curprogram   = 'SAPMM06E'    AND
     curdynpro    = DYNPRO_220    AND
     FNAM         = 'BDC_OKCODE'  AND
     FVAL         = '=DIEN'       AND
     newekpoline  = 1
    .
    newekpoline = 0.

    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'BDC_OKCODE'
                   '=DIEN'.

    "Service Screen
    PERFORM BDC_DYNPRO
            TABLES T_BDCDATA
            USING  'SAPLMLSP'
                   DYNPRO_201.

    "Target Quantity
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'ESLL-MENGE(01)'
                   curmenge.

    "Gross Price
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'ESLL-TBTWR(01)'
                   curtbtwr.

    "BACK to MAIN SCREEN
    PERFORM BDC_FIELD
            TABLES T_BDCDATA
            USING  'BDC_OKCODE'
                   '/EESB'.

    "Exit this function.
    EXIT.
  ENDIF.
ENDENHANCEMENT.
