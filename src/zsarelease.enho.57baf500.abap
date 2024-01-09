"Name: \FU:ME_REL_STRATEGIE_EKKO\SE:END\EI
ENHANCEMENT 0 ZSARELEASE.
** Start of changes COG to reset Release strategy
IF ( sy-tcode = 'ME32L' AND SY-UCOMM = 'BU').
  PERFORM reset
       USING e_frgst e_frggr
             e_frgzu e_frgkz e_frgrl.
ELSEIF  ( SY-TCODE = 'ME32K' AND SY-UCOMM = 'BU' ) .
  IF I_CEKKO_NEW-BSART = 'ZK' OR I_CEKKO_NEW-BSART = 'ZJ'.
    PERFORM reset
       USING e_frgst e_frggr
             e_frgzu e_frgkz e_frgrl.
  ENDIF.
ENDIF.

ENDENHANCEMENT.
