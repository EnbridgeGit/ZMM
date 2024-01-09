*eject
*--------------------------------------------------------------------*
*        COMMON DATA                                                 *
*--------------------------------------------------------------------*
*        Selektionsbedingungen 1                                     *
*--------------------------------------------------------------------*

DATA:    BEGIN OF COMMON PART ZM06LCS1.

PARAMETERS:     LISTU LIKE T160O-LISTU.
SELECT-OPTIONS: SELPA FOR T160T-SELPA,
                S_BSART FOR EKKO-BSART,
                S_EKGRP FOR EKKO-EKGRP.

DATA:    END OF COMMON PART.
