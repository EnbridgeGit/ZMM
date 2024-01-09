*eject
*--------------------------------------------------------------------*
*        COMMON DATA                                                 *
*--------------------------------------------------------------------*
*        11/18/1996 JLEE                                             *
*--------------------------------------------------------------------*

DATA:    BEGIN OF COMMON PART ZNMIM008.

SELECT-OPTIONS: P_BPRME FOR EKPO-BPRME.                   "JLEE 11/18/96
SELECT-OPTIONS: P_NETWR FOR EKPO-NETWR.                   "JLEE 11/18/96
SELECT-OPTIONS: P_ERNAM FOR EKKO-ERNAM DEFAULT SY-UNAME.  "JLEE 11/18/96
PARAMETERS:     P_ASCEND RADIOBUTTON GROUP RAD1,          "JLEE 11/18/96
                P_DESCED RADIOBUTTON GROUP RAD1,          "JLEE 11/18/96
                P_NOSORT RADIOBUTTON GROUP RAD1.          "JLEE 11/18/96

DATA:    END OF COMMON PART.
