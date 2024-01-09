REPORT ZMMMI012 NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMI012
*    PROGRAMMER  :  MaryLou DeMeester
*    Client      :  Union/Centra
*    Date        :  March 7,1997
*    Changed     :  Sept 3,1997    Ric Aarssen                 raarssen
*
* This ABAP will determine which reservations have already been issued.
************************************************************************
* 97/07/30 raarssen added a column for reserved outstanding
*          Reserved Qty Outstanding = Qty Requested - Qty Withdrawn
* 97/03/12 md7140 select Goods Recipient from header record
* 97/03/11 md7140 added movement types 301 & 311
*                 also Goods Recipient beginning with 'G'
************************************************************************
*-----------------------------------------------------------------------
* Declaration Section
*-----------------------------------------------------------------------
TABLES  : RESB, MSEG, RKPF.
DATA    : RESV-OUTSTD(5)  TYPE I.                             "raarssen

*-----------------------------------------------------------------------
* Selection screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-TTL.
PARAMETERS:  P-PLANT    LIKE MSEG-WERKS.                "input plant id
SELECTION-SCREEN END OF BLOCK BOX.
*-----------------------------------------------------------------------
TOP-OF-PAGE.                                   "write heading of report
  FORMAT INTENSIFIED OFF.
  WRITE: / SY-REPID, 15 TEXT-TTL, 64 TEXT-DTE, SY-DATUM.
  WRITE: /64 TEXT-011, SY-UZEIT UNDER SY-DATUM.
  WRITE: / TEXT-CLT, SY-MANDT, 64 TEXT-PGE, SY-PAGNO.
  WRITE: /.
  ULINE.
  WRITE: /3 TEXT-009, 16 TEXT-004, 27 TEXT-003, 36 TEXT-005,
         48 TEXT-001, TEXT-012 UNDER SY-PAGNO.                "raarssen
  WRITE: / TEXT-010 UNDER TEXT-009, TEXT-013 UNDER TEXT-012.  "raarssen
  ULINE.
  FORMAT INTENSIFIED ON.

*-----------------------------------------------------------------------
*                 MAIN PROGRAM
*-----------------------------------------------------------------------
START-OF-SELECTION.

SELECT * FROM RESB    "1) read reservation detail table
         WHERE WERKS = P-PLANT                   "plant
           AND BWART IN ('261','301','311')      "movement type
           AND XLOEK <> 'X'                      "delete indicator
           AND KZEAR <> 'X'                      "final issue indicator
           ORDER BY WEMPF MATNR.      "goods recipient, material number
         RESV-OUTSTD = RESB-ERFMG - RESB-ENMNG.               "raarssen

    SELECT * FROM RKPF    "2) read reservation header table
        WHERE RSNUM = RESB-RSNUM.                "reservation number
      TRANSLATE RKPF-WEMPF TO UPPER CASE.
      IF  RKPF-WEMPF(1) = 'R' OR RKPF-WEMPF(1) = 'S' OR
          RKPF-WEMPF(1) = 'T' OR RKPF-WEMPF(1) = 'G'.  "a) reference no

        SELECT * FROM MSEG    "3) read material document table
            WHERE WERKS = RESB-WERKS             "plant
              AND LGORT = RESB-LGORT             "storage location
              AND MATNR = RESB-MATNR             "material number
              AND WEMPF = RKPF-WEMPF.            "goods recipient
        IF  SY-SUBRC = 0.  "b) found document
          WRITE:  / MSEG-MBLNR  UNDER TEXT-010,               "raarssen
                    MSEG-WEMPF  UNDER TEXT-004,
                    MSEG-WERKS  UNDER TEXT-003,
                    MSEG-MATNR  UNDER TEXT-005,
                    MSEG-RSNUM NO-ZERO UNDER TEXT-001,
                    RESV-OUTSTD DECIMALS 0 UNDER TEXT-013.
        ENDIF.  "b) found document

        ENDSELECT.   "3) end of material document

      ENDIF.  "a) found reference number
    ENDSELECT.    "2) end of reservation header
    CLEAR RESV-OUTSTD.                                        "raarssen

  ENDSELECT.    "1) end of reservation detail
END-OF-SELECTION.
WRITE: / TEXT-END.
