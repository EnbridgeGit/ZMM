REPORT ZMMMC500.
************************************************************************
*    Program       :  ZMMMC500
*    Programmer    :  Gerryk Karpowicz
*
*    Date          :  September, 1996.
*
* cross reference files for conversion
* ZCFMM001_MA_01 - SAP Material Numbers to Legacy Stock Numbers
* ZCFMM001_VE_01 - SAP Vendors to Legacy Vendor Numbers
* ZCFMM001_ER_01 - SAP Vendors to Legacy Vendor Numbers
*
***********************************************************************
TABLES:
       MARA, MAKT,
       MARC,
       LFM1,
       LFB1.
DATA:
    OUTFILE01(80),
    OUTFILE02(80),
    OUTFILE03(80),
    BEGIN OF REC OCCURS 10000,   " record size is 82   characters
        BISMT(18),    """  like mara-bismt,     " char 18  old
        MATNR        LIKE MARA-MATNR,           " char 18  new
        MAKTX        LIKE MAKT-MAKTX,           " char 40
        EKGRP        LIKE MARC-EKGRP,           " char  3
        MEINS        LIKE MARA-MEINS,           " char  3  base decs
    END OF REC.                          " total ______82

DATA:
    BEGIN OF VNDRREC OCCURS 1000,   " record size is  70 characters.
*        altkn        like lfb1-altkn,                 " char 10 old
        ALTKN(10)   TYPE N,                           " char 10 old
        LIFNR        LIKE LFB1-LIFNR,                 " char 10 new
        BUKRS        LIKE LFB1-BUKRS,                 " char  4
        VERKF        LIKE  LFM1-VERKF,                " char 30
        TELF1        LIKE  LFM1-TELF1,                " char 16
    END OF VNDRREC.                     " total______________70

PARAMETERS:
    FILE01         LIKE FILENAME-FILEINTERN DEFAULT 'ZCFMM004_03',
    FILE02         LIKE FILENAME-FILEINTERN DEFAULT 'ZCFMM004_04',
    FILE03         LIKE FILENAME-FILEINTERN DEFAULT 'ZCFMM004_05'.
DATA: BEGIN OF W_NAME1,
      W_FIELD1(8) TYPE N,
      W_I         TYPE C VALUE SPACE,
      W_FIELD2(8) TYPE N,
END OF W_NAME1.
DATA: W_LFB1-ALTKN LIKE LFB1-ALTKN VALUE '0000000000'. "prev mast rec
DATA: W_OFSET(2)     TYPE N.
data: w_filed_check(8) type n  value  00000000.
***************--S_T_A_R_T---O_F--T_H_E_--M_A_I_N---L_O_O_P--***
START-OF-SELECTION.
*
     perform  000-get-files.
*_______________________________________________________
* select all material-numbers (from sap)
*_______________________________________________________
   perform  100-get-mara.
    IF  SY-SUBRC eq '0'.
        sort rec by  BISMT.   "" sort materials
        LOOP AT REC.
         TRANSFER REC    TO OUTFILE01.  " material
        ENDLOOP.
    ENDIF.
  PERFORM 150-GET-LFM1.                "purchasing vendor
*--sort
  sort VNDRREC  by  ALTKN.         " sort   70 characters.
  clear W_LFB1-ALTKN.
****************  WHERE BUKRS IN ('UGL','CGO')  " CGO  centra
    IF  SY-SUBRC eq '0'.
        LOOP AT VNDRREC.
         IF W_LFB1-ALTKN      NE   VNDRREC-ALTKN.
*j           write: / 've    ', vndrrec.
             TRANSFER VNDRREC TO OUTFILE02. " vendor    file
             W_LFB1-ALTKN     =    VNDRREC-ALTKN.
         endif.
        endloop.
    endif.
*******************--E_N_D---O_F--T_H_E_--M_A_I_N---L_O_O_P---**
*_____________________________________________________________
*_________G_E_T___M_A_R_A_______________________________
*_______________________________________________________
form   100-get-mara.
  SELECT * FROM MARA
   WHERE BISMT NE SPACE
   and  lvorm = ' '
    ORDER BY BISMT.
*-
      SELECT * FROM MARC
        WHERE MATNR = MARA-MATNR
              AND LVORM = ' '.
         IF MARC-EKGRP <> SPACE.
            EXIT.
         ENDIF.
      ENDSELECT.
      IF SY-SUBRC = 0.
*-
         SELECT SINGLE * FROM MAKT
               WHERE MATNR = MARA-MATNR
               AND SPRAS = SY-LANGU.
         MOVE MARA-MATNR            TO REC-MATNR.
*---          move mara-bismt            to rec-bismt.
         MOVE MAKT-MAKTX            TO REC-MAKTX.
*-
         MOVE MARC-EKGRP            TO REC-EKGRP.
* do not use move for that field - create german base description !!!!!
         WRITE MARA-MEINS           TO REC-MEINS.   "  base description
*-
*
        IF MARA-BISMT  CA '/'         " check slashes in data
           AND SY-FDPOS  = 8.
           CLEAR  W_NAME1-W_FIELD1.
           CLEAR  W_NAME1-W_FIELD2.
*
          PERFORM CHECK-KEY-NUMBERS.
*
        ELSE.                           " error condition
               MOVE MARA-BISMT         TO REC-BISMT.
               TRANSFER REC            TO OUTFILE03.  " ERROR FILE
*
        ENDIF.
     ENDIF.
  ENDSELECT.
endform.
*_____________________________________________________________
 form check-key-numbers.
*                                         w_filed1 =  union
     MOVE MARA-BISMT+0(SY-FDPOS) TO W_NAME1-W_FIELD1. "size 8
     if  w_filed_check ne W_NAME1-W_FIELD1        "check sequ. key
     and W_NAME1-W_FIELD1 NE 000000000.           "get only union
         W_OFSET = SY-FDPOS + 1.
*                                         w_filed2 =  centra
         MOVE MARA-BISMT+W_OFSET(9) TO W_NAME1-W_FIELD2.  "size 8
           CONDENSE: W_NAME1.
           MOVE W_NAME1 TO REC-BISMT.
*t1            transfer rec            to w_file01.   " good  FILE
               APPEND REC.    "  good  FILE
          w_filed_check   =     W_NAME1-W_FIELD1. "new sequ key
     endif.
 endform.
*_____________________________________________________________
*_________G_E_T___L_F_M_1_______________________________
*_______________________________________________________
  FORM  150-GET-LFM1.                "purchasing vendor
*___________________________________________________________
*****  WHERE BUKRS IN ('UGL','CGO')  " CGO  centra
*
  SELECT *  FROM LFM1                 "purchasing vendor
       WHERE LOEVM = ' '
       order by   LIFNR.
*           write: / 'lfb1 ', lfm1-lifnr.
       SELECT SINGLE * FROM LFB1
         WHERE LIFNR = LFM1-LIFNR
         AND  LOEVM = ' '
         AND ALTKN NE SPACE
         AND BUKRS = 'UGL'.
     IF  SY-SUBRC = 0.
         PERFORM  100-LOAD.
     ENDIF.
 ENDSELECT.
 endform.
*---------------------------------
*---------------------------------
FORM  100-LOAD.
        IF  W_LFB1-ALTKN  NE  LFB1-ALTKN+0(8)
           and  LFB1-ALTKN  NE  SPACE.
          MOVE LFB1-ALTKN+0(8)    TO VNDRREC-ALTKN.
          MOVE LFB1-LIFNR         TO VNDRREC-LIFNR.
          MOVE LFB1-BUKRS         TO VNDRREC-BUKRS.
*-
          MOVE LFM1-VERKF         TO VNDRREC-VERKF.
          MOVE LFM1-TELF1         TO VNDRREC-TELF1.
*
           APPEND VNDRREC.               " write to internal
           MOVE LFB1-ALTKN+0(8)    TO W_LFB1-ALTKN.
       ENDIF.
ENDFORM.
*___________________________________
*___________________________________
 form  000-get-files.
*________________________________________________________________
*   material numbers to legacy stock numbers cross reference
*________________________________________________________________
    CALL FUNCTION 'FILE_GET_NAME'
         EXPORTING
             CLIENT           = SY-MANDT
             LOGICAL_FILENAME = FILE01
             OPERATING_SYSTEM = SY-OPSYS
         IMPORTING
             FILE_NAME        = OUTFILE01
         EXCEPTIONS
             FILE_NOT_FOUND   = 01.

    IF  ( SY-SUBRC = 1 ).
        WRITE: 'materials ---> function return code=', SY-SUBRC.
    ENDIF.
*________________________________________________________________
*     open dataset outfile01 for output in text mode.
*________________________________________________________________
      OPEN  DATASET OUTFILE01 FOR OUTPUT IN TEXT MODE.
     IF  SY-SUBRC NE 0.
         WRITE: 'return code=', SY-SUBRC.
     ENDIF.

*________________________________________________________________
*  Vendor Number Cross Reference file
     CALL FUNCTION 'FILE_GET_NAME'
          EXPORTING
             CLIENT           = SY-MANDT
             LOGICAL_FILENAME = FILE02
             OPERATING_SYSTEM = SY-OPSYS
         IMPORTING
             FILE_NAME        = OUTFILE02
          EXCEPTIONS
            FILE_NOT_FOUND   = 01.

     IF  ( SY-SUBRC = 1 ).
         WRITE: ' VENDOR ----> function return code=', SY-SUBRC.
     ENDIF.

*    open dataset outfile02 for output in text mode.
     OPEN DATASET OUTFILE02 FOR OUTPUT IN TEXT MODE.
    IF  SY-SUBRC NE '0'.
        WRITE: 'return code=', SY-SUBRC.
    ENDIF.
*____________________________________________________________________
*  ERRORS FILE
     CALL FUNCTION 'FILE_GET_NAME'
          EXPORTING
             CLIENT           = SY-MANDT
             LOGICAL_FILENAME = FILE03
             OPERATING_SYSTEM = SY-OPSYS
         IMPORTING
             FILE_NAME        = OUTFILE03

          EXCEPTIONS
            FILE_NOT_FOUND   = 01.

   OPEN DATASET OUTFILE03 FOR OUTPUT IN TEXT MODE.
    IF  SY-SUBRC NE '0'.
        WRITE: 'outfile03 return code=', SY-SUBRC.
    ENDIF.
 endform.
*___________________________________
END-OF-SELECTION.
    CLOSE DATASET OUTFILE01.
    CLOSE DATASET OUTFILE02.
    CLOSE DATASET OUTFILE03.
