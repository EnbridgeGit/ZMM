REPORT ZMMMC001.
************************************************************************
*    Program       :  ZMMMC001
*    Programmer    :  M. DeMeester
*    Date          :  September, 1996.
*
* cross reference files for conversion
* ZCFMM001_01 - SAP Material Numbers to Legacy Stock Numbers
* ZCFMM001_02 - SAP Vendors to Legacy Vendor Numbers
***********************************************************************
TABLES:
       MARA, MAKT,
       LFB1.
DATA:
    outmatlfile(60),
    outvendorfile(60),
    BEGIN OF REC,
        MATNR        LIKE MARA-MATNR,
        BISMT        LIKE MARA-BISMT,
        MAKTX        LIKE MAKT-MAKTX,
    END OF REC.

DATA:
    BEGIN OF VNDRREC,
        LIFNR        LIKE LFB1-LIFNR,
        ALTKN        LIKE LFB1-ALTKN,
        BUKRS        LIKE LFB1-BUKRS,
    END OF VNDRREC.

DATA:
    BEGIN OF WA,
        MATNR        LIKE MARA-MATNR,
        BISMT        LIKE MARA-BISMT,
    END OF WA.

SELECT-OPTIONS: SMTART FOR MARA-MTART.

PARAMETER:
    MATLFILE         LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMC001_01',
    VENDFILE         LIKE FILENAME-FILEINTERN DEFAULT 'ZMMMC001_03'.

START-OF-SELECTION.
*  Material Numbers to Legacy Stock Numbers Cross Reference
    CALL FUNCTION 'FILE_GET_NAME'
         EXPORTING
             CLIENT           = SY-MANDT
             logical_filename = matlfile
             OPERATING_SYSTEM = SY-OPSYS
         IMPORTING
             file_name        = outmatlfile
         EXCEPTIONS
             FILE_NOT_FOUND   = 01.

    IF  ( SY-SUBRC = 1 ).
        WRITE: 'function return code=', SY-SUBRC.
    ENDIF.

    open dataset outmatlfile for output in text mode.
    IF  SY-SUBRC NE '0'.
        WRITE: 'return code=', SY-SUBRC.
    ENDIF.

*  Vendor Number Cross Reference file
    CALL FUNCTION 'FILE_GET_NAME'
         EXPORTING
             CLIENT           = SY-MANDT
             LOGICAL_FILENAME = VENDFILE
             OPERATING_SYSTEM = SY-OPSYS
         IMPORTING
             file_name        = outvendorfile
         EXCEPTIONS
             FILE_NOT_FOUND   = 01.

    IF  ( SY-SUBRC = 1 ).
        WRITE: 'function return code=', SY-SUBRC.
    ENDIF.

    open dataset outvendorfile for output in text mode.
    IF  SY-SUBRC NE '0'.
        WRITE: 'return code=', SY-SUBRC.
    ENDIF.

* select all material numbers from SAP

SELECT MATNR BISMT
   INTO WA
   FROM MARA WHERE MTART IN SMTART
   ORDER BY BISMT.
   SELECT SINGLE * FROM MAKT
       WHERE MATNR = MARA-MATNR
         AND SPRAS = SY-LANGU.
    MOVE WA-MATNR          TO REC-MATNR.      "Material Number
    MOVE WA-BISMT          TO REC-BISMT.      "Legacy Number
    MOVE MAKT-MAKTX        TO REC-MAKTX.      "Description
    TRANSFER REC           TO OUTMATLFILE.
ENDSELECT.

* select all vendor numbers from SAP

SELECT * FROM LFB1
    WHERE BUKRS IN ('UGL','CGO')
         ORDER BY LIFNR.
    MOVE LFB1-LIFNR            TO VNDRREC-LIFNR.
    MOVE LFB1-ALTKN            TO VNDRREC-ALTKN.
    MOVE LFB1-BUKRS            TO VNDRREC-BUKRS.
    transfer vndrrec           to outvendorfile.
ENDSELECT.

END-OF-SELECTION.
    close dataset matlfile.
    CLOSE DATASET VENDFILE.
