 REPORT ZMPUR001 NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65.
*-------------------------------------------------------------------*
* abap to select info records based on date criteria                *
*-------------------------------------------------------------------*
* 97/04/18 MD7140 changed ent1027 to ent1034 for efficiency         *
* 98/01/09 MD7140 #322 new request DRMM0227                         *
*-------------------------------------------------------------------*
 TABLES:
        EINA,
        A018.

 DATA:
     BEGIN OF BIG_TABLE OCCURS 0,
        LIFNR          LIKE EINA-LIFNR,"Vendor Number
        MATNR          LIKE EINA-MATNR,"Material Number
        INFNR          LIKE EINA-INFNR,"Info Record #
 END OF BIG_TABLE.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
 PARAMETER: PDTFRM     LIKE A018-DATAB OBLIGATORY,
            PDTTO      LIKE A018-DATBI OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX1.


 START-OF-SELECTION.

   SELECT * FROM A018                  "Date condition satisfied
        WHERE KSCHL = 'ZZZZ'
          AND DATAB = PDTFRM
          AND DATBI = PDTTO.
     SELECT * FROM EINA                "Matching info record
        WHERE MATNR = A018-MATNR
          AND LIFNR = A018-LIFNR.
       MOVE A018-MATNR TO BIG_TABLE-MATNR.
       MOVE A018-LIFNR TO BIG_TABLE-LIFNR.
       MOVE EINA-INFNR TO BIG_TABLE-INFNR.
       APPEND BIG_TABLE.
       CLEAR BIG_TABLE.
     ENDSELECT.
   ENDSELECT.

   SORT BIG_TABLE BY LIFNR  MATNR  INFNR.
   PERFORM DISPLAY_TABLE.
   WRITE: /.
   WRITE: / TEXT-END UNDER TEXT-TTL.

*----------------  DISPLAY_TABLE  --------------------------------------
 FORM DISPLAY_TABLE.
   LOOP AT BIG_TABLE.
     AT NEW LIFNR.
       WRITE: / BIG_TABLE-LIFNR UNDER TEXT-005.
     ENDAT.

     WRITE: / BIG_TABLE-MATNR UNDER TEXT-004,
              BIG_TABLE-INFNR UNDER TEXT-006.

   ENDLOOP.
 ENDFORM.

* EVENT -------------  TOP-OF-PAGE  ------------------------------------
 TOP-OF-PAGE.
   WRITE: /1 TEXT-RPT, SY-REPID, 54 TEXT-DTE, SY-DATUM,
             TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT,
            16 TEXT-TTL,
            TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
   WRITE: /.
   WRITE: /1 TEXT-005, 20 TEXT-004, 40 TEXT-006.
ULINE.
