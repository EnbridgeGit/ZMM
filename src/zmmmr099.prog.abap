REPORT ZMMMR099 NO STANDARD PAGE HEADING
                   LINE-SIZE 132
                   LINE-COUNT 65
                   MESSAGE-ID ZM.

*_______________________________
* Tables
*__________

TABLES :   NAST,       "Purchase order transmits prints or faxes
          T001W,
          LFA1,                       "Vendor Master
          T023T,
           EKKO,                       "Purchase Order Headers
           EKPO,                       "PO line detail
           EBAN.                       "Purchase Req

INCLUDE <SYMBOL>.

*____________________
* Variables
*____________________

DATA:
    BEGIN OF SAVE_POREQ OCCURS 10000,
       BEDNR      LIKE EBAN-BANFN,
    END OF SAVE_POREQ.

DATA:    BEGIN OF SAVE_TABLE1 OCCURS 10000,
       OBJKY      LIKE NAST-OBJKY,
       PARNR      LIKE NAST-PARNR,
       MANUE      LIKE NAST-MANUE,
       DATVR      LIKE NAST-DATVR,
       UHRVR      LIKE NAST-UHRVR,
       NACHA      LIKE NAST-NACHA,
       NAME1      LIKE LFA1-NAME1,
       POITEM(3)     TYPE I,
       POREQ(3)      TYPE I.
DATA:    END OF SAVE_TABLE1.

DATA:    BEGIN OF SAVE_TABLE2 OCCURS 10000,
       OBJKY      LIKE NAST-OBJKY,
       PARNR      LIKE NAST-PARNR,
       MANUE      LIKE NAST-MANUE,
       DATVR      LIKE NAST-DATVR,
       UHRVR      LIKE NAST-UHRVR,
       NACHA      LIKE NAST-NACHA,
       NAME1      LIKE LFA1-NAME1,
       POITEM(3)     TYPE I,
       POREQ(3)      TYPE I.
DATA:    END OF SAVE_TABLE2.
*___________________ Keeps final totals
DATA:
       POAUTO(5)     TYPE I,
       POMANUAL(5)   TYPE I,
       POEDI(5)      TYPE I,
       POPRINT(5)    TYPE I,
       POZFAX(5)     TYPE I,
       POITEM(5)     TYPE I,
       POREQ(5)      TYPE I,
       PONUM(5)      TYPE I,
*___________________ Keeps date totals and hold fields
      TDAYS(5)      TYPE I,
      TPOAUTO(5)   TYPE I,
      TPOMANUAL(5)  TYPE I,
      TPOEDI(5)     TYPE I,
      TPOPRINT(5)   TYPE I,
      TPOZFAX(5)    TYPE I,
      TPOITEM(5)    TYPE I,
      TPOREQ(5)     TYPE I,
      TPONUM(5)     TYPE I,
      PRV-OBJKY LIKE NAST-OBJKY VALUE 0000000000,
      PRV-BEDNR LIKE EBAN-BANFN VALUE 0000000000,
      PRV-DATVR LIKE NAST-DATVR,
      FIRST-FLAG TYPE I VALUE 0.

*________________________
* Select Options/Parameters
*________________________

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS:  SBUKRS FOR EKPO-BUKRS NO INTERVALS DEFAULT 'UGL ',
                 SPARNR FOR NAST-PARNR,     "vendor nast
                 SDATVR FOR NAST-DATVR DEFAULT SY-DATUM,  "proc dt nast
                 SNACHA FOR NAST-NACHA.   "Message transmission medium

SELECTION-SCREEN END OF BLOCK BOX.

*_____________________   top-of-page  ____________________
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
       50 TEXT-TTL COLOR 4 INTENSIFIED ON,
       100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: /1 TEXT-CLT, SY-MANDT, SY-SYSID, SBUKRS+3(4).
WRITE: TEXT-002 UNDER TEXT-TTL, SDATVR+3(8) COLOR 4 INTENSIFIED ON,
       TEXT-003, SDATVR+11(8) COLOR 4 INTENSIFIED ON.
WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
SKIP.

ULINE.
WRITE: /1 TEXT-PRN, 16 TEXT-005, 30 TEXT-006, 40 TEXT-007,
       75 TEXT-010, 90 TEXT-011, 105 TEXT-008, 120 TEXT-009.
ULINE.

*___________________
* Main Processing
*   NAST is the main table you are inquiring on to see if a PO was sent
*
*___________________

INITIALIZATION.

START-OF-SELECTION.


    SELECT * FROM NAST
    WHERE OBJKY > '4500000000                    '
      AND OBJKY < '4599999999                    '
      AND KSCHL = 'NEU'                 "Indicates a New PO
      AND VSTAT = '1'                   " Indicates the PO is complete
      AND OBJKY NE PRV-OBJKY            " don't count PO twice
      AND NACHA IN SNACHA               " 1 is print/zfaz , 6 is EDI
      AND PARNR IN SPARNR               " Vendor selection
      AND DATVR IN SDATVR.              " Date selection
       SELECT SINGLE * FROM EKPO WHERE
                EBELN = NAST-OBJKY
                AND BUKRS IN SBUKRS.
            IF SY-SUBRC = 0.
      PRV-OBJKY = NAST-OBJKY.
      PERFORM BUILD_SAVE_TABLE.         "builds the data to output
             ENDIF.
    ENDSELECT.

      PRV-OBJKY = '0000000000'.

  PERFORM BUILD_GOOD_TABLE.

*___    where objky > '4500000000                    '
*___      and objky < '4599999999                    '
*___         and kschl = 'NEU '
*___         and vstat = '1'
*___         and objky ne prv-objky
*___         and nacha in snacha
*___         and datvr in sdatvr.
*___         prv-objky = nast-objky.
*__    perform build_save_table.      " builds the data table to output
*___  endselect.
*___endif.


PERFORM COUNT-LINE-ITEMS.         "tally up the PO line items and Reqs
PERFORM BUILD_BIG_TABLE.          " Formats the report
*_______________
SORT SAVE_POREQ BY BEDNR.
LOOP AT SAVE_POREQ.       " counts all Purchase Reqs used in PO creates
  AT NEW BEDNR.
     TPOREQ = TPOREQ + 1.
  ENDAT.
ENDLOOP.
SKIP.
WRITE: /   'Total Line Items           =', TPOITEM.
WRITE: /   'Total Purchase Orders      =', TPONUM.
WRITE: /   '      Total Manual Orders       =', TPOMANUAL.
WRITE: /   '      Total Automatic Orders    =', TPOAUTO.
SKIP.
WRITE: /   '      Total Printed Orders      =', TPOPRINT.
WRITE: /   '      Total EDI Orders          =', TPOEDI.
WRITE: /   '      Total ZFAX Orders         =', TPOZFAX.

* ---------
NEW-PAGE.
ULINE.
ULINE.
WRITE:/   '    ********************  R E P O R T   T O T A L S  ******'.
ULINE.
SKIP.
     TDAYS = TDAYS + 1.
WRITE: /   'Total days                 =', TDAYS.
WRITE: /   'Total Purchase Requistions =', TPOREQ.
WRITE: /   'Total Line Items           =', POITEM.
WRITE: /   'Total Purchase Orders      =', PONUM.
WRITE: /   '      Total Manual Orders       =', POMANUAL.
WRITE: /   '      Total Automatic Orders    =', POAUTO.
SKIP.
WRITE: /   '      Total Printed Orders      =', POPRINT.
WRITE: /   '      Total EDI Orders          =', POEDI.
WRITE: /   '      Total ZFAX Orders         =', POZFAX.
SKIP.
ULINE.
******* end program

*__________________
*    Form build_save_table
*___________________
*
*
*
*_____________________

FORM BUILD_SAVE_TABLE.
  CLEAR SAVE_TABLE2.
*_____move nast fields to the data table

  MOVE:
       NAST-OBJKY      TO   SAVE_TABLE2-OBJKY,
       NAST-PARNR      TO   SAVE_TABLE2-PARNR,
       NAST-MANUE      TO   SAVE_TABLE2-MANUE,
       NAST-DATVR      TO   SAVE_TABLE2-DATVR,
       NAST-UHRVR      TO   SAVE_TABLE2-UHRVR,
       NAST-NACHA      TO   SAVE_TABLE2-NACHA.
*__________ pick up the vendor name for the data table
  SELECT SINGLE * FROM LFA1 WHERE
          LIFNR = SAVE_TABLE2-PARNR.
          SAVE_TABLE2-NAME1 = LFA1-NAME1.

*_____if the printer is ZFAX change the sent flag to Fax.

  IF NAST-LDEST = 'ZFAX'.
     SAVE_TABLE2-NACHA = '2'.
  ENDIF.

  APPEND SAVE_TABLE2.

ENDFORM.

*_____________________
FORM BUILD_GOOD_TABLE.
  SORT SAVE_TABLE2 BY OBJKY DATVR UHRVR.
  LOOP AT SAVE_TABLE2.
    IF PRV-OBJKY NE SAVE_TABLE2-OBJKY.
       MOVE SAVE_TABLE2 TO SAVE_TABLE1.
       APPEND SAVE_TABLE1.
    ENDIF.
   PRV-OBJKY = SAVE_TABLE2-OBJKY.

ENDLOOP.

ENDFORM.
*_____________________

FORM COUNT-LINE-ITEMS.
*__________count the purchase order line items and Purchase Reqs used
*________  to create the Purchase order

  SORT SAVE_TABLE1 BY OBJKY.

  LOOP AT SAVE_TABLE1.

  SELECT * FROM EKPO
    WHERE EBELN EQ SAVE_TABLE1-OBJKY.

      SELECT * FROM EBAN WHERE EBELN = EKPO-EBELN
               AND EBELP = EKPO-EBELP.
*___               and bednr = ekpo-bednr.
                IF EBAN-BANFN NE PRV-BEDNR.
                  READ TABLE SAVE_TABLE1 WITH KEY EKPO-EBELN.
                  SAVE_TABLE1-POREQ = SAVE_TABLE1-POREQ + 1.
*_____         write:  /  ' mm', ekpo-ebeln, ekpo-ebelp, ekpo-bednr.
                  MODIFY SAVE_TABLE1 INDEX SY-TABIX.
*__
                  CLEAR SAVE_POREQ.
                  SAVE_POREQ-BEDNR = EBAN-BANFN.
                  APPEND SAVE_POREQ.
                  PRV-BEDNR = EBAN-BANFN.

                ENDIF.
            ENDSELECT.
      READ TABLE SAVE_TABLE1 WITH KEY EKPO-EBELN.
      SAVE_TABLE1-POITEM = SAVE_TABLE1-POITEM + 1.
      MODIFY SAVE_TABLE1 INDEX SY-TABIX.

    ENDSELECT.
ENDLOOP.
ENDFORM.

*_____________________
*_____________________

*_________________________
*    form build_big_table
*__________________________
*
*___________________________
FORM BUILD_BIG_TABLE.
*______write:    'build final'.

  SORT SAVE_TABLE1 BY DATVR PARNR OBJKY.

  LOOP  AT SAVE_TABLE1.

*___     at new datvr.
      IF SAVE_TABLE1-DATVR NE PRV-DATVR.
       IF FIRST-FLAG = 1.
        SKIP.
*_____        write: /   'Total Purchase Requistions =', tporeq.
        WRITE: /   'Total Line Items           =', TPOITEM.
        WRITE: /   'Total Purchase Orders      =', TPONUM.
        WRITE: /   '      Total Manual Orders       =', TPOMANUAL.
        WRITE: /   '      Total Automatic Orders    =', TPOAUTO.
        SKIP.
        WRITE: /   '      Total Printed Orders      =', TPOPRINT.
        WRITE: /   '      Total EDI Orders          =', TPOEDI.
        WRITE: /   '      Total ZFAX Orders         =', TPOZFAX.
        NEW-PAGE.
        CLEAR:   TPOITEM,
                 TPONUM,
                 TPOMANUAL,
                 TPOAUTO,
                 TPOPRINT,
                 TPOEDI,
                 TPOZFAX.
       TDAYS = TDAYS + 1.
      ENDIF.
    ENDIF.
*__     endat.
            FIRST-FLAG = 1.
*___      write: / ' T  ', save_table1-datvr, save_table1-poitem,
*__              save_table1-nacha.
            PRV-DATVR = SAVE_TABLE1-DATVR.
            POITEM = POITEM + SAVE_TABLE1-POITEM.
            PONUM  = PONUM  + 1.
            TPOITEM = TPOITEM + SAVE_TABLE1-POITEM.
            TPONUM = TPONUM + 1.
     IF SAVE_TABLE1-MANUE EQ '  '.
              POAUTO = POAUTO + 1.
              TPOAUTO = TPOAUTO + 1.
     ELSE.
              POMANUAL = POMANUAL + 1.
              TPOMANUAL = TPOMANUAL + 1.
     ENDIF.

     IF SAVE_TABLE1-NACHA EQ '1'.
             POPRINT = POPRINT + 1.
             TPOPRINT = TPOPRINT + 1.
     ELSEIF SAVE_TABLE1-NACHA EQ '2'.
               POZFAX = POZFAX + 1.
               TPOZFAX = TPOZFAX + 1.
         ELSEIF SAVE_TABLE1-NACHA EQ '6'.
                 POEDI = POEDI + 1.
                 TPOEDI = TPOEDI + 1.
         ENDIF.

SKIP.
WRITE:  /1  SAVE_TABLE1-DATVR,
         16 SAVE_TABLE1-OBJKY,   30 SAVE_TABLE1-PARNR,
         40 SAVE_TABLE1-NAME1,   80 SAVE_TABLE1-MANUE,
         95 SAVE_TABLE1-NACHA,   105 SAVE_TABLE1-POITEM,
         120 SAVE_TABLE1-POREQ.

ENDLOOP.
ENDFORM.
