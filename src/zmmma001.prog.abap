REPORT ZMMMA001 NO STANDARD PAGE HEADING LINE-COUNT 65 MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMA001
*    Programmer  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  August 8 1996
*
* This program will identify all those materials that were extended to
* a new storage location via the automatic creation of storage location
* facilty and populate the storage location view on the MRP 3 screen
* with the Sloc MRP indicator of 1.  All storage locations excluding
* A001 will require this indicator.
*-------------------------------  CHANGES  -----------------------------
* 04/05/21 md7140 4.6C Changed BMMH1 length from  2744 to 2746

* 00/07/10 md7140 4.6B Changed DD03P to DD03L. Also changed transfer
*                      lengths.
* 98/05/05 md7140 #--- Changed tables MARA & MARC to MCON for efficiency
*                      & create flat file, use rmmmbim0 to create bdc
*                      and then post
************************************************************************
TABLES: MARA, MCON, MARD, DD03L.

FIELD-SYMBOLS: <F1> .
DATA: T_CODE       LIKE BMM00-TCODE VALUE 'MM01',
      CHAR(21)     TYPE C,
      FIRST_REC(1) TYPE C,
      NODATA(40)   VALUE '////////////////////////////////////////'.

DATA: PHYFILE LIKE FILENAMECI-FILEEXTERN.

DATA    : BEGIN OF Z_BGR00.
        INCLUDE STRUCTURE BGR00.
DATA    : END OF Z_BGR00.

DATA    : BEGIN OF Z_BMM00.
        INCLUDE STRUCTURE BMM00.
DATA    : END OF Z_BMM00.

DATA    : BEGIN OF Z_BMMH1.
        INCLUDE STRUCTURE BMMH1.
DATA    : END OF Z_BMMH1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
PARAMETERS:     L_FILE LIKE FILENAME-FILEEXTERN      "dataset for output
                DEFAULT 'ZMMMA001_01'.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
     S_MATNUM        FOR   MCON-MATNR, "material number
     S_PLANT         FOR   MCON-WERKS, "plant
     S_STORLO        FOR   MARD-LGORT, "storage location
     S_MATGRP        FOR   MCON-MATKL. "material group
SELECTION-SCREEN END OF BLOCK BOX1.

*******************************  MAIN  *********************************
START-OF-SELECTION.
* open & close dataset to remove all data from previous run (0 length).

  PERFORM OPEN_OUTPUT_FILE.
  CLOSE DATASET PHYFILE.

* start processing today's data

  PERFORM INIT_STRUCTURES USING 'BGR00'.  "I_BGR00.
  PERFORM INIT_STRUCTURES USING 'BMM00'.  "I_BGR00.
  PERFORM INIT_STRUCTURES USING 'BMMH1'.  "I_BGR00.

  SELECT * FROM MCON WHERE MATNR IN S_MATNUM
                       AND WERKS IN S_PLANT
                       AND MATKL IN S_MATGRP
                       AND MTART IN ('HAWA', 'HIBE')
                       AND DISMM NE SPACE
                       AND LVORM NE 'X'.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM MARA WHERE MATNR = MCON-MATNR.
      SELECT * FROM MARD WHERE MATNR = MCON-MATNR
                           AND WERKS = MCON-WERKS
                           AND LGORT IN S_STORLO
                           AND LVORM NE 'X'.

        IF MARD-PSTAT NA 'D' OR MARD-PSTAT NA 'L'.
           IF FIRST_REC IS INITIAL.
              PERFORM OPEN_OUTPUT_FILE.            "Material create file
              PERFORM STRUCTURE_BGR00.             "Writes structure
              FIRST_REC = 'X'.
           ENDIF.
          PERFORM STRUCTURE_BMM00.
          PERFORM STRUCTURE_BMMH1.
        ENDIF.                         "end of mard-pstat IF
      ENDSELECT.                       "MARD endselect
    ENDIF.                             "end of sy-subrc IF
  ENDSELECT.                           "MCON endselect

CLOSE DATASET PHYFILE.
*-----------------------  OPEN_OUTPUT_FILE  ----------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_OUTPUT_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            LOGICAL_FILENAME = L_FILE
       IMPORTING
            FILE_NAME        = PHYFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006 WITH L_FILE.
  ELSE.
    OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
  ENDIF.
ENDFORM.

*---------------------- INIT_STRUCTURES ------------------------------*
*  -->  tabname - name of the structire table passed                  *
*---------------------------------------------------------------------*
FORM INIT_STRUCTURES USING TABNAME.    "tab.
  SELECT * FROM DD03L WHERE TABNAME = TABNAME.
    CLEAR CHAR.
    CHAR(2)   = 'Z_'.
    CHAR+2(5) = TABNAME.
    CHAR+7(1) = '-'.
    CHAR+8    = DD03L-FIELDNAME.
    ASSIGN (CHAR) TO <F1>.
    <F1> = NODATA.
  ENDSELECT.
ENDFORM.

*-----------------  STRUCTURE_BGR00  -----------------------------------
* set up initial record for batch
*-----------------------------------------------------------------------
FORM STRUCTURE_BGR00.
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'MRP3-VIEW'    TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  TRANSFER Z_BGR00    TO  PHYFILE LENGTH 167.
ENDFORM.

*-----------------  STRUCTURE_BMM00  -----------------------------------
* set up plant/storage location
*-----------------------------------------------------------------------
FORM STRUCTURE_BMM00.
  MOVE '1'          TO Z_BMM00-STYPE.
  MOVE T_CODE       TO Z_BMM00-TCODE.
  MOVE MCON-MATNR   TO Z_BMM00-MATNR.
  MOVE MCON-WERKS   TO Z_BMM00-WERKS.
  MOVE MARD-LGORT   TO Z_BMM00-LGORT.
  MOVE MARA-MBRSH   TO Z_BMM00-MBRSH.
  MOVE MCON-MTART   TO Z_BMM00-MTART.
  IF MARD-PSTAT NA 'D'.                        "MRP3 view
     MOVE 'X'       TO Z_BMM00-XEID3.
  ENDIF.
  IF MARD-PSTAT NA 'L'.                        "Storage view
     MOVE 'X'       TO Z_BMM00-XEIL1.
  ENDIF.

* TRANSFER Z_BMM00  TO PHYFILE LENGTH 167.
  TRANSFER Z_BMM00  TO PHYFILE LENGTH 199.                        "4.6B


  PERFORM INIT_STRUCTURES USING 'BMM00'. " I_BGR00.

ENDFORM.

*-----------------  STRUCTURE_BMMH1  -----------------------------------
* various views
*-----------------------------------------------------------------------
FORM STRUCTURE_BMMH1.
  MOVE '2'               TO Z_BMMH1-STYPE.
  IF MARD-PSTAT NA 'D'.                        "MRP3 view
     MOVE '1'            TO Z_BMMH1-DISKZ.
  ENDIF.
* TRANSFER Z_BMMH1  TO PHYFILE LENGTH 2241.
* TRANSFER Z_BMMH1  TO PHYFILE LENGTH 2744.                     "4.6B
  TRANSFER Z_BMMH1  TO PHYFILE LENGTH 2746.                     "4.6C


  PERFORM INIT_STRUCTURES USING 'BMMH1'. " I_BGR00.
ENDFORM.
