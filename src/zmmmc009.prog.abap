REPORT ZMMMC009 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMC009
*    PROGRAMMER  :  Marv Radsma - OMNILOGIC SYSTEMS GROUP
*    Client      :  Union Gas Limited
*    Date        :  February 1998
*
* This ABAP will setup the necessary structure used by the load program
* RMMMBIM0.  The ABAP will process all plant and material combinations
* to create material consumption records.
************************************************************************

TABLES:   MARA,                         " material master
          MVER,                         " material consumtion
          DD03L.                        " data dictionary view

FIELD-SYMBOLS: <F1> .
DATA:     T_CODE     LIKE BMM00-TCODE VALUE 'MM02',
          CHAR(21)   TYPE C,
          NODATA(40) VALUE '////////////////////////////////////////',
          PHYFILE    LIKE FILENAMECI-FILEEXTERN,
          IDX(2)     TYPE N.

DATA:    BEGIN OF IMVER OCCURS 5000,
           MATNR     LIKE MVER-MATNR,                 " material number
           WERKS     LIKE MVER-WERKS,                 " plant code
           GJAHR     LIKE MVER-GJAHR,                 " fiscal year
           mgv01     like mver-mgv01,                 " period 01 amount
           mgv02     like mver-mgv02,                 " period 02 amount
           mgv03     like mver-mgv03,                 " period 03 amount
           mgv04     like mver-mgv04,                 " period 04 amount
           mgv05     like mver-mgv05,                 " period 05 amount
           mgv06     like mver-mgv06,                 " period 06 amount
           mgv07     like mver-mgv07,                 " period 07 amount
           mgv08     like mver-mgv08,                 " period 08 amount
           mgv09     like mver-mgv09,                 " period 09 amount
           mgv10     like mver-mgv10,                 " period 10 amount
           mgv11     like mver-mgv11,                 " period 11 amount
           mgv12     like mver-mgv12,                 " period 12 amount
           mgv13     like mver-mgv13,                 " period 13 amount
         END OF IMVER.

DATA:    BEGIN OF UPDTAB OCCURS 1000,
           MATNR     LIKE MARA-MATNR,                 " material number
           MTART     LIKE MARA-MTART,                 " material type
           WERKS     LIKE MVER-WERKS,                 " plant code
           PRIOD     LIKE BMMH4-PRIOD,                " fiscal period
           MGVBR     LIKE BMMH4-MGVBR,                " corrected amount
         END OF UPDTAB.

DATA:    BEGIN OF Z_BGR00.
           INCLUDE STRUCTURE BGR00.
DATA:    END OF Z_BGR00.

DATA:    BEGIN OF Z_BMM00.
           INCLUDE STRUCTURE BMM00.
DATA:    END OF Z_BMM00.

DATA:    BEGIN OF Z_BMMH4.
           INCLUDE STRUCTURE BMMH4.
DATA:    END OF Z_BMMH4.

DATA:    BEGIN OF Z_BMMH1.
           INCLUDE STRUCTURE BMMH1.
DATA:    END OF Z_BMMH1.

INCLUDE <ICON>.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(30) TEXT-001.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    SELECT-OPTIONS: S_WERKS    FOR MVER-WERKS OBLIGATORY
                               DEFAULT 'P200' TO 'P299'.
    SELECT-OPTIONS: S_MATNR    FOR MARA-MATNR.
    SELECT-OPTIONS: S_MATKL    FOR MARA-MATKL.
  SELECTION-SCREEN END OF BLOCK BOX2.

  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
    SELECT-OPTIONS: S_GJAHR    FOR MVER-GJAHR OBLIGATORY
                               DEFAULT SY-DATUM(4) TO SY-DATUM(4).
  SELECTION-SCREEN END OF BLOCK BOX3.

  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
    PARAMETERS: P_OUTFIL       LIKE FILENAME-FILEINTERN OBLIGATORY
                               DEFAULT 'ZMMMC009_01'.
  SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
*   Extract required data

START-OF-SELECTION.

* display message indicating start of program
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Begin of search for Plant/Material consumption'
       EXCEPTIONS
            OTHERS = 1.

* get the material consumption data
  select matnr  werks  gjahr  mgv01
         mgv02  mgv03  mgv04  mgv05
         mgv06  mgv07  mgv08  mgv09
         mgv10  mgv11  mgv12  mgv13
  FROM   MVER
  INTO   TABLE IMVER
  WHERE  MATNR IN S_MATNR
  AND    WERKS IN S_WERKS
  and    gjahr in s_gjahr.

* release rollback segments and reset lock
  COMMIT WORK.

* display progess message
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Validate selected materials in material group'
       EXCEPTIONS
            OTHERS = 1.

* ensure that the material group is as per the selection screen
  LOOP AT IMVER.
    SELECT SINGLE * FROM MARA
    WHERE  MATNR EQ IMVER-MATNR
    AND    MATKL IN S_MATKL.

    IF SY-SUBRC = 0.
      PERFORM CHECK_AMOUNTS.
    ENDIF.

  ENDLOOP.

* release rollback segments and reset lock and delete work table
  COMMIT WORK.
  REFRESH IMVER.

* display progress message
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'All Material/Plants have been found'
       EXCEPTIONS
            OTHERS = 1.

* open the output file
  PERFORM OPEN_OUTPUT_FILE.

* initialize the structures
  PERFORM INIT_STRUCTURES USING 'BGR00'.
  PERFORM INIT_STRUCTURES USING 'BMM00'.
  PERFORM INIT_STRUCTURES USING 'BMMH1'.
  PERFORM INIT_STRUCTURES USING 'BMMH4'.

* output the BGR00 batch header record
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'ZMM_CONSUMP ' TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  TRANSFER Z_BGR00    TO  PHYFILE LENGTH 167.

* sort the table
  SORT UPDTAB BY MATNR WERKS.

* display progress message
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Building the output file'
       EXCEPTIONS
            OTHERS = 1.

* now loop through the table of records created above and output a BMMH4
* record for each entry in the table.  For each new combination of plant
* and material output a BMM00 document header record
  LOOP AT UPDTAB.

    AT NEW WERKS.
      MOVE '1'          TO Z_BMM00-STYPE.             " transaction hdr
      MOVE T_CODE       TO Z_BMM00-TCODE.
      MOVE UPDTAB-MATNR TO Z_BMM00-MATNR.
      MOVE 'U'          TO Z_BMM00-MBRSH.
      MOVE UPDTAB-MTART TO Z_BMM00-MTART.
      MOVE UPDTAB-WERKS TO Z_BMM00-WERKS.
      MOVE 'X'          TO Z_BMM00-XEIB1.
      TRANSFER Z_BMM00  TO PHYFILE LENGTH 167.
      PERFORM INIT_STRUCTURES USING 'BMM00'.
      MOVE '2'          TO Z_BMMH1-STYPE.             " main data rec
      TRANSFER Z_BMMH1  TO PHYFILE LENGTH 2241.
      PERFORM INIT_STRUCTURES USING 'BMMH1'.
    ENDAT.

    MOVE '5'            TO Z_BMMH4-STYPE.             " consumption rec
    MOVE UPDTAB-PRIOD   TO Z_BMMH4-PRIOD.
    MOVE UPDTAB-MGVBR   TO Z_BMMH4-MGVBR.
    TRANSFER Z_BMMH4    TO PHYFILE LENGTH 91.
    PERFORM INIT_STRUCTURES USING 'BMMH4'.

  ENDLOOP.

* close the dataset
  CLOSE DATASET PHYFILE.

  MESSAGE I100 WITH 'Processing has completed'.
END-OF-SELECTION.

*-----------------------------------------------------------------------
*   FORM OPEN_OUTPUT_FILE.
*-----------------------------------------------------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_OUTPUT_FILE.

  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            LOGICAL_FILENAME = P_OUTFIL
       IMPORTING
            FILE_NAME        = PHYFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006 WITH P_OUTFIL.
  ELSE.
    OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM INIT_STRUCTURES                                          *
*---------------------------------------------------------------------*
*  -->  TABNAME - NAME OF THE STRUCTURE TABLE PASSED                  *
*---------------------------------------------------------------------*
FORM INIT_STRUCTURES USING TABNAME.

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

*---------------------------------------------------------------------*
*       FORM CHECK_AMOUNTS                                            *
*---------------------------------------------------------------------*
* check all 13 mgvnn amount fields on MVER and save the data if it is *
* a non-zero value in an internal table.  The 2nd character of the    *
* plant code is changed from a '2', to a '3' to represent Centra      *
*---------------------------------------------------------------------*
FORM CHECK_AMOUNTS.

  IDX = 0.
  DO 13 TIMES.

    IDX = IDX + 1.
    CLEAR CHAR.
    char(9)   = 'IMVER-mgv'.
    CHAR+9(2) = IDX.
    ASSIGN (CHAR) TO <F1>.

    IF <F1> NE 0.
      MOVE IMVER-MATNR TO UPDTAB-MATNR.
      MOVE MARA-MTART  TO UPDTAB-MTART.
      MOVE IMVER-WERKS TO UPDTAB-WERKS.
      UPDTAB-WERKS+1(1) = '3'.
      UPDTAB-PRIOD(4)   = IMVER-GJAHR.
      UPDTAB-PRIOD+4(2) = IDX.
      UPDTAB-MGVBR      = <F1>.
      APPEND UPDTAB.
      CLEAR UPDTAB.

    ENDIF.
  ENDDO.

ENDFORM.

*---------------------------------------------------------------------*
*       THIS IS THE END, MY FREIND                                    *
*---------------------------------------------------------------------*
