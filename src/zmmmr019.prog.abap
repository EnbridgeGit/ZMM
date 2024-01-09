REPORT ZMMMR019 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  ZMMMR019  - COMPRESSOR & TECHNI. PARTS: BY MANUFACT.
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  October 31, 1996
*
*    This ABAP will retrieve the Material Number associated with the
*    class entered.  It will also store any Sub-Classes that are
*    found.  This ABAP will call in two #INCLUDES.  One will contain
*    the data structure and the other will contain the sub-routines
*    used get the material number associated with the class entered.
*    The second part of the sub-routine will be to get the
*    characteristics for the material numbers retrieved in the first
*    part of the sub-routine.
************************************************************************
* 97/11/12 md7140 #235 Eliminate printing of N/A in secondary descript
* 97/08/21 md7140 increase font size - DRMM0165
************************************************************************
*THIS INCLUDE WILL BRING IN THE DATA DEFINITIONS COMMON IN MOST CATALOGS
INCLUDE ZNMMM001 .

DATA   : BEGIN OF TEMPTABLE2 OCCURS 10000.
                 INCLUDE STRUCTURE TEMPTABLE.
DATA   : END OF TEMPTABLE2.

DATA   : BEGIN OF TEMPTABLE3 OCCURS 10000.
                 INCLUDE STRUCTURE TEMPTABLE.
DATA   : END OF TEMPTABLE3.

DATA : BEGIN OF BUILDTEMP OCCURS 100000.
           INCLUDE STRUCTURE TEMPTABLE.
DATA : END OF BUILDTEMP.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS:  P_CLASS     LIKE   KLAH-CLASS OBLIGATORY
             DEFAULT 'COMPRESOR & TECHNI'.
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.

*******************************  MAIN  *********************************
TOP-OF-PAGE.
WRITE: / TEXT-RPT, SY-REPID COLOR COL_NEGATIVE, 65 TEXT-002,
     140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
         TEXT-012 UNDER TEXT-002,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
ULINE.
WRITE: /.
FORMAT COLOR COL_NORMAL.

WRITE: /1 TEXT-010, 22 TEXT-005, 53 TEXT-006, 84 TEXT-007,
      114 TEXT-009, 145 TEXT-003.
WRITE: / TEXT-011 UNDER TEXT-010,
         TEXT-008 UNDER TEXT-007,
         TEXT-008 UNDER TEXT-009,
         TEXT-004 UNDER TEXT-003.

ULINE.
WRITE: /.
******************************** MAIN **********************************
START-OF-SELECTION.
* This routine will call the subroutine that is responsible for
* retrieving the internal characteristic number for each character.
PERFORM CHAR_INT_NUM.

* This routine will retrieve the material numbers under the class
* COMPRESSOR_PARTS and move then to Table1.
MOVE 'COMPRESSOR_PARTS' TO P_CLASS.
PERFORM GET_MATNR USING P_CLASS.

*This routine will find all the records that DO NOT have a MANUFACTURER
*PART NUMBER AND POPULATE THE TEMPTABLE3 TABLE.
LOOP AT TABLE1.
   PERFORM FIND_BLANKS USING TABLE1-MATNR.
ENDLOOP.

* This routine moves the records from TABLE1 to TEMPTABLE where more
* records from TECHNICIAN_PARTS material numbers will later be added.
LOOP AT TABLE1.
    MOVE TABLE1-MATNR   TO TEMPTABLE-MATNR.
    MOVE TABLE1-MASTER  TO TEMPTABLE-CLASS.
    APPEND TEMPTABLE.
    CLEAR TEMPTABLE.
ENDLOOP.

REFRESH TABLE1.
CLEAR   TABLE1.

* This routine will retrieve the material numbers under the class
* TECHNICIAN_PARTS and move then to Table1.
MOVE 'TECHNICIAN_PARTS' TO P_CLASS.
PERFORM GET_MATNR USING P_CLASS.

*This routine will find all the records that DO NOT have a MANUFACTURER
*PART NUMBER AND POPULATE THE TEMPTABLE3 TABLE.
LOOP AT TABLE1.
   PERFORM FIND_BLANKS USING TABLE1-MATNR.
ENDLOOP.

* This routine will move the TECHNICIAN_PARTS material numbers from
* Table1 to TEMPTABLE.
LOOP AT TABLE1.
    MOVE TABLE1-MATNR   TO TEMPTABLE-MATNR.
    MOVE TABLE1-MASTER  TO TEMPTABLE-CLASS.
    APPEND TEMPTABLE.
    CLEAR TEMPTABLE.
ENDLOOP.

SORT TEMPTABLE BY MATNR ASCENDING.
MOVE 'Y'   TO  CHECKPOINT1.

REFRESH: TABLE2, TABLE3, TABLE4.

*THIS ROUTINE WILL FIND ALL THE RECORDS THAT DO NOT HAVE A MANUFACTURER
*PART NUMBER AND POPULATE THE TEMPTABLE.
LOOP AT TEMPTABLE.
   PERFORM FIND_BLANKS USING TEMPTABLE-MATNR.
ENDLOOP.

SORT TEMPTABLE2 BY MATNR ASCENDING.

LOOP AT TEMPTABLE2.
    MOVE TEMPTABLE2-MATNR  TO BUILDTEMP-MATNR.
    APPEND BUILDTEMP.
    CLEAR  BUILDTEMP.
ENDLOOP.

REFRESH TEMPTABLE2.
CLEAR   TEMPTABLE2.

*THIS ROUTINE WILL POPULATE TABLE TEMPTABLE2 WITH RECORDS THAT HAVE A
*MANUFACTURER'S PART NUMBER.
LOOP AT TEMPTABLE.
CLEAR:   TABLE2, TABLE4, TEMPTABLE2.
     PERFORM GET_CHARS USING TEMPTABLE-MATNR.
     REFRESH TABLE3.
     CLEAR TABLE3.
ENDLOOP.

MOVE 'N' TO CHECKPOINT1.
MOVE 'Y' TO CHECKPOINT2.
MOVE 'Y' TO CHECKPOINT3.

SORT TEMPTABLE2 BY SORT1 ASCENDING
                   MATNR ASCENDING.

LOOP AT TEMPTABLE2.
    MOVE TEMPTABLE2-MATNR  TO BUILDTEMP-MATNR.
    MOVE TEMPTABLE2-SORT1  TO BUILDTEMP-SORT1.
    APPEND BUILDTEMP.
    CLEAR  BUILDTEMP.
ENDLOOP.

REFRESH TEMPTABLE2.
CLEAR   TEMPTABLE2.

SORT TEMPTABLE3 BY MATNR ASCENDING.

LOOP AT TEMPTABLE3.
    MOVE TEMPTABLE3-MATNR  TO TEMPTABLE2-MATNR.
    APPEND TEMPTABLE2.
    CLEAR  TEMPTABLE2.
ENDLOOP.
REFRESH TEMPTABLE3.

LOOP AT BUILDTEMP.
    MOVE BUILDTEMP-MATNR  TO TEMPTABLE2-MATNR.
    MOVE BUILDTEMP-SORT1  TO TEMPTABLE2-SORT1.
    APPEND TEMPTABLE2.
    CLEAR  TEMPTABLE2.
ENDLOOP.

*THIS ROUTINE WILL PROCESS ALL THE RECORDS IN THE TEMPTABLE FOR OUTPUT.
LOOP AT TEMPTABLE2.
   PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
ENDLOOP.

****************************** SUB-ROUTINES ****************************
*THIS INCLUDE CONTAINS ALL THE DATA DEFINITIONS THAT ARE COMMON AMONGST
*MOST OF THE CATALOGS.
INCLUDE ZNMMM002.

*-----------------------------------------------------------------------
*         BIG_MOVE
*-----------------------------------------------------------------------
*    THIS SUBROUTINE WILL LOOK AT THE CHARACTERISTIC PASTED TO IT AND
*    DETERMINE WHERE IT BELONGS IN THE LAYOUT TO BE OUTPUTTED.  THIS
*    LAYOUT IS USED FOR DISPLAYING A COMPLETE LINE OF VALUES(1ST LINE)
*-----------------------------------------------------------------------
FORM BIG_MOVE USING SUB_DESCRIPTION.
   CASE SUB_DESCRIPTION.
      WHEN APPL_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD2.
      WHEN KEYW_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD3.
      WHEN PRID_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD4.
      WHEN SECD_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD5.
   ENDCASE.
ENDFORM.
*-----------------------------------------------------------------------
*         MULTI_VALUE
*-----------------------------------------------------------------------
*    THIS SUBROUTINE WILL LOOK AT THE CHARACTERISTIC PASTED TO IT AND
*    DETERMINE WHERE IT BELONGS IN THE LAYOUT TO BE OUTPUTTED.  THIS
*    LAYOUT IS USED FOR DISPLAYING THE MULTIPLE VALUES FOR A RECORD.
*-----------------------------------------------------------------------
FORM MULTI_VALUE USING SUB_DESCRIPTION.
   CASE SUB_DESCRIPTION.
      WHEN APPL_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD2.
      WHEN KEYW_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD3.
      WHEN PRID_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD4.
      WHEN SECD_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD5.
   ENDCASE.
APPEND TABLE2.
CLEAR TABLE2.
ENDFORM.

*-----------------------------------------------------------------------
*         PROCESS_MULTI_VALUE_TABLE
*-----------------------------------------------------------------------
*    THIS SUBROUTINE WILL HANDLE THE OUTPUTTING OF THE MULTIPLE VALUES
*    UNDERNEATH THE PROPER COLUMN HEADINGS.
*-----------------------------------------------------------------------
FORM PROCESS_MULTI_VALUE_TABLE.
FIELD2-LINE = FIELD3-LINE = FIELD4-LINE = FIELD5-LINE = SY-LINNO.
FIELD6-LINE = SY-LINNO.

     LOOP AT TABLE2.
     IF TABLE2-FIELD2 NE SPACE.
        SY-LINNO = FIELD2-LINE.
        WRITE: / TABLE2-FIELD2 UNDER TEXT-005.
        MOVE SY-LINNO TO FIELD2-LINE.
        PERFORM LASTLINE USING FIELD2-LINE.
           ELSEIF TABLE2-FIELD3 NE SPACE.
           SY-LINNO = FIELD3-LINE.
           WRITE: / TABLE2-FIELD3 UNDER TEXT-006.
           MOVE SY-LINNO TO FIELD3-LINE.
           PERFORM LASTLINE USING FIELD3-LINE.
              ELSEIF TABLE2-FIELD4 NE SPACE.
              SY-LINNO = FIELD4-LINE.
              WRITE: / TABLE2-FIELD4 UNDER TEXT-007.
              MOVE SY-LINNO TO FIELD4-LINE.
              PERFORM LASTLINE USING FIELD4-LINE.
                 ELSEIF TABLE2-FIELD5 NE SPACE.
                 SY-LINNO = FIELD5-LINE.
                 WRITE: / TABLE2-FIELD5 UNDER TEXT-009.
                 MOVE SY-LINNO TO FIELD5-LINE.
                 PERFORM LASTLINE USING FIELD5-LINE.
      ENDIF.
      MOVE 'N' TO MULTI_VALUE_IND.
     ENDLOOP.
      MOVE 'N' TO MULTI_VALUE_IND.
      CLEAR TABLE2.
      REFRESH TABLE2.
ENDFORM.
*-----------------------------------------------------------------------
*         LASTLINE
*-----------------------------------------------------------------------
*    THIS SUBROUTINE IS CALLED FROM 'PROCESS_MULTI_VALUE_TABLE'.  IT
*    CHECKS AND STORES THE LINE POSITION OF THE MULTIPLE VALUES BEING
*    OUTPUTED.  THIS IS NECESSARY INORDER FOR THE NEXT RECORD TO BE
*    PRINTED ON THE PROPER LINE AND TO AVOID THE OVER LAYING OF RECORDS.
*-----------------------------------------------------------------------
FORM LASTLINE USING SENTLINE.
     IF SENTLINE > NEXT-LINE.
         MOVE SENTLINE TO NEXT-LINE.
     ENDIF.
ENDFORM.
*-----------------------------------------------------------------------
*         OUTPUT
*-----------------------------------------------------------------------
*    THIS SUBROUTINE IS CALLED FROM THE INCLUDE 'ZNMMM002'. THIS
*    SUBROUTINE WILL PRINT THE FIRST LINE FOR A RECORD AND THEN WILL
*    CALL THE SUBROUTINE PROCESS 'MULTI_VALUE_TABLE' TO HANDLE THE
*    PRINTING OF THE MULTIPLE VALUES.
*-----------------------------------------------------------------------
FORM OUTPUT.
WRITE: /.
WRITE: / TABLE4-FIELD1 UNDER TEXT-010,
         TABLE4-FIELD2 UNDER TEXT-005,
         TABLE4-FIELD3 UNDER TEXT-006,
         TABLE4-FIELD4 UNDER TEXT-007,
*        table4-field5 under text-009,         "#235
         TABLE4-FIELD6+12(6) UNDER TEXT-003.
IF TABLE4-FIELD5(3) NE 'N/A'.                  "Secondary Descript  #235
   WRITE: TABLE4-FIELD5 UNDER TEXT-009.
ENDIF.

IF MULTI_VALUE_IND EQ 'Y'.
   PERFORM PROCESS_MULTI_VALUE_TABLE.
ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*         MOVE_TO_TEMP
*-----------------------------------------------------------------------
*    THIS SUBROUTINE IS USED TO STORE THE MATERIAL NUMBER AND
*    MANUFACTURER PART NUMBER IN THE TABLE 'TEMPTABLE'.
*-----------------------------------------------------------------------
FORM MOVE_TO_TEMP.
IF TABLE3-ATINN EQ MANU_ATINN.
   MOVE TEMP_MATNR     TO  TEMPTABLE2-MATNR.
   MOVE TABLE3-ATWRT   TO  TEMPTABLE2-SORT1.
   APPEND TEMPTABLE2.
   CLEAR TEMPTABLE2.
ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*    BIG_MOVE_SECOND
*-----------------------------------------------------------------------
*    This subroutine is called from the subroutine 'BIG_MOVE'.
*    This subroutine is executed only when a DESCRIPTION is matched.
*-----------------------------------------------------------------------
FORM BIG_MOVE_SECOND.
   MOVE 'Y' TO CHECKPOINT3.
   MOVE TEMPTABLE2-SORT1 TO TABLE4-FIELD1.
   MOVE TEMPTABLE2-MATNR TO TABLE4-FIELD6.
ENDFORM.

*-----------------------------------------------------------------------
*    MULTI_VALUE_SECOND
*-----------------------------------------------------------------------
*    This subroutine is called from the subroutine 'MULTI_VALUE'.
*    This subroutine is executed only when a DESCRIPTION is matched.
*-----------------------------------------------------------------------
FORM MULTI_VALUE_SECOND.
   MOVE 'Y' TO CHECKPOINT3.
   MOVE 'Y' TO MULTI_VALUE_IND.
   MOVE TEMP_MATNR TO TABLE2-FIELD6.
ENDFORM.

*-----------------------------------------------------------------------
*         FIND_BLANKS
*-----------------------------------------------------------------------
*    THIS SUBROUTINE IS USED TO STORE THE MATERIAL NUMBER IN THE TABLE
*    'TEMPTABLE'.  ONLY MATERIAL'S WITHOUT MANUFACTURER'S PART NUMBER
*    ARE MOVED TO THE 'TEMPTABLE'.
*-----------------------------------------------------------------------
FORM FIND_BLANKS USING BLANK-MATNR.
DATA : SEARCHKEY   LIKE   CABN-ATINN.
SELECT SINGLE * FROM CABN WHERE ATNAM = 'MANUFACTURER_PART_NUMBER'.
             MOVE CABN-ATINN TO SEARCHKEY.

       SELECT * FROM AUSP WHERE OBJEK = BLANK-MATNR
                            AND ATINN = SEARCHKEY.
       ENDSELECT.

       IF SY-SUBRC NE 0.
           MOVE BLANK-MATNR TO TEMPTABLE3-MATNR.
           APPEND TEMPTABLE3.
           CLEAR TEMPTABLE3.
       ENDIF.
ENDFORM.
