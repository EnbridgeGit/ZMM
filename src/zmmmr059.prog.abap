REPORT ZMMMR013 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  ZMMMR013  - METER & REGULATOR CATALOG 2: MANU.PART #
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  October 23, 1996
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
*
* 2003/03/13 mdemeest ____ Copied ZMMMR013 as basis
*                          Add check to remove all obsolete material
*                          Sort by material #
************************************************************************
*This INCLUDE will bring in the DATA DEFINITIONS common in most catalogs
Tables: marc.           "Plant - used to check for OBSOLETE material

INCLUDE ZNMMM001 .

DATA   : BEGIN OF VALIDCLASSES OCCURS 1,
            CLSTYPE1      LIKE KLAH-CLINT,
            CLSTYPE2      LIKE KLAH-CLINT,
            CLSTYPE3      LIKE KLAH-CLINT,
            CLSTYPE4      LIKE KLAH-CLINT,
            CLSTYPE5      LIKE KLAH-CLINT,
            CLSTYPE6      LIKE KLAH-CLINT,
            CLSTYPE7      LIKE KLAH-CLINT,
            CLSTYPE8      LIKE KLAH-CLINT,
            CLSTYPE9      LIKE KLAH-CLINT,
            CLSTYPE10     LIKE KLAH-CLINT,
            CLSTYPE11     LIKE KLAH-CLINT,
         END OF VALIDCLASSES.

DATA   : BEGIN OF VALID-RECORDS OCCURS 10000.
             INCLUDE STRUCTURE TABLE1.
DATA   : END OF VALID-RECORDS.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS:  P_CLASS     LIKE   KLAH-CLASS OBLIGATORY
             DEFAULT 'METR_AND_REGULATOR'.
SELECTION-SCREEN END OF BLOCK BOX1.

selection-screen begin of block box2 with frame.
select-options: s_mmsta for marc-mmsta no intervals.
selection-screen end of block box2.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.

************************************************************************
TOP-OF-PAGE.
IF HEADER1 = 'Y'.                             "md7140 reformat of header
   WRITE: / TEXT-RPT, SY-REPID COLOR COL_NEGATIVE, 65 TEXT-002,
        140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
        TEXT-012 UNDER TEXT-002,
        TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
   ULINE.
   WRITE: /.
   FORMAT COLOR COL_NORMAL.

   WRITE: /1 TEXT-010, 22 TEXT-017, 39 TEXT-005, 70 TEXT-007,
          100 TEXT-009, 132 TEXT-003.
   WRITE: / TEXT-006 UNDER TEXT-010,
            TEXT-011 UNDER TEXT-017,
            TEXT-008 UNDER TEXT-007,
            TEXT-008 UNDER TEXT-009,
            TEXT-004 UNDER TEXT-003.

   ULINE.
   WRITE: /.
ENDIF.

*-----------------------------------------------------------------------
*  HEADER2 - This header is used for the index
*-----------------------------------------------------------------------
IF HEADER2 = 'Y'.
   WRITE: / TEXT-RPT, SY-REPID COLOR COL_NEGATIVE, 65 TEXT-013,
        140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
        TEXT-012 UNDER TEXT-002,
        TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
   ULINE.
   FORMAT COLOR COL_NORMAL.
   WRITE: / TEXT-014, 8 DISPLAY_CLASS.
   ULINE: /1(35).
   WRITE: /.
   WRITE: /20 TEXT-015, 76 TEXT-016.
   ULINE: /19(11), 75(6).
ENDIF.
********************************* MAIN *********************************
START-OF-SELECTION.
* This routine will call the subroutine that is responsible for
* retrieving the internal characteristic number for each character.
PERFORM CHAR_INT_NUM.

* This Form will get the internal class number for the sub-classes that
* are valid.
PERFORM FILL-VALIDCLASSES.

* This Form will retrieve all material numbers under the class entered
* at the selection screen.
PERFORM GET_MATNR USING P_CLASS.

* -- 2003/03/14 mdemeest -----------------------------------------------
* There must be at least one non-obsolete material at the plant level
* for the material to remain on the report.
loop at table1.
   perform remove_obsolete_material.
endloop.
*-----------------------------------------------------------------------
SORT TABLE1 BY MATNR ASCENDING.
MOVE 'Y' TO CHECKPOINT1.

* This part will check TABLE1's material numbers to see if the material
* numbers are under the sub-class that are valid for this report.
LOOP AT TABLE1.
   PERFORM CHECK-VALID-RECORDS.
ENDLOOP.

REFRESH: TABLE1, TABLE2, TABLE3, TABLE4, TEMPTABLE.

* This routine will find all the records that do not have a
* MANUFACTURER PART NUMBER value and populate the TEMPTABLE.
LOOP AT VALID-RECORDS.
   PERFORM FIND_BLANKS USING VALID-RECORDS-MATNR.
ENDLOOP.

LOOP AT VALID-RECORDS.
     CLEAR:   TABLE2, TABLE4.
     REFRESH: TABLE2, TABLE4.
     MOVE 'N' TO MULTI_VALUE_IND.
     PERFORM GET_CHARS USING VALID-RECORDS-MATNR.
ENDLOOP.

MOVE 'N' TO CHECKPOINT1.
MOVE 'Y' TO CHECKPOINT2.
MOVE 'Y' TO CHECKPOINT3.
SORT TEMPTABLE BY SORT1 ASCENDING
                  MATNR ASCENDING.

*This routine will process all the records in the TEMPTABLE for output.
LOOP AT TEMPTABLE.
   PERFORM GET_CHARS USING TEMPTABLE-MATNR.
ENDLOOP.

****************************** SUB-ROUTINES ****************************
* This INCLUDE contains all the data DEFINITIONS that are common amongst
* most of the catalogs.
INCLUDE ZNMMM002.

*-----------------------------------------------------------------------
*         BIG_MOVE
*-----------------------------------------------------------------------
*    THIS SUBROUTINE WILL LOOK AT THE CHARACTERISTIC PAST TO IT AND
*    DETERMINE WHERE IT BELONGS IN THE LAYOUT TO BE OUTPUTTED.  THIS
*    LAYOUT IS USED FOR DISPLAYING A COMPLETE LINE OF VALUES(1ST LINE)
*-----------------------------------------------------------------------
FORM BIG_MOVE USING SUB_DESCRIPTION.
   CASE SUB_DESCRIPTION.
      WHEN MANN_ATINN.
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
*    THIS SUBROUTINE WILL LOOK AT THE CHARACTERISTIC PAST TO IT AND
*    DETERMINE WHERE IT BELONGS IN THE LAYOUT TO BE OUTPUTTED.  THIS
*    LAYOUT IS USED FOR DISPLAYING THE MULTIPLE VALUES FOR A RECORD.
*-----------------------------------------------------------------------
FORM MULTI_VALUE USING SUB_DESCRIPTION.
   CASE SUB_DESCRIPTION.
      WHEN MANN_ATINN.
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
        WRITE: / TABLE2-FIELD2 UNDER TEXT-017.
        MOVE SY-LINNO TO FIELD2-LINE.
        PERFORM LASTLINE USING FIELD2-LINE.
           ELSEIF TABLE2-FIELD3 NE SPACE.
           SY-LINNO = FIELD3-LINE.
           WRITE: / TABLE2-FIELD3 UNDER TEXT-005.
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
                    ELSEIF TABLE2-FIELD6 NE SPACE.
                    SY-LINNO = FIELD6-LINE.
                    WRITE: / TABLE2-FIELD6 UNDER TEXT-003.
                    MOVE SY-LINNO TO FIELD6-LINE.
                    PERFORM LASTLINE USING FIELD6-LINE.
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
         TABLE4-FIELD2 UNDER TEXT-017,
         TABLE4-FIELD3 UNDER TEXT-005,
         TABLE4-FIELD4 UNDER TEXT-007,
*        table4-field5 under text-009,            "#235
         TABLE4-FIELD6+12(6) UNDER TEXT-003.
IF TABLE4-FIELD5(3) NE 'N/A'.                     "#235
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
*    'KEYWORD' IN THE TABLE 'TEMPTABLE'.
*-----------------------------------------------------------------------
FORM MOVE_TO_TEMP.
IF TABLE3-ATINN EQ MANU_ATINN.
   MOVE TEMP_MATNR TO     TEMPTABLE-MATNR.
*   MOVE TABLE3-ATWRT   TO TEMPTABLE-SORT1.
   move table3-atwrt    to temptable-sort2.

   APPEND TEMPTABLE.
   CLEAR TEMPTABLE.
ENDIF.
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
           MOVE BLANK-MATNR TO TEMPTABLE-MATNR.
           APPEND TEMPTABLE.
           CLEAR TEMPTABLE.
       ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*    BIG_MOVE_SECOND
*-----------------------------------------------------------------------
*    This subroutine is called from the subroutine 'BIG_MOVE'.
*    This subroutine is executed only when a DESCRIPTION is matched.
*-----------------------------------------------------------------------
FORM BIG_MOVE_SECOND.
*   MOVE TEMPTABLE-SORT1 TO TABLE4-FIELD1.
   move temptable-sort2  to table4-field1.
   MOVE TEMPTABLE-MATNR TO TABLE4-FIELD6.
   MOVE 'Y' TO CHECKPOINT3.
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
ENDFORM.

*-----------------------------------------------------------------------
*    FILL-VALIDCLASSES
*-----------------------------------------------------------------------
*  This subroutine gets the internal class number from the table KLAH
*  and stores it in the VALIDCLASSES table.  This step is necessary
*  in order to filter the needed sub-classes.
*-----------------------------------------------------------------------
FORM FILL-VALIDCLASSES.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'REGULATOR_PARTS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE1.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'RELIEF_VALVE_PARTS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE2.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'PILOT_REGULTR_PRTS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE3.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'FILTER_PARTS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE4.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'STRAINER_PARTS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE5.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'OVRPRES_SHUTOF_PRT'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE6.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'GAUGE_PARTS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE7.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'ELECT_VL_INTGT_PRT'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE8.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'SURV_INSTR_PT_PRT'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE9.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'METER_PARTS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE10.
APPEND VALIDCLASSES.
ENDFORM.

*-----------------------------------------------------------------------
*    CHECK-VALID-RECORDS
*-----------------------------------------------------------------------
*  This subroutine checks that the records in TABLE1 are the records
*  for this report.  The check is done by checking the internal class
*  number.  If the record meets the criteria then it is appended to the
*  table VALID-RECORDS.
*-----------------------------------------------------------------------
FORM CHECK-VALID-RECORDS.
    IF TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE1 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE2 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE3 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE4 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE5 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE6 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE7 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE8 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE9 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE10.
       MOVE-CORRESPONDING TABLE1 TO VALID-RECORDS.
       APPEND VALID-RECORDS.
       CLEAR VALID-RECORDS.
    ENDIF.
ENDFORM.

form remove_obsolete_material.
data:  delete_material(1) type c.
  move 'Y' to delete_material.
  select * from marc
      where matnr = table1-matnr.
      if ( marc-mmsta in s_mmsta and s_mmsta+3(2) <> '  ' ) .
      else.
         clear delete_material.
      endif.
  endselect.
  if delete_material = 'Y'.
     delete table1.
  endif.
endform.







