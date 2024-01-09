REPORT ZMMMR009 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 80.
************************************************************************
*    Program     :  ZMMMR009  - METER & REGULATOR CATALOG: BY MODEL #
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  October 22, 1996
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
*This INCLUDE will bring in the DATA DEFINITIONS common in most catalogs
INCLUDE ZNMMM001 .

* This internal table is used for storing the internal class number the
* sub-class that are valid.
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

* This is the internal table where all valid records will be stored.
DATA   : BEGIN OF VALID-RECORDS OCCURS 10000.
             INCLUDE STRUCTURE TABLE1.
DATA   : END OF VALID-RECORDS.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS:  P_CLASS     LIKE   KLAH-CLASS OBLIGATORY
             DEFAULT 'METR_AND_REGULATOR'.
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.

************************************************************************
TOP-OF-PAGE.
IF HEADER1 = 'Y'.
WRITE: / SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 110 TEXT-002.
WRITE: 246 SY-REPID COLOR COL_NEGATIVE.
WRITE: /1 SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 113 TEXT-012.
WRITE: 246 'PAGE:' INTENSIFIED OFF.
WRITE: 252(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE.
WRITE: /.
FORMAT COLOR COL_NORMAL.

WRITE: /1 TEXT-006, 40 TEXT-010, 80 TEXT-005, 120 TEXT-007.
WRITE: 150 TEXT-009, 210 TEXT-003.

WRITE: /1 TEXT-004, 120 TEXT-008, 150 TEXT-008, 40 TEXT-011.
WRITE: 210 TEXT-004.
ULINE.
WRITE: /.
ENDIF.

*-----------------------------------------------------------------------
*  HEADER2 will be used for the index
*-----------------------------------------------------------------------
IF HEADER2 = 'Y'.
WRITE: / SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 110 TEXT-013.
WRITE: 246 SY-REPID COLOR COL_NEGATIVE.
WRITE: /1 SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 112 TEXT-012.
ULINE.
WRITE: /.
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
SORT TABLE1 BY MATNR ASCENDING.
MOVE 'Y' TO CHECKPOINT1.

* This part will check TABLE1's material numbers to see if the material
* numbers are under the sub-class that are valid for this report.
LOOP AT TABLE1.
   PERFORM CHECK-VALID-RECORDS.
ENDLOOP.

REFRESH: TABLE1, TABLE2, TABLE3, TABLE4, TEMPTABLE.

* This routine will find all the records that do not have a MODEL NUMBER
* value and populate the TEMPTABLE.
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
        WRITE: /40 TABLE2-FIELD2.
        MOVE SY-LINNO TO FIELD2-LINE.
        PERFORM LASTLINE USING FIELD2-LINE.
           ELSEIF TABLE2-FIELD3 NE SPACE.
           SY-LINNO = FIELD3-LINE.
           WRITE: /80 TABLE2-FIELD3.
           MOVE SY-LINNO TO FIELD3-LINE.
           PERFORM LASTLINE USING FIELD3-LINE.
              ELSEIF TABLE2-FIELD4 NE SPACE.
              SY-LINNO = FIELD4-LINE.
              WRITE: /120 TABLE2-FIELD4.
              MOVE SY-LINNO TO FIELD4-LINE.
              PERFORM LASTLINE USING FIELD4-LINE.
                 ELSEIF TABLE2-FIELD5 NE SPACE.
                 SY-LINNO = FIELD5-LINE.
                 WRITE: /150 TABLE2-FIELD5.
                 MOVE SY-LINNO TO FIELD5-LINE.
                 PERFORM LASTLINE USING FIELD5-LINE.
                    ELSEIF TABLE2-FIELD6 NE SPACE.
                    SY-LINNO = FIELD6-LINE.
                    WRITE: /210 TABLE2-FIELD6.
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
WRITE: /1 TABLE4-FIELD1, 40 TABLE4-FIELD2, 80 TABLE4-FIELD3.
WRITE: 120 TABLE4-FIELD4, 150 TABLE4-FIELD5, 210 TABLE4-FIELD6+12(6).


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
IF TABLE3-ATINN EQ MODL_ATINN.
   MOVE TEMP_MATNR TO     TEMPTABLE-MATNR.
   MOVE TABLE3-ATWRT   TO TEMPTABLE-SORT1.
   APPEND TEMPTABLE.
   CLEAR TEMPTABLE.
ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*         FIND_BLANKS
*-----------------------------------------------------------------------
*    THIS SUBROUTINE IS USED TO STORE THE MATERIAL NUMBER IN THE TABLE
*    'TEMPTABLE'.  ONLY MATERIAL'S WITHOUT 'MODEL NUMBER' ARE MOVED
*    TO THE 'TEMPTABLE'.
*-----------------------------------------------------------------------
FORM FIND_BLANKS USING BLANK-MATNR.
DATA : SEARCHKEY   LIKE   CABN-ATINN.
SELECT SINGLE * FROM CABN WHERE ATNAM = 'MODEL_NUMBER'.
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
   MOVE TEMPTABLE-SORT1 TO TABLE4-FIELD1.
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
*   MOVE TABLE1-MATNR TO TABLE2-MATNR.
ENDFORM.

*-----------------------------------------------------------------------
*    FILL-VALIDCLASSES
*-----------------------------------------------------------------------
*  This subroutine gets the internal class number from the table KLAH
*  and stores it in the VALIDCLASSES table.  This step is necessary
*  in order to filter the needed sub-classes.
*-----------------------------------------------------------------------
FORM FILL-VALIDCLASSES.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'REGULATORS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE1.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'RELIEF_VALVES'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE2.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'PILOT_REGULATORS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE3.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'FILTERS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE4.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'STRAINERS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE5.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'OVRPRESSUR_SHUTOFF'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE6.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'GAUGES'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE7.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'ELECTRNC_VL_INTGRT'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE8.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'SURVEY_INSTRUM_PT'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE9.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'METERS'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE10.
SELECT SINGLE * FROM KLAH WHERE CLASS EQ 'STRGHTG_VNE_SPL_PC'.
   MOVE KLAH-CLINT TO VALIDCLASSES-CLSTYPE11.
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
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE10 OR
       TABLE1-MASTER EQ VALIDCLASSES-CLSTYPE11.
       MOVE-CORRESPONDING TABLE1 TO VALID-RECORDS.
       APPEND VALID-RECORDS.
       CLEAR VALID-RECORDS.
    ENDIF.
ENDFORM.
