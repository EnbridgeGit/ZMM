REPORT ZMMMR027 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMMMR027  - CAPITAL ITEM: BY KEYWORD, PRIMARY DESCRI
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
* 97/10/08 md7140 #007 Increase font size
*                 #235 Eliminate printing of N/A in secondary descript.
************************************************************************
*This INCLUDE will bring in the DATA DEFINITIONS common in most catalogs
INCLUDE ZNMMM001 .

DATA    : BEGIN OF BUILDTEMP OCCURS 10000.
                  INCLUDE STRUCTURE TEMPTABLE.
DATA    : END OF BUILDTEMP.

DATA    : BEGIN OF BUILDTEMP2 OCCURS 10000.
                  INCLUDE STRUCTURE BUILDTEMP.
DATA    : END OF BUILDTEMP2.

DATA    : TEMPCLASS         LIKE   KLAH-CLASS,
          TEMPCLASS2        LIKE   KLAH-CLASS,
          TEMPCLINT         LIKE   KLAH-CLINT,
          TEMPCLINT2        LIKE   KLAH-CLINT,
          PARENTCLASS       LIKE   KLAH-CLINT,
          REP_CLASS         LIKE   SWOR-KSCHL,
          REP_SUBCLASS      LIKE   REP_CLASS,
          REP_SUPERIOR      LIKE   REP_CLASS.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS:  P_CLASS     LIKE   KLAH-CLASS OBLIGATORY
             DEFAULT 'CAPITAL_ACQUISITNS'.
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 34(30) TEXT-901.

************************************************************************
TOP-OF-PAGE.
IF HEADER1 = 'Y'.
   WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_GROUP,
          40 TEXT-002, 105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
            TEXT-012 UNDER TEXT-002,
            TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
   ULINE.
   FORMAT COLOR COL_BACKGROUND.

   WRITE: /17 TEXT-006, 43 TEXT-007, 73 TEXT-009, 104 TEXT-010,
          125 TEXT-003.
   WRITE: / TEXT-008 UNDER TEXT-007,
            TEXT-008 UNDER TEXT-009,
            TEXT-011 UNDER TEXT-010,
            TEXT-004 UNDER TEXT-003.
   ULINE.
   WRITE: /.
ENDIF.

*-----------------------------------------------------------------------
*  HEADER2 will be used for the index
*-----------------------------------------------------------------------
IF HEADER2 = 'Y'.
   WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_GROUP,
          80 TEXT-013, 105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
            TEXT-012 UNDER TEXT-013,
            TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
   ULINE.
   FORMAT COLOR COL_BACKGROUND.

WRITE: /50 TEXT-015, 90 TEXT-016.
ULINE: /49(11), 89(6).

ENDIF.
********************************* MAIN *********************************
START-OF-SELECTION.
* This routine will call the subroutine that is responsible for
* retrieving the internal characteristic number for each character.
PERFORM CHAR_INT_NUM.

* This Form will retrieve all material numbers under the class entered
* at the selection screen.
PERFORM GET_MATNR USING P_CLASS.
SORT TABLE1 BY MATNR ASCENDING.
MOVE 'Y'   TO  CHECKPOINT1.

REFRESH: TABLE2, TABLE3, TABLE4, TEMPTABLE.

* This routine will populate the table TEMPTABLE with records that
* have a KEYWORD as a value.
LOOP AT TABLE1.
CLEAR:   TABLE2, TABLE4, TEMPTABLE.
     PERFORM GET_CHARS USING TABLE1-MATNR.
     REFRESH TABLE3.
     CLEAR TABLE3.
ENDLOOP.

SORT BUILDTEMP BY MATNR  ASCENDING
                  SORT1  ASCENDING.

MOVE 'Y' TO CHECKPOINT4.
REFRESH TABLE1.

* This subroutine will retrieve the PRIMARY_DESCRIPTION to be used as
* the second sort field.
LOOP AT BUILDTEMP.
CLEAR:   TABLE2, TABLE4, TEMPTABLE.
     PERFORM GET_CHARS USING BUILDTEMP-MATNR.
     REFRESH TABLE3.
     CLEAR TABLE3.
ENDLOOP.

REFRESH TABLE1.
CLEAR   TABLE1.

SORT BUILDTEMP2 BY CLASS  ASCENDING
                   SORT1  ASCENDING
                   SORT2  ASCENDING.

* This subroutine will build the table TEMPTABLE, which will be used for
* output.
PERFORM BUILDTEMPTABLE.

MOVE 'N' TO CHECKPOINT1.
MOVE 'N' TO CHECKPOINT4.
MOVE 'Y' TO CHECKPOINT2.
MOVE 'Y' TO CHECKPOINT3.

*This routine will process all the records in the TEMPTABLE for output.
LOOP AT TEMPTABLE.
   PERFORM GET_CHARS USING TEMPTABLE-MATNR.
ENDLOOP.

PERFORM INDEX.

****************************** SUB-ROUTINES ****************************
*THIS INCLUDE CONTAINS ALL THE DATA DEFINITIONS THAT ARE COMMON AMONGST
*MOST OF THE CATALOGS.
INCLUDE ZNMMM002.

*-----------------------------------------------------------------------
*    INDEX - This subroutine will create the index.
*-----------------------------------------------------------------------
FORM INDEX.
MOVE 'N' TO HEADER1.
MOVE 'Y' TO HEADER2.

SELECT SINGLE * FROM SWOR WHERE CLINT = SEARCH_CLASS.
MOVE SWOR-KSCHL TO REP_SUPERIOR.
NEW-PAGE.
WRITE: /.
WRITE: TEXT-018, 17 REP_SUPERIOR.
ULINE: /1(35).
WRITE: /.

LOOP AT TABLE5.
MOVE TABLE5-FIELD1 TO ALPHABIT.

   ON CHANGE OF TABLE5-CLASS.
       SELECT SINGLE * FROM SWOR WHERE CLINT = TABLE5-CLASS.
       MOVE SWOR-KSCHL TO DISPLAY_SUBCLASS.
       WRITE: /.
       WRITE: /.
       WRITE: /30 TEXT-017, 42 DISPLAY_SUBCLASS.
       ULINE: /29(12).
   ENDON.

   ON CHANGE OF ALPHABIT.
       WRITE: /.
   ENDON.

WRITE: /50 TABLE5-FIELD1, 90 TABLE5-FIELD2.
ENDLOOP.
ENDFORM.

*-----------------------------------------------------------------------
*         BIG_MOVE
*-----------------------------------------------------------------------
*    THIS SUBROUTINE WILL LOOK AT THE CHARACTERISTIC PASTED TO IT AND
*    DETERMINE WHERE IT BELONGS IN THE LAYOUT TO BE OUTPUTTED.  THIS
*    LAYOUT IS USED FOR DISPLAYING A COMPLETE LINE OF VALUES(1ST LINE)
*-----------------------------------------------------------------------
FORM BIG_MOVE USING SUB_DESCRIPTION.
   CASE SUB_DESCRIPTION.
      WHEN PRID_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD2.
      WHEN SECD_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD3.
      WHEN MANU_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD4.
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
      WHEN PRID_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD2.
      WHEN SECD_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD3.
      WHEN MANU_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD4.
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
        WRITE: /70 TABLE2-FIELD2.
        MOVE SY-LINNO TO FIELD2-LINE.
        PERFORM LASTLINE USING FIELD2-LINE.
           ELSEIF TABLE2-FIELD3 NE SPACE.
           SY-LINNO = FIELD3-LINE.
           WRITE: /100 TABLE2-FIELD3.
           MOVE SY-LINNO TO FIELD3-LINE.
           PERFORM LASTLINE USING FIELD3-LINE.
              ELSEIF TABLE2-FIELD4 NE SPACE.
              SY-LINNO = FIELD4-LINE.
              WRITE: /150 TABLE2-FIELD4.
              MOVE SY-LINNO TO FIELD4-LINE.
              PERFORM LASTLINE USING FIELD4-LINE.
                 ELSEIF TABLE2-FIELD5 NE SPACE.
                 SY-LINNO = FIELD5-LINE.
                 WRITE: /210 TABLE2-FIELD5.
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
   ON CHANGE OF TEMPTABLE-CLASS.
       SELECT SINGLE * FROM SWOR WHERE CLINT = TEMPTABLE-CLASS.
       MOVE SWOR-KSCHL TO REP_SUBCLASS.
       WRITE: /.
       FORMAT COLOR COL_GROUP.
       WRITE: /1 TEXT-014, 8 REP_SUBCLASS.
       FORMAT COLOR COL_NORMAL.
       ULINE: /.
       WRITE: /.
   ENDON.

ON CHANGE OF TABLE4-FIELD1.
      WRITE: /.
*     write: /30 table4-field1.
      WRITE: / TABLE4-FIELD1 UNDER TEXT-006.
      ULINE: /17(30).
ENDON.

WRITE: /.
WRITE: / TABLE4-FIELD2 UNDER TEXT-007,
         TABLE4-FIELD4 UNDER TEXT-010,
         TABLE4-FIELD6+12(6) UNDER TEXT-003.
IF TABLE4-FIELD3(3) NE 'N/A'.
   WRITE:  TABLE4-FIELD3 UNDER TEXT-009.
ENDIF.

ON CHANGE OF TABLE4-FIELD1 OR TEMPTABLE-CLASS OR TEMPTABLE-CLASS2.
      MOVE TABLE4-FIELD1    TO   TABLE5-FIELD1.
      MOVE SY-PAGNO         TO   TABLE5-FIELD2.
      MOVE TEMPTABLE-CLASS  TO   TABLE5-CLASS.
      MOVE TEMPTABLE-CLASS2 TO   TABLE5-CLASS2.
      APPEND TABLE5.
      CLEAR TABLE5.
ENDON.

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
IF CHECKPOINT4 EQ 'Y'.
   IF TABLE3-ATINN EQ PRID_ATINN.
      MOVE TEMP_MATNR        TO   BUILDTEMP2-MATNR.
      MOVE TABLE3-ATWRT      TO   BUILDTEMP2-SORT2.
      MOVE BUILDTEMP-SORT1   TO   BUILDTEMP2-SORT1.
      MOVE BUILDTEMP-CLASS   TO   BUILDTEMP2-CLASS.
      MOVE BUILDTEMP-CLASS2  TO   BUILDTEMP2-CLASS2.
      APPEND BUILDTEMP2.
      CLEAR  BUILDTEMP2.
   ENDIF.
ELSE.
  IF TABLE3-ATINN EQ KEYW_ATINN.
   MOVE TEMP_MATNR     TO   BUILDTEMP-MATNR.
   MOVE TABLE3-ATWRT   TO   BUILDTEMP-SORT1.
   MOVE TABLE1-MASTER  TO   BUILDTEMP-CLASS.
   MOVE TABLE1-CLASS2  TO   BUILDTEMP-CLASS2.
   APPEND BUILDTEMP.
   CLEAR  BUILDTEMP.
  ENDIF.
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
   MOVE TEMPTABLE-SORT1 TO TABLE4-FIELD1.
   MOVE TEMPTABLE-MATNR TO TABLE4-FIELD6.
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
*    BUILDTEMPTABLE
*-----------------------------------------------------------------------
*    This subroutine will build the TEMPTABLE in the proper order so
*    that the output will be sorted in the required fashion.
*-----------------------------------------------------------------------
FORM BUILDTEMPTABLE.
   PERFORM CLASSCLINT USING 'COMPUTERS'.
   PERFORM CLASSCLINT USING 'FURNITUR_AND_EQUIP'.
   PERFORM CLASSCLINT USING 'MOBILE_EQUIPMENT'.
   PERFORM CLASSCLINT USING 'VEHICLES'.
ENDFORM.

*-----------------------------------------------------------------------
*    CLASSCLINT
*-----------------------------------------------------------------------
*    This subroutine will build the TEMPTABLE in the proper order so
*    that the output will be sorted in the required fashion.
*-----------------------------------------------------------------------
FORM CLASSCLINT USING TEMPCLASS.
SELECT SINGLE * FROM KLAH WHERE CLASS = TEMPCLASS.
MOVE KLAH-CLINT TO TEMPCLINT.
LOOP AT BUILDTEMP2.
   IF BUILDTEMP2-CLASS EQ TEMPCLINT.
   MOVE-CORRESPONDING BUILDTEMP2 TO TEMPTABLE.
   APPEND TEMPTABLE.
   CLEAR  TEMPTABLE.
   ENDIF.
ENDLOOP.
ENDFORM.
