REPORT ZMMMR006 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  ZMMMR006  - CONSTRUCTION MATERIAL - BY MATERIAL #
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
************************************************************************
* 98/08/27 raarssen - Excluded materials from the catalogue with MM/PP
*    status of 01, 02 or 04 and where quantity on hand (Company Wide) is
*    zero
*
* 97/10/17 md7140#235 Eliminate N/A from printing in secondary descript.
*
* 97/03/13 md7140 increase font size by reducing white space
************************************************************************

TABLES: MARC, MARD.

INCLUDE ZNMMM001 .

DATA: CHECK-PLANT    TYPE I,           "added for checking qty 98/08/27
      CHECK-STORAGE  TYPE I.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS:  P_CLASS     LIKE   KLAH-CLASS OBLIGATORY
             DEFAULT 'CONSTRUCT_MATERIAL'.
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.

************************************************************************
TOP-OF-PAGE.
IF HEADER1 = 'Y'.
WRITE: / SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 70 TEXT-002.
WRITE: 160 SY-REPID COLOR COL_NEGATIVE.
WRITE: /1 SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 73 TEXT-012.
WRITE: 160 'PAGE:' INTENSIFIED OFF.
WRITE: 166(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE.
WRITE: /.
FORMAT COLOR COL_NORMAL.

WRITE: /1 TEXT-003, 20 TEXT-005, 52 TEXT-003, 75 TEXT-007.
WRITE: 105 TEXT-009, 137 TEXT-006.

WRITE: /1 TEXT-004, 75 TEXT-008,  105 TEXT-008, 137 TEXT-004.
ULINE.
WRITE: /.
ENDIF.

*----------------------------------------------------------------------*
*     HEADER2 - This header is used for the index
*-----------------------------------------------------------------------
IF HEADER2 = 'Y'.
WRITE: / SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 70 TEXT-013.
WRITE: 160 SY-REPID COLOR COL_NEGATIVE.
WRITE: /1 SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 72 TEXT-012.
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

MOVE 'N' TO CHECKPOINT1.
MOVE 'Y' TO CHECKPOINT2.
* Perform GET_MATNR is used to retrieve all material numbers under
* the class entered in the selection screen.
PERFORM GET_MATNR USING P_CLASS.

*--- 1998/08/27 --------------------------------------------------------
  LOOP AT TABLE1.
    MOVE 0            TO CHECK-PLANT.
    MOVE 0            TO CHECK-STORAGE.
    SELECT * FROM MARC
      WHERE MATNR = TABLE1-MATNR AND MMSTA IN ('01', '02', '04').
      IF SY-SUBRC = 0.
        MOVE 1 TO CHECK-PLANT.
        SELECT SINGLE * FROM MARD
          WHERE MATNR = TABLE1-MATNR AND LABST > 0.
          IF SY-SUBRC = 0.
            MOVE 1 TO CHECK-STORAGE.
          ENDIF.
      ENDIF.
    ENDSELECT.
    IF CHECK-PLANT = 1 AND CHECK-STORAGE = 0.
      DELETE TABLE1.
    ENDIF.
  ENDLOOP.
*-----------------------------------------------------------------------
SORT TABLE1 BY MATNR ASCENDING.

* This routine will populate the table TEMPTABLE with records that
* have a KEYWORD as a value.
LOOP AT TABLE1.
     CLEAR:   TABLE2, TABLE4.
     REFRESH: TABLE2, TABLE4.
     MOVE 'N' TO MULTI_VALUE_IND.
     PERFORM GET_CHARS USING TABLE1-MATNR.
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
      WHEN KEYW_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD2.
      WHEN MATL_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD3.
      WHEN PRID_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD4.
      WHEN SECD_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD5.
      WHEN MODL_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD6.
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
      WHEN KEYW_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD2.
      WHEN MATL_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD3.
      WHEN PRID_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD4.
      WHEN SECD_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD5.
      WHEN MODL_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD6.
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
        WRITE: /20 TABLE2-FIELD2.
        MOVE SY-LINNO TO FIELD2-LINE.
        PERFORM LASTLINE USING FIELD2-LINE.
           ELSEIF TABLE2-FIELD3 NE SPACE.
           SY-LINNO = FIELD3-LINE.
           WRITE: /52 TABLE2-FIELD3.
           MOVE SY-LINNO TO FIELD3-LINE.
           PERFORM LASTLINE USING FIELD3-LINE.
              ELSEIF TABLE2-FIELD4 NE SPACE.
              SY-LINNO = FIELD4-LINE.
              WRITE: /75 TABLE2-FIELD4.
              MOVE SY-LINNO TO FIELD4-LINE.
              PERFORM LASTLINE USING FIELD4-LINE.
                 ELSEIF TABLE2-FIELD5 NE SPACE.
                 SY-LINNO = FIELD5-LINE.
                 WRITE: /75 TABLE2-FIELD5.
                 MOVE SY-LINNO TO FIELD5-LINE.
                 PERFORM LASTLINE USING FIELD5-LINE.
                    ELSEIF TABLE2-FIELD6 NE SPACE.
                    SY-LINNO = FIELD6-LINE.
                    WRITE: /137 TABLE2-FIELD6.
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
WRITE: /1 TABLE4-MATNR, 20 TABLE4-FIELD2, 52 TABLE4-FIELD3.
*write: 75 table4-field4, 105 table4-field5, 137 table4-field6.
WRITE: 75 TABLE4-FIELD4, 137 TABLE4-FIELD6.
IF TABLE4-FIELD5(3) NE 'N/A'.                                  "#235
   WRITE: TABLE4-FIELD5 UNDER TEXT-009.                        "#235
ENDIF.

IF MULTI_VALUE_IND EQ 'Y'.
   PERFORM PROCESS_MULTI_VALUE_TABLE.
ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*         MOVE_TO_TEMP
*-----------------------------------------------------------------------
FORM MOVE_TO_TEMP.
ENDFORM.

FORM BIG_MOVE_SECOND.
   MOVE TABLE1-MATNR TO TABLE4-MATNR.
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
