REPORT ZMMMR025 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMMMR025  - MM: CAPITAL ITEM: BY MATERIAL NUMBER
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  October 25, 1996
*
*    This ABAP will retrieve the Material Number associated with the
*    class entered.  It will also store any Sub-Classes that are
*    found.  This ABAP will call in two #INCLUDES.  One will contain
*    the data structure and the other will contain the sub-routines
*    used get the material number associated with the class entered.
*    The second part of the sub-routine will be to get the
*    characteristics for the material numbers retrieved in the first
************************************************************************
* 97/10/08 md7140 #007 Increase font size
*                 #235 Eliminate printing of N/A in secondary descript.
************************************************************************
INCLUDE ZNMMM001 .

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS:  P_CLASS     LIKE   KLAH-CLASS OBLIGATORY
             DEFAULT 'CAPITAL_ACQUISITNS'.
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.

*******************************  MAIN  *********************************
TOP-OF-PAGE.
WRITE: / TEXT-RPT, SY-REPID COLOR COL_NEGATIVE, 50 TEXT-002,    "Title
     105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
         TEXT-012 UNDER TEXT-002,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
ULINE.
WRITE: / TEXT-003, 20 TEXT-006, 52 TEXT-007, 82 TEXT-009, 115 TEXT-010.
WRITE: / TEXT-004 UNDER TEXT-003,
         TEXT-008 UNDER TEXT-007,
         TEXT-008 UNDER TEXT-009,
         TEXT-011 UNDER TEXT-010.
ULINE.
WRITE: /.
FORMAT COLOR COL_NORMAL.

************************************************************************
START-OF-SELECTION.
* This routine will call the subroutine that is responsible for
* retrieving the internal characteristic number for each character.
PERFORM CHAR_INT_NUM.

MOVE 'N' TO CHECKPOINT1.
MOVE 'Y' TO CHECKPOINT2.

* This Form will retrieve all material numbers under the class entered
* at the selection screen.
PERFORM GET_MATNR USING P_CLASS.
SORT TABLE1 BY MATNR ASCENDING.

*This routine will process all the records in the TEMPTABLE for output.
LOOP AT TABLE1.
    CLEAR:   TABLE2, TABLE4.
    REFRESH: TABLE2, TABLE4.
    MOVE 'N' TO MULTI_VALUE_IND.
    PERFORM GET_CHARS USING TABLE1-MATNR.
ENDLOOP.

****************************** SUB-ROUTINES ****************************
INCLUDE ZNMMM002.

*-----------------------------------------------------------------------
*     BIG_MOVE
*-----------------------------------------------------------------------
*    This subroutine will look at the characteristics pasted to it and
*    determine where it belongs in the layout to be outputed.  This
*    layout is used for displaying a complete line of values (1st line).
*-----------------------------------------------------------------------
FORM BIG_MOVE USING SUB_DESCRIPTION.
   CASE SUB_DESCRIPTION.
      WHEN KEYW_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD2.
      WHEN PRID_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD3.
      WHEN SECD_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD4.
      WHEN MANU_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD5.
   ENDCASE.
ENDFORM.

*-----------------------------------------------------------------------
*     MULTI_VALUE
*-----------------------------------------------------------------------
*    This subroutine will look at the characteristics pasted to it and
*    determine where it belongs in the layout to be outputed.  This
*    layout is used for displaying the multiple values for a record.
*-----------------------------------------------------------------------
FORM MULTI_VALUE USING SUB_DESCRIPTION.
   CASE SUB_DESCRIPTION.
      WHEN KEYW_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD2.
      WHEN PRID_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD3.
      WHEN SECD_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD4.
      WHEN MANU_ATINN.
          PERFORM MULTI_VALUE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE2-FIELD5.
   ENDCASE.
APPEND TABLE2.
CLEAR TABLE2.
ENDFORM.

*-----------------------------------------------------------------------
*     PROCESS_MULTI_VALUE_TABLE
*-----------------------------------------------------------------------
*    This subroutine will handle the outputting of the multiple values
*    underneath the proper column headings.
*-----------------------------------------------------------------------
FORM PROCESS_MULTI_VALUE_TABLE.
FIELD2-LINE = FIELD3-LINE = FIELD4-LINE = FIELD5-LINE = SY-LINNO.
FIELD6-LINE = SY-LINNO.

     LOOP AT TABLE2.
     IF TABLE2-FIELD2 NE SPACE.
        SY-LINNO = FIELD2-LINE.
        WRITE: / TABLE2-FIELD2 UNDER TEXT-006.
*       write: /40 table2-field2.
        MOVE SY-LINNO TO FIELD2-LINE.
        PERFORM LASTLINE USING FIELD2-LINE.
           ELSEIF TABLE2-FIELD3 NE SPACE.
           SY-LINNO = FIELD3-LINE.
           WRITE: / TABLE2-FIELD3 UNDER TEXT-007.
*          write: /100 table2-field3.
           MOVE SY-LINNO TO FIELD3-LINE.
           PERFORM LASTLINE USING FIELD3-LINE.
              ELSEIF TABLE2-FIELD4 NE SPACE.
              SY-LINNO = FIELD4-LINE.
              WRITE: / TABLE2-FIELD4 UNDER TEXT-009.
*             write: /130 table2-field4.
              MOVE SY-LINNO TO FIELD4-LINE.
              PERFORM LASTLINE USING FIELD4-LINE.
                 ELSEIF TABLE2-FIELD5 NE SPACE.
                 SY-LINNO = FIELD5-LINE.
                 WRITE: / TABLE2-FIELD5 UNDER TEXT-010.
*                write: /200 table2-field5.
                 MOVE SY-LINNO TO FIELD5-LINE.
                 PERFORM LASTLINE USING FIELD5-LINE.
      ENDIF.
      MOVE 'N' TO MULTI_VALUE_IND.
     ENDLOOP.
      MOVE 'N' TO MULTI_VALUE_IND.
ENDFORM.
*-----------------------------------------------------------------------
*     LASTLINE
*-----------------------------------------------------------------------
*    This subroutine is called from 'PROCESS_MULTI_VALUE_TABLE'.  It
*    checks and stores the line position of the multiple values being
*    outputted.  This is necessary in order for the next record to be
*    printed on the proper line and to avoid the over laying of records.
*-----------------------------------------------------------------------
FORM LASTLINE USING SENTLINE.
     IF SENTLINE > NEXT-LINE.
         MOVE SENTLINE TO NEXT-LINE.
     ENDIF.
ENDFORM.
*-----------------------------------------------------------------------
*    OUTPUT
*-----------------------------------------------------------------------
*    This subroutine is called from the #include 'ZNMMM002'.  This
*    subroutine will print the first line for a record and then will
*    call the subroutine process 'MULTI_VALUE_TABLE' to handle the
*    printing of the multiple values
*-----------------------------------------------------------------------
FORM OUTPUT.
WRITE: /.
WRITE: / TABLE4-MATNR  UNDER TEXT-003,
         TABLE4-FIELD2 UNDER TEXT-006,
         TABLE4-FIELD3 UNDER TEXT-007,
         TABLE4-FIELD5 UNDER TEXT-010.
IF TABLE4-FIELD4(3) NE 'N/A'.                               "md7140 #235
   WRITE: TABLE4-FIELD4 UNDER TEXT-009.
ENDIF.

*write: /1 table4-matnr, 40 table4-field2, 100 table4-field3.
*write: 130 table4-field4, 200 table4-field5.

IF MULTI_VALUE_IND EQ 'Y'.
   PERFORM PROCESS_MULTI_VALUE_TABLE.
ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*    BIG_MOVE_SECOND
*-----------------------------------------------------------------------
*    This subroutine is called from the subroutine 'BIG_MOVE'.
*    This subroutine is executed only when a DESCRIPTION is matched.
*-----------------------------------------------------------------------
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
   MOVE TABLE1-MATNR TO TABLE2-MATNR.
ENDFORM.

*-----------------------------------------------------------------------
*    MOVE_TO_TEMP
*-----------------------------------------------------------------------
*    Ignore this subroutine.   One of the #includes calls this
*    FORM.  When debugging for syntax errors, a error message will
*    appear when this FORM is excluded.  This is only used when the sort
*    key is not the MATERIAL NUMBER.
*-----------------------------------------------------------------------
FORM MOVE_TO_TEMP.
ENDFORM.
