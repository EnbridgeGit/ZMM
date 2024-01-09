REPORT ZMMMR020 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  ZMMMR020 - COMPRESOR & TECHNI. PART: BY KEY,APP,MANU
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  November 05, 1996
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
*This INCLUDE will bring in the DATA DEFINITIONS common in most catalogs
INCLUDE ZNMMM001 .

DATA    : BEGIN OF BUILDTEMP OCCURS 10000.
                  INCLUDE STRUCTURE TEMPTABLE.
DATA    : END OF BUILDTEMP.

DATA    : BEGIN OF BUILDTEMP2 OCCURS 10000.
                  INCLUDE STRUCTURE BUILDTEMP.
DATA    : END OF BUILDTEMP2.

DATA    : BEGIN OF BUILDTEMP3 OCCURS 10000.
                  INCLUDE STRUCTURE BUILDTEMP.
DATA    : END OF BUILDTEMP3.

DATA    : BEGIN OF TEMPTABLE2 OCCURS 10000.
                  INCLUDE STRUCTURE TEMPTABLE.
DATA    : END OF TEMPTABLE2.

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
             DEFAULT 'COMPRESOR & TECHNI'.
SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 34(30) TEXT-901.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 34(30) TEXT-902.

************************************************************************
TOP-OF-PAGE.
IF HEADER1 = 'Y'.
  WRITE: / TEXT-RPT, SY-REPID COLOR COL_NEGATIVE, 65 TEXT-002,
       140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
           TEXT-012 UNDER TEXT-002,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
  ULINE.
  WRITE: /.
  FORMAT COLOR COL_NORMAL.

  WRITE: /1 TEXT-006,  32 TEXT-005, 63 TEXT-010, 84 TEXT-007,
        114 TEXT-009, 146 TEXT-003.
  WRITE: / TEXT-011 UNDER TEXT-010,
           TEXT-008 UNDER TEXT-007,
           TEXT-008 UNDER TEXT-009,
           TEXT-004 UNDER TEXT-003.

  ULINE.
  WRITE: /.
ENDIF.
WRITE: /.
*-----------------------------------------------------------------------
*  HEADER2 is used for the index
*-----------------------------------------------------------------------
IF HEADER2 = 'Y'.
  WRITE: / TEXT-RPT, SY-REPID COLOR COL_NEGATIVE, 65 TEXT-013,
       140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
           TEXT-012 UNDER TEXT-013,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
  ULINE.
  WRITE: /50 TEXT-015, 90 TEXT-016.
  ULINE: /49(11), 89(6).

ENDIF.
********************************* MAIN *********************************
START-OF-SELECTION.
* This routine will call the subroutine that is responsible for
* retrieving the internal characteristic number for each character.
PERFORM CHAR_INT_NUM.

* This routine will retrieve the material numbers under the class
* COMPRESSOR_PARTS and move then to Table1.
MOVE 'COMPRESSOR_PARTS' TO P_CLASS.
PERFORM GET_MATNR USING P_CLASS.

* This routine moves the records from TABLE1 to TEMPTABLE where more
* records from TECHNICIAN_PARTS material numbers will later be added.
LOOP AT TABLE1.
    MOVE TABLE1-MATNR   TO TEMPTABLE2-MATNR.
    MOVE TABLE1-MASTER  TO TEMPTABLE2-CLASS.
    APPEND TEMPTABLE2.
    CLEAR TEMPTABLE2.
ENDLOOP.

* This routine will retrieve the material numbers under the class
* TECHNICIAN_PARTS and move then to Table1.* This routine will move the
MOVE 'TECHNICIAN_PARTS' TO P_CLASS.
PERFORM GET_MATNR USING P_CLASS.

* This routine will move the TECHNICIAN_PARTS material numbers from
* Table1 to TEMPTABLE.
LOOP AT TABLE1.
    MOVE TABLE1-MATNR   TO TEMPTABLE2-MATNR.
    MOVE TABLE1-MASTER  TO TEMPTABLE2-CLASS.
    APPEND TEMPTABLE2.
    CLEAR TEMPTABLE2.
ENDLOOP.

SORT TEMPTABLE2 BY MATNR ASCENDING.
MOVE 'Y'   TO  CHECKPOINT1.

REFRESH: TABLE1, TABLE2, TABLE3, TABLE4.

* This routine will populate the table BUILDTEMP with records that
* have a KEYWORD as a value.
LOOP AT TEMPTABLE2.
CLEAR:   TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
     REFRESH TABLE3.
     CLEAR TABLE3.
ENDLOOP.

SORT BUILDTEMP BY SORT1  ASCENDING
                  MATNR  ASCENDING.

MOVE 'Y' TO CHECKPOINT4.

* This routine will populate the table BUILDTEMP2 with records that
* have a APPLICATION as a value (INCLUDING SORT1).
LOOP AT BUILDTEMP.
CLEAR:   TABLE2, TABLE4.
     PERFORM GET_CHARS USING BUILDTEMP-MATNR.
     REFRESH TABLE3.
     CLEAR TABLE3.
ENDLOOP.

SORT BUILDTEMP2 BY SORT1  ASCENDING
                   MATNR  ASCENDING
                   SORT2  ASCENDING.

REFRESH BUILDTEMP.

MOVE 'N' TO CHECKPOINT4.
MOVE 'Y' TO CHECKPOINT5.

SORT BUILDTEMP2 BY SORT1 ASCENDING
                   SORT2 ASCENDING
                   MATNR ASCENDING.


* This routine will populate the table BUILDTEMP3 with records that
* have a MANUFACTURER_PART_NUMBER as a value(INCLUDING SORT1 AND SORT2).
LOOP AT BUILDTEMP2.
CLEAR:   TABLE2, TABLE4.
     PERFORM GET_CHARS USING BUILDTEMP2-MATNR.
     REFRESH TABLE3.
     CLEAR TABLE3.
ENDLOOP.

REFRESH BUILDTEMP2.

MOVE 'N' TO CHECKPOINT5.
MOVE 'Y' TO CHECKPOINT6.

LOOP AT BUILDTEMP3.
CLEAR:   TABLE2, TABLE4.
     PERFORM GET_CHARS USING BUILDTEMP3-MATNR.
     REFRESH TABLE3.
     CLEAR TABLE3.
ENDLOOP.

MOVE 'N' TO CHECKPOINT6.
MOVE 'Y' TO CHECKPOINT7.

REFRESH BUILDTEMP3.

LOOP AT BUILDTEMP2.
CLEAR:   TABLE2, TABLE4.
     PERFORM GET_CHARS USING BUILDTEMP2-MATNR.
     REFRESH TABLE3.
     CLEAR TABLE3.
ENDLOOP.

REFRESH BUILDTEMP2.
SORT BUILDTEMP3 BY SORT1 ASCENDING
                   SORT2 ASCENDING
                   MATNR ASCENDING
                   SORT3 ASCENDING.

*This routine will process all the records in the TEMPTABLE for output.
LOOP AT BUILDTEMP3.
   PERFORM OUTPUT.
ENDLOOP.

PERFORM INDEX.

****************************** SUB-ROUTINES ****************************
*THIS INCLUDE CONTAINS ALL THE DATA DEFINITIONS THAT ARE COMMON AMONGST
*MOST OF THE CATALOGS.
INCLUDE ZNMMM002.

*-----------------------------------------------------------------------
*   INDEX - This subroutine creates the index for the catalogue
*-----------------------------------------------------------------------
FORM INDEX.
MOVE 'N' TO HEADER1.
MOVE 'Y' TO HEADER2.

SORT TABLE5 BY FIELD1 ASCENDING
               FIELD2 ASCENDING.

NEW-PAGE.
WRITE: /.
WRITE: TEXT-018, 17 TEXT-019.
ULINE: /1(48).
WRITE: /.

LOOP AT TABLE5.
MOVE TABLE5-FIELD1 TO ALPHABIT.

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
*   CASE SUB_DESCRIPTION.
*      WHEN 'APPLICATION'.
*          PERFORM BIG_MOVE_SECOND.
*          MOVE TABLE3-ATWRT TO TABLE4-FIELD2.
*      WHEN 'MANUFACTURER_PART_NUMBER'.
*          PERFORM BIG_MOVE_SECOND.
*          MOVE TABLE3-ATWRT TO TABLE4-FIELD3.
*      WHEN 'PRIMARY_DESCRIPTION'.
*          PERFORM BIG_MOVE_SECOND.
*          MOVE TABLE3-ATWRT TO TABLE4-FIELD4.
*      WHEN 'SECONDARY_DESCRIPTION'.
*          PERFORM BIG_MOVE_SECOND.
*          MOVE TABLE3-ATWRT TO TABLE4-FIELD5.
*   ENDCASE.
ENDFORM.
*-----------------------------------------------------------------------
*         MULTI_VALUE
*-----------------------------------------------------------------------
*    THIS SUBROUTINE WILL LOOK AT THE CHARACTERISTIC PASTED TO IT AND
*    DETERMINE WHERE IT BELONGS IN THE LAYOUT TO BE OUTPUTTED.  THIS
*    LAYOUT IS USED FOR DISPLAYING THE MULTIPLE VALUES FOR A RECORD.
*-----------------------------------------------------------------------
FORM MULTI_VALUE USING SUB_DESCRIPTION.
*   CASE SUB_DESCRIPTION.
*      WHEN 'APPLICATION'.
*          PERFORM MULTI_VALUE_SECOND.
*          MOVE TABLE3-ATWRT TO TABLE2-FIELD2.
*      WHEN 'MANUFACTURER_PART_NUMBER'.
*          PERFORM MULTI_VALUE_SECOND.
*          MOVE TABLE3-ATWRT TO TABLE2-FIELD3.
*      WHEN 'PRIMARY_DESCRIPTION'.
*          PERFORM MULTI_VALUE_SECOND.
*          MOVE TABLE3-ATWRT TO TABLE2-FIELD4.
*      WHEN 'SECONDARY_DESCRIPTION'.
*          PERFORM MULTI_VALUE_SECOND.
*          MOVE TABLE3-ATWRT TO TABLE2-FIELD5.
*   ENDCASE.
*APPEND TABLE2.
*CLEAR TABLE2.
ENDFORM.

*-----------------------------------------------------------------------
*         PROCESS_MULTI_VALUE_TABLE
*-----------------------------------------------------------------------
*    THIS SUBROUTINE WILL HANDLE THE OUTPUTTING OF THE MULTIPLE VALUES
*    UNDERNEATH THE PROPER COLUMN HEADINGS.
*-----------------------------------------------------------------------
*FORM PROCESS_MULTI_VALUE_TABLE.
*FIELD2-LINE = FIELD3-LINE = FIELD4-LINE = FIELD5-LINE = SY-LINNO.
*FIELD6-LINE = SY-LINNO.
*
*     LOOP AT TABLE2.
*     IF TABLE2-FIELD2 NE SPACE.
*        SY-LINNO = FIELD2-LINE.
*        WRITE: /30 TABLE2-FIELD2.
*        MOVE SY-LINNO TO FIELD2-LINE.
*        PERFORM LASTLINE USING FIELD2-LINE.
*           ELSEIF TABLE2-FIELD3 NE SPACE.
*           SY-LINNO = FIELD3-LINE.
*           WRITE: /70 TABLE2-FIELD3.
*           MOVE SY-LINNO TO FIELD3-LINE.
*           PERFORM LASTLINE USING FIELD3-LINE.
*              ELSEIF TABLE2-FIELD4 NE SPACE.
*              SY-LINNO = FIELD4-LINE.
*              WRITE: /120 TABLE2-FIELD4.
*              MOVE SY-LINNO TO FIELD4-LINE.
*              PERFORM LASTLINE USING FIELD4-LINE.
*                 ELSEIF TABLE2-FIELD5 NE SPACE.
*                 SY-LINNO = FIELD5-LINE.
*                 WRITE: /150 TABLE2-FIELD5.
*                 MOVE SY-LINNO TO FIELD5-LINE.
*                 PERFORM LASTLINE USING FIELD5-LINE.
*      ENDIF.
*      MOVE 'N' TO MULTI_VALUE_IND.
*     ENDLOOP.
*      MOVE 'N' TO MULTI_VALUE_IND.
*      CLEAR TABLE2.
*      REFRESH TABLE2.
*ENDFORM.
*-----------------------------------------------------------------------
*         LASTLINE
*-----------------------------------------------------------------------
*    THIS SUBROUTINE IS CALLED FROM 'PROCESS_MULTI_VALUE_TABLE'.  IT
*    CHECKS AND STORES THE LINE POSITION OF THE MULTIPLE VALUES BEING
*    OUTPUTED.  THIS IS NECESSARY INORDER FOR THE NEXT RECORD TO BE
*    PRINTED ON THE PROPER LINE AND TO AVOID THE OVER LAYING OF RECORDS.
*-----------------------------------------------------------------------
*FORM LASTLINE USING SENTLINE.
*     IF SENTLINE > NEXT-LINE.
*         MOVE SENTLINE TO NEXT-LINE.
*     ENDIF.
*ENDFORM.

*-----------------------------------------------------------------------
*         OUTPUT
*-----------------------------------------------------------------------
*    THIS SUBROUTINE IS CALLED FROM THE INCLUDE 'ZNMMM002'. THIS
*    SUBROUTINE WILL PRINT THE FIRST LINE FOR A RECORD AND THEN WILL
*    CALL THE SUBROUTINE PROCESS 'MULTI_VALUE_TABLE' TO HANDLE THE
*    PRINTING OF THE MULTIPLE VALUES.
*-----------------------------------------------------------------------
FORM OUTPUT.

ON CHANGE OF BUILDTEMP3-SORT1.
      WRITE: /.
      WRITE: / BUILDTEMP3-SORT1 UNDER TEXT-006.
      ULINE: /1(30).
ENDON.

ON CHANGE OF BUILDTEMP3-SORT2.
      WRITE: /.
ENDON.

WRITE: / BUILDTEMP3-SORT3 UNDER TEXT-010.

ON CHANGE OF BUILDTEMP3-SORT2 OR BUILDTEMP3-MATNR.
  WRITE: BUILDTEMP3-SORT2 UNDER TEXT-005,
         BUILDTEMP3-SORT3 UNDER TEXT-010,
         BUILDTEMP3-SORT4 UNDER TEXT-007,
*        buildtemp3-sort5 under text-009,             "#235
         BUILDTEMP3-MATNR+12(6) UNDER TEXT-003.
IF BUILDTEMP3-SORT5(3) NE 'N/A'.                      "#235
   WRITE: BUILDTEMP3-SORT5 UNDER TEXT-009.
ENDIF.
ENDON.

ON CHANGE OF BUILDTEMP3-SORT1.
      MOVE BUILDTEMP3-SORT1    TO   TABLE5-FIELD1.
      MOVE SY-PAGNO            TO   TABLE5-FIELD2.
      APPEND TABLE5.
      CLEAR TABLE5.
ENDON.

ENDFORM.

*-----------------------------------------------------------------------
*         MOVE_TO_TEMP
*-----------------------------------------------------------------------
*    THIS SUBROUTINE IS USED TO STORE THE MATERIAL NUMBER AND
*    'KEYWORD' IN THE TABLE 'TEMPTABLE'.
*-----------------------------------------------------------------------
FORM MOVE_TO_TEMP.
IF CHECKPOINT4 EQ 'Y'.
   IF TABLE3-ATINN EQ APPL_ATINN.
      MOVE TEMP_MATNR        TO   BUILDTEMP2-MATNR.
      MOVE TABLE3-ATWRT      TO   BUILDTEMP2-SORT2.
      MOVE BUILDTEMP-SORT1   TO   BUILDTEMP2-SORT1.
      MOVE BUILDTEMP-CLASS   TO   BUILDTEMP2-CLASS.
      MOVE BUILDTEMP-CLASS2  TO   BUILDTEMP2-CLASS2.
      APPEND BUILDTEMP2.
      CLEAR  BUILDTEMP2.
   ENDIF.
ELSEIF CHECKPOINT5 EQ 'Y'.
   IF TABLE3-ATINN EQ MANU_ATINN.
      MOVE TEMP_MATNR         TO   BUILDTEMP3-MATNR.
      MOVE TABLE3-ATWRT       TO   BUILDTEMP3-SORT3.
      MOVE BUILDTEMP2-SORT1   TO   BUILDTEMP3-SORT1.
      MOVE BUILDTEMP2-SORT2   TO   BUILDTEMP3-SORT2.
      MOVE BUILDTEMP2-CLASS   TO   BUILDTEMP3-CLASS.
      MOVE BUILDTEMP2-CLASS2  TO   BUILDTEMP3-CLASS2.
      APPEND BUILDTEMP3.
      CLEAR  BUILDTEMP3.
   ENDIF.
ELSEIF CHECKPOINT6 EQ 'Y'.
   IF TABLE3-ATINN EQ PRID_ATINN.
      MOVE TEMP_MATNR         TO   BUILDTEMP2-MATNR.
      MOVE TABLE3-ATWRT       TO   BUILDTEMP2-SORT4.
      MOVE BUILDTEMP3-SORT1   TO   BUILDTEMP2-SORT1.
      MOVE BUILDTEMP3-SORT2   TO   BUILDTEMP2-SORT2.
      MOVE BUILDTEMP3-SORT3   TO   BUILDTEMP2-SORT3.
      MOVE BUILDTEMP3-CLASS   TO   BUILDTEMP2-CLASS.
      MOVE BUILDTEMP3-CLASS2  TO   BUILDTEMP2-CLASS2.
      APPEND BUILDTEMP2.
      CLEAR  BUILDTEMP2.
   ENDIF.
ELSEIF CHECKPOINT7 EQ 'Y'.
   IF TABLE3-ATINN EQ SECD_ATINN.
      MOVE TEMP_MATNR         TO   BUILDTEMP3-MATNR.
      MOVE TABLE3-ATWRT       TO   BUILDTEMP3-SORT5.
      MOVE BUILDTEMP2-SORT1   TO   BUILDTEMP3-SORT1.
      MOVE BUILDTEMP2-SORT2   TO   BUILDTEMP3-SORT2.
      MOVE BUILDTEMP2-SORT3   TO   BUILDTEMP3-SORT3.
      MOVE BUILDTEMP2-SORT4   TO   BUILDTEMP3-SORT4.
      MOVE BUILDTEMP2-CLASS   TO   BUILDTEMP3-CLASS.
      MOVE BUILDTEMP2-CLASS2  TO   BUILDTEMP3-CLASS2.
      APPEND BUILDTEMP3.
      CLEAR  BUILDTEMP3.
   ENDIF.
ELSE.
  IF TABLE3-ATINN EQ KEYW_ATINN.
   MOVE TEMP_MATNR         TO   BUILDTEMP-MATNR.
   MOVE TABLE3-ATWRT       TO   BUILDTEMP-SORT1.
   MOVE TEMPTABLE2-CLASS   TO   BUILDTEMP-CLASS.
   MOVE TEMPTABLE2-CLASS2  TO   BUILDTEMP-CLASS2.
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
*FORM BIG_MOVE_SECOND.
*   MOVE 'Y' TO CHECKPOINT3.
*   MOVE BUILDTEMP3-SORT1 TO TABLE4-FIELD1.
*   MOVE BUILDTEMP3-MATNR TO TABLE4-FIELD6.
*ENDFORM.

*-----------------------------------------------------------------------
*    MULTI_VALUE_SECOND
*-----------------------------------------------------------------------
*    This subroutine is called from the subroutine 'MULTI_VALUE'.
*    This subroutine is executed only when a DESCRIPTION is matched.
*-----------------------------------------------------------------------
*FORM MULTI_VALUE_SECOND.
*   MOVE 'Y' TO CHECKPOINT3.
*   MOVE 'Y' TO MULTI_VALUE_IND.
*   MOVE TEMP_MATNR TO TABLE2-FIELD6.
*ENDFORM.
