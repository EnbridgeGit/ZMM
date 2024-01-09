REPORT ZMMMR031 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  ZMMMR031 - MM: CONSTRUC. MATNR CATALOG:BY MULTI\DIFF
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  November 11, 1996
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
* 01/04/18 mdemeest #854 Eliminate ' NLA ' from report and any materials
*                        flagged for deletion.
* 97/10/17 md7140   #235 Eliminate printing of N/A in secondary descript
* 97/03/27 md7140   #007 Increase font size by reducing white space
************************************************************************
tables: mard.

INCLUDE ZNMMM001 .


DATA    : BEGIN OF BUILDTEMP OCCURS 10000.
        INCLUDE STRUCTURE TEMPTABLE.
DATA    : END OF BUILDTEMP.

DATA    : BEGIN OF BUILDTEMP2 OCCURS 10000.
        INCLUDE STRUCTURE TEMPTABLE.
DATA    : END OF BUILDTEMP2.

DATA    : BEGIN OF TEMPTABLE2 OCCURS 10000.
        INCLUDE STRUCTURE TEMPTABLE.
DATA    : END OF TEMPTABLE2.

DATA    : BEGIN OF TEMPTABLE4 OCCURS 10000.
        INCLUDE STRUCTURE TEMPTABLE.
DATA    : END OF TEMPTABLE4.

DATA    : BEGIN OF TEMPTABLE3 OCCURS 10000.
        INCLUDE STRUCTURE TEMPTABLE.
DATA    : END OF TEMPTABLE3.


DATA    : TEMPCLASS         LIKE   KLAH-CLASS,
          TEMPCLASS2        LIKE   KLAH-CLASS,
          TEMPCLINT         LIKE   KLAH-CLINT,
          TEMPCLINT2        LIKE   KLAH-CLINT,
          CHECKPOINT8       LIKE   CHECKPOINT7 VALUE 'Y',
          CHECKPOINT9       LIKE   CHECKPOINT8 VALUE 'N',
          CHECKPOINT10      LIKE   CHECKPOINT8 VALUE 'N',
          CHECKPOINT11      LIKE   CHECKPOINT8 VALUE 'N',
          CHECKPOINT12      LIKE   CHECKPOINT8 VALUE 'N',
          PARENTCLASS       LIKE   KLAH-CLINT,
          REP_CLASS         LIKE   SWOR-KSCHL,
          REP_SUBCLASS      LIKE   REP_CLASS,
          REP_SUPERIOR      LIKE   REP_CLASS,
          PRI_DIA_ATINN        LIKE   AUSP-ATINN,
          PRI_WAL_THICK_ATINN  LIKE   AUSP-ATINN,
          GRAD_ATINN           LIKE   AUSP-ATINN,
          CATY_ATINN           LIKE   AUSP-ATINN,
          DESN_TEMP_ATINN      LIKE   AUSP-ATINN,
          COAT_ATINN           LIKE   AUSP-ATINN,
          SECD_DIA_ATINN       LIKE   AUSP-ATINN,
          PRES_ATINN           LIKE   AUSP-ATINN,
          FLNG_ATINN           LIKE   AUSP-ATINN,
          END_ATINN            LIKE   AUSP-ATINN,
          BORE_ATINN           LIKE   AUSP-ATINN,
          SEC_WAL_THICK_ATINN  LIKE   AUSP-ATINN.

* This table is used to temporary store the value of the necessary
* fields that are need to sort on.
DATA    : BEGIN OF SORTTABLE OCCURS 10000,
             FIELD1            LIKE AUSP-ATWRT,
             FIELD2            LIKE AUSP-ATFLV,
             FIELD3            LIKE AUSP-ATWRT,
             FIELD4            LIKE AUSP-ATFLV,
             FIELD5            LIKE AUSP-ATWRT,
             FIELD6            LIKE AUSP-ATWRT,
             FIELD7            LIKE AUSP-ATWRT,
             FIELD8            LIKE AUSP-ATWRT,
             FIELD9            LIKE AUSP-ATFLV,
             FIELD10           LIKE AUSP-ATWRT,
             FIELD11           LIKE AUSP-ATWRT,
             FIELD12           LIKE AUSP-ATWRT,
             FIELD13           LIKE AUSP-ATFLV,
             FIELD14           LIKE AUSP-ATFLV,
*             MATNR             LIKE MARA-MATNR,
*             CLASS             LIKE KLAH-CLINT,
*             CLASS2            LIKE KLAH-CLINT,
          END OF SORTTABLE.

DATA    : BEGIN OF SORTTABLE2 OCCURS 100000,
             SORT1            LIKE AUSP-ATWRT,
             SORT2            LIKE AUSP-ATFLV,
             SORT3            LIKE AUSP-ATWRT,
             SORT4            LIKE AUSP-ATFLV,
             SORT5            LIKE AUSP-ATWRT,
             SORT6            LIKE AUSP-ATWRT,
             SORT7            LIKE AUSP-ATWRT,
             SORT8            LIKE AUSP-ATWRT,
             SORT9            LIKE AUSP-ATFLV,
             SORT10           LIKE AUSP-ATWRT,
             SORT11           LIKE AUSP-ATWRT,
             SORT12           LIKE AUSP-ATWRT,
             SORT13           LIKE AUSP-ATFLV,
             SORT14           LIKE AUSP-ATFLV,
             MATNR            LIKE MARA-MATNR,
             CLASS            LIKE KLAH-CLINT,
             CLASS2           LIKE KLAH-CLINT,
          END OF SORTTABLE2.

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
    WRITE: 77 TEXT-012.
    WRITE: 160 'PAGE:' INTENSIFIED OFF.
    WRITE: 166(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
    ULINE.
    WRITE: /.
    FORMAT COLOR COL_NORMAL.

    WRITE: /1 TEXT-006, 32 TEXT-007, 62 TEXT-009.
    WRITE: 95 TEXT-003, 116 TEXT-010, 145 TEXT-003.

    WRITE: /32 TEXT-008, 62 TEXT-008, 116 TEXT-011, 145 TEXT-004.
    ULINE.
    WRITE: /.
  ENDIF.

*-----------------------------------------------------------------------
*  HEADER2 will be used for the index
*-----------------------------------------------------------------------
  IF HEADER2 = 'Y'.
    WRITE: / SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
    WRITE: 107 TEXT-013.
    WRITE: 246 SY-REPID COLOR COL_NEGATIVE.
    WRITE: /1 SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
    WRITE: 112 TEXT-012.
    ULINE.
    WRITE: /50 TEXT-015, 90 TEXT-016.
    ULINE: /49(11), 89(6).

  ENDIF.
********************************* MAIN *********************************
START-OF-SELECTION.
* This routine will call the subroutine that is responsible for
* retrieving the internal characteristic number for each character.
  PERFORM CHAR_INT_NUM.
  PERFORM CHAR_INT_NUM_CONSTRUCT.

* This Form will retrieve all material numbers under the class entered
* at the selection screen.
  PERFORM GET_MATNR USING P_CLASS.
*---------------------------------------------------#854----- 2001/04/18
* This code deletes any materials flagged for deletion. LVORM = X.
  select single * from mara
    where matnr = table1-matnr
      and lvorm = 'X'.
  if sy-subrc = '0'.
     delete table1.
  endif.
*-----------------------------------------------------------------------
* This code is to eliminate any materials with ' NLA ' in its
* description and quantity of 0.

  loop at table1.
    select single * from mard
       where matnr = table1-matnr
         and labst > 0.
    if sy-subrc <> 0.
       select single * from ausp                     "Get description
          where objek = table1-matnr
          and atinn = secd_atinn.
       if  sy-subrc = 0.
           search ausp-atwrt for 'NLA '.    "Find ' NLA ' in description
           if  sy-subrc = '0'.
               delete table1.               "Delete if NLA & qty = 0
           endif.
       endif.
    endif.
  endloop.
*-----------------------------------------------------------------------
  SORT TABLE1 BY MATNR ASCENDING.
  MOVE 'Y'   TO  CHECKPOINT1.

  REFRESH: TABLE2, TABLE3, TABLE4, TEMPTABLE.
  MOVE 'Y' TO CHECKPOINT6.

* This routine will populate the table BUILDTEMP with records that
* have a KEYWORD as a value.
  LOOP AT TABLE1.
    CLEAR:   TABLE2, TABLE4, TEMPTABLE.
    PERFORM GET_CHARS USING TABLE1-MATNR.
    REFRESH TABLE3.
    CLEAR TABLE3.
  ENDLOOP.

  MOVE 'N' TO CHECKPOINT6.
  MOVE 'Y' TO CHECKPOINT7.

* This routine will populate the table BUILDTEMP2 with records that
* have a MATERIAL as a value.
  LOOP AT TABLE1.
    CLEAR:   TABLE2, TABLE4, TEMPTABLE.
    PERFORM GET_CHARS USING TABLE1-MATNR.
    REFRESH TABLE3.
    CLEAR TABLE3.
  ENDLOOP.

  SORT BUILDTEMP BY MATNR  ASCENDING
                    SORT1  ASCENDING
                    CLASS  ASCENDING
                    CLASS2 ASCENDING.

  REFRESH TABLE1.
  CLEAR   TABLE1.

* This subroutine will process all the records in the proper order.
  PERFORM PROCESS_ALL.


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
  ULINE: /1(37).
  WRITE: /.

  LOOP AT TABLE5.
    MOVE TABLE5-FIELD1 TO ALPHABIT.
    IF TABLE5-CLASS EQ TABLE5-CLASS2.
      ON CHANGE OF TABLE5-CLASS2.
        SELECT SINGLE * FROM SWOR WHERE CLINT = TABLE5-CLASS2.
        MOVE SWOR-KSCHL TO DISPLAY_CLASS.
        WRITE: /.
        WRITE: /.
        WRITE: /15 TEXT-014, 23 DISPLAY_CLASS.
        ULINE: /14(8).
      ENDON.
    ELSE.
      ON CHANGE OF TABLE5-CLASS2.
        SELECT SINGLE * FROM SWOR WHERE CLINT = TABLE5-CLASS2.
        MOVE SWOR-KSCHL TO DISPLAY_CLASS.
        WRITE: /.
        WRITE: /.
        WRITE: /15 TEXT-014, 23 DISPLAY_CLASS.
        ULINE: /14(8).
      ENDON.

      ON CHANGE OF TABLE5-CLASS.
        SELECT SINGLE * FROM SWOR WHERE CLINT = TABLE5-CLASS.
        MOVE SWOR-KSCHL TO DISPLAY_SUBCLASS.
        WRITE: /.
        WRITE: /.
        WRITE: /30 TEXT-017, 42 DISPLAY_SUBCLASS.
        ULINE: /29(12).
      ENDON.
    ENDIF.

    ON CHANGE OF ALPHABIT.
      WRITE: /.
    ENDON.

    WRITE: /50 TABLE5-FIELD1, 90 TABLE5-FIELD2.
  ENDLOOP.
ENDFORM.

*-----------------------------------------------------------------------
*         BIG_MOVE
*-----------------------------------------------------------------------
* IF CHECKPOINT5 IS EQUAL TO 'Y'.
*    This part is used to temporary store the values of the fields
*    that might be used as sort requirement.
* IF CHECKPOINT4 IS EQUAL TO 'Y'.
*    This part will look at the characteristic passed to it and
*    determine where it belongs in the layout to be outputted.  This
*    layout is used for displaying a complete line of values(1st line)
*-----------------------------------------------------------------------
FORM BIG_MOVE USING INTERNAL_CHAR_NUM.
  IF CHECKPOINT5 EQ 'Y'.
    CASE INTERNAL_CHAR_NUM.
      WHEN PRI_DIA_ATINN.
        MOVE TABLE3-ATFLV TO SORTTABLE-FIELD2.
      WHEN MATL_ATINN.
        MOVE TABLE3-ATWRT TO SORTTABLE-FIELD3.
      WHEN PRI_WAL_THICK_ATINN.
        MOVE TABLE3-ATFLV TO SORTTABLE-FIELD4.
      WHEN GRAD_ATINN.
        MOVE TABLE3-ATWRT TO SORTTABLE-FIELD5.
      WHEN CATY_ATINN.
        MOVE TABLE3-ATWRT TO SORTTABLE-FIELD6.
      WHEN DESN_TEMP_ATINN.
        MOVE TABLE3-ATWRT TO SORTTABLE-FIELD7.
      WHEN COAT_ATINN.
        MOVE TABLE3-ATWRT TO SORTTABLE-FIELD8.
      WHEN SECD_DIA_ATINN.
        MOVE TABLE3-ATFLV TO SORTTABLE-FIELD9.
      WHEN PRES_ATINN.
        MOVE TABLE3-ATWRT TO SORTTABLE-FIELD10.
      WHEN FLNG_ATINN.
        MOVE TABLE3-ATWRT TO SORTTABLE-FIELD11.
      WHEN END_ATINN.
        MOVE TABLE3-ATWRT TO SORTTABLE-FIELD12.
      WHEN BORE_ATINN.
        MOVE TABLE3-ATFLV TO SORTTABLE-FIELD13.
      WHEN SEC_WAL_THICK_ATINN.
        MOVE TABLE3-ATFLV TO SORTTABLE-FIELD14.
    ENDCASE.
  ELSEIF CHECKPOINT4 EQ 'Y'.
    CASE INTERNAL_CHAR_NUM.
      WHEN PRID_ATINN.
        PERFORM BIG_MOVE_SECOND.
        MOVE TABLE3-ATWRT TO TABLE4-FIELD2.
      WHEN SECD_ATINN.
        PERFORM BIG_MOVE_SECOND.
        MOVE TABLE3-ATWRT TO TABLE4-FIELD3.
      WHEN MATL_ATINN.
        PERFORM BIG_MOVE_SECOND.
        MOVE TABLE3-ATWRT TO TABLE4-FIELD4.
      WHEN MODL_ATINN.
        PERFORM BIG_MOVE_SECOND.
        MOVE TABLE3-ATWRT TO TABLE4-FIELD5.
      WHEN KEYW_ATINN.
        PERFORM BIG_MOVE_SECOND.
        MOVE TABLE3-ATWRT TO TABLE4-FIELD7.
    ENDCASE.
  ENDIF.
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
    WHEN MATL_ATINN.
      PERFORM MULTI_VALUE_SECOND.
      MOVE TABLE3-ATWRT TO TABLE2-FIELD4.
    WHEN MODL_ATINN.
      PERFORM MULTI_VALUE_SECOND.
      MOVE TABLE3-ATWRT TO TABLE2-FIELD5.
    WHEN KEYW_ATINN.
      PERFORM MULTI_VALUE_SECOND.
      MOVE TABLE3-ATWRT TO TABLE2-FIELD7.
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
  FIELD6-LINE = FIELD7-LINE = SY-LINNO.

  LOOP AT TABLE2.
    IF TABLE2-FIELD2 NE SPACE.
      SY-LINNO = FIELD2-LINE.
      WRITE: /32 TABLE2-FIELD2.
      MOVE SY-LINNO TO FIELD2-LINE.
      PERFORM LASTLINE USING FIELD2-LINE.
    ELSEIF TABLE2-FIELD3 NE SPACE.
      SY-LINNO = FIELD3-LINE.
      WRITE: /62 TABLE2-FIELD3.
      MOVE SY-LINNO TO FIELD3-LINE.
      PERFORM LASTLINE USING FIELD3-LINE.
    ELSEIF TABLE2-FIELD4 NE SPACE.
      SY-LINNO = FIELD4-LINE.
      WRITE: /95 TABLE2-FIELD4.
      MOVE SY-LINNO TO FIELD4-LINE.
      PERFORM LASTLINE USING FIELD4-LINE.
    ELSEIF TABLE2-FIELD5 NE SPACE.
      SY-LINNO = FIELD5-LINE.
      WRITE: /116 TABLE2-FIELD5.
      MOVE SY-LINNO TO FIELD5-LINE.
      PERFORM LASTLINE USING FIELD5-LINE.
    ENDIF.

    IF CHECKPOINT11 EQ 'Y'.
      IF TABLE2-FIELD7 NE SPACE.
        SY-LINNO = FIELD7-LINE.
        WRITE: /1 TABLE2-FIELD7.
        MOVE SY-LINNO TO FIELD7-LINE.
        PERFORM LASTLINE USING FIELD7-LINE.
      ENDIF.
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
  IF SORTTABLE2-CLASS2 EQ SORTTABLE2-CLASS.
    ON CHANGE OF SORTTABLE2-CLASS2.
      NEW-PAGE.
      SELECT SINGLE * FROM SWOR WHERE CLINT = SORTTABLE2-CLASS2.
      MOVE SWOR-KSCHL TO REP_CLASS.
      ULINE: /.
      ULINE: /.
      WRITE: /1 TEXT-014, 8 REP_CLASS.
      ULINE: /.
      ULINE: /.
      WRITE: /.
    ENDON.
  ELSE.
    ON CHANGE OF SORTTABLE2-CLASS2.
      NEW-PAGE.
      SELECT SINGLE * FROM SWOR WHERE CLINT = SORTTABLE2-CLASS2.
      MOVE SWOR-KSCHL TO REP_CLASS.
      ULINE: /.
      ULINE: /.
      WRITE: /1 TEXT-014, 8 REP_CLASS.
      ULINE: /.
      ULINE: /.
      WRITE: /.
    ENDON.

    ON CHANGE OF SORTTABLE2-CLASS.
      SELECT SINGLE * FROM SWOR WHERE CLINT = SORTTABLE2-CLASS.
      MOVE SWOR-KSCHL TO REP_SUBCLASS.
      WRITE: /.
      WRITE: /1 TEXT-017, 12 REP_SUBCLASS.
      ULINE: /.
      WRITE: /.
    ENDON.
  ENDIF.

  IF CHECKPOINT8 EQ 'Y'.
    ON CHANGE OF TABLE4-FIELD1.
      WRITE: /.
      WRITE: /30 TABLE4-FIELD1.
      ULINE: /30(30).
    ENDON.
  ENDIF.

  WRITE: /.
  WRITE: /32 TABLE4-FIELD2.
* write: 62 table4-field3, 95 table4-field4.                    "#235
  WRITE: 95 TABLE4-FIELD4.                                      "#235
  WRITE: 116 TABLE4-FIELD5, 145 TABLE4-FIELD6+12(6).
  IF TABLE4-FIELD3(3) NE 'N/A'.                                 "#235
     WRITE: TABLE4-FIELD3 UNDER TEXT-009.                       "
  ENDIF.                                                        "#235

  IF CHECKPOINT11 EQ 'Y'.
    WRITE: 1 TABLE4-FIELD7.
  ENDIF.

  ON CHANGE OF TABLE4-FIELD1 OR SORTTABLE2-CLASS OR SORTTABLE2-CLASS2.
    MOVE TABLE4-FIELD1     TO   TABLE5-FIELD1.
    MOVE SY-PAGNO          TO   TABLE5-FIELD2.
    MOVE SORTTABLE2-CLASS  TO   TABLE5-CLASS.
    MOVE SORTTABLE2-CLASS2 TO   TABLE5-CLASS2.
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
*    This subroutine is used to look for the MATERIAL NUMBER where
*    'KEYWORD' OR 'MATERIAL' is a sort item.  This subroutine is used
*    because KEYWORD and MATERIAL both can have multiple values for a
*    MATERIAL NUMBER.
*
* IF CHECKPOINT6 EQ 'Y'.
*    This part is used when KEYWORD  is the FIRST SORT item.
* IF CHECKPOINT9 EQ 'Y'.
*    This part is used when KEYWORD  is the THIRD SORT item.
* IF CHECKPOINT7 EQ 'Y'.
*    This part is used when MATERIAL is the FIRST SORT item.
* IF CHECKPOINT12 EQ 'Y'.
*    This part is used when MATERIAL is the SECOND SORT item.
*-----------------------------------------------------------------------
FORM MOVE_TO_TEMP.
  IF CHECKPOINT6 EQ 'Y'.
    IF TABLE3-ATINN EQ KEYW_ATINN.
      MOVE TEMP_MATNR     TO   BUILDTEMP-MATNR.
      MOVE TABLE3-ATWRT   TO   BUILDTEMP-SORT1.
      MOVE TABLE1-MASTER  TO   BUILDTEMP-CLASS.
      MOVE TABLE1-CLASS2  TO   BUILDTEMP-CLASS2.
      APPEND BUILDTEMP.
      CLEAR  BUILDTEMP.
    ENDIF.
  ELSEIF CHECKPOINT9 EQ 'Y'.
    IF TABLE3-ATINN EQ KEYW_ATINN.
      MOVE TEMPTABLE2-MATNR   TO   TEMPTABLE4-MATNR.
      MOVE TABLE3-ATWRT       TO   TEMPTABLE4-SORT3.
      MOVE TEMPTABLE2-SORT1   TO   TEMPTABLE4-SORT1.
      MOVE TEMPTABLE2-CLASS   TO   TEMPTABLE4-CLASS.
      MOVE TEMPTABLE2-CLASS2  TO   TEMPTABLE4-CLASS2.
      APPEND TEMPTABLE4.
      CLEAR  TEMPTABLE4.
    ENDIF.
  ELSEIF CHECKPOINT7 EQ 'Y'.
    IF TABLE3-ATINN EQ MATL_ATINN.
      MOVE TEMP_MATNR     TO   BUILDTEMP2-MATNR.
      MOVE TABLE3-ATWRT   TO   BUILDTEMP2-SORT1.
      MOVE TABLE1-MASTER  TO   BUILDTEMP2-CLASS.
      MOVE TABLE1-CLASS2  TO   BUILDTEMP2-CLASS2.
      APPEND BUILDTEMP2.
      CLEAR  BUILDTEMP2.
    ENDIF.
  ELSEIF CHECKPOINT12 EQ 'Y'.
    IF TABLE3-ATINN EQ MATL_ATINN.
      MOVE TEMPTABLE2-MATNR   TO   TEMPTABLE4-MATNR.
      MOVE TABLE3-ATWRT       TO   TEMPTABLE4-SORT2.
      MOVE TEMPTABLE2-SORT1   TO   TEMPTABLE4-SORT1.
      MOVE TEMPTABLE2-CLASS   TO   TEMPTABLE4-CLASS.
      MOVE TEMPTABLE2-CLASS2  TO   TEMPTABLE4-CLASS2.
      APPEND TEMPTABLE4.
      CLEAR  TEMPTABLE4.
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
  IF CHECKPOINT10 EQ 'Y'.
    MOVE 'Y' TO CHECKPOINT3.
    MOVE SORTTABLE2-SORT1 TO TABLE4-FIELD1.
    MOVE SORTTABLE2-MATNR TO TABLE4-FIELD6.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*    MULTI_VALUE_SECOND
*-----------------------------------------------------------------------
*    This subroutine is called from the subroutine 'MULTI_VALUE'.
*    This subroutine is executed only when a DESCRIPTION is matched.
*-----------------------------------------------------------------------
FORM MULTI_VALUE_SECOND.
  IF CHECKPOINT10 EQ 'Y'.
    MOVE 'Y' TO CHECKPOINT3.
    MOVE 'Y' TO MULTI_VALUE_IND.
    MOVE TEMP_MATNR TO TABLE2-FIELD6.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM CHAR_INT_NUM_CONSTRUCT                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CHAR_INT_NUM_CONSTRUCT.
PERFORM ATINN USING 'PRIMARY_DIAMETER__MM'
              CHANGING PRI_DIA_ATINN.
PERFORM ATINN USING 'PRIMARY_WALL_THICKNESS'
              CHANGING PRI_WAL_THICK_ATINN.
PERFORM ATINN USING 'GRADE'
              CHANGING GRAD_ATINN.
PERFORM ATINN USING 'CATEGORY'
              CHANGING CATY_ATINN.
PERFORM ATINN USING 'DESIGN_TEMPERATURE'
              CHANGING DESN_TEMP_ATINN.
PERFORM ATINN USING 'COATING'
              CHANGING COAT_ATINN.
PERFORM ATINN USING 'SECONDARY_DIAMETER__MM'
              CHANGING SECD_DIA_ATINN.
PERFORM ATINN USING 'PRESSURE_RATING'
              CHANGING PRES_ATINN.
PERFORM ATINN USING 'FLANGE_TYPE'
              CHANGING FLNG_ATINN.
PERFORM ATINN USING 'END_TYPE'
              CHANGING END_ATINN.
PERFORM ATINN USING 'BORE__MM'
              CHANGING BORE_ATINN.
PERFORM ATINN USING 'SECONDARY_WALL_THICKNESS'
              CHANGING SEC_WAL_THICK_ATINN.
ENDFORM.
*-----------------------------------------------------------------------
*    PROCESS_ALL
*-----------------------------------------------------------------------
* This subroutine process each subclass in the proper order. In all
* cases the table that will be processed for output will be SORTTABLE2.
* This is necessary in order to use the same code for processing the
* output.
*-----------------------------------------------------------------------
FORM PROCESS_ALL.

  PERFORM PROCESS_CORROSION.

  PERFORM PROCESS_FITTINGS.

  PERFORM PROCESS_PIPELINE_MATERIALS.
  PERFORM PROCESS_PIPES.
  PERFORM PROCESS_REPAIR_PARTS.
  PERFORM PROCESS_STATION_MATERIALS.

  PERFORM PROCESS_VALVES_AND_ACCESS.

  PERFORM PROCESS_WELL_DRILLING.

ENDFORM.

*-----------------------------------------------------------------------
*    CLASSCLINT2
*-----------------------------------------------------------------------
*   This subroutine will look for all the MATERIAL NUMBER under the
*   CLASS passed in the TABLE passed.
*   This subroutine is used when a class has a parentclass.
*   The table used can be either BUILDTEMP or BUILDTEMP2.
*   The value of TEMPCLASS2 is the actual class being processed.
*   The value of CLASS2 is the direct parentclass for the class being
*   processed.
*-----------------------------------------------------------------------
FORM CLASSCLINT2 TABLES CLINTTABLE2 STRUCTURE TEMPTABLE
                 USING TEMPCLASS2 CLASS2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = TEMPCLASS2.
  MOVE KLAH-CLINT TO TEMPCLINT2.
  LOOP AT CLINTTABLE2.
    IF CLINTTABLE2-CLASS EQ TEMPCLINT2.
      MOVE-CORRESPONDING CLINTTABLE2    TO TEMPTABLE2.
      MOVE CLASS2                       TO TEMPTABLE2-CLASS2.
      APPEND TEMPTABLE2.
      CLEAR  TEMPTABLE2.
    ENDIF.
  ENDLOOP.
ENDFORM.

*-----------------------------------------------------------------------
*    CLASSCLINT
*-----------------------------------------------------------------------
*   This subroutine will look for all the MATERIAL NUMBER under the
*   CLASS passed in the TABLE passed.
*   This subroutine is used when a class DO NOT have a parentclass.
*   The table used can be either BUILDTEMP or BUILDTEMP2.
*   The value of TEMPCLASS is the actual class being processed.
*-----------------------------------------------------------------------
FORM CLASSCLINT TABLES CLINTTABLE STRUCTURE TEMPTABLE
                USING TEMPCLASS.
  SELECT SINGLE * FROM KLAH WHERE CLASS = TEMPCLASS.
  MOVE KLAH-CLINT TO TEMPCLINT.
  LOOP AT CLINTTABLE.
    IF CLINTTABLE-CLASS EQ TEMPCLINT.
      MOVE-CORRESPONDING CLINTTABLE   TO   TEMPTABLE2.
      MOVE CLINTTABLE-CLASS           TO   TEMPTABLE2-CLASS2.
      APPEND TEMPTABLE2.
      CLEAR  TEMPTABLE2.
    ENDIF.
  ENDLOOP.
ENDFORM.

************************* PIPELINE MATERIALS ***************************
FORM PROCESS_PIPELINE_MATERIALS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  PERFORM CLASSCLINT TABLES BUILDTEMP
                     USING 'PIPELINE_MATERIALS'.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

* This subroutine will get the second sort item PRIMARY_DIAMETER__MM
  LOOP AT TEMPTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    MOVE TEMPTABLE2-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE2-SORT1   TO  SORTTABLE2-SORT1.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE TEMPTABLE2-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE2-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1 ASCENDING
                     SORT2 ASCENDING
                     MATNR ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.
  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.

****************************** PIPES ***********************************
FORM PROCESS_PIPES.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  PERFORM CLASSCLINT TABLES BUILDTEMP2
                     USING 'PIPES'.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    MOVE TEMPTABLE2-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE2-SORT1   TO  SORTTABLE2-SORT1.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD4   TO  SORTTABLE2-SORT4.
    MOVE SORTTABLE-FIELD5   TO  SORTTABLE2-SORT5.
    MOVE SORTTABLE-FIELD6   TO  SORTTABLE2-SORT6.
    MOVE SORTTABLE-FIELD7   TO  SORTTABLE2-SORT7.
    MOVE SORTTABLE-FIELD8   TO  SORTTABLE2-SORT8.
    MOVE TEMPTABLE2-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE2-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1 ASCENDING
                     SORT2 ASCENDING
                     SORT4 ASCENDING
                     SORT5 ASCENDING
                     SORT6 ASCENDING
                     SORT7 ASCENDING
                     SORT8 ASCENDING
                     MATNR ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'N' TO CHECKPOINT8.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT11.
  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
  MOVE 'Y' TO CHECKPOINT8.
  MOVE 'N' TO CHECKPOINT11.
ENDFORM.

*************************** REPAIR PARTS *******************************
FORM PROCESS_REPAIR_PARTS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  PERFORM CLASSCLINT TABLES BUILDTEMP
                     USING 'REPAIR_PARTS'.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    MOVE TEMPTABLE2-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE2-SORT1   TO  SORTTABLE2-SORT1.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD9   TO  SORTTABLE2-SORT9.
    MOVE TEMPTABLE2-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE2-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1 ASCENDING
                     SORT2 ASCENDING
                     SORT9 ASCENDING
                     MATNR ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.
  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.

ENDFORM.

************************* STATION_MATERIALS ****************************
FORM PROCESS_STATION_MATERIALS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  PERFORM CLASSCLINT TABLES BUILDTEMP
                     USING 'STATION_MATERIALS'.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    MOVE TEMPTABLE2-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE2-SORT1   TO  SORTTABLE2-SORT1.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE TEMPTABLE2-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE2-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1 ASCENDING
                     SORT2 ASCENDING
                     MATNR ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.
  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.

*************************** WELL_DRILLING ******************************
FORM PROCESS_WELL_DRILLING.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  PERFORM CLASSCLINT TABLES BUILDTEMP
                     USING 'WELL_DRILLING'.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

* This routine will simply move the contents of TEMPTABLE2 TO
* SORTTABLE2 for output.
  LOOP AT TEMPTABLE2.
    MOVE-CORRESPONDING TEMPTABLE2   TO   SORTTABLE2.
    MOVE TEMPTABLE2-MATNR           TO   SORTTABLE2-MATNR.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1 ASCENDING
                     MATNR ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.
  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.

*********************** VALVES AND ACCESS ** VALVES ********************
FORM PROCESS_VALVES_AND_ACCESS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = 'VALVES_AND_ACCESS'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP2
                      USING 'VALVES' PARENTCLASS.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

  MOVE 'Y' TO CHECKPOINT1.
  MOVE 'Y' TO CHECKPOINT9.
  MOVE 'N' TO CHECKPOINT2.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT6.
  MOVE 'N' TO CHECKPOINT7.
  MOVE 'N' TO CHECKPOINT10.

* This routine will get the multiple values for KEYWORD for each
* MATERIAL.
  LOOP AT TEMPTABLE2.
    CLEAR:    TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    REFRESH TABLE3.
    CLEAR TABLE3.
  ENDLOOP.

  REFRESH TEMPTABLE2.
  CLEAR   TEMPTABLE2.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE4 BY MATNR ASCENDING
                     SORT1 ASCENDING
                     SORT3 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE4.
    MOVE 'N' TO CHECKPOINT3.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE4-MATNR.
    MOVE TEMPTABLE4-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE4-SORT1   TO  SORTTABLE2-SORT3.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE TEMPTABLE4-SORT3   TO  SORTTABLE2-SORT1.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE SORTTABLE-FIELD12  TO  SORTTABLE2-SORT12.
    MOVE TEMPTABLE4-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE4-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT2  ASCENDING
                     SORT1  ASCENDING
                     SORT3  ASCENDING
                     SORT10 ASCENDING
                     SORT12 ASCENDING
                     MATNR ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.

  REFRESH : TABLE2, TABLE3, TABLE4.

* This subroutine will get the necessary sort items.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.

* This will call the subroutine to process the subclass Valve
* Accessories.
  PERFORM PROCESS_VALVE_ACCESSORIES.
ENDFORM.

************* VALVES AND ACCESS ** VALVE_ACCESSORIES *******************
FORM PROCESS_VALVE_ACCESSORIES.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = 'VALVES_AND_ACCESS'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'VALVE_ACCESSORIES' PARENTCLASS.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.
  MOVE 'N' TO CHECKPOINT10.

* This routine will simply move the contents of TEMPTABLE2 TO
* SORTTABLE2 for output.
  LOOP AT TEMPTABLE2.
    MOVE-CORRESPONDING TEMPTABLE2 TO SORTTABLE2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1 ASCENDING
                     MATNR ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.

 REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.

******************** CORROSION - INSULATE_FLG_SETS *********************
FORM PROCESS_CORROSION.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.

  SELECT SINGLE * FROM KLAH WHERE CLASS = 'CORROSION'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'INSULATE_FLG_SETS' PARENTCLASS.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    MOVE TEMPTABLE2-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE2-SORT1   TO  SORTTABLE2-SORT1.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE TEMPTABLE2-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE2-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1  ASCENDING
                     SORT2  ASCENDING
                     SORT10 ASCENDING
                     MATNR  ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.
  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.

* These two Perform will process the subclasses 1) INSULATING JOINTS
*                                               2) OTHER CORROSION ACC.
  PERFORM PROCESS_INSULATING_JOINTS.
  PERFORM PROCESS_OTH_CORROSION_ACC.

ENDFORM.

******************** CORROSION - INSULATING_JOINTS *********************
FORM PROCESS_INSULATING_JOINTS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.

  SELECT SINGLE * FROM KLAH WHERE CLASS = 'CORROSION'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'INSULATING_JOINTS' PARENTCLASS.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    MOVE TEMPTABLE2-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE2-SORT1   TO  SORTTABLE2-SORT1.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD4   TO  SORTTABLE2-SORT4.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE TEMPTABLE2-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE2-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1  ASCENDING
                     SORT2  ASCENDING
                     SORT4  ASCENDING
                     SORT10 ASCENDING
                     MATNR  ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.
  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.

******************** CORROSION - OTH_CORROSION_ACC *********************
FORM PROCESS_OTH_CORROSION_ACC.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, SORTTABLE2.

  SELECT SINGLE * FROM KLAH WHERE CLASS = 'CORROSION'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'OTH_CORROSION_ACC' PARENTCLASS.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

* This routine will simply move the contents of TEMPTABLE2 TO
* SORTTABLE2 for output.
  LOOP AT TEMPTABLE2.
    MOVE-CORRESPONDING TEMPTABLE2 TO SORTTABLE2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1 ASCENDING
                     MATNR ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.
  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.

********************* FITTINGS - BRANCH CONNECTIONS ********************
FORM PROCESS_FITTINGS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, PARENTCLASS, SORTTABLE2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = 'FITTINGS'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'BRANCH_CONNECTIONS' PARENTCLASS.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

  MOVE 'Y' TO CHECKPOINT1.
  MOVE 'Y' TO CHECKPOINT12.
  MOVE 'N' TO CHECKPOINT2.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT6.
  MOVE 'N' TO CHECKPOINT7.
  MOVE 'N' TO CHECKPOINT9.
  MOVE 'N' TO CHECKPOINT10.

* This routine will get the multiple values for MATERIAL where MATERIAL
* is a sort item.  This is necessary because each KEYWORD can have
* multiple MATERIAL values.
  LOOP AT TEMPTABLE2.
    CLEAR:    TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    REFRESH TABLE3.
    CLEAR TABLE3.
  ENDLOOP.

  REFRESH TEMPTABLE2.
  CLEAR   TEMPTABLE2.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT12.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE4 BY MATNR ASCENDING
                     SORT1 ASCENDING
                     SORT2 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE4.
    MOVE 'N' TO CHECKPOINT3.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE4-MATNR.
    MOVE TEMPTABLE4-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE4-SORT1   TO  SORTTABLE2-SORT1.
    MOVE TEMPTABLE4-SORT2   TO  SORTTABLE2-SORT3.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD4   TO  SORTTABLE2-SORT4.
    MOVE SORTTABLE-FIELD9   TO  SORTTABLE2-SORT9.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE SORTTABLE-FIELD5   TO  SORTTABLE2-SORT5.
    MOVE SORTTABLE-FIELD6   TO  SORTTABLE2-SORT6.
    MOVE SORTTABLE-FIELD7   TO  SORTTABLE2-SORT7.
    MOVE TEMPTABLE4-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE4-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1  ASCENDING
                     SORT3  ASCENDING
                     SORT2  ASCENDING
                     SORT4  ASCENDING
                     SORT9  ASCENDING
                     SORT10 ASCENDING
                     SORT5  ASCENDING
                     SORT6  ASCENDING
                     SORT7  ASCENDING
                     MATNR  ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.

  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.

* These Performs will process the retaining subclass listed
* under FITTINGS.
  PERFORM PROCESS_FLANGES.
  PERFORM PROCESS_OTHER_FITTINGS.
  PERFORM PROCESS_REDUCERS.
  PERFORM PROCESS_STOPPER_FITTINGS.
  PERFORM PROCESS_STRAIGHT_FITTINGS.
  PERFORM PROCESS_TRANSITION_FITTING.

ENDFORM.

************************** FITTINGS - FLANGES **************************
FORM PROCESS_FLANGES.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, PARENTCLASS, SORTTABLE2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = 'FITTINGS'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'FLANGES' PARENTCLASS.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT10.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    MOVE TEMPTABLE2-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE2-SORT1   TO  SORTTABLE2-SORT1.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD11  TO  SORTTABLE2-SORT11.
    MOVE SORTTABLE-FIELD12  TO  SORTTABLE2-SORT12.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE SORTTABLE-FIELD13  TO  SORTTABLE2-SORT13.
    MOVE SORTTABLE-FIELD5   TO  SORTTABLE2-SORT5.
    MOVE SORTTABLE-FIELD6   TO  SORTTABLE2-SORT6.
    MOVE SORTTABLE-FIELD7   TO  SORTTABLE2-SORT7.
    MOVE TEMPTABLE2-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE2-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1  ASCENDING
                     SORT2  ASCENDING
                     SORT11 ASCENDING
                     SORT12 ASCENDING
                     SORT10 ASCENDING
                     SORT13 ASCENDING
                     SORT5 ASCENDING
                     SORT6 ASCENDING
                     SORT7 ASCENDING
                     MATNR ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.
  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.

************************* FITTINGS - OTHER_FITTINGS ********************
FORM PROCESS_OTHER_FITTINGS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, PARENTCLASS, SORTTABLE2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = 'FITTINGS'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'OTHER_FITTINGS' PARENTCLASS.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

  MOVE 'Y' TO CHECKPOINT1.
  MOVE 'Y' TO CHECKPOINT12.
  MOVE 'N' TO CHECKPOINT2.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT6.
  MOVE 'N' TO CHECKPOINT7.
  MOVE 'N' TO CHECKPOINT9.
  MOVE 'N' TO CHECKPOINT10.

* This routine will get the multiple values for MATERIAL where MATERIAL
* is a sort item.  This is necessary because each KEYWORD can have
* multiple MATERIAL values.
  LOOP AT TEMPTABLE2.
    CLEAR:    TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    REFRESH TABLE3.
    CLEAR TABLE3.
  ENDLOOP.

  REFRESH TEMPTABLE2.
  CLEAR   TEMPTABLE2.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT12.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE4 BY MATNR ASCENDING
                     SORT1 ASCENDING
                     SORT2 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE4.
    MOVE 'N' TO CHECKPOINT3.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE4-MATNR.
    MOVE TEMPTABLE4-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE4-SORT1   TO  SORTTABLE2-SORT1.
    MOVE TEMPTABLE4-SORT2   TO  SORTTABLE2-SORT3.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD4   TO  SORTTABLE2-SORT4.
    MOVE SORTTABLE-FIELD12  TO  SORTTABLE2-SORT12.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE SORTTABLE-FIELD5   TO  SORTTABLE2-SORT5.
    MOVE SORTTABLE-FIELD6   TO  SORTTABLE2-SORT6.
    MOVE SORTTABLE-FIELD7   TO  SORTTABLE2-SORT7.
    MOVE TEMPTABLE4-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE4-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1  ASCENDING
                     SORT3  ASCENDING
                     SORT2  ASCENDING
                     SORT4  ASCENDING
                     SORT12 ASCENDING
                     SORT10 ASCENDING
                     SORT5  ASCENDING
                     SORT6  ASCENDING
                     SORT7  ASCENDING
                     MATNR  ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.

  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.


************************* FITTINGS - REDUCERS **************************
FORM PROCESS_REDUCERS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, PARENTCLASS, SORTTABLE2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = 'FITTINGS'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'REDUCERS' PARENTCLASS.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

  MOVE 'Y' TO CHECKPOINT1.
  MOVE 'Y' TO CHECKPOINT12.
  MOVE 'N' TO CHECKPOINT2.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT6.
  MOVE 'N' TO CHECKPOINT7.
  MOVE 'N' TO CHECKPOINT9.
  MOVE 'N' TO CHECKPOINT10.

* This routine will get the multiple values for MATERIAL where MATERIAL
* is a sort item.  This is necessary because each KEYWORD can have
* multiple MATERIAL values.
  LOOP AT TEMPTABLE2.
    CLEAR:    TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    REFRESH TABLE3.
    CLEAR TABLE3.
  ENDLOOP.

  REFRESH TEMPTABLE2.
  CLEAR   TEMPTABLE2.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT12.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE4 BY MATNR ASCENDING
                     SORT1 ASCENDING
                     SORT2 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE4.
    MOVE 'N' TO CHECKPOINT3.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE4-MATNR.
    MOVE TEMPTABLE4-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE4-SORT1   TO  SORTTABLE2-SORT1.
    MOVE TEMPTABLE4-SORT2   TO  SORTTABLE2-SORT3.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD9   TO  SORTTABLE2-SORT9.
    MOVE SORTTABLE-FIELD4   TO  SORTTABLE2-SORT4.
    MOVE SORTTABLE-FIELD14  TO  SORTTABLE2-SORT14.
    MOVE SORTTABLE-FIELD12  TO  SORTTABLE2-SORT12.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE SORTTABLE-FIELD5   TO  SORTTABLE2-SORT5.
    MOVE SORTTABLE-FIELD6   TO  SORTTABLE2-SORT6.
    MOVE SORTTABLE-FIELD7   TO  SORTTABLE2-SORT7.
    MOVE TEMPTABLE4-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE4-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1  ASCENDING
                     SORT3  ASCENDING
                     SORT2  ASCENDING
                     SORT9  ASCENDING
                     SORT4  ASCENDING
                     SORT14 ASCENDING
                     SORT12 ASCENDING
                     SORT10 ASCENDING
                     SORT5  ASCENDING
                     SORT6  ASCENDING
                     SORT7  ASCENDING
                     MATNR  ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.

  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.


*********************** FITTINGS - STOPPER_FITTINGS ********************
FORM PROCESS_STOPPER_FITTINGS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, PARENTCLASS, SORTTABLE2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = 'FITTINGS'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'STOPPER_FITTINGS' PARENTCLASS.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

  MOVE 'Y' TO CHECKPOINT1.
  MOVE 'Y' TO CHECKPOINT12.
  MOVE 'N' TO CHECKPOINT2.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT6.
  MOVE 'N' TO CHECKPOINT7.
  MOVE 'N' TO CHECKPOINT9.
  MOVE 'N' TO CHECKPOINT10.

* This routine will get the multiple values for MATERIAL where MATERIAL
* is a sort item.  This is necessary because each KEYWORD can have
* multiple MATERIAL values.
  LOOP AT TEMPTABLE2.
    CLEAR:    TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    REFRESH TABLE3.
    CLEAR TABLE3.
  ENDLOOP.

  REFRESH TEMPTABLE2.
  CLEAR   TEMPTABLE2.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT12.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE4 BY MATNR ASCENDING
                     SORT1 ASCENDING
                     SORT2 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE4.
    MOVE 'N' TO CHECKPOINT3.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE4-MATNR.
    MOVE TEMPTABLE4-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE4-SORT1   TO  SORTTABLE2-SORT1.
    MOVE TEMPTABLE4-SORT2   TO  SORTTABLE2-SORT3.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE SORTTABLE-FIELD5   TO  SORTTABLE2-SORT5.
    MOVE TEMPTABLE4-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE4-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1  ASCENDING
                     SORT3  ASCENDING
                     SORT2  ASCENDING
                     SORT10 ASCENDING
                     SORT5  ASCENDING
                     MATNR  ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.

  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.


********************** FITTINGS - STRAIGHT_FITTINGS ********************
FORM PROCESS_STRAIGHT_FITTINGS.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, PARENTCLASS, SORTTABLE2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = 'FITTINGS'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'STRAIGHT_FITTINGS' PARENTCLASS.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

  MOVE 'Y' TO CHECKPOINT1.
  MOVE 'Y' TO CHECKPOINT12.
  MOVE 'N' TO CHECKPOINT2.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT6.
  MOVE 'N' TO CHECKPOINT7.
  MOVE 'N' TO CHECKPOINT9.
  MOVE 'N' TO CHECKPOINT10.

* This routine will get the multiple values for MATERIAL where MATERIAL
* is a sort item.  This is necessary because each KEYWORD can have
* multiple MATERIAL values.
  LOOP AT TEMPTABLE2.
    CLEAR:    TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    REFRESH TABLE3.
    CLEAR TABLE3.
  ENDLOOP.

  REFRESH TEMPTABLE2.
  CLEAR   TEMPTABLE2.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT12.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE4 BY MATNR ASCENDING
                     SORT1 ASCENDING
                     SORT2 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE4.
    MOVE 'N' TO CHECKPOINT3.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE4-MATNR.
    MOVE TEMPTABLE4-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE4-SORT1   TO  SORTTABLE2-SORT1.
    MOVE TEMPTABLE4-SORT2   TO  SORTTABLE2-SORT3.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD4   TO  SORTTABLE2-SORT4.
    MOVE SORTTABLE-FIELD12  TO  SORTTABLE2-SORT12.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE SORTTABLE-FIELD5   TO  SORTTABLE2-SORT5.
    MOVE SORTTABLE-FIELD6   TO  SORTTABLE2-SORT6.
    MOVE SORTTABLE-FIELD7   TO  SORTTABLE2-SORT7.
    MOVE TEMPTABLE4-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE4-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1  ASCENDING
                     SORT3  ASCENDING
                     SORT2  ASCENDING
                     SORT4  ASCENDING
                     SORT12 ASCENDING
                     SORT10 ASCENDING
                     SORT5  ASCENDING
                     SORT6  ASCENDING
                     SORT7  ASCENDING
                     MATNR  ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.

  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.

********************** FITTINGS - TRANSITION_FITTING *******************
FORM PROCESS_TRANSITION_FITTING.
  REFRESH:  TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, SORTTABLE2.
  CLEAR:    TEMPTABLE2, TEMPTABLE3, TEMPTABLE4, PARENTCLASS, SORTTABLE2.
  SELECT SINGLE * FROM KLAH WHERE CLASS = 'FITTINGS'.
  MOVE KLAH-CLINT TO PARENTCLASS.
  PERFORM CLASSCLINT2 TABLES BUILDTEMP
                      USING 'TRANSITION_FITTING' PARENTCLASS.

  SORT TEMPTABLE2 BY MATNR ASCENDING
                     SORT1 ASCENDING.

  MOVE 'Y' TO CHECKPOINT1.
  MOVE 'Y' TO CHECKPOINT12.
  MOVE 'N' TO CHECKPOINT2.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT6.
  MOVE 'N' TO CHECKPOINT7.
  MOVE 'N' TO CHECKPOINT9.
  MOVE 'N' TO CHECKPOINT10.

* This routine will get the multiple values for MATERIAL where MATERIAL
* is a sort item.  This is necessary because each KEYWORD can have
* multiple MATERIAL values.
  LOOP AT TEMPTABLE2.
    CLEAR:    TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE2-MATNR.
    REFRESH TABLE3.
    CLEAR TABLE3.
  ENDLOOP.

  REFRESH TEMPTABLE2.
  CLEAR   TEMPTABLE2.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT4.
  MOVE 'N' TO CHECKPOINT3.
  MOVE 'N' TO CHECKPOINT12.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT5.

  SORT TEMPTABLE4 BY MATNR ASCENDING
                     SORT1 ASCENDING
                     SORT2 ASCENDING.

* This subroutine will get the necessary sort items.
  LOOP AT TEMPTABLE4.
    MOVE 'N' TO CHECKPOINT3.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING TEMPTABLE4-MATNR.
    MOVE TEMPTABLE4-MATNR   TO  SORTTABLE2-MATNR.
    MOVE TEMPTABLE4-SORT1   TO  SORTTABLE2-SORT1.
    MOVE TEMPTABLE4-SORT2   TO  SORTTABLE2-SORT3.
    MOVE SORTTABLE-FIELD2   TO  SORTTABLE2-SORT2.
    MOVE SORTTABLE-FIELD4   TO  SORTTABLE2-SORT4.
    MOVE SORTTABLE-FIELD10  TO  SORTTABLE2-SORT10.
    MOVE SORTTABLE-FIELD5   TO  SORTTABLE2-SORT5.
    MOVE SORTTABLE-FIELD6   TO  SORTTABLE2-SORT6.
    MOVE SORTTABLE-FIELD7   TO  SORTTABLE2-SORT7.
    MOVE TEMPTABLE4-CLASS   TO  SORTTABLE2-CLASS.
    MOVE TEMPTABLE4-CLASS2  TO  SORTTABLE2-CLASS2.
    APPEND SORTTABLE2.
    CLEAR  SORTTABLE2.
  ENDLOOP.

  SORT SORTTABLE2 BY SORT1  ASCENDING
                     SORT3  ASCENDING
                     SORT2  ASCENDING
                     SORT4  ASCENDING
                     SORT10 ASCENDING
                     SORT5  ASCENDING
                     SORT6  ASCENDING
                     SORT7  ASCENDING
                     MATNR  ASCENDING.

  MOVE 'N' TO CHECKPOINT1.
  MOVE 'N' TO CHECKPOINT5.
  MOVE 'Y' TO CHECKPOINT2.
  MOVE 'Y' TO CHECKPOINT3.
  MOVE 'Y' TO CHECKPOINT4.
  MOVE 'Y' TO CHECKPOINT10.

  REFRESH : TABLE2, TABLE3, TABLE4.

*This routine will process all the records in SORTTABLE2 for output.
  LOOP AT SORTTABLE2.
    CLEAR: TABLE2, TABLE4.
    PERFORM GET_CHARS USING SORTTABLE2-MATNR.
  ENDLOOP.
ENDFORM.
