REPORT ZMMMR008 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  ZMMMR008  - METER & REGULATOR CATALOG: BY MATERIAL #
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
* 97/10/17 md7140 #235 Eliminate printing of N/A in secondary descript.
* 97/03/27 md7140 increase font size by reducing white space
************************************************************************

TABLES: MARC, MARD.

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

DATA: CHECK-PLANT    TYPE I,           "added for checking qty 98/08/27
      CHECK-STORAGE  TYPE I.

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
IF HEADER1 = 'Y'.                             "md7140 reformat of header
   WRITE: / TEXT-RPT, SY-REPID COLOR COL_NEGATIVE, 65 TEXT-002,
        140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID,
        TEXT-012 UNDER TEXT-002,
        TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
   ULINE.
   WRITE: /.
   FORMAT COLOR COL_NORMAL.

   WRITE: /1 TEXT-003, 20 TEXT-005, 55 TEXT-007, 85 TEXT-009,
         110 TEXT-010, 130 TEXT-006.
   WRITE: / TEXT-004 UNDER TEXT-003,
            TEXT-008 UNDER TEXT-007,
            TEXT-008 UNDER TEXT-009,
            TEXT-011 UNDER TEXT-010,
            TEXT-004 UNDER TEXT-006.

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
        TEXT-012 UNDER TEXT-013,
        TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
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

MOVE 'N' TO CHECKPOINT1.
MOVE 'Y' TO CHECKPOINT2.
* This Form will retrieve all material numbers under the class entered
* at the selection screen.
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

* This part will check TABLE1's material numbers to see if the material
* numbers are under the sub-class that are valid for this report.
LOOP AT TABLE1.
   PERFORM CHECK-VALID-RECORDS.
ENDLOOP.

REFRESH: TABLE1, TABLE2, TABLE3, TABLE4, TEMPTABLE.

* This routine will process all the valid records for output.
LOOP AT VALID-RECORDS.
     CLEAR:   TABLE2, TABLE4.
     REFRESH: TABLE2, TABLE4.
     MOVE 'N' TO MULTI_VALUE_IND.
     PERFORM GET_CHARS USING VALID-RECORDS-MATNR.
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
      WHEN KEYW_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD2.
      WHEN PRID_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD3.
      WHEN SECD_ATINN.
          PERFORM BIG_MOVE_SECOND.
          MOVE TABLE3-ATWRT TO TABLE4-FIELD4.
      WHEN MANN_ATINN.
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
*    THIS SUBROUTINE WILL LOOK AT THE CHARACTERISTIC PAST TO IT AND
*    DETERMINE WHERE IT BELONGS IN THE LAYOUT TO BE OUTPUTTED.  THIS
*    LAYOUT IS USED FOR DISPLAYING THE MULTIPLE VALUES FOR A RECORD.
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
      WHEN MANN_ATINN.
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
           WRITE: /55 TABLE2-FIELD3.
           MOVE SY-LINNO TO FIELD3-LINE.
           PERFORM LASTLINE USING FIELD3-LINE.
              ELSEIF TABLE2-FIELD4 NE SPACE.
              SY-LINNO = FIELD4-LINE.
              WRITE: /85 TABLE2-FIELD4.
              MOVE SY-LINNO TO FIELD4-LINE.
              PERFORM LASTLINE USING FIELD4-LINE.
                 ELSEIF TABLE2-FIELD5 NE SPACE.
                 SY-LINNO = FIELD5-LINE.
                 WRITE: /110 TABLE2-FIELD5.
                 MOVE SY-LINNO TO FIELD5-LINE.
                 PERFORM LASTLINE USING FIELD5-LINE.
                    ELSEIF TABLE2-FIELD6 NE SPACE.
                    SY-LINNO = FIELD6-LINE.
                    WRITE: /130 TABLE2-FIELD6.
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
WRITE: /1 TABLE4-MATNR, 20 TABLE4-FIELD2, 55 TABLE4-FIELD3.
*write: 85 table4-field4, 110 table4-field5, 130 table4-field6. "#235
WRITE: 110 TABLE4-FIELD5, 130 TABLE4-FIELD6.
IF TABLE4-FIELD4(3) NE 'N/A'.                                   "#235
   WRITE: TABLE4-FIELD4 UNDER TEXT-009.
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

*-----------------------------------------------------------------------
*    BIG_MOVE_SECOND
*-----------------------------------------------------------------------
*    This subroutine is called from the subroutine 'BIG_MOVE'.
*    This subroutine is executed only when a DESCRIPTION is matched.
*-----------------------------------------------------------------------
FORM BIG_MOVE_SECOND.
   MOVE VALID-RECORDS-MATNR TO TABLE4-MATNR.
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
