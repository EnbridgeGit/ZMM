************************************************************************
*    Program     :  ZNMMM002
*    Programmer  :  Gus Spartalis/Omnilogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  September 30, 1996
*
*    This ABAP is stored as an #INCLUDE.  This #include stores two
*    sub-routines.  The first retreives information for which material
*    number is directly associated with what class.  The second one
*    finds the characteristics for the Material Number's found in the
*    first sub-routune.
************************************************************************
FORM GET_MATNR USING S_CLASS.
CLEAR TABLE1.
REFRESH TABLE1.
*  This Select will take the name entered for the CLASS and find the
*  Internal Class Number.
SELECT SINGLE * FROM KLAH WHERE CLASS = S_CLASS.
MOVE KLAH-CLINT TO SEARCH_CLASS.

*  This section will loop through the same table and look for the
*  hierarchy structure for the classes that lead to the Material Number.
*  When a Material Number is found at the end of the hierarchy the
*  path and material number is entered into the internal table.
*  When a Sub-Class is found then, the class is moved to one of the
*  class fields in the internal table.
*  When moving to a lower nested SELECT, the Sub-Fields for storing the
*  class in the internal table are deleted from the the point of the
*  nested SELECT, onward.  The fields called LINK followed by a number
*  is used as a pointer for the nested SELECTS.  LINKS are deleted after
*  moving to a NEST SELECT and are deleted from the current LINK
*  in the nested SELECT, onward.

SELECT * FROM KSSK WHERE CLINT = SEARCH_CLASS.
CLEAR:  TABLE1-CLASS1, TABLE1-CLASS2, TABLE1-CLASS3, TABLE1-CLASS4.
CLEAR:  TABLE1-CLASS5, TABLE1-CLASS6, TABLE1-CLASS7, TABLE1-TEXT.
CLEAR:  LINK1, LINK2, LINK3, LINK4, LINK5, LINK6.
  IF KSSK-MAFID = 'O'.
    MOVE KSSK-OBJEK TO TABLE1-MATNR.
    MOVE KSSK-CLINT TO TABLE1-MASTER.
    APPEND TABLE1.
    CLEAR: TABLE1-MATNR, TABLE1-MASTER.
  ELSE.
    MOVE KSSK-CLINT TO TABLE1-CLASS1.
    MOVE KSSK-OBJEK TO LINK1.
    SELECT * FROM KSSK WHERE CLINT = LINK1.
    CLEAR:  TABLE1-CLASS2, TABLE1-CLASS3, TABLE1-CLASS4.
    CLEAR:  TABLE1-CLASS5, TABLE1-CLASS6, TABLE1-CLASS7, TABLE1-TEXT.
    CLEAR:  LINK2, LINK3, LINK4, LINK5, LINK6.
      IF KSSK-MAFID = 'O'.
        MOVE KSSK-OBJEK TO TABLE1-MATNR.
        MOVE KSSK-CLINT TO TABLE1-MASTER.
        APPEND TABLE1.
        CLEAR: TABLE1-MATNR, TABLE1-MASTER.
      ELSE.
        MOVE KSSK-CLINT TO TABLE1-CLASS2.
        MOVE KSSK-OBJEK TO LINK2.
        SELECT * FROM KSSK WHERE CLINT = LINK2.
        CLEAR:  TABLE1-CLASS3, TABLE1-CLASS4, TABLE1-CLASS5.
        CLEAR:  TABLE1-CLASS6, TABLE1-CLASS7, TABLE1-TEXT.
        CLEAR:  LINK3, LINK4, LINK5, LINK6.
          IF KSSK-MAFID = 'O'.
            MOVE KSSK-OBJEK TO TABLE1-MATNR.
            MOVE KSSK-CLINT TO TABLE1-MASTER.
            APPEND TABLE1.
            CLEAR: TABLE1-MATNR, TABLE1-MASTER.
          ELSE.
            MOVE KSSK-CLINT TO TABLE1-CLASS3.
            MOVE KSSK-OBJEK TO LINK3.
            SELECT * FROM KSSK WHERE CLINT = LINK3.  .
            CLEAR:  TABLE1-CLASS4, TABLE1-CLASS5.
            CLEAR:  TABLE1-CLASS6, TABLE1-CLASS7, TABLE1-TEXT.
            CLEAR:  LINK4, LINK5, LINK6.
              IF KSSK-MAFID = 'O'.
                MOVE KSSK-OBJEK TO TABLE1-MATNR.
                MOVE KSSK-CLINT TO TABLE1-MASTER.
                APPEND TABLE1.
                CLEAR: TABLE1-MATNR, TABLE1-MASTER.
              ELSE.
                MOVE KSSK-CLINT TO TABLE1-CLASS4.
                MOVE KSSK-OBJEK TO LINK4.
                SELECT * FROM KSSK WHERE CLINT = LINK4.
                CLEAR:  TABLE1-CLASS5, TABLE1-CLASS6.
                CLEAR:  TABLE1-CLASS7, TABLE1-TEXT.
                CLEAR:  LINK5, LINK6.
                  IF KSSK-MAFID = 'O'.
                    MOVE KSSK-OBJEK TO TABLE1-MATNR.
                    MOVE KSSK-CLINT TO TABLE1-MASTER.
                    APPEND TABLE1.
                    CLEAR: TABLE1-MATNR, TABLE1-MASTER.
                  ELSE.
                    MOVE KSSK-CLINT TO TABLE1-CLASS5.
                    MOVE KSSK-OBJEK TO LINK5.
                    SELECT * FROM KSSK WHERE CLINT = LINK5.
                    CLEAR:  TABLE1-CLASS6, TABLE1-CLASS7, TABLE1-TEXT.
                    CLEAR:  LINK6.
                      IF KSSK-MAFID = 'O'.
                        MOVE KSSK-OBJEK TO TABLE1-MATNR.
                        MOVE KSSK-CLINT TO TABLE1-MASTER.
                        APPEND TABLE1.
                        CLEAR: TABLE1-MATNR, TABLE1-MASTER.
                      ELSE.
                        MOVE KSSK-CLINT TO TABLE1-CLASS6.
                        MOVE KSSK-OBJEK TO LINK6.
                        SELECT * FROM KSSK WHERE CLINT = LINK6.
                        CLEAR:  TABLE1-CLASS7, TABLE1-TEXT.
                          IF KSSK-MAFID = 'O'.
                            MOVE KSSK-OBJEK TO TABLE1-MATNR.
                            MOVE KSSK-CLINT TO TABLE1-MASTER.
                            APPEND TABLE1.
                            CLEAR: TABLE1-MATNR, TABLE1-MASTER.
                          ELSE.
                            MOVE KSSK-CLINT TO TABLE1-CLASS7.
                            MOVE 'UPDATE THIS PROGRAM' TO TABLE1-TEXT.
                            APPEND TABLE1.
                            CLEAR TABLE1-TEXT.
                          ENDIF.
                        ENDSELECT.
                      ENDIF.
                    ENDSELECT.
                  ENDIF.
                ENDSELECT.
              ENDIF.
            ENDSELECT.
          ENDIF.
        ENDSELECT.
      ENDIF.
    ENDSELECT.
  ENDIF.
ENDSELECT.
ENDFORM.

************************************************************************
FORM GET_CHARS USING SUB_MATNR.
MOVE 'N' TO CHECKPOINT3.
MOVE SUB_MATNR TO TEMP_MATNR.

     CALL FUNCTION 'CLFM_SELECT_AUSP'
          EXPORTING
               MAFID              ='O'
               CLASSTYPE          ='001'
               OBJECT             = TEMP_MATNR
          TABLES
              EXP_AUSP            = TABLE3
          EXCEPTIONS
              NO_VALUES          = 1
              OTHERS             = 2.


  IF SY-SUBRC EQ 0.
     LOOP AT TABLE3.
       CALL FUNCTION 'CTUT_FEATURE_CHECK'
             EXPORTING
                  CLASS_TYPE                  = '001'
                  FEATURE_ID                  = TABLE3-ATINN
*                 MESSAGE_TYPE_INP            = 'E'
*                 KEY_DATE                    = SY-DATUM
             IMPORTING
                  FEATURE_NAME                = DESCRIPTION
             EXCEPTIONS
                   OTHERS                      = 6.
*
             IF SY-SUBRC EQ 0.
                IF CHECKPOINT1 = 'Y'.
                   PERFORM MOVE_TO_TEMP.
                ENDIF.

                IF CHECKPOINT2 = 'Y'.
                  ON CHANGE OF DESCRIPTION.
                       PERFORM BIG_MOVE USING DESCRIPTION.
                  ELSE.
                       PERFORM MULTI_VALUE USING DESCRIPTION.
                  ENDON.
                ENDIF.
             ENDIF.
     ENDLOOP.
ENDIF.

*IF CHECKPOINT2 = 'T'.
*  APPEND TABLE4.
*  CLEAR TABLE4.
*ENDIF.

IF CHECKPOINT3 = 'Y'.
   PERFORM OUTPUT.
   CLEAR TABLE4.
ENDIF.

ENDFORM.
