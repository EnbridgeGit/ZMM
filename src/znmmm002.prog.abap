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
*       CALL FUNCTION 'CTUT_FEATURE_CHECK'
*             EXPORTING
*                  CLASS_TYPE                  = '001'
*                  FEATURE_ID                  = TABLE3-ATINN
*                 MESSAGE_TYPE_INP            = 'E'
*                 KEY_DATE                    = SY-DATUM
*             IMPORTING
*                  FEATURE_NAME                = DESCRIPTION
*             EXCEPTIONS
*                   OTHERS                      = 6.
*
      IF SY-SUBRC EQ 0.
        IF CHECKPOINT1 = 'Y'.
          PERFORM MOVE_TO_TEMP.
        ENDIF.

        IF CHECKPOINT2 = 'Y'.
*                  ON CHANGE OF DESCRIPTION.
          ON CHANGE OF TABLE3-ATINN.
            PERFORM BIG_MOVE USING TABLE3-ATINN.
          ELSE.
            PERFORM MULTI_VALUE USING TABLE3-ATINN.
          ENDON.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF CHECKPOINT3 = 'Y'.
    PERFORM GET_ALTERNATE_MATERIAL.    " NESH ADDED THIS
    PERFORM OUTPUT.
    CLEAR TABLE4.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHAR_INT_NUM                                             *
*---------------------------------------------------------------------*
*  -->  This subroutine will convert the characteristic name to it's  *
*       internal characteristics number                               *
*---------------------------------------------------------------------*
FORM CHAR_INT_NUM.
  PERFORM ATINN USING 'APPLICATION'
                CHANGING APPL_ATINN.
  PERFORM ATINN USING 'KEYWORD'
                CHANGING KEYW_ATINN.
  PERFORM ATINN USING 'MANUFACTURER_PART_NUMBER'
                CHANGING MANU_ATINN.
  PERFORM ATINN USING 'PRIMARY_DESCRIPTION'
                CHANGING PRID_ATINN.
  PERFORM ATINN USING 'SECONDARY_DESCRIPTION'
                CHANGING SECD_ATINN.
  PERFORM ATINN USING 'MODEL_NUMBER'
                CHANGING MODL_ATINN.
  PERFORM ATINN USING 'MATERIAL'
                CHANGING MATL_ATINN.
  PERFORM ATINN USING 'MANUFACTURER_NAME'
                CHANGING MANN_ATINN.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM ATINN                                                    *
*---------------------------------------------------------------------*
*  -->  CHARACT is the name of the characteristic passed              *
*  -->  G_ATINN is the internal number for the charac. returned       *
*---------------------------------------------------------------------*
FORM ATINN USING CHARACT CHANGING G_ATINN.

  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
            CLASSTYPE                   = '001'
*            FEATURE_ID                  = CHAR_TAB-ATINN
            FEATURE_NEUTRAL_NAME         = CHARACT
       IMPORTING
*            FEATURE_NAME                = NAME
*            FEATURE_NEUTAL_NAME         = NEUTNAME
            FEATURE_ID                  =  G_ATINN
       EXCEPTIONS
            INVALID_CLASS_TYPE          = 1
            MISSING_FEATURE_INFORMATION = 2
            NO_FEATURE_FOUND            = 3
            NO_FEATURE_VALID            = 4
            NO_LANGUAGE                 = 5
            OTHERS                      = 6.
  IF  SY-SUBRC <> 0.
    WRITE: / 'Unable to determine internal characteristic'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_ALTERNATE_MATERIAL
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ALTERNATE_MATERIAL.
* Count how many alternate materials do we have.
  CASE INCLFLAG.
* Program Zmmmr002.
    WHEN '1'.
*                   - Firs let me see how many alternate materials
*                     and store the number in indicator.
      SELECT COUNT( * ) FROM AUSP INTO INDICATOR WHERE
                                OBJEK EQ TABLE4-MATNR AND
                                ATINN EQ '598' AND
                                MAFID = 'O' AND
                                KLART = '001'.
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.

      CASE INDICATOR.
        WHEN '1'.             " It means just one alternate matnr
          SELECT SINGLE * FROM AUSP WHERE
                                    OBJEK EQ TABLE4-MATNR AND
                                    ATINN EQ '598' AND
                                    ATZHL EQ '1' AND
                                    MAFID = 'O' AND
                                    KLART = '001'.
*         write ausp-atwrt to table4-altmatnr.
          MOVE AUSP-ATWRT TO TABLE4-ALTMATNR.
          CLEAR INDICATOR.
        WHEN '0'.
          EXIT.
        WHEN OTHERS.
          OPEN CURSOR C1 FOR SELECT * FROM AUSP WHERE
                                    OBJEK EQ TABLE4-MATNR AND
                                    ATINN EQ '598' AND
                                    MAFID = 'O' AND
                                    KLART = '001'.
          FETCH NEXT CURSOR C1 INTO WA.
*          write wa-atwrt to table4-altmatnr.
          MOVE WA-ATWRT TO TABLE4-ALTMATNR.
      ENDCASE.


* Program zmmmr003.
* Program zmmmr004
* Program zmmmr005
    WHEN '2'.
      SELECT COUNT( * ) FROM AUSP INTO INDICATOR WHERE
                                OBJEK EQ TABLE4-FIELD6 AND
                                ATINN EQ '598' AND
                                MAFID = 'O' AND
                                KLART = '001'.
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.

      CASE INDICATOR.
        WHEN 1.               " It means just one alternate matnr
          SELECT SINGLE * FROM AUSP WHERE
                                    OBJEK EQ TABLE4-FIELD6 AND
                                    ATINN EQ '598' AND
                                    ATZHL EQ '1' AND
                                    MAFID = 'O' AND
                                    KLART = '001'.
          WRITE AUSP-ATWRT TO TABLE4-ALTMATNR.
          CLEAR INDICATOR.
        WHEN '0'.
          EXIT.
        WHEN OTHERS.
          OPEN CURSOR C1 FOR SELECT * FROM AUSP WHERE
                                    OBJEK EQ TABLE4-FIELD6 AND
                                    ATINN EQ '598' AND
                                    MAFID = 'O' AND
                                    KLART = '001'.
          FETCH NEXT CURSOR C1 INTO WA.
          WRITE WA-ATWRT TO TABLE4-ALTMATNR.
      ENDCASE.

    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                               " GET_ALTERNATE_MATERIAL
