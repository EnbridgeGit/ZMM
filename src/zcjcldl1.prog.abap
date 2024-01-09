REPORT ZCJCLDL1.

PARAMETERS: UPDAT.                          "x = delete selected records
                                            "space = test run
TABLES: KSSK.
DATA: CON_KKR_TNAMPR LIKE TCLA-OBTAB VALUE 'KKRAMERK'.
DATA: BEGIN OF KTAB OCCURS 0.
        INCLUDE STRUCTURE KSSK.
DATA: END OF KTAB.

SELECT * INTO TABLE KTAB FROM KSSK WHERE MAFID = 'O'
                                     AND KLART = '014'
                                     AND OBJEK LIKE 'TM%'.
LOOP AT KTAB.
  WRITE: / KTAB-OBJEK(15).
  CHECK NOT UPDAT IS INITIAL.
  CALL FUNCTION 'CLFM_DELETE_CLASSIFICATION'
       EXPORTING
            OBJECT                 = KTAB-OBJEK
            TABLE                  = CON_KKR_TNAMPR.
    WRITE: / 'deleted.'.
ENDLOOP.
IF NOT UPDAT IS INITIAL.
  COMMIT WORK.
ENDIF.
