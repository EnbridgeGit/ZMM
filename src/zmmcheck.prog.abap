REPORT ZMMCHECK.

*-----------------------------------------------------------------------

TABLES: MARA, MAKT, KSSK, KLAH.
DATA:   BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA:   END OF CHAR_TAB.

DATA:   TEMP_MATNR        LIKE     AUSP-OBJEK,
        G_ATINN           LIKE     AUSP-ATINN.

*-----------------------------------------------------------------------

CALL FUNCTION 'CTUT_FEATURE_CHECK'
     EXPORTING
          CLASS_TYPE                  = '001'
          FEATURE_NEUTRAL_NAME        = 'PRIMARY_DESCRIPTION'
     IMPORTING
          FEATURE_ID                  = G_ATINN
     EXCEPTIONS
          INVALID_CLASS_TYPE          = 1
          MISSING_FEATURE_INFORMATION = 2
          NO_FEATURE_FOUND            = 3
          NO_FEATURE_VALID            = 4
          NO_LANGUAGE                 = 5
          OTHERS                      = 6.

IF SY-SUBRC <> 0.
  WRITE: / 'Unable to determine internal characteristic for',
           'PRIMARY_DESCRIPTION'.
ENDIF.

SELECT * FROM MARA
   WHERE LVORM NE 'X'
   ORDER BY MATNR.
  SELECT SINGLE * FROM MAKT
    WHERE MATNR = MARA-MATNR
    AND SPRAS = SY-LANGU.
  SELECT * FROM KSSK
    WHERE MAFID = 'O'
    AND KLART = '001'
    AND OBJEK = MARA-MATNR.
  ENDSELECT.

  MOVE MARA-MATNR TO TEMP_MATNR.
  CALL FUNCTION 'CLFM_SELECT_AUSP'
       EXPORTING
            MAFID     = 'O'
            CLASSTYPE = '001'
            OBJECT    = TEMP_MATNR
       TABLES
            EXP_AUSP  = CHAR_TAB
       EXCEPTIONS
            NO_VALUES = 1
            OTHERS    = 2.

  SORT CHAR_TAB BY ATINN.
  READ TABLE CHAR_TAB WITH KEY ATINN = G_ATINN BINARY SEARCH.
  CHECK SY-SUBRC = 0.
  IF MAKT-MAKTG+0(30) <> CHAR_TAB-ATWRT.
    WRITE : / MARA-MATNR, CHAR_TAB-ATWRT, MAKT-MAKTG.
  ENDIF.
ENDSELECT.
