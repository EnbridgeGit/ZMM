REPORT ZMCLI001 LINE-COUNT 65 LINE-SIZE 80 MESSAGE-ID ZM.

************************************************************************
*  Author:   Dorothy Bialkowska
*  Brief Description:
*  - The purpose of this program is to create SAP to AM/FM interface
*        file.
************************************************************************
* 2000/01/17 mdemeest #738 Move description to record-comments
* 1999/12/20 mdemeest #727 Characteristics with class changes
************************************************************************
TABLES:   CABN, KLAH, KSSK, MARA, MARC, T001W, T023T, T134T,
          MAKT.                        "Material Description  2000/01/17

DATA:     LINE     TYPE     I,
          MAT_LINE TYPE     I,
          KEY_LINE TYPE     I,
          MOD_LINE TYPE     I,
          ROW      TYPE     I,
          NAME     LIKE     CABN-ATNAM,
          FLAG(1)  TYPE     C,
          COUNTER  TYPE     I.

DATA:     BEGIN OF CHAR_TAB OCCURS 20.
        INCLUDE STRUCTURE AUSP.
DATA:     END OF CHAR_TAB.

DATA:    BEGIN OF BIG_TAB OCCURS 100000,
               MATNR     LIKE    MARC-MATNR,
               MAKTX     LIKE    MAKT-MAKTX,                 "2000/01/17
               CLASS     LIKE    KLAH-CLASS,
               CHAR      LIKE    CABN-ATNAM,
               VALUE     LIKE    AUSP-ATWRT,
         END OF BIG_TAB.

*-----------------------------------------------------------------------
DATA:   VAR1(6)     TYPE     C,        "material number
        VAR2(18)    TYPE     C,        "class name
        VAR3(10)    TYPE     C,        "primary_diameter__nps
        VAR4(10)    TYPE     C,        "secondary_diameter_nps
        VAR5(10)    TYPE     C,        "primary_wall_thickness
        VAR6(10)    TYPE     C,        "secondary_wall_thicness
        VAR7(15)    TYPE     C,        "end_type
        VAR8(4)     TYPE     C,        "flange_type
        VAR9(3)     TYPE     C,        "coating
        VAR10(7)    TYPE     C,        "category
        VAR11(5)    TYPE     C,        "grade
        VAR12(8)    TYPE     C,        "pressure_rating
        VAR13(30)   TYPE     C,        "specification
        VAR14(10)   TYPE     C,        "number_of_turns
        VAR15(5)    TYPE     C,        "maximum_component_pressure_kpa
        VAR16(15)   TYPE     C,        "primary_diameter__mm
        VAR17(15)   TYPE     C,        "secondary_diameter_mm
        VAR18(7)    TYPE     C,        "length
        VAR19(1)    TYPE     C,        "approved
        VAR20(20)   TYPE     C,        "material
        VAR21(1)    TYPE     C,        "insulated
        VAR22(40)   TYPE     C.        "comments
*                 --------------------
DATA:   BEGIN OF MAT_TAB OCCURS 5,
              MATERIAL(20)   TYPE C,
        END OF MAT_TAB.
*                 --------------------
DATA:   BEGIN OF KEY_TAB OCCURS 5,
              KEY(30)        TYPE C,
        END OF KEY_TAB.
*                 --------------------
DATA:   BEGIN OF MODEL_TAB OCCURS 5,
              MODEL(20)      TYPE C,
        END OF MODEL_TAB.
*                 --------------------
DATA:   BEGIN OF STAT_TAB OCCURS 20,
              STAT           LIKE MARC-MMSTA,
        END OF STAT_TAB.
*                 --------------------
DATA:   STOCK(1)             TYPE     C,
        COMPOT(30)           TYPE     C,
        SUB(30)              TYPE     C.
*                 ---------------------
DATA:   BEGIN OF RECORD,
              STOCK_NR(8)       TYPE C,
              DUMY1(1)          TYPE C     VALUE '^',
              COPM_TYPE(30)     TYPE C,
              DUMY2(1)          TYPE C     VALUE '^',
              SUB_COMP(30)      TYPE C,
              DUMY3(1)          TYPE C     VALUE '^',
              MATERIAL(20)      TYPE C,
              DUMY4(1)          TYPE C     VALUE '^',
              PRI_SIZE_NPS(10)  TYPE C,
              DUMY5(1)          TYPE C     VALUE '^',
              SEC_SIZE_NPS(10)  TYPE C,
              DUMY6(1)          TYPE C     VALUE '^',
              PRI_WALL_1(10)    TYPE C,
              DUMY7(1)          TYPE C     VALUE '^',
              SEC_WALL_1(10)    TYPE C,
              DUMY8(1)          TYPE C     VALUE '^',
              END_TYPE(15)      TYPE C,
              DUMY9(1)          TYPE C     VALUE '^',
              FLANGE_TYPE(4)    TYPE C,
              DUMY10(1)         TYPE C     VALUE '^',
              INSULATED(1)      TYPE C,
              DUMY11(1)         TYPE C     VALUE '^',
              COATING(3)        TYPE C,
              DUMY12(1)         TYPE C     VALUE '^',
              MAT_CAT(7)        TYPE C,
              DUMY13(1)         TYPE C     VALUE '^',
              MAT_GRADE(3)      TYPE C,
              DUMY14(1)         TYPE C     VALUE '^',
              SPEC(40)          TYPE C,
              DUMY15(1)         TYPE C     VALUE '^',
              MODEL(30)         TYPE C,
              DUMY16(1)         TYPE C     VALUE '^',
              NR_OF_TURNS(10)   TYPE C,
              DUMY17(1)         TYPE C     VALUE '^',
              PRESSURE(5)       TYPE C,
              DUMY18(1)         TYPE C     VALUE '^',
              MANUF(15)         TYPE C,
              DUMY19(1)         TYPE C     VALUE '^',
              COMMENTS(40)      TYPE C
              VALUE '                                        ',
              DUMY20(1)         TYPE C     VALUE '^',
              DATE(10)          TYPE C     VALUE '0000000000',
              DUMY21(1)         TYPE C     VALUE '^',
              TIME(10)          TYPE C     VALUE '0000000000',
              DUMY22(1)         TYPE C     VALUE '^',
              USER_ID(7)        TYPE C     VALUE '       ',
              DUMY23(1)         TYPE C     VALUE '^',
              STOCK_GR(1)       TYPE C,
              DUMY24(1)         TYPE C     VALUE '^',
              PRI_SIZE_ALT(15)  TYPE C,
              DUMY25(1)         TYPE C     VALUE '^',
              SEC_SIZE_ALT(15)  TYPE C,
              DUMY26(1)         TYPE C     VALUE '^',
              PRI_WALL_2(15)    TYPE C,
              DUMY27(1)         TYPE C     VALUE '^',
              SEC_WALL_2(15)    TYPE C,
              DUMY28(1)         TYPE C     VALUE '^',
              APPROVED(1)       TYPE C,
              DUMY29(8)         TYPE C     VALUE '^^^^^^^^',
              ACCESS(1)         TYPE C     VALUE 'A',
              DUMY30(1)         TYPE C     VALUE '^',
              DIM(15)           TYPE C,
              DUMY31(3)         TYPE C     VALUE '^^^',
              UNIQUE_ID(10)     TYPE C,
        END OF RECORD.

DATA:   DATAFILE     LIKE     FILENAME-FILEINTERN.
*-----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS: S_MATTP     FOR T134T-MTART     DEFAULT 'HIBE'
                                                NO INTERVALS,
                S_MATGRP    FOR T023T-MATKL,
                S_PLANT     FOR T001W-WERKS,
                S_CLASS     FOR KLAH-CLASS,
                S_CHAR      FOR CABN-ATNAM.
SELECTION-SCREEN ULINE.
PARAMETER: LOGICFLE     LIKE     FILENAME-FILEEXTERN     DEFAULT
                                                         'ZMCLI001_03'.
SELECTION-SCREEN END OF BLOCK BOX.

AT SELECTION-SCREEN.
  PERFORM GET_FILE.

START-OF-SELECTION.

  SELECT * FROM MARA
    WHERE MTART IN S_MATTP
    AND MATKL IN S_MATGRP
    AND LVORM NE 'X'
    ORDER BY MATNR.
    SELECT *  FROM MARC
       WHERE MATNR = MARA-MATNR
       AND WERKS IN S_PLANT
       AND LVORM NE 'X'.
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      SELECT * FROM KSSK
         WHERE MAFID = 'O'
         AND KLART = '001'
         AND OBJEK = MARA-MATNR.
      ENDSELECT.
      IF SY-SUBRC EQ 0.
        SELECT SINGLE * FROM KLAH
           WHERE CLINT = KSSK-CLINT.
        CHECK KLAH-CLASS IN S_CLASS.
        PERFORM ASSIGN_CHAR USING MARA-MATNR.
      ENDIF.
    ENDIF.
  ENDSELECT.

  SORT BIG_TAB BY MATNR.

  LOOP AT BIG_TAB.
    FLAG = SPACE.
    ROW = 1.
    PERFORM SEARCH_TAB.
    AT END OF MATNR.
      SELECT * FROM MARC
         WHERE MATNR = BIG_TAB-MATNR.
        MOVE MARC-MMSTA TO STAT_TAB-STAT.
        APPEND STAT_TAB.
        CLEAR STAT_TAB.
      ENDSELECT.
      PERFORM DETERMINE_STATUS.
      CLEAR STAT_TAB.
      REFRESH STAT_TAB.
      PERFORM PRINT_ERRORS.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF ( VAR2 = 'FLANGES' OR VAR2 = 'REDUCERS' OR
          VAR2 = 'STOPPER_FITTINGS' OR VAR2 = 'STRAIGHT_FITTINGS' OR
          VAR2 = 'TRANSITION_FITTING' ).
        STOCK = 'F'.
        COMPOT = VAR2.
        READ TABLE KEY_TAB INDEX 1.
        MOVE KEY_TAB-KEY TO SUB.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'BRANCH_CONNECTIONS'.
        SORT KEY_TAB BY KEY.
        LOOP AT KEY_TAB.
          READ TABLE KEY_TAB WITH KEY = 'WELD FULL-ENCIRCLEMENT SADDLE'
                                                         BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            MOVE 'M' TO STOCK.
            MOVE 'REINFORCEMENT' TO COMPOT.
            MOVE KEY_TAB-KEY TO SUB.
          ELSE.
            READ TABLE KEY_TAB WITH KEY = 'SADDLE' BINARY SEARCH.
            IF SY-SUBRC = 0.
              MOVE 'M' TO STOCK.
              MOVE 'REINFORCEMENT' TO COMPOT.
              MOVE KEY_TAB-KEY TO SUB.
            ELSE.
              READ TABLE KEY_TAB WITH KEY = 'WELD SPLIT_LONG SADDLE'
                                                      BINARY SEARCH.
              IF SY-SUBRC = 0.
                MOVE 'M' TO STOCK.
                MOVE 'REINFORCEMENT' TO COMPOT.
                MOVE KEY_TAB-KEY TO SUB.
              ELSE.
                READ TABLE KEY_TAB INDEX 1.
                MOVE 'F' TO STOCK.
                MOVE VAR2 TO COMPOT.
                MOVE KEY_TAB-KEY TO SUB.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'PIPES'.
        SORT KEY_TAB BY KEY.
        LOOP AT KEY_TAB.
          READ TABLE KEY_TAB WITH KEY = 'CASING' BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            MOVE 'P' TO STOCK.
            MOVE VAR2 TO COMPOT.
            MOVE KEY_TAB-KEY TO SUB.
          ELSE.
            READ TABLE KEY_TAB WITH KEY = 'PIPE' BINARY SEARCH.
            IF SY-SUBRC = 0.
              MOVE 'P' TO STOCK.
              MOVE VAR2 TO COMPOT.
              MOVE KEY_TAB-KEY TO SUB.
            ELSE.
              READ TABLE KEY_TAB WITH KEY = 'PIPES' BINARY SEARCH.
              IF SY-SUBRC = 0.
                MOVE 'P' TO STOCK.
                MOVE VAR2 TO COMPOT.
                MOVE KEY_TAB-KEY TO SUB.
              ELSE.
                READ TABLE KEY_TAB WITH KEY = 'TUBING' BINARY SEARCH.
                IF SY-SUBRC = 0.
                  FLAG = 'X'.
                  EXIT.
                ELSE.
                  MESSAGE I008 WITH VAR1 'KEYWORD' 'DIFFERENT KEYWORD'.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'VALVES'.
        MOVE 'V' TO STOCK.
        MOVE VAR2 TO COMPOT.
        READ TABLE KEY_TAB INDEX 1.
        MOVE KEY_TAB-KEY TO SUB.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'VALVE_ACCESSORIES'.
        SORT KEY_TAB BY KEY.
        LOOP AT KEY_TAB.
          READ TABLE KEY_TAB WITH KEY = 'CURB BOX' BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            MOVE 'M' TO STOCK.
            MOVE 'VALVE BOX' TO COMPOT.
            MOVE KEY_TAB-KEY TO SUB.
          ELSE.
            READ TABLE KEY_TAB WITH KEY = 'VALVE BOX' BINARY SEARCH.
            IF SY-SUBRC = 0.
              MOVE 'M' TO STOCK.
              MOVE 'VALVE BOX' TO COMPOT.
              MOVE KEY_TAB-KEY TO SUB.
            ELSE.
              MESSAGE I008 WITH VAR1 'KEYWORD' 'DIFFERENT KEYWORD'.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'REPAIR_PARTS'.
        MOVE 'M' TO STOCK.
        MOVE VAR2 TO COMPOT.
        READ TABLE KEY_TAB INDEX 1.
        MOVE KEY_TAB-KEY TO SUB.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'INSULATE_FLG_SETS'.
        SORT KEY_TAB BY KEY.
        LOOP AT KEY_TAB.
*         read table key_tab with key = 'INSULATING FLANGE SET'"99/12/20
*                                                binary search."99/12/20
          READ TABLE KEY_TAB WITH KEY = 'INSULATING FLANGE KIT'"99/12/20
                                                 BINARY SEARCH."99/12/20
          IF SY-SUBRC EQ 0.
            MOVE 'M' TO STOCK.
            MOVE 'GASKET' TO COMPOT.
            MOVE KEY_TAB-KEY TO SUB.
            MOVE 'Y' TO VAR21.
          ELSE.                                  "added flanges 99/12/20
            READ TABLE KEY_TAB WITH KEY = 'FLANGES'
                                                 BINARY SEARCH.
            IF SY-SUBRC EQ 0.
               MOVE 'M' TO STOCK.
               MOVE 'GASKET' TO COMPOT.
               MOVE KEY_TAB-KEY TO SUB.
               MOVE 'Y' TO VAR21.
            ELSE.
               MESSAGE I008 WITH VAR1 'KEYWORD' 'DIFFERENT KEYWORD'.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'OTHER_GEN_SUPPLIES'.
        SORT KEY_TAB BY KEY.
        LOOP AT KEY_TAB.
          READ TABLE KEY_TAB WITH KEY = 'SIGN' BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            MOVE 'M' TO STOCK.
            MOVE VAR2 TO COMPOT.
            MOVE KEY_TAB-KEY TO SUB.
          ENDIF.
        ENDLOOP.
      ENDIF.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'OTH_CORROSION_ACC'.
        SORT KEY_TAB BY KEY.
        LOOP AT KEY_TAB.
          READ TABLE KEY_TAB WITH KEY = 'ANODE' BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            MOVE 'M' TO STOCK.
            MOVE 'CATHODIC PROTECTION MATERIAL' TO COMPOT.
            MOVE KEY_TAB-KEY TO SUB.
          ELSE.
            READ TABLE KEY_TAB WITH KEY = 'CASING INSULATOR'
                                                          BINARY SEARCH.
            IF SY-SUBRC = 0.
              MOVE 'M' TO STOCK.
              MOVE 'CATHODIC PROTECTION MATERIAL' TO COMPOT.
              MOVE KEY_TAB-KEY TO SUB.
            ELSE.
              READ TABLE KEY_TAB WITH KEY = 'GROUND ROD' BINARY SEARCH.
              IF SY-SUBRC = 0.
                MOVE 'M' TO STOCK.
                MOVE 'CATHODIC PROTECTION MATERIAL' TO COMPOT.
                MOVE KEY_TAB-KEY TO SUB.
              ELSE.
*                read table key_tab with key = 'INSULATING ROD AND LUG'
*                                                        binary search.
*                if sy-subrc eq 0.
*                  move 'M' to stock.
*                  move 'RESTRAINT' to compot.
*                  move key_tab-key to sub.
*                else.
                  READ TABLE KEY_TAB WITH KEY = 'LINK SEAL'
                                             BINARY SEARCH.
                  IF SY-SUBRC EQ 0.
                    MOVE 'M' TO STOCK.
                    MOVE 'CATHODIC PROTECTION MATERIAL' TO COMPOT.
                    MOVE KEY_TAB-KEY TO SUB.
                  ELSE.
                    READ TABLE KEY_TAB WITH KEY = 'TEST BOX'
                                              BINARY SEARCH.
                    IF SY-SUBRC EQ 0.
                      MOVE 'M' TO STOCK.
                      MOVE 'CATHODIC PROTECTION MATERIAL' TO COMPOT.
                      MOVE KEY_TAB-KEY TO SUB.
                    ELSE.
                   MESSAGE I008 WITH VAR1 'KEYWORD' 'DIFFERENT KEYWORD'.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
*          endif.
        ENDLOOP.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'INSULATING_JOINTS'.
        SORT KEY_TAB BY KEY.
        LOOP AT KEY_TAB.
*         read table key_tab with key = 'WELD INSULATOR'.      "99/12/20
*                                            binary search.    "99/12/20
          READ TABLE KEY_TAB WITH KEY = 'INSULATED JOINTS'     "99/12/20
                                             BINARY SEARCH.    "99/12/20
          IF SY-SUBRC EQ 0.
            MOVE 'F' TO STOCK.
            MOVE VAR2 TO COMPOT.
            MOVE KEY_TAB-KEY TO SUB.
            MOVE 'Y' TO VAR21.
          ENDIF.
        ENDLOOP.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF VAR2 = 'PIPELINE_MATERIALS'.
        SORT KEY_TAB BY KEY.
        LOOP AT KEY_TAB.
          READ TABLE KEY_TAB WITH KEY = 'BOLT-ON WEIGHT' BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            MOVE 'M' TO STOCK.
            MOVE VAR2 TO COMPOT.
            MOVE KEY_TAB-KEY TO SUB.
          ELSE.
            READ TABLE KEY_TAB WITH KEY = 'O RING GASKET' BINARY SEARCH.
            IF SY-SUBRC = 0.
              MOVE 'P' TO STOCK.
              MOVE 'GASKET' TO COMPOT.
              MOVE KEY_TAB-KEY TO SUB.
            ELSE.
            READ TABLE KEY_TAB WITH KEY = 'SADDLE WEIGHT' BINARY SEARCH.
              IF SY-SUBRC = 0.
                MOVE 'M' TO STOCK.
                MOVE VAR2 TO COMPOT.
                MOVE KEY_TAB-KEY TO SUB.
              ELSE.
                READ TABLE KEY_TAB INDEX 1.
                MOVE 'M' TO STOCK.
                MOVE VAR2 TO COMPOT.
                MOVE KEY_TAB-KEY TO SUB.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PERFORM DETERMINE_MATERIAL.
      DESCRIBE TABLE MODEL_TAB LINES MOD_LINE.
      IF MOD_LINE EQ 0.
        COUNTER = COUNTER + 1.
        PERFORM GET_CONST.
        PERFORM SURE_STUFF.
        MOVE 'N/A'    TO RECORD-MODEL.
        MOVE 'N/A'    TO RECORD-MANUF.
        MOVE COUNTER  TO RECORD-UNIQUE_ID.
        IF FLAG EQ SPACE.
          TRANSFER RECORD TO DATAFILE LENGTH 456.
        ENDIF.
      ELSE.
        DATA:     L_STRING1(20),
                  L_STRING2(15),
                  DELIMITER   VALUE SPACE.
        LOOP AT MODEL_TAB.
          COUNTER = COUNTER + 1.
          SPLIT MODEL_TAB-MODEL AT DELIMITER INTO L_STRING1
                                                  L_STRING2.
          MOVE L_STRING1     TO     RECORD-MODEL.
          MOVE L_STRING2     TO     RECORD-MANUF.
          PERFORM GET_CONST.
          PERFORM SURE_STUFF.
          MOVE COUNTER  TO RECORD-UNIQUE_ID.
          IF FLAG EQ SPACE.
            TRANSFER RECORD TO DATAFILE LENGTH 456.
          ENDIF.
          CLEAR: L_STRING1, L_STRING2.
        ENDLOOP.
      ENDIF.

     CLEAR: VAR1, VAR2, VAR3, VAR4, VAR5, VAR6, VAR7, VAR8, VAR9, VAR10,
               VAR11, VAR12, VAR13, VAR14, VAR15, VAR16, VAR17, VAR18,
               VAR19, VAR20, VAR21, VAR22.
      CLEAR: STOCK, COMPOT, SUB.
      CLEAR: MAT_LINE, MOD_LINE, KEY_LINE.
      CLEAR: MAT_TAB, KEY_TAB, MODEL_TAB, STAT_TAB.
      REFRESH: MAT_TAB, KEY_TAB, MODEL_TAB, STAT_TAB.
    ENDAT.
  ENDLOOP.

  CLOSE DATASET DATAFILE.

*-----------------------------------------------------------------------
*         ~~~~~~~SUBROUTINES USED BY A MAIN PROGRAM~~~~~~~~            *
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*      FORM ASSIGN_CHAR
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine selects all characteristic and theirvalues for
*     a given material number.
*-----------------------------------------------------------------------
FORM ASSIGN_CHAR USING NUMBER.

  DATA: TEMP_MATNR LIKE AUSP-OBJEK.

  MOVE NUMBER TO TEMP_MATNR.
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

  DESCRIBE TABLE CHAR_TAB LINES LINE.
  IF LINE GE 1.
    LOOP AT CHAR_TAB.
      PERFORM GET_NAME_FOR_CHAR.
      CHECK NAME IN S_CHAR.
      PERFORM BUILD_BIG_TAB USING MARA-MATNR.
    ENDLOOP.
  ENDIF.

  CLEAR CHAR_TAB.
  REFRESH CHAR_TAB.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_NAME_FOR_CHAR
*-----------------------------------------------------------------------
*   Description:
*   - In this subroutine an internal number assigned by a system to a
*     characteristic is used to find a characteristic name.
*-----------------------------------------------------------------------
FORM GET_NAME_FOR_CHAR.
  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
            CLASS_TYPE                  = '001'
            FEATURE_ID                  = CHAR_TAB-ATINN
       IMPORTING
            FEATURE_NAME                = NAME
       EXCEPTIONS
            INVALID_CLASS_TYPE          = 1
            MISSING_FEATURE_INFORMATION = 2
            NO_FEATURE_FOUND            = 3
            NO_FEATURE_VALID            = 4
            NO_LANGUAGE                 = 5
            OTHERS                      = 6.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM BUILD_BIG_TAB.
*-----------------------------------------------------------------------
*   Description:
*   - For every material number all required information (material
*     number, class name, characteristic name and characteristic value)
*     extracted from a SAP system, is stored in the table.
*-----------------------------------------------------------------------
FORM BUILD_BIG_TAB USING NUMBER.
  MOVE NUMBER               TO BIG_TAB-MATNR.

* AM/FM requires short description                    2000/01/17
  MOVE 'N/A'                TO BIG_TAB-MAKTX.
  SELECT SINGLE * FROM MAKT
     WHERE MATNR = NUMBER.
  IF SY-SUBRC = '0'.
     MOVE MAKT-MAKTX        TO BIG_TAB-MAKTX.
  ENDIF.

  MOVE KLAH-CLASS           TO BIG_TAB-CLASS.
  MOVE NAME                 TO BIG_TAB-CHAR.
  IF CHAR_TAB-ATWRT NE SPACE.
    MOVE CHAR_TAB-ATWRT     TO BIG_TAB-VALUE.
  ELSE.
    WRITE CHAR_TAB-ATFLV EXPONENT 0 DECIMALS 1 LEFT-JUSTIFIED
                            TO BIG_TAB-VALUE.
  ENDIF.
  APPEND BIG_TAB.
  CLEAR BIG_TAB.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM DETERMINE_STATUS.
*-----------------------------------------------------------------------
*   Description:
*   - A given material can be listed accross different plants.  This
*     subroutine checks if there is an instance where this material
*     is denoted as 'obsolite' or 'do not issue' and based on the result
*     assignes value to VAR19.
*-----------------------------------------------------------------------
FORM DETERMINE_STATUS.

  SORT STAT_TAB BY STAT.
  LOOP AT STAT_TAB.
    READ TABLE STAT_TAB WITH KEY STAT ='01' BINARY SEARCH.
    IF SY-SUBRC = 0.
      VAR19 = 'N'.
    ELSE.
      READ TABLE STAT_TAB WITH KEY STAT ='03' BINARY SEARCH.
      IF SY-SUBRC = 0.
        VAR19 = 'N'.
      ELSE.
        VAR19 = 'Y'.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM DETERMINE_MATERIAL.
*-----------------------------------------------------------------------
*   Description:
*   - This soubroutine checks if there is a characteristic named
*     'MATERIAL' and how many times this characteristic occurs.  This is
*     used to assign value to VAR20.
*-----------------------------------------------------------------------
FORM DETERMINE_MATERIAL.
  DESCRIBE TABLE MAT_TAB LINES MAT_LINE.
  IF MAT_LINE = 0.
    VAR20 = 'N/A'.
  ELSE.
    READ TABLE MAT_TAB INDEX 1.
    MOVE MAT_TAB-MATERIAL TO VAR20.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM PRINT_ERRORS.
*-----------------------------------------------------------------------
*   Description:
*   - If for a given material number, there is more than one
*     characteristic named KEYWORD or MATERIAL, the infrormation error
*     message is issued.
*-----------------------------------------------------------------------
FORM PRINT_ERRORS.
  DESCRIBE TABLE KEY_TAB LINES KEY_LINE.
  IF KEY_LINE GT 1.
    MESSAGE I008 WITH VAR1 'KEYWORD' 'MORE THAN ONE VALUE EXISTS'.
  ENDIF.
  KEY_LINE = 0.

  DESCRIBE TABLE MAT_TAB LINES MAT_LINE.
  IF MAT_LINE GT 1.
    MESSAGE I008 WITH VAR1 'MATERIAL' 'MORE THAN ONE VALUE EXISTS'.
  ENDIF.
  MAT_LINE = 0.
ENDFORM.

*-----------------------------------------------------------------------
*    FORM SEARCH_TAB.
*-----------------------------------------------------------------------
*   Description:
*   - This routine reads consequtive records stored in BIG_TAB and
*     assignes values to VAR1...VAR18.  For multivalued characteristics
*     values are stored in tables.
*-----------------------------------------------------------------------
FORM SEARCH_TAB.

  IF ROW = 1.
    MOVE BIG_TAB-MATNR+12(6)     TO     VAR1.
    MOVE BIG_TAB-CLASS           TO     VAR2.
    MOVE BIG_TAB-MAKTX           TO     VAR22.
    ROW = ROW + 1.
  ENDIF.
  IF BIG_TAB-CHAR = 'PRIMARY_DIAMETER__NPS'.
    MOVE BIG_TAB-VALUE TO VAR3.
  ENDIF.
  IF BIG_TAB-CHAR = 'SECONDARY_DIAMETER__NPS'.
    MOVE BIG_TAB-VALUE TO VAR4.
  ENDIF.
  IF BIG_TAB-CHAR = 'PRIMARY_WALL_THICKNESS'.
    MOVE BIG_TAB-VALUE TO VAR5.
  ENDIF.
  IF BIG_TAB-CHAR = 'SECONDARY_WALL_THICKNESS'.
    MOVE BIG_TAB-VALUE TO VAR6.
  ENDIF.
  IF BIG_TAB-CHAR = 'END_TYPE'.
    MOVE BIG_TAB-VALUE TO VAR7.
  ENDIF.
  IF BIG_TAB-CHAR = 'FLANGE_TYPE'.
    MOVE BIG_TAB-VALUE TO VAR8.
  ENDIF.
  IF BIG_TAB-CHAR = 'COATING'.
    MOVE BIG_TAB-VALUE TO VAR9.
  ENDIF.
  IF BIG_TAB-CHAR = 'CATEGORY'.
    MOVE BIG_TAB-VALUE TO VAR10.
  ENDIF.
  IF BIG_TAB-CHAR = 'GRADE'.
    MOVE BIG_TAB-VALUE TO VAR11.
  ENDIF.
  IF BIG_TAB-CHAR = 'PRESSURE_RATING'.
    MOVE BIG_TAB-VALUE TO VAR12.
  ENDIF.
  IF BIG_TAB-CHAR = 'SPECIFICATION'.
    MOVE BIG_TAB-VALUE TO VAR13.
  ENDIF.
  IF BIG_TAB-CHAR = 'NUMBER_OF_TURNS'.
    MOVE BIG_TAB-VALUE TO VAR14.
  ENDIF.
  IF BIG_TAB-CHAR = 'MAXIMUM_COMPONENT_PRESSURE_KPA'.
    MOVE BIG_TAB-VALUE TO VAR15.
  ENDIF.
  IF BIG_TAB-CHAR = 'PRIMARY_DIAMETER__MM'.
    MOVE BIG_TAB-VALUE TO VAR16.
  ENDIF.
  IF BIG_TAB-CHAR = 'SECONDARY_DIAMETER__MM'.
    MOVE BIG_TAB-VALUE TO VAR17.
  ENDIF.
  IF BIG_TAB-CHAR = 'LENGTH'.
    MOVE BIG_TAB-VALUE TO VAR18.
  ENDIF.
  IF BIG_TAB-CHAR = 'MATERIAL'.
    CLEAR MAT_TAB.
    MOVE BIG_TAB-VALUE TO MAT_TAB-MATERIAL.
    APPEND MAT_TAB.
    CLEAR MAT_TAB.
  ENDIF.
  IF BIG_TAB-CHAR = 'KEYWORD'.
    CLEAR KEY_TAB.
    MOVE BIG_TAB-VALUE TO KEY_TAB-KEY.
    APPEND KEY_TAB.
    CLEAR KEY_TAB.
  ENDIF.

  IF BIG_TAB-CHAR = 'MODEL_NUMBER'.
    CLEAR MODEL_TAB.
    MOVE BIG_TAB-VALUE TO MODEL_TAB-MODEL.
    APPEND MODEL_TAB.
    CLEAR MODEL_TAB.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM GET_FILE.
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine opens a file for writting.  If this attempt fails

*     the error message is displayed.
*-----------------------------------------------------------------------
FORM GET_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            CLIENT           = SY-MANDT
            LOGICAL_FILENAME = LOGICFLE
            OPERATING_SYSTEM = SY-OPSYS
       IMPORTING
            FILE_NAME        = DATAFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1.

  DATA:     MESS(100).

  OPEN DATASET DATAFILE FOR OUTPUT IN TEXT MODE MESSAGE MESS.
  IF SY-SUBRC NE 0.
    MESSAGE E009 WITH LOGICFLE MESS.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*      FORM GET_CONST.
*-----------------------------------------------------------------------
*   Description:
*   - There are several fields in the file that do not change.  This
*     subroutine assignes values to these fields.
*-----------------------------------------------------------------------
FORM GET_CONST.
  MOVE '^' TO RECORD-DUMY1.
  MOVE '^' TO RECORD-DUMY2.
  MOVE '^' TO RECORD-DUMY3.
  MOVE '^' TO RECORD-DUMY4.
  MOVE '^' TO RECORD-DUMY5.
  MOVE '^' TO RECORD-DUMY6.
  MOVE '^' TO RECORD-DUMY7.
  MOVE '^' TO RECORD-DUMY8.
  MOVE '^' TO RECORD-DUMY9.
  MOVE '^' TO RECORD-DUMY10.
  MOVE '^' TO RECORD-DUMY11.
  MOVE '^' TO RECORD-DUMY12.
  MOVE '^' TO RECORD-DUMY13.
  MOVE '^' TO RECORD-DUMY14.
  MOVE '^' TO RECORD-DUMY15.
  MOVE '^' TO RECORD-DUMY16.
  MOVE '^' TO RECORD-DUMY17.
  MOVE '^' TO RECORD-DUMY18.
  MOVE '^' TO RECORD-DUMY19.
  MOVE '^' TO RECORD-DUMY20.
  MOVE '^' TO RECORD-DUMY21.
  MOVE '^' TO RECORD-DUMY22.
  MOVE '       ' TO RECORD-USER_ID.
  MOVE '^' TO RECORD-DUMY23.
  MOVE '^' TO RECORD-DUMY24.
  MOVE '^' TO RECORD-DUMY25.
  MOVE '^' TO RECORD-DUMY26.
  MOVE '^' TO RECORD-DUMY27.
  MOVE '^' TO RECORD-DUMY28.
  MOVE '^^^^^^^^' TO RECORD-DUMY29.
  MOVE '^' TO RECORD-DUMY30.
  MOVE '^^^' TO RECORD-DUMY31.
*  move '                                     ' to record-comments."#738
  MOVE '0000000000' TO RECORD-DATE.
  MOVE '0000000000' TO RECORD-TIME.
  MOVE 'A' TO RECORD-ACCESS.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM SURE_STUFF.
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine writes to the RECORD all data that is independent
*     from characteristic 'MODEL_NUMBER'.
*-----------------------------------------------------------------------
FORM SURE_STUFF.
  MOVE VAR1         TO RECORD-STOCK_NR.
  MOVE COMPOT       TO RECORD-COPM_TYPE.
  MOVE SUB          TO RECORD-SUB_COMP.
  MOVE VAR20        TO RECORD-MATERIAL.
  MOVE VAR22        TO RECORD-COMMENTS.                           " #738
  IF VAR3 EQ SPACE.
    MOVE 'N/A'     TO RECORD-PRI_SIZE_NPS.
  ELSE.
    MOVE VAR3      TO RECORD-PRI_SIZE_NPS.
  ENDIF.
  IF VAR4 EQ SPACE.
    MOVE 'N/A'     TO RECORD-SEC_SIZE_NPS.
  ELSE.
    MOVE VAR4      TO RECORD-SEC_SIZE_NPS.
  ENDIF.
  IF VAR5 EQ SPACE.
    MOVE '0'       TO RECORD-PRI_WALL_1.
    MOVE '0'       TO RECORD-PRI_WALL_2.
  ELSE.
    MOVE VAR5      TO RECORD-PRI_WALL_1.
    MOVE VAR5      TO RECORD-PRI_WALL_2.
  ENDIF.
  IF VAR6 EQ SPACE.
    MOVE '0'       TO RECORD-SEC_WALL_1.
    MOVE '0'       TO RECORD-SEC_WALL_2.
  ELSE.
    MOVE VAR6      TO RECORD-SEC_WALL_1.
    MOVE VAR6      TO RECORD-SEC_WALL_2.
  ENDIF.
  IF VAR7 EQ SPACE.
    MOVE 'N/A'     TO RECORD-END_TYPE.
  ELSE.
    MOVE VAR7      TO RECORD-END_TYPE.
  ENDIF.
  IF VAR8 EQ SPACE.
    MOVE 'N/A'     TO RECORD-FLANGE_TYPE.
  ELSE.
    MOVE VAR8      TO RECORD-FLANGE_TYPE.
  ENDIF.
*~~~
  IF VAR21 EQ SPACE.
    MOVE 'N'       TO RECORD-INSULATED.
  ELSE.
    MOVE VAR21     TO RECORD-INSULATED.
  ENDIF.
*~~~
  IF VAR9 EQ SPACE.
    MOVE 'N/A'     TO RECORD-COATING.
  ELSE.
    MOVE VAR9      TO RECORD-COATING.
  ENDIF.
  IF VAR10 EQ SPACE.
    MOVE 'N/A'     TO RECORD-MAT_CAT.
  ELSE.
    MOVE VAR10     TO RECORD-MAT_CAT.
  ENDIF.

  IF VAR11 EQ SPACE.
    MOVE '0'       TO RECORD-MAT_GRADE.
  ELSE.
    DATA     LOC_LENGTH     TYPE I.
    LOC_LENGTH = STRLEN( VAR11 ).
    IF LOC_LENGTH GT 3.
      MESSAGE I008 WITH VAR1 'GRADE' 'MORE THAN 3 CHARACTERS'.
      MOVE VAR11+0(3) TO RECORD-MAT_GRADE.
    ENDIF.
    IF LOC_LENGTH LE 3.
      MOVE VAR11 TO RECORD-MAT_GRADE.
    ENDIF.
  ENDIF.

  IF VAR12 EQ SPACE.
    IF VAR13 EQ SPACE.
      MOVE 'N/A'     TO RECORD-SPEC.
    ELSE.
      MOVE VAR13     TO RECORD-SPEC.
    ENDIF.
  ELSE.
    IF VAR13 EQ SPACE.
      MOVE VAR12     TO RECORD-SPEC.
    ELSE.
      CONCATENATE VAR12 VAR13 INTO VAR12 SEPARATED BY SPACE.
      MOVE VAR12     TO RECORD-SPEC.
    ENDIF.
  ENDIF.

  IF VAR14 EQ SPACE.
    MOVE 'N/A'        TO RECORD-NR_OF_TURNS.
  ELSE.
    MOVE VAR14        TO RECORD-NR_OF_TURNS.
  ENDIF.

  IF VAR15 EQ SPACE.
    MOVE 'N/A'        TO RECORD-PRESSURE.
  ELSE.
    MOVE VAR15        TO RECORD-PRESSURE.
  ENDIF.

  MOVE STOCK           TO RECORD-STOCK_GR.

  IF VAR16 EQ SPACE.
    MOVE '0'          TO RECORD-PRI_SIZE_ALT.
  ELSE.
    MOVE VAR16        TO RECORD-PRI_SIZE_ALT.
  ENDIF.

  IF VAR17 EQ SPACE.
    MOVE '0'          TO RECORD-SEC_SIZE_ALT.
  ELSE.
    MOVE VAR17        TO RECORD-SEC_SIZE_ALT.
  ENDIF.

  MOVE VAR19           TO RECORD-APPROVED.

  IF VAR18 EQ SPACE.
    MOVE '0'          TO RECORD-DIM.
  ELSE.
    MOVE VAR18       TO RECORD-DIM.
  ENDIF.
ENDFORM.
*-----------------------------------------------------------------------
