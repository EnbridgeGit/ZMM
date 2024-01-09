REPORT ZMIMR017 LINE-SIZE 132 LINE-COUNT 65 MESSAGE-ID ZM
NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Report ZMIMR017
* Date: May 07, 1998
* Purposed - to compare the rate of turnover for specified materials
* for the current and previous 12 month periods.
* Author: Nesh N. Laurencic - Omnilogic Systems Group
* This program is another version of ZMIMR007
*
*-----------------------------------------------------------------------
*
* Note:
*        This is new version of old program.
*-----------------------------------------------------------------------
* 97/04/17 md7140 Fix AVG-MONTHS
* 97/04/17 md7140 Fix TOTAL-MONTH-CNTR & TOTAL-LOC-CNTR.
* 98/05/07 Nesh Lurencic - Omnilogic Systems Group /fix
*-----------------------------------------------------------------------
TABLES: S031,       "Statistics: Movements for current stocks
        S032,       "Statistics: Current Stock and grouping Terms
        MARA.                          "Material Master: General Data

TYPES: BEGIN OF TURNOVER_SORT_REC,
       TURNOVER_SORT_FIELD(30),
       END OF TURNOVER_SORT_REC.

DATA: TURNOVER_SORT_TABLE TYPE TURNOVER_SORT_REC OCCURS 4
          WITH HEADER LINE.

DATA: TOTAL_CONS1 LIKE S031-MGVBR, "Total consumption quantity period 1
      TOTAL_CONS2 LIKE S031-MGVBR, "Total consumption quantity period 2
      TOTAL_AV1   LIKE S031-MZUBB,     " Total average period 1
      TOTAL_AV2   LIKE S031-MZUBB.     " Tota; average period 2


DATA: P1_START LIKE S031-SPMON,
      P1_END   LIKE S031-SPMON,
      P2_START LIKE S031-SPMON,
      P2_END   LIKE S031-SPMON.
DATA: TEMP(4) TYPE N,                  " For year
      TEMP1(2) TYPE N.                 " For period
DATA: NUNMBER TYPE I.

DATA: TABIX LIKE SY-TABIX,             "To ount int. tables rows
      DIF   LIKE SY-TABIX VALUE 12.    "To populate the rest

DATA: BEGIN OF  ITABLE_S032 OCCURS 10.
        INCLUDE STRUCTURE S032.
DATA: END OF ITABLE_S032.

DATA: BEGIN OF ITABLE_S031 OCCURS 10,
      SPMON LIKE S031-SPMON,
      WERKS LIKE S031-WERKS,
      MATNR LIKE S031-MATNR,
      LGORT LIKE S031-LGORT,
      MZUBB LIKE S031-MZUBB,
      MAGBB LIKE S031-MAGBB,
      MGVBR LIKE S031-MGVBR,
      END OF ITABLE_S031.


*Current period
DATA: BEGIN OF ITAB OCCURS 10,
      MATKL LIKE S032-MATKL,           "Material group
      MATNR LIKE S032-MATNR,           "Material number
      FIRST LIKE S031-MZUBB,                                "
      LAST LIKE  S031-MZUBB,
      AVERAGE LIKE S031-MZUBB,
      CONS    LIKE S031-MGVBR,         " Consumption
      PERIOD(2) TYPE C,
      WERKS    LIKE S031-WERKS,        "Plant
      LGORT    LIKE S031-LGORT,        "Storage location
      SPMON    LIKE S031-SPMON,        "Period to analyze - month
      ORDER(2) TYPE N ,                " To keep the order
      END OF ITAB.
* Previous period
DATA: BEGIN OF ITAB1 OCCURS 100.
        INCLUDE STRUCTURE ITAB.
DATA: END OF ITAB1.

* SUM internal table ( contans average quantity per material ) Curr. per
DATA: BEGIN OF SUM_TAB1 OCCURS 100.
        INCLUDE STRUCTURE ITAB.
DATA: END OF SUM_TAB1.

* SUM internal table ( contans average quantity per material ) Prev. per
DATA: BEGIN OF SUM_TAB2 OCCURS 100.
        INCLUDE STRUCTURE ITAB.
DATA: END OF SUM_TAB2.

DATA: FLAG TYPE C,                "Just filter to ensure one time pass
      SWITCH TYPE C.                   " 2 times all section flag
DATA: COUNTER(2) TYPE N,               " Period counter
      GAP_CALC(2) TYPE N.              " For period diff. calc.
SELECT-OPTIONS SWERKS FOR S032-WERKS OBLIGATORY.
SELECT-OPTIONS SLGORT FOR S032-LGORT .
SELECT-OPTIONS SMATKL FOR S032-MATKL.
SELECT-OPTIONS SMATNR FOR S032-MATNR MATCHCODE OBJECT MAT1.

DATA: CREATION LIKE MARA-ERSDA,        " Date of creation
      DATECHCK LIKE MARA-ERSDA,        " For Date of creation check
      AVERAGE  LIKE ITAB-AVERAGE, " Average stock, just for adjustment
      AVRCOUNT TYPE I,                 " re. creation dates of material
      SUMITAB  TYPE I,                                      "Sum
      SUMCONS LIKE S031-MGVBR,         "Total consumption quantity
      SUMCONS1 LIKE S031-MGVBR.        "Total consumption quantity

DATA: PERIOD2S LIKE P2_START,
      PERIOD2E LIKE P2_END.

DATA: AVERAGE1(12) TYPE C,
      AVERAGE2(12) TYPE C,
      SUMC1(12) TYPE C,
      SUMC2(12) TYPE C,
      TURNOVER TYPE P DECIMALS 2,
      TURN(5) TYPE C.

DATA: GO_OUT TYPE P VALUE '0'.

AT SELECTION-SCREEN.
  IF ( SMATKL-LOW IS INITIAL ) AND ( SMATNR-LOW IS INITIAL ).
    MESSAGE E018.
  ENDIF.

IF ( ( NOT  SMATKL-LOW IS INITIAL ) AND ( NOT SMATNR-LOW IS INITIAL ) ).
    MESSAGE E018.
  ENDIF.

START-OF-SELECTION.

* Find starting and ending periods
  PERFORM FIND_STARTING_AND_ENDING_P.

* Just store this second period for later
  MOVE:
        P2_START TO PERIOD2S,
        P2_END   TO PERIOD2E.

* Loop through the current stock file, to find all relevant materials
  SELECT * INTO TABLE ITABLE_S032
               FROM   S032 WHERE
                      SSOUR = SPACE     AND
                      VRSIO = '000'     AND
                      WERKS IN SWERKS   AND
                      LGORT IN SLGORT   AND
                      MATNR IN SMATNR   AND
                      MATKL IN SMATKL.
  SORT ITABLE_S032 BY MATNR MATKL.

  LOOP AT ITABLE_S032.
    CLEAR ITAB.
* Just for period 2
    SELECT SPMON WERKS MATNR LGORT MZUBB MAGBB MGVBR
             INTO ITABLE_S031
             FROM S031    WHERE
                              SSOUR = SPACE AND
                              VRSIO = '000' AND
                              SPMON GE P1_START   AND "This is changed
                              SPMON LE P2_END     AND
                              SPTAG = '00000000'  AND
                              SPWOC = '000000'    AND
                              WERKS EQ ITABLE_S032-WERKS AND
                              MATNR EQ ITABLE_S032-MATNR AND
                              LGORT EQ ITABLE_S032-LGORT.
      APPEND ITABLE_S031.
    ENDSELECT.
  ENDLOOP.
  CHECK SY-SUBRC = 0.

  SORT ITABLE_S031 BY MATNR SPMON DESCENDING.

  LOOP AT ITABLE_S032.
    REFRESH: ITAB, ITAB1.
    CLEAR:   ITAB, ITAB1.
    CLEAR CREATION.                    "Clear creation date
    PERFORM FIND_THE_CREATION_DATE. "Find creation date of the material
    COUNTER = P2_END+4(2).
    COUNTER = COUNTER + 1.
    CLEAR: FLAG, SWITCH.
    CLEAR: ITABLE_S031,
           SUM_TAB1, SUM_TAB2.


    LOOP AT ITABLE_S031 WHERE
                           WERKS EQ ITABLE_S032-WERKS AND
                           MATNR EQ ITABLE_S032-MATNR AND
                           LGORT EQ ITABLE_S032-LGORT.

      COUNTER = COUNTER - 1.           "Counter period
      IF COUNTER = '00'.
        COUNTER = '12'.
      ENDIF.
      WRITE ITABLE_S031-SPMON+4(2) TO TEMP1.    "Period
*break-point.
      IF ITABLE_S031-SPMON LE CREATION+0(6) OR
         ITABLE_S031-SPMON LE '199612'.
        CONTINUE.
      ENDIF.


   IF ( ITABLE_S031-SPMON GE P2_START AND ITABLE_S031-SPMON LE P2_END ).

* This if statement will execute just the first time maybe...
        IF TEMP1 = COUNTER AND FLAG = 0.  "Just the first time
          PERFORM FIRST_TIME_LOOP.
          CONTINUE.
        ENDIF.

        IF TEMP1 <> COUNTER AND FLAG = 0. "Just the first time
          PERFORM FIRST_TIME_LOOP_CASE.
          CONTINUE.
        ENDIF.

* This will happen every other time
        IF TEMP1 = COUNTER AND FLAG = '1'.
          PERFORM REGULAR_LOOP_CASE.
          CONTINUE.
        ENDIF.

        IF TEMP1 <> COUNTER AND FLAG = '1'.
          PERFORM REGULAR_LOOP_CASE2.
          CONTINUE.
        ENDIF.

      ELSE.                            "Then populate itab1

* This if statement will execute just the first time maybe...
        IF TEMP1 = COUNTER AND FLAG = 0.  "Just the first time
          PERFORM FIRST_TIME_LOOP_P1.                       "Itab1
          CONTINUE.
        ENDIF.

        IF TEMP1 <> COUNTER AND FLAG = 0. "Just the first time
          PERFORM FIRST_TIME_LOOP_CASE_P1.
          CONTINUE.
        ENDIF.

* This will happen every other time
        IF TEMP1 = COUNTER AND FLAG = '1'.    "Flag - not the first loop
          PERFORM REGULAR_LOOP_CASE_P1.
          CONTINUE.
        ENDIF.

        IF TEMP1 <> COUNTER AND FLAG = '1'.
          PERFORM REGULAR_LOOP_CASE2_P1.
          CONTINUE.
        ENDIF.


      ENDIF.

    ENDLOOP.

    PERFORM GENERATE_SUM_TABLE.

  ENDLOOP.

*break-point.
* Prepare for printing
  REFRESH: ITAB, ITAB1.
  LOOP AT SUM_TAB1.
    AT LAST.
      SUM.
      APPEND SUM_TAB1 TO ITAB.
    ENDAT.
  ENDLOOP.

  LOOP AT SUM_TAB2.
    AT LAST.
      SUM.
      APPEND SUM_TAB2 TO ITAB1.
    ENDAT.
  ENDLOOP.

*
*
  LOOP AT ITAB1.
    AT LAST.
      SUM.
*      itab1-last = itab1-last / 12.
      WRITE ITAB1-LAST TO AVERAGE1 LEFT-JUSTIFIED.

      WRITE:/1 AVERAGE1(12) .

      TURNOVER =  ( ITAB1-CONS / ITAB1-LAST ) .
      WRITE TURNOVER TO TURN.

      WRITE TURN UNDER TEXT-044.

      WRITE:  ITAB1-CONS TO SUMC1 LEFT-JUSTIFIED.

      WRITE:  SUMC1 UNDER TEXT-033.

    ENDAT.
  ENDLOOP.

  LOOP AT ITAB.
    AT LAST.
*      break-point.
      SUM.
*      itab-last = itab-last / 12.
      WRITE ITAB-LAST TO AVERAGE2 LEFT-JUSTIFIED.
      WRITE:  AVERAGE2 UNDER TEXT-HE2.
      TURNOVER =  ( ITAB-CONS / ITAB-LAST ) .
      WRITE TURNOVER TO TURN.
      CLEAR TURNOVER.

      WRITE  TURN UNDER TEXT-004.

      WRITE: ITAB-CONS TO SUMC2 LEFT-JUSTIFIED.
      WRITE: SUMC2 UNDER TEXT-003.

    ENDAT.
  ENDLOOP.
*
*
*
*
TOP-OF-PAGE.
  FORMAT RESET.
  FORMAT COLOR 1.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         25 SY-TITLE COLOR 4 INTENSIFIED ON,
        100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID.
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  SKIP.
  WRITE: /1 TEXT-PLN, SWERKS-LOW NO-GAP.
  IF NOT SWERKS-HIGH IS INITIAL.
    WRITE: '-' NO-GAP, SWERKS-HIGH.
  ENDIF.

  WRITE: /1 TEXT-LGO, SLGORT-LOW NO-GAP.
  IF NOT SLGORT-HIGH IS INITIAL.
    WRITE: '-' NO-GAP, SLGORT-HIGH.
  ENDIF.

  WRITE: /1 TEXT-MTG, SMATKL-LOW(4) NO-GAP.
  IF NOT SMATKL-HIGH IS INITIAL.
    WRITE: '-' NO-GAP, SMATKL-HIGH.
  ENDIF.

  WRITE: /1 TEXT-MAT, SMATNR-LOW+12(6) NO-GAP.
  IF NOT SMATNR-HIGH IS INITIAL.
    WRITE: '-' NO-GAP, SMATNR-HIGH.
  ENDIF.
  SKIP.
  WRITE: /1 TEXT-HE1, P1_START, 'to', P1_END.
  WRITE: 60 TEXT-HE2, PERIOD2S, 'to', PERIOD2E.
  ULINE.
  SKIP.
  FORMAT COLOR 4.
  WRITE:  TEXT-002 UNDER TEXT-HE1, 20 TEXT-033, 36 TEXT-044.
  WRITE:  TEXT-002 UNDER TEXT-HE2, 82 TEXT-003, 102 TEXT-004.
  FORMAT RESET.

END-OF-PAGE.

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIRST_TIME_LOOP.

  ITAB-LAST = ITABLE_S032-MBWBEST.
  ITAB-PERIOD = COUNTER.
  ITAB-FIRST =
         ITAB-LAST + ITABLE_S031-MAGBB - ITABLE_S031-MZUBB.
  ITAB-CONS = ITABLE_S031-MGVBR.       "Consumption

  ITAB-MATNR = ITABLE_S031-MATNR.
  ITAB-MATKL = ITABLE_S032-MATKL.
  ITAB-WERKS = ITABLE_S031-WERKS.      "Plant
  ITAB-LGORT = ITABLE_S031-LGORT.      " Storage Location
  ITAB-SPMON = ITABLE_S031-SPMON.
  APPEND ITAB.
  FLAG = 1.

ENDFORM.                               " FIRST_TIME_LOOP

*&---------------------------------------------------------------------*
*&      Form  FIRST_TIME_LOOP_CASE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIRST_TIME_LOOP_CASE.
  ITAB-LAST = ITABLE_S032-MBWBEST.
  ITAB-PERIOD = COUNTER.
  ITAB-FIRST = ITAB-LAST.

  ITAB-CONS = '0'.                     "Consumption
  ITAB-MATNR = ITABLE_S032-MATNR.
  ITAB-MATKL = ITABLE_S032-MATKL.
  ITAB-WERKS = ITABLE_S032-WERKS.      "Plant
  ITAB-LGORT = ITABLE_S032-LGORT.      " Storage Location
*  itab-spmon = itable_s031-spmon.
  APPEND ITAB.
  FLAG = 1.

  COUNTER = COUNTER - 1.
  IF COUNTER = '0'.
    COUNTER = '12'.
  ENDIF.

  WHILE COUNTER <> TEMP1.
    ITAB-PERIOD = COUNTER.
    APPEND ITAB.

    COUNTER = COUNTER - 1.
    IF COUNTER = '0'.
      COUNTER = '12'.
    ENDIF.
  ENDWHILE.

* Now temp1 = counter
  ITAB-LAST = ITABLE_S032-MBWBEST.
  ITAB-PERIOD = COUNTER.
  ITAB-FIRST =
         ITAB-LAST + ITABLE_S031-MAGBB - ITABLE_S031-MZUBB.
  ITAB-CONS = ITABLE_S031-MGVBR.       "Consumption

  ITAB-MATNR = ITABLE_S031-MATNR.
  ITAB-MATKL = ITABLE_S032-MATKL.
  ITAB-WERKS = ITABLE_S031-WERKS.      "Plant
  ITAB-LGORT = ITABLE_S031-LGORT.      " Storage Location
  ITAB-SPMON = ITABLE_S031-SPMON.
  APPEND ITAB.

ENDFORM.                               " FIRST_TIME_LOOP_CASE
*&---------------------------------------------------------------------*
*&      Form  REGULAR_LOOP_CASE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REGULAR_LOOP_CASE.
* Now temp1 = counter
  ITAB-LAST = ITAB-FIRST.
  ITAB-PERIOD = COUNTER.
  ITAB-FIRST =
         ITAB-LAST + ITABLE_S031-MAGBB - ITABLE_S031-MZUBB.
  ITAB-CONS = ITABLE_S031-MGVBR.       "Consumption

  ITAB-MATNR = ITABLE_S031-MATNR.
  ITAB-MATKL = ITABLE_S032-MATKL.
  ITAB-WERKS = ITABLE_S031-WERKS.      "Plant
  ITAB-LGORT = ITABLE_S031-LGORT.      " Storage Location
  ITAB-SPMON = ITABLE_S031-SPMON.
  APPEND ITAB.

ENDFORM.                               " REGULAR_LOOP_CASE

*&---------------------------------------------------------------------*
*&      Form  REGULAR_LOOP_CASE2
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REGULAR_LOOP_CASE2.
* Now temp1 <> counter
  ITAB-LAST = ITAB-FIRST.
  ITAB-PERIOD = COUNTER.
  ITAB-CONS = '0'.                     "Consumption

  ITAB-MATNR = ITABLE_S032-MATNR.
  ITAB-MATKL = ITABLE_S032-MATKL.
  ITAB-WERKS = ITABLE_S032-WERKS.      "Plant
  ITAB-LGORT = ITABLE_S032-LGORT.      " Storage Location
  APPEND ITAB.

  COUNTER = COUNTER - 1.
  IF COUNTER = '0'.
    COUNTER = '12'.
  ENDIF.

  WHILE COUNTER <> TEMP1.
    ITAB-PERIOD = COUNTER.
    APPEND ITAB.

    COUNTER = COUNTER - 1.
    IF COUNTER = '0'.
      COUNTER = '12'.
    ENDIF.
  ENDWHILE.

* Now temp1 = counter
  ITAB-LAST = ITAB-FIRST.
  ITAB-PERIOD = COUNTER.
  ITAB-FIRST =
         ITAB-LAST + ITABLE_S031-MAGBB - ITABLE_S031-MZUBB.
  ITAB-CONS = ITABLE_S031-MGVBR.       "Consumption

  ITAB-MATNR = ITABLE_S031-MATNR.
  ITAB-MATKL = ITABLE_S032-MATKL.
  ITAB-WERKS = ITABLE_S031-WERKS.      "Plant
  ITAB-LGORT = ITABLE_S031-LGORT.      " Storage Location
  ITAB-SPMON = ITABLE_S031-SPMON.
  APPEND ITAB.

ENDFORM.                               " REGULAR_LOOP_CASE2
*&---------------------------------------------------------------------*
*&      Form  FIRST_TIME_LOOP_P1
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIRST_TIME_LOOP_P1.
  ITAB1-LAST = ITABLE_S032-MBWBEST.
  ITAB1-PERIOD = COUNTER.
  ITAB1-FIRST =
         ITABLE_S032-MBWBEST + ITABLE_S031-MAGBB - ITABLE_S031-MZUBB.
  ITAB1-CONS = ITABLE_S031-MGVBR.      "Consumption

  ITAB1-MATNR = ITABLE_S031-MATNR.
  ITAB1-MATKL = ITABLE_S032-MATKL.
  ITAB1-WERKS = ITABLE_S031-WERKS.     "Plant
  ITAB1-LGORT = ITABLE_S031-LGORT.     " Storage Location
  ITAB1-SPMON = ITABLE_S031-SPMON.
  APPEND ITAB1.
  FLAG = 1.

ENDFORM.                               " FIRST_TIME_LOOP_P1

*&---------------------------------------------------------------------*
*&      Form  FIRST_TIME_LOOP_CASE_P1
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIRST_TIME_LOOP_CASE_P1.
  ITAB1-LAST = ITABLE_S032-MBWBEST.
  ITAB1-PERIOD = COUNTER.
  ITAB1-FIRST = ITAB1-LAST.

  ITAB1-CONS = '0'.                    "Consumption
  ITAB1-MATNR = ITABLE_S032-MATNR.
  ITAB1-MATKL = ITABLE_S032-MATKL.
  ITAB1-WERKS = ITABLE_S032-WERKS.     "Plant
  ITAB1-LGORT = ITABLE_S032-LGORT.     " Storage Location
*  itab-spmon = itable_s031-spmon.
  APPEND ITAB1.
  FLAG = 1.

  COUNTER = COUNTER - 1.
  IF COUNTER = '0'.
    COUNTER = '12'.
  ENDIF.

  WHILE COUNTER <> TEMP1.
    ITAB1-PERIOD = COUNTER.
    APPEND ITAB1.

    COUNTER = COUNTER - 1.
    IF COUNTER = '0'.
      COUNTER = '12'.
    ENDIF.
  ENDWHILE.

* Now temp1 = counter
  ITAB1-LAST = ITAB1-FIRST.            "
  ITAB1-PERIOD = COUNTER.
  ITAB1-FIRST =
         ITAB1-LAST + ITABLE_S031-MAGBB - ITABLE_S031-MZUBB.
  ITAB1-CONS = ITABLE_S031-MGVBR.      "Consumption

  ITAB1-MATNR = ITABLE_S031-MATNR.
  ITAB1-MATKL = ITABLE_S032-MATKL.
  ITAB1-WERKS = ITABLE_S031-WERKS.     "Plant
  ITAB1-LGORT = ITABLE_S031-LGORT.     " Storage Location
  ITAB1-SPMON = ITABLE_S031-SPMON.
  APPEND ITAB1.

ENDFORM.                               " FIRST_TIME_LOOP_CASE_P1

*&---------------------------------------------------------------------*
*&      Form  REGULAR_LOOP_CASE_P1
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REGULAR_LOOP_CASE_P1.
* Now temp1 = counter
  IF SWITCH EQ SPACE.
    ITAB1-LAST = ITAB-FIRST.           "Notice table itab
    SWITCH = '1'.
  ELSE.
    ITAB1-LAST = ITAB1-FIRST.
  ENDIF.
  ITAB1-PERIOD = COUNTER.
  ITAB1-FIRST =
         ITAB1-LAST + ITABLE_S031-MAGBB - ITABLE_S031-MZUBB.
  ITAB1-CONS = ITABLE_S031-MGVBR.      "Consumption

  ITAB1-MATNR = ITABLE_S031-MATNR.
  ITAB1-MATKL = ITABLE_S032-MATKL.
  ITAB1-WERKS = ITABLE_S031-WERKS.     "Plant
  ITAB1-LGORT = ITABLE_S031-LGORT.     " Storage Location
  ITAB1-SPMON = ITABLE_S031-SPMON.
  APPEND ITAB1.

ENDFORM.                               " REGULAR_LOOP_CASE_P1

*&---------------------------------------------------------------------*
*&      Form  REGULAR_LOOP_CASE2_P1
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REGULAR_LOOP_CASE2_P1.
* Now temp1 <> counter
  IF SWITCH EQ SPACE.
    ITAB1-LAST = ITAB-FIRST.
    SWITCH = '1'.
  ELSE.
    ITAB1-LAST = ITAB1-FIRST.
  ENDIF.
  ITAB1-PERIOD = COUNTER.
  ITAB1-CONS = '0'.                    "Consumption

  ITAB1-MATNR = ITABLE_S032-MATNR.
  ITAB1-MATKL = ITABLE_S032-MATKL.
  ITAB1-WERKS = ITABLE_S032-WERKS.     "Plant
  ITAB1-LGORT = ITABLE_S032-LGORT.     " Storage Location
  APPEND ITAB1.

  COUNTER = COUNTER - 1.
  IF COUNTER = '0'.
    COUNTER = '12'.
  ENDIF.

  WHILE COUNTER <> TEMP1.
    ITAB1-PERIOD = COUNTER.
    APPEND ITAB1.

    COUNTER = COUNTER - 1.
    IF COUNTER = '0'.
      COUNTER = '12'.
    ENDIF.
  ENDWHILE.

* Now temp1 = counter
  ITAB1-LAST = ITAB1-FIRST.
  ITAB1-PERIOD = COUNTER.
  ITAB1-FIRST =
         ITAB1-LAST + ITABLE_S031-MAGBB - ITABLE_S031-MZUBB.
  ITAB1-CONS = ITABLE_S031-MGVBR.      "Consumption

  ITAB1-MATNR = ITABLE_S031-MATNR.
  ITAB1-MATKL = ITABLE_S032-MATKL.
  ITAB1-WERKS = ITABLE_S031-WERKS.     "Plant
  ITAB1-LGORT = ITABLE_S031-LGORT.     " Storage Location
  ITAB1-SPMON = ITABLE_S031-SPMON.
  APPEND ITAB1.

ENDFORM.                               " REGULAR_LOOP_CASE2_P1

*&---------------------------------------------------------------------*
*&      Form  FIND_THE_CREATION_DATE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_THE_CREATION_DATE.
  SELECT SINGLE * FROM MARA WHERE
                                 MATNR EQ ITABLE_S032-MATNR.
  CHECK SY-SUBRC = 0.
  MOVE: MARA-ERSDA TO CREATION.

ENDFORM.                               " FIND_THE_CREATION_DATE
*&---------------------------------------------------------------------*
*&      Form  FIND_STARTING_AND_ENDING_P
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_STARTING_AND_ENDING_P.
* Find starting and ending periods
  IF SY-DATUM+4(2) <> '01'.            " If it's no January

    TEMP = SY-DATUM(4).
    TEMP1 = SY-DATUM+4(2) - '01'.
    CONCATENATE TEMP TEMP1 INTO P2_END.
    TEMP = TEMP - 1.
    TEMP1 = TEMP1 + 1.
    CONCATENATE TEMP TEMP1 INTO P2_START.

    IF P2_START+4(2) <> '01'.
      TEMP1 = TEMP1 - 1.
      CONCATENATE TEMP TEMP1 INTO P1_END.
      TEMP  = TEMP - 1.
      TEMP1 = TEMP1 + 1.
      CONCATENATE TEMP TEMP1 INTO P1_START.
    ELSE.
      TEMP = TEMP - 1.
      TEMP1 = '12'.
      CONCATENATE TEMP TEMP1 INTO P1_END.
      TEMP1 = '01'.
      CONCATENATE TEMP TEMP1 INTO P1_START.
    ENDIF.
  ELSE.                                " If it's January
    TEMP = SY-DATUM+0(4).
    TEMP = TEMP - 1.
    TEMP1 = '12'.
    CONCATENATE TEMP TEMP1 INTO P2_END.
    TEMP1 = '01'.
    CONCATENATE TEMP TEMP1 INTO P2_START.
    TEMP = TEMP - 1.
    TEMP1 = '12'.
    CONCATENATE TEMP TEMP1 INTO P1_END.
    TEMP1 = '01'.
    CONCATENATE TEMP TEMP1 INTO P1_START.

  ENDIF.

ENDFORM.                               " FIND_STARTING_AND_ENDING_P
*&---------------------------------------------------------------------*
*&      Form  GENERATE_SUM_TABLE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GENERATE_SUM_TABLE.
  CLEAR TABIX.
* Table itab
  LOOP AT ITAB.
    MOVE SY-TABIX TO TABIX.
  ENDLOOP.


  IF SY-SUBRC <> 0.
 PERFORM SPECIAL_CASE.   " This is the cas where we don't have movements
    TABIX = 12.
  ENDIF.

  IF TABIX < 12.
    READ TABLE ITAB INDEX TABIX.
    DIF = 12 - TABIX.
    DO DIF TIMES.
      ITAB-CONS = '0'.
      ITAB-LAST = ITAB-FIRST.
      APPEND ITAB.
    ENDDO.
  ENDIF.
  CLEAR TABIX.
* Table itab1
  LOOP AT ITAB1.
    MOVE SY-TABIX TO TABIX.
  ENDLOOP.

  IF SY-SUBRC <> 0.
 PERFORM SPECIAL_CASEII. " This is the cas where we don't have movements
    TABIX = 12.
  ENDIF.


  IF TABIX < 12.
    READ TABLE ITAB1 INDEX TABIX.
    DIF = 12 - TABIX.
    DO DIF TIMES.
      ITAB1-CONS = '0'.
      ITAB1-LAST = ITAB1-FIRST.
      APPEND ITAB1.
    ENDDO.
  ENDIF.

  SORT ITAB BY SPMON.
  LOOP AT ITAB.
    MOVE ITAB-MATNR TO SUM_TAB1-MATNR.
    AT LAST.
      SUM.
      ITAB-LAST = ITAB-LAST / 12.
      MOVE: ITAB-LAST TO SUM_TAB1-LAST,
            ITAB-CONS TO SUM_TAB1-CONS.
      APPEND SUM_TAB1.
    ENDAT.
  ENDLOOP.

  SORT ITAB1 BY SPMON.
  LOOP AT ITAB1.
    MOVE ITAB1-MATNR TO SUM_TAB2-MATNR.
    AT LAST.
      SUM.
      ITAB1-LAST = ITAB1-LAST / 12.
      MOVE: ITAB1-LAST TO SUM_TAB2-LAST,
            ITAB1-CONS TO SUM_TAB2-CONS.

      APPEND SUM_TAB2.
    ENDAT.
  ENDLOOP.











ENDFORM.                               " GENERATE_SUM_TABLE
*&---------------------------------------------------------------------*
*&      Form  SPECIAL_CASE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SPECIAL_CASE.
  DO 12 TIMES.
    ITAB-LAST = ITABLE_S032-MBWBEST.
    ITAB-PERIOD = COUNTER.
    ITAB-FIRST = ITAB-LAST.

    ITAB-CONS = '0'.                   "Consumption

    ITAB-MATNR = ITABLE_S032-MATNR.
    ITAB-MATKL = ITABLE_S032-MATKL.
    ITAB-WERKS = ITABLE_S032-WERKS.    "Plant
    ITAB-LGORT = ITABLE_S032-LGORT.    " Storage Location
    APPEND ITAB.
  ENDDO.

ENDFORM.                               " SPECIAL_CASE
*&---------------------------------------------------------------------*
*&      Form  SPECIAL_CASEII
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SPECIAL_CASEII.
  READ TABLE ITAB INDEX 12.
  DO 12 TIMES.
    ITAB1-LAST = ITAB-FIRST.
    ITAB1-PERIOD = COUNTER.
    ITAB1-FIRST = ITAB1-LAST.

    ITAB1-CONS = '0'.                  "Consumption

    ITAB1-MATNR = ITABLE_S032-MATNR.
    ITAB1-MATKL = ITABLE_S032-MATKL.
    ITAB1-WERKS = ITABLE_S032-WERKS.   "Plant
    ITAB1-LGORT = ITABLE_S032-LGORT.   " Storage Location
    APPEND ITAB1.
  ENDDO.

ENDFORM.                               " SPECIAL_CASEII
