REPORT ZMMMR040 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65
                MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMR040 - MM: PHYSICAL INVENTORY COUNT DATE REPORT
*    Programmer  :  Ric Aarssen
*    Date        :  March 5, 1998
*
*    This ABAP will retrieve the materials within the last count date
*    range for a specified plant and optional storage locations. It can
*    further be within a specified material groups entered.  It will
*    list the material, description, ABC indicator, last count date and
*    bin location.
*    The report will display the total counts at the end of each period
*    (mth) and go to a new page.  There also is a break at the end of
*    each storage location.
*    Lastly it displays a total count for the report period.
*
*    This ABAP will use an internal table to gather the material
*    records that fall within the selection criteria It will then be
*    sorted and be reported on.
************************************************************************

*****************************  TABLES   ********************************

TABLES: ENT1034,             "an entity view that combines mara & makt
*       mara                 -material master general
*            matnr - material number
*            matkl - material group
*       makt                 -material description
*            maktx - 40 character description
*            spars - language code, U = user signon default
        MARC,                "material master plant
*            werks - plant
*            abcin - abc physical count indicator
        MARD.                "material master storage location
*            lgort - storage location
*            dlinl - last count date
*            lgpbe - bin location

*------ Internal table for items that fall in selected date range ------
*  the first 6 fields are listed in the order required for the sort
*  and summarization

DATA: BEGIN OF TABLE_1 OCCURS 0,
*        matkl            like ent1034-matkl   "Material Group
        WERKS            LIKE MARC-WERKS,     "Plant            (1 sort)
        LGORT            LIKE MARD-LGORT,     "Storage Location (2 sort)
        PERIOD(4)        TYPE N,              "Period (mth)     (3 sort)
        DLINL            LIKE MARD-DLINL,     "Last Count Date  (4 sort)
        ABCIN            LIKE MARC-ABCIN,     "Cycle Count ABC indicator
        MATNR            LIKE ENT1034-MATNR,  "Material Number  (6 sort)
        MAKTX            LIKE ENT1034-MAKTX,  "Description
        LGPBE            LIKE MARD-LGPBE,     "Bin Location
        PLACE            TYPE I VALUE 1,      "placement for summarizing
      END OF TABLE_1.

*------ Internal table for items not counted ---------------------------
*  the first 4 fields are listed in the order required for the sort
*  and summarization

DATA: BEGIN OF TABLE_2 OCCURS 0,
*        matkl            like ent1034-matkl   "Material Group
        WERKS            LIKE MARC-WERKS,     "Plant            (1 sort)
        LGORT            LIKE MARD-LGORT,     "Storage Location (2 sort)
        ABCIN            LIKE MARC-ABCIN,     "Cycle Count ABC indicator
        MATNR            LIKE ENT1034-MATNR,  "Material Number  (4 sort)
        PERIOD(4)        TYPE N,              "Period (mth)
        DLINL            LIKE MARD-DLINL,     "Last Count Date
        MAKTX            LIKE ENT1034-MAKTX,  "Description
        LGPBE            LIKE MARD-LGPBE,     "Bin Location
        PLACE            TYPE I VALUE 1,      "placement for summarizing
      END OF TABLE_2.

**************************  DATA ELEMENTS  *****************************

*---------------------- Work Area --------------------------------------
DATA: W_WERKS         LIKE MARC-WERKS,          "Plant (print header)
      W_LGORT         LIKE MARD-LGORT,          "Storage Location (ph)
      W_HDG(35)       TYPE C.                   "Second line heading

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_WERKS LIKE MARC-WERKS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: S_LGORT FOR MARD-LGORT.
  SELECT-OPTIONS: S_MATKL FOR ENT1034-MATKL.
  SELECT-OPTIONS: S_ABCIN FOR MARC-ABCIN.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-015.
  SELECT-OPTIONS: S_DLINL FOR MARD-DLINL.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-014.
    PARAMETERS: P_NOCNT AS CHECKBOX.
    SELECTION-SCREEN COMMENT 36(41) TEXT-003.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-901.

***************************  MAIN ROUTINE  *****************************
TOP-OF-PAGE.
*------------------------ print report header --------------------------
FORMAT COLOR 4.
WRITE: /1 TEXT-RPT, SY-REPID COLOR 7 INTENSIFIED ON,
       45 TEXT-HDG,                                            "Title
      105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT,
         SY-MANDT UNDER SY-REPID INTENSIFIED ON,
         W_HDG    UNDER TEXT-HDG,
         TEXT-PGE UNDER TEXT-DTE,
         SY-PAGNO UNDER SY-DATUM INTENSIFIED ON.
ULINE.
SKIP.
WRITE: /1 TEXT-009, 9 W_WERKS, 14 TEXT-010, 32 W_LGORT.
SKIP.
FORMAT COLOR 1.
WRITE: /1 TEXT-004, 14 TEXT-005, 56 TEXT-006, 61 TEXT-007, 79 TEXT-008.
ULINE.
WRITE: /.
FORMAT RESET.

START-OF-SELECTION.
*--------------  check date selection made properly  -------------------
IF P_NOCNT <> 'X'.
   IF S_DLINL-LOW = 0.
      MESSAGE E100 WITH 'Count date range not present -'
                        'Press Enter  &  try again'.
   ENDIF.
ENDIF.

*--------------------  load internal tables  ---------------------------
CLEAR: TABLE_1, TABLE_2.
*--- read material master plant view for abc indicator -----------------
SELECT * FROM MARC
    WHERE WERKS = P_WERKS
    AND   ABCIN IN S_ABCIN.

    IF SY-SUBRC = 0.
*--- read material master for a material group and description ---------
       SELECT SINGLE * FROM ENT1034
           WHERE MATNR = MARC-MATNR
           AND   MATKL IN S_MATKL
           AND   SPRAS = SY-LANGU.

           IF SY-SUBRC = 0.
*--- read material master storage location view for date and bin -------
              SELECT * FROM MARD
                  WHERE MATNR = MARC-MATNR
                  AND   WERKS = MARC-WERKS
                  AND   LGORT IN S_LGORT
                  AND ( DLINL IN S_DLINL
                  OR    DLINL = 0 ).

              IF SY-SUBRC = 0.
                 IF MARD-DLINL > 0 AND S_DLINL-LOW <> SPACE.
                    PERFORM COUNTED.
                 ELSE.
                    PERFORM NOT-COUNTED.
                 ENDIF.
              ENDIF.
              ENDSELECT.
           ENDIF.
    ENDIF.
ENDSELECT.

*-----------------  create report of counted items  --------------------
IF P_NOCNT <> 'X'.
   PERFORM COUNTED-REPORT.
ENDIF.

*--------------  create report of non-counted items --------------------
PERFORM NOT-COUNTED-REPORT.

END-OF-SELECTION.

**************************   SUB-ROUTINES   ****************************

*---  counted sub-routine  ---------------------------------------------
* routine used to add records selected to table_1 for later reporting
* materials with a last count date
*-----------------------------------------------------------------------
FORM COUNTED.
    MOVE MARC-WERKS        TO TABLE_1-WERKS.
    MOVE MARD-LGORT        TO TABLE_1-LGORT.
    MOVE MARD-MATNR        TO TABLE_1-MATNR.
    MOVE ENT1034-MAKTX     TO TABLE_1-MAKTX.
    MOVE MARC-ABCIN        TO TABLE_1-ABCIN.
    MOVE MARD-DLINL        TO TABLE_1-DLINL.
    MOVE MARD-LGPBE        TO TABLE_1-LGPBE.
    MOVE MARD-DLINL+2(4)   TO TABLE_1-PERIOD.
    MOVE 1                 TO TABLE_1-PLACE.
    APPEND TABLE_1.
ENDFORM.

*---  not-counted sub-routine  -----------------------------------------
* routine used to add records selected to table_1 for later reporting
* materials with a last count date
*-----------------------------------------------------------------------
FORM NOT-COUNTED.
    MOVE MARC-WERKS        TO TABLE_2-WERKS.
    MOVE MARD-LGORT        TO TABLE_2-LGORT.
    MOVE MARD-MATNR        TO TABLE_2-MATNR.
    MOVE ENT1034-MAKTX     TO TABLE_2-MAKTX.
    MOVE MARC-ABCIN        TO TABLE_2-ABCIN.
    MOVE MARD-DLINL        TO TABLE_2-DLINL.
    MOVE MARD-LGPBE        TO TABLE_2-LGPBE.
    MOVE MARD-DLINL+2(4)   TO TABLE_2-PERIOD.
    MOVE 1                 TO TABLE_2-PLACE.
    APPEND TABLE_2.
ENDFORM.

*---  counted report sub-routine      ----------------------------------
* routine used to report materials counted within the physical inventory
* last count date range selected
*-----------------------------------------------------------------------
FORM COUNTED-REPORT.
    MOVE TEXT-HG2 TO W_HDG.
*- sort table by plant, storage, period, last cnt date, abc, material --
    SORT TABLE_1 ASCENDING BY WERKS LGORT PERIOD DLINL ABCIN MATNR.
    LOOP AT TABLE_1.
      AT NEW LGORT.
         MOVE TABLE_1-WERKS TO W_WERKS.
         MOVE TABLE_1-LGORT TO W_LGORT.
         NEW-PAGE.
      ENDAT.

      AT NEW PERIOD.
         NEW-PAGE.
      ENDAT.

*-------------------- write report from table --------------------------
      WRITE: / TABLE_1-MATNR     UNDER TEXT-004,
               TABLE_1-MAKTX     UNDER TEXT-005,
               TABLE_1-ABCIN     UNDER TEXT-006,
               TABLE_1-DLINL     UNDER TEXT-007,
               TABLE_1-LGPBE     UNDER TEXT-008.

*-------------------  write total counted for period  ------------------
      AT END OF PERIOD.
         SUM.
         SKIP.
         WRITE: /1 TEXT-011, 22 TABLE_1-PERIOD, 27 TABLE_1-PLACE.
      ENDAT.

*--------------  write total counted for storage location  -------------
      AT END OF LGORT.
         SKIP.
         SUM.
         WRITE: /1 TEXT-012, 27 TABLE_1-PLACE.
      ENDAT.

    ENDLOOP.
ENDFORM.

*---  not-counted report sub-routine  ----------------------------------
* routine used to report non-counted materials with a physical inventory
* last count date of zero
*-----------------------------------------------------------------------
FORM NOT-COUNTED-REPORT.
    MOVE TEXT-HG3 TO W_HDG.
*----  sort table by plant, storage loc, ABC ind, last count date  -----
    SORT TABLE_2 ASCENDING BY WERKS LGORT ABCIN MATNR.
    LOOP AT TABLE_2.
      AT NEW LGORT.
         MOVE TABLE_2-WERKS TO W_WERKS.
         MOVE TABLE_2-LGORT TO W_LGORT.
         NEW-PAGE.
      ENDAT.

*-------------------- write report from table --------------------------
      WRITE: / TABLE_2-MATNR     UNDER TEXT-004,
               TABLE_2-MAKTX     UNDER TEXT-005,
               TABLE_2-ABCIN     UNDER TEXT-006,
               TABLE_2-DLINL     UNDER TEXT-007,
               TABLE_2-LGPBE     UNDER TEXT-008.

*----------  write total non-counted for storage location  -------------
      AT END OF LGORT.
         SUM.
         SKIP.
         WRITE: /1 TEXT-013, 27 TABLE_2-PLACE.
      ENDAT.

    ENDLOOP.
ENDFORM.
***************************  END OF PROGRAM  ***************************
