REPORT ZDRMM132 LINE-Size 132.

******************************************************************
*       Owner: Centra/Union                                      *
*  Programmer: Marv Radsma                                       *
*        Date: Dec 11, 1996                                      *
*  Request ID: DRMM0132                                          *
*                                                                *
* This program produces a report listing hazardous materials.    *
* The output will depend on the supplied parameters ie. only     *
* the description is provided when no additional selection data  *
* is provided.                                                   *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YY/MM/DD - USERID - MOD# - DESCRIPTION                         *
* -------------------------------------------------------------- *
*          -        -      -                                     *
*          -        -      -                                     *
******************************************************************

TABLES: MARA,      "Material master, general data
        MARD,      "Material master, storage location/batch segment
        MAKT.      "Material descriptions

DATA:   LN_CNTR    TYPE I,                        "Line Counter
        BEGIN OF MAT_TAB     OCCURS 5000,         "Required data
          BEGIN OF AKEY,
            WERKS  LIKE MARD-WERKS,               "Plant
            LGORT  LIKE MARD-LGORT,               "Storage Location
          END OF AKEY,
          MATNR    LIKE MARA-MATNR,               "material Number
          MAKTX    LIKE MAKT-MAKTX,               "Material Description
          INSME    LIKE MARD-INSME,               "Quantity on Hand
          LGPBE    LIKE MARD-LGPBE,               "Bin Location
        END OF MAT_TAB.

* This first block produces a border like frame around the following: *

SELECTION-SCREEN BEGIN OF BLOCK
    INTRO WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 26(20) TEXT-001 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN ULINE  26(20).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* The following second block produces a frame within the first block. *

SELECTION-SCREEN BEGIN OF BLOCK PLANT
                 WITH FRAME TITLE TEXT-004.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 17(13) TEXT-002.
PARAMETERS:
  P_PLANT1    LIKE MARD-WERKS                 "From Plant
              MODIF ID ABC.
SELECTION-SCREEN: COMMENT 42(13) TEXT-003.
PARAMETERS:
  P_PLANT2    LIKE MARD-WERKS                 "To Plant
              MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* This statement ends the second block. *

SELECTION-SCREEN END OF BLOCK PLANT.

SELECTION-SCREEN SKIP.

* The following third block produces a frame within the first block. *

SELECTION-SCREEN BEGIN OF BLOCK STORAGE
                 WITH FRAME TITLE TEXT-005.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 17(13) TEXT-002.
PARAMETERS:
  P_STOR1     LIKE MARD-LGORT                 "From Storage Location
              MODIF ID ABC.
SELECTION-SCREEN: COMMENT 42(13) TEXT-003.
PARAMETERS:
  P_STOR2     LIKE MARD-LGORT                 "To Storage Location
              MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* This statement ends the third block. *

SELECTION-SCREEN END OF BLOCK STORAGE.

SELECTION-SCREEN SKIP.

* This statement ends the first block. *

SELECTION-SCREEN END OF BLOCK INTRO.

* The following will highlight the screens output for certain texts *

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.

*********************BEGINNING OF MAIN PROGRAM *************************

START-OF-SELECTION.

* Initialize the second input field if not entered *

  IF P_PLANT2 EQ SPACE.
    MOVE P_PLANT1 TO P_PLANT2.
  ENDIF.

  IF P_STOR2 EQ SPACE.
    MOVE P_STOR1 TO P_STOR2.
  ENDIF.

  SELECT * FROM MARA
  WHERE  STOFF = 'HAZARD'.
    MOVE MARA-MATNR TO MAT_TAB-MATNR.

    SELECT * FROM MAKT
    WHERE  MATNR = MARA-MATNR
    AND    SPRAS = 'E'.
      MOVE MAKT-MAKTX TO MAT_TAB-MAKTX.
    ENDSELECT.

    IF  P_PLANT1 NE SPACE
    AND P_STOR1  NE SPACE.
      SELECT * FROM MARD
      WHERE  MATNR = MARA-MATNR
      AND    WERKS BETWEEN P_PLANT1 AND P_PLANT2
      AND    LGORT BETWEEN P_STOR1  AND P_STOR2.
        MOVE MARA-MATNR TO MAT_TAB-MATNR.
        MOVE MAKT-MAKTX TO MAT_TAB-MAKTX.
        MOVE MARD-WERKS TO MAT_TAB-AKEY-WERKS.
        MOVE MARD-LGORT TO MAT_TAB-AKEY-LGORT.
        MOVE MARD-INSME TO MAT_TAB-INSME.
        ADD  MARD-LABST TO MAT_TAB-INSME.
        MOVE MARD-LGPBE TO MAT_TAB-LGPBE.
        APPEND MAT_TAB.
        CLEAR  MAT_TAB.
      ENDSELECT.
    ELSE.
      IF  P_PLANT1 NE SPACE.
        SELECT * FROM MARD
        WHERE  MATNR = MARA-MATNR
        AND    WERKS BETWEEN P_PLANT1 AND P_PLANT2.
          MOVE MARA-MATNR TO MAT_TAB-MATNR.
          MOVE MAKT-MAKTX TO MAT_TAB-MAKTX.
          MOVE MARD-WERKS TO MAT_TAB-AKEY-WERKS.
          MOVE MARD-LGORT TO MAT_TAB-AKEY-LGORT.
          MOVE MARD-INSME TO MAT_TAB-INSME.
          ADD  MARD-LABST TO MAT_TAB-INSME.
          MOVE MARD-LGPBE TO MAT_TAB-LGPBE.
          APPEND MAT_TAB.
          CLEAR  MAT_TAB.
        ENDSELECT.
      ELSE.
        IF  P_STOR1 NE SPACE.
          SELECT * FROM MARD
          WHERE  MATNR = MARA-MATNR
          AND    LGORT BETWEEN P_STOR1 AND P_STOR2.
            MOVE MARA-MATNR TO MAT_TAB-MATNR.
            MOVE MAKT-MAKTX TO MAT_TAB-MAKTX.
            MOVE MARD-WERKS TO MAT_TAB-AKEY-WERKS.
            MOVE MARD-LGORT TO MAT_TAB-AKEY-LGORT.
            MOVE MARD-INSME TO MAT_TAB-INSME.
            ADD  MARD-LABST TO MAT_TAB-INSME.
            MOVE MARD-LGPBE TO MAT_TAB-LGPBE.
            APPEND MAT_TAB.
            CLEAR  MAT_TAB.
          ENDSELECT.
        ELSE.
          APPEND MAT_TAB.
          CLEAR  MAT_TAB.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDSELECT.

* Output the report Headings. *
  PERFORM WRT_HDG.

* Sort the table just extracted by material number. *
  SORT MAT_TAB BY AKEY MATNR.

* Process the table, looping thru and outputing the detail liness. *
  LOOP AT MAT_TAB.
    IF P_PLANT1 NE SPACE
    OR P_STOR1  NE SPACE.
      AT NEW AKEY.
        PERFORM WRT_SUB.
      ENDAT.
    ENDIF.
    PERFORM WRT_DET.
  ENDLOOP.

END-OF-SELECTION.

*************************BEGINNING OF FORMS*****************************

* This routine writes the specifics of the specified G/L Account(s). *

FORM WRT_DET.

  IF LN_CNTR >= 59.                     "Line counter for page break
    PERFORM WRT_HDG.
  ENDIF.

  IF P_PLANT1 NE SPACE
  OR P_STOR1  NE SPACE.
    WRITE: /10 MAT_TAB-MATNR, 30 MAT_TAB-MAKTX,
            72 MAT_TAB-INSME, 99 MAT_TAB-LGPBE.
  ELSE.
    WRITE: /10 MAT_TAB-MATNR, 30 MAT_TAB-MAKTX.
  ENDIF.

  ADD 1 TO LN_CNTR.

ENDFORM.

* This routine writes the report headings. *

FORM WRT_HDG.

  NEW-PAGE WITH-TITLE.
  MOVE '0' TO LN_CNTR.
  FORMAT INTENSIFIED ON.
  WRITE: /1  TEXT-006, 7 SYST-DATUM,  40 TEXT-001.
  SKIP.
  IF P_PLANT1 NE SPACE
  OR P_STOR1  NE SPACE.
    WRITE: /10 TEXT-007, 77 TEXT-010, 99 TEXT-012.
    WRITE: /10 TEXT-008, 30 TEXT-009, 77 TEXT-011, 99 TEXT-013.
  ELSE.
    WRITE: /10 TEXT-007.
    WRITE: /10 TEXT-008, 30 TEXT-009.
  ENDIF.
  ULINE: /.
  MOVE '11' TO LN_CNTR.
  FORMAT INTENSIFIED OFF.

ENDFORM.

* This routine writes the report Sub-headings. *

FORM WRT_SUB.

  SKIP.
  FORMAT INTENSIFIED ON.
  WRITE: /1  TEXT-004, 18 MAT_TAB-AKEY-WERKS.
  WRITE: /1  TEXT-005, 18 MAT_TAB-AKEY-LGORT.
  SKIP.
  ADD 4 TO LN_CNTR.
  FORMAT INTENSIFIED OFF.

ENDFORM.
