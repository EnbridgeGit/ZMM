REPORT ZMINR024 NO STANDARD PAGE HEADING LINE-SIZE 100 LINE-COUNT 65.

******************************************************************
*       Owner: Centra/Union                                      *
*  Programmer: Marv Radsma                                       *
*        Date: Dec 11, 1996                                      *
*  Request ID: DRMM????                                          *
*                                                                *
* This program produces a report listing the value of inventory. *
* The value will be of the material groups reported by the plant *
* and the storage location.  Parameters may be supplied to limit *
* the size of the report.                                        *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YY/MM/DD - USERID - MOD# - DESCRIPTION                         *
* -------------------------------------------------------------- *
* 98/06/05 - md7140 - #529 - Deleted materials s/b part of report*
*                            Add report, client  (DRMM0250)      *
*          -        -      -                                     *
******************************************************************

TABLES: MARA,      "Material master, general data
        MARC,      "Material master, plant code
        MARD,      "Material master, storage location/batch segment
        MBEW,      "Material valuation
        T001W,     "Plant Code Lookup
        T001L.     "Storage Location Lookup

DATA:   QTYONHAND  TYPE I,                        "Quantity on Hand
        BEGIN OF MAT_TAB     OCCURS 5000,         "Required data
          MATKL    LIKE MARA-MATKL,               "Material Group
          WERKS    LIKE MARD-WERKS,               "Plant
          LGORT    LIKE MARD-LGORT,               "Storage Location
          VALUE    LIKE MBEW-SALKV,               "Moving Average Value
        END OF MAT_TAB.

* This first block produces a border like frame around the following: *

SELECTION-SCREEN BEGIN OF BLOCK
    INTRO WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 26(33) TEXT-001 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN ULINE  26(33).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* The following second block produces a frame within the first block. *

SELECTION-SCREEN BEGIN OF BLOCK GROUP
                 WITH FRAME.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS:
  S_GROUP     FOR MARA-MATKL                  "Material Group
              OBLIGATORY MODIF ID ABC.

SELECTION-SCREEN SKIP.

* This statement ends the second block. *

SELECTION-SCREEN END OF BLOCK GROUP.

SELECTION-SCREEN SKIP.

* The following third block produces a frame within the first block. *

SELECTION-SCREEN BEGIN OF BLOCK PLANT
                 WITH FRAME.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS:
  S_PLANT     FOR MARC-WERKS                  "Plant Code
              OBLIGATORY MODIF ID ABC.

SELECTION-SCREEN SKIP.

* This statement ends the third block. *

SELECTION-SCREEN END OF BLOCK PLANT.

SELECTION-SCREEN SKIP.

* The following fourth block produces a frame within the first block. *

SELECTION-SCREEN BEGIN OF BLOCK STORE
                 WITH FRAME.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS:
  S_STORE     FOR  MARD-LGORT                 "Storage Location
              OBLIGATORY MODIF ID ABC.

SELECTION-SCREEN SKIP.

* This statement ends the fourth block. *

SELECTION-SCREEN END OF BLOCK STORE.

SELECTION-SCREEN SKIP.

* This statement ends the first block. *

SELECTION-SCREEN END OF BLOCK INTRO.

*********************BEGINNING OF MAIN PROGRAM *************************

TOP-OF-PAGE.

  write: /1 text-rpt, sy-repid, 28 text-ttl,                     "Title
         74 text-dte, sy-datum, text-amp, sy-uzeit.
  write: / text-clt under text-rpt, sy-mandt, sy-sysid,
           text-pge under text-dte, sy-pagno.
  WRITE: /.
  ULINE.
  WRITE: /.

************************************************************************

START-OF-SELECTION.

  SELECT * FROM MARA
  WHERE  MATKL IN S_GROUP.

    SELECT * FROM MARD
    WHERE  MATNR = MARA-MATNR
*   AND    LVORM NE 'X'
    AND    WERKS IN S_PLANT
    AND    LGORT IN S_STORE.

      SELECT SINGLE * FROM MBEW
      WHERE  MATNR = MARA-MATNR
      AND    BWKEY = MARD-WERKS
      AND    BWTAR = SPACE.

      COMPUTE QTYONHAND = ( MARD-INSME + MARD-LABST
             + MARD-UMLME + MARD-SPEME + MARD-EINME ).
      IF QTYONHAND NE 0.
        MOVE MARA-MATKL TO MAT_TAB-MATKL.
        MOVE MARD-WERKS TO MAT_TAB-WERKS.
        MOVE MARD-LGORT TO MAT_TAB-LGORT.
        COMPUTE MAT_TAB-VALUE = MBEW-VERPR * QTYONHAND.
        APPEND MAT_TAB.
      ENDIF.

      CLEAR  MAT_TAB.

    ENDSELECT.
  ENDSELECT.

* Sort the table just extracted. *
  SORT MAT_TAB BY MATKL WERKS LGORT.

* Process the table, looping thru and outputing the detail liness. *
  LOOP AT MAT_TAB.
    AT NEW MATKL.
      NEW-PAGE.
      WRITE: /.
      WRITE: /1  TEXT-002 COLOR COL_POSITIVE INVERSE ON.
      WRITE:  17 MAT_TAB-MATKL.
      WRITE: /.
      ULINE.
    ENDAT.
    AT NEW WERKS.
      SELECT SINGLE * FROM T001W
      WHERE WERKS = MAT_TAB-WERKS.
      WRITE: /.
      WRITE: /6  MAT_TAB-WERKS.
      WRITE:  11 ' - '.
      WRITE:  15 T001W-NAME1.
      WRITE: /.
    ENDAT.
    AT END OF LGORT.
      SUM.
      SELECT SINGLE * FROM T001L
      WHERE WERKS = MAT_TAB-WERKS
      AND   LGORT = MAT_TAB-LGORT.
      WRITE: /10 MAT_TAB-LGORT.
      WRITE:  16 ' - '.
      WRITE:  20 T001L-LGOBE.
      WRITE:  60 MAT_TAB-VALUE COLOR COL_TOTAL INVERSE ON.
    ENDAT.
    AT END OF WERKS.
      SUM.
      WRITE: /.
      WRITE: /25 TEXT-003.
      WRITE:  42 MAT_TAB-WERKS.
      WRITE:  60 MAT_TAB-VALUE COLOR COL_NEGATIVE INVERSE ON.
      WRITE: /.
      ULINE.
    ENDAT.
    AT END OF MATKL.
      SUM.
      WRITE: /.
      WRITE: /25 TEXT-004.
      WRITE:  51 MAT_TAB-MATKL.
      WRITE:  60 MAT_TAB-VALUE COLOR COL_KEY INVERSE ON.
      WRITE: /.
      ULINE.
      ULINE.
    ENDAT.
    AT LAST.
      SUM.
      WRITE: /.
      WRITE: /25 TEXT-005.
      WRITE:  60 MAT_TAB-VALUE COLOR COL_KEY INTENSIFIED ON.
      WRITE: /.
      ULINE.
      WRITE: /25 'END of REPORT'.
      ULINE.
    ENDAT.
  ENDLOOP.

END-OF-SELECTION.

**************************** THE END ***********************************
