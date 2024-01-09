REPORT ZMINR020 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 80
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMINR020
*    Programmer  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  December 16 1996
*
*    This ABAP will take a new VENDOR NUMBER, MATERIAL GROUP, and a
*    PURCHASE ORGANIZATION and get all the material numbers. The output
*    list will be displayed in the following order.  Material Number,
*    Material Group, Vendor Number, Purchase Organization.
************************************************************************
TABLES: MARA, MAKT, EINA, EINE, EORD.

DATA   : BEGIN OF TABLE1 OCCURS 100000,
          MATNR     LIKE MARA-MATNR,
          MATKL     LIKE EINA-MATKL,
          LIFNR     LIKE EINA-LIFNR,
          EKORG     LIKE EORD-EKORG,
        END OF TABLE1.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECT-OPTIONS:
       S_LIFNR          FOR EINA-LIFNR,
       S_MATKL          FOR EINA-MATKL,
       S_EKORG          FOR EORD-EKORG.
SELECTION-SCREEN END OF BLOCK BOX1.

INCLUDE <ICON>.
************************************************************************
TOP-OF-PAGE.
WRITE: / ICON_DATE AS ICON.
WRITE: SY-DATUM COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: 40 TEXT-002 COLOR COL_HEADING.
WRITE: 71 SY-REPID COLOR COL_NEGATIVE.

WRITE: / ICON_TIME AS ICON.
WRITE: SY-UZEIT COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE: 39(15).
WRITE: 71 'PAGE:' INTENSIFIED OFF.
WRITE: 76(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
WRITE: /.
FORMAT COLOR COL_NORMAL.
ULINE.

WRITE: /1 TEXT-011, 20 TEXT-011, 40 TEXT-006, 50 TEXT-014.
WRITE: /1 TEXT-005, 20 TEXT-013, 40 TEXT-005, 50 TEXT-015.
ULINE.
WRITE: /.
**************************** MAIN  PROGRAM *****************************
START-OF-SELECTION.
SELECT * FROM EINA WHERE LIFNR IN S_LIFNR.

      SELECT SINGLE * FROM MARA WHERE MATNR = EINA-MATNR
                                  AND LVORM NE 'X'
                                  AND MATKL IN S_MATKL.
      IF SY-SUBRC EQ 0.
          SELECT SINGLE * FROM EINE WHERE INFNR = EINA-INFNR
                                      AND EKORG IN S_EKORG.
          IF SY-SUBRC EQ 0.
               MOVE EINA-MATNR       TO TABLE1-MATNR.
               MOVE MARA-MATKL       TO TABLE1-MATKL.
               MOVE EINA-LIFNR       TO TABLE1-LIFNR.
               MOVE EINE-EKORG       TO TABLE1-EKORG.
               APPEND TABLE1.
               CLEAR  TABLE1.
           ENDIF.
       ENDIF.
ENDSELECT.

SORT TABLE1 BY MATNR ASCENDING
               MATKL ASCENDING
               LIFNR ASCENDING
               EKORG ASCENDING.

LOOP AT TABLE1.
ON CHANGE OF TABLE1-MATNR.
    WRITE: /.
ENDON.

    WRITE: /1  TABLE1-MATNR, 20 TABLE1-MATKL.
    WRITE:  40 TABLE1-LIFNR, 50 TABLE1-EKORG.
ENDLOOP.
