REPORT ZMINR021 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 80
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMINR021
*    Programmer  :  Gus Spartalis/OmniLogic Systems Group
*    Client      :  Centra Union Gas Limited
*    Date        :  December 17 1996
*
*    This ABAP will take a new VENDOR NUMBER, MATERIAL GROUP, and a
*    PURCHASE ORGANIZATION and display an indepth list of the info.
*    records that will be used for data entry.  This ABAP will be used
*    for a one time Data Entry situation.
************************************************************************
TABLES: MARA, MAKT, EINA, EINE, EORD, KONP, A018.

DATA: TEMPSRCE(4)   TYPE C,
      TEMPSEQNO(5),
      COUNTER(1)    TYPE I VALUE 0,
      CHECKPOINT1(1) TYPE C VALUE 'Y',
      TEMPNAME      LIKE THEAD-TDNAME,
      TEMPMEINS     LIKE EINA-MEINS,
      ZTHEAD        LIKE THEAD,
      TEMPTYPE(2).

DATA : BEGIN OF ZTLINE OCCURS 100.
        INCLUDE STRUCTURE TLINE.
DATA : END OF ZTLINE.

DATA   : BEGIN OF TABLE1 OCCURS 100000,
          INFNR     LIKE EINA-INFNR,
          MATNR     LIKE MARA-MATNR,
          MATKL     LIKE MARA-MATKL,
          LIFNR     LIKE EINA-LIFNR,
          MEINS     LIKE EINA-MEINS,
          EKORG     LIKE EORD-EKORG,
          APLFZ     LIKE EINE-APLFZ,
          EKGRP     LIKE EINE-EKGRP,
          NORBM     LIKE EINE-NORBM,
          MWSKZ     LIKE EINE-MWSKZ,
          NETPR     LIKE EINE-NETPR,
          WAERS     LIKE EINE-WAERS,
          PEINH     LIKE EINE-PEINH,
          BPRME     LIKE EINE-BPRME,
          BPUMN     LIKE EINE-BPUMN,
          BPUMZ     LIKE EINE-BPUMZ,
          INCO1     LIKE EINE-INCO1,
          ANGNR     LIKE EINE-ANGNR,
          UNTTO     LIKE EINE-UNTTO,
          UEBTO     LIKE EINE-UEBTO,
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
WRITE: 71 'PAGE:' INTENSIFIED OFF.
WRITE: 76(3) SY-PAGNO COLOR COL_GROUP INTENSIFIED OFF INVERSE ON.
ULINE.
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
               MOVE EINA-INFNR       TO TABLE1-INFNR.
               MOVE EINA-MATNR       TO TABLE1-MATNR.
               MOVE EINA-MEINS       TO TABLE1-MEINS.
               MOVE MARA-MATKL       TO TABLE1-MATKL.
               MOVE EINA-LIFNR       TO TABLE1-LIFNR.
               MOVE EINE-EKORG       TO TABLE1-EKORG.
               MOVE EINE-APLFZ       TO TABLE1-APLFZ.
               MOVE EINE-EKGRP       TO TABLE1-EKGRP.
               MOVE EINE-NORBM       TO TABLE1-NORBM.
               MOVE EINE-MWSKZ       TO TABLE1-MWSKZ.
               MOVE EINE-NETPR       TO TABLE1-NETPR.
               MOVE EINE-WAERS       TO TABLE1-WAERS.
               MOVE EINE-PEINH       TO TABLE1-PEINH.
               MOVE EINE-BPRME       TO TABLE1-BPRME.
               MOVE EINE-BPUMN       TO TABLE1-BPUMN.
               MOVE EINE-BPUMZ       TO TABLE1-BPUMZ.
               MOVE EINE-INCO1       TO TABLE1-INCO1.
               MOVE EINE-ANGNR       TO TABLE1-ANGNR.
               MOVE EINE-UNTTO       TO TABLE1-UNTTO.
               MOVE EINE-UEBTO       TO TABLE1-UEBTO.
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
IF COUNTER  = 2.
   CLEAR COUNTER.
   NEW-PAGE.
ENDIF.
COUNTER = COUNTER + 1.

ON CHANGE OF TABLE1-INFNR.
WRITE: /.
WRITE: /1 TEXT-900, 40 TEXT-900.
ENDON.

    WRITE: /1 TEXT-003, 11 TABLE1-INFNR, 25 TEXT-004, 35 TABLE1-LIFNR.
    WRITE: 43 TEXT-005, 53 TABLE1-MATNR.
    WRITE: /1 TEXT-006, 17 TABLE1-MATKL, 39 TEXT-007, 53 TABLE1-EKORG.
    WRITE: /1 TEXT-901.

    WRITE: /1 TEXT-008, 17 TABLE1-APLFZ, 37 TEXT-012, 54 TABLE1-UNTTO.
    WRITE: /1 TEXT-009, 17 TABLE1-EKGRP, 37 TEXT-013, 54 TABLE1-UEBTO.
    WRITE: /1 TEXT-010, 17 TABLE1-NORBM.
    WRITE: /37 TEXT-011, 54 TABLE1-MWSKZ.
    WRITE: /.
    WRITE: /1 TEXT-014, 15 TABLE1-NETPR, 29 TABLE1-WAERS, 35 TEXT-021.
    WRITE: 38 TABLE1-PEINH, 45 TABLE1-BPRME.

IF TABLE1-BPRME EQ SPACE.
   TEMPMEINS = TABLE1-MEINS.
ELSE.
   TEMPMEINS = TABLE1-BPRME.
ENDIF.

    WRITE: /1 TEXT-015, 20 TABLE1-BPUMN, 27 TABLE1-MEINS.
    WRITE: 40 TABLE1-BPUMZ, 57 TEMPMEINS.
    WRITE: /1 TEXT-016, 20 TABLE1-INCO1.
    WRITE: /1 TEXT-901, 40 TEXT-901.

    WRITE: /1 TEXT-017, 25 TABLE1-ANGNR.
    WRITE: /1  TEXT-901, 40 TEXT-901.

    CONCATENATE TABLE1-INFNR TABLE1-EKORG INTO TEMPNAME.
    CONCATENATE TEMPNAME '0'              INTO TEMPNAME.

          CALL FUNCTION 'READ_TEXT'
                 EXPORTING
*                CLIENT                  = SY-MANDT
                 ID                      = 'BT'
                 LANGUAGE                = SY-LANGU
                 NAME                    = TEMPNAME
                 OBJECT                  = 'EINE'
*                ARCHIVE_HANDLE          = 0
          IMPORTING
                  HEADER                  = ZTHEAD
          TABLES
                  LINES                   = ZTLINE
          EXCEPTIONS
                  ID                      = 1
                  LANGUAGE                = 2
                  NAME                    = 3
                  NOT_FOUND               = 4
                  OBJECT                  = 5
                  REFERENCE_CHECK         = 6
                  WRONG_ACCESS_TO_ARCHIVE = 7
                  OTHERS                  = 8.

         WRITE: /1 TEXT-018.
         IF SY-SUBRC EQ 0.
           LOOP AT ZTLINE.
               WRITE: /1 ZTLINE-TDLINE.
           ENDLOOP.
         ENDIF.

               WRITE: /1  TEXT-901, 40 TEXT-901.

          SELECT * FROM A018 WHERE KSCHL EQ 'ZZZZ'
                               AND LIFNR EQ TABLE1-LIFNR
                               AND MATNR EQ TABLE1-MATNR
                               AND EKORG EQ TABLE1-EKORG
                               AND DATBI => SY-DATUM
                               AND DATAB =< SY-DATUM.
                SELECT * FROM KONP WHERE KNUMH = A018-KNUMH.
                IF CHECKPOINT1 EQ 'Y'.
                    WRITE: /1 TEXT-019, 15 A018-DATAB.
                    WRITE: /1 TEXT-020, 15 A018-DATBI.
                    WRITE: /.
                    MOVE 'N' TO CHECKPOINT1.
                ENDIF.
                    IF KONP-KRECH EQ 'A'.
                        KONP-KBETR = ( KONP-KBETR / 10 ).
                    ENDIF.

                    WRITE: /1  KONP-KSCHL, 15 KONP-KBETR, 32 KONP-KONWA.
                    WRITE:  35 KONP-KPEIN, 40 KONP-KMEIN.
                ENDSELECT.
          ENDSELECT.
CLEAR TABLE1.
MOVE 'Y' TO CHECKPOINT1.
ENDLOOP.
