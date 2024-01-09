REPORT ZMMMC011.
*----------------------------------------------------------------------*
* Owner: Centra/Union Gas Ltd. - BIS                                   *
* Author: Mike Moore / Selwyn Rodricks
*         OmniLogic Systems Group                                      *
* Date: November xx, 1996                                              *
*                                                                      *
* Brief Description:                                                   *
* This program checks the material master and prints out error messages*
*----------------------------------------------------------------------*

TABLES: MARA, MARC, MBEW, MARD, KSSK, MAPR, PROP, MAKT.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1  WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: SMATNR FOR MARA-MATNR.
SELECT-OPTIONS: SMTART FOR MARA-MTART.
SELECT-OPTIONS: SMATKL FOR MARA-MATKL.
SELECT-OPTIONS: SWERKS FOR MARC-WERKS.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK ERRCOND WITH FRAME TITLE TEXT-002.
PARAMETERS: C1  AS CHECKBOX,
            C2  AS CHECKBOX,
            C3  AS CHECKBOX,
            C4  AS CHECKBOX,
            C5  AS CHECKBOX,
            C6  AS CHECKBOX,
            C7  AS CHECKBOX,
            C8  AS CHECKBOX,
            C9  AS CHECKBOX,
            C10 AS CHECKBOX,
            C11 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK ERRCOND.


SELECT * FROM MARA WHERE MATNR IN SMATNR AND
                         MTART IN SMTART AND
                         MATKL IN SMATKL AND
                         LVORM <> 'X'
                         ORDER BY MATNR.
  SELECT SINGLE * FROM MAKT WHERE MATNR = MARA-MATNR
                              AND SPRAS = SY-LANGU.

*  No purchasing view
  SELECT * FROM MARC WHERE MATNR EQ MARA-MATNR AND
                           WERKS IN SWERKS AND
                           LVORM <> 'X'.
    IF MARC-EKGRP EQ SPACE.
      IF C1 = 'X'.
        WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35), 47 MARC-WERKS,
                53 'No Purchasing Group assigned'.
      ENDIF.
    ENDIF.

*   Pipe with MRP
    IF MARA-MATKL = '0100' AND MARC-DISMM <> SPACE.
      IF MARA-MTART = 'HAWA' OR MARA-MTART = 'HIBE'.
        IF C6 = 'X'.
          WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35), 47 MARC-WERKS,
                   53 'Pipes with MRP'.
        ENDIF.
      ENDIF.
    ENDIF.

*   Material without MRP
    IF MARA-MATKL NE '0100' AND
                           ( MARC-DISMM = SPACE OR MARC-DISMM = 'ND').
      IF C8 = 'X'.
        WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35), 47 MARC-WERKS,
                 53 'Material without MRP'.
      ENDIF.
    ENDIF.

*   No serial number profile for appliance
    IF MARA-MATKL >= '0300' AND MARA-MATKL <= '0399'
                            AND MARC-SERNP = SPACE.
      IF MARA-MTART = 'HAWA'.
        IF C7 = 'X'.
          WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35), 47 MARC-WERKS,
                   53 'No Serial Number Profile for appliance'.
        ENDIF.
      ENDIF.
    ENDIF.

*   No storage location
    SELECT * FROM MARD WHERE MATNR = MARC-MATNR AND
                             WERKS = MARC-WERKS AND
                             LVORM <> 'X'.
    ENDSELECT.
    IF SY-SUBRC <> 0.
      IF MARA-MTART = 'HAWA' OR MARA-MTART = 'HIBE'.
        IF C5 = 'X'.
          WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35), 47 MARC-WERKS,
                   53 'No Storage Location present'.
        ENDIF.
      ENDIF.
    ENDIF.

*   No accounting view
    SELECT SINGLE * FROM MBEW WHERE MATNR = MARC-MATNR AND
                                    BWKEY = MARC-WERKS AND
                                    BWTAR = SPACE      AND
                                    LVORM <> 'X'.
    IF SY-SUBRC <> 0.
      IF C2 = 'X'.
        WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35), 47 MARC-WERKS,
                 53 'No Accounting View for plant'.
      ENDIF.
    ENDIF.

*   VM MRP without forecast
    IF MARC-DISMM = 'VM'.
      SELECT SINGLE * FROM MAPR WHERE MATNR = MARC-MATNR AND
                                      WERKS = MARC-WERKS.
      IF SY-SUBRC = 0.
        SELECT * FROM PROP WHERE PNUM1 = MAPR-PNUM1
                                 ORDER BY HSNUM DESCENDING.
          IF PROP-PRMOD = SPACE OR PROP-PRMOD = 'N'.
            IF C9 = 'X'.
             WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35), 47 MARC-WERKS,
                      53 'VM MRP without forecast'.
            ENDIF.
          ENDIF.
          EXIT.
        ENDSELECT.
      ENDIF.
    ENDIF.

*   Non VM with forecast
    IF MARC-DISMM <> 'VM'.
      SELECT SINGLE * FROM MAPR WHERE MATNR = MARC-MATNR AND
                                      WERKS = MARC-WERKS.
      IF SY-SUBRC = 0.
        SELECT * FROM PROP WHERE PNUM1 = MAPR-PNUM1
                                 ORDER BY HSNUM DESCENDING.
          IF  ( PROP-PRMOD <> SPACE AND PROP-PRMOD <> 'N' ).
            IF C10 = 'X'.
             WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35), 47 MARC-WERKS,
                      53 'Non VM with forecast'.
            ENDIF.
          ENDIF.
          EXIT.
        ENDSELECT.
      ENDIF.
    ENDIF.

*   Material is blocked
    IF MARC-MMSTA <> SPACE.
      SELECT SINGLE * FROM MAPR WHERE MATNR = MARC-MATNR AND
                                      WERKS = MARC-WERKS.
      IF SY-SUBRC = 0.
        SELECT * FROM PROP WHERE PNUM1 = MAPR-PNUM1
                                 ORDER BY HSNUM DESCENDING.
          IF  ( PROP-PRMOD <> SPACE AND PROP-PRMOD <> 'N' )
                 OR  ( MARC-DISMM <> 'ND' AND MARC-DISMM <> SPACE ).
            IF C11 = 'X'.
             WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35), 47 MARC-WERKS,
                      53 'Non VM with forecast'.
            ENDIF.
          ENDIF.
          EXIT.
        ENDSELECT.
      ENDIF.
    ENDIF.


  ENDSELECT.                           "marc

* No classification view
  SELECT * FROM KSSK WHERE OBJEK = MARA-MATNR AND
                           MAFID = 'O'        AND
                           KLART = '001'.
    IF KSSK-STATU <> 1.
      IF C4 = 'X'.
        WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35),
                 53 'Status of Class is not released'.
      ENDIF.
    ENDIF.
  ENDSELECT.

  IF SY-SUBRC <> 0.
    IF C3 = 'X'.
      WRITE: / MARA-MATNR+12(6), 9 MAKT-MAKTX(35),
               53 'No Classification View'.
    ENDIF.
  ENDIF.

ENDSELECT.                             "mara
