REPORT ZMINR016 LINE-SIZE 132 LINE-COUNT 65 NO STANDARD PAGE HEADING.
************************************************************************
*
*   PROGRAM:      ZMINR016
*   REQUEST ID:   DRMM0110a
*   PROGRAMMER:   Dorothy Bialkowska
*   CLIENT:       Centra - Union
*   DATE:         3 December,  1996.
*
*   The purpose of this report is to produce a list of materials with
*   missing serial number (all listed movement types) and/or contract
*   number (for selectes movement types only) for goods issues for
*   selected plant and storage locations.
*
*   NOTE:
*        Following movement types: 935/936 and 941/942 have been
*        excluded from list.
************************************************************************

TABLES: MARA, MAKT, MARC, mard, mkpf, MSEG, LFA1, T001L, T001W.

DATA:   NUMBER LIKE MSEG-ZEILE.

DATA:   BEGIN OF worktab OCCURS 10000,
              matnr      LIKE MSEG-MATNR,
              sernr      LIKE EQUI-SERNR,
              bwart      LIKE MSEG-BWART,
              shkzg      LIKE MSEG-SHKZG,
              lgort      LIKE MSEG-LGORT,
              i_stnme    LIKE T001L-LGOBE,
              mblnr      LIKE MSEG-MBLNR,
              bldat      LIKE MKPF-BLDAT,
              umwrk      LIKE MSEG-UMWRK,
              umlgo      LIKE MSEG-UMLGO,
              r_stnme    LIKE T001L-LGOBE,
              lifnr      LIKE MSEG-LIFNR,
              wempf      LIKE MSEG-WEMPF.
DATA:   END OF worktab.

DATA:   BEGIN OF MISTAB OCCURS 10000.
        INCLUDE STRUCTURE WORKTAB.
DATA:   END OF MISTAB.

DATA:   BEGIN OF ZTab OCCURS 100.
        INCLUDE STRUCTURE EQUI.
DATA:   END OF ZTab.

DATA:   Counter          TYPE I,
        Rem              TYPE I,
        STRING           LIKE MSEG-BWART.
************************************************************************
*   Beginning of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK Box WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN BEGIN OF BLOCK Box1 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (29) TEXT-001.
PARAMETERS P_CCode   LIKE MSEG-BUKRS DEFAULT 'UGL'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) TEXT-002.
PARAMETERS P_date1 LIKE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN COMMENT 40(5) TEXT-003.
PARAMETERS P_date2 LIKE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK Box1.

SELECTION-SCREEN BEGIN OF BLOCK Box2 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (29) TEXT-004.
PARAMETERS: P_Plant   LIKE T001W-WERKS OBLIGATORY.
SELECTION-SCREEN END OF LINE.
PARAMETERS: S_Store LIKE MSEG-LGORT OBLIGATORY.
SELECT-OPTIONS S_MatNr FOR MARA-MATNR NO INTERVALS.
SELECTION-SCREEN END OF BLOCK Box2.
SELECTION-SCREEN END OF BLOCK Box.

*   End of selection screen.
************************************************************************

INITIALIZATION.
  P_date1+6(2) = '01'.
  P_date1+4(2) = '01'.
  P_date1(4) = P_date2(4) - 1.

************************************************************************

TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: / TEXT-020, SY-DATUM, 118 TEXT-018, SY-PAGNO.
  WRITE: / SY-UZEIT UNDER SY-DATUM, 118 SY-REPID.
  WRITE: /55 TEXT-019.
  ULINE.
  FORMAT INTENSIFIED ON.

  PERFORM GetPlantName.
  PERFORM PrintPlantInfo.
  CLEAR T001L-LGOBE.
  SELECT * FROM T001L
     WHERE WERKS = P_Plant
     AND LGORT = S_Store.
  ENDSELECT.
  WRITE: /32 TEXT-024, 40 S_Store, T001L-LGOBE.
  SKIP 1.
  PERFORM PrintHeadings.

*   Information to be printed at the top of new page.
************************************************************************

START-OF-SELECTION.
  SELECT * FROM MARA
     WHERE MATNR IN S_MATNR
     AND MATKL = '0300'
     AND MTART = 'HAWA'
     AND LVORM NE 'X'.
    SELECT * FROM MARC
      WHERE MATNR = MARA-MATNR
      AND WERKS = P_PLANT
      AND SERNP  = 'MMSL'
      AND LVORM NE 'X'.
      SELECT * FROM MARD
        WHERE MATNR = MARA-MATNR
        AND WERKS = MARC-WERKS
        AND LGORT = S_STORE
        AND LVORM NE 'X'.
        SELECT * FROM MSEG
           WHERE MATNR = MARA-MATNR
           AND ( MJAHR = P_DATE1(4) OR MJAHR = P_DATE2(4) )
                                       " refer to NOTE
           AND ( BWART = '101' OR BWART = '102' OR BWART = '122' OR
                 BWART = '201' OR BWART = '202' OR BWART = '221' OR
                 BWART = '222' OR BWART = '261' OR BWART = '262' OR
                 BWART = '291' OR BWART = '292' OR BWART = '561' OR
                 BWART = '562' OR BWART = '301' OR BWART = '302' OR
                 BWART = '303' OR BWART = '304' OR BWART = '305' OR
                 BWART = '306' OR BWART = '311' OR BWART = '312' )
            AND WERKS = MARD-WERKS
            AND LGORT = MARD-LGORT
            AND BUKRS = P_CCODE.

          SELECT SINGLE * FROM MKPF
             WHERE MBLNR = MSEG-MBLNR
             AND MJAHR = MSEG-MJAHR.
          CHECK MKPF-BLDAT BETWEEN P_DATE1 AND P_DATE2.
************************************************************************
          REFRESH ZTAB.
          CLEAR ZTAB.
          CALL FUNCTION 'Z_MM_GET_SERIAL_NUMBER'
               EXPORTING
                    P_MBLNR = MSEG-MBLNR
                    P_MJAHR = MSEG-MJAHR
                    P_ZEILE = MSEG-ZEILE
               TABLES
                    ZEQUI   = ZTAB
               EXCEPTIONS
                    OTHERS  = 1.

          DESCRIBE TABLE ZTAB LINES COUNTER.
          IF COUNTER > 0.
            LOOP AT ZTAB.
              PERFORM SERNRFOUND.
            ENDLOOP.

          ELSE.
            REFRESH ZTAB.
            CLEAR: ZTAB, COUNTER, NUMBER.
            NUMBER = MSEG-ZEILE.
            NUMBER = NUMBER + 1.
            CALL FUNCTION 'Z_MM_GET_SERIAL_NUMBER'
                 EXPORTING
                      P_MBLNR = MSEG-MBLNR
                      P_MJAHR = MSEG-MJAHR
                      P_ZEILE = NUMBER
                 TABLES
                      ZEQUI   = ZTAB
                 EXCEPTIONS
                      OTHERS  = 1.

            DESCRIBE TABLE ZTAB LINES COUNTER.
            IF COUNTER > 0.
              LOOP AT ZTAB.
                PERFORM SERNRFOUND.
              ENDLOOP.
            ELSE.
              PERFORM SERNRNOTFOUND.
            ENDIF.
          ENDIF.
        ENDSELECT.
      ENDSELECT.
    ENDSELECT.
  ENDSELECT.
*   End of FROM STORAGE LOCATION.
************************************************************************

  CLEAR COUNTER.
SORT worktab BY matnr sernr mblnr bldat bwart lgort i_stnme umwrk umlgo
                    r_stnme lifnr wempf.

  LOOP AT WORKTAB.
    IF WORKTAB-SERNR = TEXT-021 OR ( WORKTAB-WEMPF = SPACE AND
       WORKTAB-BWART = '201' AND  WORKTAB-BWART = '202' AND
       WORKTAB-BWART = '221' AND  WORKTAB-BWART = '222' AND
       WORKTAB-BWART = '261' AND  WORKTAB-BWART = '262' AND
       WORKTAB-BWART = '291' AND  WORKTAB-BWART = '292' ).
      MOVE WORKTAB TO MISTAB.
      APPEND MISTAB.
      CLEAR MISTAB.
    ENDIF.
  ENDLOOP.

  LOOP AT MISTAB.
    ON CHANGE OF MISTAB-MATNR.
      SELECT SINGLE * FROM MAKT
         WHERE MATNR = MISTAB-MATNR
         AND SPRAS = SY-LANGU.
      SKIP 1.
      FORMAT INTENSIFIED OFF.
      WRITE : /20 'MATERIAL NUMBER: ',MISTAB-MATNR, 50 MAKT-MAKTX.
      FORMAT INTENSIFIED ON.
    ENDON.
    IF MISTAB-SERNR = TEXT-021.
      FORMAT INTENSIFIED OFF.
      WRITE: / MISTAB-SERNR.
      FORMAT INTENSIFIED ON.
    ELSE.
      WRITE: / MISTAB-SERNR.
    ENDIF.

    MOVE MISTAB-BWART TO STRING.
    REM = string MOD 2.
    IF REM = 0.
      IF MISTAB-BWART = '122'.
        WRITE: 19 MISTAB-BWART, 24 TEXT-023.
      ELSE.
        WRITE: 19 MISTAB-BWART, 24 TEXT-022.
      ENDIF.
    ELSE.
      WRITE: 19 MISTAB-BWART.
    ENDIF.

    IF MISTAB-SHKZG EQ 'H'.
      WRITE: 34 TEXT-027.
    ENDIF.
    IF MISTAB-SHKZG EQ 'S'.
      WRITE: 34 TEXT-026.
    ENDIF.
    WRITE: 41 MISTAB-I_STNME, 58(10) MISTAB-MBLNR, 71 MISTAB-BLDAT,
           85 MISTAB-UMWRK, 92 MISTAB-UMLGO, 99 MISTAB-R_STNME,
           115 MISTAB-LIFNR.
    IF MISTAB-WEMPF = SPACE.
      IF MISTAB-BWART = '201' OR MISTAB-BWART = '202' OR
         MISTAB-BWART = '221' OR MISTAB-BWART = '222' OR
         MISTAB-BWART = '261' OR MISTAB-BWART = '262' OR
         MISTAB-BWART = '291' OR MISTAB-BWART = '292'.
        MOVE TEXT-021 TO MISTAB-WEMPF.
        FORMAT INTENSIFIED OFF.
        WRITE: 120 MISTAB-WEMPF.
        FORMAT INTENSIFIED ON.
      ELSE.
        WRITE: 120 MISTAB-WEMPF.
      ENDIF.
    ENDIF.
  ENDLOOP.

************************************************************************
*   Subroutines used by a program:

FORM GetPlantName.
  SELECT * FROM T001W
           WHERE WERKS = P_Plant.
  ENDSELECT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PrintPlantInfo                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PrintPlantInfo.
  SKIP 1.
  WRITE: / TEXT-000, TEXT-004, P_Plant, T001W-Name1.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PrintHeadings                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PrintHeadings.
  FORMAT INTENSIFIED OFF.
  ULINE.
  WRITE: / TEXT-005, 19 TEXT-006, 41 TEXT-010, 58 TEXT-011,
           71 TEXT-013, 87 TEXT-025, 99 TEXT-010, 113 TEXT-016,
           124 TEXT-017.
  WRITE: /58 TEXT-012, 85 TEXT-015, 92 TEXT-009, 113 TEXT-012,
          124 TEXT-012.
  ULINE.
  FORMAT INTENSIFIED ON.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GetOutName                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  Field                                                         *
*---------------------------------------------------------------------*
FORM GetOutName USING Field.
  SELECT * FROM T001L
           WHERE WERKS = MSEG-WERKS
           AND LGORT = MSEG-LGORT.
    MOVE T001L-LGOBE TO Field.
  ENDSELECT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GetInName                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  Field                                                         *
*---------------------------------------------------------------------*
FORM GetInName USING Field.
  SELECT * FROM T001L
                WHERE WERKS = MSEG-UMWRK
                AND LGORT = MSEG-UMLGO.
    MOVE T001L-LGOBE TO Field.
  ENDSELECT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SerNrNotFound                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SerNrNotFound.
  CLEAR: worktab-i_stnme, worktab-r_stnme.
  PERFORM GetInName USING Worktab-r_stnme.
  PERFORM GetOutName USING WorkTab-i_stnme.
  MOVE TEXT-021 TO worktab-sernr.
  MOVE-CORRESPONDING MSEG TO worktab.
  MOVE-CORRESPONDING MKPF TO worktab.
  APPEND Worktab.
  CLEAR Worktab.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SerNrFound                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SerNrFound.
  CLEAR: worktab-i_stnme, worktab-r_stnme.
  PERFORM GetInName USING Worktab-r_stnme.
  PERFORM GetOutName USING Worktab-i_stnme.
  MOVE-CORRESPONDING MSEG TO worktab.
  MOVE-CORRESPONDING ZTAB TO WORktab.
  MOVE-CORRESPONDING MKPF TO worktab.
  APPEND Worktab.
  CLEAR Worktab.
ENDFORM.
