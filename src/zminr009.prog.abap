REPORT ZMINR009 LINE-SIZE 132 LINE-COUNT 65 NO STANDARD PAGE HEADING.
************************************************************************
*  Author:            Dorothy Bialkowska
*  Brief Description:
*     - The purpose of this report is to summarize activity performed
*     - in the main warehouse at the end of the week.
************************************************************************
* 98/01/07 md7140 #320 Improve efficiency by reducing number of complete
*                      passes of MSEG from 6 to 1
************************************************************************

TABLES: MARA, MSEG, MKPF, T001W.

DATA:   Char   TYPE C,
        Q_01_io   LIKE MSEG-ERFMG,
        Q_01_to   LIKE MSEG-ERFMG,
        Q_01_ro   LIKE MSEG-ERFMG,
        Q_01_ri   LIKE MSEG-ERFMG,
        Q_01_ti   LIKE MSEG-ERFMG,

        Q_02_io   LIKE MSEG-ERFMG,
        Q_02_to   LIKE MSEG-ERFMG,
        Q_02_ro   LIKE MSEG-ERFMG,
        Q_02_ri   LIKE MSEG-ERFMG,
        Q_02_ti   LIKE MSEG-ERFMG,

        Q_03_io   LIKE MSEG-ERFMG,
        Q_03_to   LIKE MSEG-ERFMG,
        Q_03_ro   LIKE MSEG-ERFMG,
        Q_03_ri   LIKE MSEG-ERFMG,
        Q_03_ti   LIKE MSEG-ERFMG,

        Q_04_io   LIKE MSEG-ERFMG,
        Q_04_to   LIKE MSEG-ERFMG,
        Q_04_ro   LIKE MSEG-ERFMG,
        Q_04_ri   LIKE MSEG-ERFMG,
        Q_04_ti   LIKE MSEG-ERFMG,

        Q_07_io   LIKE MSEG-ERFMG,
        Q_07_to   LIKE MSEG-ERFMG,
        Q_07_ro   LIKE MSEG-ERFMG,
        Q_07_ri   LIKE MSEG-ERFMG,
        Q_07_ti   LIKE MSEG-ERFMG,
        count     TYPE I,
        string    LIKE MSEG-BWART.
************************************************************************
*   Begin of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK Box WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (29) TEXT-001.
PARAMETERS P_CCode   LIKE MSEG-BUKRS DEFAULT 'UGL'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (29) TEXT-002.
PARAMETERS: P_Plant   LIKE T001W-WERKS OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 31(22) TEXT-004.
PARAMETERS P_DATE1  LIKE SY-DATUM DEFAULT SY-DATUM.

SELECTION-SCREEN COMMENT 66(3) TEXT-005.
PARAMETERS P_DATE2  LIKE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************

INITIALIZATION.
  CALL FUNCTION 'DATE_COMPUTE_DAY'
       EXPORTING
            DATE   = P_DATE2
       IMPORTING
            DAY    = CHAR
       EXCEPTIONS
            OTHERS = 1.

  P_date2 = P_date2 - ( char + 1 ).
  P_date1 = P_date2 - 6.

************************************************************************

top-of-page.
  format intensified off.
  write: / text-006, sy-datum, 118 text-007, sy-pagno.
  write: / sy-uzeit under sy-datum, sy-repid under text-007.
  write: /33 TEXT-000, TEXT-008, P_date1, TEXT-005, P_date2.

  format intensified on.

*  Information to be printed at the top of new page.
************************************************************************
START-OF-SELECTION.

  SELECT * FROM MSEG
      WHERE MATNR NE SPACE
      AND ( MJAHR = P_date1(4) OR MJAHR = P_date2(4) )
      AND WERKS = P_Plant
      AND LGORT = 'A001'
      AND BUKRS = P_CCode.

    if ( mseg-bwart = '201' ) or ( mseg-bwart = '221' ) or
       ( mseg-bwart = '261' ) or ( mseg-bwart = '281' ) or
       ( mseg-bwart = '901' ).
      PERFORM GrabDocData.
      CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

      PERFORM GrabMaraRec.
      CHECK MARA-LVORM NE 'X'.
                                       "column 1(issues)
      PERFORM Get_01_Qty USING Q_01_io.
      PERFORM Get_02_Qty USING Q_02_io.
      PERFORM Get_03_Qty USING Q_03_io.
      PERFORM Get_04_Qty USING Q_04_io.
      PERFORM Get_07_Qty USING Q_07_io.
    elseif ( mseg-bwart = '301' ) or ( mseg-bwart = '303' ) or
           ( mseg-bwart = '311' ).
      PERFORM GrabDocData.
      CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

      PERFORM GrabMaraRec.
      CHECK MARA-LVORM NE 'X'.
                                       "column 2(transfers)
      PERFORM Get_01_Qty USING Q_01_to.
      PERFORM Get_02_Qty USING Q_02_to.
      PERFORM Get_03_Qty USING Q_03_to.
      PERFORM Get_04_Qty USING Q_04_to.
      PERFORM Get_07_Qty USING Q_07_to.
    elseif ( mseg-bwart = '122' ).
      PERFORM GrabDocData.
      CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

      PERFORM GrabMaraRec.
      CHECK MARA-LVORM NE 'X'.
                                       "column 3(returns)
      PERFORM Get_01_Qty USING Q_01_ro.
      PERFORM Get_02_Qty USING Q_02_ro.
      PERFORM Get_03_Qty USING Q_03_ro.
      PERFORM Get_04_Qty USING Q_04_ro.
      PERFORM Get_07_Qty USING Q_07_ro.
    elseif ( mseg-bwart = '101' ) or ( mseg-bwart = '941' ).
      PERFORM GrabDocData.
      CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

      PERFORM GrabMaraRec.
      CHECK MARA-LVORM NE 'X'.
                                       "column 4(receipts)
      PERFORM Get_01_Qty USING Q_01_ri.
      PERFORM Get_02_Qty USING Q_02_ri.
      PERFORM Get_03_Qty USING Q_03_ri.
      PERFORM Get_04_Qty USING Q_04_ri.
      PERFORM Get_07_Qty USING Q_07_ri.
    elseif ( mseg-bwart = '305' ).
      PERFORM GrabDocData.
      CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

      PERFORM GrabMaraRec.
      CHECK MARA-LVORM NE 'X'.
                                       "column 5 (in - transfers)
      PERFORM Get_01_Qty USING Q_01_ti.
      PERFORM Get_02_Qty USING Q_02_ti.
      PERFORM Get_03_Qty USING Q_03_ti.
      PERFORM Get_04_Qty USING Q_04_ti.
      PERFORM Get_07_Qty USING Q_07_ti.
    elseif ( mseg-bwart = '935' ).
      MOVE MSEG-BWART TO STRING.

      PERFORM GrabDocData.
      CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

      PERFORM GrabMaraRec.
      CHECK MARA-LVORM NE 'X'.
                                       "number of transactions counted
      IF MARA-MATKL = '0400'.
        count = count + 1.
      ENDIF.
    endif.
  ENDSELECT.

******************************************************************* #320
*SELECT * FROM MSEG
*  WHERE MATNR NE SPACE
*  AND ( MJAHR = P_date1(4) OR MJAHR = P_date2(4) )
*  AND WERKS = P_Plant
*  AND LGORT = 'A001'
*  AND BUKRS = P_CCode
* AND ( BWART = '201' OR BWART = '221' OR BWART = '261' OR BWART = '281'
*       OR BWART = '901' ).

* PERFORM GrabDocData.
* CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

*  PERFORM GrabMaraRec.
*  CHECK MARA-LVORM NE 'X'.

*  PERFORM Get_01_Qty USING Q_01_io.
*  PERFORM Get_02_Qty USING Q_02_io.
*  PERFORM Get_03_Qty USING Q_03_io.
*  PERFORM Get_04_Qty USING Q_04_io.
*  PERFORM Get_07_Qty USING Q_07_io.
*ENDSELECT.

*   Values for first column gathered.
************************************************************************

*SELECT * FROM MSEG
*  WHERE MATNR NE SPACE
*  AND ( MJAHR = P_date1(4) OR MJAHR = P_date2(4) )
*  AND WERKS = P_Plant
*  AND LGORT = 'A001'
*  AND BUKRS = P_CCode
*  AND ( BWART = '301' OR BWART = '303' OR BWART = '311' ).

*  PERFORM GrabDocData.
*  CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

*  PERFORM GrabMaraRec.
*  CHECK MARA-LVORM NE 'X'.

*  PERFORM Get_01_Qty USING Q_01_to.
*  PERFORM Get_02_Qty USING Q_02_to.
*  PERFORM Get_03_Qty USING Q_03_to.
*  PERFORM Get_04_Qty USING Q_04_to.
*  PERFORM Get_07_Qty USING Q_07_to.
*ENDSELECT.

*   Values for second column gathered.
************************************************************************

*SELECT * FROM MSEG
*  WHERE MATNR NE SPACE
*  AND ( MJAHR = P_date1(4) OR MJAHR = P_date2(4) )
*  AND WERKS = P_Plant
*  AND LGORT = 'A001'
*  AND BUKRS = P_CCode
*  AND BWART = '122'.

*  PERFORM GrabDocData.
*  CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

*  PERFORM GrabMaraRec.
*  CHECK MARA-LVORM NE 'X'.

*  PERFORM Get_01_Qty USING Q_01_ro.
*  PERFORM Get_02_Qty USING Q_02_ro.
*  PERFORM Get_03_Qty USING Q_03_ro.
*  PERFORM Get_04_Qty USING Q_04_ro.
*  PERFORM Get_07_Qty USING Q_07_ro.
*ENDSELECT.

*   Values for third column gathered.
************************************************************************

*SELECT * FROM MSEG
*  WHERE MATNR NE SPACE
*  AND ( MJAHR = P_date1(4) OR MJAHR = P_date2(4) )
*  AND WERKS = P_Plant
*  AND LGORT = 'A001'
*  AND BUKRS = P_CCode
*  AND ( BWART = '101' OR BWART = '941' ).

*  PERFORM GrabDocData.
*  CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

*  PERFORM GrabMaraRec.
*  CHECK MARA-LVORM NE 'X'.

*  PERFORM Get_01_Qty USING Q_01_ri.
*  PERFORM Get_02_Qty USING Q_02_ri.
*  PERFORM Get_03_Qty USING Q_03_ri.
*  PERFORM Get_04_Qty USING Q_04_ri.
*  PERFORM Get_07_Qty USING Q_07_ri.
*ENDSELECT.

*   Values for fourth column are gathered.
************************************************************************

*SELECT * FROM MSEG
*  WHERE MATNR NE SPACE
*  AND ( MJAHR = P_date1(4) OR MJAHR = P_date2(4) )
*  AND WERKS = P_Plant
*  AND LGORT = 'A001'
*  AND BUKRS = P_CCode
*  AND BWART = '305'.

*  PERFORM GrabDocData.
*  CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

*  PERFORM GrabMaraRec.
*  CHECK MARA-LVORM NE 'X'.

*  PERFORM Get_01_Qty USING Q_01_ti.
*  PERFORM Get_02_Qty USING Q_02_ti.
*  PERFORM Get_03_Qty USING Q_03_ti.
*  PERFORM Get_04_Qty USING Q_04_ti.
*  PERFORM Get_07_Qty USING Q_07_ti.
*ENDSELECT.

*  Values for last column are gathered.
************************************************************************

*SELECT * FROM MSEG
*  WHERE MATNR NE SPACE
*  AND ( MJAHR = P_date1(4) OR MJAHR = P_date2(4) )
*  AND WERKS = P_Plant
*  AND LGORT = 'A001'
*  AND BUKRS = P_CCode
*  AND BWART = '935'.

*  MOVE MSEG-BWART TO STRING.

*  PERFORM GrabDocData.
*  CHECK MKPF-BLDAT BETWEEN P_date1 and P_date2.

*  PERFORM GrabMaraRec.
*  CHECK MARA-LVORM NE 'X'.

*  IF MARA-MATKL = '0400'.
*     count = count + 1.
*  ENDIF.

*ENDSELECT.

*   Number of transaction counted.
************************************************************************
  PERFORM GetPlantName.
  PERFORM PrintColHeadings.
  PERFORM PrintVert.

  WRITE: 2 TEXT-009, 33 Q_01_io, 53  Q_01_to, 72  Q_01_ro,
         94 Q_01_ri, 114 Q_01_ti.
  WRITE: /1(89) SY-ULINE, 91(41) SY-ULINE.

  PERFORM PrintVert.
  WRITE: 2 TEXT-010, 33 Q_02_io, 53  Q_02_to, 72  Q_02_ro,
         94 Q_02_ri, 114 Q_02_ti.
  WRITE: /1(89) SY-ULINE, 91(41) SY-ULINE.

  PERFORM PrintVert.
  WRITE: 2 TEXT-011, 33 Q_03_io, 53  Q_03_to, 72  Q_03_ro,
         94 Q_03_ri, 114 Q_03_ti.
  WRITE: /1(89) SY-ULINE, 91(41) SY-ULINE.

  PERFORM PrintVert.
  WRITE: 2 TEXT-012, 33 Q_04_io, 53  Q_04_to, 72  Q_04_ro,
         94 Q_04_ri, 114 Q_04_ti.
  WRITE: /1(89) SY-ULINE, 91(41) SY-ULINE.

  PERFORM PrintVert.
  WRITE: 2 TEXT-013, 33 Q_07_io, 53  Q_07_to, 72  Q_07_ro,
         94 Q_07_ri, 114 Q_07_ti.
  WRITE: /1(89) SY-ULINE, 91(41) SY-ULINE.

  SKIP 3.
  FORMAT INTENSIFIED OFF.
  WRITE : /3 TEXT-012, 31 TEXT-023,  ':', count.
************************************************************************
* Subroutines used by a program

FORM PrintColHeadings.
  WRITE: /1(89) SY-ULINE, 91(41) SY-ULINE.
  WRITE: /31(58) TEXT-019 NO-GAP COLOR COL_HEADING INTENSIFIED ON,
          1 SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP, 89 SY-VLINE NO-GAP,
          91 SY-VLINE NO-GAP, 131 SY-VLINE NO-GAP,
          92(39) TEXT-020 NO-GAP COLOR COL_HEADING INTENSIFIED ON.
  WRITE: /31(58) SY-ULINE, 91(41) SY-ULINE,
          1 SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP, 89 SY-VLINE NO-GAP,
          91 SY-VLINE NO-GAP, 131 SY-VLINE NO-GAP.
  WRITE: /1 SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP, 50 SY-VLINE NO-GAP,
          70 SY-VLINE NO-GAP, 89 SY-VLINE NO-GAP, 91 SY-VLINE NO-GAP,
          111 SY-VLINE NO-GAP, 131 SY-VLINE NO-GAP,
          2(28) TEXT-018 NO-GAP COLOR COL_HEADING INTENSIFIED ON,
          31(19) TEXT-014 NO-GAP COLOR COL_HEADING INTENSIFIED ON,
          51(19) TEXT-015 NO-GAP COLOR COL_HEADING INTENSIFIED ON,
          71(18) TEXT-016 NO-GAP COLOR COL_HEADING INTENSIFIED ON,
          92(19) TEXT-017 NO-GAP COLOR COL_HEADING INTENSIFIED ON,
          112(19) TEXT-015 NO-GAP COLOR COL_HEADING INTENSIFIED ON.
  WRITE: /1(88) SY-ULINE, 91(41) SY-ULINE,
          30 SY-VLINE NO-GAP, 50 SY-VLINE NO-GAP, 89 SY-VLINE NO-GAP,
          91 SY-VLINE NO-GAP, 131 SY-VLINE NO-GAP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PrintVert                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PrintVert.
  WRITE: /1 SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP, 50 SY-VLINE NO-GAP,
          70 SY-VLINE NO-GAP, 89 SY-VLINE NO-GAP, 91 SY-VLINE NO-GAP,
          111 SY-VLINE NO-GAP, 131 SY-VLINE NO-GAP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GrabDocData                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GrabDocData.
  SELECT SINGLE *  FROM MKPF
    WHERE MBLNR = MSEG-MBLNR
    AND MJAHR = MSEG-MJAHR.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GrabMaraRec                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GrabMaraRec.
  SELECT SINGLE * FROM MARA
    WHERE MATNR = MSEG-MATNR.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Get_01_Qty                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  quantity                                                      *
*---------------------------------------------------------------------*
FORM Get_01_Qty USING quantity.
  IF MARA-MATKL = '0100'.
    quantity = MSEG-ERFMG + quantity.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Get_02_Qty                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  Quantity                                                      *
*---------------------------------------------------------------------*
FORM Get_02_Qty USING Quantity.
  IF MARA-MATKL = '0201'.
  ELSEIF MARA-MATKL = '0202'.
  ELSEIF MARA-MATKL = '0203'.
  ELSEIF MARA-MATKL = '0207'.
  ELSEIF MARA-MATKL = '0208'.
  ELSEIF MARA-MATKL = '0210'.
  ELSEIF MARA-MATKL = '0211'.
  ELSEIF MARA-MATKL = '0212'.
  ELSEIF MARA-MATKL = '0213'.
  ELSEIF MARA-MATKL = '0214'.
    quantity = MSEG-ERFMG + quantity.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Get_03_Qty                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  quantity                                                      *
*---------------------------------------------------------------------*
FORM Get_03_Qty USING quantity.
  IF MARA-MATKL = '0300'.
    quantity = MSEG-ERFMG + QUANTITY.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Get_04_Qty                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  quantity                                                      *
*---------------------------------------------------------------------*
FORM Get_04_Qty USING quantity.
  IF MARA-MATKL = '0400'.
    quantity = MSEG-ERFMG + quantity.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Get_07_QTY                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  quantity                                                      *
*---------------------------------------------------------------------*
FORM Get_07_QTY USING quantity.
  IF MARA-MATKL = '0701'.
  ELSEIF MARA-MATKL = '0702'.
  ELSEIF MARA-MATKL = '0703'.
  ELSEIF MARA-MATKL = '0704'.
  ELSEIF MARA-MATKL = '0705'.
  ELSEIF MARA-MATKL = '0708'.
  ELSEIF MARA-MATKL = '0709'.
  ELSEIF MARA-MATKL = '0710'.
    quantity = MSEG-ERFMG + quantity.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GetPlantName                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GetPlantName.
  SELECT * FROM T001W
    WHERE WERKS = P_Plant.
  ENDSELECT.

  FORMAT INTENSIFIED OFF.
  WRITE: /1 TEXT-022, P_plant, T001W-NAME1.
  WRITE: /1 TEXT-021.
  SKIP 1.
  FORMAT INTENSIFIED ON.
ENDFORM.
