REPORT zmimr008 LINE-SIZE 132 LINE-COUNT 65 MESSAGE-ID zm
                NO STANDARD PAGE HEADING.
***********************************************************************
*   Report      : ZMIMR008
*   Programmer  : Cathy McCoy
*   Date        : August 20. 2001
*
*   Purpose     : This report identifies the quanitity and value of all
*                 issues and transfers by Plant for a stated period of
*                 time. This report will collect the material usage as
*                 part of the calculation to identify the number of
*                 Inventory turns.
*  Issue Log #846
************************************************************************
TABLES: mseg.                              "Goods Movement

DATA: total_qnt TYPE mseg-menge,             "quantity counter
      total_val TYPE mseg-dmbtr,             "credit value
      total_deb TYPE mseg-dmbtr.             "debit value

DATA: BEGIN OF int_wa  OCCURS 0,

      werks     LIKE mseg-werks,                            "Plant
      bwart     LIKE mseg-bwart,            "Goods Issue / Transfer
      lgort     LIKE mseg-lgort,            "Storage Location
      mblnr     LIKE mseg-mblnr,            "Material Document
      matnr     LIKE mseg-matnr,            "Material Number
      mjahr     LIKE mseg-mjahr,                            "Year
      dmbtr     LIKE mseg-dmbtr,                            "Value
      shkzg     LIKE mseg-shkzg,            "Debit or Credit
      menge     LIKE mseg-menge,                            "Quantity

      END OF int_wa.




*_____________________  SELECTION SCREEN  ______________________________

SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 15(56) text-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME.
PARAMETERS: s_plnt   LIKE mseg-werks OBLIGATORY.
PARAMETERS: s_year   LIKE mseg-mjahr OBLIGATORY
                     DEFAULT sy-datum(4).

SELECTION-SCREEN END OF BLOCK box2.


SELECTION-SCREEN END OF BLOCK box.


*____________________  AT SELECTION-SCREEN  ____________________________





*_____________________  START OF SELECTION  ____________________________

START-OF-SELECTION.

  PERFORM:
           write_header,
           print_headings,
           data_select,
           total.


*_____________________   WRITE HEADER  _________________________________

FORM write_header.
  FORMAT INTENSIFIED OFF.
  ULINE.
  WRITE:   /01 sy-vline
         , 003 sy-datum
         , 120 text-002,  sy-pagno
         , 132 sy-vline.
  WRITE:   /01 sy-vline
         , sy-uzeit UNDER sy-datum
         , sy-repid UNDER text-002
         , 132 sy-vline.
  WRITE:   /01 sy-vline
         , 040 text-003
         , 132 sy-vline.
  WRITE:  /01 sy-vline
         , text-006 UNDER sy-uzeit
         , s_year
         , 132 sy-vline
         , /01 sy-vline
         , text-004 UNDER text-006
         , s_plnt
         , text-005 UNDER text-002
         , sy-mandt
         , 132 sy-vline.
  ULINE.

  FORMAT INTENSIFIED ON.
  ULINE.
ENDFORM.


************************************************************************
*                            SUBROUTINES
************************************************************************


*_________________________ DATA_SELECT _________________________________

*Select data by year and Plant ---- Move into Internal Table
*_______________________________________________________________________

FORM data_select.
  total_qnt = '0'.
  total_val = '0'.
  total_deb = '0'.

  SELECT * FROM mseg INTO CORRESPONDING FIELDS OF int_wa
    WHERE mjahr EQ s_year AND werks EQ s_plnt.
    PERFORM  print_data..
  ENDSELECT.


ENDFORM.

*_________________________ PRINT_HEADINGS ______________________________

* Print Column Headings
*_______________________________________________________________________

FORM print_headings.
  FORMAT COLOR 2 ON.
  WRITE: 10 text-plt,                                       "plant
         20 text-typ,          "Movement Type
         37 text-gdi,          "Goods Issue
         52 text-gdt,          "Goods Transfer
         69 text-val,                                       "Value
         85 text-qnt.                                       "quantity
  FORMAT COLOR 2 OFF.
  SKIP.
ENDFORM.

*________________________  PRINT_DATA __________________________________

* Print Data under Column Headings
*_______________________________________________________________________

FORM print_data.
  WRITE 84 sy-vline.

  IF int_wa-bwart BETWEEN '201' AND '292'.
    PERFORM write_gi.
  ELSEIF int_wa-bwart  BETWEEN '331' AND '336'.
    PERFORM write_gi.
  ELSEIF int_wa-bwart BETWEEN '101' AND '162'.
    PERFORM write_gi.
  ELSEIF int_wa-bwart BETWEEN  '451' AND '452'.
    PERFORM write_gi.
  ELSEIF int_wa-bwart BETWEEN  '621' AND '634'.
    PERFORM write_gi.
  ELSEIF int_wa-bwart BETWEEN  '661' AND '662'.
    PERFORM write_gi.
  ELSEIF int_wa-bwart BETWEEN  '702' AND '718'.
    PERFORM write_gi.
  ELSEIF int_wa-bwart BETWEEN  '901' AND '902'.
    PERFORM write_gi.
  ELSEIF int_wa-bwart BETWEEN  '935' AND '936'.
    PERFORM write_gi.
  ENDIF.

  IF int_wa-bwart BETWEEN '301' AND '326'.
    PERFORM write_gt.
  ELSEIF int_wa-bwart  BETWEEN '341' AND '352'.
    PERFORM write_gt.
  ELSEIF int_wa-bwart BETWEEN  '411' AND '416'.
    PERFORM write_gt.
  ELSEIF int_wa-bwart BETWEEN  '455' AND '456'.
    PERFORM write_gt.
  ELSEIF int_wa-bwart BETWEEN  '603' AND '606'.
    PERFORM write_gt.
  ELSEIF int_wa-bwart BETWEEN  '641' AND '648'.
    PERFORM write_gt.
  ELSEIF int_wa-bwart BETWEEN  '671' AND '678'.
    PERFORM write_gt.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  WRITE_GI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_gi.
  WRITE: / int_wa-mblnr UNDER text-gdi,
           int_wa-werks UNDER text-plt,
           int_wa-bwart UNDER text-typ.

  IF int_wa-shkzg = 'S'.
    COMPUTE: total_deb = total_deb + int_wa-dmbtr.
    FORMAT COLOR 6 INVERSE.
    WRITE: (08) int_wa-dmbtr UNDER text-val,
           (07) int_wa-menge DECIMALS 0 UNDER text-qnt RIGHT-JUSTIFIED.
    FORMAT COLOR OFF.
    FORMAT INVERSE OFF.
  ELSE.
    COMPUTE: total_qnt = total_qnt + int_wa-menge,
             total_val = total_val + int_wa-dmbtr.
    WRITE: (08) int_wa-dmbtr UNDER text-val,
           (07) int_wa-menge DECIMALS 0 UNDER text-qnt RIGHT-JUSTIFIED.
  ENDIF.

ENDFORM.                                                    " WRITE_GI

*&---------------------------------------------------------------------*
*&      Form  WRITE_GT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_gt.
  WRITE: / int_wa-mblnr UNDER text-gdt,
           int_wa-werks UNDER text-plt,
           int_wa-bwart UNDER text-typ.

  IF int_wa-shkzg = 'S'.
    COMPUTE: total_deb = total_deb + int_wa-dmbtr.
    FORMAT COLOR 6 INVERSE.
    WRITE: (08) int_wa-dmbtr UNDER text-val,
           (07) int_wa-menge DECIMALS 0 UNDER text-qnt RIGHT-JUSTIFIED.
    FORMAT COLOR OFF.
    FORMAT INVERSE OFF.
  ELSE.
    COMPUTE: total_qnt = total_qnt + int_wa-menge,
            total_val = total_val + int_wa-dmbtr.
    WRITE: (08) int_wa-dmbtr UNDER text-val,
           (07) int_wa-menge DECIMALS 0 UNDER text-qnt RIGHT-JUSTIFIED.
  ENDIF.

ENDFORM.                                                    " WRITE_GT

*&---------------------------------------------------------------------*
*&      Form  TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM total.
  SKIP.
  ULINE.
  WRITE: 'TOTAL:' UNDER text-plt,
         (08) total_val UNDER text-val,
         'credit',
         (07) total_qnt DECIMALS 0 UNDER text-qnt,
          'credit'.
  WRITE: /(08) total_deb UNDER text-val,
         'debit'.

  ULINE.
ENDFORM.                                                    " TOTAL

