REPORT ZMIMR007 LINE-SIZE 132 LINE-COUNT 65 MESSAGE-ID ZM
                NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Report ZMIMR007
* Date: Feb 25, 1997
* Purpose - To determine the turnover of specified materials for the
*           last 12 complete months.
*-----------------------------------------------------------------------
* Note:
* You have 2 choices for sorting the report and one and only one
* must be selected each time the report is executed.
* 1) by Plant / Material Group
* 2) by Material Group / Plant
*-----------------------------------------------------------------------
*  No. of turns = consumption / avg. inventory

*         where consumption = sum of all issues for the period
*           and avg. inventory =  sum of inventory at end of month
*                    divided by the number of months in the period

*  Inventory at the end of the month can be calculated by summing
*    opening balance and the months receipts and subtracting the issues
*
*  Months in period is calculated by:
*  - if activity prior to period, each month in counted
*  - if no activity prior to period, the month count starts at the
*         month the first activity occurred
*-----------------------------------------------------------------------
* 03/03/20 gymana   #--- Added capability to print report based on
*                        single material number
* 03/01/14 mdemeest #--- Fix number of months when last month in period
*                        doesn't have a source list (WS_MNTHS)
* 99/10/27 mdemeest #--- Fix turns report
* 97/04/17 md7140 Fix AVG-MONTHS
* 97/04/17 md7140 Fix TOTAL-MONTH-CNTR & TOTAL-LOC-CNTR.
*-----------------------------------------------------------------------
TABLES: S031,
        S032.

DATA: MONTH(02) TYPE N,
       YEAR(04) TYPE N,
       YEARC(04),
       MONTHC(02),
       THIS_MONTH(6)  TYPE N,
       CURRFROM(6)    TYPE N,
       CURRTO(6)      TYPE N,
       start_date(6)  type n,
* used to loop thru S031
       WORK_YEAR(04) TYPE N,
       WORK_MNTH(02) TYPE N,
       WORK_DATE(06) TYPE N,


       MATR_INV_TOTAL(09) TYPE P DECIMALS 3,
       MATR_MON_STRT_INV(09) TYPE P DECIMALS 3,
       MATR_MON_END_INV(09) TYPE P DECIMALS 3,
       MATR_INV_TOT_PER1(09) TYPE P DECIMALS 3,
*      matr_inv_tot_per2(09) type p decimals 3,
       MATR_MON_CONSUMPT(07) TYPE P DECIMALS 3,
       MATR_CONS_TOT_PER1 TYPE I,
*      matr_cons_tot_per2 type i,
       MATR_AVG_INV_PER1(09) TYPE P DECIMALS 3,
*      matr_avg_inv_per2(09) type p decimals 3,
       MATR_TURNOVER1(3) TYPE P DECIMALS 2,
*      matr_turnover2(3) type p decimals 2,
       MONTH_CNTR TYPE I,
       WS_MNTHS TYPE I.

DATA:  AVG-MONTHS(09)      TYPE P  DECIMALS 3.

DATA:    WRK_QOH        LIKE S031-MZUBB,
         AVG_INV(09)    TYPE P DECIMALS 3,
         TURNS(05)      TYPE P DECIMALS 3,
         INPUT_LEN      TYPE I,
         SORT_DESC1(5)  TYPE C,
         SORT_DESC2(5)  TYPE C,
         SORT_MATL_DESC1(8) TYPE C,
         SORT_MATL_DESC2(8) TYPE C.

DATA:  BEGIN OF WA OCCURS 1000,
         SORTKEY1(20)   TYPE C,
         SORTKEY2(20)   TYPE C,
         SORTKEY3(20)   TYPE C,
         MATNR          LIKE S032-MATNR,         "Material Number
         WERKS          LIKE S031-WERKS,         "Plant
         LGORT          LIKE S031-LGORT,         "Storage Location
         MATKL          LIKE S032-MATKL,         "Material Group
         CONSUMP        LIKE S031-MZUBB,         "12 Mth Consump
         QOH            LIKE S031-MZUBB,         "12 Mth opening balance
         AVG_INV(09)    TYPE P DECIMALS 3,

         MONTHS(9)      TYPE P DECIMALS 3.
DATA:  END OF WA.


SELECT-OPTIONS PLANT    FOR S032-WERKS.
* 99/11/01 Removed per Mike McCarty - Inventory handled at main warehous
*select-options storloc  for s032-lgort default 'A001'.
SELECT-OPTIONS MATRGRP  FOR S032-MATKL.
SELECT-OPTIONS MATERIAL FOR S032-MATNR MATCHCODE OBJECT MAT1.
selection-screen skip 2.
Select-options s_date   for s031-spmon.
SELECTION-SCREEN  SKIP 2.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (2) TEXT-011.
PARAMETER: P_SORT1  AS CHECKBOX.
SELECTION-SCREEN: COMMENT (30) TEXT-012.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (2) TEXT-013.
PARAMETER: P_SORT2 AS CHECKBOX.
SELECTION-SCREEN: COMMENT (30) TEXT-014.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (5) TEXT-015.
PARAMETER: P_DETAIL AS CHECKBOX.
SELECTION-SCREEN: COMMENT (40) TEXT-016.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (2) TEXT-017.
PARAMETER: P_MATL AS CHECKBOX.
SELECTION-SCREEN: COMMENT (30) TEXT-018.
SELECTION-SCREEN END OF LINE.

at selection-screen output.
  move 'IBT' to s_date+0(3).
  move sy-datum+0(6) to s_date+9(6).
  compute s_date+3(6) = s_date+9(6) - 100.   "Starting period
  compute s_date+9(6) = s_date+9(6) - 1.     "Ending period
  if s_date+13(2) = '00'.
     compute s_date+9(6) = s_date+9(6) - 88. "December of previous year
  endif.
  append s_date.

AT SELECTION-SCREEN.
  IF P_MATL = ' '.
     IF ( P_SORT1 = ' ' AND P_SORT2 = ' ' ) OR
        ( P_SORT1 = 'X' AND P_SORT2 = 'X' ).
        MESSAGE E100 WITH 'Please select only one of the sort methods'.
     ENDIF.
  ENDIF.
  IF P_MATL = 'X'.
     IF ( P_SORT1 = 'X' OR P_SORT2 = 'X' OR P_DETAIL = 'X' ).
        MESSAGE E100 WITH
        'Do not select sort or detail with material report.'.
     ELSE.
        IF MATERIAL-OPTION = 'BT'.
           MESSAGE E100 WITH
        'Only one material number can be selected.'.
        ENDIF.
        IF MATERIAL-LOW = ' '.
           MESSAGE E100 WITH
           'Please input one material number.'.
        ENDIF.
     ENDIF.
  ENDIF.

*
START-OF-SELECTION.
*-----------------------------------------------------------------------
* Determine period
*-----------------------------------------------------------------------
 move s_date+9(6) to currto.
 move s_date+3(6) to currfrom.

   IF P_MATL = 'X'.
      PERFORM PROCESS_MATL_DATA.
      PERFORM SET_SORT_KEY.
      PERFORM DISPLAY_MATL_REPORT.
   ELSE.
      PERFORM PROCESS_DATA.
      PERFORM SET_SORT_KEY.
      PERFORM DISPLAY_REPORT.
   ENDIF.

END-OF-SELECTION.

FORM PROCESS_DATA.
*-----------------------------------------------------------------------
* Select all rows from S032 that meet the selection criteria - This
* table is a picture of the inventory at the precise moment, the abap
* is executed and contains the current quantity on hand (mbwbest).
*-----------------------------------------------------------------------
 SELECT * FROM S032
    WHERE SSOUR = SPACE
      AND VRSIO = '000'
      AND WERKS IN PLANT
      AND MATNR IN MATERIAL
      AND LGORT = 'A001'
      AND MATKL IN MATRGRP.

*-----------------------------------------------------------------------
* Reinitialized THIS_MONTH to the end of period selected.  This is
* the month variable that is changed thru looping.
*-----------------------------------------------------------------------
  move currto to this_month.

*-----------------------------------------------------------------------
* Clear all temporary fields used for calculations in material
*-----------------------------------------------------------------------
   CLEAR WA.                   "Clear header record
   clear: ws_mnths,            "Months counter for start of new material
          wrk_qoh.             "Closing balance
   MOVE S032-MATNR   TO WA-MATNR.
   MOVE S032-WERKS   TO WA-WERKS.
   MOVE S032-LGORT   TO WA-LGORT.
   MOVE S032-MATKL   TO WA-MATKL.
   MOVE S032-MBWBEST TO WRK_QOH.


*-----------------------------------------------------------------------
*  Determine the starting month.  It will either be the starting month
*  in the variant or the first month that the material is used (ie.
*  whichever is the most recent.)
*-----------------------------------------------------------------------
 clear start_date.
 select min( spmon ) from S031 into start_date
                         WHERE SSOUR = SPACE
                           AND VRSIO = '000'
                           AND SPTAG = '00000000'
                           AND SPWOC = '000000'
                           AND SPBUP = '000000'
                           AND WERKS = WA-WERKS
                           AND MATNR = WA-MATNR
                           AND LGORT = WA-LGORT.
  if currfrom > start_date.
     move currfrom to start_date.
  endif.

*-----------------------------------------------------------------------
* Determine inventory at the end of the period selected.
* ----------------------------------------------------------------------
 SELECT * FROM S031 WHERE SSOUR = SPACE
                      AND VRSIO = '000'
                      AND SPMON > THIS_MONTH
                      AND SPTAG = '00000000'
                      AND SPWOC = '000000'
                      AND SPBUP = '000000'
                      AND WERKS = WA-WERKS
                      AND MATNR = WA-MATNR
                      AND LGORT = WA-LGORT.
       WRK_QOH = WRK_QOH + S031-MAGBB - S031-MZUBB.

 ENDSELECT.                                          "End of S031 select

*-----------------------------------------------------------------------
* Determine the inventory at the end of each month in period
* Adjust the current inventory from the S032 (mbwbest) and adding the
* issues and subtracting the receipts, you can derive the quantity on
* hand for the current month.
*-----------------------------------------------------------------------
 move currto to this_month.
 do.
 add wrk_qoh to wa-qoh.
 SELECT * FROM S031 WHERE SSOUR = SPACE
                           AND VRSIO = '000'
                           AND SPMON = THIS_MONTH
                           AND SPTAG = '00000000'
                           AND SPWOC = '000000'
                           AND SPBUP = '000000'
                           AND WERKS = WA-WERKS
                           AND MATNR = WA-MATNR
                           AND LGORT = WA-LGORT.
       WRK_QOH = WRK_QOH + S031-MAGBB - S031-MZUBB.
       add s031-magbb to wa-consump.         "Sum of Monthly consumption
 ENDSELECT.
    compute wa-months = wa-months + 1.

 compute this_month = this_month - 1.
 if this_month+4(2) = '00'.
    compute this_month(4) = this_month(4) - 1.
    move '12' to this_month+4(2).
 endif.
 if this_month < start_date.
    exit.
 endif.
 enddo.

 compute wa-avg_inv = wa-qoh / wa-months.

 APPEND WA.           "Contains all info for 1 material
ENDSELECT.                           "End of S032 select
ENDFORM.

FORM DISPLAY_REPORT.

  SORT WA BY SORTKEY1 SORTKEY2 SORTKEY3.
  LOOP AT WA.
     AT NEW SORTKEY1.
        WRITE: / WA-SORTKEY1 UNDER SORT_DESC1.
     ENDAT.

     IF WA-MONTHS = 0.
        MOVE 0 TO AVG_INV.
     ELSE.
        COMPUTE AVG_INV = WA-QOH / WA-MONTHS.
     ENDIF.
     MOVE AVG_INV TO WA-AVG_INV.
     MODIFY WA.

     IF AVG_INV = 0.
        MOVE 0 TO TURNS.
     ELSE.
         COMPUTE TURNS = WA-CONSUMP / AVG_INV.
     ENDIF.

* print material detail only if requested
     IF P_DETAIL = 'X'.
        WRITE: / WA-MATNR     UNDER TEXT-001,
                 WA-AVG_INV   UNDER TEXT-002,
                 WA-CONSUMP   UNDER TEXT-003,
                 TURNS        UNDER TEXT-004,
                 WA-QOH       UNDER TEXT-008,
                 WA-MONTHS    UNDER TEXT-010.
     ENDIF.

    AT END OF SORTKEY2.
       write: /.
       SUM.
       IF WA-AVG_INV = 0.
           TURNS = 0.
       ELSE.
           COMPUTE TURNS = WA-CONSUMP / WA-AVG_INV.
       ENDIF.

       WRITE: / WA-SORTKEY2   UNDER SORT_DESC2,
                WA-AVG_INV    UNDER TEXT-002,
                WA-CONSUMP    UNDER TEXT-003,
                TURNS         UNDER TEXT-004.
    ENDAT.

    AT END OF SORTKEY1.
       SUM.
       IF WA-AVG_INV = 0.
           TURNS = 0.
       ELSE.
           COMPUTE TURNS = WA-CONSUMP / WA-AVG_INV.
       ENDIF.

       WRITE: / SY-ULINE(18)  UNDER TEXT-002,
                SY-ULINE(18)  UNDER TEXT-003,
                SY-ULINE(11)  UNDER TEXT-004.
       WRITE: / WA-SORTKEY1   UNDER SORT_DESC1, TEXT-006,
                WA-AVG_INV    UNDER TEXT-002,
                WA-CONSUMP    UNDER TEXT-003,
                TURNS         UNDER TEXT-004.
       ULINE.
    ENDAT.

    AT LAST.
       SUM.
       IF WA-AVG_INV = 0.
           TURNS = 0.
       ELSE.
           COMPUTE TURNS = WA-CONSUMP / WA-AVG_INV.
       ENDIF.

       WRITE: / TEXT-007      UNDER SORT_DESC1,
                WA-AVG_INV    UNDER TEXT-002,
                WA-CONSUMP    UNDER TEXT-003,
                TURNS         UNDER TEXT-004.
       ULINE.
    ENDAT.

  ENDLOOP.
ENDFORM.
*  else.
*    move material-high+12(6) to disp_matnr2.
*    write:disp_matnr1, '-', disp_matnr2, 18 matr_avg_inv_per2,
*     39 matr_cons_tot_per2,
*     54 matr_turnover2,
*     78 matr_avg_inv_per1,
*     99 matr_cons_tot_per1,
*     114 matr_turnover1.
*  endif.
*endif.
*if matrgrp_ind = 'y'.
*  if matrgrp-high < '0'.
*    write:matrgrp-low, 18 matr_avg_inv_per2,
*     39 matr_cons_tot_per2,
*     54 matr_turnover2,
*     78 matr_avg_inv_per1,
*     99 matr_cons_tot_per1,
*     114 matr_turnover1.
*  else.
*    concatenate matrgrp-low '-' matrgrp-high into long_matgr
*      separated by space.
*    condense long_matgr.
*    move matrgrp-low+0(4) to disp_matgr1.
*    move matrgrp-high+0(4) to disp_matgr2.
*    write:long_matgr, 18 matr_avg_inv_per2,
*     39 matr_cons_tot_per2,
*     54 matr_turnover2,
*     78 matr_avg_inv_per1,
*     99 matr_cons_tot_per1,
*     114 matr_turnover1.
*  endif.
*endif.
* format and write a warning message if the full range of 24 months of
* data is not available.
*if avg-months le 12.
*  if avg-months = 0.
*    skip 2.
*    write: 5 '** NOTE - There is no data available for the selection',
*        'criteria chosen.'.
*  elseif month_cntr = 12.
*    skip 2.
*    write: 5 '** NOTE - There is no data available for the previous',
*       ' 12 month period.'.
*  else.
*    skip 2.
*    write: 5 '** NOTE - There is only an average of ', avg-months,
*   ' months data available for the completed current 12 month period.'.
*    write:/15 'There is no data available for the previous 12 month',
*     ' period.'.
*  endif.
*elseif month_cntr < 24.
*  skip 2.
*  month_cntr3 = month_cntr - 12.
*  write: 5 '** NOTE - There is only an average of ', avg-months,
*  'months data availables for the previous 12 month period.'.
*endif.
FORM PROCESS_MATL_DATA.
*-----------------------------------------------------------------------
* Select all rows from S032 that meet the selection criteria - This
* table is a picture of the inventory at the precise moment, the abap
* is executed and contains the current quantity on hand (mbwbest).
* Note: This form is the same as PROCESS_DATA except for the S032
*       SELECT statement below.
*-----------------------------------------------------------------------
 SELECT * FROM S032
    WHERE SSOUR = SPACE
      AND VRSIO = '000'
      AND MATNR IN MATERIAL
      AND LGORT = 'A001'.

*-----------------------------------------------------------------------
* Reinitialized THIS_MONTH to the end of period selected.  This is
* the month variable that is changed thru looping.
*-----------------------------------------------------------------------
  move currto to this_month.

*-----------------------------------------------------------------------
* Clear all temporary fields used for calculations in material
*-----------------------------------------------------------------------
   CLEAR WA.                   "Clear header record
   clear: ws_mnths,            "Months counter for start of new material
          wrk_qoh.             "Closing balance
   MOVE S032-MATNR   TO WA-MATNR.
   MOVE S032-WERKS   TO WA-WERKS.
   MOVE S032-LGORT   TO WA-LGORT.
   MOVE S032-MATKL   TO WA-MATKL.
   MOVE S032-MBWBEST TO WRK_QOH.


*-----------------------------------------------------------------------
*  Determine the starting month.  It will either be the starting month
*  in the variant or the first month that the material is used (ie.
*  whichever is the most recent.)
*-----------------------------------------------------------------------
 clear start_date.
 select min( spmon ) from S031 into start_date
                         WHERE SSOUR = SPACE
                           AND VRSIO = '000'
                           AND SPTAG = '00000000'
                           AND SPWOC = '000000'
                           AND SPBUP = '000000'
                           AND WERKS = WA-WERKS
                           AND MATNR = WA-MATNR
                           AND LGORT = WA-LGORT.
  if currfrom > start_date.
     move currfrom to start_date.
  endif.

*-----------------------------------------------------------------------
* Determine inventory at the end of the period selected.
* ----------------------------------------------------------------------
 SELECT * FROM S031 WHERE SSOUR = SPACE
                      AND VRSIO = '000'
                      AND SPMON > THIS_MONTH
                      AND SPTAG = '00000000'
                      AND SPWOC = '000000'
                      AND SPBUP = '000000'
                      AND WERKS = WA-WERKS
                      AND MATNR = WA-MATNR
                      AND LGORT = WA-LGORT.
       WRK_QOH = WRK_QOH + S031-MAGBB - S031-MZUBB.

 ENDSELECT.                                          "End of S031 select

*-----------------------------------------------------------------------
* Determine the inventory at the end of each month in period
* Adjust the current inventory from the S032 (mbwbest) and adding the
* issues and subtracting the receipts, you can derive the quantity on
* hand for the current month.
*-----------------------------------------------------------------------
 move currto to this_month.
 do.
 add wrk_qoh to wa-qoh.
 SELECT * FROM S031 WHERE SSOUR = SPACE
                           AND VRSIO = '000'
                           AND SPMON = THIS_MONTH
                           AND SPTAG = '00000000'
                           AND SPWOC = '000000'
                           AND SPBUP = '000000'
                           AND WERKS = WA-WERKS
                           AND MATNR = WA-MATNR
                           AND LGORT = WA-LGORT.
       WRK_QOH = WRK_QOH + S031-MAGBB - S031-MZUBB.
       add s031-magbb to wa-consump.         "Sum of Monthly consumption
 ENDSELECT.
    compute wa-months = wa-months + 1.

 compute this_month = this_month - 1.
 if this_month+4(2) = '00'.
    compute this_month(4) = this_month(4) - 1.
    move '12' to this_month+4(2).
 endif.
 if this_month < start_date.
    exit.
 endif.
 enddo.

 compute wa-avg_inv = wa-qoh / wa-months.

 APPEND WA.           "Contains all info for 1 material
ENDSELECT.                           "End of S032 select
ENDFORM.


FORM DISPLAY_MATL_REPORT.

  SORT WA BY SORTKEY1 SORTKEY2.
  LOOP AT WA.
     AT NEW SORTKEY1.
        WRITE: / WA-SORTKEY1+12(6) UNDER SORT_MATL_DESC1.
     ENDAT.

     IF WA-MONTHS = 0.
        MOVE 0 TO AVG_INV.
     ELSE.
        COMPUTE AVG_INV = WA-QOH / WA-MONTHS.
     ENDIF.
     MOVE AVG_INV TO WA-AVG_INV.
     MODIFY WA.

     IF AVG_INV = 0.
        MOVE 0 TO TURNS.
     ELSE.
         COMPUTE TURNS = WA-CONSUMP / AVG_INV.
     ENDIF.

     WRITE: / WA-WERKS     UNDER SORT_MATL_DESC2,
              WA-AVG_INV   UNDER TEXT-002,
              WA-CONSUMP   UNDER TEXT-003,
              TURNS        UNDER TEXT-004,
              WA-QOH       UNDER TEXT-008,
              WA-MONTHS    UNDER TEXT-010.

    AT END OF SORTKEY1.
       WRITE: /.
       SUM.
       IF WA-AVG_INV = 0.
           TURNS = 0.
       ELSE.
           COMPUTE TURNS = WA-CONSUMP / WA-AVG_INV.
       ENDIF.

       WRITE: / SY-ULINE(18)  UNDER TEXT-002,
                SY-ULINE(18)  UNDER TEXT-003,
                SY-ULINE(11)  UNDER TEXT-004.
       WRITE: / WA-SORTKEY1+12(7) UNDER SORT_MATL_DESC1, TEXT-006,
                WA-AVG_INV    UNDER TEXT-002,
                WA-CONSUMP    UNDER TEXT-003,
                TURNS         UNDER TEXT-004.
       ULINE.
    ENDAT.

  ENDLOOP.
ENDFORM.
TOP-OF-PAGE.
WRITE: / TEXT-RPT, SY-REPID, 30 TEXT-TTL,
      62 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO,
         CURRFROM UNDER TEXT-TTL, '-', CURRTO.
SKIP 1.
ULINE.
SKIP 1.
IF P_MATL = 'X'.
    WRITE: /1 SORT_MATL_DESC1, 12 SORT_MATL_DESC2,
           35 TEXT-002,   60 TEXT-003,  80 TEXT-004,
           98 TEXT-008,  119 TEXT-009.
    WRITE: / TEXT-019 UNDER TEXT-002,
             TEXT-019 UNDER TEXT-003,
             TEXT-010 UNDER TEXT-008,
             TEXT-010 UNDER TEXT-009.
ELSE.
    IF P_DETAIL = 'X'.
       WRITE: /1 SORT_DESC1, 8 SORT_DESC2, 20 TEXT-001,
              35 TEXT-002,   60 TEXT-003,  80 TEXT-004,
              98 TEXT-008,  119 TEXT-009.
       WRITE: / TEXT-019 UNDER TEXT-002,
                TEXT-019 UNDER TEXT-003,
                TEXT-010 UNDER TEXT-008,
                TEXT-010 UNDER TEXT-009.
    ELSE.
       WRITE: /1 SORT_DESC1, 8 SORT_DESC2, 20 TEXT-001,
              35 TEXT-002,   60 TEXT-003,  80 TEXT-004.
       WRITE: / TEXT-019 UNDER TEXT-002,
                TEXT-019 UNDER TEXT-003.

    ENDIF.
ENDIF.
SKIP 1.
ULINE.
SKIP 1.
*ove sy-datum to calc_date1.
*ove 01 to calc_date1+6(2).
*ubtract 1 from calc_date1.
*alc_date2 = calc_date1 - 364.
*ove 01 to calc_date2+6(2).
*ove calc_date1+2(6) to display_date4.
*ove calc_date2+2(6) to display_date3.
*ubtract 1 from calc_date2.
*alc_date1 = calc_date2 - 365.
*ove 01 to calc_date1+6(2).
*ve calc_date1+2(6) to display_date1.
*ove calc_date2+2(6) to display_date2.
*rite: 43 'Compare Previous 12 Month Period: ', display_date1, ' to ',
*   display_date2, ' TO:'.
*rite:/43 'Completed Current 12 Month Period: ', display_date3, ' to ',
*   display_date4.
*f storloc-low > ' '.
*   skip 2.
*   write:/'Plant ', plant-low.
*   if plant-high > ' '.
*     write: ' - ', plant-high.
*   endif.
*   write:/'Storage Location ', storloc-low.
*   if storloc-high > ' '.
*      write: ' - ', storloc-high.
*   endif.
*lseif plant-low > ' '.
*   skip 3.
*   write:/'Plant ', plant-low.
*   if plant-high > ' '.
*     write: ' - ', plant-high.
*   endif.
*lse.
*  skip 3.
*  write:/'Cumulative'.
*ndif.
*kip 2.
*rite: 28 'Previous 12 Month Period', 88
*    'Completed Current 12 Month Period'.
*f matrgrp_ind = 'y'.
* skip 2.
* write: 'Material'.
* write:/'Group', 20 'Average Inventory', 40 'Total Usage', 55 'Turns',
*   80 'Average Inventory', 100 'Total Usage', 115 'Turns'.
*lse.
* skip 3.
* write:/'Material', 20 'Average Inventory', 40 'Total Usage',
*   55 'Turns', 80 'Average Inventory', 100 'Total Usage', 115 'Turns'.
*ndif.
*line.

FORM SET_SORT_KEY.
  IF P_SORT1 = 'X'.
     MOVE 'Plant' TO SORT_DESC1.
     MOVE 'Group' TO SORT_DESC2.
     LOOP AT WA.
       MOVE WA-WERKS TO WA-SORTKEY1.
       MOVE WA-MATKL TO WA-SORTKEY2.
       MOVE WA-MATNR TO WA-SORTKEY3.
       MODIFY WA.
     ENDLOOP.
  ENDIF.
  IF P_SORT2 = 'X'.
     MOVE 'Plant' TO SORT_DESC2.
     MOVE 'Group' TO SORT_DESC1.
     LOOP AT WA.
       MOVE WA-WERKS TO WA-SORTKEY2.
       MOVE WA-MATKL TO WA-SORTKEY1.
       MOVE WA-MATNR TO WA-SORTKEY3.
       MODIFY WA.
     ENDLOOP.
  ENDIF.
  IF P_MATL = 'X'.
     MOVE 'Material' TO SORT_MATL_DESC1.
     MOVE 'Plant' TO SORT_MATL_DESC2.
     LOOP AT WA.
       MOVE WA-MATNR TO WA-SORTKEY1.
       MOVE WA-WERKS TO WA-SORTKEY2.
       MODIFY WA.
     ENDLOOP.
  ENDIF.

ENDFORM.
