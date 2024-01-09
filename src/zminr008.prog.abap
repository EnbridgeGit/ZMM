REPORT zminr008 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65.
************************************************************************
*    Program     :  ZMINR008
*    PROGRAMMER  :  GUS SPARTALIS/OMNILOGIC SYSTEMS GROUP
*    Client      :  Centra Union Gas Limited
*    Date        :  August 29 1996
*
* This report will the information on surplus stock. Surplus stock being
* WHERE QUANTITY ON HAND IS GREATER THAN THE SAFETY STOCK PLUS THE
* re-order quantity.
************************************************************************
* 98/06/18 md7140 #538 Change SURPLUS calculation for VM materials
* 97/07/21 md7140 Fix Past 12 Months Consumption
* 97/06/04 md7140 Dev Req: DRMM0174  Transport: D30K904108
*                 Header, add MRP parameter, add max level column
*                 o/s reservation = all open reservations for plant
************************************************************************
TABLES  : mara, marc, mard, resb, makt, mbew, mver.
DATA    : qtyhand           TYPE i,
          temp-bdmng        LIKE resb-bdmng,
          safety-reorder    TYPE i,
          date_amount       TYPE i,
          total_enmng       LIKE resb-enmng,
          total_bdmng       LIKE resb-bdmng,
          difference        TYPE i,
          c_month           TYPE i,
          c_year            TYPE i,
          p_month           LIKE c_month,
          p_year            LIKE c_month,
          c_total           TYPE i,
          p_total           TYPE i,
          consumption       TYPE i,
          surplus_factor    TYPE p DECIMALS 1 VALUE '2.5',
          total_value       TYPE p DECIMALS 2.
DATA    : BEGIN OF table1 OCCURS 5000,
              matnr         LIKE mard-matnr,
              werks         LIKE mard-werks,
              lgort         LIKE mard-lgort,
              lvorm         LIKE mard-lvorm,
              dismm         LIKE marc-dismm,
              minbe         LIKE marc-minbe,
              matkl         LIKE mara-matkl,
              eisbe         LIKE marc-eisbe,
              bdmng         LIKE resb-bdmng,
              mabst         LIKE marc-mabst,
              t_consumption LIKE consumption,
              t_qtyhand     LIKE qtyhand,
          END OF table1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) text-001.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.
SELECT-OPTIONS:
     s_matgrp        FOR   mara-matkl OBLIGATORY,
     s_plant         FOR   marc-werks,
     s_storlo        FOR   mard-lgort OBLIGATORY,
     s_matnum        FOR   marc-matnr,
     s_dismm         FOR   marc-dismm.
SELECTION-SCREEN END OF BLOCK box1.

INCLUDE <icon>.
*******************************  MAIN  *********************************
TOP-OF-PAGE.
*********************** TOP-OF-PAGE ***********************************
TOP-OF-PAGE.
  WRITE: /1 text-rpt, sy-repid COLOR COL_NEGATIVE,      "Report Id
        110 text-ttl,                                   "Title
        220 text-dte, sy-datum,                         "Date
        text-amp, sy-uzeit.                             "Time

  WRITE: / text-cnt UNDER text-rpt,
           sy-mandt UNDER sy-repid, sy-sysid,
           text-pge UNDER text-dte, sy-pagno UNDER sy-datum.

  WRITE: /.
  ULINE.

  FORMAT COLOR COL_NORMAL.
  WRITE:  /1 text-004,                 "Location of Plant/Storage
          15 text-006,                 "Material Number
          30 text-032,                 "Material Description
          73 text-023,                 "Reorder Quantity
          89 text-021,                 "Safety Stock
         105 text-030,                 "Max Level
         121 text-026,                 "Outstanding Reservations
         140 text-018,                 "Total
         153 text-034,                 "Quantity on Hand
         169 text-025,                 "Surplus
         185 text-008,                 "Past 12 Mths Consumption
         201 text-028,                 "MRP Type
         207 text-016,                 "Average Unit Price
         223 text-033.                 "Total Value

  WRITE: / text-005 UNDER text-004, text-007 UNDER text-006,
           text-013 UNDER text-032, text-011 UNDER text-023,
           text-022 UNDER text-021, text-031 UNDER text-030,
           text-027 UNDER text-026, text-012 UNDER text-034,
           text-009 UNDER text-008, text-029 UNDER text-028,
           text-017 UNDER text-016, text-019 UNDER text-033.

  WRITE: / text-010 UNDER text-008, text-020 UNDER text-016.

  ULINE.
  WRITE: /.
************************************************************************
************************************************************************
START-OF-SELECTION.
  PERFORM determine_dates.                                  "md7140
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  CLEAR table1.
  REFRESH table1.

  SELECT * FROM mara WHERE matnr IN s_matnum
                       AND matkl IN s_matgrp
                       AND lvorm NE 'X'.
    IF sy-subrc EQ 0.
      SELECT * FROM marc WHERE matnr = mara-matnr
                         AND werks IN s_plant
                         AND dismm IN s_dismm               "md7140
                         AND lvorm NE 'X'.

        CLEAR temp-bdmng.                       "Sum reservations
        SELECT * FROM resb WHERE matnr = marc-matnr
                             AND werks = marc-werks
*                          and lgort = mard-lgort                "md7140
                             AND xloek NE 'X'
                             AND kzear NE 'X'.
*              temp-bdmng = resb-bdmng - resb-enmng.             "md7140
          temp-bdmng = temp-bdmng + resb-bdmng - resb-enmng.
        ENDSELECT.

        SELECT * FROM mard WHERE matnr = marc-matnr
                             AND werks = marc-werks
                             AND lgort IN s_storlo
                             AND lvorm NE 'X'.
          qtyhand = qtyhand + mard-labst + mard-umlme +
          mard-insme + mard-speme + mard-einme.

          CLEAR: c_total, p_total.                          "md7140
          SELECT * FROM mver WHERE matnr = mard-matnr
                               AND werks = mard-werks       "md7140
                               AND gjahr = c_year.
            PERFORM calc USING c_month CHANGING c_total.
          ENDSELECT.

          IF c_month NE 12.
            SELECT * FROM mver WHERE matnr = mard-matnr
                                 AND werks = mard-werks     "md7140
                                 AND gjahr = p_year.
              PERFORM calc2 USING p_month CHANGING p_total.
            ENDSELECT.
          ENDIF.

          consumption = c_total + p_total.
          MOVE-CORRESPONDING mard TO table1.
          MOVE marc-minbe TO table1-minbe.
          MOVE marc-eisbe TO table1-eisbe.
          MOVE marc-dismm TO table1-dismm.
          MOVE marc-mabst TO table1-mabst.
          MOVE mara-matkl TO table1-matkl.
          MOVE temp-bdmng TO table1-bdmng.
          MOVE consumption TO table1-t_consumption.
          MOVE qtyhand TO table1-t_qtyhand.
          APPEND table1.
          CLEAR table1.
          CLEAR qtyhand.
        ENDSELECT.
      ENDSELECT.
    ENDIF.
  ENDSELECT.

  SORT table1 BY matkl  ASCENDING
                 werks  ASCENDING
                 matnr  ASCENDING.

*LOOP AT TABLE1.
*WRITE: / TABLE1.
*ENDLOOP.
*STOP.

  LOOP AT table1.
    CLEAR:  difference, safety-reorder.
    IF  table1-dismm = 'VB' OR table1-dismm = 'V1'.         "MD7140
      safety-reorder = table1-mabst + table1-bdmng.
    ELSEIF table1-dismm = 'VM'.                             "#538
      safety-reorder = table1-minbe * surplus_factor.
    ELSE.
      safety-reorder = table1-minbe + table1-eisbe + table1-bdmng.
    ENDIF.
*write: / table1-matnr, table1-t_qtyhand, safety-reorder,
*         table1-minbe, table1-eisbe, table1-bdmng, table1-mabst,
*         table1-dismm.
    IF table1-t_qtyhand > safety-reorder.
      IF  table1-dismm = 'VB' OR table1-dismm = 'V1'.       "MD7140
        difference = table1-t_qtyhand - table1-mabst - table1-bdmng.
      ELSE.
        difference = table1-t_qtyhand - safety-reorder.
      ENDIF.

      SELECT SINGLE * FROM makt WHERE spras = sy-langu        "Desc
                                  AND matnr = table1-matnr.
      SELECT SINGLE * FROM mbew WHERE matnr = table1-matnr    "Price
                                  AND bwkey = table1-werks
                                  AND bwtar = space.
      total_value = mbew-verpr * table1-t_qtyhand.            "Total Value

      ON CHANGE OF table1-matkl.
        WRITE: /.
        WRITE: /.
        WRITE: / text-903, table1-matkl COLOR COL_KEY INVERSE ON.
        ULINE: /1(21).
      ENDON.

      ON CHANGE OF table1-werks.
        WRITE: /.
      ENDON.
      WRITE: / table1-werks UNDER text-004, text-900, table1-lgort,
               table1-matnr+12(6) UNDER text-006
                        COLOR COL_NEGATIVE INTENSIFIED ON, "Material
                makt-maktx   UNDER text-032,             "Desc
                table1-minbe UNDER text-023 DECIMALS 0,  "Reorder
                table1-eisbe UNDER text-021 DECIMALS 0,  "Safety St
                table1-mabst UNDER text-030 DECIMALS 0,  "Max Level
                table1-bdmng UNDER text-026 DECIMALS 0,  "O/S Resv
                safety-reorder UNDER text-018,           "Total
                table1-t_qtyhand COLOR COL_KEY INTENSIFIED ON
                               UNDER text-034,           "QOH
                difference     UNDER text-025,           "Surplus
                table1-t_consumption UNDER text-008,        "12 mths
                table1-dismm         UNDER text-028,     "MRP Type
                mbew-verpr COLOR COL_KEY INTENSIFIED ON
                                     UNDER text-016,     "Avg u prc
                total_value COLOR COL_KEY INTENSIFIED ON
                                     UNDER text-033.     "Total Value
*       write: 237 total_value color col_total.
*       write: 254 icon_calculation as icon.
*    EXIT.
    ENDIF.
  ENDLOOP.

*-----------------------------------------------------------------------
*       SUBROUTINE CALC
*-----------------------------------------------------------------------
*     - This routine calculates the CURRENT years portion of total
*       consumption.
*-----------------------------------------------------------------------
FORM calc USING month CHANGING total.

  CASE month.
    WHEN '1'.
      total = mver-mgv01.
    WHEN '2'.
      total = mver-mgv01 + mver-mgv02.
    WHEN '3'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03.
    WHEN '4'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04.
    WHEN '5'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04
              + mver-mgv05.
    WHEN '6'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04
            + mver-mgv05 + mver-mgv06.
    WHEN '7'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04
            + mver-mgv05 + mver-mgv06 + mver-mgv07.
    WHEN '8'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04
            + mver-mgv05 + mver-mgv06 + mver-mgv07 + mver-mgv08.
    WHEN '9'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04
            + mver-mgv05 + mver-mgv06 + mver-mgv07 + mver-mgv08
            + mver-mgv09.
    WHEN '10'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04
            + mver-mgv05 + mver-mgv06 + mver-mgv07 + mver-mgv08
            + mver-mgv09 + mver-mgv10.
    WHEN '11'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04
            + mver-mgv05 + mver-mgv06 + mver-mgv07 + mver-mgv08
            + mver-mgv09 + mver-mgv10 + mver-mgv11.
    WHEN '12'.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04
            + mver-mgv05 + mver-mgv06 + mver-mgv07 + mver-mgv08
            + mver-mgv09 + mver-mgv10 + mver-mgv11 + mver-mgv12.
    WHEN OTHERS.
      total = mver-mgv01 + mver-mgv02 + mver-mgv03 + mver-mgv04
            + mver-mgv05 + mver-mgv06 + mver-mgv07 + mver-mgv08
            + mver-mgv09 + mver-mgv10 + mver-mgv11 + mver-mgv12.
  ENDCASE.
ENDFORM.                    "CALC
*-----------------------------------------------------------------------
*       SUBROUTINE CALC2
*-----------------------------------------------------------------------
*     - This routine calculates the PREVIOUS years portion of total
*       consumption.
*-----------------------------------------------------------------------
FORM calc2 USING month CHANGING total.

  CASE month.
    WHEN '1'.
      total = 0.
    WHEN '2'.
      total = mver-mgv12 + mver-mgv11 + mver-mgv10 + mver-mgv09 +
              mver-mgv08 + mver-mgv07 + mver-mgv06 + mver-mgv05 +
              mver-mgv04 + mver-mgv03 + mver-mgv02.
    WHEN '3'.
      total = mver-mgv12 + mver-mgv11 + mver-mgv10 + mver-mgv09 +
              mver-mgv08 + mver-mgv07 + mver-mgv06 + mver-mgv05 +
              mver-mgv04 + mver-mgv03.
    WHEN '4'.
      total = mver-mgv12 + mver-mgv11 + mver-mgv10 + mver-mgv09 +
              mver-mgv08 + mver-mgv07 + mver-mgv06 + mver-mgv05 +
              mver-mgv04.
    WHEN '5'.
      total = mver-mgv12 + mver-mgv11 + mver-mgv10 + mver-mgv09 +
              mver-mgv08 + mver-mgv07 + mver-mgv06 + mver-mgv05.
    WHEN '6'.
      total = mver-mgv12 + mver-mgv11 + mver-mgv10 + mver-mgv09 +
              mver-mgv08 + mver-mgv07 + mver-mgv06.
    WHEN '7'.
      total = mver-mgv12 + mver-mgv11 + mver-mgv10 + mver-mgv09 +
              mver-mgv08 + mver-mgv07.
    WHEN '8'.
      total = mver-mgv12 + mver-mgv11 + mver-mgv10 + mver-mgv09 +
              mver-mgv08.
    WHEN '9'.
      total = mver-mgv12 + mver-mgv11 + mver-mgv10 + mver-mgv09.
    WHEN '10'.
      total = mver-mgv12 + mver-mgv11 + mver-mgv10.
    WHEN '11'.
      total = mver-mgv12 + mver-mgv11.
    WHEN '12'.
      total = mver-mgv12.
    WHEN OTHERS.
      total = 0.
  ENDCASE.
ENDFORM.                                                    "CALC2

*------------------------ DETERMINE_DATES -----------------------------*
* calculate beginning month/year and final month/year for LATEST 12    *
* MONTHS CONSUMPTION - must be 12 complete months                      *
*----------------------------------------------------------------------*
FORM determine_dates.
  c_month = sy-datum+4(2) - 1.          "require 12 complete months
  c_year  = sy-datum(4).
  IF c_month = 0.                       "if January, change to December
    c_month = 12.                      "of previous year.
    c_year = c_year - 1.
  ENDIF.

  p_month = sy-datum+4(2).
  IF c_month = 12.
    p_year = c_year.
  ELSE.
    p_year  = c_year - 1.
  ENDIF.
*  write: / 'sy-datum=', sy-datum.
*  write: / 'current= ', c_year, c_month.
*  write: / 'previous=', p_year, p_month.
ENDFORM.                    "DETERMINE_DATES
