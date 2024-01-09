REPORT zmmmr037. " NO STANDARD PAGE HEADING LINE-SIZE 255.
************************************************************************
* 14/10/2010 M Khan    WEST-TR865, EAST-TR789
* This Program has been copied from the East System.
*
* NOTE: If a change is required, Developer should make changes to both
* systems (EAST & WEST). I recommend to make changes in both systems
* instead of making change in one system, copy to other system and then
* changes for the differences between the two.
*-----------------------------------------------------------------------
* The differences between East and West programs are as follows:
* 1- MARD-LGORT = 'A001'.                   "EAST Main Storage Location
*    MARD-LGORT = '0001'.                   "WEST
* 2- MATNR (Material # is six digits)       "EAST Material #
*    MATNR (Material # is seven digits)     "WEST
* 3- RSSTA = 'M'                            "EAST Status of Reservation
*    RSSTA = ' '                            "WEST
* 4- POSTP <> 'N'                           "WEST Only: Item Category
*
*
************************************************************************
*  Author:      MaryLou DeMeester
*  Date:        April 1997
*  Description:
*     - The purpose of this program is to produce the
*       Plant Material/MRP Summary by Plant/Material Group
************************************************************************
* 2014/11/25 panusuri SDP75085 Include Plant-specific material status
*                     to the output of the report.
* 2014/01/24 panusuri SDP57347 Long text not displaying in Excel.
* 2013/10/23 gymana   Long text not displaying in excel when part
* SDP57437            numbers checkbox not checked. Corrected the code
*                     to not hide header fields.  Excel export FM
*                     expects the number of header fields to match the
*                     same number of fields in the data table.
* 2013/07/09 gymana   SDP51117 Report modifications
* 2012/08/30 M Khan   TR995 Change C: drive to H: drive with directory,
*                           file selection using F4 & move the
*                           hard-coded file path/name to variant.
* 2012/02/22 SAHMAD   QR101 add long description, cross plant status
* 2010/09/30 M Khan   TR789 Include Non-Zero QOH check as selection and
*                           in program logic.
* 2007/08/27 mdemeest TR217 - Add buyer as selection & in report.
* 2005/02/17 mdemeest 1069 Unit of Measure translation required for
*                          excel spreadsheet.
* 2004/11/29 mdemeest 1069 add unit of measure from ENT1027
* 2003/04/24 mdemeest ---- Added Rounding Value  marc-bstrf and changed
*                          to incorporate excel spreadsheet.  Deb Bossy
*                          wanted this as an integer even though
*                          decimals can be entered.  This was discussed
*                          with Deb and she is aware that any change to
*                          Rounding Value will be treated as the
*                          ultimate low in priorities.

* 98/06/17 mdemeest #532 Added YTD Consumption
* 98/04/15 raarssen - converted qoo from purchase uom to unit of issue
* 97/08/07 md7140 - fix qoo and qty reserved Dev Req DRMM0189
* 97/06/23 md7140 - fix consumption when < 12 mths history - M. Dufault
*                 - consump s/b mgvnn (Corrected) rather than gsvnn
*                 - Development Request DRMM0184
* 97/04/14 MD7140 - new request from D.Lambert/A.Malcolm
*
************************************************************************
TYPE-POOLS:  slis.

TABLES: ent1027, marc, mard, mbew, mver, resb, t001w, t023t,
                 mara, ekpo, ekbe, dbstaihdb4.              "SDP51117

DATA:
   BEGIN OF wa  OCCURS 0,
       matnr         LIKE ent1027-matnr,       "Material Number
       matkl         LIKE ent1027-matkl,       "Material Group
       maktx         LIKE ent1027-maktx,       "Description
   END OF wa.

DATA:
    BEGIN OF excltab OCCURS 10000,
       werks         LIKE marc-werks,           "Plant
       matkl         LIKE mara-matkl,           "Material Group
       ekgrp(5) TYPE c,        " like marc-ekgrp,           "Buyer
       matnr(6) TYPE c,                         "Mat.# East
*      matnr(7) type c,                         "Mat.# West
       maktx         LIKE makt-maktx,           "Description
       zmstae LIKE mara-mstae,     "Cross plant status
       mmsta         TYPE mmsta,               "(+)PANUSURI Ticket 75085
       labst(9)      TYPE c,                    "Quantity on Hand
       labsta001(8)  TYPE c,                    "Qty on Hand for A001
       verpr(11)      TYPE c,                    "Average Moving Price
       meins         LIKE ent1027-meins,        "Unit of measure
       plifz(8)      TYPE c,                    "Lead Time
       bstrf(8)      TYPE c,             "Rounding Value as per D. Bossy
       dismm         LIKE marc-dismm,           "MRP Type
       eisbe(8)      TYPE c,                    "Safety Level
       minbe(8)      TYPE c,                    "Reorder
       mabst(8)      TYPE c,                    "Maximum Stock Level
       reserv(8)     TYPE c,                    "Reserved Qty
       qoo(8)        TYPE c,                    "Quantity on Order
       lgpbe         LIKE mard-lgpbe,           "Bin Location for A001
       ytdcons(11)   TYPE c,                    "YTD Consumption
       consump(11)   TYPE c,                    "Last 12 months consump
       ovlcons(11)   TYPE c,                    "Over all consumption
       mfrpn1        LIKE mara-mfrpn,      "Manuf. Part No.1   SDP51117
       mfrpn2        LIKE mara-mfrpn,      "Manuf. Part No.2   SDP51117
       mfrpn3        LIKE mara-mfrpn,      "Manuf. Part No.3   SDP51117
       mfrpn4        LIKE mara-mfrpn,      "Manuf. Part No.4   SDP51117
       mfrpn5        LIKE mara-mfrpn,      "Manuf. Part No.5   SDP51117
       mfrpn6        LIKE mara-mfrpn,      "Manuf. Part No.6   SDP51117
       long_text(2000)  TYPE c,
*       long_text1 type table of CHAR61,
   END OF excltab.

DATA: excltab1 LIKE STANDARD TABLE OF excltab."(+)PANUSURI ticket 57437

DATA: BEGIN OF w_mfrpn OCCURS 0,                            "SDP51117
         mfrpn LIKE mard-matnr,                             "SDP51117
      END OF w_mfrpn.                                       "SDP51117

TYPES: BEGIN OF ty_wrd,
      long_text(65),
      END OF ty_wrd.

DATA: BEGIN OF w_mard OCCURS 1,                             "TR789
         matnr LIKE mard-matnr,
         werks LIKE mard-werks,
         labst LIKE mard-labst,
         lgort LIKE mard-lgort,
         lgpbe LIKE mard-lgpbe,
      END OF w_mard.

DATA: BEGIN OF w_mbew OCCURS 1,                             "TR789
         matnr LIKE mbew-matnr,
         bwkey LIKE mbew-bwkey,
         verpr LIKE mbew-verpr,
      END OF w_mbew.

*-----------------------------------------------------------------------
*  Temporary fields - controls size of fields in background report
*  so that the entire report can be seen.
*-----------------------------------------------------------------------
DATA:  plant_qoh        TYPE i,
       labsta001        TYPE i,
       plant_aup(11)    TYPE c,
       ytd_consump      TYPE i,
       ttl_consump      TYPE i,
       ttl_consump2     TYPE i,
       wrk_consump      TYPE i,
       ovl_consump      TYPE i,
       wa_temp          TYPE i,
       ttl_reserv       TYPE i,
       ttl_qoo          TYPE i,
       wa_lgpbe         LIKE mard-lgpbe,
       fperio(2)        TYPE n,
       tperio(2)        TYPE n,
       ov_text(25)      TYPE c,
       ov_text2(15)     TYPE c.

*BOI by PANUSURI ticket 57437
DATA:  fieldcat         TYPE slis_t_fieldcat_alv,
       fc_str           TYPE slis_fieldcat_alv.
DATA:  lt_fieldcat_exl  TYPE lvc_t_fcat,
       lwa_fieldcat_exl TYPE lvc_s_fcat.
*EOI by PANUSURI ticket 57437

*-----------------------------------------------------------------------
*  Date - begin and end periods
*-----------------------------------------------------------------------
DATA:  strind        TYPE i.                    "Starting point in table
DATA:  endind        TYPE i.                    "Ending point in table
DATA:  curyr         LIKE mver-gjahr.           "Current year
DATA:  prvyr         LIKE mver-gjahr.           "Previous year

*data:  begin of consump_table  occurs 0,
*       mgvnn         like mver-mgv01,
*       end of consump_table.

DATA:  w_option(12)  TYPE c VALUE 'START_EXCEL'.
DATA:  w_head01(132)  TYPE c.
DATA:  w_head02(120)  TYPE c.
DATA:  retcode       LIKE sy-subrc.
DATA:  w_repttl      LIKE sy-title.
DATA: dt_mt(6) TYPE c.

DATA:  BEGIN OF prot_header OCCURS 1,
        spaltenname(20)  TYPE c,
         ddic_table(5)    TYPE c,
         ddic_field(5)    TYPE c,
         key              TYPE c,
       END OF prot_header.

DATA:  errortab  LIKE hrerror  OCCURS 0 WITH HEADER LINE.

RANGES: year_range FOR mver-gjahr.

DATA: gt_sentence TYPE TABLE OF ty_wrd ,
      wa_word TYPE ty_wrd.
CONSTANTS : c_len TYPE i VALUE 65 .

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-000.
SELECT-OPTIONS:  swerks FOR marc-werks,            "Plant
                 slgort FOR mard-lgort,            "Storage Location
                 smatnr FOR marc-matnr,            "Material #
                 smatkl FOR ent1027-matkl,         "Material Group
                 sdismm FOR marc-dismm,            "Material type
                 sekgrp FOR marc-ekgrp,            "Buyer
                 peryer FOR dbstaihdb4-crttime.
"marc-awsls OBLIGATORY. "Period & Year
SELECTION-SCREEN SKIP 1.                                    "TR789
PARAMETERS: b_nzero AS CHECKBOX.         "Non Zero Stock Only TR789
PARAMETERS: b_ltext AS CHECKBOX.
PARAMETERS: b_mfrpn AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK box.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-034.
PARAMETERS:      p_rprt RADIOBUTTON GROUP rbcr,     "Print Report
                 p_excl RADIOBUTTON GROUP rbcr,     "Excel Spreadsheet
          p_file LIKE rlgrap-filename DEFAULT 'H:\SAPTEMP\MM-'. "TR995
SELECTION-SCREEN END OF BLOCK box1.


* End of selection screen
*--------------------------------------------------------------------
INITIALIZATION.                                           "TR995
  CONCATENATE 'H:\SAPTEMP\MM-' sy-datum '-' sy-uzeit INTO p_file.

******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  PERFORM check_file_path.
*End of TR995 changes
*---------------------------------------------------------------------
START-OF-SELECTION.
*Start of TR789 changes
  MOVE peryer-low(2)    TO fperio.
  MOVE peryer-high(2)   TO tperio.
  MOVE peryer-low+2(4)  TO year_range-low.
  MOVE peryer-high+2(4) TO year_range-high.
  MOVE 'I' TO year_range-sign.
  MOVE 'BT' TO year_range-option.
  APPEND year_range.

  CONCATENATE text-039 peryer-low(2) '/' peryer-low+2(4) INTO ov_text.
  CONCATENATE ov_text 'to' INTO ov_text SEPARATED BY space.
  CONCATENATE ov_text peryer-high(2) '/' peryer-high+2(4) INTO ov_text.

  CONCATENATE 'C.' peryer-low(2)  peryer-low+4(2) '-'
                   peryer-high(2) peryer-high+4(2) INTO ov_text2.
*End of TR789 changes

* set indices for selection of consumption from mver table
* current & previous year moved to consumption table
* set indices to take required 12 month period
  curyr = sy-datum(4).
  strind = sy-datum+4(2).
  endind = sy-datum+4(2) - 1.
*  if  sy-datum+4(2) = '01'.           "if january, adjust current year
*      curyr = sy-datum(4) - 1.
*  endif.
  prvyr = sy-datum(4) - 1.

  PERFORM select_mard_data.                                 "TR789
  PERFORM select_mbew_data.                                 "TR789

  SELECT * FROM marc
     WHERE matnr IN smatnr                       "Material #
       AND werks IN swerks                       "Plantmatnr in smatnr
       AND dismm IN sdismm                       "Material Type
       AND ekgrp IN sekgrp.                      "Buyer

    CLEAR wa_lgpbe.     "Clears temp. storage location for new material

    SELECT * FROM ent1027
       WHERE matnr = marc-matnr
         AND matkl IN smatkl.                  "Material Groups
      CLEAR labsta001.
      PERFORM calculate_plant_qoh.
      PERFORM determine_avg_unit_price.
      PERFORM calculate_consumption.
      PERFORM determine_reservations.
      PERFORM determine_qty_on_order.
      PERFORM build_excltab.
    ENDSELECT.

  ENDSELECT.

  IF sy-subrc EQ 0.
    IF excltab[] IS INITIAL.                                "TR789
      CALL FUNCTION 'C14A_POPUP_NO_HITS'.                   "TR789
      STOP.                                                 "TR789
    ENDIF.                                                  "TR789
    SORT excltab BY werks matkl matnr.

*    IF p_rprt = 'X'.   "(-)PANUSURI ticket 57437
    PERFORM output_alv.
*    ELSE.              "(-)PANUSURI ticket 57437
    PERFORM create_output_report.
*    ENDIF.             "(-)PANUSURI ticket 57437
  ENDIF.


*-----------------------------------------------------------------------

************************************************************************
*                     Subroutines in main program                      *
************************************************************************
*-----------------------------------------------------------------------
*     FORM BUILD_excltab
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine moves all data to excltab.
*-----------------------------------------------------------------------

FORM build_excltab.
  DATA: lt_lines TYPE TABLE OF tline,
        ls_lines LIKE LINE OF lt_lines,
        lv_object TYPE thead-tdobject VALUE 'MATERIAL',
        lv_name TYPE thead-tdname,
        lv_id TYPE thead-tdid VALUE 'GRUN',
        lv_cnt TYPE i.

  IF b_nzero = 'X' AND plant_qoh = 0.                       "TR789
  ELSE.                                                     "TR789

    CLEAR excltab.
    MOVE marc-werks     TO excltab-werks.
    MOVE marc-ekgrp     TO excltab-ekgrp.
    MOVE marc-matnr+12(6) TO excltab-matnr.          "East Material #
*     move marc-matnr+11(7) to excltab-matnr.          "West Material #
    MOVE marc-dismm     TO excltab-dismm.            "MRP Type

    MOVE marc-eisbe     TO wa_temp.                   "Safety Stock
    MOVE wa_temp        TO excltab-eisbe.

    MOVE marc-mabst     TO wa_temp.                  "Maximum
    MOVE wa_temp        TO excltab-mabst.

    MOVE marc-plifz     TO wa_temp.                  "Lead time
    MOVE wa_temp        TO excltab-plifz.

    MOVE marc-bstrf     TO wa_temp.                  "Rounding Value
    MOVE wa_temp        TO excltab-bstrf.

    MOVE marc-minbe     TO wa_temp.                  "Reorder
    MOVE wa_temp        TO excltab-minbe.

    MOVE marc-mmsta     TO excltab-mmsta.      "(+)PANUSURI Ticket 75085

    MOVE ent1027-matkl  TO excltab-matkl.            "Material Group
    MOVE ent1027-maktx  TO excltab-maktx.            "Description
    MOVE ent1027-mstae  TO excltab-zmstae.         "Cross Plant Status
    IF p_rprt = 'X'.
      MOVE ent1027-meins  TO excltab-meins.         "Unit of Measure
    ELSE.
      WRITE ent1027-meins TO excltab-meins.         "Unit of Measure
    ENDIF.
    WRITE plant_qoh     TO excltab-labst.            "Qty on Hand
    MOVE labsta001      TO excltab-labsta001.        "QOH for A001
    MOVE plant_aup      TO excltab-verpr.            "Avg moving pr
    MOVE ttl_consump    TO excltab-consump.          "Consumptions
    MOVE ytd_consump    TO excltab-ytdcons.          "YTD Consumptn
    MOVE ovl_consump    TO excltab-ovlcons.          "Over all Consmp
    MOVE ttl_reserv     TO excltab-reserv.           "Reservations
    MOVE ttl_qoo        TO excltab-qoo.              "QOO
    MOVE wa_lgpbe       TO excltab-lgpbe.            "Bin location
    IF b_mfrpn = 'X'.                                       "SDP51117
      PERFORM load_manufacturer_partno.                     "SDP51117
    ENDIF.                                                  "SDP51117
    IF b_ltext = 'X'.
      CLEAR: lt_lines,
             ls_lines,
             excltab-long_text.
      lv_name = ent1027-matnr.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*            CLIENT                        = SY-MANDT
          id                            = lv_id
          language                      = sy-langu
          name                          = lv_name
          object                        = lv_object
*            ARCHIVE_HANDLE                = 0
*            LOCAL_CAT                     = ' '
*          IMPORTING
*            HEADER                        =
        TABLES
          lines                         = lt_lines
       EXCEPTIONS
         id                            = 1
         language                      = 2
         name                          = 3
         not_found                     = 4
         object                        = 5
         reference_check               = 6
         wrong_access_to_archive       = 7
         OTHERS                        = 8.
      IF sy-subrc <> 0.

      ENDIF.
      IF lt_lines IS NOT INITIAL.
        lv_cnt = 1.
        LOOP AT lt_lines INTO ls_lines.
          CONCATENATE excltab-long_text ls_lines-tdline
                      INTO excltab-long_text SEPARATED BY space.
*          lv_cnt = lv_cnt + 1.
*          IF lv_cnt > 4.
*            EXIT.
*          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    APPEND excltab.
  ENDIF.                                                    "TR789
ENDFORM.                    "BUILD_excltab

*&---------------------------------------------------------------------*
*&      Form  SELECT_MARD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_mard_data.                                      "TR789

  SELECT matnr werks labst lgort lgpbe
    FROM mard
    INTO TABLE w_mard.

  DELETE w_mard WHERE NOT matnr IN smatnr.
  DELETE w_mard WHERE NOT werks IN swerks.
  DELETE w_mard WHERE NOT lgort IN slgort.
  SORT w_mard BY matnr werks.
ENDFORM.                    "SELECT_MARD_DATA
*-----------------------------------------------------------------------
*     FORM CALCULATE_PLANT_QOH
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine calculates the qoh for a plant by summing
*     qoh from individual storage locations within the plant
*-----------------------------------------------------------------------
FORM calculate_plant_qoh.
  CLEAR plant_qoh.
*    READ TABLE W_MARD WITH KEY MATNR = MARC-MATNR
*                               WERKS = MARC-WERKS BINARY SEARCH.
  LOOP AT w_mard WHERE matnr = marc-matnr AND
                       werks = marc-werks.
    ADD w_mard-labst TO plant_qoh.                "Total Plant qoh
    IF  w_mard-lgort = 'A001'.               "East Storage Location
*       IF  W_MARD-LGORT = '0001'.               "West Storage Location
      MOVE w_mard-labst TO labsta001.
      MOVE w_mard-lgpbe TO wa_lgpbe.       "Bin Location
    ENDIF.
  ENDLOOP.
*-----------------
*    SELECT * FROM MARD
*        WHERE MATNR = MARC-MATNR
*          AND WERKS = MARC-WERKS
*          AND LGORT IN SLGORT.
*        ADD MARD-LABST TO PLANT_QOH.                "Total Plant qoh
**       IF  MARD-LGORT = 'A001'.               "East Storage Location
*        IF  MARD-LGORT = '0001'.               "West Storage Location
*            MOVE MARD-LABST TO LABSTA001.
*            move mard-lgpbe to wa_lgpbe.            "Bin Location
*        ENDIF.
*    ENDSELECT.
ENDFORM.                    "CALCULATE_PLANT_QOH

*&---------------------------------------------------------------------*
*&      Form  SELECT_MBEW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_mbew_data.
  SELECT matnr bwkey verpr
    FROM mbew
   INTO TABLE w_mbew
  WHERE matnr IN smatnr
    AND bwkey IN swerks.

  SORT w_mbew BY matnr bwkey.
ENDFORM.                    "SELECT_MBEW_DATA
*-----------------------------------------------------------------------
*     FORM DETERMINE_AVG_UNIT_PRICE
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine finds the average moving price for material
*     within a plant.
*-----------------------------------------------------------------------
FORM determine_avg_unit_price.
  CLEAR plant_aup.

  READ TABLE w_mbew WITH KEY matnr = marc-matnr
                          bwkey = marc-werks BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE w_mbew-verpr     TO plant_aup.
  ENDIF.

*    SELECT * FROM MBEW
*        WHERE MATNR = MARC-MATNR
*          AND BWKEY = MARC-WERKS.
*        MOVE MBEW-VERPR     TO PLANT_AUP.
**     move '120000.69' to plant_aup.  "Tests the size of avg unitprice
*    ENDSELECT.
ENDFORM.                    "DETERMINE_AVG_UNIT_PRICE
*-----------------------------------------------------------------------
*     FORM PRINT_HEADINGS
*-----------------------------------------------------------------------
*   Description:
*   - This subroutine prints column headings.
*-----------------------------------------------------------------------
FORM print_headings.
  WRITE: /55 text-022, 65 text-013, 90 text-033, 100 text-032.
  WRITE: / text-009,  10 text-010,  55 text-011,  65 text-011,
        75 text-012,  90 text-014, 100 text-014, 110 text-015,
       115 text-016,
       120 text-017, 130 text-018, 140 text-019, 150 text-020,
       160 text-021.
  ULINE.
ENDFORM.                    "PRINT_HEADINGS

*&---------------------------------------------------------------------*
*&      Form  PLANT_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plant_title.
  SELECT SINGLE * FROM t001w
       WHERE werks = excltab-werks.
  WRITE: /1 text-006,
       excltab-werks COLOR COL_GROUP INTENSIFIED OFF INVERSE ON,
       text-008, t001w-name1.
ENDFORM.                    "PLANT_TITLE

*&---------------------------------------------------------------------*
*&      Form  MATERIAL_GROUP_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM material_group_title.
  SELECT SINGLE * FROM t023t
      WHERE spras = sy-langu
        AND matkl = excltab-matkl.

  WRITE: /1 text-007,
        excltab-matkl(4) COLOR COL_GROUP INTENSIFIED OFF INVERSE ON,
        text-008, t023t-wgbez.
  WRITE: /.
ENDFORM.                    "MATERIAL_GROUP_TITLE

*&---------------------------------------------------------------------*
*&      Form  CALCULATE_CONSUMPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM calculate_consumption.
  CLEAR: ttl_consump, ttl_consump2, ytd_consump, ovl_consump.
* calculate consumption from previous year
  SELECT * FROM mver
     WHERE matnr = marc-matnr
       AND werks = marc-werks
       AND gjahr = prvyr.
    IF sy-datum+4(2) = '01'.
      ADD mver-mgv01 FROM 1 TO 12 GIVING ttl_consump.
    ELSE.
      ADD mver-mgv01 FROM strind TO 12 GIVING ttl_consump.
    ENDIF.
  ENDSELECT.                            "END OF MVER FROM PREVIOUS YEAR

* calculate consumption from current year
  SELECT * FROM mver
     WHERE matnr = marc-matnr
       AND werks = marc-werks
       AND gjahr = curyr.
    IF sy-datum+4(2) = '01'.
    ELSE.
      ADD mver-mgv01 FROM 1 TO endind GIVING ttl_consump2.
      ADD ttl_consump2 TO ttl_consump.
    ENDIF.

    ADD mver-mgv01 FROM 1 TO sy-datum+4(2) GIVING ytd_consump.
  ENDSELECT.                            "END OF MVER FROM CURRENT YEAR

* calculate consumption from/to period and year.

  SELECT * FROM mver
      WHERE matnr = marc-matnr
        AND werks = marc-werks
        AND gjahr IN year_range.

    IF mver-gjahr = year_range-low.
      ADD mver-mgv01 FROM fperio TO 12 GIVING wrk_consump.
      ADD wrk_consump TO ovl_consump.
    ELSEIF mver-gjahr = year_range-high.
      ADD mver-mgv01 FROM 1 TO tperio  GIVING wrk_consump.
      ADD wrk_consump TO ovl_consump.
    ELSE.
      ADD mver-mgv01 FROM 1 TO 12      GIVING wrk_consump.
      ADD wrk_consump TO ovl_consump.
    ENDIF.
  ENDSELECT.
ENDFORM.                    "CALCULATE_CONSUMPTION
***********************************************************************
FORM determine_reservations.
  CLEAR ttl_reserv.
  SELECT * FROM resb
     WHERE matnr = marc-matnr
       AND werks = marc-werks
*        AND RSSTA = ' '                     "For West
       AND rssta = 'M'                     "For East
       AND xloek = space                        "Item has been deleted
       AND kzear = space.                       "Item is final issued
*        AND POSTP <> 'N'.                   "For West Only
*        and xloek <> 'X'                         "Item has been deleted
*        and kzear <> 'X'.                        "Item is final issued
    IF  resb-shkzg = 'H'.
      ttl_reserv = ttl_reserv + resb-bdmng.
    ELSE.
      ttl_reserv = ttl_reserv - resb-bdmng.
    ENDIF.
  ENDSELECT.
ENDFORM.                    "DETERMINE_RESERVATIONS

*-----------------------------------------------------------------------
*     FORM DETERMINE_QTY_ON_ORDER
*-----------------------------------------------------------------------
*   - This subroutine calculates the quantity outstanding for a
*     material on a PO
*-----------------------------------------------------------------------
FORM determine_qty_on_order.
  CLEAR ttl_qoo.
  SELECT * FROM ekpo
      WHERE matnr = marc-matnr
        AND werks = marc-werks
        AND loekz <> 'L'
        AND elikz <> 'X'.
    ttl_qoo = ttl_qoo + ekpo-menge.
    SELECT * FROM ekbe
        WHERE ebeln = ekpo-ebeln
          AND ebelp = ekpo-ebelp
          AND bewtp = 'E'.
      IF ekbe-shkzg = 'H'.                                "Credit
        ttl_qoo = ttl_qoo + ekbe-menge.
      ELSE.                                               "Debit
        ttl_qoo = ttl_qoo - ekbe-menge.
      ENDIF.
    ENDSELECT.
  ENDSELECT.
  ttl_qoo = ( ttl_qoo * ekpo-umrez ) / ekpo-umren.   "1998/04/15  ra
ENDFORM.                    "DETERMINE_QTY_ON_ORDER

*-----------------------------------------------------------------------
*     FORM LOAD_MANUFACTURER_PARTNO
*-----------------------------------------------------------------------
*   - This subroutine loads the manufacturers part numbers (up to 6)
*     for a material number
*     2013/07/09 gymana SDP51117
*-----------------------------------------------------------------------
FORM load_manufacturer_partno.

  SELECT mfrpn FROM mara
       INTO CORRESPONDING FIELDS OF TABLE w_mfrpn
      WHERE bmatn = marc-matnr
        AND mtart = 'HERS'.

  IF sy-subrc = 0.
    LOOP AT w_mfrpn.
      IF sy-tabix = 1.
        MOVE w_mfrpn-mfrpn TO excltab-mfrpn1.
      ELSEIF sy-tabix = 2.
        MOVE w_mfrpn-mfrpn TO excltab-mfrpn2.
      ELSEIF sy-tabix = 3.
        MOVE w_mfrpn-mfrpn TO excltab-mfrpn3.
      ELSEIF sy-tabix = 4.
        MOVE w_mfrpn-mfrpn TO excltab-mfrpn4.
      ELSEIF sy-tabix = 5.
        MOVE w_mfrpn-mfrpn TO excltab-mfrpn5.
      ELSEIF sy-tabix = 6.
        MOVE w_mfrpn-mfrpn TO excltab-mfrpn6.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "LOAD_MANUFACTURER_PARTNO


*&---------------------------------------------------------------------*
*&      Form  create_output_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_output_report.
*BOC by PANUSURI ticket 57437
*  DATA: lv_filename TYPE rlgrap-filename.
*
*  MOVE sy-repid  TO w_repttl.
*  MOVE '-'       TO w_repttl+10(1).
*  MOVE text-005  TO w_repttl+12(30).
*
**  can enter info for W_HEAD01.
*  IF slgort = ' '.
*    CONCATENATE text-023 'ALL' INTO w_head01.
*  ELSE.
*    MOVE text-023 TO w_head01.
*    LOOP AT slgort.
*      IF slgort+1(2) = 'EQ'.
*        CONCATENATE w_head01 slgort+3(4) INTO w_head01 SEPARATED BY
*  ';'.
*      ELSEIF slgort+1(2) = 'BT'.
*        CONCATENATE w_head01 slgort+3(4) INTO w_head01 SEPARATED BY
*';'.
*        CONCATENATE w_head01 slgort+7(4) INTO w_head01 SEPARATED BY
*  '-'.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*  IF w_head01+28(1) = ';'.
*    MOVE ' ' TO w_head01+28(1).
*  ENDIF.
*
*  MOVE text-002  TO w_head02.
*  WRITE sy-datum TO w_head02+6(10).
*  MOVE text-004  TO w_head02+17(8).
*  MOVE sy-mandt  TO w_head02+27(3).
*  MOVE sy-sysid  TO w_head02+32(3).
*
*  PERFORM prot_header.
*  IF  p_rprt = 'X'.
*    CLEAR w_option.
*    IF sy-batch = 'X'.
*      w_option = 'LINESELMOD:1'.
*    ENDIF.
*  ENDIF.
*EOC by PANUSURI ticket 57437

*  call function 'HR_DISPLAY_BASIC_LIST'
*    exporting
*      basic_list_title           = w_repttl
*      file_name                  = sy-cprog
*      head_line1                 = w_head01
*      head_line2                 = w_head02
*      additional_options         = w_option
*    importing
*      return_code                = retcode
*    tables
*      data_tab                   = excltab
*      fieldname_tab              = prot_header
*      errortab                   = errortab
*    exceptions
*      download_problem           = 1
*      no_data_tab                = 2
*      table_mismatch             = 3
*      print_problems             = 4
*      others                     = 5.

*TR995
* CONCATENATE 'C:\SAPTEMP\MM-' sy-datum '-' sy-uzeit INTO lv_filename.

*BOC by PANUSURI ticket 57437
*  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
*    EXPORTING
*      file_name                 = p_file "lv_filename "'C:\SAPTEMP\MM'
*      create_pivot              = 0
*    TABLES
*      data_tab                  = excltab
*      fieldnames                = prot_header
*    EXCEPTIONS
*      file_not_exist            = 1
*      filename_expected         = 2
*      communication_error       = 3
*      ole_ojbect_method_error   = 4
*      ole_object_property_block = 5
*      invalid_filename          = 6
*      invalid_pivot_fields      = 7
*      download_problem          = 8
*      OTHERS                    = 9.
*
*  IF sy-subrc <> 0.
*    WRITE: /1 'table download unsuccessful - reason = ', sy-subrc.
*  ENDIF.
*EOC by PANUSURI ticket 57437

*BOI by PANUSURI ticket 57437
  DATA: lv_len TYPE i,
        lv_extn TYPE char5.

  IF p_excl = 'X'.
*   Check for File extensions - XLS/XLSX
    lv_len = strlen( p_file ).

    IF lv_len GT 5.
      lv_len = lv_len - 5.
      lv_extn = p_file+lv_len(5).
      TRANSLATE lv_extn TO UPPER CASE.
      IF lv_extn NE '.XLSX'.
        lv_extn = '.XLSX'.
      ELSE.
        CLEAR: lv_extn.
      ENDIF.
    ELSE.
      lv_extn = '.XLSX'.
    ENDIF.

    IF lv_extn NE space.
      CONCATENATE p_file lv_extn INTO p_file.
    ENDIF.

*   Fill data table for Excel
    excltab1[] = excltab[].
*   Build Fieldcatalog for Excel
    LOOP AT fieldcat INTO fc_str.
      lwa_fieldcat_exl-fieldname = fc_str-fieldname.
      lwa_fieldcat_exl-seltext = fc_str-seltext_l.
      lwa_fieldcat_exl-no_out = fc_str-no_out.
      APPEND lwa_fieldcat_exl TO lt_fieldcat_exl.
      CLEAR lwa_fieldcat_exl.
    ENDLOOP.

    CALL METHOD zcl_utilities=>create_xls_from_itab
      EXPORTING
        i_filename  = p_file
        it_fieldcat = lt_fieldcat_exl[]
        i_filetype  = if_salv_bs_xml=>c_type_xlsx
      CHANGING
        ct_data     = excltab1.
  ENDIF.
*EOI by PANUSURI ticket 57437

ENDFORM.                    "create_output_report

*&---------------------------------------------------------------------*
*&      Form  prot_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prot_header.

  MOVE text-006  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-007  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-037 TO  prot_header-spaltenname.        "Buyer
  APPEND prot_header.
  MOVE text-009  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-010  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-040  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-011  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-013  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-012  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-036  TO prot_header-spaltenname.
  APPEND prot_header.
*  move text-039  to prot_header-spaltenname.

  MOVE text-015  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-033  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-016  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-017  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-018  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-019  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-020  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-021  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-035  TO prot_header-spaltenname.
  APPEND prot_header.
******
  MOVE text-014  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE text-032  TO prot_header-spaltenname.
  APPEND prot_header.
  MOVE ov_text2  TO prot_header-spaltenname.
  APPEND prot_header.
  IF b_mfrpn = 'X'.                                         "SDP57437
    MOVE text-042  TO prot_header-spaltenname.              "SDP51117
    APPEND prot_header.                                     "SDP51117
    MOVE text-043  TO prot_header-spaltenname.              "SDP51117
    APPEND prot_header.                                     "SDP51117
    MOVE text-044  TO prot_header-spaltenname.              "SDP51117
    APPEND prot_header.                                     "SDP51117
    MOVE text-045  TO prot_header-spaltenname.              "SDP51117
    APPEND prot_header.                                     "SDP51117
    MOVE text-046  TO prot_header-spaltenname.              "SDP51117
    APPEND prot_header.                                     "SDP51117
    MOVE text-047  TO prot_header-spaltenname.              "SDP51117
    APPEND prot_header.                                     "SDP51117
  ENDIF.                                                    "SDP57437
  IF b_ltext = 'X'.                                         "SDP57437
    MOVE text-041  TO prot_header-spaltenname.
    APPEND prot_header.
  ENDIF.                                                    "SDP57437
ENDFORM.                    "prot_header


*&---------------------------------------------------------------------*
*&      Form  output_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_alv.

*  DATA:  fieldcat TYPE slis_t_fieldcat_alv, "(-)PANUSURI ticket 57437
*         fc_str   TYPE slis_fieldcat_alv,   "(-)PANUSURI ticket 57437
  DATA: layout   TYPE slis_layout_alv,
          title    TYPE lvc_title,
*       repid    like sy-repid,
          variant  LIKE disvariant,
          sort     TYPE slis_t_sortinfo_alv,
          sort_str TYPE slis_sortinfo_alv,
          lv_tabix TYPE sy-tabix,
          lt_evt TYPE slis_t_event,
          wa_evt LIKE LINE OF lt_evt.

  layout-colwidth_optimize = 'X'.                           "TR789
  layout-zebra = 'X'.                                       "TR789

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'EXCLTAB'
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
*-----------------------------------------------------------------------
*  List each field from EXCLTAB to put in titles
*-----------------------------------------------------------------------
  LOOP AT fieldcat INTO fc_str.
    CASE fc_str-fieldname.
      WHEN 'WERKS'.
        fc_str-seltext_l = text-006.
        fc_str-ddictxt = 'L'.
        fc_str-key    = ' '.          " TR789 Key column -not first
      WHEN 'MATKL'.
        fc_str-seltext_l = text-007.
        fc_str-ddictxt = 'L'.
      WHEN 'EKGRP'.
        fc_str-seltext_l = text-037.
        fc_str-ddictxt = 'L'.
      WHEN 'MATNR'.
        fc_str-seltext_l = text-009.
        fc_str-ddictxt = 'L'.
      WHEN 'MATKX'.
        fc_str-seltext_l = text-010.
        fc_str-ddictxt = 'L'.
*BOI by PANUSURI Ticket 75085
      WHEN 'MMSTA'.
        fc_str-seltext_l = text-038.
        fc_str-ddictxt = 'L'.
*EOI by PANUSURI Ticket 75085
      WHEN 'LABST'.
        fc_str-seltext_l = text-011.
        fc_str-ddictxt = 'L'.
      WHEN 'LABSTA001'.
        fc_str-seltext_l = text-013.
        fc_str-ddictxt = 'L'.
      WHEN 'VERPR'.
        fc_str-seltext_l = text-012.
        fc_str-ddictxt = 'L'.
      WHEN 'MEINS'.
        fc_str-seltext_l = text-036.
        fc_str-ddictxt = 'L'.
      WHEN 'YTDCONS'.                                  "YTD consump
        fc_str-seltext_l = text-014.
        fc_str-ddictxt = 'L'.
      WHEN 'CONSUMP'.                                  "Last 12 months
        fc_str-seltext_l = text-032.
        fc_str-ddictxt = 'L'.
      WHEN 'OVLCONS'.                                  "Last 12 months
        IF p_rprt = 'X'.  "(+)PANUSURI ticket 57437
          fc_str-seltext_l = ov_text.
*BOI by PANUSURI ticket 57437
        ELSE.
          fc_str-seltext_l = ov_text2.
        ENDIF.
*EOI by PANUSURI ticket 57437
        fc_str-ddictxt = 'L'.
      WHEN 'PLIFZ'.                                    "Lead time
        fc_str-seltext_l = text-015.
        fc_str-ddictxt = 'L'.
      WHEN 'BSTRF'.                                    "Rounding Value
        fc_str-seltext_l = text-033.
        fc_str-ddictxt = 'L'.
      WHEN 'DISMM'.                                    "MRP type
        fc_str-seltext_l = text-016.
        fc_str-ddictxt = 'L'.
      WHEN 'EISBE'.                                    "Safety level
        fc_str-seltext_l = text-017.
        fc_str-ddictxt = 'L'.
      WHEN 'MINBE'.                                    "Reorder
        fc_str-seltext_l = text-018.
        fc_str-ddictxt = 'L'.
      WHEN 'MABST'.                                    "Max Stock Level
        fc_str-seltext_l = text-019.
        fc_str-ddictxt = 'L'.
      WHEN 'RESERV'.                                   "Reserved qty
        fc_str-seltext_l = text-020.
        fc_str-ddictxt = 'L'.
      WHEN 'QOO'.                                      "Qty on Order
        fc_str-seltext_l = text-021.
        fc_str-ddictxt = 'L'.
      WHEN 'LGPBE'.                                   "Bin Loc for A001
        fc_str-seltext_l = text-035.
        fc_str-ddictxt = 'L'.
      WHEN 'ZMSTAE'.
        fc_str-seltext_l = text-040.
        fc_str-ddictxt = 'L'.
      WHEN 'MFRPN1'.
        IF b_mfrpn = 'X'.
          fc_str-seltext_l = text-042.
          fc_str-ddictxt = 'L'.
          fc_str-hotspot = 'X'.
        ELSE.
          fc_str-no_out = 'X'.          " Hide Columns
          fc_str-key    = ' '.          " Key columns-not first
        ENDIF.
      WHEN 'MFRPN2'.
        IF b_mfrpn = 'X'.
          fc_str-seltext_l = text-043.
          fc_str-ddictxt = 'L'.
          fc_str-hotspot = 'X'.
        ELSE.
          fc_str-no_out = 'X'.          " Hide Columns
          fc_str-key    = ' '.          " Key columns-not first
        ENDIF.
      WHEN 'MFRPN3'.
        IF b_mfrpn = 'X'.
          fc_str-seltext_l = text-044.
          fc_str-ddictxt = 'L'.
          fc_str-hotspot = 'X'.
        ELSE.
          fc_str-no_out = 'X'.          " Hide Columns
          fc_str-key    = ' '.          " Key columns-not first
        ENDIF.
      WHEN 'MFRPN4'.
        IF b_mfrpn = 'X'.
          fc_str-seltext_l = text-045.
          fc_str-ddictxt = 'L'.
          fc_str-hotspot = 'X'.
        ELSE.
          fc_str-no_out = 'X'.          " Hide Columns
          fc_str-key    = ' '.          " Key columns-not first
        ENDIF.
      WHEN 'MFRPN5'.
        IF b_mfrpn = 'X'.
          fc_str-seltext_l = text-046.
          fc_str-ddictxt = 'L'.
          fc_str-hotspot = 'X'.
        ELSE.
          fc_str-no_out = 'X'.          " Hide Columns
          fc_str-key    = ' '.          " Key columns-not first
        ENDIF.
      WHEN 'MFRPN6'.
        IF b_mfrpn = 'X'.
          fc_str-seltext_l = text-047.
          fc_str-ddictxt = 'L'.
          fc_str-hotspot = 'X'.
        ELSE.
          fc_str-no_out = 'X'.          " Hide Columns
          fc_str-key    = ' '.          " Key columns-not first
        ENDIF.
      WHEN 'LONG_TEXT'.
        IF b_ltext = 'X'.
          fc_str-seltext_l = text-041.
          fc_str-ddictxt = 'L'.
          fc_str-hotspot = 'X'.
        ELSE.
          fc_str-no_out = 'X'.          " Hide Columns
          fc_str-key    = ' '.          " Key columns-not first
        ENDIF.
      WHEN 'OTHERS'.
    ENDCASE.
    MODIFY fieldcat FROM fc_str.
  ENDLOOP.
**************************************
* Word Wrap the text in multiple lines
*loop at excltab.
*  LV_TABIX = SY-TABIX .
*  CLEAR : GT_SENTENCE[].
*
*  CALL FUNCTION 'RKD_WORD_WRAP'
*   EXPORTING
*     TEXTLINE                  = excltab-long_text
*     OUTPUTLEN                 = C_LEN
*   TABLES
*     OUT_LINES                 = GT_SENTENCE
*   EXCEPTIONS
*     OUTPUTLEN_TOO_LARGE       = 1
*     OTHERS                    = 2
*            .
*  IF SY-SUBRC eq 0.
*    IF NOT GT_SENTENCE IS INITIAL .
*      READ TABLE GT_SENTENCE INTO WA_WORD INDEX 1 .
*        excltab-long_text = WA_WORD-long_text.
*        MODIFY excltab INDEX LV_TABIX .
*    ENDIF.
*  ENDIF.
*ENDLOOP.

  IF p_rprt = 'X'.  "(+)PANUSURI ticket 57437
* Get event.. we will handle BEFORE and AFTER line output
    CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
      IMPORTING
        et_events = lt_evt.
*
    READ TABLE lt_evt INTO wa_evt
          WITH KEY name = slis_ev_user_command.
    "slis_ev_user_command.
    "SLIS_EV_AFTER_LINE_OUTPUT .
    wa_evt-form = slis_ev_user_command.
    "slis_ev_user_command.
    "SLIS_EV_AFTER_LINE_OUTPUT .
    MODIFY lt_evt FROM wa_evt INDEX sy-tabix .

*  Display ALV - report display
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        it_fieldcat            = fieldcat
        is_layout              = layout
*       i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
        i_callback_program     = sy-repid
        it_events              = lt_evt
      TABLES
        t_outtab               = excltab
      EXCEPTIONS
        program_error          = 1
        OTHERS                 = 2.
  ENDIF.             "(+)PANUSURI ticket 57437

ENDFORM.                    "output_alv

*&---------------------------------------------------------------------*
*&      Form  ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_top_of_page.
  DATA: ls_line         TYPE slis_listheader.
  DATA: lt_top_of_page  TYPE slis_t_listheader.

* HEADER LINE 1
  CLEAR ls_line.
  ls_line-typ = 'H'.
  CONCATENATE sy-repid '-' text-005 INTO ls_line-info
                          SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

* HEADER LINE 2
  CLEAR ls_line.
  ls_line-typ = 'A'.
  ls_line-key = ' '.
  CONCATENATE text-023 slgort+3(4) slgort+7(4) INTO ls_line-info
                              SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

* HEADER LINE 3
  CLEAR ls_line.
  ls_line-typ = 'A'.
  ls_line-key = ' '.
*Start of changes TR789
  MOVE text-clt  TO ls_line-info+0(7).
  MOVE sy-sysid  TO ls_line-info+8(5).
  MOVE sy-mandt  TO ls_line-info+14(4).
  MOVE text-dte  TO ls_line-info+21(5).
  WRITE sy-datum TO ls_line-info+27(10).
  MOVE text-tme  TO ls_line-info+40(5).
  WRITE sy-uzeit TO ls_line-info+46(10).

*  concatenate sy-datum text-026 sy-mandt sy-sysid into ls_line-info
*                               separated by space.
*End of changes TR789
  APPEND ls_line TO lt_top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.
ENDFORM.                    "ALV_TOP_OF_PAGE
*---------------------------------------------------------------------*
* FORM AFTER_LINE_OUTPUT                                              *
*---------------------------------------------------------------------*

FORM after_line_output USING rs_lineinfo TYPE slis_lineinfo .
  DATA: lv_tabix TYPE sy-tabix.

  break sahmad.
  CLEAR : gt_sentence.
  READ TABLE excltab INDEX rs_lineinfo-tabindex .
  CHECK sy-subrc = 0 .

  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline  = excltab-long_text
      outputlen = c_len
    TABLES
      out_lines = gt_sentence.

  DESCRIBE TABLE gt_sentence LINES lv_tabix .
  CHECK lv_tabix > 1 .
  LOOP AT gt_sentence INTO wa_word FROM 2 .
    WRITE: / sy-vline ,
    12  sy-vline ,
    13  wa_word-long_text,
    514 sy-vline .
  ENDLOOP.

ENDFORM . "after_line_output
*---------------------------------------------------------------------*
* FORM USER_COMMAND                                              *
*---------------------------------------------------------------------*

FORM user_command USING r_ucomm     LIKE sy-ucomm
                               rs_selfield TYPE slis_selfield.


  DATA: lv_tabix TYPE sy-tabix.
  DATA: lt_tline TYPE TABLE OF tline,
        ls_tline LIKE LINE OF lt_tline.

  CLEAR: gt_sentence.
  break sahmad.
  CASE r_ucomm.
    WHEN '&IC1'. "for double click
      IF rs_selfield-fieldname = 'LONG_TEXT'.
        READ TABLE excltab INDEX rs_selfield-tabindex .
        CALL FUNCTION 'RKD_WORD_WRAP'
          EXPORTING
            textline  = excltab-long_text
            outputlen = c_len
          TABLES
            out_lines = gt_sentence.
        LOOP AT gt_sentence INTO wa_word.
          ls_tline-tdline = wa_word-long_text.
          APPEND ls_tline TO lt_tline.
        ENDLOOP.
        CALL FUNCTION 'COPO_POPUP_TO_DISPLAY_TEXTLIST'
          EXPORTING
*           TASK             = 'DISPLAY'
            titel            = 'Material Long Text'
*         IMPORTING
*           FUNCTION         =
          TABLES
            text_table       = lt_tline.
      ENDIF.
  ENDCASE.

ENDFORM . "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

  IF p_excl IS NOT INITIAL.
*Separate Path and file
    CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = p_file
      IMPORTING
        stripped_name = sep_file
        file_path     = sep_path
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*lv_dir = sep_path.
    IF sep_path CS 'C:' OR sep_path CS 'c:'.
      MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
    ELSE.
*Check if directory path exist or not.
      CALL METHOD cl_gui_frontend_services=>directory_exist
        EXPORTING
          directory            = sep_path      "lv_dir
        RECEIVING
          result               = lv_bol
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.
      IF lv_bol IS INITIAL.
        CONCATENATE text-099 sep_path sep_file INTO sep_path.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
        MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
