*&---------------------------------------------------------------------*
*&  Include           ZMMORDERSDATA_SCR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :   ZMMORDERSDATA                                 *
* Include            :   ZMMORDERSDATA_SCR                             *
* Author             :   Rajeshwar Reddy                               *
* Date               :   23-Jan-2020                                   *
* Technical Contact  :   Rajeshwar Reddy                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Procurement performance report                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 23-Jan-2020  JOOKONTR  D30K930414 CHG0172306   Initial               *
* 28-Feb-2020  JOOKONTR  D30K930458 CHG0176546 consider only Document  *
*                        Types 'NB','ZF','RP' and Change the file name *
*                        as requested,Change output date format to     *
*                        MM/DD/YYYY                                    *
*10-Apr-2020   AKMADASU  D30K930490 CHG0177517 - PO Report output      *
*                                   Alignment issue                    *
*22-Feb-2022   DADIM     D30K932036 CHG0241607 - Add delivery date     *
*                                   to selection screen                *
*&---------------------------------------------------------------------*

*&-ORDERS DATA SCREEN
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: SO_EBELN FOR EKKO-EBELN,    "Purchasing Document Number
                SO_BEDAT FOR EKKO-BEDAT,    "Purchasing Document Date
                SO_BSART FOR EKKO-BSART,    "Purchasing Document Type
                SO_EKGRP FOR EKKO-EKGRP,    "Purchasing Group
                SO_AEDAT FOR EKKO-AEDAT.    "Date on Which Record Was Created
SELECTION-SCREEN END OF BLOCK B1.

*&ON TIME DELIVERY SCREEN
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: SO_SLFDT FOR EKET-SLFDT,
                SO_EINDT FOR EKET-EINDT,   "Added by DADIM for CHG0241607
                SO_LIFNR FOR EKKO-LIFNR.
SELECTION-SCREEN END OF BLOCK B2.

*&RADIAOBUTTION
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS : RB_APP RADIOBUTTON GROUP RAD2 DEFAULT 'X' USER-COMMAND CMND,
             RB_PRS RADIOBUTTON GROUP RAD2,
             RB_ALV RADIOBUTTON GROUP RAD2.
SELECTION-SCREEN END OF BLOCK B3.

*&Presentation Server
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) TEXT-P01 FOR FIELD P_PRS MODIF ID M1.
PARAMETERS:P_PRS TYPE STRING MODIF ID M1.
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(50) TEXT-P02 MODIF ID M1.
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK B4.

*&Application Server
SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) TEXT-P01 FOR FIELD P_APP MODIF ID M2.
PARAMETERS: P_APP TYPE RLGRAP-FILENAME MODIF ID M2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(50) TEXT-P02 MODIF ID M2. " commented by JOOKONTR for CHG0176546
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK B5.

*&---------------------------------------------------------------------*
* INITIALIZATION                                                       *
*&---------------------------------------------------------------------*
INITIALIZATION.
*BOC chaanges by JOOKONTR for CHG0176546
*  CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/RPA/SAP_UG_' SY-SYSID '_PODATA_*' SY-DATUM SY-UZEIT '.xlsx' INTO P_APP.
  CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/RPA/UG_PODATA_*' SY-DATUM SY-UZEIT '.xls' INTO P_APP.
*EOC chaanges by JOOKONTR for CHG0176546
*&---------------------------------------------------------------------*
* MODIFY SCREEN                                                        *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF RB_PRS EQ ABAP_TRUE.
      IF SCREEN-GROUP1 = GC_M2.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF RB_APP EQ ABAP_TRUE.
      IF SCREEN-GROUP1 = GC_M1.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
      CLEAR P_PRS.
    ELSEIF RB_ALV EQ ABAP_TRUE.
      IF SCREEN-GROUP1 = GC_M2 OR  SCREEN-GROUP1 = GC_M1.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.


*&---------------------------------------------------------------------*
* F4 FOR PRESENTATION SERVER                                           *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PRS.
  PERFORM PATH_PRS.

*&---------------------------------------------------------------------*
* F4 FOR APPLICATION SERVER                                            *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_APP.
  PERFORM PATH_APP.

*&---------------------------------------------------------------------*
* SELECTION SCREEN FIELD VALIDATIONS                                   *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  DATA: LV_PATH TYPE CHAR50,
        LV_EXT TYPE CHAR5,
        LV_LEN  TYPE I.
*BOC chaanges by JOOKONTR for CHG0176546
  DATA: LV_DATE(2) TYPE C,
        LV_MONTH(2) TYPE C,
        LV_YEAR(4) TYPE C,
        LV_FDATE(10) TYPE C,
        LV_LDATE(10) TYPE C.
  DATA: LS_ZFIT_XPARAM TYPE ZFIT_XPARAM.
*EOC chaanges by JOOKONTR for CHG0176546
  IF ( SO_SLFDT IS NOT INITIAL OR SO_LIFNR IS NOT INITIAL ).
*BOC chaanges by JOOKONTR for CHG0176546
*    CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/RPA/SAP_UG_' SY-SYSID '_PODATA_ONTIME_' SY-DATUM SY-UZEIT '.xlsx' INTO P_APP.
    CLEAR:LV_YEAR,LV_MONTH, LV_DATE,LV_FDATE.
    LV_YEAR = SO_SLFDT-LOW+0(4).
    LV_MONTH = SO_SLFDT-LOW+4(2).
    LV_DATE = SO_SLFDT-LOW+6(2).
    CONCATENATE LV_MONTH LV_DATE LV_YEAR INTO LV_FDATE SEPARATED BY '.'.

    CLEAR:LV_YEAR,LV_MONTH, LV_DATE,LV_LDATE.
    LV_YEAR = SO_SLFDT-HIGH+0(4).
    LV_MONTH = SO_SLFDT-HIGH+4(2).
    LV_DATE = SO_SLFDT-HIGH+6(2).
    CONCATENATE LV_MONTH LV_DATE LV_YEAR INTO LV_LDATE SEPARATED BY '.'.
**--Start of changes by akmadasu for CHG0177517
*    CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/RPA/UG_Ontime Data_' LV_FDATE '-' LV_LDATE'.xls' INTO P_APP.
    CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/RPA/UG_Ontime_Data_' LV_FDATE '-' LV_LDATE'.xls' INTO P_APP.
**--End of changes by akmadasu for CHG0177517
*EOC chaanges by JOOKONTR for CHG0176546
*Start of changes by DADIM for CHG0241607
  ELSEIF SO_EINDT IS NOT INITIAL.
    CLEAR:LV_YEAR,LV_MONTH, LV_DATE,LV_FDATE.
    LV_YEAR = SO_EINDT-LOW+0(4).
    LV_MONTH = SO_EINDT-LOW+4(2).
    LV_DATE = SO_EINDT-LOW+6(2).
    CONCATENATE LV_MONTH LV_DATE LV_YEAR INTO LV_FDATE SEPARATED BY '.'.

    CLEAR:LV_YEAR,LV_MONTH, LV_DATE,LV_LDATE.
    LV_YEAR = SO_EINDT-HIGH+0(4).
    LV_MONTH = SO_EINDT-HIGH+4(2).
    LV_DATE = SO_EINDT-HIGH+6(2).
    CONCATENATE LV_MONTH LV_DATE LV_YEAR INTO LV_LDATE SEPARATED BY '.'.
    CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/RPA/UG_Ontime_Data_' LV_FDATE '-' LV_LDATE'.xls' INTO P_APP.
*End of changes by DADIM for CHG0241607
  ELSEIF ( SO_BEDAT IS NOT INITIAL OR SO_EBELN IS NOT INITIAL OR SO_BSART IS NOT INITIAL
         OR SO_EKGRP IS NOT INITIAL OR SO_AEDAT IS NOT INITIAL ).
*BOC chaanges by JOOKONTR for CHG0176546
*    CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/RPA/SAP_UG_' SY-SYSID '_PODATA_ORDER_' SY-DATUM SY-UZEIT '.xlsx' INTO P_APP.
    CLEAR:LV_YEAR,LV_MONTH, LV_DATE,LV_FDATE.
    LV_YEAR = SO_BEDAT-LOW+0(4).
    LV_MONTH = SO_BEDAT-LOW+4(2).
    LV_DATE = SO_BEDAT-LOW+6(2).
    CONCATENATE LV_MONTH LV_DATE LV_YEAR INTO LV_FDATE SEPARATED BY '.'.

    CLEAR:LV_YEAR,LV_MONTH, LV_DATE,LV_LDATE.
    LV_YEAR = SO_BEDAT-HIGH+0(4).
    LV_MONTH = SO_BEDAT-HIGH+4(2).
    LV_DATE = SO_BEDAT-HIGH+6(2).
    CONCATENATE LV_MONTH LV_DATE LV_YEAR INTO LV_LDATE SEPARATED BY '.'.
**--Start of changes by akmadasu for CHG0177517
*    CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/RPA/UG_Orders Data_' LV_FDATE '-' LV_LDATE'.xls' INTO P_APP.
    CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/RPA/UG_Orders_Data_' LV_FDATE '-' LV_LDATE'.xls' INTO P_APP.
**--end of changes by akmadasu for CHG0177517

*EOC chaanges by JOOKONTR for CHG0176546
  ENDIF.
  IF SO_SLFDT IS INITIAL AND SO_LIFNR IS NOT INITIAL
     AND SO_EINDT IS NOT INITIAL.    "Added by DADIM for CHG0241607
    MESSAGE TEXT-013 TYPE 'E'.
  ENDIF.
  IF RB_PRS EQ ABAP_TRUE.
    IF P_PRS IS INITIAL AND SY-UCOMM EQ 'ONLI'.
      MESSAGE TEXT-012 TYPE 'E'.
    ELSEIF P_PRS IS NOT INITIAL AND SY-UCOMM EQ 'ONLI'.
      SPLIT P_PRS AT '.' INTO LV_PATH LV_EXT.
      TRANSLATE LV_EXT TO UPPER CASE.
      IF LV_EXT NE GC_XLSX.
        MESSAGE TEXT-009 TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF P_APP IS INITIAL AND SY-UCOMM EQ 'ONLI'.
    MESSAGE TEXT-012 TYPE 'E'.
  ELSEIF P_APP IS NOT INITIAL AND SY-UCOMM EQ 'ONLI'.
*BOC changed by JOOKONTR for CHG0176546
*    LV_LEN = STRLEN( P_APP ).
*    LV_LEN = LV_LEN - 4.
*    LV_EXT = P_APP+LV_LEN(4).
*    TRANSLATE LV_EXT TO UPPER CASE.
*    IF LV_EXT NE GC_XLSX.
*      MESSAGE TEXT-009 TYPE 'E'.
*    ENDIF.
*EOC changed by JOOKONTR for CHG0176546
  ENDIF.

*Start of change by DADIM for CHG0241607
*  IF ( SO_SLFDT IS NOT INITIAL OR SO_LIFNR IS NOT INITIAL ) AND
  IF ( SO_SLFDT IS NOT INITIAL OR SO_LIFNR IS NOT INITIAL OR SO_EINDT IS NOT INITIAL ) AND
*End of change by DADIM for CHG0241607
     ( SO_EBELN IS NOT INITIAL OR SO_BEDAT IS NOT INITIAL OR SO_BSART IS NOT INITIAL
       OR SO_EKGRP IS NOT INITIAL OR SO_AEDAT IS NOT INITIAL ) AND SY-UCOMM EQ 'ONLI'.
    MESSAGE TEXT-010 TYPE 'E'.
*Start of change by DADIM for CHG0241607
*  ELSEIF ( SO_SLFDT IS INITIAL AND SO_LIFNR IS INITIAL ) AND
  ELSEIF ( SO_SLFDT IS INITIAL AND SO_LIFNR IS INITIAL AND SO_EINDT IS INITIAL ) AND
*End of change by DADIM for CHG0241607
         ( SO_EBELN IS INITIAL AND SO_BEDAT IS INITIAL AND SO_BSART IS INITIAL
          AND SO_EKGRP IS INITIAL AND SO_AEDAT IS INITIAL ) AND SY-UCOMM EQ 'ONLI'.
    MESSAGE TEXT-010 TYPE 'E'.
  ENDIF.
