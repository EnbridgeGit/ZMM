REPORT ZMMMI018 LINE-SIZE 120 LINE-COUNT 90 NO STANDARD PAGE HEADING
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMI018
*    Programmer  :  Nesh N. Laurencic / Omnilogic Systems Group
*    Client      :  Union Gas Limited
*    Date        :  April 01, 1998
*
* This ABAP will setup the necessary structure used by the load program
* RMMMBIM0.
* It updates records from the Material Master table (MARC).
* A program is required to change  the MRP type to 'ND' for selected
* plants and material groups/ materials
*
* This will initally be used for the Centra MM Merger.
*
* 2000/06/21 - gymana - 4.6B Upgrade
*   - Changed table DD03P to DD03L
************************************************************************

TABLES:
        MARA,                          "Material Master: General Data
        MARC,                          "Material Master: C Segment
        MAPR,                          "Material Index for Forecast
        PROP,                          "Forecast parameters
        PROW.                          "Forecast Values



DATA: BEGIN OF BDCDATA OCCURS 10.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: BEGIN OF ITAB OCCURS 100,
      MATNR    LIKE MARA-MATNR,        " Material
      MATKL    LIKE MARA-MATKL,        " Material Group
      WERKS    LIKE MARC-WERKS,        " Plant
      DISSM    LIKE MARC-DISMM,        " MRP type
      KAUTB    LIKE MARC-KAUTB,        " Indicator: "automatic purchase
      DISPO    LIKE MARC-DISPO,        " MRP controller
      PRMOD    LIKE MPOP-PRMOD,        " Forecast model
      AUTRU    LIKE MARC-AUTRU,        " Reset forecast model automatic
      KOPRW1   LIKE RM03M-KOPRW,       " Corrected value for forecast
      KOPRW2   LIKE RM03M-KOPRW,       " Corrected value for forecast
      KOPRW3   LIKE RM03M-KOPRW,       " Corrected value for forecast
      KOPRW4   LIKE RM03M-KOPRW,       " Corrected value for forecast
      KOPRW5   LIKE RM03M-KOPRW,       " Corrected value for forecast
      KOPRW6   LIKE RM03M-KOPRW,       " Corrected value for forecast
      KOPRW7   LIKE RM03M-KOPRW,       " Corrected value for forecast
      KOPRW8   LIKE RM03M-KOPRW,       " Corrected value for forecast
      KOPRW9   LIKE RM03M-KOPRW,       " Corrected value for forecast
      KOPRW10   LIKE RM03M-KOPRW,      " Corrected value for forecast
      KOPRW11   LIKE RM03M-KOPRW,      " Corrected value for forecast
      KOPRW12   LIKE RM03M-KOPRW,      " Corrected value for forecast
      ERDAT1    LIKE PROW-ERTAG ,      "First day of the period to whi
      ERDAT2    LIKE PROW-ERTAG ,      "First day of the period to whi
      ERDAT3    LIKE PROW-ERTAG ,      "First day of the period to whi
      ERDAT4    LIKE PROW-ERTAG ,      "First day of the period to whi
      ERDAT5    LIKE PROW-ERTAG ,      "First day of the period to whi
      ERDAT6    LIKE PROW-ERTAG ,      "First day of the period to whi
      ERDAT7    LIKE PROW-ERTAG ,      "First day of the period to whi
      ERDAT8    LIKE PROW-ERTAG ,      "First day of the period to whi
      ERDAT9    LIKE PROW-ERTAG ,      "First day of the period to whi
      ERDAT10    LIKE PROW-ERTAG ,     "First day of the period to whi
      ERDAT11    LIKE PROW-ERTAG ,     "First day of the period to whi
      ERDAT12    LIKE PROW-ERTAG ,     "First day of the period to whi
      END OF   ITAB.

DATA:   BEGIN OF MESSTAB OCCURS 10.    "Transaction return
        INCLUDE STRUCTURE BDCMSGCOLL.  "messages
DATA:   END OF MESSTAB.

SELECT-OPTIONS:
                SWERKS FOR MARC-WERKS, "Plant
                SMATKL FOR MARA-MATKL, "Material group
                SMATNR FOR MARC-MATNR. "MAterial

DATA: BEGIN OF ITAB1 OCCURS 100,
      MATNR    LIKE EINA-MATNR,        "Material
      WERKS    LIKE EINE-WERKS,        "Werks
      END OF ITAB1.
DATA: DBCINDEX LIKE SY-DBCNT.
************************************************************************
* This part of data of the program is connected to RMMMBIM0
************************************************************************
TABLES  :  DD03L.

FIELD-SYMBOLS: <F1> .
DATA      : T_CODE       LIKE BMM00-TCODE VALUE 'MM02',
            CHAR(21)     TYPE C,
            NODATA(40) VALUE '////////////////////////////////////////'.

DATA      : PHYFILE LIKE FILENAMECI-FILEEXTERN.
DATA:       INDEX LIKE SY-INDEX.


SELECTION-SCREEN ULINE.
PARAMETERS: CORRFILE LIKE FILENAME-FILEINTERN
            DEFAULT 'ZMMMI018' NO-DISPLAY.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECTION-SCREEN COMMENT 1(70) TEXT-CO1.
SELECTION-SCREEN COMMENT /1(70) TEXT-CO2.
SELECTION-SCREEN COMMENT /1(70) TEXT-CO3 MODIF ID A.
SELECTION-SCREEN END OF BLOCK B1.

DATA    : BEGIN OF Z_BGR00.            "Header
        INCLUDE STRUCTURE BGR00.
DATA    : END OF Z_BGR00.

DATA    : BEGIN OF Z_BMM00.                                 " 0
        INCLUDE STRUCTURE BMM00.
DATA    : END OF Z_BMM00.

DATA    : BEGIN OF Z_BMMH1.                                 " 1
        INCLUDE STRUCTURE BMMH1.
DATA    : END OF Z_BMMH1.

DATA    : BEGIN OF Z_BMMH3.                                 "3
        INCLUDE STRUCTURE BMMH3.
DATA    : END OF Z_BMMH3.

INCLUDE <ICON>.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'A'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.

************************************************************************

START-OF-SELECTION.
* Color the screen
IF ( ( NOT  SMATKL-LOW IS INITIAL ) AND ( NOT SMATNR-LOW IS INITIAL ) ).
    MESSAGE E018.
  ENDIF.

  SELECT * FROM MARC WHERE
                           MATNR IN SMATNR AND
                           WERKS IN SWERKS AND
                           DISMM NE SPACE.
    CLEAR MARA.
* Check material type.
    SELECT SINGLE * FROM MARA WHERE
                             MATNR EQ MARC-MATNR AND
                             MATKL IN SMATKL.
    CHECK SY-SUBRC = 0.

    MOVE :
          MARC-MATNR TO ITAB-MATNR,    " Material
          MARC-WERKS TO ITAB-WERKS,    " Plant
          MARA-MATKL TO ITAB-MATKL,    " Material Group
          MARC-DISMM TO ITAB-DISSM,    " MRP type
          MARC-KAUTB TO ITAB-KAUTB,    " Indicator: "automatic purchase
          MARC-DISPO TO ITAB-DISPO,    " MRP controller
          MARC-AUTRU TO ITAB-AUTRU.    " Reset forecast model automatic

* If you need forecasting values, just remove following comments.

*    IF ITAB-DISSM EQ 'VM'.
*      PERFORM FIND_FORECASTING_VALUES.
*    ENDIF.
    APPEND ITAB.
    CLEAR ITAB.
  ENDSELECT.
  CHECK SY-SUBRC = 0.

  SORT ITAB BY MATNR WERKS.
* Print the candidates for changing.
  FORMAT COLOR 2.
  SKIP.
  WRITE: / 'Candidates for changing : '.
  SKIP.
  LOOP AT ITAB.
    WRITE: / ITAB-MATNR UNDER TEXT-001,
             ITAB-WERKS UNDER TEXT-003,
             ITAB-DISSM UNDER TEXT-004.
    AT END OF MATNR.
      WRITE: / SY-ULINE(25).
    ENDAT.
  ENDLOOP.
*break-point.

  WRITE: / 'END OF REPORT1'.
  SKIP.
  ULINE.
******************************* Second Part of The Program**************
*break-point.

  PERFORM OPEN_OUTPUT_FILE.            " Get Physical name and open file
  PERFORM INIT_STRUCTURES USING 'BGR00'.  "I_BGR00.
  PERFORM INIT_STRUCTURES USING 'BMM00'.  "I_BGR00.
  PERFORM INIT_STRUCTURES USING 'BMMH1'.  "I_BGR00.
* perform init_structures using 'BMMH3'.  "I_BGR00.

* Fill the struct. BGR00
  MOVE '0'            TO  Z_BGR00-STYPE.
  MOVE 'ZMMMI018'     TO  Z_BGR00-GROUP.
  MOVE SY-MANDT       TO  Z_BGR00-MANDT.
  MOVE SY-UNAME       TO  Z_BGR00-USNAM.
  TRANSFER Z_BGR00    TO  PHYFILE LENGTH 167. "Another file is that size

* Fill the struct. BMM00.
  LOOP AT ITAB.

    MOVE '1'            TO Z_BMM00-STYPE.  " Obligatory
    MOVE T_CODE         TO Z_BMM00-TCODE.  " T-code
    MOVE ITAB-MATNR     TO Z_BMM00-MATNR.  " Material
    MOVE ITAB-WERKS     TO Z_BMM00-WERKS.  " Plant
    MOVE 'X'            TO Z_BMM00-XEID1.  " MRP View

    IF ITAB-DISSM = 'VM'.
      MOVE 'X'            TO Z_BMM00-XEIP1.  " Forecasting view
    ENDIF.

    MOVE 'X'            TO Z_BMM00-XEIE1.  " Purchasing view
    TRANSFER Z_BMM00  TO PHYFILE LENGTH 167.

* Fill the struct BMMH1
    MOVE '2'              TO Z_BMMH1-STYPE.    ""Obligatory
    MOVE 'ND'   TO Z_BMMH1-DISMM.      " MRP type
    MOVE SPACE   TO Z_BMMH1-KAUTB.     " automatic purchasing
    MOVE SPACE   TO Z_BMMH1-DISPO.     "MRP controller
    IF ITAB-DISSM = 'VM'.
      MOVE 'N'   TO Z_BMMH1-PRMOD.     "Forecasting Model
      MOVE SPACE   TO Z_BMMH1-AUTRU. "forecast model automatically rst.
      MOVE SPACE TO Z_BMMH1-PERKZ.     "Period indicator (reset) !!!
    ENDIF.
    TRANSFER Z_BMMH1      TO PHYFILE LENGTH 2241.

*    IF ITAB-DISSM = 'VM'. " and ( itab-koprw1 ne '0' or
**                             ITAB-KOPRW2 NE '0' OR
**                             ITAB-KOPRW3 NE '0' OR
**                             ITAB-KOPRW4 NE '0' OR
**                             ITAB-KOPRW5 NE '0' OR
**                             ITAB-KOPRW6 NE '0' OR
**                             ITAB-KOPRW6 NE '0' OR
**                             ITAB-KOPRW7 NE '0' OR
**                             ITAB-KOPRW8 NE '0' OR
**                             ITAB-KOPRW9 NE '0' OR
**                             ITAB-KOPRW10 NE '0' OR
**                             ITAB-KOPRW11 NE '0' OR
**                             ITAB-KOPRW12 NE '0' ).
*
*
** Fill BMMH3 12 times (once for each period)
*
** Fill the struct BMMH3                              I
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT1 TO Z_BMMH3-PRIOD.
*      WRITE SPACE TO Z_BMMH3-KOPRW. " right-justified.
*                                       "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI. " right-justified.
*                                        "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                              II
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE  ITAB-ERDAT2 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                        "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                        "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                              III
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT3 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                        "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                        "Rel between cor. and original
**      WRITE '0' TO Z_BMMH3-PRWRT RIGHT-JUSTIFIED.
**                                        "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                              IV
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT4 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'  TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                       "Corrected forecast value
*      WRITE '0.00'  TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                       "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                               V
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT5 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                        "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                        "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                              VI
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT6 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                        "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                                       "Rel between cor
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                              VII
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT7 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                        "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                        "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                              VIII
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT8 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                                       "Corrected forec
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                       "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                              IX
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT9 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                       "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                       "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                                X
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT10 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                       "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                        "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                               XI
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT11 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                        "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                        "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*
** Fill the struct BMMH3                                XII
*      MOVE '4'              TO Z_BMMH3-STYPE.    ""Obligatory
*      MOVE ITAB-ERDAT12 TO Z_BMMH3-PRIOD.
*      WRITE '0.00'   TO Z_BMMH3-KOPRW RIGHT-JUSTIFIED.
*                                        "Corrected forecast value
*      WRITE '0.00' TO Z_BMMH3-ANTEI RIGHT-JUSTIFIED.
*                                        "Rel between cor. and original
*      TRANSFER Z_BMMH3     TO PHYFILE LENGTH 2241.
*    ENDIF.
    PERFORM INIT_STRUCTURES USING 'BMM00'. " I_BGR00.
    PERFORM INIT_STRUCTURES USING 'BMMH1'. " I_BGR00.
*   perform init_structures using 'BMMH3'. " I_BGR00.

  ENDLOOP.
  CLOSE DATASET PHYFILE.
  IF SY-SUBRC <> 0.
    WRITE: / 'Problem with closing a file !'.
  ELSE.
 WRITE: / 'Ok! Now run the program RMMMBIM0 with the variant ZMMMI018 '
                   COLOR 7.
  ENDIF.
******************************* Second Part of The Program**************

TOP-OF-PAGE.
  FORMAT RESET.
  FORMAT COLOR 1.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         20 SY-TITLE COLOR 4 INTENSIFIED ON,
        76 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.


  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID.
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  SKIP.
  WRITE: /1 TEXT-001, 12 TEXT-003, 19 TEXT-004.
*&---------------------------------------------------------------------*
*&      Form  BDC_SCREEN
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_SCREEN USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                               " BDC_SCREEN

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                               " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  OPEN_OUTPUT_FILE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OPEN_OUTPUT_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            LOGICAL_FILENAME = CORRFILE
       IMPORTING
            FILE_NAME        = PHYFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006 WITH CORRFILE.
  ELSE.
    OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
  ENDIF.

ENDFORM.                               " OPEN_OUTPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  INIT_STRUCTURES
*&---------------------------------------------------------------------*
*       Obligatory                                                 *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_STRUCTURES USING TABNAME.    "tab.
  SELECT * FROM DD03L WHERE TABNAME = TABNAME.
    CLEAR CHAR.
    CHAR(2)   = 'Z_'.
    CHAR+2(5) = TABNAME.
    CHAR+7(1) = '-'.
    CHAR+8    = DD03L-FIELDNAME.
    ASSIGN (CHAR) TO <F1>.
    <F1> = NODATA.
  ENDSELECT.
ENDFORM.                               " INIT_STRUCTURES
*&---------------------------------------------------------------------*
*&      Form  FIND_FORECASTING_VALUES
*&---------------------------------------------------------------------*
*       Forecasting values                                             *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_FORECASTING_VALUES.
* Take the Index for Forecast
  CLEAR MAPR.
  SELECT SINGLE * FROM MAPR WHERE
                            WERKS EQ MARC-WERKS AND   " Plant
                            MATNR EQ MARC-MATNR.      " Material #
  CHECK SY-SUBRC = 0.

  CLEAR PROP.
  SELECT SINGLE * FROM
                       PROP WHERE
                                 PNUM1 EQ MAPR-PNUM1 AND
                                 HSNUM = '00' AND
                                 VERSP EQ '00' .
  CHECK SY-SUBRC = 0.

  MOVE PROP-PRMOD TO ITAB-PRMOD.       " Forecas model

*break-point.
  CLEAR PROW.
  SELECT * FROM PROW WHERE
                           PNUM2 EQ PROP-PNUM2.
    MOVE SY-DBCNT TO DBCINDEX.
    PERFORM FILL_THE_ITAB.
  ENDSELECT.
  CHECK SY-SUBRC = 0.
ENDFORM.                               " FIND_FORECASTING_VALUES
*&---------------------------------------------------------------------*
*&      Form  FILL_THE_ITAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_THE_ITAB.
  CASE DBCINDEX.
    WHEN '1'.
      MOVE PROW-KOPRW TO ITAB-KOPRW1.
      MOVE PROW-ERTAG TO ITAB-ERDAT1.
    WHEN '2'.
      MOVE PROW-KOPRW TO ITAB-KOPRW2.
      MOVE PROW-ERTAG TO ITAB-ERDAT2.

    WHEN '3'.
      MOVE PROW-KOPRW TO ITAB-KOPRW3.
      MOVE PROW-ERTAG TO ITAB-ERDAT3.

    WHEN '4'.
      MOVE PROW-KOPRW TO ITAB-KOPRW4.
      MOVE PROW-ERTAG TO ITAB-ERDAT4.

    WHEN '5'.
      MOVE PROW-KOPRW TO ITAB-KOPRW5.
      MOVE PROW-ERTAG TO ITAB-ERDAT5.

    WHEN '6'.

      MOVE PROW-KOPRW TO ITAB-KOPRW6.
      MOVE PROW-ERTAG TO ITAB-ERDAT6.

    WHEN '7'.

      MOVE PROW-KOPRW TO ITAB-KOPRW7.
      MOVE PROW-ERTAG TO ITAB-ERDAT7.

    WHEN '8'.

      MOVE PROW-KOPRW TO ITAB-KOPRW8.
      MOVE PROW-ERTAG TO ITAB-ERDAT8.

    WHEN '9'.

      MOVE PROW-KOPRW TO ITAB-KOPRW9.
      MOVE PROW-ERTAG TO ITAB-ERDAT9.

    WHEN '10'.

      MOVE PROW-KOPRW TO ITAB-KOPRW10.
      MOVE PROW-ERTAG TO ITAB-ERDAT10.

    WHEN '11'.

      MOVE PROW-KOPRW TO ITAB-KOPRW11.
      MOVE PROW-ERTAG TO ITAB-ERDAT11.

    WHEN '12'.
      MOVE PROW-KOPRW TO ITAB-KOPRW12.
      MOVE PROW-ERTAG TO ITAB-ERDAT12.

  ENDCASE.
ENDFORM.                               " FILL_THE_ITAB
