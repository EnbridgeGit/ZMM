REPORT ZMMMR042 NO STANDARD PAGE HEADING LINE-SIZE 132
                LINE-COUNT 65 MESSAGE-ID PP.
************************************************************************
*
*   PROGRAM:    ZMMMR042
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas
*   DATE:       May 1998.
*
*   The purpose of this program is to report on the movements of
*   hazardous materials.  The selection screen has mandatory parameters
*   of plant code and storage location, the optional parameters are
*   material group, material number, movement type and transaction
*   date.  Totals are produced for material groups within storage
*   location within plant code and at the end of the report.
*
************************************************************************

TABLES:   MARA,                         " material master
          MAKT,                         " material descriptions
          MKPF,                         " Material Document - header
          MSEG.                         " Material Document - line item

DATA:     XBWART(1)     TYPE C VALUE 'N',               " type entered
          XBUDAT(1)     TYPE C VALUE 'N',               " date entered
          XMENGE        TYPE I,                         " work quantity
          RMATNR(6)     TYPE C,                         " rpt mat nbr
          RMAKTX        LIKE MAKT-MAKTX,                " rpt mat desc
          RMENGE(11)    TYPE C.                         " rpt quantity

DATA:     BEGIN OF IMARA OCCURS 1000,
            MATNR       LIKE MARA-MATNR,                " material nbr
            MATKL       LIKE MARA-MATKL,                " material group
          END OF IMARA.

DATA:     BEGIN OF REPTAB OCCURS 1000,                  " report data
            WERKS       LIKE MSEG-WERKS,                " plant code
            LGORT       LIKE MSEG-LGORT,                " storage loc
            MATKL       LIKE MARA-MATKL,                " material group
            MAKTX       LIKE MAKT-MAKTX,                " material desc
            MATNR       LIKE MARA-MATNR,                " material nbr
            BUDAT       LIKE MKPF-BUDAT,                " transaction dt
            BWART       LIKE MSEG-BWART,                " movement type
            MENGE       LIKE MARD-VMEIN,                " quantity
            MEINS       LIKE MSEG-MEINS,                " unit of measur
            INOUT(3)    TYPE C,                         " move direction
            TOFROM(16)  TYPE C,                         " move location
            TOFROMP     LIKE MSEG-PS_PSP_PNR,           " move loc WBS
            MBLNR       LIKE MSEG-MBLNR,                " document nbr
          END OF REPTAB.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(35) TEXT-003.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-022.
    SELECT-OPTIONS S_WERKS     FOR MSEG-WERKS OBLIGATORY.
    SELECT-OPTIONS S_LGORT     FOR MSEG-LGORT OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK BOX2.

  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-023.
    SELECT-OPTIONS S_MATKL     FOR MARA-MATKL.
    SELECT-OPTIONS S_MATNR     FOR MARA-MATNR.
    SELECT-OPTIONS S_BWART     FOR MSEG-BWART.
    SELECT-OPTIONS S_BUDAT     FOR MKPF-BUDAT
                               DEFAULT '00000000' TO SY-DATUM.
  SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* clear tables
REFRESH: IMARA, REPTAB.
CLEAR:   IMARA, REPTAB.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* extract required data
START-OF-SELECTION.

* set indicators for heading lines
  IF S_BWART-LOW <> SPACE.
    XBWART = 'Y'.
  ENDIF.
  IF S_BUDAT-LOW <> '00000000'.
    XBUDAT = 'Y'.
  ENDIF.

* get the hazardous materials
  SELECT MATNR MATKL
  FROM   MARA
  INTO   TABLE IMARA
  WHERE  MATNR IN S_MATNR
  AND    MATKL IN S_MATKL
  AND    STOFF NE SPACE.

* get the materials movements
  LOOP AT IMARA.

    CLEAR MAKT.
    SELECT SINGLE * FROM MAKT                     " get the description
    WHERE  MATNR EQ IMARA-MATNR
    AND    SPRAS EQ SY-LANGU.

    CLEAR MSEG.
    SELECT * FROM MSEG                            " get movement lin itm
    WHERE MATNR EQ IMARA-MATNR
    AND   BWART IN S_BWART
    AND   WERKS IN S_WERKS
    AND   LGORT IN S_LGORT.

      SELECT SINGLE * FROM MKPF                   " get movement header
      WHERE  MBLNR EQ MSEG-MBLNR
      AND    MJAHR EQ MSEG-MJAHR
      AND    BUDAT IN S_BUDAT.

      IF SY-SUBRC = 0.
        REPTAB-WERKS = MSEG-WERKS.
        REPTAB-LGORT = MSEG-LGORT.
        REPTAB-MATKL = IMARA-MATKL.
        REPTAB-MATNR = IMARA-MATNR.
        REPTAB-MAKTX = MAKT-MAKTX.
        REPTAB-BUDAT = MKPF-BUDAT.
        REPTAB-BWART = MSEG-BWART.
        IF MSEG-SHKZG = 'H'.
          REPTAB-MENGE = MSEG-MENGE * -1.
        ELSE.
          REPTAB-MENGE = MSEG-MENGE.
        ENDIF.
        REPTAB-MEINS = MSEG-MEINS.
        IF MSEG-SHKZG = 'S'.
          REPTAB-INOUT = 'IN'.
        ELSE.
          REPTAB-INOUT = 'OUT'.
        ENDIF.
        IF MSEG-AUFNR <> SPACE.
          REPTAB-TOFROM = MSEG-AUFNR.
        ELSEIF MSEG-PS_PSP_PNR <> SPACE.
          REPTAB-TOFROMP = MSEG-PS_PSP_PNR.
        ELSEIF MSEG-KOSTL <> SPACE.
          REPTAB-TOFROM = MSEG-KOSTL.
        ELSEIF MSEG-UMMAT <> SPACE.
          CONCATENATE MSEG-UMMAT+12(6) '/'
                      MSEG-UMWRK '/'
                      MSEG-UMLGO INTO REPTAB-TOFROM.
        ELSEIF MSEG-UMWRK <> SPACE.
          CONCATENATE MSEG-UMWRK '/'
                      MSEG-UMLGO INTO REPTAB-TOFROM.
        ELSEIF MSEG-UMLGO <> SPACE.
          REPTAB-TOFROM = MSEG-UMLGO.
        ENDIF.
        REPTAB-MBLNR = MSEG-MBLNR.
        APPEND REPTAB.
        CLEAR REPTAB.

      ENDIF.
    ENDSELECT.
  ENDLOOP.
  COMMIT WORK.                                    " release buffers

* sort the table for the report
  SORT REPTAB ASCENDING BY WERKS LGORT MATKL MATNR
                           BUDAT DESCENDING BWART.
*                          bwart ascending.

* output the report
  LOOP AT REPTAB.

    AT NEW WERKS.
      RMATNR = REPTAB-MATNR+12(6).
      RMAKTX = REPTAB-MAKTX.
      NEW-PAGE.
    ENDAT.

    AT NEW LGORT.
      RMATNR = REPTAB-MATNR+12(6).
      RMAKTX = REPTAB-MAKTX.
      NEW-PAGE.
    ENDAT.

    AT NEW MATKL.
      RMATNR = REPTAB-MATNR+12(6).
      RMAKTX = REPTAB-MAKTX.
      NEW-PAGE.
    ENDAT.

    AT NEW MATNR.
      RMATNR = REPTAB-MATNR+12(6).
      RMAKTX = REPTAB-MAKTX.
    ENDAT.

    IF REPTAB-MENGE > 999
    OR REPTAB-MENGE > 999.
      XMENGE = REPTAB-MENGE.
      WRITE: XMENGE TO RMENGE USING EDIT MASK 'RR______,___V'.
    ELSE.
      XMENGE = REPTAB-MENGE.
      WRITE: XMENGE TO RMENGE USING EDIT MASK 'RR__________V'.
    ENDIF.
    PERFORM WRITE_DETAIL.
    RMATNR = SPACE.
    RMAKTX = SPACE.

    AT END OF MATKL.
      ULINE.
    ENDAT.

  ENDLOOP.

END-OF-SELECTION.

************************************************************************
*  This section outputs the report headings
************************************************************************
FORM WRITE_HEADER.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 SY-DATUM
            , 057 TEXT-001
            , 121 TEXT-002,  SY-PAGNO
            , 132 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 052 TEXT-003, TEXT-036
            ,     SY-REPID UNDER TEXT-002
            , 132 SY-VLINE.
     IF XBUDAT = 'Y'.
       WRITE: /01 SY-VLINE
            , 003 TEXT-004, REPTAB-WERKS
            , 045 TEXT-007, S_BUDAT-LOW, TEXT-017, S_BUDAT-HIGH
            , 132 SY-VLINE.
     ELSE.
       WRITE: /01 SY-VLINE
            , 003 TEXT-004, REPTAB-WERKS
            , 132 SY-VLINE.
     ENDIF.
     IF XBWART = 'Y'.
       WRITE: /01 SY-VLINE
            , 003 TEXT-005, REPTAB-LGORT
            , 053 TEXT-008, S_BWART-LOW, TEXT-017, S_BWART-HIGH
            , 132 SY-VLINE.
     ELSE.
       WRITE: /01 SY-VLINE
            , 003 TEXT-005, REPTAB-LGORT
            , 132 SY-VLINE.
     ENDIF.
     WRITE:   /01 SY-VLINE
            , 003 TEXT-006, REPTAB-MATKL
            , 132 SY-VLINE.
     ULINE.
     WRITE:   /03 TEXT-009
            , 014 TEXT-010
            , 057 TEXT-011
            , 070 TEXT-012
            , 077 TEXT-013
            , 090 TEXT-024
            , 096 TEXT-014
            , 102 TEXT-015
            , 121 TEXT-016.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

************************************************************************
*  This section outputs the detail lines to the report
************************************************************************
FORM WRITE_DETAIL.
     WRITE: /     RMATNR           UNDER TEXT-009
            ,     RMAKTX           UNDER TEXT-010
            ,     REPTAB-BUDAT     UNDER TEXT-011
            ,     REPTAB-BWART     UNDER TEXT-012
            ,     RMENGE           UNDER TEXT-013
            ,     REPTAB-MEINS     UNDER TEXT-024
            ,     REPTAB-INOUT     UNDER TEXT-014.
     IF REPTAB-TOFROMP <> SPACE.
       WRITE:     REPTAB-TOFROMP   UNDER TEXT-015
            ,     REPTAB-MBLNR     UNDER TEXT-016.
     ELSE.
       WRITE:     REPTAB-TOFROM    UNDER TEXT-015
            ,     REPTAB-MBLNR     UNDER TEXT-016.
     ENDIF.
     PERFORM SHOWVLINE.
ENDFORM.

************************************************************************
*  These sections write vertical lines between the columns of the report
************************************************************************
FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 012 SY-VLINE
            , 055 SY-VLINE
            , 068 SY-VLINE
            , 075 SY-VLINE
            , 088 SY-VLINE
            , 094 SY-VLINE
            , 100 SY-VLINE
            , 119 SY-VLINE
            , 132 SY-VLINE.
ENDFORM.

************************************************************************
*  This is the end, my freind
************************************************************************
