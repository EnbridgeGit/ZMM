REPORT ZMINR900 NO STANDARD PAGE HEADING LINE-SIZE 088
                LINE-COUNT 65 MESSAGE-ID PP.
************************************************************************
*
*   PROGRAM:    ZMINR900
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas
*   DATE:       May 1998.
*
*   The purpose of this program is to create the Material Characterisic
*   listing.  The selection screen has mandatory parameters of a single
*   characteristic, the optional parameters are material class, material
*   group and materila number.  The report is sorted by material within
*   characteristic or, optionally, characteristic within material.
*
************************************************************************

TABLES:   CABN,                         " Characteristic
          MARA,                         " Material Master
          MAKT,                         " material descriptions
          AUSP,                         " Characteristic Values
          KSSK,                         " Alloc Table: Object to Class
          KLAH.                         " Class Header Data

DATA:     OBJECT        LIKE AUSP-OBJEK,                " chg obj type
          RMATNR(6)     TYPE C,                         " rpt material
          PRVMATNR      LIKE MARA-MATNR,                " material nbr
          RMAKTX        LIKE MAKT-MAKTX,                " rpt descrip
          RATNAM        LIKE KLVMERK-ATNAM.             " rpt char name

DATA:     BEGIN OF REPTAB OCCURS 1000,                  " report data
            CLASS       LIKE KLAH-CLASS,                " char class
            ATWRT       LIKE AUSP-ATWRT,                " characteristic
            MAKTX       LIKE MAKT-MAKTX,                " material desc
            MATNR       LIKE MARA-MATNR,                " material nbr
          END OF REPTAB.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(35) TEXT-003.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-008.
    PARAMETERS P_ATNAM         LIKE KLVMERK-ATNAM OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK BOX2.

  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-009.
    SELECT-OPTIONS S_CLASS     FOR KLAH-CLASS NO INTERVALS.
    SELECT-OPTIONS S_MATKL     FOR MARA-MATKL.
    SELECT-OPTIONS S_MATNR     FOR MARA-MATNR.
  SELECTION-SCREEN END OF BLOCK BOX3.

  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-010.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-011.
      PARAMETERS: P_SRTCHR    RADIOBUTTON GROUP RAD1.
   SELECTION-SCREEN END OF LINE.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(29) TEXT-012.
      PARAMETERS: P_SRTMAT    RADIOBUTTON GROUP RAD1.
   SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* clear tables
REFRESH: REPTAB.
CLEAR:   REPTAB.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* extract required data
START-OF-SELECTION.

* get all the internal characteristic number
  TRANSLATE P_ATNAM TO UPPER CASE.
  SELECT SINGLE * FROM CABN
  WHERE  ATNAM EQ P_ATNAM.

* get the materials
  SELECT * FROM MARA
  WHERE  MATNR IN S_MATNR
  AND    MATKL IN S_MATKL.

    CLEAR MAKT.
    SELECT SINGLE * FROM MAKT                     " get the description
    WHERE  MATNR EQ MARA-MATNR
    AND    SPRAS EQ SY-LANGU.

    MOVE MARA-MATNR TO OBJECT.

    SELECT * FROM AUSP                            " get characteristics
    WHERE  OBJEK EQ OBJECT
    AND    ATINN EQ CABN-ATINN
    AND    MAFID EQ 'O'
    AND    KLART EQ '001'.

      PERFORM GET_OBJECT_CLASS.
      IF KLAH-CLASS IN S_CLASS.
        REPTAB-CLASS = KLAH-CLASS.
        REPTAB-ATWRT = AUSP-ATWRT.
        REPTAB-MAKTX = MAKT-MAKTX.
        REPTAB-MATNR = MARA-MATNR.
        APPEND REPTAB.
        CLEAR REPTAB.
      ENDIF.

    ENDSELECT.
  ENDSELECT.

* sort the table for the report by characteristic or material number
  IF P_SRTCHR = 'X'.
    SORT REPTAB BY CLASS ATWRT MATNR.
  ELSE.
    SORT REPTAB BY CLASS MATNR ATWRT.
  ENDIF.

* delete duplicate entries from the table
  DELETE ADJACENT DUPLICATES
      FROM REPTAB
          COMPARING CLASS ATWRT MATNR.

* output the report
  WRITE: P_ATNAM RIGHT-JUSTIFIED TO RATNAM.
  LOOP AT REPTAB.

    AT NEW CLASS.
      RMATNR = REPTAB-MATNR+12(6).
      RMAKTX = REPTAB-MAKTX.
      NEW-PAGE.
    ENDAT.

    AT NEW MATNR.
      IF   P_SRTCHR = 'X'
      OR ( P_SRTMAT = 'X'
      AND  REPTAB-MATNR <> PRVMATNR ).
        PRVMATNR = REPTAB-MATNR.
        RMATNR   = REPTAB-MATNR+12(6).
        RMAKTX   = REPTAB-MAKTX.
      ENDIF.
    ENDAT.

    PERFORM WRITE_DETAIL.
    RMATNR = SPACE.
    RMAKTX = SPACE.

    AT END OF CLASS.
      ULINE.
    ENDAT.

    AT LAST.
      ULINE.
    ENDAT.

  ENDLOOP.

END-OF-SELECTION.

************************************************************************
*  This section gets the object class
************************************************************************
FORM GET_OBJECT_CLASS.

     CLEAR KSSK.
     SELECT SINGLE * FROM KSSK
     WHERE  OBJEK EQ AUSP-OBJEK
     AND    MAFID EQ AUSP-MAFID
     AND    KLART EQ AUSP-KLART
     AND    ADZHL EQ '0000'.

     CLEAR KLAH.
     SELECT SINGLE * FROM KLAH
     WHERE  CLINT EQ KSSK-CLINT.

ENDFORM.

************************************************************************
*  This section outputs the report headings
************************************************************************
FORM WRITE_HEADER.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 SY-DATUM
            , 037 TEXT-001
            , 077 TEXT-002,  SY-PAGNO
            , 088 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 032 TEXT-003
            ,     SY-REPID UNDER TEXT-002
            , 088 SY-VLINE.
     WRITE:   /01 SY-VLINE
            , 003 TEXT-004
            ,     REPTAB-CLASS
            , 056 RATNAM
            , 088 SY-VLINE.
     ULINE.
     WRITE:   /03 TEXT-005
            , 014 TEXT-006
            , 057 TEXT-007.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

************************************************************************
*  This section outputs the detail lines to the report
************************************************************************
FORM WRITE_DETAIL.
     WRITE: /     RMATNR           UNDER TEXT-005
            ,     RMAKTX           UNDER TEXT-006
            ,     REPTAB-ATWRT     UNDER TEXT-007.
     PERFORM SHOWVLINE.
ENDFORM.

************************************************************************
*  These sections write vertical lines between the columns of the report
************************************************************************
FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 012 SY-VLINE
            , 055 SY-VLINE
            , 088 SY-VLINE.
ENDFORM.

************************************************************************
*  This is the end, my freind
************************************************************************
