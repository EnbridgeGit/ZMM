
REPORT ZMMMR065 NO STANDARD PAGE HEADING LINE-COUNT 65
                                         LINE-SIZE 255 MESSAGE-ID ZS.
************************************************************************
*  Client:     Spectra Energy.                                         *
*                                                                      *
*  Date:       June, 2010                                              *
*  Author:     Glenn Ymana                                             *
*  Program Description:                                                *
*  This program will download MDM data in the EAST                     *
*-----------------------------------------------------------------------
* Change History
*-----------------------------------------------------------------------
* Chng.Req#  |Date       |Developer       |Description
*-----------------------------------------------------------------------
*
*
************************************************************************

TABLES: MARA,         "General Material Data
        MAKT,         "Material Descriptions
        MARD,         "Storage Location Data for Material
        KSSK,         "Allocation Table: Object to Class
        KLAH,         "Class Header Data
        KSML,         "Characteristics of a Class
        AUSP,         "Characteristic Values
        CABN,         "Characteristic
        EINA,         "Purchasing Info Record: General Data
        LFA1,         "Vendor Master (General Section)
        T006A.        "Unit of Measurement

TYPES: I_REC TYPE STRING.

*DATA FROM MARA and MAKT.
DATA: BEGIN OF ITAB_MATL OCCURS 0,         "Data from MARA & MAKT Tables
          MATNR LIKE MARA-MATNR,
          LVORM LIKE MARA-LVORM,
          MTART LIKE MARA-MTART,
          MATKL LIKE MARA-MATKL,
          BISMT LIKE MARA-BISMT,
          MEINS LIKE MARA-MEINS,
          MSTAE LIKE MARA-MSTAE,
          BSTME LIKE MARA-BSTME,
          MAKTX LIKE MAKT-MAKTX,
          MFRPN LIKE MARA-MFRPN,
      END OF ITAB_MATL.

DATA: BEGIN OF ITAB_MAT OCCURS 0,         "Output order of ITAB_MAT
          MATNR LIKE MARA-MATNR,
          ZZLOC(1) TYPE C,
          LVORM LIKE MARA-LVORM,
          MTART LIKE MARA-MTART,
          MATKL LIKE MARA-MATKL,
          BISMT LIKE MARA-BISMT,
          MEINS LIKE T006A-MSEH3,
          MSTAE LIKE MARA-MSTAE,
          BSTME LIKE MARA-BSTME,
          MAKTX LIKE MAKT-MAKTX,
          MFRPN LIKE MARA-MFRPN,
          ZCDATA(30) TYPE C,
          ZQUTY(13)  TYPE N,
          ZMPOT      TYPE STRING,
          ZIRCT(3)   TYPE N,
          ZCLCT(3)   TYPE N,
      END OF ITAB_MAT.

DATA: BEGIN OF ITAB_CLASS OCCURS 0,     "Classes Assigned to Material
          OBJEK LIKE KSSK-OBJEK,
          KLART LIKE KSSK-KLART,
         CLINT LIKE KSSK-CLINT,
          CLASS LIKE KLAH-CLASS,
          ZAEHL LIKE KSSK-ZAEHL,
          STATU LIKE KSSK-STATU,
          ZNOCF(3) TYPE N,
      END OF ITAB_CLASS.

DATA: ITAB_CLASHDR LIKE ITAB_CLASS OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF ITAB_CHAR OCCURS 0,     "Characteristics
          OBJEK LIKE KSSK-OBJEK,
          CLINT LIKE KSSK-CLINT,
          ZAEHL LIKE KSSK-ZAEHL,
          POSNR LIKE KSML-POSNR,
          ATINN LIKE AUSP-ATINN,
          ATZHL LIKE AUSP-ATZHL,
          ATNAM LIKE CABN-ATNAM,
          ATFOR LIKE CABN-ATFOR,
          ATERF LIKE CABN-ATERF,
          ATEIN LIKE CABN-ATEIN,
          ATSON LIKE CABN-ATSON,
          ATWRT LIKE AUSP-ATWRT,
          ATFLV LIKE AUSP-ATFLV,
          ATAWE LIKE AUSP-ATAWE,
          MSEHI LIKE CABN-MSEHI,
          ZCDATA(30) TYPE C,
      END OF ITAB_CHAR.

DATA: BEGIN OF ITAB_CLASVAL OCCURS 0,
          OBJEK LIKE KSSK-OBJEK,
          CLINT LIKE KSSK-CLINT,
          ZAEHL LIKE KSSK-ZAEHL,
          POSNR LIKE KSML-POSNR,
          ATINN LIKE AUSP-ATINN,
          ATZHL LIKE AUSP-ATZHL,
          ATNAM LIKE CABN-ATNAM,
          ATFOR LIKE CABN-ATFOR,
          ATERF LIKE CABN-ATERF,
          ATEIN LIKE CABN-ATEIN,
          ATSON LIKE CABN-ATSON,
          ATWRT LIKE AUSP-ATWRT,
          ATFLV LIKE AUSP-ATFLV,
          ATAWE LIKE T006A-MSEH3,
          MSEHI LIKE T006A-MSEH3,
          ZCDATA(30) TYPE C,
      END OF ITAB_CLASVAL.


DATA: BEGIN OF ITAB_INFO OCCURS 0,          "Info Record
          MATNR LIKE EINA-MATNR,
          INFNR LIKE EINA-INFNR,
          LIFNR LIKE EINA-LIFNR,
          NAME1 LIKE LFA1-NAME1,
          NAME2 LIKE LFA1-NAME2,
          NAME3 LIKE LFA1-NAME3,
          NAME4 LIKE LFA1-NAME4,
          MEINS LIKE EINA-MEINS,
          IDNLF LIKE EINA-IDNLF,
          RELIF LIKE EINA-RELIF,
 	   ZIRTX TYPE STRING,
      END OF ITAB_INFO.

DATA: BEGIN OF ITAB_IR OCCURS 0,          "Info Record
          MATNR LIKE EINA-MATNR,
          INFNR LIKE EINA-INFNR,
          LIFNR LIKE EINA-LIFNR,
          NAME1 LIKE LFA1-NAME1,
          NAME2 LIKE LFA1-NAME2,
          NAME3 LIKE LFA1-NAME3,
          NAME4 LIKE LFA1-NAME4,
          MEINS LIKE T006A-MSEH3,
          IDNLF LIKE EINA-IDNLF,
          RELIF LIKE EINA-RELIF,
 	   ZIRTX TYPE STRING,
      END OF ITAB_IR.

DATA: BEGIN OF ITAB_MAT2 OCCURS 0,         "Calculatd fields of ITAB_MAT
          MATNR LIKE MARA-MATNR,           "Material Number
          ZCDATA(30) TYPE C,               "Manufcturer Name
          ZIRCT(3)   TYPE N,               "Number of Info Records
          ZCLCT(3)   TYPE N,               "Number of Classes
      END   OF ITAB_MAT2.

DATA: BEGIN OF MAT_HEADER,
          MATNR(8)  TYPE C VALUE 'Material',
          ZZLOC(1)  TYPE C VALUE 'L',
          LVORM(1)  TYPE C VALUE 'D',
          MTART(4)  TYPE C VALUE 'Mtyp',
          MATKL(6)  TYPE C VALUE 'MatGrp',
          BISMT(6)  TYPE C VALUE 'OldMat',
          MEINS(2)  TYPE C VALUE 'BM',
          MSTAE(2)  TYPE C VALUE 'MS',
          BSTME(3)  TYPE C VALUE 'PBM',
          MAKTX(11) TYPE C VALUE 'Description',
          MFRPN(8)  TYPE C VALUE 'ManPrNum',
          ZCDATA(6) TYPE C VALUE 'ManNam',
          ZQUTY(2)  TYPE C VALUE 'TS',
          ZMPOT(9)  TYPE C VALUE 'MatPOText',
          ZIRCT(3)  TYPE C VALUE 'NIR',
          ZCLCT(3)  TYPE C VALUE 'NCL',
      END OF MAT_HEADER.

DATA: BEGIN OF CLASS_HEADER,
          KLART(3)  TYPE C VALUE 'CTp',
          CLINT(4)  TYPE C VALUE 'CNum',
          CLASS(5)  TYPE C VALUE 'CName',
          ZAEHL(2)  TYPE C VALUE 'SI',
          STATU(1)  TYPE C VALUE 'S',
          ZNOCF(3)  TYPE C VALUE 'NCh',
      END OF CLASS_HEADER.

DATA: BEGIN OF CHAR_HEADER,
          ATINN(5)  TYPE C VALUE 'ChNum',
          ATZHL(3)  TYPE C VALUE 'ChC',
          ATNAM(6)  TYPE C VALUE 'ChName',
          ATFOR(4)  TYPE C VALUE 'DaTp',
          ATERF(1)  TYPE C VALUE 'R',
          ATEIN(1)  TYPE C VALUE 'S',
          ATSON(1)  TYPE C VALUE 'A',
          ZCDATA(7) TYPE C VALUE 'ChValue',
          ATAWE(3)  TYPE C VALUE 'UMP',
          MSEHI(3)  TYPE C VALUE 'UMC',
      END OF CHAR_HEADER.

DATA: BEGIN OF IR_HEADER,
          INFNR(3)  TYPE C VALUE 'IRN',
          LIFNR(3)  TYPE C VALUE 'Ven',
          NAME1(3)  TYPE C VALUE 'VN1',
          NAME2(3)  TYPE C VALUE 'VN2',
          NAME3(3)  TYPE C VALUE 'VN3',
          NAME4(3)  TYPE C VALUE 'VN4',
          MEINS(3)  TYPE C VALUE 'POM',
          IDNLF(5)  TYPE C VALUE 'VMatN',
          RELIF(1)  TYPE C VALUE 'R',
          ZIRTX(8)  TYPE C VALUE 'InfoText',
      END OF IR_HEADER.

DATA: BEGIN OF DUMMY_CLASS,
          F01(1)  TYPE C VALUE ' ',
          F02(1)  TYPE C VALUE ' ',
          F03(1)  TYPE C VALUE ' ',
          F04(1)  TYPE C VALUE ' ',
          F05(1)  TYPE C VALUE ' ',
          F06(1)  TYPE C VALUE ' ',
      END OF DUMMY_CLASS.

DATA: BEGIN OF DUMMY_CHAR,
          F01(1)  TYPE C VALUE ' ',
          F02(1)  TYPE C VALUE ' ',
          F03(1)  TYPE C VALUE ' ',
          F04(1)  TYPE C VALUE ' ',
          F05(1)  TYPE C VALUE ' ',
          F06(1)  TYPE C VALUE ' ',
          F07(1)  TYPE C VALUE ' ',
          F08(1)  TYPE C VALUE ' ',
          F09(1)  TYPE C VALUE ' ',
          F10(1)  TYPE C VALUE ' ',
      END OF DUMMY_CHAR.

DATA: BEGIN OF DUMMY_IR,
          F01(1)  TYPE C VALUE ' ',
          F02(1)  TYPE C VALUE ' ',
          F03(1)  TYPE C VALUE ' ',
          F04(1)  TYPE C VALUE ' ',
          F05(1)  TYPE C VALUE ' ',
          F06(1)  TYPE C VALUE ' ',
          F07(1)  TYPE C VALUE ' ',
          F08(1)  TYPE C VALUE ' ',
          F09(1)  TYPE C VALUE ' ',
          F10(1)  TYPE C VALUE ' ',
      END OF DUMMY_IR.

DATA: MT_TAB TYPE STANDARD TABLE OF I_REC,
      M_REC TYPE I_REC,
      M_CNT TYPE I.

DATA: WA_IRNUM(20)      TYPE C,
      ZCDATA_FOUND(1)   TYPE C,
      WA_ZAEHL(5)       TYPE N,
      WA_ZCDATA         LIKE RSCVP-VALC2,
      WA_ATNAM(30)      TYPE C VALUE 'MANUFACTURER_NAME             ',
      WA_UNIT_INT       LIKE T006A-MSEHI,
      WA_UNIT_EXT       LIKE T006A-MSEH3,
      WA_MAXCLSCNT      TYPE I VALUE 0,
      WA_MAXCHRCNT      TYPE I VALUE 0,
      WA_MAXIRCNT       TYPE I VALUE 0,
      WA_MAXDUMCLS      TYPE I VALUE 0,
      WA_MAXDUMCHR      TYPE I VALUE 0,
      WA_MAXDUMIR       TYPE I VALUE 0,
      OUT_STRING        TYPE STRING,
      MAT_STRING        TYPE STRING,
      CLASS_STRING      TYPE STRING,
      IR_STRING         TYPE STRING.

DATA: TNAME LIKE THEAD-TDNAME.
DATA: BEGIN OF LTEXT OCCURS 50.
        INCLUDE STRUCTURE TLINE.
DATA: END OF LTEXT.

DATA: BEGIN OF LTEXT2 OCCURS 50.
        INCLUDE STRUCTURE TLINE.
DATA: END OF LTEXT2.

DATA: TABCHAR TYPE X VALUE '09'.
DATA: MSG_TEXT(50).

************************************************************************
*                   Selection Screen                                   *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS: S_MATNR  FOR MARA-MATNR,         "Material Number
                S_LVORM  FOR MARA-LVORM
                             NO INTERVALS,       "Material Deletion Flag
                S_MTART  FOR MARA-MTART,         "Material Type
                S_MSTAE  FOR MARA-MSTAE,         "Mat.Cross Plant Status
                S_MATKL  FOR MARA-MATKL,         "Material Group
                S_MAKTX  FOR MAKT-MAKTX
                             NO INTERVALS.       "Material Description

* PARAMETERS: P_LVORM.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_LOCAL RADIOBUTTON GROUP GR1 DEFAULT 'X',
            P_UNIX  RADIOBUTTON GROUP GR1.

SELECTION-SCREEN SKIP.
PARAMETERS: P_LOCFIL LIKE FILENAME-FILEEXTERN,
            P_OUTFIL LIKE FILENAME-FILEEXTERN.
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX1.

************************************************************************
*                   INITIALIZATION                                     *
************************************************************************
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/CFMM001'
              '/mdmdata.dat' INTO P_OUTFIL.
  CONCATENATE 'C:/SAPTEMP/' 'mdmdata.dat' INTO P_LOCFIL.

************************************************************************
*                 AT SELECTION-SCREEN                                  *
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LOCFIL.
   PERFORM SEARCH_FILENAME USING P_LOCFIL.

************************************************************************
*                   START-OF-SELECTION                                 *
************************************************************************
START-OF-SELECTION.

  PERFORM COLLECT_PART_01_DATA.

************************************************************************
*                END-OF-SELECTION                                      *
************************************************************************
END-OF-SELECTION.
  PERFORM CREATE_FILE.

************************************************************************
*                   COLLECT_PART_01_DATA                               *
*              Data from MARA, MAKT & MARD Tables                      *
************************************************************************
FORM COLLECT_PART_01_DATA.

  REFRESH: ITAB_MAT, ITAB_MATL.
  CLEAR:   ITAB_MAT, ITAB_MATL.

  SELECT MARA~MATNR MARA~LVORM MARA~MTART MARA~MATKL MARA~BISMT
         MARA~MEINS MARA~MSTAE MARA~BSTME MAKT~MAKTX MARA~MFRPN
    INTO TABLE ITAB_MATL
    FROM ( MARA INNER JOIN MAKT
             ON MAKT~MATNR = MARA~MATNR )
   WHERE MARA~MATNR  IN S_MATNR
     AND MARA~MTART  IN S_MTART
     AND MARA~MSTAE  IN S_MSTAE
     AND MARA~MATKL  IN S_MATKL
     AND MAKT~MAKTX  IN S_MAKTX
     AND MARA~LVORM  IN S_LVORM
     AND MAKT~SPRAS  EQ 'EN'
   ORDER BY MARA~MATNR.

  LOOP AT ITAB_MATL.
    MOVE-CORRESPONDING ITAB_MATL TO ITAB_MAT.

*   Find correct unit of measure

    PERFORM F_LOOKUP_MEINS USING ITAB_MATL-MEINS
                        CHANGING ITAB_MAT-MEINS.
    PERFORM F_LOOKUP_MEINS USING ITAB_MATL-BSTME
                        CHANGING ITAB_MAT-BSTME.
    APPEND ITAB_MAT.
  ENDLOOP.

  LOOP AT ITAB_MAT.

    MOVE 'U' TO ITAB_MAT-ZZLOC.

* Calculate the Total Stock for each Material

    SELECT * FROM MARD
       WHERE MATNR = ITAB_MAT-MATNR.

      IF SY-SUBRC = 0.
        ITAB_MAT-ZQUTY = ITAB_MAT-ZQUTY + MARD-LABST + MARD-UMLME +
                         MARD-INSME + MARD-SPEME.
      ENDIF.
    ENDSELECT.

    MODIFY ITAB_MAT.

* Gather Material Purchase Order Text

    MOVE ITAB_MAT-MATNR TO TNAME.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID             = 'BEST'
        LANGUAGE       = 'E'
        NAME           = TNAME
        OBJECT         = 'MATERIAL'
      TABLES
        LINES                   = LTEXT
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    LOOP AT LTEXT.
      IF LTEXT-TDLINE NE ''.
         CONCATENATE ITAB_MAT-ZMPOT LTEXT-TDLINE
                INTO ITAB_MAT-ZMPOT
                SEPARATED BY '|'.
      ENDIF.
    ENDLOOP.
    CONDENSE ITAB_MAT-ZMPOT.
    SHIFT ITAB_MAT-ZMPOT LEFT DELETING LEADING '|'.

    MOVE ITAB_MAT-MATNR TO ITAB_MAT2-MATNR.

    PERFORM COLLECT_PART_2B_DATA.
    PERFORM COLLECT_PART_2D_DATA.

    MOVE ITAB_MAT2-ZCDATA TO ITAB_MAT-ZCDATA.
    MOVE ITAB_MAT2-ZCLCT TO ITAB_MAT-ZCLCT.
    MOVE ITAB_MAT2-ZIRCT TO ITAB_MAT-ZIRCT.

    MODIFY ITAB_MAT.
    REFRESH LTEXT.

*   Determine largest number of Classes &
*   IR records for this run

    IF ITAB_MAT2-ZCLCT > WA_MAXCLSCNT.
       MOVE ITAB_MAT2-ZCLCT TO WA_MAXCLSCNT.
    ENDIF.
    IF ITAB_MAT2-ZIRCT > WA_MAXIRCNT.
       MOVE ITAB_MAT2-ZIRCT TO WA_MAXIRCNT.
    ENDIF.

    CLEAR ITAB_MAT2.

  ENDLOOP.
ENDFORM.                    "COLLECT_PART_01_DATA

************************************************************************
*                   COLLECT_PART_2B_DATA                               *
*                                                                      *
************************************************************************
FORM COLLECT_PART_2B_DATA.

  REFRESH ITAB_CLASS.
  CLEAR   ITAB_CLASS.

  SELECT KSSK~OBJEK KSSK~KLART KSSK~CLINT KLAH~CLASS
         KSSK~ZAEHL KSSK~STATU
    INTO TABLE ITAB_CLASS
    FROM ( KSSK INNER JOIN KLAH
             ON KSSK~CLINT = KLAH~CLINT
            AND KSSK~KLART = KLAH~KLART )
   WHERE KSSK~OBJEK = ITAB_MAT-MATNR
     AND KSSK~KLART = '001'
   ORDER BY KSSK~OBJEK KSSK~KLART KSSK~ZAEHL KSSK~CLINT.

  LOOP AT ITAB_CLASS.

    ADD +1 TO ITAB_MAT2-ZCLCT.
    CLEAR ITAB_CLASS-ZNOCF.

    PERFORM COLLECT_PART_2C_DATA.
    MODIFY ITAB_CLASS.

*   Determine largest number of Characteristics
*   Per Class for this run.

    IF ITAB_CLASS-ZNOCF > WA_MAXCHRCNT.
       MOVE ITAB_CLASS-ZNOCF TO WA_MAXCHRCNT.
    ENDIF.

  ENDLOOP.

  APPEND LINES OF ITAB_CLASS TO ITAB_CLASHDR.

ENDFORM.                    "COLLECT_PART_2B_DATA

************************************************************************
*                   COLLECT_PART_2C_DATA                               *
*                                                                      *
************************************************************************
FORM COLLECT_PART_2C_DATA.

  REFRESH ITAB_CHAR.
  CLEAR   ITAB_CHAR.
  MOVE 'N' TO ZCDATA_FOUND.

  SELECT KSSK~OBJEK KSSK~CLINT KSSK~ZAEHL KSML~POSNR AUSP~ATINN
         AUSP~ATZHL CABN~ATNAM CABN~ATFOR CABN~ATERF CABN~ATEIN
         CABN~ATSON AUSP~ATWRT AUSP~ATFLV AUSP~ATAWE CABN~MSEHI
    INTO TABLE ITAB_CHAR
    FROM ( KSSK
           INNER JOIN KSML
           ON KSSK~CLINT = KSML~CLINT
           AND KSSK~KLART = KSML~KLART
           INNER JOIN AUSP
           ON AUSP~ATINN = KSML~IMERK
           AND AUSP~KLART = KSML~KLART
           AND AUSP~OBJEK = KSSK~OBJEK
           INNER JOIN CABN
           ON CABN~ATINN = AUSP~ATINN )
   WHERE KSSK~OBJEK = ITAB_CLASS-OBJEK
     AND AUSP~KLART = '001'
     AND KSSK~CLINT = ITAB_CLASS-CLINT
   ORDER BY KSSK~OBJEK KSSK~ZAEHL KSSK~CLINT KSML~POSNR AUSP~ATZHL.

  IF SY-SUBRC = 0.

    LOOP AT ITAB_CHAR.
      IF ITAB_CHAR-ATFOR = 'CHAR'.
         ITAB_CHAR-ZCDATA = ITAB_CHAR-ATWRT.
      ENDIF.

      IF ITAB_CHAR-ATFOR = 'NUM'.

         CALL FUNCTION 'FLTP_CHAR_CONV_FROM_SI_RFC'
           EXPORTING
             fltp_value_si     = ITAB_CHAR-ATFLV
             number_digits     = '30'
             number_decimals   = '3'
             unit_is_optional  = 'X'
           IMPORTING
             char_value        = WA_ZCDATA.

         MOVE WA_ZCDATA TO ITAB_CHAR-ZCDATA.
      ENDIF.

      IF ITAB_CHAR-ATNAM = WA_ATNAM AND
            ZCDATA_FOUND = 'N'.
         MOVE ITAB_CHAR-ZCDATA TO ITAB_MAT2-ZCDATA.
         MOVE 'Y' TO ZCDATA_FOUND.
      ENDIF.

      ADD +1 TO ITAB_CLASS-ZNOCF.
      MODIFY ITAB_CHAR.
      MOVE-CORRESPONDING ITAB_CHAR TO ITAB_CLASVAL.

*     Find correct unit of measure

      PERFORM F_LOOKUP_MEINS USING ITAB_CHAR-ATAWE
                         CHANGING ITAB_CLASVAL-ATAWE.
      PERFORM F_LOOKUP_MEINS USING ITAB_CHAR-MSEHI
                         CHANGING ITAB_CLASVAL-MSEHI.

      APPEND ITAB_CLASVAL.

    ENDLOOP.

  ENDIF.
ENDFORM.                    "COLLECT_PART_2C_DATA

************************************************************************
*                   COLLECT_PART_2D_DATA                               *
*                                                                      *
************************************************************************
FORM COLLECT_PART_2D_DATA.

  REFRESH ITAB_INFO.
  CLEAR   ITAB_INFO.

  SELECT EINA~MATNR EINA~INFNR EINA~LIFNR LFA1~NAME1 LFA1~NAME2
         LFA1~NAME3 LFA1~NAME4 EINA~MEINS EINA~IDNLF EINA~RELIF
    INTO TABLE ITAB_INFO
    FROM ( EINA INNER JOIN LFA1
             ON EINA~LIFNR = LFA1~LIFNR )
   WHERE EINA~MATNR = ITAB_MAT-MATNR
     AND EINA~LOEKZ <> 'X'
   ORDER BY EINA~MATNR EINA~INFNR.

  IF SY-SUBRC = 0.

    LOOP AT ITAB_INFO.

      CONCATENATE ITAB_INFO-INFNR 'MATL0'
             INTO WA_IRNUM.
*   Gather Material Purchase Order Text

      MOVE WA_IRNUM TO TNAME.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          ID                      = 'BT'
          LANGUAGE                = 'E'
          NAME                    = TNAME
          OBJECT                  = 'EINE'
        TABLES
          LINES                   = LTEXT2
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      LOOP AT LTEXT2.
        IF LTEXT2-TDLINE NE ''.
           CONCATENATE ITAB_INFO-ZIRTX LTEXT2-TDLINE
                  INTO ITAB_INFO-ZIRTX
                  SEPARATED BY '|'.
        ENDIF.
      ENDLOOP.
      CONDENSE ITAB_INFO-ZIRTX.
      SHIFT ITAB_INFO-ZIRTX LEFT DELETING LEADING '|'.
      MODIFY ITAB_INFO.
      REFRESH LTEXT2.

      MOVE-CORRESPONDING ITAB_INFO TO ITAB_IR.

*     Find correct unit of measure

      PERFORM F_LOOKUP_MEINS USING ITAB_INFO-MEINS
                          CHANGING ITAB_IR-MEINS.
      APPEND ITAB_IR.
      ADD +1 TO ITAB_MAT2-ZIRCT.

    ENDLOOP.

  ENDIF.

ENDFORM.                    "COLLECT_PART_2D_DATA

************************************************************************
*    LOOKUP UNIT OF MEASURE                                            *
************************************************************************
FORM F_LOOKUP_MEINS USING IUNIT_INT CHANGING EUNIT_EXT.
     CLEAR EUNIT_EXT.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          INPUT                   = IUNIT_INT
          LANGUAGE                = 'E'
        IMPORTING
*          LONG_TEXT               = TEXT_LONG
          OUTPUT                  = EUNIT_EXT
*          SHORT_TEXT              = TEXT_SHORT
        EXCEPTIONS
          UNIT_NOT_FOUND          = 01.
          .
ENDFORM.

************************************************************************
*      CREATE FILE                                                     *
************************************************************************
FORM CREATE_FILE.

DATA: L_FILE TYPE STRING,
      L_SUBRC TYPE SY-SUBRC.

*- Output/Create Extract File
   IF NOT ( P_UNIX IS INITIAL ).
      OPEN DATASET P_OUTFIL FOR OUTPUT IN TEXT MODE MESSAGE MSG_TEXT.
      IF SY-SUBRC NE 0.
         MESSAGE I002 WITH P_OUTFIL MSG_TEXT.
         STOP.
      ENDIF.
   ENDIF.

   PERFORM GENERATE_HEADERS.

   LOOP AT ITAB_MAT.

   CLEAR: MAT_STRING, CLASS_STRING, IR_STRING, OUT_STRING.

*  WRITE ITAB_MAT RECORDS
     CONCATENATE ITAB_MAT-MATNR ITAB_MAT-ZZLOC ITAB_MAT-LVORM
                 ITAB_MAT-MTART ITAB_MAT-MATKL ITAB_MAT-BISMT
                 ITAB_MAT-MEINS ITAB_MAT-MSTAE ITAB_MAT-BSTME
                 ITAB_MAT-MAKTX ITAB_MAT-MFRPN ITAB_MAT-ZCDATA
                 ITAB_MAT-ZQUTY ITAB_MAT-ZMPOT ITAB_MAT-ZIRCT
                 ITAB_MAT-ZCLCT
                 INTO MAT_STRING SEPARATED BY TABCHAR.

*  WRITE ITAB_CLASHDR / ITAB_CLASVAL RECORDS
     LOOP AT ITAB_CLASHDR
             WHERE OBJEK EQ ITAB_MAT-MATNR.

       MOVE ITAB_CLASHDR-ZAEHL TO WA_ZAEHL.

       CONCATENATE CLASS_STRING
                   ITAB_CLASHDR-KLART ITAB_CLASHDR-CLINT
                   ITAB_CLASHDR-CLASS WA_ZAEHL
                   ITAB_CLASHDR-STATU ITAB_CLASHDR-ZNOCF
                   INTO CLASS_STRING SEPARATED BY TABCHAR.

*      WRITE ITAB_CLASVAL RECORDS FOR EACH CLASHDR (CLASS REC)

       LOOP AT ITAB_CLASVAL
               WHERE OBJEK EQ ITAB_MAT-MATNR
                 AND CLINT EQ ITAB_CLASHDR-CLINT.
         CONCATENATE CLASS_STRING
                     ITAB_CLASVAL-ATINN ITAB_CLASVAL-ATZHL
                     ITAB_CLASVAL-ATNAM ITAB_CLASVAL-ATFOR
                     ITAB_CLASVAL-ATERF ITAB_CLASVAL-ATEIN
                     ITAB_CLASVAL-ATSON ITAB_CLASVAL-ZCDATA
                     ITAB_CLASVAL-ATAWE ITAB_CLASVAL-MSEHI
                     INTO CLASS_STRING SEPARATED BY TABCHAR.
       ENDLOOP.

*If the No. of Characteristics is less than the Max Char count. Add the
*    appropriate no. of dummy characteristics to bring it up to the max.

       IF WA_MAXCHRCNT > ITAB_CLASHDR-ZNOCF.
          WA_MAXDUMCHR = WA_MAXCHRCNT - ITAB_CLASHDR-ZNOCF.

          DO WA_MAXDUMCHR TIMES.
             CONCATENATE CLASS_STRING
                         DUMMY_CHAR-F01 DUMMY_CHAR-F02 DUMMY_CHAR-F03
                         DUMMY_CHAR-F04 DUMMY_CHAR-F05 DUMMY_CHAR-F06
                         DUMMY_CHAR-F07 DUMMY_CHAR-F08 DUMMY_CHAR-F09
                         DUMMY_CHAR-F10
                         INTO CLASS_STRING SEPARATED BY TABCHAR.
          ENDDO.
        ENDIF.
     ENDLOOP.

*     SHIFT CLASS_STRING LEFT DELETING LEADING TABCHAR.

*    If the No. of Classes is less than the Max Class count. Add the
*    appropriate no. of dummy classes to bring it up to the max.

     IF WA_MAXCLSCNT > ITAB_MAT-ZCLCT.
        WA_MAXDUMCLS = WA_MAXCLSCNT - ITAB_MAT-ZCLCT.

        DO WA_MAXDUMCLS TIMES.
           CONCATENATE CLASS_STRING
                       DUMMY_CLASS-F01 DUMMY_CLASS-F02 DUMMY_CLASS-F03
                       DUMMY_CLASS-F04 DUMMY_CLASS-F05 DUMMY_CLASS-F06
                       INTO CLASS_STRING SEPARATED BY TABCHAR.

           DO WA_MAXCHRCNT TIMES.
              CONCATENATE CLASS_STRING
                          DUMMY_CHAR-F01 DUMMY_CHAR-F02 DUMMY_CHAR-F03
                          DUMMY_CHAR-F04 DUMMY_CHAR-F05 DUMMY_CHAR-F06
                          DUMMY_CHAR-F07 DUMMY_CHAR-F08 DUMMY_CHAR-F09
                          DUMMY_CHAR-F10
                          INTO CLASS_STRING SEPARATED BY TABCHAR.
           ENDDO.
        ENDDO.

     ENDIF.

     SHIFT CLASS_STRING LEFT BY 1 PLACES.

*  WRITE ITAB_IR

     LOOP AT ITAB_IR
             WHERE MATNR EQ ITAB_MAT-MATNR.
       CONCATENATE IR_STRING
                   ITAB_IR-INFNR ITAB_IR-LIFNR ITAB_IR-NAME1
                   ITAB_IR-NAME2 ITAB_IR-NAME3 ITAB_IR-NAME4
                   ITAB_IR-MEINS ITAB_IR-IDNLF ITAB_IR-RELIF
 	            ITAB_IR-ZIRTX
                   INTO IR_STRING SEPARATED BY TABCHAR.
     ENDLOOP.

*    If the No. of Info records is less than the Max IR count. Add the
*    appropriate no. of dummy info records to bring it up to the max.

     IF WA_MAXIRCNT > ITAB_MAT-ZIRCT.
        WA_MAXDUMIR = WA_MAXIRCNT - ITAB_MAT-ZIRCT.

        DO WA_MAXDUMIR TIMES.
           CONCATENATE IR_STRING
                       DUMMY_CHAR-F01 DUMMY_CHAR-F02 DUMMY_CHAR-F03
                       DUMMY_CHAR-F04 DUMMY_CHAR-F05 DUMMY_CHAR-F06
                       DUMMY_CHAR-F07 DUMMY_CHAR-F08 DUMMY_CHAR-F09
                       DUMMY_CHAR-F10
                       INTO IR_STRING SEPARATED BY TABCHAR.
        ENDDO.
     ENDIF.

     SHIFT IR_STRING LEFT DELETING LEADING TABCHAR.

     CONCATENATE MAT_STRING CLASS_STRING IR_STRING
                 INTO OUT_STRING SEPARATED BY TABCHAR.
     PERFORM WRITE_REC.

   ENDLOOP.

*- Close Output File
   IF NOT ( P_UNIX IS INITIAL ).
      CLOSE DATASET P_OUTFIL.
      IF NOT ( M_CNT IS INITIAL ).
         MESSAGE I034.
      ENDIF.

*- Create Local File
   ELSE.
      L_FILE = P_LOCFIL.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
        EXPORTING
          FILENAME                  = L_FILE
          FILETYPE                  = 'ASC'
        CHANGING
          DATA_TAB                  = MT_TAB
         EXCEPTIONS
           FILE_WRITE_ERROR          = 1
           NO_BATCH                  = 2
           GUI_REFUSE_FILETRANSFER   = 3
           INVALID_TYPE              = 4
           NO_AUTHORITY              = 5
           UNKNOWN_ERROR             = 6
           HEADER_NOT_ALLOWED        = 7
           SEPARATOR_NOT_ALLOWED     = 8
           FILESIZE_NOT_ALLOWED      = 9
           HEADER_TOO_LONG           = 10
           DP_ERROR_CREATE           = 11
           DP_ERROR_SEND             = 12
           DP_ERROR_WRITE            = 13
           UNKNOWN_DP_ERROR          = 14
           ACCESS_DENIED             = 15
           DP_OUT_OF_MEMORY          = 16
           DISK_FULL                 = 17
           DP_TIMEOUT                = 18
           FILE_NOT_FOUND            = 19
           DATAPROVIDER_EXCEPTION    = 20
           CONTROL_FLUSH_ERROR       = 21
           NOT_SUPPORTED_BY_GUI      = 22
           ERROR_NO_GUI              = 23
           others                    = 24.

      IF SY-SUBRC EQ 0.
         MESSAGE I034.
      ELSEIF SY-SUBRC EQ 15.
         MESSAGE I033.
      ELSEIF SY-SUBRC NE 0.
         MESSAGE I002 WITH P_LOCFIL MSG_TEXT.
      ENDIF.

   ENDIF.
ENDFORM.

************************************************************************
*      GENERATE REPORT HEADERS                                         *
************************************************************************
FORM GENERATE_HEADERS.

   CLEAR: MAT_STRING, CLASS_STRING, IR_STRING, OUT_STRING.

*  Write Material Headers

   CONCATENATE MAT_HEADER-MATNR MAT_HEADER-ZZLOC MAT_HEADER-LVORM
               MAT_HEADER-MTART MAT_HEADER-MATKL MAT_HEADER-BISMT
               MAT_HEADER-MEINS MAT_HEADER-MSTAE MAT_HEADER-BSTME
               MAT_HEADER-MAKTX MAT_HEADER-MFRPN MAT_HEADER-ZCDATA
               MAT_HEADER-ZQUTY MAT_HEADER-ZMPOT MAT_HEADER-ZIRCT
               MAT_HEADER-ZCLCT
               INTO MAT_STRING SEPARATED BY TABCHAR.

*  Write Class/Characteristics headers based on the maximum
*  Class & Char Counts

   DO WA_MAXCLSCNT TIMES.
      CONCATENATE CLASS_STRING
                  CLASS_HEADER-KLART CLASS_HEADER-CLINT
                  CLASS_HEADER-CLASS CLASS_HEADER-ZAEHL
                  CLASS_HEADER-STATU CLASS_HEADER-ZNOCF
                  INTO CLASS_STRING SEPARATED BY TABCHAR.

      DO WA_MAXCHRCNT TIMES.
         CONCATENATE CLASS_STRING
                     CHAR_HEADER-ATINN CHAR_HEADER-ATZHL
                     CHAR_HEADER-ATNAM CHAR_HEADER-ATFOR
                     CHAR_HEADER-ATERF CHAR_HEADER-ATEIN
                     CHAR_HEADER-ATSON CHAR_HEADER-ZCDATA
                     CHAR_HEADER-ATAWE CHAR_HEADER-MSEHI
                     INTO CLASS_STRING SEPARATED BY TABCHAR.
      ENDDO.
   ENDDO.
   SHIFT CLASS_STRING LEFT DELETING LEADING TABCHAR.

*  WRITE ITAB_IR

   DO WA_MAXIRCNT TIMES.
      CONCATENATE CLASS_STRING
                  IR_HEADER-INFNR IR_HEADER-LIFNR IR_HEADER-NAME1
                  IR_HEADER-NAME2 IR_HEADER-NAME3 IR_HEADER-NAME4
                  IR_HEADER-MEINS IR_HEADER-IDNLF IR_HEADER-RELIF
                  IR_HEADER-ZIRTX
                  INTO CLASS_STRING SEPARATED BY TABCHAR.
   ENDDO.

     SHIFT IR_STRING LEFT DELETING LEADING TABCHAR.

     CONCATENATE MAT_STRING CLASS_STRING IR_STRING
                 INTO OUT_STRING SEPARATED BY TABCHAR.
     PERFORM WRITE_REC.


ENDFORM.
************************************************************************
*      SEARCH_FILENAME                                                 *
************************************************************************
FORM SEARCH_FILENAME USING P_FILE.

DATA: L_DIR TYPE STRING,
      L_RC TYPE I,
      L_FILETABLE TYPE FILETABLE.

   L_DIR = 'G:\'.

*- Call Method to retrieve filename
   CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
     EXPORTING
       WINDOW_TITLE            = 'Select a file...'
       DEFAULT_EXTENSION       = 'CSV'
       INITIAL_DIRECTORY       = L_DIR
    CHANGING
       FILE_TABLE              = L_FILETABLE
       RC                      = L_RC
     EXCEPTIONS
       FILE_OPEN_DIALOG_FAILED = 1
       CNTL_ERROR              = 2
       ERROR_NO_GUI            = 3
       NOT_SUPPORTED_BY_GUI    = 4
       OTHERS                  = 5.

  CHECK SY-SUBRC EQ 0.
  READ TABLE L_FILETABLE INDEX 1 INTO P_FILE.

ENDFORM.                    " SEARCH_FILENAME

************************************************************************
*      Form  WRITE_REC
************************************************************************
FORM WRITE_REC.

*  WRITE: /1 OUT_STRING.

  IF NOT ( P_UNIX IS INITIAL ).
     TRANSFER OUT_STRING TO P_OUTFIL.

     IF SY-SUBRC EQ 0.
        ADD 1 TO M_CNT.
     ENDIF.
  ELSE.
     M_REC = OUT_STRING.
     APPEND M_REC TO MT_TAB.
  ENDIF.

ENDFORM.                    " WRITE_REC
