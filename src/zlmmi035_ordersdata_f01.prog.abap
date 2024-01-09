*&---------------------------------------------------------------------*
*&  Include           ZMMORDERSDATA_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :   ZMMORDERSDATA                                 *
* Include            :   ZMMORDERSDATA_F01                             *
* Author             :   Rajeshwar Reddy                               *
* Date               :   23-Jan-2020`                                   *
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
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PATH_APP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PATH_APP .

  IF RB_APP EQ 'X'.
*F4 help for file name on SAP application server
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        DIRECTORY        = '\\SAPFileShare.gtna.gt.ds\FI\DEV\Out\RPA\'
      IMPORTING
        SERVERFILE       = P_APP
      EXCEPTIONS
        CANCELED_BY_USER = 1
        OTHERS           = 2.
    IF SY-SUBRC <> 0.
      MESSAGE TEXT-011 TYPE 'I'.
    ENDIF.
  ENDIF.
ENDFORM.                    " PATH_APP
*&---------------------------------------------------------------------*
*&      Form  PATH_PRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PATH_PRS.
  DATA: FNAME       TYPE STRING,
        FPATH       TYPE STRING,
        FPATH_FULL  TYPE STRING,
        LV_MASK     TYPE STRING,
        LV_CMASK    TYPE CHAR255.
  DATA: LV_LEN      TYPE I,
        LV_EXTN     TYPE CHAR5,
        LV_TEXT     TYPE STRING.

  LV_CMASK = GC_XLSX.
  CALL METHOD CL_ALV_BDS=>CREATE_MASK_FOR_FILEFILTER
    EXPORTING
      I_FRONTEND          = 'Y'
*    IMPORTING
*      e_default_extension = 'XLSX'
    CHANGING
      C_MASK              = LV_CMASK.

  LV_MASK = LV_CMASK.
  IF RB_PRS EQ 'X'.
    LV_TEXT = TEXT-200.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
      EXPORTING
        WINDOW_TITLE         = LV_TEXT
        DEFAULT_EXTENSION    = 'XLSX'
        FILE_FILTER          = LV_MASK
      CHANGING
        FILENAME             = FNAME
        PATH                 = FPATH
        FULLPATH             = FPATH_FULL
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        NOT_SUPPORTED_BY_GUI = 3
        OTHERS               = 4.
    IF SY-SUBRC = 0.
      P_PRS = FPATH_FULL.
    ELSE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " PATH_PRS
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  IF SO_SLFDT IS INITIAL AND SO_LIFNR IS INITIAL
     AND SO_EINDT IS INITIAL.  "Added by DADIM for CHG0241607
    SELECT A~EBELN A~BSART A~ERNAM A~LIFNR A~EKGRP A~WAERS A~BEDAT A~SUBMI A~INCO1 A~ZZARIBA_APPROVER
           B~EBELP B~TXZ01 B~MATNR B~WERKS B~LGORT B~MATKL B~MENGE B~MEINS B~NETPR B~BEDNR B~KNTTP B~KONNR B~AFNAM
           C~ZEKKN C~SAKTO C~KOSTL C~AUFNR C~WEMPF C~PS_PSP_PNR C~NPLNR
      FROM EKKO AS A INNER JOIN EKPO AS B
*      ON A~EBELN EQ B~EBELN INNER JOIN EKKN AS C      "Commented by JOOKONTR for CHG0176546
      ON A~EBELN EQ B~EBELN LEFT OUTER JOIN EKKN AS C  "Added by JOOKONTR for CHG0176546
*      ON A~EBELN EQ C~EBELN AND B~EBELP EQ C~EBELP    "Commented by JOOKONTR for CHG0176546
      ON B~EBELN EQ C~EBELN AND B~EBELP EQ C~EBELP     "Added by JOOKONTR for CHG0176546
      INTO TABLE GT_DATA
      WHERE A~EBELN IN SO_EBELN
      AND A~BEDAT IN SO_BEDAT
*BOC changed by JOOKONTR for CHG0176546
*      AND A~BSART IN SO_BSART
      AND A~BSART IN ('NB','ZF','RP')
*EOC changed by JOOKONTR for CHG0176546
      AND A~EKGRP IN SO_EKGRP
      AND A~AEDAT IN SO_AEDAT.
    IF SY-SUBRC EQ 0.
      SORT GT_DATA BY EBELN EBELP.
      SELECT EBELN
             EBELP
             EINDT
             SLFDT FROM EKET
        INTO TABLE GT_EKET
        FOR ALL ENTRIES IN GT_DATA
        WHERE EBELN EQ GT_DATA-EBELN
        AND EBELP EQ GT_DATA-EBELP.
      IF SY-SUBRC EQ 0.
        SORT GT_EKET BY EBELN EBELP.
      ENDIF.
      SELECT EBELN
             EBELP
             GJAHR
             VGABE
             BELNR
             BUZEI
             BWART FROM EKBE
        INTO TABLE GT_EKBE
       FOR ALL ENTRIES IN GT_DATA
          WHERE EBELN EQ GT_DATA-EBELN
          AND EBELP EQ GT_DATA-EBELP
          AND VGABE EQ '1'
          AND BWART EQ '101'.
      IF SY-SUBRC EQ 0.
        SORT GT_EKBE BY EBELN EBELP.
      ENDIF.
      IF GT_EKBE IS NOT INITIAL.
        SELECT MBLNR
               MJAHR
               ZEILE
               EBELN
               EBELP
               ERFMG
               BUDAT_MKPF
               CPUDT_MKPF
               USNAM_MKPF FROM MSEG
          INTO TABLE GT_MSEG
          FOR ALL ENTRIES IN GT_EKBE
          WHERE MBLNR EQ GT_EKBE-BELNR
          AND ZEILE EQ GT_EKBE-BUZEI
          AND MJAHR EQ GT_EKBE-GJAHR
          AND EBELN EQ GT_EKBE-EBELN
          AND EBELP EQ GT_EKBE-EBELP.
      ENDIF.
      IF GT_MSEG IS NOT INITIAL.
        SORT GT_MSEG BY MBLNR MJAHR ZEILE EBELN EBELP.
        SELECT MBLNR
               MJAHR
               BLDAT
               USNAM FROM MKPF
          INTO TABLE GT_MKPF
          FOR ALL ENTRIES IN GT_MSEG
          WHERE MBLNR EQ GT_MSEG-MBLNR
          AND MJAHR EQ GT_MSEG-MJAHR.
        IF SY-SUBRC EQ 0.
          SORT GT_MKPF BY MBLNR MJAHR.
        ENDIF.
      ENDIF.
      SELECT LIFNR
             NAME1 FROM LFA1
        INTO TABLE GT_LFA1
        FOR ALL ENTRIES IN GT_DATA
        WHERE LIFNR = GT_DATA-LIFNR.
      IF  SY-SUBRC EQ 0.
        SORT GT_LFA1 BY LIFNR.
      ENDIF.
    ELSE.
      MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ELSE.
    GV_FLAG = ABAP_TRUE.
    REFRESH GT_EKET.
    SELECT EBELN
           EBELP
           EINDT
           SLFDT FROM EKET
    INTO TABLE GT_EKET
    WHERE SLFDT IN SO_SLFDT
      AND EINDT IN SO_EINDT.    "Added by DADIM for CHG0241607
    IF SY-SUBRC EQ 0.
      SORT GT_EKET BY EBELN EBELP.
      SELECT A~EBELN A~BSART A~ERNAM A~LIFNR A~EKGRP A~WAERS A~BEDAT A~SUBMI A~INCO1 A~ZZARIBA_APPROVER
             B~EBELP B~TXZ01 B~MATNR B~WERKS B~LGORT B~MATKL B~MENGE B~MEINS B~NETPR B~BEDNR B~KNTTP B~KONNR B~AFNAM
             C~ZEKKN C~SAKTO C~KOSTL C~AUFNR C~WEMPF C~PS_PSP_PNR C~NPLNR
        FROM EKKO AS A INNER JOIN EKPO AS B
*      ON A~EBELN EQ B~EBELN INNER JOIN EKKN AS C      "Commented by JOOKONTR for CHG0176546
      ON A~EBELN EQ B~EBELN LEFT OUTER JOIN EKKN AS C  "Added by JOOKONTR for CHG0176546
*      ON A~EBELN EQ C~EBELN AND B~EBELP EQ C~EBELP    "Commented by JOOKONTR for CHG0176546
      ON B~EBELN EQ C~EBELN AND B~EBELP EQ C~EBELP     "Added by JOOKONTR for CHG0176546
        INTO TABLE GT_DATA
        FOR ALL ENTRIES IN GT_EKET
        WHERE A~EBELN EQ GT_EKET-EBELN
        AND B~EBELP EQ GT_EKET-EBELP
        AND A~LIFNR IN SO_LIFNR
*BOC changed by JOOKONTR for CHG0176546
        AND A~BSART IN ('NB','ZF','RP').
*EOC changed by JOOKONTR for CHG0176546
      IF SY-SUBRC EQ 0.
        SORT GT_DATA BY EBELN EBELP.
        SELECT EBELN
               EBELP
               GJAHR
               VGABE
               BELNR
               BUZEI
               BWART FROM EKBE
          INTO TABLE GT_EKBE
  FOR ALL ENTRIES IN GT_DATA
               WHERE EBELN EQ GT_DATA-EBELN
                 AND EBELP EQ GT_DATA-EBELP
                AND VGABE EQ '1'
                AND BWART EQ '101'.
        IF SY-SUBRC EQ 0.
          SORT GT_EKBE BY EBELN EBELP.
        ENDIF.
        IF GT_EKBE IS NOT INITIAL.
          SELECT MBLNR
             MJAHR
             ZEILE
             EBELN
             EBELP
             ERFMG
             BUDAT_MKPF
             CPUDT_MKPF
             USNAM_MKPF FROM MSEG
        INTO TABLE GT_MSEG
        FOR ALL ENTRIES IN GT_EKBE
        WHERE MBLNR EQ GT_EKBE-BELNR
        AND ZEILE EQ GT_EKBE-BUZEI
        AND MJAHR EQ GT_EKBE-GJAHR
        AND EBELN EQ GT_EKBE-EBELN
        AND EBELP EQ GT_EKBE-EBELP.
        ENDIF.
        IF GT_MSEG IS NOT INITIAL.
          SORT GT_MSEG BY MBLNR MJAHR ZEILE EBELN EBELP.
          SELECT MBLNR
               MJAHR
               BLDAT
               USNAM FROM MKPF
          INTO TABLE GT_MKPF
          FOR ALL ENTRIES IN GT_MSEG
          WHERE MBLNR EQ GT_MSEG-MBLNR
          AND MJAHR EQ GT_MSEG-MJAHR.
          IF SY-SUBRC EQ 0.
            SORT GT_MKPF BY MBLNR MJAHR.
          ENDIF.
        ENDIF.
        SELECT LIFNR
               NAME1 FROM LFA1
          INTO TABLE GT_LFA1
          FOR ALL ENTRIES IN GT_DATA
          WHERE LIFNR = GT_DATA-LIFNR.
        IF  SY-SUBRC EQ 0.
          SORT GT_LFA1 BY LIFNR.
        ENDIF.
      ELSE.
        MESSAGE TEXT-014 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ELSE.
      MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PREP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREP_DATA .
  FIELD-SYMBOLS: <FS_DATA> TYPE GTY_DATA.
  DATA: LS_EKET   LIKE LINE OF GT_EKET,
        LS_ORDERS LIKE LINE OF GT_ORDERS,
        LS_OTD    LIKE LINE OF GT_OTD,
        LS_MSEG   LIKE LINE OF GT_MSEG,
        LS_LFA1   LIKE LINE OF GT_LFA1,
        LS_EKBE   LIKE LINE OF GT_EKBE,
        LS_MKPF   LIKE LINE OF GT_MKPF.
  DATA: LV_INDEX TYPE SY-TABIX.
*BOC changed by JOOKONTR for CHG0176546
  DATA: LV_DT(2) TYPE C,
        LV_MT(2) TYPE C,
        LV_YR(4) TYPE C,
        LV_TDATE(10) TYPE C.
*EOC changed by JOOKONTR for CHG0176546

  IF GV_FLAG NE ABAP_TRUE.
    LOOP AT GT_DATA ASSIGNING <FS_DATA>.
      LS_ORDERS-EBELN       = <FS_DATA>-EBELN.
      LS_ORDERS-EBELP       = <FS_DATA>-EBELP.
      IF <FS_DATA>-BEDAT IS NOT INITIAL.

**BOC added by JOOKONTR for CHG0176546
*       LS_ORDERS-BEDAT       = <FS_DATA>-BEDAT.
        CLEAR:LV_YR,LV_MT, LV_DT,LV_TDATE.
        LV_YR = <FS_DATA>-BEDAT+0(4).
        LV_MT = <FS_DATA>-BEDAT+4(2).
        LV_DT = <FS_DATA>-BEDAT+6(2).
        CONCATENATE LV_MT LV_DT LV_YR INTO LV_TDATE SEPARATED BY '/'.
        LS_ORDERS-BEDAT       = LV_TDATE.
**EOC added by JOOKONTR for CHG0176546
      ENDIF.

      LS_ORDERS-EKGRP       = <FS_DATA>-EKGRP.
      LS_ORDERS-KONNR       = <FS_DATA>-KONNR.
      CLEAR LS_LFA1.
      READ TABLE GT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = <FS_DATA>-LIFNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LS_ORDERS-NAME1       = LS_LFA1-NAME1.
      ENDIF.
      LS_ORDERS-TXZ01       = <FS_DATA>-TXZ01.
      LS_ORDERS-WERKS       = <FS_DATA>-WERKS.
      LS_ORDERS-NETPR       = <FS_DATA>-NETPR .
      LS_ORDERS-WAERS       = <FS_DATA>-WAERS.
      LS_ORDERS-BEDNR       = <FS_DATA>-BEDNR.
      LS_ORDERS-SUBMI       = <FS_DATA>-SUBMI.
      LS_ORDERS-MATNR       = <FS_DATA>-MATNR.
      LS_ORDERS-MATKL       = <FS_DATA>-MATKL.
      LS_ORDERS-MENGE       = <FS_DATA>-MENGE.
      LS_ORDERS-MEINS       = <FS_DATA>-MEINS.
      LS_ORDERS-BSART       = <FS_DATA>-BSART.
      LS_ORDERS-KNTTP       = <FS_DATA>-KNTTP.
      LS_ORDERS-SAKTO       = <FS_DATA>-SAKTO.

      IF <FS_DATA>-PS_PSP_PNR IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            INPUT  = <FS_DATA>-PS_PSP_PNR
          IMPORTING
            OUTPUT = LS_ORDERS-PS_PSP_PNR.
      ENDIF.
      LS_ORDERS-KOSTL       = <FS_DATA>-KOSTL.
      LS_ORDERS-AUFNR       = <FS_DATA>-AUFNR.
      LS_ORDERS-AFNAM       = <FS_DATA>-AFNAM.
      LS_ORDERS-ERNAM       = <FS_DATA>-ERNAM.
      APPEND LS_ORDERS TO GT_ORDERS.
      CLEAR LS_ORDERS.
    ENDLOOP.
  ELSE.
    LOOP AT GT_DATA ASSIGNING <FS_DATA>.
      LS_OTD-EBELN       = <FS_DATA>-EBELN.
      LS_OTD-EBELP       = <FS_DATA>-EBELP.
      IF <FS_DATA>-BEDAT IS NOT INITIAL.

**BOC added by JOOKONTR for CHG0176546
*        LS_OTD-BEDAT       = <FS_DATA>-BEDAT.
        CLEAR:LV_YR,LV_MT, LV_DT,LV_TDATE.
        LV_YR = <FS_DATA>-BEDAT+0(4).
        LV_MT = <FS_DATA>-BEDAT+4(2).
        LV_DT = <FS_DATA>-BEDAT+6(2).
        CONCATENATE LV_MT LV_DT LV_YR INTO LV_TDATE SEPARATED BY '/'.
        LS_OTD-BEDAT       = LV_TDATE.
**EOC added by JOOKONTR for CHG0176546
      ENDIF.
      LS_OTD-EKGRP       = <FS_DATA>-EKGRP.
      LS_OTD-KONNR       = <FS_DATA>-KONNR.
      LS_OTD-TXZ01       = <FS_DATA>-TXZ01.
      LS_OTD-WERKS       = <FS_DATA>-WERKS.
      LS_OTD-NETPR       = <FS_DATA>-NETPR.
      LS_OTD-WAERS       = <FS_DATA>-WAERS.
      LS_OTD-BEDNR       = <FS_DATA>-BEDNR.
      LS_OTD-SUBMI       = <FS_DATA>-SUBMI.
      LS_OTD-MATNR       = <FS_DATA>-MATNR.
      LS_OTD-MATKL       = <FS_DATA>-MATKL.
      LS_OTD-MENGE       = <FS_DATA>-MENGE.
      LS_OTD-MEINS       = <FS_DATA>-MEINS.
      LS_OTD-BSART       = <FS_DATA>-BSART.
      LS_OTD-KNTTP       = <FS_DATA>-KNTTP.
      LS_OTD-KONNR       = <FS_DATA>-KONNR.
      LS_OTD-LGORT       = <FS_DATA>-LGORT.
      LS_OTD-INCO1       = <FS_DATA>-INCO1.
      LS_OTD-AFNAM       = <FS_DATA>-AFNAM.
      LS_OTD-WEMPF       = <FS_DATA>-WEMPF.
      LS_OTD-ZZARIBA_APPROVER       = <FS_DATA>-ZZARIBA_APPROVER.

      IF <FS_DATA>-PS_PSP_PNR IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            INPUT  = <FS_DATA>-PS_PSP_PNR
          IMPORTING
            OUTPUT = LS_OTD-PS_PSP_PNR.
      ENDIF.

      CLEAR LS_LFA1.
      READ TABLE GT_LFA1 INTO LS_LFA1 WITH KEY LIFNR = <FS_DATA>-LIFNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LS_OTD-NAME1       = LS_LFA1-NAME1.
      ENDIF.
      CLEAR LS_EKET.
      READ TABLE GT_EKET INTO LS_EKET WITH KEY EBELN = <FS_DATA>-EBELN
                                               EBELP = <FS_DATA>-EBELP BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        IF LS_EKET-SLFDT IS NOT INITIAL.
**BOC added by JOOKONTR for CHG0176546
*          LS_OTD-SLFDT  =   LS_EKET-SLFDT.
          CLEAR:LV_YR,LV_MT, LV_DT,LV_TDATE.
          LV_YR = LS_EKET-SLFDT+0(4).
          LV_MT = LS_EKET-SLFDT+4(2).
          LV_DT = LS_EKET-SLFDT+6(2).
          CONCATENATE LV_MT LV_DT LV_YR INTO LV_TDATE SEPARATED BY '/'.
          LS_OTD-SLFDT       = LV_TDATE.
**EOC added by JOOKONTR for CHG0176546
        ENDIF.
        IF LS_EKET-EINDT IS NOT INITIAL.
**BOC added by JOOKONTR for CHG0176546
*          LS_OTD-EINDT  =   LS_EKET-EINDT.
          CLEAR:LV_YR,LV_MT, LV_DT,LV_TDATE.
          LV_YR = LS_EKET-EINDT+0(4).
          LV_MT = LS_EKET-EINDT+4(2).
          LV_DT = LS_EKET-EINDT+6(2).
          CONCATENATE LV_MT LV_DT LV_YR INTO LV_TDATE SEPARATED BY '/'.
          LS_OTD-EINDT       = LV_TDATE.
**EOC added by JOOKONTR for CHG0176546
        ENDIF.
      ENDIF.
      CLEAR LS_EKBE.
      READ TABLE GT_EKBE INTO LS_EKBE WITH KEY EBELN = <FS_DATA>-EBELN
                                               EBELP = <FS_DATA>-EBELP BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        CLEAR: LS_MSEG.
        READ TABLE GT_MSEG WITH KEY  MBLNR = LS_EKBE-BELNR
                                                 MJAHR = LS_EKBE-GJAHR
                                                 ZEILE = LS_EKBE-BUZEI
                                                 EBELN = <FS_DATA>-EBELN
                                                 EBELP = <FS_DATA>-EBELP
                                                 TRANSPORTING NO FIELDS BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          LV_INDEX = SY-TABIX.
          LOOP AT GT_MSEG INTO LS_MSEG FROM LV_INDEX.
            IF LS_MSEG-MBLNR NE LS_EKBE-BELNR
              OR LS_MSEG-MJAHR NE LS_EKBE-GJAHR
              OR LS_MSEG-ZEILE NE LS_EKBE-BUZEI
              OR LS_MSEG-EBELN NE <FS_DATA>-EBELN
              OR LS_MSEG-EBELP NE <FS_DATA>-EBELP.
              EXIT.
            ENDIF.
            IF LS_MSEG-BUDAT_MKPF IS NOT INITIAL.
**BOC added by JOOKONTR for CHG0176546
*                LS_OTD-BUDAT = LS_MSEG-BUDAT_MKPF.
              CLEAR:LV_YR,LV_MT, LV_DT,LV_TDATE.
              LV_YR = LS_MSEG-BUDAT_MKPF+0(4).
              LV_MT = LS_MSEG-BUDAT_MKPF+4(2).
              LV_DT = LS_MSEG-BUDAT_MKPF+6(2).
              CONCATENATE LV_MT LV_DT LV_YR INTO LV_TDATE SEPARATED BY '/'.
              LS_OTD-BUDAT       = LV_TDATE.
**EOC added by JOOKONTR for CHG0176546
            ENDIF.
            LS_OTD-ERFMG = LS_MSEG-ERFMG.
            LS_OTD-USNAM = LS_MSEG-USNAM_MKPF.
            CLEAR:LS_MKPF.
            READ TABLE GT_MKPF INTO LS_MKPF WITH KEY MBLNR = LS_MSEG-MBLNR
                                                     MJAHR = LS_MSEG-MJAHR
                                                     BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              IF LS_MKPF-BLDAT IS NOT INITIAL.
**BOC added by JOOKONTR for CHG0176546
*                LS_OTD-BLDAT = LS_MKPF-BLDAT.
                CLEAR:LV_YR,LV_MT, LV_DT,LV_TDATE.
                LV_YR = LS_MKPF-BLDAT+0(4).
                LV_MT = LS_MKPF-BLDAT+4(2).
                LV_DT = LS_MKPF-BLDAT+6(2).
                CONCATENATE LV_MT LV_DT LV_YR INTO LV_TDATE SEPARATED BY '/'.
                LS_OTD-BLDAT       = LV_TDATE.
**EOC added by JOOKONTR for CHG0176546
              ENDIF.
            ENDIF.
            APPEND LS_OTD TO GT_OTD.
            CLEAR: LS_OTD-USNAM,LS_OTD-ERFMG,LS_OTD-BUDAT,LS_OTD-BLDAT.
            CLEAR LS_MSEG.
          ENDLOOP.
        ENDIF.
      ELSE.
        CLEAR: LS_OTD-USNAM,LS_OTD-ERFMG,LS_OTD-BUDAT,LS_OTD-BLDAT.
        APPEND LS_OTD TO GT_OTD.
        CLEAR: LS_OTD,LS_MSEG.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " PREP_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_TO_APP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TO_APP .
  DATA: LV_MESSAGE TYPE STRING,
        LV_NETPR(13) TYPE C,
        LV_WAERS(5) TYPE C,
        LV_MENGE(16) TYPE C,
        LV_MEINS(3) TYPE C,
        LV_ERFMG(16) TYPE C.
  FIELD-SYMBOLS: <FS_ORDERS> LIKE LINE OF GT_ORDERS.
  FIELD-SYMBOLS: <FS_OTD> LIKE LINE OF GT_OTD.
  GV_PATH = P_APP.
  OPEN DATASET GV_PATH FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE LV_MESSAGE.
  IF SY-SUBRC EQ 0.
    IF GV_FLAG NE ABAP_TRUE.
      CONCATENATE TEXT-C01 TEXT-C02 TEXT-C03 TEXT-C04 TEXT-C05
                  TEXT-C06 TEXT-C07 TEXT-C08 TEXT-C09 TEXT-C10
                  TEXT-C11 TEXT-C12 TEXT-C13 TEXT-C14 TEXT-C15
                  TEXT-C16 TEXT-C17 TEXT-C18 TEXT-C19 TEXT-C20
                  TEXT-C21 TEXT-C22 TEXT-C23 TEXT-C24
                          INTO GV_STRING
                          SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
      TRANSFER GV_STRING TO GV_PATH.
      CLEAR GV_STRING.

      LOOP AT GT_ORDERS ASSIGNING <FS_ORDERS>.
        CLEAR: LV_NETPR,LV_WAERS,LV_MENGE,LV_MEINS.
        IF <FS_ORDERS>-NETPR IS NOT INITIAL.
          MOVE <FS_ORDERS>-NETPR TO LV_NETPR.
        ENDIF.
        IF <FS_ORDERS>-WAERS IS NOT INITIAL.
          MOVE <FS_ORDERS>-WAERS TO LV_WAERS.
        ENDIF.
        IF <FS_ORDERS>-MENGE IS NOT INITIAL.
          MOVE <FS_ORDERS>-MENGE TO LV_MENGE.
        ENDIF.
        IF <FS_ORDERS>-MEINS IS NOT INITIAL.
          MOVE <FS_ORDERS>-MEINS TO LV_MEINS.
        ENDIF.
**--START OF CHANGES BY AKMADASU FOR CHG0177517
      REPLACE ALL OCCURRENCES OF '"' in <FS_ORDERS>-TXZ01 with space.
**--END OF CHANGES BY AKMADASU FOR CHG0177517

        CONCATENATE <FS_ORDERS>-EBELN
                    <FS_ORDERS>-EBELP
                    <FS_ORDERS>-BEDAT
                    <FS_ORDERS>-EKGRP
                    <FS_ORDERS>-KONNR
                    <FS_ORDERS>-NAME1
                    <FS_ORDERS>-TXZ01
                    <FS_ORDERS>-WERKS
                    LV_NETPR
                    LV_WAERS
                    <FS_ORDERS>-BEDNR
                    <FS_ORDERS>-SUBMI
                    <FS_ORDERS>-MATNR
                    <FS_ORDERS>-MATKL
                    LV_MENGE
                    LV_MEINS
                    <FS_ORDERS>-BSART
                    <FS_ORDERS>-KNTTP
                    <FS_ORDERS>-SAKTO
                    <FS_ORDERS>-PS_PSP_PNR
                    <FS_ORDERS>-KOSTL
                    <FS_ORDERS>-AUFNR
                    <FS_ORDERS>-AFNAM
                    <FS_ORDERS>-ERNAM
                    INTO GV_STRING
                    SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
        TRANSFER GV_STRING TO GV_PATH .
        CLEAR GV_STRING.
      ENDLOOP.
      CLOSE DATASET GV_PATH.
      IF SY-SUBRC EQ 0.
        WRITE: TEXT-008, GV_PATH.
      ENDIF.
    ELSE.
      CONCATENATE TEXT-C25 TEXT-C26 TEXT-C27 TEXT-C28 TEXT-C29
                  TEXT-C30 TEXT-C31 TEXT-C32 TEXT-C33 TEXT-C34
                  TEXT-C35 TEXT-C36 TEXT-C37 TEXT-C38 TEXT-C39
                  TEXT-C40 TEXT-C41 TEXT-C42 TEXT-C43 TEXT-C44
                  TEXT-C45 TEXT-C46 TEXT-C47 TEXT-C48 TEXT-C49
                  TEXT-C50 TEXT-C51 TEXT-C52 TEXT-C53 TEXT-C54
                    INTO GV_STRING
                    SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
      TRANSFER GV_STRING TO GV_PATH.
      CLEAR GV_STRING.
      LOOP AT GT_OTD ASSIGNING <FS_OTD>.
        CLEAR: LV_NETPR,LV_WAERS,LV_MENGE,LV_MEINS.
        IF <FS_OTD>-NETPR IS NOT INITIAL.
          MOVE <FS_OTD>-NETPR TO LV_NETPR.
        ENDIF.
        IF <FS_OTD>-WAERS IS NOT INITIAL.
          MOVE <FS_OTD>-WAERS TO LV_WAERS.
        ENDIF.
        IF <FS_OTD>-MENGE IS NOT INITIAL.
          MOVE <FS_OTD>-MENGE TO LV_MENGE.
        ENDIF.
        IF <FS_OTD>-MEINS IS NOT INITIAL.
          MOVE <FS_OTD>-MEINS TO LV_MEINS.
        ENDIF.
**--START OF CHANGES BY AKMADASU FOR CHG0177517
      REPLACE ALL OCCURRENCES OF '"' in <FS_OTD>-TXZ01 with space.
**--END OF CHANGES BY AKMADASU FOR CHG0177517
        CONCATENATE <FS_OTD>-EBELN
                    <FS_OTD>-EBELP
                    <FS_OTD>-EKGRP
                    <FS_OTD>-BEDAT
                    <FS_OTD>-SLFDT
                    <FS_OTD>-EINDT
                    <FS_OTD>-BLDAT
                    <FS_OTD>-BUDAT
                    <FS_OTD>-KNTTP
                    <FS_OTD>-PS_PSP_PNR
                    <FS_OTD>-WERKS
                    <FS_OTD>-KONNR
                    <FS_OTD>-BSART
                    <FS_OTD>-SUBMI
                    <FS_OTD>-NAME1
                    <FS_OTD>-TXZ01
                    LV_MENGE
                    LV_ERFMG
                    LV_MEINS
                    <FS_OTD>-USNAM
                    <FS_OTD>-BEDNR
                    <FS_OTD>-MATNR
                    <FS_OTD>-MATKL
                    <FS_OTD>-LGORT
                    LV_NETPR
                    LV_WAERS
                    <FS_OTD>-INCO1
                    <FS_OTD>-AFNAM
                    <FS_OTD>-WEMPF
                    <FS_OTD>-ZZARIBA_APPROVER
                    INTO GV_STRING
                    SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
        TRANSFER GV_STRING TO GV_PATH.
      ENDLOOP.
      CLOSE DATASET GV_PATH.
      IF SY-SUBRC EQ 0.
        WRITE: TEXT-008, GV_PATH.
      ENDIF.
    ENDIF.
  ELSE.
    WRITE: TEXT-007, LV_MESSAGE.
  ENDIF.

ENDFORM.                    " SAVE_TO_APP
*&---------------------------------------------------------------------*
*&      Form  ALV_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_OUTPUT .
*&-Build field catalog
  PERFORM BUILD_FCAT.
ENDFORM.                    " ALV_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FCAT.
  IF GV_FLAG NE ABAP_TRUE.
    PERFORM FCAT: USING 'EBELN' TEXT-C01,
                  USING 'EBELP' TEXT-C02,
                  USING 'BEDAT' TEXT-C03,
                  USING 'EKGRP' TEXT-C04,
                  USING 'KONNR' TEXT-C05,
                  USING 'NAME1' TEXT-C06,
                  USING 'TXZ01' TEXT-C07,
                  USING 'WERKS' TEXT-C08,
                  USING 'NETPR' TEXT-C09,
                  USING 'WAERS' TEXT-C10,
                  USING 'BEDNR' TEXT-C11,
                  USING 'SUBMI' TEXT-C12,
                  USING 'MATNR' TEXT-C13,
                  USING 'MATKL' TEXT-C14,
                  USING 'MENGE' TEXT-C15,
                  USING 'MEINS' TEXT-C16,
                  USING 'BSART' TEXT-C17,
                  USING 'KNTTP' TEXT-C18,
                  USING 'SAKTO' TEXT-C19,
                  USING 'PS_PSP_PNR' TEXT-C20,
                  USING 'KOSTL' TEXT-C21,
                  USING 'AUFNR' TEXT-C22,
                  USING 'AFNAM' TEXT-C23,
                  USING 'ERNAM' TEXT-C24.
    PERFORM DISPLAY_REPORT TABLES GT_ORDERS.
  ELSE.
    PERFORM FCAT: USING 'EBELN' TEXT-C25,
                  USING 'EBELP' TEXT-C26,
                  USING 'EKGRP' TEXT-C27,
                  USING 'BEDAT' TEXT-C28,
                  USING 'SLFDT' TEXT-C29,
                  USING 'EINDT' TEXT-C30,
                  USING 'BLDAT' TEXT-C31,
                  USING 'BUDAT' TEXT-C32,
                  USING 'KNTTP' TEXT-C33,
                  USING 'PS_PSP_PNR' TEXT-C34,
                  USING 'WERKS' TEXT-C35,
                  USING 'KONNR' TEXT-C36,
                  USING 'BSART' TEXT-C37,
                  USING 'SUBMI' TEXT-C38,
                  USING 'NAME1' TEXT-C39,
                  USING 'TXZ01' TEXT-C40,
                  USING 'MENGE' TEXT-C41,
                  USING 'ERFMG' TEXT-C42,
                  USING 'MEINS' TEXT-C43,
                  USING 'USNAM' TEXT-C44,
                  USING 'BEDNR' TEXT-C45,
                  USING 'MATNR' TEXT-C46,
                  USING 'MATKL' TEXT-C47,
                  USING 'LGORT' TEXT-C48,
                  USING 'NETPR' TEXT-C49,
                  USING 'WAERS' TEXT-C50,
                  USING 'INCO1' TEXT-C51,
                  USING 'AFNAM' TEXT-C52,
                  USING 'WEMPF' TEXT-C53,
                  USING 'ZZARIBA_APPROVER' TEXT-C54.

    PERFORM DISPLAY_REPORT TABLES GT_OTD.
  ENDIF.


ENDFORM.                    " BUILD_FCAT
*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1179   text
*      -->P_1180   text
*      -->P_1181   text
*----------------------------------------------------------------------*
FORM FCAT  USING    IV_FIELDNAME TYPE ANY
                    IV_SELTEXT TYPE ANY.
  DATA: LS_FCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR LS_FCAT.
  LS_FCAT-TABNAME  = 'LT_OUTPUT'.
  LS_FCAT-FIELDNAME = IV_FIELDNAME.
  LS_FCAT-SELTEXT_L = IV_SELTEXT.
  APPEND LS_FCAT TO GT_FCAT.
ENDFORM.                    " FCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_REPORT TABLES GT_TAB .
  DATA: LS_LAYOUT      TYPE SLIS_LAYOUT_ALV.

  LS_LAYOUT-COLWIDTH_OPTIMIZE = ABAP_TRUE.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_CALLBACK_PROGRAM       = ' '
*     I_CALLBACK_PF_STATUS_SET = ' '
*     I_CALLBACK_USER_COMMAND  = ' '
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_STRUCTURE_NAME         =
*     I_GRID_TITLE             =
      IS_LAYOUT                = LS_LAYOUT
      IT_FIELDCAT              = GT_FCAT
*     IT_SORT                  =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
    TABLES
      T_OUTTAB                 = GT_TAB
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DISPLAY_REPORT
*&---------------------------------------------------------------------*
*&      Form  F_TRANSFER_DATA_PRES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_DATA[]  text
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM F_TRANSFER_DATA_PRES  USING PT_DATA TYPE STANDARD TABLE
                                 P_FILE  TYPE ANY.

  DATA: GT_BINTAB  TYPE SOLIX_TAB,
        LV_SIZE    TYPE I.

  PERFORM CREATE_XLS_FROM_ITAB USING    PT_DATA[]
                               CHANGING GT_BINTAB[] LV_SIZE.

  CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD(
      EXPORTING
        BIN_FILESIZE              = LV_SIZE
        FILENAME                  = P_FILE
        FILETYPE                  = 'BIN'
      CHANGING
        DATA_TAB                  = GT_BINTAB
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
        OTHERS                    = 24
           ).



ENDFORM.                    " F_TRANSFER_DATA_PRES
*&---------------------------------------------------------------------*
*&      Form  CREATE_XLS_FROM_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_DATA    text
*      <--PT_BINTAB  text
*      <--P_SIZE     text
*----------------------------------------------------------------------*
FORM CREATE_XLS_FROM_ITAB USING    PT_DATA   TYPE STANDARD TABLE
                          CHANGING PT_BINTAB TYPE SOLIX_TAB
                                   P_SIZE    TYPE I.



  DATA: MT_FCAT        TYPE LVC_T_FCAT,
        MT_DATA        TYPE REF TO DATA,
        M_FLAVOUR      TYPE STRING,
        M_VERSION      TYPE STRING,
        MO_RESULT_DATA TYPE REF TO CL_SALV_EX_RESULT_DATA_TABLE,
        MO_COLUMNS     TYPE REF TO CL_SALV_COLUMNS_TABLE,
        MO_AGGREG      TYPE REF TO CL_SALV_AGGREGATIONS,
        MO_SALV_TABLE  TYPE REF TO CL_SALV_TABLE,
        M_FILE_TYPE    TYPE SALV_BS_CONSTANT,
        IT_SORT        TYPE LVC_T_SORT,    " -> alv control: table of sort criteria
        IT_FILT        TYPE LVC_T_FILT,    " -> alv control: table of filter conditions
        E_XSTRING      TYPE XSTRING,       " -> xstring with our excel file
        IS_LAYOUT      TYPE LVC_S_LAYO.



  FIELD-SYMBOLS: <TAB> TYPE ANY TABLE,
                 <FS_FCAT> LIKE LINE OF MT_FCAT.



  GET REFERENCE OF PT_DATA INTO MT_DATA.



*create fieldcatalog
  ASSIGN MT_DATA->* TO <TAB>.
  TRY .
      CL_SALV_TABLE=>FACTORY(
      EXPORTING
        LIST_DISPLAY = ABAP_FALSE
      IMPORTING
        R_SALV_TABLE = MO_SALV_TABLE
      CHANGING
        T_TABLE      = <TAB> ).
    CATCH CX_SALV_MSG.



  ENDTRY.



*get colums & aggregation info to create fieldcat
  MO_COLUMNS  = MO_SALV_TABLE->GET_COLUMNS( ).
  MO_AGGREG   = MO_SALV_TABLE->GET_AGGREGATIONS( ).
  MT_FCAT     =  CL_SALV_CONTROLLER_METADATA=>GET_LVC_FIELDCATALOG(
                                R_COLUMNS      = MO_COLUMNS
                                R_AGGREGATIONS = MO_AGGREG ).
  LOOP AT MT_FCAT ASSIGNING <FS_FCAT>.
    CASE <FS_FCAT>-FIELDNAME.
      WHEN 'EBELN'.
        <FS_FCAT>-REPTEXT = TEXT-C01.
      WHEN 'EBELP'.
        <FS_FCAT>-REPTEXT = TEXT-C02.
      WHEN 'BEDAT'.
        <FS_FCAT>-REPTEXT = TEXT-C03.
      WHEN 'EKGRP'.
        <FS_FCAT>-REPTEXT = TEXT-C04.
      WHEN 'KONNR'.
        <FS_FCAT>-REPTEXT = TEXT-C05.
      WHEN 'NAME1'.
        <FS_FCAT>-REPTEXT = TEXT-C06.
      WHEN 'TXZ01'.
        <FS_FCAT>-REPTEXT = TEXT-C07.
      WHEN 'WERKS'.
        <FS_FCAT>-REPTEXT = TEXT-C08.
      WHEN 'NETPR'.
        <FS_FCAT>-REPTEXT = TEXT-C09.
      WHEN 'WAERS'.
        <FS_FCAT>-REPTEXT = TEXT-C10.
      WHEN 'BEDNR'.
        <FS_FCAT>-REPTEXT = TEXT-C11.
      WHEN 'SUBMI'.
        <FS_FCAT>-REPTEXT = TEXT-C12.
      WHEN 'MATNR'.
        <FS_FCAT>-REPTEXT = TEXT-C13.
      WHEN 'MATKL'.
        <FS_FCAT>-REPTEXT = TEXT-C14.
      WHEN 'MENGE'.
        <FS_FCAT>-REPTEXT = TEXT-C15.
      WHEN 'MEINS'.
        <FS_FCAT>-REPTEXT = TEXT-C16.
      WHEN 'BSART'.
        <FS_FCAT>-REPTEXT = TEXT-C17.
      WHEN 'KNTTP'.
        <FS_FCAT>-REPTEXT = TEXT-C18.
      WHEN 'SAKTO'.
        <FS_FCAT>-REPTEXT = TEXT-C19.
      WHEN 'PS_PSP_PNR'.
        <FS_FCAT>-REPTEXT = TEXT-C20.
      WHEN 'KOSTL'.
        <FS_FCAT>-REPTEXT = TEXT-C21.
      WHEN 'AUFNR'.
        <FS_FCAT>-REPTEXT = TEXT-C22.
      WHEN 'AFNAM'.
        <FS_FCAT>-REPTEXT = TEXT-C23.
      WHEN 'ERNAM'.
        <FS_FCAT>-REPTEXT = TEXT-C24.
      WHEN 'SLFDT'.
        <FS_FCAT>-REPTEXT = TEXT-C29.
      WHEN 'EINDT'.
        <FS_FCAT>-REPTEXT = TEXT-C30.
      WHEN 'BLDAT'.
        <FS_FCAT>-REPTEXT = TEXT-C31.
      WHEN 'BUDAT'.
        <FS_FCAT>-REPTEXT = TEXT-C32.
      WHEN 'ERFMG'.
        <FS_FCAT>-REPTEXT = TEXT-C42.
      WHEN 'USNAM'.
        <FS_FCAT>-REPTEXT = TEXT-C44.
      WHEN 'LGORT'.
        <FS_FCAT>-REPTEXT = TEXT-C48.
      WHEN 'INCO1'.
        <FS_FCAT>-REPTEXT = TEXT-C51.
      WHEN 'WEMPF'.
        <FS_FCAT>-REPTEXT = TEXT-C53.
      WHEN 'ZZARIBA_APPROVER'.
        <FS_FCAT>-REPTEXT = TEXT-C54.
      WHEN OTHERS.
        "Do Nothing
    ENDCASE.
  ENDLOOP.




  IF CL_SALV_BS_A_XML_BASE=>GET_VERSION( ) EQ IF_SALV_BS_XML=>VERSION_25 OR
     CL_SALV_BS_A_XML_BASE=>GET_VERSION( ) EQ IF_SALV_BS_XML=>VERSION_26.



    MO_RESULT_DATA = CL_SALV_EX_UTIL=>FACTORY_RESULT_DATA_TABLE(
        R_DATA                      = MT_DATA
        S_LAYOUT                    = IS_LAYOUT
        T_FIELDCATALOG              = MT_FCAT
        T_SORT                      = IT_SORT
        T_FILTER                    = IT_FILT
    ).



    CASE CL_SALV_BS_A_XML_BASE=>GET_VERSION( ).
      WHEN IF_SALV_BS_XML=>VERSION_25.
        M_VERSION = IF_SALV_BS_XML=>VERSION_25.
      WHEN IF_SALV_BS_XML=>VERSION_26.
        M_VERSION = IF_SALV_BS_XML=>VERSION_26.
    ENDCASE.



    M_FILE_TYPE = IF_SALV_BS_XML=>C_TYPE_XLSX.
    M_FLAVOUR   = IF_SALV_BS_C_TT=>C_TT_XML_FLAVOUR_EXPORT.



* transformation of data to excel
    CALL METHOD CL_SALV_BS_TT_UTIL=>IF_SALV_BS_TT_UTIL~TRANSFORM
      EXPORTING
        XML_TYPE      = M_FILE_TYPE
        XML_VERSION   = M_VERSION
        R_RESULT_DATA = MO_RESULT_DATA
        XML_FLAVOUR   = M_FLAVOUR
        GUI_TYPE      = IF_SALV_BS_XML=>C_GUI_TYPE_GUI
      IMPORTING
        XML           = E_XSTRING.



    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        BUFFER        = E_XSTRING
      IMPORTING
        OUTPUT_LENGTH = P_SIZE
      TABLES
        BINARY_TAB    = PT_BINTAB.
  ENDIF.



ENDFORM.                    "create_xls_from_itab
