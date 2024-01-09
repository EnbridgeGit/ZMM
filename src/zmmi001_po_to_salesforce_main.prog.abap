*&---------------------------------------------------------------------*
*&  Include           ZMMI001_PO_TO_SALESFORCE_MAIN
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZMMI001_PO_TO_SALESFORCE                      *
*& Include Name       :  ZMMI001_PO_TO_SALESFORCE_MAIN
*& Author             :  Tawfeeq Ahmad                                 *
*& Creation Date      :  18/12/2019                                    *
*& Change Request     :  CHG0164927                                    *
*& Description        :  SAP PO Data to Sales Force                    *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 18-Dec-2019  AHMADT        D30K930342 CHG0164927 Initial Development *
*                            D30K930336                                *
*                            D30K930452                                *
*                            D30K930545                                *
*                            D30K930541                                *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  IF R_DELTA = 'X'.
    SELECT SINGLE PREV_EXEC_DATE
                  PREV_EXEC_TIME
      FROM ZMMT_SF_PARAM
      INTO (GV_EXEC_DATE, GV_EXEC_TIME)
      WHERE PARAMTYPE = 'SAP_SF_PARAM'
        AND SUBTYPE = 'DATE_TIME'.

    IF GV_EXEC_DATE IS INITIAL.
      MESSAGE 'Update execution date in the DB Table' TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.

  ENDIF.

AT SELECTION-SCREEN.
  IF R_FULL = 'X'.
    IF S_AEDAT IS INITIAL.
      MESSAGE 'Creation Date is mandatory for Full load' TYPE 'E'.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  IF GV_EXEC_DATE IS INITIAL AND
     GV_EXEC_TIME IS INITIAL AND R_FULL IS INITIAL.
    SELECT SINGLE PREV_EXEC_DATE
                    PREV_EXEC_TIME
        FROM ZMMT_SF_PARAM
        INTO (GV_EXEC_DATE, GV_EXEC_TIME)
        WHERE PARAMTYPE = 'SAP_SF_PARAM'
          AND SUBTYPE = 'DATE_TIME'.

    IF GV_EXEC_DATE IS INITIAL.
      MESSAGE 'Update execution date in the DB Table' TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

  PERFORM GET_PO_DATA.

  PERFORM PREPARE_FILE_DATA.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  GET_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PO_DATA.

  TYPES: BEGIN OF LTY_PO,
          EBELN TYPE EBELN,
         END OF LTY_PO.

  DATA: LT_PO   TYPE STANDARD TABLE OF LTY_PO,
        LT_EKKN TYPE STANDARD TABLE OF TY_EKKN,
        LS_PO   TYPE LTY_PO,
*      Start of changes by AHMADT for CHG0164927
        lv_ndate TYPE sy-datum,
*      End of changes by AHMADT for CHG0164927
        R_TCODE TYPE RANGE OF CDTCODE,
        LS_TCODE LIKE LINE OF R_TCODE.

  LS_TCODE-SIGN = 'I'.
  LS_TCODE-OPTION = 'EQ'.
  LS_TCODE-LOW = 'ME21N'.
  APPEND LS_TCODE TO R_TCODE.
  LS_TCODE-LOW = 'ME22N'.
  APPEND LS_TCODE TO R_TCODE.
  LS_TCODE-LOW = 'ME23N'.
  APPEND LS_TCODE TO R_TCODE.

  IF R_FULL = 'X'.
    SELECT EBELN
           BUKRS
           BSTYP
           BSART
           AEDAT
           EKORG
           LIFNR
           MEMORYTYPE
      FROM EKKO
      INTO TABLE GT_EKKO
      WHERE EBELN IN S_EBELN AND
            AEDAT IN S_AEDAT.
    IF SY-SUBRC EQ 0.
*      DELETE gt_cdhdr WHERE tcode NOT IN r_tcode.
    ELSE.
      WRITE:/ 'No data found'.
      STOP.
    ENDIF.
  ENDIF.
  IF R_DELTA = 'X'.

    SELECT SINGLE * FROM ZMMT_SF_PARAM
                    INTO GS_SF_PARAM
                   WHERE PARAMTYPE = 'SAP_SF_PARAM'
                     AND SUBTYPE = 'DATE_TIME'.
    IF SY-SUBRC EQ 0.
      GET TIME FIELD GV_CURR_TIME.
      lv_ndate = GS_SF_PARAM-PREV_EXEC_DATE + 1.
      SELECT OBJECTCLAS
                 OBJECTID
                 UDATE
                 UTIME
                 TCODE
                 CHANGE_IND
            FROM CDHDR
            INTO TABLE GT_CDHDR
            WHERE OBJECTCLAS = 'EINKBELEG' AND
                ( ( UDATE EQ GS_SF_PARAM-PREV_EXEC_DATE AND
                  UTIME GE GS_SF_PARAM-PREV_EXEC_TIME ) OR
                   ( UDATE GE lv_ndate  ) ) .
      IF SY-SUBRC EQ 0.
      ELSE.
        WRITE:/ 'No data found'.
        STOP.
      ENDIF.
      clear:lv_ndate.
    ENDIF.
    LOOP AT GT_CDHDR INTO GS_CDHDR.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GS_CDHDR-OBJECTID
        IMPORTING
          OUTPUT = LS_PO-EBELN.
      IF S_EBELN IS NOT INITIAL.
        READ TABLE S_EBELN WITH KEY LOW = LS_PO-EBELN.
        IF SY-SUBRC NE 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      APPEND LS_PO TO LT_PO.
    ENDLOOP.
    IF LT_PO IS NOT INITIAL.
      SELECT EBELN
             BUKRS
             BSTYP
             BSART
             AEDAT
             EKORG
             LIFNR
             MEMORYTYPE
        FROM EKKO
        INTO TABLE GT_EKKO
        FOR ALL ENTRIES IN LT_PO
        WHERE EBELN = LT_PO-EBELN.
    ENDIF.
  ENDIF.

  IF GT_EKKO IS NOT INITIAL.
    IF S_BSART IS NOT INITIAL.
      DELETE GT_EKKO WHERE BSART NOT IN S_BSART.
    ENDIF.

    IF S_EKORG IS NOT INITIAL.
      DELETE GT_EKKO WHERE EKORG NOT IN S_EKORG.
    ENDIF.

    IF S_BUKRS IS NOT INITIAL.
      DELETE GT_EKKO WHERE BUKRS NOT IN S_BUKRS.
    ENDIF.

    IF P_BSTYP IS NOT INITIAL.
      DELETE GT_EKKO WHERE BSTYP NE P_BSTYP.
    ENDIF.

  ENDIF.
  IF GT_EKKO IS NOT INITIAL.

    SELECT EBELN
           EBELP
           LOEKZ
           TXZ01
           WERKS
           BEDNR
           MENGE
           AFNAM
      FROM EKPO
      INTO TABLE GT_EKPO
      FOR ALL ENTRIES IN GT_EKKO
      WHERE EBELN = GT_EKKO-EBELN.
    IF SY-SUBRC EQ 0.
      SORT GT_EKPO BY EBELN.
    ENDIF.

    SELECT EBELN
           EBELP
           ZEKKN
           MENGE
           VPROZ
           SAKTO
           KOSTL
           AUFNR
           PS_PSP_PNR
           NPLNR
           AUFPL
           APLZL
      FROM EKKN
      INTO TABLE GT_EKKN
      FOR ALL ENTRIES IN GT_EKKO
      WHERE EBELN = GT_EKKO-EBELN.
    IF SY-SUBRC EQ 0.
      SORT GT_EKKN BY EBELN.
    ENDIF.

    SELECT LIFNR
           NAME1
           NAME2
      FROM LFA1
      INTO TABLE GT_LFA1
      FOR ALL ENTRIES IN GT_EKKO
      WHERE LIFNR = GT_EKKO-LIFNR.
    IF SY-SUBRC EQ 0.
      SORT GT_LFA1 BY LIFNR.
    ENDIF.

    SELECT BUKRS
           BUTXT
      FROM T001
      INTO TABLE GT_T001
      WHERE SPRAS = SY-LANGU..

  ENDIF.     " End of GT_EKKO NOT Initial

  IF GT_EKKN IS NOT INITIAL.
    LT_EKKN = GT_EKKN.
    SORT LT_EKKN BY AUFNR.
    DELETE ADJACENT DUPLICATES FROM LT_EKKN COMPARING AUFNR.

    SELECT AUFNR
           KTEXT
      FROM AUFK
      INTO TABLE GT_AUFK_ORD
      FOR ALL ENTRIES IN LT_EKKN
      WHERE AUFNR = LT_EKKN-AUFNR.
    IF SY-SUBRC EQ 0.

    ENDIF.

    REFRESH LT_EKKN.
    LT_EKKN = GT_EKKN.
    SORT LT_EKKN BY NPLNR.
    DELETE ADJACENT DUPLICATES FROM LT_EKKN COMPARING NPLNR.

    SELECT AUFNR
           KTEXT
      FROM AUFK
      APPENDING TABLE GT_AUFK_ORD
      FOR ALL ENTRIES IN LT_EKKN
      WHERE AUFNR = LT_EKKN-NPLNR.
    IF SY-SUBRC EQ 0.

    ENDIF.
    SORT GT_AUFK_ORD BY AUFNR.

    REFRESH LT_EKKN.
    LT_EKKN = GT_EKKN.
    SORT LT_EKKN BY AUFPL APLZL.
    DELETE ADJACENT DUPLICATES FROM LT_EKKN COMPARING AUFPL APLZL.

    SELECT AUFPL
           APLZL
           VORNR
           LTXA1
      FROM AFVC
      INTO TABLE GT_AFVC
      FOR ALL ENTRIES IN LT_EKKN
      WHERE AUFPL = LT_EKKN-AUFPL
        AND APLZL = LT_EKKN-APLZL.
    IF SY-SUBRC EQ 0.
      SORT GT_AFVC BY AUFPL APLZL.
    ENDIF.

    REFRESH LT_EKKN.
    LT_EKKN = GT_EKKN.
    SORT LT_EKKN BY PS_PSP_PNR.
    DELETE ADJACENT DUPLICATES FROM LT_EKKN COMPARING PS_PSP_PNR.

    SELECT PSPNR
           POST1
      FROM PRPS
      INTO TABLE GT_PRPS
      FOR ALL ENTRIES IN LT_EKKN
      WHERE PSPNR = LT_EKKN-PS_PSP_PNR.
    IF SY-SUBRC EQ 0.
      SORT GT_PRPS BY PSPNR.
    ENDIF.

    SELECT KOSTL
           KTEXT
      FROM CSKT
      INTO TABLE GT_CSKT
      WHERE SPRAS = SY-LANGU.
    IF SY-SUBRC EQ 0.
      SORT GT_CSKT BY KOSTL.
    ENDIF.

  ENDIF.     " End of GT_EKKN NOT Initial

ENDFORM.                    " GET_PO_DATA
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FILE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARE_FILE_DATA.

  DATA: LV_INDEX TYPE SY-TABIX,
        LS_EKKO TYPE TY_EKKO.
  DATA: LV_FILEPATH(200) TYPE C,
        LV_FILENAME(50) TYPE C,
        LV_STR TYPE STRING,
        LV_PO_QUAN TYPE CHAR16,
        LV_AC_QUAN TYPE CHAR16,
        LV_VPROZ   TYPE CHAR4,
        LV_ERROR(100) TYPE C,
        LV_TXZ01(50) TYPE C,
        LV_BUTXT(50) TYPE C,
        LV_KTEXT(45) TYPE C,
        LV_CC_KTEXT(30) TYPE C,
        LV_NET_KTEXT(45) TYPE C,
        lv_sysid TYPE sy-sysid,
        lv_ebeln(12) TYPE c,
        lv_ebelp(7) TYPE c,
        lv_revno(4) TYPE c,
        lv_ven_name(75) TYPE c,
        lv_lifnr(12) TYPE c,
        lv_system(4) TYPE c,
        lv_action(3) TYPE c,
        lv_bednr(12) TYPE c,
        lv_werks(6) TYPE c,
        lv_afname(14) TYPE c,
        lv_loekz(9) TYPE c,
        lv_zekkn(4) TYPE c,
        lv_sakto(12) TYPE c,
        lv_ps_psp_pnr(26) TYPE c,
        lv_post1(42) TYPE c,
        lv_aufnr(14) TYPE c,
        lv_kostl(12) TYPE c,
        lv_nplnr(14) TYPE c,
        lv_vornr(6) TYPE c,
        lv_ltxa1(42) TYPE c.        .

  CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/SALESFORCE' INTO LV_FILEPATH.
  CONCATENATE 'SAP_UG_' SY-SYSID '_SALESFORCE_' SY-DATUM SY-UZEIT '.TXT' INTO LV_FILENAME.

  CONCATENATE LV_FILEPATH LV_FILENAME INTO LV_FILEPATH SEPARATED BY '/'.

* Open file
  OPEN DATASET LV_FILEPATH FOR OUTPUT MESSAGE LV_ERROR IN TEXT MODE ENCODING DEFAULT.
  CHECK SY-SUBRC EQ 0.

  LOOP AT GT_EKKO INTO GS_EKKO.
    CLEAR: GS_FILE_DATA, GS_CDHDR, GS_LFA1, GS_EKKN, GS_EKPO.
*&-------------HEADER DATA--------------*
    GS_FILE_DATA-SYSTEM = 'UG'.                    "System Name
    IF R_FULL = 'X'.
      GS_FILE_DATA-ACTION = 'C'.                    "Change Indicator
    ELSE.
      READ TABLE GT_CDHDR INTO GS_CDHDR
           WITH KEY OBJECTID = GS_EKKO-EBELN.
      IF SY-SUBRC EQ 0.
        GS_FILE_DATA-ACTION = GS_CDHDR-CHANGE_IND.
      ENDIF.
    ENDIF.

    GS_FILE_DATA-EBELN = GS_EKKO-EBELN.            "PURCHASING DOCUMENT NUMBER
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = GS_EKKO-LIFNR
      IMPORTING
        OUTPUT = GS_FILE_DATA-LIFNR.     "VENDOR ACCOUNT NUMBER
    GS_FILE_DATA-MEMORYTYPE = GS_EKKO-MEMORYTYPE.  "Category of Incompleteness

    READ TABLE GT_LFA1 INTO GS_LFA1
         WITH KEY LIFNR = GS_EKKO-LIFNR
         BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      CONCATENATE GS_LFA1-NAME1 GS_LFA1-NAME2
             INTO GS_FILE_DATA-VEN_NAME            "Vendor Name
             SEPARATED BY SPACE.
    ENDIF.

    READ TABLE GT_T001 INTO GS_T001
         WITH KEY BUKRS = GS_EKKO-BUKRS
         BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_FILE_DATA-BUTXT = GS_T001-BUTXT.
    ELSE.
      CLEAR GS_FILE_DATA-BUTXT.
    ENDIF.
    lv_ebeln = gs_file_data-ebeln .
    lv_revno = gs_file_data-revno .
    lv_ven_name = gs_file_data-ven_name .
    lv_lifnr = gs_file_data-lifnr .
    lv_system = gs_file_data-system.
    lv_action = gs_file_data-action.

    CONCATENATE 'H'
                lv_ebeln
                lv_revno
                lv_ven_name
                lv_lifnr
                lv_system
                lv_action
                INTO LV_STR
                SEPARATED BY '^^^'.
    TRANSFER LV_STR TO LV_FILEPATH.
    CLEAR: LV_STR,lv_ebeln,lv_revno,lv_ven_name,lv_lifnr,lv_system,lv_action.
*&------------ITEM DATA----------------&*
    READ TABLE GT_EKPO WITH KEY EBELN = GS_EKKO-EBELN
               TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      LV_INDEX = SY-TABIX.
      LOOP AT GT_EKPO INTO GS_EKPO FROM LV_INDEX.
        IF GS_EKPO-EBELN <> GS_EKKO-EBELN.
          EXIT.
        ENDIF.
        GS_FILE_DATA-BEDNR = GS_EKPO-BEDNR.        "Requirement Tracking Number
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = GS_EKPO-EBELP
          IMPORTING
            OUTPUT = GS_FILE_DATA-EBELP. "Item Number of Purchasing Document
        GS_FILE_DATA-TXZ01 = GS_EKPO-TXZ01.        "Short Text
        GS_FILE_DATA-PO_MENGE = GS_EKPO-MENGE.     "PURCHASE ORDER QUANTITY
        GS_FILE_DATA-WERKS = GS_EKPO-WERKS.        "Plant
        GS_FILE_DATA-AFNAME = GS_EKPO-AFNAM.       "Name of Requestor
        IF GS_EKPO-LOEKZ EQ 'S'.
          GS_FILE_DATA-LOEKZ = 'Locked'.
        ELSEIF GS_EKPO-LOEKZ NE 'S'.
          IF GS_EKPO-LOEKZ NE SPACE.
            GS_FILE_DATA-LOEKZ = 'Deleted'.
          ELSE.
            GS_FILE_DATA-LOEKZ = ''.
          ENDIF.
        ENDIF.
        WRITE GS_FILE_DATA-PO_MENGE TO LV_PO_QUAN.
        LV_TXZ01 = GS_FILE_DATA-TXZ01.
        LV_BUTXT = GS_FILE_DATA-BUTXT.
        lv_ebeln = gs_file_data-ebeln.
        lv_ebelp = gs_file_data-ebelp.
        lv_bednr = gs_file_data-bednr.
        lv_werks = gs_file_data-werks.
        lv_afname = gs_file_data-afname.
        lv_loekz = gs_file_data-loekz.
        CONCATENATE 'I'
                    lv_ebeln
                    lv_ebelp
                    LV_TXZ01
                    LV_PO_QUAN
                    LV_BUTXT
                    lv_bednr
                    lv_werks
                    lv_afname
                    lv_loekz
                    INTO LV_STR
                    SEPARATED BY '^^^'.
        TRANSFER LV_STR TO LV_FILEPATH.
        CLEAR: LV_STR,LV_TXZ01,LV_PO_QUAN,LV_BUTXT,
               lv_ebeln,lv_ebelp,lv_bednr,lv_werks,lv_afname,lv_loekz.
*&--------------ACCOUNT ASSIGNMENT DATA-------------------*
        READ TABLE GT_EKKN WITH KEY EBELN = GS_EKPO-EBELN
                                    EBELP = GS_EKPO-EBELP
                           TRANSPORTING NO FIELDS.
        IF SY-SUBRC EQ 0.
          LOOP AT GT_EKKN INTO GS_EKKN
                 WHERE EBELN = GS_EKPO-EBELN AND
                       EBELP = GS_EKPO-EBELP.
            CLEAR: GS_PRPS, GS_CSKT, GS_AUFK_ORD, GS_AFVC.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = GS_EKKN-ZEKKN
              IMPORTING
                OUTPUT = GS_FILE_DATA-ZEKKN.     "Sequential Number of Account Assignment
            GS_FILE_DATA-AC_MENGE = GS_EKKN-MENGE.    "Quantity in Account Assignment
            GS_FILE_DATA-VPROZ = GS_EKKN-VPROZ.       "Distribution Percentage
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = GS_EKKN-SAKTO
              IMPORTING
                OUTPUT = GS_FILE_DATA-SAKTO.     "G/L Account Number
            IF GS_EKKN-PS_PSP_PNR IS NOT INITIAL AND GS_EKKN-PS_PSP_PNR NE '00000000'.
              CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                EXPORTING
                  INPUT  = GS_EKKN-PS_PSP_PNR
                IMPORTING
                  OUTPUT = GS_FILE_DATA-PS_PSP_PNR. "WBS Element
            ELSE.
              CLEAR GS_FILE_DATA-PS_PSP_PNR.
            ENDIF.
            WRITE GS_FILE_DATA-AC_MENGE TO LV_AC_QUAN.
            WRITE GS_FILE_DATA-VPROZ    TO LV_VPROZ.

            READ TABLE GT_PRPS INTO GS_PRPS
                 WITH KEY PSPNR = GS_EKKN-PS_PSP_PNR
                 BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FILE_DATA-POST1 = GS_PRPS-POST1.    "WBS short Description
            ELSE.
              CLEAR GS_FILE_DATA-POST1.
            ENDIF.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = GS_EKKN-AUFNR
              IMPORTING
                OUTPUT = GS_FILE_DATA-AUFNR.    "Order
            READ TABLE GT_AUFK_ORD INTO GS_AUFK_ORD
                 WITH KEY AUFNR = GS_EKKN-AUFNR
                 BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FILE_DATA-KTEXT = GS_AUFK_ORD-KTEXT.  "Order Description
            ELSE.
              CLEAR GS_FILE_DATA-KTEXT.
            ENDIF.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = GS_EKKN-KOSTL
              IMPORTING
                OUTPUT = GS_FILE_DATA-KOSTL.    "Cost Center

            READ TABLE GT_CSKT INTO GS_CSKT
                 WITH KEY KOSTL = GS_EKKN-KOSTL
                 BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FILE_DATA-CC_KTEXT = GS_CSKT-KTEXT.  "Cost Center Description
            ELSE.
              CLEAR GS_FILE_DATA-CC_KTEXT.
            ENDIF.

            GS_FILE_DATA-NPLNR = GS_EKKN-NPLNR.       "Network number
            READ TABLE GT_AUFK_ORD INTO GS_AUFK_ORD
                 WITH KEY AUFNR = GS_EKKN-NPLNR
                 BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FILE_DATA-NET_KTEXT = GS_AUFK_ORD-KTEXT.  "Network Description
            ELSE.
              CLEAR GS_FILE_DATA-NET_KTEXT.
            ENDIF.

            READ TABLE GT_AFVC INTO GS_AFVC
                 WITH KEY AUFPL = GS_EKKN-AUFPL
                          APLZL = GS_EKKN-APLZL
                 BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FILE_DATA-VORNR = GS_AFVC-VORNR.      "Operation/Activity number
              GS_FILE_DATA-LTXA1 = GS_AFVC-LTXA1.      "Activity Text
            ELSE.
              CLEAR: GS_FILE_DATA-VORNR,
                     GS_FILE_DATA-LTXA1.
            ENDIF.
            LV_KTEXT = GS_FILE_DATA-KTEXT.
            LV_CC_KTEXT = GS_FILE_DATA-CC_KTEXT.
            LV_NET_KTEXT = GS_FILE_DATA-NET_KTEXT.
            lv_ebeln = gs_file_data-ebeln.
            lv_ebelp = gs_file_data-ebelp.
            lv_zekkn = gs_file_data-zekkn.
            lv_sakto = gs_file_data-sakto.
            lv_ps_psp_pnr = gs_file_data-ps_psp_pnr.
            lv_post1 = gs_file_data-post1.
            lv_aufnr = gs_file_data-aufnr.
            lv_kostl = gs_file_data-kostl.
            lv_nplnr = gs_file_data-nplnr.
            lv_vornr = gs_file_data-vornr.
            lv_ltxa1 = gs_file_data-ltxa1.
            CONCATENATE 'A'
                        lv_ebeln
                        lv_ebelp
                        lv_zekkn
                        LV_AC_QUAN
                        LV_VPROZ
                        lv_sakto
                        lv_ps_psp_pnr
                        lv_post1
                        lv_aufnr
                        LV_KTEXT
                        lv_kostl
                        LV_CC_KTEXT
                        lv_nplnr
                        LV_NET_KTEXT
                        lv_vornr
                        lv_ltxa1
                        INTO LV_STR
                        SEPARATED BY '^^^'.
            TRANSFER LV_STR TO LV_FILEPATH.
            CLEAR: LV_STR,LV_AC_QUAN,LV_VPROZ,LV_KTEXT,LV_CC_KTEXT,LV_NET_KTEXT,
                   lv_ebeln,lv_ebelp,lv_zekkn,lv_sakto,lv_ps_psp_pnr,lv_post1,
                   lv_aufnr,lv_kostl,lv_nplnr,lv_vornr,lv_ltxa1.
            APPEND GS_FILE_DATA TO GT_FILE_DATA.
            CLEAR: GS_EKKN.
          ENDLOOP.   "End of GT_EKKN iteration
        ELSE.
          APPEND GS_FILE_DATA TO GT_FILE_DATA.
        ENDIF.
      ENDLOOP.    " End of GT_EKPO Iteration
    ENDIF.      "End of GT_EKPO Read
    CLEAR:GS_FILE_DATA.
  ENDLOOP.    " End of GT_EKKO iteration

* close file
  CLOSE DATASET LV_FILEPATH.
  IF SY-SUBRC IS INITIAL.
    WRITE: 'File Outputed Successfully to:', LV_FILEPATH.
  ENDIF.
  IF R_DELTA = 'X'.
    SELECT SINGLE * FROM ZMMT_SF_PARAM
     INTO GS_SF_PARAM
     WHERE PARAMTYPE = 'SAP_SF_PARAM'
       AND SUBTYPE = 'DATE_TIME'.

    GS_SF_PARAM-PREV_EXEC_DATE = SY-DATUM.
    GS_SF_PARAM-PREV_EXEC_TIME = GV_CURR_TIME.

    MODIFY ZMMT_SF_PARAM FROM GS_SF_PARAM.
    CLEAR GS_SF_PARAM.
  ELSEIF R_FULL EQ 'X'.
    GS_SF_PARAM-PARAMTYPE = 'SAP_SF_PARAM'.
    GS_SF_PARAM-SUBTYPE = 'DATE_TIME'.
    GS_SF_PARAM-PREV_EXEC_DATE = SY-DATUM.
    GS_SF_PARAM-PREV_EXEC_TIME = SY-UZEIT.
    MODIFY ZMMT_SF_PARAM FROM GS_SF_PARAM.
    CLEAR GS_SF_PARAM.
  ENDIF.

ENDFORM.                    " PREPARE_FILE_DATA
