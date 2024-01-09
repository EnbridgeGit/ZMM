*----------------------------------------------------------------------*
*   INCLUDE LMGD1O1K                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*    Prepare_Sub_Cl                             ch/4.6
* Vorbereitung zum Prozessieren oder Unterdrücken des Subscreens zur
* Merkmalsbewertung
*----------------------------------------------------------------------*
MODULE PREPARE_SUB_CL OUTPUT.

  DATA  : BEGIN OF OBJECTTAB OCCURS 1.
          INCLUDE STRUCTURE CLTABLE.
  DATA  : END   OF OBJECTTAB.
  DATA  : KZ_CHANGE_SERVICE(1).
  DATA  : LV_DISPLAY_MODUS LIKE RMCLF-KREUZ VALUE SPACE .    "H: 327936

  IF  T130M-AKTYP NE AKTYPH                                  "H: 327936
  AND T130M-AKTYP NE AKTYPV.
    LV_DISPLAY_MODUS = 'X'.
  ENDIF.

* Ermitteln der Klasse
  CALL FUNCTION 'T134_SINGLE_READ'
       EXPORTING
            T134_MTART = MARA-MTART
       IMPORTING
            WT134      = T134.

  IF  T134-CLASS NE SPACE
  AND T134-CTYPE NE SPACE
  AND AKTVSTATUS CA STATUS_C
  AND T130M-VERAR NE VERAR_PL
  AND T130M-VERAR NE VERAR_AD.
    CHECK CLO0_IS_OPEN = SPACE.
    SUB_PROG_CL = SAPLCTMS.
    SUB_DYNP_CL = CTMS4000.
*   SUB_DYNP_CL = '5100'.         testweise

*   IF init_clo0 = space.
*     init_clo0 = x.
*   Initialisierungsflag ist RMMZU-FLG_CLINIT
* note 549774: in der MGMI ist für Subscreens die *RMMZU aktuell,
* hier wird aber die RMMZU geholt
* die *RMMZU wird jeweils aktuell in die MGD1 repliziert
*    CALL FUNCTION 'MAIN_PARAMETER_GET_RMMZU'
*         IMPORTING
*              WRMMZU = RMMZU.
    IF RMMZU-FLG_CLINIT = SPACE.
      CALL FUNCTION 'CLO0_DDB_INIT'
           EXPORTING
                FRAME_TEXT_IMP = TEXT-020
           EXCEPTIONS
                OTHERS         = 1.
      IF SY-SUBRC = 0.
        RMMZU-FLG_CLINIT = X.
      ENDIF.
*     Übergabe der Objektmerkmale
      REFRESH OBJECTTAB.
      OBJECTTAB-TNAME = T_MARA.       "note 204906

*      OBJECTTAB-TABLE = MARA.                             "Unicode
      class CL_ABAP_CONTAINER_UTILITIES definition load.
      CALL METHOD CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
        EXPORTING
          IM_VALUE               = mara
        IMPORTING
          EX_CONTAINER           = OBJECTTAB-TABLE
        EXCEPTIONS
          ILLEGAL_PARAMETER_TYPE = 1
          others                 = 2
              .
      IF SY-SUBRC <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      APPEND OBJECTTAB.
      OBJECTTAB-TNAME = T_MAKT.
      OBJECTTAB-TABLE = MAKT.                                 "#EC ENHOK
      APPEND OBJECTTAB.
      CALL FUNCTION 'CTMS_OBJECT_AREA'
           TABLES
                OBJECTS = OBJECTTAB.
    ENDIF.

*   Berücksichtigung Änderungsnummer/-datum
    OBJECT_CLO0 = MARA-MATNR.
    IF RMMG1-AENNR IS INITIAL.
      CLEAR DATUM_CLO0.
      CLEAR KZ_CHANGE_SERVICE.
    ELSE.
      DATUM_CLO0 = RMMG1-AEDAT.
      KZ_CHANGE_SERVICE = 'X'.
    ENDIF.

*   Berücksichtigung Vorlage
    IF  T130M-AKTYP      =  AKTYPH
    AND RMMG1_REf-MATNR  <> SPACE
    AND RMMZU-CLASS_REF  =  SPACE.
      RMMZU-CLASS_REF = X.
      MATERIAL     = MARA-MATNR.
      REF_MATERIAL = RMMG1_REF-MATNR.
      TABELLE  = T_MARA.
      PTABLE   = T_MARA.
      CL_STATUS = 1.
      REF_ALL_TYPE = X.
      DO.
        CALL FUNCTION 'CLFM_OBJECT_CLASSIFICATION'
             EXPORTING  TABLE         = TABELLE
                        PTABLE        = PTABLE
                        OBJECT        = MATERIAL
                        OBJTXT        = MAKT-MAKTX
*                       CLASSTYPE     = KLASSENART
                        TYPETEXT      = ARTTEXT
                        CHANGE_SERVICE_NUMBER  = RMMG1-AENNR
                        DATE_OF_CHANGE         = DATUM_CLO0
                        OBJ_HAS_CHANGE_SERVICE = KZ_CHANGE_SERVICE
                        STATUS        = CL_STATUS
                        REF_OBJECT    = REF_MATERIAL
                        REF_ALL_TYPE  = REF_ALL_TYPE
                        REF_DATUV     = SY-DATUM
                        MEINS         = MARA-MEINS
             IMPORTING  UPDATEFLAG    = RMMG2-VB_KLAS
*                       CLASSTYPE     = KLASSENART
                        TYPETEXT      = ARTTEXT
                        OK_CODE       = SY-UCOMM
             EXCEPTIONS CLASSIFICATION_NOT_FOUND = 1
                        CLASS_NOT_VALID          = 2.
        IF SY-SUBRC = 0.
          MESSAGE S691.    "Klassif. der VorlMat wurde übernommen
        ENDIF.
        IF SY-UCOMM NE 'WECH'.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

* note 599655
  DATA: LV_MSGID LIKE SY-MSGID, LV_MSGTY LIKE SY-MSGTY,
        LV_MSGNO LIKE SY-MSGNO, LV_MSGV1 LIKE SY-MSGV1,
        LV_MSGV2 LIKE SY-MSGV2, LV_MSGV3 LIKE SY-MSGV3,
        LV_MSGV4 LIKE SY-MSGV4.
  LV_MSGID = SY-MSGID. LV_MSGTY = SY-MSGTY. LV_MSGNO = SY-MSGNO.
  LV_MSGV1 = SY-MSGV1. LV_MSGV2 = SY-MSGV2. LV_MSGV3 = SY-MSGV3.
  LV_MSGV4 = SY-MSGV4.


* Merkmalsbewertung aktivieren
    CALL FUNCTION 'CLO0_DDB_OBJ_VALUATION_OPEN'
         EXPORTING
              CLASSTYPE_IMP          = T134-CTYPE
              CLASS_IMP              = T134-CLASS
              CHANGE_NUMBER_IMP      = RMMG1-AENNR
              DATE_OF_CHANGE_IMP     = DATUM_CLO0
*             DISPLAY_MODUS          = SPACE
              DISPLAY_MODUS          = LV_DISPLAY_MODUS
              MTART_IMP              = T134-MTART
              OBJECT_IMP             = OBJECT_CLO0
              OBJ_STRUCTURE_IMP      = TCLA_OBTAB_MARA
         EXCEPTIONS
              INVALID_CLASSCAT       = 1
              CLASS_STATUS_NOT_VALID = 2
              CLASS_NOT_FOUND        = 3
              FOREIGN_LOCK           = 4
              SYSTEM_FAILURE         = 5
              CHANGE_NR_NOT_EXIST    = 6
              OTHERS                 = 7.
    IF SY-SUBRC NE 0.
      IF LV_DISPLAY_MODUS IS INITIAL.                       "note 523419
        IF LV_MSGID <> SY-MSGID OR LV_MSGTY <> SY-MSGTY OR
           LV_MSGNO <> SY-MSGNO OR LV_MSGV1 <> SY-MSGV1 OR
           LV_MSGV2 <> SY-MSGV2 OR LV_MSGV3 <> SY-MSGV3 OR
           LV_MSGV4 <> SY-MSGV4.                            "note 599655
          MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          MESSAGE S317(MM) WITH 'CLO0_DDB_OBJ_VALUATION_OPEN'.
        ENDIF.
        SUB_PROG_CL = 'SAPLMGD1'.
        SUB_DYNP_CL = '0001'.
        EXIT.
      ELSE.
        DATA ALLOCATIONS LIKE API_ALLOC occurs 0 with header line.
        DATA ob_key like api_ob_key occurs 0 with header line.
        DATA KEYF0 LIKE TCLO-KEYF0.
        REFRESH ALLOCATIONS.
        SELECT SINGLE KEYF0 FROM tclo
                            INTO KEYF0
                            WHERE obtab = TCLA_OBTAB_MARA.
        REFRESH ob_key.
        ob_key-FIELD = keyf0.
        ob_key-VALUE = MARA-MATNR.
        APPEND ob_key.
        CALL FUNCTION 'CACL_OBJECT_READ_ALLOCATIONS'
            EXPORTING
                 OBJECT_TYPE           = TCLA_OBTAB_MARA
                 CLASS_TYPE            = T134-CTYPE
            TABLES
                 OBJECT_IDENTIFICATION = ob_key
                 ALLOCATIONS           = ALLOCATIONS
            EXCEPTIONS
                 ERROR                 = 1
                 WARNING               = 2
                 OTHERS                = 3.
        IF SY-SUBRC = 0 or SY-SUBRC = 2.                  "note 1036050
          READ TABLE ALLOCATIONS WITH KEY CLASS = T134-CLASS.
          IF SY-SUBRC NE 0.
            MESSAGE S334(MM) WITH T134-CLASS T134-MTART MARA-MATNR.
          ELSE.
            MESSAGE S317(MM) WITH 'CLO0_DDB_OBJ_VALUATION_OPEN'.
          ENDIF.
        ELSE.
          MESSAGE S317(MM) WITH 'CLO0_DDB_OBJ_VALUATION_OPEN'.
        ENDIF.
      ENDIF.
    ENDIF.
    CLO0_IS_OPEN = X.
* note 549774: in der MGMI ist für Subscreens die *RMMZU aktuell,
* hier wird aber die RMMZU geholt
* die *RMMZU wird jeweils aktuell in die MGD1 repliziert
*    CALL FUNCTION 'MAIN_PARAMETER_SET_RMMZU'
*         EXPORTING
*              WRMMZU = RMMZU.
  ELSE.     "Klassifzizierung kann nicht durchgeführt werden
    SUB_PROG_CL = 'SAPLMGD1'.
    SUB_DYNP_CL = '0001'.
  ENDIF.

ENDMODULE.
