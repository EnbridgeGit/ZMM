REPORT ZMMMI009 LINE-SIZE 255 LINE-COUNT 59 MESSAGE-ID ZM.

*----------------------------------------------------------------------
* Program:    ZMMMI009
* Created On: DEC 6, 1996.
* Created By: Gus Spartalis (Omnilogic Systems Group)
*----------------------------------------------------------------------
* (1) This program extracts data for DB2 table:
*        corpu.stock_item
* (2) Output file:
*        Logical File  - ZMMMI005_01
*        Physical File - /usr/sap/interfaces/D30/DRXX0104/ZMMMI005.SAP
* (3) The structure of the output is:
*      start    end
*       col     col   width              field description
*      -----   -----  -----              -----------------
*        1      40      40               Material description
*       41      58      18               Material ID
*       59      74      16               Current selling price
*       75     104      30               Primary description
*      105     134      30               Secondary description
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* data declaration
*----------------------------------------------------------------------
TABLES: MARA,                          "Material Master: General Data
        MAKT,                          "Material Descriptions
        A904,                          "Material
        AUSP,                          "Characteristic Values   GSPARTAL
        KONP.                          "Conditions (Item)

                                       "return value from call function
DATA: BEGIN OF ZAUSP OCCURS 50.
        INCLUDE STRUCTURE AUSP.
DATA: END OF ZAUSP.

DATA: MATNR_OBJ LIKE AUSP-OBJEK,       "parameter to call function
      FEATURE   LIKE CABNT-ATBEZ.      "parameter to call function

DATA: PRI_DESC(30) TYPE C,             "primary description
      SEC_DESC(30) TYPE C,             "secondary description
      P_ATINN      LIKE AUSP-ATINN,
      S_ATINN      LIKE AUSP-ATINN.

CONSTANTS: LEN TYPE I VALUE 134.       "length of a line to transfer

DATA: LINE(134) TYPE C.                "line of data to be transferred
DATA: FOUND(1)  TYPE C.                "found pri/sec description?
DATA: MESS(100) TYPE C.                "storing error message
DATA: P_FILE LIKE FILENAME-FILEINTERN. "physical file name

*----------------------------------------------------------------------
* parameters
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME.
SELECT-OPTIONS: P_MATNR FOR MARA-MATNR."material id
SELECT-OPTIONS: P_MTART FOR MARA-MTART."material type
SELECT-OPTIONS: P_MATKL FOR MARA-MATKL."material group
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME.
PARAMETERS:     DOWNLOAD AS CHECKBOX.  "download or not
PARAMETERS:     L_FILE LIKE FILENAME-FILEEXTERN      "dataset for output
                DEFAULT 'ZMMMI005_01'.
SELECTION-SCREEN END OF BLOCK BLK2.

*----------------------------------------------------------------------
* open file for output
*----------------------------------------------------------------------
START-OF-SELECTION.
  IF DOWNLOAD = 'X'.
    CALL FUNCTION 'FILE_GET_NAME'
         EXPORTING
              CLIENT           = SY-MANDT
              LOGICAL_FILENAME = L_FILE
              OPERATING_SYSTEM = SY-OPSYS
         IMPORTING
              FILE_NAME        = P_FILE
         EXCEPTIONS
              FILE_NOT_FOUND   = 1.
    OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE MESSAGE MESS.
    IF SY-SUBRC <> 0.
      MESSAGE E009 WITH L_FILE MESS.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------
* main program
*----------------------------------------------------------------------
  PERFORM ATINN USING 'PRIMARY_DESCRIPTION' CHANGING P_ATINN.
  PERFORM ATINN USING 'SECONDARY_DESCRIPTION' CHANGING S_ATINN.

  SELECT * FROM MARA WHERE MATNR IN P_MATNR       "material id
                       AND MTART IN P_MTART       "material type
                       AND MATKL IN P_MATKL.      "material group

    "clear all tables header
    CLEAR: A904, KONP, MAKT.

    "find out the condition record # for selling price
    SELECT SINGLE * FROM A904 WHERE KAPPL =  'V '
                                AND KSCHL =  'ZR00'
                                AND MATNR =  MARA-MATNR
                                AND DATBI >= SY-DATUM.

    "find out the selling price using the condition record # above
    SELECT SINGLE * FROM KONP WHERE KNUMH = A904-KNUMH
                                AND KOPOS = '01'.

    "find out the description for the material master record
    SELECT SINGLE * FROM MAKT WHERE MATNR = MARA-MATNR
                                AND SPRAS = 'E'.

    "find out the primary / secondary description
    MOVE MARA-MATNR TO MATNR_OBJ.
    CALL FUNCTION 'CLFM_SELECT_AUSP'
         EXPORTING
              MAFID     = 'O'
              CLASSTYPE = '001'
              OBJECT    = MATNR_OBJ
         TABLES
              EXP_AUSP  = ZAUSP
         EXCEPTIONS
              NO_VALUE  = 1
              OTHERS    = 2.

    IF  SY-SUBRC = 0.
      SORT ZAUSP BY ATINN.
      READ TABLE ZAUSP WITH KEY ATINN = P_ATINN BINARY SEARCH.
      IF SY-SUBRC = 0.
         MOVE ZAUSP-ATWRT      TO    PRI_DESC.
      ENDIF.

      READ TABLE ZAUSP WITH KEY ATINN = S_ATINN BINARY SEARCH.
      IF SY-SUBRC = 0.
         MOVE ZAUSP-ATWRT      TO    SEC_DESC.
      ENDIF.
    ENDIF.


*   IF SY-SUBRC = 0.
*      FOUND = '0'.
*      LOOP AT ZAUSP.
*         CALL FUNCTION 'CTUT_FEATURE_CHECK'
*              EXPORTING  CLASS_TYPE       = '001'
*                         FEATURE_ID       = ZAUSP-ATINN
*              IMPORTING  FEATURE_NAME     = FEATURE
*              EXCEPTIONS OTHERS           = 6.
*
*         "GET THE PRIMARY DESCRIPTION
*         IF FEATURE+0(19) = 'PRIMARY_DESCRIPTION'.
*            MOVE ZAUSP-ATWRT TO PRI_DESC.
*            IF FOUND = '0'.      FOUND = '1'.
*            ELSEIF FOUND = '1'.  EXIT.
*            ENDIF.
*         "GET THE SECONDARY DESCRIPTION
*         ELSEIF FEATURE+0(21) = 'SECONDARY_DESCRIPTION'.
*            MOVE ZAUSP-ATWRT TO SEC_DESC.
*            IF FOUND = '0'.      FOUND = '1'.
*            ELSEIF FOUND = '1'.  EXIT.
*            ENDIF.
*         ENDIF.
*      ENDLOOP.
*  ENDIF.

    MOVE: SPACE TO LINE.               "initialization
    MOVE: MAKT-MAKTX       TO LINE+0(40),   "material description
          MARA-MATNR+12(6) TO LINE+52(6),   "material id
          KONP-KBETR       TO LINE+58(16),  "selling price
          PRI_DESC         TO LINE+74(30),  "primary description
          SEC_DESC         TO LINE+104(30). "secondary description
    WRITE:/ LINE.
    IF DOWNLOAD = 'X'.
      TRANSFER LINE TO P_FILE LENGTH LEN.
    ENDIF.
ENDSELECT.

*----------------------------------------------------------------------
* close output file
*----------------------------------------------------------------------
  IF DOWNLOAD = 'X'.
    CLOSE DATASET P_FILE.
  ENDIF.



*---------------------------------------------------------------------*
*       FORM ATINN                                                    *
*---------------------------------------------------------------------*
*  -->  CHARACT is the name of the characteristic passed              *
*  -->  G_ATINN is the internal number for the charac. returned       *
*---------------------------------------------------------------------*
FORM ATINN USING CHARACT CHANGING G_ATINN.

  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
            CLASSTYPE                   = '001'
*            FEATURE_ID                  = CHAR_TAB-ATINN
            FEATURE_NEUTRAL_NAME         = CHARACT
       IMPORTING
*            FEATURE_NAME                = NAME
*            FEATURE_NEUTAL_NAME         = NEUTNAME
            FEATURE_ID                  =  G_ATINN
       EXCEPTIONS
            INVALID_CLASS_TYPE          = 1
            MISSING_FEATURE_INFORMATION = 2
            NO_FEATURE_FOUND            = 3
            NO_FEATURE_VALID            = 4
            NO_LANGUAGE                 = 5
            OTHERS                      = 6.
  IF  SY-SUBRC <> 0.
    WRITE: / 'Unable to determine internal characteristic'.
  ENDIF.

ENDFORM.
