REPORT ZEXTRACT LINE-SIZE 79 LINE-COUNT 59 MESSAGE-ID ZM.
*----------------------------------------------------------------------
* Program:    ZMMMI008
* Created On: Nov 21, 1996.
* Created By: Joanne Lee (Omnilogic Systems Group)
*----------------------------------------------------------------------
* (1) This program extracts data for DB2 table:
*        corpu.manufacturer_part
* (2) Output file:
*        Logical File  - ZMMMI006_01
*        Physical File - /usr/sap/interfaces/D30/DRXX0104/ZMMMI006.SAP
* (3) The structure of the output is:
*      start    end
*       col     col   width              field description
*      -----   -----  -----              -----------------
*        1      20      20               Manufacturer Part Number
*       21      38      18               Material ID
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* data declaration
*----------------------------------------------------------------------
TABLES: MARA.  "Material Master: General Data

DATA: BEGIN OF ZAUSP OCCURS 50.
      INCLUDE STRUCTURE AUSP.
DATA: END OF ZAUSP.

DATA: MATNR_OBJ  LIKE AUSP-OBJEK,
      FEATURE    LIKE CABNT-ATBEZ,
      MANU_ATINN LIKE AUSP-ATINN.     "manu. part #, internal#  GSPARTAL

DATA: PART_NUM(20) TYPE C.   "part number

CONSTANTS: LEN TYPE I VALUE 38.          "length of a line to transfer

DATA: LINE(38) TYPE C.                   "line of data to be transferred
DATA: MESS(100) TYPE C.                  "storing error message
DATA: P_FILE LIKE FILENAME-FILEINTERN.   "physical file name

*----------------------------------------------------------------------
* parameters
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME.
SELECT-OPTIONS: P_MATNR FOR MARA-MATNR.              "material id
SELECT-OPTIONS: P_MTART FOR MARA-MTART.              "material type
SELECT-OPTIONS: P_MATKL FOR MARA-MATKL.              "material group
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME.
PARAMETERS:     DOWNLOAD AS CHECKBOX.                "download or not
PARAMETERS:     L_FILE LIKE FILENAME-FILEEXTERN      "dataset for output
                DEFAULT 'ZMMMI006_01'.
SELECTION-SCREEN END OF BLOCK BLK2.

*----------------------------------------------------------------------
* open file for output
*----------------------------------------------------------------------
START-OF-SELECTION.
IF DOWNLOAD = 'X'.
   CALL FUNCTION 'FILE_GET_NAME'
        EXPORTING CLIENT           = SY-MANDT
                  LOGICAL_FILENAME = L_FILE
                  OPERATING_SYSTEM = SY-OPSYS
        IMPORTING FILE_NAME        = P_FILE
        EXCEPTIONS FILE_NOT_FOUND  = 1.
   OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE MESSAGE MESS.
   IF SY-SUBRC <> 0.
      MESSAGE E009 WITH L_FILE MESS.
      EXIT.
   ENDIF.
ENDIF.

*----------------------------------------------------------------------
* main program
*----------------------------------------------------------------------
PERFORM ATINN USING 'MANUFACTURER_PART_NUMBER'
              CHANGING MANU_ATINN.

SELECT * FROM MARA WHERE MATNR IN P_MATNR       "material id
                     AND MTART IN P_MTART       "material type
                     AND MATKL IN P_MATKL.      "material group

   MOVE MARA-MATNR TO MATNR_OBJ.
   CALL FUNCTION 'CLFM_SELECT_AUSP' EXPORTING MAFID     = 'O'
                                              CLASSTYPE = '001'
                                              OBJECT    = MATNR_OBJ
                                    TABLES EXP_AUSP = ZAUSP
                                    EXCEPTIONS NO_VALUE = 1
                                               OTHERS   = 2.

    IF  SY-SUBRC = 0.                                          "GSPARTAL
      SORT ZAUSP BY ATINN.                                     "GSPARTAL
      LOOP AT ZAUSP.
         IF ZAUSP-ATINN EQ MANU_ATINN.
            MOVE: SPACE TO LINE.
            MOVE: ZAUSP-ATWRT      TO LINE+0(20),  "manufacturer part id
                  MARA-MATNR+12(6) TO LINE+32(06). "material id
            WRITE:/ LINE.
            IF DOWNLOAD = 'X'.
               TRANSFER LINE TO P_FILE LENGTH LEN.
            ENDIF.
         ENDIF.
      ENDLOOP.
    ENDIF.
ENDSELECT.

*   IF SY-SUBRC = 0.
*      LOOP AT ZAUSP.
*         "GET THE MANUFACTURING PART ID
*         CALL FUNCTION 'CTUT_FEATURE_CHECK'
*              EXPORTING  CLASS_TYPE       = '001'
*                         FEATURE_ID       = ZAUSP-ATINN
*              IMPORTING  FEATURE_NAME     = FEATURE
*              EXCEPTIONS OTHERS           = 6.
*         IF FEATURE = 'MANUFACTURER_PART_NUMBER'.
*            MOVE: SPACE TO LINE.
*            MOVE: ZAUSP-ATWRT      TO LINE+0(20),  "manufacturer part i
*                  MARA-MATNR+12(6) TO LINE+32(06). "material id
*            WRITE:/ LINE.
*            IF DOWNLOAD = 'X'.
*               TRANSFER LINE TO P_FILE LENGTH LEN.
*            ENDIF.
*            EXIT.
*         ENDIF.
*      ENDLOOP.
*   ENDIF.
*ENDSELECT.

*----------------------------------------------------------------------
* close output file
*----------------------------------------------------------------------
IF DOWNLOAD = 'X'.
   CLOSE DATASET P_FILE.
ENDIF.

*---------------------------------------------------------------------*
*       FORM ATINN                                             "GSPARTAL
*---------------------------------------------------------------------*
*  -->  CHARACT is the name of the characteristic passed              *
*  -->  G_ATINN is the internal number for the charac. returned       *
*---------------------------------------------------------------------*
FORM ATINN USING CHARACT CHANGING G_ATINN.                     "GSPARTAL

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
ENDFORM.                                                       "GSPARTAL
