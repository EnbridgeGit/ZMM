REPORT ZMWMC006 LINE-SIZE 79 LINE-COUNT 65 MESSAGE-ID ZM.
*----------------------------------------------------------------------
* Program    : ZMWMC006
* Created On : September 23, 1998    Last Changed : September 28, 2998
* Created By : Ric Aarssen           Last Chged by: Ric Aarssen
*----------------------------------------------------------------------
* - 98/09/28 raarssen -
*   Replace the hard coded characteristic name with a parameter and
*     changed the layout of the output to be material number then
*     characterisic value
*----------------------------------------------------------------------
* (1) This program extracts characteristic data for Union Energy
*     Master Data Load by material number.
*
* (2) Output file:
*        Logical File  - ZMWMC006_01
*        Physical File - /usr/sap/interfaces/D30/UEC/ZMWMC006.SAP
* (3) The structure of the output is:
*      start    end
*       col     col   width              field description
*      -----   -----  -----              -----------------
*        1       6       6               Material Number
*        8      38      30               Characteristic Value
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* data declaration
*----------------------------------------------------------------------
TABLES: MARA,  "Material Master: General Data
        AUSP,  "Characteristics Values
        CABN.  "Characteristics

DATA: C_OBJEK LIKE AUSP-OBJEK.     "key for object in chracteristics
DATA: C_ATNAM LIKE CABN-ATNAM.     "characteristic name
DATA: C_ATINN LIKE AUSP-ATINN.     "internal characteristic id

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
PARAMETERS:     L_CHAR LIKE CABN-ATNAM.              "Characteristic
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME.
PARAMETERS:     DOWNLOAD AS CHECKBOX.                "download or not
PARAMETERS:     L_FILE LIKE FILENAME-FILEEXTERN      "dataset for output
                DEFAULT 'ZMWMC006_01'.
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

* find out the internal characteristic id for "manuf's part number"
* Note that characteristic name & internal characteristic id is
* 1 to 1 relation because SAP does not allow creating a characteristic
* with a name already existed on the system (transaction CT01).
* Therefore, characteristic name is unique.
MOVE L_CHAR TO C_ATNAM.
SELECT * FROM CABN WHERE ATNAM = C_ATNAM.
   MOVE CABN-ATINN TO C_ATINN.
ENDSELECT.

* find out the part # of the material falls within selection criteria
SELECT * FROM MARA WHERE MATNR IN P_MATNR       "material id
                     AND MTART IN P_MTART       "material type
                     AND MATKL IN P_MATKL.      "material group

   MOVE MARA-MATNR TO C_OBJEK.

   SELECT * FROM AUSP WHERE OBJEK = C_OBJEK      "Object
                        AND ATINN = C_ATINN      "Internal Charact. No.
                        AND MAFID = 'O'          "Object/Class
                        AND KLART = '001'.       "class Type
      MOVE: SPACE TO LINE.                       "initialization
      MOVE: MARA-MATNR+12(6) TO LINE+0(06).      "material id
      MOVE: AUSP-ATWRT       TO LINE+8(30).      "characteristic value
      IF DOWNLOAD = SPACE.
        WRITE:/ LINE.
      ELSE.
         TRANSFER LINE TO P_FILE LENGTH LEN.
      ENDIF.
   ENDSELECT.
ENDSELECT.

*----------------------------------------------------------------------
* close output file
*----------------------------------------------------------------------
IF DOWNLOAD = 'X'.
   CLOSE DATASET P_FILE.
ENDIF.
