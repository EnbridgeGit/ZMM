report zmwmc003 no standard page heading line-size 255 line-count 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  zmwmc003
*    Programmer  :  Ric Aarssen
*    Created on  :  October 4, 1998
*----------------------------------------------------------------------
* (1) This program extracts data for Union Energy separation:
*               - purchasing information data
*     All Purchasing info records will be extracted for P105.
*       Cambridge Technologies will have to decide if they wish to
*       eliminate some info reocrds based on possibly Material Number
*       and Vendor.
*
* (2) Selection Criteria - plant
*                        - and logical file name
*
* (3) Output file:
*        Logical File  - zmwmc003_01
*        Physical File - /usr/sap/interfaces/P01/UEC/ZMWMC003.SAP
*
*----------------------------------------------------------------------
*    TABLE DECLARATIONS
*----------------------------------------------------------------------
TABLES:   MARC,                  "material master / plant
          MARA,                  "material master / general
          STXH,                  "Purchasing Info Record - text
          EINA,                  "Purchasing Info Record - General Data
          EINE.                  "Purchasing Info Record - Org. Data

*----------------------------------------------------------------------
*    INTERNAL TABLE DECLARATIONS
*----------------------------------------------------------------------
*data:   begin of mat_table occurs 0,
*          matnr            like mara-matnr,       "Material Number
*          lifnr            like eina-lifnr,       "Vendor
*          infnr            like eina-infnr,       "Info Record Id
*        end of mat_table.

*---------- READ_TEXT  Function Call Data Elements  --------------------
DATA:   BEGIN OF THEADTAB OCCURS 100.
         INCLUDE STRUCTURE THEAD.
DATA:   END OF THEADTAB.

DATA:   BEGIN OF TINLINETAB OCCURS 100.         "Aktuelle Inlinezeile
         INCLUDE STRUCTURE TLINE.
DATA:   END   OF TINLINETAB.
*-----------------------------------------------------------------------

*----------------------------------------------------------------------
*    OUTPUT RECORD LAYOUT
*----------------------------------------------------------------------
data:    outfile(100),
*--> purchase info record to be downloaded to Union Energy
         BEGIN OF INFOREC,
           LOEKZ     LIKE EINA-LOEKZ,       " deletion flag
           MATNR     LIKE MARC-MATNR,       " material number
           LIFNR     LIKE EINA-LIFNR,       " vendor number
           EKORG     LIKE EINE-EKORG,       " purchase organization
           IDNLF     LIKE EINA-IDNLF,       " vendor material number
           LMEIN     LIKE EINA-LMEIN,       " base unit of measure
           MEINS     LIKE EINA-MEINS,       " order unit of measure
           UMREN(5)  TYPE C,                " Denominator for Conversion
           UMREZ(5)  TYPE C,                " Numerator for Conversion
           MINBM(13) TYPE C,                " Minimum order quantity
           NORBM(13) TYPE C,                " Standard purchase quantity
           NETPR(11) TYPE C,                " net price
           PEINH(5)  TYPE C,                " price unit
           TEXT1(40) TYPE C,                " 1st Text line
           TEXT2(40) TYPE C,                " 2nd Text line
           TEXT3(40) TYPE C,                " 3rd Text line
           TEXT4(40) TYPE C,                " 4th Text line
         END OF INFOREC.

*----------------------------------------------------------------------
*    WORKING STORAGE DATA DECLARATIONS
*----------------------------------------------------------------------
DATA:    TDNAME      LIKE STXH-TDNAME.      "concatenate info text

*---------------------------------------------------------------------
*    SELECTION CRITERIA EVENT
*---------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(70) TEXT-001.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
    parameters: p_werks        like marc-werks obligatory
                               DEFAULT 'P105'.
    PARAMETERS: P_OUTFIL       LIKE FILENAME-FILEINTERN OBLIGATORY
                               DEFAULT 'ZMWMC003_01'.
  SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN END OF BLOCK BOX.

AT SELECTION-SCREEN.
  if ( p_werks is initial ) and
     ( P_OUTFIL IS INITIAL ).
     MESSAGE E100 WITH
         'Please enter plant and logical file'.
  ENDIF.

*   End of selection screen.

*---------------------------------------------------------------------
*    MAIN PROCESSING EVENT
*---------------------------------------------------------------------
START-OF-SELECTION.

* open the output file
  PERFORM OPEN_OUTPUT_FILE.

* display progress message
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = 'Building the output file'
       EXCEPTIONS
            OTHERS = 1.

* get the purchase information records data
SELECT * FROM MARC
    WHERE WERKS =  P_WERKS
      AND LVORM <> 'X'.

   SELECT * FROM EINA
      WHERE MATNR = MARC-MATNR
        AND LOEKZ =' '.                       "Eliminate flagged deleted
      IF SY-SUBRC = '0'.
         MOVE MARC-MATNR TO INFOREC-MATNR.
         MOVE EINA-LIFNR TO INFOREC-LIFNR.
         MOVE EINA-IDNLF TO INFOREC-IDNLF.
         MOVE EINA-LMEIN TO INFOREC-LMEIN.
         MOVE EINA-MEINS TO INFOREC-MEINS.
         MOVE EINA-UMREN TO INFOREC-UMREN.
         MOVE EINA-UMREZ TO INFOREC-UMREZ.

         SELECT SINGLE * FROM MARA
            WHERE MATNR = MARC-MATNR.
*            if sy-subrc ne 0.  'should always be a mara if marc exists
*              write: / 'UNABLE TO find material master', sy-subrc.
*             else.
             IF MARA-MATKL = '0300'.
               MOVE 'MDSE'   TO INFOREC-EKORG.
              ELSE.
               MOVE 'MATL'   TO INFOREC-EKORG.
             ENDIF.
*            endif.

         SELECT * FROM EINE
            WHERE INFNR = EINA-INFNR
              AND EKORG = INFOREC-EKORG.
            IF SY-SUBRC = '0'.
              MOVE EINE-MINBM TO INFOREC-MINBM.
              MOVE EINE-NORBM TO INFOREC-NORBM.
              MOVE EINE-NETPR TO INFOREC-NETPR.
              MOVE EINE-PEINH TO INFOREC-PEINH.
              CONCATENATE EINE-INFNR EINE-EKORG '0' INTO TDNAME.

* get the information text lines for the information records
              PERFORM GET_INFO_TEXT.
                 IF SY-SUBRC = 0.                 "Print Info text
                   LOOP AT TINLINETAB.
                     CASE SY-TABIX.
                       WHEN 1.
                         MOVE TINLINETAB-TDLINE TO INFOREC-TEXT1.
                       WHEN 2.
                         MOVE TINLINETAB-TDLINE TO INFOREC-TEXT2.
                       WHEN 3.
                         MOVE TINLINETAB-TDLINE TO INFOREC-TEXT3.
                       WHEN 4.
                         MOVE TINLINETAB-TDLINE TO INFOREC-TEXT4.
                     ENDCASE.
                   ENDLOOP.
                 ENDIF.
* create info record output
              TRANSFER INFOREC TO OUTFILE LENGTH 272.
              CLEAR INFOREC.
            ENDIF.
         ENDSELECT.
      ENDIF.
   ENDSELECT.
ENDSELECT.
* close the dataset
  CLOSE DATASET OUTFILE.

  MESSAGE I100 WITH 'Processing has completed'.
END-OF-SELECTION.

*-----------------------------------------------------------------------
*   FORM OPEN_OUTPUT_FILE.
*-----------------------------------------------------------------------
*  -  Attempts to open the physical file to determine if there are any
*     errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_OUTPUT_FILE.
  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            CLIENT           = SY-MANDT
            LOGICAL_FILENAME = P_OUTFIL
            OPERATING_SYSTEM = SY-OPSYS
       IMPORTING
            FILE_NAME        = OUTFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E006 WITH P_OUTFIL.
  ELSE.
    OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*   FORM GET_INFO_TEXT.
*-----------------------------------------------------------------------
*  -  retrieves the detail text lines for the purchasing information
*     record.  We are only going after the first four lines.
*-----------------------------------------------------------------------
FORM GET_INFO_TEXT.
  SELECT * FROM STXH
    WHERE TDOBJECT = 'EINE'
      AND TDNAME = TDNAME
      AND TDID = 'BT  '
      AND TDSPRAS = SY-LANGU.
  IF SY-SUBRC = '0'.
     MOVE-CORRESPONDING STXH TO THEADTAB.
     APPEND THEADTAB.
     CALL FUNCTION 'READ_TEXT'
       EXPORTING
           ID                           = THEADTAB-TDID
           LANGUAGE                     = THEADTAB-TDSPRAS
           NAME                         = THEADTAB-TDNAME
           OBJECT                       = THEADTAB-TDOBJECT
       IMPORTING HEADER                 = THEADTAB
       TABLES LINES                     = TINLINETAB
       EXCEPTIONS
           ID                           = 1
           LANGUAGE                     = 2
           NAME                         = 3
           NOT_FOUND                    = 4
           OBJECT                       = 5
           OTHERS                       = 6.
  ENDIF.
 IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO process info text records', SY-SUBRC.
 ENDIF.

  ENDSELECT.

ENDFORM.

*---------------------------------------------------------------------*
*       THIS IS THE END                                               *
*---------------------------------------------------------------------*
