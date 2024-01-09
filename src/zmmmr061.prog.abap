REPORT ZMMMR061.
*
************************************************************************
*  Author:        Mohammad T. Khan
*  Date:          April, 2003.
*  Description:
*     - The purpose of this program is to compare the INCO terms of
*       Vendor Master record purchasing organization data and Purchasing
*       info record.
************************************************************************
*  CHANGES/CORRECTIONS
* 2012/09/04 M Khan   TR995 Change C: drive to H: drive with directory,
*                           file selection using F4 & move the
*                           hard-coded file path/name to variant.
*
************************************************************************

TABLES:   EINA,            "Purchasing Info Record: General Data
          EINE,            "Purchasing Info Record: Purchasing Org. Data
          LFM1,            "Vendor Master Record Purchasing Org. data
          LFA1,            "Vendor Master (General Section)
          MARA.            "General Material Data
*
DATA:
    BEGIN OF EXCLTAB OCCURS 0,
        LIFNR        LIKE EINA-LIFNR,          "Vendor #
        NAME1        LIKE LFA1-NAME1,          "Vendor Name
        INCO11(20) type c,    "liKE LFM1-INCO1, "NCO1-Vendor Master Rec.
        INCO12(20) type c,    "LiKE EINE-INCO1, "INCO1 -Info Record
        INFNR        LIKE EINA-INFNR,          "Info Record Number
        MATNR        LIKE EINA-MATNR,          "Material Number
        MATKL        LIKE MARA-MATKL,          "Material Group
    END OF EXCLTAB.

DATA: BEGIN OF PROT_HEADER OCCURS 1,
        SPALTENNAME(20) TYPE C,
        DDIC_TABLE(5)   TYPE C,
        DDIC_FIELD(5)   TYPE C,
        KEY             TYPE C,
      END OF PROT_HEADER.

DATA  ERRORTAB    LIKE HRERROR    OCCURS 0 WITH HEADER LINE.
DATA: RETCODE   LIKE SY-SUBRC,
      W_REPTTL  LIKE SY-TITLE,          "Report Title
      W_OPTION(11) TYPE C VALUE 'START_EXCEL',
      W_HEAD01(40) TYPE C,
      W_HEAD02(25) TYPE C.

DATA: INSERT_FLAG.
*-----------------------------------------------------------------------
*    Start of the selection screen
*-----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:  SLIFNR FOR EINA-LIFNR,                 "Vendor #
                 SMATKL FOR EINA-MATKL,                 "Material Group
                 SMATNR FOR EINA-MATNR.                 "Material #

PARAMETERS: PEKORG LIKE EINE-EKORG DEFAULT 'MATL'.      "Purchasing Org.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-002.
PARAMETERS: INCOALL RADIOBUTTON GROUP RBG1,     "Display all records
            INCOMM  RADIOBUTTON GROUP RBG1,     "Display Mismatch recs.
            INCOMIS RADIOBUTTON GROUP RBG1,     "Display Missing INCO
            INCOMMS RADIOBUTTON GROUP RBG1.     "Missing/Mismatch recs.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-003.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBG2,            "PRINT REPORT
                P_EXCL RADIOBUTTON GROUP RBG2,            "EXCEL SHEET
                P_FILE LIKE RLGRAP-FILENAME DEFAULT 'H:\SAPTEMP'. "TR995
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX.

******************************************************************
*                   AT SELECTION-SCREEN                          *
******************************************************************
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      P_FILE = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILE.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON P_FILE.
   PERFORM CHECK_FILE_PATH.
*End of TR995 changes
*-----------------------------------------------------------------------
*    End of selection screen
*-----------------------------------------------------------------------

START-OF-SELECTION.

 SELECT  LFM1~LIFNR LFM1~INCO1 LFA1~NAME1
   INTO (LFM1-LIFNR, LFM1-INCO1, LFA1-NAME1)
   FROM ( LFM1 INNER JOIN LFA1
          ON LFM1~LIFNR = LFA1~LIFNR )
  WHERE  LFM1~LIFNR  IN  SLIFNR
    AND  LFM1~EKORG  =   PEKORG
    AND  LFM1~LOEVM  =   SPACE.

     IF SY-SUBRC = 0.
        MOVE 'Y'  TO  INSERT_FLAG.

        SELECT EINE~INCO1 EINA~INFNR EINA~MATNR MARA~MATKL
         INTO (EINE-INCO1, EINA-INFNR, EINA-MATNR, MARA-MATKL)
         FROM ( ( EINA INNER JOIN EINE
                  ON EINA~INFNR = EINE~INFNR )
                       INNER JOIN MARA
                  ON EINA~MATNR = MARA~MATNR )

         WHERE  EINA~LIFNR  =   LFM1-LIFNR
           AND  EINA~MATNR  IN  SMATNR
           AND  EINA~LOEKZ  =   SPACE
           AND  EINE~LOEKZ  =   SPACE
           AND  MARA~MATKL  IN  SMATKL
           AND  MARA~LVORM  =   SPACE.

         IF SY-SUBRC = 0.
            MOVE: LFM1-LIFNR   TO  EXCLTAB-LIFNR,
                  LFM1-INCO1   TO  EXCLTAB-INCO11,
                  LFA1-NAME1   TO  EXCLTAB-NAME1,
                  EINE-INCO1  TO  EXCLTAB-INCO12,
                  EINA-INFNR  TO  EXCLTAB-INFNR,
                  EINA-MATNR  TO  EXCLTAB-MATNR,
                  MARA-MATKL  TO  EXCLTAB-MATKL,
                  'N'         TO  INSERT_FLAG.
            APPEND EXCLTAB.
            CLEAR EXCLTAB.
         ENDIF.
        ENDSELECT.
        IF INSERT_FLAG = 'Y'.
            MOVE: LFM1-LIFNR   TO  EXCLTAB-LIFNR,
                  LFM1-INCO1   TO  EXCLTAB-INCO11,
                  LFA1-NAME1   TO  EXCLTAB-NAME1.
           APPEND EXCLTAB.
           CLEAR EXCLTAB.
        ENDIF.
      ENDIF.
  ENDSELECT.


LOOP AT EXCLTAB.

     IF INCOMM = 'X'.                      "Mismatch INCOs
        IF EXCLTAB-INCO11 <> EXCLTAB-INCO12.
        ELSE.
           DELETE EXCLTAB.
        ENDIF.
     ENDIF.

     IF INCOMIS = 'X'.                     "Missing INCOs
        IF EXCLTAB-INCO11 = SPACE  AND
           EXCLTAB-INCO12 = SPACE.
        ELSE.
           DELETE EXCLTAB.
        ENDIF.
     ENDIF.

      IF INCOMMS = 'X'.                    "Mismatch and Missing INCOs
        IF EXCLTAB-INCO11 = EXCLTAB-INCO12 AND
           EXCLTAB-INCO11 <> SPACE.
           DELETE EXCLTAB.
        ENDIF.
     ENDIF.
 ENDLOOP.

 PERFORM PROT_HEADER.
 CONCATENATE SY-REPID  '-'  TEXT-TTL INTO W_REPTTL
             SEPARATED BY SPACE.
 MOVE TEXT-016  TO W_HEAD01+0(5).
 WRITE SY-DATUM TO W_HEAD01+6(10).
 MOVE TEXT-017  TO W_HEAD01+24(5).
 WRITE SY-UZEIT TO W_HEAD01+30(10).

 MOVE TEXT-CLT  TO W_HEAD02+0(7).
 MOVE SY-MANDT  TO W_HEAD02+8(4).
 MOVE SY-SYSID  TO W_HEAD02+13(5).

*  IF P_RPRT = 'X'.
*    CLEAR W_OPTION.
*    IF SY-BATCH = 'X'.
*      W_OPTION = 'LINESELMOD:1'.
*    ENDIF.
*  ENDIF.

  if p_rprt = 'X'.
     clear w_option.
     perform print_report.
  else.
     perform create_spreadsheet.
  endif.

*-----------------------------------------------------------------------
form print_report.
     CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
          EXPORTING
            BASIC_LIST_TITLE        = W_REPTTL
            HEAD_LINE1              = W_HEAD01
            HEAD_LINE2              = W_HEAD02
               FILE_NAME            = SY-CPROG
            ADDITIONAL_OPTIONS      = W_OPTION
          IMPORTING
               RETURN_CODE          = RETCODE
          TABLES
               DATA_TAB             = EXCLTAB
               FIELDNAME_TAB        = PROT_HEADER
               ERROR_TAB            = ERRORTAB
          EXCEPTIONS
               DOWNLOAD_PROBLEM     = 1
               NO_DATA_TAB_ENTRIES  = 2
               TABLE_MISMATCH       = 3
               PRINT_PROBLEMS       = 4
               OTHERS               = 5.
  IF SY-SUBRC NE 0.
    WRITE: /1 'TABLE DOWNLOAD UNSUCCESSFULL - REASON = ', SY-SUBRC.
  ENDIF.

endform.
*---------------------------------------------------------------------*
*       FORM PROT_HEADER                                              *
*---------------------------------------------------------------------*
FORM PROT_HEADER.
  MOVE TEXT-CL1 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL2 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL3 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL4 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL5 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL6 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL7 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
ENDFORM.                               " PROT_HEADER

form create_spreadsheet.

data: begin of lt_fnames occurs 0,
      text(60)  type c,
      end of lt_fnames.

  lt_fnames-text = text-cl1.
  append lt_fnames.
  lt_fnames-text = text-cl2.
  append lt_fnames.
  lt_fnames-text = text-cl3.
  append lt_fnames.
  lt_fnames-text = text-cl4.
  append lt_fnames.
  lt_fnames-text = text-cl5.
  append lt_fnames.
  lt_fnames-text = text-cl6.
  append lt_fnames.
  lt_fnames-text = text-cl7.
  append lt_fnames.

  call function 'MS_EXCEL_OLE_STANDARD_DAT'
       exporting
*          file_name                 = 'c:\saptemp' "TR995
           file_name                 = P_FILE       "TR995
           create_pivot              = 0
       tables
           data_tab                  = excltab
           fieldnames                = lt_fnames
       exceptions
           file_not_exist            = 1
           filename_expected         = 2
           communication_error       = 3
           ole_object_method_error   = 4
           ole_object_property_error = 5
           invalid_filename          = 6
           invalid_pivot_fields      = 7
           download_problem          = 8
           others                    = 9.

  if sy-subrc <> 0.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM CHECK_FILE_PATH.
DATA: sep_file type string,
      sep_path type string,
      LV_BOL TYPE C.        "abap_bool.

*Separate Path and file
     CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
       EXPORTING
         FULL_NAME           = P_FILE
      IMPORTING
        STRIPPED_NAME       = sep_file
        FILE_PATH           = sep_path
      EXCEPTIONS
        X_ERROR             = 1
        OTHERS              = 2
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

*lv_dir = sep_path.
IF sep_path CS 'C:' OR sep_path CS 'c:'.
   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
ELSE.
*Check if directory path exist or not.
CALL METHOD cl_gui_frontend_services=>directory_exist
  EXPORTING
    directory            = sep_path      "lv_dir
  RECEIVING
    result               = lv_bol
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    wrong_parameter      = 3
    not_supported_by_gui = 4
    OTHERS               = 5.
IF lv_bol IS INITIAL.
   CONCATENATE TEXT-099 sep_path sep_file into SEP_PATH.
*   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH w_error.
   MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH SEP_PATH.
ENDIF.
ENDIF.
ENDFORM.

******************** END OF PROGRAM ************************************
