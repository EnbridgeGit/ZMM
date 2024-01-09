REPORT ZMMMC006 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZMMMC006
*    PROGRAMMER  :  Marv Radsma - OMNILOGIC SYSTEMS GROUP
*    Client      :  Union Gas Limited
*    Date        :  March 1998
*
* This ABAP creates two materials movement BDC sessions.  The first
* transfers material quantities from the Centra plants using a 303
* movement type. The second receives the quantities in the new Union
* plants for Centra using a 305 movement type.
************************************************************************

TABLES:  MARA,                          " material master
         MARD.                          " material master: Storage Loc

DATA:    T_CODE      LIKE BMM00-TCODE VALUE 'MB1B',
         QUANT(13)   TYPE C,
         GROUP(12)   TYPE C,
         MTYPE(3)    TYPE C,
         IDX(2)      TYPE N,
         XMATNR(14)  VALUE 'MSEG-MATNR(  )',
         XERFMG(14)  VALUE 'MSEG-ERFMG(  )',
         DATE        LIKE SY-DATUM.

DATA:    BEGIN OF IMARD OCCURS 5000,
           WERKS     LIKE MARD-WERKS,                 " Union plant code
           LGORT     LIKE MARD-LGORT,                 " storage location
           MATNR     LIKE MARD-MATNR,                 " material number
           LABST     LIKE MARD-LABST,                 " quantity
           CGO_PLT   LIKE MARD-WERKS,                 " Centra plant cde
         END OF IMARD.

DATA  : BEGIN OF BDCDATA OCCURS 100.                   "Batch Input Data
          INCLUDE STRUCTURE BDCDATA.
DATA  : END OF BDCDATA.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(30) TEXT-001.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    SELECT-OPTIONS: S_WERKS    FOR MARD-WERKS OBLIGATORY
                               DEFAULT 'P200' TO 'P299'.
  SELECTION-SCREEN END OF BLOCK BOX2.
  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
    SELECT-OPTIONS: S_LGORT    FOR MARD-LGORT.
  SELECTION-SCREEN END OF BLOCK BOX3.
  SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
    SELECT-OPTIONS: S_MATNR    FOR MARA-MATNR.
  SELECTION-SCREEN END OF BLOCK BOX4.
  SELECTION-SCREEN BEGIN OF BLOCK BOX5 WITH FRAME.
    SELECT-OPTIONS: S_MATKL    FOR MARA-MATKL.
  SELECTION-SCREEN END OF BLOCK BOX5.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
*   Extract required data

START-OF-SELECTION.

* get the material/plant/storage locations
  SELECT * FROM MARD
  WHERE  MATNR IN S_MATNR
  AND    WERKS IN S_WERKS
  AND    LGORT IN S_LGORT
  AND    LABST NE SPACE.

    SELECT SINGLE * FROM MARA
    WHERE  MATNR EQ MARD-MATNR
    AND    MATKL IN S_MATKL.

    IF SY-SUBRC          = 0.
      IMARD-WERKS        = MARD-WERKS.
      IMARD-LGORT        = MARD-LGORT.
      IMARD-MATNR        = MARD-MATNR.
      IMARD-LABST        = MARD-LABST.
      IMARD-CGO_PLT      = MARD-WERKS.
      IMARD-CGO_PLT+1(1) = '3'.
      APPEND IMARD.
      CLEAR IMARD.
    ENDIF.

  ENDSELECT.

* sort the table
  SORT IMARD BY WERKS LGORT MATNR.

* create the BDC sessions for the 303 movement types with multiple lines
* for materials (5 per page)
  WRITE: SY-DATUM TO DATE.
  GROUP  = 'ZMM_303'.
  MTYPE  = '303'.
  IDX    = 1.
  PERFORM OPEN_BATCH_SESSION.
  LOOP AT IMARD.
*   at end of lgort.                             " new plant/storage loc
*     idx = 5.
*   endat.
*   if idx = 5.                                  " max lines per page
*     perform bdc_303_save.
*     idx = 0.
*   endif.
*   idx = idx + 1.
*   if idx = 1.                                  " new plant or page
      PERFORM BDC_303_INIT.
*   endif.
    PERFORM BDC_303_LINES.                       " materials in plant
*   at last.                                     " save last batch
      PERFORM BDC_303_SAVE.
*   endat.
  ENDLOOP.
  PERFORM CLOSE_BATCH_SESSION.

* create the BDC sessions for the 305 movement types with multiple lines
  GROUP  = 'ZMM_305'.
  MTYPE  = '305'.
* idx    = 0.
  PERFORM OPEN_BATCH_SESSION.
  LOOP AT IMARD.
*   at end of lgort.                             " new plant/storage loc
*     idx = 5.
*   endat.
*   if idx = 5.                                  " max lines per page
*     perform bdc_305_save.
*     idx = 0.
*   endif.
*   idx = idx + 1.
*   if idx = 1.                                  " new plant or page
      PERFORM BDC_305_INIT.
*   endif.
    PERFORM BDC_305_LINES.                       " materials in plant
*   at last.                                     " save last batch
      PERFORM BDC_305_SAVE.
*   endat.
  ENDLOOP.
  PERFORM CLOSE_BATCH_SESSION.

  MESSAGE I100 WITH 'Processing has completed'.

END-OF-SELECTION.

*-----------------------------------------------------------------------
*   FORM CREATE_303_MOVEMENT
*-----------------------------------------------------------------------
*  These sections build the data for the 303 movement type.  It will
*  transfer quantities out of the Centra plants.
*-----------------------------------------------------------------------
FORM BDC_303_INIT.

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0400'.
  PERFORM BDC_FIELD  USING 'MKPF-BLDAT'     DATE.         " document dte
  PERFORM BDC_FIELD  USING 'MKPF-BUDAT'     DATE.         " posting date
  PERFORM BDC_FIELD  USING 'RM07M-BWARTWA'  MTYPE.        " movement typ
  PERFORM BDC_FIELD  USING 'RM07M-WERKS'    IMARD-WERKS.  " Union plant
  PERFORM BDC_FIELD  USING 'RM07M-LGORT'    IMARD-LGORT.  " storage loc
  PERFORM BDC_FIELD  USING 'RM07M-WVERS3'   'X'.          " collect slip

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0421'.
  PERFORM BDC_FIELD  USING 'MSEGK-UMWRK'    IMARD-CGO_PLT." Centra plant

ENDFORM.

FORM BDC_303_LINES.

  QUANT = IMARD-LABST.
  XMATNR+11(2) = IDX.
  XERFMG+11(2) = IDX.
  PERFORM BDC_FIELD  USING XMATNR           IMARD-MATNR.  " material nbr
  PERFORM BDC_FIELD  USING XERFMG           QUANT.        " quantity

ENDFORM.

FORM BDC_303_SAVE.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/11'.        " <save>

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0410'.

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0410'.       " handle msg

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0410'.

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

  PERFORM INSERT_SESSION.
  REFRESH BDCDATA.
  CLEAR BDCDATA.

ENDFORM.

*-----------------------------------------------------------------------
*   FORM CREATE_305_MOVEMENT
*-----------------------------------------------------------------------
*  These sections build the data for the 305 movement type.  It will
*  create the transfer receipts for the movements above.
*-----------------------------------------------------------------------
FORM BDC_305_INIT.

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0400'.
  PERFORM BDC_FIELD  USING 'MKPF-BLDAT'     DATE.         " document dte
  PERFORM BDC_FIELD  USING 'MKPF-BUDAT'     DATE.         " posting date
  PERFORM BDC_FIELD  USING 'RM07M-BWARTWA'  MTYPE.        " movement typ
  PERFORM BDC_FIELD  USING 'RM07M-WERKS'    IMARD-CGO_PLT." Centra plant
  PERFORM BDC_FIELD  USING 'RM07M-LGORT'    IMARD-LGORT.  " storage loc
  PERFORM BDC_FIELD  USING 'RM07M-WVERS3'   'X'.          " collect slip

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0421'.

ENDFORM.

FORM BDC_305_LINES.

  QUANT = IMARD-LABST.
  XMATNR+11(2) = IDX.
  XERFMG+11(2) = IDX.
  PERFORM BDC_FIELD  USING XMATNR           IMARD-MATNR.  " material nbr
  PERFORM BDC_FIELD  USING XERFMG           QUANT.        " quantity

ENDFORM.

FORM BDC_305_SAVE.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/11'.        " <save>

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0410'.

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0410'.       " handle msg

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

  PERFORM BDC_SCREEN USING 'SAPMM07M'       '0410'.

  PERFORM BDC_SCREEN USING 'SAPLKACB'       '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'     '/8'.         " <continue>

  PERFORM INSERT_SESSION.
  REFRESH BDCDATA.
  CLEAR BDCDATA.

ENDFORM.

*-------------------------  BDC_SCREEN  --------------------------------
* This routine adds an entry to the table BDCDATA with screen
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  PROGRAM - Program name of the screen
*      DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*-------------------------  BDC_FIELD  ---------------------------------
* This routine adds an entry to the table BDCDATA with field
* information from a particular transaction.  This is used as part
* of the process for creating data for batch input.
* Parameters:
* -->  fnam - name of the field on the screen
*      fval - value to be entered for that field on the screen.
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.

*------------------------  OPEN_BATCH_SESSION --------------------------
*   - This opens up the batch session for input.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.
  CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
          CLIENT              = SY-MANDT
          GROUP               = GROUP
          KEEP                = 'X'
          USER                = SY-UNAME
      EXCEPTIONS
          CLIENT_INVALID      = 1
          DESTINATION_INVALID = 2
          GROUP_INVALID       = 3
          GROUP_IS_LOCKED     = 4
          HOLDDATE_INVALID    = 5
          INTERNAL_ERROR      = 6
          QUEUE_ERROR         = 7
          RUNNING             = 8
          SYSTEM_LOCK_ERROR   = 9
          USER_INVALID        = 10
          OTHERS              = 11.
  IF SY-SUBRC NE 0.
    MESSAGE E001 WITH GROUP.
  ENDIF.
ENDFORM.

*-------------------------  INSERT_SESSION  ----------------------------
*-----------------------------------------------------------------------
FORM INSERT_SESSION.
  CALL FUNCTION 'BDC_INSERT'
      EXPORTING
          TCODE               = T_CODE
      TABLES
          DYNPROTAB           = BDCDATA
      EXCEPTIONS
          INTERNAL_ERROR      = 1
          NOT_OPEN            = 2
          QUEUE_ERROR         = 3
          TCODE_INVALID       = 4
          OTHERS              = 5.
  IF SY-SUBRC NE 0.
    MESSAGE I002 WITH T_CODE.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*    CLOSE_BATCH_SESSION
*-----------------------------------------------------------------------
*  - This closes the batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_BATCH_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
      EXCEPTIONS
          NOT_OPEN            = 1
          QUEUE_ERROR         = 2
          OTHERS              = 3.
  IF SY-SUBRC NE 0.
    MESSAGE I003 WITH 'ZMM_DELETE'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       THIS IS THE END, MY FREIND                                    *
*---------------------------------------------------------------------*
