REPORT ZMLNGTXT_EINA MESSAGE-ID ZA LINE-SIZE 132 LINE-COUNT 65
       NO STANDARD PAGE HEADING.

************************************************************************
*                                                                      *
*  Spectra  nergy Inc.            Creation Date: 2010/05/27            *
*                                                                      *
*  Program Name: ZMLNGTXT_EINA     Author: J. Sennett                  *
*                                                                      *
*  Program Description:                                                *
*                                                                      *
*  This program lists the PO long text.                                *
*                                                                      *
*  Note: This program has been copied from the West on 2010/06/02 and  *
*        no change has been made for the East. M. Khan                 *
*                                                                      *
*  Called by: None                                                     *
*  Calls:     None                                                     *
*  Functions: None                                                     *
*  Includes:  None                                                     *
************************************************************************

************************************************************************
*                                                                      *
*  Modification History                                                *
*                                                                      *
*  YYYY/MM/DD  A. Programmer        SIR/CR# 9999                       *
*              X-----------------------------------------------------X *
*              X-----------------------------------------------------X *
*              X-----------------------------------------------------X *
*              X-----------------------------------------------------X *
*              X-----------------------------------------------------X *
*                                                                      *
************************************************************************
TABLES: EINA,                          "purchasing info rec: generaldata
        STXH,                          "text header
        STXL.                          "text lines


DATA: BEGIN OF T_STXH OCCURS 100,
    TDOBJECT                            LIKE STXH-TDOBJECT,
    TDNAME                              LIKE STXH-TDNAME,
    TDID                                LIKE STXH-TDID,
    TDSPRAS                             LIKE STXH-TDSPRAS,
   END OF T_STXH.

DATA:
  BEGIN OF W_TLINETAB OCCURS 100.      "Internal table for long text.
        INCLUDE STRUCTURE TLINE.
DATA:
  END OF W_TLINETAB.

DATA: BOOLEAN       TYPE C,
      TRUE          LIKE BOOLEAN VALUE 'T',
      FALSE         LIKE BOOLEAN VALUE 'F',
      RECORDS_FOUND LIKE BOOLEAN VALUE 'F'.

DATA:
      W_TDNAME LIKE STXH-TDNAME,
      ENG_TAG(25)  VALUE ' '.

SELECT-OPTIONS: S_INFNR FOR EINA-INFNR.
PARAMETERS:
          P_ID      LIKE STXL-TDID OBLIGATORY,
          P_LANG    LIKE STXL-TDSPRAS OBLIGATORY DEFAULT 'EN',
          P_NAME    LIKE STXL-TDNAME NO-DISPLAY,
          P_OBJ     LIKE STXL-TDOBJECT OBLIGATORY.

AT SELECTION-SCREEN.
   IF S_INFNR IS INITIAL.
      S_INFNR-LOW = '0000000000'.
      S_INFNR-SIGN = 'I'.
      S_INFNR-OPTION = 'BT'.
      S_INFNR-HIGH = '9999999999'.
      APPEND S_INFNR.
    ENDIF.






START-OF-SELECTION.

  WRITE:/   'InfoRec',
         12 'Vendor#',
         23 'Material#',
         42 'Line#',
         50 'Text'.


  SELECT * FROM EINA
          WHERE INFNR IN S_INFNR.

      CONCATENATE EINA-INFNR '%' INTO W_TDNAME.

      SELECT TDOBJECT TDNAME TDID TDSPRAS FROM STXH INTO T_STXH
           WHERE TDOBJECT = P_OBJ
             AND TDNAME LIKE W_TDNAME
             AND TDID = P_ID
             AND TDSPRAS = P_LANG.

         WRITE: / EINA-INFNR, EINA-LIFNR, EINA-MATNR, '000'.
         PERFORM 4000_LONG_TEXT TABLES W_TLINETAB.
         PERFORM 5000_PRINT_TEXT.

      ENDSELECT.

  ENDSELECT.



*---------------------------------------------------------------------*
*       FORM 4000_LONG_TEXT                                           *
*---------------------------------------------------------------------*
*       Called by: 2000_RETRIEVE_TEXT                                 *
*       Calls    : FUNCTION 'READ_TEXT_INLINE'                        *
*---------------------------------------------------------------------*
FORM 4000_LONG_TEXT TABLES F_TAB STRUCTURE TLINE.


CLEAR: W_TLINETAB, ENG_TAG.
REFRESH W_TLINETAB.
*look for engineering tag text for specified item
CALL FUNCTION 'READ_TEXT'
     EXPORTING
          ID                      = T_STXH-TDID
          LANGUAGE                = T_STXH-TDSPRAS
          NAME                    = T_STXH-TDNAME
          OBJECT                  = T_STXH-TDOBJECT
*    importing
*         header                  =
     TABLES
          LINES                   = W_TLINETAB
     EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.
*any text returned?
READ TABLE W_TLINETAB INDEX 1.
IF SY-SUBRC EQ 0.
*remove 'TAG' and 'TAG:' from beginning of text.
  IF W_TLINETAB-TDLINE(3) EQ 'TAG'.
    SHIFT W_TLINETAB-TDLINE LEFT BY 3 PLACES.
  ENDIF.
  IF W_TLINETAB-TDLINE(1) EQ ':'.
    SHIFT W_TLINETAB-TDLINE LEFT BY 1 PLACES.
  ENDIF.
  ENG_TAG = W_TLINETAB-TDLINE(25).

ENDIF.


ENDFORM.
*---------------------------------------------------------------------*
*       FORM 5000_PRINT_TEXT                                          *
*---------------------------------------------------------------------*
*       Called by: 2000_RETRIEVE_TEXT                                 *
*       Calls    : none                                               *
*---------------------------------------------------------------------*
FORM 5000_PRINT_TEXT.

  DATA:
    F_NBR_LINES(4)                     TYPE N VALUE 0,
    f_ln_no(3)                         type n value 0.

  DESCRIBE TABLE W_TLINETAB LINES F_NBR_LINES. "find no. of lines

    IF F_NBR_LINES GT 0.
      f_ln_no = 0.
      LOOP AT W_TLINETAB.
        IF W_TLINETAB-TDLINE <> ' '.
          add 1 to f_ln_no.
          WRITE: / EINA-INFNR, EINA-LIFNR, EINA-MATNR,
                  42 f_ln_no,                      "print txt ln #
                  50 W_TLINETAB-TDLINE.            "print text
        ENDIF.
      ENDLOOP.
      CLEAR W_TLINETAB. REFRESH W_TLINETAB.
    ELSE.
      WRITE:/032 'No long text exists for this material.'.
    ENDIF.

ENDFORM.
