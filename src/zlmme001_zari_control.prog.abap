*---------------------------------------------------------------------*
*       REPORT  ZLMME001_ZARI_CONTROL                                 *
*       AUTHOR  btboundy                                              *
*       DATE    January, 2011.                                        *
*---------------------------------------------------------------------*
*     This program is used for ARIBA outbound PO's, when message      *
*     output type ZARI is used this will set a RC flag that           *
*     will be read and modify the functioning of edi_processing       *
*---------------------------------------------------------------------*
*CHANGES:
*
* BY:          ISSUE:  DATE:        DESCRIPTION
*
*---------------------------------------------------------------------*

REPORT  ZLMME001_ZARI_CONTROL.
*&---------------------------------------------------------------------*
*&      Form  FORM_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*----------------------------------------------------------------------*
FORM form_print USING ent_retco ent_screen.

  DATA: l_druvo LIKE t166k-druvo.

  CLEAR ent_retco.

  "Set Return Code
  ent_retco = 990.

  PERFORM edi_processing IN PROGRAM rsnasted USING ent_retco ent_screen.


ENDFORM.                    "FORM_PRINT
