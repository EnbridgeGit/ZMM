REPORT ZMMMR034 LINE-SIZE 80 LINE-COUNT 59.
***********************************************************************
*       Owner: Centra/Union                                           *
*  Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)        *
*        Date: December 13th, 1996                                    *
*  Request ID:                                                        *
*                                                                     *
* This report will report on the dollar amounts selected by the user. *
* The following will be displayed: Infro rec#, Material # and Amounts.*
***********************************************************************
TABLES: EINA,                         "Purchasing info record - General
        EINE.                         "Purchasing info record - Purchase

DATA:   BEGIN OF ITAB OCCURS 10000,
          LIFNR  LIKE EINA-LIFNR,     "Vendor number
          MATNR  LIKE EINA-MATNR,     "Material number
          NETPR  LIKE EINE-NETPR,     "Deletion flag - purchasing
        END OF ITAB.

SELECTION-SCREEN BEGIN OF BLOCK INTRO WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS S_PRICE FOR EINE-NETPR MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK INTRO.

* The following will highlight the screen's output for certain texts. *
AT SELECTION-SCREEN OUTPUT.
 LOOP AT SCREEN.
  CHECK SCREEN-GROUP1 = 'ABC'.
  SCREEN-INTENSIFIED = '1'.
  MODIFY SCREEN.
 ENDLOOP.

************************************************************************

START-OF-SELECTION.
 PERFORM INITIALIZE.
 PERFORM GET_PRICE.
 PERFORM PROC_ITAB.
END-OF-SELECTION.

************************************************************************

* The following routine will clear and initialize the internal table. *
FORM INITIALIZE.
 REFRESH ITAB.
 CLEAR ITAB.
ENDFORM.

* The following will get all appropriate info for dollar amounts. *
FORM GET_PRICE.
 SELECT * FROM EINE WHERE NETPR IN S_PRICE.
   SELECT * FROM EINA WHERE INFNR = EINE-INFNR.
          CLEAR: ITAB.
          MOVE: EINA-LIFNR TO ITAB-LIFNR,
                EINA-MATNR TO ITAB-MATNR,
                EINE-NETPR TO ITAB-NETPR.
          APPEND ITAB.
    ENDSELECT.
  ENDSELECT.
ENDFORM.


* The following routine will process all data for the dollar amounts.*
FORM PROC_ITAB.
 SORT ITAB BY LIFNR MATNR.
 LOOP AT ITAB.
   WRITE: /3 ITAB-LIFNR, 15 ITAB-MATNR, 35 ITAB-NETPR.
 ENDLOOP.
ENDFORM.
