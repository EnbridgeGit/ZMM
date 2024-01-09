REPORT ZMMMR035 LINE-SIZE 255 LINE-COUNT 59.

***********************************************************************
*       Owner: Centra/Union                                           *
*  Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)        *
*        Date: December 16th, 1996                                    *
*  Request ID:                                                        *
*                                                                     *
* This report will generate a list addressess from Vendors.           *
***********************************************************************


TABLES: LFA1.                                   " Vendor Table

SELECTION-SCREEN BEGIN OF BLOCK INTRO WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_NAME FOR LFA1-NAME1,          " Name search
                S_CITY FOR LFA1-ORT01.          " City search
SELECTION-SCREEN END OF BLOCK INTRO.

START-OF-SELECTION.
 SELECT * FROM LFA1 WHERE NAME1 IN S_NAME
   AND ORT01 IN S_CITY
   ORDER BY ORT01 NAME1.
   WRITE: /1 LFA1-LIFNR, 12 LFA1-NAME1, 47 LFA1-NAME2,
          84 LFA1-NAME3, 121 LFA1-NAME4, 158 LFA1-ORT01, 195 LFA1-PSTLZ.
 ENDSELECT.
END-OF-SELECTION.
