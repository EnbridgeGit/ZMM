REPORT ZMMMR033 LINE-SIZE 80 LINE-COUNT 59.
***********************************************************************
*       Owner: Centra/Union                                           *
*  Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)        *
*        Date: December 12th, 1996                                    *
*  Request ID:                                                        *
*                                                                     *
* This report will check for all materials that have been flag        *
* for deletion in the materials master table and info record tables.  *
***********************************************************************
TABLES: EINA,                         "Purchasing info record - General
        EINE,                         "Purchasing info record - Purchase
        LFA1,                         "Vendor
        LFM1,                         "Vendor view
        MARA,                         "Material master - General Data
        MARC.                         "Material master - C segment

DATA:   BEGIN OF ITAB OCCURS 10000,
          LIFNR  LIKE EINA-LIFNR,     "Vendor number
          MATNR  LIKE EINA-MATNR,     "Material number
          LOEKZ  LIKE EINA-LOEKZ,     "Deletion flag - purchasing
          LOEKZ2 LIKE EINE-LOEKZ,     "Deletion flag2 - purchasing
          LOVRM  LIKE MARA-LVORM,     "Deletion flag - materials
          LOVRM2 LIKE MARC-LVORM,     "Deletion flag2 - materials
          LOEVM  LIKE LFA1-LOEVM,
          LOEVM2 LIKE LFA1-LOEVM,
        END OF ITAB.
************************************************************************

START-OF-SELECTION.
 PERFORM INITIALIZE.
 PERFORM GET_FLAG1.
 PERFORM GET_FLAG2.
 PERFORM PROC_ITAB.
END-OF-SELECTION.

************************************************************************

* The following routine will clear and initialize the internal table. *
FORM INITIALIZE.
 REFRESH ITAB.
 CLEAR ITAB.
ENDFORM.

* Gets all data for deleted vendors. *
FORM GET_FLAG1.
  SELECT * FROM LFA1 WHERE LOEVM = 'X'.
   SELECT * FROM EINA WHERE LIFNR = LFA1-LIFNR.
    SELECT SINGLE * FROM MARA WHERE MATNR = EINA-MATNR.
          CLEAR: ITAB.
          MOVE: LFA1-LIFNR TO ITAB-LIFNR,
                MARA-MATNR TO ITAB-MATNR,
                EINA-LOEKZ TO ITAB-LOEKZ,
                LFA1-LOEVM TO ITAB-LOEVM.
          APPEND ITAB.
   ENDSELECT.
  ENDSELECT.
ENDFORM.

* Gets all data for deleted vendors/purchasing in view table. *
FORM GET_FLAG2.
  SELECT * FROM LFM1 WHERE LOEVM = 'X'.
   SELECT * FROM EINA WHERE LIFNR = LFM1-LIFNR.
   SELECT SINGLE * FROM MARA WHERE MATNR = EINA-MATNR.
          CLEAR: ITAB.
          MOVE: LFM1-LIFNR TO ITAB-LIFNR,
                MARA-MATNR TO ITAB-MATNR,
                EINA-LOEKZ TO ITAB-LOEKZ,
                LFM1-LOEVM TO ITAB-LOEVM2.
          APPEND ITAB.
   ENDSELECT.
  ENDSELECT.
ENDFORM.

* The following routine will process all data for all flags.*
FORM PROC_ITAB.
 SORT ITAB BY LIFNR MATNR.
 LOOP AT ITAB.
   AT NEW MATNR.
      WRITE: /3 ITAB-LIFNR, 15 ITAB-MATNR.
   ENDAT.

   WRITE: /33 ITAB-LOEVM, 49 ITAB-LOEVM2.
 ENDLOOP.
ENDFORM.
