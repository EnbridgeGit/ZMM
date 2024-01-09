REPORT ZMIMR006 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.

*----------------------------------------------------------------------
* Report:  ZMIMR006
* Date:    December 16, 1996.
* Purpose: To list all critical parts.
*----------------------------------------------------------------------
* Note:
* (1) The output listing is sorted by:
*     - werks (Plant)
*     - lgort (Storage Location)
*     - matnr (Material ID)
* (2) Quantity On Order is calculated using an SAP provided function
*     MB_ADD_PURCHASE_ORDER_QUANTITY
*----------------------------------------------------------------------
* 1997/05/21 md7140 Development request DRMM0168
*                   Titles/Headers & Order of Columns
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* data declaration
*----------------------------------------------------------------------
TABLES: ENT1027,
        MARC,
        MARD.

DATA:  DIFF    LIKE MARC-EISBE,                  "QOH - Safety Stock
       MENGE LIKE EKPO-MENGE.                    "Temp Qty on Hand


* internal table storing an output line
DATA: BEGIN OF TAB OCCURS 100,
         WERKS LIKE MARC-WERKS,                  "Plant
         LGORT LIKE MARD-LGORT,                  "Storage location
         MATNR LIKE MARD-MATNR,                  "Material number
         LABST LIKE MARD-LABST,                  "Qty on hand
         EISBE LIKE MARC-EISBE,                  "Safety stock
         MINBE LIKE MARC-MINBE,                  "Reorder point
         DISMM LIKE MARC-DISMM,                  "MRP Type
         MEINS LIKE ENT1027-MEINS,               "Base unit of measure
         MAKTX LIKE ENT1027-MAKTX,               "Description
      END OF TAB.

* internal tables storing retva for function to find # qty on order
DATA: BEGIN OF XTAB OCCURS 20,
         WERKS LIKE EKPO-WERKS,
         LGORT LIKE EKPO-LGORT,
         MATNR LIKE EKPO-MATNR,
         MENGE LIKE EKPO-MENGE,
         MENGK LIKE EKPO-MENGE,
      END OF XTAB.

*----------------------------------------------------------------------
* parameters
*----------------------------------------------------------------------
SELECT-OPTIONS: P_WERKS FOR MARC-WERKS.              "plant
SELECT-OPTIONS: P_LGORT FOR MARD-LGORT.              "storage location
SELECT-OPTIONS: P_MATNR FOR MARC-MATNR.              "material id
SELECT-OPTIONS: P_DISMM FOR MARC-DISMM DEFAULT 'VM'. "MRP type
SELECT-OPTIONS: P_MATKL FOR ENT1027-MATKL DEFAULT '0400'."bligatory.

*******************************  MAIN  *********************************
TOP-OF-PAGE.
WRITE: /1 TEXT-011, SY-REPID COLOR COL_NEGATIVE,
       72 TEXT-016 COLOR COL_HEADING,
      144 TEXT-013, SY-DATUM, TEXT-015, SY-UZEIT.
WRITE: / TEXT-012 UNDER TEXT-011, SY-MANDT UNDER SY-REPID,
         TEXT-014 UNDER TEXT-013, SY-PAGNO UNDER SY-DATUM.
ULINE.

FORMAT RESET.
FORMAT COLOR COL_HEADING.
   WRITE:  2 TEXT-001 NO-GAP,                         "Plant
           8 TEXT-002 NO-GAP,                         "Storage Location
          19 TEXT-003 NO-GAP,                         "Material Number
          29 TEXT-009 NO-GAP,                         "Description
          70 TEXT-004 NO-GAP,                         "Qty on Hand
          83 TEXT-005 NO-GAP,                         "Safety Stock
         100 TEXT-010 NO-GAP,                         "Difference
         119 TEXT-017 NO-GAP,                         "MRP Type
         123 TEXT-007 NO-GAP,                         "Reorder Point
         139 TEXT-006 NO-GAP,                         "Qty On Order
         158 TEXT-008 NO-GAP.                         "Unit of measure

WRITE: / TEXT-019 UNDER TEXT-002,
         TEXT-018 UNDER TEXT-017.
ULINE.

*----------------  START-OF-SELECTION  --------------------------------
START-OF-SELECTION.
   PERFORM GET_DATA.
   SORT TAB BY WERKS LGORT MATNR.
   PERFORM PRINT_ITEM.

*-----------------  FORM GET_DATA  -------------------------------------
* form get_data
*----------------------------------------------------------------------
FORM GET_DATA.
   REFRESH TAB.
   CLEAR   TAB.

   SELECT * FROM MARC
      WHERE MATNR IN P_MATNR                       "Material id
        AND WERKS IN P_WERKS                       "Plant
        AND DISMM IN P_DISMM                       "MRP type
        AND KZKRI = 'X'.                           "Critical part ind

      SELECT * FROM ENT1027
         WHERE MATNR = MARC-MATNR
           AND MATKL IN P_MATKL.                   "Material grp

         SELECT * FROM MARD
            WHERE MATNR =  MARC-MATNR              "Material id
              AND WERKS =  MARC-WERKS              "Plant
              AND LGORT IN P_LGORT.                "Storage location

            MOVE MARC-WERKS    TO TAB-WERKS.       "plant
            MOVE MARD-LGORT    TO TAB-LGORT.       "storage location
            MOVE MARD-MATNR    TO TAB-MATNR.       "material number
            MOVE MARD-LABST    TO TAB-LABST.       "qty on hand
            MOVE MARC-EISBE    TO TAB-EISBE.       "safety stock
            MOVE MARC-MINBE    TO TAB-MINBE.       "reorder point
            MOVE MARC-DISMM    TO TAB-DISMM.       "MRP Type
            MOVE ENT1027-MEINS TO TAB-MEINS.       "base uom
            MOVE ENT1027-MAKTX TO TAB-MAKTX.       "Description
            APPEND TAB.
         ENDSELECT.
      ENDSELECT.
   ENDSELECT.
ENDFORM.

*---------------------  PRINT_ITEM  ------------------------------------
* form print_item
*----------------------------------------------------------------------
FORM PRINT_ITEM.
   FORMAT RESET.
   LOOP AT TAB.
      AT NEW WERKS.
         NEW-PAGE.
         WRITE: / TAB-WERKS UNDER TEXT-001.           "Plant
       ENDAT.
      AT NEW LGORT.
         WRITE: / TAB-LGORT UNDER TEXT-002.           "Storage Location
       ENDAT.
      ON CHANGE OF TAB-MATNR.
         CLEAR XTAB.
         REFRESH XTAB.
         CALL FUNCTION 'MB_ADD_PURCHASE_ORDER_QUANTITY'
            EXPORTING  X_ELIKZ = SPACE
                       X_LOEKZ = SPACE
                       X_MATNR = TAB-MATNR
                       X_MEINS = TAB-MEINS
            TABLES     XTAB    = XTAB
                       XWERKS  = P_WERKS
            EXCEPTIONS OTHERS  = 1.
      ENDON.

      CLEAR MENGE.
      LOOP AT XTAB WHERE MATNR = TAB-MATNR
                     AND WERKS = TAB-WERKS.
      COMPUTE MENGE = MENGE + XTAB-MENGE.
*     write: / 'xtab',                            "See what was returned
*        xtab-werks, xtab-lgort, xtab-matnr,      "from function module
*        xtab-menge, xtab-mengk, menge.
      ENDLOOP.
      DIFF = TAB-LABST - TAB-EISBE.
      FORMAT COLOR COL_NORMAL.
      WRITE: / TAB-MATNR UNDER TEXT-003,            "Material Number
               TAB-MAKTX UNDER TEXT-009,            "Description
             TAB-LABST UNDER TEXT-004,              "Qty on Hand
             TAB-EISBE UNDER TEXT-005 DECIMALS 0,   "Safety Stock
             DIFF UNDER TEXT-010      DECIMALS 0,   "QOH - Safety Stock
             TAB-DISMM UNDER TEXT-017,              "MRP Type
             MENGE UNDER TEXT-006 DECIMALS 0,       "qty on order
             TAB-MINBE  UNDER TEXT-007 DECIMALS 0, "reorder point
             TAB-MEINS UNDER TEXT-008.             "base unit of measure
      FORMAT RESET.
   ENDLOOP.

   DESCRIBE TABLE TAB.
   IF SY-TFILL > 0. WRITE: SY-ULINE. ENDIF.
ENDFORM.
