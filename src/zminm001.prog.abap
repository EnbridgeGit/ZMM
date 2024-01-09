REPORT ZMINM001.

DATA: BEGIN OF A OCCURS 5.
INCLUDE STRUCTURE ZBMSEG.
DATA: END OF A.

START-OF-SELECTION.
   CLEAR A. REFRESH A.
*  perform clear_structure.
   PERFORM FILL_REC1.
   APPEND A.
   CLEAR A.
*  perform clear_structure.
   PERFORM FILL_REC2.
   APPEND A.
   CLEAR A.
*  perform clear_structure.
*   PERFORM FILL_REC3.
*  append a.
   CLEAR A.
   PERFORM OUTPUT_TABLE.
  WRITE: / 'DONE!'.
******************************************************************
*********************************************************************
* RECORD #1
*********************************************************************
FORM FILL_REC1.
A-MAPPE      = 'ZMMLEETEST'. " Session name
A-TCODE      = 'MB1A'.            " Transaction code
A-BWART      = '201'.           " Movement type (inventory management)
A-BLDAT      = SY-DATUM.       " Date of the document
A-BUDAT      = SY-DATUM.       " Posting date in the document
A-XBLNR      = 'R123456      RAA'.     " Reference document number
*a-xblnr      = 'P123458         '.        " Reference document number
*A-BKTXT      = 'RIC            1234567890'.     " Document header text
*A-FRBNR      = 'D234567890x'.      " Number of bill of lading
A-MATNR      = '100071'.          " Material number
A-WERKS      = 'P100'.             " Plant
*A-LGORT      = 'A001'.            " Storage location
*A-LIFNR      = '60'.             " Vendor'S ACCOUNT NUMBER
A-ERFMG      = '6'.               " Quantity (batch input field)
*A-ERFME      = 'EA'.              " Unit of entry
*a-wempf      = 'TRUCK1'. " Goods recipient
*A-EBELN      = '4500000116'.         " Purchase order number
*A-EBELP      = '00001'.       " Line item number of purchasing document
A-KOSTL      = '20202'.       " Cost center
*a-aufnr      = '210005'.        " Order number
*a-ps_psp_pnr = '04-96-207-5458'. " Project account assignment: batch in
*a-umwrk      = 'P105'.            " Receiving plant/issuing plant
*A-UMLGO      = 'A001'.           " Receiving/issuing storage location
*a-elikz      = 'X'.             " Delivery completed Indicator
*a-grund      = '0003'.          " Reason for goods movement
*a-konto      = '490001'.        " Account number
*a-exwrt      = '1000.00'.            " Amount in local currency
*a-gernr      = '20'.                 "serial number
A-RSNUM      = '2'. " Number of reservation (batch input format)
A-RSPOS      = '0001'. " Item number of reservation (batch input forat)
*a-wever      = '/'. " Version for printing of GR/GI slip
*a-ldest      = '/'. " Logical destination / printer name
*a-charg      = '/'. " Batch number
*a-insmk      = '/'. " Quality inspection indicator
*a-zusch      = '/'. " Batch status key
*a-sobkz      = '/'. " Special stock indicator
*A-KUNNR      = '420500'. " Account number of customer
*a-kdauf      = '/'. " Sales order number
*a-kdpos      = '/'. " Sales document line item (batch input field)
*a-kdein      = '/'. " Scheduled delivery (batch input field)
*a-plpla      = '/'. " Production storage bin
*a-bpmng      = '/'. " Quantity (batch input field)
*a-bprme      = '/'. " Order price unit (purchasing)
*A-EBELN      = '4500000015'. " Purchase order number
*a-sgtxt      = '/'. " Line item text
*a-equnr      = '/'. " Equipment number
*a-ablad      = '/'. " Unloading point
*a-kokrs      = '/'. " Controlling area
*a-projn      = '04-96-207'. " Project number
*a-pargb      = '/'. " Trading partner'S BUSINESS AREA
*a-parbu      = '/'. " Clearing company code
*a-anln1      = '/'. " Asset main number
*a-anln2      = '/'. " Asset sub-number
*a-kzear      = '/'. " Indicator: final issue for this reservation
*a-ummat      = '/'. " Receiving/issuing material
*a-umcha      = '/'. " Receiving/issuing batch
*a-umzus      = '/'. " Status key of transfer batch
*a-umbar      = '/'. " Valuation type of transfer batch
*a-umsok      = '/'. " Special stock indicator physical stock transfer
*a-weanz      = '/'. " Number of GR/GI slips to be printed
*a-xzgvh      = '/'. " Indicator: certificate exists
*a-lgtyp      = '/'. " Storage type
*a-lgpla      = '/'. " Storage bin
*a-gsber      = '/'. " Business area
*a-kstrg      = '/'. " Cost object
*a-paobjnr    = '/'. " Number for profitaility segments
*a-prctr      = '/'. " Profit center
*a-nplnr      = '/'. " Network number for account assignment
*a-aufpl      = '/'. " Plan no. for order operations: batch input field
*a-aplzl      = '/'. " Consecutive counter to distinguish DB entries
*a-aufps      = '/'. " Order item number: batch input field
*a-vptnr      = '/'. " Partner account number
*a-fipos      = '/'. " Commitment item
*a-exvkw      = '/'. " Externally entered sales value in local currency
*a-mhdat      = '/'. " Shelf life expiration date or date of production
* No fields below this line have been used in any test
*  A-GERNR = SPACE.
ENDFORM.
*********************************************************************
* RECORD #2
*********************************************************************
FORM FILL_REC2.
A-MAPPE      = 'ZMMLEETEST'. " Session name
A-TCODE      = 'MB1A'.            " Transaction code
A-BWART      = '201'.           " Movement type (inventory management)
A-BLDAT      = SY-DATUM.       " Date of the document
A-BUDAT      = SY-DATUM.       " Posting date in the document
A-XBLNR      = 'R123456      RAA'.     " Reference document number
*a-xblnr      = 'P123458         '.        " Reference document number
*A-BKTXT      = 'RIC            1234567890'.     " Document header text
*A-FRBNR      = 'D234567890x'.      " Number of bill of lading
A-MATNR      = '100072'.          " Material number
A-WERKS      = 'P100'.             " Plant
*A-LGORT      = 'A001'.            " Storage location
*A-LIFNR      = '60'.             " Vendor'S ACCOUNT NUMBER
A-ERFMG      = '6'.               " Quantity (batch input field)
*A-ERFME      = 'EA'.              " Unit of entry
*a-wempf      = 'TRUCK1'. " Goods recipient
*A-EBELN      = '4500000116'.         " Purchase order number
*A-EBELP      = '00001'.       " Line item number of purchasing document
A-KOSTL      = '20202'.       " Cost center
*a-aufnr      = '210005'.        " Order number
*a-ps_psp_pnr = '04-96-207-5458'. " Project account assignment: batch in
*a-umwrk      = 'P105'.            " Receiving plant/issuing plant
*A-UMLGO      = 'A001'.           " Receiving/issuing storage location
*a-elikz      = 'X'.             " Delivery completed Indicator
*a-grund      = '0003'.          " Reason for goods movement
*a-konto      = '490001'.        " Account number
*a-exwrt      = '1000.00'.            " Amount in local currency
*a-gernr      = '20'.                 "serial number
*A-RSNUM      = '2'. " Number of reservation (batch input format)
*A-RSPOS      = '0001'. " Item number of reservation (batch input forat)
*a-wever      = '/'. " Version for printing of GR/GI slip
*a-ldest      = '/'. " Logical destination / printer name
*a-charg      = '/'. " Batch number
*a-insmk      = '/'. " Quality inspection indicator
*a-zusch      = '/'. " Batch status key
*a-sobkz      = '/'. " Special stock indicator
A-KUNNR      = '420500'. " Account number of customer
*a-kdauf      = '/'. " Sales order number
*a-kdpos      = '/'. " Sales document line item (batch input field)
*a-kdein      = '/'. " Scheduled delivery (batch input field)
*a-plpla      = '/'. " Production storage bin
*a-bpmng      = '/'. " Quantity (batch input field)
*a-bprme      = '/'. " Order price unit (purchasing)
*A-EBELN      = '4500000015'. " Purchase order number
*a-sgtxt      = '/'. " Line item text
*a-equnr      = '/'. " Equipment number
*a-ablad      = '/'. " Unloading point
*a-kokrs      = '/'. " Controlling area
*a-projn      = '04-96-207'. " Project number
*a-pargb      = '/'. " Trading partner'S BUSINESS AREA
*a-parbu      = '/'. " Clearing company code
*a-anln1      = '/'. " Asset main number
*a-anln2      = '/'. " Asset sub-number
*a-kzear      = '/'. " Indicator: final issue for this reservation
*a-ummat      = '/'. " Receiving/issuing material
*a-umcha      = '/'. " Receiving/issuing batch
*a-umzus      = '/'. " Status key of transfer batch
*a-umbar      = '/'. " Valuation type of transfer batch
*a-umsok      = '/'. " Special stock indicator physical stock transfer
*a-weanz      = '/'. " Number of GR/GI slips to be printed
*a-xzgvh      = '/'. " Indicator: certificate exists
*a-lgtyp      = '/'. " Storage type
*a-lgpla      = '/'. " Storage bin
*a-gsber      = '/'. " Business area
*a-kstrg      = '/'. " Cost object
*a-paobjnr    = '/'. " Number for profitaility segments
*a-prctr      = '/'. " Profit center
*a-nplnr      = '/'. " Network number for account assignment
*a-aufpl      = '/'. " Plan no. for order operations: batch input field
*a-aplzl      = '/'. " Consecutive counter to distinguish DB entries
*a-aufps      = '/'. " Order item number: batch input field
*a-vptnr      = '/'. " Partner account number
*a-fipos      = '/'. " Commitment item
*a-exvkw      = '/'. " Externally entered sales value in local currency
*a-mhdat      = '/'. " Shelf life expiration date or date of production
* No fields below this line have been used in any test
*  A-GERNR = SPACE.
ENDFORM.
*********************************************************************
* RECORD #3
*********************************************************************
FORM FILL_REC3.
A-MATNR      = '100001'. " Material number
A-WERKS      = 'P105'. " Plant
A-LGORT      = 'A001'. " Storage location
A-ERFMG      = '1'. " Quantity (batch input field)
A-ERFME      = 'EA'. " Unit of entry
A-AUFNR      = '100001'. " Order number
A-BWART      = '261'. " Movement type (inventory management)
A-MAPPE      = 'MIKETEST'. " Session name
A-TCODE      = 'MB1A'. " Transaction code
A-BLDAT      = '19960711'. " Date of the document
A-BUDAT      = '19960711'. " Posting date in the document
A-XBLNR      = '1111111'. " Reference document number
A-BKTXT      = 'DOC TEXT'. " Document header text
* No fields below this line have been used in any test
*a-ummat      = '/'. " Receiving/issuing material
*a-umwrk      = '/'. " Receiving plant/issuing plant
*a-umlgo      = '/'. " Receiving/issuing storage location
*a-umcha      = '/'. " Receiving/issuing batch
*a-umzus      = '/'. " Status key of transfer batch
*a-umbar      = '/'. " Valuation type of transfer batch
*a-umsok      = '/'. " Special stock indicator physical stock transfer
*a-weanz      = '/'. " Number of GR/GI slips to be printed
*a-grund      = '/'. " Reason for goods movement (batch input format)
*a-parbu      = '/'. " Clearing company code
*a-kostl      = '/'. " Cost center
*a-projn      = '/'. " Project number
*a-anln1      = '/'. " Asset main number
*a-anln2      = '/'. " Asset sub-number
*a-rsnum      = '/'. " Number of reservation (batch input format)
*a-rspos      = '/'. " Item number of reservation (batch input forat)
*a-kzear      = '/'. " Indicator: final issue for this reservation
*a-ps_psp_pnr = '/'. " Project account assignment: batch input fied
*a-nplnr      = '/'. " Network number for account assignment
*a-aufpl      = '/'. " Plan no. for order operations: batch input field
*a-aplzl      = '/'. " Consecutive counter to distinguish DB entries
*a-aufps      = '/'. " Order item number: batch input field
*a-vptnr      = '/'. " Partner account number
*a-fipos      = '/'. " Commitment item
*a-exvkw      = '/'. " Externally entered sales value in local currency
*a-mhdat      = '/'. " Shelf life expiration date or date of production
*a-konto      = '/'. " Account number
*a-exwrt      = '/'. " Amount in local currency (batch input field)
*a-xzgvh      = '/'. " Indicator: certificate exists
*a-lgtyp      = '/'. " Storage type
*a-lgpla      = '/'. " Storage bin
*a-gsber      = '/'. " Business area
*a-kstrg      = '/'. " Cost object
*a-paobjnr    = '/'. " Number for profitaility segments
*a-prctr      = '/'. " Profit center
*a-charg      = '/'. " Batch number
*a-insmk      = '/'. " Quality inspection indicator
*a-zusch      = '/'. " Batch status key
*a-sobkz      = '/'. " Special stock indicator
*a-lifnr      = '/'. " Vendor'S ACCOUNT NUMBER
*a-xblnr      = '/'. " Reference document number
*a-bktxt      = '/'. " Document header text
*a-frbnr      = '/'. " Number of bill of lading at time of goods receipt
*a-wever      = '/'. " Version for printing of GR/GI slip
*a-ldest      = '/'. " Logical destination / printer name
*a-ebeln      = '/'. " Purchase order number
*a-ebelp      = '/'. " Line item number of purchasing document
*a-elikz      = '/'. " Delivery completed Indicator
*a-sgtxt      = '/'. " Line item text
*a-equnr      = '/'. " Equipment number
*a-wempf      = '/'. " Goods recipient
*a-ablad      = '/'. " Unloading point
*a-kokrs      = '/'. " Controlling area
*a-pargb      = '/'. " Trading partner'S BUSINESS AREA
*a-kunnr      = '/'. " Account number of customer
*a-kdauf      = '/'. " Sales order number
*a-kdpos      = '/'. " Sales document line item (batch input field)
*a-kdein      = '/'. " Scheduled delivery (batch input field)
*a-plpla      = '/'. " Production storage bin
*a-bpmng      = '/'. " Quantity (batch input field)
*a-bprme      = '/'. " Order price unit (purchasing)
ENDFORM.
******************************************************************
* NO CHANGES REQUIRED AFTER THIS POINT
******************************************************************
FORM CLEAR_STRUCTURE.
A-UMMAT      = '/'. " Receiving/issuing material
A-UMWRK      = '/'. " Receiving plant/issuing plant
A-UMLGO      = '/'. " Receiving/issuing storage location
A-UMCHA      = '/'. " Receiving/issuing batch
A-UMZUS      = '/'. " Status key of transfer batch
A-UMBAR      = '/'. " Valuation type of transfer batch
A-UMSOK      = '/'. " Special stock indicator physical stock transfer
A-WEANZ      = '/'. " Number of GR/GI slips to be printed
A-GRUND      = '/'. " Reason for goods movement (batch input format)
A-PARBU      = '/'. " Clearing company code
A-KOSTL      = '/'. " Cost center
A-PROJN      = '/'. " Project number
A-AUFNR      = '/'. " Order number
A-ANLN1      = '/'. " Asset main number
A-ANLN2      = '/'. " Asset sub-number
A-RSNUM      = '/'. " Number of reservation (batch input format)
A-RSPOS      = '/'. " Item number of reservation (batch input forat)
A-KZEAR      = '/'. " Indicator: final issue for this reservation
A-PS_PSP_PNR = '/'. " Project account assignment: batch input fied
A-NPLNR      = '/'. " Network number for account assignment
A-AUFPL      = '/'. " Plan no. for order operations: batch input field
A-APLZL      = '/'. " Consecutive counter to distinguish DB entries
A-AUFPS      = '/'. " Order item number: batch input field
A-VPTNR      = '/'. " Partner account number
A-FIPOS      = '/'. " Commitment item
A-EXVKW      = '/'. " Externally entered sales value in local currency
A-MHDAT      = '/'. " Shelf life expiration date or date of production
A-KONTO      = '/'. " Account number
A-EXWRT      = '/'. " Amount in local currency (batch input field)
A-XZGVH      = '/'. " Indicator: certificate exists
A-LGTYP      = '/'. " Storage type
A-LGPLA      = '/'. " Storage bin
A-GSBER      = '/'. " Business area
A-KSTRG      = '/'. " Cost object
A-PAOBJNR    = '/'. " Number for profitaility segments
A-PRCTR      = '/'. " Profit center
A-BWART      = '/'. " Movement type (inventory management)
A-MATNR      = '/'. " Material number
A-WERKS      = '/'. " Plant
A-LGORT      = '/'. " Storage location
A-CHARG      = '/'. " Batch number
A-INSMK      = '/'. " Quality inspection indicator
A-ZUSCH      = '/'. " Batch status key
A-SOBKZ      = '/'. " Special stock indicator
A-LIFNR      = '/'. " Vendor'S ACCOUNT NUMBER
A-MAPPE      = '/'. " Session name
A-TCODE      = '/'. " Transaction code
A-BLDAT      = '/'. " Date of the document
A-BUDAT      = '/'. " Posting date in the document
A-XBLNR      = '/'. " Reference document number
A-BKTXT      = '/'. " Document header text
A-FRBNR      = '/'. " Number of bill of lading at time of goods receipt
A-WEVER      = '/'. " Version for printing of GR/GI slip
A-LDEST      = '/'. " Logical destination / printer name
A-EBELN      = '/'. " Purchase order number
A-EBELP      = '/'. " Line item number of purchasing document
A-ELIKZ      = '/'. " Delivery completed Indicator
A-SGTXT      = '/'. " Line item text
A-EQUNR      = '/'. " Equipment number
A-WEMPF      = '/'. " Goods recipient
A-ABLAD      = '/'. " Unloading point
A-KOKRS      = '/'. " Controlling area
A-PARGB      = '/'. " Trading partner'S BUSINESS AREA
A-KUNNR      = '/'. " Account number of customer
A-KDAUF      = '/'. " Sales order number
A-KDPOS      = '/'. " Sales document line item (batch input field)
A-KDEIN      = '/'. " Scheduled delivery (batch input field)
A-PLPLA      = '/'. " Production storage bin
A-ERFMG      = '/'. " Quantity (batch input field)
A-ERFME      = '/'. " Unit of entry
A-BPMNG      = '/'. " Quantity (batch input field)
A-BPRME      = '/'. " Order price unit (purchasing)
ENDFORM.

FORM OUTPUT_TABLE.
DATA: THEFILE(100).
CALL FUNCTION 'FILE_GET_NAME'
  EXPORTING
*     LOGICAL_FILENAME        = 'MM_INVENTORY_MANAGEMENT_GOODS_MOVEMENT'
     LOGICAL_FILENAME        = 'ZMINM002_CAMB'
     IMPORTING
*         emergency_flag          =
*         file_format             =
          FILE_NAME               = THEFILE
     EXCEPTIONS
          FILE_NOT_FOUND          = 1
          OTHERS                  = 2.
     IF SY-SUBRC <> 0.
        WRITE: 'Unable to determine physical file name'.
     ENDIF.
     OPEN DATASET THEFILE FOR OUTPUT IN TEXT MODE.
     IF SY-SUBRC <> 0.
        WRITE: 'Unable to open file ', THEFILE.
     ENDIF.
     LOOP AT A.
        TRANSFER A TO THEFILE.
     ENDLOOP.
     CLOSE DATASET THEFILE.
ENDFORM.
