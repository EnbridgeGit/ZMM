*&---------------------------------------------------------------------*
*&  Include           ZMMI001_PO_TO_SALESFORCE_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZMMI001_PO_TO_SALESFORCE                      *
*& Include Name       :  ZMMI001_PO_TO_SALESFORCE_TOP                  *
*& Author             :  Tawfeeq Ahmad                                 *
*& Creation Date      :  18/12/2019                                    *
*& Change Request     :  CHG0164927                                    *
*& Description        :  SAP PO Data to Sales Force                    *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 18-Dec-2019  AHMADT        D30K930342 CHG0164927 Initial Development *
*                            D30K930336                                *
*                            D30K930452                                *
*                            D30K930545                                *
*                            D30K930541                                *
*&---------------------------------------------------------------------*

* Types
TYPES: BEGIN OF TY_FILE_DATA,
        EBELN      TYPE EBELN,          "Purchasing Document Number
        EBELP      TYPE EBELP,          "Item Number of Purchasing Document
        ZEKKN      TYPE DZEKKN,         "Sequential Number of Account Assignment
        REVNO      TYPE CHAR2,          "Revison Number
        VEN_NAME   TYPE CHAR73,         "Vendor Name
        LIFNR      TYPE LIFNR,         	"Vendor Account Number
        MEMORYTYPE TYPE MEMORYTYPE,    	"Category of Incompleteness
        SYSTEM     TYPE CHAR2,          "System name
        ACTION     TYPE CHAR1,          "Indicator of Create/Change/Delete
        TXZ01	     TYPE TXZ01,          "Short Text
        AC_MENGE   TYPE MENGE_D,       	"Quantity
        BUTXT      TYPE BUTXT,       	  "Name of Company Code
        BEDNR      TYPE BEDNR,          "Requirement Tracking Number/Shopping Cart
        WERKS      TYPE WERKS,          "Plant
        AFNAME     TYPE AFNAM,          "Name of Requisitioner/Requester
        LOEKZ      TYPE char7,          "Deletion Indicator in Purchasing Document
        PO_MENGE   TYPE BSTMG,         	"Purchase Order Quantity
        VPROZ      TYPE VPROZ,          "Distribution Percentage in Case of Multiple Account Assgt
        SAKTO      TYPE SAKNR,       	  "G/L Account Number
        PS_PSP_PNR TYPE PS_POSID,    	  "Work Breakdown Structure Element (WBS Element)
        POST1      TYPE PS_POST1,       "hort description (1st text line)
        AUFNR      TYPE AUFNR,     	    "Order Number
        KTEXT      TYPE AUFTEXT,     	  "Order Description
        KOSTL      TYPE KOSTL,          "Cost Center
        CC_KTEXT   TYPE KTEXT,          "Cost Center Description
        NPLNR      TYPE NPLNR,          "Network Number for Account Assignment
        NET_KTEXT  TYPE AUFTEXT,   	    "Network Description
        VORNR      TYPE VORNR,          "Operation/Activity Number
        LTXA1      TYPE LTXA1,          "Operation short text
       END OF TY_FILE_DATA,

       BEGIN OF TY_CDHDR,
        OBJECTCLAS TYPE CDOBJECTCL,     "Object Class
        OBJECTID   TYPE CDOBJECTV,      "object ID
        UDATE      TYPE CDDATUM,        "Change Date
        UTIME      TYPE CDUZEIT,        "Change time
        TCODE      TYPE CDTCODE,        "Transaction in which a change was made
        CHANGE_IND TYPE CDCHNGINDH,     "Change indicator
       END OF   TY_CDHDR,

       BEGIN OF TY_EKKO,
        EBELN      TYPE EBELN,          "Purchasing Document Number,
        BUKRS      TYPE BUKRS,          "Company code
        BSTYP      TYPE EBSTYP,
        BSART      TYPE BSART,
        AEDAT      TYPE ERDAT,          "Date on Which Record Was Created
        EKORG      TYPE VKORG,
        LIFNR      TYPE ELIFN,          "Vendor Account Number
        MEMORYTYPE TYPE MEMORYTYPE,          "Category of Incompleteness
       END OF   TY_EKKO,

       BEGIN OF TY_EKPO,
        EBELN      TYPE EBELN,
        EBELP      TYPE EBELP,
        LOEKZ      TYPE ELOEK,
        TXZ01      TYPE TXZ01,
        WERKS      TYPE EWERK,
        BEDNR      TYPE BEDNR,
        MENGE      TYPE BSTMG,
        AFNAM      TYPE AFNAM,
       END OF   TY_EKPO,

       BEGIN OF TY_EKKN,
        EBELN      TYPE EBELN,
        EBELP      TYPE EBELP,
        ZEKKN      TYPE DZEKKN,
        MENGE      TYPE MENGE_D,
        VPROZ      TYPE VPROZ,
        SAKTO      TYPE SAKNR,
        KOSTL      TYPE KOSTL,
        AUFNR      TYPE AUFNR,
        PS_PSP_PNR TYPE PS_PSP_PNR,
        NPLNR      TYPE NPLNR,
        AUFPL      TYPE CO_AUFPL,
        APLZL      TYPE CIM_COUNT,
       END OF    TY_EKKN,

       BEGIN OF  TY_LFA1,
        LIFNR      TYPE LIFNR,
        NAME1      TYPE NAME1_GP,
        NAME2      TYPE NAME2_GP,
       END OF    TY_LFA1,

       BEGIN OF  TY_T001,
        BUKRS      TYPE BUKRS,
        BUTXT      TYPE BUTXT,
       END OF    TY_T001,

       BEGIN OF  TY_PRPS,
        PSPNR      TYPE PS_POSNR,
        POST1      TYPE PS_POST1,
       END OF    TY_PRPS,

       BEGIN OF  TY_AUFK_ORD,
        AUFNR      TYPE AUFNR,
        KTEXT      TYPE AUFTEXT,
       END OF    TY_AUFK_ORD,

       BEGIN OF  TY_AFVC,
        AUFPL      TYPE CO_AUFPL,
        APLZL      TYPE CO_APLZL,
        VORNR      TYPE VORNR,
        LTXA1      TYPE LTXA1,
       END OF    TY_AFVC,

       BEGIN OF  TY_CSKT,
        KOSTL      TYPE KOSTL,
        KTEXT      TYPE KTEXT,
       END OF    TY_CSKT.

* Internal Tables
DATA: GT_FILE_DATA TYPE STANDARD TABLE OF TY_FILE_DATA,
      GT_CDHDR     TYPE STANDARD TABLE OF TY_CDHDR,
      GT_EKKO      TYPE STANDARD TABLE OF TY_EKKO,
      GT_EKPO      TYPE STANDARD TABLE OF TY_EKPO,
      GT_EKKN      TYPE STANDARD TABLE OF TY_EKKN,
      GT_LFA1      TYPE STANDARD TABLE OF TY_LFA1,
      GT_T001      TYPE STANDARD TABLE OF TY_T001,
      GT_PRPS      TYPE STANDARD TABLE OF TY_PRPS,
      GT_AUFK_ORD  TYPE STANDARD TABLE OF TY_AUFK_ORD,
      GT_AFVC      TYPE STANDARD TABLE OF TY_AFVC,
      GT_CSKT      TYPE STANDARD TABLE OF TY_CSKT.

* Work Area
DATA: GS_FILE_DATA TYPE TY_FILE_DATA,
      GS_CDHDR     TYPE TY_CDHDR,
      GS_EKKO      TYPE TY_EKKO,
      GS_EKPO      TYPE TY_EKPO,
      GS_EKKN      TYPE TY_EKKN,
      GS_LFA1      TYPE TY_LFA1,
      GS_T001      TYPE TY_T001,
      GS_PRPS      TYPE TY_PRPS,
      GS_AUFK_ORD  TYPE TY_AUFK_ORD,
      GS_AFVC      TYPE TY_AFVC,
      GS_CSKT      TYPE TY_CSKT,
      GS_SF_PARAM TYPE ZMMT_SF_PARAM..

* Variables
DATA: GV_EXEC_DATE TYPE ZZEXEC_DATE,
      GV_EXEC_TIME TYPE ZZEXEC_TIME,
      GV_CURR_TIME  TYPE ZZEXEC_TIME.

* Constants
