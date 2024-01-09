*&---------------------------------------------------------------------*
*& Report  ZLSDI013_ENT_SO_TRANSLATE
*&
*&---------------------------------------------------------------------*
************************************************************************
*                            COGS                                      *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLSDI013_ENT_SO_TRANSLATE                     *
*& Author             :  Jaydeep Waychal/Durga Biruduraju              *
*& Creation Date      :  March 26, 2021                                *
*& Object ID          :  S01K900879                                    *
*& Application Area   :  MM                                            *
*& Description        : Program to upload transactions from EnTRAC     *
*&                      to SAP system using upload file                *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*

REPORT  zlsdi013_ent_so_translate NO STANDARD PAGE HEADING
        MESSAGE-ID zm.

TABLES: zlsde01.

* Input file format
DATA:  BEGIN OF entrac,
        app_yr(4)             TYPE c,     "Reporting Year
        app_mth(2)            TYPE c,     "Reporting Month
        charge_type(40)       TYPE c,     "Charge type
        gl_account(10)        TYPE c,     "G/L Account
        del_volume(18)        TYPE c,     "Delivery Volume
        delivery_uom(8)       TYPE c,     "Delivery Volume UOM
        charge_amt(16)        TYPE c,     "Charge Amount
        currency(3)           TYPE c.     "Currency
DATA: END OF entrac.


DATA: inrec(2000).

* CONSTANTS: W_DMTR Type X VALUE '09'.
CONSTANTS: w_dmtr TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

* Output file format
DATA:  BEGIN OF idocrec,
        audat                 TYPE d,  "Document Date
        fkdat                 TYPE d,  "Billing Date
        curcy(3)              TYPE c,    "Order Currency
        bsart(4)              TYPE c,    "Sales Document Type
        autlf(1)              TYPE c,    "Complete Delivery Indicator
        vkorg(4)              TYPE c,    "Sales Organization
        vtweg(2)              TYPE c,    "Distribution Channel
        spart(2)              TYPE c,    "Division
        parvw(3)              TYPE c,    "Sold-To-Party (Customer)
        partn(17)             TYPE c,    "Customer Number
        zuonr(18)             TYPE c,    "Assignment
        bstkd(35)             TYPE c,    "PO Number
        dwerk(4)              TYPE c,    "Delivering Plant
        vkbur(4)              TYPE c,    "Sales Office
        augru(3)              TYPE c,    "Order Reason
        konda(2)              TYPE c,    "Price Group
        kdgrp(2)              TYPE c,  "Customer Group
        prsdt                 TYPE d,  "Pricing Date
        kvgr1(3)              TYPE c,    "Customer Group 1
        bzirk(4)              TYPE c,  "Sales District
        mabnr(18)             TYPE c,  "Material Number
        kwmeng(15)            TYPE c,  "Order Quantity
        vrkme(3)              TYPE c,  "Unit of Measure
        kschl(4)              TYPE c,     "Condition Type
        kbetr(11)             TYPE c,  "Condition Amount
        kdkg1(2)              TYPE c.    "Material Pricing Group
DATA: END OF idocrec.

DATA: w_date_in  LIKE sy-datum,
      w_date_out LIKE sy-datum,
      w_amount(12)     TYPE p DECIMALS 2,
      w_sign     TYPE sign.

*------------------------  Selection Screen  ---------------------------
*
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-000.
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-001.
PARAMETER:
p_waerk TYPE vbak-waerk  DEFAULT 'CAD'  OBLIGATORY,    "Order Currency
p_autlf TYPE c           DEFAULT 'X'    OBLIGATORY,    "Complete Delivery
p_vkorg TYPE vbak-vkorg  DEFAULT 'Z102' OBLIGATORY,    "Sales Orgnisation
p_vtweg TYPE vbak-vtweg  DEFAULT 'Z0'   OBLIGATORY,    "Dist Chanel
p_spart TYPE vbak-spart  DEFAULT 'Z0'   OBLIGATORY,    "Division
p_parvw(2) TYPE c        DEFAULT 'AG'   OBLIGATORY,    "Partner function
p_vrkme TYPE t006-isocode   DEFAULT 'CR' OBLIGATORY,
*p_bstkd TYPE bvbapkom-bstkd DEFAULT 'DP GAS SALES',
p_partn TYPE vbak-kunnr     DEFAULT 'EGD-ENT'    OBLIGATORY,  "Partner
p_kwmeng TYPE dzwert    OBLIGATORY.
*SELECT-OPTIONS: s_matnr     FOR     zlsde01-matnr OBLIGATORY. "Material #

SELECTION-SCREEN END OF BLOCK box2.
* Input/Output Files
SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-002.
PARAMETERS:
infile TYPE filenameci-fileextern OBLIGATORY,
outfile TYPE filenameci-fileextern OBLIGATORY.
SELECTION-SCREEN END OF BLOCK box3.
SKIP 1.
*SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-003.
*PARAMETERS:     p_act RADIOBUTTON GROUP rbcr,            "Actual
*                p_est RADIOBUTTON GROUP rbcr,            "Estimate
*                p_rev RADIOBUTTON GROUP rbcr.            "Reversal
*SELECTION-SCREEN END OF BLOCK box4.
SELECTION-SCREEN END OF BLOCK box1.
*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE:
  '/usr/sap/interfaces/' sy-sysid(3) '/ENT/zentbilling100.dat'
                                                           INTO infile,
  '/usr/sap/interfaces/' sy-sysid(3) '/ENT/zentbilling100-so-Z102.dat'
                                                           INTO outfile.

START-OF-SELECTION.

  OPEN DATASET infile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH infile.
  ENDIF.

  OPEN DATASET outfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH outfile.
  ENDIF.

  DO.
    CLEAR: entrac,inrec.
    READ DATASET infile INTO inrec.
    IF sy-subrc <> '0'.
      EXIT.
    ENDIF.
    SPLIT inrec AT w_dmtr INTO entrac-app_yr
        entrac-app_mth
        entrac-charge_type
        entrac-gl_account
        entrac-del_volume
        entrac-delivery_uom
        entrac-charge_amt
        entrac-currency.

*Move Data from Variant Screen Fields
    MOVE: p_waerk   TO  idocrec-curcy,   "Order Currency
          p_autlf   TO  idocrec-autlf,   "Complete Delivery
          p_vkorg   TO  idocrec-vkorg,   "Sales Organization
          p_vtweg   TO  idocrec-vtweg,   "Distribution Channel
          p_spart   TO  idocrec-spart,   "Division
          p_parvw   TO  idocrec-parvw,   "Partner Function
          p_vrkme   TO  idocrec-vrkme.   "Sales Unit
*          p_bstkd   TO  idocrec-bstkd.   "PO Number

*Move Data from input file fields

    idocrec-kwmeng  = abs( entrac-del_volume ).
    IF idocrec-kwmeng = 0.
      MOVE p_kwmeng TO idocrec-kwmeng.
    ENDIF.

**Calculate and Move Data

    MOVE entrac-charge_amt TO w_amount.

    idocrec-kbetr   = abs( entrac-charge_amt ).


** Start of changes    COG
*    CONCATENATE 'CX' Entrac-CUST_ID INTO IDOCREC-PARTN.
    idocrec-partn = p_partn.
* End of changes      COG
    CONDENSE idocrec-partn NO-GAPS.
    CONCATENATE entrac-app_yr entrac-app_mth '01' INTO w_date_in.

    MOVE sy-datum TO: idocrec-audat, idocrec-fkdat.
    MOVE 'ZAC'    TO  idocrec-augru.


* New PRSDT logic
    CONCATENATE entrac-app_yr entrac-app_mth '01' INTO w_date_in.
    PERFORM get_last_day_of_month USING w_date_in CHANGING w_date_out.
    MOVE w_date_out TO idocrec-prsdt.
*
*Move Data from DB Tables
    CLEAR: zlsde01.
    TRANSLATE entrac-charge_type TO UPPER CASE.
    MOVE entrac-charge_type TO idocrec-bstkd.
    SELECT SINGLE * FROM zlsde01
     WHERE c_chgtyp   = entrac-charge_type.
    IF sy-subrc = 0.
      MOVE zlsde01-kdgrp TO idocrec-kdgrp.
      MOVE zlsde01-kschl TO idocrec-kschl.
      MOVE zlsde01-kdkg1 TO idocrec-kdkg1.
      MOVE zlsde01-matnr TO idocrec-mabnr.
      MOVE zlsde01-konda TO idocrec-konda.
      MOVE zlsde01-vkbur TO idocrec-vkbur.
      MOVE zlsde01-werks TO idocrec-dwerk.
      MOVE zlsde01-bzirk TO idocrec-bzirk.
      MOVE zlsde01-auart TO idocrec-bsart.

      IF idocrec-bsart = 'ZCR' OR idocrec-bsart = 'ZDR'.
        MOVE 'ZPB2'  TO idocrec-kschl.
      ENDIF.
    ENDIF.

    IF w_amount <> 0.
      TRANSFER idocrec TO outfile.
    ENDIF.
    CLEAR: idocrec, zlsde01, w_amount.
  ENDDO.
  CLOSE DATASET: infile, outfile.

  MESSAGE i100(zm) WITH text-100.

END-OF-SELECTION.

***********************************************************************
FORM get_last_day_of_month USING w_date_in CHANGING value(w_date_out).

  CALL FUNCTION 'FKK_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in            = w_date_in
    IMPORTING
      last_day_of_month = w_date_out
    EXCEPTIONS
      day_in_no_date    = 0.
  IF sy-subrc = 0.

  ENDIF.
ENDFORM.                    "GET_LAST_DAY_OF_MONTH
