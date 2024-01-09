*&---------------------------------------------------------------------*
*&  Include           ZXM06U43
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_EKKO) LIKE  EKKO STRUCTURE  EKKO
*"     VALUE(I_TRTYP)
*"     VALUE(I_BSTYP) LIKE  EKKO-BSTYP
*"     VALUE(I_NO_SCREEN)
*"     VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"     VALUE(I_LFM1) LIKE  LFM1 STRUCTURE  LFM1
*"     VALUE(I_KEKKO) LIKE  EKKO STRUCTURE  EKKO
*"     VALUE(I_AEKKO) LIKE  EKKO STRUCTURE  EKKO
*"     VALUE(I_REKKO) LIKE  EKKO STRUCTURE  EKKO
*"     VALUE(I_CI_EKKO) LIKE  EKKO_CI STRUCTURE  EKKO_CI
*"     VALUE(I_VORGA) LIKE  T160-VORGA
*"  TABLES
*"      TEKPO STRUCTURE  BEKPO OPTIONAL
*"      TEKET STRUCTURE  BEKET OPTIONAL
*"      TEKKN STRUCTURE  EKKNU OPTIONAL
*"      TKOMV STRUCTURE  KOMV OPTIONAL
*"  CHANGING
*"     VALUE(C_CI_EKKO) LIKE  EKKO_CI STRUCTURE  EKKO_CI OPTIONAL
*"----------------------------------------------------------------------
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include :  ZXM06U43                                                 *
*  Function:  EXIT_SAPMM06E_012                                        *
*  Author  :  Vishal Kommera                                           *
*  Date    :  September 26, 2011                                       *
*  Track # :  EE005                                                    *
*                                                                      *
*  Description:                                                        *
*     - Enhancement MM06E005 - user exits to maintain customer         *
*                              fields in purchase order                *
*                                                                      *
*     This user exit is used to get   the changed amount of the PO     *
*     and compare it with the Threshold  value. If the changed value   *
*     is greater than the threshold value then check if the user       *
*     entered Authorization code, if No provide an error message.      *
************************************************************************
TYPES abap.

* Define Types
TYPES : BEGIN OF ty_ekpo,
        packno TYPE ekpo-packno,
        netwr  TYPE ekpo-netwr,
        END OF ty_ekpo.


* Define Types
TYPES : BEGIN   OF ty_status,
        p_guid  TYPE char16 ,
        stat    TYPE char5  ,
        inact   TYPE char1  ,
        END     OF ty_status.

TYPES : BEGIN OF ty_item.
        INCLUDE STRUCTURE zbbp_item_details.
TYPES : END OF ty_item.

TYPES : BEGIN OF ty_ebeln,
        ebeln TYPE ekko-ebeln,
        zz_po_auth_code TYPE ekko-zz_po_auth_code,
        END OF ty_ebeln.

TYPES : BEGIN OF ty_packno,
        packno TYPE esuh-packno,
        END OF ty_packno.
TYPES : BEGIN OF ty_esuh.
        INCLUDE STRUCTURE uesuh.
TYPES : END OF ty_esuh,
        tt_esuh   TYPE STANDARD TABLE OF ty_esuh.

* Define Internal Table
DATA : ta_ekpo    TYPE STANDARD TABLE OF ty_ekpo.
* Define Internal Table
DATA: ta_status   TYPE STANDARD TABLE OF ty_status.




DATA :  lta_item            TYPE STANDARD TABLE OF ty_item ,
        lta_packno          TYPE STANDARD TABLE OF ty_packno.
DATA :  ltp_zz_po_auth_code TYPE ekko-zz_po_auth_code.
* Define Structure
DATA : lwa_ekpo         TYPE bekpo,
       lwa_ekpo1        TYPE ty_ekpo,
       lwa_zvar         TYPE zvar,
       lwa_esuh         TYPE ty_esuh,
       lwa_ebeln        TYPE ty_ebeln,
       lwa_item         TYPE ty_item,
       lwa_packno       TYPE ty_packno,
       lwa_status       TYPE ty_status,
       lv_svr_found(1)  TYPE c.

TYPE-POOLS abap.
* Define varaibles
DATA : ltp_netwr    TYPE ekpo-netwr,
       ltp_net_ch   TYPE ekpo-netwr,
       ltp_packno   TYPE esuh-packno,
       ltp_netwr1   TYPE ekpo-netwr,
       ltp_amount   TYPE ekpo-netwr,
       ltp_amount1  TYPE ekpo-netwr,
       ltp_auth     TYPE ekko-zz_po_auth_code,
       ltp_rfcdest  TYPE rfcdisplay-rfcdest,
       ltp_check    TYPE char1,
       ltp_netwr2   TYPE ekpo-netwr,
       ltp_last     TYPE char1,
       ltp_thre     TYPE zvar-value1,
       ls_eban      TYPE eban,
       lv_exempt(1) TYPE c,
       lv_ekposc     TYPE char10,
*      BOI by PANUSURI Ticket 91203
       ltp_waers     TYPE waerh,
       ltp_rcomp     TYPE rcomp_d,
       ltp_exch_rate TYPE c,
       ltp_wkurs     TYPE wkurs.
*      EOI by PANUSURI Ticket 91203

*data:  lta_item type STANDARD TABLE OF ty_item .

* Define Constants
CONSTANTS : co_varname  TYPE zvar-varname     VALUE 'ZMM_DOA_THRESHOLD_CHANGE',
            co_programm TYPE zvar-programm    VALUE 'ZXM06U43',
            co_i        TYPE char1            VALUE 'I',
            co_varsrm   TYPE zvarsys-varname  VALUE 'SRM_RFC',
            co_prosrm   TYPE zvarsys-programm VALUE 'LOGSYS',
            co_varnum   TYPE zvarsys-varnum   VALUE '1',
            co_v        TYPE char1            VALUE 'V',
*            co_i1129    TYPE char5            VALUE 'I1129',  "(-) PANUSURI Ticket 57362
            co_i1016    TYPE char5            VALUE 'I1016',   "(+) PANUSURI Ticket 57362
            co_pro      TYPE zvar-programm    VALUE 'ZXM06O01',
            co_var      TYPE zvar-varname     VALUE 'BSART'.

DATA :      ltp_esuh(20)                      VALUE '(SAPLMLSL)XESUH[]'.
FIELD-SYMBOLS : <esuh>  TYPE tt_esuh.

*************************************
*Rollup Logic for Service Confirmer
*************************************
CLEAR lv_svr_found.

IF c_ci_ekko-zzariba_approver IS NOT INITIAL.
  lv_svr_found = abap_true.
ENDIF.

LOOP AT tekpo INTO lwa_ekpo.
  IF lv_ekposc IS INITIAL.
    lv_ekposc = lwa_ekpo-bednr.
  ENDIF.

  IF lv_svr_found IS NOT INITIAL.
    CONTINUE.
  ENDIF.

  IF lwa_ekpo-loekz IS INITIAL AND lwa_ekpo-zzariba_approver IS NOT INITIAL.
    c_ci_ekko-zzariba_approver = lwa_ekpo-zzariba_approver.
    lv_svr_found = abap_true.
  ENDIF.
ENDLOOP.

*************************************
*Auth Code Exempt Check.
*************************************
lv_exempt = abap_false.
LOOP AT tekpo INTO lwa_ekpo.
  "Skip loekz items
  IF lwa_ekpo-loekz IS NOT INITIAL.
    CONTINUE.
  ENDIF.

  "Check first item
  IF lwa_ekpo-banfn IS NOT INITIAL.
    SELECT SINGLE banfn eprofile
      FROM eban
      INTO ls_eban
      WHERE banfn = lwa_ekpo-banfn
        AND eprofile = ''
    .
    IF sy-subrc = 0.
      lv_exempt = abap_true.
    ELSE.
      lv_exempt = abap_false.
    ENDIF.
  ENDIF.

  "Exit after first item
  EXIT.
ENDLOOP.

"Make excempt if not change
IF gf_trtyp <> co_v.
  lv_exempt = abap_true.
ENDIF.

*************************************
*Get SRM RFC destination
*************************************
SELECT SINGLE value1
  FROM zvarsys
  INTO ltp_rfcdest
  WHERE programm = co_prosrm
    AND varname  = co_varsrm
    AND varnum   = co_varnum.
IF sy-subrc <> 0.
  MESSAGE e009(zmm_message).
ENDIF.

*************************************
*Auth Code Multi-Use
*************************************
IF i_ekko-zz_po_auth_code IS NOT INITIAL AND lv_exempt = abap_false.
  "Fetch Authorization code details from Purchasing Document Header database table
  SELECT SINGLE ebeln zz_po_auth_code
         FROM ekko
         INTO lwa_ebeln
         WHERE zz_po_auth_code = i_ekko-zz_po_auth_code.

  "Check if Authorization code is already used.
  IF sy-subrc = 0 AND i_ekko-ebeln <> lwa_ebeln-ebeln.
    MESSAGE e002(zmm_message) WITH i_ekko-zz_po_auth_code .
  ENDIF.
ENDIF.

*************************************
*Auth Code Dollar Amount Check
*************************************
IF ltp_rfcdest IS NOT INITIAL AND lv_exempt = abap_false.

  "Check Doc Type for Validation
  SELECT SINGLE *
      FROM zvar
      INTO lwa_zvar
      WHERE programm = co_pro
      AND   varname  = co_var
      AND   value1   = i_ekko-bsart.

  IF sy-subrc = 0. "AND gf_trtyp = co_v.

    "Get the changed PO Amount Values Items that are were NOT INSERTED
    LOOP AT tekpo INTO lwa_ekpo WHERE loekz <> 'L' AND updkz <> 'I'.
      SELECT SINGLE packno
             FROM esuh
             INTO ltp_packno
             WHERE packno = lwa_ekpo-packno.
      IF sy-subrc <> 0.
        ltp_netwr = ltp_netwr + lwa_ekpo-netwr.
      ENDIF.
    ENDLOOP.

    "Sum the changed PO values of limit items
    ASSIGN (ltp_esuh) TO <esuh>.
    IF <esuh> IS ASSIGNED.
      LOOP AT <esuh> INTO lwa_esuh .
        READ TABLE tekpo INTO lwa_ekpo WITH KEY packno = lwa_esuh-packno .
        IF sy-subrc = 0 AND ( lwa_esuh-restnolim = 'X' OR lwa_esuh-sumlimit IS NOT INITIAL ).
          ltp_netwr = ltp_netwr + lwa_esuh-sumlimit.
          lwa_packno-packno =  lwa_esuh-packno.
          APPEND lwa_packno TO lta_packno.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Get the changed PO Amount Values Itmes that were INSERTED.
    LOOP AT tekpo INTO lwa_ekpo WHERE updkz = 'I'.
      READ TABLE lta_packno INTO lwa_packno WITH KEY packno = lwa_ekpo-packno.
      IF sy-subrc <> 0.
        ltp_netwr = ltp_netwr + lwa_ekpo-netwr.
      ENDIF.
    ENDLOOP.

    "Get all packno and net price for all lines
    SELECT packno netwr
           FROM ekpo
           INTO TABLE ta_ekpo
           WHERE ebeln = i_ekko-ebeln
           AND   loekz <> 'L'.

    "Get OLD DB auth code.
    SELECT SINGLE zz_po_auth_code
           FROM ekko
           INTO ltp_auth
           WHERE ebeln = i_ekko-ebeln.

    "Sum all the dollar values.
    LOOP AT ta_ekpo INTO lwa_ekpo1 .
      ltp_netwr2 = lwa_ekpo1-netwr.
      SELECT SINGLE sumlimit
             INTO lwa_ekpo1-netwr
             FROM esuh
             WHERE packno = lwa_ekpo1-packno.
      IF sy-subrc = 0.
        "For Limit Items
        IF <esuh> IS ASSIGNED.
          READ TABLE <esuh> INTO lwa_esuh WITH KEY packno = lwa_ekpo1-packno.
        ELSE.
          sy-subrc = 4.
        ENDIF.

        IF sy-subrc = 0.
          "Original PO Amount For Limit Items
          ltp_netwr1 = ltp_netwr1 + lwa_ekpo1-netwr.
        ELSE.
          "Original PO Amount For Limit Items Which are not changed
          ltp_netwr = ltp_netwr + lwa_ekpo1-netwr.
          ltp_netwr1 = ltp_netwr1 + lwa_ekpo1-netwr.
        ENDIF.
      ELSE.
        "Original PO Amoutn For Items Other Than Limit
        ltp_netwr1 = ltp_netwr1 + ltp_netwr2.
      ENDIF.
    ENDLOOP.

*   Insert by SPARGA    71248 / 71858
    ltp_net_ch = ltp_netwr - ltp_netwr1.

*   BOI by PANUSURI Ticket ACR-49
    CLEAR: ltp_exch_rate.
    IF i_ekko-waers <> 'CAD'.
      ltp_exch_rate = 'X'.
    ENDIF.
*   EOI by PANUSURI Ticket ACR-49

    IF ltp_net_ch GE 1000.
      IF NOT i_ekko-zz_po_auth_code IS INITIAL.
        lv_ekposc = i_ekko-zz_po_auth_code.
      ENDIF.

      CALL FUNCTION 'ZSRM_SC_CUMULATIVE'
        DESTINATION ltp_rfcdest
        EXPORTING
          i_object_id = lv_ekposc
        IMPORTING
          exp_amount  = ltp_netwr1.

*     BOI by PANUSURI Ticket 91203
      "Currency conversion
      IF ltp_exch_rate = 'X' AND i_ekko-wkurs IS NOT INITIAL.
        IF i_ekko-wkurs > 0.
          ltp_netwr1 = ltp_netwr1 / i_ekko-wkurs.
        ELSE.
          ltp_wkurs =  i_ekko-wkurs * -1.
          ltp_netwr1 = ltp_netwr1 * ltp_wkurs.
        ENDIF.
      ENDIF.
*     EOI by PANUSURI Ticket 91203
    ENDIF.
* end of insert SPARGA

    ltp_net_ch = ltp_netwr - ltp_netwr1.

    "Get Threshold amount.
    SELECT SINGLE value1
           FROM zvar
           INTO ltp_thre
           WHERE programm = co_programm
           AND   varname  = co_varname
           AND   varnum   = co_varnum.

    IF sy-subrc = 0.
      IF ltp_net_ch > ltp_thre.
        IF ekko-zz_po_auth_code IS INITIAL.
          MESSAGE e004(zmm_message).
        ELSEIF ltp_auth = ekko-zz_po_auth_code.
*          MESSAGE e016(zmm_message). "(-)PANUSURI Ticket 91203
          MESSAGE e024(zmm_message).  "(+)PANUSURI Ticket 91203
        ENDIF.
      ENDIF.
    ENDIF.

*************************************
*Auth Code Validity Check in SRM
*************************************
    IF i_ekko-zz_po_auth_code <> ltp_auth AND i_ekko-zz_po_auth_code IS NOT INITIAL. "gf_trtyp = co_v AND
      "FM to Get Shopping Cart Status from SRM System
      CALL FUNCTION 'ZSRM_SC_DETAILS_GET'
        DESTINATION ltp_rfcdest
        EXPORTING
          i_object_id    = ekko-zz_po_auth_code
          i_be_object_id = i_ekko-ebeln
        IMPORTING
          exp_amount_hd  = ltp_amount
          exp_amount     = ltp_amount1
          exp_check      = ltp_check
          exp_last       = ltp_last.

      "This is not the most recent Change Shopping Cart on this Shopping Cart.
      IF ltp_last IS INITIAL.
        MESSAGE e015(zmm_message)  .
      ELSE.
        "Auth code is not a change shopping cart.
        IF ltp_check <> 'X' .
          MESSAGE e014(zmm_message) WITH ekko-zz_po_auth_code i_ekko-ebeln  .
        ENDIF.
      ENDIF.

      IF ltp_amount1 IS NOT INITIAL.
*       BOI by PANUSURI Ticket 91203
        IF ltp_exch_rate = 'X' AND i_ekko-wkurs IS NOT INITIAL.
          "Currency conversion
          IF i_ekko-wkurs > 0.
            ltp_amount1 = ltp_amount1 / i_ekko-wkurs.
          ELSE.
            ltp_wkurs =  i_ekko-wkurs * -1.
            ltp_amount1 = ltp_amount1 * ltp_wkurs.
          ENDIF.
        ENDIF.
*       EOI by PANUSURI Ticket 91203
        IF ltp_amount1 < ltp_netwr.
          "PO Amount is more than the SC amount.
          MESSAGE e013(zmm_message) WITH ltp_netwr ltp_amount1.
        ENDIF.
      ENDIF.

* Begin of Changes by <Chaya>

*      "FM to Get Shopping Cart Status from SRM System
*      CALL FUNCTION 'BBP_PD_SC_GETDETAIL'
*        DESTINATION ltp_rfcdest
*        EXPORTING
*          i_object_id = ekko-zz_po_auth_code
*        TABLES
*          e_item      = lta_item
*          e_status    = ta_status.
*
*      "Check if Authorization code has Approved Status. if not then display error msg.
**      READ TABLE ta_status INTO lwa_status WITH KEY stat = co_i1129 inact = space.    "(-) PANUSURI Ticket 57362
*      "Check if Authorization code has Rejected Status. if not then display error msg. "(+) PANUSURI Ticket 57362
**      READ TABLE ta_status INTO lwa_status WITH KEY stat = co_i1016 inact = space.     "(+) PANUSURI Ticket 57362
*      CLEAR lwa_item.                                                   "(+) PANUSURI Ticket 57362
*      READ TABLE lta_item INTO lwa_item WITH KEY source_rel_ind = 'Y'.  "(+) PANUSURI Ticket 57362
*      IF sy-subrc <> 0.
**        MESSAGE e003(zmm_message) WITH ekko-zz_po_auth_code  .  "(-) PANUSURI Ticket 57362
*        MESSAGE e022(zmm_message) WITH ekko-zz_po_auth_code  .  "(+) PANUSURI Ticket 57362
*      ENDIF.

      DATA: lv_source_rel_ind TYPE c.

      CALL FUNCTION 'ZBBP_PD_SC_GETDETAIL'
        DESTINATION ltp_rfcdest
        EXPORTING
          i_object_id    = ekko-zz_po_auth_code
        IMPORTING
          source_rel_ind = lv_source_rel_ind.

      IF sy-subrc = 0.
        IF lv_source_rel_ind <> 'Y'.
          MESSAGE e022(zmm_message) WITH ekko-zz_po_auth_code  .
        ENDIF.
      ENDIF.
* End of Changes by <Chaya>

    ENDIF.
  ENDIF.
ENDIF.
