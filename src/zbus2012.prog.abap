*****           Implementation of object type ZBUS2012             *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      PURCHASEORDER LIKE EKKO-EBELN,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated

BEGIN_METHOD GETLIMITCONTRACT CHANGING CONTAINER.
DATA:
      ZPOCONTRACT LIKE ZPOCONTRACT OCCURS 0,
      EBELN TYPE EKPO-EBELN,
      ITEM TYPE EKPO-EBELP.
  SWC_GET_ELEMENT CONTAINER 'Item' ITEM.

EBELN = object-key.
CALL FUNCTION 'ZSRM_GET_CONTRACT_NO'
  EXPORTING
    EBELN         = EBELN
    EBELP         = ITEM
  TABLES
    DETAILS       = ZPOCONTRACT.

  SWC_SET_TABLE CONTAINER 'ZPOCONTRACT' ZPOCONTRACT.
END_METHOD.

BEGIN_METHOD UPDATEPO CHANGING CONTAINER.
DATA:
      ZPOCONTRACT LIKE ZPOCONTRACT OCCURS 0 with header line.
DATA: EBELN TYPE EBELN,
      ITEM LIKE BAPIMEPOITEM occurs 0 with header line,
      ITEMX LIKE BAPIMEPOITEMX  occurs 0 with header line,
      T_RETURN LIKE BAPIRET2  occurs 0 with header line.
Data: msg1 type char50,
      msg2 type char50,
      msg3 type char50,
      msg4 type char50.
data: lv_tabix type i.

  EBELN = OBJECT-KEY.
  SWC_GET_TABLE CONTAINER 'ZPOCONTRACT' ZPOCONTRACT.

  CALL FUNCTION 'BAPI_PO_GETDETAIL1'
    EXPORTING
      PURCHASEORDER            = EBELN
    TABLES
      POITEM                   = ITEM
          .

LOOP AT ZPOCONTRACT.
read table ITEM with key po_item = zpocontract-EBELP.
lv_tabix = sy-tabix.
* ITEM-po_item = zpocontract-EBELP.
 ITEM-AGREEMENT = zpocontract-KONNR.
 ITEM-AGMT_ITEM = zpocontract-KTPNR.
* append ITEM.
modify ITEM index lv_tabix transporting AGREEMENT AGMT_ITEM.
 clear ITEM.
 ITEMX-po_item = zpocontract-EBELP.
 ITEMX-AGREEMENT = 'X'.
 ITEMX-AGMT_ITEM = 'X'.
 append ITEMX.
 clear ITEMX.
ENDLOOP.

CALL FUNCTION 'BAPI_PO_CHANGE'
  EXPORTING
    PURCHASEORDER                = EBELN
  TABLES
   RETURN                       = T_RETURN
   POITEM                       = ITEM
   POITEMX                      = ITEMX
          .
*READ TABLE T_RETURN WITH KEY TYPE = 'E'.
loop at T_RETURN WHERE TYPE = 'E'.
endloop.
IF sy-subrc = 0.
clear: msg1, msg2, msg3, msg4.
msg1 = t_return-message+0(50).
msg2 = t_return-message+50(50).
msg3 = t_return-message+100(50).
msg4 = t_return-message+150(50).
    exit_return 9001 msg1 msg2 msg3 msg4.
ENDIF.

END_METHOD.

BEGIN_METHOD GETUSERTYPE CHANGING CONTAINER.
DATA:
      AGENT TYPE WFSYST-AGENT,
      TYPE TYPE USR02-USTYP.
Data: lv_bname like usr02-bname.

  SWC_GET_ELEMENT CONTAINER 'Agent' AGENT.
lv_bname = Agent+2(12).

select single USTYP from USR02 into TYPE
where bname = lv_bname.

  SWC_SET_ELEMENT CONTAINER 'type' TYPE.
END_METHOD.
