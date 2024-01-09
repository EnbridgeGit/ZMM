*****           Implementation of object type ZZBUS2012            *****
INCLUDE <object>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      PURCHASEORDER LIKE EKKO-EBELN,
  END OF KEY,
      REQUISITIONER TYPE SWC_OBJECT,
      SRC TYPE SWC_OBJECT,
      REQNO TYPE EKPO-BEDNR.
END_DATA OBJECT. " Do not change.. DATA is generated

get_property reqno changing container.

SELECT SINGLE bednr FROM ekpo INTO object-reqno
WHERE ebeln = object-key-purchaseorder AND
      bednr NE ' '.

swc_set_element container 'ReqNo' object-reqno.
end_property.

get_property requisitioner changing container.

DATA: w_user TYPE ekpo-afnam,
      w_obj TYPE swc_object.

SELECT SINGLE afnam FROM ekpo INTO w_user
WHERE ebeln = object-key-purchaseorder AND
      afnam NE ' '.

IF NOT w_user IS INITIAL.
  swc_create_object object-requisitioner 'USR01DOHR' w_user.
ENDIF.


swc_set_element container 'Requisitioner' object-requisitioner.
end_property.

get_property src changing container.

DATA: lv_appr TYPE ekko-zzariba_approver.

SELECT SINGLE zzariba_approver FROM ekko INTO lv_appr
WHERE ebeln = object-key-purchaseorder.

IF NOT lv_appr IS INITIAL.
  swc_create_object object-src 'USR01DOHR' lv_appr.
ENDIF.


swc_set_element container 'SRC' object-src.
end_property.

begin_method dummy changing container.
MESSAGE i000(zfi_workflow) WITH 'Execute from UWL'.
end_method.
*&---------------------------------------------------------------------*
*& Program Name       : ZBUS2012                                       *
*& Author             : Snehasis Dutta / sdutta                        *
*& Creation Date      : 09-MAY-2016                                    *
*& Object ID          : ACR-159                                        *
*& Application Area   : SC-MM                                          *
*& Description        : Depending on a Purchase Order creation a       *
*& notification mail will be sent to requisitioner & goods recepient   *
*&---------------------------------------------------------------------*
begin_method zpoemail changing container.
*TYPES: BEGIN OF lty_req,
*        ebelp       TYPE ebelp,
*        afnam       TYPE afnam,
*       END OF lty_req,
*
*       BEGIN OF lty_goods,
*         ebelp      TYPE ebelp,
*         wempf      TYPE wempf ,
*       END OF lty_goods,
*
*       BEGIN OF lty_usr21,
*         bname      TYPE xubname,
*         persnumber TYPE ad_persnum,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_usr21,
*
*       BEGIN OF lty_adr6,
*         smtp_addr   TYPE ad_smtpadr,
*       END OF lty_adr6,
*
*       BEGIN OF lty_usrid,
*         usrid TYPE char12,
*       END OF lty_usrid,
*
*       BEGIN OF lty_parid,
*         bname TYPE xubname,
*         parid TYPE memoryid,
*         parva TYPE xuvalue,
*       END OF lty_parid.
*
*DATA: lta_req    TYPE STANDARD TABLE OF lty_req,
*      lwa_req    TYPE lty_req,
*      lta_goods  TYPE STANDARD TABLE OF lty_goods,
*      lwa_goods  TYPE lty_goods,
*      lta_usr21  TYPE STANDARD TABLE OF lty_usr21,
*      lwa_usr21  TYPE lty_usr21,
*      lta_adr6   TYPE STANDARD TABLE OF lty_adr6,
*      lwa_adr6   TYPE lty_adr6,
*      lta_usrid  TYPE STANDARD TABLE OF lty_usrid,
*      lwa_usrid  TYPE lty_usrid,
*      lv_doctype TYPE ekko-bsart,
*      lv_vendor  TYPE lifnr,
*      lv_vendor_name TYPE name1_gp,
*      lta_body   TYPE soli_tab,
*      lwa_body   TYPE soli,
*      lv_subject TYPE string,
*      lv_var1    TYPE string,
*      lv_var2    TYPE string,
*      lv_var3    TYPE string,
*      lv_var4    TYPE string,
*      lta_return TYPE  bapiret2_tab,
*      lta_parid  TYPE STANDARD TABLE OF lty_parid,
*      lwa_parid  TYPE lty_parid,
*      lv_reqno   TYPE bednr.
*
*CONSTANTS: lc_zf TYPE bsart VALUE 'ZF',
*           lc_nb TYPE bsart VALUE 'NB',
*           lc_x  TYPE char1 VALUE 'X',
*           lc_parid TYPE memoryid VALUE 'Z_NO_CRT_EM'.
*
*SELECT SINGLE bsart FROM ekko INTO lv_doctype
*WHERE ebeln = object-key-purchaseorder AND
*      bsart NE ' '.
*
*IF lv_doctype = lc_zf OR
*   lv_doctype = lc_nb.
*
*  SELECT SINGLE lifnr FROM ekko INTO lv_vendor
*    WHERE ebeln = object-key-purchaseorder.
*
*  IF sy-subrc IS INITIAL.
*    SELECT SINGLE name1 FROM lfa1 INTO lv_vendor_name
*      WHERE lifnr = lv_vendor.
*  ENDIF.
*
*  SELECT ebelp afnam
*    INTO TABLE lta_req
*    FROM ekpo
*    WHERE ebeln = object-key-purchaseorder.
*
*  IF sy-subrc IS INITIAL.
*    LOOP AT lta_req INTO lwa_req.
*      lwa_usrid-usrid = lwa_req-afnam.
*      APPEND lwa_usrid TO lta_usrid.
*    ENDLOOP.
*  ENDIF.
*
*  SELECT ebelp wempf
*    INTO TABLE lta_goods
*    FROM ekkn
*    WHERE ebeln = object-key-purchaseorder.
*
*  IF sy-subrc IS INITIAL.
*    LOOP AT lta_goods INTO lwa_goods.
*      lwa_usrid-usrid = lwa_goods-wempf.
*      APPEND lwa_usrid TO lta_usrid.
*    ENDLOOP.
*  ENDIF.
*
*  SORT lta_usrid BY usrid.
*  DELETE ADJACENT DUPLICATES FROM lta_usrid COMPARING ALL FIELDS.
*
*  IF NOT lta_usrid[] IS INITIAL.
*
*    SELECT bname parid parva
*      FROM usr05
*      INTO TABLE lta_parid
*      FOR ALL ENTRIES IN lta_usrid
*      WHERE bname = lta_usrid-usrid AND
*            parid = lc_parid.
*
*    IF sy-subrc = 0.
*      SORT lta_parid BY bname.
*      LOOP AT lta_parid INTO lwa_parid WHERE parva = lc_x.
*        DELETE lta_usrid WHERE usrid = lwa_parid-bname.
*        CLEAR lwa_parid.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
**       Get Email address
*  IF NOT lta_usrid[] IS INITIAL.
*
*    SELECT bname
*           persnumber
*           addrnumber
*           FROM usr21
*           INTO TABLE lta_usr21
*           FOR ALL ENTRIES IN lta_usrid
*           WHERE bname = lta_usrid-usrid.
*
*    IF sy-subrc IS INITIAL AND lta_usr21 IS NOT INITIAL.
*
*      SELECT smtp_addr
*        FROM adr6
*        INTO TABLE lta_adr6
*        FOR ALL ENTRIES IN lta_usr21
*        WHERE persnumber = lta_usr21-persnumber AND
*              addrnumber = lta_usr21-addrnumber.
*
*      IF sy-subrc IS INITIAL.
*        SORT lta_adr6 BY smtp_addr."persnumber addrnumber.
*        DELETE ADJACENT DUPLICATES FROM lta_adr6 COMPARING ALL FIELDS.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  lv_reqno = object-reqno.
*
*  lv_var1 = 'Shopping Cart'.
*  lv_var2 = 'has been completely approved and created a Purchase Order. Please find the PO number:'.
*  lv_var3 = 'Vendor:'.
*
*  CONCATENATE lv_var1 lv_reqno lv_var2 object-key-purchaseorder lv_var3 lv_vendor_name
*              INTO lwa_body-line SEPARATED BY space.
*  APPEND lwa_body TO lta_body.
*  CLEAR lwa_body.
*
*  lv_var4 = 'PO Creation notification for SC'.
*  CONCATENATE lv_var4 lv_reqno INTO lv_subject SEPARATED BY space.
*
*  CALL FUNCTION 'ZMM_EMAIL_SEND'
*    EXPORTING
*      iw_subject    = lv_subject
*      it_email_data = lta_body
*      it_receivers  = lta_adr6
*    TABLES
*      it_return     = lta_return.
*ENDIF.

end_method.

BEGIN_METHOD ZGETUSEREMAIL CHANGING CONTAINER.
DATA:
      USERID TYPE WFSYST-INITIATOR,
      USEREMAIL TYPE ADR6-SMTP_ADDR.

  SWC_GET_ELEMENT CONTAINER 'Userid' USERID.

  CALL FUNCTION 'ZMM_USER_EMAIL'
    EXPORTING
      USERID = userid
    IMPORTING
      USER_EMAIL = useremail
    EXCEPTIONS
      NO_EMAIL = 01
      OTHERS = 02.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN 01.    " to be implemented
    WHEN OTHERS.       " to be implemented
  ENDCASE.
  SWC_SET_ELEMENT CONTAINER 'UserEmail' USEREMAIL.

END_METHOD.

BEGIN_METHOD ZCAP_BUYERSDECISION CHANGING CONTAINER.
DATA:
      ivebeln TYPE ekko-ebeln,
      evflag type usr07-REASON,
      rejectcomments TYPE wfsyst-result.


ivebeln = object-key-purchaseorder.
"swc_get_element container '' releasecode.

  CALL FUNCTION 'ZMM_CAPPO_DECISION'
    EXPORTING
      IV_EBELN = ivebeln
    IMPORTING
      EV_COMMENTS = rejectcomments
      EV_FLAG = evflag
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.

swc_set_element container 'RejectComments' rejectcomments.
swc_set_element container 'ActionFlag' evflag.

END_METHOD.

BEGIN_METHOD ZCAP_INITIATORDECISION CHANGING CONTAINER.
DATA:
      ivebeln TYPE ekko-ebeln.

ivebeln = object-key-purchaseorder.
  CALL FUNCTION 'ZMM_CAPPO_DECISION_INIT'
    EXPORTING
      IV_EBELN = ivebeln
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.
END_METHOD.

BEGIN_METHOD ZCAP_APSCREEN CHANGING CONTAINER.
DATA:
      ivebeln TYPE ekko-ebeln.

ivebeln = object-key-purchaseorder.

  CALL FUNCTION 'ZMM_CAPPO_DECISION_AP'
    EXPORTING
      IV_EBELN = ivebeln
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.

END_METHOD.

BEGIN_METHOD ZDOWNPAYMENT_REQUIRE CHANGING CONTAINER.

DATA: ivebeln TYPE ekko-ebeln,
      ev_downpayment TYPE xfeld.

ivebeln = object-key-purchaseorder.

  CALL FUNCTION 'ZMM_CAPPO_DOWNPAY_REQUIRE'
    EXPORTING
      IV_EBELN = ivebeln
    IMPORTING
      EV_DOWNPAYMENT = ev_downpayment
    EXCEPTIONS
      OTHERS = 01.
  CASE SY-SUBRC.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.

   SWC_SET_ELEMENT CONTAINER 'DownPayment_Step' ev_downpayment.

END_METHOD.
