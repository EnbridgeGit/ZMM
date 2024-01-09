FUNCTION ZMM_GET_ARIBA_APPROVER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EXP_ARIBA_APRV) TYPE  Z_ARIBA_APPROVER OPTIONAL
*"  EXPORTING
*"     VALUE(IMP_ARIBA_APRV) TYPE  ZTT_ARIBAAPRV
*"----------------------------------------------------------------------

*Get Ariba Approver
*  if exp_ariba_aprv is not initial.
*    select mandt
*           zariba_approver
*           zariba_email
*           into table imp_ariba_aprv
*           from zaribaaprv
*           where zariba_approver = exp_ariba_aprv.
*  else.
*    select mandt
*           zariba_approver
*  zariba_email
*  into table imp_ariba_aprv
*  from zaribaaprv.
*
*  endif.
  types : begin of lty_ariba_aprv,
          ZARIBA_APPROVER type Z_ARIBA_APPROVER,
          ZARIBA_EMAIL type Z_ARIBA_EMAIL,
          ZSVR_CONF_SAP_ID type XUBNAME,
          end of lty_ariba_aprv,

          begin of lty_address,
          BNAME type char12,
          NAME_FIRST type char40,
          NAME_LAST type char40,
          end of lty_address.

  data : lt_ariba type standard table of lty_ariba_aprv,
         lwa_ariba type  lty_ariba_aprv,
         lt_address type standard tabLe of lty_address,
         lwa_address type lty_address,
         lwa_ariba_aprv type zmms_ariba.

* Get Ariba approvers
  if exp_ariba_aprv is not initial.
    select zariba_approver
         zariba_email
         ZSVR_CONF_SAP_ID
         from zaribaaprv
         into table lt_ariba
         where zariba_approver = exp_ariba_aprv.
  else.
    select zariba_approver
           zariba_email
           ZSVR_CONF_SAP_ID
           from zaribaaprv
           into table lt_ariba .
  endif.
  if sy-subrc = 0 and lt_ariba is not initial.
*   Get Ariba approver first name and last name
    select bname
           name_first
           name_last
           from user_addr
           into table lt_address
           for all entries in lt_ariba
           where bname = lt_ariba-ZSVR_CONF_SAP_ID.
  endif.

  loop at lt_ariba into lwa_ariba.
    lwa_ariba_aprv-zariba_approver = lwa_ariba-zariba_approver.
    lwa_ariba_aprv-zariba_email = lwa_ariba-zariba_email.
    lwa_ariba_aprv-ZSVR_CONF_SAP_ID = lwa_ariba-ZSVR_CONF_SAP_ID.
    read table lt_address into lwa_address with key bname = lwa_ariba-ZSVR_CONF_SAP_ID.
    if sy-subrc = 0.
      lwa_ariba_aprv-name_first = lwa_address-name_first.
      lwa_ariba_aprv-name_last = lwa_address-name_last.
    endif.
    append lwa_ariba_aprv to imp_ariba_aprv.
    clear lwa_ariba_aprv.
  endloop.

ENDFUNCTION.
