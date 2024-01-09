FUNCTION-POOL zariba_contract.              "MESSAGE-ID ..

* INCLUDE LZARIBA_CONTRACTD...               " Local class definition

************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Function Group Name:  ZARIBA_CONTRACT                               *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 17, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        : This FM creates Service contracts through BDC  *
*&                      Call Transaction and Material/Quantity contract*
*&                      using BAPI: BAPI_CONTRACT_CREATE               *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No : DECK914182                                           *
* Description   : Initial development                                  *
*----------------------------------------------------------------------*

************************************************************************
*                  Global Internal Tables                              *
************************************************************************
DATA: git_bdcdata  TYPE TABLE OF bdcdata,                    " Internal table for BDCDATA
      gwa_bdcdata  LIKE LINE OF git_bdcdata,                 " Work area for BDCDATA
      git_messtab  TYPE TABLE OF bdcmsgcoll,                 " Internal table for BDC error msg
      git_messtab1 TYPE TABLE OF bdcmsgcoll,                 " Internal table for BDC error msg
      git_messtab2 TYPE TABLE OF bdcmsgcoll,                 " Internal table for BDC error msg
      gwa_messtab  LIKE LINE OF git_messtab,                 " Work area for BDC error msg
      git_olaid    TYPE STANDARD TABLE OF zcontract_olaid,   " Internal table for OLAID
      gwa_olaid    LIKE LINE OF git_olaid,                   " Work Area for it_olaid
      git_msg      TYPE STANDARD TABLE OF bapiret2,          " Internal table for Return parameter
      git_msg1     TYPE STANDARD TABLE OF bapiret2,          " Internal table for Return parameter
      git_msg2     TYPE STANDARD TABLE OF bapiret2,          " Internal table for Return parameter
      gwa_msg      LIKE LINE OF git_msg.                     " Work Area for it_msg

************************************************************************
*                  Global Data Types                             *
************************************************************************
DATA: gv_olaid     TYPE ebeln. " OLAID in ECC (PO)
