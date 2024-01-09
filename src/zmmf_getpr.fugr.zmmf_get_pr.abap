FUNCTION zmmf_get_pr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BANFN) TYPE  BANFN OPTIONAL
*"  EXPORTING
*"     VALUE(E_AFNAM) TYPE  AFNAM
*"     VALUE(E_ERNAM) TYPE  ERNAM
*"  TABLES
*"      LIT_EBAN STRUCTURE  ZMMS_GET_RP
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Program Name       : ZMMF_GET_PR                                    *
*& Author             : NPATIL                                        *
*& Creation Date      : 12-Spt-2011                                   *
*& Object ID          : SRM_SE033-PO_DOCUMENT_TYPE_DETERMINATION_CANADA        *
*& Application Area   : SRM                                           *
*& Description        : To cover the portion of Purchase to Pay process*
*&                      associated with the determination of           *
*&                      backend PO document type                       *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*


  DATA: lwa_eban1 TYPE zmms_get_rp.
  TYPES : BEGIN OF ty_eban,
          banfn TYPE banfn,
          ernam TYPE ernam,
          afnam TYPE afnam,
    END OF ty_eban.

  DATA : it_eban TYPE  TABLE OF ty_eban,
         lwa_eban TYPE ty_eban.
  SELECT
         banfn
         ernam
         afnam
         FROM eban
         INTO TABLE  it_eban
         WHERE banfn = i_banfn.

  IF sy-subrc  = 0.

    LOOP AT it_eban INTO lwa_eban.
      IF sy-tabix = 1.
        e_afnam = lwa_eban-afnam.
      ENDIF.
      lwa_eban1-banfn = lwa_eban-banfn .
      lwa_eban1-afnam  = lwa_eban-afnam .
      APPEND  lwa_eban1 TO  lit_eban.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
