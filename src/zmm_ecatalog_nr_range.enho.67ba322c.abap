"Name: \PR:SAPLMEPO\FO:MEPO_CHECK_BSART\SE:END\EI
ENHANCEMENT 0 ZMM_ECATALOG_NR_RANGE.
*&---------------------------------------------------------------------*
*& Program Name     :  ZMM_ECATALOG_NR_RANGE                           *
*& Author           :  Praveena Anusuri                                *
*& Creation Date    :  12/16/2015                                      *
*& Object ID        :  SDP91677 - eCatalog Project                     *
*& Application Area :  MM                                              *
*& Description      :  Control Number Range for eCatalog POs.          *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                  Modification Log(Latest Version on Top)             *
*----------------------------------------------------------------------*
* Version No    : Initial version                                      *
* Date          : 12/16/2015                                           *
* Modified By   : Praveena Anusuri                                     *
* Object ID     : SDP91677 - eCatalog Project                          *
* Description   : Control Number Range for eCatalog Purchase Orders.   *
*----------------------------------------------------------------------*
DATA: lit_zvar TYPE STANDARD TABLE OF zvar,
      lv_po_from    TYPE ekko-ebeln,
      lv_po_to      TYPE ekko-ebeln.

FIELD-SYMBOLS: <lfs_zvar> LIKE LINE OF lit_zvar.

*Get data from ZVAR table
SELECT * FROM zvar
       INTO TABLE lit_zvar
       WHERE programm = 'ZMM_ECATALOG_NR_RANGE'.
IF sy-subrc EQ 0.
  SORT lit_zvar BY varname.
ENDIF.

CLEAR: lv_po_from,
       lv_po_to.

*Check for eCatalog PO based on Purchase Order number coming from SRM
READ TABLE lit_zvar ASSIGNING <lfs_zvar> WITH KEY varname = 'DOC_RANGE' BINARY SEARCH.
IF sy-subrc EQ 0.
  lv_po_from = <lfs_zvar>-value1.
  lv_po_to   = <lfs_zvar>-value2.
  IF fekkoadd-extnr GE lv_po_from AND fekkoadd-extnr LE lv_po_to.
*   Get ECC new Number Range for eCatalog Purchase Orders
    READ TABLE lit_zvar ASSIGNING <lfs_zvar> WITH KEY varname = 'RANGE' BINARY SEARCH.
    IF sy-subrc EQ 0.
      t161-numke = <lfs_zvar>-value1.
    ENDIF.
  ENDIF.
ENDIF.

ENDENHANCEMENT.
