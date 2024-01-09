FUNCTION ZMM_GET_TAX_CODE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_PLANT) TYPE  WERKS_D
*"  EXPORTING
*"     VALUE(EXP_TAX_CODE) TYPE  Z_TAX_CODE
*"----------------------------------------------------------------------
* Get tax code for the plant
select single zztax_code
       from t001w
       into exp_tax_code
       where werks = imp_plant.

ENDFUNCTION.
