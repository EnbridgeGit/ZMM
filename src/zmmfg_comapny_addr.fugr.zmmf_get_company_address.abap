FUNCTION zmmf_get_company_address.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"  EXPORTING
*"     VALUE(E_NAME) TYPE  AD_NAME1
*"     VALUE(E_CITY1) TYPE  AD_CITY1
*"     VALUE(E_POST_CODE1) TYPE  AD_PSTCD1
*"     VALUE(E_STREET) TYPE  AD_STREET
*"     VALUE(E_COUNTRY) TYPE  LKVRZ
*"     VALUE(E_REGION) TYPE  REGIO
*"  TABLES
*"      IT_T001 STRUCTURE  T001
*"----------------------------------------------------------------------


  DATA : wa_adrc TYPE adrc,
         lv_adrnr TYPE adrnr,
          lv_country TYPE lkvrz.

** Get the Address number from Company code
  SELECT SINGLE
          adrnr
         FROM
        t001
       INTO lv_adrnr
       WHERE  bukrs = i_bukrs.

  IF sy-subrc = 0.
** Get the Address details from the address number
    SELECT SINGLE
           name1
           city1
           post_code1
           street
           country
           region
           FROM adrc
           INTO  (wa_adrc-name1 ,wa_adrc-city1 , wa_adrc-post_code1, wa_adrc-street ,wa_adrc-country,wa_adrc-region)
           WHERE addrnumber = lv_adrnr.

    IF sy-subrc = 0.
      e_name         = wa_adrc-name1 .
      e_city1        = wa_adrc-city1 .
      e_post_code1   = wa_adrc-post_code1 .
      e_street       = wa_adrc-street .
      e_region       = wa_adrc-region.

*** Get the Country description from country key
      SELECT SINGLE
             lkvrz
            INTO lv_country
            FROM  t005
            WHERE land1 = wa_adrc-country.
      IF  sy-subrc = 0.
        e_country      = lv_country.
      ENDIF.

    ENDIF.
  ENDIF.





ENDFUNCTION.
