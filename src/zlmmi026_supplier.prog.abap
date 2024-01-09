*&---------------------------------------------------------------------*
*& Report  ZLMMI026_SUPPLIER
*&
*&---------------------------------------------------------------------*
REPORT  zlmmi026_supplier .
************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI026_SUPPLIER                             *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 05, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts supplier data and places the *
*&                       Outbound CSV file on to SE FTP server. These  *
*&                       file is being used in ARIBA Spend Viz         *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No : D30K925438                                           *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*

INCLUDE zlmmi026_supplier_top.



***********************************************************************
*                           Selection Screen                           *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: rb_appl RADIOBUTTON GROUP grp1
                    USER-COMMAND cmd
                    DEFAULT 'X'.
PARAMETERS: rb_pres RADIOBUTTON GROUP grp1.

PARAMETERS: p_fpath  TYPE rlgrap-filename
                     DEFAULT gc_fpath MODIF ID csv.       " Application server path
PARAMETERS: p_ppath  TYPE rlgrap-filename
                     DEFAULT gc_ppath MODIF ID pre.       " Presentaion server path

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  IF NOT rb_pres IS INITIAL.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'CSV'.
          screen-input = '0'.
          screen-output = '0'.
          screen-invisible = '1'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF NOT rb_appl IS INITIAL.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PRE'.
          screen-input = '0'.
          screen-output = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.

**  .CSV file name validations
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath.
  PERFORM f_directory_selection CHANGING p_fpath.

** Presentation server path
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ppath.
  PERFORM f_directory_selection CHANGING p_ppath.

AT SELECTION-SCREEN.
  IF rb_appl IS NOT INITIAL.
    IF p_fpath IS INITIAL.
      CLEAR p_fpath.
      MOVE gc_fpath TO p_fpath.
    ENDIF.
  ENDIF.
  IF rb_pres IS NOT INITIAL.
    IF p_ppath IS INITIAL.
      CLEAR p_ppath.
      MOVE gc_ppath TO p_ppath.
    ENDIF.
  ENDIF.



************************************************************************
*                        START-OF-SELECTION                            *
************************************************************************
START-OF-SELECTION.
  PERFORM f_get_program_parms. "Get progam execution date details
  PERFORM f_get_language_desc. "Language description

** Fetch and send Activ Supplier data
  PERFORM f_get_active_suppliers. "Fetch Active suppliers data from data base"
  IF git_lfa1[] IS NOT INITIAL.
    PERFORM f_prepare_header.    "Prepare header for file  "
    PERFORM f_prepare_file.   " Prepare final output     "
    PERFORM f_send_file USING gc_act. " send file to location    "
*  ELSE.
*    MESSAGE 'No Active Suppliers Selected for given date range' TYPE 'S'.
  ENDIF.

** Fetch and send Inactive/Blocked Supplier data
  PERFORM f_get_inactive_suppliers. "Fetch Inactive/Blocked suppliers data from data base"
  IF git_lfa1[] IS NOT INITIAL.
    PERFORM f_prepare_header.    "Prepare header for file  "
    PERFORM f_prepare_file.   " Prepare final output     "
    PERFORM f_send_file USING gc_inact. " send file to location    "
*  ELSE.
*    MESSAGE 'No Inactive/Blocked Suppliers Selected for given date range' TYPE 'S'.
  ENDIF.

**  File download status message
  IF gv_flag EQ 'X'.
    MESSAGE 'File downloaded to path successfully..' TYPE 'S'.
  ELSE.
    MESSAGE 'No Data Found' TYPE 'E'.
  ENDIF.
************************************************************************
*                        END-OF-SELECTION                            *
************************************************************************
END-OF-SELECTION.

**Inserting execution date&time
  CLEAR gwa_zprog.
  gwa_zprog-zclnt = sy-mandt.
  gwa_zprog-zprog = sy-cprog.
  gwa_zprog-zexec_date = sy-datum.
  gwa_zprog-zexec_time = sy-uzeit.
  MODIFY zexec_date FROM gwa_zprog.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  f_get_active_suppliers
*&---------------------------------------------------------------------*
*   Fetch Active suppliers data from  ECC tables
*----------------------------------------------------------------------*
FORM f_get_active_suppliers .
**Get data from tables(lfa1,lfb1,lfm1 and fmfg_lfaccr)
  IF gv_ldate IS NOT INITIAL.
    SELECT lifnr
           name1
           stras
           ort01
           regio
           land1
           pstlz
           ktokk
           brsch
           telf1
           spras
           telfx
           emnfr
           erdat
           adrnr
       INTO TABLE git_lfa1 FROM lfa1
       WHERE ( erdat GT gv_ldate AND erdat LE sy-datum ) " Record Creation date
         AND loevm EQ space    " Record should not have deletion indicator in lfa1
         AND sperr EQ space    " Record should not have Central Posting Block
         AND sperm EQ space    " Record should not have Central Puchasing Block
         AND sperz EQ space.   " Record should not have Payment Block
  ELSE. "For Initial Run/load"
    SELECT lifnr
           name1
           stras
           ort01
           regio
           land1
           pstlz
           ktokk
           brsch
           telf1
           spras
           telfx
           emnfr
           erdat
           adrnr
       INTO TABLE git_lfa1 FROM lfa1
       WHERE erdat LE sy-datum. " Record Creation date
  ENDIF.

  IF git_lfa1[] IS INITIAL.
    EXIT. " No data selected for given criteria.
  ELSE.

**  Get data email details from adr6
    SELECT addrnumber
           smtp_addr
      INTO TABLE git_adr6 FROM adr6 FOR ALL ENTRIES IN git_lfa1
      WHERE addrnumber EQ git_lfa1-adrnr.

** Get ContactFirstName(VERKF)from lfm1
    SELECT lifnr
           verkf
     INTO TABLE git_lfm1 FROM lfm1 FOR ALL ENTRIES IN git_lfa1
      WHERE lifnr EQ git_lfa1-lifnr.
    IF git_lfm1[] IS NOT INITIAL.
      SORT git_lfm1 BY lifnr.
    ENDIF.

** Get Vendor account group  description
    SELECT ktokk txt30 FROM t077y INTO TABLE git_t077y FOR ALL ENTRIES IN git_lfa1
      WHERE ktokk EQ git_lfa1-ktokk
       AND  spras EQ git_lfa1-spras.
    IF sy-subrc EQ 0.
      SORT git_t077y BY ktokk.
      DELETE ADJACENT DUPLICATES FROM git_t077y COMPARING ALL FIELDS.
    ENDIF.
  ENDIF.
ENDFORM.                    " f_get_active_suppliers


*&---------------------------------------------------------------------*
*&      Form  f_get_inactive_suppliers
*&---------------------------------------------------------------------*
*   Fetch Inactive/Blocked suppliers data from  ECC tables
*----------------------------------------------------------------------*
FORM f_get_inactive_suppliers .
  REFRESH : git_lfa1, git_adr6, git_lfm1, git_fmfg, git_t077y.
**Get data from tables(lfa1,lfb1,lfm1 and fmfg_lfaccr)
  IF gv_ldate IS NOT INITIAL.
    PERFORM f_read_changedocs.        " Read change documents
    IF git_cdpos[] IS NOT INITIAL.
      SELECT lifnr
             name1
             stras
             ort01
             regio
             land1
             pstlz
             ktokk
             brsch
             telf1
             spras
             telfx
             emnfr
             erdat
             adrnr
         INTO TABLE git_lfa1 FROM lfa1 FOR ALL ENTRIES IN git_cdpos
        WHERE lifnr EQ git_cdpos-lifnr.
    ENDIF.

  ELSE. "for initial load/run

    SELECT lifnr
             name1
             stras
             ort01
             regio
             land1
             pstlz
             ktokk
             brsch
             telf1
             spras
             telfx
             emnfr
             erdat
             adrnr
         INTO TABLE git_lfa1 FROM lfa1
        WHERE erdat LE sy-datum  " Creation date
         AND ( loevm EQ gc_x    " Record have deletion indicator in lfa1
         OR sperr EQ gc_x    " Record have Central Posting Block
         OR sperm EQ gc_x    " Record have Central Puchasing Block
         OR sperz EQ gc_x ).   " Record have Payment Bloc
  ENDIF.

  IF git_lfa1[] IS INITIAL.
    EXIT. " No data selected for given criteria.
  ELSE.

**  Get data email details from adr6
    SELECT addrnumber
           smtp_addr
      INTO TABLE git_adr6 FROM adr6 FOR ALL ENTRIES IN git_lfa1
      WHERE addrnumber EQ git_lfa1-adrnr.

** Get ContactFirstName(VERKF)from lfm1
    SELECT lifnr
           verkf
     INTO TABLE git_lfm1 FROM lfm1 FOR ALL ENTRIES IN git_lfa1
      WHERE lifnr EQ git_lfa1-lifnr.
    IF git_lfm1[] IS NOT INITIAL.
      SORT git_lfm1 BY lifnr.
    ENDIF.

** Get Vendor account group  description
    SELECT ktokk txt30 FROM t077y INTO TABLE git_t077y FOR ALL ENTRIES IN git_lfa1
      WHERE ktokk EQ git_lfa1-ktokk
       AND  spras EQ git_lfa1-spras.
    IF sy-subrc EQ 0.
      SORT git_t077y BY ktokk.
      DELETE ADJACENT DUPLICATES FROM git_t077y COMPARING ALL FIELDS.
    ENDIF.

  ENDIF.
ENDFORM.                    " f_get_inactive_suppliers
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_HEADER
*&---------------------------------------------------------------------*
*      File header preparation
*----------------------------------------------------------------------*
FORM f_prepare_header .
  CLEAR: gwa_final, gwa_file.

  gwa_final-lifnr  = '"SupplierId"'.
  gwa_final-bukrs  = '"SupplierLocationId"'.
  gwa_final-name1  = '"SupplierName"'.
  gwa_final-stras  = '"StreetAddress"'.
  gwa_final-ort01  = '"City"'.
  gwa_final-regio  = '"State"'.
  gwa_final-land1  = '"Country"'.
  gwa_final-pstlz  = '"PostalCode"'.
  gwa_final-ktokk  = '"SupplierType"'.
  gwa_final-verkf  = '"ContactFirstName"'.
  gwa_final-brsch  = '"ContactLastName"'.
  gwa_final-telf1  = '"ContactPhoneNumber"'.
  gwa_final-smtp_addr = '"ContactEmail"'.
  gwa_final-spras  = '"PreferredLanguage"'.
  gwa_final-telfx  = '"FaxNumber"'.
  gwa_final-ordtyp = '"OrderRoutingType"'.
  gwa_final-duns   = '"DUNSNumber"'.
  gwa_final-emnfr  = '"ANNumber"'.
  gwa_final-zwels  = '"PaymentType"'.
  gwa_final-dsity  = '"Diversity"'.
  gwa_final-mowned = '"MinorityOwned"'.
  gwa_final-wowned = '"WomanOwned"'.
  gwa_final-vowned = '"VeteranOwned"'.
  gwa_final-dsba8a = '"DiversitySBA8A"'.
  gwa_final-dhzone = '"DiversityHUBZone"'.
  gwa_final-dstb   = '"DiversitySDB"'.
  gwa_final-ddvo   = '"DiversityDVO"'.
  gwa_final-decity = '"DiversityEthnicity"'.
  gwa_final-dgo    = '"DiversityGLBTOwned"'.
  gwa_final-ddo    = '"DiversityDisabledOwned"'.
  gwa_final-dls    = '"DiversityLaborSurplus"'.
  gwa_final-dhbcu  = '"DiversityHBCU"'.
  gwa_final-dsb    = '"DiversitySmallBusiness"'.
  gwa_final-dg     = '"DiversityGreen"'.
  gwa_final-cd     = '"CertifiedDiversity"'.
  gwa_final-cmo    = '"CertifiedMinorityOwned"'.
  gwa_final-cwo    = '"CertifiedWomanOwned"'.
  gwa_final-cvo    = '"CertifiedVeteranOwned"'.
  gwa_final-csba8a = '"CertifiedSBA8A"'.
  gwa_final-chzone = '"CertifiedHUBZone"'.
  gwa_final-csdb   = '"CertifiedSDB"'.
  gwa_final-ce     = '"CertifiedEthnicity"'.
  gwa_final-cdo    = '"CertifiedDisabledOwned"'.
  gwa_final-cdis   = '"CertifiedDisadvantaged"'.
  gwa_final-dep    = '"DiversityEnterprise"'.
  gwa_final-moe    = '"MinorityOwnedEnterprise"'.
  gwa_final-woe    = '"WomanOwnedEnterprise"'.
  gwa_final-voe    = '"VeteranOwnedEnterprise"'.
  gwa_final-dvoe   = '"DVOEnterprise"'.
  gwa_final-ee     = '"EthnicityEnterprise"'.
  gwa_final-dent   = '"DisadvantagedEnterprise"'.
  gwa_final-noe    = '"NumberOfEmployees"'.
  gwa_final-ff1    = '"FlexField1 (Reconciliation Account)"'.
  gwa_final-ff2    = '"FlexField2 (Payment term)"'.
  gwa_final-ff3    = '"FlexField3"'.
*  INSERT gwa_final INTO git_final INDEX 1. " Added as first line

  CONCATENATE gwa_final-lifnr
              gwa_final-bukrs
              gwa_final-name1
              gwa_final-stras
              gwa_final-ort01
              gwa_final-regio
              gwa_final-land1
              gwa_final-pstlz
              gwa_final-ktokk
              gwa_final-verkf
              gwa_final-brsch
              gwa_final-telf1
              gwa_final-smtp_addr
              gwa_final-spras
              gwa_final-telfx
              gwa_final-ordtyp
              gwa_final-duns
              gwa_final-emnfr
              gwa_final-zwels
              gwa_final-dsity
              gwa_final-mowned
              gwa_final-wowned
              gwa_final-vowned
              gwa_final-dsba8a
              gwa_final-dhzone
              gwa_final-dstb
              gwa_final-ddvo
              gwa_final-decity
              gwa_final-dgo
              gwa_final-ddo
              gwa_final-dls
              gwa_final-dhbcu
              gwa_final-dsb
              gwa_final-dg
              gwa_final-cd
              gwa_final-cmo
              gwa_final-cwo
              gwa_final-cvo
              gwa_final-csba8a
              gwa_final-chzone
              gwa_final-csdb
              gwa_final-ce
              gwa_final-cdo
              gwa_final-cdis
              gwa_final-dep
              gwa_final-moe
              gwa_final-woe
              gwa_final-voe
              gwa_final-dvoe
              gwa_final-ee
              gwa_final-dent
              gwa_final-noe
              gwa_final-ff1
              gwa_final-ff2
              gwa_final-ff3 INTO gwa_file-line SEPARATED BY ','.
  INSERT gwa_file INTO git_file INDEX 1.  " Inserting as first line in file

ENDFORM.                    " F_PREPARE_HEADER

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_FILE
*&---------------------------------------------------------------------*
*       Prepare Output CSV file
*----------------------------------------------------------------------*
FORM f_prepare_file.
  CLEAR:gwa_lfa1, gwa_final.
  REFRESH: gr_actsupp, git_final.
** Fill Final internal table
  LOOP AT git_lfa1 INTO gwa_lfa1.

**  Fill Range table with active suppliers
*    gr_actsupp-low  = gwa_lfa1-lifnr.
*    gr_actsupp-sign = 'I'.
*    gr_actsupp-option = 'EQ'.
*    APPEND gr_actsupp.
*    CLEAR gr_actsupp.

    gwa_final-lifnr  = gwa_lfa1-lifnr.
    gwa_final-name1  = gwa_lfa1-name1.
    gwa_final-stras  = gwa_lfa1-stras.
    gwa_final-ort01  = gwa_lfa1-ort01.
    gwa_final-regio  = gwa_lfa1-regio.
    gwa_final-land1  = gwa_lfa1-land1.
    gwa_final-pstlz  = gwa_lfa1-pstlz.
    gwa_final-brsch  = gwa_lfa1-brsch.
    gwa_final-telf1  = gwa_lfa1-telf1.
    gwa_final-telfx  = gwa_lfa1-telfx.
    gwa_final-emnfr  = gwa_lfa1-emnfr.

**Add Business Unit in SupplierLocationID
    gwa_final-bukrs  = gc_bunit.

**  Convert language key to ISO language key (two char ex: E->EN)
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = gwa_lfa1-spras
      IMPORTING
        output = gwa_final-spras.

    READ TABLE git_t002 INTO gwa_t002 WITH KEY laiso = gwa_final-spras.
    IF sy-subrc EQ 0.
      READ TABLE git_t002t INTO gwa_t002t WITH KEY spras = 'E'
                                                   sprsl = gwa_t002-spras.
      IF sy-subrc EQ 0.
        gwa_final-spras = gwa_t002t-sptxt.
      ENDIF.
      CLEAR: gwa_t002, gwa_t002t.
    ENDIF.

**Read Vendor account group name
    READ TABLE git_t077y INTO gwa_t077y WITH KEY ktokk = gwa_lfa1-ktokk.
    IF sy-subrc EQ 0.
      gwa_final-ktokk = gwa_t077y-desc.
      CLEAR gwa_t077y.
    ENDIF.

** Read VERKF for ContactLastNname from git_lfm1
    CLEAR gwa_lfm1.
    READ TABLE git_lfm1 INTO gwa_lfm1 WITH KEY lifnr = gwa_lfa1-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gwa_final-verkf  = gwa_lfm1-verkf.
    ENDIF.

**Read Email address from git_adr6
    CLEAR gwa_adr6.
    READ TABLE git_adr6 INTO gwa_adr6 WITH KEY addrnumber = gwa_lfa1-adrnr.
    IF sy-subrc EQ 0.
      gwa_final-smtp_addr = gwa_adr6-smtp_addr.
    ENDIF.

    APPEND gwa_final TO git_final.
    CLEAR: gwa_lfa1, gwa_final.
  ENDLOOP.

** Prepare lines in required csv formate
  CLEAR gwa_file.
  LOOP AT git_final INTO gwa_final.
** Delete un-wanted additional spaces in all fields
    CONDENSE:
             gwa_final-lifnr  ,
             gwa_final-bukrs  ,
             gwa_final-name1  ,
             gwa_final-stras  ,
             gwa_final-ort01  ,
             gwa_final-regio  ,
             gwa_final-land1  ,
             gwa_final-pstlz  ,
             gwa_final-ktokk  ,
             gwa_final-verkf  ,
             gwa_final-brsch  ,
             gwa_final-telf1  ,
             gwa_final-smtp_addr,
             gwa_final-spras  ,
             gwa_final-telfx  ,
             gwa_final-ordtyp ,
             gwa_final-duns   ,
             gwa_final-emnfr  ,
             gwa_final-zwels  ,
             gwa_final-dsity  ,
             gwa_final-mowned ,
             gwa_final-wowned ,
             gwa_final-vowned ,
             gwa_final-dsba8a ,
             gwa_final-dhzone ,
             gwa_final-dstb   ,
             gwa_final-ddvo   ,
             gwa_final-decity ,
             gwa_final-dgo    ,
             gwa_final-ddo    ,
             gwa_final-dls    ,
             gwa_final-dhbcu  ,
             gwa_final-dsb    ,
             gwa_final-dg     ,
             gwa_final-cd     ,
             gwa_final-cmo    ,
             gwa_final-cwo    ,
             gwa_final-cvo    ,
             gwa_final-csba8a ,
             gwa_final-chzone ,
             gwa_final-csdb   ,
             gwa_final-ce     ,
             gwa_final-cdo    ,
             gwa_final-cdis   ,
             gwa_final-dep    ,
             gwa_final-moe    ,
             gwa_final-woe    ,
             gwa_final-voe    ,
             gwa_final-dvoe   ,
             gwa_final-ee     ,
             gwa_final-dent   ,
             gwa_final-noe    ,
             gwa_final-ff1    ,
             gwa_final-ff2    ,
             gwa_final-ff3    .

    IF gwa_final-lifnr NE space.
      CONCATENATE '"' gwa_final-lifnr '"' INTO gwa_final-lifnr.
    ENDIF.
** SupplierLocationId -company code/business unit
    IF gwa_final-bukrs NE space.
      CONCATENATE '"' gwa_final-bukrs '"' INTO gwa_final-bukrs.
    ENDIF.
** SupplierName
    IF gwa_final-name1 NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-name1 WITH '""'.
      CONCATENATE '"' gwa_final-name1 '"' INTO gwa_final-name1.
    ENDIF.
** StreetAddress
    IF gwa_final-stras NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-stras WITH '""'.
      CONCATENATE '"' gwa_final-stras '"' INTO gwa_final-stras.
    ENDIF.
**  City
    IF gwa_final-ort01 NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-ort01 WITH '""'.
      CONCATENATE '"' gwa_final-ort01 '"' INTO gwa_final-ort01.
    ENDIF.
**  State
    IF gwa_final-regio NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-regio WITH '""'.
      CONCATENATE '"' gwa_final-regio '"' INTO gwa_final-regio.
    ENDIF.
**  Country
    IF gwa_final-land1 NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-land1 WITH '""'.
      CONCATENATE '"' gwa_final-land1 '"' INTO gwa_final-land1.
    ENDIF.
**  PostalCode
    IF gwa_final-pstlz NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-pstlz WITH '""'.
      CONCATENATE '"' gwa_final-pstlz '"' INTO gwa_final-pstlz.
    ENDIF.
**  SupplierType
    IF gwa_final-ktokk NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-ktokk WITH '""'.
      CONCATENATE '"' gwa_final-ktokk '"' INTO gwa_final-ktokk.
    ENDIF.
** ContactFirstName
    IF gwa_final-verkf NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-verkf WITH '""'.
      CONCATENATE '"' gwa_final-verkf '"' INTO gwa_final-verkf.
    ENDIF.
**  ContactLastName
    IF gwa_final-brsch NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-brsch WITH '""'.
      CONCATENATE '"' gwa_final-brsch '"' INTO gwa_final-brsch.
    ENDIF.
**  ContactPhoneNumber
    IF gwa_final-telf1 NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-telf1 WITH '""'.
      CONCATENATE '"' gwa_final-telf1 '"' INTO gwa_final-telf1.
    ENDIF.
**  ContactEmail
    IF gwa_final-smtp_addr NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-smtp_addr WITH '""'.
      CONCATENATE '"' gwa_final-smtp_addr '"' INTO gwa_final-smtp_addr.
    ENDIF.
**  PreferredLanguage
    IF gwa_final-spras NE space.
      CONCATENATE '"' gwa_final-spras '"' INTO gwa_final-spras.
    ENDIF.
**  FaxNumber
    IF gwa_final-telfx NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-telfx WITH '""'.
      CONCATENATE '"' gwa_final-telfx '"' INTO gwa_final-telfx.
    ENDIF.

**  ANNumber
    IF gwa_final-emnfr NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-emnfr WITH '""'.
      CONCATENATE '"' gwa_final-emnfr '"' INTO gwa_final-emnfr.
    ENDIF.

** Prepare comma(,) seperated rerod
    CLEAR gwa_file.
    CONCATENATE gwa_final-lifnr
                gwa_final-bukrs
                gwa_final-name1
                gwa_final-stras
                gwa_final-ort01
                gwa_final-regio
                gwa_final-land1
                gwa_final-pstlz
                gwa_final-ktokk
                gwa_final-verkf
                gwa_final-brsch
                gwa_final-telf1
                gwa_final-smtp_addr
                gwa_final-spras
                gwa_final-telfx
                gwa_final-ordtyp
                gwa_final-duns
                gwa_final-emnfr
                gwa_final-zwels
                gwa_final-dsity
                gwa_final-mowned
                gwa_final-wowned
                gwa_final-vowned
                gwa_final-dsba8a
                gwa_final-dhzone
                gwa_final-dstb
                gwa_final-ddvo
                gwa_final-decity
                gwa_final-dgo
                gwa_final-ddo
                gwa_final-dls
                gwa_final-dhbcu
                gwa_final-dsb
                gwa_final-dg
                gwa_final-cd
                gwa_final-cmo
                gwa_final-cwo
                gwa_final-cvo
                gwa_final-csba8a
                gwa_final-chzone
                gwa_final-csdb
                gwa_final-ce
                gwa_final-cdo
                gwa_final-cdis
                gwa_final-dep
                gwa_final-moe
                gwa_final-woe
                gwa_final-voe
                gwa_final-dvoe
                gwa_final-ee
                gwa_final-dent
                gwa_final-noe
                gwa_final-ff1
                gwa_final-ff2
                gwa_final-ff3 INTO gwa_file-line SEPARATED BY ','.
    APPEND gwa_file TO git_file.
    CLEAR gwa_final.

  ENDLOOP.

ENDFORM.                    " F_PREPARE_FILE
*&---------------------------------------------------------------------*
*&      Form  F_SEND_FILE
*&---------------------------------------------------------------------*
*       Send output CSV file
*----------------------------------------------------------------------*
FORM f_send_file USING p_name TYPE string.
  DATA: lv_fname       TYPE string,  "File name
        lv_ffname      TYPE string,  "Full File name
        lv_msg         TYPE c,  "Error messages
        lv_date(10)    TYPE c,  " Preferred date formate
        lv_clnt        TYPE sy-mandt, " System client
        lv_sys         TYPE sy-sysid, " Systerm Name
        lv_name        TYPE string.   " Dynamic file name

  lv_name  = p_name. "file name
  lv_sys   = sy-sysid. " Logon System Name
  lv_clnt  = sy-mandt. " Lognon Client Name

** Date formate dd-mm-yyyy
  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum+0(4) INTO lv_date SEPARATED BY '-'.
**prepare file name
  CONCATENATE lv_sys lv_clnt lv_name lv_date '_' sy-uzeit '.CSV' INTO lv_fname.
  IF rb_appl IS NOT INITIAL
    AND p_fpath IS NOT INITIAL. " File download to Application server
** Combine path and file name
    CONCATENATE p_fpath lv_fname INTO lv_ffname.

**  Open Dataset to write data
    OPEN DATASET lv_ffname FOR OUTPUT IN TEXT MODE MESSAGE lv_msg ENCODING DEFAULT.
    IF sy-subrc EQ 0.
      LOOP AT git_file INTO gwa_file.
        TRANSFER gwa_file-line TO lv_ffname.
        CLEAR gwa_file.
      ENDLOOP.
      REFRESH git_file.
      gv_flag = 'X'.  " File download message flag
    ELSE.
**  Error handling here
      MESSAGE 'File open error' TYPE 'E'.
    ENDIF.
**  Close the Dataset
    CLOSE DATASET lv_ffname.
  ENDIF.

  IF rb_pres IS NOT INITIAL
    AND p_ppath IS NOT INITIAL. " File download to Presentation server
    CLEAR gv_flag.
** Combine path and file name
    CONCATENATE p_ppath lv_fname INTO lv_ffname.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_ffname
        filetype                = 'ASC'
      TABLES
        data_tab                = git_file
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc EQ 0.
      gv_flag = 'X'.
    ENDIF.
    REFRESH git_file.
  ENDIF.
ENDFORM.                    " F_SEND_FILE
*&---------------------------------------------------------------------*
*&      Form  F_GET_PROGRAM_PARMS
*&---------------------------------------------------------------------*
*      Get last execution date and finalise date for selection
*----------------------------------------------------------------------*
FORM f_get_program_parms .
  CLEAR:gv_ldate, gv_ltime.
** Get Last Execution data from custom table zexec_date
  SELECT zclnt zprog zexec_date zexec_time FROM zexec_date INTO TABLE git_zprog
    WHERE zprog EQ sy-cprog
      AND zexec_date LT sy-datum.
  IF sy-subrc EQ 0.
    SORT git_zprog BY zexec_date zexec_time DESCENDING.
    READ TABLE git_zprog INTO gwa_zprog INDEX 1.
    gv_ldate = gwa_zprog-zexec_date.
    gv_ltime = gwa_zprog-zexec_time.
    CLEAR gwa_zprog.
  ENDIF.

ENDFORM.                    " F_GET_PROGRAM_PARMS
*&---------------------------------------------------------------------*
*&      Form  F_GET_LANGUAGE_DESC
*&---------------------------------------------------------------------*
*       Language Description
*----------------------------------------------------------------------*
FORM f_get_language_desc .

  REFRESH: git_t002, git_t002t.

**Get all records from t002 table
  SELECT spras laiso FROM t002 INTO CORRESPONDING FIELDS OF TABLE git_t002 WHERE spras NE space.

** Get only records which has language key 'E'
  SELECT spras sprsl sptxt FROM t002t INTO CORRESPONDING FIELDS OF TABLE git_t002t WHERE spras EQ 'E'.

ENDFORM.                    " F_GET_LANGUAGE_DESC

*&---------------------------------------------------------------------*
*&      Form  F_DIRECTORY_SELECTION
*&---------------------------------------------------------------------*
*      Select Directory for download
*----------------------------------------------------------------------*

FORM f_directory_selection  CHANGING p_p_path TYPE localfile.
  DATA lv_path TYPE string.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder = lv_path
    EXCEPTIONS
      OTHERS          = 0.
  IF sy-subrc EQ 0.
    CONCATENATE lv_path '\' INTO lv_path.
    p_p_path = lv_path.
    WRITE: p_p_path.
  ENDIF.
ENDFORM.                    " F_DIRECTORY_SELECTION

*&---------------------------------------------------------------------*
*&      Form  F_READ_CHANGEDOCS
*&---------------------------------------------------------------------*
*      Change details
*----------------------------------------------------------------------*
FORM f_read_changedocs .
  REFRESH: git_cdhdr, git_cdpos.
**  T-codes
  gr_tcode-sign   = 'I'.
  gr_tcode-option = 'EQ'.
  gr_tcode-low    = 'XK05'.
  APPEND gr_tcode. CLEAR gr_tcode.

  gr_tcode-sign   = 'I'.
  gr_tcode-option = 'EQ'.
  gr_tcode-low    = 'MK05'.
  APPEND gr_tcode. CLEAR gr_tcode.

  gr_tcode-sign   = 'I'.
  gr_tcode-option = 'EQ'.
  gr_tcode-low    = 'FK05'.
  APPEND gr_tcode. CLEAR gr_tcode.

  gr_tcode-sign   = 'I'.
  gr_tcode-option = 'EQ'.
  gr_tcode-low    = 'XK06'.
  APPEND gr_tcode. CLEAR gr_tcode.

**  Field names
  gr_fname-sign   = 'I'.
  gr_fname-option = 'EQ'.
  gr_fname-low    = 'LOEVM'.
  APPEND gr_fname. CLEAR gr_fname.

  gr_fname-sign   = 'I'.
  gr_fname-option = 'EQ'.
  gr_fname-low    = 'SPERR'.
  APPEND gr_fname. CLEAR gr_fname.

  gr_fname-sign   = 'I'.
  gr_fname-option = 'EQ'.
  gr_fname-low    = 'SPERM'.
  APPEND gr_fname. CLEAR gr_fname.

  gr_fname-sign   = 'I'.
  gr_fname-option = 'EQ'.
  gr_fname-low    = 'SPERZ'.
  APPEND gr_fname. CLEAR gr_fname.

  SORT gr_actsupp.

  SELECT objectid changenr udate tcode FROM cdhdr INTO TABLE git_cdhdr
    WHERE ( udate GE gv_ldate AND udate LE sy-datum )
*      AND objectid NOT IN gr_actsupp[]
      AND tcode IN gr_tcode.

  IF git_cdhdr[] IS NOT INITIAL.
    SELECT objectid changenr fname value_new FROM cdpos INTO CORRESPONDING FIELDS OF TABLE git_cdpos
      FOR ALL ENTRIES IN git_cdhdr
      WHERE changenr EQ git_cdhdr-changenr
        AND tabname  EQ 'LFA1'
        AND fname IN gr_fname
        AND value_new EQ gc_x.

    IF git_cdpos[] IS NOT INITIAL.
      SORT git_cdpos BY objectid.
      DELETE ADJACENT DUPLICATES FROM git_cdpos COMPARING objectid.
      LOOP AT git_cdpos INTO gwa_cdpos.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gwa_cdpos-objectid
          IMPORTING
            output = gwa_cdpos-lifnr.
        MODIFY git_cdpos FROM gwa_cdpos INDEX sy-tabix TRANSPORTING lifnr.
        CLEAR gwa_cdpos.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_READ_CHANGEDOCS
