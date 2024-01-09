*&---------------------------------------------------------------------*
*& Report  ZLMMI033_SUPPLIER_ORG
*&---------------------------------------------------------------------*

REPORT  zlmmi033_supplier_org.
************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI033_SUPPLIER_ORG                         *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 09, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  Program extracts supplier Organization/ID Part*
*&                       data and places the Outbound CSV files on to  *
*&                       SE FTP server. These files are used in        *
*&                       ARIBA Upstream for Supplier data update       *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No : D30K925223                                           *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 06/25/2015                                           *
* Modified By   : Praveena Anusuri                                     *
* Object ID     : SDP87437                                             *
* Description   : Changes made to extract SAP Ariba Supplier.          *
*----------------------------------------------------------------------*

INCLUDE zlmmi033_supplier_org_top.

***********************************************************************
*                           Selection Screen                           *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_acgrp FOR lfa1-ktokk NO INTERVALS,
                s_porg FOR lfm1-ekorg NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.

PARAMETERS: rb_appl RADIOBUTTON GROUP grp1
                    USER-COMMAND cmd
                    DEFAULT 'X'.
PARAMETERS: rb_pres RADIOBUTTON GROUP grp1.

PARAMETERS: p_fpath  TYPE rlgrap-filename
                     DEFAULT gc_fpath MODIF ID csv.       " .CSV file path
PARAMETERS: p_zpath  TYPE rlgrap-filename
                     DEFAULT gc_zpath MODIF ID zip.       " .ZIP file path
PARAMETERS: p_ppath  TYPE rlgrap-filename
                     DEFAULT gc_ppath MODIF ID pre.       " Presentaion server path

SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  IF NOT rb_pres IS INITIAL.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'CSV'.
          screen-input = '0'.
          screen-output = '0'.
          screen-invisible = '1'.
        WHEN 'ZIP'.
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

**  .ZIP file name validations
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_zpath.
  PERFORM f_directory_selection CHANGING p_zpath.

** Presentation server path
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ppath.
  PERFORM f_directory_selection CHANGING p_ppath.

AT SELECTION-SCREEN.
  IF rb_appl IS NOT INITIAL.
    IF p_fpath IS INITIAL.
      CLEAR p_fpath.
      MOVE gc_fpath TO p_fpath.
    ENDIF.
    IF p_zpath IS INITIAL.
      CLEAR p_zpath.
      MOVE gc_zpath TO p_zpath.
    ENDIF.
  ENDIF.
  IF rb_pres IS NOT INITIAL.
    IF p_ppath IS INITIAL.
      CLEAR p_ppath.
      MOVE gc_ppath TO p_ppath.
    ENDIF.
  ENDIF.
*  IF s_acgrp[] IS INITIAL.
*    MESSAGE 'Please Enter a value for Account group' TYPE 'E'.
*  ENDIF.

************************************************************************
*                        START-OF-SELECTION                            *
************************************************************************
START-OF-SELECTION.
  PERFORM f_get_program_parms. "Get progam execution date details

** Active suppliers file data
  PERFORM f_get_data_act.       " Fetch Active suppliers data from database "
*  IF git_lfa1 IS NOT INITIAL. "(+) PANUSURI Ticket 87347 (-) PANUSURI Ticket 88323
  PERFORM f_prepare_header1. " Prepare header for SupplierOrganization file "
  PERFORM f_prepare_file.           " Prepare final output   "
  PERFORM f_send_file USING gc_org. " send file to location  "
  PERFORM f_prepare_file2 USING gc_idpart.
*  ENDIF.                       "(+) PANUSURI Ticket 87347  (-) PANUSURI Ticket 88323

** Inactive suppliers file data
  PERFORM f_get_data_inact.       " Fetch Inactive suppliers data from database "
*  IF git_lfa1 IS NOT INITIAL. "(+) PANUSURI Ticket 87347 (-) PANUSURI Ticket 88323
  PERFORM f_prepare_header1. " Prepare header for SupplierOrganization_Delete file "
  PERFORM f_prepare_file.               " Prepare final output  "
  PERFORM f_send_file USING gc_org_del. " send file to location "
**  Prepare and send SupplierOrganizationIDPart file
*    IF git_final1[] IS NOT INITIAL.
  PERFORM f_prepare_file2 USING gc_idpart_del.
*  ENDIF.                       "(+) PANUSURI Ticket 87347  (-) PANUSURI Ticket 88323
*    ENDIF.

**if Application server is selected
  IF rb_appl IS NOT INITIAL.
*      DESCRIBE TABLE git_lfa1 LINES gv_orgdel.
*      EXPORT gv_orgdel TO MEMORY ID 'GV_ORGDEL'. " Export lines to memory for zip program
    EXPORT p_fpath TO MEMORY ID 'CSV_PATH'. "Export CSV file path for zip program
    EXPORT p_zpath TO MEMORY ID 'ZIP_PATH'. "Export ZIP file path for zip program
    SUBMIT zlmmi033_supplier_org_zip AND RETURN.
  ENDIF.
*  ENDIF.

**  File download status message
  IF gv_flag EQ 'X'.
    MESSAGE 'File downloaded to path successfully..' TYPE 'S'.
*  ELSE.
*    MESSAGE 'No Data Selected' TYPE 'W'.
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
*&      Form  F_GET_DATA_ACT
*&---------------------------------------------------------------------*
*   Fetch Active Supplier data from ECC tables
*----------------------------------------------------------------------*
FORM f_get_data_act .
*- Begin of Change by PANUSURI - SDP87437 : Ariba Supplier Extract Issue
  DATA: lit_lfa1 TYPE STANDARD TABLE OF ty_lfa1,
        lit_lfm1 TYPE STANDARD TABLE OF ty_lfm1.

  CLEAR: lit_lfm1, lit_lfa1.

**Get data from tables(lfa1,lfb1 and Adr6)
  IF gv_ldate IS NOT INITIAL.
    SELECT lifnr
           name1
           stras
           ort01
           regio
           land1
           pstlz
           telf1
           telfx
           stceg
           adrnr
           stcd1
       INTO CORRESPONDING FIELDS OF TABLE git_lfa1 FROM lfa1
*- Begin of Change by PANUSURI - SDP87437 : Ariba Supplier Extract Issue 6/30/2015
*       WHERE ( erdat GT gv_ldate AND erdat LE sy-datum ) " Record Creation date
       WHERE ( erdat GE gv_ldate AND erdat LE sy-datum )
*- End of Change by PANUSURI - SDP87437 : Ariba Supplier Extract Issue  6/30/2015
         AND  ktokk IN s_acgrp   " Account group
         AND  name1 NE space    " Name1 is mandatory field on output file
         AND  loevm EQ space    " Record should not have deletion indicator in lfa1
         AND  sperr EQ space    " Record should not have Central Posting Block
         AND  sperm EQ space    " Record should not have Central Puchasing Block
         AND  sperz EQ space.    " Record should not have Payment Block

*- Begin of Change by PANUSURI - SDP87437 : Ariba Supplier Extract Issue  6/30/2015

    SELECT lifnr FROM lfm1 INTO TABLE lit_lfm1
      WHERE ( erdat GE gv_ldate AND erdat LE sy-datum )
      AND ekorg IN s_porg
      AND loevm EQ space
      AND sperm EQ space.
    IF sy-subrc EQ 0 .
      SORT lit_lfm1 BY lifnr.
      DELETE ADJACENT DUPLICATES FROM git_lfm1 COMPARING lifnr.
      SELECT  lifnr    "SupplierId
              name1    "SupplierName
              stras    "StreetAddress
              ort01    " City
              regio    " State
              land1    " Country
              pstlz    " Postal Code
              telf1    " Telephone Number
              telfx    " Fax Number
              stceg    " VAT Registrastion ID
              adrnr    " Address Number
              stcd1    " Tax Number1
        FROM lfa1 INTO TABLE lit_lfa1
        FOR ALL ENTRIES IN lit_lfm1
        WHERE lifnr = lit_lfm1-lifnr
         AND  ktokk IN s_acgrp
         AND  name1 NE space    " Name1 is mandatory field on output file
         AND  loevm EQ space    " Record should not have deletion indicator in lfa1
         AND  sperr EQ space    " Record should not have Central Posting Block
         AND  sperm EQ space    " Record should not have Central Puchasing Block
         AND  sperz EQ space.    " Record should not have Payment Block
      IF sy-subrc EQ 0.
        APPEND LINES OF lit_lfa1 TO git_lfa1.
        SORT git_lfa1 BY lifnr.
        DELETE ADJACENT DUPLICATES FROM git_lfa1 COMPARING lifnr.
      ENDIF.
    ENDIF.
*- End of Change by PANUSURI - SDP87437 : Ariba Supplier Extract Issue 6/30/2015

  ELSE.  "For Initial Run/load"

    SELECT lifnr
           name1
           stras
           ort01
           regio
           land1
           pstlz
           telf1
           telfx
           stceg
           adrnr
           stcd1
       INTO CORRESPONDING FIELDS OF TABLE git_lfa1 FROM lfa1
       WHERE erdat LE sy-datum  " Record Creation date
        AND  ktokk IN s_acgrp   " Account group
        AND  name1 NE space.    " Name1 is mandatory field on output file
  ENDIF.

  IF git_lfa1[] IS NOT INITIAL.
*- Begin of Change by PANUSURI - SDP87437 : Ariba Supplier Extract Issue  6/30/2015
***   Get Data from LFB1
*    SELECT lifnr bukrs FROM lfb1 INTO TABLE git_lfb1 FOR ALL ENTRIES IN git_lfa1
*      WHERE lifnr EQ git_lfa1-lifnr
*        AND loevm EQ space.
*    IF git_lfb1[] IS NOT INITIAL.
*      SORT git_lfb1 BY lifnr bukrs.
*      DELETE ADJACENT DUPLICATES FROM git_lfb1 COMPARING ALL FIELDS.
*    ENDIF.
*- End of Change by PANUSURI - SDP87437 : Ariba Supplier Extract Issue  6/30/2015

**   Get Data from LFM1
    SELECT lifnr ekorg FROM lfm1 INTO TABLE git_lfm1 FOR ALL ENTRIES IN git_lfa1
    WHERE lifnr EQ git_lfa1-lifnr
      AND ekorg IN s_porg
      AND loevm EQ space
      AND sperm EQ space.
    IF git_lfm1[] IS NOT INITIAL.
      SORT git_lfm1 BY lifnr ekorg.
*      DELETE ADJACENT DUPLICATES FROM git_lfm1 COMPARING ALL FIELDS. "(-) PANUSURI Ticket 87347
    ENDIF.
**  Get data email details from adr6
    SELECT addrnumber
           smtp_addr
      INTO TABLE git_adr6 FROM adr6 FOR ALL ENTRIES IN git_lfa1
      WHERE addrnumber EQ git_lfa1-adrnr.

    IF git_adr6[] IS NOT INITIAL.
      SORT git_adr6 BY addrnumber.
      DELETE ADJACENT DUPLICATES FROM git_adr6 COMPARING ALL FIELDS.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_GET_DATA_ACT

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_INACT
*&---------------------------------------------------------------------*
*   Fetch Inactive Supplier data from ECC tables
*----------------------------------------------------------------------*
FORM f_get_data_inact .
  REFRESH: git_lfa1, git_lfb1, git_lfm1, git_adr6.
**Get data from tables(lfa1,lfb1 and Adr6)
  SELECT lifnr
          name1
          stras
          ort01
          regio
          land1
          pstlz
          telf1
          telfx
          stceg
          adrnr
          stcd1
      INTO TABLE git_lfa1 FROM lfa1
      WHERE ktokk IN s_acgrp  " Account Group
       AND  name1 NE space    " Name1 is mandatory field on output file
       AND ( loevm EQ gc_x    " Record have deletion indicator in lfa1
       OR sperr EQ gc_x       " Record have Central Posting Block
       OR sperm EQ gc_x       " Record have Central Puchasing Block
       OR sperz EQ gc_x ).    " Record have Payment Block

  IF gv_ldate IS NOT INITIAL.
    IF git_lfa1[] IS NOT INITIAL.
      LOOP AT git_lfa1 INTO gwa_lfa1.
        gwa_cdhdr-objectid = gwa_lfa1-lifnr.
        APPEND gwa_cdhdr TO git_lifnr.
        CLEAR:gwa_lfa1,gwa_cdhdr.
      ENDLOOP.
      REFRESH git_lfa1.
    ENDIF.

    IF git_lifnr[] IS NOT INITIAL.
      PERFORM f_read_changedocs.        " Read change documents
    ENDIF.

    IF git_cdpos[] IS NOT INITIAL.
      SELECT lifnr
             name1
             stras
             ort01
             regio
             land1
             pstlz
             telf1
             telfx
             stceg
             adrnr
             stcd1
         INTO TABLE git_lfa1 FROM lfa1 FOR ALL ENTRIES IN git_cdpos
        WHERE lifnr EQ git_cdpos-lifnr
          AND  ktokk IN s_acgrp
          AND  name1 NE space.    " Name1 is mandatory field on output file
    ENDIF.
  ENDIF.

  IF git_lfa1[] IS NOT INITIAL.
**    Delete duplicate suppliers/vendors
    SORT git_lfa1 BY lifnr.
    DELETE ADJACENT DUPLICATES FROM git_lfa1 COMPARING lifnr.
*- Begin of Change by PANUSURI - SDP87437 : Ariba Supplier Extract Issue  6/30/2015
***   Get Data from LFB1
*    SELECT lifnr bukrs FROM lfb1 INTO TABLE git_lfb1 FOR ALL ENTRIES IN git_lfa1
*      WHERE lifnr EQ git_lfa1-lifnr.
*    IF git_lfb1[] IS NOT INITIAL.
*      SORT git_lfb1 BY lifnr bukrs.
*      DELETE ADJACENT DUPLICATES FROM git_lfb1 COMPARING ALL FIELDS.
*    ENDIF.
*- End of Change by PANUSURI - SDP87437 : Ariba Supplier Extract Issue  6/30/2015

**   Get Data from LFM1
    SELECT lifnr ekorg FROM lfm1 INTO TABLE git_lfm1 FOR ALL ENTRIES IN git_lfa1
    WHERE lifnr EQ git_lfa1-lifnr
      AND ekorg IN s_porg.
    IF git_lfm1[] IS NOT INITIAL.
      SORT git_lfm1 BY lifnr ekorg.
      DELETE ADJACENT DUPLICATES FROM git_lfm1 COMPARING ALL FIELDS.
    ENDIF.

**  Get data email details from adr6
    SELECT addrnumber
           smtp_addr
      INTO TABLE git_adr6 FROM adr6 FOR ALL ENTRIES IN git_lfa1
      WHERE addrnumber EQ git_lfa1-adrnr.

    IF git_adr6[] IS NOT INITIAL.
      SORT git_adr6 BY addrnumber.
      DELETE ADJACENT DUPLICATES FROM git_adr6 COMPARING ALL FIELDS.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_GET_DATA_INACT

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_HEADER1
*&---------------------------------------------------------------------*
*    SupplierOrganization.csv header preparation
*----------------------------------------------------------------------*
FORM f_prepare_header1 .
  CLEAR gwa_file.

**   " UTF-8 header line "
  DATA lc_hdr TYPE string VALUE '"UTF-8",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'.
  MOVE lc_hdr TO gwa_file-line.
  INSERT gwa_file INTO git_file INDEX 1.  " Inserting as first line in file
  CLEAR gwa_file.

  CONCATENATE
            'TaxID'
            'StateTIN'
            'RegionalTIN'
            'PreferredLanguage.UniqueName'
            'SystemID'
            'VatID'
            'AnnualRevenueMaximum.Amount'
            'AnnualRevenueMaximum.Currency.UniqueName'
            'AnnualRevenueMinimum.Amount'
            'AnnualRevenueMinimum.Currency.UniqueName'
            'CorporateAddress.PostalAddress.City'
            'CorporateAddress.PostalAddress.Country.UniqueName'
            'CorporateAddress.PostalAddress.Lines'
            'CorporateAddress.PostalAddress.PostalCode'
            'CorporateAddress.PostalAddress.State'
            'CorporateAddress.UniqueName'
            'CorporateEmailAddress'
            'CorporateFax'
            'CorporatePhone'
            'CorporateURL'
            'HasSyncRelationship'
            'HasTradingRelationship'
            'IsCustomer'
            'IsManaged'
            'IsOrgApproved'
            'IsSupplier'
            'Name'
            'NumberOfEmployees'
            'OrganizationType'
            'PreferredCurrency.UniqueName'
            'StateOfIncorporation'
            'YearFounded'
             INTO gwa_file-line SEPARATED BY ','.
  INSERT gwa_file INTO git_file INDEX 2.  " Inserting as second line in file
ENDFORM.                    " F_PREPARE_HEADER1

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_FILE
*&---------------------------------------------------------------------*
*       Prepare Output CSV file
*----------------------------------------------------------------------*
FORM f_prepare_file.
  REFRESH:git_final, git_final1, gr_actsupp.
  DATA lv_supp(10).

** Fill Final internal table
  LOOP AT git_lfa1 INTO gwa_lfa1.

**  Consider records which has Company code and purchase.Org'.
*    IF git_lfb1[] IS NOT INITIAL.
*      READ TABLE git_lfb1 INTO gwa_lfb1 WITH KEY lifnr = gwa_lfa1-lifnr BINARY SEARCH.
*      IF sy-subrc NE 0.
*        CONTINUE.
*      ENDIF.
*    ELSE.
*      CONTINUE.
*    ENDIF.

    IF git_lfm1[] IS NOT INITIAL.
      READ TABLE git_lfm1 INTO gwa_lfm1 WITH KEY lifnr = gwa_lfa1-lifnr BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

*    gwa_final-lifnr  = gwa_lfa1-lifnr.
    gwa_final-name1  = gwa_lfa1-name1.
    gwa_final-stras  = gwa_lfa1-stras.
    gwa_final-ort01  = gwa_lfa1-ort01.
    gwa_final-regio  = gwa_lfa1-regio.
    gwa_final-land1  = gwa_lfa1-land1.
    gwa_final-pstlz  = gwa_lfa1-pstlz.
    gwa_final-telf1  = gwa_lfa1-telf1.
    gwa_final-telfx  = gwa_lfa1-telfx.
    gwa_final-stceg  = gwa_lfa1-stceg.

    IF gwa_lfa1-lifnr IS NOT INITIAL.
      CLEAR lv_supp.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gwa_lfa1-lifnr
        IMPORTING
          output = lv_supp.
    ENDIF.

**ADD 'US' prefix to SystemID (lfa1-lifnr)
    IF gwa_lfa1-lifnr NE space.
      CONCATENATE gc_bunit lv_supp INTO gwa_final-lifnr SEPARATED BY '-'.
    ENDIF.

**  TaxID field uses the format 'BussinessUnit-SystemID_lfa1-stcd1'
    IF gwa_lfa1-stcd1 IS NOT INITIAL.
      CONCATENATE gwa_final-lifnr gwa_lfa1-stcd1 INTO gwa_final-taxid SEPARATED BY '_'.
    ELSE.
      gwa_final-taxid = gwa_final-lifnr.
    ENDIF.

**Read Email address from git_adr6
    CLEAR gwa_adr6.
    READ TABLE git_adr6 INTO gwa_adr6 WITH KEY addrnumber = gwa_lfa1-adrnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gwa_final-smtp_addr = gwa_adr6-smtp_addr.
    ENDIF.

**   CorporateAddress.UniqueName
    gwa_final-bukrs  = gwa_final-lifnr.

**    Hardcode the required values
    gwa_final-hsr    = gc_no.  "HasSyncRelationship = NO    "
    gwa_final-htr    = gc_no.  "HasTradingRelationship = NO "
    gwa_final-iscust = gc_no.  "IsCustomer = NO             "
    gwa_final-ismng  = gc_0.   "IsManaged = 0               "
    gwa_final-isorga = gc_2.   " IsOrgApproved = 2          "
    gwa_final-issup  = gc_yes. " IsSupplier = YES           "

    APPEND gwa_final TO git_final.  "SupplierOrganization Data"

**Start-Fill internal table for SupplierOrganizationIDPart.csv file "
    gwa_final1-domain = gc_buyer.
    CONCATENATE gc_bunit lv_supp INTO gwa_final1-value SEPARATED BY '-'.
    CONCATENATE gc_bunit lv_supp INTO gwa_final1-psys SEPARATED BY '-'.

    APPEND gwa_final1 TO git_final1.  "SupplierOrganizationIDPart Data"
**End-Fill internal table for SupplierOrganizationIDPart.csv file "

    CLEAR: gwa_lfa1, gwa_final, gwa_final1.
  ENDLOOP.

** Prepare lines in required csv formate
  CLEAR gwa_file.
  LOOP AT git_final INTO gwa_final.
** Delete un-wanted additional spaces in all fields
    CONDENSE:
            gwa_final-taxid,
            gwa_final-stin,
            gwa_final-rtin,
            gwa_final-plun,
            gwa_final-lifnr,
            gwa_final-stceg,
            gwa_final-armax,
            gwa_final-armaxcu,
            gwa_final-armin,
            gwa_final-armincu,
            gwa_final-ort01,
            gwa_final-land1,
            gwa_final-stras,
            gwa_final-pstlz,
            gwa_final-regio,
            gwa_final-bukrs,
            gwa_final-smtp_addr,
            gwa_final-telfx,
            gwa_final-telf1,
            gwa_final-curl,
            gwa_final-hsr,
            gwa_final-htr,
            gwa_final-iscust,
            gwa_final-ismng,
            gwa_final-isorga,
            gwa_final-issup,
            gwa_final-name1,
            gwa_final-noe,
            gwa_final-orgtyp,
            gwa_final-pcun,
            gwa_final-stoi,
            gwa_final-yfound.

**  TaxID
    IF gwa_final-taxid NE space.
      CONCATENATE '"' gwa_final-taxid '"' INTO gwa_final-taxid.
    ENDIF.

**  SystemID
    IF gwa_final-lifnr NE space.
      CONCATENATE '"' gwa_final-lifnr '"' INTO gwa_final-lifnr.
    ENDIF.
**  CorporateAddress.UniqueName
    IF gwa_final-bukrs NE space.
      CONCATENATE '"' gwa_final-bukrs '"' INTO gwa_final-bukrs.
    ENDIF.
** Supplier Name
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

** PhoneNumber
    IF gwa_final-telf1 NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-telf1 WITH '""'.
      CONCATENATE '"' gwa_final-telf1 '"' INTO gwa_final-telf1.
    ENDIF.
**  ContactEmail
    IF gwa_final-smtp_addr NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-smtp_addr WITH '""'.
      CONCATENATE '"' gwa_final-smtp_addr '"' INTO gwa_final-smtp_addr.
    ENDIF.

**  FaxNumber
    IF gwa_final-telfx NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-telfx WITH '""'.
      CONCATENATE '"' gwa_final-telfx '"' INTO gwa_final-telfx.
    ENDIF.
** VAT Registration ID
    IF gwa_final-stceg NE space.
      REPLACE ALL OCCURRENCES OF '"' IN gwa_final-stceg WITH '""'.
      CONCATENATE '"' gwa_final-stceg '"' INTO gwa_final-stceg.
    ENDIF.
**Hard coded values enclosed with""
    CONCATENATE '"' gwa_final-hsr '"' INTO gwa_final-hsr.
    CONCATENATE '"' gwa_final-htr '"' INTO gwa_final-htr.
    CONCATENATE '"' gwa_final-iscust '"' INTO gwa_final-iscust.
    CONCATENATE '"' gwa_final-ismng '"' INTO gwa_final-ismng.
    CONCATENATE '"' gwa_final-isorga '"' INTO gwa_final-isorga.
    CONCATENATE '"' gwa_final-issup '"' INTO gwa_final-issup.

** Prepare comma(,) seperated rerod
    CLEAR gwa_file.
    CONCATENATE gwa_final-taxid
                gwa_final-stin
                gwa_final-rtin
                gwa_final-plun
                gwa_final-lifnr
                gwa_final-stceg
                gwa_final-armax
                gwa_final-armaxcu
                gwa_final-armin
                gwa_final-armincu
                gwa_final-ort01
                gwa_final-land1
                gwa_final-stras
                gwa_final-pstlz
                gwa_final-regio
                gwa_final-bukrs
                gwa_final-smtp_addr
                gwa_final-telfx
                gwa_final-telf1
                gwa_final-curl
                gwa_final-hsr
                gwa_final-htr
                gwa_final-iscust
                gwa_final-ismng
                gwa_final-isorga
                gwa_final-issup
                gwa_final-name1
                gwa_final-noe
                gwa_final-orgtyp
                gwa_final-pcun
                gwa_final-stoi
                gwa_final-yfound INTO gwa_file-line SEPARATED BY ','.
    APPEND gwa_file TO git_file.
    CLEAR gwa_final.

  ENDLOOP.

ENDFORM.                    " F_PREPARE_FILE
*&---------------------------------------------------------------------*
*&      Form  F_SEND_FILE
*&---------------------------------------------------------------------*
*       Send output CSV file
*----------------------------------------------------------------------*
FORM f_send_file USING p_val TYPE string .
  DATA: lv_fname  TYPE string,  "File name
        lv_ffname TYPE string,  "Full File name
        lv_msg    TYPE string,  "Error messages
        lv_name   TYPE string.      "File name key

  lv_name = p_val. "Read file name
**prepare file name
  CONCATENATE lv_name '.csv' INTO lv_fname.
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
      MESSAGE 'File open error' TYPE 'W'.
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
*&      Form  F_PREPARE_FILE2
*&---------------------------------------------------------------------*
*      prepare and send SupplierOrganizationIDPart file
*----------------------------------------------------------------------*

FORM f_prepare_file2 USING p_name TYPE string.

**Prepare header for SupplierOrganizationIDPart file
  DATA: lc_hdr  TYPE string VALUE '"UTF-8",,,', " UTF-8 header line "
        lv_file TYPE string.    " File Name
  lv_file = p_name. "File Name
  MOVE lc_hdr TO gwa_file-line.
  INSERT gwa_file INTO git_file INDEX 1.  " Inserting as first line in file
*  REFRESH git_file.
  CLEAR gwa_file.
  CONCATENATE 'Domain' 'Value' 'Parent.SystemID' INTO gwa_file-line SEPARATED BY ','.
  INSERT gwa_file INTO git_file INDEX 2. "Second line of the file

  LOOP AT git_final1 INTO gwa_final1.
    CLEAR gwa_file.
**    Enclosing "" for all fields
    CONCATENATE '"' gwa_final1-domain '"' INTO gwa_final1-domain.
    CONCATENATE '"' gwa_final1-value  '"' INTO gwa_final1-value.
    CONCATENATE '"' gwa_final1-psys   '"' INTO gwa_final1-psys.

**    Prepare Final output
    CONCATENATE gwa_final1-domain
                gwa_final1-value
                gwa_final1-psys
                INTO gwa_file-line SEPARATED BY ','.
    APPEND gwa_file TO git_file.

  ENDLOOP.

**  Send IDPart file
  PERFORM f_send_file USING lv_file.

ENDFORM.                    " F_PREPARE_FILE2


*&---------------------------------------------------------------------*
*&      Form  F_DIRECTORY_SELECTION
*&---------------------------------------------------------------------*
*      Select Directory for .CSV files download
*----------------------------------------------------------------------*

FORM f_directory_selection  CHANGING p_p_path TYPE localfile.
  DATA: lv_path TYPE string.

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
*    SORT git_zprog BY zexec_date zexec_time DESCENDING.  "(-) PANUSURI Ticket 87347
    SORT git_zprog BY zexec_date DESCENDING zexec_time DESCENDING. "(+) PANUSURI Ticket 87347
    READ TABLE git_zprog INTO gwa_zprog INDEX 1.
    gv_ldate = gwa_zprog-zexec_date.
    gv_ltime = gwa_zprog-zexec_time.
    CLEAR gwa_zprog.
  ENDIF.

ENDFORM.                    " F_GET_PROGRAM_PARMS

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

**Read change documents
  SORT git_lifnr BY objectid.  " Addition

  SELECT objectid changenr udate tcode
    FROM cdhdr INTO TABLE git_cdhdr
    FOR ALL ENTRIES IN git_lifnr
      WHERE objectclas EQ 'KRED' " Vendor Master Object class
      AND objectid EQ git_lifnr-objectid
      AND ( udate GE gv_ldate AND udate LE sy-datum )
      AND tcode IN gr_tcode.

  IF git_cdhdr[] IS NOT INITIAL.
    SORT git_cdhdr BY changenr.
    SELECT objectid changenr fname value_new FROM cdpos INTO CORRESPONDING FIELDS OF TABLE git_cdpos
      FOR ALL ENTRIES IN git_cdhdr
      WHERE objectclas EQ 'KRED'
        AND changenr EQ git_cdhdr-changenr
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
