*&---------------------------------------------------------------------*
*&  Include           ZLMMI026_SUPPLIER_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
*&---------------------------------------------------------------------*
*& Program Name       :  ZLMMI026_SUPPLIER                             *
*& Author             :  Sambasivarao Gopisetti                        *
*& Creation Date      :  February 05, 2015                             *
*& Object ID          :                                                *
*& Application Area   :  MM                                            *
*& Description        :  TOP Include - Outbound - Supplier Data        *
*&                       For Ariba Integration                         *
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

TABLES : lfa1,         " Vendor Master general data
         lfb1,         " Vendor Master Company data
         lfm1.         " Vendor Master Purchase organisation data

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:BEGIN OF ty_lfa1,
      lifnr TYPE lfa1-lifnr,   "SupplierId
      name1 TYPE lfa1-name1,   "SupplierName
      stras TYPE lfa1-stras,   "StreetAddress
      ort01 TYPE lfa1-ort01,   " City
      regio TYPE lfa1-regio,   " State
      land1 TYPE lfa1-land1,   " Country
      pstlz TYPE lfa1-pstlz,   " Postal Code
      ktokk TYPE lfa1-ktokk,   " Supplier type
      brsch TYPE lfa1-brsch,   " ContactLastName
      telf1 TYPE lfa1-telf1,   " Telephone Number
      spras TYPE lfa1-spras,   " Preffered Language
      telfx TYPE lfa1-telfx,   " Fax Number
      emnfr TYPE lfa1-emnfr,   " Ariba Number
      erdat TYPE lfa1-erdat,   " Creation date
      adrnr TYPE lfa1-adrnr,   " Address Number
      END OF ty_lfa1.

TYPES: BEGIN OF ty_lfb1,
      lifnr TYPE lfb1-lifnr,
      bukrs TYPE lfb1-bukrs,   " LocationID/Company code
      zwels TYPE lfb1-zwels,   " Payment type
      END OF ty_lfb1.

**Structure for vendor account group description
TYPES: BEGIN OF ty_t077y,
       ktokk TYPE t077y-ktokk, " Vendor account group
       desc  TYPE t077y-txt30, " Description for account group
      END OF ty_t077y.

**  Language description
TYPES: BEGIN OF ty_t002,
        spras TYPE t002-spras, " Language key
        sprsl TYPE t002t-sprsl, " Langaukey in t002t
        laiso TYPE t002-laiso, " ISO Language Key
        sptxt TYPE t002t-sptxt, " Lang .Description
       END OF ty_t002.

TYPES:BEGIN OF ty_adr6,
      addrnumber TYPE adr6-addrnumber, " Address Nubmer
      smtp_addr   TYPE adr6-smtp_addr, " Email address
      END OF ty_adr6.

TYPES :BEGIN OF ty_lfm1,
       lifnr TYPE lfa1-lifnr,   "SupplierId
       verkf TYPE lfm1-verkf,   " ContactFirstName
      END OF ty_lfm1.

TYPES:BEGIN OF ty_fmfg,
      lifnr TYPE lfa1-lifnr,   "SupplierId
      duns  TYPE fmfg_lfaccr-duns, "DUNS Number
      END OF ty_fmfg.

**Finla table structure
TYPES:BEGIN OF ty_final,
      lifnr(20) TYPE c,    "SupplierId
      bukrs(20) TYPE c,    "SupplierLocationId
      name1(100) TYPE c,    "SupplierName
      stras(100) TYPE c,    "StreetAddress
      ort01(35) TYPE c,    "City
      regio(20) TYPE c,    "State
      land1(20) TYPE c,    "Country
      pstlz(20) TYPE c,    "PostalCode
      ktokk(40) TYPE c,    "SupplierType
      verkf(35) TYPE c,    "ContactFirstName
      brsch(35) TYPE c,    "ContactLastName
      telf1(35) TYPE c,    "ContactPhoneNumber
      smtp_addr(200) TYPE c, "ContactEmail
      spras(20) TYPE c,    "PreferredLanguage
      telfx(35) TYPE c,    "FaxNumber
      ordtyp(20) TYPE c,   "OrderRoutingType
      duns(12)  TYPE c,    "DUNSNumber
      emnfr(12) TYPE c,    "ANNumber
      zwels(20) TYPE c,    "PaymentType
      dsity(30)  TYPE c,   "Diversity
      mowned(30) TYPE c,   "MinorityOwned
      wowned(30) TYPE c,   "WomanOwned
      vowned(30) TYPE c,   "VeteranOwned
      dsba8a(30) TYPE c,   "DiversitySBA8A
      dhzone(30) TYPE c,   "DiversityHUBZone
      dstb(30)   TYPE c,   "DiversitySDB
      ddvo(30)   TYPE c,   "DiversityDVO
      decity(30) TYPE c,   "DiversityEthnicity
      dgo(30)    TYPE c,   "DiversityGLBTOwned
      ddo(30)    TYPE c,   "DiversityDisabledOwned
      dls(30)    TYPE c,   "DiversityLaborSurplus
      dhbcu(30)  TYPE c,   "DiversityHBCU
      dsb(30)    TYPE c,   "DiversitySmallBusiness
      dg(30)     TYPE c,   "DiversityGreen
      cd(30)     TYPE c,   "CertifiedDiversity
      cmo(30)    TYPE c,   "CertifiedMinorityOwned
      cwo(30)    TYPE c,   "CertifiedWomanOwned
      cvo(30)    TYPE c,   "CertifiedVeteranOwned
      csba8a(30) TYPE c,   "CertifiedSBA8A
      chzone(30) TYPE c,   "CertifiedHUBZone
      csdb(30)   TYPE c,   "CertifiedSDB
      ce(30)     TYPE c,   "CertifiedEthnicity
      cdo(30)    TYPE c,   "CertifiedDisabledOwned
      cdis(30)   TYPE c,   "CertifiedDisadvantaged
      dep(30)    TYPE c,   "DiversityEnterprise
      moe(30)    TYPE c,   "MinorityOwnedEnterpris
      woe(30)    TYPE c,   "WomanOwnedEnterprise
      voe(30)    TYPE c,   "VeteranOwnedEnterprise
      dvoe(30)   TYPE c,   "DVOEnterprise
      ee(30)     TYPE c,   "EthnicityEnterprise
      dent(30)   TYPE c,   "DisadvantagedEnterprise
      noe(30)    TYPE c,   "NumberOfEmployees
      ff1(40)    TYPE c,   "FlexField1
      ff2(40)    TYPE c,   "FlexField2
      ff3(40)    TYPE c,   "FlexField3
     END OF ty_final.

TYPES: BEGIN OF ty_file,
       line TYPE string,  "record in a file
       END OF ty_file.

TYPES: BEGIN OF ty_zprog, "Cutoum table data
       zclnt        TYPE mandt,
       zprog        TYPE zprog_name,      " Program Name
       zexec_date   TYPE zdate,           "Last Execution date
       zexec_time   TYPE ztime,           "Time of Execution
       END OF ty_zprog.

** CDPOS structure
TYPES: BEGIN OF ty_cdpos,
       objectid LIKE cdpos-objectid,
       lifnr    LIKE lfa1-lifnr,
       changenr LIKE cdpos-changenr,
       fname    LIKE cdpos-fname,
       value_new LIKE cdpos-value_new,
       END OF ty_cdpos.
** CDHDR structure
TYPES: BEGIN OF ty_cdhdr,
       objectid LIKE cdhdr-objectid,
       changenr LIKE cdhdr-changenr,
       udate    LIKE cdhdr-udate,
       tcode    LIKE cdhdr-tcode,
       END OF ty_cdhdr.


************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA: git_lfa1 TYPE STANDARD TABLE OF ty_lfa1, " Internal Table for structure ty_lfa1
      gwa_lfa1 TYPE ty_lfa1,                   "  Work Area for git_lfa1
      git_lfb1 TYPE STANDARD TABLE OF ty_lfb1, " Internal Table for structure ty_lfb1
      gwa_lfb1 TYPE ty_lfb1,                   " Work Area for git_lfb1
      git_adr6 TYPE STANDARD TABLE OF ty_adr6, " Internal Table for structure ty_adr6
      gwa_adr6 TYPE ty_adr6,                   " Work Area for git_adr6
      git_final TYPE STANDARD TABLE OF ty_final, "Internal Table for structure ty_final
      gwa_final TYPE ty_final,                 " Work Area for git_final
      git_file TYPE STANDARD TABLE OF ty_file, " Internal Table for structure ty_file
      gwa_file TYPE ty_file,                   " Work Area for git_file
      git_lfm1 TYPE STANDARD TABLE OF ty_lfm1, " Internal Table for structure ty_lfm1
      gwa_lfm1 TYPE ty_lfm1,                   " Work Area git_lfm1
      git_fmfg TYPE STANDARD TABLE OF ty_fmfg, " Internal Table for structure ty_fmfg
      gwa_fmfg TYPE ty_fmfg,                   " Work Area for git_fmfg
      git_zprog TYPE STANDARD TABLE OF ty_zprog," Internal Table for structure ty_zprog
      gwa_zprog TYPE ty_zprog,                  " Work Area for git_zprog
      git_t077y TYPE STANDARD TABLE OF ty_t077y," Internal Table for structure ty_t077y
      gwa_t077y TYPE ty_t077y,                  " Work Area for git_t077y
      git_t002 TYPE STANDARD TABLE OF ty_t002,  " Internal Table for structure ty_t002
      gwa_t002 TYPE ty_t002,                    " Work Area for git_t002
      git_t002t TYPE STANDARD TABLE OF ty_t002, " Internal Table for structure ty_t002
      gwa_t002t TYPE ty_t002,                    " Work Area for git_t002
      git_cdhdr TYPE STANDARD TABLE OF ty_cdhdr, " Internal Table for structure ty_cdhdr
      gwa_chhdr LIKE LINE OF git_cdhdr,          " Work Area for git_cdhdr
      git_cdpos TYPE STANDARD TABLE OF ty_cdpos, " Internal Table for structure ty_cdpos
      gwa_cdpos LIKE LINE OF git_cdpos.           " Work Area for git_cdpos

************************************************************************
*                          Custom Data Types                           *
************************************************************************
DATA: gv_ldate TYPE sy-datum,  " Last execution date
      gv_ltime TYPE sy-uzeit,  " Last Execution time
      gv_path  TYPE string,    " Selected download path
      gv_flag  TYPE c.         " File sent status flag

************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS: gc_x      TYPE c VALUE 'X',  " value X
           gc_act    TYPE string VALUE '_UG_Supplier_',
           gc_inact  TYPE string VALUE '_UG_Supplier_Delete_',
           gc_bunit(2) TYPE c VALUE 'UG',
           gc_ppath   TYPE localfile VALUE 'H:\my documents\',  " All .CSV files to Presentations server
           gc_fpath   TYPE localfile VALUE '/usr/sap/interfaces/D30/ARIBA/'.
*           gc_fpath   TYPE localfile VALUE '\\fifileserver.gtna.gt.ds\data\FI\DEV\Out\I_PTP_MM_020\'.

************************************************************************
*                             Ranges                                   *
************************************************************************
RANGES: gr_actsupp FOR lfa1-lifnr,  "Range Table for Vendors
        gr_tcode   FOR cdhdr-tcode,  " T-codes
        gr_fname   FOR cdpos-fname.  " Field Names
