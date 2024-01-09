*&---------------------------------------------------------------------*
*&  Include           ZLMMI020_GL_ACCOUNT_TOP
*&---------------------------------------------------------------------*

TABLES: ska1,      " GL Account General Master data
        skb1,      " GL Account Company data
        skat.      " GL Account chart of accounts descriptions

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF ty_ska1,
       ktopl LIKE ska1-ktopl,   " Chart of accounts
       saknr LIKE ska1-saknr,   " GL Account
       END OF ty_ska1.

TYPES:BEGIN OF ty_skat,
       ktopl LIKE ska1-ktopl,   " Chart of accounts
       saknr LIKE ska1-saknr,   " GL Account
       txt50 LIKE skat-txt50,   " Description of GL account
    END OF ty_skat.

TYPES: BEGIN OF ty_t004t,
      ktopl LIKE t004t-ktopl,  " Chart of accounts
      ktplt LIKE t004t-ktplt,  " Description of Chart of accounts
     END OF ty_t004t.

TYPES: BEGIN OF ty_final,
       saknr(30) TYPE c,   " GL Account
       bukrs(15) TYPE c,   " Company Code
       txt50(100) TYPE c,  " Description of GL account
       ktopl(20) TYPE c,   " Chart of accounts
       ktplt(100) TYPE c,  " Description of Chart of accounts
      END OF ty_final.

TYPES: BEGIN OF ty_file,
        line TYPE string,  " record in a file
       END OF ty_file.

TYPES: BEGIN OF ty_zprog, "Cutoum table data
       zclnt        TYPE mandt,
       zprog        TYPE zprog_name,      " Program Name
       zexec_date   TYPE zdate,           "Last Execution date
       zexec_time   TYPE ztime,           "Time of Execution
       END OF ty_zprog.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA: git_ska1 TYPE STANDARD TABLE OF ty_ska1, " Internal Table for structure ty_ska1
      gwa_ska1 TYPE ty_ska1,                   "  Work Area for git_ska1
      git_skat TYPE STANDARD TABLE OF ty_skat, " Internal Table for structure ty_skat
      gwa_skat TYPE ty_skat,                   "  Work Area for git_skat
      git_t004t TYPE STANDARD TABLE OF ty_t004t, " Internal Table for structure ty_t004t
      gwa_t004t TYPE ty_t004t,                   "  Work Area for git_t004t
      git_final TYPE STANDARD TABLE OF ty_final, " Internal Table for structure ty_final
      gwa_final TYPE ty_final,                   "  Work Area for git_final
      git_file TYPE STANDARD TABLE OF ty_file, " Internal Table for structure ty_skat
      gwa_file TYPE ty_file,                  "  Work Area for git_skat
      git_zprog TYPE STANDARD TABLE OF ty_zprog,"  Internal Table for structure ty_zprog
      gwa_zprog TYPE ty_zprog.                  " Work Area for git_zprog

************************************************************************
*                          Custom Data Types                           *
************************************************************************
DATA: gv_ldate TYPE sy-datum,  " Last execution date
      gv_ltime TYPE sy-uzeit,  " Last Execution time
      gv_path  TYPE string,    " Selected download path
      gv_flag.

CONSTANTS: gc_acnt TYPE string VALUE '_UG_Account_',
           gc_bunit(2) TYPE c VALUE 'UG',
           gc_chacc(4) TYPE c VALUE 'COAT',  " For UG system
           gc_ppath   TYPE localfile VALUE 'H:\my documents\',  " All .CSV files to Presentations server
           gc_fpath   TYPE localfile VALUE '/usr/sap/interfaces/D30/ARIBA/'.
*           gc_fpath   TYPE localfile VALUE '\\fifileserver.gtna.gt.ds\data\FI\DEV\Out\I_PTP_MM_020\'.
