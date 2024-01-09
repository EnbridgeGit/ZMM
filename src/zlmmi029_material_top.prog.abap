*&---------------------------------------------------------------------*
*&  Include           ZLMMI029_MATERIAL_TOP
*&---------------------------------------------------------------------*


TABLES: mara, marc, mbew.

TYPES: BEGIN OF ty_mara,
        matnr TYPE mara-matnr,  " Material
        mfrnr TYPE mara-mfrnr,  " Manufacturer
        mfrpn TYPE mara-mfrpn,  " Manufacturer part number
        przus TYPE mara-przus,  " MaterialComposition
       END OF ty_mara.

TYPES: BEGIN OF ty_mbew,
       matnr TYPE mbew-matnr, " material
       bwkey TYPE mbew-bwkey, " Valution Area
       verpr TYPE mbew-verpr, " Moving Avg Price
       zkdat TYPE mbew-zkdat, " Effective from date
       END OF ty_mbew.

TYPES: BEGIN OF ty_t001,
       bwkey TYPE t001k-bwkey,  " Valution Area
       bukrs TYPE t001k-bukrs,  " company code
       waers TYPE t001-waers,   " Currency Code
      END OF ty_t001.

TYPES: BEGIN OF ty_makt,
        matnr TYPE mara-matnr,
        maktx TYPE makt-maktx, " Material Description1
        maktg TYPE makt-maktg, " Material Description2
      END OF ty_makt.

TYPES: BEGIN OF ty_marc,
        matnr TYPE mara-matnr, " Material
        werks TYPE marc-werks, " Plant
        plifz TYPE marc-plifz, "LeadTimeInDays
        herkl TYPE marc-herkl, "Origin
      END OF ty_marc.

TYPES: BEGIN OF ty_final,
       matnr(30),
       bunit(10),
       maktx(100),
       maktg(100),
       stdcost(18),
       stdcurr(10),
       stdcdat(15),
       stk_ind,
       mfrnr(100),
       mfrpn(100),
       plifz(30),
       ff1,
       ff2,
       ff3,
       pf1,
       pf2,
       pf3,
       herkl(20),
       przus(40),
       fcu1,
       fcu2,
       fcu3,
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
DATA: git_mara TYPE STANDARD TABLE OF ty_mara, " Internal Table for structure ty_mara
      gwa_mara TYPE ty_mara,                   "  Work Area for git_mara
      git_mbew TYPE STANDARD TABLE OF ty_mbew, " Internal Table for structure ty_mbew
      gwa_mbew TYPE ty_mbew,                   "  Work Area for git_mbew
      git_t001 TYPE STANDARD TABLE OF ty_t001, " Internal Table for structure ty_t001
      gwa_t001 TYPE ty_t001,                   "  Work Area for git_t001
      git_t001k TYPE STANDARD TABLE OF ty_t001, " Internal Table for structure ty_t001
      gwa_t001k TYPE ty_t001,                  "  Work Area for git_t001k
      git_marc TYPE STANDARD TABLE OF ty_marc, " Internal Table for structure ty_marc
      gwa_marc TYPE ty_marc,                   "  Work Area for git_marc
      git_makt TYPE STANDARD TABLE OF ty_makt, " Internal Table for structure ty_makt
      gwa_makt TYPE ty_makt,                   "  Work Area for git_makt
      git_final TYPE STANDARD TABLE OF ty_final, " Internal Table for structure ty_final
      gwa_final TYPE ty_final,                   "  Work Area for git_final
      git_file TYPE STANDARD TABLE OF ty_file, " Internal Table for structure ty_skat
      gwa_file TYPE ty_file,                   "  Work Area for git_skat
      git_zprog TYPE STANDARD TABLE OF ty_zprog,"  Internal Table for structure ty_zprog
      gwa_zprog TYPE ty_zprog.                  " Work Area for git_zprog

************************************************************************
*                          Custom Data Types                           *
************************************************************************
DATA: gv_ldate TYPE sy-datum,  " Last execution date
      gv_ltime TYPE sy-uzeit,  " Last Execution time
      gv_path  TYPE string,    " Selected download path
      gv_flag.

CONSTANTS: gc_part TYPE string VALUE '_UG_Part_',  " File Name string
           gc_bunit(2) TYPE c VALUE 'UG',       " Business Unit
           gc_ppath   TYPE localfile VALUE 'H:\my documents\',  " All .CSV files to Presentations server
           gc_fpath   TYPE localfile VALUE '/usr/sap/interfaces/D30/ARIBA/'.
*           gc_fpath   TYPE localfile VALUE '\\fifileserver.gtna.gt.ds\data\FI\DEV\Out\I_PTP_MM_020\'.
