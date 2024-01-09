REPORT ZMMMI023 MESSAGE-ID ZS line-count 65 line-size 250.

************************************************************************
*  Author:    M L De Meester.
*
*  Date:      July 2009..
*
*  Issue Log: TR726
*
*  Description:
*    The purpose of this program is to extract material/plant/vendor
*    information for the Vendor Relationship program..
*    This abap is for both the East and the West.
*
*    The client will execute this abap, filling in the appropriate
*    values in the variant.
*
*    Since this program is used both in the EAST and the WEST, changes
*    should be applied to both.
*
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
* CHANGES                                                              *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
* NOTE:  Differences between EAST & WEST - to find all the differences
*        do a find on DIFFERENCES
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
*                EAST              |           WEST
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
*                                  |
*                                  |
*                                  |
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
* ...
*
************************************************************************
tables:  MARA,                  "Material Master
         MARC,                  "Plant
         MARD,                  "Storage Location
         MAKT,                  "Material Description
         cabn,
         ausp,
         klah,                  "Class name
         kssk,                  "Class with material
         t001w,                 "Plant Name
         t001k,                 "Xref plant/company code
         t001l,                 "StorageLocation Name
         t001,                  "Company name
         t023t,                 "Material group description
         t025t,                 "ValuationClass description
*         mver,                  "Consumption
         mseg,                  "Consumption at s/loc
         mbew,                  "AVG Price table
         lfa1,                  "VendorName
         eina.                  "Info Record



data: inrec(400).
data: OUTREC(2000).


data: begin of wamat     occurs 0,
      matnr    like mara-matnr,          "01-Material Number
      src(4)   type c,                   "02-Data Source EAST/WEST
      class    like klah-class,          "03-Class Name
      char1    like ausp-atwrt,          "04-Keyword
      matkl(29) type c, "like mara-matkl,"05-Material Group
      char2(80) type c,                  "06-Primary/Secondary Desc
      maktx    like makt-maktx,          "07-Material Description
      meins    like mara-meins,          "08-Material UOM
      char4(60)  type c,                 "09-Manufacturer Name
      char5(80)  type c,                 "10-Manufacturer PartNumber
      char6(120) type c,                 "11-Model#
      mtart    like mara-mtart,          "12-Material Type
      lvorm    like mara-lvorm,          "13-Flag for Deletion
      mstae    like mara-mstae,          "14-Cross-plant Status
      NA15(3),                           "15-Critical Spare Ind
      NA16(3),                           "16-Equipment
      NA17(3),                           "17-Catalog #
      filler1(1) type c,
      end of wamat.

data: begin of waplt      occurs 0,
      matnr     like mara-matnr,         "01-Material Number
      werks     like marc-werks,         "23-Plant
      lgort(30) type c,                  "24-Storage Location
      bukrs(30) type c,                  "22-Company Code_Name
      name1     like t001w-name1,        "25-Plant Name
      lvorm     like marc-lvorm,         "26-Flag for Deletion
      mmsta     like marc-mmsta,         "27-PlantSp Status
      lgpbe     like mard-lgpbe,         "28-BinLocation
      labst(10) type c,  "like mard-labst"29-QOH
      verpr(12) type c,c,                "30-Average Unit Price
      dismm     like marc-dismm,          "31-MRP type
      eisbe(10) type c,  "like marc-eisbe,"32-Safety level
      minbe(10) type c,  "like marc-minbe "33-Minimum level
      mabst(10) type c,  "like marc-mabst "34-Maximum level
      gjahr     like mver-gjahr,          "37-FiscalYr on Query
      erfmg_0(12) type c,                 "38-ytd
      erfmg_1(12) type c,                 "39-YTD for p_gjahr-1
      erfmg_2(12) type c,                 "40-YTD for p_gjahr-2
      erfmg_3(12) type c,                 "41-YTD for p_gjahr-3
      bklas(30) type c,                   "35-Valuation Class
      plifz(10) type c,                   "36 PltDelivTime
      filler1(1) type c,
      end of waplt.

data: begin of wavndr   occurs 0,
      matnr    like mara-matnr,           "01-Material Number
      vdrnm(30) type c,                   "18-Vendor#/Name
      idnlf     like eina-idnlf,          "19-Vendor PartNumber
      infnr     like eina-infnr,          "20-Info record #
      loekz     like eina-loekz,          "21-Info Record Status
      filler1(1) type c,
      end of wavndr.


data: wa_mjahr  like mseg-mjahr,
      wa_mjahr_1 like mseg-mjahr,
      wa_mjahr_2 like mseg-mjahr,
      wa_mjahr_3 like mseg-mjahr.

data: walng1 type i,
      walng2 type i,
      walng3 type i.


data: begin of waconsump    occurs 0,
      matnr   like mseg-matnr,
      werks   like mseg-werks,
      lgort   like mseg-lgort,
      erfmg_0 like mseg-erfmg,
      erfmg_1 like mseg-erfmg,
      erfmg_2 like mseg-erfmg,
      erfmg_3 like mseg-erfmg,

      end of waconsump.

data:  wa_outfile(300)        type c.

* Input file name with path
data: infile(70).
* Output file name with path
DATA: OUTFILE(70).

*-----------------------------------------------------------------------

*=======================================================================
* Selection Screen
*=======================================================================
selection-screen begin of block BOX1 with frame title text-102.
parameter:
    p_fl1 like filename-fileextern obligatory default
                '/usr/sap/interfaces/P01/CFMM001/material.csv',
    p_fl2 like filename-fileextern obligatory default
                '/usr/sap/interfaces/P01/CFMM001/plant.csv',
    p_fl3 like filename-fileextern obligatory default
                '/usr/sap/interfaces/P01/CFMM001/inforec.csv'.

selection-screen end of block BOX1.

selection-screen skip.

selection-screen begin of block BOX2 with frame title text-103.
parameter:
    p_src(4) type c obligatory.
select-options:
    s_werks      for marc-werks,
    s_lgort      for mard-lgort.
selection-screen end of block BOX2.

selection-screen skip.

selection-screen begin of block BOX3 with frame title text-100.
select-options:
    s_mtart      for mara-mtart     no intervals,
    s_matkl      for mara-matkl,
    s_matnr      for mara-matnr,
    s_bwart      for mseg-bwart,
    s_mstae      for mara-mstae,
    s_mmsta      for marc-mmsta.

selection-screen end of block BOX3.

selection-screen skip.

selection-screen begin of block BOX4 with frame title text-104.
parameter:
    p_mjahr      like cosp-gjahr    obligatory default sy-datum(4).
select-options:
    s_cpudt      for sy-datum obligatory.
selection-screen end of block BOX4.

selection-screen skip.

selection-screen begin of block BOX5 with frame title text-101.
select-options:
    s_atnam       for cabn-atnam      no intervals,
    s_atinn      for cabn-atinn      no intervals.
selection-screen end of block BOX5.



*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================


*=======================================================================
*     Start of Main Processing Block
*=======================================================================
INITIALIZATION.
   clear s_cpudt.
   move 'IBT' to s_cpudt+0(3).
   move sy-datum to s_cpudt+3(8).
   compute s_cpudt+3(4) = s_cpudt+3(4) - 3.
   move '01' to: s_cpudt+7(2), s_cpudt+9(2).
   move sy-datum to s_cpudt+11(8).
   append s_cpudt.

START-OF-SELECTION.
   perform get_characteristic_objectid.
   perform open_files.
   perform get_plant_consumption.
   perform get_material_information.

   write: /1 'ZMMMI023 has completed.  Contact SAP for file transfer'.

   close dataset: p_fl1, p_fl2, p_fl3.
*
END-OF-SELECTION.



*==========================  OPEN_FILE =================================
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*=======================================================================
FORM OPEN_FILES.

  DATA: MSG(100).
*-----------------------------------------------------------------------
  move sy-sysid to: p_fl1+20(3), p_fl2+20(3), p_fl3+20(3).

  OPEN DATASET P_FL1 FOR OUTPUT IN TEXT MODE MESSAGE MSG.
  OPEN DATASET P_FL2 FOR OUTPUT IN TEXT MODE MESSAGE MSG.
  OPEN DATASET P_FL3 FOR OUTPUT IN TEXT MODE MESSAGE MSG.
  if ( sy-subrc <> 0 ).
    message E002 with outFILE msg.
  endif.

  compute wa_mjahr   = p_mjahr - 0.
  compute wa_mjahr_1 = p_mjahr - 1.
  compute wa_mjahr_2 = p_mjahr - 2.
  compute wa_mjahr_3 = p_mjahr - 3.

* Headings File 1 - Material

  concatenate
   '(1)'  '(2)'  '(3)' '(4)'  '(5)'  '(6)'  '(7)'  '(8)'  '(9)'
   '(10)' '(11)' '(12)' '(13)' '(14)' '(15)' '(16)' '(17)' ' '
                                 into outrec separated by ','.
  walng1 = strlen( outrec ).
  transfer outrec to P_FL1 length walng1.

  concatenate
      'Material#'         'Data Source'       'Classification'
      'Keyword'           'Material Group'    'Long Description'
      'Short Description' 'UOM'               'Manufacturer Name'
      'Mfg Part Number'   'Model Number'      'Material Type'
      'Flag for Deletion' 'CrossPlant Stat'   'N/A'
      'N/A'               'N/A' ' ' into outrec separated by ','.
  walng1 = strlen( outrec ).
  transfer outrec to P_FL1 length walng1.

* Headings File 2 - Plant

  concatenate
   '(01)' '(22)' '(23)' '(24)' '(25)' '(26)' '(27)' '(28)' '(29)' '(30)'
   '(31)' '(32)' '(33)' '(34)' '(35)' '(36)' '(37)' '(38)' '(39)' '(40)'
   '(41)' ' '                              into outrec separated by ','.
  walng2 = strlen( outrec ).
  transfer outrec to P_FL2 length walng2.

  concatenate
     'Material#'         'Company Code'       'Plant'
     'Storage Location'  'Plant Description'  'Flag for Deletion'
     'PlantSp Status'    'BIN Location'       'QOH'
     'AvgUnitPrice'      'MRP Type'           'Safety Level'
     'Min'               'Max'
     'ValuationClass'    'PlanDelTime'         'CurrFiscalYr'
     'YTD Consump'       'Year-1 Consump'     'Year-2 Consump'
     'Year-3 Consump'    ' '    into outrec separated by ','.
  walng2 = strlen( outrec ).
  transfer outrec to P_FL2 length walng2.

* Headings File 3 - Info Record

  concatenate  '(01)' '(18)' '(19)' '(20)'  '(21)'  ' '
                                 into outrec separated by ','.
  transfer outrec to P_FL3 length walng3.
  walng3 = strlen( outrec ).

  concatenate  'Material#'  'Vendor Name'  'VendorPart#'
               'InfoRec#'  'Status'   ' '
                                  into outrec separated by ','.
  walng3 = strlen( outrec ).
  transfer outrec to P_FL3 length walng3.

ENDFORM.

FORM GET_MATERIAL_INFORMATION.
  select * from mara
      where matnr in s_matnr
        and matkl in s_matkl
        and mtart in s_mtart
        and mstae in s_mstae.

      clear: wamat.
      move p_src              to wamat-src.
      move 'N/A' to: wamat-na15, wamat-na16, wamat-na17.
      move-corresponding mara to wamat.
      write mara-meins        to wamat-meins. "Translate UOM

*     material group description

      select single * from t023t
          where spras = sy-langu
            and matkl = mara-matkl.
      if sy-subrc = '0'.
         concatenate mara-matkl t023t-wgbez into wamat-matkl
                         separated by '_'.
      endif.

*     description
      select single * from makt
        where matnr = wamat-matnr
          and spras = sy-langu.
        if sy-subrc = '0'.
           move makt-maktx to wamat-maktx.
           replace all occurrences of ',' in wamat-maktx with '_'.
           replace all occurrences of '"' in wamat-maktx with ' '.
        endif.
*       class
      select single * from kssk
         where objek = wamat-matnr
           and klart = '001'
           and mafid = 'O'.
        if sy-subrc = '0'.
           select single * from klah
               where clint = kssk-clint
                 and klart = kssk-klart.
           if sy-subrc = '0'.
              move klah-class to wamat-class.
           endif.
        endif.
       perform get_characteristics.

   concatenate  wamat-matnr  wamat-src   wamat-class
                wamat-char1  wamat-matkl wamat-char2
                wamat-maktx  wamat-meins wamat-char4
                wamat-char5  wamat-char6 wamat-mtart
                wamat-lvorm  wamat-mstae wamat-na15
                wamat-na16   wamat-na17  wamat-filler1
                into outrec separated by ','.
    walng1 = strlen( outrec ).
    transfer outrec to p_fl1 length walng1.

*  For each material, get all INFO RECORDS.
*  Some materials may not have INFO Records.

   perform get_info_record_info.

*  For each material, get all PLANT/STORAGE records.
   perform get_plant_information.


endselect.   "End of material

endform.

form get_characteristic_objectid.
  refresh s_atinn.
  loop at s_atnam.
    select single * from cabn
      where atnam = s_atnam+3(30).
      if sy-subrc = '0'.
        move cabn-atinn to s_atinn+3(10).
        move 'IEQ'      to s_atinn+0(3).
        append s_atinn.
      endif.
  endloop.


endform.

form get_characteristics.
   select * from ausp
     where atinn in s_atinn
       and mafid = 'O'
       and klart = '001'
       and objek = wamat-matnr.
     loop at s_atinn.
       case sy-tabix.
         when '1'.          "Keyword
          if s_atinn+3(10) = ausp-atinn.
             move ausp-atwrt to wamat-char1.
             replace all occurrences of ',' in wamat-char1 with '_'.
             replace all occurrences of '"' in wamat-char1 with ' '.
          endif.
         when '2'.          "Primary Description
          if s_atinn+3(10) = ausp-atinn.
             move ausp-atwrt to wamat-char2.
             replace all occurrences of ',' in wamat-char2 with '_'.
             replace all occurrences of '"' in wamat-char2 with ' '.
          endif.
         when '3'.          "Secondary Description appended to Primary
          if s_atinn+3(10) = ausp-atinn.
             concatenate wamat-char2 ausp-atwrt into wamat-char2
separated by '_'.
             replace all occurrences of ',' in wamat-char2 with '_'.
             replace all occurrences of '"' in wamat-char2 with ' '.
          endif.
         when '4'.          "Model Number
          if s_atinn+3(10) = ausp-atinn.
             replace all occurrences of ',' in ausp-atwrt with '_'.
             replace all occurrences of '"' in ausp-atwrt with '_'.
             if wamat-char6 = ' '.
                move ausp-atwrt to wamat-char6.
             else.
                concatenate wamat-char6 ausp-atwrt into wamat-char6
                                                       separated by '^'.
             endif.
          endif.
         when '5'.          "Manufacturer Part Number - multiples
          if s_atinn+3(10) = ausp-atinn.
             replace all occurrences of ',' in ausp-atwrt with '_'.
             replace all occurrences of '"' in ausp-atwrt with '_'.
             if wamat-char5 = ' '.
                move ausp-atwrt to wamat-char5.
             else.
                concatenate wamat-char5 ausp-atwrt into wamat-char5
                                                       separated by '^'.
             endif.
           endif.
          when '6'.          "Manufacturer Name
          if s_atinn+3(10) = ausp-atinn.
             replace all occurrences of ',' in ausp-atwrt with '_'.
             replace all occurrences of '"' in ausp-atwrt with '_'.
             if wamat-char4 = ' '.
                move ausp-atwrt to wamat-char4.
             else.
                concatenate wamat-char4 ausp-atwrt into wamat-char4
                                                       separated by '^'.
             endif.
          endif.
       endcase.
     endloop.

   endselect.


endform.

form get_plant_information.
    select * from marc
        where werks in s_werks
          and matnr = wamat-matnr
          and mmsta in s_mmsta.
        move p_mjahr             to waplt-gjahr.
*
*       fields being moved: werks, lvorm, mmsta, dismm,
        move-corresponding marc to waplt.
*
        move marc-eisbe to waplt-eisbe.  "Safety Level
        move marc-minbe to waplt-minbe.  "Minimum Level
        move marc-mabst to waplt-mabst.  "Maximum level
        move marc-plifz to waplt-plifz.  "PlanDeltTime
        perform get_company_code_name.
        perform get_plant_name.
        perform get_average_unitprice.
        move p_mjahr to wa_mjahr.
*
*       Determine if there are any storage locations
*       by selecting any one plant for the material.
*       If no storage locations, then write the plant record;
*       otherwise, write each storage location record.
*
        select single * from MARD
          where werks = marc-werks
            and matnr = marc-matnr.
        if sy-subrc = '0'.
           select * from MARD
             where werks = marc-werks
               and matnr = marc-matnr.
               perform storage_location_info.
           endselect.
        else.
           clear: waplt-lgort,
                  waplt-erfmg_0,  waplt-erfmg_1,
                  waplt-erfmg_2,  waplt-erfmg_3,
                  waplt-dismm,    waplt-eisbe,
                  waplt-minbe,    waplt-mabst,
                  waplt-bklas,    waplt-plifz,
                  waplt-labst,    waplt-lgpbe.
           perform write_plt.
        endif.
   endselect.
endform.

form storage_location_info.
   clear:  waplt-erfmg_0, waplt-erfmg_1,
           waplt-erfmg_2, waplt-erfmg_3.

   move mard-lgpbe to waplt-lgpbe.
   move mard-lgort to waplt-lgort.
   move mard-labst to waplt-labst.
   if mard-lgort = 'A001'.
   else.
     clear waplt-dismm.
   endif.

   select single * from t001l
      where werks = mard-werks
        and lgort = mard-lgort.
    if sy-subrc = '0'.
       concatenate waplt-lgort t001l-lgobe into waplt-lgort
                                           separated by '_'.
    endif.

*   get consumption for material/plant/storagelocation
*   already calculated and stored in waconsump interial table

    read table waconsump with key matnr = mard-matnr
                                  werks = mard-werks
                                  lgort = mard-lgort.
    if sy-subrc = '0'.
       move waconsump-erfmg_0 to waplt-erfmg_0.
       move waconsump-erfmg_1 to waplt-erfmg_1.
       move waconsump-erfmg_2 to waplt-erfmg_2.
       move waconsump-erfmg_3 to waplt-erfmg_3.
    endif.

    perform write_plt.

endform.

form get_company_code_name.
  clear waplt-bukrs.
  select single * from t001k
     where bwkey = marc-werks.
     if sy-subrc = '0'.
        move t001k-bukrs to waplt-bukrs.
        select single * from T001
           where bukrs = t001k-bukrs.
           if sy-subrc = '0'.
              concatenate waplt-bukrs t001-butxt into waplt-bukrs
                 separated by '_'.
           endif.
      endif.
endform.

form get_plant_name.
  select single * from t001w
     where werks = marc-werks.
  if sy-subrc = '0'.
     move t001w-name1 to waplt-name1.
  endif.
endform.



form get_average_unitprice.
  clear waplt-verpr.
  select single * from mbew
    where matnr = marc-matnr
      and bwkey = marc-werks.
   if sy-subrc = '0'.
      move mbew-verpr to waplt-verpr.
*     BKLAS with description
      select single * from t025T
        where spras = sy-langu
          and bklas = mbew-bklas.
      concatenate mbew-bklas t025t-bkbez into waplt-bklas
                                       separated by '_'.
   endif.
endform.

form get_info_record_info.
    select * from eina
       where matnr = wamat-matnr.

       move-corresponding eina to wavndr.
       replace all occurrences of ',' in wavndr-idnlf with '_'.
       replace all occurrences of '"' in wavndr-idnlf with ' '.
        select single * from lfa1
          where lifnr = eina-lifnr.
       if sy-subrc = '0'.
          concatenate eina-lifnr lfa1-name1 into wavndr-vdrnm
               separated by '_'.
          replace all occurrences of ',' in wavndr-vdrnm with '_'.
          replace all occurrences of '"' in wavndr-vdrnm with ' '.
       endif.
       concatenate '_' wavndr-idnlf into wavndr-idnlf.
       concatenate wavndr-matnr wavndr-vdrnm wavndr-idnlf
                   wavndr-infnr wavndr-loekz wavndr-filler1
                   into outrec separated by ','.
       walng3 = strlen( outrec ).

       transfer outrec to p_fl3 length walng3.
    endselect.

   endform.

form get_plant_consumption.

  select * from mseg
     where matnr in s_matnr
       and werks in s_werks
       and lgort in s_lgort
       and bwart in s_bwart
       and sobkz = ' '
       and mjahr in (wa_mjahr, wa_mjahr_1, wa_mjahr_2, wa_mjahr_3 )
       and lgort <> ' '.

     clear waconsump.
     move-corresponding mseg to waconsump.
     if mseg-shkzg = 'H'.
        compute mseg-erfmg = mseg-erfmg * -1.
     endif.
     case mseg-mjahr.
       when wa_mjahr.
         move mseg-erfmg to waconsump-erfmg_0.
       when wa_mjahr_1.
         move mseg-erfmg to waconsump-erfmg_1.
       when wa_mjahr_2.
         move mseg-erfmg to waconsump-erfmg_2.
       when wa_mjahr_3.
         move mseg-erfmg to waconsump-erfmg_3.
     endcase.
     collect waconsump.  "sum consumption by year
  endselect.

  sort waconsump by matnr werks lgort.

endform.

form write_plt.
  concatenate waplt-matnr  waplt-bukrs waplt-werks waplt-lgort
                   waplt-name1   waplt-lvorm waplt-mmsta waplt-lgpbe
                   waplt-labst   waplt-verpr waplt-dismm waplt-eisbe
                   waplt-minbe   waplt-mabst
                   waplt-bklas   waplt-plifz
                   waplt-gjahr   waplt-erfmg_0
                   waplt-erfmg_1 waplt-erfmg_2
                   waplt-erfmg_3 waplt-filler1
                   into outrec separated by ','.
      walng2 = strlen( outrec ).
      transfer outrec to p_fl2 length walng2.
endform.
