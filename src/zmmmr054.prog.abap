REPORT ZMMMR054 NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 58.
************************************************************************
*    Program     :  ZMMMR054 - MM: CONSTRUC. MATNR CATALOG
*    Programmer  :  M L De Meester
*    Client      :  Union Gas Limited
*    Date        :  May 25,2001
*
*    This ABAP will retrieve the Material Number associated with the
*    class entered.  It will also store any Sub-Classes that are
*    found.
************************************************************************
* 2001/05/25 mdemeest #--- Copied ZMMMR053
************************************************************************
tables: klah,                "Class Description table
        kssk,                "Class linking to subclass
        ausp,
        mard,
        swor,                "English description of Class
        cabn,
        mara,                "Material Master
        makt.                "Material Description

data:  outfile(100).                "File name for output individual
data:  outfile1(100).               "File name for output all materials

data:  wa_pridesc         like cabn-atinn.
data:  wa_secdesc         like cabn-atinn.
data:  wa_material        like cabn-atinn.
data:  wa_model           like cabn-atinn.
data:  wa_prim_dia        like cabn-atinn.
data:  wa_sec_dia         like cabn-atinn.
data:  wa_prim_dia_nps    like cabn-atinn.
data:  wa_keyword         like cabn-atinn.
data:  wa_prim_wall       like cabn-atinn.
data:  wa_grade           like cabn-atinn.
data:  wa_flangetype      like cabn-atinn.
data:  wa_endtype         like cabn-atinn.
data:  wa_pressure        like cabn-atinn.
data:   wa_bore           like cabn-atinn.
data:  wa_atwrt_dia_nps   like ausp-atwrt.
data:  wa_model_flag(1)   type c.
data: atflv_p   type p decimals 5.
data: atflv_n(15) type n.
data: wa_atflv_dia(6) type c.
data:  cp_class3(40) type c.
data: cp_category2 like kssk-clint.
data:  cp_category3 like kssk-clint.
data: lngth type i.


data: wa_dup_mat_flag(1) type c.


data:  dataline(150)    type c.
data:  wa_dataline(150) type c.
*
data:  wa_number(15)   type p decimals 3.
data:  temp_matnr  like ausp-objek.

data:  begin of tablechar   occurs 20.
       include structure ausp.
data:  end of tablechar.

data:  begin of all_cabn    occurs 1000.
       include structure cabn.
data:  end of all_cabn.

data:  begin of nodetable1  occurs 5,
       category1       like kssk-clint,
       class1          like klah-class,
       end of nodetable1.

data:  begin of nodetable2  occurs 100,
       category1       like kssk-clint,
       category2       like kssk-clint,
       class1          like klah-class,
       class2(40) type c,
       matnr           like mard-matnr,
       end of nodetable2.

data:  begin of nodetable3  occurs 100,
       category1       like kssk-clint,
       category2       like kssk-clint,
       category3       like kssk-clint,
       class1          like klah-class,
       class2(40) type c,
       class3(40) type c,
       matnr           like mard-matnr,
       end of nodetable3.


data:  begin of nodetable4  occurs 100,
       class1          like klah-class,
       class2(40) type c,
       class3(40) type c,
       keyword         like ausp-atwrt,  "Keyword
       sortkey(80) type c,
       matnr           like mard-matnr,
       category1       like klah-clint,
       category2       like klah-clint,
       category3       like klah-clint,
       pridesc         like ausp-atwrt,  "Primary Description
       secdesc         like ausp-atwrt,  "Secondary Description
       model           like ausp-atwrt,  "Model Number
       material        like ausp-atwrt,  "Material characteristic
       end of nodetable4.

data:  cp_pridesc  like nodetable4-pridesc.
data:  cp_secdesc  like nodetable4-secdesc.
data:  cp_material like nodetable4-material.

data: keyw_atinn     like   ausp-atinn.

*------------------------  SELECTION SCREEN  ---------------------------
SELECTION-SCREEN SKIP.
SELECTION-SCREEN begin of block box2 with frame.
PARAMETERS: p_file(100) type c lower case
    default '/usr/sap/interfaces/P01/SAPMM023/constructcat.html'.
selection-screen end of block box2.
selection-screen skip.

SELECTION-SCREEN COMMENT 1(80) TEXT-001.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS: P_CLASS     LIKE   KLAH-CLASS OBLIGATORY
                               DEFAULT 'CONSTRUCT_MATERIAL'.
SELECTION-SCREEN END OF BLOCK BOX1.


SELECTION-SCREEN COMMENT 1(80) TEXT-002.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.

************************************************************************
START-OF-SELECTION.

  perform get_all_characteristics.
  PERFORM CHAR_INT_NUM.

  perform populate_table1.
  perform populate_table2.
  perform populate_table3.
  perform populate_table4.
  perform get_keyword_char.
  perform delete_nla_zero_qty.        "Eliminate NLA_ZERO materials
  perform remove_deleted_materials.   "Eliminate flagged materials

  sort nodetable4 by class1 class2 class3 keyword sortkey matnr.
*sort nodetable4 by category1 category2 category3 keyword sortkey matnr.
  loop at nodetable4.
     if nodetable4-class2 = 'PIPES' or nodetable4-class3 = 'VALVES'.
*        write: / 'BEFORE', nodetable4-keyword.
        clear nodetable4-keyword(6).
        modify nodetable4.
*        write: / 'AFTER ', nodetable4-class2, nodetable4-keyword,
*                 nodetable4-sortkey, nodetable4-matnr.
     endif.
*     write: / 'AFTER ', nodetable4-class2, nodetable4-keyword,
*            nodetable4-sortkey, nodetable4-matnr.

  endloop.

  perform create_all_materials.


*---------------------  REMOVE_DELETED_MATERIALS  ----------------------
* This code deletes any materials flagged for deletion. LVORM = X.
*-----------------------------------------------------------------------
form remove_deleted_materials.
  loop at nodetable4.
  select single * from mara
   where matnr = nodetable4-matnr
      and lvorm = 'X'.
   if sy-subrc = '0'.
      delete nodetable4.
   endif.
  endloop.
endform.
*-----------------------------------------------------------------------
*-------------------- CREATE_INDIVIDUAL_MATERIAL_FILES  ----------------
* This routine builds individual material files into the same directory.
* The material number is used as the file name.
*-----------------------------------------------------------------------
form create_indiv_material_files.
   outfile = p_file.
   outfile+33 = nodetable4-matnr+12(6).
   outfile+39 = '.htm'.
   condense outfile no-gaps.
   open dataset outfile for output in text mode.

  move '<html>'                       to dataline.
  transfer dataline                   to outfile length 6.

  move '<head>'                       to dataline.
  transfer dataline                   to outfile length 6.

  move '<title>xxxxxx</title>'        to dataline.
  move nodetable4-matnr+12(6)         to dataline+7(6).
  transfer dataline                   to outfile length 22.

  move '</head>'                      to dataline.
  transfer dataline                   to outfile length 7.

  move '<body>'                       to dataline.
  transfer dataline                   to outfile length 6.
  move '<font face="Arial" size="2">' to dataline.
  transfer dataline                   to outfile length 28.

  move '<p><b>xxxxxx</b> <u>'         to dataline(20).
  move '</u></p>'                     to dataline+65(8).
  select single maktx from makt into dataline+21(40)
    where matnr = nodetable4-matnr
      and spras = sy-langu.
  move nodetable4-matnr+12(6)         to dataline+6(6).
  perform transfer_dataline.

*  move '<font size="2"> CLASS:</font> xxxxxxxxxxxxxxxxxx <br>'
*                                      to dataline.
  move nodetable4-class2(18)          to dataline+30(18).
  perform transfer_dataline.
*  move '<font size="2"> SUBCLASS:</font> xxxxxxxxxxxxxxxxxx <br>'
  move 'SUBCLASS: xxxxxxxxxxxxxxxxxx <br>'
                                      to dataline.
  move nodetable4-class3(18)          to dataline+10(18).
  perform transfer_dataline.

  loop at tablechar.
    clear dataline.
    if tablechar-atwrt <> 'N/A'.        "Don't show the Not Applicable"
      loop at all_cabn.
        if tablechar-atinn = all_cabn-atinn.
*           move '<font size="2">'      to dataline(15).
           move all_cabn-atnam         to dataline+15(40).
           move '<i>'                   to dataline+55(10).
*           move '</font><i>'           to dataline+55(10).
           if tablechar-atwrt = space.
              move tablechar-atflv     to wa_number.
              move wa_number           to dataline+65(10).
           else.
              move tablechar-atwrt     to dataline+65(40).
           endif.
           move '</i><br>'             to dataline+105(8).
           perform transfer_dataline.
        endif.
      endloop.
    endif.
  endloop.

  move '</body>'        to dataline.
  transfer dataline     to outfile length 7.

  move '</html>'        to dataline.
  transfer dataline     to outfile length 7.

  close dataset outfile.

endform.
*-------------------- CREATE_ALL_MATERIAL  -----------------------------
* This routine builds 1 file with all materials
*-----------------------------------------------------------------------
form create_all_materials.

   outfile1 = p_file.
*   outfile1+33(15) = 'allmaterial.htm'.
   open dataset outfile1 for output in text mode.

  move '<html>'                       to dataline.
  transfer dataline                   to outfile1 length 6.

  move '<head>'                       to dataline.
  transfer dataline                   to outfile1 length 6.

  move '<title>Catalog </title>'                 to dataline.
  transfer dataline                   to outfile1 length 60.

  move '<script language="JavaScript" fptype="dynamicoutline">'
                                                    to dataline.
  transfer dataline                   to outfile1 length 60.

  move '<!--'                         to dataline.
  transfer dataline                   to outfile1 length 4.

  move 'function dynOutline() {}'     to dataline.
  transfer dataline                   to outfile1 length 24.

  move '//-->'                        to dataline.
  transfer dataline                   to outfile1 length 5.

  move '</script>'                    to dataline.
  transfer dataline                   to outfile1 length 9.
  move '<script language="JavaScript1.2" fptype="dynamicoutline"'
                                      to dataline.
  move ' src="outline.js">'           to dataline+58(20).
  transfer dataline                   to outfile1 length 76.

  move '</script>'                    to dataline.
  transfer dataline                   to outfile1 length 9.


  move '<script language="JavaScript1.2" fptype="dynamicoutline"'
                                      to dataline.
  move ' for="document" event="onreadystatechange()">'
                                      to dataline+57.
  transfer dataline                   to outfile1 length 105.

  move '<!--'                         to dataline.
  transfer dataline                   to outfile1 length 5.

  move 'initOutline()'                to dataline.
  transfer dataline                   to outfile1 length 13.

  move '//-->'                        to dataline.
  transfer dataline                   to outfile1 length 5.

  move '</script>'                    to dataline.
  transfer dataline                   to outfile1 length 9.

  move '<base target="rbottom">'      to dataline.
  transfer dataline                   to outfile1 length 24.

  move '</head>'                      to dataline.
  transfer dataline                   to outfile1 length 7.

  move '<body onclick="dynOutline()">' to dataline.
  transfer dataline                    to outfile1 length 40.
*  move '<font size="1">'               to dataline.
*  transfer dataline                    to outfile1 length 15.

  loop at nodetable4.
    move nodetable4-pridesc to cp_pridesc.
    move nodetable4-secdesc to cp_secdesc.
    move nodetable4-material to cp_material.
    move nodetable4-class3   to cp_class3.
    move nodetable4-category2 to cp_category2.
    move nodetable4-category3 to cp_category3.

    at new class1.
*-->  title of catalogue
      move '<h1><font face="Arial" size="5" '  to dataline.
      move 'color="#000080">Construction Material Catalogue'
                                               to dataline+32(47).
      move '</font></h1>'                      to dataline+79(13).
      transfer dataline                        to outfile1 length 92.

*     move '<h1> Construction Material Catalogue </h1>' to dataline.
*     transfer dataline                        to outfile1 length 42.

*-->  date of report
      move '<p><font size="2" face="Arial"> <b>' to dataline.
      move sy-datum(4)                         to dataline+35(4).
      move sy-datum+4(2)                       to dataline+40(2).
      move sy-datum+6(2)                       to dataline+43(2).
      move '-'                                 to dataline+39(1).
      move '-'                                 to dataline+42(1).
      move '<b></font></p>'                    to dataline+45(14).
      transfer dataline                        to outfile1 length 59.

      move '<ul dynamicoutline initcollapsed>' to dataline.
      transfer dataline                        to outfile1 length 40.
* Collapse/Expand Buttons
      move '<input type="button" name="cmdCollapse" id="cmdCollapse"'
                                               to dataline(57).
      move 'value="Collapse" onclick="collapseAll()"/>'
                                               to dataline+57(45).

      transfer dataline to outfile1 length 103.
      move '<input type="button" name="cmdExpand" id="cmdExpand"'
                                               to dataline(57).
      move 'value=" Expand " onclick="expandAll()"/>'
                                               to dataline+57(45).
      transfer dataline to outfile1 length 103.
      move '<font face="Arial" size="2">'      to dataline.
      transfer dataline to outfile1 length 29.

    endat.

    at new class2.
*-->  Change font for rest of report to Arial  10 pt.
      select single * from swor
        where clint = cp_category2
          and spras = sy-langu
          and klpos = '01'.
      move '<li> <b> xxxxxxxxxxxxxxxxxxxx</b>' to dataline.
      move swor-kschg                          to dataline+9(25).
      transfer dataline                        to outfile1 length 40.
      move '<ul>'                              to dataline.
      transfer dataline                        to outfile1 length 4.
    endat.

   if nodetable4-class3 <> space.
      at new class3.
         select single * from swor
           where clint = cp_category3
             and spras = sy-langu
             and klpos = '01'.
      move '<li> <b> xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx</b>' to dataline.
      move swor-kschg                          to dataline+9(33).
      transfer dataline                        to outfile1 length 50.
      move '<ul>'                              to dataline.
      transfer dataline                        to outfile1 length 4.
      endat.
   endif.

   at new keyword.
     move '<li>'                               to dataline.
     move nodetable4-keyword                   to dataline+4(30).
     transfer dataline                         to outfile1 length 35.
     if cp_class3 = space.
* changed from 160 to 135   2001/10/31
     move '<table border="0" width="135%">' to dataline.  "2 levels
     else.
* changed from 150 to 125   2001/10/31
     move '<table border="0" width="125%">' to dataline.  "3 levels
     endif.

*    move '<table border="0" width="1500">' to dataline.
     transfer dataline to outfile1 length 32.

   endat.

   perform get_all_char_for_material.
*   perform create_indiv_material_files.

*   Material & its detail


at new matnr.
   move '<tr>' to dataline.
   transfer dataline to outfile1 length 4.

   move nodetable4-matnr+12(6) to wa_dataline(6).    "Material #
   move cp_pridesc     to wa_dataline+7(30). "Primary Desc
   move cp_secdesc     to wa_dataline+37(30)."Secondary Desc


   move '<td width="58%"><font size="2">' to dataline(33).
   move wa_dataline(67)    to dataline+33(67).
   move '</font></td>' to dataline+100(12).
*   move '</td>'            to dataline+100(12).
   perform remove_spaces.
*   transfer dataline to outfile1 length 7.




   move '<td width="18%"><font size="2">' to dataline(31)."Material Type
   move cp_material to dataline+31(20).
   move '</font></td>'               to dataline+51(12).
   perform remove_spaces.
   move 'X' to wa_dup_mat_flag.

endat.


   if wa_dup_mat_flag = 'X'.
      move '<td width="24%"><font size="2">' to dataline(31)." Model No.
      move nodetable4-model   to dataline+31(30).
      move '</font></td>'     to dataline+61(12).
      perform remove_spaces.

      move '</tr>'            to dataline(5).
      transfer dataline       to outfile1 length 5.
      clear wa_dup_mat_flag.
   else.
      move '<tr>' to dataline.
      transfer dataline to outfile1 length 4.

      move '<td width="58%"><font size="2">' to dataline(31).   "> 1
      move '</font></td>'                    to dataline+47(12).
      perform remove_spaces.

      move '<td width="18%">' to dataline(16).        "> 1 Model Number
      move '</td>'            to dataline+46(5).
      perform remove_spaces.


      move '<td width="24%"><font size="2">' to dataline(31)."FirstModel
      move nodetable4-model   to dataline+31(30).
      move '</font></td>'     to dataline+61(12).
      perform remove_spaces.

      move '</tr>'            to dataline(5).
      transfer dataline       to outfile1 length 5.
      clear wa_dup_mat_flag.

   endif.

   at end of keyword.
     move '</table>' to dataline.
     transfer dataline to outfile1 length 10.

  endat.

   if nodetable4-class3 <> space.
     at end of class3.
       move '</ul>'                              to dataline.
       transfer dataline                         to outfile1 length 5.
       move '</li>'                              to dataline.
       transfer dataline                         to outfile1 length 5.
     endat.
   endif.

    at end of class2.
       move '</ul>'                              to dataline.
       transfer dataline                         to outfile1 length 5.
       move '</li>'                              to dataline.
       transfer dataline                         to outfile1 length 5.
    endat.

    at last.
       move '</font>'                            to dataline.
       transfer dataline                         to outfile1 length 7.
       move '</body>'                            to dataline.
       transfer dataline                         to outfile1 length 7.

       move '</html>'                            to dataline.
       transfer dataline                         to outfile1 length 7.

    endat.

    at end of class1.
       move '</ul>'                              to dataline.
       transfer dataline                         to outfile1 length 5.

   endat.


endloop.



  close dataset outfile1.

endform.

form remove_spaces.
   condense dataline.
   lngth = strlen( dataline ).
   transfer dataline to outfile1 length lngth.
   clear dataline.
endform.




*----------------------  DELETE_NLA_ZERO_QTY ---------------------------
* Materials with 'NLA' in their secondary description and no quantity
* in the company are removed from the report/file
*-----------------------------------------------------------------------
form delete_nla_zero_qty.

 loop at nodetable4.
   select single * from mard
      where matnr = nodetable4-matnr
        and labst > 0.
   if sy-subrc <> 0.
      search nodetable4-secdesc for 'NLA '. "Find ' NLA ' in description
      if  sy-subrc = '0'.
          delete nodetable4.                "Delete if NLA & qty = 0
      endif.
   endif.
 endloop.
endform.

*---------- SUBROUTINES ------------------------- SUBROUTINES ----------

*---------------------------  ATINN  -----------------------------------
*  This routine translates the English characteristic to the internal
*  SAP characteristic number
*-----------------------------------------------------------------------
form atinn using description changing atinn_value.

   CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
         CLASS_TYPE                = '001'
         FEATURE_NEUTRAL_NAME      = description
      IMPORTING
         FEATURE_ID                = atinn_value
      EXCEPTIONS
         OTHERS                      = 6.

endform.

*---------------------------  CHAR_INT_NUM -----------------------------
* This routine gets the internal characteristic number of the
* characteristic.
*-----------------------------------------------------------------------
FORM CHAR_INT_NUM.
  PERFORM ATINN USING 'KEYWORD'
                CHANGING KEYW_ATINN.
ENDFORM.

*--------------------  GET_ALL_CHARACTERISTICS -------------------------
*  Get all valid characteristics from CABN ignoring any description with
*  'SAP' -  ALL_CABN contains the English description of characteristic
*-----------------------------------------------------------------------
form get_all_characteristics.
   select * from cabn into table all_cabn
     where atnam <> space.

*   select * from cabn into table all_cabn
*     where atnam in ('PRIMARY_DESCRIPTION','SECONDARY_DESCRIPTION',
*                       'MATERIAL','MODEL_NUMBER' ).
     loop at all_cabn.
        case all_cabn-atnam.
          when 'PRIMARY_DESCRIPTION'.
            move all_cabn-atinn to wa_pridesc.
          when 'SECONDARY_DESCRIPTION'.
            move all_cabn-atinn to wa_secdesc.
          when 'MATERIAL'.
            move all_cabn-atinn to wa_material.
          when 'MODEL_NUMBER'.
            move all_cabn-atinn to wa_model.
          when 'PRIMARY_DIAMETER__MM'.
            move all_cabn-atinn to wa_prim_dia.
          when 'SECONDARY_DIAMETER__MM'.
            move all_cabn-atinn to wa_sec_dia.
          when 'PRIMARY_WALL_THICKNESS'.
            move all_cabn-atinn to wa_prim_wall.
          when 'GRADE'.
            move all_cabn-atinn to wa_grade.
          when 'PRESSURE_RATING'.
            move all_cabn-atinn to wa_pressure.
          when 'KEYWORD'.
            move all_cabn-atinn to wa_keyword.
          when 'FLANGE_TYPE'.
            move all_cabn-atinn to wa_flangetype.
          when 'END_TYPE'.
            move all_cabn-atinn to wa_endtype.
          when 'BORE__MM'.
            move all_cabn-atinn to wa_bore.
          when 'PRIMARY_DIAMETER__NPS'.
            move all_cabn-atinn to wa_prim_dia_nps.

        endcase.
     endloop.
*  select * from cabn into table all_cabn
*     where atnam <> space.
*     if all_cabn-atnam(3) = 'SAP'.
*        delete all_cabn.
*     endif.
endform.

*-------------------  GET_ALL_CHAR_FOR_MATERIAL ------------------------
*  Selects all characteristics for a particular material from table AUSP
*-----------------------------------------------------------------------
form get_all_char_for_material.
  move nodetable4-matnr  to temp_matnr.
  CALL FUNCTION 'CLFM_SELECT_AUSP'
      exporting
           MAFID              ='O'
           CLASSTYPE          ='001'
           OBJECT             = temp_MATNR
      TABLES
           EXP_AUSP            = tablechar
      EXCEPTIONS
           NO_VALUES          = 1
           OTHERS             = 2.

endform.
*------------------------  GET_KEYWORD_CHAR  ---------------------------
*  Add KEYWORD characteristics  to NODETABLE4
*-----------------------------------------------------------------------
form get_keyword_char.
*  loop at nodetable4.
*     select single * from ausp
*       where objek = nodetable4-matnr
*         and mafid = 'O'
*         and klart = '001'
*         and atinn = keyw_atinn.
*       if sy-subrc = '0'.
*          move ausp-atwrt to nodetable4-keyword.
*       else.
*          clear nodetable4-keyword.
*       endif.
*       modify nodetable4.
*  endloop.

endform.



form populate_table1.
  select single * from KLAH
    where klart = '001'
      and class = p_class.
  if sy-subrc = '0'.
     nodetable1-category1 = klah-clint.
     nodetable1-class1    = klah-class.
     append nodetable1.
  else.
     write: / 'ERROR SELECTING CLASS'.
  endif.
endform.

*-----------------------------------------------------------------------
* Get all nodes in the chain - level 2
*-----------------------------------------------------------------------
form populate_table2.
  loop at nodetable1.
    move nodetable1-category1 to nodetable2-category1.           "   #1
    move nodetable1-class1    to nodetable2-class1.
    select * from kssk
         where klart = '001'
         and clint = nodetable1-category1.
         move kssk-objek           to nodetable2-category2.      "   #2
         select single * from klah where clint = kssk-objek.
         move klah-class           to nodetable2-class2.

         if kssk-mafid = 'O'.                                  "Material
            move kssk-objek        to nodetable2-matnr.
         endif.

         append nodetable2.
    endselect.
  endloop.
*-----------------------------------------------------------------------
*  Use this if you want only a quick test of one/two classes
*loop at nodetable2.
*if nodetable2-category2 = '0000000054' or
*  nodetable2-category2 = '0000000089'.
* write: / nodetable2-category2, nodetable2-class2, nodetable2-matnr.
*else.
*  delete nodetable2.
*endif.
*endloop.
*-----------------------------------------------------------------------

endform.

*-----------------------------------------------------------------------
* Get all nodes in the chain - level 3
*-----------------------------------------------------------------------
form populate_table3.
  loop at nodetable2.
    clear nodetable3.
    move nodetable2-category1 to nodetable3-category1.   "#1
    move nodetable2-class1    to nodetable3-class1.
    move nodetable2-category2 to nodetable3-category2.   "#2
    move nodetable2-class2    to nodetable3-class2.
    select * from kssk
       where klart = '001'
         and clint = nodetable2-category2.
       if sy-subrc = '0'.
          if kssk-mafid = 'K'.
             move kssk-objek           to nodetable3-category3.
             select single * from klah where clint = kssk-objek.
             move klah-class           to nodetable3-class3.
          else.
             move kssk-objek           to nodetable3-matnr.
          endif.
          append nodetable3.
       endif.
    endselect.
    if sy-subrc = '4'.
       move nodetable2-matnr     to nodetable3-matnr.
       append nodetable3.
    endif.
  endloop.
endform.

form populate_table4.
  loop at nodetable3.
    clear nodetable4.
    move nodetable3-category1 to nodetable4-category1.   "#1
    move nodetable3-class1    to nodetable4-class1.
    move nodetable3-category2 to nodetable4-category2.   "#2
    move nodetable3-class2    to nodetable4-class2.
    move nodetable3-category3 to nodetable4-category3.   "#3
    move nodetable3-class3    to nodetable4-class3.

    if nodetable3-matnr = space.          "Material not found
       select * from kssk
          where klart = '001'
            and clint = nodetable3-category3.
          move kssk-objek        to nodetable4-matnr.
          perform pop_table4_characteristics.
*          append nodetable4.
       endselect.
    else.
       move nodetable3-matnr     to nodetable4-matnr.
       perform pop_table4_characteristics.
*      append nodetable4.
    endif.
  endloop.
endform.

*-------------------------  TRANSFER_DATALINE  -------------------------
*  Remove all unnecessary spaces from dataline.  Calculate the
*  resulting length and write to file
*-----------------------------------------------------------------------
form transfer_dataline.
  condense dataline.
  lngth = strlen( dataline ).
  transfer dataline                   to outfile length lngth.
endform.

form pop_table4_characteristics.
   perform get_all_char_for_material.
   clear: nodetable4-pridesc, nodetable4-secdesc, nodetable4-model,
         nodetable4-material, nodetable4-sortkey,
         wa_atwrt_dia_nps.
   loop at tablechar.
     if tablechar-atinn = wa_pridesc.
        move tablechar-atwrt to nodetable4-pridesc.
     elseif tablechar-atinn = wa_secdesc.
        if tablechar-atwrt = 'N/A'.
           move space        to nodetable4-secdesc.
        else.
        move tablechar-atwrt to nodetable4-secdesc.
        endif.
     elseif tablechar-atinn = wa_material.
        move tablechar-atwrt to nodetable4-material.
     elseif tablechar-atinn = wa_prim_dia.
        move tablechar-atflv  to atflv_p.
        move atflv_p          to atflv_n.
        move atflv_n+9(6)     to wa_atflv_dia.
     elseif tablechar-atinn = wa_prim_dia_nps.
        move tablechar-atwrt  to wa_atwrt_dia_nps. "For PIPES & VALVES
     endif.
   endloop.

*  build sortkey.

   loop at tablechar.
     if nodetable4-class3 = 'OTH_CORROSION_ACC'.
*        perform keyword_first.
        perform matnr_fourth.     "Pos. 2 & 3 in sortkey are left blank
     endif.
     if nodetable4-class3 = 'INSULATING_JOINTS'.
*        perform keyword_first.
        perform prim_dia_second.
        perform wall_thick_35_for_6.
        perform pressure_rating_65_for_6.
     endif.
     if nodetable4-class3 = 'INSULATE_FLG_SETS'.
*        perform keyword_first.
        perform prim_dia_second.
        perform pressure_rating_third.
        perform matnr_fourth.
     endif.
* FITTINGS - SORTKEY
*              1-24    25-39  40-45   46-51   52-57  58-64  65-70
* BRANCH CONN  KEYWORD MatTyp PrimDia WallTh  SecDia -----  PressRt
* FLANGES      KEYWORD ------ PrimDia FlngTyp ------ EndTyp PressRt
* REDUCERS     KEYWORD MatTyp PrimDia SecDia  WallTh EndTyp PressRt
* STOPPER      KEYWORD MatTyp PrimDia ------- ------ ------ PressRt
* STRAIGHT     KEYWORD MatTyp PrimDia WallTh  ------ EndTyp PressRt
* TRANSITION   KEYWORD MatTyp PrimDia WallTh  ------ ------ PressRt
* OTHER        KEYWORD MatTyp PrimDia WallTh  ------ EndTyp PressRt
     if nodetable4-class2 = 'FITTINGS'.
        if nodetable4-class3 = 'BRANCH_CONNECTIONS'.
           perform material_type_25_for_15.
           perform primary_dia_40_for_6.
           perform wall_thickness_46_for_6.
           perform sec_dia_52_for_6.
           perform pressure_rating_65_for_6.
        elseif nodetable4-class3 = 'FLANGES'.
           perform primary_dia_40_for_6.
           perform flange_type_46_for_6.
           perform end_type_58_for_6.
           perform pressure_rating_65_for_6.

        elseif nodetable4-class3 = 'REDUCERS'.
           perform material_type_25_for_15.
           perform primary_dia_40_for_6.
           perform sec_dia_46_for_6.
           perform wall_thickness_52_for_6.
           perform end_type_58_for_6.
           perform pressure_rating_65_for_6.

        elseif nodetable4-class3 = 'STOPPER_FITTINGS'.
           perform material_type_25_for_15.
           perform primary_dia_40_for_6.
           perform pressure_rating_65_for_6.

        elseIf  nodetable4-class3 = 'STRAIGHT_FITTINGS'.
           perform material_type_25_for_15.
           perform primary_dia_40_for_6.
           perform wall_thickness_46_for_6.
           perform end_type_58_for_6.
           perform pressure_rating_65_for_6.

        elseif nodetable4-class3 = 'TRANSITION_FITTING'.
           perform material_type_25_for_15.
           perform primary_dia_40_for_6.
           perform wall_thickness_46_for_6.
           perform pressure_rating_65_for_6.

        elseif nodetable4-class3 =   'OTHER_FITTINGS'.
           perform material_type_25_for_15.
           perform primary_dia_40_for_6.
           perform wall_thickness_46_for_6.
           perform end_type_58_for_6.
           perform pressure_rating_65_for_6.

        endif.
     endif.

     if nodetable4-class2 = 'PIPELINE_MATERIALS'.
        perform prim_dia_second.
     endif.

     if nodetable4-class2 = 'PIPES'.  "***PIPES is in CLASS2 ******
        perform material_type_25_for_15.
        perform primary_dia_40_for_6.
        perform wall_thickness_46_for_6.
        perform adjust_keyword.
     endif.
     if nodetable4-class2  = 'REPAIR_PARTS'.
        perform prim_dia_second.
        perform sec_dia_46_for_6.
     endif.
     if nodetable4-class2 = 'STATION_MATERIALS'.
        perform prim_dia_second.
     endif.
     if nodetable4-class2 = 'WELL_DRILLING'.
     endif.
     if nodetable4-class3 = 'VALVES'.
        perform material_type_25_for_15.
        perform primary_dia_40_for_6.
*        perform keyword_fourth.
        perform pressure_rating_46_for_10.
*        perform end_type_sixth.
     endif.
     if nodetable4-class3 = 'VALVES_ACCESSORIES'.
     endif.

   endloop.

  loop at tablechar.
    if tablechar-atinn = wa_keyword.
       perform keyword_first.
       if nodetable4-class2 = 'PIPES' or nodetable4-class3 = 'VALVES'.
*          move nodetable4-sortkey(8)  to nodetable4-sortkey+8(8).
          move nodetable4-sortkey(10)  to nodetable4-sortkey+8(10).
          move wa_atflv_dia           to nodetable4-sortkey(8).
          move nodetable4-sortkey(18) to nodetable4-keyword.
          move wa_atwrt_dia_nps       to nodetable4-keyword+18(6).
       endif.
       clear wa_model_flag.
       loop at tablechar.             "MODEL #'s for Material
          if tablechar-atinn = wa_model.
             move tablechar-atwrt     to nodetable4-model.
             move 'X'                 to wa_model_flag.
             append nodetable4.
          endif.
       endloop.
       if wa_model_flag <> 'X'.     "NO MODEL# for Material
          append nodetable4.
       endif.
    endif.
  endloop.
                                       "Takes care of multi KEYWORDS

endform.

form keyword_first.
        move tablechar-atwrt  to nodetable4-keyword.
        move tablechar-atwrt  to nodetable4-sortkey(25).
endform.

form keyword_fourth.
     if tablechar-atinn = wa_keyword.
        move tablechar-atwrt  to nodetable4-keyword.
        move tablechar-atwrt  to nodetable4-sortkey+50(10).
     endif.
endform.

form prim_dia_second.
data: atflv_p   type p decimals 5.
data: atflv_n(15) type n.
     if tablechar-atinn = wa_prim_dia.
        move tablechar-atflv to atflv_p.
        move atflv_p to atflv_n.
        move atflv_n+9(6) to nodetable4-sortkey+25(6).
     endif.
endform.

form wall_thickness_46_for_6.
     if tablechar-atinn = wa_prim_wall.
        move tablechar-atflv to atflv_p.
        move atflv_p to atflv_n.
        move atflv_n+9(6) to nodetable4-sortkey+46(6).
     endif.
endform.



form sec_dia_46_for_6.
data: atflv_p   type p decimals 5.
data: atflv_n(15) type n.
     if tablechar-atinn = wa_sec_dia.
        move tablechar-atflv to atflv_p.
        move atflv_p to atflv_n.
        move atflv_n+9(6) to nodetable4-sortkey+46(6).
     endif.
endform.

form sec_dia_52_for_6.
     if tablechar-atinn = wa_sec_dia.
        move tablechar-atflv to atflv_p.
        move atflv_p to atflv_n.
        move atflv_n+9(6) to nodetable4-sortkey+52(6).
     endif.
endform.



form material_type_25_for_15.
  if tablechar-atinn = wa_material.
     move tablechar-atwrt  to nodetable4-sortkey+25(15).
  endif.
endform.

form primary_dia_40_for_6.
     if tablechar-atinn = wa_prim_dia.
        move tablechar-atflv to atflv_p.
        move atflv_p to atflv_n.
        move atflv_n+9(6) to nodetable4-sortkey+40(6).
     endif.
endform.



form wall_thickness_52_for_6.
     if tablechar-atinn = wa_prim_wall.
        move tablechar-atflv to atflv_p.
        move atflv_p to atflv_n.
        move atflv_n+9(6) to nodetable4-sortkey+52(6).
     endif.
endform.


form wall_thick_35_for_6.
data: atflv_p   type p decimals 5.
data: atflv_n(15) type n.
data: atflv_c(15) type c.
     if tablechar-atinn = wa_prim_wall.
        move tablechar-atflv to atflv_p.
        move atflv_p to atflv_n.
        move atflv_n+9(6) to nodetable4-sortkey+35(6).
     endif.



endform.

form pressure_rating_third.
     if tablechar-atinn = wa_pressure.
        write tablechar-atwrt to nodetable4-sortkey+40(10).
     endif.
endform.

form flange_type_46_for_6.
   if tablechar-atinn = wa_flangetype.
       write tablechar-atwrt to nodetable4-sortkey+46(6).
   endif.
endform.

form end_type_58_for_6.
  if tablechar-atinn = wa_endtype.
     write tablechar-atwrt to nodetable4-sortkey+58(6).
  endif.
endform.

form matnr_fourth.
     move nodetable4-matnr+12(6) to nodetable4-sortkey+50(10).
endform.


form pressure_rating_65_for_6.
     if tablechar-atinn = wa_pressure.
        write tablechar-atwrt to nodetable4-sortkey+65(6).
     endif.
endform.

form pressure_rating_46_for_10.
     if tablechar-atinn = wa_pressure.
        write tablechar-atwrt to nodetable4-sortkey+46(10).
     endif.
endform.


form bore_sixth.
data: atflv_p   type p decimals 5.
data: atflv_n(15) type n.
     if tablechar-atinn = wa_bore.
        move tablechar-atflv to atflv_p.
        move atflv_p to atflv_n.
        move atflv_n+10(5) to nodetable4-sortkey+50(5).
     endif.
endform.


form grade_seventh.
     if tablechar-atinn = wa_grade.
        write tablechar-atwrt to nodetable4-sortkey+55(5).
     endif.
endform.

form adjust_keyword.
    if tablechar-atinn = wa_prim_dia_nps.
       move nodetable4-sortkey(8) to nodetable4-sortkey+8(8).
       move tablechar-atwrt(8)    to nodetable4-sortkey(8).
    endif.
endform.













































