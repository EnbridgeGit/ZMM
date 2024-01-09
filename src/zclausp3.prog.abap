*$*$----------------------------------------------------------------$*$*
*$ Correction instructions  : 0120024545 0000091592                   $*
*$ Delivery package         : R/3 CORE   R/3 Standard                 $*
*$--------------------------------------------------------------------$*
*$ Validity    : 300          No patches                              $*
*$             : 30A          No patches                              $*
*$             : 30B          No patches                              $*
*$             : 30C          SAPKH30C01 - SAPKH30C14                 $*
*$             : 30D          SAPKH30D01 - SAPKH30D87                 $*
*$             : 30E          SAPKH30E01 - SAPKH30E27                 $*
*$             : 30F          SAPKH30F01 - SAPKH30F81                 $*
*$             : 310          No patches                              $*
*$             : 31G          SAPKH31G01 - SAPKH31G13                 $*
*$             : 31H          SAPKH31H01 - SAPKH31H60                 $*
*$             : 31I          SAPKH31I01 - SAPKH31I33                 $*
*$             : 40A          SAPKH40A01 - SAPKH40A25                 $*
*$             : 40B          SAPKH40B01 - SAPKH40B26                 $*
*$             : 45A          SAPKH45A01 - SAPKH45A25                 $*
*$             : 45B          SAPKH45B01 - SAPKH45B07                 $*
*$--------------------------------------------------------------------$*
*$ Changes/objects not contained in standard SAP system               $*
*$*$----------------------------------------------------------------$*$*
*&---------------------------------------------------------------------*
*& Object          REPS ZCLAUSP3
*& Object header   PROG ZCLAUSP3
*&---------------------------------------------------------------------*
*& REPORT ZCLAUSP3
*&---------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
* Create report 'ZCLAUSP3' with transaction SE38.
* Insert following lines.
*
*  The report deletes entries in table AUSP:
*  Those entries of AUSP are deleted referring to characteristics
*  that are not used in the specified class or in other classes
*  (in the case of multiple classification)
*  i.e. there is no entry in table KSML for this characteristic.
*
*  !!!  Note: Before any data in the database are deleted
*  !!!        the user has to confirm this.
*
*Procedure:
*Enter a class which classifications you want to check.
*You can also specify a single object of this class too.
*
*1.The system starts with the class or the first class you have
*entered on the selection screen
*2.selects the assigned characteristics of this class
*(table KSML) and the assigned or enterd object(s) (table KSSK)
*3.selects all characteristic values of these objects
*     (table AUSP)
*4.compare the characteristics of AUSP entries with the
*     characteristics of the selected class
*     If there are no difference between the characteristics,
*     AUSP entries are correct - no action is necessary.
*5.There exist differences:
*     Determine the objects of those AUSP entries which have no
*     corresponding characteristic in table KSML
*     Select all classes where these objects are assigned to.
*     Select the assigned characteristics of these classes
*     Compare these characteristics with the characteristics of
*     AUSP (step 3)
*     If there are no difference between the characteristics,
*     AUSP entries are correct - no action is necessary.
*     If differences exist, these entries are not correct.
*     The system deletes these inconsistencies in table AUSP.
*6.If you enter more than one class on the selection screen, the
*     system processes step 1 - 5 for each of these classes.
*
*Parameters:
*class:
*Class or first class (multiple classification) to be checked
*klart:
*     Class type to be checked
*objek
*if you enter objects, the system checks only inconsistencies for
*     these objects (step 2)
*  test_on:
*default: no database changes
*if you reset the default, the system deletes the inconsistencies
*
*#####################################################################

report zclausp3 no standard page heading.

tables: klah, kssk, ksml, ausp, tcla, inob.

data: begin of tab_klah occurs 1,
             clint like klah-clint,
             klart like klah-klart,
             class like klah-class,
           end of tab_klah.
data: begin of tab_kssk occurs 1,
             objek like kssk-objek,
             klart like kssk-klart,
             clint like kssk-clint,
           end of tab_kssk.
data: begin of tab_ksml occurs 1,
             clint like ksml-clint,
             imerk like ksml-imerk,
           end of tab_ksml.
data: begin of tab_ausp occurs 1,
             objek like ausp-objek,
             atinn like ausp-atinn,
           end of tab_ausp.
data: begin of tab_tcla occurs 1,
             klart like tcla-klart,
             obtab like tcla-obtab,
             multobj like tcla-multobj,
           end of tab_tcla.
data: begin of tab_inob occurs 1,
             cuobj like inob-cuobj,
             klart like tcla-klart,
             obtab like tcla-obtab,
           end of tab_inob.
data: tab_index like sy-index.
data: line_string(30).
data: tab_ausp_ex like ausp occurs 1 with header line.
data: tab_excess like tab_ausp occurs 1 with header line.
data: text_header1(22) value
           'Class             type',
           text_header11(6) value
           'status',
           text_header2(50) value
           'AUSP-OBJEK',
           text_header3(30) value
           'AUSP-ATINN (characteristic)',
           text_header4(20) value
           '| AUSP-MAFID',
           text_header5(20) value
           '| AUSP-ATZHL',
           text_header6(20) value
           '| value'.
selection-screen skip.
selection-screen comment /1(80) text-001.
select-options: class for klah-class obligatory,
       klart for klah-klart,
       objek for kssk-objek.
selection-screen skip.
selection-screen begin of block addition with frame.
parameters: test_on as checkbox default 'X'.
selection-screen end of block addition.

*-select class data
select distinct clint klart class from klah into table tab_klah
     where class in class
     and   klart in klart.

sort tab_klah by clint.

describe table tab_klah lines sy-tabix.
check sy-tabix > 0.

*-select class type data
select distinct klart obtab multobj from tcla into table tab_tcla
     for all entries in tab_klah
     where klart = tab_klah-klart.

*-for each class
loop at tab_klah.

*-initialize internal tables
  clear: tab_ksml, tab_kssk, tab_ausp, tab_ausp_ex.
  refresh: tab_ksml, tab_kssk, tab_ausp, tab_ausp_ex.
*-select assigned characteristics
  select distinct clint imerk from ksml into table tab_ksml
         where clint = tab_klah-clint.

*-select assigned objects
  select distinct objek klart clint from kssk into table tab_kssk
         where clint = tab_klah-clint
         and   klart = tab_klah-klart
         and   objek in objek.

  describe table tab_kssk lines sy-tabix.
  if sy-tabix > 0.

*-select values of the characteristics
    select distinct objek atinn from ausp into table tab_ausp
             for all entries in tab_kssk
             where objek = tab_kssk-objek
             and   klart = tab_klah-klart.

    sort tab_ausp by atinn.

*-compare defined characteristics <> valuated characteristics
    loop at tab_ksml.
      delete tab_ausp where atinn = tab_ksml-imerk.
    endloop.

*-check the rest
    describe table tab_ausp lines sy-tabix.
    if sy-tabix > 0.
      loop at tab_kssk.
        tab_index = sy-tabix.
*-select object keys of the rest
        read table tab_ausp with key objek = tab_kssk-objek.
        if not sy-subrc is initial.
          delete tab_kssk index tab_index.
        endif.
      endloop.
      perform check_of_the_rest.
    endif.

    format color 1.
    write: / tab_klah-class, tab_klah-klart.
    format color off.

    describe table tab_ausp lines sy-tabix.
    if sy-tabix > 0.
      select * from ausp into table tab_ausp_ex
                 for all entries in tab_ausp
                 where klart = tab_klah-klart
                 and   atinn = tab_ausp-atinn
                 and   objek = tab_ausp-objek
                 and   ( atimb is null or atimb = 0 ).
    endif.

    describe table tab_ausp_ex lines sy-tabix.
    if sy-tabix > 0.

      sort tab_ausp_ex by objek.

      format color 6.
      write: 'characteristics incorrect'.
      format color off.

      loop at tab_ausp_ex.
        format color 2.
        write: / tab_ausp_ex-objek.
        format color off.
        clear line_string.
        if not tab_ausp_ex-atwrt is initial.
          line_string = tab_ausp_ex-atwrt.
        elseif not tab_ausp_ex-atflv is initial.
          write tab_ausp_ex-atflv to line_string left-justified.
        else.
          write tab_ausp_ex-atflb to line_string left-justified.
        endif.
        write: /5 tab_ausp_ex-atinn, '|',
                  tab_ausp_ex-mafid, '|',
                  tab_ausp_ex-atzhl, '|',
                  line_string.
      endloop.

      if test_on = ' '.
        skip 2.
        format color 3.
        delete ausp from table tab_ausp_ex.
        if sy-subrc is initial.
          write: /61 'incorrect AUSP data deleted'.
        else.
          write: /61 'incorrect AUSP data not deleted - deletion error'.
        endif.
        format color off.
      endif.
    else.
      format color 5.
      write: 'characteristics ok'.
      format color off.
    endif.

  endif.

endloop.

*---------------------------------------------------------------------*
form check_of_the_rest.

  data:  init_class_name like klah-class,
                init_classtype  like tcla-klart.

  data:charact_of_class like api_char occurs 1 with header line.

  data:h_objek like kssk-objek,
               h_field like ddb_c02-instance,
               h_table like tcla-obtab,
               h_cuobj like inob-cuobj.

  loop at tab_kssk.

    if h_objek ne tab_kssk-objek.


      h_objek = tab_kssk-objek.
      h_cuobj = tab_kssk-objek.

      read table tab_tcla with key klart = tab_klah-klart.
      if tab_tcla-multobj = ' '.
        h_table = tab_tcla-obtab.
      else.
*- select multobj
        select obtab from inob into inob-obtab
                     where cuobj = h_cuobj
                     and   klart = tab_klah-klart.
        endselect.
        if sy-subrc is initial.
          h_table = inob-obtab.
        endif.
      endif.

      call function 'CTMS_CLASS_OBJECT_DDB'
      exporting
      batch                    = 'X'
      class                    = tab_klah-class
      classtype                = tab_klah-klart
      objectid                 = h_table
      object                   = tab_kssk-objek
      display                  = 'X'
      key_date                 = sy-datum
      set_values_from_db       = 'X'
      application              = '1'
      enqueue                  = 'X'
      importing
      instance                 = h_field
      exceptions
      not_found                = 1
      no_allocation_to_classes = 2
      others                   = 3.
    endif.

    call function 'CTMS_DDB_HAS_CHARACTERISTICS'
    exporting
    required                 = 'X'
    optional                 = 'X'
    inherited_only           = ' '
    assigned                 = 'X'
    unassigned               = 'X'
    inherited_excluding      = ' '
    tables
    exp_characteristics      = charact_of_class
    exceptions
    not_found                = 01
    no_characteristic_in_ddb = 02.

    loop at charact_of_class.
      delete tab_ausp where atinn = charact_of_class-atinn.
    endloop.

  endloop.

endform.

top-of-page.
  format color 1.
  write: text_header1.
  format color off.
  write: text_header11.
  format color 2.
  write: / text_header2.
  format color off.
  write:
        /5 text_header3,
        /36 text_header4,
        /40 text_header5,
        /46 text_header6.
  uline.

*>>>> END OF INSERTION <<<<<<
