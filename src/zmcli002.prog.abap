REPORT zmcli002 LINE-COUNT 65 LINE-SIZE 170 MESSAGE-ID zm.

************************************************************************
*  Author:   M De Meester
*  Brief Description:
*  - The purpose of this program is to extract information from SAP and
*        create a input file from AM/FM
***********************************************************************
*  2011/09/01 btboundy TR835 Re-Write for Clarity and MDM
*  2010/02/23 lritchie TR717 Calculate average unit price & quantity on hand
*  2010/02/09 lritchie TR717 Default material flag to "F"
*  2010/02/02 lritchie TR717 PRESS as numeric, stock group R
*  2009/12/16 lritchie TR717 Put back the initialization of the record i.e. N/A
*  2009/11/18 lritchie TR717 Use only the fields required from SAP
*                            Clear the output fields
*  2009/10/13 mdemeest TR--- Changes required for GIS implementation
*  2002/12/17 mdemeest fix PRESSURE RATING & MAX COMPONENT PRESSURE KPA
*  2002/09/30 mdemeest new abap                                        *
************************************************************************



*-----------------------------------------------------------------------
* record to be transferred to AM/FM
*-----------------------------------------------------------------------
TYPES:  BEGIN OF ty_amfm,
              stock_nr(8)       TYPE c,
              copm_type(30)     TYPE c,
              sub_comp(30)      TYPE c,
              material(30)      TYPE c,
              pri_size_nps(30)  TYPE c,
              sec_size_nps(30)  TYPE c,
              pri_wall(30)      TYPE c,
              sec_wall(30)      TYPE c,
              end_type(30)      TYPE c,
              flange_type(30)   TYPE c,
              insulated(3)      TYPE c,
              coating(30)       TYPE c,
              mat_cat(30)       TYPE c,
              mat_grade(30)     TYPE c,
              prspec(60)        TYPE c,
              spec(30)          TYPE c,
              model(30)         TYPE c,
              nr_of_turns(30)   TYPE c,
              pressure(30)      TYPE c,
              manuf(15)         TYPE c,
              comments(40)      TYPE c,
              date(10)          TYPE c,
              user_id(7)        TYPE c,
              stock_gr(3)       TYPE c,
              approved(3)       TYPE c,
              length(30)        TYPE c,
              unique_id(10)     TYPE c,
              mat_flag(2)       TYPE c,
              avg_pr(13)        TYPE c,
              qoh(13)           TYPE c,
              keyword(30)       TYPE c,
              nla_cd(10)        TYPE c,
              nla_dt(30)        TYPE c,
              long_desc         TYPE string,
        END OF ty_amfm.


DATA: ls_amfm       TYPE          ty_amfm,
      lt_amfm       LIKE TABLE OF ls_amfm,
      ls_classlist  TYPE          zamfmchar,
      lt_classlist  LIKE TABLE OF ls_classlist,
      ls_class      TYPE          zamfmclass,
      ls_char       TYPE          zamfmchar.

*-----------------------------------------------------------------------
* Average Price and Quantity Calculations
*-----------------------------------------------------------------------
DATA: BEGIN OF tbl_matnr OCCURS 0,
         matnr         LIKE mara-matnr,
      END OF tbl_matnr.

DATA: BEGIN OF tbl_marc OCCURS 0,
         matnr         LIKE marc-matnr,
         werks         LIKE marc-werks,
      END OF tbl_marc.

DATA: BEGIN OF tbl_mbew OCCURS 0,
         matnr      LIKE mbew-matnr,
         bwkey      LIKE mbew-bwkey,
         lbkum      LIKE mbew-lbkum,            "quantity
         salk3      LIKE mbew-salk3,            "value of the stock
         vprsv      LIKE mbew-vprsv,            "price control S or V
         verpr      LIKE mbew-verpr,            "moving average price
         stprs      LIKE mbew-stprs,            "standard price
         peinh      LIKE mbew-peinh,
      END OF tbl_mbew.

DATA: BEGIN OF tbl_aup_qoh OCCURS 0,
         matnr      LIKE mbew-matnr,
         aup        LIKE mbew-verpr,            "average unit price
         qoh        LIKE mbew-lbkum,            "quantity on hand
      END OF tbl_aup_qoh.

DATA: v_prev_matnr  LIKE mbew-matnr,
      v_total_value LIKE mbew-salk3,
      v_total_qoh   LIKE mbew-lbkum,
      v_total_price LIKE mbew-verpr,
      v_price_count TYPE i.



*DB Lookups
DATA: ls_klah       LIKE klah,
      ls_kssk       LIKE kssk,
      ls_mara       LIKE mara.

*Long text lookup.
DATA: lv_matnr      TYPE tdobname,
      ls_txthead    TYPE thead,
      ls_txtline    TYPE tline,
      lt_txtline    TYPE TABLE OF tline.

*Row counter
DATA: lv_currow     TYPE integer.

*String manipulation
DATA: lv_charloc    TYPE integer,
      lv_number     TYPE p LENGTH 16 DECIMALS 4,
      lv_convert    TYPE n.

*Data output
DATA: msg(80)       TYPE c,
      st_datarec    TYPE string,
      t_data        LIKE TABLE OF st_datarec,
      lv_record_count TYPE i.

*Constants
CONSTANTS: c_delim(1) TYPE c VALUE '^'.


*-----------------------  SELECTION-SCREEN -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK a1.
PARAMETER:    p_amfm   LIKE filename-fileintern LOWER CASE
              DEFAULT '/usr/sap/interfaces/P01/IFMM011/amfm.dat'.
SELECTION-SCREEN END OF BLOCK a1.


*----------------------- START-OF-SELECTION -----------------------------
START-OF-SELECTION.

  PERFORM add_headers.

  SELECT class
    FROM zamfmchar
    INTO CORRESPONDING FIELDS OF TABLE lt_classlist
  .

  SORT lt_classlist BY class.
  DELETE ADJACENT DUPLICATES FROM lt_classlist.

  "Set counter
  lv_currow = 0.

  LOOP AT lt_classlist INTO ls_classlist.

    "Get the Internal Class Number from Class Header
    SELECT clint klart class
      INTO CORRESPONDING FIELDS OF ls_klah
      FROM klah
      WHERE class = ls_classlist-class
        AND klart = '001'
      .

      "Get Object from the Internal Class Number
      SELECT objek mafid klart clint
        INTO CORRESPONDING FIELDS OF ls_kssk
        FROM kssk
        WHERE mafid = 'O'
          AND klart = ls_klah-klart
          AND clint = ls_klah-clint
        .

        "Get the material object
        SELECT SINGLE mara~matnr mara~zzkwrd mara~mstae mara~lvorm mara2~mtart mara2~bmatn mara2~mfrpn mara2~mfrnr
          INTO CORRESPONDING FIELDS OF ls_mara
          FROM mara LEFT OUTER JOIN mara AS mara2 ON mara~matnr = mara2~bmatn
          WHERE mara~matnr = ls_kssk-objek
        .

        IF sy-subrc = 0.
          PERFORM getamfmdata.
        ELSE.
          WRITE:/ '*** ERROR - material', ls_kssk-objek, ' not in table MARA'.
        ENDIF.
      ENDSELECT.                                            "End of KSSK
    ENDSELECT.                                              "End of KLAH
  ENDLOOP.






  IF NOT tbl_matnr[] IS INITIAL.
    PERFORM setup_price_qoh.
  ENDIF.

  "SORT lt_amfm BY stock_nr.

  LOOP AT lt_amfm INTO ls_amfm.
    AT FIRST.
      CONTINUE.
    ENDAT.
    IF ls_amfm-approved = 'Y'.
      READ TABLE tbl_aup_qoh WITH KEY matnr+12(6) = ls_amfm-stock_nr
                                      BINARY SEARCH.
      IF sy-subrc = 0.
        ls_amfm-avg_pr = tbl_aup_qoh-aup.
        ls_amfm-qoh = tbl_aup_qoh-qoh.
      ENDIF.
    ENDIF.

    MODIFY lt_amfm FROM ls_amfm.
  ENDLOOP.








  LOOP AT lt_amfm INTO ls_amfm.

    CLEAR st_datarec.
    CONCATENATE ls_amfm-stock_nr ls_amfm-copm_type ls_amfm-sub_comp ls_amfm-material
                ls_amfm-pri_size_nps ls_amfm-sec_size_nps ls_amfm-pri_wall
                ls_amfm-sec_wall ls_amfm-end_type ls_amfm-flange_type
                ls_amfm-insulated ls_amfm-coating ls_amfm-mat_cat ls_amfm-mat_grade
                ls_amfm-prspec ls_amfm-model ls_amfm-nr_of_turns ls_amfm-pressure
                ls_amfm-manuf ls_amfm-comments ls_amfm-date ls_amfm-user_id
                ls_amfm-stock_gr ls_amfm-approved ls_amfm-length ls_amfm-unique_id
                ls_amfm-mat_flag ls_amfm-avg_pr ls_amfm-qoh ls_amfm-keyword
                ls_amfm-nla_cd ls_amfm-nla_dt ls_amfm-long_desc
      INTO st_datarec SEPARATED BY c_delim.

    APPEND st_datarec TO t_data.
  ENDLOOP.

  DESCRIBE TABLE t_data LINES lv_record_count.
  "v_record_count = v_record_count + 1.            "for header
  SKIP 2.
  WRITE:/ '  AMFM RECORD COUNT =                             ', lv_record_count.



  PERFORM open_amfmfile.

  LOOP AT t_data INTO st_datarec.
    TRANSFER st_datarec TO p_amfm.
  ENDLOOP.

  PERFORM close_amfmfile.
  WRITE:/ 'File Outputed Successfully to: ', p_amfm.

  "END of Program




















*-----------------------  MOVE_INFO ------------------------------------
*  Moves all other info into record to be passed to AM/FM
*-----------------------------------------------------------------------
FORM getamfmdata.

  IF ls_mara-mtart <> 'HERS' AND ls_mara-mtart <> ''.
    "We only want blank mtart (No MFRPN) and MFRPN with HERS
    EXIT.
  ENDIF.

  "Save a list of all the material numbers
  tbl_matnr-matnr = ls_mara-matnr.
  APPEND tbl_matnr.

  CLEAR ls_amfm.

  "Mara Fields
  MOVE ls_mara-matnr+12(6)  TO ls_amfm-stock_nr.  "Material Number
  MOVE ls_mara-zzkwrd       TO ls_amfm-sub_comp.  "Keyword
  MOVE ls_mara-zzkwrd       TO ls_amfm-keyword.   "Keyword
  MOVE ls_mara-mstae        TO ls_amfm-nla_cd.    "Material Status
  MOVE ls_mara-mfrpn        TO ls_amfm-model.     "Model number

  "Blank Fields
  MOVE ''                   TO ls_amfm-date.      "Date
  MOVE ''                   TO ls_amfm-user_id.   "User ID
  MOVE ''                   TO ls_amfm-nla_dt.    "Date

  "Delete Flag
  IF ls_mara-lvorm = 'X'.
    MOVE 'N'                TO ls_amfm-approved.
  ELSE.
    MOVE 'Y'                TO ls_amfm-approved.
  ENDIF.


  "Comments
  SELECT SINGLE maktx
    INTO ls_amfm-comments
    FROM makt
    WHERE matnr = ls_mara-matnr
  .

  "Manufacturer
  IF ls_mara-mfrnr IS NOT INITIAL.
    SELECT SINGLE name1
      INTO ls_amfm-manuf
      FROM lfa1
      WHERE lifnr = ls_mara-mfrnr
    .
  ENDIF.

  "Long description
  CLEAR: ls_txthead, lt_txtline.
  lv_matnr = ls_mara-matnr.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'GRUN'
      language                = 'E'
      name                    = lv_matnr
      object                  = 'MATERIAL'
    IMPORTING
      header                  = ls_txthead
    TABLES
      lines                   = lt_txtline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc = 0.
    CLEAR ls_amfm-long_desc.
    LOOP AT lt_txtline INTO ls_txtline.
      CONCATENATE ls_amfm-long_desc ls_txtline-tdline INTO ls_amfm-long_desc SEPARATED BY ' '.
    ENDLOOP.
    CONDENSE ls_amfm-long_desc.
  ELSE.
    WRITE:/ 'BAD LONG TEXT'.
    WRITE:/ '    material = ', ls_mara-matnr+12(6).
    WRITE:/ '    class    = ', ls_classlist-class.
    WRITE:/ '    keyword  = ', ls_mara-zzkwrd.
    WRITE:/ '    This will NOT be sent to AM/FM'.
    EXIT.
  ENDIF.


  "ZAMFMCLASS
  CLEAR ls_class.
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF ls_class
    FROM zamfmclass
    WHERE class   = ls_classlist-class
      AND keyword = ls_mara-zzkwrd
  .
  IF sy-subrc = 0.
    MOVE ls_class-amfm_component   TO ls_amfm-copm_type.
    MOVE ls_class-amfm_type        TO ls_amfm-stock_gr.
    MOVE ls_class-insul_ind        TO ls_amfm-insulated.
  ELSE.
    "Check wildcard entry.
    CLEAR ls_class.
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF ls_class
      FROM zamfmclass
      WHERE class = ls_classlist-class
        AND keyword = '*'
    .
    IF sy-subrc = 0.
      MOVE ls_class-amfm_component   TO ls_amfm-copm_type.
      MOVE ls_class-amfm_type        TO ls_amfm-stock_gr.
      MOVE ls_class-insul_ind        TO ls_amfm-insulated.
    ELSE.
      WRITE:/ 'ADD TO ZAMFMCLASS'.
      WRITE:/ '    material = ', ls_mara-matnr+12(6).
      WRITE:/ '    class    = ', ls_classlist-class.
      WRITE:/ '    keyword  = ', ls_mara-zzkwrd.
      WRITE:/ '    This will NOT be sent to AM/FM'.
      EXIT.
    ENDIF.
  ENDIF.

  IF ls_class-insul_ind IS INITIAL.
    ls_class-insul_ind = 'N/A'.
  ENDIF.

  "ZAMFMCHAR
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF ls_char
    FROM zamfmchar
    WHERE class = ls_classlist-class
  .

  "Material
  IF ls_char-material IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-material
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-material
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-material = 'N/A'.
  ENDIF.



  "Primary Size
  IF ls_char-pri_sz_nps IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-pri_size_nps
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-pri_sz_nps
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-pri_size_nps = 'N/A'.
  ENDIF.

  "Secondary Size
  IF ls_char-sec_sz_nps IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-sec_size_nps
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-sec_sz_nps
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-sec_size_nps = 'N/A'.
  ENDIF.

  "Primary Wall
  IF ls_char-pri_wall IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-pri_wall
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-pri_wall
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-pri_wall = 'N/A'.
  ENDIF.


  "Secondary Wall
  IF ls_char-sec_wall IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-sec_wall
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-sec_wall
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-sec_wall = 'N/A'.
  ENDIF.

  "End Type
  IF ls_char-end_type IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-end_type
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-end_type
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-end_type = 'N/A'.
  ENDIF.

  "Flange
  IF ls_char-flng IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-flange_type
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-flng
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-flange_type = 'N/A'.
  ENDIF.

  "Coating
  IF ls_char-ctg IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-coating
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-ctg
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-coating = 'N/A'.
  ENDIF.

  "Mat Cat
  IF ls_char-mat_cat IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-mat_cat
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-mat_cat
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-mat_cat = 'N/A'.
  ENDIF.

  "Mat Grade
  IF ls_char-grd IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-mat_grade
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-grd
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-mat_grade = 'N/A'.
  ENDIF.

  "Mat Grade
  IF ls_char-grd IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-mat_grade
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-grd
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-mat_grade = 'N/A'.
  ENDIF.

  "Pressure Rating
  IF ls_char-pressure_rating IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-prspec
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-pressure_rating
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-prspec = ''.
  ENDIF.

  "Specification
  IF ls_char-specification IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-spec
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-specification
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-spec = ''.
  ENDIF.

  "Number of Turns
  IF ls_char-num_turns IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-nr_of_turns
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-num_turns
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-nr_of_turns = 'N/A'.
  ENDIF.


  "Length
  IF ls_char-length IS NOT INITIAL.
    SELECT SINGLE ausp~atwrt
      INTO ls_amfm-length
      FROM cabn INNER JOIN ausp ON cabn~atinn = ausp~atinn
      WHERE cabn~atnam = ls_char-length
        AND ausp~objek = ls_mara-matnr
    .
  ELSE.
    ls_amfm-length = 'N/A'.
  ENDIF.


**************************************************
****Perform string manipulation on the fields.****
**************************************************

  "Material Flag
  CASE ls_amfm-material.
    WHEN 'STEEL'.
      MOVE 'S'  TO ls_amfm-mat_flag.
    WHEN 'PLASTIC'.
      MOVE 'PE'  TO ls_amfm-mat_flag.
    WHEN 'STEEL/PLASTIC'.
      MOVE 'B'   TO ls_amfm-mat_flag.
    WHEN 'STAINLESS STEEL'.
      MOVE 'S'   TO ls_amfm-mat_flag.
    WHEN 'COPPER'.
      MOVE 'S'   TO ls_amfm-mat_flag.
    WHEN 'BRASS'.
      MOVE 'S'   TO ls_amfm-mat_flag.
    WHEN 'CAST IRON'.
      MOVE 'S'   TO ls_amfm-mat_flag.
    WHEN 'CARBON STEEL'.
      MOVE 'S'   TO ls_amfm-mat_flag.
    WHEN 'FORGED STEEL'.
      MOVE 'S'   TO ls_amfm-mat_flag.
    WHEN 'BRONZE'.
      MOVE 'S'   TO ls_amfm-mat_flag.
    WHEN 'GALVANIZED'.
      MOVE 'S'   TO ls_amfm-mat_flag.
    WHEN 'BLACK MALLEABLE IRON'.
      MOVE 'S'   TO ls_amfm-mat_flag.
    WHEN OTHERS.
      MOVE 'F'   TO ls_amfm-mat_flag.
  ENDCASE.

  "Material
  IF ls_amfm-material = 'STAINLESS STEEL'.
    "do nothing
  ELSEIF ls_amfm-material = 'STEEL/PLASTIC'.
    "do nothing
  ELSEIF ls_amfm-material CS 'STEEL'.
    ls_amfm-material = 'STEEL'.
  ELSEIF ls_amfm-material = 'POLYETHYLENE'.
    ls_amfm-material = 'PLASTIC'.
  ELSEIF ls_amfm-material = 'MALLEABLE IRON'.
    ls_amfm-material = 'BLACK MALLEABLE IRON'.
  ELSEIF ls_amfm-material = 'CAST IRON/BRASS'.
    ls_amfm-material = 'BRASS'.
  ELSE.
    "Truncate
    ls_amfm-material = ls_amfm-material(20).
  ENDIF.





  "Primary Size / Secondary Size
  IF ls_amfm-pri_size_nps CA 'X'.
    FIND FIRST OCCURRENCE OF 'X' IN ls_amfm-pri_size_nps MATCH OFFSET lv_charloc.
    "Move past the X
    lv_charloc = lv_charloc + 1.
    ls_amfm-sec_size_nps = ls_amfm-pri_size_nps+lv_charloc.
    "Move before the X
    lv_charloc = lv_charloc - 1.
    ls_amfm-pri_size_nps = ls_amfm-pri_size_nps(lv_charloc).
  ELSE.
    "do nothing
  ENDIF.

  REPLACE ALL OCCURRENCES OF 'IN' IN ls_amfm-pri_size_nps WITH ''.
  REPLACE ALL OCCURRENCES OF 'OD' IN ls_amfm-pri_size_nps WITH ''.
  REPLACE ALL OCCURRENCES OF 'MM' IN ls_amfm-pri_size_nps WITH ''.
  CONDENSE ls_amfm-pri_size_nps.
  ls_amfm-pri_size_nps = ls_amfm-pri_size_nps(10).


  REPLACE ALL OCCURRENCES OF 'IN' IN ls_amfm-sec_size_nps WITH ''.
  REPLACE ALL OCCURRENCES OF 'OD' IN ls_amfm-sec_size_nps WITH ''.
  REPLACE ALL OCCURRENCES OF 'MM' IN ls_amfm-sec_size_nps WITH ''.
  CONDENSE ls_amfm-sec_size_nps.
  ls_amfm-sec_size_nps = ls_amfm-sec_size_nps(10).



  "Primary Wall / Secondary Wall
  IF ls_amfm-pri_wall CA 'X'.
    FIND FIRST OCCURRENCE OF 'X' IN ls_amfm-pri_wall MATCH OFFSET lv_charloc.
    "Move past the X
    lv_charloc = lv_charloc + 1.
    ls_amfm-sec_wall = ls_amfm-pri_wall+lv_charloc.
    "Move before the X
    lv_charloc = lv_charloc - 1.
    ls_amfm-pri_wall = ls_amfm-pri_wall(lv_charloc).
  ELSE.
    "do nothing
  ENDIF.

  REPLACE ALL OCCURRENCES OF 'IN' IN ls_amfm-pri_wall WITH ''.
  REPLACE ALL OCCURRENCES OF 'OD' IN ls_amfm-pri_wall WITH ''.
  REPLACE ALL OCCURRENCES OF 'MM' IN ls_amfm-pri_wall WITH ''.
  CONDENSE ls_amfm-pri_wall.
  ls_amfm-pri_wall = ls_amfm-pri_wall(10).


  REPLACE ALL OCCURRENCES OF 'IN' IN ls_amfm-sec_wall WITH ''.
  REPLACE ALL OCCURRENCES OF 'OD' IN ls_amfm-sec_wall WITH ''.
  REPLACE ALL OCCURRENCES OF 'MM' IN ls_amfm-sec_wall WITH ''.
  CONDENSE ls_amfm-sec_wall.
  ls_amfm-sec_wall = ls_amfm-sec_wall(10).



  "End Type
  REPLACE ALL OCCURRENCES OF 'FLANGED' IN ls_amfm-end_type WITH 'FLG'.
  REPLACE ALL OCCURRENCES OF 'RAISED FACE' IN ls_amfm-end_type WITH 'RF'.
  REPLACE ALL OCCURRENCES OF 'WELDED' IN ls_amfm-end_type WITH 'WE'.
  REPLACE ALL OCCURRENCES OF 'RING TYPE JOINT' IN ls_amfm-end_type WITH 'RTJ'.
  REPLACE ALL OCCURRENCES OF 'THREADED' IN ls_amfm-end_type WITH 'THD'.
  REPLACE ALL OCCURRENCES OF 'COMPRESSION' IN ls_amfm-end_type WITH 'COMP'.
  REPLACE ALL OCCURRENCES OF 'FLAT FACE' IN ls_amfm-end_type WITH 'FF'.
  REPLACE ALL OCCURRENCES OF 'BUTT WELD' IN ls_amfm-end_type WITH 'BW'.
  ls_amfm-end_type = ls_amfm-end_type(15).



  "Flange
  IF ls_amfm-flange_type CS 'RAISED'.
    ls_amfm-flange_type = 'RF'.
  ELSEIF ls_amfm-flange_type CS 'FLAT'.
    ls_amfm-flange_type = 'FF'.
  ELSEIF ls_amfm-flange_type CS 'RING TYPE JOINT'.
    ls_amfm-flange_type = 'RTJ'.
  ELSE.
    REPLACE ALL OCCURRENCES OF 'FLANGED' IN ls_amfm-flange_type WITH ''.
    CONDENSE ls_amfm-flange_type.
    ls_amfm-flange_type = ls_amfm-flange_type(4).
  ENDIF.


  "Coating
  IF ls_amfm-coating = 'YELLOW JACKET'.
    ls_amfm-coating = 'YJ'.
  ELSEIF ls_amfm-coating = 'BLACK'.
    ls_amfm-coating = 'BJ'.
  ELSEIF ls_amfm-coating = 'FUSION BONDED EPOXY'.
    ls_amfm-coating = 'FBE'.
  ELSEIF ls_amfm-coating = 'URETHANE'.
    ls_amfm-coating = 'UR'.
  ELSEIF ls_amfm-coating = 'ABRASIVE FUSION BOND'.
    ls_amfm-coating = 'AFB'.
  ELSEIF ls_amfm-coating = 'BLACK/URETHANE'.
    ls_amfm-coating = 'B/UR'.
  ELSEIF ls_amfm-coating CS 'FUSION BONDED EPOXY'.
    ls_amfm-coating = 'FBE'.
  ELSE.
    ls_amfm-coating = ls_amfm-coating(4).
  ENDIF.



  "Category
  IF ls_amfm-mat_cat = 'N/A'.
    FIND FIRST OCCURRENCE OF 'MAT CAT:' IN ls_amfm-long_desc MATCH OFFSET lv_charloc.
    IF sy-subrc =  0.
      "skip to end of MAT CAT:
      lv_charloc = lv_charloc + 8.
      ls_amfm-mat_cat = ls_amfm-long_desc+lv_charloc.
      "find comma
      FIND FIRST OCCURRENCE OF ',' IN ls_amfm-mat_cat MATCH OFFSET lv_charloc.
      IF sy-subrc = 0.
        ls_amfm-mat_cat = ls_amfm-mat_cat(lv_charloc).
      ENDIF.
    ENDIF.

  ELSE.
    REPLACE ALL OCCURRENCES OF 'CATEGORY' IN ls_amfm-mat_cat WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN ls_amfm-mat_cat WITH ''.
  ENDIF.
  ls_amfm-mat_cat = ls_amfm-mat_cat(7).



  "Grade
  IF ls_amfm-mat_grade = 'N/A'.
    FIND FIRST OCCURRENCE OF 'MATERIAL GR:' IN ls_amfm-long_desc MATCH OFFSET lv_charloc.
    IF sy-subrc = 0.
      "skip to end of MAT CAT:
      lv_charloc = lv_charloc + 12.
      ls_amfm-mat_grade = ls_amfm-long_desc+lv_charloc.
      "find comma
      FIND FIRST OCCURRENCE OF ',' IN ls_amfm-mat_grade MATCH OFFSET lv_charloc.
      IF sy-subrc = 0.
        ls_amfm-mat_grade = ls_amfm-mat_grade(lv_charloc).
      ENDIF.
    ENDIF.
  ELSE.
    REPLACE ALL OCCURRENCES OF 'GRADE' IN ls_amfm-mat_cat WITH ''.
  ENDIF.
  ls_amfm-mat_grade = ls_amfm-mat_grade(3).



  "Pressure Rating
  IF ls_amfm-prspec = ''.
    FIND FIRST OCCURRENCE OF 'PRESSURE RATING:' IN ls_amfm-long_desc MATCH OFFSET lv_charloc.
    IF sy-subrc = 0.
      "skip to end of MAT CAT:
      lv_charloc = lv_charloc + 16.
      ls_amfm-prspec = ls_amfm-long_desc+lv_charloc.
      "find comma
      FIND FIRST OCCURRENCE OF ',' IN ls_amfm-prspec MATCH OFFSET lv_charloc.
      IF sy-subrc = 0.
        ls_amfm-prspec = ls_amfm-prspec(lv_charloc).
      ENDIF.
    ENDIF.
  ENDIF.

  "Check again incase above populated it.
  IF ls_amfm-prspec <> ''.
    IF ls_amfm-prspec CS 'KPA'.
      ls_amfm-pressure = ls_amfm-prspec.
      ls_amfm-prspec = ''.
    ENDIF.
  ENDIF.

  CONDENSE ls_amfm-pressure.
  CONDENSE ls_amfm-prspec.



  "Specification
  IF ls_amfm-spec = ''.
    FIND FIRST OCCURRENCE OF 'Specification:' IN ls_amfm-long_desc MATCH OFFSET lv_charloc.
    IF sy-subrc = 0.
      "skip to end of MAT CAT:
      lv_charloc = lv_charloc + 16.
      ls_amfm-spec = ls_amfm-long_desc+lv_charloc.
      "find comma
      FIND FIRST OCCURRENCE OF ',' IN ls_amfm-spec MATCH OFFSET lv_charloc.
      IF sy-subrc = 0.
        ls_amfm-spec = ls_amfm-spec(lv_charloc).
      ENDIF.
    ENDIF.
  ENDIF.
  CONDENSE ls_amfm-spec.


  CONCATENATE ls_amfm-prspec ls_amfm-spec INTO ls_amfm-prspec SEPARATED BY ' '.
  CONDENSE ls_amfm-prspec.



  "Number of Turns
  REPLACE ALL OCCURRENCES OF 'TURNS' IN ls_amfm-nr_of_turns WITH ''.
  CONDENSE ls_amfm-nr_of_turns.
  ls_amfm-nr_of_turns = ls_amfm-nr_of_turns(10).



  "Length
  IF ls_amfm-length CS 'IN'.
    REPLACE ALL OCCURRENCES OF 'IN' IN ls_amfm-length WITH ''.
    REPLACE ALL OCCURRENCES OF '1/2' IN ls_amfm-length WITH '.5'.
    CONDENSE ls_amfm-length NO-GAPS.


    "Move length to 4 decimal places
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_amfm-length
      IMPORTING
        output = lv_convert.
    IF sy-subrc <> 0.
      CONCATENATE ls_amfm-length 'ERR' INTO ls_amfm-length.
    ELSE.
      lv_number = lv_convert * '0.0254' .
      ls_amfm-length = lv_number.
    ENDIF.
  ENDIF.






  lv_currow = lv_currow + 1.
  ls_amfm-unique_id = lv_currow.




  "Add to the table
  APPEND ls_amfm TO lt_amfm.
ENDFORM.                    "getamfmdata



*&---------------------------------------------------------------------*
*&      Form  setup_price_qoh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM setup_price_qoh .

  REFRESH: tbl_marc, tbl_mbew, tbl_aup_qoh.

  SORT tbl_matnr.
  DELETE ADJACENT DUPLICATES FROM tbl_matnr.

  SELECT marc~matnr marc~werks
         INTO TABLE tbl_marc
         FROM marc INNER JOIN t001k
           ON marc~werks = t001k~bwkey
         FOR ALL entries IN tbl_matnr
         WHERE marc~matnr = tbl_matnr-matnr
           AND t001k~bukrs = 'UGL'
           AND marc~lvorm = ' '.

  SORT tbl_marc.

  IF NOT tbl_marc[] IS INITIAL.
    SELECT matnr bwkey lbkum salk3 vprsv verpr stprs peinh
                INTO TABLE tbl_mbew
                FROM mbew
                FOR ALL ENTRIES IN tbl_marc
                WHERE matnr = tbl_marc-matnr
                  AND bwkey = tbl_marc-werks
                  AND lvorm = ' '.

    SORT tbl_mbew BY matnr bwkey.
  ENDIF.

  CLEAR: v_prev_matnr, v_total_value, v_total_qoh,
         v_total_price, v_price_count.

  LOOP AT tbl_mbew.
    IF sy-tabix = 1.                        "first table entry
      v_prev_matnr = tbl_mbew-matnr.
      v_total_value = tbl_mbew-salk3.
      v_total_qoh = tbl_mbew-lbkum.
      IF tbl_mbew-vprsv = 'S'.              "standard price
        v_total_price = tbl_mbew-stprs.
      ELSE.
        v_total_price = tbl_mbew-verpr.       "moving average price
      ENDIF.
      v_price_count = 1.
      CONTINUE.
    ENDIF.
    IF v_prev_matnr <> tbl_mbew-matnr.        "new material
      tbl_aup_qoh-matnr = v_prev_matnr.
      tbl_aup_qoh-qoh = v_total_qoh.
      IF v_total_value <> 0 AND v_total_qoh <> 0.
        tbl_aup_qoh-aup = v_total_value / v_total_qoh.
      ELSE.
        tbl_aup_qoh-aup = v_total_price / v_price_count.
      ENDIF.
      APPEND tbl_aup_qoh.
      v_prev_matnr = tbl_mbew-matnr.
      CLEAR: v_total_value, v_total_qoh, v_total_price, v_price_count.
    ENDIF.

    v_total_value = v_total_value + tbl_mbew-salk3.
    v_total_qoh = v_total_qoh + tbl_mbew-lbkum.

    IF tbl_mbew-vprsv = 'S'.
      v_total_price = v_total_price + tbl_mbew-stprs.
    ELSE.
      v_total_price = v_total_price + tbl_mbew-verpr.
    ENDIF.
    v_price_count = v_price_count + 1.

  ENDLOOP.

  tbl_aup_qoh-matnr = v_prev_matnr.
  tbl_aup_qoh-qoh = v_total_qoh.
  IF v_total_value <> 0 AND v_total_qoh <> 0.
    tbl_aup_qoh-aup = v_total_value / v_total_qoh.
  ELSE.
    tbl_aup_qoh-aup = v_total_price / v_price_count.
  ENDIF.
  APPEND tbl_aup_qoh.

ENDFORM.                    " SETUP_PRICE_QOH





















*----------------------------------------------------------------------*
FORM add_headers.
  CLEAR ls_amfm.

  MOVE 'STOCKNUM'                       TO ls_amfm-stock_nr.
  MOVE 'COMPONENT_TYPE/SAPCLASS'        TO ls_amfm-copm_type.
  MOVE 'SUB_COMPONENT/SAPGIS_KEYWORD'   TO ls_amfm-sub_comp.
  MOVE 'MATERIAL'                       TO ls_amfm-material.
  MOVE 'PRI_SZ_NPS'                     TO ls_amfm-pri_size_nps.
  MOVE 'SEC_SZ_NPS'                     TO ls_amfm-sec_size_nps.
  MOVE 'PRI_WALL'                       TO ls_amfm-pri_wall.
  MOVE 'SEC_WALL'                       TO ls_amfm-sec_wall.
  MOVE 'END_TYPE'                       TO ls_amfm-end_type.
  MOVE 'FLNG'                           TO ls_amfm-flange_type.
  MOVE 'I'                              TO ls_amfm-insulated.
  MOVE 'CTG'                            TO ls_amfm-coating.
  MOVE 'MAT_CAT'                        TO ls_amfm-mat_cat.
  MOVE 'GRD'                            TO ls_amfm-mat_grade.
  MOVE 'PRESSURE_RATING+SPECIFICATION'  TO ls_amfm-prspec.
  MOVE 'MODEL_NUMBER'                   TO ls_amfm-model.
  MOVE 'NUM_TURNS'                      TO ls_amfm-nr_of_turns.
  MOVE 'PRESS'                          TO ls_amfm-pressure.
  MOVE 'MANUFACTURER'                   TO ls_amfm-manuf.
  MOVE 'COMMENTS_SAPSHORT DESCRIP'      TO ls_amfm-comments.
  MOVE 'DATE_STAMP'                     TO ls_amfm-date.
  MOVE 'USERID'                         TO ls_amfm-user_id.
  MOVE 'G'                              TO ls_amfm-stock_gr.
  MOVE 'A'                              TO ls_amfm-approved.
  MOVE 'LENGTH_M'                       TO ls_amfm-length.
  MOVE 'UNIQUEID'                       TO ls_amfm-unique_id.
  MOVE 'MF'                             TO ls_amfm-mat_flag.
  MOVE 'AVGPRICE'                       TO ls_amfm-avg_pr.
  MOVE 'QOH'                            TO ls_amfm-qoh.
  MOVE 'SAPKEYWORD'                     TO ls_amfm-keyword.
  MOVE 'NLA_CODE'                       TO ls_amfm-nla_cd.
  MOVE 'NLA_DATE'                       TO ls_amfm-nla_dt.
  MOVE 'LONG_DESC'                      TO ls_amfm-long_desc.

  APPEND ls_amfm TO lt_amfm.

ENDFORM.                    "add_headers



*----------------------------------------------------------------------*
FORM open_amfmfile.
  OPEN DATASET p_amfm FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_amfm msg.
    STOP.
  ENDIF.
ENDFORM.                    "OPEN_CSVFILE


*----------------------------------------------------------------------*
FORM close_amfmfile.
  CLOSE DATASET p_amfm.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' p_amfm msg.
    STOP.
  ENDIF.
ENDFORM.                    "CLOSE_ALL_FILES
