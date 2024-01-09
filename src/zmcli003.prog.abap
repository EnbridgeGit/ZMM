REPORT ZMCLI003 LINE-COUNT 65 LINE-SIZE 170 MESSAGE-ID ZM.
* add insulated char (1) to zamfmclass.
************************************************************************
*  Author:   M De Meester
*  Brief Description:
*  - Produce file of select material numbers with requested
*    characteristics
***********************************************************************
*  2005/09/19 mdemeest new abap                                        *
************************************************************************
tables:  ent1027,                 "View MARA/MAKT
         cabn,                    "Characteristics
         ausp,                    "Materials with Characteristic
         kssk,                    "Materials with Class #
         klah.                    "Class Masters
*-----------------------------------------------------------------------
data:   outfile(1000)  type c.

*-----------------------------------------------------------------------


*-----------------------  SELECTION-SCREEN -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
select-options: s_matnr   for ent1027-matnr obligatory,
                s_atnam    for cabn-atnam no intervals,
                s_atinn   for ausp-atinn no intervals.

SELECTION-SCREEN ULINE.
PARAMETER:     p_out1    LIKE FILENAMECI-FILEEXTERN
                 DEFAULT '/usr/sap/interfaces/P01/IFMM011/zmcli003_01'.
SELECTION-SCREEN END OF BLOCK BOX.

*-----------------------------------------------------------------------
* This routine takes the English name of the characteristic and
* translates it into the SAP Characteristic object number.
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.

start-of-selection.
  open dataset p_out1 for output in text mode.

*-----------------------------------------------------------------------
* setting up spreadsheet column headers & translating characteristics
* into object numbers.
*-----------------------------------------------------------------------
  clear outfile.
  concatenate 'Material' 'Description' 'Class' into outfile
                                                       separated by ','.

  refresh s_atinn.
  select * from cabn
      where atnam in s_atnam.
    s_atinn-sign = 'I'.
    s_atinn-option = 'EQ'.
    s_atinn-low    = cabn-atinn.
    append s_atinn.
    concatenate outfile cabn-atnam into outfile separated by ','.
  endselect.

  transfer outfile to p_out1 length 1000.

  select * from ent1027
      where matnr in s_matnr
        and lvorm = ' '.                    "Select only valid materials
      translate ent1027-maktx using ', '.   "Eliminate commas for excel

data: ausp_matnr  like ausp-objek.
     move ent1027-matnr to ausp_matnr.
     clear outfile.
     concatenate ent1027-matnr ent1027-maktx into outfile
                 separated by ','.
     perform get_class.
     loop at s_atinn.
       select  single * from ausp
          where objek = ausp_matnr
            and atinn = s_atinn+3(10).
          if sy-subrc = '0'.
             if ausp-atwrt = space.
data: atflv_char(15) type c.
data: atflv_num(10)  type p  decimals 3.
                move ausp-atflv to atflv_num.
                move atflv_num to atflv_char.
                translate atflv_char using ', '.
                concatenate outfile atflv_char into outfile
                                    separated by ','.
             else.
                translate ausp-atwrt using ', '.
                concatenate outfile ausp-atwrt into outfile
                                    separated by ','.
             endif.
          else.
           concatenate outfile ' ' into outfile separated by ','.
          endif.
     endloop.
  transfer outfile to p_out1 length 1000.

  endselect.

  close dataset p_out1.

*-----------------------------------------------------------------------



CLOSE DATASET: p_out1.



form get_class.
    select single * from kssk
        where objek = ausp_matnr
          and mafid = 'O'
          and klart = '001'.
    if sy-subrc = '0'.
       select single * from klah
           where clint = kssk-clint.
       concatenate outfile klah-class into outfile separated by ','.
    else.
       concatenate outfile ' ' into outfile separated by ','.
    endif.
endform.
