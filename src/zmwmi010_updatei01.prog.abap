*----------------------------------------------------------------------*
***INCLUDE MZMM_SLOC_UPDATEI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_ALL  INPUT
*&---------------------------------------------------------------------*
*       This modules includes all the user commands for this dialog.
*----------------------------------------------------------------------*
MODULE USER_COMMAND_ALL INPUT.
data: answer(1).

CASE OKCODE.
 WHEN 'DISPLAY'.
  PERFORM GET_DATA.
  perform find_position.  "ml
  Set screen 200.
 WHEN 'NEW'.
CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
 EXPORTING
  DEFAULTOPTION       = 'Y'
  TEXTLINE1           = 'Adding new entries will eliminate any changes'
  TEXTLINE2           = 'to bin locations you have not saved. Continue?'
  TITEL               = 'Any unsaved changes will be lost'
*   START_COLUMN        = 25
*   START_ROW           = 6
 IMPORTING
  ANSWER              = answer
          .
   if answer <> 'J'. "Continue code (YES)
    clear okcode.
    exit.
   endif.

  Clear itab.
  Refresh itab.
  Set screen 300.

 WHEN 'SAVE'.
  PERFORM SAVE_ITAB.
 WHEN 'BACK0100'.     "Not used
  set screen 0.
 WHEN 'BACK0200'.     "Not used
  clear: mard, itab.
  refresh itab.
  set screen '0100'.
 WHEN 'BACK0201'.
  set screen 200.
  clear p_matnr.   "ML added 03/19
 WHEN 'EXIT'.         "Not used
  clear p_matnr.   "ML add 03/19
  set screen 0.

 WHEN 'DELETE'.
  perform remove_lines.


 WHEN 'FIND'.
  Set Screen 201.
 WHEN 'POSITION'.
  PERFORM FIND_POSITION.
  Set Screen 200.

"Page up/down functionality
 WHEN 'P--'.
      CLEAR OKCODE.
      PERFORM PAGING USING 'P--'.
 WHEN 'P-'.
      CLEAR OKCODE.
      PERFORM PAGING USING 'P-'.
 WHEN 'P+'.
      CLEAR OKCODE.
      PERFORM PAGING USING 'P+'.
 WHEN 'P++'.
      CLEAR OKCODE.
      PERFORM PAGING USING 'P++'.
 WHEN OTHERS.

ENDCASE.

clear okcode.
ENDMODULE.                 " USER_COMMAND_ALL  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_LINE_COUNT  INPUT
*&---------------------------------------------------------------------*
*       Sets the scrolling line count.
*----------------------------------------------------------------------*
MODULE SET_LINE_COUNT INPUT.
LINE_COUNT = SY-LOOPC.
*tc_storagebin-lines = tc_storagebin-lines + 1.
ENDMODULE.                 " SET_LINE_COUNT  INPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATES  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE UPDATES INPUT.

  MODIFY ITAB INDEX TC_STORAGEBIN-CURRENT_LINE.

ENDMODULE.                 " UPDATES  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PLANT_AUTHORIZATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_PLANT_AUTHORIZATION INPUT.
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
                 ID 'ACTVT' FIELD '02'
                 ID 'WERKS' FIELD P_WERKS.
  if sy-subrc <> 0.
   Message E368(00) with
     'You have no authorization for plant'(001)
     P_WERKS.
  endif.

ENDMODULE.                 " CHECK_PLANT_AUTHORIZATION  INPUT
*&---------------------------------------------------------------------*
*&      Module  CANCEL100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CANCEL100 INPUT.
Set Screen 0.
Leave screen.
ENDMODULE.                 " CANCEL100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CANCEL200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CANCEL200 INPUT.

tc_storagebin-top_line = 1.
leave to screen 100.

"There were problems setting the line items for display so I
"decided to call the transaction again to reinitialize!
*call transaction 'Z_STORAGEBINLOCATION'.  "ML


*  clear: mard, itab.
**  clear  TC_STORAGEBIN-CURRENT_LINE.
*
*" Clearing the lines integer for the table control was added as there
*" was sporadic problems with this indicator resulting in an error where
*" only one line item was being displayed after saving and retrying
*" display of other materials.
** clear TC_STORAGEBIN-LINES.
*  REFRESH CONTROL 'TC_STORAGEBIN' FROM SCREEN '0200'.
*
*  refresh itab.
**  TC_STORAGEBIN-TOP_LINE = 1. "Set to top line for next display.
*Set screen 100.
*Leave screen.
ENDMODULE.                 " CANCEL200  INPUT
*&---------------------------------------------------------------------*
*&      Module  DELETE_LINES  INPUT
*&---------------------------------------------------------------------*
*       Deletes line items that are not required by user
*----------------------------------------------------------------------*
MODULE DELETE_LINES INPUT.

 IF OKCODE = 'DELETE'.
 check SEL = 'X'.
  itab-message = 'DELETELINE'.
  Modify itab INDEX TC_STORAGEBIN-CURRENT_LINE.

 Endif.

ENDMODULE.                 " DELETE_LINES  INPUT
*&---------------------------------------------------------------------*
*&      Module  DELETE_LINES2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DELETE_LINES2 INPUT.
 IF OKCODE = 'DELETE'.
 check SEL = 'X'.
  itab-message = 'DELETELINE'.
  Modify itab INDEX TC_STORAGEBIN2-CURRENT_LINE.

 Endif.

ENDMODULE.                 " DELETE_LINES2  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_AND_UPDATE_LINE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_AND_UPDATE_LINE INPUT.

* Transfer screen data to ITAB fields
ITAB-WERKS     =  MARD-WERKS     . "Plant
ITAB-MATNR     =  MARD-MATNR    .  "Material Number
ITAB-LGORT     =  MARD-LGORT    .  "Storage location
ITAB-LGPBE     =  MARD-LGPBE    .  "Storage bin location
ITAB-NEW_LGPBE =  NEW_LGPBE.       "New Storage bin location
*  MODIFY ITAB INDEX TC_STORAGEBIN2-CURRENT_LINE.



  select single * from MARD
   where MATNR = itab-matnr
     and WERKS = itab-werks
     and LGORT = itab-lgort.

  if sy-subrc = 0.
   move MARD-LGPBE to itab-LGPBE.
*   Modify itab INDEX TC_STORAGEBIN2-CURRENT_LINE.
*  else.
*   exit.
  endif.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
                 ID 'ACTVT' FIELD '02'
                 ID 'WERKS' FIELD ITAB-WERKS.
  if sy-subrc <> 0.
  Move 'No Authorization' to itab-message.
  endif.

append itab.
*  Modify itab INDEX TC_STORAGEBIN2-CURRENT_LINE.


ENDMODULE.                 " CHECK_AND_UPDATE_LINE  INPUT
*&---------------------------------------------------------------------*
*&      Module  refresh_itab  INPUT
*&---------------------------------------------------------------------*
*       Clears ITAB to avoid duplicate entries with append function
*----------------------------------------------------------------------*
MODULE refresh_itab INPUT.
Clear itab.
refresh itab.
ENDMODULE.                 " refresh_itab  INPUT
