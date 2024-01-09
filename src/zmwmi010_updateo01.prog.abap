*----------------------------------------------------------------------*
***INCLUDE MZMM_SLOC_UPDATEO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       Transfers ITAB data to the screen fields
*----------------------------------------------------------------------*
MODULE DISPLAY_DATA OUTPUT.

* Transfer ITAB data to screen fields in table control
MARD-WERKS     =  ITAB-WERKS    .  "Plant
MARD-MATNR     =  ITAB-MATNR    .  "Material Number
MARD-LGORT     =  ITAB-LGORT    .  "Storage location
MARD-LGPBE     =  ITAB-LGPBE    .  "Storage bin location
NEW_LGPBE      =  ITAB-NEW_LGPBE.  "New Storage bin location
MAKT-MAKTX     =  ITAB-MAKTX.      "Material Description

*tc_storagebin-lines = ( tc_storagebin-lines + 2 ) / 2 + 1.
*tc_storagebin-lines =  tc_storagebin-lines + 1.

ENDMODULE.                 " DISPLAY_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Sets the status for screen 0100 and refreshes global data
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_ALL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       Sets the status for screen 0200 and refreshes global data
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*
*       Sets the status for screen 0201
*----------------------------------------------------------------------*
MODULE STATUS_0201 OUTPUT.
  SET PF-STATUS '0201'.
  SET TITLEBAR '201'.

ENDMODULE.                 " STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       Sets the status for screen 0300
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS '0300'.
  SET TITLEBAR '300'.

ENDMODULE.                 " STATUS_0300  OUTPUT
