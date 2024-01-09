*  All changes indicated with "UGL
*  2009/03/13 mdemeest Copied from the west SAPZMMM_SLOC_UPDATE
*                      Added Material Description on screen 0200
*                      Everything renamed to ZMWMI010 to match the
*                      East's naming convention.  Add verbage added
*                      on some screens per D. Bossy
*                      Screens used are: 0100, 0200, 0201
*
*&---------------------------------------------------------------------*
*& Module pool       ZMWMI010   SAPMZMM_SLOC_UPDATE                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
* Original Transport req. C11K912244

************************************************************************
* Program      : SAPMZMM_SLOC_UPDATE                                   *
* Author       : Dave Playfair, Coral Technology Services Inc.         *
* Date Created : 2001/10/24                                            *
* Function     : Summary of functionality                              *
* This program is used to update storage locations for individual      *
* materials per plant. This is executed online via transaction         *
* ZSLOC_UPDATE and restricts usage based on security parameters, thus  *
* only users assigned to a given plant are able to update material     *
* locations in their plant
************************************************************************
* Program Maintenance History                                          *
*----------------------------------------------------------------------*
* Date Changed | Developer       | Description Of Change               *
*----------------------------------------------------------------------*
*  2013/05/07  | Praveena Anusuri| SDP47969   TR D30K921902            *
*                                | Record the change history of the    *
*                                | Material master                     *
*----------------------------------------------------------------------*
************************************************************************
* INCLUDE MZMM_SLOC_UPDATETOP.                                "UGL
   .
* INCLUDE MZMM_SLOC_UPDATEI01.                                "UGL

* INCLUDE MZMM_SLOC_UPDATEO01.                                "UGL

* INCLUDE MZMM_SLOC_UPDATEF01.                                "UGL

INCLUDE ZMWMI010_UPDATETOP.                                   "UGL

INCLUDE ZMWMI010_UPDATEI01.                                   "UGL

INCLUDE ZMWMI010_UPDATEO01.                                   "UGL

INCLUDE ZMWMI010_UPDATEF01.                                   "UGL
