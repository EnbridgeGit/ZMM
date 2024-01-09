REPORT ZMWMI009.
*----------------------------------------------------------------------*
* CHANGES
* 2005/01/31 mdemeest Requested by Sherri Burm                         *
*                     Material numbers flagged as deleted should not   *
*                     be passed to the CARS application                *
*----------------------------------------------------------------------*
* abap to create construction material master for CARS validation      *
*----------------------------------------------------------------------*
* 2007/09/26 TR277 mdemeest - change in selection criteria (mstae)
* 2002/02/01 CARS  mdemeest - Original abap
*----------------------------------------------------------------------*
TABLES:
       MARA,
       MARC,
       MAKT,
       mard.

*--> plant record to be downloaded to CARS
DATA:
    OUTFILE(100).

data: begin of warec  occurs 3000,
          MATNR          LIKE MARC-MATNR, "Material Number
          meins          LIKE MARa-meins, "Issuing uom
          MAKTX          LIKE MAKT-MAKTX, "Description
       end of warec.


SELECTION-SCREEN  BEGIN OF BLOCK BLOCK1 with frame.
parameter:  p_file(100) type c lower case obligatory default
             '/usr/sap/interfaces/P01/IFMM005/carsmaterial.sap'.
SELECTION-SCREEN  END OF BLOCK BLOCK1.

skip 1.

selection-screen begin of block block4 with frame title text-102.
select-options:
            s_mstae for mara-mstae no intervals. "X-lznt status
selection-screen end of block block4.
skip 1.

SELECTION-SCREEN  BEGIN OF BLOCK BLOCK2 with frame title text-100.
select-options:
            S_werks for mard-werks obligatory.   "Main storage locations
SELECTION-SCREEN  END OF BLOCK BLOCK2.

skip 1.

SELECTION-SCREEN  BEGIN OF BLOCK BLOCK3 with frame title text-101.
select-options:
             S_werks1 for mard-werks.              "Contractor locations
SELECTION-SCREEN  END OF BLOCK BLOCK3.


*--> output files

START-OF-SELECTION.
* open output files
*-----------------------------------------------------------------------
  outfile = p_file.
  OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE.
  IF  SY-SUBRC NE '0'.
    WRITE: 'open error.  return code=', SY-SUBRC.
  ENDIF.
*-----------------------------------------------------------------------
  PERFORM SET-UP-PLANT.
  Perform set-up-contractor.

  perform get-description.

  sort warec by matnr.

  perform write-file.
                         .
END-OF-SELECTION.
  CLOSE DATASET OUTFILE.


*---------------------------  SET-UP-PLANT  ----------------------------
* select plant information from MARC creating unique material numbers
*-----------------------------------------------------------------------
FORM SET-UP-PLANT.
  SELECT * FROM MARC
    WHERE WERKS in s_werks.
    MOVE MARC-MATNR      TO waREC-matnr.
    select single * from mara
        where matnr = marc-matnr
          and mstae in s_mstae.
        if sy-subrc = '0'.
           WRITE MARA-meins   TO waREC-meins. "Translates it to English
           write: / marc-matnr, marc-werks, 'Plant'.
           collect warec.
        endif.
  ENDSELECT.

endform.

*------------------------  SET-UP-CONTRACTOR  --------------------------
*  Selects all materials from storage locations where the storage
*  location is defined by "E".
*-----------------------------------------------------------------------
FORM SET-UP-CONTRACTOR.

  SELECT * FROM MARD
    WHERE WERKS in s_werks1
      and lgort like 'E%'.
                              .
    MOVE MARD-MATNR      TO waREC-matnr.
    select single * from mara
        where matnr = mard-matnr
          and mstae in s_mstae.
        if sy-subrc = '0'.
           WRITE MARA-meins   TO waREC-meins. "Translates it to English
           write: / mard-matnr, mard-werks, mard-lgort, 'Storage Loc'.
           collect warec.
        endif.
  ENDSELECT.
endform.

*--------------------------  GET-DESCRIPTION ---------------------------
*  Gets description and adds it to the internal table.
*-----------------------------------------------------------------------
form get-description.
  loop at warec.
    select single * from makt
      where matnr = warec-matnr
        and spras = sy-langu.
    if sy-subrc = '0'.
       move makt-maktx  to warec-maktx.
    else.
       move 'DESCRIPTION NOT AVAILABLE' to warec-maktx.
    endif.
    modify warec.
  endloop.
endform.

*---------------------------  WRITE-FILE -------------------------------
*   Write flat file
*-----------------------------------------------------------------------
form write-file.
  loop at warec.
     transfer warec to outfile length 62.
  endloop.
endform.
