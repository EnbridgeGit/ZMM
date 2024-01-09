*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(SEGMENT) LIKE  EDIDD STRUCTURE  EDIDD OPTIONAL
*"  TABLES
*"      I_CONTROL STRUCTURE  EDIDC OPTIONAL
*"      I_DATA STRUCTURE  EDIDD OPTIONAL
*"      I_STATUS STRUCTURE  BDIDOCSTAT OPTIONAL
*"      C_EKPO STRUCTURE  EKPO OPTIONAL
*"      IT_FEKPO STRUCTURE  CONFEKPO OPTIONAL
*"  CHANGING
*"     VALUE(ERROR) OPTIONAL
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&  Include           ZXM06U19
*&---------------------------------------------------------------------*
DATA: wa_e1edk02    TYPE e1edk02,
      wa_e1edkt1    TYPE e1edkt1,
      wa_e1edkt2    TYPE e1edkt2,
      belnr         LIKE thead-tdname,
      wa_txtheader  TYPE thead,
      wa_lines      TYPE TABLE OF tline,
      wa_newline    LIKE LINE OF wa_lines,
      lv_count      type i.

IF i_control-mestyp EQ 'ORDRSP'.




  IF segment-segnam EQ 'E1EDKT1'.
    wa_e1edkt1 = segment-sdata.
    IF wa_e1edkt1-tdid = 'F17'. "EAST F17 West F08


      LOOP AT i_data.
        IF i_data-segnam = 'E1EDK02'.

          wa_e1edk02 = i_data-sdata.
          IF wa_e1edk02-qualf = '001'.
            belnr = wa_e1edk02-belnr.
            EXIT.
          ENDIF.

        ENDIF.
      ENDLOOP.

      CLEAR: wa_txtheader, wa_lines.


      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*           CLIENT                        = SY-MANDT
          id                            = wa_e1edkt1-tdid
          language                      = wa_e1edkt1-tsspras(1)
          name                          = belnr
          object                        = 'EKKO'
*           ARCHIVE_HANDLE                = 0
*           LOCAL_CAT                     = ' '
       IMPORTING
         header                         = wa_txtheader
       TABLES
         lines                          = wa_lines
       EXCEPTIONS
           id                            = 1
           language                      = 2
           name                          = 3
           not_found                     = 4
           object                        = 5
           reference_check               = 6
           wrong_access_to_archive       = 7
           OTHERS                        = 8
                .
      IF sy-subrc = 0.
        "EDIT
        LOOP AT i_data.
          IF i_data-segnam = 'E1EDKT2' AND i_data-psgnum = segment-segnum.

            wa_e1edkt2 = i_data-sdata.
            wa_newline-tdformat = wa_e1edkt2-tdformat.
            wa_newline-tdline = wa_e1edkt2-tdline.
            APPEND wa_newline  TO wa_lines.

          ENDIF.
        ENDLOOP.


        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
*           CLIENT                = SY-MANDT
            header                = wa_txtheader
*           INSERT                = ' '
            savemode_direct       = 'X'
*           OWNER_SPECIFIED       = ' '
*           LOCAL_CAT             = ' '
*         IMPORTING
*           function              =
*           NEWHEADER             =
          TABLES
            lines                 = wa_lines
          EXCEPTIONS
            id                    = 1
            language              = 2
            name                  = 3
            object                = 4
            OTHERS                = 5
                  .
        IF sy-subrc <> 0.
          "ERROR Edit then SAVE
        ENDIF.





      ELSEIF sy-subrc = 4.
        "CREATE

        wa_txtheader-tdobject = 'EKKO'.
        wa_txtheader-tdname   = belnr.
        wa_txtheader-tdid     = wa_e1edkt1-tdid.
        wa_txtheader-tdspras  = wa_e1edkt1-tsspras(1).


        LOOP AT i_data.
          IF i_data-segnam = 'E1EDKT2' AND i_data-psgnum = segment-segnum.

            wa_e1edkt2 = i_data-sdata.
            wa_newline-tdformat = wa_e1edkt2-tdformat.
            wa_newline-tdline = wa_e1edkt2-tdline.
            APPEND wa_newline  TO wa_lines.

          ENDIF.
        ENDLOOP.


        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
*           CLIENT                = SY-MANDT
            header                = wa_txtheader
            insert                = 'X'
            savemode_direct       = 'X'
*           OWNER_SPECIFIED       = ' '
*           LOCAL_CAT             = ' '
*         IMPORTING
*           function              =
          TABLES
            lines                 = wa_lines
          EXCEPTIONS
            id                    = 1
            language              = 2
            name                  = 3
            object                = 4
            OTHERS                = 5
                  .
        IF sy-subrc <> 0.
          "ERROR Create then SAVE
        ENDIF.
      ELSE.
        "ERROR READ
      ENDIF.

    ENDIF.
  ENDIF.



  DATA: wa_e1edp02    TYPE e1edp02,
        wa_e1edpt1    TYPE e1edpt1,
        wa_e1edpt2    TYPE e1edpt2,
        linenum       LIKE e1edp02-zeile,
        documentnum   LIKE thead-tdname.

  IF segment-segnam EQ 'E1EDPT1'.
    wa_e1edpt1 = segment-sdata.
    IF wa_e1edpt1-tdid = 'F06'. "EAST F06 West F11


      LOOP AT i_data.
        IF i_data-segnam = 'E1EDP02' AND i_data-psgnum = segment-psgnum.
          wa_e1edp02 = i_data-sdata.
          IF wa_e1edp02-qualf = '001'.
            linenum = wa_e1edp02-zeile.
            SHIFT linenum LEFT DELETING LEADING '0'.

            lv_count = STRLEN( linenum ).

            lv_count = 5 - lv_count.

            do lv_count times.
              CONCATENATE '0' linenum into linenum.
            enddo.

            belnr   = wa_e1edp02-belnr.
            CONCATENATE belnr linenum INTO documentnum.
            EXIT.
          ENDIF.

        ENDIF.
      ENDLOOP.

      CLEAR: wa_txtheader, wa_lines.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*           CLIENT                        = SY-MANDT
          id                            = wa_e1edpt1-tdid
          language                      = wa_e1edpt1-tsspras(1)
          name                          = documentnum
          object                        = 'EKPO'
*           ARCHIVE_HANDLE                = 0
*           LOCAL_CAT                     = ' '
       IMPORTING
         header                         = wa_txtheader
       TABLES
         lines                          = wa_lines
       EXCEPTIONS
           id                            = 1
           language                      = 2
           name                          = 3
           not_found                     = 4
           object                        = 5
           reference_check               = 6
           wrong_access_to_archive       = 7
           OTHERS                        = 8
                .
      IF sy-subrc = 0.
        "EDIT
        LOOP AT i_data.
          IF i_data-segnam = 'E1EDPT2' AND i_data-psgnum = segment-segnum.

            wa_e1edpt2 = i_data-sdata.
            wa_newline-tdformat = wa_e1edpt2-tdformat.
            wa_newline-tdline = wa_e1edpt2-tdline.
            APPEND wa_newline  TO wa_lines.

          ENDIF.
        ENDLOOP.


        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
*           CLIENT                = SY-MANDT
            header                = wa_txtheader
*           INSERT                = ' '
            savemode_direct       = 'X'
*           OWNER_SPECIFIED       = ' '
*           LOCAL_CAT             = ' '
*         IMPORTING
*           function              =
*           NEWHEADER             =
          TABLES
            lines                 = wa_lines
          EXCEPTIONS
            id                    = 1
            language              = 2
            name                  = 3
            object                = 4
            OTHERS                = 5
                  .
        IF sy-subrc <> 0.
          "ERROR Edit then SAVE
        ENDIF.





      ELSEIF sy-subrc = 4.
        "CREATE

        wa_txtheader-tdobject = 'EKPO'.
        wa_txtheader-tdname   = documentnum.
        wa_txtheader-tdid     = wa_e1edpt1-tdid.
        wa_txtheader-tdspras  = wa_e1edpt1-tsspras(1).


        LOOP AT i_data.
          IF i_data-segnam = 'E1EDPT2' AND i_data-psgnum = segment-segnum.

            wa_e1edpt2 = i_data-sdata.
            wa_newline-tdformat = wa_e1edpt2-tdformat.
            wa_newline-tdline = wa_e1edpt2-tdline.
            APPEND wa_newline  TO wa_lines.

          ENDIF.
        ENDLOOP.


        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
*           CLIENT                = SY-MANDT
            header                = wa_txtheader
            insert                = 'X'
            savemode_direct       = 'X'
*           OWNER_SPECIFIED       = ' '
*           LOCAL_CAT             = ' '
*         IMPORTING
*           function              =
          TABLES
            lines                 = wa_lines
          EXCEPTIONS
            id                    = 1
            language              = 2
            name                  = 3
            object                = 4
            OTHERS                = 5
                  .
        IF sy-subrc <> 0.
          "ERROR Create then SAVE
        ENDIF.
      ELSE.
        "ERROR READ
      ENDIF.

    ENDIF.
  ENDIF.



ENDIF.
