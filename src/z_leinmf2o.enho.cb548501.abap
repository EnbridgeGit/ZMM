"Name: \PR:SAPLEINM\FO:MAINTAIN_DATA_ORDRSP\SE:END\EI
ENHANCEMENT 0 Z_LEINMF2O.
*Custom Enhancement Point.
*BTBOUNDY Jan 11/2011

  DATA: xreturn  like BAPIRET2      occurs 0 with header line,
        xpoitem  like BAPIMEPOITEM  occurs 0 with header line,
        xpoitemx like BAPIMEPOITEMX occurs 0 with header line.

  loop at fekpo.
    if fekpo-action = '003'.
      loop at yekpo.
        if yekpo-ebelp = fekpo-ebelp.
          clear: xpoitem, xpoitemx.

          xpoitem-po_item = yekpo-ebelp.  " set position here
          xpoitem-delete_ind = 'S'.
          xpoitem-acknowl_no = fekpo-labnr.
          append xpoitem.
          xpoitemx-po_item = yekpo-ebelp. " set position here
          xpoitemx-delete_ind = 'X'.
          xpoitemx-acknowl_no = 'X'.
          append xpoitemx.


          CALL FUNCTION 'BAPI_PO_CHANGE'
            EXPORTING
              purchaseorder    = yekpo-ebeln "set ebeln here
            TABLES
              return           = xreturn
              poitem           = xpoitem
              poitemx          = xpoitemx.

          if sy-subrc = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING WAIT = 'X'.
          endif.


          exit.
        endif.
      endloop.
    endif.

  endloop.

ENDENHANCEMENT.
