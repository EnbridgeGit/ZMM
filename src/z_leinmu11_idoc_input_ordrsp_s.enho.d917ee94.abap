"Name: \FU:IDOC_INPUT_ORDRSP\SE:BEGIN\EI
ENHANCEMENT 0 Z_LEINMU11_IDOC_INPUT_ORDRSP_S.
* Begin changes - insert code   JRHARTUNG  04/07/11  TR0872  D30K916441

* Unblock any blocked PO items before processing the IDoc

  PERFORM  f_unblock_po_items IN PROGRAM zlmme002_idoc_ordrsp_po_unblck
                                  TABLES idoc_contrl
                                         idoc_data
                                         idoc_status
                                         return_variables
                                  USING  mass_processing
                                CHANGING workflow_result
                                         exitflag.

* End changes   - insert code   JRHARTUNG  04/07/11  TR0872  D30K916441
ENDENHANCEMENT.
