*&---------------------------------------------------------------------*
*& Include          ZINFI_IVA_PAGADO_CLS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

  METHODS mi_handle_double_click
  FOR EVENT double_click OF cl_gui_alv_grid
  IMPORTING e_row
    e_column
    es_row_no.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD mi_handle_double_click.

    DATA: ls_aux_tab TYPE zsfi_012_iva_pagado.
    CLEAR: ls_aux_tab.

    READ TABLE gt_outtab INTO ls_aux_tab INDEX es_row_no-row_id.

    CASE e_column.

    WHEN 'DOC_SAP_FACTURA'.

      SET PARAMETER ID 'BLN' FIELD ls_aux_tab-doc_sap_factura.
      SET PARAMETER ID 'BUK' FIELD gv_buk.
      SET PARAMETER ID 'GJR' FIELD ls_aux_tab-gjahr.

      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN 'DOC_PAGO_SAP'.

      SET PARAMETER ID 'BLN' FIELD ls_aux_tab-doc_pago_sap.
      SET PARAMETER ID 'BUK' FIELD gv_buk.
      SET PARAMETER ID 'GJR' FIELD gv_gjr.

      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.                    "mi_handle_double_click

ENDCLASS.
