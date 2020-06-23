*&---------------------------------------------------------------------*
*& Include          ZINFI_IVA_PAGADO_TOP
*&---------------------------------------------------------------------*

"Uso de tablas.
TABLES: bkpf,
        acdoca.

"Uso de pools.
TYPE-POOLS: slis.

"Global Types
TYPES: BEGIN OF ty_cabecera,
         nombre_pro TYPE string,
         rfc_pro    TYPE string,
       END OF ty_cabecera.

*Global Tables
DATA: gt_outtab   TYPE STANDARD TABLE OF zsfi_012_iva_pagado_new,
      gt_fcat     TYPE lvc_t_fcat,
      gt_cabecera TYPE STANDARD TABLE OF ty_cabecera.

*Global Structures
DATA: gs_layout   TYPE lvc_s_layo,
      gs_cabecera TYPE ty_cabecera.

*Global Variable
DATA: gv_r_social(255)   TYPE c,
      gv_nombre_re(255)  TYPE c,
      gv_nombre_pro(255) TYPE c,
      gv_rfc_pro(255)    TYPE c,
      gv_buk             TYPE bukrs,
      r_blart TYPE RANGE OF bkpf-blart,
      gv_gjr             TYPE gjahr,
      gv_moneda          TYPE waers,
      gv_tc              TYPE kursf.

CLASS lcl_event_receiver DEFINITION DEFERRED.

*Objects
DATA: gr_custom_container TYPE REF TO cl_gui_custom_container,
      gr_header           TYPE REF TO cl_gui_custom_container,
      gr_top_viewer       TYPE REF TO cl_gui_html_viewer,
      gr_top_document     TYPE REF TO cl_dd_document,
      gr_alv_grid         TYPE REF TO cl_gui_alv_grid,
      gr_eveny_receiver   TYPE REF TO lcl_event_receiver.

*OK CODE
DATA: ok_code TYPE syucomm.
