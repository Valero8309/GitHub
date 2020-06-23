*&---------------------------------------------------------------------*
*& Include          ZINFI_IVA_PAGADO_S01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Selección deudor
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk_deudor WITH FRAME TITLE TEXT-b01.

PARAMETERS:
  p_bukrs TYPE bkpf-bukrs OBLIGATORY.   "Sociedad
SELECT-OPTIONS:
  s_lifnr FOR acdoca-lifnr NO INTERVALS NO-EXTENSION. "Proovedor
PARAMETERS:
  p_monat TYPE bkpf-monat OBLIGATORY,   "Periodo contable
  p_gjahr TYPE bkpf-gjahr OBLIGATORY.   "Ejercicio

SELECTION-SCREEN END OF BLOCK bk_deudor.

*&---------------------------------------------------------------------*
*& Filtros adicionales
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk_partidas WITH FRAME TITLE TEXT-b02.

SELECT-OPTIONS:
  s_belnr FOR bkpf-belnr, "No. documento
  s_budat FOR bkpf-budat. "Fecha contabilizacion

SELECTION-SCREEN END OF BLOCK bk_partidas.
