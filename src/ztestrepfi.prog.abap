*&---------------------------------------------------------------------*
*& Report ZREFI_IVA_PAGADO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztestrepfi.


INCLUDE ZTESTREPFI_TOP.
*zinfi_iva_pagado_top,
INCLUDE ZTESTREPFI_CLS.
*zinfi_iva_pagado_cls,
INCLUDE ZTESTREPFI_S01.
*zinfi_iva_pagado_s01,
INCLUDE ZTESTREPFI_PBO.
*zinfi_iva_pagado_pbo,
INCLUDE ZTESTREPFI_PAI.
*zinfi_iva_pagado_pai,
INCLUDE ZTESTREPFI_F01.
*zinfi_iva_pagado_f01.

START-OF-SELECTION.

  PERFORM f_main.
