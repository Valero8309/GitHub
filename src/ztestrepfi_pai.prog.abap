*&---------------------------------------------------------------------*
*& Include          ZINFI_IVA_PAGADO_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  ok_code = sy-ucomm.

  CASE ok_code.

  WHEN 'BACK'.
    LEAVE TO SCREEN 0.

  WHEN 'EXIT'.
    LEAVE PROGRAM.

  WHEN 'CANCEL'.
    LEAVE PROGRAM.

  WHEN OTHERS.
  ENDCASE.

ENDMODULE.
