*&---------------------------------------------------------------------*
*& Report  ZPPR0037
*&
*&---------------------------------------------------------------------*
*&    Autor     |   Solicitante   |    Chamado
*& Luis Aquino
*&---------------------------------------------------------------------*

REPORT  ZPPR0037.

tables: zppt0015.
include: ZPPR0037_TOP,
         ZPPR0037_FORM.

selection-screen begin of block b0 with frame title text-000.

  select-options: s_user  for zppt0015-NOME_USUARIO,
                  s_data  for zppt0015-DATA_REIMPRESSAO,
                  s_hora  for zppt0015-HORA_REIMPRESSAO,
                  s_ordem for ZPPT0015-AUFNR,
                  s_lote  for ZPPT0015-CHARG.

selection-screen end of block b0.

start-of-selection.
**Lógica de processamento de exibição de informações
  perform seleciona_dados tables s_user
                                 s_data
                                 s_hora
                                 s_ordem
                                 s_lote.
**Exibe ALV
  perform exibe_result.

end-of-selection.