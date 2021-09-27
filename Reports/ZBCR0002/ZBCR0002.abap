*&---------------------------------------------------------------------*
*& Report  ZBCR0002
*&
*&---------------------------------------------------------------------*
*&    Autor     |      Solicitante
*& Luis Aquino        Itallo Barros
*&---------------------------------------------------------------------*

report  zbcr0002.

tables: agr_users,
        agr_1251.

include: zbcr0002_top,
         zbcr0002_form.
**Parâmetros de Seleção
selection-screen begin of block b0
                          with frame title text-t00.

select-options:  s_nusua for agr_users-uname,
                 s_nfunc for agr_1251-agr_name,
                 s_objec for agr_1251-object,
                 s_trans for agr_1251-low.
selection-screen end of block b0.

start-of-selection.
**Lógica de processamento
  perform seleciona_dados  tables s_nusua
                                  s_nfunc
                                  s_objec
                                  s_trans.
**Exibição do ALV
  perform exibe_result.

end-of-selection.