*&---------------------------------------------------------------------*
*& Report  ZQMR0008
*&
*&---------------------------------------------------------------------*
*&Autor            Data      Chamado
*&Luis Aquino   03/05/2021    42703
*&---------------------------------------------------------------------*

report  zqmr0008.

tables: qals,
        jest,
        mara,
        zqm0001,
        sscrfields.

include: zqmr0008_top,
         zqmr0008_form.

*---------------------------------------------------------------------------*
*---------------------------------------------------------------------------*
selection-screen: function key 1.

initialization.
    l_sel_button-icon_id = icon_change.
    l_sel_button-icon_text = text-v01.
    sscrfields-functxt_01 = l_sel_button.
*Parâmetros de Seleção
selection-screen begin of block b0 with frame title text-t00.

    select-options:  s_nrmat for mara-matnr,
                     s_lotec for qals-prueflos,
                     s_loten for qals-charg.

  selection-screen end of block b0.
*Caso seja selecionada a opção para editar o nome do responsável pelo laborátório, chama a manutenção de tabela para ZQMT0005
 at selection-screen.

  case sy-ucomm.
    when'FC01'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
       EXPORTING
         action                               = 'U'
         view_name                            = 'ZQMT0005'
       EXCEPTIONS
        client_reference                     = 1
        foreign_lock                         = 2
        invalid_action                       = 3
        no_clientindependent_auth            = 4
        no_database_function                 = 5
        no_editor_function                   = 6
        no_show_auth                         = 7
        no_tvdir_entry                       = 8
        no_upd_auth                          = 9
        only_show_allowed                    = 10
        system_failure                       = 11
        unknown_field_in_dba_sellist         = 12
        view_not_found                       = 13
        maintenance_prohibited               = 14
        OTHERS                               = 15.
  endcase.

  start-of-selection.
*Seleciona os resultados.
    perform seleciona_dados  tables s_nrmat
                                    s_lotec
                                    s_loten.
*Exibe resultado no ALV e imprime caso seja selecionada a função.
    perform exibe_result.
  end-of-selection.
*---------------------------------------------------------------------------*
*---------------------------------------------------------------------------*
