*&---------------------------------------------------------------------*
*&  Include           ZPPR0037_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_USER  text
*      -->P_S_DATA  text
*      -->P_S_HORA  text
*----------------------------------------------------------------------*
form SELECIONA_DADOS  tables  s_user  type ty_s_user
                              s_data  type ty_s_data
                              s_hora  type ty_s_hora
                              s_ordem type ty_s_ordem
                              s_lote  type ty_s_lote.
*Seleciona informações
  select nome_usuario
         data_reimpressao
         hora_reimpressao
         aufnr
         charg
         motivo
  appending table t_out
  from zppt0015
  where nome_usuario in s_user and
        data_reimpressao in s_data and
        hora_reimpressao in s_hora and
        aufnr in s_ordem and
        charg in s_lote.

endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  EXIBE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form EXIBE_RESULT .

**Exibe resultado  no ALV
  cl_salv_table=>factory(
    exporting
      list_display = abap_false
    importing
      r_salv_table = lo_salv_table
    changing
      t_table      = t_out ).

  lo_salv_table->set_top_of_list( go_header ).
  lo_salv_table->set_top_of_list_print( go_header ).
  lo_columns  = lo_salv_table->get_columns( ).
**Muda nome de exibição da coluna
  try.
      o_column = lo_columns->get_column( 'MOTIVO' ).
      o_column->set_long_text( 'Motivo de reimpressão' ).
      o_column->set_medium_text( 'Motivo reimpressão' ).
    catch cx_salv_not_found.
  endtry.

  lo_columns->set_optimize( abap_true ).
**Subtotal para não repetir o nome de usuário na visualização do alv
  lr_groups = lo_salv_table->get_sorts( ).
  lr_groups->add_sort( columnname = 'NOME_USUARIO' subtotal = abap_false ).
  lr_aggregations = lo_salv_table->get_aggregations( ).

  lo_functions = lo_salv_table->get_functions( ).
  lo_functions->set_all( abap_true ).

  gr_layout = lo_salv_table->get_layout( ).
  key-report = sy-repid.
  gr_layout->set_key( key ).
  gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  gr_layout->set_default( abap_true ).

  o_events = lo_salv_table->get_event( ).

  lo_salv_table->display( ).

endform.                    " EXIBE_RESULT