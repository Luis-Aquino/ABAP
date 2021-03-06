*&---------------------------------------------------------------------*
*&  include           zqmr0008_form
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      form  seleciona_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_s_nrmat  text
*      -->p_s_lotec  text
*      -->p_s_loten  text
*----------------------------------------------------------------------*
form seleciona_dados  tables   s_nrmat type ty_s_matnr
                               s_lotec type ty_s_lotec
                               s_loten type ty_s_loten.
*seleciona as informações
  select qals~matnr      "nº do material
         makt~maktx      "texto breve de material
         qals~prueflos   "nº lote de controle
         qals~charg      "número do lote
         qave~vcode      "code da decisão de utilização
         qave~vauswahlmg "conjunto selecionado da decisão de utilização
         qave~vdatum     "data do code para a decisão de utilização
         qave~vezeiterf  "hora da entrada da decisão de utilização
         qave~vname      "autor da decisão de utilização
         zqmt0005~name_text
    appending table t_infmat
    from qals
      inner join qave on qals~prueflos eq qave~prueflos
      inner join makt on qals~matnr eq makt~matnr
      inner join zqmt0005 on qals~werk eq zqmt0005~centro
    where
      qals~matnr in s_nrmat and
      qals~prueflos in s_lotec and
      qals~charg in s_loten and
      qals~art eq '08' and
      qave~vcode eq '001' and
      makt~spras eq sy-langu.

    contador1 = 0.
*loop para pegar o nome e sobrenome do usuário da du
    loop at t_infmat into w_infmat.
      contador1 = contador1 + 1.
      call function 'SUSR_USER_ADDRESS_READ'
        exporting
          user_name                    = w_infmat-vname
*         read_db_directly             = ' '
        importing
          user_address                 = user_address
*         user_usr03                   =
        exceptions
          user_address_not_found       = 1
          others                       = 2.

      if sy-subrc = 0.
         concatenate user_address-name_text w_infmat-name_text into w_infmat-name_text.
         clear user_address.
      endif.

      modify t_infmat from w_infmat index contador1.

    endloop.

endform.                    " seleciona_dados
*&---------------------------------------------------------------------*
*&      form  exibe_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exibe_result .
    cl_salv_table=>factory(
      exporting
        list_display = abap_false
      importing
        r_salv_table = lo_salv_table
      changing
        t_table      = t_infmat ).

*muda tela padrão para exibir o ícone de impressão
    lo_salv_table->set_screen_status( report = sy-repid
                               pfstatus = 'ZSTATUSPRINT'
                               set_functions = 2 ).
*
    data: lr_events type ref to cl_salv_events_table.

    lr_events = lo_salv_table->get_event( ).

    create object gr_events.

    set handler gr_events->on_user_command for lr_events.

    lo_salv_table->set_top_of_list( go_header ).
    lo_salv_table->set_top_of_list_print( go_header ).
    o_columns  = lo_salv_table->get_columns( ).

    try.
        o_column = o_columns->get_column( 'NAME_TEXT2' ).
        o_column->set_long_text( 'Nome Farmac Resp' ).
        o_column->set_medium_text( 'Nome Farmac Resp' ).
    catch cx_salv_not_found.
    endtry.
      try.
        o_column = o_columns->get_column( 'OBSER' ).
        o_column->set_long_text( 'Observações' ).
        o_column->set_medium_text( 'Observações' ).
        o_column->set_output_length( 255 ).
    catch cx_salv_not_found.
    endtry.

    o_columns->set_optimize( abap_true ).

    lo_functions = lo_salv_table->get_functions( ).
    lo_functions->set_all( abap_true ).

    gr_layout = lo_salv_table->get_layout( ).
    key-report = sy-repid.
    gr_layout->set_key( key ).
    gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    gr_layout->set_default( abap_true ).
    o_selections = lo_salv_table->get_selections( ).
    o_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
    lcl_salv_buddy=>set_editable( i_fieldname  = 'OBSER'
                                  i_salv_table = lo_salv_table ).
*exibe alv
    lo_salv_table->display( ).

endform.                    " exibe_result
*&---------------------------------------------------------------------*
*&      form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_e_salv_function  text
*----------------------------------------------------------------------*
form handle_user_command using i_ucomm type salv_de_function.
*caso seja selecionado o botão de imprimir
  case i_ucomm.
    when '&ZPR'.
      perform imprime_formulario.
  endcase.
endform.                    " handle_user_command
*&---------------------------------------------------------------------*
*&      form  imprime_formulario
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form imprime_formulario .
*pega linhas selecionadas
   gr_selections = lo_salv_table->get_selections( ).
   gt_rows = gr_selections->get_selected_rows( ).
*exibe warning, caso o usuário quiser imprimir o formulário vazio, já que o tipo w permite que o usuário prossiga ma função
   if gt_rows is initial.
     message 'Selecione ao menos uma linha para a impressão.' type 'W'.
   endif.
*limpa tabela caso a impressão seja cancelada, a atribui as linhas selecionadas para a tabela de input do smartform
   clear t_infmat2[].
   contador1 = 0.
   loop at t_infmat into w_infmat.
     contador1 = contador1 + 1.
     loop at gt_rows into wt_rows.
       if ( contador1 eq wt_rows ).
         w_infmat2 = w_infmat.
         append w_infmat2 to t_infmat2.
       endif.
     endloop.
   endloop.

*chama o formulário
    call function 'SSF_FUNCTION_MODULE_NAME'
     exporting
       formname                 = c_form

    importing
      fm_name                  = vl_fm_name
    exceptions
      no_form                  = 1
      no_function_module       = 2
      others                   = 3
         .
    call function vl_fm_name

      tables
        dados                      =  t_infmat2

              .
    if sy-subrc <> 0.
      message 'Não foi possível carregar o formulário' type 'w'.
    endif.

endform.                    " imprime_formulario
