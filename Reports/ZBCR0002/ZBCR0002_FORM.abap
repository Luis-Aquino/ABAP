*&---------------------------------------------------------------------*
*&  Include           ZBCR0002_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_NPED  text
*      -->P_S_DATA  text
*      -->P_S_COMP  text
*      -->P_S_FORNEC  text
*----------------------------------------------------------------------*
form seleciona_dados  tables   s_nusua type ty_s_nusua
                               s_nfunc type ty_s_nfunc
                               s_objec type ty_s_objec
                               s_trans type ty_s_trans.
**Consulta Usuários Principais
  select agr_users~uname
         agr_1251~agr_name
         agr_1251~object
         agr_1251~field
         agr_1251~low
    appending table t_out
    from agr_users
      inner join agr_1251 on agr_users~agr_name eq agr_1251~agr_name
    where agr_users~uname   in s_nusua and
          agr_1251~agr_name in s_nfunc and
          agr_1251~object   in s_objec and
          agr_1251~low      in s_trans and
          agr_1251~deleted ne 'X'.

  loop at t_out into w_t_out.
    linhas = sy-tabix.
    w_t_out-tipo = 'Principal'.
    modify t_out index linhas from w_t_out .
  endloop.

**Consulta Sub Usuários
  select ztbc_perfis_ind~pernr
         agr_1251~agr_name
         agr_1251~object
         agr_1251~field
         agr_1251~low
    appending table t_out
    from ztbc_perfis_ind
      inner join agr_1251 on ztbc_perfis_ind~agr_name eq agr_1251~agr_name
    where ztbc_perfis_ind~pernr in s_nusua and
          agr_1251~agr_name     in s_nfunc and
          agr_1251~object       in s_objec and
          agr_1251~low          in s_trans and
          agr_1251~deleted      ne 'X'.

  loop at t_out into w_t_out.
    linhas2 = sy-tabix.
    if ( linhas2 > linhas ).
      w_t_out-tipo = 'Sub-Usuário'.
      modify t_out index linhas2 from w_t_out.
    endif.
  endloop.

endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  EXIBE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exibe_result .
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
      o_column = lo_columns->get_column( 'TIPO' ).
      o_column->set_long_text( 'Tipo de Usuário' ).
      o_column->set_medium_text( 'Tipo de Usuário' ).
    catch cx_salv_not_found.
  endtry.

  lo_columns->set_optimize( abap_true ).
**Subtotal para não repetir o nome de usuário na visualização do alv
  lr_groups = lo_salv_table->get_sorts( ).
  lr_groups->add_sort( columnname = 'UNAME' subtotal = abap_false ).
  lr_aggregations = lo_salv_table->get_aggregations( ).
**Subtotal para não repetir o tipo de usuário na visualização do alv
  lr_groups = lo_salv_table->get_sorts( ).
  lr_groups->add_sort( columnname = 'TIPO' subtotal = abap_false ).
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