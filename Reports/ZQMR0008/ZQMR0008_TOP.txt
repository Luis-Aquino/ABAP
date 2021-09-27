*&---------------------------------------------------------------------*
*&  Include           ZQMR0008_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class LCL_HANDLE_EVENTS
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class LCL_HANDLE_EVENTS definition.
  public section.
    methods:
      on_user_command for event added_function of cl_salv_events
        importing e_salv_function.
endclass.               "LCL_HANDLE_EVENTS
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_handle_events
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_handle_events implementation.
  method on_user_command.
      perform handle_user_command using e_salv_function.
    endmethod.                    "on_user_command
          "on_single_click
endclass.               "lcl_handle_events

types:begin of ty_mch1,
    matnr      type qals-selmatnr,      "Nº do material
    maktx      type makt-maktx,         "Texto breve de material
    prueflos   type qals-prueflos,      "Nº lote de controle
    charg      type qals-charg,         "Número do lote
    vcode      type qave-vcode,         "Code da decisão de utilização
    vauswahlmg type qave-vauswahlmg,    "Conjunto selecionado da decisão de utilização
    vdatum     type qave-vdatum,        "Mandante
    vezeiterf  type qave-vezeiterf,     "Hora da entrada da decisão de utilização
    vname      type qave-vname,         "Autor da decisão de utilização
    name_text  type addr3_val-name_text,"Nome completo da pessoa
  end of ty_mch1,

begin of ty_es_s_matnr,
   sign(1)   type c,
   option(2) type c,
   low  like mch1-matnr,
   high like mch1-matnr,
end of ty_es_s_matnr,

begin of ty_es_s_lotec,
   sign(1)   type c,
   option(2) type c,
   low       like qals-prueflos,
   high      like qals-prueflos,
end of ty_es_s_lotec,

begin of ty_es_s_loten,
   sign(1)   type c,
   option(2) type c,
   low       like qals-charg,
   high      like qals-charg,
end of ty_es_s_loten.
*Variáveis
data:
      gt_rows      type salv_t_row,
      contador1    type i,
      vl_fm_name   type rs38l_fnam,
      user_address like addr3_val,
      l_sel_button type smp_dyntxt.
*Tabelas
types:
      ty_s_matnr type table of ty_es_s_matnr,
      ty_s_lotec type table of ty_es_s_lotec,
      ty_s_loten type table of ty_es_s_loten,
      it_exclude TYPE TABLE OF sy-ucomm.
data:
      t_infmat   type table of zqm0001,
      t_infmat2  type table of zqm0001.
*Work Areas
data:
    w_infmat  like zqm0001,
    w_infmat2 like zqm0001,
    wt_rows   like line of gt_rows.
*Variáveis que comportam objetos
data:
    o_columns       type ref to cl_salv_columns,
    o_column        type ref to cl_salv_column,
    lo_aggregations type ref to cl_salv_aggregations,
    lo_salv_table   type ref to cl_salv_table,
    lr_table        type ref to data,
    rt_fcat         type        lvc_t_fcat,
    key             type        salv_s_layout_key,
    gr_events       type ref to lcl_handle_events,
    gr_layout       type ref to cl_salv_layout,
    lo_functions    type ref to cl_salv_functions_list,
    grana           type ref to salv_de_sort_group,
    lr_aggregations type ref to cl_salv_aggregations,
    lr_groups       type ref to cl_salv_sorts,
    go_header       type ref to cl_salv_form_layout_grid,
    gr_salv_table   type ref to cl_salv_table,
    gr_alvgrid      type ref to cl_gui_alv_grid,
    gr_selections   type ref to cl_salv_selections,
    o_selections    type ref to cl_salv_selections,
    gv_editable     type abap_bool.
*Constantes
constants:
    c_form TYPE rs38l_fnam VALUE 'Z_RQGQ0223'.
