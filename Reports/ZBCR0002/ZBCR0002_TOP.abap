*&---------------------------------------------------------------------*
*&  Include           ZBCR0002_TOP
*&---------------------------------------------------------------------*

types: begin of ty_out,
*      ebeln type ekko-ebeln,
       uname    type agr_users-uname,
       agr_name type agr_1251-agr_name,
       object   type agr_1251-object,
       field    type agr_1251-field,
       low      type agr_1251-low,
       tipo(12) type c,
      end of ty_out,

      begin of ty_es_s_nusua,
         sign(1)   type c,
         option(2) type c,
         low  like agr_users-uname,
         high like agr_users-uname,
      end of ty_es_s_nusua,

      begin of ty_es_s_nfunc,
         sign(1)   type c,
         option(2) type c,
         low  like agr_1251-agr_name,
         high like agr_1251-agr_name,
      end of ty_es_s_nfunc,

      begin of ty_es_s_objec,
         sign(1)   type c,
         option(2) type c,
         low  like agr_1251-object,
         high like agr_1251-object,
      end of ty_es_s_objec,

      begin of ty_es_s_trans,
         sign(1)   type c,
         option(2) type c,
         low  like agr_1251-low,
         high like agr_1251-low,
      end of ty_es_s_trans.

types: ty_s_nusua type table of ty_es_s_nusua,
       ty_s_nfunc type table of ty_es_s_nfunc,
       ty_s_objec type table of ty_es_s_objec,
       ty_s_trans type table of ty_es_s_trans.

data: t_out type table of ty_out,
      w_t_out like line of t_out.

data: lo_salv_table   type ref to cl_salv_table,
      lo_columns      type ref to cl_salv_columns,
      lr_groups type ref to cl_salv_sorts,
      go_header type ref to cl_salv_form_layout_grid,
      lr_aggregations type ref to cl_salv_aggregations,
      lo_functions type ref to cl_salv_functions_list,
      gr_layout type ref to cl_salv_layout,
      o_events  type ref to cl_salv_events_table,
      key     type        salv_s_layout_key,
      o_column        type ref to cl_salv_column,
      linhas like sy-tabix,
      linhas2 like sy-tabix.