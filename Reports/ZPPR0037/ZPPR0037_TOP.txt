*&---------------------------------------------------------------------*
*&  Include           ZPPR0037_TOP
*&---------------------------------------------------------------------*

types: begin of ty_out,
       NOME_USUARIO     type ZPPT0015-NOME_USUARIO,
       DATA_REIMPRESSAO type ZPPT0015-DATA_REIMPRESSAO,
       HORA_REIMPRESSAO type ZPPT0015-HORA_REIMPRESSAO,
       AUFNR            type ZPPT0015-AUFNR,
       CHARG            type ZPPT0015-CHARG,
       MOTIVO           type ZPPT0015-MOTIVO,
      end of ty_out,

      begin of ty_es_s_user,
         sign(1)   type c,
         option(2) type c,
         low       like zppt0015-nome_usuario,
         high      like zppt0015-nome_usuario,
      end of ty_es_s_user,

      begin of ty_es_s_data,
         sign(1)   type c,
         option(2) type c,
         low       like zppt0015-data_reimpressao,
         high      like zppt0015-data_reimpressao,
      end of ty_es_s_data,

      begin of ty_es_s_hora,
         sign(1)   type c,
         option(2) type c,
         low       like zppt0015-hora_reimpressao,
         high      like zppt0015-hora_reimpressao,
      end of ty_es_s_hora,

      begin of ty_es_s_ordem,
         sign(1)   type c,
         option(2) type c,
         low       like zppt0015-aufnr,
         high      like zppt0015-aufnr,
      end of ty_es_s_ordem,

      begin of ty_es_s_lote,
         sign(1)   type c,
         option(2) type c,
         low       like zppt0015-charg,
         high      like zppt0015-charg,
      end of ty_es_s_lote.

types: ty_s_user type table of ty_es_s_user,
       ty_s_data type table of ty_es_s_data,
       ty_s_hora type table of ty_es_s_hora,
       ty_s_ordem type table of ty_es_s_ordem,
       ty_s_lote type table of ty_es_s_lote.

**Tables
data: t_out type table of ty_out,
**Work Areas  
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