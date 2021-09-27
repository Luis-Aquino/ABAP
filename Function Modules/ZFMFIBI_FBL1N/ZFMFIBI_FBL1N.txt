FUNCTION zfmfibi_fbl1n.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(EM_ABERTO) TYPE  BOOLEAN OPTIONAL
*"     VALUE(COMPENSADAS) TYPE  BOOLEAN OPTIONAL
*"     VALUE(TODAS) TYPE  BOOLEAN OPTIONAL
*"     VALUE(DATA_INICIO) TYPE  SY-DATUM OPTIONAL
*"     VALUE(DATA_FIM) TYPE  SY-DATUM OPTIONAL
*"  TABLES
*"      IT_TAB STRUCTURE  ZFIS0002
*"----------------------------------------------------------------------
  TYPES:    BEGIN OF w_tab.
          INCLUDE STRUCTURE rkpos .
  TYPES:    END OF w_tab.

  DATA: li_selection  TYPE TABLE OF rsparams,
        lwa_selection TYPE rsparams.

  DATA: text       TYPE c LENGTH 10,
       lt_selscreen  TYPE TABLE OF rsparams WITH HEADER LINE .

  FIELD-SYMBOLS  : <lt_pay_data>   TYPE ANY TABLE .
  FIELD-SYMBOLS : <lt_test> TYPE ANY . 

  DATA lr_pay_data              TYPE REF TO data.
  
*********************************************************************************** 
** Lógica do programa
  
  cl_salv_bs_runtime_info=>set(    EXPORTING display  = abap_false
                                             metadata = abap_false
                                             data     = abap_true ).

  IF ( em_aberto EQ 'X' ).
    SUBMIT rfitemap WITH kd_bukrs-low EQ '1000' WITH x_opsel EQ 'X' WITH pa_stida EQ data_inicio WITH x_norm EQ 'X' WITH x_shbv EQ 'X' WITH x_merk EQ 'X' WITH x_park EQ 'X'  WITH x_apar EQ 'X' AND RETURN.
  ENDIF.

  IF ( compensadas EQ 'X' ).

    CLEAR lwa_selection.
    lwa_selection-selname = 'SO_AUGDT'.  
    lwa_selection-kind = 'S'.
    lwa_selection-sign = 'I'.
    lwa_selection-option = 'BT'.
    lwa_selection-low = data_inicio.            
    lwa_selection-high = data_fim.
    APPEND lwa_selection TO li_selection.

    SUBMIT rfitemap WITH kd_bukrs-low EQ '1000' WITH x_clsel EQ 'X' WITH x_norm EQ 'X' WITH x_shbv EQ 'X' WITH x_merk EQ 'X' WITH x_park EQ 'X'  WITH x_apar EQ 'X' WITH SELECTION-TABLE li_selection AND RETURN.
  ENDIF.

  IF ( todas EQ 'X' ).

    CLEAR lwa_selection.
    lwa_selection-selname = 'SO_BUDAT'.  
    lwa_selection-kind = 'S'.
    lwa_selection-sign = 'I'.
    lwa_selection-option = 'BT'.
    lwa_selection-low = data_inicio.            
    lwa_selection-high = data_fim.
    APPEND lwa_selection TO li_selection.

    SUBMIT rfitemap WITH kd_bukrs-low EQ '1000'  WITH x_aisel EQ 'X' WITH x_norm EQ 'X' WITH x_shbv EQ 'X' WITH x_merk EQ 'X' WITH x_park EQ 'X'  WITH x_apar EQ 'X' WITH SELECTION-TABLE li_selection AND RETURN.
  ENDIF.

  TRY.

      cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_pay_data ).
      ASSIGN lr_pay_data->* TO <lt_pay_data>.
    CATCH cx_salv_bs_sc_runtime_info.

      MESSAGE `Unable to retrieve ALV data` TYPE 'E'.

  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

**Move os resultados do field symbol para a tabela de saída do módulo de função
  LOOP AT <lt_pay_data> ASSIGNING  <lt_test>.

    MOVE-CORRESPONDING  <lt_test> TO it_tab  .
    APPEND it_tab .

  ENDLOOP.

ENDFUNCTION.