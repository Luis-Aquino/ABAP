*&---------------------------------------------------------------------*
*& Report  ZPM0003
*&
*&---------------------------------------------------------------------*
*&Autor/Modificador | Chamado | Data
*&Luis Aquino         25773     28/05/2020
*&
*&---------------------------------------------------------------------*
report  zpm0003.
type-pools sscr.

tables: viqmelst,
        viaufkst,
        itob,
        rihaufk_list,
        aufk,
        afih,
        tj02t,
        rihea,
        jcds,
        pmco,
        iloa.

type-pools slis .
types : begin of ty_pmp,
  nobjnr type viaufkst-objnr, "Nº objeto
  qmnum type viqmelst-qmnum, "Nº da nota
  qmtxt type viqmelst-qmtxt, "Texto breve
  stat(5)  type c,           "Status"
  erdat type viqmelst-erdat, "Data de criação do registro
  objnr type viaufkst-objnr, "Nº objeto
  aufnr type viaufkst-aufnr, "Nº ordem
  ktext type viaufkst-ktext, "Tipo de ordem
  auart type viaufkst-auart, "Tipo de ordem
  plgrp type viaufkst-plgrp, "Grupo de planejamento/departamento responsável
  iwerk type viqmelst-iwerk, "Centro de planejamento de manutenção
  equnr type viqmelst-equnr, "Nº equipamento
  qmnam type viqmelst-qmnam, "Nome do autor da nota
  qmart type viqmelst-qmart, "Tipo de nota
  ilart type viaufkst-ilart, "Tipo de atividade de manutenção
  addat type viaufkst-addat, "Ordem-PM: Data de referência
  aduhr type viaufkst-aduhr, "Hora da data de referência
  kostv type viaufkst-kostv, "Centro de custo responsável
  kostl type viaufkst-kostl, "Centro de custo
  ingpr type viaufkst-ingpr, "Grupo de planejamento/departamento responsável
  iphas type viaufkst-iphas, "Fase de processamento da manutenção
  eqktx type eqkt-eqktx,     "Denominação do objeto técnico
  eqfnr type iloa-eqfnr,     "Campo de ordenação
end of ty_pmp.

types : begin of ty_excluir,

  objnr type viaufkst-objnr,
  stat  type bsvx-sttxt,

end of ty_excluir.


types : begin of ty_pmpo,
  sel,
  nobjnr type viaufkst-objnr, "Nº objeto
  qmnum  type viqmelst-qmnum, "Nº da nota
  qmtxt  type viqmelst-qmtxt, "Texto breve
  statind(5)   type c,
  erdat  type viqmelst-erdat,
  nudate type jcds-udate, "Data de criação do registro
  nutime type jcds-utime, "Hora da nota
  stat   type bsvx-sttxt,     "Status individual de um objeto
  statn  type bsvx-sttxt,         "Status individual de um objeto
  nufdate type jcds-udate, "Data de criação do registro
  nuftime type jcds-utime, "Hora da nota
  objnr type viaufkst-objnr, "Nº objeto
  aufnr  type viaufkst-aufnr, "Nº ordem
  ktext  type viaufkst-ktext, "Nº ordem
  auart  type viaufkst-auart, "Tipo de ordem
  udate  type jcds-udate,     "Ordem-PM: Data de referência
  utime  type jcds-utime,     "Hora da data de referência
  plgrp  type viaufkst-plgrp,  "Grupo de planejamento/departamento responsável
  iwerk  type viaufkst-iwerk, "Centro de planejamento de manutenção
  astnr  type viaufkst-astnr, "Status da ordem
  equnr  type viqmelst-equnr, "Nº equipamento
  qmnam  type viqmelst-qmnam, "Nome do autor da nota
  qmart  type viqmelst-qmart, "Tipo de nota
  ilart  type viaufkst-ilart, "Tipo de atividade de manutenção
  ufdate type jcds-udate,     "Fim confirmado da ordem
  uftime type jcds-utime,     "Fim confirmado da ordem (hora)
  uadate type jcds-udate,     "Data inicial de apontamento
  uatime type jcds-utime,     "Hora inicial de apontamento
  ufadate type jcds-udate,    "Data final de apontamento
  ufatime type jcds-utime,    "Hora final de apontamento
  addat  type viaufkst-addat, "Ordem-PM: Data de referência
  aduhr  type viaufkst-aduhr, "Hora da data de referência
  kostv  type viaufkst-kostv, "Centro de custo responsável
  kostl  type viaufkst-kostl, "Centro de custo
  ingpr  type viaufkst-ingpr, "Grupo de planejamento/departamento responsável
  iphas  type viaufkst-iphas, "Fase de processamento da manutenção
  eqktx  type eqkt-eqktx,     "Denominação do objeto técnico
  wrt03  type pmco-wrt03,      "Valor período em moeda de Ledger
  eqfnr  type iloa-eqfnr,
end of ty_pmpo.

types : begin of ty_pmpoo,
  sel,
  nobjnr     type viaufkst-objnr, "Nº objeto
  qmnum      type viqmelst-qmnum, "Nº da nota
  qmtxt      type viqmelst-qmtxt, "Texto breve
  statind(5) type c,
  erdat      type viqmelst-erdat,
  nudate     type jcds-udate, "Data de criação do registro
  nutime     type jcds-utime, "Hora da nota
  stat       type bsvx-sttxt,     "Status individual de um objeto
  statn      type bsvx-sttxt,         "Status individual de um objeto
  diffdate   type p decimals 2,
  diffdate2  type p decimals 2,
  diffdatee(8),
  nufdate    type jcds-udate, "Data de criação do registro
  nuftime    type jcds-utime, "Hora da nota
  objnr      type viaufkst-objnr, "Nº objeto
  aufnr      type viaufkst-aufnr, "Nº ordem
  ktext      type viaufkst-ktext, "Nº ordem
  auart      type viaufkst-auart, "Tipo de ordem
  udate      type jcds-udate,     "Ordem-PM: Data de referência
  utime   type jcds-utime,     "Hora da data de referência
  plgrp   type viaufkst-plgrp,  "Grupo de planejamento/departamento responsável
  iwerk   type viaufkst-iwerk, "Centro de planejamento de manutenção
  astnr   type viaufkst-astnr, "Status da ordem
  equnr   type viqmelst-equnr, "Nº equipamento
  qmnam   type viqmelst-qmnam, "Nome do autor da nota
  qmart   type viqmelst-qmart, "Tipo de nota
  ilart   type viaufkst-ilart, "Tipo de atividade de manutenção
  ufdate  type jcds-udate,     "Fim confirmado da ordem
  uftime  type jcds-utime,     "Fim confirmado da ordem (hora)
  uadate  type jcds-udate,     "Data inicial de apontamento
  uatime  type jcds-utime,     "Hora inicial de apontamento
  ufadate type jcds-udate,    "Data final de apontamento
  ufatime type jcds-utime,    "Hora final de apontamento
  addat   type viaufkst-addat, "Ordem-PM: Data de referência
  aduhr   type viaufkst-aduhr, "Hora da data de referência
  kostv   type viaufkst-kostv, "Centro de custo responsável
  kostl   type viaufkst-kostl, "Centro de custo
  ingpr   type viaufkst-ingpr, "Grupo de planejamento/departamento responsável
  iphas   type viaufkst-iphas, "Fase de processamento da manutenção
  eqktx   type eqkt-eqktx,     "Denominação do objeto técnico
  wrt03   type pmco-wrt03,      "Valor período em moeda de Ledger
  eqfnr   type iloa-eqfnr,
end of ty_pmpoo.

data: st_layout   type slis_layout_alv,
      gv_count    type i,
      i_fcat      type slis_t_fieldcat_alv,
      cont        type i,
      st_varia    type disvariant,
      contalinhas type i,
      gd_restrict type sscr_restrict,
      gd_optlist  type sscr_opt_list,
      gd_ass      type sscr_ass,
      idx         type sy-tabix,
      gd_layout   type slis_layout_alv,
      gd_repid    like sy-repid,
      datalow     type jcds-udate,
      datahight   type jcds-udate,
      diffdaten   type jcds-udate,
      difftimen   type jcds-utime.

types: begin of ty_auxdat,
  objnr type aufk-objnr,
  udate type jcds-udate,
  utime type jcds-utime,
  inact type jcds-inact,
  chind type jcds-chind,
end of ty_auxdat.

types: begin of ty_datas,

  objnr  type aufk-objnr,
  udate  type jcds-udate,
  utime  type jcds-utime,
  ufdate type jcds-udate,
  uftime type jcds-utime,

end of ty_datas.

types: begin of ty_datasaponta,

  aufnr  type aufk-aufnr,
  udate  type jcds-udate,
  utime  type jcds-utime,
  ufdate type jcds-udate,
  uftime type jcds-utime,
  conta  type afru-rmzhl,

end of ty_datasaponta.

data: wa_fcat like line of i_fcat,
      f_ucomm like sy-ucomm.

types : begin of ty_tem,
  nobjnr type viqmelst-objnr,        "Nº objeto
  qmnum  type viqmelst-qmnum,        "Nº da nota
  objnr  type viaufkst-objnr,"Nº objeto
  aufnr  type viaufkst-aufnr,        "Nº ordem
  txt30  type bsvx-sttxt,
 " txt30  TYPE string,
  iphas  type viaufkst-iphas,        "Fase de processamento da manutenção
  udate  type jcds-udate,
  utime  type jcds-utime,
  aufnra type viaufkst-aufnr,
end of ty_tem.

types : begin of ty_temm,
  objnr  type viaufkst-objnr,
  aufnr  type viaufkst-qmnum,
  aufnra type viaufkst-aufnr,
  txt30  type bsvx-sttxt,

end of ty_temm.

types : begin of ty_getstatus,

  objnr type viaufkst-objnr,
  txt30  type bsvx-sttxt,

end of ty_getstatus.

types : begin of ty_enc,
  objnr like viaufkst-/isdfps/objnr,
end of ty_enc.

types: begin of ty_snotes,

  nobjnr type viqmelst-objnr, "Nº objeto
  qmnum  type viqmelst-qmnum, "Nº da nota
  qmnam  type viqmelst-qmnam, "Nome do autor da nota
  qmtxt  type viqmelst-qmtxt, "Texto breve
  iwerk  type viqmelst-iwerk, "Centro de planejamento de manutenção
  erdat  type viqmelst-erdat, "Data de criação do registro
  mzeit  type viqmelst-mzeit, "Hora da nota
  equnr  type viqmelst-equnr, "Nº equipamento
  qmart  type viqmelst-qmart, "Tipo de nota
  kostl  type viaufkst-kostl, "Centro de custo
  eqktx  type eqkt-eqktx,     "Denominação do objeto técnico
  eqfnr  type viqmelst-eqfnr,
  txt30  type bsvx-sttxt,     "Linha de status de sistema

end of ty_snotes.

types: begin of ty_cost,

  objnr type viaufkst-objnr, "Nº da nota
  wrttp type pmco-wrttp,
  wrt00 type pmco-wrt00,     "Valor período em moeda de Ledger
  wrt01 type pmco-wrt01,
  wrt02 type pmco-wrt02,
  wrt03 type pmco-wrt03,
  wrt04 type pmco-wrt04,
  wrt05 type pmco-wrt05,
  wrt06 type pmco-wrt06,
  wrt07 type pmco-wrt07,
  wrt08 type pmco-wrt08,
  wrt09 type pmco-wrt09,
  wrt10 type pmco-wrt10,
  wrt11 type pmco-wrt11,
  wrt12 type pmco-wrt12,
  wrt13 type pmco-wrt13,
  wrt14 type pmco-wrt14,
  wrt15 type pmco-wrt15,
  wrt16 type pmco-wrt16,

end of ty_cost.

types: begin of ty_coca,

  objnr  type coss-objnr,
  wrttp  type coss-wrttp,
  kstar  type coss-kstar,
  wtg001 type coss-wtg001,
  wtg002 type coss-wtg002,
  wtg003 type coss-wtg003,
  wtg004 type coss-wtg004,
  wtg005 type coss-wtg005,
  wtg006 type coss-wtg006,
  wtg007 type coss-wtg007,
  wtg008 type coss-wtg008,
  wtg009 type coss-wtg009,
  wtg010 type coss-wtg010,
  wtg011 type coss-wtg011,
  wtg012 type coss-wtg012,
  wtg013 type coss-wtg013,
  wtg014 type coss-wtg014,
  wtg015 type coss-wtg015,
  wtg016 type coss-wtg016,

 end of ty_coca.

 types: begin of ty_coca2,

  objnr type coss-objnr,
  wrttp type coss-wrttp,
  kstar type coss-kstar,
  wtg00 type coss-wtg001,

 end of ty_coca2.

types: begin of ty_cost2,

objnr type viaufkst-objnr, "Nº da nota
wrttp type pmco-wrttp,
wrt00 type pmco-wrt00,     "Valor período em moeda de Ledger

end of ty_cost2.

types: begin of ty_sorder,
  objnr type viaufkst-objnr,
  aufnr type viaufkst-aufnr,
  ktext type viaufkst-ktext,
  auart type viaufkst-auart,
  plgrp type viaufkst-plgrp,
  iwerk type viaufkst-iwerk,
  equnr type viaufkst-equnr,
  ilart type viaufkst-ilart,
  addat type viaufkst-addat,
  aduhr type viaufkst-aduhr,
  kostv type viaufkst-kostv,
  kostl type viaufkst-kostl,
  ingpr type viaufkst-ingpr,
  iphas type viaufkst-iphas,
  eqktx type eqkt-eqktx,
  eqfnr type viaufkst-eqfnr,
end of ty_sorder .

ranges: r_ordem for viaufkst-objnr."Objeto da order

data:
      final   type table of ty_pmp    with header line,
      all     type table of ty_tem    with header line,
      temp    type table of ty_tem    with header line,
      tempout type table of ty_tem    with header line,
      tempn   type table of ty_temm   with header line,
      pmp     type table of ty_pmp    with header line,
      pmpp    type table of ty_pmp    with header line,
      pmpo    type table of ty_pmpo   with header line,
      pmpotemp type table of ty_pmpo  with header line,
      pmpout  type table of ty_pmpo   with header line,
      pmpobuff  type table of ty_pmpo with header line,
      sec type i,
      hh  type string,
      resto  type string,
      mm  type i,
      ss  type i,
*      RESULT type p DECIMALS 2,
      result type string,
      pmpoutt type table of ty_pmpo   with header line,
      pmpouttt type table of ty_pmpo  with header line,
      pmpoutttt type table of ty_pmpoo with header line,
      sorder  type table of ty_sorder with header line,
      sorders type table of ty_sorder with header line,
      snote   type table of ty_snotes with header line,
      snotes  type table of ty_snotes with header line,
      pmpoo   type table of ty_pmpo   with header line,
      auxdate type table of ty_auxdat with header line,
      auxdatee type table of ty_auxdat with header line,
      cost    type table of ty_cost    with header line,
      cocacp type table of ty_coca     with header line,
      cocacp2 type table of ty_coca2 with header line,
      cocacp3 type table of ty_coca2 with header line,
      cocacs type table of ty_coca     with header line,
      cocacs2 type table of ty_coca2   with header line,
      cocacs3 type table of ty_coca2   with header line,

      cost2   type table of ty_cost2  with header line,
      cost3   type table of ty_cost2  with header line,
      aufnr   type table of rihaufk   with header line,
      finaln  type table of ty_getstatus with header line,

      notadatainicial type table of ty_auxdat with header line,
      notadatafinal type table of ty_auxdat with header line,
      notadatafinalretif type table of ty_auxdat with header line,
      datasnotas type table of ty_datas with header line,

      ordemdatainicial type table of ty_auxdat with header line,
      ordemdatafinal type table of ty_auxdat with header line,
      ordemdatafinalretif type table of ty_auxdat with header line,
      datasordens type table of ty_datas with header line,

      datasaponta type table of ty_datasaponta with header line,
      datasapontap type table of ty_datasaponta with header line,
      datasapontau type table of ty_datasaponta with header line,
      datasapontaf type table of ty_datasaponta with header line,

      dominantes type table of ty_excluir with header line,
      excluir  type table of ty_excluir with header line,
      statustemp type bsvx-sttxt,

      it_list_aufk   type standard table of rihaufk,
      wa_list_aufk   like line of it_list_aufk,

      it_list_pmco   type standard table of pmco_ext,
      wa_list_pmco   like line of it_list_pmco,

      it_comp_pmco   type standard table of pmco_kgr,
      wa_comp_pmco   like line of it_comp_pmco.

selection-screen begin of block b01 with frame title text-001.
selection-screen begin of line.

parameter    statem as checkbox default 'X'.
selection-screen position 2.
selection-screen comment (20) text-p01 for field statem.

parameter    statsen as checkbox default 'X'.
selection-screen position 40.
selection-screen comment (30) text-p02 for field statsen.

selection-screen end of line .

selection-screen end of block b01.

selection-screen begin of block b02 with frame title text-002.
select-options:
                s_nnota  for viaufkst-qmnum, "Nº da nota
                s_tnota  for viqmelst-qmart, "Tipo de nota
                s_dnota  for viqmelst-erdat obligatory, "Data de criação da nota
                s_hrnota for viqmelst-erzeit,
                s_nequip for viaufkst-equnr, "Nº equipamento
                s_cmpord for iloa-eqfnr matchcode object zspm0001.

selection-screen end of block b02.

selection-screen begin of block b03 with frame title text-003.
select-options:
                s_nordem for viaufkst-aufnr, "Nº ordem
                s_tordem for viaufkst-auart, "Tipo de ordem
                s_tativ  for viaufkst-ilart, "Tipo de atividade de manutenção
                stain    for tj02t-txt04 matchcode object z_statuz, "Status inclusivo ordem de manuteção
                staex    for tj02t-istat matchcode object z_statuz, "Status exclusivo ordem PM
                s_gplan  for viaufkst-ingpr, "Grupo de planejamento para serviços cliente e manutenção
                s_resp   for viaufkst-kostv. "Centro de custo responsável

selection-screen end of block b03.

selection-screen begin of block b04 with frame title text-004.
select-options:
                s_snota  for viaufkst-iwerk, "Centro de planejamento de manutenção
                s_ccusto for viaufkst-kostl. "Centro de custo

selection-screen end of block b04.

selection-screen begin of block b05 with frame title text-005.

parameters :
  p_vari type disvariant-variant.

selection-screen end of block b05.

at selection-screen on value-request for p_vari.
  perform z_select_variavel_alv.

start-of-selection.
  if statsen = '' and statem = ''.
    write: 'Selecione ao menos um dos status.'.
    stop.
  endif.

  perform selecionaall."Seleciona somente notas que possuem ordens, excluindo notas sem ordens associadas e ordens sem notas associadas: INNER JOIN.

  perform montastatusnota."Monta o status das notas usando a função: STATUS_TEXT_EDIT, pois foi solicitado que o status aparecesse na ordem da iw23.
  perform agrupastatus."Monta o status das ordens usando a função: STATUS_TEXT_EDIT, pois foi solicitado que o status aparecesse na ordem da iw33.
  perform selecionaoutput."Associa a tabela do select geral com as tabelas de status .
  if staex[] is not initial.
    perform statusexclusivo."Status exclusivo onde são selecionadas as ordens que possuem o status inclusivo digitado e serão verificadas para exclusão.
  endif.
  if stain[] is not initial.
    perform statusinclusivo."Status inclusivo onde são selecionadas as ordens que não possuem o status exclusivo digitado e serão verificadas para eAhamxclusão.
  endif.
  perform deletaxclusivas."Deleta as ordens de acordo com os filtros de inclusivo e exclusivo se houverem, sleciona informações de custos e seleciona novamente os valores de acordo com os filtros
  perform selecionadatasnotas."Seleciona as datas de abertura das notas e fechamento das notas de acordo com os status I0068 e I0072 respectivamente.
  perform seleciondadatasordens."Seelciona as datas de abertura, fechamento , primeiro e último apontamento da ordem, de acordo com o status I0001, I0045 e na tabela AFRU.
  perform montafieldcat.
  perform montaalv.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selecionaall .

  select
         viqmelst~objnr
         viqmelst~qmnum
         viqmelst~qmtxt
         viqmelst~stat
         viqmelst~erdat
         viaufkst~objnr
         viaufkst~aufnr
         viaufkst~ktext
         viaufkst~auart
         viaufkst~plgrp
         viqmelst~iwerk
         viqmelst~equnr
         viqmelst~qmnam
         viqmelst~qmart
         viaufkst~ilart
         viaufkst~addat
         viaufkst~aduhr
         viaufkst~kostv
         viaufkst~kostl
         viaufkst~ingpr
         viaufkst~iphas
         eqkt~eqktx
         viqmelst~eqfnr
     appending table final
     from viqmelst
     left join eqkt
      on viqmelst~equnr eq eqkt~equnr
     inner join viaufkst
      on viqmelst~aufnr eq viaufkst~aufnr
     where
           viaufkst~aufnr in s_nordem and
           viqmelst~qmart in s_tnota  and
           viaufkst~auart in s_tordem and
           viaufkst~ilart in s_tativ  and
           viaufkst~kostl in s_ccusto and
           viqmelst~iwerk in s_snota  and
           viaufkst~kostv in s_resp   and
           viqmelst~qmnum in s_nnota  and
           viqmelst~erdat in s_dnota  and
           "viaufkst~equnr IN s_nequip AND
           viqmelst~equnr in s_nequip and
           viaufkst~ingpr in s_gplan  and
           viqmelst~erzeit in s_hrnota and
           viqmelst~eqfnr in s_cmpord and
           viqmelst~artpr eq 'PM'  .

  sort final by objnr.

endform.                    " SELECIONAALL
*&---------------------------------------------------------------------*
*&      Form  MONTAFIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montafieldcat .
  data: vl_pos like sy-cucol.
  clear vl_pos.

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'QMNUM' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-datatype = 'NUMC'.
  wa_fcat-seltext_m = 'Nº da nota' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'IWERK' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Cen.Planej' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'QMTXT' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Texto da Nota' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'NUDATE' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Data de criação da Nota' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'NUTIME' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Hora da Criação da Nota' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'STAT' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Status da Nota' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'NUFDATE' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Data de Enc. da Nota' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'NUFTIME' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Hora de Enc. Nota' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .


  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'AUFNR' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Nº ordem' .
  wa_fcat-datatype = 'NUMC'.
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'KTEXT' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Texto da Ordem' .
  wa_fcat-datatype = 'NUMC'.
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'UDATE' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Data de criação da Ordem' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'UTIME' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Hora de criação da Ordem' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'DIFFDATE' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Lead Time N x O' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

*  vl_pos = vl_pos + 1.
*  wa_fcat-col_pos = vl_pos .
*  wa_fcat-fieldname = 'DIFFDATEE' .
*  wa_fcat-tabname = 'pmpoutttt' .
*  wa_fcat-seltext_m = 'Lead Time' .
*  wa_fcat-just = 'L'.
*  append wa_fcat to i_fcat .
*  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'STATN' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Status da Ordem' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'EQUNR' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Nº equipamento' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'EQFNR' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Campo de ordenação' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'QMNAM' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Notificador' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'QMART' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Tipo de nota' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'ILART' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Tipo de atividade de manutenção' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'UFDATE' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Data Enc. Ordem' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'UFTIME' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Hora Enc. Ordem ' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'UADATE' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Dt Inic Ativ' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'UATIME' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Hr Inic ativ ' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .
  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'UFADATE' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Dt fim ativ' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'UFATIME' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Hr fim ativ' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'DIFFDATE2' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Lead Time N x A' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .


*   vl_pos = vl_pos + 1.
*  wa_fcat-col_pos = vl_pos .
*  wa_fcat-fieldname = 'GETRI' .
*  wa_fcat-tabname = 'pmpoutttt' .
*  wa_fcat-seltext_m = 'Data Fim Real' .
*  wa_fcat-just = 'L'.
*  APPEND wa_fcat TO i_fcat .
*  CLEAR wa_fcat .
*
*   vl_pos = vl_pos + 1.
*  wa_fcat-col_pos = vl_pos .
*  wa_fcat-fieldname = 'GEUZI' .
*  wa_fcat-tabname = 'pmpoutttt' .
*  wa_fcat-seltext_m = 'Hora Fim Real' .
*  wa_fcat-just = 'L'.
*  APPEND wa_fcat TO i_fcat .
*  CLEAR wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'KOSTV' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Centro de custo responsável' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'KOSTL' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Centro de custo' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'INGPR' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Grupo de planejamento/departamento responsável' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'EQKTX' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Denominação do objeto ´Técnico' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

  vl_pos = vl_pos + 1.
  wa_fcat-col_pos = vl_pos .
  wa_fcat-fieldname = 'WRT03' .
  wa_fcat-tabname = 'pmpoutttt' .
  wa_fcat-seltext_m = 'Custos Reais Totais' .
  wa_fcat-just = 'L'.
  append wa_fcat to i_fcat .
  clear wa_fcat .

endform.                    " MONTAFIELDCAT
*&---------------------------------------------------------------------*
*&      Form  MONTAALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montaalv .

  gd_layout-box_fieldname = 'SEL'.
  "set field name to store row selection
  "gd_layout-edit              = 'X'. "makes whole ALV table editable
  "gd_layout-zebra             = 'X'.
  gd_repid = sy-repid.
  if pmpoutttt[] is initial.
    message 'Não existem notas/ordens com os parâmetros selecionados.' type 'I'.
    stop.
  endif.
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_buffer_active          = space
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'PFSTATUS'
      i_callback_program       = gd_repid
      it_fieldcat              = i_fcat[]
      i_save                   = 'A'
      is_layout                = gd_layout
    tables
      t_outtab                 = pmpoutttt      "is_variant = st_varia
    exceptions
      program_error            = 1
      others                   = 2.

  if sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

*  CLEAR st_varia.

endform.                    " MONTAALV
*&---------------------------------------------------------------------*
*&      Form  AGRUPASTATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form agrupastatus .

  loop at final.
    call function 'STATUS_TEXT_EDIT'
      exporting
        objnr            = final-objnr
        spras            = sy-langu
        flg_user_stat    = 'X'
      importing
        line             = finaln-txt30
      exceptions
        object_not_found = 01.

    finaln-objnr = final-objnr.
    append finaln.
    clear finaln.
  endloop.

  sort finaln by objnr.


endform.                    " AGRUPASTATUS
*&---------------------------------------------------------------------*
*&      Form  SELECIONAOUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selecionaoutput .
  sort finaln by objnr.

  loop at final.

    pmpp-nobjnr = final-nobjnr.
    pmpp-qmnum = final-qmnum.
    pmpp-qmtxt = final-qmtxt.
    pmpp-stat = final-stat.
    pmpp-objnr = final-objnr.
    pmpp-aufnr = final-aufnr.
    pmpp-erdat = final-erdat.
    pmpp-ktext = final-ktext.
    pmpp-auart = final-auart.
    pmpp-plgrp = final-plgrp.
    pmpp-iwerk = final-iwerk.
    pmpp-equnr = final-equnr.
    pmpp-addat = final-addat.
    pmpp-aduhr = final-aduhr.
    pmpp-qmnam = final-qmnam.
    pmpp-qmart = final-qmart.
    pmpp-ilart = final-ilart.
    pmpp-kostv = final-kostv.
    pmpp-kostl = final-kostl.
    pmpp-ingpr = final-ingpr.
    pmpp-eqktx = final-eqktx.
    pmpp-eqfnr = final-eqfnr.
    append pmpp.
    clear pmpp.

  endloop.
  clear final[].
  sort pmpp by nobjnr.

  if statsen = 'X'.
    loop at pmpp.
      pmpo-nobjnr = pmpp-nobjnr.
      pmpo-qmnum = pmpp-qmnum.
      pmpo-qmtxt = pmpp-qmtxt.
      pmpo-statind = pmpp-stat.
      pmpo-objnr = pmpp-objnr.
      pmpo-aufnr = pmpp-aufnr.
      pmpo-erdat = pmpp-erdat.
      pmpo-ktext = pmpp-ktext.
      pmpo-auart = pmpp-auart.
      pmpo-plgrp = pmpp-plgrp.
      pmpo-iwerk = pmpp-iwerk.
      pmpo-equnr = pmpp-equnr.
      pmpo-addat = pmpp-addat.
      pmpo-aduhr = pmpp-aduhr.
      pmpo-qmnam = pmpp-qmnam.
      pmpo-qmart = pmpp-qmart.
      pmpo-ilart = pmpp-ilart.
      pmpo-kostv = pmpp-kostv.
      pmpo-kostl = pmpp-kostl.
      pmpo-ingpr = pmpp-ingpr.
      pmpo-eqktx = pmpp-eqktx.
      pmpo-eqfnr = pmpp-eqfnr.
      read table temp with key nobjnr = pmpp-nobjnr binary search.
      if sy-subrc = 0.
        pmpo-stat = temp-txt30.
      endif.

      if pmpp-aufnr is not initial.
        read table finaln with key objnr = pmpp-objnr binary search.
        if sy-subrc = 0.
          pmpo-statn = finaln-txt30.
        endif.
      endif.
      if pmpp-stat eq 'I0072'.
        append pmpo.
        clear pmpo.
      endif.
      clear pmpo.
    endloop.

    sort pmpo by objnr.
  endif.

  if statem = 'X'.
    loop at pmpp.
      pmpo-nobjnr = pmpp-nobjnr.
      pmpo-qmnum = pmpp-qmnum.
      pmpo-erdat = pmpp-erdat.
      pmpo-qmtxt = pmpp-qmtxt.
      pmpo-statind = pmpp-stat.
      pmpo-objnr = pmpp-objnr.
      pmpo-aufnr = pmpp-aufnr.
      pmpo-ktext = pmpp-ktext.
      pmpo-auart = pmpp-auart.
      pmpo-plgrp = pmpp-plgrp.
      pmpo-iwerk = pmpp-iwerk.
      pmpo-equnr = pmpp-equnr.
      pmpo-addat = pmpp-addat.
      pmpo-aduhr = pmpp-aduhr.
      pmpo-qmnam = pmpp-qmnam.
      pmpo-qmart = pmpp-qmart.
      pmpo-ilart = pmpp-ilart.
      pmpo-kostv = pmpp-kostv.
      pmpo-kostl = pmpp-kostl.
      pmpo-ingpr = pmpp-ingpr.
      pmpo-eqktx = pmpp-eqktx.
      pmpo-eqfnr = pmpp-eqfnr.

      read table temp with key nobjnr = pmpp-nobjnr binary search.
      if sy-subrc = 0.
        pmpo-stat = temp-txt30.
      endif.

      if pmpp-aufnr is not initial.
        read table finaln with key objnr = pmpp-objnr binary search.
        if sy-subrc = 0.
          pmpo-statn = finaln-txt30.
        endif.
      endif.
      if pmpp-stat ne 'I0072'.
        append pmpo.
        clear pmpo.
      endif.
      clear pmpo.
    endloop.
    sort pmpo by qmnum.
  endif.

  clear pmp[].
  clear temp[].
  clear tempn[].
  clear auxdate[].
  clear finaln[].

  if stain eq space and staex eq space.
    perform adicionasinglenotes.
  endif.
  "IF soordens = 'X'.
    perform adicionasingleorders.
  "ENDIF.
  pmpotemp[] = pmpo[].
  clear pmpo[].

  loop at pmpotemp.

    if pmpotemp-qmnum in s_nnota and pmpotemp-qmart in s_tnota and pmpotemp-equnr in s_nequip
       and pmpotemp-aufnr in s_nordem and pmpotemp-auart in s_tordem and pmpotemp-ilart in s_tativ
       and pmpotemp-ingpr in s_gplan and pmpotemp-iwerk in s_snota and pmpotemp-kostl in s_ccusto and pmpotemp-kostv in s_resp and pmpotemp-eqfnr in s_cmpord.

      append pmpotemp to pmpo.

    endif.

  endloop.
  clear pmpotemp[].
endform.                   " SELECIONAOUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTASTATUSNOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montastatusnota .

  loop at final.
    call function 'STATUS_TEXT_EDIT'
      exporting
        objnr            = final-nobjnr
        spras            = sy-langu
        flg_user_stat    = 'X'
      importing
        line             = temp-txt30
      exceptions
        object_not_found = 01.

    temp-qmnum  = final-qmnum.
    temp-nobjnr = final-nobjnr.
    temp-objnr  = final-objnr.

    append temp.
    clear temp.
  endloop.

  "clear final[].
  sort temp by nobjnr ascending.

endform.                    " MONTASTATUSNOTA
*&---------------------------------------------------------------------*
*&      Form  ADICIONASINGLENOTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form adicionasinglenotes.
  if statsen = 'X'.
    select  viqmelst~objnr
            viqmelst~qmnum
            viqmelst~qmnam
            viqmelst~qmtxt
            viqmelst~iwerk
            viqmelst~erdat
            viqmelst~mzeit
            viqmelst~equnr
            viqmelst~qmart
            viqmelst~kostl
            eqkt~eqktx
            viqmelst~eqfnr
      appending table snote
      from viqmelst
      left join eqkt
        on viqmelst~equnr eq eqkt~equnr
      where viqmelst~aufnr eq space and
            viqmelst~qmnum in s_nnota and
            viqmelst~kostl in s_ccusto and
            viqmelst~equnr in s_nequip and
            viqmelst~iwerk in s_snota and
            viqmelst~qmart in s_tnota and
            viqmelst~erdat in s_dnota and
            viqmelst~erzeit in s_hrnota and
            viqmelst~stat  eq 'I0072' and
            viqmelst~artpr eq 'PM'.
  endif.

  if statem = 'X'.
    select viqmelst~objnr
           viqmelst~qmnum
           viqmelst~qmnam
           viqmelst~qmtxt
           viqmelst~iwerk
           viqmelst~erdat
           viqmelst~mzeit
           viqmelst~equnr
           viqmelst~qmart
           viqmelst~kostl
           eqkt~eqktx
           viqmelst~eqfnr
    appending table snote
    from viqmelst
    left join eqkt
      on viqmelst~equnr eq eqkt~equnr
    where viqmelst~aufnr eq space and
          viqmelst~qmnum in s_nnota and
          viqmelst~kostl in s_ccusto and
          viqmelst~equnr in s_nequip and
          viqmelst~iwerk in s_snota and
          viqmelst~qmart in s_tnota and
          viqmelst~erdat in s_dnota and
          viqmelst~erzeit in s_hrnota and
          viqmelst~stat  ne 'I0072' and
          viqmelst~artpr eq 'PM'.
  endif.

  sort  snote by qmnum.

  loop at snote.
    call function 'STATUS_TEXT_EDIT'
      exporting
        objnr            = snote-nobjnr
        spras            = sy-langu
        flg_user_stat    = 'X'
      importing
        line             = pmpo-stat
      exceptions
        object_not_found = 01.

    pmpo-nobjnr = snote-nobjnr.
    pmpo-qmnum = snote-qmnum.
    pmpo-qmnam = snote-qmnam.
    pmpo-qmtxt = snote-qmtxt.
    pmpo-iwerk = snote-iwerk.
    pmpo-erdat = snote-erdat.
    pmpo-equnr = snote-equnr.
    pmpo-qmart = snote-qmart.
    pmpo-eqktx = snote-eqktx.
    pmpo-kostl = snote-kostl.
    pmpo-eqfnr = snote-eqfnr.
    append pmpo.
    clear pmpo.

  endloop.
  clear snote[].

endform.                    " ADICIONASINGLENOTES
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form user_command using f_ucomm like sy-ucomm
     i_selfield type slis_selfield.

  data: f_subrc like sy-subrc,
        s_arseg like pmpoutttt.
  case f_ucomm.
    when 'EXIT'.

      leave program.

    when '&REFRESH'.
      perform selecionaall."Seleciona somente notas que possuem ordens, excluindo notas sem ordens associadas e ordens sem notas associadas: INNER JOIN.
      perform montastatusnota."Monta o status das notas usando a função: STATUS_TEXT_EDIT, pois foi solicitado que o status aparecesse na ordem da iw23.
      perform agrupastatus."Monta o status das ordens usando a função: STATUS_TEXT_EDIT, pois foi solicitado que o status aparecesse na ordem da iw33.
      perform selecionaoutput."Associa a tabela do select geral com as tabelas de status .
      if staex[] is not initial.
        perform statusexclusivo."Status exclusivo onde são selecionadas as ordens que possuem o status inclusivo digitado e serão verificadas para exclusão.
      endif.
      if stain[] is not initial.
        perform statusinclusivo."Status inclusivo onde são selecionadas as ordens que não possuem o status exclusivo digitado e serão verificadas para eAhamxclusão.
      endif.
      perform deletaxclusivas."Deleta as ordens de acordo com os filtros de inclusivo e exclusivo se houverem, sleciona informações de custos e seleciona novamente os valores de acordo com os filtros
      perform selecionadatasnotas."Seleciona as datas de abertura das notas e fechamento das notas de acordo com os status I0068 e I0072 respectivamente.
      perform seleciondadatasordens."Seelciona as datas de abertura, fechamento , primeiro e último apontamento da ordem, de acordo com o status I0001, I0045 e na tabela AFRU.
      perform montafieldcat.
      perform montaalv.
    when '&IC1'.
      read table pmpoutttt into s_arseg index i_selfield-tabindex.
      check sy-subrc = 0.
      case i_selfield-fieldname.
        when 'AUFNR'.
          if s_arseg-aufnr <> space.
            set parameter id 'ANR' field s_arseg-aufnr.
            call transaction 'IW32' and skip first screen.
          endif.
        when 'QMNUM'.
          if s_arseg-qmnum <> space.
            set parameter id 'IQM' field s_arseg-qmnum.
            call transaction 'IW22' and skip first screen.
          endif.
        when 'EQUNR'.
          if s_arseg-equnr <> space.
            set parameter id 'EQN' field s_arseg-equnr.
            call transaction 'IE03' and skip first screen.
          endif.
        when 'KOSTV'.
          if s_arseg-kostv <> space.
            set parameter id 'KOS' field s_arseg-kostv.
            set parameter id 'CAC' field '1000'.
            call transaction 'KS03' and skip first screen.
          endif.
        when 'KOSTL'.
          if s_arseg-kostl <> space.
            set parameter id 'KOS' field s_arseg-kostl.
            set parameter id 'CAC' field '1000'.
            call transaction 'KS03' and skip first screen.
          endif.
          when 'DIFFDATE2'.
          if s_arseg-aufnr <> space.
            set parameter id 'ANR' field s_arseg-aufnr.
            call transaction 'IW43' and skip first screen.
          endif.
      endcase.
  endcase.

endform.                    " USERCOMAND

form pfstatus using pt_pfst type slis_t_extab.

       set pf-status 'ZPF_STATUS'.
endform.
*&---------------------------------------------------------------------*
*&      Form  STATUSEXCLUSIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form statusexclusivo .
  loop at pmpo.
    if pmpo-objnr is not initial.
      r_ordem-sign = 'I'.
      r_ordem-option = 'EQ'.
      r_ordem-low = pmpo-objnr.
      append r_ordem.
      clear r_ordem.
    endif.
  endloop.

  select  viaufkst~objnr
    appending table excluir
    from viaufkst
    inner join jest
       on  viaufkst~objnr eq jest~objnr
    inner join tj02t
       on jest~stat eq tj02t~istat

    where
*        objnr IN
*            viaufkst~objnr EQ pmpo-objnr AND
             tj02t~spras eq sy-langu and
             tj02t~txt04 in staex and
             jest~inact ne 'X' and
             viaufkst~objnr in r_ordem.
*            viaufkst~aufnr IN s_nordem AND
*            viaufkst~auart IN s_tordem AND
*            viaufkst~ilart IN s_tativ  AND
*            viaufkst~kostl IN s_ccusto AND
*            viaufkst~iwerk IN s_snota  AND
*            viaufkst~kostv IN s_resp   AND
*            viaufkst~equnr IN s_nequip AND
*            viaufkst~ingpr IN s_gplan  AND
*            viaufkst~artpr EQ 'PM'.

  sort excluir by objnr.

  clear r_ordem[].

endform.                    " STATUSEXCLUSIVO
*&---------------------------------------------------------------------*
*&      Form  DELETAXCLUSIVAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form deletaxclusivas .
  if excluir[] is not initial.
    loop at pmpo.
      read table excluir with key objnr = pmpo-objnr binary search.
      if sy-subrc = 0.
        clear pmpout.
      else.
        pmpout = pmpo.
        append pmpout.
      endif.
    endloop.
  else.
    pmpout[] = pmpo[].
  endif.

  sort pmpout by objnr.

  if stain is not initial.
    loop at pmpout.
      read table dominantes with key objnr = pmpout-objnr binary search.
      if sy-subrc = 0.
        pmpobuff = pmpout.
        append pmpobuff.
        clear pmpobuff.

      else.
        clear pmpobuff.
      endif.
    endloop.
    clear pmpout[].
    pmpout[] = pmpobuff[].
  endif.

  clear pmpo[].
  clear pmpobuff[].

  ranges: r_cordem for viaufkst-objnr."Objeto da order

  loop at pmpout.
    if pmpout-objnr is not initial.
      r_cordem-sign = 'I'.
      r_cordem-option = 'EQ'.
      r_cordem-low = pmpout-objnr.
      append r_cordem.
      clear r_cordem.
    endif.
  endloop.

  select pmco~objnr wrttp wrt00 wrt01 wrt02 wrt03 wrt04 wrt05 wrt06 wrt07 wrt08 wrt09
     wrt10 wrt11 wrt12 wrt13 wrt14 wrt15 wrt16
  appending table cost
  from pmco
        where objnr in r_cordem.

    sort cost by objnr.
  loop at cost.
    case cost-wrttp.
      when '04'.
        cost2-objnr = cost-objnr.
        if ( cost-wrt00 >< 0 ).
          cost2-wrt00 = cost-wrt00 + cost2-wrt00.
        endif.
        if ( cost-wrt01 >< 0 ).
          cost2-wrt00 = cost-wrt01 + cost2-wrt00.
        endif.
        if ( cost-wrt02 >< 0 ).
          cost2-wrt00 = cost-wrt02 + cost2-wrt00.
        endif.
        if ( cost-wrt03 >< 0  ).
          cost2-wrt00 = cost-wrt03 + cost2-wrt00.
        endif.
        if ( cost-wrt04 >< 0  ).
          cost2-wrt00 = cost-wrt04 + cost2-wrt00.
        endif.
        if ( cost-wrt05 >< 0  ).
          cost2-wrt00 = cost-wrt05 + cost2-wrt00.
        endif.
        if ( cost-wrt06 >< 0  ).
          cost2-wrt00 = cost-wrt06 + cost2-wrt00.
        endif.
        if ( cost-wrt07 >< 0  ).
          cost2-wrt00 = cost-wrt07 + cost2-wrt00.
        endif.
        if ( cost-wrt08 >< 0  ).
          cost2-wrt00 = cost-wrt08 + cost2-wrt00 .
        endif.
        if ( cost-wrt09 >< 0  ).
          cost2-wrt00 = cost-wrt09 + cost2-wrt00.
        endif.
        if ( cost-wrt10 >< 0  ).
          cost2-wrt00 = cost-wrt10 + cost2-wrt00.
        endif.
        if ( cost-wrt11 >< 0  ).
          cost2-wrt00 = cost-wrt11 + cost2-wrt00.
        endif.
        if ( cost-wrt12 >< 0  ).
          cost2-wrt00 = cost-wrt12 + cost2-wrt00.
        endif.
        if ( cost-wrt13 >< 0  ).
          cost2-wrt00 = cost-wrt13 + cost2-wrt00.
        endif.
        if ( cost-wrt14 >< 0  ).
          cost2-wrt00 = cost-wrt14 + cost2-wrt00.
        endif.
        if ( cost-wrt15 >< 0  ).
          cost2-wrt00 = cost-wrt15 + cost2-wrt00.
        endif.
        append cost2.
        clear cost2.
    endcase.
  endloop.

  data: vl_objnr type objnr.

  loop at cost2.
    if cost2-objnr ne vl_objnr.
      if vl_objnr is not initial.
        append cost3.
      endif.
      cost3 = cost2.
      clear cost3-wrt00.
      vl_objnr = cost2-objnr.
    endif.
    if cost3-wrt00 is initial.
      cost3-wrt00 = cost2-wrt00.
    else.
      cost3-wrt00 = cost3-wrt00 + cost2-wrt00 .
    endif.
  endloop.
  if vl_objnr is not initial.
    append cost3.
  endif.

  clear cost[].
  clear cost2[].

  sort cost3 by objnr.

  loop at pmpout.
    pmpoutt-nobjnr = pmpout-nobjnr.
    pmpoutt-qmnum  = pmpout-qmnum.
    pmpoutt-erdat  = pmpout-erdat.
    pmpoutt-qmtxt  = pmpout-qmtxt.
    pmpoutt-iwerk  = pmpout-iwerk.
    pmpoutt-stat   = pmpout-stat.
    pmpoutt-statn  = pmpout-statn.
    pmpoutt-objnr  = pmpout-objnr.
    pmpoutt-aufnr  = pmpout-aufnr.
    pmpoutt-ktext  = pmpout-ktext.
    pmpoutt-auart  = pmpout-auart.
    pmpoutt-udate  = pmpout-udate.
    pmpoutt-utime  = pmpout-utime.
    pmpoutt-plgrp  = pmpout-plgrp.
    pmpoutt-astnr  = pmpout-astnr.
    pmpoutt-equnr  = pmpout-equnr.
    pmpoutt-qmnam  = pmpout-qmnam.
    pmpoutt-qmart  = pmpout-qmart.
    pmpoutt-ilart  = pmpout-ilart.
    pmpoutt-ufdate = pmpout-ufdate.
    pmpoutt-uftime = pmpout-uftime.
    pmpoutt-addat  = pmpout-addat.
    pmpoutt-aduhr  = pmpout-aduhr.
    pmpoutt-kostv  = pmpout-kostv.
    pmpoutt-kostl  = pmpout-kostl.
    pmpoutt-ingpr  = pmpout-ingpr.
    pmpoutt-iphas  = pmpout-iphas.
    pmpoutt-eqktx  = pmpout-eqktx.
    pmpoutt-eqfnr  = pmpout-eqfnr.
    read table cost3 with key objnr = pmpout-objnr binary search.
    if sy-subrc = 0.
      pmpoutt-wrt03  = cost3-wrt00.
    endif.
    append pmpoutt .
    clear pmpoutt.
  endloop.

  clear pmpout[].
  clear cost3[].

  sort pmpoutt by qmnum ascending equnr .
  pmpouttt[] = pmpoutt[].

  sort pmpouttt by nobjnr.
  clear pmpoutt[].
endform.                    " DELETAXCLUSIVAS
*&---------------------------------------------------------------------*
*&      Form  ADICIONASINGLEORDERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form adicionasingleorders .

  select   viaufkst~objnr
           viaufkst~aufnr
           viaufkst~ktext
           viaufkst~auart
           viaufkst~plgrp
           viaufkst~iwerk
           viaufkst~equnr
           viaufkst~ilart
           viaufkst~addat
           viaufkst~aduhr
           viaufkst~kostv
           viaufkst~kostl
           viaufkst~ingpr
           viaufkst~iphas
           eqkt~eqktx
           viaufkst~eqfnr
       appending table sorder
       from viaufkst
       left join eqkt
        on viaufkst~equnr eq eqkt~equnr
       where
             viaufkst~aufnr in s_nordem and
             viaufkst~auart in s_tordem and
             viaufkst~ilart in s_tativ  and
             viaufkst~kostl in s_ccusto and
             viaufkst~iwerk in s_snota  and
             viaufkst~erdat in s_dnota  and
             viaufkst~kostv in s_resp   and
             viaufkst~qmnum eq space  and
             viaufkst~equnr in s_nequip and
             viaufkst~ingpr in s_gplan  and
             viaufkst~artpr eq 'PM'.

  sort  sorder by aufnr.

  loop at sorder.
    call function 'STATUS_TEXT_EDIT'
      exporting
        objnr            = sorder-objnr
        spras            = sy-langu
        flg_user_stat    = 'X'
      importing
        line             = pmpo-statn
      exceptions
        object_not_found = 01.

    pmpo-objnr = sorder-objnr.
    pmpo-aufnr = sorder-aufnr.
    pmpo-ktext = sorder-ktext.
    pmpo-auart = sorder-auart.
    pmpo-plgrp = sorder-plgrp.
    pmpo-iwerk = sorder-iwerk.
    pmpo-equnr = sorder-equnr.
    pmpo-ilart = sorder-ilart.
    pmpo-addat = sorder-addat.
    pmpo-aduhr = sorder-aduhr.
    pmpo-kostv = sorder-kostv.
    pmpo-kostl = sorder-kostl.
    pmpo-ingpr = sorder-ingpr.
    pmpo-iphas = sorder-iphas.
    pmpo-eqktx = sorder-eqktx.
    pmpo-eqfnr = sorder-eqfnr.

    append pmpo.
    clear pmpo.
  endloop.

endform.                    " ADICIONASINGLEORDERS
*&---------------------------------------------------------------------*
*&      Form  Z_SELECT_VARIAVEL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_select_variavel_alv .
  st_varia-report = sy-repid.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = st_varia
      i_save        = 'A'
    importing
      es_variant    = st_varia
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.

*  Caso tenha dado tudo certo será repassado o valor escolhido para a
* tela de seleção
  if sy-subrc is initial.
    p_vari = st_varia-variant.
  endif.

endform.                    " Z_SELECT_VARIAVEL_ALV
*&---------------------------------------------------------------------*
*&      Form  STATUSINCLUSIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form statusinclusivo .

  loop at pmpo.
    if pmpo-objnr is not initial.
      r_ordem-sign = 'I'.
      r_ordem-option = 'EQ'.
      r_ordem-low = pmpo-objnr.
      append r_ordem.
      clear r_ordem.
    endif.
  endloop.

  select  viaufkst~objnr
    appending table dominantes
    from viaufkst
    inner join jest
      on  viaufkst~objnr eq jest~objnr
    inner join tj02t
      on jest~stat eq tj02t~istat
    where
          tj02t~spras eq sy-langu and
          tj02t~txt04 in stain and
          jest~inact ne 'X' and
          viaufkst~objnr in r_ordem.
*           viaufkst~aufnr IN s_nordem AND
*           viaufkst~auart IN s_tordem AND
*           viaufkst~ilart IN s_tativ  AND
*           viaufkst~kostl IN s_ccusto AND
*           viaufkst~iwerk IN s_snota  AND
*           viaufkst~kostv IN s_resp   AND
*           viaufkst~equnr IN s_nequip AND
*           viaufkst~ingpr IN s_gplan  AND
*           viaufkst~artpr EQ 'PM'.

  sort dominantes by objnr.
  clear r_ordem[].

endform.                    " STATUSINCLUSIVO
*&---------------------------------------------------------------------*
*&      Form  SELECIONADATASNOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selecionadatasnotas .
  ranges: num_objn for viqmelst-objnr.""Objeto da nota

  loop at pmpouttt.

    if pmpouttt-nobjnr is not initial.
      num_objn-sign = 'I'.
      num_objn-option = 'EQ'.
      num_objn-low = pmpouttt-nobjnr.
      append num_objn.
      clear num_objn.
    endif.
  endloop.

  select   jcds~objnr
           jcds~udate
           jcds~utime
      appending table notadatainicial
      from jcds
*    INNER JOIN viqmelst
*         ON viqmelst~objnr EQ jcds~objnr
      where
           "viqmelst~artpr EQ 'PM' AND
           jcds~stat eq 'I0068' and
           jcds~chind eq 'I' and
           jcds~objnr in num_objn.
*         viqmelst~qmnum IN s_nnota AND
*         viqmelst~kostl IN s_ccusto AND
*         viqmelst~equnr IN s_nequip AND
*         viqmelst~iwerk IN s_snota AND
*         viqmelst~qmart IN s_tnota AND
*         viqmelst~erdat IN s_dnota.

  select jcds~objnr
         jcds~udate
         jcds~utime
         jcds~inact
         jcds~chind
  appending table notadatafinal
  from jcds
*    INNER JOIN viqmelst
*         ON jcds~objnr EQ viqmelst~objnr
  where
        "viqmelst~artpr EQ 'PM' AND
        jcds~stat eq 'I0072' and
        jcds~objnr in num_objn.
*          viqmelst~qmnum IN s_nnota AND
*          viqmelst~kostl IN s_ccusto AND
*          viqmelst~equnr IN s_nequip AND
*          viqmelst~iwerk IN s_snota AND
*          viqmelst~qmart IN s_tnota AND
*          viqmelst~erdat IN s_dnota.

  select viaufkst~objnr
         jcds~udate
         jcds~utime

   appending table ordemdatainicial
   from jcds
     inner join viqmelst
                    on jcds~objnr eq viqmelst~objnr
      inner join viaufkst
                    on viqmelst~aufnr eq viaufkst~aufnr
*     INNER JOIN viaufkst
*         ON jcds~objnr EQ viaufkst~objnr
   where
        jcds~stat eq 'I0071' and
*         viaufkst~autyp EQ '30' AND
        jcds~chind eq 'I' and
        jcds~objnr in num_objn.

  sort ordemdatainicial by objnr.

  loop at notadatafinal.
    on change of notadatafinal-objnr.
      append notadatafinalretif.
      clear notadatafinalretif.
    endon.

    notadatafinalretif-objnr = notadatafinal-objnr.
    notadatafinalretif-udate = notadatafinal-udate.
    notadatafinalretif-utime = notadatafinal-utime.
    notadatafinalretif-inact = notadatafinal-inact.
    notadatafinalretif-chind = notadatafinal-chind.

  endloop.
  if notadatafinal-objnr is not initial.
    append notadatafinalretif.
  endif.

  sort notadatafinalretif by objnr.
  sort notadatainicial by objnr.
  loop at notadatainicial.

    datasnotas-objnr = notadatainicial-objnr.
    datasnotas-udate = notadatainicial-udate.
    datasnotas-utime = notadatainicial-utime.

    read table notadatafinalretif with key objnr = notadatainicial-objnr binary search.
    if sy-subrc eq 0 and notadatafinalretif-inact ne 'X'.

      datasnotas-ufdate = notadatafinalretif-udate.
      datasnotas-uftime = notadatafinalretif-utime.

    endif.
    append datasnotas.
    clear datasnotas.
  endloop.

  clear notadatainicial[].
  clear notadatafinal[].
  clear notadatafinalretif[].
  sort datasnotas by objnr.

  clear num_objn[].
endform.                    " SELECIONADATASNOTAS
*&---------------------------------------------------------------------*
*&      Form  SELECIONDADATASORDENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciondadatasordens .
  ranges: num_objeo for viaufkst-objnr."Objeto da order

  loop at pmpouttt.
    "IF pmpouttt-objnr IS NOT INITIAL.
    num_objeo-sign = 'I'.
    num_objeo-option = 'EQ'.
    num_objeo-low = pmpouttt-objnr.
    append num_objeo.
    clear num_objeo.
    "ENDIF.
  endloop.

  select jcds~objnr
         jcds~udate
         jcds~utime
         jcds~inact
         jcds~chind
  appending table ordemdatafinal
  from jcds

  where
       "viaufkst~autyp EQ '30' AND
       jcds~stat eq 'I0045' and
       jcds~objnr in num_objeo.
*         viaufkst~aufnr IN s_nordem AND
*         viaufkst~auart IN s_tordem AND
*         viaufkst~ilart IN s_tativ  AND
*         viaufkst~kostl IN s_ccusto AND
*         viaufkst~iwerk IN s_snota  AND
*         viaufkst~kostv IN s_resp   AND
*         viaufkst~equnr IN s_nequip AND
*         viaufkst~ingpr IN s_gplan .

  loop at ordemdatafinal.
    if ordemdatafinal-objnr is not initial.
      on change of ordemdatafinal-objnr.
        append ordemdatafinalretif.
        clear ordemdatafinalretif.
      endon.
    endif.
    ordemdatafinalretif-objnr = ordemdatafinal-objnr.
    ordemdatafinalretif-udate = ordemdatafinal-udate.
    ordemdatafinalretif-utime = ordemdatafinal-utime.
    ordemdatafinalretif-inact = ordemdatafinal-inact.
    ordemdatafinalretif-chind = ordemdatafinal-chind.
  endloop.

  if ordemdatafinal-objnr is not initial.
    append ordemdatafinalretif.
    clear ordemdatafinalretif.
  endif.
  sort ordemdatafinalretif by objnr.
  sort ordemdatainicial by objnr.

  loop at ordemdatainicial.

    datasordens-objnr = ordemdatainicial-objnr.
    datasordens-udate = ordemdatainicial-udate.
    datasordens-utime = ordemdatainicial-utime.

    read table ordemdatafinalretif with key objnr = ordemdatainicial-objnr binary search.
    if sy-subrc eq 0 and ordemdatafinalretif-inact ne 'X'.

      datasordens-ufdate = ordemdatafinalretif-udate.
      datasordens-uftime = ordemdatafinalretif-utime.

    endif.

    append datasordens.
    clear datasordens.

  endloop.
  clear ordemdatafinalretif[].
  clear ordemdatafinal[].
  clear ordemdatainicial[].

  ranges: d for afru-satza.
  d-sign = 'I'.
  d-option = 'BT'.
  d-low = 'I20'.
  d-high = 'I40'.
  append d.

  ranges: num_ordap for viaufkst-aufnr.
  loop at pmpouttt.

    if pmpouttt-aufnr is not initial.
      num_ordap-sign = 'I'.
      num_ordap-option = 'EQ'.
      num_ordap-low = pmpouttt-aufnr.
      append num_ordap.
      clear num_ordap.
    endif.
  endloop.

  select afru~aufnr
          isdd
          isdz
          iedd
          iedz
          rmzhl
    appending table datasaponta
    from afru
    where
          afru~stokz ne 'X' and
          afru~satza in d and
          afru~stzhl eq space and
          afru~aufnr in num_ordap.

  sort datasaponta by aufnr ascending conta ascending.

  loop at datasaponta.

    datasapontap-aufnr = datasaponta-aufnr.
    datasapontap-udate = datasaponta-udate.
    datasapontap-utime = datasaponta-utime.
    on change of datasaponta-aufnr.

      append datasapontap.
      clear datasapontap.

    endon.
  endloop.

  loop at datasaponta.
    on change of datasaponta-aufnr.

      append datasapontau.
      clear datasapontau.

    endon.

    datasapontau-aufnr = datasaponta-aufnr.
    datasapontau-ufdate = datasaponta-ufdate.
    datasapontau-uftime = datasaponta-uftime.
  endloop.
  if datasapontau-aufnr is not initial.
    append datasapontau.
    clear datasapontau.
  endif.

  sort datasapontap by aufnr.
  sort datasapontau by aufnr.
  clear datasaponta[].

  loop at datasapontap.
    datasapontaf-aufnr = datasapontap-aufnr.
    datasapontaf-udate = datasapontap-udate.
    datasapontaf-utime  = datasapontap-utime.
    read table datasapontau with key aufnr = datasapontap-aufnr binary search.
    if sy-subrc = 0.
      datasapontaf-ufdate = datasapontau-ufdate.
      datasapontaf-uftime = datasapontau-uftime.
    endif.
    append datasapontaf.
    clear  datasapontaf.

  endloop.

  clear datasapontap[].
  clear datasapontau[].
  sort datasapontaf by aufnr.
  loop at pmpouttt.

    pmpoutttt-nobjnr = pmpouttt-nobjnr.
    pmpoutttt-qmnum = pmpouttt-qmnum.
    pmpoutttt-qmtxt = pmpouttt-qmtxt.
    pmpoutttt-iwerk = pmpouttt-iwerk.
    pmpoutttt-stat  = pmpouttt-stat.
    pmpoutttt-statn = pmpouttt-statn.
    pmpoutttt-objnr = pmpouttt-objnr.
    pmpoutttt-aufnr = pmpouttt-aufnr.
    pmpoutttt-ktext = pmpouttt-ktext.
    pmpoutttt-auart = pmpouttt-auart.
    pmpoutttt-udate = pmpouttt-udate.
    pmpoutttt-utime = pmpouttt-utime.
    pmpoutttt-plgrp = pmpouttt-plgrp.
    pmpoutttt-astnr = pmpouttt-astnr.
    pmpoutttt-equnr = pmpouttt-equnr.
    pmpoutttt-qmnam = pmpouttt-qmnam.
    pmpoutttt-qmart = pmpouttt-qmart.
    pmpoutttt-ilart = pmpouttt-ilart.
    pmpoutttt-ufdate = pmpouttt-ufdate.
    pmpoutttt-uftime = pmpouttt-uftime.
    pmpoutttt-addat = pmpouttt-addat.
    pmpoutttt-aduhr = pmpouttt-aduhr.
    pmpoutttt-kostv = pmpouttt-kostv.
    pmpoutttt-kostl = pmpouttt-kostl.
    pmpoutttt-ingpr = pmpouttt-ingpr.
    pmpoutttt-iphas = pmpouttt-iphas.
    pmpoutttt-eqktx = pmpouttt-eqktx.
    pmpoutttt-wrt03 = pmpouttt-wrt03.
    pmpoutttt-eqfnr = pmpouttt-eqfnr.

    read table datasnotas with key objnr = pmpouttt-nobjnr binary search.
    if sy-subrc = 0.

      pmpoutttt-nudate = datasnotas-udate.
      pmpoutttt-nutime = datasnotas-utime.
      pmpoutttt-nufdate = datasnotas-ufdate.
      pmpoutttt-nuftime = datasnotas-uftime.
      diffdaten = datasnotas-udate.
      difftimen = datasnotas-utime.

    endif.
    read table datasordens with key objnr = pmpouttt-objnr binary search.
    if sy-subrc = 0.

      pmpoutttt-udate = datasordens-udate.
      pmpoutttt-utime = datasordens-utime.
      pmpoutttt-ufdate = datasordens-ufdate.
      pmpoutttt-uftime = datasordens-uftime.

      if ( diffdaten le datasordens-udate and diffdaten ne '00000000' and datasordens-udate ne '00000000' ).

       call function 'SALP_SM_CALC_TIME_DIFFERENCE'
        exporting
          date_1                      = diffdaten
          time_1                      = difftimen
          date_2                      = datasordens-udate
          time_2                      = datasordens-utime
       importing
          seconds                     = sec.

       result = sec / 3600.
       split result at '.' into hh resto.
       sec = sec mod 3600.
       mm = sec / 60.
       sec = sec mod 60.
       ss = sec.

       pmpoutttt-diffdate = hh + ( mm / 60 ) + ( sec / 3600 ).

*       CONCATENATE HH ':' MM ':' SS INTO pmpoutttt-diffdatee.

       endif.

    endif.
    read table datasapontaf with key aufnr = pmpoutttt-aufnr binary search.
    if sy-subrc = 0.

      pmpoutttt-uadate  = datasapontaf-udate.
      pmpoutttt-uatime  = datasapontaf-utime.
      pmpoutttt-ufadate = datasapontaf-ufdate.
      pmpoutttt-ufatime = datasapontaf-uftime.

      if pmpoutttt-nudate is not initial.
        clear hh.
        clear sec.
        clear mm.
        clear ss.
        clear result.

        call function 'SALP_SM_CALC_TIME_DIFFERENCE'
        exporting
          date_1  = pmpoutttt-nudate
          time_1  = pmpoutttt-nutime
          date_2  = datasapontaf-ufdate
          time_2  = datasapontaf-uftime
       importing
          seconds = sec.

       result = sec / 3600.
       split result at '.' into hh resto.
       sec = sec mod 3600.
       mm = sec / 60.
       sec = sec mod 60.
       ss = sec.

       pmpoutttt-diffdate2 = hh + ( mm / 60 ) + ( sec / 3600 ).

      endif.

    endif.
    append pmpoutttt.
    clear pmpoutttt.
  endloop.



  clear pmpouttt[].
  clear num_objeo[].
  clear num_ordap[].
  clear hh.
  clear sec.
  clear mm.
  clear ss.

  sort pmpoutttt by aufnr.

endform.                    " SELECIONDADATASORDENS