CLASS zcl_i_a2cc_aggregator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_i_a2cc_aggregator .
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_i_a2cc_aggregator IMPLEMENTATION.
  METHOD zif_i_a2cc_aggregator~aggregate_metrics.
    DATA aggregated_metric TYPE zsi_a2cc_code_metrics.

    LOOP AT metrics REFERENCE INTO DATA(metric).
      aggregated_metric-component_to_be_changed =  metric->component_to_be_changed.
      aggregated_metric-package     =  metric->package.
      aggregated_metric-category    =  metric->category.
      aggregated_metric-modu_unit_1 =  metric->modu_unit_1.
      aggregated_metric-loc        = aggregated_metric-loc        + metric->loc       .
      IF metric->nos >= 2.
        aggregated_metric-nos        = aggregated_metric-nos        + metric->nos - 2   . "DO not Consider METHOD/ENDMETHOD
      ELSE.
        aggregated_metric-nos        = aggregated_metric-nos        + metric->nos.
      ENDIF.
      aggregated_metric-noc        = aggregated_metric-noc        + metric->noc       .
      aggregated_metric-nop        = aggregated_metric-nop        + metric->nop       .
      aggregated_metric-mcc_com    = aggregated_metric-mcc_com    + metric->mcc_com   .
      aggregated_metric-vrsd_chan  = aggregated_metric-vrsd_chan  + metric->vrsd_chan .
      aggregated_metric-dd_total   = aggregated_metric-dd_total   + metric->dd_total  .
      aggregated_metric-stmts_c    = aggregated_metric-stmts_c    + metric->stmts_c   .
      aggregated_metric-stmts_p    = aggregated_metric-stmts_p    + metric->stmts_p   .
      aggregated_metric-stmts_i    = aggregated_metric-stmts_i    + metric->stmts_i   .
      aggregated_metric-stmts_e    = aggregated_metric-stmts_e    + metric->stmts_e   .
      aggregated_metric-stmts_ei   = aggregated_metric-stmts_ei   + metric->stmts_ei  .
      aggregated_metric-stmts_d    = aggregated_metric-stmts_d    + metric->stmts_d   .
      aggregated_metric-stmts_w    = aggregated_metric-stmts_w    + metric->stmts_w   .
      aggregated_metric-stmts_l    = aggregated_metric-stmts_l    + metric->stmts_l   .
      aggregated_metric-stmts_ca   = aggregated_metric-stmts_ca   + metric->stmts_ca  .
      aggregated_metric-stmts_wh   = aggregated_metric-stmts_wh   + metric->stmts_wh  .
      aggregated_metric-stmts_sl   = aggregated_metric-stmts_sl   + metric->stmts_sl  .
      aggregated_metric-stmts_ins  = aggregated_metric-stmts_ins  + metric->stmts_ins .
      aggregated_metric-stmts_upd  = aggregated_metric-stmts_upd  + metric->stmts_upd .
      aggregated_metric-stmts_del  = aggregated_metric-stmts_del  + metric->stmts_del .
      aggregated_metric-stmts_mod  = aggregated_metric-stmts_mod  + metric->stmts_mod .
      aggregated_metric-diff_vers  = aggregated_metric-diff_vers  + metric->diff_vers .
      aggregated_metric-diff_mod_l = aggregated_metric-diff_mod_l + metric->diff_mod_l.
      aggregated_metric-diff_new_l = aggregated_metric-diff_new_l + metric->diff_new_l.
      aggregated_metric-diff_sap   = aggregated_metric-diff_sap   + metric->diff_sap  .
      aggregated_metric-mod_db     = aggregated_metric-mod_db     + metric->mod_db    .
      aggregated_metric-mod_call   = aggregated_metric-mod_call   + metric->mod_call  .
      aggregated_metric-mod_bra    = aggregated_metric-mod_bra    + metric->mod_bra   .
      aggregated_metric-number_of_methods = aggregated_metric-number_of_methods + 1.
      AT END OF modu_unit_1.
        aggregated_metric-average_nos_per_method = aggregated_metric-nos / aggregated_metric-number_of_methods.
        aggregated_metric-complexity_of_conditions = aggregated_metric-mcc_com / aggregated_metric-number_of_methods.
        aggregated_metric-decission_depht = aggregated_metric-dd_total / aggregated_metric-number_of_methods.
        APPEND aggregated_metric TO result.
        CLEAR aggregated_metric.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
