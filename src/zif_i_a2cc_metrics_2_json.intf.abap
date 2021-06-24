INTERFACE zif_i_a2cc_metrics_2_json
  PUBLIC .
  METHODS
    to_json
      IMPORTING
        VALUE(metrics)     TYPE ztti_a2cc_code_metrics
        analyze_dependecies   TYPE abap_bool
        analyze_direct_cycles TYPE abap_bool
      RETURNING
        VALUE(result)         TYPE string.

endinterface.
