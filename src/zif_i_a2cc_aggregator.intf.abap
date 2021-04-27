INTERFACE zif_i_a2cc_aggregator
  PUBLIC .
  METHODS aggregate_metrics
    IMPORTING
      metrics TYPE ztti_a2cc_code_metrics
    RETURNING
      VALUE(result) TYPE ztti_a2cc_code_metrics.
ENDINTERFACE.
