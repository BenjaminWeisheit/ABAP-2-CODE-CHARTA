INTERFACE zif_i_a2cc_dependency_analyzer
  PUBLIC .
  METHODS find_object_usages
    IMPORTING
      metrics    TYPE ztti_a2cc_code_metrics
    RETURNING
      VALUE(result) TYPE zif_i_a2cc_where_used=>where_used_list.
ENDINTERFACE.
