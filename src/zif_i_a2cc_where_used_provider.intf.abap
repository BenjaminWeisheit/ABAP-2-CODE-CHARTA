interface ZIF_I_A2CC_WHERE_USED_PROVIDER
  public .
  TYPES objecttype TYPE c LENGTH 4.
  TYPES subtype TYPE c LENGTH 4.
  METHODS  get IMPORTING objecttype TYPE zif_i_a2cc_where_used_provider=>objecttype subtype TYPE zif_i_a2cc_where_used_provider=>subtype RETURNING VALUE(result) TYPE REF TO zif_i_A2CC_where_used
  RAISING zcx_i_A2CC_metrics_impl_error.

endinterface.
