CLASS zcl_i_a2cc_metrics_runner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS run_quality_metrics_report
      IMPORTING
        selection_variant TYPE variant
      RETURNING
        VALUE(result)     TYPE ztti_a2cc_code_metrics
      RAISING
        cx_salv_bs_sc_runtime_info.
  PRIVATE SECTION.
    METHODS set_alv_runtime_info.
    METHODS submit_code_metrics_report
      IMPORTING
        selection_variant TYPE variant.
    METHODS get_alv_list_from_report
      RETURNING
        VALUE(result) TYPE ztti_a2cc_code_metrics
      RAISING
        cx_salv_bs_sc_runtime_info.
ENDCLASS.

CLASS zcl_i_a2cc_metrics_runner IMPLEMENTATION.
  METHOD run_quality_metrics_report.
    set_alv_runtime_info( ).
    submit_code_metrics_report( selection_variant ).
    result = get_alv_list_from_report( ).
  ENDMETHOD.

  METHOD get_alv_list_from_report.
    FIELD-SYMBOLS <alv_list>   TYPE STANDARD TABLE.
    DATA alv_list              TYPE REF TO data.
    cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = alv_list ).
    ASSIGN alv_list->* TO <alv_list>.
    MOVE-CORRESPONDING <alv_list> TO result.
  ENDMETHOD.

  METHOD submit_code_metrics_report.
    SUBMIT /sdf/cd_custom_code_metric
    USING SELECTION-SET selection_variant
    EXPORTING LIST TO MEMORY
    AND RETURN.
  ENDMETHOD.

  METHOD set_alv_runtime_info.
    cl_salv_bs_runtime_info=>set(
      EXPORTING display  = abap_false
                metadata = abap_false
                data     = abap_true ).
  ENDMETHOD.
ENDCLASS.
