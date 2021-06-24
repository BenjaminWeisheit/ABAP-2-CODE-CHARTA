CLASS zcl_i_a2cc_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF aggregation_levels,
                 package TYPE char1 VALUE 'P',
                 class   TYPE char1 VALUE 'C',
               END OF aggregation_levels.
    CLASS-METHODS create
      IMPORTING
        aggregation_level TYPE char1
      RETURNING
        VALUE(result)     TYPE REF TO Zif_I_A2CC_METRICS_2_JSON.
  PRIVATE SECTION.
    class-METHODS create_json_writer
      RETURNING
        VALUE(result) TYPE REF TO if_sxml_writer.

ENDCLASS.

CLASS zcl_i_a2cc_factory IMPLEMENTATION.
  METHOD create.
    DATA: dependency_analyzer TYPE REF TO zif_i_a2cc_dependency_analyzer,
          metric_aggregator   TYPE REF TO zif_i_a2cc_aggregator.

    dependency_analyzer = COND #( WHEN aggregation_level = aggregation_levels-package
                                  THEN NEW zcl_i_a2cc_dpndncy_anlyzr_devc( )
                                  ELSE NEW zcl_i_a2cc_dpndncy_anlyzr_Clas( ) ).

    metric_aggregator = COND #( WHEN aggregation_level = aggregation_levels-package
                                  THEN NEW zcl_i_a2cc_aggregator_package( )
                                  ELSE NEW zcl_i_a2cc_aggregator_class( ) ).

    result = NEW zcl_i_a2cc_metrics_2_json(
      json_writer         = create_json_writer( )
      dependency_analyzer = dependency_analyzer
      package_analyzer    = NEW zcl_i_a2cc_package_analyzer( )
      metric_aggregator   = metric_aggregator ).
  ENDMETHOD.

  METHOD create_json_writer.
    result = CAST #( cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
  ENDMETHOD.
ENDCLASS.
