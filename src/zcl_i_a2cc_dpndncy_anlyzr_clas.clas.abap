CLASS zcl_i_a2cc_dpndncy_anlyzr_clas DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_i_a2cc_dependency_analyzer.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS collect_object_where_used_list
      IMPORTING
        metric            TYPE REF TO zsi_a2cc_code_metrics
        depending_objects TYPE zif_i_a2cc_where_used=>object_list.
    METHODS where_used_analysis
      IMPORTING
        metrics TYPE ztti_a2cc_code_metrics.

    DATA where_used_provider TYPE REF TO zif_i_a2cc_where_used_provider.
    DATA object_where_used_list_by TYPE zif_i_a2cc_where_used=>where_used_list.

    METHODS has_where_used_entries
      IMPORTING
        object_list   TYPE REF TO zif_i_a2cc_where_used=>object_list
      RETURNING
        VALUE(result) TYPE abap_bool.
  PRIVATE SECTION.
    DATA: analyze_dependecies   TYPE abap_bool,
          analyze_direct_cycles TYPE abap_bool.
    METHODS analyze_cycles.
ENDCLASS.

CLASS zcl_i_a2cc_dpndncy_anlyzr_clas IMPLEMENTATION.
  METHOD constructor.
    where_used_provider = NEW zcl_i_a2cc_where_used_provider( ).
  ENDMETHOD.

  METHOD zif_i_a2cc_dependency_analyzer~find_object_usages.
    me->analyze_dependecies = analyze_dependecies.
    me->analyze_direct_cycles = analyze_direct_cycles.

    IF analyze_dependecies = abap_true OR analyze_direct_cycles = abap_true.
      where_used_analysis( metrics ).
    ENDIF.
    IF analyze_direct_cycles = abap_true.
      analyze_cycles( ).
    ENDIF.
    result = object_where_used_list_by.
  ENDMETHOD.

  METHOD where_used_analysis.
    LOOP AT metrics REFERENCE INTO DATA(metric).
      TRY.
          DATA(depending_objects) = where_used_provider->get( objecttype = metric->category subtype = metric->sub_type )->get_cross_references( VALUE #( object    = metric->modu_unit_1
                                                                                                                                                         subobject = metric->modu_unit_2 ) ).
          IF has_where_used_entries( depending_objects ).
            collect_object_where_used_list( metric = metric depending_objects = depending_objects->* ).
          ENDIF.
        CATCH zcx_i_a2cc_metrics_impl_error.
          "Fallback for unknown references
          INSERT VALUE #( object_identifier = metric->modu_unit_1 references = VALUE #( ( depending_object = |{ zcl_i_a2cc_metrics_2_json=>system }| number_of_usages = 1 ) ) )
                 INTO TABLE object_where_used_list_by.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD analyze_cycles.
    LOOP AT object_where_used_list_by REFERENCE INTO DATA(object_where_used).
      LOOP AT object_where_used->references REFERENCE INTO DATA(reference).
        DATA(line_index) = sy-tabix.
        TRY.
            DATA(back_refenrence) = object_where_used_list_by[ object_identifier = reference->depending_object ]-references[ depending_object = object_where_used->object_identifier ].
            reference->cycle = 1.
          CATCH cx_sy_itab_line_not_found.
            DELETE TABLE object_where_used->references FROM reference->*.
        ENDTRY.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD has_where_used_entries.
    result = boolc( object_list IS NOT INITIAL AND lines( object_list->* ) > 0 ).
  ENDMETHOD.

  METHOD collect_object_where_used_list.
    TRY.
        DATA(references) = REF #( me->object_where_used_list_by[ object_identifier = metric->modu_unit_1 ]-references ).
        LOOP AT depending_objects REFERENCE INTO DATA(depending_object).
          IF line_exists( references->*[ depending_object = depending_object->depending_object ] ).
            ASSIGN references->*[ depending_object = depending_object->depending_object ] TO FIELD-SYMBOL(<existing_reference>).
            <existing_reference>-number_of_usages = <existing_reference>-number_of_usages + 1.
          ELSE.
            INSERT depending_object->* INTO TABLE references->*.
          ENDIF.
        ENDLOOP.
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( object_identifier = metric->modu_unit_1 references = depending_objects ) INTO TABLE object_where_used_list_by.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
