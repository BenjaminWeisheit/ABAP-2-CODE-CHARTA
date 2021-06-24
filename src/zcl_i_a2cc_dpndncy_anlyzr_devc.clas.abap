CLASS zcl_i_a2cc_dpndncy_anlyzr_devc DEFINITION
  PUBLIC INHERITING FROM zcl_i_a2cc_dpndncy_anlyzr_clas
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS collect_object_where_used_list REDEFINITION.
    METHODS where_used_analysis REDEFINITION.

private section.

  types:
    BEGIN OF object_data,
             object_id TYPE zei_a2cc_object_id,
             package   TYPE char30,
           END OF object_data .

  data:
    packages TYPE HASHED TABLE OF object_data WITH UNIQUE KEY object_id .

  methods AGGREGATE_TO_PACKAGE_LEVEL
    importing
      !DEPENDING_OBJECTS type ZIF_I_A2CC_WHERE_USED=>OBJECT_LIST
    returning
      value(RESULT) type ZIF_I_A2CC_WHERE_USED=>OBJECT_LIST .
ENDCLASS.



CLASS ZCL_I_A2CC_DPNDNCY_ANLYZR_DEVC IMPLEMENTATION.


  METHOD aggregate_to_package_level.
    result = depending_objects.

    LOOP AT result REFERENCE INTO DATA(depending_object).
      data(package) = COND string( WHEN line_exists( packages[ object_id = depending_object->depending_object ] )
                                   THEN packages[ object_id = depending_object->depending_object ]-package
                                   ELSE depending_object->depending_object ).

    insert VALUE #( depending_object = package ) into TABLE result.
    ENDLOOP.
    SORT result.
    DELETE ADJACENT DUPLICATES FROM result COMPARING depending_object.
  ENDMETHOD.


  METHOD collect_object_where_used_list.
    TRY.
        DATA(references) = REF #( me->object_where_used_list_by[ object_identifier = metric->package ]-references ).
        LOOP AT depending_objects REFERENCE INTO DATA(depending_object).
          IF line_exists( references->*[ depending_object = depending_object->depending_object ] ).
            ASSIGN references->*[ depending_object = depending_object->depending_object ] TO FIELD-SYMBOL(<existing_reference>).
            <existing_reference>-number_of_usages = <existing_reference>-number_of_usages + 1.
          ELSE.
            INSERT depending_object->* INTO TABLE references->*.
          ENDIF.
        ENDLOOP.
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( object_identifier = metric->package references = depending_objects ) INTO TABLE object_where_used_list_by.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.


  METHOD where_used_analysis.
    LOOP AT metrics REFERENCE INTO DATA(metric).
      INSERT VALUE #( object_id = metric->modu_unit_1 package = metric->package ) INTO TABLE packages.
    ENDLOOP.

    LOOP AT metrics REFERENCE INTO metric.
      TRY.
          DATA(depending_objects) = where_used_provider->get( objecttype = metric->category subtype = metric->sub_type )->get_cross_references( VALUE #( object    = metric->modu_unit_1
                                                                                                                                                         subobject = metric->modu_unit_2 ) ).
          IF has_where_used_entries( depending_objects ).
            collect_object_where_used_list( metric = metric depending_objects = aggregate_to_package_level( depending_objects->* ) ).
          ENDIF.
        CATCH zcx_i_a2cc_metrics_impl_error.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
