*
* This is version 0.1.0
*
*The MIT License (MIT)
*
*Copyright (c) 2021 Benjamin Weisheit
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
CLASS zcl_i_a2cc_metrics_2_json DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_i_a2cc_metrics_2_json.
    METHODS constructor
      IMPORTING
        json_writer         TYPE REF TO if_sxml_writer
        package_analyzer    TYPE REF TO zif_i_a2cc_package_analyzer
        dependency_analyzer TYPE REF TO zif_i_a2cc_dependency_analyzer
        metric_aggregator   TYPE REF TO zif_i_a2cc_aggregator  .


    CONSTANTS system TYPE string VALUE `/root/SYSTEM/OUTSIDE`.

  PRIVATE SECTION.
    CONSTANTS numeric TYPE string VALUE 'num' ##NO_TEXT.
    CONSTANTS strng TYPE string VALUE 'str' ##NO_TEXT.
    CONSTANTS array TYPE string VALUE 'array' ##NO_TEXT.
    CONSTANTS objct TYPE string VALUE 'object' ##NO_TEXT.
    DATA object_where_used_list_by TYPE zif_i_a2cc_where_used=>where_used_list.
    DATA analyze_dependecies   TYPE abap_bool.
    DATA analyze_direct_cycles TYPE abap_bool.


    CLASS-DATA json_writer TYPE REF TO if_sxml_writer.

    METHODS write_package
      IMPORTING
        package TYPE zsi_a2cc_packages
      RAISING
        cx_sxml_state_error.
    METHODS convert2json
      RETURNING
        VALUE(result) TYPE string.
    METHODS write_edges.
    METHODS get_full_classpath
      IMPORTING
        class         TYPE classname
      RETURNING
        VALUE(result) TYPE string.
    METHODS open_document
      RAISING
        cx_sxml_state_error.
    METHODS write_node_system_outside
      RAISING
        cx_sxml_state_error.
    METHODS open_nodes
      RAISING
        cx_sxml_state_error.
    METHODS close_nodes
      RAISING
        cx_sxml_state_error.
    METHODS open_edges
      RAISING
        cx_sxml_state_error.
    METHODS close_edges
      RAISING
        cx_sxml_state_error.
    METHODS close_document
      RAISING
        cx_sxml_state_error.

    METHODS write_nodes
      RAISING
        cx_sxml_state_error.

    METHODS find_object_usages
      IMPORTING
        metrics TYPE ztti_a2cc_code_metrics.

    CLASS-METHODS write_element IMPORTING name  TYPE string
                                          attr  TYPE string OPTIONAL
                                          value TYPE string OPTIONAL
                                RAISING   cx_sxml_state_error.
    DATA: aggregated_metrics  TYPE ztti_a2cc_code_metrics,
          package_analyzer    TYPE REF TO zif_i_a2cc_package_analyzer,
          dependency_analyzer TYPE REF TO zif_i_a2cc_dependency_analyzer,
          metric_aggregator   TYPE REF TO zif_i_a2cc_aggregator,
          packages            TYPE ztti_a2cc_packages.
ENDCLASS.



CLASS zcl_i_a2cc_metrics_2_json IMPLEMENTATION.


  METHOD close_document.
    json_writer->close_element( ).
  ENDMETHOD.


  METHOD close_edges.
    json_writer->close_element( ).
  ENDMETHOD.


  METHOD close_nodes.
    json_writer->close_element( ).
    json_writer->close_element( ).
    json_writer->close_element( ).
  ENDMETHOD.


  METHOD constructor.
    me->json_writer = json_writer.
    me->package_analyzer = package_analyzer.
    me->dependency_analyzer = dependency_analyzer.
    me->metric_aggregator = metric_aggregator.
  ENDMETHOD.


  METHOD convert2json.
    open_document( ).
    open_nodes( ).
    write_node_system_outside( ).
    write_nodes( ).
    close_nodes( ).
    open_edges( ).
    write_edges( ).
    close_edges( ).
    close_document( ).

    DATA(xjson) = CAST cl_sxml_string_writer( json_writer )->get_output(  ).
    DATA(reader) = cl_sxml_string_reader=>create( xjson ).
    DATA(writer) = CAST if_sxml_writer(
                                 cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
    writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    writer->set_option( option = if_sxml_writer=>co_opt_indent ).
    reader->next_node( ).
    reader->skip_node( writer ).
    result = cl_abap_codepage=>convert_from( CAST cl_sxml_string_writer( writer )->get_output( ) ).
  ENDMETHOD.


  METHOD find_object_usages.
    object_where_used_list_by = dependency_analyzer->find_object_usages( metrics = metrics
                                                                         analyze_dependecies = analyze_dependecies
                                                                         analyze_direct_cycles = analyze_direct_cycles ).
  ENDMETHOD.


  METHOD get_full_classpath.
    DATA path TYPE string.

    TRY.
        DATA(package) = aggregated_metrics[ modu_unit_1 = class ]-package.
        path = package.
        path = package_analyzer->get_parent_packages( package ).
        result = |/root/{ path }/{ class }|.
      CATCH cx_sy_itab_line_not_found.
        result = system.
    ENDTRY.
  ENDMETHOD.


  METHOD open_document.
    write_element( name  = objct ).
    write_element( name  = strng  attr = 'projectName'  value = 'ABAP' ).
    json_writer->close_element( ).
    write_element( name  = strng  attr = 'apiVersion'  value = '1.1' ).
    json_writer->close_element( ).
  ENDMETHOD.


  METHOD open_edges.
    write_element( name  = array attr = 'edges' ).
  ENDMETHOD.


  METHOD open_nodes.
    write_element( name  = array attr = 'nodes' ).
    write_element( name  = objct ).
    write_element( name  = strng  attr = 'name'  value = 'root' ).
    json_writer->close_element( ).
    write_element( name  = strng  attr = 'type'  value = 'Folder' ).
    json_writer->close_element( ).
    write_element( name  = objct attr = 'attributes' ).
    json_writer->close_element( ).
    write_element( name  = strng  attr = 'link' ).
    json_writer->close_element( ).
    write_element( name  = array attr = 'children' ).
  ENDMETHOD.


  METHOD zif_i_a2cc_metrics_2_json~to_json.
    me->analyze_dependecies = analyze_dependecies.
    me->analyze_direct_cycles = analyze_direct_cycles.

    SORT metrics.

    find_object_usages( metrics ).
    aggregated_metrics = metric_aggregator->aggregate_metrics( metrics ).
    packages = package_analyzer->aggregate_packages( aggregated_metrics ).
    result = convert2json( ).
  ENDMETHOD.


  METHOD write_edges.
    IF analyze_dependecies = abap_true OR analyze_direct_cycles = abap_true.
      LOOP AT object_where_used_list_by REFERENCE INTO DATA(usage).
        LOOP AT usage->references REFERENCE INTO DATA(used_by)
          WHERE depending_object <> usage->object_identifier.
          write_element( name  = objct ).
          write_element( name  = strng  attr = 'fromNodeName'  value = get_full_classpath( CONV #( used_by->depending_object ) ) ).
          json_writer->close_element( ).
          write_element( name  = strng  attr = 'toNodeName'  value = get_full_classpath( CONV #( usage->object_identifier ) ) ).
          json_writer->close_element( ).
          write_element( name  = objct attr = 'attributes' ).
          IF analyze_dependecies = abap_true.
            write_element( name  = numeric  attr = 'usage'  value = CONV #( used_by->number_of_usages ) ).
            json_writer->close_element( ).
          ENDIF.
          IF used_by->cycle > 0 AND analyze_direct_cycles = abap_true.
            write_element( name  = numeric  attr = 'cycle'  value = CONV #( used_by->cycle ) ).
            json_writer->close_element( ).
          ENDIF.
          json_writer->close_element( ).
          json_writer->close_element( ).
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD write_element.
    json_writer->open_element( name ).
    IF attr IS NOT INITIAL.
      json_writer->write_attribute( name = 'name' value = attr ).
    ENDIF.
    IF value IS NOT INITIAL.
      json_writer->write_value( value ).
    ENDIF.
  ENDMETHOD.


  METHOD write_nodes.
    LOOP AT packages REFERENCE INTO DATA(package)
      WHERE parent IS INITIAL.
      write_package( package->* ).
    ENDLOOP.
  ENDMETHOD.


  METHOD write_node_system_outside.
    write_element( name  = objct ).
    write_element( name  = strng  attr = 'name'  value = 'SYSTEM' ).
    json_writer->close_element( ).
    write_element( name  = strng  attr = 'type'  value = 'Folder' ).
    json_writer->close_element( ).
    write_element( name  = objct attr = 'attributes' ).
    json_writer->close_element( ).
    write_element( name  = strng  attr = 'link' ).
    json_writer->close_element( ).
    write_element( name  = array attr = 'children' ).

    write_element( name  = objct ).
    write_element( name  = strng  attr = 'name'  value = CONV #( 'OUTSIDE' ) ).
    json_writer->close_element( ).
    write_element( name  = strng  attr = 'type'  value = 'File' ).
    json_writer->close_element( ).
    write_element( name  = objct attr = 'attributes' ).


    write_element( name  = numeric  attr = 'LinesOfCode'  value = '200' ).
    json_writer->close_element( ).
    write_element( name  = numeric  attr = 'Statements'  value = '200' ).
    json_writer->close_element( ).
    write_element( name  = numeric  attr = 'AvgStatementsPerMethod'  value = '30' ).
    json_writer->close_element( ).
    write_element( name  = numeric  attr = 'NumberOfChanges'  value = '10' ).
    json_writer->close_element( ).
    write_element( name  = numeric  attr = 'ComplexityOfConditions'  value = '10' ).
    json_writer->close_element( ).
    write_element( name  = numeric  attr = 'DecissionDepth'  value = '10' ).
    json_writer->close_element( ).

    json_writer->close_element( ).
    write_element( name  = strng  attr = 'link'  value = VALUE #(  ) ).
    json_writer->close_element( ).
    write_element( name  = array attr = 'children' ).
    json_writer->close_element( ).
    json_writer->close_element( ).

    json_writer->close_element( ).
    json_writer->close_element( ).
  ENDMETHOD.


  METHOD write_package.
    write_element( name  = objct ).
    write_element( name  = strng  attr = 'name'  value = CONV #( package-package ) ).
    json_writer->close_element( ).
    write_element( name  = strng  attr = 'type'  value = 'Folder' ).
    json_writer->close_element( ).
    write_element( name  = objct attr = 'attributes' ).
    json_writer->close_element( ).
    write_element( name  = strng  attr = 'link' ).
    json_writer->close_element( ).
    write_element( name  = array attr = 'children' ).
    LOOP AT aggregated_metrics REFERENCE INTO DATA(class)
      WHERE package = package-package.
      write_element( name  = objct ).
      write_element( name  = strng  attr = 'name'  value = |{ class->modu_unit_1 }.{ class->category }|  ).
      json_writer->close_element( ).
      write_element( name  = strng  attr = 'type'  value = 'File' ).
      json_writer->close_element( ).
      write_element( name  = objct attr = 'attributes' ).


      write_element( name  = numeric  attr = 'LinesOfCode'  value = CONV #( class->loc ) ).
      json_writer->close_element( ).
      write_element( name  = numeric  attr = 'Statements'  value = CONV #( class->nos ) ).
      json_writer->close_element( ).
      write_element( name  = numeric  attr = 'AvgStatementsPerMethod'  value = CONV #( class->average_nos_per_method ) ).
      json_writer->close_element( ).
      write_element( name  = numeric  attr = 'NumberOfChanges'  value = CONV #( class->diff_vers ) )..
      json_writer->close_element( ).
      write_element( name  = numeric  attr = 'DecissionDepth'  value = CONV #( class->decission_depth_complexity ) ).
      json_writer->close_element( ).
      write_element( name  = numeric  attr = 'DbAccesses'  value = CONV #( class->db_access_statements ) ).
      json_writer->close_element( ).
      write_element( name  = numeric  attr = 'Complexity'  value = CONV #( class->cyclomatic_complexity ) ).
      json_writer->close_element( ).
      write_element( name  = numeric  attr = 'AvgComplexityPerMethod'  value = CONV #( class->cyclomatic_complexity_avg ) ).
      json_writer->close_element( ).

      json_writer->close_element( ).
      write_element( name  = strng  attr = 'link'  value = VALUE #(  ) ).
      json_writer->close_element( ).
      write_element( name  = array attr = 'children' ).
      json_writer->close_element( ).
      json_writer->close_element( ).
    ENDLOOP.
    LOOP AT packages REFERENCE INTO DATA(child_package)
    WHERE parent = package-package.
      write_package( child_package->* ).
    ENDLOOP.
    json_writer->close_element( ).
    json_writer->close_element( ).
  ENDMETHOD.
ENDCLASS.
