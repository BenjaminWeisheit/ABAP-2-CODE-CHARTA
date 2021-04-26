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
*SOFTWARE.CLASS zcl_i_metrics_to_json DEFINITION
CLASS zcl_i_a2cc_metrics_2_json DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS
      to_json
        IMPORTING
          VALUE(it_metrics) TYPE ztti_a2cc_code_metrics
        RETURNING
          VALUE(result)     TYPE string.

  PRIVATE SECTION.
    "  TYPES classnames TYPE HASHED TABLE OF classname WITH UNIQUE DEFAULT KEY.
    " TYPES: BEGIN OF object_usages,
    "         class      TYPE classname,
    "        references TYPE classnames,
    "     END OF object_usages.
    "TYPES where_used_list TYPE HASHED TABLE OF object_usages WITH UNIQUE KEY class.
    CONSTANTS system TYPE string VALUE `/root/SYSTEM/OUTSIDE`.
    "DATA class_uses TYPE where_used_list.
    "DATA class_used_by TYPE where_used_list.

    " DATA object_where_used_list TYPE zif_i_where_used=>where_used_list.
    DATA object_where_used_list_by TYPE zif_i_a2cc_where_used=>where_used_list.


    CLASS-DATA json_writer TYPE REF TO if_sxml_writer.
    METHODS aggregate_metrics
      IMPORTING
        it_metrics TYPE ztti_a2cc_code_metrics.

    METHODS aggregate_packages.
    METHODS find_object_usages
      IMPORTING
        it_metrics TYPE ztti_a2cc_code_metrics.

    METHODS write_package
      IMPORTING
        package TYPE zsi_a2cc_packages
      RAISING
        cx_sxml_state_error.
    METHODS convert2json
      RETURNING
        VALUE(result) TYPE string.
    .
    METHODS write_edges.
    METHODS get_full_classpath
      IMPORTING
        class         TYPE classname
      RETURNING
        VALUE(result) TYPE string.
    METHODS get_parent_packages
      IMPORTING
        package       TYPE zsi_a2cc_code_metrics-package
        path          TYPE string
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

    METHODS has_where_used_entries
      IMPORTING
        object_list   TYPE REF TO zif_i_a2cc_where_used=>object_list
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS collect_object_where_used_list
      IMPORTING
        metric      TYPE REF TO zsi_a2cc_code_metrics
        object_list TYPE REF TO zif_i_a2cc_where_used=>object_list.

    CLASS-METHODS write_element IMPORTING name  TYPE string
                                          attr  TYPE string OPTIONAL
                                          value TYPE string OPTIONAL
                                RAISING   cx_sxml_state_error.
    DATA: aggregated_metrics TYPE ztti_a2cc_code_metrics,
          packages           TYPE ztti_a2cc_packages.
ENDCLASS.

CLASS zcl_i_a2cc_metrics_2_json IMPLEMENTATION.
  METHOD constructor.
    json_writer =
      CAST if_sxml_writer(
             cl_sxml_string_writer=>create(
               type = if_sxml=>co_xt_json  ) ).
  ENDMETHOD.
  METHOD to_json.
    SORT it_metrics.

    find_object_usages( it_metrics ).
    aggregate_metrics( it_metrics ).
    aggregate_packages( ).
    result = convert2json( ).
  ENDMETHOD.

  METHOD aggregate_metrics.
    DATA aggregated_metric TYPE zsi_a2cc_code_metrics.

    LOOP AT it_metrics REFERENCE INTO DATA(metric).
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
        APPEND aggregated_metric TO aggregated_metrics.
        CLEAR aggregated_metric.
      ENDAT.
    ENDLOOP.
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

  METHOD write_nodes.

    LOOP AT packages REFERENCE INTO DATA(package)
      WHERE parent IS INITIAL.
      write_package( package->* ).
    ENDLOOP.

  ENDMETHOD.

  METHOD close_document.
    json_writer->close_element( ).
  ENDMETHOD.

  METHOD close_edges.
    json_writer->close_element( ).
  ENDMETHOD.

  METHOD open_edges.
    write_element( name  = 'array' attr = 'edges' ).
  ENDMETHOD.

  METHOD close_nodes.
    json_writer->close_element( ).
    json_writer->close_element( ).
    json_writer->close_element( ).
  ENDMETHOD.

  METHOD open_nodes.
    write_element( name  = 'array' attr = 'nodes' ).
    write_element( name  = 'object' ).
    write_element( name  = 'str'  attr = 'name'  value = 'root' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'type'  value = 'Folder' ).
    json_writer->close_element( ).
    write_element( name  = 'object' attr = 'attributes' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'link' ).
    json_writer->close_element( ).
    write_element( name  = 'array' attr = 'children' ).
  ENDMETHOD.

  METHOD write_node_system_outside.
    write_element( name  = 'object' ).
    write_element( name  = 'str'  attr = 'name'  value = 'SYSTEM' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'type'  value = 'Folder' ).
    json_writer->close_element( ).
    write_element( name  = 'object' attr = 'attributes' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'link' ).
    json_writer->close_element( ).
    write_element( name  = 'array' attr = 'children' ).

    write_element( name  = 'object' ).
    write_element( name  = 'str'  attr = 'name'  value = CONV #( 'OUTSIDE' ) ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'type'  value = 'File' ).
    json_writer->close_element( ).
    write_element( name  = 'object' attr = 'attributes' ).


    write_element( name  = 'num'  attr = 'LinesOfCode'  value = '200' ).
    json_writer->close_element( ).
    write_element( name  = 'num'  attr = 'Statements'  value = '200' ).
    json_writer->close_element( ).
    write_element( name  = 'num'  attr = 'AvgStatementsPerMethod'  value = '30' ).
    json_writer->close_element( ).
    write_element( name  = 'num'  attr = 'NumberOfChanges'  value = '10' ).
    json_writer->close_element( ).
    write_element( name  = 'num'  attr = 'ComplexityOfConditions'  value = '10' ).
    json_writer->close_element( ).
    write_element( name  = 'num'  attr = 'DecissionDepth'  value = '10' ).
    json_writer->close_element( ).

    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'link'  value = VALUE #(  ) ).
    json_writer->close_element( ).
    write_element( name  = 'array' attr = 'children' ).
    json_writer->close_element( ).
    json_writer->close_element( ).

    json_writer->close_element( ).
    json_writer->close_element( ).
  ENDMETHOD.

  METHOD open_document.
    write_element( name  = 'object').
    write_element( name  = 'str'  attr = 'projectName'  value = 'ABAP' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'apiVersion'  value = '1.1' ).
    json_writer->close_element( ).
  ENDMETHOD.

  METHOD write_package.
    write_element( name  = 'object' ).
    write_element( name  = 'str'  attr = 'name'  value = CONV #( package-package ) ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'type'  value = 'Folder' ).
    json_writer->close_element( ).
    write_element( name  = 'object' attr = 'attributes' ).
    json_writer->close_element( ).
    write_element( name  = 'str'  attr = 'link' ).
    json_writer->close_element( ).
    write_element( name  = 'array' attr = 'children' ).
    LOOP AT aggregated_metrics REFERENCE INTO DATA(class)
      WHERE package = package-package.
      write_element( name  = 'object' ).
      write_element( name  = 'str'  attr = 'name'  value = CONV #( class->modu_unit_1 ) ).
      json_writer->close_element( ).
      write_element( name  = 'str'  attr = 'type'  value = 'File' ).
      json_writer->close_element( ).
      write_element( name  = 'object' attr = 'attributes' ).


      write_element( name  = 'num'  attr = 'LinesOfCode'  value = CONV #( class->loc ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'Statements'  value = CONV #( class->nos ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'AvgStatementsPerMethod'  value = CONV #( class->average_nos_per_method ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'NumberOfChanges'  value = CONV #( class->diff_vers ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'ComplexityOfConditions'  value = CONV #( class->complexity_of_conditions ) ).
      json_writer->close_element( ).
      write_element( name  = 'num'  attr = 'DecissionDepth'  value = CONV #( class->decission_depht ) ).
      json_writer->close_element( ).

      json_writer->close_element( ).
      write_element( name  = 'str'  attr = 'link'  value = VALUE #(  ) ).
      json_writer->close_element( ).
      write_element( name  = 'array' attr = 'children' ).
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

  METHOD write_element.
    json_writer->open_element( name ).
    IF attr IS NOT INITIAL.
      json_writer->write_attribute( name = 'name' value = attr ).
    ENDIF.
    IF value IS NOT INITIAL.
      json_writer->write_value( value ).
    ENDIF.
  ENDMETHOD.

  METHOD aggregate_packages.
    DATA devclasses TYPE RANGE OF devclass.

    devclasses = VALUE #( FOR package IN aggregated_metrics ( sign = 'I' option = 'EQ' low = package-package ) ).
    SORT devclasses.
    DELETE ADJACENT DUPLICATES FROM devclasses.

    SELECT devclass, parentcl FROM tdevc INTO TABLE @DATA(package_hierarchy) WHERE devclass IN @devclasses AND parentcl IN @devclasses.

    LOOP AT aggregated_metrics REFERENCE INTO DATA(metric).
      AT NEW package.
        TRY.
            APPEND VALUE #( package  = metric->package
                            parent   = package_hierarchy[ devclass = metric->package ]-parentcl ) TO packages.
          CATCH cx_sy_itab_line_not_found.
            APPEND VALUE #( package  = metric->package ) TO packages.
        ENDTRY.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_object_usages.
    DATA: object_list TYPE REF TO zif_i_a2cc_where_used=>object_list.
    DATA(where_used_provider) = CAST zif_i_a2cc_where_used_provider( NEW zcl_i_a2cc_where_used_provider( ) ).
    LOOP AT it_metrics REFERENCE INTO DATA(metric).
      TRY.
          object_list = where_used_provider->get( objecttype = metric->category subtype = metric->sub_type )->get_cross_references( VALUE #( object    = metric->modu_unit_1
                                                                                                                                             subobject = metric->modu_unit_2 ) ).
          IF has_where_used_entries( object_list ).
            collect_object_where_used_list( metric = metric object_list = object_list ).
          ENDIF.
        CATCH zcx_i_a2cc_metrics_impl_error.
          "Fallback for unknown references
          INSERT VALUE #( object_identifier = metric->modu_unit_1 references = VALUE #( ( |{ system }| ) ) )
                 INTO TABLE object_where_used_list_by.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD has_where_used_entries.
    result = boolc( object_list IS NOT INITIAL AND lines( object_list->* ) > 0 ).
  ENDMETHOD.

  METHOD collect_object_where_used_list.

    TRY.
        DATA(references) = REF #( me->object_where_used_list_by[ object_identifier = metric->modu_unit_1 ]-references ).
        LOOP AT object_list->* REFERENCE INTO DATA(object).
          INSERT object->* INTO TABLE references->*.
        ENDLOOP.
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( object_identifier = metric->modu_unit_1 references = object_list->* ) INTO TABLE object_where_used_list_by.
    ENDTRY.

  ENDMETHOD.

  METHOD write_edges.
    LOOP AT object_where_used_list_by REFERENCE INTO DATA(usage).
      LOOP AT usage->references REFERENCE INTO DATA(used_by).
        write_element( name  = 'object' ).
        write_element( name  = 'str'  attr = 'fromNodeName'  value = get_full_classpath( CONV #( used_by->* ) ) ).
        json_writer->close_element( ).
        write_element( name  = 'str'  attr = 'toNodeName'  value = get_full_classpath( CONV #( usage->object_identifier ) ) ).
        json_writer->close_element( ).
        write_element( name  = 'object' attr = 'attributes' ).
        write_element( name  = 'num'  attr = 'usage'  value = '1' ).
        json_writer->close_element( ).
        json_writer->close_element( ).
        json_writer->close_element( ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_full_classpath.
    DATA path TYPE string.

    TRY.
        DATA(package) = aggregated_metrics[ modu_unit_1 = class ]-package.
        path = package.
        path = get_parent_packages( package = package
                                    path    = path ).
        result = |/root/{ path }/{ class }|.
      CATCH cx_sy_itab_line_not_found.
        result = system.
    ENDTRY.
  ENDMETHOD.

  METHOD get_parent_packages.
    TRY.
        DATA(parent_package) = packages[ package = package ]-parent.
        IF parent_package IS INITIAL.
          result = path.
        ELSE.
          result = get_parent_packages( package = parent_package
                                        path    = |{ parent_package }/{ path }| ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        result = path.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
