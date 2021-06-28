INTERFACE zif_i_a2cc_where_used
  PUBLIC .
  TYPES object_identifier TYPE string.

  TYPES: BEGIN OF depending_object,
           depending_object TYPE object_identifier,
           depending_type   TYPE trobjtype,
           number_of_usages TYPE i,
           cycle            TYPE i,
         END OF depending_object.

  TYPES object_list TYPE HASHED TABLE OF depending_object WITH UNIQUE KEY depending_object depending_type.
  TYPES: BEGIN OF object_usages,
           object_identifier TYPE object_identifier,
           object_type       TYPE trobjtype,
           references        TYPE object_list,
         END OF object_usages.

  TYPES: BEGIN OF object_id,
           object    TYPE zei_a2cc_object_id,
           subobject TYPE zei_a2cc_object_id,
         END OF object_id.

  TYPES where_used_list TYPE HASHED TABLE OF object_usages WITH UNIQUE KEY object_identifier object_type.

  METHODS get_cross_references
    IMPORTING
      object        TYPE object_id
    RETURNING
      VALUE(result) TYPE REF TO object_list.
ENDINTERFACE.
