INTERFACE zif_i_a2cc_where_used
  PUBLIC .
  TYPES object_identifier TYPE string.
  TYPES object_list TYPE HASHED TABLE OF object_identifier WITH UNIQUE DEFAULT KEY.
  TYPES: BEGIN OF object_usages,
           object_identifier TYPE object_identifier,
           references        TYPE object_list,
         END OF object_usages.

  TYPES: BEGIN OF object_id,
           object    TYPE zei_a2cc_object_id,
           subobject TYPE zei_a2cc_object_id,
         END OF object_id.

  TYPES where_used_list TYPE HASHED TABLE OF object_usages WITH UNIQUE KEY object_identifier.

  METHODS get_cross_references
    IMPORTING
      object        TYPE object_id
    RETURNING
      VALUE(result) TYPE REF TO object_list.
ENDINTERFACE.
