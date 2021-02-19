INTERFACE zif_i_where_used
  PUBLIC .
  types object_identifier type string.
  TYPES object_list TYPE HASHED TABLE OF object_identifier WITH UNIQUE DEFAULT KEY.
  TYPES: BEGIN OF object_usages,
           object_identifier TYPE object_identifier,
           references TYPE object_list,
         END OF object_usages.

  TYPES where_used_list TYPE HASHED TABLE OF object_usages WITH UNIQUE KEY object_identifier.

  METHODS get_cross_references
    IMPORTING
      object TYPE string
      subobject TYPE string
    RETURNING VALUE(result) type ref to object_list.
ENDINTERFACE.
