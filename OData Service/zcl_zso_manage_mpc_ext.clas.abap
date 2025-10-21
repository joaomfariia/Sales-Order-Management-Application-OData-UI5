CLASS zcl_zso_manage_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zso_manage_mpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_soheader_deep,
        salesorderid TYPE vbeln,
        customerid   TYPE numc4,
        createdby    TYPE uname,
        createdat    TYPE timestamp,
        totalitems   TYPE p LENGTH 15 DECIMALS 2,
        totalfreight TYPE p LENGTH 15 DECIMALS 2,
        totalorder   TYPE p LENGTH 15 DECIMALS 2,
        status       TYPE c LENGTH 1,
        toSOItem     TYPE TABLE OF ts_soitem WITH DEFAULT KEY,
      END OF ts_soheader_deep .

    METHODS define
        REDEFINITION .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZSO_MANAGE_MPC_EXT IMPLEMENTATION.


METHOD define.

  DATA lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ.

  super->define( ).

  lo_entity_type = model->get_entity_type( iv_entity_name = 'SOHeader' ).
  lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZSO_MANAGE_MPC_EXT=>TS_SOHEADER_DEEP' ).

ENDMETHOD.
ENDCLASS.
