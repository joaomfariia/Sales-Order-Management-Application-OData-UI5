class ZCL_ZSO_MANAGE_DPC_EXT definition
  public
  inheriting from ZCL_ZSO_MANAGE_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
protected section.

  methods SOHEADERSET_CREATE_ENTITY
    redefinition .
  methods SOHEADERSET_DELETE_ENTITY
    redefinition .
  methods SOHEADERSET_GET_ENTITY
    redefinition .
  methods SOHEADERSET_GET_ENTITYSET
    redefinition .
  methods SOHEADERSET_UPDATE_ENTITY
    redefinition .
  methods SOITEMSET_CREATE_ENTITY
    redefinition .
  methods SOITEMSET_DELETE_ENTITY
    redefinition .
  methods SOITEMSET_GET_ENTITY
    redefinition .
  methods SOITEMSET_GET_ENTITYSET
    redefinition .
  methods SOITEMSET_UPDATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZSO_MANAGE_DPC_EXT IMPLEMENTATION.


METHOD soheaderset_create_entity.

  DATA ls_soheader TYPE zsoheader.

  TRY.
      io_data_provider->read_entry_data(
        IMPORTING
          es_data = ls_soheader
      ).
    CATCH /iwbep/cx_mgw_tech_exception /iwbep/cx_mgw_busi_exception INTO DATA(cx_error).
      cx_error->get_longtext( ).
  ENDTRY.

  SELECT SINGLE MAX( salesorderid )
  FROM zsoheader
  INTO @DATA(lv_max_id).

  DATA(lv_last_id) = CONV int4( lv_max_id ).
  lv_last_id += 1.
  ls_soheader-salesorderid = lv_last_id.
  ls_soheader-salesorderid = |{ ls_soheader-salesorderid ALPHA = IN }|.
  ls_soheader-createdby = cl_abap_context_info=>get_user_technical_name( ).
  ls_soheader-totalorder = ls_soheader-totalitems + ls_soheader-totalfreight.
  GET TIME STAMP FIELD ls_soheader-createdat.
  INSERT zsoheader FROM ls_soheader.

  IF sy-subrc NE 0.
    DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during order creation'
    ).

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  er_entity = CORRESPONDING #( ls_soheader ).

ENDMETHOD.


METHOD soheaderset_delete_entity.

  DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  DATA(lv_salesorderid) = VALUE #( it_key_tab[ name = 'SalesOrderId' ]-value OPTIONAL ).

  DELETE FROM zsoitem WHERE salesorderid EQ lv_salesorderid.
  IF sy-subrc NE 0.
    ROLLBACK WORK.

    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during sales order item deletion'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  DELETE FROM zsoheader WHERE salesorderid EQ lv_salesorderid.
  IF sy-subrc NE 0.
    ROLLBACK WORK.

    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during sales order deletion'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDMETHOD.


METHOD soheaderset_get_entity.

  DATA(lv_salesorderid) = VALUE #( it_key_tab[ name = 'SalesOrderId' ]-value OPTIONAL ).

  SELECT SINGLE *
  FROM zsoheader
  WHERE salesorderid EQ @lv_salesorderid
  INTO @DATA(ls_soheader).

  IF sy-subrc NE 0.
    DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Invalid Sales Order'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  er_entity = CORRESPONDING #( ls_soheader ).

ENDMETHOD.


METHOD soheaderset_get_entityset.

  DATA:
    lt_orderby TYPE TABLE OF string,
    lv_orderby TYPE string.

  LOOP AT it_order INTO DATA(ls_order).
    ls_order-order = to_upper( ls_order-order ).
    ls_order-order = COND #( WHEN ls_order-order EQ 'DESC' THEN 'DESCENDING'
                                                           ELSE 'ASCENDING' ).
    APPEND |{ ls_order-property } { ls_order-order }| TO lt_orderby.
  ENDLOOP.
  CONCATENATE LINES OF lt_orderby INTO lv_orderby.

  IF lv_orderby IS INITIAL.
    lv_orderby = |SalesOrderId ASCENDING|.
  ENDIF.

  SELECT FROM zsoheader
  FIELDS *
  WHERE (iv_filter_string)
  ORDER BY (lv_orderby)
  INTO TABLE @DATA(lt_soheader)
  UP TO @is_paging-top ROWS
  OFFSET @is_paging-skip.

  IF lines( lt_soheader ) EQ 0.
    DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'No sales orders found'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  et_entityset = CORRESPONDING #( lt_soheader ).

ENDMETHOD.


METHOD soheaderset_update_entity.

  DATA ls_soheader TYPE zsoheader.
  DATA has_error TYPE abap_bool.

  DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = ls_soheader
  ).

  IF ls_soheader-salesorderid EQ 0.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Invalid Sales Order number'
    ).
  ENDIF.

  IF ls_soheader-customerid EQ 0.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Invalid Customer number'
    ).
  ENDIF.

  IF ls_soheader-totalitems EQ 0
  OR ls_soheader-totalfreight EQ 0.
    has_error = abap_true.
    lo_message->add_message(
      iv_msg_type   = 'E'
      iv_msg_id     = 'ZSO'
      iv_msg_number = '001'
      iv_msg_v1     = |{ ls_soheader-salesorderid }|
    ).
  ENDIF.

  IF has_error EQ abap_true.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message
        http_status_code  = 400.
  ENDIF.

  UPDATE zsoheader
  SET customerid = ls_soheader-customerid
      totalitems = ls_soheader-totalitems
      totalfreight = ls_soheader-totalfreight
      totalorder = ls_soheader-totalorder
      status = ls_soheader-status
  WHERE salesorderid EQ ls_soheader-salesorderid.

  IF sy-subrc NE 0.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during update processing'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  er_entity = CORRESPONDING #( ls_soheader ).

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

  DATA:
    ls_deep_entity TYPE zcl_zso_manage_mpc_ext=>ts_soheader_deep,
    ls_soheader    TYPE zsoheader,
    lt_items       TYPE TABLE OF zsoitem,
    ls_item        LIKE LINE OF lt_items,
    is_update      TYPE abap_bool.

  DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = ls_deep_entity
  ).

  IF ls_deep_entity-salesorderid EQ 0. "Insert/POST
    is_update = abap_false.

    ls_soheader = CORRESPONDING #( ls_deep_entity ).
    ls_soheader-createdby = cl_abap_context_info=>get_user_technical_name( ).
    GET TIME STAMP FIELD ls_soheader-createdat.
    ls_soheader-totalorder = ls_soheader-totalitems + ls_soheader-totalfreight.

    SELECT SINGLE MAX( salesorderid )
    FROM zsoheader
    INTO @DATA(lv_max_id).
    IF sy-subrc EQ 0 .
      DATA(lv_last_id) = CONV int4( lv_max_id ).
      lv_last_id += 1.
      ls_soheader-salesorderid = lv_last_id.
      ls_soheader-salesorderid = |{ ls_soheader-salesorderid ALPHA = IN }|.
    ENDIF.

  ELSE. "Update/PUT/PATCH
    is_update = abap_true.

    "Fetch old data
    SELECT SINGLE *
    FROM zsoheader
    INTO @ls_soheader
    WHERE salesorderid EQ @ls_deep_entity-salesorderid.

    "Update with new data
    ls_soheader = CORRESPONDING #( ls_deep_entity ).
  ENDIF.

  LOOP AT ls_deep_entity-tosoitem ASSIGNING FIELD-SYMBOL(<fs_item>).
    ls_item = CORRESPONDING #( <fs_item> ).
    ls_item-salesorderid = ls_deep_entity-salesorderid.
    APPEND ls_item TO lt_items.
    CLEAR ls_item.
  ENDLOOP.

  IF is_update EQ abap_false. "Insert/POST
    "Header persistence
    INSERT zsoheader FROM ls_soheader.
    IF sy-subrc NE 0.
      lo_message->add_message_text_only(
        iv_msg_type = 'E'
        iv_msg_text = 'Error during Sales Order creation'
      ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message.
    ENDIF.

  ELSE. "Update/PUT/PATCH
    MODIFY zsoheader FROM ls_soheader.
    IF sy-subrc NE 0.
      ROLLBACK WORK.

      lo_message->add_message_text_only(
        iv_msg_type = 'E'
        iv_msg_text = 'Error during Sales Order update'
      ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message.
    ENDIF.
  ENDIF.

  "Item persistence
  DELETE FROM zsoitem WHERE salesorderid EQ ls_soheader-salesorderid.
  IF lines( lt_items ) GT 0. "New items to be added
    INSERT zsoitem FROM TABLE lt_items.
    IF sy-subrc NE 0.
      lo_message->add_message_text_only(
        iv_msg_type = 'E'
        iv_msg_text = 'Error during item insertion'
      ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message.
    ENDIF.
  ENDIF.

  COMMIT WORK AND WAIT.

  "Updating the return structure
  ls_deep_entity = CORRESPONDING #( ls_soheader ).
  ls_deep_entity-tosoitem = lt_items[].

  LOOP AT ls_deep_entity-tosoitem ASSIGNING FIELD-SYMBOL(<fs_deep_item>).
    <fs_deep_item>-salesorderid = ls_soheader-salesorderid.
  ENDLOOP.

  me->copy_data_to_ref(
    EXPORTING
      is_data = ls_deep_entity
    CHANGING
      cr_data = er_deep_entity
  ).

ENDMETHOD.


METHOD soitemset_create_entity.

  DATA ls_soitem TYPE zsoitem.
  DATA has_error TYPE abap_bool.

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = ls_soitem
  ).

  DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  SELECT SINGLE FROM zsoheader
  FIELDS salesorderid
  WHERE salesorderid EQ @ls_soitem-salesorderid
  INTO @DATA(lv_salesorderid).

  IF sy-subrc NE 0.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Invalid Sales Order number'
    ).
  ENDIF.

  SELECT SINGLE FROM zsoitem
  FIELDS MAX( salesorderitem ) AS maxitemid
  WHERE salesorderid EQ @ls_soitem-salesorderid
  INTO @DATA(lv_max_itemid).

  ls_soitem-salesorderitem = lv_max_itemid + 10.
  ls_soitem-totalprice = ls_soitem-quantity * ls_soitem-priceunit.

  INSERT zsoitem FROM ls_soitem.
  IF sy-subrc NE 0.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during item creation'
    ).
  ENDIF.

  IF has_error EQ abap_true.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  er_entity = CORRESPONDING #( ls_soitem ).

ENDMETHOD.


METHOD soitemset_delete_entity.

  DATA lv_salesid TYPE zsoitem-salesorderid.
  DATA has_error TYPE abap_bool.

  DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  lv_salesid = VALUE #( it_key_tab[ name = 'SalesOrderId' ]-value OPTIONAL ).
  lv_salesid = |{ lv_salesid ALPHA = IN }|.
  DATA(lv_itemid) = VALUE #( it_key_tab[ name = 'SalesOrderItem' ]-value OPTIONAL ).

  SELECT SINGLE FROM zsoitem
  FIELDS salesorderid, salesorderitem
  WHERE salesorderid EQ @lv_salesid
    AND salesorderitem EQ @lv_itemid
  INTO @DATA(ls_item).

  IF sy-subrc NE 0.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Sales Order and Item not found'
    ).
  ENDIF.

  DELETE FROM zsoitem WHERE salesorderid EQ lv_salesid
                        AND salesorderitem EQ lv_itemid.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during item deletion'
    ).
  ENDIF.

  IF has_error EQ abap_true.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

ENDMETHOD.


METHOD soitemset_get_entity.

  DATA lv_salesid TYPE zsoitem-salesorderid.

  lv_salesid = VALUE #( it_key_tab[ name = 'SalesOrderId' ]-value OPTIONAL ).
  lv_salesid = |{ lv_salesid ALPHA = IN }|.
  DATA(lv_itemid) = VALUE #( it_key_tab[ name = 'SalesOrderItem' ]-value OPTIONAL ).

  SELECT SINGLE FROM zsoitem
  FIELDS *
  WHERE salesorderid EQ @lv_salesid
    AND salesorderitem EQ @lv_itemid
  INTO @DATA(ls_soitem).

  IF sy-subrc NE 0.
    DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Sales Order and Item not found'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  er_entity = CORRESPONDING #( ls_soitem ).

ENDMETHOD.


METHOD soitemset_get_entityset.

  DATA:
    lt_orderby TYPE TABLE OF string,
    lv_orderby TYPE string.

  LOOP AT it_order INTO DATA(ls_order).
    ls_order-order = to_upper( ls_order-order ).
    ls_order-order = COND #( WHEN ls_order-order EQ 'DESC' THEN 'DESCENDING'
                                                           ELSE 'ASCENDING' ).
    APPEND |{ ls_order-property } { ls_order-order }| TO lt_orderby.
  ENDLOOP.

  CONCATENATE LINES OF lt_orderby INTO lv_orderby.

  IF lv_orderby IS INITIAL.
    lv_orderby = |SalesOrderId ASCENDING|.
  ENDIF.

  SELECT FROM zsoitem
  FIELDS *
  WHERE (iv_filter_string)
  ORDER BY (lv_orderby)
  INTO TABLE @DATA(lt_soitem)
  UP TO @is_paging-top ROWS
  OFFSET @is_paging-skip.

  IF lines( lt_soitem ) EQ 0.
    DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'No sales orders found'
    ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  et_entityset = VALUE #( FOR row IN lt_soitem ( CORRESPONDING #( row ) ) ).

ENDMETHOD.


METHOD soitemset_update_entity.

  DATA ls_soitem TYPE zsoitem.
  DATA has_error TYPE abap_bool.

  DATA(lo_message) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = ls_soitem
  ).

  IF ls_soitem-salesorderitem EQ 0
  OR ls_soitem-salesorderid EQ 0.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Invalid Sales Order and Item'
    ).
  ENDIF.

  IF ls_soitem-material IS INITIAL.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Invalid Material Code'
    ).
  ENDIF.

  IF ls_soitem-quantity EQ 0
  OR ls_soitem-priceunit EQ 0.
    has_error = abap_true.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'The Quantity/Price Unit must be higher than 0'
    ).
  ENDIF.

  IF has_error EQ abap_true.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_message.
  ENDIF.

  UPDATE zsoitem
  SET salesorderid = ls_soitem-salesorderid
      salesorderitem = ls_soitem-salesorderitem
      material = ls_soitem-material
      description = ls_soitem-description
      quantity = ls_soitem-quantity
      priceunit = ls_soitem-priceunit
      totalprice = ls_soitem-priceunit
  WHERE salesorderid EQ ls_soitem-salesorderid
    AND salesorderitem EQ ls_soitem-salesorderitem.

  IF sy-subrc NE 0.
    lo_message->add_message_text_only(
      iv_msg_type = 'E'
      iv_msg_text = 'Error during update processing'
    ).
  ENDIF.

  er_entity = CORRESPONDING #( ls_soitem ).

ENDMETHOD.
ENDCLASS.
