(** PostgreSQL SQLSTATE codes.

    Generated from [src/backend/utils/errcodes.txt] in the PostgreSQL source;
    262 codes across 43 classes. The point is to let a caller act on a failure
    rather than print it: retry a serialization failure, return 409 on a unique
    violation, 400 on a check violation.

    {[
      match Sqlml.Error.sqlstate e with
      | Some s when Sqlml.Sqlstate.is_retryable s -> retry ()
      | Some s when Sqlml.Sqlstate.is_unique_violation s -> conflict ()
      | _ -> internal_error ()
    ]} *)

type t
(** A five-character SQLSTATE. *)

val of_string : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit

val name : t -> string
(** Canonical condition name, e.g. ["unique_violation"]. The code itself for
    anything PostgreSQL does not name. *)

(** {1 Conditions} *)

type condition =
  | Successful_completion
  | Warning
  | Warning_dynamic_result_sets_returned
  | Warning_implicit_zero_bit_padding
  | Warning_null_value_eliminated_in_set_function
  | Warning_privilege_not_granted
  | Warning_privilege_not_revoked
  | Warning_string_data_right_truncation
  | Warning_deprecated_feature
  | No_data
  | No_additional_dynamic_result_sets_returned
  | Sql_statement_not_yet_complete
  | Connection_exception
  | Connection_does_not_exist
  | Connection_failure
  | Sqlclient_unable_to_establish_sqlconnection
  | Sqlserver_rejected_establishment_of_sqlconnection
  | Transaction_resolution_unknown
  | Protocol_violation
  | Triggered_action_exception
  | Feature_not_supported
  | Invalid_transaction_initiation
  | Locator_exception
  | L_e_invalid_specification
  | Invalid_grantor
  | Invalid_grant_operation
  | Invalid_role_specification
  | Diagnostics_exception
  | Stacked_diagnostics_accessed_without_active_handler
  | Invalid_argument_for_xquery
  | Case_not_found
  | Cardinality_violation
  | Data_exception
  | Array_subscript_error
  | Character_not_in_repertoire
  | Datetime_field_overflow
  | Division_by_zero
  | Error_in_assignment
  | Escape_character_conflict
  | Indicator_overflow
  | Interval_field_overflow
  | Invalid_argument_for_log
  | Invalid_argument_for_ntile
  | Invalid_argument_for_nth_value
  | Invalid_argument_for_power_function
  | Invalid_argument_for_width_bucket_function
  | Invalid_character_value_for_cast
  | Invalid_datetime_format
  | Invalid_escape_character
  | Invalid_escape_octet
  | Invalid_escape_sequence
  | Nonstandard_use_of_escape_character
  | Invalid_indicator_parameter_value
  | Invalid_parameter_value
  | Invalid_preceding_or_following_size
  | Invalid_regular_expression
  | Invalid_row_count_in_limit_clause
  | Invalid_row_count_in_result_offset_clause
  | Invalid_tablesample_argument
  | Invalid_tablesample_repeat
  | Invalid_time_zone_displacement_value
  | Invalid_use_of_escape_character
  | Most_specific_type_mismatch
  | Null_value_not_allowed
  | Null_value_no_indicator_parameter
  | Numeric_value_out_of_range
  | Sequence_generator_limit_exceeded
  | String_data_length_mismatch
  | String_data_right_truncation
  | Substring_error
  | Trim_error
  | Unterminated_c_string
  | Zero_length_character_string
  | Floating_point_exception
  | Invalid_text_representation
  | Invalid_binary_representation
  | Bad_copy_file_format
  | Untranslatable_character
  | Not_an_xml_document
  | Invalid_xml_document
  | Invalid_xml_content
  | Invalid_xml_comment
  | Invalid_xml_processing_instruction
  | Duplicate_json_object_key_value
  | Invalid_argument_for_sql_json_datetime_function
  | Invalid_json_text
  | Invalid_sql_json_subscript
  | More_than_one_sql_json_item
  | No_sql_json_item
  | Non_numeric_sql_json_item
  | Non_unique_keys_in_a_json_object
  | Singleton_sql_json_item_required
  | Sql_json_array_not_found
  | Sql_json_member_not_found
  | Sql_json_number_not_found
  | Sql_json_object_not_found
  | Too_many_json_array_elements
  | Too_many_json_object_members
  | Sql_json_scalar_required
  | Sql_json_item_cannot_be_cast_to_target_type
  | Integrity_constraint_violation
  | Restrict_violation
  | Not_null_violation
  | Foreign_key_violation
  | Unique_violation
  | Check_violation
  | Exclusion_violation
  | Invalid_cursor_state
  | Invalid_transaction_state
  | Active_sql_transaction
  | Branch_transaction_already_active
  | Held_cursor_requires_same_isolation_level
  | Inappropriate_access_mode_for_branch_transaction
  | Inappropriate_isolation_level_for_branch_transaction
  | No_active_sql_transaction_for_branch_transaction
  | Read_only_sql_transaction
  | Schema_and_data_statement_mixing_not_supported
  | No_active_sql_transaction
  | In_failed_sql_transaction
  | Idle_in_transaction_session_timeout
  | Transaction_timeout
  | Invalid_sql_statement_name
  | Triggered_data_change_violation
  | Invalid_authorization_specification
  | Invalid_password
  | Dependent_privilege_descriptors_still_exist
  | Dependent_objects_still_exist
  | Invalid_transaction_termination
  | Sql_routine_exception
  | S_r_e_function_executed_no_return_statement
  | S_r_e_modifying_sql_data_not_permitted
  | S_r_e_prohibited_sql_statement_attempted
  | S_r_e_reading_sql_data_not_permitted
  | Invalid_cursor_name
  | External_routine_exception
  | E_r_e_containing_sql_not_permitted
  | E_r_e_modifying_sql_data_not_permitted
  | E_r_e_prohibited_sql_statement_attempted
  | E_r_e_reading_sql_data_not_permitted
  | External_routine_invocation_exception
  | E_r_i_e_invalid_sqlstate_returned
  | E_r_i_e_null_value_not_allowed
  | E_r_i_e_trigger_protocol_violated
  | E_r_i_e_srf_protocol_violated
  | E_r_i_e_event_trigger_protocol_violated
  | Savepoint_exception
  | S_e_invalid_specification
  | Invalid_catalog_name
  | Invalid_schema_name
  | Transaction_rollback
  | T_r_integrity_constraint_violation
  | T_r_serialization_failure
  | T_r_statement_completion_unknown
  | T_r_deadlock_detected
  | Syntax_error_or_access_rule_violation
  | Syntax_error
  | Insufficient_privilege
  | Cannot_coerce
  | Grouping_error
  | Windowing_error
  | Invalid_recursion
  | Invalid_foreign_key
  | Invalid_name
  | Name_too_long
  | Reserved_name
  | Datatype_mismatch
  | Indeterminate_datatype
  | Collation_mismatch
  | Indeterminate_collation
  | Wrong_object_type
  | Generated_always
  | Undefined_column
  | Undefined_function
  | Undefined_table
  | Undefined_parameter
  | Undefined_object
  | Duplicate_column
  | Duplicate_cursor
  | Duplicate_database
  | Duplicate_function
  | Duplicate_pstatement
  | Duplicate_schema
  | Duplicate_table
  | Duplicate_alias
  | Duplicate_object
  | Ambiguous_column
  | Ambiguous_function
  | Ambiguous_parameter
  | Ambiguous_alias
  | Invalid_column_reference
  | Invalid_column_definition
  | Invalid_cursor_definition
  | Invalid_database_definition
  | Invalid_function_definition
  | Invalid_pstatement_definition
  | Invalid_schema_definition
  | Invalid_table_definition
  | Invalid_object_definition
  | With_check_option_violation
  | Insufficient_resources
  | Disk_full
  | Out_of_memory
  | Too_many_connections
  | Configuration_limit_exceeded
  | Program_limit_exceeded
  | Statement_too_complex
  | Too_many_columns
  | Too_many_arguments
  | Object_not_in_prerequisite_state
  | Object_in_use
  | Cant_change_runtime_param
  | Lock_not_available
  | Unsafe_new_enum_value_usage
  | Operator_intervention
  | Query_canceled
  | Admin_shutdown
  | Crash_shutdown
  | Cannot_connect_now
  | Database_dropped
  | Idle_session_timeout
  | System_error
  | Io_error
  | Undefined_file
  | Duplicate_file
  | File_name_too_long
  | Config_file_error
  | Lock_file_exists
  | Fdw_error
  | Fdw_column_name_not_found
  | Fdw_dynamic_parameter_value_needed
  | Fdw_function_sequence_error
  | Fdw_inconsistent_descriptor_information
  | Fdw_invalid_attribute_value
  | Fdw_invalid_column_name
  | Fdw_invalid_column_number
  | Fdw_invalid_data_type
  | Fdw_invalid_data_type_descriptors
  | Fdw_invalid_descriptor_field_identifier
  | Fdw_invalid_handle
  | Fdw_invalid_option_index
  | Fdw_invalid_option_name
  | Fdw_invalid_string_length_or_buffer_length
  | Fdw_invalid_string_format
  | Fdw_invalid_use_of_null_pointer
  | Fdw_too_many_handles
  | Fdw_out_of_memory
  | Fdw_no_schemas
  | Fdw_option_name_not_found
  | Fdw_reply_handle
  | Fdw_schema_not_found
  | Fdw_table_not_found
  | Fdw_unable_to_create_execution
  | Fdw_unable_to_create_reply
  | Fdw_unable_to_establish_connection
  | Plpgsql_error
  | Raise_exception
  | No_data_found
  | Too_many_rows
  | Assert_failure
  | Internal_error
  | Data_corrupted
  | Index_corrupted
  | Other of string  (** a code this version of sqlml does not know *)

val condition : t -> condition

(** {1 Classes} *)

module Class : sig
  type t =
    | Successful_completion
    | Warning
    | No_data_this_is_also_a_warning_class_per_the_sql_standard
    | Sql_statement_not_yet_complete
    | Connection_exception
    | Triggered_action_exception
    | Feature_not_supported
    | Invalid_transaction_initiation
    | Locator_exception
    | Invalid_grantor
    | Invalid_role_specification
    | Diagnostics_exception
    | Xquery_error
    | Case_not_found
    | Cardinality_violation
    | Data_exception
    | Integrity_constraint_violation
    | Invalid_cursor_state
    | Invalid_transaction_state
    | Invalid_sql_statement_name
    | Triggered_data_change_violation
    | Invalid_authorization_specification
    | Dependent_privilege_descriptors_still_exist
    | Invalid_transaction_termination
    | Sql_routine_exception
    | Invalid_cursor_name
    | External_routine_exception
    | External_routine_invocation_exception
    | Savepoint_exception
    | Invalid_catalog_name
    | Invalid_schema_name
    | Transaction_rollback
    | Syntax_error_or_access_rule_violation
    | With_check_option_violation
    | Insufficient_resources
    | Program_limit_exceeded
    | Object_not_in_prerequisite_state
    | Operator_intervention
    | System_error_errors_external_to_postgresql_itself
    | Configuration_file_error
    | Foreign_data_wrapper_error_sql_med
    | Pl_pgsql_error
    | Internal_error
    | Other of string

  val to_string : t -> string
end

val class_ : t -> Class.t

(** {1 Common predicates} *)

val is_unique_violation : t -> bool
val is_foreign_key_violation : t -> bool
val is_not_null_violation : t -> bool
val is_check_violation : t -> bool
val is_exclusion_violation : t -> bool

val is_integrity_violation : t -> bool
(** Any class 23 code. *)

val is_serialization_failure : t -> bool
(** [40001] or [40P01]: the transaction lost a race and should be retried. *)

val is_connection_failure : t -> bool
(** Any class 08 code. *)

val is_retryable : t -> bool
(** A serialization failure, a deadlock, or a connection problem: re-running the
    transaction may succeed. Anything else will fail the same way again. *)

val is_syntax_or_access_error : t -> bool
(** Class 42: a bug in the query or a missing object, not a runtime condition. *)
