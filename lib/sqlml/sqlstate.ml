type t = string

let of_string s = s
let to_string s = s
let equal = String.equal
let pp fmt s = Format.pp_print_string fmt s

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
  | Other of string

let condition = function
  | "00000" -> Successful_completion
  | "01000" -> Warning
  | "0100C" -> Warning_dynamic_result_sets_returned
  | "01008" -> Warning_implicit_zero_bit_padding
  | "01003" -> Warning_null_value_eliminated_in_set_function
  | "01007" -> Warning_privilege_not_granted
  | "01006" -> Warning_privilege_not_revoked
  | "01004" -> Warning_string_data_right_truncation
  | "01P01" -> Warning_deprecated_feature
  | "02000" -> No_data
  | "02001" -> No_additional_dynamic_result_sets_returned
  | "03000" -> Sql_statement_not_yet_complete
  | "08000" -> Connection_exception
  | "08003" -> Connection_does_not_exist
  | "08006" -> Connection_failure
  | "08001" -> Sqlclient_unable_to_establish_sqlconnection
  | "08004" -> Sqlserver_rejected_establishment_of_sqlconnection
  | "08007" -> Transaction_resolution_unknown
  | "08P01" -> Protocol_violation
  | "09000" -> Triggered_action_exception
  | "0A000" -> Feature_not_supported
  | "0B000" -> Invalid_transaction_initiation
  | "0F000" -> Locator_exception
  | "0F001" -> L_e_invalid_specification
  | "0L000" -> Invalid_grantor
  | "0LP01" -> Invalid_grant_operation
  | "0P000" -> Invalid_role_specification
  | "0Z000" -> Diagnostics_exception
  | "0Z002" -> Stacked_diagnostics_accessed_without_active_handler
  | "10608" -> Invalid_argument_for_xquery
  | "20000" -> Case_not_found
  | "21000" -> Cardinality_violation
  | "22000" -> Data_exception
  | "2202E" -> Array_subscript_error
  | "22021" -> Character_not_in_repertoire
  | "22008" -> Datetime_field_overflow
  | "22012" -> Division_by_zero
  | "22005" -> Error_in_assignment
  | "2200B" -> Escape_character_conflict
  | "22022" -> Indicator_overflow
  | "22015" -> Interval_field_overflow
  | "2201E" -> Invalid_argument_for_log
  | "22014" -> Invalid_argument_for_ntile
  | "22016" -> Invalid_argument_for_nth_value
  | "2201F" -> Invalid_argument_for_power_function
  | "2201G" -> Invalid_argument_for_width_bucket_function
  | "22018" -> Invalid_character_value_for_cast
  | "22007" -> Invalid_datetime_format
  | "22019" -> Invalid_escape_character
  | "2200D" -> Invalid_escape_octet
  | "22025" -> Invalid_escape_sequence
  | "22P06" -> Nonstandard_use_of_escape_character
  | "22010" -> Invalid_indicator_parameter_value
  | "22023" -> Invalid_parameter_value
  | "22013" -> Invalid_preceding_or_following_size
  | "2201B" -> Invalid_regular_expression
  | "2201W" -> Invalid_row_count_in_limit_clause
  | "2201X" -> Invalid_row_count_in_result_offset_clause
  | "2202H" -> Invalid_tablesample_argument
  | "2202G" -> Invalid_tablesample_repeat
  | "22009" -> Invalid_time_zone_displacement_value
  | "2200C" -> Invalid_use_of_escape_character
  | "2200G" -> Most_specific_type_mismatch
  | "22004" -> Null_value_not_allowed
  | "22002" -> Null_value_no_indicator_parameter
  | "22003" -> Numeric_value_out_of_range
  | "2200H" -> Sequence_generator_limit_exceeded
  | "22026" -> String_data_length_mismatch
  | "22001" -> String_data_right_truncation
  | "22011" -> Substring_error
  | "22027" -> Trim_error
  | "22024" -> Unterminated_c_string
  | "2200F" -> Zero_length_character_string
  | "22P01" -> Floating_point_exception
  | "22P02" -> Invalid_text_representation
  | "22P03" -> Invalid_binary_representation
  | "22P04" -> Bad_copy_file_format
  | "22P05" -> Untranslatable_character
  | "2200L" -> Not_an_xml_document
  | "2200M" -> Invalid_xml_document
  | "2200N" -> Invalid_xml_content
  | "2200S" -> Invalid_xml_comment
  | "2200T" -> Invalid_xml_processing_instruction
  | "22030" -> Duplicate_json_object_key_value
  | "22031" -> Invalid_argument_for_sql_json_datetime_function
  | "22032" -> Invalid_json_text
  | "22033" -> Invalid_sql_json_subscript
  | "22034" -> More_than_one_sql_json_item
  | "22035" -> No_sql_json_item
  | "22036" -> Non_numeric_sql_json_item
  | "22037" -> Non_unique_keys_in_a_json_object
  | "22038" -> Singleton_sql_json_item_required
  | "22039" -> Sql_json_array_not_found
  | "2203A" -> Sql_json_member_not_found
  | "2203B" -> Sql_json_number_not_found
  | "2203C" -> Sql_json_object_not_found
  | "2203D" -> Too_many_json_array_elements
  | "2203E" -> Too_many_json_object_members
  | "2203F" -> Sql_json_scalar_required
  | "2203G" -> Sql_json_item_cannot_be_cast_to_target_type
  | "23000" -> Integrity_constraint_violation
  | "23001" -> Restrict_violation
  | "23502" -> Not_null_violation
  | "23503" -> Foreign_key_violation
  | "23505" -> Unique_violation
  | "23514" -> Check_violation
  | "23P01" -> Exclusion_violation
  | "24000" -> Invalid_cursor_state
  | "25000" -> Invalid_transaction_state
  | "25001" -> Active_sql_transaction
  | "25002" -> Branch_transaction_already_active
  | "25008" -> Held_cursor_requires_same_isolation_level
  | "25003" -> Inappropriate_access_mode_for_branch_transaction
  | "25004" -> Inappropriate_isolation_level_for_branch_transaction
  | "25005" -> No_active_sql_transaction_for_branch_transaction
  | "25006" -> Read_only_sql_transaction
  | "25007" -> Schema_and_data_statement_mixing_not_supported
  | "25P01" -> No_active_sql_transaction
  | "25P02" -> In_failed_sql_transaction
  | "25P03" -> Idle_in_transaction_session_timeout
  | "25P04" -> Transaction_timeout
  | "26000" -> Invalid_sql_statement_name
  | "27000" -> Triggered_data_change_violation
  | "28000" -> Invalid_authorization_specification
  | "28P01" -> Invalid_password
  | "2B000" -> Dependent_privilege_descriptors_still_exist
  | "2BP01" -> Dependent_objects_still_exist
  | "2D000" -> Invalid_transaction_termination
  | "2F000" -> Sql_routine_exception
  | "2F005" -> S_r_e_function_executed_no_return_statement
  | "2F002" -> S_r_e_modifying_sql_data_not_permitted
  | "2F003" -> S_r_e_prohibited_sql_statement_attempted
  | "2F004" -> S_r_e_reading_sql_data_not_permitted
  | "34000" -> Invalid_cursor_name
  | "38000" -> External_routine_exception
  | "38001" -> E_r_e_containing_sql_not_permitted
  | "38002" -> E_r_e_modifying_sql_data_not_permitted
  | "38003" -> E_r_e_prohibited_sql_statement_attempted
  | "38004" -> E_r_e_reading_sql_data_not_permitted
  | "39000" -> External_routine_invocation_exception
  | "39001" -> E_r_i_e_invalid_sqlstate_returned
  | "39004" -> E_r_i_e_null_value_not_allowed
  | "39P01" -> E_r_i_e_trigger_protocol_violated
  | "39P02" -> E_r_i_e_srf_protocol_violated
  | "39P03" -> E_r_i_e_event_trigger_protocol_violated
  | "3B000" -> Savepoint_exception
  | "3B001" -> S_e_invalid_specification
  | "3D000" -> Invalid_catalog_name
  | "3F000" -> Invalid_schema_name
  | "40000" -> Transaction_rollback
  | "40002" -> T_r_integrity_constraint_violation
  | "40001" -> T_r_serialization_failure
  | "40003" -> T_r_statement_completion_unknown
  | "40P01" -> T_r_deadlock_detected
  | "42000" -> Syntax_error_or_access_rule_violation
  | "42601" -> Syntax_error
  | "42501" -> Insufficient_privilege
  | "42846" -> Cannot_coerce
  | "42803" -> Grouping_error
  | "42P20" -> Windowing_error
  | "42P19" -> Invalid_recursion
  | "42830" -> Invalid_foreign_key
  | "42602" -> Invalid_name
  | "42622" -> Name_too_long
  | "42939" -> Reserved_name
  | "42804" -> Datatype_mismatch
  | "42P18" -> Indeterminate_datatype
  | "42P21" -> Collation_mismatch
  | "42P22" -> Indeterminate_collation
  | "42809" -> Wrong_object_type
  | "428C9" -> Generated_always
  | "42703" -> Undefined_column
  | "42883" -> Undefined_function
  | "42P01" -> Undefined_table
  | "42P02" -> Undefined_parameter
  | "42704" -> Undefined_object
  | "42701" -> Duplicate_column
  | "42P03" -> Duplicate_cursor
  | "42P04" -> Duplicate_database
  | "42723" -> Duplicate_function
  | "42P05" -> Duplicate_pstatement
  | "42P06" -> Duplicate_schema
  | "42P07" -> Duplicate_table
  | "42712" -> Duplicate_alias
  | "42710" -> Duplicate_object
  | "42702" -> Ambiguous_column
  | "42725" -> Ambiguous_function
  | "42P08" -> Ambiguous_parameter
  | "42P09" -> Ambiguous_alias
  | "42P10" -> Invalid_column_reference
  | "42611" -> Invalid_column_definition
  | "42P11" -> Invalid_cursor_definition
  | "42P12" -> Invalid_database_definition
  | "42P13" -> Invalid_function_definition
  | "42P14" -> Invalid_pstatement_definition
  | "42P15" -> Invalid_schema_definition
  | "42P16" -> Invalid_table_definition
  | "42P17" -> Invalid_object_definition
  | "44000" -> With_check_option_violation
  | "53000" -> Insufficient_resources
  | "53100" -> Disk_full
  | "53200" -> Out_of_memory
  | "53300" -> Too_many_connections
  | "53400" -> Configuration_limit_exceeded
  | "54000" -> Program_limit_exceeded
  | "54001" -> Statement_too_complex
  | "54011" -> Too_many_columns
  | "54023" -> Too_many_arguments
  | "55000" -> Object_not_in_prerequisite_state
  | "55006" -> Object_in_use
  | "55P02" -> Cant_change_runtime_param
  | "55P03" -> Lock_not_available
  | "55P04" -> Unsafe_new_enum_value_usage
  | "57000" -> Operator_intervention
  | "57014" -> Query_canceled
  | "57P01" -> Admin_shutdown
  | "57P02" -> Crash_shutdown
  | "57P03" -> Cannot_connect_now
  | "57P04" -> Database_dropped
  | "57P05" -> Idle_session_timeout
  | "58000" -> System_error
  | "58030" -> Io_error
  | "58P01" -> Undefined_file
  | "58P02" -> Duplicate_file
  | "58P03" -> File_name_too_long
  | "F0000" -> Config_file_error
  | "F0001" -> Lock_file_exists
  | "HV000" -> Fdw_error
  | "HV005" -> Fdw_column_name_not_found
  | "HV002" -> Fdw_dynamic_parameter_value_needed
  | "HV010" -> Fdw_function_sequence_error
  | "HV021" -> Fdw_inconsistent_descriptor_information
  | "HV024" -> Fdw_invalid_attribute_value
  | "HV007" -> Fdw_invalid_column_name
  | "HV008" -> Fdw_invalid_column_number
  | "HV004" -> Fdw_invalid_data_type
  | "HV006" -> Fdw_invalid_data_type_descriptors
  | "HV091" -> Fdw_invalid_descriptor_field_identifier
  | "HV00B" -> Fdw_invalid_handle
  | "HV00C" -> Fdw_invalid_option_index
  | "HV00D" -> Fdw_invalid_option_name
  | "HV090" -> Fdw_invalid_string_length_or_buffer_length
  | "HV00A" -> Fdw_invalid_string_format
  | "HV009" -> Fdw_invalid_use_of_null_pointer
  | "HV014" -> Fdw_too_many_handles
  | "HV001" -> Fdw_out_of_memory
  | "HV00P" -> Fdw_no_schemas
  | "HV00J" -> Fdw_option_name_not_found
  | "HV00K" -> Fdw_reply_handle
  | "HV00Q" -> Fdw_schema_not_found
  | "HV00R" -> Fdw_table_not_found
  | "HV00L" -> Fdw_unable_to_create_execution
  | "HV00M" -> Fdw_unable_to_create_reply
  | "HV00N" -> Fdw_unable_to_establish_connection
  | "P0000" -> Plpgsql_error
  | "P0001" -> Raise_exception
  | "P0002" -> No_data_found
  | "P0003" -> Too_many_rows
  | "P0004" -> Assert_failure
  | "XX000" -> Internal_error
  | "XX001" -> Data_corrupted
  | "XX002" -> Index_corrupted
  | s -> Other s

let name = function
  | "00000" -> "successful_completion"
  | "01000" -> "warning"
  | "0100C" -> "dynamic_result_sets_returned"
  | "01008" -> "implicit_zero_bit_padding"
  | "01003" -> "null_value_eliminated_in_set_function"
  | "01007" -> "privilege_not_granted"
  | "01006" -> "privilege_not_revoked"
  | "01004" -> "string_data_right_truncation"
  | "01P01" -> "deprecated_feature"
  | "02000" -> "no_data"
  | "02001" -> "no_additional_dynamic_result_sets_returned"
  | "03000" -> "sql_statement_not_yet_complete"
  | "08000" -> "connection_exception"
  | "08003" -> "connection_does_not_exist"
  | "08006" -> "connection_failure"
  | "08001" -> "sqlclient_unable_to_establish_sqlconnection"
  | "08004" -> "sqlserver_rejected_establishment_of_sqlconnection"
  | "08007" -> "transaction_resolution_unknown"
  | "08P01" -> "protocol_violation"
  | "09000" -> "triggered_action_exception"
  | "0A000" -> "feature_not_supported"
  | "0B000" -> "invalid_transaction_initiation"
  | "0F000" -> "locator_exception"
  | "0F001" -> "invalid_locator_specification"
  | "0L000" -> "invalid_grantor"
  | "0LP01" -> "invalid_grant_operation"
  | "0P000" -> "invalid_role_specification"
  | "0Z000" -> "diagnostics_exception"
  | "0Z002" -> "stacked_diagnostics_accessed_without_active_handler"
  | "10608" -> "invalid_argument_for_xquery"
  | "20000" -> "case_not_found"
  | "21000" -> "cardinality_violation"
  | "22000" -> "data_exception"
  | "2202E" -> "array_subscript_error"
  | "22021" -> "character_not_in_repertoire"
  | "22008" -> "datetime_field_overflow"
  | "22012" -> "division_by_zero"
  | "22005" -> "error_in_assignment"
  | "2200B" -> "escape_character_conflict"
  | "22022" -> "indicator_overflow"
  | "22015" -> "interval_field_overflow"
  | "2201E" -> "invalid_argument_for_logarithm"
  | "22014" -> "invalid_argument_for_ntile_function"
  | "22016" -> "invalid_argument_for_nth_value_function"
  | "2201F" -> "invalid_argument_for_power_function"
  | "2201G" -> "invalid_argument_for_width_bucket_function"
  | "22018" -> "invalid_character_value_for_cast"
  | "22007" -> "invalid_datetime_format"
  | "22019" -> "invalid_escape_character"
  | "2200D" -> "invalid_escape_octet"
  | "22025" -> "invalid_escape_sequence"
  | "22P06" -> "nonstandard_use_of_escape_character"
  | "22010" -> "invalid_indicator_parameter_value"
  | "22023" -> "invalid_parameter_value"
  | "22013" -> "invalid_preceding_or_following_size"
  | "2201B" -> "invalid_regular_expression"
  | "2201W" -> "invalid_row_count_in_limit_clause"
  | "2201X" -> "invalid_row_count_in_result_offset_clause"
  | "2202H" -> "invalid_tablesample_argument"
  | "2202G" -> "invalid_tablesample_repeat"
  | "22009" -> "invalid_time_zone_displacement_value"
  | "2200C" -> "invalid_use_of_escape_character"
  | "2200G" -> "most_specific_type_mismatch"
  | "22004" -> "null_value_not_allowed"
  | "22002" -> "null_value_no_indicator_parameter"
  | "22003" -> "numeric_value_out_of_range"
  | "2200H" -> "sequence_generator_limit_exceeded"
  | "22026" -> "string_data_length_mismatch"
  | "22001" -> "string_data_right_truncation"
  | "22011" -> "substring_error"
  | "22027" -> "trim_error"
  | "22024" -> "unterminated_c_string"
  | "2200F" -> "zero_length_character_string"
  | "22P01" -> "floating_point_exception"
  | "22P02" -> "invalid_text_representation"
  | "22P03" -> "invalid_binary_representation"
  | "22P04" -> "bad_copy_file_format"
  | "22P05" -> "untranslatable_character"
  | "2200L" -> "not_an_xml_document"
  | "2200M" -> "invalid_xml_document"
  | "2200N" -> "invalid_xml_content"
  | "2200S" -> "invalid_xml_comment"
  | "2200T" -> "invalid_xml_processing_instruction"
  | "22030" -> "duplicate_json_object_key_value"
  | "22031" -> "invalid_argument_for_sql_json_datetime_function"
  | "22032" -> "invalid_json_text"
  | "22033" -> "invalid_sql_json_subscript"
  | "22034" -> "more_than_one_sql_json_item"
  | "22035" -> "no_sql_json_item"
  | "22036" -> "non_numeric_sql_json_item"
  | "22037" -> "non_unique_keys_in_a_json_object"
  | "22038" -> "singleton_sql_json_item_required"
  | "22039" -> "sql_json_array_not_found"
  | "2203A" -> "sql_json_member_not_found"
  | "2203B" -> "sql_json_number_not_found"
  | "2203C" -> "sql_json_object_not_found"
  | "2203D" -> "too_many_json_array_elements"
  | "2203E" -> "too_many_json_object_members"
  | "2203F" -> "sql_json_scalar_required"
  | "2203G" -> "sql_json_item_cannot_be_cast_to_target_type"
  | "23000" -> "integrity_constraint_violation"
  | "23001" -> "restrict_violation"
  | "23502" -> "not_null_violation"
  | "23503" -> "foreign_key_violation"
  | "23505" -> "unique_violation"
  | "23514" -> "check_violation"
  | "23P01" -> "exclusion_violation"
  | "24000" -> "invalid_cursor_state"
  | "25000" -> "invalid_transaction_state"
  | "25001" -> "active_sql_transaction"
  | "25002" -> "branch_transaction_already_active"
  | "25008" -> "held_cursor_requires_same_isolation_level"
  | "25003" -> "inappropriate_access_mode_for_branch_transaction"
  | "25004" -> "inappropriate_isolation_level_for_branch_transaction"
  | "25005" -> "no_active_sql_transaction_for_branch_transaction"
  | "25006" -> "read_only_sql_transaction"
  | "25007" -> "schema_and_data_statement_mixing_not_supported"
  | "25P01" -> "no_active_sql_transaction"
  | "25P02" -> "in_failed_sql_transaction"
  | "25P03" -> "idle_in_transaction_session_timeout"
  | "25P04" -> "transaction_timeout"
  | "26000" -> "invalid_sql_statement_name"
  | "27000" -> "triggered_data_change_violation"
  | "28000" -> "invalid_authorization_specification"
  | "28P01" -> "invalid_password"
  | "2B000" -> "dependent_privilege_descriptors_still_exist"
  | "2BP01" -> "dependent_objects_still_exist"
  | "2D000" -> "invalid_transaction_termination"
  | "2F000" -> "sql_routine_exception"
  | "2F005" -> "function_executed_no_return_statement"
  | "2F002" -> "modifying_sql_data_not_permitted"
  | "2F003" -> "prohibited_sql_statement_attempted"
  | "2F004" -> "reading_sql_data_not_permitted"
  | "34000" -> "invalid_cursor_name"
  | "38000" -> "external_routine_exception"
  | "38001" -> "containing_sql_not_permitted"
  | "38002" -> "modifying_sql_data_not_permitted"
  | "38003" -> "prohibited_sql_statement_attempted"
  | "38004" -> "reading_sql_data_not_permitted"
  | "39000" -> "external_routine_invocation_exception"
  | "39001" -> "invalid_sqlstate_returned"
  | "39004" -> "null_value_not_allowed"
  | "39P01" -> "trigger_protocol_violated"
  | "39P02" -> "srf_protocol_violated"
  | "39P03" -> "event_trigger_protocol_violated"
  | "3B000" -> "savepoint_exception"
  | "3B001" -> "invalid_savepoint_specification"
  | "3D000" -> "invalid_catalog_name"
  | "3F000" -> "invalid_schema_name"
  | "40000" -> "transaction_rollback"
  | "40002" -> "transaction_integrity_constraint_violation"
  | "40001" -> "serialization_failure"
  | "40003" -> "statement_completion_unknown"
  | "40P01" -> "deadlock_detected"
  | "42000" -> "syntax_error_or_access_rule_violation"
  | "42601" -> "syntax_error"
  | "42501" -> "insufficient_privilege"
  | "42846" -> "cannot_coerce"
  | "42803" -> "grouping_error"
  | "42P20" -> "windowing_error"
  | "42P19" -> "invalid_recursion"
  | "42830" -> "invalid_foreign_key"
  | "42602" -> "invalid_name"
  | "42622" -> "name_too_long"
  | "42939" -> "reserved_name"
  | "42804" -> "datatype_mismatch"
  | "42P18" -> "indeterminate_datatype"
  | "42P21" -> "collation_mismatch"
  | "42P22" -> "indeterminate_collation"
  | "42809" -> "wrong_object_type"
  | "428C9" -> "generated_always"
  | "42703" -> "undefined_column"
  | "42883" -> "undefined_function"
  | "42P01" -> "undefined_table"
  | "42P02" -> "undefined_parameter"
  | "42704" -> "undefined_object"
  | "42701" -> "duplicate_column"
  | "42P03" -> "duplicate_cursor"
  | "42P04" -> "duplicate_database"
  | "42723" -> "duplicate_function"
  | "42P05" -> "duplicate_prepared_statement"
  | "42P06" -> "duplicate_schema"
  | "42P07" -> "duplicate_table"
  | "42712" -> "duplicate_alias"
  | "42710" -> "duplicate_object"
  | "42702" -> "ambiguous_column"
  | "42725" -> "ambiguous_function"
  | "42P08" -> "ambiguous_parameter"
  | "42P09" -> "ambiguous_alias"
  | "42P10" -> "invalid_column_reference"
  | "42611" -> "invalid_column_definition"
  | "42P11" -> "invalid_cursor_definition"
  | "42P12" -> "invalid_database_definition"
  | "42P13" -> "invalid_function_definition"
  | "42P14" -> "invalid_prepared_statement_definition"
  | "42P15" -> "invalid_schema_definition"
  | "42P16" -> "invalid_table_definition"
  | "42P17" -> "invalid_object_definition"
  | "44000" -> "with_check_option_violation"
  | "53000" -> "insufficient_resources"
  | "53100" -> "disk_full"
  | "53200" -> "out_of_memory"
  | "53300" -> "too_many_connections"
  | "53400" -> "configuration_limit_exceeded"
  | "54000" -> "program_limit_exceeded"
  | "54001" -> "statement_too_complex"
  | "54011" -> "too_many_columns"
  | "54023" -> "too_many_arguments"
  | "55000" -> "object_not_in_prerequisite_state"
  | "55006" -> "object_in_use"
  | "55P02" -> "cant_change_runtime_param"
  | "55P03" -> "lock_not_available"
  | "55P04" -> "unsafe_new_enum_value_usage"
  | "57000" -> "operator_intervention"
  | "57014" -> "query_canceled"
  | "57P01" -> "admin_shutdown"
  | "57P02" -> "crash_shutdown"
  | "57P03" -> "cannot_connect_now"
  | "57P04" -> "database_dropped"
  | "57P05" -> "idle_session_timeout"
  | "58000" -> "system_error"
  | "58030" -> "io_error"
  | "58P01" -> "undefined_file"
  | "58P02" -> "duplicate_file"
  | "58P03" -> "file_name_too_long"
  | "F0000" -> "config_file_error"
  | "F0001" -> "lock_file_exists"
  | "HV000" -> "fdw_error"
  | "HV005" -> "fdw_column_name_not_found"
  | "HV002" -> "fdw_dynamic_parameter_value_needed"
  | "HV010" -> "fdw_function_sequence_error"
  | "HV021" -> "fdw_inconsistent_descriptor_information"
  | "HV024" -> "fdw_invalid_attribute_value"
  | "HV007" -> "fdw_invalid_column_name"
  | "HV008" -> "fdw_invalid_column_number"
  | "HV004" -> "fdw_invalid_data_type"
  | "HV006" -> "fdw_invalid_data_type_descriptors"
  | "HV091" -> "fdw_invalid_descriptor_field_identifier"
  | "HV00B" -> "fdw_invalid_handle"
  | "HV00C" -> "fdw_invalid_option_index"
  | "HV00D" -> "fdw_invalid_option_name"
  | "HV090" -> "fdw_invalid_string_length_or_buffer_length"
  | "HV00A" -> "fdw_invalid_string_format"
  | "HV009" -> "fdw_invalid_use_of_null_pointer"
  | "HV014" -> "fdw_too_many_handles"
  | "HV001" -> "fdw_out_of_memory"
  | "HV00P" -> "fdw_no_schemas"
  | "HV00J" -> "fdw_option_name_not_found"
  | "HV00K" -> "fdw_reply_handle"
  | "HV00Q" -> "fdw_schema_not_found"
  | "HV00R" -> "fdw_table_not_found"
  | "HV00L" -> "fdw_unable_to_create_execution"
  | "HV00M" -> "fdw_unable_to_create_reply"
  | "HV00N" -> "fdw_unable_to_establish_connection"
  | "P0000" -> "plpgsql_error"
  | "P0001" -> "raise_exception"
  | "P0002" -> "no_data_found"
  | "P0003" -> "too_many_rows"
  | "P0004" -> "assert_failure"
  | "XX000" -> "internal_error"
  | "XX001" -> "data_corrupted"
  | "XX002" -> "index_corrupted"
  | s -> s

module Class = struct
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

  let to_string = function
    | Successful_completion -> "Successful Completion"
    | Warning -> "Warning"
    | No_data_this_is_also_a_warning_class_per_the_sql_standard ->
        "No Data (this is also a warning class per the SQL standard)"
    | Sql_statement_not_yet_complete -> "SQL Statement Not Yet Complete"
    | Connection_exception -> "Connection Exception"
    | Triggered_action_exception -> "Triggered Action Exception"
    | Feature_not_supported -> "Feature Not Supported"
    | Invalid_transaction_initiation -> "Invalid Transaction Initiation"
    | Locator_exception -> "Locator Exception"
    | Invalid_grantor -> "Invalid Grantor"
    | Invalid_role_specification -> "Invalid Role Specification"
    | Diagnostics_exception -> "Diagnostics Exception"
    | Xquery_error -> "XQuery Error"
    | Case_not_found -> "Case Not Found"
    | Cardinality_violation -> "Cardinality Violation"
    | Data_exception -> "Data Exception"
    | Integrity_constraint_violation -> "Integrity Constraint Violation"
    | Invalid_cursor_state -> "Invalid Cursor State"
    | Invalid_transaction_state -> "Invalid Transaction State"
    | Invalid_sql_statement_name -> "Invalid SQL Statement Name"
    | Triggered_data_change_violation -> "Triggered Data Change Violation"
    | Invalid_authorization_specification -> "Invalid Authorization Specification"
    | Dependent_privilege_descriptors_still_exist ->
        "Dependent Privilege Descriptors Still Exist"
    | Invalid_transaction_termination -> "Invalid Transaction Termination"
    | Sql_routine_exception -> "SQL Routine Exception"
    | Invalid_cursor_name -> "Invalid Cursor Name"
    | External_routine_exception -> "External Routine Exception"
    | External_routine_invocation_exception -> "External Routine Invocation Exception"
    | Savepoint_exception -> "Savepoint Exception"
    | Invalid_catalog_name -> "Invalid Catalog Name"
    | Invalid_schema_name -> "Invalid Schema Name"
    | Transaction_rollback -> "Transaction Rollback"
    | Syntax_error_or_access_rule_violation -> "Syntax Error or Access Rule Violation"
    | With_check_option_violation -> "WITH CHECK OPTION Violation"
    | Insufficient_resources -> "Insufficient Resources"
    | Program_limit_exceeded -> "Program Limit Exceeded"
    | Object_not_in_prerequisite_state -> "Object Not In Prerequisite State"
    | Operator_intervention -> "Operator Intervention"
    | System_error_errors_external_to_postgresql_itself ->
        "System Error (errors external to PostgreSQL itself)"
    | Configuration_file_error -> "Configuration File Error"
    | Foreign_data_wrapper_error_sql_med -> "Foreign Data Wrapper Error (SQL/MED)"
    | Pl_pgsql_error -> "PL/pgSQL Error"
    | Internal_error -> "Internal Error"
    | Other s -> s
end

let class_ s =
  let c = if String.length s >= 2 then String.sub s 0 2 else s in
  match c with
  | "00" -> Class.Successful_completion
  | "01" -> Class.Warning
  | "02" -> Class.No_data_this_is_also_a_warning_class_per_the_sql_standard
  | "03" -> Class.Sql_statement_not_yet_complete
  | "08" -> Class.Connection_exception
  | "09" -> Class.Triggered_action_exception
  | "0A" -> Class.Feature_not_supported
  | "0B" -> Class.Invalid_transaction_initiation
  | "0F" -> Class.Locator_exception
  | "0L" -> Class.Invalid_grantor
  | "0P" -> Class.Invalid_role_specification
  | "0Z" -> Class.Diagnostics_exception
  | "10" -> Class.Xquery_error
  | "20" -> Class.Case_not_found
  | "21" -> Class.Cardinality_violation
  | "22" -> Class.Data_exception
  | "23" -> Class.Integrity_constraint_violation
  | "24" -> Class.Invalid_cursor_state
  | "25" -> Class.Invalid_transaction_state
  | "26" -> Class.Invalid_sql_statement_name
  | "27" -> Class.Triggered_data_change_violation
  | "28" -> Class.Invalid_authorization_specification
  | "2B" -> Class.Dependent_privilege_descriptors_still_exist
  | "2D" -> Class.Invalid_transaction_termination
  | "2F" -> Class.Sql_routine_exception
  | "34" -> Class.Invalid_cursor_name
  | "38" -> Class.External_routine_exception
  | "39" -> Class.External_routine_invocation_exception
  | "3B" -> Class.Savepoint_exception
  | "3D" -> Class.Invalid_catalog_name
  | "3F" -> Class.Invalid_schema_name
  | "40" -> Class.Transaction_rollback
  | "42" -> Class.Syntax_error_or_access_rule_violation
  | "44" -> Class.With_check_option_violation
  | "53" -> Class.Insufficient_resources
  | "54" -> Class.Program_limit_exceeded
  | "55" -> Class.Object_not_in_prerequisite_state
  | "57" -> Class.Operator_intervention
  | "58" -> Class.System_error_errors_external_to_postgresql_itself
  | "F0" -> Class.Configuration_file_error
  | "HV" -> Class.Foreign_data_wrapper_error_sql_med
  | "P0" -> Class.Pl_pgsql_error
  | "XX" -> Class.Internal_error
  | other -> Class.Other other

let has_class c s = String.length s >= 2 && String.sub s 0 2 = c
let is_unique_violation s = s = "23505"
let is_foreign_key_violation s = s = "23503"
let is_not_null_violation s = s = "23502"
let is_check_violation s = s = "23514"
let is_exclusion_violation s = s = "23P01"
let is_integrity_violation s = has_class "23" s
let is_serialization_failure s = s = "40001" || s = "40P01"
let is_connection_failure s = has_class "08" s
let is_retryable s = is_serialization_failure s || is_connection_failure s
let is_syntax_or_access_error s = has_class "42" s

(* every code in the table, for tooling and for testing the table itself *)
let all =
  [
    "00000";
    "01000";
    "0100C";
    "01008";
    "01003";
    "01007";
    "01006";
    "01004";
    "01P01";
    "02000";
    "02001";
    "03000";
    "08000";
    "08003";
    "08006";
    "08001";
    "08004";
    "08007";
    "08P01";
    "09000";
    "0A000";
    "0B000";
    "0F000";
    "0F001";
    "0L000";
    "0LP01";
    "0P000";
    "0Z000";
    "0Z002";
    "10608";
    "20000";
    "21000";
    "22000";
    "2202E";
    "22021";
    "22008";
    "22012";
    "22005";
    "2200B";
    "22022";
    "22015";
    "2201E";
    "22014";
    "22016";
    "2201F";
    "2201G";
    "22018";
    "22007";
    "22019";
    "2200D";
    "22025";
    "22P06";
    "22010";
    "22023";
    "22013";
    "2201B";
    "2201W";
    "2201X";
    "2202H";
    "2202G";
    "22009";
    "2200C";
    "2200G";
    "22004";
    "22002";
    "22003";
    "2200H";
    "22026";
    "22001";
    "22011";
    "22027";
    "22024";
    "2200F";
    "22P01";
    "22P02";
    "22P03";
    "22P04";
    "22P05";
    "2200L";
    "2200M";
    "2200N";
    "2200S";
    "2200T";
    "22030";
    "22031";
    "22032";
    "22033";
    "22034";
    "22035";
    "22036";
    "22037";
    "22038";
    "22039";
    "2203A";
    "2203B";
    "2203C";
    "2203D";
    "2203E";
    "2203F";
    "2203G";
    "23000";
    "23001";
    "23502";
    "23503";
    "23505";
    "23514";
    "23P01";
    "24000";
    "25000";
    "25001";
    "25002";
    "25008";
    "25003";
    "25004";
    "25005";
    "25006";
    "25007";
    "25P01";
    "25P02";
    "25P03";
    "25P04";
    "26000";
    "27000";
    "28000";
    "28P01";
    "2B000";
    "2BP01";
    "2D000";
    "2F000";
    "2F005";
    "2F002";
    "2F003";
    "2F004";
    "34000";
    "38000";
    "38001";
    "38002";
    "38003";
    "38004";
    "39000";
    "39001";
    "39004";
    "39P01";
    "39P02";
    "39P03";
    "3B000";
    "3B001";
    "3D000";
    "3F000";
    "40000";
    "40002";
    "40001";
    "40003";
    "40P01";
    "42000";
    "42601";
    "42501";
    "42846";
    "42803";
    "42P20";
    "42P19";
    "42830";
    "42602";
    "42622";
    "42939";
    "42804";
    "42P18";
    "42P21";
    "42P22";
    "42809";
    "428C9";
    "42703";
    "42883";
    "42P01";
    "42P02";
    "42704";
    "42701";
    "42P03";
    "42P04";
    "42723";
    "42P05";
    "42P06";
    "42P07";
    "42712";
    "42710";
    "42702";
    "42725";
    "42P08";
    "42P09";
    "42P10";
    "42611";
    "42P11";
    "42P12";
    "42P13";
    "42P14";
    "42P15";
    "42P16";
    "42P17";
    "44000";
    "53000";
    "53100";
    "53200";
    "53300";
    "53400";
    "54000";
    "54001";
    "54011";
    "54023";
    "55000";
    "55006";
    "55P02";
    "55P03";
    "55P04";
    "57000";
    "57014";
    "57P01";
    "57P02";
    "57P03";
    "57P04";
    "57P05";
    "58000";
    "58030";
    "58P01";
    "58P02";
    "58P03";
    "F0000";
    "F0001";
    "HV000";
    "HV005";
    "HV002";
    "HV010";
    "HV021";
    "HV024";
    "HV007";
    "HV008";
    "HV004";
    "HV006";
    "HV091";
    "HV00B";
    "HV00C";
    "HV00D";
    "HV090";
    "HV00A";
    "HV009";
    "HV014";
    "HV001";
    "HV00P";
    "HV00J";
    "HV00K";
    "HV00Q";
    "HV00R";
    "HV00L";
    "HV00M";
    "HV00N";
    "P0000";
    "P0001";
    "P0002";
    "P0003";
    "P0004";
    "XX000";
    "XX001";
    "XX002";
  ]
