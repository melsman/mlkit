
signature ERROR_CODE =     (* Support for error testing. *)
  sig
    type ErrorCode and ErrorInfo
    val from_ErrorInfo : ErrorInfo -> ErrorCode
    val error_code_parse : ErrorCode
    val error_code_eof : ErrorCode
    val eq : ErrorCode * ErrorCode -> bool
    val pr : ErrorCode -> string
  end
