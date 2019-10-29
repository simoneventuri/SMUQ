! This include file 'Logger_WriteProcedures_Inline.F90' is called from the file '...'.
! Before calling the procedure, the following marco should be set:
! * _ProcedurePrefix_   Prefix of the procedure name. This variable can take the following values:
!                         * 'WriteToFile'     For procedures writing to a file
!                         * 'WriteToString'   For procedures writing into a string
! * _OutputLocation_    Output location where to write. This variable can take the following values:
!                         * 'This%Unit'       File unit number to be used for 'WriteToFile'
!                         * 'Type of the varibale variable to be added. Possible values:  'logical'
!                       'integer(INT8)', 'integer(INT16)', 'integer(INT32)', 'integer(INT64)'
!                       'real(REAL32)', 'real(REAL64)', 'real(REAL128)'
! * _DefFormat        Default format

Module Procedure StopFromLogger
  write(This%Unit,"(a)") "*** Stopping ***"
  if ( present(Message) ) &
  write(This%Unit,"(a)") Message
  write(This%Unit,"(a)") " -> "//This%GetPath()
  error stop
End Procedure

Module Procedure Write_Blank_Line
  write(This%Unit,*)
End Procedure

Module Procedure Write_1xV0
  character(:)  ,allocatable                                            ::  S1
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_2xV0
  character(:)  ,allocatable                                            ::  S1,  S2
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_4xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_5xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_6xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_7xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_8xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_9xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_10xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_11xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_12xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_13xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_14xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_15xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_16xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_17xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_18xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_19xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_20xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_21xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_22xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21, S22
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V22, S22, F22, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_23xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21, S22, S23
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V22, S22, F22, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V23, S23, F23, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_24xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21, S22, S23, S24
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V22, S22, F22, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V23, S23, F23, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V24, S24, F24, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_25xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21, S22, S23, S24, S25
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V22, S22, F22, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V23, S23, F23, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V24, S24, F24, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V25, S25, F25, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_26xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21, S22, S23, S24, S25, S26
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V22, S22, F22, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V23, S23, F23, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V24, S24, F24, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V25, S25, F25, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V26, S26, F26, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_27xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21, S22, S23, S24, S25, S26, S27
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V22, S22, F22, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V23, S23, F23, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V24, S24, F24, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V25, S25, F25, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V26, S26, F26, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V27, S27, F27, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26, S27
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_28xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21, S22, S23, S24, S25, S26, S27, S28
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V22, S22, F22, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V23, S23, F23, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V24, S24, F24, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V25, S25, F25, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V26, S26, F26, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V27, S27, F27, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V28, S28, F28, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26, S27, S28
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_29xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21, S22, S23, S24, S25, S26, S27, S28, S29  ! Character strings corresponding to the input variable
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V22, S22, F22, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V23, S23, F23, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V24, S24, F24, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V25, S25, F25, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V26, S26, F26, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V27, S27, F27, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V28, S28, F28, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V29, S29, F29, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26, S27, S28, S29
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_30xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                                                S21, S22, S23, S24, S25, S26, S27, S28, S29, S30  ! Character strings corresponding to the input variable
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V11, S11, F11, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V12, S12, F12, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V13, S13, F13, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V14, S14, F14, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V15, S15, F15, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V16, S16, F16, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V17, S17, F17, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V18, S18, F18, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V19, S19, F19, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V20, S20, F20, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V21, S21, F21, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V22, S22, F22, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V23, S23, F23, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V24, S24, F24, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V25, S25, F25, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V26, S26, F26, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V27, S27, F27, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V28, S28, F28, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V29, S29, F29, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V30, S30, F30, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26, S27, S28, S29, S30
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV1
  character(:)  ,allocatable                                            ::  S1
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_2xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2, Fidx
  character(:)  ,allocatable  ,dimension(:)                             ::  S3
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call GetIntegerFormat( size(V3), F0, Fidx, Status=ios );      if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
!                                             S1   i                                      S2  S3
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "a,"//Fidx//"," // Default_Spacing2_Format // ",a,a))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do i = 1,size(V3)
    write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, i, S2, S3(i)
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_5xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_7xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_9xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_1xV1_1xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_1xV1_2xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_1xV1_1xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_2xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV0_2xV1
  character(:)  ,allocatable                                            ::  S1, Fidx
  character(:)  ,allocatable  ,dimension(:)                             ::  S3,  S2
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call GetIntegerFormat( size(V3), F0, Fidx, Status=ios );      if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
!                                             S1   i                                      S2  S3
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "a,"//Fidx//"," // Default_Spacing2_Format // ",a,a))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do i = 1,size(V3)
    write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, i, S2(i), S3(i)
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV0_2xV0V1
  character(:)  ,allocatable                                            ::  S1, S2, S4, Fidx
  character(:)  ,allocatable  ,dimension(:)                             ::  S3,  S5
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call GetIntegerFormat( size(V3), F0, Fidx, Status=ios );      if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "a,"//Fidx//",*(" // Default_Spacing2_Format // ",a,a)))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do i = 1,size(V3)
    write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, i, S2, S3(i), S4, S5(i)
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV0_3xV0V1
  character(:)  ,allocatable                                            ::  S1, S2, S4, S6, Fidx
  character(:)  ,allocatable  ,dimension(:)                             ::  S3,  S5,  S7
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call GetIntegerFormat( size(V3), F0, Fidx, Status=ios );      if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "a,"//Fidx//",*(" // Default_Spacing2_Format // ",a,a)))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do i = 1,size(V3)
    write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, i, S2, S3(i), S4, S5(i), S6, S7(i)
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_4xV0_2xV0V1
  character(:)  ,allocatable                                            ::  S1, S2, S3, S4, S5, S7,     S6,  S8
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
!   call GetIntegerFormat( size(V3), F0, Fidx, Status=ios );      if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, S2, S3, S4, S5, S6, S7, S8
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV0_1xV2
  character(:)  ,allocatable                                            ::  S1
  character(:)  ,allocatable  ,dimension(:)                             ::  S2
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  character(:)  ,allocatable                                            ::  FmtStr                    ! Local prefix variable
  character(:)  ,allocatable  ,dimension(:)                             ::  LinePrefix
  integer                                                               ::  Local_Status, ios, i
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  allocate( Character(len(S1)) :: LinePrefix(size(S2,1)) )
  LinePrefix(:)   =   ""
  LinePrefix(1)   =   S1
  do i = 1,size(S2,1)
    write( This%Unit,LineFormat, Advance=Adv, IOStat=ios ) LinePrefix(i) // S2(i)
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_2xV0V1
  character(:)  ,allocatable                                            ::  S1, S3
  character(:)  ,allocatable  ,dimension(:)                             ::  S2, S4
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do i = 1,size(V2)
    write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, S2(i), S3, S4(i)
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0V1
  character(:)  ,allocatable                                            ::  S1, S3, S5
  character(:)  ,allocatable  ,dimension(:)                             ::  S2, S4, S6
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do i = 1,size(V2)
    write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, S2(i), S3, S4(i), S5, S6(i)
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_4xV0V1
  character(:)  ,allocatable                                            ::  S1, S3, S5, S7
  character(:)  ,allocatable  ,dimension(:)                             ::  S2, S4, S6, S8
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V6 , S6 , F6 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V7 , S7 , F7 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V8 , S8 , F8 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios

  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do i = 1,size(V2)
    write( This%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, S2(i), S3, S4(i), S5, S6(i), S7, S8(i)
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!                                   Private procedures
! **************************************************************************************************************
! **************************************************************************************************************

Pure Subroutine SetAdvancingMode( This, AdvanceString, Advanceindicator )
  class(Logger_Type)                                    ,intent(inout)  ::  This                                !< Passed-object dummy argument
  character(:)  ,allocatable                            ,intent(out)    ::  AdvanceString
  logical                                     ,optional ,intent(in)     ::  Advanceindicator                             !< Indicator whether or not the line should be advanced
  AdvanceString       =     "YES"
  This%Advancing      =     .True.
  if ( PresentAndFalse(Advanceindicator) ) then
!   if ( present(Advanceindicator) ) then
!     if (.Not.Advanceindicator) then
      AdvanceString   =     "NO"
      This%Advancing  =     .False.
!     end if
!   end if
  end if
End Subroutine

Pure Function PresentAndFalse( OptionalArgument ) result(Indicator)
  logical                                     ,optional ,intent(in)     ::  OptionalArgument
  logical                                                               ::  Indicator
  if ( Present(OptionalArgument) ) then
    Indicator =   .Not. OptionalArgument
  else
    Indicator =   .False.
  end if
End Function
