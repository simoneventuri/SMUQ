SubModule(Logger_Class) Logger_SubClass

  use Logger_Tools_Module   ,only:  GetScalarFormat, GetVectorFormat, GetIntegerFormat, Convert_Variable_To_String

! #define Purity Pure
#define Purity

  implicit none

  character(*)  ,parameter                      ::  Default_Spacing2_Format   =   "3x"

  contains

Module Procedure StopFromLogger
  integer                                                               ::  k, Unit
  do k = 1,This%NUnits
    Unit    =   This%Units(k)%Unit
    write(Unit,"(a)") "*** Stopping ***"
    if ( present(Message) ) &
    write(Unit,"(a)") Message
    write(Unit,"(a)") " -> "//This%GetPath()
  end do
  error stop
End Procedure

Module Procedure Write_Blank_Line
  integer                                                               ::  k
  if ( .Not. This%On() ) return                                                                             ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit,*)
  end do
!   write(This%Unit,*)
End Procedure

Module Procedure Write_1xV0
  character(:)  ,allocatable                                            ::  S1
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  character(100)::qqq
  integer                                                               ::  Local_Status, ios, k
!   write(*,*) "[Write_1xV0] Entering in Write_1xV0"
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
!   write(*,*) "[Write_1xV0] This%Active(LogLevel) = ", This%Active(LogLevel)
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
!   write(*,*) "[Write_1xV0] This%NUnits = ", This%NUnits
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
!   write(*,*) "[Write_1xV0] This%NUnits = ", This%NUnits
  Local_Status  =       0
!   write(*,*) "[Write_1xV0] Calling Convert_Variable_To_String"
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
!   write(*,*) "[Write_1xV0] -> V1 = ", V1
!   write(*,*) "[Write_1xV0] -> S1 = ", S1
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
!   write(*,*) "[Write_1xV0] LineFormat = ", LineFormat
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status

  do k = 1,This%NUnits
!     write(*,*) "[Write_1xV0] -> k = ", k, "This%Units(k)%Active = ", This%Units(k)%Active
    if ( .Not. This%Units(k)%Active ) cycle
!     write(*,*) "[Write_1xV0]    *** writing ***"
!     write(*,*) "[Write_1xV0]    This%Units(k)%Unit = ", This%Units(k)%Unit
!     write(*,*) "[Write_1xV0]    Adv = ", Adv
!     write(*,*) "[Write_1xV0]    S1 = ", S1
!
!     write(*,*) "[Write_1xV0]    This%Units(k)%ToScreen  = ", This%Units(k)%ToScreen
!     write(*,*) "[Write_1xV0]    This%Units(k)%Active    = ", This%Units(k)%Active
!     write(*,*) "[Write_1xV0]    This%Units(k)%Index     = ", This%Units(k)%Index
!     write(*,*) "[Write_1xV0]    This%Units(k)%Unit      = ", This%Units(k)%Unit
!     write(*,*) "[Write_1xV0]    This%Units(k)%FileName  = ", This%Units(k)%FileName

!     write(*,"(a,i0,a,a,a,g0,a,a,a)") "[Write_1xV0]    |write(",This%Units(k)%Unit,",",'"'//LineFormat//'"',",Advance=",'"'//Adv//'"',",IOStat=ios) ",'"{DEBUG}'//S1//'"',"|"
!     write(6,"('',*(a,a,:,3x))",Advance="YES",IOStat=ios) "[Write_1xV0]    1 Testing 'GetSeconds' procedure"
!     write(This%Units(k)%Unit,"('',*(a,a,:,3x))",Advance="YES",IOStat=ios) "[Write_1xV0]    2 Testing 'GetSeconds' procedure"
!     write(This%Units(k)%Unit,LineFormat,Advance="YES",IOStat=ios) "[Write_1xV0]    3 Testing 'GetSeconds' procedure"
!     write(This%Units(k)%Unit,"('',*(a,a,:,3x))",Advance="YES",IOStat=ios) "[Write_1xV0]    3b Testing 'GetSeconds' procedure"
!     write(This%Units(k)%Unit,"('',*(a,a,:,3x))",Advance=Adv,IOStat=ios) "4 Testing 'GetSeconds' procedure"
!     write(This%Units(k)%Unit,"('',*(a,a,:,3x))",Advance=Adv,IOStat=ios) "5 "//S1
!     write(*,*) "[Write_1xV0] LineFormat = |"//LineFormat//"|"
!     write(*,*) "[Write_1xV0] LineFormat = |"//"('',*(a,a,:,3x))"//"|"
!
!     write(*,*) "[Write_1xV0] Next line should print "
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1
!     write( This%Units(k)%Unit, LineFormat, IOStat=ios ) S1
!     write(*,*) "[Write_1xV0] ***** ios = ", ios
!
!     qqq    =   LineFormat
!     write(*,*) "[Write_1xV0] Next line should print "
!     write( This%Units(k)%Unit, qqq, Advance=Adv, IOStat=ios ) S1
!     write(*,*) "[Write_1xV0] ***** ios = ", ios
!
!     qqq    =   "('',*(a,a,:,3x))"
!     write(*,*) "[Write_1xV0] Next line should print "
!     write( This%Units(k)%Unit, "('',*(a,a,:,3x))", Advance=Adv, IOStat=ios ) S1
!     write(*,*) "[Write_1xV0] ***** ios = ", ios

  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_2xV0
  character(:)  ,allocatable                                            ::  S1,  S2
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_4xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_5xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_6xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_7xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_8xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_9xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_10xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_11xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_12xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_13xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_14xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_15xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_16xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_17xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_18xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_19xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_20xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                                                S11, S12, S13, S14, S15, S16, S17, S18, S19, S20
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26, S27
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26, S27, S28
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26, S27, S28, S29
  end do
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
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,   &
                                                   S11, S12, S13, S14, S15, S16, S17, S18, S19, S20,   &
                                                   S21, S22, S23, S24, S25, S26, S27, S28, S29, S30
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV1
  use Utilities_Library   ,only:  GetOptArgValue
  character(:)  ,allocatable                                            ::  S1, S1b(:)
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k, i
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  if ( GetOptArgValue(.True.,Inline) ) then
    call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
    LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a))"
    call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
    call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1
    end do
  else
    call Convert_Variable_To_String( V1 , S1b, F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
    LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a))"
    call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
    call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
    do i = 1,size(V1)
      do k = 1,This%NUnits
        if ( .Not. This%Units(k)%Active ) cycle
        write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1b(i)
      end do
    end do
  end if
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios, NItemMax ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_2xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2, Fidx
  character(:)  ,allocatable  ,dimension(:)                             ::  S3
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, i, S2, S3(i)
    end do
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_5xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_7xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_9xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_1xV1_1xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_1xV1_2xV0
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_1xV1_1xV0_1xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5,  S6
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5,  S6
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_3xV0_2xV1
  character(:)  ,allocatable                                            ::  S1,  S2,  S3,  S4,  S5
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V5 , S5 , F5 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2,  S3,  S4,  S5
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV0_2xV1
  character(:)  ,allocatable                                            ::  S1, Fidx
  character(:)  ,allocatable  ,dimension(:)                             ::  S3,  S2
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
    do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, i, S2(i), S3(i)
  end do
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
  integer                                                               ::  Local_Status, ios, k
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, i, S2, S3(i), S4, S5(i)
    end do
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
  integer                                                               ::  Local_Status, ios, k
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, i, S2, S3(i), S4, S5(i), S6, S7(i)
    end do
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_4xV0_2xV0V1
  character(:)  ,allocatable                                            ::  S1, S2, S3, S4, S5, S7,     S6,  S8
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, S2, S3, S4, S5, S6, S7, S8
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_6xV0_2xV0V1
  character(:)  ,allocatable                                            ::  S1, S2, S3, S4, S5, S6, S7, S8, S9, S10
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  integer                                                               ::  Local_Status, ios, k
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
  call Convert_Variable_To_String( V9 , S9 , F9 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V10, S10, F10, Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do k = 1,This%NUnits
    if ( .Not. This%Units(k)%Active ) cycle
    write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, S2, S3, S4, S5, S6, S7, S8, S9, S10
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure

Module Procedure Write_1xV0_1xV2
  character(:)  ,allocatable                                            ::  S1
  character(:)  ,allocatable  ,dimension(:)                             ::  S2
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  character(:)  ,allocatable  ,dimension(:)                             ::  LinePrefix
  integer                                                               ::  Local_Status, ios, k, i
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios, MaxRow, MaxCol ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  allocate( Character(len(S1)) :: LinePrefix(size(S2,1)) )
  LinePrefix(:)   =   ""
  LinePrefix(1)   =   S1
  do i = 1,size(S2,1)
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit,LineFormat, Advance=Adv, IOStat=ios ) LinePrefix(i) // S2(i)
    end do
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
  integer                                                               ::  Local_Status, ios, k
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V3 , S3 , F3 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V4 , S4 , F4 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  do i = 1,size(V2)
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, S2(i), S3, S4(i)
    end do
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
  integer                                                               ::  Local_Status, ios, k
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, S2(i), S3, S4(i), S5, S6(i)
    end do
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
  integer                                                               ::  Local_Status, ios, k
  integer                                                               ::  i                               ! Index of the elements
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
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
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1, S2(i), S3, S4(i), S5, S6(i), S7, S8(i)
    end do
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
  if ( PresentAndFalse(Advanceindicator) ) then
    AdvanceString   =   "no"
    This%Advancing  =   .False.
  else
    AdvanceString   =   "yes"
    This%Advancing  =   .True.
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





Module Procedure WriteMatrix_1xV0_1xV2
  character(:)  ,allocatable                                            ::  S1
  character(:)  ,allocatable  ,dimension(:)                             ::  S2
  character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
  character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
  character(:)  ,allocatable  ,dimension(:)                             ::  LinePrefix
  character(:)  ,allocatable                                            ::  ColsInfo, RowsInfo
  character(:)  ,allocatable                                            ::  Sc
  integer                                                               ::  Local_Status, ios, k, i
  if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
  if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
  if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
  Local_Status  =       0
  call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
  LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a))"
  call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
  call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
  allocate( Character(len(S1)) :: LinePrefix(size(S2,1)) )
  LinePrefix(:)   =   ""
  LinePrefix(1)   =   S1
  if ( present(Cols) ) then
    call Convert_Variable_To_String( Cols , Sc , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
    ColsInfo  = repeat(" ",len(S1)) // Sc
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit,LineFormat, Advance=Adv, IOStat=ios ) ColsInfo
    end do
  end if
  do i = 1,size(S2,1)
    RowsInfo    =   ""
    if ( present(Rows) ) then
    if ( i <= size(Rows) ) then
      RowsInfo  =   "   " // Rows(i)
    end if
    end if
    do k = 1,This%NUnits
      if ( .Not. This%Units(k)%Active ) cycle
      write( This%Units(k)%Unit,LineFormat, Advance=Adv, IOStat=ios ) LinePrefix(i) // S2(i) // RowsInfo
    end do
  end do
  call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
  if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
  if ( present(Status) ) Status = Local_Status
End Procedure






! Module Procedure WriteToString_2xV0
!   character(:)  ,allocatable                                            ::  S1,  S2
!   character(:)  ,allocatable                                            ::  Adv                             ! Advance atatus: 'YES' (Default) or 'NO'
!   character(:)  ,allocatable                                            ::  LineFormat                      ! Format for writing an entire line
!   integer                                                               ::  Local_Status, ios, k
!   if (Present(Unused)) then; end if                                                                             ! Just for the "Unused" variable to be used (yes, its tricky) in order to avoid the "This variable has not been used." warning message in the compilation phase.
!   if ( .Not. This%On(LogLevel) ) return                                                                     ! Exiting the procedure if Logger not active for current LogLevel
!   if ( This%NUnits == 0 ) call This%AddUnit( LoggerUnit_Type() )                                                ! Adding a default LoggerUnit if no unit are currently loaded. This default LoggerUnit is connected to the screen.
!   Local_Status  =       0
!   call Convert_Variable_To_String( V1 , S1 , F1 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
!   call Convert_Variable_To_String( V2 , S2 , F2 , Fc, Fi, Fr, Fmt, ios ); if ( ios /= 0 ) Local_Status = ios
!   LineFormat    =       This%GetFormatPrefix(LogLevel,Error,Warning,Info,Debug,HeavyDebug,Prefix) // "*(a,a,:," // Default_Spacing2_Format // "))"
!   call This%Write_NewLine( NewLine )                                                                            ! Writing a new line if required
!   call SetAdvancingMode( This, Adv, Advance )                                                                       ! Setting the line advancement status
!   do k = 1,This%NUnits
!     if ( .Not. This%Units(k)%Active ) cycle
!     write( This%Units(k)%Unit, LineFormat, Advance=Adv, IOStat=ios ) S1,  S2
!   call This%Set_Backspace( Backspace )                                                                          ! Setting the backspacing if required
!   if ( ios /= 0 ) Local_Status = ios                                                                            ! Setting the local status indicator
!   if ( present(Status) ) Status = Local_Status
! End Procedure

End SubModule