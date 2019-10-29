SubModule(AbstractTime_Class) AbstractTime_SubClass

  use Logger_Class                ,only:  Logger

  implicit none

  contains

Pure Module Subroutine  InitializeTime( This, Name )
  class(AbstractTime_Type)                              ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Time object
  character(*)                                ,optional ,intent(in)     ::  Name                            !< Timer name
  This%Name     =       ""                                                                                      ! Initializing the time name to an empty string
  if ( present(Name)  ) then
    if ( allocated(This%Name) ) deallocate(This%Name)
    allocate( This%Name , source = Name )                                                                      ! Setting the time name if present
  end if
  call This%ResetTime()                                                                                        ! Resetting the time values
End Subroutine

Module Subroutine Output_Time( This, Unit )
  class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
  integer                                               ,intent(in)     ::  Unit                            !< File unit where the time data need to be outputed
  character(:)  ,allocatable                                            ::  String                          ! String character
  String        =       This%Name // ": "
  write(Unit,"(a,g0)") String, This%ElapsedTime
End Subroutine

Module Procedure GetName
  use String_Library      ,only:  Convert_To_String
  if ( present(Length) ) then
    Name    =     Convert_To_String( This%Name, Len=Length )
  else
    Name    =     This%Name
  end if
End Procedure

Module Procedure GetElapsedTime
  ElapsedTime       =       This%ElapsedTime
End Procedure

Module Procedure SetElapsedTime
  This%ElapsedTime  =       ElapsedTime
End Procedure

Module Function GetTimeInDHMS( This ) result(Str)
  use TimerUtilities_Library       ,only:  Convert_Secondes_To_HMD
  class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
  character(:)  ,allocatable                                            ::  Str
  Str   =   Convert_Secondes_To_HMD( This%ElapsedTime )
End Function

Module Function GetPercentageString( This, Total, Length ) result(Str)
  use String_Library      ,only:  Convert_To_String
  class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
  real(8)                                               ,intent(in)     ::  Total                           !< Total time
  integer                                     ,optional ,intent(in)     ::  Length
  character(:)  ,allocatable                                            ::  Str
  character(*)                                              ,parameter  ::  Fmt_Def = "(f6.2)"
  real(8)                                                               ::  X
  real(8)                                                               ::  Tolerance = 1.0E-10_8
  X     =       0.0_8
  if ( abs(Total) > Tolerance ) X   =       100 * This%ElapsedTime / Total
  Str   =       Convert_To_String( X, Fmt=Fmt_Def ) // "%"
  if ( present(Length) ) Str = Convert_To_String( Str, Len=Length )
End Function

Module Function GetTimeInSeconds( This, Fmt ) result(Str)
  use String_Library      ,only:  Convert_To_String
  class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
  character(*)                                ,optional ,intent(in)     ::  Fmt
  character(:)  ,allocatable                                            ::  Str
  character(*)                                              ,parameter  ::  Fmt_Def = "(f15.3)"
  character(:)  ,allocatable                                            ::  Fmt_Loc
  Fmt_Loc       =       Fmt_Def
  if ( present(Fmt) ) Fmt_Loc = Fmt
  Str           =       Convert_To_String( This%ElapsedTime, Fmt=Fmt_Loc ) // "s"
End Function

Pure Module Function GetFormat( This ) result(Fmt)
  use Time_Parameters     ,only:  SecondsPerMinute, SecondsPerHour, SecondsPerDay
  class(AbstractTime_Type)                              ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to a Time object
  character(:)  ,allocatable                                            ::  Fmt
  if      ( This%ElapsedTime < SecondsPerMinute ) then                                                                                ! If the time is smaller than 1 minute
    Fmt         =       "(f6.3)"
  else if ( This%ElapsedTime < SecondsPerHour ) then                                                                                ! If the time is smaller than 1 hour
    Fmt         =       "(f8.3)"
  else if ( This%ElapsedTime < SecondsPerDay ) then                                                                                ! If the time is smaller than 1 day
    Fmt         =       "(f10.3)"
  else                                                                                                          ! If the time is greater than 1 day
    Fmt         =       "(g0)"
  end if
End Function

End SubModule