SubModule(CommandLineArgument_Class) CommandLineArgument_SubClass

  use Logger_Class              ,only:  Logger, LogLevel_HEAVYDEBUG

  implicit none

  contains

Module Procedure InitializeCommandLineArgument
  use String_Library              ,only:  Parse, RemoveQuotes
  character(:)  ,allocatable                                            ::  Strings(:)
  character(*)          ,parameter      ::  DefaultIgnoreBetween(2) = ['"',"'"] ! ['"']
!   character(*)          ,parameter      ::  DefaultIgnoreBetween(1) = ['"'] !      ['"',"'"] ! ['"']
  This%Raw      =   String
  This%Name     =   ""
  This%Value    =   ""
  call Parse( String, '=', Strings, IgnoreBetween=DefaultIgnoreBetween )

!   call Parse( Arg, ArgSep_, Argument )
!  ['"',"'"] ! ['"']
  This%Name     =   trim( Strings(1) )
  if ( size(Strings) >= 2 ) This%Value = trim( Strings(2) )

  if ( len(This%Name) >= 3 ) then
    if ( This%Name(1:2) == "--" ) This%Name = This%Name(3:)
  end if

  if ( len(This%Name) >= 2 ) then
    if ( This%Name(1:1) == "-" ) This%Name = This%Name(2:)
  end if

  This%Value   =   RemoveQuotes(This%Value)


End Procedure

Module Procedure GetCommandLineArgumentName
  integer                                                               ::  i
  Name    =   This%Name
  if ( len_trim(This%Name) == 0 ) then
    return
  else
    do i = 1,len_trim(This%Name)
      if ( i > len(Name) ) exit
      if ( Name(i:i) /= "-") exit
      Name  =   Name(i+1:len(Name))
    end do
  end if
End Procedure

Module Procedure GetCommandLineArgumentValue
  Value   =   This%Value
End Procedure

End SubModule
