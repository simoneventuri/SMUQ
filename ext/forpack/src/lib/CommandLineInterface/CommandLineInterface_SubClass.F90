SubModule(CommandLineInterface_Class) CommandLineInterface_SubClass

  use Logger_Class              ,only:  Logger

  implicit none

  contains

Module Procedure InitializeCommandLineInterface

  use Utilities_Library            ,only:  PresentAndTrue
  use CommandLineInterface_Module ,only:  Get_Command_Line

  character(*)                                              ,parameter  ::  ProcName='InitializeCommandLineInterface'
  logical                                                               ::  Dbg
  integer                                                               ::  i, j                            ! Index of arguments passed
  integer                                                               ::  Length, LengthMax, NArg         ! Length of a given argument
  character(:)  ,allocatable                                            ::  Argument                        ! Value of a given argument
  character(:)  ,allocatable                                            ::  List(:)

  Dbg   =   PresentAndTrue(Debug)
  if (Dbg) call Logger%Entering( ProcName )

  call Get_Command_Line( This%Command_Line )
!   if (Dbg) call Logger%Write( "-> This%Command_Line = ", This%Command_Line )

!! ****************************************************************************************
!!   !@COMPILER_BUG:gcc-7.2.0 The returned variable has the correct size but each element is empty
!!   ! If 'This%Arguments' is used instead of 'List', during its assignement, the last eleemnt assign is set to all element ===> @COMPILER_BUG:gcc-7.2.0
!! ****************************************************************************************
!!   call Get_Command_Args( This%Arguments )
!! ****************************************************************************************
  This%NArg   =   Command_Argument_Count()
  LengthMax   =   0
  do i = 1,This%NArg
    call Get_Command_Argument( i, Length=Length )
    LengthMax =   max(LengthMax,Length)
  end do
!   if (Dbg) call Logger%Write( "LengthMax = ", LengthMax )
  if ( allocated(List) ) deallocate(List)
  allocate( character(LengthMax) :: List(This%NArg) )
  allocate( character(LengthMax) :: Argument )
  List(:) = repeat(" ",LengthMax)
  do i = 1,This%NArg
    call Get_Command_Argument( i, Value=Argument )
!     if (Dbg) call Logger%Write( "Argument = ", Argument )
    List(i)   =   Argument
  end do
!! ****************************************************************************************

  if (Dbg) call Logger%Write( "Constructing each individual Arg object" )
  if (Dbg) call Logger%Write( "-> This%NArg = ", This%NArg )
  allocate( This%Arg(This%NArg) )
  do i = 1,This%NArg
    if (Dbg) call Logger%Write( "-> i = ", i, "List(i) = ", trim(List(i)) )
    call This%Arg(i)%Initialize( List(i) )
    if (Dbg) call Logger%Write( "  -> This%Arg(i)%Raw   = ", This%Arg(i)%Raw )
    if (Dbg) call Logger%Write( "  -> This%Arg(i)%Name  = ", This%Arg(i)%Name )
    if (Dbg) call Logger%Write( "  -> This%Arg(i)%Value = ", This%Arg(i)%Value )
  end do
  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure GetArgumentsList
  integer                                                               ::  i
  integer                                                               ::  Length
  Length    =   0
  do i = 1,This%NArg
    Length  =   max( Length , len(This%Arg(i)%Raw) )
  end do
  allocate( character(Length) :: Arguments(This%NArg) )
  do i = 1,This%NArg
    Arguments(i)    =   This%Arg(i)%Raw
  end do
End Procedure

Module Procedure GetArgumentsItem
  Argument  =   ""
  if ( .Not. This%ArgumentExists(iArg) ) return
  allocate( Argument , source = This%Arg(iArg)%Raw )
End Procedure

Module Procedure GetArgumentNameValue
  Name    =   ""
  Value   =   ""
  if ( .Not. This%ArgumentExists(iArg) ) return
  Name    =   This%Arg(iArg)%GetName()
  Value   =   This%Arg(iArg)%GetValue()
End Procedure

Module Procedure GetValueFromName
  use Utilities_Library   ,only:  GetOptArgValue, SetOptArg
  use String_Library      ,only:  Equal
  integer                                                               ::  i
  logical                                                               ::  Found_
  Value           =   GetOptArgValue( "", DefaultValue )
  Found_          =   .False.
  do i = 1,This%NArg
    if ( Equal( This%Arg(i)%GetName(), Name, Trimed=.True., CaseSensitive=CaseSensitive ) ) then
!     if ( This%Arg(i)%GetName() /= Name ) cycle
      Value   =   This%Arg(i)%GetValue()
      Found_  =   .True.
      exit
    end if
  end do
  call SetOptArg( Found_, Found )
End Procedure


Module Procedure ArgumentExistsFromIndex
  Exist   =   ( iArg >= 1 ) .and. ( iArg <= This%NArg )
End Procedure

Module Procedure ArgumentExistsFromName
  integer                                                               ::  i
  do i = 1,This%NArg
    if ( This%Arg(i)%GetName() /= Name ) cycle
    Exist =   .True.
    return
  end do
  Exist   =   .False.
End Procedure



Module Procedure WriteCommandLineInterfaceToUnit
  character(:)  ,allocatable  ,dimension(:)                             ::  Lines
  integer                                                               ::  i
  call This%Write( Lines )
  do i = 1,size(Lines)
    write(Unit,"(a)") trim( Lines(i) )
  end do
End Procedure


Module Procedure WriteCommandLineInterfaceToString

  use String_Library      ,only:  Convert_To_String, Add_Line_To_String, SetLength

  character(:)  ,allocatable                                            ::  Line, Name, Value
  integer                                                               ::  i, NameLength
  allocate( Character(0) :: Lines(0) )

  NameLength    =   GetMaxNameLength(This)

  Line          =   "This%Command_Line = " // This%Command_Line
  call Add_Line_To_String(Lines,Line)

  Line          =   "This%NArg         = " // Convert_To_String(This%NArg)
  call Add_Line_To_String(Lines,Line)

  do i = 1,This%NArg
    Name    =   SetLength( This%Arg(i)%GetName() , NameLength )
    Value   =   This%Arg(i)%GetValue()
    Line    =   "-> i = "     // Convert_To_String(i,Fmt="i3")  // &
                "   Name = "  // Name                           // &
                "   Value = " // Value
    call Add_Line_To_String(Lines,Line)
  end do

End Procedure

Function GetMaxNameLength( This ) result(Length)
  class(CommandLineInterface_Type)                      ,intent(in)     ::  This                            !< Passed-object dummy argument
  integer                                                               ::  Length
  integer                                                               ::  i
  Length    =   0
  do i = 1,This%NArg
    Length  =   Max( Length , len_trim(This%Arg(i)%GetName()) )
  end do
End Function

End SubModule
