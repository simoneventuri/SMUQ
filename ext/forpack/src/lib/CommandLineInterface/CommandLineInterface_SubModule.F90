SubModule(CommandLineInterface_Module) CommandLineInterface_SubModule

  use Logger_Class              ,only:  Logger

  implicit none

  contains

! This procedure returns a character string which contains the full command
! line by which the program was invoked.
Module Procedure Get_Command_Line
  integer                                                               ::  Length                              ! Length of a given argument
  integer                                                               ::  Status                              ! Status variables of the Get_Command_Argument procedure
  call Get_Command( Length=Length, Status=Status )                                                              ! Getting the length of the command-line string
  allocate( character(Length) :: Command_Line )                                                                 ! Allocating the command-line string to the correct length
  call Get_Command( Command=Command_Line, Status=Status )                                                       ! Getting the value of the command-line string
End Procedure

! This procedure returns a vector of character strings whose elements
! corresponds the command-line arguments.
Module Procedure Get_Command_Args
  integer                                                               ::  i                                   ! Index of arguments passed
  integer                                                               ::  Length, LengthMax, NArg             ! Length of a given argument
  character(:)  ,allocatable                                            ::  Argument                            ! Value of a given argument
  NArg        =   Command_Argument_Count()
  LengthMax   =   0
  do i = 1,NArg
    call Get_Command_Argument( i, Length=Length )
    LengthMax =   max(LengthMax,Length)
  end do
  if ( allocated(Arguments) ) deallocate(Arguments)
  allocate( character(LengthMax) :: Arguments(NArg) )
  allocate( character(LengthMax) :: Argument )
  do i = 1,NArg
    call Get_Command_Argument( i, Value=Argument )
    Arguments(i)   =   Argument
  end do
End Procedure

Module Procedure GetCommandLineArgumentValue

  use Error_Class               ,only:  Error
  use String_Library            ,only:  Split, Convert_Ratio
  use Utilities_Library         ,only:  PresentAndTrue

  character(*)                                              ,parameter  ::  ProcName='GetCommandLineArgumentValue'
  logical                                                               ::  Dbg
  logical                                                               ::  Found_
  integer                                                               ::  i, j, N
  character(:)  ,allocatable                                            ::  Argument, Name, ArgName, ArgValue
  character(:)  ,allocatable                                            ::  Arguments(:)                    !< Vector of character string corresponding the command-line arguments

  Dbg   =   PresentAndTrue(Debug)
  if (Dbg) call Logger%Entering( ProcName )

  Value   =   ""
  Found_  =   .False.

  if (Dbg) call Logger%Write( "Calling Get_Command_Args" )
  call Get_Command_Args( Arguments )
  if (Dbg) call Logger%Write( "-> Arguments = ", Arguments )

  N  =  size(Arguments)

  SearchLoop: do i = 1,N
    Argument  =   trim(Arguments(i))
    if (Dbg) call Logger%Write( "-> Processing argument "//Convert_Ratio(i,N)//": ", Argument )
    do j = 1,size(Names)
      Name    =   trim(Names(j))

      call Split( Argument, ArgName, "=", ArgValue )

      if ( ArgName /= Name ) cycle
      if (Dbg) call Logger%Write( "-> Argument '"//Name//"' found as argument "// Convert_Ratio(i,N) )

      if ( len_trim(ArgValue) == 0 ) then
        if ( size(Arguments) < i+1 ) call Error%Raise( "Argument '"//Name//"' expects a value", ProcName = ProcName )
        Value   =   Arguments(i+1)
      else
        Value   =   ArgValue
      end if
      Found_    =   .True.

      exit SearchLoop

    end do

  end do SearchLoop

  if ( present(Found) ) Found = Found_

  if (Dbg) call Logger%Write( "-> Value = ",Value )
  if (Dbg) call Logger%Exiting()

End Procedure

End SubModule
