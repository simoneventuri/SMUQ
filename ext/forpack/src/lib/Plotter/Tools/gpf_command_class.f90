! #include "preprocessor_variables.inc"

Module GPF_Command_Class

  implicit none

  private
  public  ::  GPF_Command_Type

  Type  ,abstract                                       ::  GPF_Command_Type
    logical                                             ::  i_Debug         =       .False.                 !< Debugging indicator
    logical                                             ::  Presence        =       .False.                 !< Presence indicator
    character(:)        ,allocatable                    ::  Command                                         !< Gnuplot command (single-line instruction)
    character(:)        ,allocatable                    ::  Keyword                                         !< Keyword associated to current command
  contains
    procedure   ,public   ::  Set_Debug       =>  Set_Command_Debug       !< Sets the debugging indicator
    procedure   ,public   ::  Get_Presence    =>  Get_Command_Presence    !< Gets presence indicator
    procedure   ,public   ::  Write           =>  Write_Command           !< Writes the command
    procedure   ,public   ::  Get_Keyword     =>  Get_Command_Keyword     !< Gets the command keyword
    procedure   ,public   ::  Set_Keyword     =>  Set_Command_Keyword     !< Sets the command keyword
    procedure   ,public   ::  Set_Command_Length
    procedure(Command_Setter)   ,deferred        ::  Set_Command                                     !< Sets the command instruction
  End Type

!   One could add a Debug optional argument to this procedure.
!   However, this will requires to change all the inheritend object to conform to the new interface of this TBP.
  Abstract Interface
    Subroutine Command_Setter( This )
      import    ::  GPF_Command_Type
      class(GPF_Command_Type)           ,intent(inout)  ::  This
    End Subroutine
  End Interface

  contains

Subroutine Set_Command_Debug( This, Debug )
  use GPF_Parameters            ,only:  i_Debug_Default
  class(GPF_Command_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Command object
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Derived-type debugging indicator
  This%i_Debug = i_Debug_Default                                                                                ! Setting debugging indicator to default value
  if ( present(Debug) ) This%i_Debug = Debug                                                                    ! If present optional input argument, then setting debugging indicator to input value
End Subroutine

Function Get_Command_Presence( This ) result(Presence)
  class(GPF_Command_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Command object
  logical                                                               ::  Presence                        !< Presence indicator
  Presence      =       This%Presence                                                                           ! Getting presence indicator
End Function

Subroutine Set_Command_Keyword( This, Keyword, i_Trim )
  class(GPF_Command_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Command object
  character(*)                                ,optional ,intent(in)     ::  Keyword                         !< Command keyword
  logical                                     ,optional ,intent(in)     ::  i_Trim                          !< Indicator that no space have to be added at the end of the keyword
  This%Keyword  =       ''                                                                                      ! Initializing to an empty string
  if ( present(Keyword) ) This%Keyword = trim(Keyword) // ' '                                                   ! If present optional input argument, then setting the command keyword
  if ( present(i_Trim)  ) then
    if (i_Trim) This%Keyword = trim(Keyword)
  end if
!   if (This%i_Debug) write(DbgUnit,"(14x,'[Set_Command_Keyword]: This%Keyword = ',a)") This%Keyword
End Subroutine

Function Get_Command_Keyword( This ) result(Keyword)
  class(GPF_Command_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Command object
  character(:)        ,allocatable                                      ::  Keyword                         !< Command keyword
  Keyword       =       This%Keyword                                                                            ! Getting the command keyword
End Function

Subroutine Set_Command_Length( This, Length )
  class(GPF_Command_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Command object
  integer                                               ,intent(in)     ::  Length                          !< Length of the command
!   character(:)        ,allocatable                                      ::  Command                         !< Temporary command
  character(Length)                                                     ::  Command                         !< Temporary command
! #ifndef GFORTRAN
  Command               =       This%Command
  if ( allocated(This%Command) ) deallocate( This%Command )
  allocate( character(Length) :: This%Command )
!   This%Command(:)       =       Command ! GFORTRAN_ @COMPILER_BUG
  This%Command       =       Command
! #endif
End Subroutine

Subroutine Write_Command( This, Unit )
  class(GPF_Command_Type)                               ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Command object
  integer                                               ,intent(in)     ::  Unit                            !< File unit number of the file where to command has to be written
  if ( len_trim(This%Command) /= 0 ) write(Unit,"(a)") This%Command
End Subroutine

End Module