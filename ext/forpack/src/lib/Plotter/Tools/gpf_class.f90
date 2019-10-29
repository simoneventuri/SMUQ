Module GPF_Class

  use GPF_Default_Class         ,only:  GPF_Default_Type
  use GPF_Parameters            ,only:  DbgUnit, i_Debug_Default

  implicit none

  private
  public  ::  GPF

  Type  ::  GPF_Type
    logical                             ::  i_Supported
    type(GPF_Default_Type)              ::  Default
  contains
    procedure   ,public   ::  Initialize      =>  Initialize_GPF
    procedure   ,private ,nopass        ::  Set_Log_File    =>  Set_GPF_Log_File
    procedure   ,public  ,nopass        ::  Is_Supported    =>  Is_GPF_Supported
    procedure   ,public   ::  Get_Default_File_Output_Directory
    procedure   ,public   ::  Get_Default_i_Graph_Grid
    procedure   ,public   ::  Set_Default_LineStyle_Width
    procedure   ,public   ::  Get_Default_LineStyle_Width
    procedure   ,public   ::  Get_Default_View_Map
    procedure   ,public   ::  Get_Default_Size_Isometric
  End Type

!   type(GPF_Type)        ,protected      ::  GPF
  type(GPF_Type)                        ::  GPF

  contains

Subroutine Initialize_GPF( This, Unit_LogFile, Name_LogFile, File_Output_Directory, i_Graph_Grid, LineStyle_Width, i_Debug )

  implicit none

  class(GPF_Type)                                       ,intent(out)    ::  This                            !< Passed-object dummy argument
  integer                                     ,optional ,intent(in)     ::  Unit_LogFile
  character(*)                                ,optional ,intent(in)     ::  Name_LogFile
  character(*)                                ,optional ,intent(in)     ::  File_Output_Directory           !< Name of the output directory
  logical                                     ,optional ,intent(in)     ::  i_Graph_Grid                    !< Graph grid indicator
  character(*)                                ,optional ,intent(in)     ::  LineStyle_Width
  logical                                     ,optional ,intent(in)     ::  i_Debug                         !< Debugging indicator

  logical                                                               ::  i_Debug_Loc                     ! Local debugging indicator

  call This%Set_Log_File( Unit_LogFile, Name_LogFile )                                                          ! Setting the unit number associated to the log file

  i_Debug_Loc   =       i_Debug_Default                                                                         ! Initializing local debugging indicator to the default value
  if ( present(i_Debug) ) i_Debug_Loc = i_Debug                                                                 ! Setting local debugging indicator to the optional input value
  if (i_Debug_Loc) write(DbgUnit,"(6x,'[Initialize_GPF]: Entering')")

  if (i_Debug_Loc) write(DbgUnit,"(6x,'[Initialize_GPF]: Calling This%Default%Reset_Parameters')")
  call This%Default%Reset_Parameters( i_Debug=i_Debug_Loc )                                                     ! Resting all configurable parameters to default value

  if (i_Debug_Loc) write(DbgUnit,"(6x,'[Initialize_GPF]: Calling This%Default%Set_Parameters')")
  call This%Default%Set_Parameters(     File_Output_Directory   =       File_Output_Directory,  &
                                        i_Graph_Grid            =       i_Graph_Grid,           &
                                        LineStyle_Width         =       LineStyle_Width,        &
                                        i_Debug                 =       i_Debug_Loc             )       !

  This%i_Supported      =       This%Is_Supported()

  if (i_Debug_Loc) then
    write(DbgUnit,"(6x,'[Initialize_GPF]: This%Default%File_Output_Directory = ',a)")    This%Default%File_Output_Directory
    write(DbgUnit,"(6x,'[Initialize_GPF]: This%Default%i_Graph_Grid          = ',l3)")   This%Default%i_Graph_Grid
    write(DbgUnit,"(6x,'[Initialize_GPF]: This%Default%LineStyle_Width       = ',a)")    This%Default%LineStyle_Width
    write(DbgUnit,"(6x,'[Initialize_GPF]: This%i_Supported = ',l3)") This%i_Supported
    write(DbgUnit,"(6x,'[Initialize_GPF]: Entering')")
  end if

End Subroutine

Subroutine Set_GPF_Log_File( Unit, Name )
  use GPF_Parameters            ,only:  DbgUnit, DbgFile
  use iso_fortran_env           ,only:  Output_Unit
  implicit none
  integer                                     ,optional ,intent(in)     ::  Unit
  character(*)                                ,optional ,intent(in)     ::  Name
  logical                                                               ::  i_Exist
  logical                                                               ::  i_Opened
  integer                                                               ::  ios
  if ( present(Unit) ) DbgUnit  =       Unit
  if ( present(Name) ) DbgFile  =       trim(adjustl(Name))
  if ( DbgUnit /= Output_Unit) then
    inquire( Unit=DbgUnit, Exist=i_Exist, Opened=i_Opened )
    if ( .not.i_Opened ) then
      if ( allocated(DbgFile) ) then
        open( Unit=DbgUnit, file=DbgFile, action='WRITE', status='UNKNOWN', position='APPEND', iostat=ios )  ! Opening the debug file
      else
        open( Unit=DbgUnit, action='WRITE', status='UNKNOWN', position='APPEND', iostat=ios )  ! Opening the debug file
      end if
    end if
  end if
End Subroutine

Function Is_GPF_Supported()
!   use ifport
  implicit none
  logical                                                               ::  Is_GPF_Supported                !<< Indicator whether of not GPF is supported
  integer                                                               ::  Status                          ! Status indicator
  character(:)  ,allocatable                                            ::  Command                         ! Command line instruction
  Command               =       "gnuplot --version > /dev/null 2>&1"                                                             ! Setting the command line
#ifdef INTEL_WORKAROUND_EXECUTE_COMMAND_LINE
  Block
    integer                                                             ::  System
    Status    =   System( Command )
  End Block
#else
  call Execute_Command_Line(Command,CmdStat=Status)
#endif
  Is_GPF_Supported      =       Status == 0
End Function

Function Get_Default_File_Output_Directory( This ) result(Variable)
  implicit none
  class(GPF_Type)                                       ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the GPF object
  character(:)  ,allocatable                                            ::  Variable                        !< Output variable
  Variable      =       This%Default%File_Output_Directory                                                      ! Getting current variable
End Function

Function Get_Default_i_Graph_Grid( This ) result(Variable)
  implicit none
  class(GPF_Type)                                       ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the GPF object
  logical                                                               ::  Variable                        !< Output variable
  Variable      =       This%Default%i_Graph_Grid                                                               ! Getting current variable
End Function

Subroutine Set_Default_LineStyle_Width( This, Variable )
  implicit none
  class(GPF_Type)                                       ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the GPF object
  character(*)                                          ,intent(in)     ::  Variable                        !< Input variable
  call This%Default%Set_Parameters( LineStyle_Width = Variable )                                                ! Setting current variable
End Subroutine

Function Get_Default_LineStyle_Width( This ) result(Variable)
  implicit none
  class(GPF_Type)                                       ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the GPF object
  character(:)  ,allocatable                                            ::  Variable                        !< Output variable
  Variable      =       This%Default%LineStyle_Width                                                            ! Getting current variable
End Function

Function Get_Default_View_Map( This ) result(Variable)
  implicit none
  class(GPF_Type)                                       ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the GPF object
  character(:)  ,allocatable                                            ::  Variable                        !< Output variable
  Variable      =       This%Default%View_Map                                                                   ! Getting current variable
End Function

Function Get_Default_Size_Isometric( This ) result(Variable)
  implicit none
  class(GPF_Type)                                       ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the GPF object
  character(:)  ,allocatable                                            ::  Variable                        !< Output variable
  Variable      =       This%Default%Size_Isometric                                                             ! Getting current variable
End Function

End Module