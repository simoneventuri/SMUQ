Module GPF_Class

  use GPF_Default_Class         ,only:  GPF_Default_Type

  implicit none

  public

  Type  ::  GPF_Type
    logical                     ::  i_Supported
    type(GPF_Default_Type)      ::  Default
  contains
    procedure   ,public   ::  Initialize      =>  Initialize_GPF
    procedure   ,public   ::  Set_Log_File    =>  Set_GPF_Log_File
    procedure   ,public   ::  Is_Supported    =>  Is_GPF_Supported
  End Type

  contains

Subroutine Initialize_GPF( This, Unit_LogFile, Name_LogFile, File_Output_Directory )

  implicit none

  class(GPF_Type)                                       ,intent(out)    ::  This                            !< Passed-object dummy argument
  integer                                     ,optional ,intent(in)     ::  Unit_LogFile
  character(*)                                ,optional ,intent(in)     ::  Name_LogFile
  character(*)                                ,optional ,intent(in)     ::  File_Output_Directory

  call This%Set_Log_File( Unit_LogFile, Name_LogFile )

  call This%Default%Set_File_Output_Directory( File_Output_Directory )

  This%i_Supported      =       This%Is_Supported()

End Subroutine

Subroutine Set_GPF_Log_File( This, Unit, Name )
  use GPF_Parameters            ,only:  DbgUnit, DbgFile
  use iso_fortran_env           ,only:  Output_Unit
  implicit none
  class(GPF_Type)                                       ,intent(in)     ::  This                            !< Passed-object dummy argument
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

Function Is_GPF_Supported( This )
  use ifport
  implicit none
  class(GPF_Type)                                       ,intent(in)     ::  This                            !< Passed-object dummy argument
  logical                                                               ::  Is_GPF_Supported                !<< Indicator whether of not GPF is supported
  integer                                                               ::  Status                          ! Status indicator
  character(:)  ,allocatable                                            ::  Command                         ! Command line instruction
  Command               =       "gnuplot --version"                                                             ! Setting the command line
#ifdef INTEL_WORKAROUND_EXECUTE_COMMAND_LINE
  Status                =       System( Command )
#else
  call Execute_Command_Line(Command,CmdStat=Status)
#endif
  Is_GPF_Supported      =       Status == 0
End Function

End Module