program SMUQ

use Input_Library
use String_Library
use Test_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use ProgramDefs_Class                                             ,only:    ProgramDefs
use Root_Class                                                    ,only:    Root_Type

implicit none

type(InputReader_Type)                                                ::    Input
type(InputSection_Type), pointer                                      ::    InputSection=>null()
type(InputSection_Type)                                               ::    CMDArgsSection
type(Root_Type)                                                       ::    Root
character(:), allocatable                                             ::    ParameterName
character(:), allocatable                                             ::    SectionName
character(:), allocatable                                             ::    FileName
character(:), allocatable                                             ::    RunDirectory
logical                                                               ::    Found
logical                                                               ::    Debug_Loc = .false.
character(:), allocatable                                             ::    SectionChain
character(:), allocatable                                             ::    SMUQTask
integer                                                               ::    StatLoc=0
character(:), allocatable                                             ::    VarC0D
logical                                                               ::    VarL0D


StatLoc = GetCWD( RunDirectory )

call Logger%Initialize( Status='REPLACE', Position='REWIND', Procedure='SMUQ', Indentation=2 )

call CMDArgsSection%SetName( 'cmdargs' )
call CMDArgsSection%AddCommandLineArguments()

call ProgramDefs%Construct( Input=CMDArgsSection, Prefix=RunDirectory )

call Logger%Initialize( FileName=ProgramDefs%GetLogFilePath(), Status='REPLACE', Position='REWIND', Procedure='SMUQ'              &
                                                                                                                 , Indentation=2 )

call MakeDirectory( Path=ProgramDefs%GetOutputDir(), Options='-p' )
call execute_command_line( Command='rm -rf ' // ProgramDefs%GetOutputDir() // '/*' )
call MakeDirectory( Path=ProgramDefs%GetLogDir(), Options='-p' )
call execute_command_line( Command='rm -rf ' // ProgramDefs%GetLogDir() // '/*' )
call MakeDirectory( Path=ProgramDefs%GetRestartDir(), Options='-p' )
call execute_command_line( Command='rm -rf ' // ProgramDefs%GetRestartDir() // '/*' )
call MakeDirectory( Path=ProgramDefs%GetCaseDir(), Options='-p' )

VarC0D = ProgramDefs%GetSuppliedCaseDir()
if ( len_trim(VarC0D) /= 0 ) then
  FileName = VarC0D // ProgramDefs%GetInputFilePrefix() // ProgramDefs%GetInputFileSuffix()
  inquire( File=FileName, Exist=VarL0D )
  if ( .not. VarL0D ) call Error%Raise( 'Supplied an incompatible external case or it may not exist', ProcName=ProcName )
  call execute_command_line( Command='rm -rf ' // ProgramDefs%GetCaseDir() // '/*' )
  call execute_command_line( Command='cp -rf ' VarC0D // '/* ' // ProgramDefs%GetCaseDir() )
else
  FileName = ProgramDefs%GetCaseDir() // ProgramDefs%GetInputFilePrefix() // ProgramDefs%GetInputFileSuffix()
  inquire( File=FileName, Exist=VarL0D )
  if ( .not. VarL0D ) call Error%Raise( 'Did not find the case directory in the run directory and no external alternative was ' //&
                                        'supplied', ProcName=ProcName )
end if

FileName = ProgramDefs%GetInputFilePath()
call Input%Read( FileName=FileName )

call Input%Write( Logger=Logger )

SMUQTask = Input%GetName()

select case ( LowerCase(SMUQTask) )
  case('main')
    SectionChain = 'main'
    call Root%Construct( Input=Input, SectionChain=SectionChain, Prefix=ProgramDefs%GetCaseDir() )
    call system( 'cp -rf ' // ProgramDefs%GetCaseDir() // '/* ' // ProgramDefs%GetRestartDir )
    call RestartUtility%Construct( Input=Root%GetInput(MainSectionName='main', Prefix=ProgramDefs%GetRestartDir() ,               &
                                 Directory=RestartUtility%GetDirectory(SectionChain='main')), Prefix=ProgramDefs%GetRestartDir() )
    call Root%Run()   
  case('test')
    call Test( Input=Input, Prefix=ProgramDefs%GetCaseDir() )
  case default
    call Error%Raise( Line='Specified task (name of main input section) not recognized' )
end select

end program
