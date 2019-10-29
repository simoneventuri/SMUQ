program SMUQ

use Input_Library
use String_Library
use Test_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use ProgramDefs_Class                                             ,only:    ProgramDefs
use Root_Class                                                    ,only:    Root_Type
use Restart_Class                                                 ,only:    RestartUtility

implicit none

type(InputReader_Type)                                                ::    Input
type(InputSection_Type), pointer                                      ::    InputSection=>null()
type(InputSection_Type)                                               ::    CMDArgsSection
type(Root_Type)                                                       ::    Root
character(:), allocatable                                             ::    ParameterName
character(:), allocatable                                             ::    SectionName
character(:), allocatable                                             ::    FileName
character(:), allocatable                                             ::    RunDir
character(:), allocatable                                             ::    LogDir
logical                                                               ::    Found
logical                                                               ::    Debug_Loc = .false.
character(:), allocatable                                             ::    SectionChain
character(:), allocatable                                             ::    SMUQTask

call CMDArgsSection%SetName( 'cmdargs' )
call CMDArgsSection%AddCommandLineArguments()
call CMDArgsSection%GetValue( Value=LogDir, ParameterName='logdir', Mandatory=.true. )

FileName = LogDir // '/runlog.log'
call Logger%Initialize( FileName=FileName,  &
            Status          =       'REPLACE',                                                   &
            Position        =       'REWIND',                                                    &
            Procedure       =       'SurrogateModeling',                                         &
            Indentation     =       2                                                            )

call ProgramDefs%Construct( Input=CMDArgsSection )

FileName = ProgramDefs%GetInputFilePath()

call Input%Read( FileName=FileName )

call Input%Write( Logger=Logger )

SMUQTask = Input%GetName()

select case ( LowerCase(SMUQTask) )
  case('main')
    SectionChain = 'main'
    call Root%Construct( Input=Input, SectionChain=SectionChain, Prefix=ProgramDefs%GetCaseDir() )
    call RestartUtility%Construct( Input=Root%GetInput(MainSectionName='main', Prefix=ProgramDefs%GetRestartDir() ,               &
                                 Directory=RestartUtility%GetDirectory(SectionChain='main')), Prefix=ProgramDefs%GetRestartDir() )
    call Root%Run()   
  case('test')
    call Test( Input=Input, Prefix=ProgramDefs%GetCaseDir() )
  case default
    call Error%Raise( Line='Specified SMUQTask (name of main input section) not recognized' )
end select

end program
