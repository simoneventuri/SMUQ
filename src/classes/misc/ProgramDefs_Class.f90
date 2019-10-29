! -*-f90-*-
!!--------------------------------------------------------------------------------------------------------------------------------
!!
!! Stochastic Modeling & Uncertainty Quantification (SMUQ)
!!
!! Copyright (C) 2016 Venturi, Simone & Rostkowski, Przemyslaw (University of Illinois at Urbana-Champaign)
!!
!! This program is free software; you can redistribute it and/or modify it under the terms of the Version 2.1 GNU Lesser General
!! Public License as published by the Free Software Foundation.
!!
!! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free 
!! Software Foundation, Inc. 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!!
!!--------------------------------------------------------------------------------------------------------------------------------

module ProgramDefs_Class

use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Library

implicit none

private

public                                                                ::    ProgramDefs

type                                                                  ::    ProgramDefs_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    iProcess
  integer                                                             ::    NbProcesses
  character(:), allocatable                                           ::    RunDir
  character(:), allocatable                                           ::    CaseDir
  character(:), allocatable                                           ::    InputFilePath
  character(:), allocatable                                           ::    InputFilePrefix
  character(:), allocatable                                           ::    InputFileSuffix
  character(:), allocatable                                           ::    OutputDir
  character(:), allocatable                                           ::    RestartDir
  character(:), allocatable                                           ::    LogDir
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct                   =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetCaseDir
  procedure, public                                                   ::    GetRunDir
  procedure, public                                                   ::    GetInputFilePath
  procedure, public                                                   ::    GetInputFilePrefix
  procedure, public                                                   ::    GetInputFileSuffix
  procedure, public                                                   ::    GetOutputDir
  procedure, public                                                   ::    GetRestartDir
  procedure, public                                                   ::    GetNbProcesses
  procedure, public                                                   ::    GetiProcess
  procedure, private                                                  ::    WriteNbProcesses
  generic, public                                                     ::    assignment(=)               =>    Copy
  procedure, public                                                   ::    Copy
end Type

type(ProgramDefs_Type)                                                ::    ProgramDefs
logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(ProgramDefs_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'programdefs'
      call This%SetDefaults()
      if (DebugLoc) call Logger%Write( "Initialization Successful" )
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(ProgramDefs_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    call This%Initialize()
    if (DebugLoc) call Logger%Write( "Reset Successful" )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(ProgramDefs_Type),intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='SetDefaults'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%iProcess = 0
    This%NbProcesses = 0
    This%RunDir = '<undefined>'
    This%LogDir = '<undefined>'
    This%CaseDir = '<undefined>'
    This%InputFilePath = '<undefined>'
    This%InputFilePrefix = '/input'
    This%InputFileSuffix = '/input.dat'
    This%OutputDir = '<undefined>'
    This%RestartDir = '<undefined>'

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------  

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )
    
    use String_Library

    class(ProgramDefs_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructInput'
    logical                                                           ::    DebugLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    Found
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset
    if ( .not. This%Initialized ) call This%Initialize 

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'iprocess'
    call Input%GetValue( VarI0D, ParameterName, Mandatory=.true. )
    This%iProcess = VarI0D

    ParameterName = 'nbprocesses'
    call Input%GetValue( VarI0D, ParameterName, Mandatory=.true. )
    This%NbProcesses = VarI0D

    ParameterName = 'rundir'
    call Input%GetValue( VarC0D, ParameterName, Mandatory=.true. )
    This%RunDir = VarC0D

    ParameterName = 'logdir'
    call Input%GetValue( VarC0D, ParameterName, Mandatory=.true. )
    This%LogDir = VarC0D

    ParameterName = 'casedir'
    call Input%GetValue( VarC0D, ParameterName, Mandatory=.true. )
    This%CaseDir = VarC0D

    This%InputFilePath = This%CaseDir // This%InputFilePrefix // This%InputFileSuffix

    ParameterName = 'outputdir'
    call Input%GetValue( VarC0D, ParameterName, Mandatory=.true. )
    This%OutputDir = VarC0D

    ParameterName = 'restartdir'
    call Input%GetValue( VarC0D, ParameterName, Mandatory=.true. )
    This%RestartDir = VarC0D

    This%Constructed = .true.

    call This%WriteNbProcesses()
    
    if (DebugLoc) call Logger%Exiting
    
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(ProgramDefs_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='iprocess', Value=ConvertToString( Value=This%iProcess ) )
    call GetInput%AddParameter( Name='nbprocesses', Value=ConvertToString( Value=This%NbProcesses ) )
    call GetInput%AddParameter( Name='rundir', Value=This%RunDir )
    call GetInput%AddParameter( Name='logdir', Value=This%LogDir )
    call GetInput%AddParameter( Name='casedir', Value=This%CaseDir )
    call GetInput%AddParameter( Name='outputdir', Value=This%OutputDir )
    call GetInput%AddParameter( Name='restartdir', Value=This%RestartDir )
    call GetInput%AddParameter( Name='logdir', Value=This%LogDir )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  Subroutine WriteNbProcesses( This, Debug )
    
    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='WriteNbProcesses'
    logical                                                           ::    DebugLoc
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    UnitLoc
    integer                                                           ::    IOLoc
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if (This%iProcess == 1) then
      FileName = This%OutputDir // "/NbProcesses.dat"
      open(newunit=UnitLoc, FILE=FileName, status='replace', action='write' )
        write(10,'(A)')        "# Number of Processes"
        write(10,'(I0)')   This%NbProcesses
      close(unit=10)
    end if
    
    if (DebugLoc) call Logger%Exiting
    
  End Subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOutputDir( This, Debug )

    character(:), allocatable                                         ::    GetOutputDir

    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetOutputDir'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetOutputDir = This%OutputDir

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetRestartDir( This, Debug )

    character(:), allocatable                                         ::    GetRestartDir

    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetRestartDir'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetRestartDir = This%RestartDir

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInputFilePath( This, Debug )

    character(:), allocatable                                         ::    GetInputFilePath

    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetInputFilePath'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetInputFilePath = This%InputFilePath

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInputFilePrefix( This, Debug )

    character(:), allocatable                                         ::    GetInputFilePrefix

    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetInputFilePrefix'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetInputFilePrefix = This%InputFilePrefix

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInputFileSuffix( This, Debug )

    character(:), allocatable                                         ::    GetInputFileSuffix

    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetInputFileSuffix'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetInputFileSuffix = This%InputFileSuffix

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCaseDir( This, Debug )

    character(:), allocatable                                         ::    GetCaseDir

    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetCaseDir'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetCaseDir = This%CaseDir

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbProcesses( This, Debug )

    integer                                                           ::    GetNbProcesses

    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetNbProcesses'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbProcesses = This%NbProcesses

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetiProcess( This, Debug )

    integer                                                           ::    GetiProcess

    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetiProcess'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetiProcess = This%iProcess

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetRunDir( This, Debug )

    character(:), allocatable                                         ::    GetRunDir

    class(ProgramDefs_Type),intent(in)                                ::    This
    logical, optional, intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetRunDir'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetRunDir = This%RunDir

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(ProgramDefs_Type), intent(out)                              ::    LHS
    class(ProgramDefs_Type), intent(in)                               ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Reset()

    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed
    
    if ( RHS%Constructed ) then
      LHS%iProcess = RHS%iProcess
      LHS%NbProcesses = RHS%NbProcesses
      LHS%RunDir = RHS%RunDir
      LHS%LogDir = RHS%LogDir
      LHS%CaseDir = RHS%CaseDir
      LHS%InputFilePrefix = RHS%InputFilePrefix
      LHS%InputFileSuffix = RHS%InputFileSuffix
      LHS%InputFilePath = RHS%InputFilePath
      LHS%OutputDir = RHS%OutputDir
      LHS%RestartDir = RHS%RestartDir
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

End Module
