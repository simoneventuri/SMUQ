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
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    ProgramDefs

type                                                                  ::    ProgramDefs_Type
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    RunDir
  character(:), allocatable                                           ::    SuppliedCaseDir
  character(:), allocatable                                           ::    CaseDir
  character(:), allocatable                                           ::    InputFilePath
  character(:), allocatable                                           ::    InputFilePrefix
  character(:), allocatable                                           ::    InputFileSuffix
  character(:), allocatable                                           ::    OutputDir
  character(:), allocatable                                           ::    RestartDir
  character(:), allocatable                                           ::    LogDir
  character(:), allocatable                                           ::    LogFilePath
contains
  generic, public                                                     ::    Construct                   =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetSuppliedCaseDir
  procedure, public                                                   ::    GetCaseDir
  procedure, public                                                   ::    GetRunDir
  procedure, public                                                   ::    GetLogDir
  procedure, public                                                   ::    GetLogFilePath
  procedure, public                                                   ::    GetOutputDir
  procedure, public                                                   ::    GetRestartDir
  procedure, public                                                   ::    GetInputFilePath
  procedure, public                                                   ::    GetInputFilePrefix
  procedure, public                                                   ::    GetInputFileSuffix
  generic, public                                                     ::    assignment(=)               =>    Copy
  procedure, public                                                   ::    Copy
end Type

type(ProgramDefs_Type)                                                ::    ProgramDefs

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(ProgramDefs_Type), intent(inout)                              ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  This%RunDir = ''
  This%LogDir = ''
  This%CaseDir = ''
  This%SuppliedCaseDir = ''
  This%InputFilePath = ''
  This%InputFilePrefix = 'input/'
  This%InputFileSuffix = 'input.dat'
  This%OutputDir = ''
  This%RestartDir = ''

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------  

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)
  
  class(ProgramDefs_Type), intent(inout)                              ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix
  
  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    VarC0D
  character(:), allocatable                                           ::    PrefixLoc
  logical                                                             ::    Found
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  This%RunDir = PrefixLoc

  ParameterName = 'log_directory'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(VarC0D, ParameterName, Mandatory=.false., Found=Found)
  This%LogDir = PrefixLoc // 'log/'
  if (Found) This%LogDir = PrefixLoc // VarC0D
  if (This%LogDir(len(This%LogDir):len(This%LogDir)) /= '/') This%LogDir = This%LogDir // '/'

  ParameterName = 'log_file_name'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(VarC0D, ParameterName, Mandatory=.false., Found=Found)
  This%LogFilePath = This%LogDir // 'log.dat'
  if (Found) This%LogFilePath = This%LogDir // VarC0D

  ParameterName = 'output_directory'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(VarC0D, ParameterName, Mandatory=.false., Found=Found)
  This%OutputDir = PrefixLoc // 'output/'
  if (Found) This%OutputDir = PrefixLoc // VarC0D
  if (This%OutputDir(len(This%OutputDir):len(This%OutputDir)) /= '/') This%OutputDir = This%OutputDir // '/'

  ParameterName = 'restart_directory'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(VarC0D, ParameterName, Mandatory=.false., Found=Found)
  This%RestartDir = PrefixLoc // 'restart/'
  if (Found) This%RestartDir = PrefixLoc // VarC0D
  if (This%RestartDir(len(This%RestartDir):len(This%RestartDir)) /= '/') This%RestartDir = This%RestartDir // '/'

  ParameterName = 'case_directory'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(VarC0D, ParameterName, Mandatory=.false., Found=Found)
  This%CaseDir = PrefixLoc // 'case/'
  if (Found) This%CaseDir = PrefixLoc // VarC0D
  if (This%CaseDir(len(This%CaseDir):len(This%CaseDir)) /= '/') This%CaseDir = This%CaseDir // '/'

  This%InputFilePath = This%CaseDir // This%InputFilePrefix // This%InputFileSuffix

  ParameterName = 'case'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(VarC0D, ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%SuppliedCaseDir = VarC0D

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.
  
end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetOutputDir(This)

  character(:), allocatable                                           ::    GetOutputDir

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetOutputDir'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetOutputDir = This%OutputDir

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetRestartDir(This)

  character(:), allocatable                                           ::    GetRestartDir

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetRestartDir'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetRestartDir = This%RestartDir

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInputFilePath(This)

  character(:), allocatable                                           ::    GetInputFilePath

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetInputFilePath'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetInputFilePath = This%InputFilePath

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInputFilePrefix(This)

  character(:), allocatable                                           ::    GetInputFilePrefix

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetInputFilePrefix'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetInputFilePrefix = This%InputFilePrefix

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInputFileSuffix(This)

  character(:), allocatable                                           ::    GetInputFileSuffix

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetInputFileSuffix'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetInputFileSuffix = This%InputFileSuffix

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetSuppliedCaseDir(This)

  character(:), allocatable                                           ::    GetSuppliedCaseDir

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetSuppliedCaseDir'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetSuppliedCaseDir = This%SuppliedCaseDir

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetCaseDir(This)

  character(:), allocatable                                           ::    GetCaseDir

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetCaseDir'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetCaseDir = This%CaseDir

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetRunDir(This)

  character(:), allocatable                                           ::    GetRunDir

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetRunDir'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetRunDir = This%RunDir

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetLogDir(This)

  character(:), allocatable                                           ::    GetLogDir

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetLogDir'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetLogDir = This%LogDir

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetLogFilePath(This)

  character(:), allocatable                                           ::    GetLogFilePath

  class(ProgramDefs_Type),intent(in)                                  ::    This
  
  character(*), parameter                                             ::    ProcName='GetLogFilePath'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetLogFilePath = This%LogFilePath

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(ProgramDefs_Type), intent(out)                                ::    LHS
  class(ProgramDefs_Type), intent(in)                                 ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  call LHS%Reset()

  LHS%Constructed = RHS%Constructed
  
  if (RHS%Constructed) then
    LHS%RunDir = RHS%RunDir
    LHS%LogDir = RHS%LogDir
    LHS%CaseDir = RHS%CaseDir
    LHS%SuppliedCaseDir = RHS%SuppliedCaseDir
    LHS%InputFilePrefix = RHS%InputFilePrefix
    LHS%InputFileSuffix = RHS%InputFileSuffix
    LHS%InputFilePath = RHS%InputFilePath
    LHS%OutputDir = RHS%OutputDir
    LHS%RestartDir = RHS%RestartDir
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

End Module
