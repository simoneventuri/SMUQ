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

module CVLOO_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use CVMethod_Class                                                ,only:    CVMethod_Type, CVFitTarget

implicit none

private

public                                                                ::    CVLOO_Type

type, extends(CVMethod_Type)                                          ::    CVLOO_Type
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Calculate
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(CVLOO_Type), intent(inout)                                    ::    This

  character(*), parameter                                             ::    ProcName='Initialize'

  if (.not. This%Initialized) then
    This%Name = 'CVloo'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(CVLOO_Type), intent(inout)                                    ::    This

  character(*), parameter                                             ::    ProcName='Reset'

  This%Initialized=.false.
  This%Constructed=.false.

  call This%SetDefaults()

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(CVLOO_Type), intent(inout)                                    ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%Normalized=.true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(CVLOO_Type), intent(inout)                                    ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    PrefixLoc
  logical                                                             ::    Found

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  ParameterName = 'normalized'
  call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if(Found) This%Normalized = VarL0D

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, Normalized)

  class(CVLOO_Type), intent(inout)                                    ::    This
  logical, optional, intent(in)                                       ::    Normalized

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    ParameterName
  logical                                                             ::    Found

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  if(present(Normalized)) This%Normalized = Normalized

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput

  class(CVLOO_Type), intent(in)                                       ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))
  call GetInput%AddParameter(Name='normalized', Value=ConvertToString(Value=This%Normalized))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Calculate(This, Fit, FitData)

  real(rkp)                                                           ::    Calculate

  class(CVLOO_Type), intent(in)                                       ::    This
  procedure(CVFitTarget), pointer                                     ::    Fit 
  real(rkp), dimension(:), intent(in)                                 ::    FitData

  character(*), parameter                                             ::    ProcName='Calculate'
  integer                                                             ::    StatLoc=0
  real(rkp)                                                           ::    MSESum
  real(rkp), allocatable, dimension(:)                                ::    TrainingSet
  integer, allocatable, dimension(:)                                  ::    TrainingSetIndices
  real(rkp), dimension(1)                                             ::    ValidationSet
  integer, dimension(1)                                               ::    ValidationSetIndices
  real(rkp), dimension(1)                                             ::    Residual
  integer                                                             ::    NbData
  integer                                                             ::    i
  real(rkp)                                                           ::    FitDataVariance

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  NbData = size(FitData,1)
  if (NbData < 2) call Error%Raise('Need to have at least 2 data points for cross validation', ProcName=ProcName)

  allocate(TrainingSet(NbData-1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='TrainingSet', ProcName=ProcName, stat=StatLoc)

  allocate(TrainingSetIndices(NbData-1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='TrainingSetIndices', ProcName=ProcName, stat=StatLoc)

  ValidationSet(1) = FitData(1)
  ValidationSetIndices(1) = 1
  TrainingSet = FitData(2:)

  MSESum = Zero

  i = 1
  do i = 1, NbData
    call Fit(TrainingSet=TrainingSet, TrainingSetIndices=TrainingSetIndices, ValidationSet=ValidationSet, &
             ValidationSetIndices=ValidationSetIndices, Residual=Residual)
    MSESum = MSESum + Residual(1)**2
    if (i == NbData) exit
    ValidationSet(1) = FitData(i+1)
    ValidationSetIndices(1) = i+1
    TrainingSetIndices(i) = i 
    TrainingSet(i) = FitData(i)
  end do

  Calculate = MSESum / real(NbData,rkp)

  if (This%Normalized) then
    FitDataVariance = ComputeVariance(Values=FitData)
    if (FitDataVariance > Zero) then
      Calculate = Calculate / FitDataVariance
    else
      Calculate = Zero
    end if
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(CVLOO_Type), intent(out)                                      ::    LHS
  class(CVMethod_Type), intent(in)                                    ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)
    type is (CVLOO_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed
      if (RHS%Constructed) then
        LHS%Normalized = RHS%Normalized
      end if
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)
  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
