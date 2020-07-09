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

module CovLogisticDiag_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use CovFunction_Class                                             ,only:    CovFunction_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    CovLogisticDiag_Type

type, extends(CovFunction_Type)                                       ::    CovLogisticDiag_Type
  real(rkp)                                                           ::    Sigma
  real(rkp)                                                           ::    K
  real(rkp)                                                           ::    X0
  character(:), allocatable                                           ::    CoordinateLabel
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Evaluate_1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(CovLogisticDiag_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'CovLogisticDiag'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(CovLogisticDiag_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(CovLogisticDiag_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Sigma = One
    This%K = One
    This%X0 = Zero
    This%CoordinateLabel = ''

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(CovLogisticDiag_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    ParameterName
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'sigma'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Sigma=VarR0D
    if (This%Sigma < Zero) call Error%Raise(Line='Sigma value below 0', ProcName=ProcName)

    ParameterName = 'k'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
    This%K=VarR0D

    ParameterName = 'x0'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
    This%X0=VarR0D

    ParameterName = 'coordinate_label'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
    This%CoordinateLabel=VarC0D

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, Sigma, K, X0, Coordinate)

    class(CovLogisticDiag_Type), intent(inout)                        ::    This
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in)                                             ::    K
    real(rkp), intent(in)                                             ::    X0
    character(*), intent(in)                                          ::    Coordinate

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    This%Sigma = Sigma
    if (This%Sigma < Zero) call Error%Raise(Line='Sigma value below 0', ProcName=ProcName)

    This%K = K

    This%X0 = X0

    This%CoordinateLabel = trim(adjustl(Coordinate))

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(CovLogisticDiag_Type), intent(in)                           ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    call GetInput%AddParameter(Name='coordinate_label', Value=This%CoordinateLabel)
    call GetInput%AddParameter(Name='sigma', Value=ConvertToString(This%Sigma))
    call GetInput%AddParameter(Name='k', Value=ConvertToString(This%K))
    call GetInput%AddParameter(Name='x0', Value=ConvertToString(This%X0))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Evaluate_1D(This, Coordinates, CoordinateLabels, Covariance)

    class(CovLogisticDiag_Type), intent(in)                           ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Coordinates
    type(SMUQString_Type), dimension(:), intent(in)                   ::    CoordinateLabels
    real(rkp), dimension(:,:), intent(inout)                          ::    Covariance

    character(*), parameter                                           ::    ProcName='Evaluate_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbNodes
    integer                                                           ::    iCoordinate

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    NbNodes = size(Coordinates,1)

    i = 1
    iCoordinate = 0
    do i = 1, size(Coordinates,2)
      if (CoordinateLabels(i) == This%CoordinateLabel) then
        iCoordinate = i
        exit
      end if
    end do
    if (iCoordinate == 0) call Error%Raise('Did not find matching coordinate label: ' // This%CoordinateLabel,                 &
                                                                                                               ProcName=ProcName)

    if (size(Covariance,1) /= size(Covariance,2)) call Error%Raise('Passed non-square array', ProcName=ProcName)
    if (size(Covariance,1) /= NbNodes) call Error%Raise('Covariance array dimensions and number of coordinates mismatch',      &
                                                                                                               ProcName=ProcName)

    Covariance = Zero

    i = 1
    do i = 1, NbNodes
      Covariance(i,i) = This%Sigma**2 / (One + dexp(-This%K*(Coordinates(i,iCoordinate)-This%X0)))
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(CovLogisticDiag_Type), intent(out)                          ::    LHS
    class(CovFunction_Type), intent(in)                               ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (CovLogisticDiag_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%Sigma = RHS%Sigma
          LHS%K = RHS%K
          LHS%X0 = RHS%X0
          LHS%CoordinateLabel = RHS%CoordinateLabel
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(CovLogisticDiag_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
