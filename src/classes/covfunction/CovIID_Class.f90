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

module CovIID_Class

use Input_Library
use Parameters_Library
use ArrayRoutines_Module
use StringConversion_Module
use CommandRoutines_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use CovFunction_Class                                             ,only:    CovFunction_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    CovIID_Type

type, extends(CovFunction_Type)                                       ::    CovIID_Type
  real(rkp)                                                           ::    Sigma
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

    class(CovIID_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'CovIID'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(CovIID_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(CovIID_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Sigma = One

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(CovIID_Type), intent(inout)                                 ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'sigma'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Sigma=VarR0D

    if (This%Sigma < Zero) call Error%Raise(Line='Sigma value below 0', ProcName=ProcName)

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, Sigma)

    class(CovIID_Type), intent(inout)                                 ::    This
    real(rkp), intent(in)                                             ::    Sigma

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    This%Sigma = Sigma
    if (This%Sigma < Zero) call Error%Raise(Line='Sigma value below 0', ProcName=ProcName)

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(CovIID_Type), intent(in)                      ::    This
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
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.
    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    call GetInput%AddParameter(Name='sigma', Value=ConvertToString(This%Sigma))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Evaluate_1D(This, Coordinates, CoordinateLabels, Covariance)

    class(CovIID_Type), intent(in)                                    ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Coordinates
    type(SMUQString_Type), dimension(:), intent(in)                   ::    CoordinateLabels
    real(rkp), dimension(:,:), intent(inout)                          ::    Covariance

    character(*), parameter                                           ::    ProcName='Evaluate_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbNodes

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    NbNodes = size(Coordinates,1)

    if (size(Covariance,1) /= size(Covariance,2)) call Error%Raise('Passed non-square array', ProcName=ProcName)
    if (size(Covariance,1) /= NbNodes) call Error%Raise('Covariance array dimensions and number of coordinates mismatch',      &
                                                                                                               ProcName=ProcName)

    call Eye(Array=Covariance)

    Covariance = Covariance * This%Sigma**2

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(CovIID_Type), intent(out)                                   ::    LHS
    class(CovFunction_Type), intent(in)                               ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (CovIID_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%Sigma = RHS%Sigma
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(CovIID_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
