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

module HierCovGExp1L_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use CovFunction_Class                                             ,only:    CovFunction_Type
use HierCovFunction_Class                                         ,only:    HierCovFunction_Type
use CovGExp1L_Class                                               ,only:    CovGExp1L_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    HierCovGExp1L_Type

type, extends(HierCovFunction_Type)                                   ::    HierCovGExp1L_Type
  character(:), allocatable                                           ::    L_Dependency
  real(rkp)                                                           ::    L
  character(:), allocatable                                           ::    Sigma_Dependency
  real(rkp)                                                           ::    Sigma
  character(:), allocatable                                           ::    Gam_Dependency
  real(rkp)                                                           ::    Gam
  real(rkp)                                                           ::    Tolerance
  character(:), allocatable                                           ::    CoordinateLabel
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Generate
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(HierCovGExp1L_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'HierCovGExp1L'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(HierCovGExp1L_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(HierCovGExp1L_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%L = One
    This%L_Dependency = ''
    This%Sigma = One
    This%Sigma_Dependency = ''
    This%Gam = Zero
    This%Gam_Dependency = ''
    This%Tolerance = 1e-10
    This%CoordinateLabel = ''
    This%InputRequired = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(HierCovGExp1L_Type), intent(inout)                          ::    This
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
    logical                                                           ::    MandatoryLoc
    logical                                                           ::    InputRequiredTrip

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    InputRequiredTrip = .false.

    MandatoryLoc = .false.

    ParameterName = 'l_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) then
      This%L_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'l'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%L=VarR0D

    ParameterName = 'sigma_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) then
      This%Sigma_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'sigma'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%Sigma=VarR0D

    ParameterName = 'gamma_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) then
      This%Gam_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'gamma'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%Gam=VarR0D

    ParameterName = 'tolerance'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%Tolerance=VarR0D

    ParameterName = 'coordinate_label'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
    if (Found) This%CoordinateLabel=VarC0D

    if (.not. InputRequiredTrip) This%InputRequired = .false.

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(HierCovGExp1L_Type), intent(in)                             ::    This
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

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    call GetInput%AddParameter(Name='coordinate_label', Value=This%CoordinateLabel)

    call GetInput%AddParameter(Name='l', Value=ConvertToString(This%L))
    if (len_trim(This%L_Dependency) /= 0)call GetInput%AddParameter(Name='l_dependency', Value=This%L_Dependency)

    call GetInput%AddParameter(Name='sigma', Value=ConvertToString(This%Sigma))
    if (len_trim(This%Sigma_Dependency) /= 0) call GetInput%AddParameter(Name='sigma_dependency', Value=This%Sigma_Dependency)

    call GetInput%AddParameter(Name='gamma', Value=ConvertToString(This%Gam))
    if (len_trim(This%Gam_Dependency) /= 0)call GetInput%AddParameter(Name='gamma_dependency', Value=This%Gam_Dependency)

    call GetInput%AddParameter(Name='tolerance', Value=ConvertToString(This%Tolerance))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate(This, Input, CovFunction)

    class(HierCovGExp1L_Type), intent(in)                             ::    This
    type(Input_Type), intent(in)                                      ::    Input
    class(CovFunction_Type), allocatable, intent(out)                 ::    CovFunction

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    L
    real(rkp)                                                         ::    Gamma
    real(rkp)                                                         ::    Sigma

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    L = This%L
    if (len_trim(This%L_Dependency) /= 0) call Input%GetValue(Value=L, Label=This%L_Dependency)

    Gamma = This%Gam
    if (len_trim(This%Gam_Dependency) /= 0) call Input%GetValue(Value=Gamma, Label=This%Gam_Dependency)

    Sigma = This%Sigma
    if (len_trim(This%Sigma_Dependency) /= 0) call Input%GetValue(Value=Sigma, Label=This%Sigma_Dependency)

    allocate(CovGExp1L_Type :: CovFunction)

    select type (CovFunction)
      type is (CovGExp1L_Type)
        call CovFunction%Construct(Sigma=Sigma, L=L, Gamma=Gamma, Coordinate=This%CoordinateLabel, Tolerance=This%Tolerance)
      class default
        call Error%Raise("Something went wrong", ProcName=ProcName)
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(HierCovGExp1L_Type), intent(out)                            ::    LHS
    class(HierCovFunction_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (HierCovGExp1L_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%L_Dependency = RHS%L_Dependency
          LHS%L = RHS%L
          LHS%Sigma_Dependency = RHS%Sigma_Dependency
          LHS%Sigma = RHS%Sigma
          LHS%CoordinateLabel = RHS%CoordinateLabel
          LHS%Gam_Dependency = RHS%Gam_Dependency
          LHS%Gam = RHS%Gam
          LHS%Tolerance = RHS%Tolerance
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(HierCovGExp1L_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
