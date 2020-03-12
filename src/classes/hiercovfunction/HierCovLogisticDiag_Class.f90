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

module HierCovLogisticDiag_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use HierCovFunction_Class                                         ,only:    HierCovFunction_Type
use CovLogisticDiag_Class                                         ,only:    CovLogisticDiag_Type

implicit none

private

public                                                                ::    HierCovLogisticDiag_Type

type, extends(HierCovFunction_Type)                                   ::    HierCovLogisticDiag_Type
  character(:), allocatable                                           ::    Sigma_Dependency
  real(rkp)                                                           ::    Sigma
  character(:), allocatable                                           ::    K_Dependency
  real(rkp)                                                           ::    K
  character(:), allocatable                                           ::    X0_Dependency
  real(rkp)                                                           ::    X0
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
  subroutine Initialize( This )

    class(HierCovLogisticDiag_Type), intent(inout)                    ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'HierCovLogisticDiag'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(HierCovLogisticDiag_Type), intent(inout)                    ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(HierCovLogisticDiag_Type), intent(inout)                    ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Sigma = One
    This%Sigma_Dependency = ''
    This%K = One
    This%K_Dependency = ''
    This%X0 = Zero
    This%X0_Dependency = ''
    This%CoordinateLabel = ''
    This%InputRequired = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(HierCovLogisticDiag_Type), intent(inout)                    ::    This
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

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    InputRequiredTrip = .false.

    MandatoryLoc = .false.

    ParameterName = 'sigma_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%Sigma_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'sigma'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Sigma=VarR0D

    ParameterName = 'k_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%K_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'k'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%K=VarR0D

    ParameterName = 'x0_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%X0_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'x0'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%X0=VarR0D

    ParameterName = 'coordinate_label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    if ( Found ) This%CoordinateLabel=VarC0D

    if ( .not. InputRequiredTrip ) This%InputRequired = .false.

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(HierCovLogisticDiag_Type), intent(in)                       ::    This
    character(*), intent(in)                                          ::    MainSectionName
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

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='coordinate_label', Value=This%CoordinateLabel )

    call GetInput%AddParameter( Name='sigma', Value=ConvertToString(This%Sigma) )
    if ( len_trim(This%Sigma_Dependency) /= 0 )call GetInput%AddParameter( Name='sigma_dependency', Value=This%Sigma_Dependency )

    call GetInput%AddParameter( Name='k', Value=ConvertToString(This%K) )
    if ( len_trim(This%K_Dependency) /= 0 )call GetInput%AddParameter( Name='k_dependency', Value=This%K_Dependency )

    call GetInput%AddParameter( Name='x0', Value=ConvertToString(This%X0) )
    if ( len_trim(This%X0_Dependency) /= 0 )call GetInput%AddParameter( Name='x0_dependency', Value=This%X0_Dependency )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate( This, Input, CovFunction )

    class(HierCovLogisticDiag_Type), intent(in)                       ::    This
    type(Input_Type), intent(in)                                      ::    Input
    class(CovFunction_Type), allocatable, intent(out)                 ::    CovFunction

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    Sigma
    real(rkp)                                                         ::    K
    real(rkp)                                                         ::    X0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    Sigma = This%Sigma
    if ( len_trim(This%Sigma_Dependency) /= 0 ) call Input%GetValue( Value=Sigma, Label=This%Sigma_Dependency )

    K = This%K
    if ( len_trim(This%K_Dependency) /= 0 ) call Input%GetValue( Value=K, Label=This%K_Dependency )

    X0 = This%X0
    if ( len_trim(This%X0_Dependency) /= 0 ) call Input%GetValue( Value=X0, Label=This%X0_Dependency )

    allocate( CovLogisticDiag_Type :: CovFunction )

    select type (CovFunction)
      type is (CovLogisticDiag_Type)
        call CovFunction%Construct( Sigma=Sigma, K=K, X0=X0, Coordinate=This%CoordinateLabel )
      class default
        call Error%Raise( "Something went wrong", ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(HierCovLogisticDiag_Type), intent(out)                      ::    LHS
    class(HierCovConstructor_Type), intent(in)                        ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (HierCovLogisticDiag_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Sigma_Dependency = RHS%=Sigma_Dependency
          LHS%Sigma = RHS%Sigma
          LHS%K_Dependency = RHS%K_Dependency
          LHS%K = RHS%K
          LHS%X0_Dependency = RHS%X0_Dependency
          LHS%X0 = RHS%X0
          LHS%CoordinateLabel = RHS%CoordinateLabel
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(HierCovLogisticDiag_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
