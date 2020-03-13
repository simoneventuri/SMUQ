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

module HierCovGExp2L_Class

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
use CovGExp2L_Class                                               ,only:    CovGExp2L_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    HierCovGExp2L_Type

type, extends(HierCovFunction_Type)                                   ::    HierCovGExp2L_Type
  character(:), allocatable                                           ::    L1_Dependency
  real(rkp)                                                           ::    L1
  character(:), allocatable                                           ::    L2_Dependency
  real(rkp)                                                           ::    L2
  character(:), allocatable                                           ::    Lr_Dependency
  real(rkp)                                                           ::    Lr
  character(:), allocatable                                           ::    Zs_Dependency
  real(rkp)                                                           ::    Zs
  character(:), allocatable                                           ::    Sigma_Dependency
  real(rkp)                                                           ::    Sigma
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
  subroutine Initialize( This )

    class(HierCovGExp2L_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'HierCovGExp2L'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(HierCovGExp2L_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(HierCovGExp2L_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%L1 = One
    This%L1_Dependency = ''
    This%L2 = One
    This%L2_Dependency = ''
    This%Lr = One
    This%Lr_Dependency = ''
    This%Sigma = One
    This%Sigma_Dependency = ''
    This%Zs = Zero
    This%Zs_Dependency = ''
    This%Tolerance = 1e-10
    This%CoordinateLabel = ''
    This%InputRequired = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(HierCovGExp2L_Type), intent(inout)                          ::    This
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

    ParameterName = 'l1_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%L1_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'l1'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%L1=VarR0D

    ParameterName = 'l2_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%L2_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'l2'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%L2=VarR0D

    ParameterName = 'lr_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%Lr_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'lr'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Lr=VarR0D

    ParameterName = 'zs_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%Zs_Dependency=VarC0D
      InputRequiredTrip = .true.
    end if
    MandatoryLoc = .not. Found

    ParameterName = 'zs'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Zs=VarR0D

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

    ParameterName = 'tolerance'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Tolerance=VarR0D

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

    class(HierCovGExp2L_Type), intent(in)                          ::    This
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

    call GetInput%AddParameter( Name='l1', Value=ConvertToString(This%L1) )
    if ( len_trim(This%L1_Dependency) /= 0 )call GetInput%AddParameter( Name='l1_dependency', Value=This%L1_Dependency )

    call GetInput%AddParameter( Name='l2', Value=ConvertToString(This%L2) )
    if ( len_trim(This%L2_Dependency) /= 0 )call GetInput%AddParameter( Name='l2_dependency', Value=This%L2_Dependency )

    call GetInput%AddParameter( Name='lr', Value=ConvertToString(This%Lr) )
    if ( len_trim(This%Lr_Dependency) /= 0 )call GetInput%AddParameter( Name='lr_dependency', Value=This%Lr_Dependency )

    call GetInput%AddParameter( Name='zs', Value=ConvertToString(This%Zs) )
    if ( len_trim(This%Zs_Dependency) /= 0 )call GetInput%AddParameter( Name='zs_dependency', Value=This%Zs_Dependency )

    call GetInput%AddParameter( Name='sigma', Value=ConvertToString(This%Sigma) )
    if ( len_trim(This%Sigma_Dependency) /= 0 )call GetInput%AddParameter( Name='sigma_dependency', Value=This%Sigma_Dependency )

    call GetInput%AddParameter( Name='tolerance', Value=ConvertToString(This%Tolerance) )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate( This, Input, CovFunction )

    class(HierCovGExp2L_Type), intent(in)                             ::    This
    type(Input_Type), intent(in)                                      ::    Input
    class(CovFunction_Type), allocatable, intent(out)                 ::    CovFunction

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    L1
    real(rkp)                                                         ::    L2
    real(rkp)                                                         ::    Lr
    real(rkp)                                                         ::    Zs
    real(rkp)                                                         ::    Sigma

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    L1 = This%L1
    if ( len_trim(This%L1_Dependency) /= 0 ) call Input%GetValue( Value=L1, Label=This%L1_Dependency )

    L2 = This%L2
    if ( len_trim(This%L2_Dependency) /= 0 ) call Input%GetValue( Value=L2, Label=This%L2_Dependency )

    Lr = This%Lr
    if ( len_trim(This%Lr_Dependency) /= 0 ) call Input%GetValue( Value=Lr, Label=This%Lr_Dependency )

    Zs = This%Zs
    if ( len_trim(This%Zs_Dependency) /= 0 ) call Input%GetValue( Value=Zs, Label=This%Zs_Dependency )

    Sigma = This%Sigma
    if ( len_trim(This%Sigma_Dependency) /= 0 ) call Input%GetValue( Value=Sigma, Label=This%Sigma_Dependency )

    allocate( CovGExp2L_Type :: CovFunction )

    select type (CovFunction)
      type is (CovGExp2L_Type)
        call CovFunction%Construct( Sigma=Sigma, L1=L1, L2=L2, Lr=Lr, Zs=Zs, Coordinate=This%CoordinateLabel,                     &
                                                                                                        Tolerance=This%Tolerance )
      class default
        call Error%Raise( "Something went wrong", ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(HierCovGExp2L_Type), intent(out)                            ::    LHS
    class(HierCovFunction_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (HierCovGExp2L_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%L1_Dependency = RHS%L1_Dependency
          LHS%L1 = RHS%L1
          LHS%L2_Dependency = RHS%L2_Dependency
          LHS%L2 = RHS%L2
          LHS%Lr_Dependency = RHS%Lr_Dependency
          LHS%Lr = RHS%Lr
          LHS%Zs_Dependency = RHS%Zs_Dependency
          LHS%Zs = RHS%Zs
          LHS%Sigma_Dependency = RHS%Sigma_Dependency
          LHS%Sigma = RHS%Sigma
          LHS%CoordinateLabel = RHS%CoordinateLabel
          LHS%Tolerance = RHS%Tolerance
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(HierCovGExp2L_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
