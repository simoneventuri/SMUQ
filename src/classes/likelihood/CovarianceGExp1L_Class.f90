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

module CovarianceGExp1L_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use CovarianceConstructor_Class                                   ,only:    CovarianceConstructor_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    CovarianceGExp1L_Type

type, extends(CovarianceConstructor_Type)                             ::    CovarianceGExp1L_Type
  character(:), allocatable                                           ::    L_Dependency
  real(rkp)                                                           ::    L
  character(:), allocatable                                           ::    M_Dependency
  real(rkp)                                                           ::    M
  character(:), allocatable                                           ::    M_Transform
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
  procedure, public                                                   ::    AssembleCov
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(CovarianceGExp1L_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'CovarianceGExp1L'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(CovarianceGExp1L_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(CovarianceGExp1L_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%L = One
    This%L_Dependency = ''
    This%M = One
    This%M_Dependency = ''
    This%M_Transform = ''
    This%Gam = Zero
    This%Gam_Dependency = ''
    This%Tolerance = 1e-10
    This%CoordinateLabel = ''

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(CovarianceGExp1L_Type), intent(inout)                       ::    This
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

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    MandatoryLoc = .false.

    ParameterName = 'l_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%L_Dependency=VarC0D
    MandatoryLoc = .not. Found

    ParameterName = 'l'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%L=VarR0D

    ParameterName = 'm_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%M_Dependency=VarC0D
    MandatoryLoc = .not. Found

    ParameterName = 'm'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%M=VarR0D

    ParameterName = 'm_transform'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%M_Transform = VarC0D

    ParameterName = 'gamma_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Gam_Dependency=VarC0D
    MandatoryLoc = .not. Found

    ParameterName = 'gamma'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Gam=VarR0D

    ParameterName = 'tolerance'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Tolerance=VarR0D

    ParameterName = 'coordinate_label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    if ( Found ) This%CoordinateLabel=VarC0D

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(CovarianceGExp1L_Type), intent(in)                          ::    This
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

    call GetInput%AddParameter( Name='l', Value=ConvertToString(This%L) )
    if ( len_trim(This%L_Dependency) /= 0 )call GetInput%AddParameter( Name='l_dependency', Value=This%L_Dependency )

    call GetInput%AddParameter( Name='m', Value=ConvertToString(This%M) )
    if ( len_trim(This%M_Dependency) /= 0 ) call GetInput%AddParameter( Name='m_dependency', Value=This%M_Dependency)
    if ( len_trim(This%M_Transform) > 0 ) call GetInput%AddParameter( Name='m_transform', Value=This%M_Transform )

    call GetInput%AddParameter( Name='gamma', Value=ConvertToString(This%Gam) )
    if ( len_trim(This%Gam_Dependency) /= 0 )call GetInput%AddParameter( Name='gamma_dependency', Value=This%Gam_Dependency )

    call GetInput%AddParameter( Name='tolerance', Value=ConvertToString(This%Tolerance) )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AssembleCov( This, Coordinates, CoordinateLabels, Input, Cov )

    class(CovarianceGExp1L_Type), intent(in)                          ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Coordinates
    type(String_Type), dimension(:), intent(in)                       ::    CoordinateLabels
    type(Input_Type), intent(in)                                      ::    Input
    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Cov

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    LLoc
    real(rkp)                                                         ::    MLoc
    real(rkp)                                                         ::    GamLoc
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    NbNodes
    integer                                                           ::    iCoordinate

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    NbNodes = size(Coordinates,1)

    i = 1
    iCoordinate = 0
    do i = 1, size(Coordinates,2)
      if ( CoordinateLabels(i)%GetValue() == This%CoordinateLabel ) then
        iCoordinate = i
        exit
      end if
    end do

    if ( iCoordinate == 0 ) call Error%Raise( 'Did not find matching coordinate label: ' // This%CoordinateLabel,                 &
                                                                                                               ProcName=ProcName )

    if ( allocated(Cov) ) then
      if ( size(Cov,1) /= size(Cov,2) .or. size(Cov,1) /= NbNodes ) then
        deallocate(Cov, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Cov', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    if ( .not. allocated(Cov) ) then
      allocate(Cov(NbNodes,NbNodes), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Cov', ProcName=ProcName, stat=StatLoc )
    end if
    Cov = Zero

    LLoc = This%L
    if ( len_trim(This%L_Dependency) /= 0 ) call Input%GetValue( Value=LLoc, Label=This%L_Dependency )
    MLoc = This%M
    if ( len_trim(This%M_Dependency) /= 0 ) call Input%GetValue( Value=MLoc, Label=This%M_Dependency )

    if ( len_trim(This%M_Transform) > 0 ) call Transform( Transformation=This%M_Transform, Value=MLoc )

    GamLoc = This%Gam
    if ( len_trim(This%Gam_Dependency) /= 0 ) call Input%GetValue( Value=GamLoc, Label=This%Gam_Dependency )

    if ( GamLoc < Zero ) call Error%Raise( Line='Gamma value below 0', ProcName=ProcName )
    if ( MLoc < Zero ) call Error%Raise( Line='M value below 0', ProcName=ProcName )
    if ( LLoc < Zero ) call Error%Raise( Line='Characteristic length scale value below 0', ProcName=ProcName )

    i = 1
    do i = 1, NbNodes
      ii = 1
      do ii = i, NbNodes
        if ( i == ii ) then
          Cov(ii,ii) = MLoc
          cycle
        end if
        Cov(i,ii) = MLoc * dexp(- (dabs(Coordinates(i,iCoordinate)-Coordinates(ii,iCoordinate))/LLoc)**GamLoc )
        if ( abs(Cov(i,ii) / MLoc) < This%Tolerance ) Cov(i,ii) = Zero
      end do
      Cov(i:NbNodes,i) = Cov(i,i:NbNodes)
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(CovarianceGExp1L_Type), intent(out)                         ::    LHS
    class(CovarianceConstructor_Type), intent(in)                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (CovarianceGExp1L_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%L_Dependency = RHS%L_Dependency
          LHS%L = RHS%L
          LHS%M_Dependency = RHS%M_Dependency
          LHS%M = RHS%M
          LHS%CoordinateLabel = RHS%CoordinateLabel
          LHS%M_Transform = RHS%M_Transform
          LHS%Gam_Dependency = RHS%Gam_Dependency
          LHS%Gam = RHS%Gam
          LHS%Tolerance = RHS%Tolerance
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(CovarianceGExp1L_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
