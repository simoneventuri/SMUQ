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

module CovariancePredefined_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use ArrayIORoutines_Module
use ArrayRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type
use CovarianceConstructor_Class                                   ,only:    CovarianceConstructor_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    CovariancePredefined_Type

type, extends(CovarianceConstructor_Type)                             ::    CovariancePredefined_Type
  real(rkp), allocatable, dimension(:,:)                              ::    PredefinedCov
  real(rkp)                                                           ::    PredefinedScalar
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    AssembleCov
  procedure, public                                                   ::    GetCovariance
  procedure, public                                                   ::    IsStochastic
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(CovariancePredefined_Type), intent(inout)                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'CovariancePredefined'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(CovariancePredefined_Type), intent(inout)                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%PredefinedCov) ) deallocate(This%PredefinedCov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PredefinedCov', ProcName=ProcName, stat=StatLoc )

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(CovariancePredefined_Type), intent(inout)                   ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%PredefinedScalar = One

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(CovariancePredefined_Type), intent(inout)                   ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
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

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'format'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )

    SectionName = 'format'
    select case (LowerCase(VarC0D))
      case( 'source' )
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        call ImportArray( Input=InputSection, Array=This%PredefinedCov, Prefix=PrefixLoc )
        nullify(InputSection)
      case( 'diagonal' )
        call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
        call ImportArray( Input=InputSection, Array=VarR1D, Prefix=PrefixLoc )
        nullify(InputSection)
        allocate(This%PredefinedCov(size(VarR1D,1),size(VarR1D,1)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%PredefinedCov', ProcName=ProcName, stat=StatLoc )
        This%PredefinedCov = Zero
        i = 1
        do i = 1, size(VarR1D,1)
          This%PredefinedCov(i,i) = VarR1D(i)
        end do
        deallocate(VarR1D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
      case ( 'scalar' )
        ParameterName = 'value'
        call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
        This%PredefinedScalar = VarR0D
    end select
    
    nullify(InputSection)

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Scalar, Abscissa, Debug )

    class(CovariancePredefined_Type), intent(inout)                   ::    This
    real(rkp), intent(in)                                             ::    Scalar
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%PredefinedCov(size(Abscissa),size(Abscissa)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%PredefinedCov', ProcName=ProcName, stat=StatLoc )
    This%PredefinedCov = Zero

    call Eye( Array=This%PredefinedCov )
    This%PredefinedCov = This%PredefinedCov * Scalar

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(CovariancePredefined_Type), intent(in)                      ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
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
    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    SectionName = 'format'
    call GetInput%AddSection( SectionName=SectionName )

    if ( allocated(This%PredefinedCov) ) then
      call GetInput%AddParameter( Name='format', Value='source' )
      call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      if ( ExternalFlag ) then
        FileName = DirectoryLoc // '/covariance.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%PredefinedCov, File=File )
      else
        call ExportArray( Input=InputSection, Array=This%PredefinedCov )
      end if
      nullify(InputSection)
    else
      call GetInput%AddParameter( Name='format', Value='scalar' )
      call GetInput%AddParameter( Name='value', Value=ConvertToString(This%PredefinedScalar), SectionName=SectionName )
    end if

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AssembleCov( This, Abscissa, Input, Cov, Debug )

    class(CovariancePredefined_Type), intent(in)                      ::    This
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    type(InputDet_Type), intent(in)                                   ::    Input
    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Cov
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='AssembleCov'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    MLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( allocated(Cov) ) then
      if ( size(Cov,1) /= size(Cov,2) .or. size(Cov,1) /= size(Abscissa,1) ) then
        deallocate(Cov, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Cov', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    if ( .not. allocated(Cov) ) then
      allocate(Cov(size(Abscissa,1),size(Abscissa,1)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Cov', ProcName=ProcName, stat=StatLoc )
    end if
    Cov = Zero

    if ( allocated(This%PredefinedCov) ) then
      if ( size(Abscissa,1) /= size(This%PredefinedCov,1) )                                                                  &
                   call Error%Raise( Line='Incompatible predefined covariance matrix with specified abscissa', ProcName=ProcName )
      Cov = This%PredefinedCov
    else
      call Eye( Array=Cov )
      Cov = Cov * This%PredefinedScalar
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetCovariance( This, Abscissa, Cov, Debug )

    class(CovariancePredefined_Type), intent(in)                      ::    This
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Cov
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCov'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    MLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( allocated(Cov) ) then
      if ( size(Cov,1) /= size(Cov,2) .or. size(Cov,1) /= size(Abscissa,1) ) then
        deallocate(Cov, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Cov', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    if ( .not. allocated(Cov) ) then
      allocate(Cov(size(Abscissa,1),size(Abscissa,1)), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Cov', ProcName=ProcName, stat=StatLoc )
    end if
    Cov = Zero

    if ( allocated(This%PredefinedCov) ) then
      if ( size(Abscissa,1) /= size(This%PredefinedCov,1) )                                                                  &
                   call Error%Raise( Line='Incompatible predefined covariance matrix with specified abscissa', ProcName=ProcName )
      Cov = This%PredefinedCov
    else
      call Eye( Array=Cov )
      Cov = Cov * This%PredefinedScalar
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsStochastic( This, Input, Debug )

    logical                                                           ::    IsStochastic

    class(CovariancePredefined_Type), intent(in)                      ::    This
    class(Input_Type), intent(in)                                     ::    Input
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsStochastic'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (Input)
      type is (InputDet_Type)
        IsStochastic = .false.
      type is (InputStoch_Type)
        IsStochastic = .false.
      class default
        call Error%Raise( Line='Update input type definitions', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(CovariancePredefined_Type), intent(out)                     ::    LHS
    class(CovarianceConstructor_Type), intent(in)                     ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (CovariancePredefined_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%PredefinedScalar = RHS%PredefinedScalar
          if ( allocated(RHS%PredefinedCov) ) then
            allocate(LHS%PredefinedCov, source=RHS%PredefinedCov, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%PredefinedCov', ProcName=ProcName, stat=StatLoc )
          end if
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(CovariancePredefined_Type), intent(inout)                    ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )
  
    if ( allocated(This%PredefinedCov) ) deallocate(This%PredefinedCov, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%PredefinedCov', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------


end module
