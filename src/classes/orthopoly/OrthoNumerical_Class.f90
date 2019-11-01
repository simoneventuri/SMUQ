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
module OrthoNumerical_Class

use QuadPack_Library
use Parameters_Library
use ArrayRoutines_Module
use Input_Library
use ArrayIORoutines_Module
use CommandRoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Factory_Class                                        ,only:    DistProb_Factory
use OrthoPoly_Class                                               ,only:    OrthoPoly_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    OrthoNumerical_Type

type, extends(OrthoPoly_Type)                                         ::    OrthoNumerical_Type
  class(DistProb_Type), pointer                                       ::    Weights=>null()
  real(rkp), allocatable, dimension(:)                                ::    Alpha
  real(rkp), allocatable, dimension(:)                                ::    Beta
  real(rkp), allocatable, dimension(:)                                ::    NFactor
  integer                                                             ::    Order=-1
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Eval_N
  procedure, public                                                   ::    Eval_MN
  procedure, private                                                  ::    IncreaseOrder
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type


abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  function Integrand( X )  
    real(8)                                                           ::    Integrand
    real(8), intent(in)                                               ::    X
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )
    class(OrthoNumerical_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name         =   'numerical'
      This%Initialized  =   .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized = .false.
    This%Constructed = .false.

    if ( allocated(This%Alpha) ) deallocate(This%Alpha, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Alpha', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Beta) ) deallocate(This%Beta, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Beta', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%NFactor) ) deallocate(This%NFactor, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%NFactor', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Weights) ) deallocate( This%Weights, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Weights', ProcName=ProcName, stat=StatLoc )

    This%Order = -1

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This, Debug)

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )
   
    This%Normalized = .false.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    Found
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'weights'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call DistProb_Factory%ConstructPointer( Object=This%Weights, Input=InputSection, Prefix=PrefixLoc )

    ParameterName = 'normalized'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Normalized = VarL0D

    SectionName = 'precomputed_coefficients'

    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      ParameterName = 'order'
      call Input%GetValue( Value=VarI0D, Parametername=Parametername, SectionName=SectionName, Mandatory=.true. )
      This%Order = VarI0D

      SubSectionName = SectionName // '>alpha'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%Alpha, Prefix=PrefixLoc )
      nullify( InputSection )
      if ( size(This%Alpha) /= This%Order ) call Error%Raise( "Order and number of alpha coefficients do not match" )

      SubSectionName = SectionName // '>beta'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%Beta, Prefix=PrefixLoc )
      nullify( InputSection )
      if ( size(This%Beta) /= This%Order ) call Error%Raise( "Order and number of beta coefficients do not match" )

      SubSectionName = SectionName // '>nfactor'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%NFactor, Prefix=PrefixLoc )
      nullify( InputSection )
      if ( size(This%NFactor) /= This%Order ) call Error%Raise( "Order and number of normalization factors do not match" )
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Weights, Normalized, Debug )

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    class(DistProb_Type), intent(in)                                  ::    Weights
    logical, optional ,intent(in)                                     ::    Normalized
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%Weights, source=Weights, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Weights', ProcName=ProcName, stat=StatLoc )

    if ( present(Normalized) ) This%Normalized = Normalized

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(OrthoNumerical_Type), intent(in)                            ::    This
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
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File

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

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )   
 
    call GetInput%AddParameter( Name='normalized', Value=ConvertToString(This%Normalized) )

    SectionName = 'weights'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/weights'
    call GetInput%AddSection( Section=DistProb_Factory%GetObjectInput( Object=This%Weights ,MainSectionName=SectionName,          &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub ) )      

    if ( This%Order > 0 ) then
      SectionName = 'precomputed_coefficients'
      call GetInput%AddSection( SectionName=SectionName )
      if ( ExternalFlag ) then

        call GetInput%AddParameter( Name='order', Value=ConvertToString( Value=This%Order ), SectionName=SectionName )

        SubSectionName = 'alpha'
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        FileName = DirectoryLoc // '/alpha.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%Alpha, File=File )
        nullify(InputSection)

        SubSectionName = 'beta'
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        FileName = DirectoryLoc // '/beta.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%Beta, File=File )
        nullify(InputSection)

        SubSectionName = 'nfactor'
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        FileName = DirectoryLoc // '/nfactor.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Input=InputSection, Array=This%NFactor, File=File )
        nullify(InputSection)
      else
        SubSectionName = 'alpha'
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ExportArray( Input=InputSection, Array=This%Alpha )
        nullify(InputSection)

        SubSectionName = 'beta'
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ExportArray( Input=InputSection, Array=This%Beta )
        nullify(InputSection)

        SubSectionName = 'nfactor'
        call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
        SubSectionName = SectionName // '>' // SubSectionName
        call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
        call ExportArray( Input=InputSection, Array=This%NFactor )
        nullify(InputSection)

      end if

    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Returns the value of a polynomial of order 'n' for value of 'x'
  function Eval_N( This, Order, X, Normalized, Debug )

    real(rkp)                                                         ::    Eval_N

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    real(rkp), intent(in)                                             ::    X
    integer, intent(in)                                               ::    Order
    logical, optional, intent(in)                                     ::    Normalized
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Eval_N'
    real(rkp)                                                         ::    valnm1
    real(rkp)                                                         ::    valnp0
    real(rkp)                                                         ::    valnp1
    integer                                                           ::    i
    logical                                                           ::    NormalizedLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    NormalizedLoc = This%Normalized
    if ( present(Normalized) ) NormalizedLoc = Normalized

    if ( Order < -1 ) call Error%Raise( "An order of below -1 was requested but is not supported" )

    if ( This%Order < Order ) call This%IncreaseOrder( Order=Order )

    if ( Order == -1 ) then
      Eval_N = This%polyorderm1
    elseif ( Order == 0 ) then
      Eval_N = This%polyorder0
    else
      valnm1 = This%polyorderm1
      valnp0 = This%polyorder0
      
      i = 1
      do i = 1, Order
        valnp1 = (X-This%Alpha(i))*valnp0-This%Beta(i)*valnm1
        valnm1 = valnp0
        valnp0 = valnp1
      end do
      Eval_N = valnp1
    end if

    if ( NormalizedLoc .and. Order > -1 ) then
      Eval_N = Eval_N / This%NFactor(Order+1)
    end if 

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Eval_MN( This, MinOrder, MaxOrder, X, Normalized, Debug)

    real(rkp), allocatable, dimension(:)                              ::    Eval_MN

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    real(rkp), intent(in)                                             ::    X
    integer, intent(in)                                               ::    MinOrder
    integer, intent(in)                                               ::    MaxOrder
    logical, optional, intent(in)                                     ::    Normalized
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Eval_MN'
    real(rkp)                                                         ::    valnm1
    real(rkp)                                                         ::    valnp0
    real(rkp)                                                         ::    valnp1
    integer                                                           ::    i, i_offset, ii, iii
    integer                                                           ::    StatLoc=0
    logical                                                           ::    NormalizedLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    NormalizedLoc = This%Normalized
    if ( present(Normalized) ) NormalizedLoc = Normalized

    if ( MinOrder < -1 ) call Error%Raise( "A starting order of below -1 was requested but is not supported" )
    if ( MinOrder > MaxOrder ) call Error%Raise( "Starting order was specified to be larger than the final order" )

    allocate(Eval_MN(MaxOrder-MinOrder+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Eval_MN', ProcName=ProcName, stat=StatLoc )

    if ( MaxOrder > This%Order ) call This%IncreaseOrder( Order=MaxOrder )

    if ( MinOrder==MaxOrder ) then
      Eval_MN(1) = This%Eval( Order=MinOrder, X=X )
    else
      i_offset = 0
      if ( MinOrder == -1 )  then
        Eval_MN(1) = This%Eval( Order=-1, X=X, Normalized=NormalizedLoc )
        Eval_MN(2) = This%Eval( Order=0, X=X, Normalized=NormalizedLoc )
        i_offset = 2
      elseif (MinOrder == 0) then
        Eval_MN(1) = This%Eval( Order=0, X=X, Normalized=NormalizedLoc )
        i_offset = 1
      end if
      valnm1 = This%polyorderm1
      valnp0 = This%polyorder0

      i = 1
      ii = 0
      iii = 0
      do i = 1, MaxOrder
        valnp1 = (X-This%Alpha(i))*valnp0-This%Beta(i)*valnm1
        valnm1 = valnp0
        valnp0 = valnp1
        if ( i >= MinOrder ) then
          iii = i+i_offset-ii
          Eval_MN(iii) = valnp1
          if ( NormalizedLoc ) Eval_MN(iii) = Eval_MN(iii) / This%NFactor( i+1 )
        else
          ii = ii + 1
        end if

      end do

    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine IncreaseOrder( This, Order, Debug )

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    integer, intent(in)                                               ::    Order
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IncreaseOrder'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    Alpha
    real(rkp), allocatable, dimension(:)                              ::    Beta
    real(rkp), allocatable, dimension(:)                              ::    NFactor
    real(8)                                                           ::    Num
    real(8)                                                           ::    EpsAbs
    real(8)                                                           ::    EpsRel
    integer                                                           ::    Key
    real(8)                                                           ::    AbsErr
    integer                                                           ::    NEval
    integer                                                           ::    Limit
    integer                                                           ::    LenW
    integer, allocatable, dimension(:)                                ::    iWork
    real(8), allocatable, dimension(:)                                ::    Work
    integer                                                           ::    Last
    integer                                                           ::    OrderLoc
    integer                                                           ::    i
    integer                                                           ::    i_start
    procedure(Integrand), pointer                                     ::    PIntegrand1=>null()
    procedure(Integrand), pointer                                     ::    PIntegrand2=>null()
    real(8)                                                           ::    iNFactor
    real(8)                                                           ::    A
    real(8)                                                           ::    B

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    StatLoc = 0

    if ( This%Order < Order ) then

      AbsErr = Zero
      NEval=0
      EpsAbs = 0.0
      EpsRel = 1.d-3
      Key = 6
      Limit = 500
      LenW = 4*Limit

      if ( This%Weights%IsTruncatedLeft() ) A = This%Weights%GetA()
      if ( This%Weights%IsTruncatedRight() ) B = This%Weights%GetB()

      PIntegrand1 => Integrand1
      PIntegrand2 => Integrand2

      if ( This%Order > -1 ) then
        allocate(Alpha, source=This%Alpha, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Alpha', ProcName=ProcName, stat=StatLoc )
        allocate(Beta, source=This%Beta, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Beta', ProcName=ProcName, stat=StatLoc )
        allocate(NFactor, source=This%NFactor, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='NFactor', ProcName=ProcName, stat=StatLoc )

        deallocate(This%Alpha, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Alpha', ProcName=ProcName, stat=StatLoc )
        deallocate(This%Beta, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Beta', ProcName=ProcName, stat=StatLoc )
        deallocate(This%NFactor, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%NFactor', ProcName=ProcName, stat=StatLoc )

        allocate(This%Alpha(Order+1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Alpha', ProcName=ProcName, stat=StatLoc )
        allocate(This%Beta(Order+1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Beta', ProcName=ProcName, stat=StatLoc )
        allocate(This%NFactor(Order+1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%NFactor', ProcName=ProcName, stat=StatLoc )
        This%Alpha(1:This%Order+1) = Alpha
        This%Beta(1:This%Order+1) = Beta
        This%NFactor(1:This%Order+1) = NFactor

        deallocate(Alpha, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Alpha', ProcName=ProcName, stat=StatLoc )
        deallocate(Beta, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Beta', ProcName=ProcName, stat=StatLoc )
        deallocate(NFactor, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='NFactor', ProcName=ProcName, stat=StatLoc )
      else
        allocate(This%Alpha(Order+1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Alpha', ProcName=ProcName, stat=StatLoc )
        allocate(This%Beta(Order+1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='THis%Beta', ProcName=ProcName, stat=StatLoc )
        allocate(This%NFactor(Order+1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%NFactor', ProcName=ProcName, stat=StatLoc )
        This%Alpha = Zero
        This%Beta = Zero
        This%NFactor = Zero
      end if

      allocate( iWork(Limit), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='iWork', ProcName=ProcName, stat=StatLoc )
      allocate( Work(LenW), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Work', ProcName=ProcName, stat=StatLoc )

      i_start = This%Order+1
      do i = i_start, Order
        This%Order = i

        iNFactor = Zero
        if ( This%Weights%IsTruncatedLeft() .and. This%Weights%IsTruncatedRight() ) then
          call dqag( PIntegrand2, A, B, EpsAbs, EpsRel, Key, iNFactor, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work )
          if ( StatLoc /= 0 .and. StatLoc /= 2 ) call Error%Raise( "Dqag exited with non zero exit code : " //                    &
                                                                                                  ConvertToString(Value=StatLoc) )
        elseif ( This%Weights%IsTruncatedLeft() ) then
          call dqagi( PIntegrand2, A, 1, EpsAbs, EpsRel, iNFactor, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work )
          if ( StatLoc /= 0 .and. StatLoc /= 2 ) call Error%Raise( "Dqagi exited with non zero exit code : " //                   &
                                                                                                  ConvertToString(Value=StatLoc) )
        elseif ( This%Weights%IsTruncatedRight() ) then
          call dqagi( PIntegrand2, B, -1, EpsAbs, EpsRel, iNFactor, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work )
          if ( StatLoc /= 0 .and. StatLoc /= 2 ) call Error%Raise( "Dqagi exited with non zero exit code : " //                   &
                                                                                                  ConvertToString(Value=StatLoc) )
        else
          call dqagi( PIntegrand2, Zero, 2, EpsAbs, EpsRel, iNFactor, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work )
          if ( StatLoc /= 0 .and. StatLoc /= 2 ) call Error%Raise( "Dqagi exited with non zero exit code : " //                   &
                                                                                                  ConvertToString(Value=StatLoc) )
        end if

        if ( iNFactor /= iNFactor ) call Error%Raise( "Detected NaN answer from integrator at polynomial order " //               &
                                                                                     ConvertToString(Value=i), ProcName=ProcName )

        This%NFactor(i+1) = dsqrt(iNFactor)

        Num = Zero
        if ( This%Weights%IsTruncatedLeft() .and. This%Weights%IsTruncatedRight() ) then
          !truncated on both sides
          call dqag( PIntegrand1, A, B, EpsAbs, EpsRel, Key, Num, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work )
          if ( StatLoc /= 0 .and. StatLoc /= 2 ) call Error%Raise( "Dqag exited with non zero exit code : " //                    &
                                                                                                  ConvertToString(Value=StatLoc) )
        elseif ( This%Weights%IsTruncatedLeft() ) then
          ! truncated on the left
          call dqagi( PIntegrand1, A, 1, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work )
          if ( StatLoc /= 0 .and. StatLoc /= 2 ) call Error%Raise( "Dqagi exited with non zero exit code : " //                   &
                                                                                                  ConvertToString(Value=StatLoc) )
        elseif ( This%Weights%IsTruncatedRight() ) then
          ! truncated on the right
          call dqagi( PIntegrand1, B, -1, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work )
          if ( StatLoc /= 0 .and. StatLoc /= 2 ) call Error%Raise( "Dqagi exited with non zero exit code : " //                   &
                                                                                                  ConvertToString(Value=StatLoc) )
        else
          ! infiinite on both bounds
          call dqagi( PIntegrand1, Zero, 2, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work )
          if ( StatLoc /= 0 .and. StatLoc /= 2 ) call Error%Raise( "Dqagi exited with non zero exit code : " //                   &
                                                                                                  ConvertToString(Value=StatLoc) )
        end if

        if ( Num /= Num ) call Error%Raise( "Detected NaN answer from integrator at polynomial order " //                         &
                                                                                     ConvertToString(Value=i), ProcName=ProcName )

        if ( i > 0 ) then
          This%Alpha(i+1) = Num / This%NFactor(i+1)**2
          This%Beta(i+1) = ( This%NFactor(i+1) / This%NFactor(i) )**2
        else
          This%Alpha(1) = Num / This%NFactor(1)**2
          This%Beta(1) = This%NFactor(1)**2
        end if
      end do

      deallocate(iWork, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='iWork', ProcName=ProcName, stat=StatLoc )
      deallocate(Work, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='Work', ProcName=ProcName, stat=StatLoc )

      nullify(PIntegrand1)
      nullify(PIntegrand2)

    end if

    if (DebugLoc) call Logger%Exiting()

    contains

      !!--------------------------------------------------------------------------------------------------------------------------
      function Integrand1( X )  
        
        real(8)                                                           ::    Integrand1
        real(8), intent(in)                                               ::    X

        Integrand1 = X * This%Eval( Order=i, X=X, Normalized=.false. )**2 * This%Weights%PDF( X=X )

      end function
      !!--------------------------------------------------------------------------------------------------------------------------

      !!--------------------------------------------------------------------------------------------------------------------------
      function Integrand2( X )  
        
        real(8)                                                           ::    Integrand2
        real(8), intent(in)                                               ::    X

        Integrand2 = This%Eval( Order=i, X=X, Normalized=.false. )**2 * This%Weights%PDF( X=X )

      end function
      !!--------------------------------------------------------------------------------------------------------------------------

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(OrthoNumerical_Type), intent(out)                           ::    LHS
    class(OrthoPoly_Type), intent(in)                                 ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (OrthoNumerical_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Normalized = RHS%Normalized
          allocate(LHS%Weights, source=RHS%Weights, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Weights', ProcName=ProcName, stat=StatLoc )

          if ( RHS%Order > 0 ) then
            allocate( LHS%Alpha, source=RHS%Alpha, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Alpha', ProcName=ProcName, stat=StatLoc )
            allocate( LHS%Beta, source=RHS%Beta, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Beta', ProcName=ProcName, stat=StatLoc )
            allocate( LHS%NFactor, source=RHS%NFactor, stat=StatLoc )
            if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%NFactor', ProcName=ProcName, stat=StatLoc )
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

    type(OrthoNumerical_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%Alpha) ) deallocate(This%Alpha, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Alpha', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Beta) ) deallocate(This%Beta, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Beta', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%NFactor) ) deallocate(This%NFactor, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%NFactor', ProcName=ProcName, stat=StatLoc )

    if ( associated(This%Weights) ) deallocate( This%Weights, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Weights', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
