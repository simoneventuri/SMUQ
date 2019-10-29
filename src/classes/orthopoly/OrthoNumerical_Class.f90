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
  procedure(Integrand1), pointer                                      ::    PIntegrand1=>null()
  procedure(Integrand2), pointer                                      ::    PIntegrand2=>null()
  procedure(Integrand3), pointer                                      ::    PIntegrand3=>null()
  procedure(Integrand4), pointer                                      ::    PIntegrand4=>null()
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

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
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

      This%PIntegrand1 => Integrand1
      This%PIntegrand2 => Integrand2
      This%PIntegrand3 => Integrand3
      This%PIntegrand4 => Integrand4

    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
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

    if ( associated(This%PIntegrand1) ) nullify(This%PIntegrand1)
    if ( associated(This%PIntegrand2) ) nullify(This%PIntegrand2)
    if ( associated(This%PIntegrand3) ) nullify(This%PIntegrand3)
    if ( associated(This%PIntegrand4) ) nullify(This%PIntegrand4)

    This%Order = -1

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
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
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
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

    SectionName = 'distribution'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call DistProb_Factory%ConstructPointer( Object=This%Weights, Input=InputSection, Prefix=PrefixLoc )

    ParameterName = 'normalized'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Normalized = VarL0D
    
    SectionName = 'precomputed_coefficients'

    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      ParameterName = 'order'
      call Input%GetValue( Value=VarI0D, Parametername=Parametername, SectionName=SectionName, Mandatory=.true. )

      SubSectionName = SectionName // '>alpha'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%Alpha, Prefix=PrefixLoc )
      nullify( InputSection )

      SubSectionName = SectionName // '>beta'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%Beta, Prefix=PrefixLoc )
      nullify( InputSection )

      SubSectionName = SectionName // '>nfactor'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%NFactor, Prefix=PrefixLoc )
      nullify( InputSection )
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
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
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
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

    SectionName = 'distribution'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/distribution'
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
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  ! Returns the value of a polynomial of order 'n' for value of 'x'
  function Eval_N( This, Order, X, Debug )

    real(rkp)                                                         ::    Eval_N

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    real(rkp), intent(in)                                             ::    X
    integer, intent(in)                                               ::    Order
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Eval_N'
    real(rkp)                                                         ::    valnm1
    real(rkp)                                                         ::    valnp0
    real(rkp)                                                         ::    valnp1
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Order < -1 ) call Error%Raise( "An order of below -1 was requested but is not supported" )

    if ( This%Order < Order ) call This%IncreaseOrder( Order=Order )

    if ( Order == -1 ) then
      Eval_N = This%polyorderm1
      if (DebugLoc) call Logger%Exiting()
      return
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
    
    if ( This%Normalized ) Eval_N = Eval_N / This%NFactor( Order+1 )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Eval_MN( This, MinOrder, MaxOrder, X, Debug)

    real(rkp), allocatable, dimension(:)                              ::    Eval_MN

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    real(rkp), intent(in)                                             ::    X
    integer, intent(in)                                               ::    MinOrder
    integer, intent(in)                                               ::    MaxOrder
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Eval_MN'
    real(rkp)                                                         ::    valnm1
    real(rkp)                                                         ::    valnp0
    real(rkp)                                                         ::    valnp1
    integer                                                           ::    i, i_offset, ii, iii
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

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
        Eval_MN(1) = This%Eval( Order=-1, X=X )
        Eval_MN(2) = This%Eval( Order=0, X=X )
        i_offset = 2
      elseif (MinOrder == 0) then
        Eval_MN(1) = This%Eval( Order=0, X=X )
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
          if ( This%Normalized ) Eval_MN(iii) = Eval_MN(iii) / This%NFactor( i+1 )
        else
          ii = ii + 1
        end if

      end do

    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine IncreaseOrder( This, Order, Debug )

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    integer, intent(in)                                               ::    Order
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IncreaseOrder'
    real(rkp), allocatable, dimension(:)                              ::    Alpha
    real(rkp), allocatable, dimension(:)                              ::    Beta
    real(rkp), allocatable, dimension(:)                              ::    NFactor
    real(8)                                                           ::    Num
    real(8)                                                           ::    Den
    real(8)                                                           ::    EpsAbs=0.0
    real(8)                                                           ::    EpsRel=1.E-4
    integer                                                           ::    Key=6
    real(8)                                                           ::    AbsErr
    integer                                                           ::    NEval
    integer                                                           ::    Limit=500
    integer                                                           ::    LenW=4*500
    integer, allocatable, dimension(:)                                ::    iWork
    real(8), allocatable, dimension(:)                                ::    Work
    integer                                                           ::    Last
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Order < Order ) then
      if ( (Order - This%Order ) > 1 ) call This%IncreaseOrder( Order=Order-1 )

      This%Order = This%Order + 1

      allocate( Alpha( This%Order+1 ), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Beta', ProcName=ProcName, stat=StatLoc )
      allocate( Beta( This%Order+1 ), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Beta', ProcName=ProcName, stat=StatLoc )
      allocate( NFactor( This%Order+1 ), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='NFactor', ProcName=ProcName, stat=StatLoc )
      if ( This%Order > 0 ) then
        Alpha = This%Alpha(1:This%Order)
        Beta = This%Beta(1:This%Order)
        NFactor = This%NFactor(1:This%Order)      
      end if

      allocate( iWork(Limit), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='iWork', ProcName=ProcName, stat=StatLoc )
      allocate( Work(LenW), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Work', ProcName=ProcName, stat=StatLoc )

      if ( This%Weights%IsTruncatedLeft() .and. This%Weights%IsTruncatedRight() ) then

        !truncated on both sides
        call dqag( This%PIntegrand1, This%Weights%GetA(), This%Weights%GetB(), EpsAbs, EpsRel, Key, Num, AbsErr, NEval, StatLoc,  &
                   Limit, LenW, Last, iWork, Work )

        call dqag( This%PIntegrand2, This%Weights%GetA(), This%Weights%GetB(), EpsAbs, EpsRel, Key, Den, AbsErr, NEval, StatLoc,  &
                   Limit, LenW, Last, iWork, Work )
        Alpha(This%Order+1) = Num / Den

        if ( Order == 0 ) then
          call dqag( This%PIntegrand4, This%Weights%GetA(), This%Weights%GetB(), EpsAbs, EpsRel, Key, Num, AbsErr, NEval, StatLoc,&
                     Limit, LenW, Last, iWork, Work )
          Beta(1) = Num
          NFactor(1) = Num
        else
          call dqag( This%PIntegrand2, This%Weights%GetA(), This%Weights%GetB(), EpsAbs, EpsRel, Key, Num, AbsErr, NEval, StatLoc,&
                     Limit, LenW, Last, iWork, Work )
          call dqag( This%PIntegrand3, This%Weights%GetA(), This%Weights%GetB(), EpsAbs, EpsRel, Key, Den, AbsErr, NEval, StatLoc,&
                     Limit, LenW, Last, iWork, Work )
          Beta(This%Order+1) = Num / Den
          NFactor(This%Order+1) = product(Beta,1)
        end if


      elseif ( This%Weights%IsTruncatedLeft() ) then

        ! truncated on the left
        call dqagi( This%PIntegrand1, This%Weights%GetA(), 1, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit, LenW, Last,     &
                    iWork, Work )
        call dqagi( This%PIntegrand2, This%Weights%GetA(), 1, EpsAbs, EpsRel, Den, AbsErr, NEval, StatLoc, Limit, LenW, Last,     &
                    iWork, Work )
        Alpha(This%Order+1) = Num / Den

        if ( Order == 0 ) then
          call dqagi( This%PIntegrand4, This%Weights%GetA(), 1, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit,LenW, Last,    &
                      iWork, Work )
          Beta(1) = Num
          NFactor(1) = Num
        else
          call dqagi( This%PIntegrand2, This%Weights%GetA(), 1, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit,LenW, Last,    &
                      iWork, Work )
          call dqagi( This%PIntegrand3, This%Weights%GetA(), 1, EpsAbs, EpsRel, Den, AbsErr, NEval, StatLoc, Limit,LenW, Last,    &
                      iWork, Work )
          Beta(This%Order+1) = Num / Den
          NFactor(This%Order+1) = product(Beta,1)
        end if

      elseif ( This%Weights%IsTruncatedLeft() ) then

        ! truncated on the right
        call dqagi( This%PIntegrand1, This%Weights%GetB(), -1, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit, LenW, Last,    &
                    iWork, Work )
        call dqagi( This%PIntegrand2, This%Weights%GetB(), -1, EpsAbs, EpsRel, Den, AbsErr, NEval, StatLoc, Limit, LenW, Last,    &
                    iWork, Work )
        Alpha(This%Order+1) = Num / Den

        if ( Order == 0 ) then
          call dqagi( This%PIntegrand4, This%Weights%GetB(), -1, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc,Limit,LenW, Last,    &
                      iWork, Work )
          Beta(1) = Num
          NFactor(1) = Num
        else
          call dqagi( This%PIntegrand2, This%Weights%GetB(), -1, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc,Limit,LenW, Last,    &
                      iWork, Work )
          call dqagi( This%PIntegrand3, This%Weights%GetB(), -1, EpsAbs, EpsRel, Den, AbsErr, NEval, StatLoc,Limit,LenW, Last,    &
                      iWork, Work )
          Beta(This%Order+1) = Num / Den
          NFactor(This%Order+1) = product(Beta,1)
        end if

      else

        ! infiinite on both bounds
        call dqagi( This%PIntegrand1, This%Weights%GetB(), 2, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit, LenW, Last,     &
                    iWork, Work )
        call dqagi( This%PIntegrand2, This%Weights%GetB(), 2, EpsAbs, EpsRel, Den, AbsErr, NEval, StatLoc, Limit, LenW, Last,     &
                    iWork, Work )
        Alpha(This%Order+1) = Num / Den

        if ( Order == 0 ) then
          call dqagi( This%PIntegrand4, This%Weights%GetB(), 2, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit,LenW, Last,    &
                      iWork, Work )
          Beta(1) = Num
          NFactor(1) = Num
        else
          call dqagi( This%PIntegrand2, This%Weights%GetB(), 2, EpsAbs, EpsRel, Num, AbsErr, NEval, StatLoc, Limit,LenW, Last,    &
                      iWork, Work )
          call dqagi( This%PIntegrand3, This%Weights%GetB(), 2, EpsAbs, EpsRel, Den, AbsErr, NEval, StatLoc, Limit,LenW, Last,    &
                      iWork, Work )
          Beta(This%Order+1) = Num / Den
          NFactor(This%Order+1) = product(Beta,1)
        end if

      end if

      if ( allocated(This%Alpha) ) deallocate(This%Alpha, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Alpha', ProcName=ProcName, stat=StatLoc )
      if ( allocated(This%Beta) ) deallocate(This%Beta, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Beta', ProcName=ProcName, stat=StatLoc )
      if ( allocated(This%NFactor) ) deallocate(This%NFactor, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%NFactor', ProcName=ProcName, stat=StatLoc )

      call move_alloc( from=Alpha, to=This%Alpha )
      call move_alloc( from=Beta, to=This%Beta )
      call move_alloc( from=NFactor, to=This%NFactor )

      deallocate(iWork, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='iWork', ProcName=ProcName, stat=StatLoc )
      deallocate(Work, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='Work', ProcName=ProcName, stat=StatLoc )

    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Integrand1( This, X, Debug )  
    
    real(8)                                                           ::    Integrand1

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    real(8), intent(in)                                               ::    X
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Integrand1'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    Integrand1 = X * This%Eval( Order=This%Order, X=X )**2 * This%Weights%PDF( X=X )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Integrand2( This, X, Debug )  
    
    real(8)                                                           ::    Integrand2

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    real(8), intent(in)                                               ::    X
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Integrand2'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    Integrand2 = This%Eval( Order=This%Order, X=X )**2 * This%Weights%PDF( X=X )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Integrand3( This, X, Debug )  
    
    real(8)                                                           ::    Integrand3

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    real(8), intent(in)                                               ::    X
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Integrand3'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    Integrand3 = This%Eval( Order=This%Order-1, X=X )**2 * This%Weights%PDF( X=X )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Integrand4( This, X, Debug )  
    
    real(8)                                                           ::    Integrand4

    class(OrthoNumerical_Type), intent(inout)                         ::    This
    real(8), intent(in)                                               ::    X
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Integrand4'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    Integrand4 = This%Weights%PDF( X=X )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
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
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
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
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
