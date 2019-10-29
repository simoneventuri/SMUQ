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
! X~U(-1,1)
module OrthoLegendre_Class

use Input_Library
use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error
use OrthoPoly_class
use ComputingRoutines_Module
use Parameters_Library

implicit none

private

public                                                                ::    OrthoLegendre_Type

type, public, extends(OrthoPoly_Type)                                 ::    OrthoLegendre_Type
  integer                                                             ::    Normalization=0
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
  procedure, nopass, public                                           ::    NFactor
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This, Debug )
    class(OrthoLegendre_Type), intent(inout)                          ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Write( "Initializing Legendre orthogonal polynomial object" )

    if ( .not. This%Initialized ) then
      This%Name = 'legendre'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Write( "Initialization Successful" )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This, Debug )

    class(OrthoLegendre_Type), intent(inout)                          ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults(This, Debug)

    class(OrthoLegendre_Type), intent(inout)                          ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Normalized = .false.
    This%Normalization = 0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput( This, Input, Prefix, Debug )

    use String_Library

    class(OrthoLegendre_Type), intent(inout)                          ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    logical                                                           ::    Found
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'normalization'
    call Input%GetValue( value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      select case (LowerCase(VarC0D))
        case ('none')
          This%Normalization = 0
          This%Normalized = .false.
        case ('standard')
          This%Normalization = 1
          This%Normalized = .true.
        case ('uniform')
          This%Normalization = 2
          This%Normalized = .true.
        case default
          call Error%Raise( Line='Unrecognized normalization option', ProcName=ProcName )
      end select
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1( This, Normalization, Debug )
    
    class(OrthoLegendre_Type), intent(inout)                          ::    This
    integer, optional, intent(in)                                     ::    Normalization
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(Normalization) ) then
      if ( Normalization > 2 .or. Normalization < 0 ) call Error%Raise( Line='Invalid normalization option', ProcName=ProcName )
      This%Normalization = Normalization
      if ( Normalization /= 0 ) This%Normalized=.true.
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use String_Library

    type(InputSection_Type)                                           ::    GetInput
    class(OrthoLegendre_Type), intent(in)                             ::    This
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
    character(:), allocatable                                         ::    SectionName

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    select case (This%Normalization)
      case(0)
        call GetInput%AddParameter( Name='normalization', Value='none' )
      case(1)
        call GetInput%AddParameter( Name='normalization', Value='standard' )
      case(2)
        call GetInput%AddParameter( Name='normalization', Value='uniform' )
      case default
        call Error%Raise( Line='Unrecognized normalization option', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  ! Returns the value of a polynomial of order 'n' for value of 'x'
  function Eval_N( This, Order, X, Debug )

    real(rkp)                                                         ::    Eval_N

    class(OrthoLegendre_Type), intent(inout)                          ::    This
    real(rkp), intent(in)                                             ::    X
    integer, intent(in)                                               ::    Order
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Eval_N'
    real(rkp)                                                         ::    valnm1
    real(rkp)                                                         ::    valnp0
    real(rkp)                                                         ::    valnp1
    real(rkp)                                                         ::    nt
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( X < -One ) call Error%Raise( Line='X argument below allowable minimum of -1', ProcName=ProcName )
    if ( X > One ) call Error%Raise( Line='X argument above allowable maximum of 1', ProcName=ProcName )

    if ( Order < -1 ) call Error%Raise( "An order of below -1 was requested but is not supported" )

    if ( Order == -1 ) then
      Eval_N = This%polyorderm1
    elseif ( Order == 0 ) then
      Eval_N = This%polyorder0
    else
      valnm1 = This%polyorderm1
      valnp0 = This%polyorder0
      i = 1
      do i = 1, Order
        nt = real(i-1,rkp)
        valnp1 = ((Two*nt+One)*X*valnp0-nt*valnm1)/(nt+One)
        valnm1 = valnp0
        valnp0 = valnp1
      end do
      Eval_N = valnp1
    end if

    if ( This%Normalized ) Eval_N = Eval_N * This%NFactor( Order=Order, Normalization=This%Normalization )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  ! Returns the values of a polynomials of order 'm' to 'n' for value of 'x'
  function Eval_MN( This, MinOrder, MaxOrder, X, Debug)

    real(rkp), dimension(:), allocatable                              ::    Eval_MN

    class(OrthoLegendre_Type), intent(inout)                          ::    This
    real(rkp), intent(in)                                             ::    X
    integer, intent(in)                                               ::    MinOrder
    integer, intent(in)                                               ::    MaxOrder
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Eval_MN'
    real(rkp)                                                         ::    valnm1
    real(rkp)                                                         ::    valnp0
    real(rkp)                                                         ::    valnp1
    real(rkp)                                                         ::    nt
    integer                                                           ::    i, i_offset, ii
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( X < -One ) call Error%Raise( Line='X argument below allowable minimum of -1', ProcName=ProcName )
    if ( X > One ) call Error%Raise( Line='X argument above allowable maximum of 1', ProcName=ProcName )

    if ( MinOrder < -1 ) call Error%Raise( "A starting order of below -1 was requested but is not supported" )
    if ( MinOrder > MaxOrder ) call Error%Raise( "Starting order was specified to be larger than the final order" )

    allocate(Eval_MN(MaxOrder-MinOrder+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Eval_MN', ProcName=ProcName, stat=StatLoc )

    if ( MinOrder == MaxOrder ) then
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
      i = 1
      valnm1 = This%polyorderm1
      valnp0 = This%polyorder0
      ii = 0
      do i = 1, MaxOrder
        nt = real(i-1,rkp)
        valnp1 = ((Two*nt+One)*X*valnp0-nt*valnm1)/(nt+One)
        valnm1 = valnp0
        valnp0 = valnp1
        if ( i >= MinOrder ) then
          Eval_MN(i+i_offset-ii) = valnp1
          if ( This%Normalized ) Eval_MN(i+i_offset-ii) = Eval_MN(i+i_offset-ii) *                                                &
                                                                         This%NFactor( Order=i, Normalization=This%Normalization )
        else
          ii = ii + 1
        end if

      end do

    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function NFactor( Order, Normalization, Debug )

    real(rkp)                                                         ::    NFactor

    integer, intent(in)                                               ::    Order
    integer, intent(in)                                               ::    Normalization
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='NFactor'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NFactor = dsqrt( Two*real(Order,rkp)+One )

    select case (Normalization)
      case (0)
        NFactor = One
      case (1)
        NFactor = dsqrt( (Two*real(Order,rkp)+One) / Two )
      case (2)
        NFactor = dsqrt( Two*real(Order,rkp)+One )
      case default
        call Error%Raise( Line='Invalid normalization option', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(OrthoLegendre_Type), intent(out)                            ::    LHS
    class(OrthoPoly_Type), intent(in)                                 ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (OrthoLegendre_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          LHS%Normalized = RHS%Normalized
          LHS%Normalization = RHS%Normalization
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(OrthoLegendre_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
