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

module LowDiscSobol_Class

use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error
use LowDiscSequence_Class                                         ,only:  LowDiscSequence_Type
use Sobol_Library

implicit none

private

public                                                                ::    LowDiscSobol_Type

type, extends(LowDiscSequence_Type)                                   ::    LowDiscSobol_Type
  integer                                                             ::    Skip=0
  integer                                                             ::    Leap=0
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1      
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Get_0D
  procedure, public                                                   ::    Get_1D
  procedure, public                                                   ::    GetPoint_0D
  procedure, public                                                   ::    GetPoint_1D
  procedure, public                                                   ::    GetPoints_0D
  procedure, public                                                   ::    GetPoints_1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This, Debug )

    class(LowDiscSobol_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) then
      This%Name = 'lowdiscsobol'
      This%Initialized = .True.
    end if

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This, Debug )

    class(LowDiscSobol_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This, Debug )

    class(LowDiscSobol_Type), intent(inout)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Skip = 0
    This%Leap = 0

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput( This, Input, Prefix, Debug )

    use StringRoutines_Module

    class(LowDiscSobol_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ProcessInput'
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    if (DebugLoc) call Logger%Write( "Processing passed down settings")

    ParameterName = 'skip'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%Skip = VarI0D
      if ( This%Skip < 0 ) call Error%Raise( Line="Step < 0 was specified, please supply a value at or above 0",                  &
                                                                                                               ProcName=ProcName )
    end if

    ParameterName = 'leap'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%Leap = VarI0D
      if ( This%Leap < 0 ) call Error%Raise( Line="Leap < 0 was specified, please supply a value at or above 0",                  &
                                                                                                               ProcName=ProcName )
    end if

    if (DebugLoc) call Logger%Write( "Done processing passed down settings")

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine 
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1 ( This, Skip, Leap, Debug )

    class(LowDiscSobol_Type), intent(inout)                           ::    This
    integer, optional, intent(in)                                     ::    Skip
    integer, optional, intent(in)                                     ::    Leap
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( present(Skip) ) then
      This%Skip = Skip 
      if ( This%Skip < 0 ) call Error%Raise( Line="Step < 0 was specified, please supply a value at or above 0",                  &
                                                                                                               ProcName=ProcName )
    end if

    if ( present(Leap) ) then
      This%Leap = Leap 
      if ( This%Skip < 0 ) call Error%Raise( Line="Step < 0 was specified, please supply a value at or above 0",                  &
                                                                                                               ProcName=ProcName )
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine 
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(LowDiscSobol_Type), intent(in)                              ::    This
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
    character(:), allocatable                                         ::    SubSectionName
    character(100)                                                    ::    VarC0D

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

    call GetInput%AddParameter( Name='skip', Value=ConvertToString( Value=This%Skip ) )
    call GetInput%AddParameter( Name='leap', Value=ConvertToString( Value=This%Leap ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Get_0D( This, NbPoints, Debug )

    real(rkp), allocatable, dimension(:)                              ::    Get_0D

    class(LowDiscSobol_Type), intent(in)                              ::    This
    integer, intent(in)                                               ::    NbPoints 
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Get_0D'
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:), allocatable                              ::    SeqVal
    integer(8)                                                        ::    Step
    integer(8)                                                        ::    SkipLoc
    integer(8)                                                        ::    LeapLoc
    integer(8)                                                        ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( NbPoints <= 0 ) call Error%Raise( Line='Requested 0 or less points from the sequence', ProcName=ProcName )

    allocate(Get_0D(NbPoints), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get_0D', ProcName=ProcName, stat=StatLoc )

    allocate( SeqVal(1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )
    SeqVal = Zero

    SkipLoc = int(This%Skip,8)
    LeapLoc = int(This%Leap,8) + 1

    do i = 1, NbPoints
      Step = SkipLoc + LeapLoc*(i-1) + 1
      call i8_sobol( int(1,8), Step, SeqVal)
      Get_0D(i) = SeqVal(1)
    end do

    deallocate(SeqVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Get_1D( This, NbPoints, NbDim, Debug )

    real(rkp), allocatable, dimension(:,:)                            ::    Get_1D

    class(LowDiscSobol_Type), intent(in)                              ::    This
    integer, intent(in)                                               ::    NbPoints
    integer, intent(in)                                               ::    NbDim
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Get_1D'
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:), allocatable                              ::    SeqVal
    integer(8)                                                        ::    Step
    integer(8)                                                        ::    SkipLoc
    integer(8)                                                        ::    LeapLoc
    integer(8)                                                        ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( NbPoints <= 0 ) call Error%Raise( Line='Requested 0 or less points from the sequence', ProcName=ProcName )

    if ( NbDim <= 0 ) call Error%Raise( Line='Requested sequence of dimension 0 or less', ProcName=ProcName )

    allocate(Get_1D(NbDim, NbPoints), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get_0D', ProcName=ProcName, stat=StatLoc )

    allocate( SeqVal(NbDim), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )
    SeqVal = Zero

    SkipLoc = int(This%Skip,8)
    LeapLoc = int(This%Leap,8) + 1

    do i = 1, NbPoints
      Step = SkipLoc + LeapLoc*(i-1) + 1
      call i8_sobol( int(NbDim,8), Step, SeqVal)
      Get_1D(:,i) = SeqVal(:)
    end do

    deallocate(SeqVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPoint_0D( This, Point, Debug )

    real(rkp)                                                         ::    GetPoint_0D

    class(LowDiscSobol_Type), intent(in)                              ::    This
    integer, intent(in)                                               ::    Point 
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetPoint_0D'
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:), allocatable                              ::    SeqVal
    integer(8)                                                        ::    Step
    integer(8)                                                        ::    SkipLoc
    integer(8)                                                        ::    LeapLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( Point <= 0 ) call Error%Raise( Line='Requested point 0 or smaller from the sequence', ProcName=ProcName )

    allocate( SeqVal(1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )
    SeqVal = Zero

    SkipLoc = int(This%Skip,8)
    LeapLoc = int(This%Leap,8) + 1

    Step = SkipLoc + LeapLoc*(int(Point,8)-1) + 1
    call i8_sobol( int(1,8), Step, SeqVal)
    GetPoint_0D = SeqVal(1)

    deallocate(SeqVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPoint_1D( This, Point, NbDim, Debug )

    real(rkp), allocatable, dimension(:)                              ::    GetPoint_1D

    class(LowDiscSobol_Type), intent(in)                              ::    This
    integer, intent(in)                                               ::    Point
    integer, intent(in)                                               ::    NbDim
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetPoint_1D'
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:), allocatable                              ::    SeqVal
    integer(8)                                                        ::    Step
    integer(8)                                                        ::    SkipLoc
    integer(8)                                                        ::    LeapLoc
    integer(8)                                                        ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( Point <= 0 ) call Error%Raise( Line='Requested point 0 or smaller from the sequence', ProcName=ProcName )

    if ( NbDim <= 0 ) call Error%Raise( Line='Requested sequence of dimension 0 or less', ProcName=ProcName )

    allocate(GetPoint_1D(NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetPoint_1D', ProcName=ProcName, stat=StatLoc )

    allocate( SeqVal(NbDim), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )
    SeqVal = Zero

    SkipLoc = int(This%Skip,8)
    LeapLoc = int(This%Leap,8) + 1

    Step = SkipLoc + LeapLoc*(int(Point,8)-1) + 1
    call i8_sobol( int(NbDim,8), Step, SeqVal)
    GetPoint_1D = SeqVal

    deallocate(SeqVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPoints_0D( This, SeqStart, SeqEnd, Debug )

    real(rkp), allocatable, dimension(:)                              ::    GetPoints_0D

    class(LowDiscSobol_Type), intent(in)                              ::    This
    integer, intent(in)                                               ::    SeqStart
    integer, intent(in)                                               ::    SeqEnd
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetPoints_0D'
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:), allocatable                              ::    SeqVal
    integer(8)                                                        ::    Step
    integer(8)                                                        ::    SkipLoc
    integer(8)                                                        ::    LeapLoc
    integer(8)                                                        ::    SeqStartLoc
    integer(8)                                                        ::    SeqEndLoc
    integer(8)                                                        ::    i
    integer                                                           ::    NbPoints

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    NbPoints = SeqEnd - SeqStart + 1

    if ( NbPoints <= 0 ) call Error%Raise( Line='Requested 0 or less points from the sequence', ProcName=ProcName )

    allocate(GetPoints_0D(NbPoints), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get_0D', ProcName=ProcName, stat=StatLoc )

    allocate( SeqVal(1), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )
    SeqVal = Zero

    SkipLoc = int(This%Skip,8)
    LeapLoc = int(This%Leap,8) + 1
    SeqStartLoc = int(SeqStart)
    SeqEndLoc = int(SeqEnd)

    do i = SeqStartLoc, SeqEndLoc
      Step = SkipLoc + LeapLoc*(i-1) + 1
      call i8_sobol( int(1,8), Step, SeqVal)
      GetPoints_0D(i-SeqStartLoc+1) = SeqVal(1)
    end do

    deallocate(SeqVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPoints_1D( This, SeqStart, SeqEnd, NbDim, Debug )

    real(rkp), allocatable, dimension(:,:)                            ::    GetPoints_1D

    class(LowDiscSobol_Type), intent(in)                              ::    This
    integer, intent(in)                                               ::    SeqStart
    integer, intent(in)                                               ::    SeqEnd
    integer, intent(in)                                               ::    NbDim
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetPoints_1D'
    integer                                                           ::    StatLoc=0
    real(rkp), dimension(:), allocatable                              ::    SeqVal
    integer(8)                                                        ::    Step
    integer(8)                                                        ::    SkipLoc
    integer(8)                                                        ::    LeapLoc
    integer(8)                                                        ::    SeqStartLoc
    integer(8)                                                        ::    SeqEndLoc
    integer(8)                                                        ::    i
    integer                                                           ::    NbPoints

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    NbPoints = SeqEnd - SeqStart + 1

    if ( NbPoints <= 0 ) call Error%Raise( Line='Requested 0 or less points from the sequence', ProcName=ProcName )

    if ( NbDim <= 0 ) call Error%Raise( Line='Requested sequence of dimension 0 or less', ProcName=ProcName )

    allocate(GetPoints_1D(NbDim,NbPoints), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get_0D', ProcName=ProcName, stat=StatLoc )

    allocate( SeqVal(NbDim), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )
    SeqVal = Zero

    SkipLoc = int(This%Skip,8)
    LeapLoc = int(This%Leap,8) + 1
    SeqStartLoc = int(SeqStart)
    SeqEndLoc = int(SeqEnd)

    do i = SeqStartLoc, SeqEndLoc
      Step = SkipLoc + LeapLoc*(i-1) + 1
      call i8_sobol( int(NbDim,8), Step, SeqVal)
      GetPoints_1D(:,i-SeqStartLoc+1) = SeqVal(:)
    end do

    deallocate(SeqVal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='SeqVal', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(LowDiscSobol_Type), intent(out)                             ::    LHS
    class(LowDiscSequence_Type), intent(in)                           ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (LowDiscSobol_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Skip = RHS%Skip
          LHS%Leap = RHS%Leap
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(LowDiscSobol_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
