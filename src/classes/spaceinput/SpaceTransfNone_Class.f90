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

module SpaceTransfNone_Class

use Input_Library
use String_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SpaceTransf_Class                                             ,only:    SpaceTransf_Type
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Vec_Class                                            ,only:    DistProb_Vec_Type
use SpaceInput_Class                                              ,only:    SpaceInput_Type
use SpaceParam_Class                                              ,only:    SpaceParam_Type

implicit none

private

public                                                                ::    SpaceTransfNone_Type

type, extends(SpaceTransf_Type)                                       ::    SpaceTransfNone_Type

contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    Transform1D
  procedure, public                                                   ::    InvTransform1D
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal=.false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(SpaceTransfNone_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      call This%SetDefaults()
      This%Name='transfnone'
      This%Initialized = .true.
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(SpaceTransfNone_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    call This%SpaceInput%Reset()
    call This%SpaceInputOrig%Reset()

    This%NbDim = 0

    This%Correlated=.false.

    This%Initialized=.false.
    This%Constructed=.false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(SpaceTransfNone_Type),intent(inout)                         ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='SetDefaults'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------       

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructInput( This, Input, SpaceInput, Prefix, Debug )

    class(SpaceTransfNone_Type), intent(inout)                        ::    This
    type(SpaceParam_Type), optional, intent(in)                       ::    SpaceInput
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructInput2'
    logical                                                           ::    DebugLoc
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    if ( present(SpaceInput) ) then
      This%SpaceInputOrig = SpaceInput
    else
      SectionName='parameter_space'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call This%SpaceInputOrig%Construct( Input=InputSection, Prefix=PrefixLoc )
      nullify(InputSection)
    end if

    This%NbDim = This%SpaceInputOrig%GetNbDim()

    This%SpaceInput = SpaceInput

    This%Correlated = This%SpaceInput%IsCorrelated()

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------ 

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructCase1( This, SpaceInput, Debug )
    
    class(SpaceTransfNone_Type), intent(inout)                        ::    This
    type(SpaceParam_Type), intent(in)                                 ::    SpaceInput
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset
    if ( .not. This%Initialized ) call This%Initialize  

    This%SpaceInputOrig = SpaceInput

    This%NbDim = This%SpaceInputOrig%GetNbDim()

    This%SpaceInput = SpaceInput

    This%Correlated = This%SpaceInput%IsCorrelated()

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------ 

  !!------------------------------------------------------------------------------------------------------------------------------
  function Transform1D( This, X, Debug )
    
    real(rkp), allocatable, dimension(:)                              ::    Transform1D

    class(SpaceTransfNone_Type), intent(inout)                        ::    This
    real(rkp), dimension(:), intent(in)                               ::    X
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Transform1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate( Transform1D, source=X, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Transform1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvTransform1D( This, Z, Debug )
    
    real(rkp), allocatable, dimension(:)                              ::    InvTransform1D

    class(SpaceTransfNone_Type), intent(inout)                        ::    This
    real(rkp), dimension(:), intent(in)                               ::    Z
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='InvTransform1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate( InvTransform1D, source=Z, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InvTransform1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput
    class(SpaceTransfNone_Type), intent(in)                           ::    This
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

    SectionName = 'parameter_space'
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/parameter_space'
    call GetInput%AddSection( Section=This%SpaceInputOrig%GetInput( MainSectionName=SectionName, Prefix=PrefixLoc,                &
                                                                                                         Directory=DirectorySub) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(SpaceTransfNone_Type),intent(out)                           ::    LHS
    class(SpaceInput_Type),intent(in)                                 ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (SpaceTransfNone_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          LHS%NbDim = RHS%NbDim
          LHS%Correlated = RHS%Correlated
          LHS%SpaceInput = RHS%SpaceInput
          LHS%SpaceInputOrig = RHS%SpaceInputOrig
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(SpaceTransfNone_Type),intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
