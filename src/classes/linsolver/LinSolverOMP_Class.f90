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

module LinSolverOMP_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverMethod_Class                                         ,only:    LinSolverMethod_Type
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
use OMPMethod_Class                                               ,only:    OMPMethod_Type
use OMPMethod_Factory_Class                                       ,only:    OMPMethod_Factory
use LinSolverSparse_Class                                         ,only:    LinSolverSparse_Type

implicit none

private

public                                                                ::    LinSolverOMP_Type

type, extends(LinSolverSparse_Type)                                   ::    LinSolverOMP_Type
  class(OMPMethod_Type), allocatable                                  ::    OMPMethod
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    SolveSparse
  procedure, public                                                   ::    SolveFull
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(LinSolverOMP_Type), intent(inout)                           ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'LinSolverOMP'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(LinSolverOMP_Type), intent(inout)                           ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized = .false.
    This%Constructed = .false.

    deallocate(This%OMPMethod, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OMPMethod', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(LinSolverOMP_Type), intent(inout)                           ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(LinSolverOMP_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    call OMPMethod_Factory%Construct( Object=This%OMPMethod, Input=Input, Prefix=PrefixLoc )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, OMPMethod, Debug )

    use String_Library

    class(LinSolverOMP_Type), intent(inout)                           ::    This
    class(OMPMethod_Type), intent(in)                                 ::    OMPMethod
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%OMPMethod, source=OMPMethod, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OMPMethod', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(LinSolverOMP_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

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

    GetInput = OMPMethod_Factory%GetObjectInput( Object=This%OMPMethod, MainSectionName=MainSectionName, Prefix=PrefixLoc,        &
                                                                                                          Directory=DirectoryLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveSparse( This, System, Goal, ModelSet, CoefficientsSet, CVError, Debug )

    class(LinSolverOMP_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, allocatable, dimension(:), intent(out)                   ::    ModelSet
    real(rkp), allocatable, dimension(:), intent(out)                 ::    CoefficientsSet
    real(rkp), optional, intent(out)                                  ::    CVError
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SolveSparse'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( present(CVError) ) then
      call This%OMPMethod%SolveSparse( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet,              &
                                                                                                                 CVError=CVError )
    else
      call This%OMPMethod%SolveSparse( System=System, Goal=Goal, ModelSet=ModelSet, CoefficientsSet=CoefficientsSet )
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SolveFull( This, System, Goal, Coefficients, CVError, Debug )

    class(LinSolverOMP_Type), intent(in)                              ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Coefficients
    real(rkp), optional, intent(out)                                  ::    CVError
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SolveFull'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( present(CVError) ) then
      call This%OMPMethod%SolveFull( System=System, Goal=Goal, Coefficients=Coefficients, CVError=CVError )
    else
      call This%OMPMethod%SolveFull( System=System, Goal=Goal, Coefficients=Coefficients )
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(LinSolverOMP_Type), intent(out)                             ::    LHS
    class(LinSolverMethod_Type), intent(in)                           ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
      type is (LinSolverOMP_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          allocate(LHS%OMPMethod, source=RHS%OMPMethod, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%OMPMethod', ProcName=ProcName, stat=StatLoc )
        end if
      class default
        call Error%Raise( Line='Mismatching object types', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(LinSolverOMP_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%OMPMethod) ) deallocate(This%OMPMethod, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OMPMethod', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
