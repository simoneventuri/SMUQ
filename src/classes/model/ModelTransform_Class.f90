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

module ModelTransform_class

use Input_Library
use Parameters_Library
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use SpaceTransf_Class                                             ,only:    SpaceTransf_Type
use SpaceTransf_Factory_Class                                     ,only:    SpaceTransf_Factory
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type

implicit none

private

public                                                                ::    ModelTransform_Type

type, extends(Model_Type)                                             ::    ModelTransform_Type
  class(Model_Type), allocatable                                      ::    Model
  class(SpaceTransf_Type), allocatable                                ::    SpaceTransform
  type(String_Type), allocatable, dimension(:)                        ::    InputLabels
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, public                                                   ::    RunCase1
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This, Debug )

    class(ModelTransform_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'modeltransform'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This, Debug )

    class(ModelTransform_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%SpaceTransform) ) deallocate(This%SpaceTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%SpaceTransform', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Model) ) deallocate(This%Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%InputLabels) ) deallocate(This%InputLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabels', ProcName=ProcName, stat=StatLoc )

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This, Debug )

    class(ModelTransform_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1( This, SpaceTransform, Model, Debug )

    class(ModelTransform_Type), intent(inout)                         ::    This
    class(SpaceTransf_Type), intent(in)                               ::    SpaceTransform
    class(Model_Type), intent(in)                                     ::    Model
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%SpaceTransform, source=SpaceTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%SpaceTransform', ProcName=ProcName, stat=StatLoc )

    allocate(This%Model, source=Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    allocate(This%InputLabels(This%SpaceTransform%GetNbDim()), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%InputLabels', ProcName=ProcName, stat=StatLoc )
    This%InputLabels = This%SpaceTransform%GetLabel()

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine RunCase1( This, Input, Output, Stat, Debug )

    class(ModelTransform_Type), intent(inout)                         ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    integer, optional, intent(out)                                    ::    Stat
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='RunCase1'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    ExternalFlag=.false.
    class(Input_Type), allocatable                                    ::    InputLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    select type (Input)
      type is (InputDet_Type)
        allocate( InputDet_Type :: InputLoc )
        select type (InputLoc)
          type is (InputDet_Type)
            call Input%GetValue( Values=VarR1D, Labels=This%InputLabels )
            call InputLoc%Construct( Input=This%SpaceTransform%InvTransform(Z=VarR1D), Labels=This%InputLabels )
            deallocate(VarR1D, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
          class default
            call Error%Raise( Line='Something went wrong', ProcName=ProcName )
        end select
      type is (InputStoch_Type)
        allocate( InputStoch_Type :: InputLoc )
        select type (InputLoc)
          type is (InputStoch_Type)
            call Input%GetValue( Values=VarR2D, Labels=This%InputLabels )
            call InputLoc%Construct( Input=This%SpaceTransform%InvTransform(Z=VarR2D), Labels=This%InputLabels )
            deallocate(VarR2D, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
          class default
            call Error%Raise( Line='Something went wrong', ProcName=ProcName )
        end select
      class default
        call Error%Raise( Line='Class not recognized, update definitions', ProcName=ProcName )
    end select

    call This%Model%Run( Input=InputLoc, Output=Output, Stat=Stat )

    deallocate(InputLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='InputLoc', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(ModelTransform_Type), intent(out)                           ::    LHS
    class(Model_Type), intent(in)                            ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type ( RHS )
      type is (ModelTransform_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          allocate(LHS%SpaceTransform, source=RHS%SpaceTransform, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%SpaceTransform', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Model, source=RHS%Model, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Model', ProcName=ProcName, stat=StatLoc )
        end if
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )
    end select
    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(ModelTransform_Type),intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%SpaceTransform) ) deallocate(This%SpaceTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%SpaceTransform', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Model) ) deallocate(This%Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%InputLabels) ) deallocate(This%InputLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabels', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
