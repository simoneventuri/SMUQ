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
use TransfSampleSpace_Class                                       ,only:    TransfSampleSpace_Type
use TransfSampleSpace_Factory_Class                               ,only:    TransfSampleSpace_Factory
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type

implicit none

private

public                                                                ::    ModelTransform_Type

type, extends(Model_Type)                                             ::    ModelTransform_Type
  class(Model_Type), allocatable                                      ::    Model
  class(TransfSampleSpace_Type), allocatable                          ::    SpaceTransform
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
  subroutine Initialize( This )

    class(ModelTransform_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    if ( .not. This%Initialized ) then
      This%Name = 'modeltransform'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This )

    class(ModelTransform_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0
    if ( allocated(This%SpaceTransform) ) deallocate(This%SpaceTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%SpaceTransform', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Model) ) deallocate(This%Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%InputLabels) ) deallocate(This%InputLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabels', ProcName=ProcName, stat=StatLoc )

    call This%SetDefaults()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This )

    class(ModelTransform_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1( This, SpaceTransform, Model )

    class(ModelTransform_Type), intent(inout)                         ::    This
    class(TransfSampleSpace_Type), intent(in)                         ::    SpaceTransform
    class(Model_Type), intent(in)                                     ::    Model

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
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

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine RunCase1( This, Input, Output, Stat )

    class(ModelTransform_Type), intent(inout)                         ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='RunCase1'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    ExternalFlag=.false.
    class(Input_Type), allocatable                                    ::    InputLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
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

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  impure elemental subroutine Copy( LHS, RHS )

    class(ModelTransform_Type), intent(out)                           ::    LHS
    class(Model_Type), intent(in)                            ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

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

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(ModelTransform_Type),intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%SpaceTransform) ) deallocate(This%SpaceTransform, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%SpaceTransform', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Model) ) deallocate(This%Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%InputLabels) ) deallocate(This%InputLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabels', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
