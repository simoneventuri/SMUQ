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

module ModelInterface_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringRoutines_Module
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Response_Class                                                ,only:    Response_Type
use Model_Class                                                   ,only:    Model_Type
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    ModelInterface_Type

type                                                                  ::    ModelInterface_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  class(Model_Type), allocatable                                      ::    Model
  type(String_Type), allocatable, dimension(:)                        ::    ResponseLabels
  integer, allocatable, dimension(:)                                  ::    ResponseNbNodes
  integer                                                             ::    NbResponses=0
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructCase1
  generic, public                                                     ::    Run                     =>    Run_0D,                 &
                                                                                                          Run_1D
  procedure, private                                                  ::    Run_0D
  procedure, private                                                  ::    Run_1D
  procedure, public                                                   ::    GetNbResponses
  procedure, public                                                   ::    GetResponseNbNodes
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(ModelInterface_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'ModelInterface'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(ModelInterface_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%Model) ) deallocate(This%Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseLabels) ) deallocate(This%ResponseLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseNbNodes) ) deallocate(This%ResponseNbNodes, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseNbNodes', ProcName=ProcName, stat=StatLoc )

    This%NbResponses=0

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(ModelInterface_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Model, Responses )

    class(ModelInterface_Type), intent(inout)                         ::    This
    class(Model_Type), intent(in)                                     ::    Model
    type(Response_Type), dimension(:), intent(in)                     ::    Responses

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%Model, source=Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    This%NbResponses = size(Responses,1)

    allocate(This%ResponseNbNodes(This%NbResponses), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ResponseNbNodes', ProcName=ProcName, stat=StatLoc )

    allocate(This%ResponseLabels(This%NbResponses), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ResponseLabels', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbResponses
      This%ResponseLabels(i) = Responses(i)%GetLabel()
      This%ResponseNbNodes(i) = Responses(i)%GetNbNodes()
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_0D( This, Input, Output, Stat )

    class(ModelInterface_Type), intent(inout)                         ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    StatRun=0
    type(Output_Type), dimension(:), allocatable                      ::    OutputLoc
    character(:), allocatable                                         ::    LabelLoc
    integer                                                           ::    NbOutputs
    integer                                                           ::    i
    integer                                                           ::    ii
    logical                                                           ::    CorrectOrder

    StatRun = 1

    NbOutputs = This%Model%GetNbOutputs()

    if ( size(Output,1) /= This%NbResponses ) call Error%Raise( 'Passed an output array of incorrect length', ProcName=ProcName )
    if ( NbOutputs < This%NbResponses ) call Error%Raise( 'Insufficient number of model outputs', ProcName=ProcName )

    if ( This%NbOutputs == This%NbResponses ) then
      call This%Model%Run( Input=Input, Output=Output, Stat=StatRun )
    else
      allocate(OutputLoc(NbOutputs), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

      call This%Model%Run( Input=Input, Output=OutputLoc, Stat=StatRun )

      i = 1
      do i = 1, This%NbResponses
        LabelLoc = This%ResponseLabels(i)%GetValue()
        ii = 1
        do ii = 1, NbOutputs
          if ( LabelLoc == OutputLoc(ii)%GetLabel() ) then
            Output(i) = OutputLoc(ii)
            exit
          end if
          call Error%Raise( 'Did not find required output : ' // LabelLoc, ProcName=ProcName )
        end do
      end do

      deallocate(OutputLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )
    end if

    if ( present(Stat) ) then
      Stat=StatRun
      if ( StatRun /= 0 ) return
    else
      if ( StatRun /= 0 ) call Error%Raise( 'Model returned a non-zero status indicator: ' // ConvertToString(Value=StatLoc),     &
                                                                                                               ProcName=ProcName )
    end if

    CorrectOrder = .true.
    i = 1
    do i = 1, This%NbResponses
      if ( This%ResponseLabels(i)%GetValue() == Output(i)%GetLabel() ) cycle
      CorrectOrder = .false.
      exit
    end do

    if ( .not. CorrectOrder ) then

      allocate(OutputLoc, source=Output, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

      i = 1
      do i = 1, This%NbResponses
        LabelLoc = This%ResponseLabels(i)%GetValue()
        ii = 1
        do ii = 1, NbOutputs
          if ( LabelLoc == OutputLoc(ii)%GetLabel() ) then
            Output(i) = OutputLoc(ii)
            exit
          end if
          call Error%Raise( 'Did not find required output : ' // LabelLoc, ProcName=ProcName )
        end do
      end do

      deallocate(OutputLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )  
    end if

    i = 1
    do i = 1, This%NbResponses
      if ( Output(i)%GetNbNodes() /= This%ResponseNbNodes(i) ) call Error%Raise( Line='Mismatching number of ' //               &
                                          'output nodes for response : ' // This%ResponseLabels(i)%GetValue(), ProcName=ProcName )
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_1D( This, Input, Output, Stat )

    class(ModelInterface_Type), intent(inout)                         ::    This
    type(Input_Type), dimension(:), intent(in)                        ::    Input
    type(Output_Type), dimension(:,:), intent(inout)                  ::    Output
    integer, dimension(:), optional, intent(inout)                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_1D'
    integer                                                           ::    StatLoc=0
    integer, allocatable, dimension(:)                                ::    StatRun=0
    type(Output_Type), dimension(:), allocatable                      ::    OutputLoc
    character(:), allocatable                                         ::    LabelLoc
    integer                                                           ::    NbInputs
    integer                                                           ::    NbOutputs
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    logical                                                           ::    CorrectOrder

    NbInputs = size(Input,1)
    NbOutputs = This%Model%GetNbOutputs()
    
    allocate(StatRun(NbInputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='StatRun', ProcName=ProcName, stat=StatLoc )
    StatRun = 1

    if ( size(Output,1) /= This%NbResponses .or. size(Output,2) /= This%NbInputs) call Error%Raise( 'Passed an output array of' //&
                                                                                          ' incorrect length', ProcName=ProcName )

    if ( This%NbOutputs == This%NbResponses ) then
      call This%Model%Run( Input=Input, Output=Output, Stat=StatRun )
    else
      allocate(OutputLoc(NbOutputs, NbInputs), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

      call This%Model%Run( Input=Input, Output=OutputLoc, Stat=StatRun )

      iii = 1
      do iii = 1, NbInputs
        i = 1
        do i = 1, This%NbResponses
          LabelLoc = This%ResponseLabels(i)%GetValue()
          ii = 1
          do ii = 1, NbOutputs
            if ( LabelLoc == OutputLoc(ii,iii)%GetLabel() ) then
              Output(i,iii) = OutputLoc(ii,iii)
              exit
            end if
            call Error%Raise( 'Did not find required output : ' // LabelLoc, ProcName=ProcName )
          end do
        end do
        i = 1
        do i = 1, NbOutputs
          call OutputLoc(i,iii)%Reset()
        end do
      end do

      deallocate(OutputLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )
    end if

    if ( present(Stat) ) then
      Stat=StatRun
      if ( all(StatRun /= 0) ) then
        deallocate(StatRun, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='StatRun', ProcName=ProcName, stat=StatLoc )
        return
      end if
    else
      if ( any(StatRun) /= 0 ) call Error%Raise( 'A model returned a non-zero status indicator: ' //                              &
                                                                              ConvertToString(Values=StatRun), ProcName=ProcName )
    end if

    CorrectOrder = .true.
    ii = 1
    do ii = 1, NbInputs
      i = 1
      do i = 1, This%NbResponses
        if ( This%ResponseLabels(i)%GetValue() == Output(i,ii)%GetLabel() ) cycle
        CorrectOrder = .false.
        exit
      end do
      if ( .not. CorrectOrder ) exit
    end do

    if ( .not. CorrectOrder ) then
      allocate(OutputLoc(NbOutputs,1), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

      iii = 1
      do iii = 1, NbInputs
        OutputLoc(:,1) = Output(:,iii)
        i = 1
        do i = 1, This%NbResponses
          LabelLoc = This%ResponseLabels(i)%GetValue()
          ii = 1
          do ii = 1, NbOutputs
            if ( LabelLoc == OutputLoc(ii,1)%GetLabel() ) then
              Output(i,iii) = OutputLoc(ii,1)
              exit
            end if
            call Error%Raise( 'Did not find required output : ' // LabelLoc, ProcName=ProcName )
          end do
        end do
      end do
      deallocate(OutputLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )  
    end if

    ii = 1
    do ii = 1, NbInputs
      i = 1
      do i = 1, This%NbResponses
        if ( Output(i,ii)%GetNbNodes() /= This%ResponseNbNodes(i) ) call Error%Raise( Line='Mismatching number of ' //            &
                                          'output nodes for response : ' // This%ResponseLabels(i)%GetValue(), ProcName=ProcName )
      end do
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbResponses( This )

    integer                                                           ::    GetNbResponses

    class(ModelInterface_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetNbResponses'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbResponses = This%NbResponses

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetResponseNbNodes( This, Label )

    integer                                                           ::    GetResponseNbNodes

    class(ModelInterface_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetResponseNbNodes'
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    ii = 0
    do i = 1, This%NbResponses
      if ( This%ResponseLabels(i)%GetValue() /= Label ) cycle
      ii = i
      exit
    end do

    if ( ii == 0 ) call Error%Raise( 'Did not find required response: ' // Label, ProcName=ProcName )

    GetResponseNbNodes = This%ResponseNbNodes(ii)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(ModelInterface_Type), intent(out)                           ::    LHS
    class(ModelInterface_Type), intent(in)                            ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (ModelInterface_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          allocate(LHS%ResponseLabels, source=RHS%ResponseLabels, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ResponseLabels', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%ResponseNbNodes, source=RHS%ResponseNbNodes, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ResponseNbNodes', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Model, source=RHS%Model, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Model', ProcName=ProcName, stat=StatLoc )
          LHS%NbResponses = RHS%NbResponses
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(ModelInterface_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%Model) ) deallocate(This%Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseLabels) ) deallocate(This%ResponseLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseNbNodes) ) deallocate(This%ResponseNbNodes, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseNbNodes', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------


end module
