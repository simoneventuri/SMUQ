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
  generic, public                                                     ::    Run                     =>    Run0D,                  &
                                                                                                          Run1D
  procedure, private                                                  ::    Run0D
  procedure, private                                                  ::    Run1D
  procedure, public                                                   ::    GetNbResponses
  procedure, public                                                   ::    GetResponseNbNodes
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(ModelInterface_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'ModelInterface'
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(ModelInterface_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(ModelInterface_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Model, Responses, Debug )

    class(ModelInterface_Type), intent(inout)                         ::    This
    class(Model_Type), intent(in)                                     ::    Model
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run0D( This, Input, Output, Stat, Debug )

    class(ModelInterface_Type), intent(inout)                         ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    integer, optional, intent(out)                                    ::    Stat
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Run0D'
    integer                                                           ::    StatLoc=0
    type(Output_Type), dimension(:), allocatable                      ::    OutputLoc
    character(:), allocatable                                         ::    LabelLoc
    integer                                                           ::    NbOutputs
    integer                                                           ::    i, ii, iii

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( size(Output,1) /= This%NbResponses ) then
      deallocate(Output, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    end if

    if ( .not. allocated(Output) ) then
      allocate(Output(This%NbResponses), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    end if

    call This%Model%Run( Input=Input, Output=OutputLoc, Stat=StatLoc )
    if ( present(Stat) ) Stat = StatLoc

    if ( StatLoc /= 0 ) then
      if ( present(Stat) ) then
        Stat = StatLoc
      else
        call Error%Raise( 'Model returned a non-zero status indicator: ' // ConvertToString(Value=StatLoc), ProcName=ProcName )
      end if
    else
      NbOutputs = size(OutputLoc,1)

      allocate(Output(This%NbResponses), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )

      do i = 1, This%NbResponses
        LabelLoc = This%ResponseLabels(i)%GetValue()

        iii = 0
        ii = 1
        do ii = 1, NbOutputs
          if ( LabelLoc == OutputLoc(ii)%GetLabel() ) then
            iii = ii
            exit
          end if
        end do
        if ( iii == 0 ) call Error%Raise( Line='Did not finding matching output label for response : ' //                         &
                                                                            This%ResponseLabels(i)%GetValue(), ProcName=ProcName )

        if ( OutputLoc(iii)%GetNbNodes() /= This%ResponseNbNodes(i) ) call Error%Raise( Line='Mismatching number of ' //          &
                                                                       'output nodes with specified response', ProcName=ProcName )
        Output(i) = OutputLoc(iii)

      end do

      deallocate(OutputLoc, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run1D( This, Input, Output, Stat, Debug )

    class(ModelInterface_Type), intent(inout)                         ::    This
    class(Input_Type), dimension(:), intent(in)                       ::    Input
    type(Output_Type), dimension(:,:), allocatable, intent(out)       ::    Output
    integer, optional, intent(out)                                    ::    Stat
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Run0D'
    integer                                                           ::    StatLoc=0
    type(Output_Type), dimension(:), allocatable                      ::    OutputLoc
    character(:), allocatable                                         ::    LabelLoc
    integer                                                           ::    NbInputs
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NbInputs = size(Input,1)

    if ( present(Stat) ) Stat=StatLoc

    i = 1
    do i = 1, NbInputs
      call This%Run(Input=Input(i), Output=OutputLoc, Stat=StatLoc )
      if ( StatLoc /= 0 ) then
        if ( present(Stat) ) then
          Stat=StatLoc
        else
          call Error%Raise( 'Model returned a non-zero status indicator: ' // ConvertToString(Value=StatLoc), ProcName=ProcName )
        end if
        exit
      else
        if ( .not. allocated(Output) ) then
          allocate(Output(size(OutputLoc,1),NbInputs), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
        end if
        Output(:,i) = OutputLoc
      end if
    end do

    if ( allocated(OutputLoc) ) deallocate(OutputLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbResponses( This, Debug )

    integer                                                           ::    GetNbResponses

    class(ModelInterface_Type), intent(in)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbResponses'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbResponses = This%NbResponses

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetResponseNbNodes( This, Label, Debug )

    integer                                                           ::    GetResponseNbNodes

    class(ModelInterface_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    Label
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetResponseNbNodes'
    integer                                                           ::    i
    integer                                                           ::    ii

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

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

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(ModelInterface_Type), intent(out)                           ::    LHS
    class(ModelInterface_Type), intent(in)                            ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(ModelInterface_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%Model) ) deallocate(This%Model, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Model', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseLabels) ) deallocate(This%ResponseLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseLabels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseNbNodes) ) deallocate(This%ResponseNbNodes, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseNbNodes', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------


end module
