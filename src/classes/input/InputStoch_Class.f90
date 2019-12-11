module InputStoch_Class

use Parameters_Library
use String_Library
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    InputStoch_Type

type, extends(Input_Type)                                             ::    InputStoch_Type
  real(rkp), dimension(:,:), allocatable                              ::    Input
  integer                                                             ::    NbDegen=0
  logical, allocatable, dimension(:)                                  ::    Stochastic
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetNbDegen
  generic, public                                                     ::    GetValue                =>    GetValue1D_Label,       &
                                                                                                          GetValue2D_Labels,      &
                                                                                                          GetValue2D
  procedure, public                                                   ::    GetValue1D_Label
  procedure, public                                                   ::    GetValue2D_Labels
  procedure, public                                                   ::    GetValue2D
  procedure, public                                                   ::    IsStochastic
  generic, public                                                     ::    GetDetInput             =>    GetInputDet0D,          &
                                                                                                          GetInputDet1D
  procedure, public                                                   ::    GetInputDet0D
  procedure, public                                                   ::    GetInputDet1D
  generic, public                                                     ::    Append                  =>    AppendInput0D_0D,       &
                                                                                                          AppendInput1D_0D,       &
                                                                                                          AppendInput1D_1D,       &
                                                                                                          AppendInput2D_1D
  procedure, public                                                   ::    AppendInput0D_0D
  procedure, public                                                   ::    AppendInput1D_0D
  procedure, public                                                   ::    AppendInput1D_1D
  procedure, public                                                   ::    AppendInput2D_1D
  procedure, public                                                   ::    Transform0D
  procedure, public                                                   ::    Transform1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(InputStoch_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = '<undefined>'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(InputStoch_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%Stochastic) ) deallocate(This%Stochastic, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Stochastic', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Input) ) deallocate(This%Input, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Input', ProcName=ProcName, stat=StatLoc )

    This%NbInputs = 0
    This%NbDegen = 0

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(InputStoch_Type),intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------ 

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Input, Labels, Stochastic )

    class(InputStoch_Type), intent(inout)                             ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Input
    type(String_Type), dimension(:), intent(in)                       ::    Labels
    logical, dimension(:), optional, intent(in)                       ::    Stochastic
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( size(Labels,1) /= size(Input,1) ) call Error%Raise( Line='Mismatch between number of labels and number of inputs',       &
                                                                                                                ProcName=ProcName)

    This%NbInputs = size(Input,1)
    This%NbDegen = size(Input,2)

    allocate(This%Input, source=Input, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Input', ProcName=ProcName, stat=StatLoc )

    allocate(This%Label, source=Labels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    allocate(This%Stochastic(This%NbInputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Stochastic', ProcName=ProcName, stat=StatLoc )
    This%Stochastic = .true.
  
    if ( present(Stochastic) ) This%Stochastic = Stochastic

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AppendInput0D_0D( This, Value, Label )

    class(InputStoch_Type), intent(inout)                             ::    This
    real(rkp), intent(in)                                             ::    Value
    character(*), intent(in)                                          ::    Label
    
    character(*), parameter                                           ::    ProcName='AppendInput0D_0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    type(String_Type), allocatable, dimension(:)                      ::    LabelsLoc
    real(rkp), allocatable, dimension(:,:)                            ::    ValuesLoc
    logical, allocatable, dimension(:)                                ::    StochasticLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    allocate(LabelsLoc(This%NbInputs+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )
    
    allocate(StochasticLoc(This%NbInputs+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )
    StochasticLoc(1:This%NbInputs) = This%Stochastic
    StochasticLoc(This%NbInputs+1) = .false.

    i = 1
    do i = 1, This%NbInputs
      LabelsLoc(i) = This%Label(i)%GetValue()
      if ( LabelsLoc(i)%GetValue() == Label ) call Error%Raise( Line='Tried to append an input that was already part of the ' //  &
                                                                                    'input object :' // Label, ProcName=ProcName )
    end do
    LabelsLoc(This%NbInputs+1) = Label

    allocate(ValuesLoc(This%NbInputs+1,This%NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )
    ValuesLoc(1:This%NbInputs,:) = This%Input
    ValuesLoc(This%NbInputs+1,:) = Value

    call This%Construct( Input=ValuesLoc, Labels=LabelsLoc, Stochastic=StochasticLoc )

    deallocate(ValuesLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(LabelsLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(StochasticLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='StochasticLoc', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AppendInput1D_0D( This, Values, Label, Stochastic )

    class(InputStoch_Type), intent(inout)                             ::    This
    real(rkp), dimension(:), intent(in)                               ::    Values
    character(*), intent(in)                                          ::    Label
    logical, optional, intent(in)                                     ::    Stochastic
    
    character(*), parameter                                           ::    ProcName='AppendInput0D_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    type(String_Type), allocatable, dimension(:)                      ::    LabelsLoc
    real(rkp), allocatable, dimension(:,:)                            ::    ValuesLoc
    logical, allocatable, dimension(:)                                ::    StochasticLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    if ( This%NbDegen /= size(Values,1) ) call Error%Raise( Line='Attempted to append an input that did not have the same ' //    &
                                                                'degeneracy as the original input :' // Label, ProcName=ProcName )

    allocate(LabelsLoc(This%NbInputs+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )
    
    allocate(StochasticLoc(This%NbInputs+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )
    StochasticLoc(1:This%NbInputs) = This%Stochastic
    if ( present(Stochastic) ) then
      StochasticLoc(This%NbInputs+1) = Stochastic
    else
      StochasticLoc(This%NbInputs+1) = .true.
    end if

    i = 1
    do i = 1, This%NbInputs
      LabelsLoc(i) = This%Label(i)%GetValue()
      if ( LabelsLoc(i)%GetValue() == Label ) call Error%Raise( Line='Tried to append an input that was already part of the ' //  &
                                                                                    'input object :' // Label, ProcName=ProcName )
    end do
    LabelsLoc(This%NbInputs+1) = Label

    allocate(ValuesLoc(This%NbInputs+1,This%NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )
    ValuesLoc(1:This%NbInputs,:) = This%Input
    ValuesLoc(This%NbInputs+1,:) = Values

    call This%Construct( Input=ValuesLoc, Labels=LabelsLoc, Stochastic=StochasticLoc )

    deallocate(ValuesLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(LabelsLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(StochasticLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='StochasticLoc', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AppendInput1D_1D( This, Values, Labels )

    class(InputStoch_Type), intent(inout)                             ::    This
    real(rkp), dimension(:), intent(in)                               ::    Values
    type(String_Type), dimension(:), intent(in)                       ::    Labels
    
    character(*), parameter                                           ::    ProcName='AppendInput1D_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii
    type(String_Type), allocatable, dimension(:)                      ::    LabelsLoc
    real(rkp), allocatable, dimension(:,:)                            ::    ValuesLoc
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    NbAppendInputs
    logical, allocatable, dimension(:)                                ::    StochasticLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    NbAppendInputs = size(Values,1)

    allocate(StochasticLoc(This%NbInputs+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )
    StochasticLoc(1:This%NbInputs) = This%Stochastic
    StochasticLoc(This%NbInputs+1:) = .false.

    allocate(LabelsLoc(This%NbInputs+NbAppendInputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbInputs
      LabelsLoc(i) = This%Label(i)%GetValue()
      ii = 1
      do ii = 1, NbAppendInputs
        if ( LabelsLoc(i)%GetValue() == Labels(ii)%GetValue()) call Error%Raise( Line='Tried to append an input that was ' //     &
                                               ' already part of the input object :' // Labels(ii)%GetValue(), ProcName=ProcName )
      end do
    end do

    i = 1
    do i = 1, NbAppendInputs
      LabelsLoc(i+This%NbInputs) = Labels(i)%GetValue()
    end do

    allocate(ValuesLoc(This%NbInputs+NbAppendInputs, This%NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )
    ValuesLoc(1:This%NbInputs,:) = This%Input

    i = 1
    do i = 1, NbAppendInputs
      ValuesLoc(This%NbInputs+i,:) = Values(i)
    end do

    call This%Construct( Input=ValuesLoc, Labels=LabelsLoc, Stochastic=StochasticLoc )

    deallocate(ValuesLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(LabelsLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(StochasticLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='StochasticLoc', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AppendInput2D_1D( This, Values, Labels, Stochastic )

    class(InputStoch_Type), intent(inout)                             ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Values
    type(String_Type), dimension(:), intent(in)                       ::    Labels
    logical, dimension(:), optional, intent(in)                       ::    Stochastic
    
    character(*), parameter                                           ::    ProcName='AppendInput2D_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii
    type(String_Type), allocatable, dimension(:)                      ::    LabelsLoc
    real(rkp), allocatable, dimension(:,:)                            ::    ValuesLoc
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    NbAppendInputs
    logical, allocatable, dimension(:)                                ::    StochasticLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    if ( This%NbDegen /= size(Values,2) ) call Error%Raise( Line='Attempted to append an input that did not have the same ' //    &
                                                 'degeneracy as the original input :' // Labels(i)%GetValue(), ProcName=ProcName )

    NbAppendInputs = size(Values,1)

    allocate(StochasticLoc(This%NbInputs+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )
    StochasticLoc(1:This%NbInputs) = This%Stochastic
    if ( present(Stochastic) ) then
      StochasticLoc(This%NbInputs+1:) = Stochastic
    else
      StochasticLoc(This%NbInputs+1:) = .true.
    end if

    allocate(LabelsLoc(This%NbInputs+NbAppendInputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbInputs
      LabelsLoc(i) = This%Label(i)%GetValue()
      ii = 1
      do ii = 1, NbAppendInputs
        if ( LabelsLoc(i)%GetValue() == Labels(ii)%GetValue()) call Error%Raise( Line='Tried to append an input that was ' //     &
                                               ' already part of the input object :' // Labels(ii)%GetValue(), ProcName=ProcName )
      end do
    end do

    i = 1
    do i = 1, NbAppendInputs
      LabelsLoc(i+This%NbInputs) = Labels(i)%GetValue()
    end do

    allocate(ValuesLoc(This%NbInputs+NbAppendInputs, This%NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )
    ValuesLoc(1:This%NbInputs,:) = This%Input
    i = 1
    do i = 1, NbAppendInputs
      ValuesLoc(This%NbInputs+i,:) = Values(i,:)
    end do

    call This%Construct( Input=ValuesLoc, Labels=LabelsLoc, Stochastic=StochasticLoc )

    deallocate(ValuesLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(LabelsLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(StochasticLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='StochasticLoc', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetValue1D_Label( This, Values, Label, Mandatory, Found )

    class(InputStoch_Type), intent(in)                                ::    This
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Values
    character(*), intent(in)                                          ::    Label
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    
    character(*), parameter                                           ::    ProcName='GetValue1D_Label'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    logical                                                           ::    FoundLoc
    logical                                                           ::    MandatoryLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    MandatoryLoc = .true.
    if ( present(Mandatory) ) MandatoryLoc = Mandatory

    allocate(Values(This%NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )
    Values = Zero

    FoundLoc = .false.

    if ( len_trim(Label) /= 0 ) then
      i = 1
      do i = 1, This%NbInputs
        if ( This%Label(i)%GetValue() == Label ) then
          Values = This%Input(i,:)
          FoundLoc = .true.
          exit
        end if
      end do
    end if

    if ( MandatoryLoc .and. .not. FoundLoc ) call Error%Raise( Line='Mandatory label not found : ' // Label, ProcName=ProcName )

    if ( present(Found) ) Found = FoundLoc

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetValue2D_Labels( This, Values, Labels, Mandatory, Found )

    class(InputStoch_Type), intent(in)                                ::    This
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    Values
    type(String_Type), dimension(:), intent(in)                       ::    Labels
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    
    character(*), parameter                                           ::    ProcName='GetValue2D_Labels'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    NbLabels
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    FoundLoc
    logical                                                           ::    MandatoryLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    MandatoryLoc = .true.
    if ( present(Mandatory) ) MandatoryLoc = Mandatory

    NbLabels = size(Labels,1)

    allocate(Values(This%NbInputs,NbLabels), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )
    Values = Zero

    ii = 1
    do ii = 1, NbLabels
      VarC0D = Labels(ii)%GetValue()
      i = 1
      FoundLoc = .false.
      if ( len_trim(VarC0D) > 0 ) then
        do i = 1, This%NbInputs
          if ( This%Label(i)%GetValue() == VarC0D ) then
            Values(ii,:) = This%Input(ii,:)
            FoundLoc = .true.
            exit
          end if
        end do
      end if
      if ( .not. FoundLoc ) then
        if ( MandatoryLoc ) call Error%Raise( Line='Mandatory label not found : ' // VarC0D, ProcName=ProcName )
        exit
      end if
    end do

    if ( present(Found) ) Found = FoundLoc

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetValue2D( This, Values )

    class(InputStoch_Type), intent(in)                                ::    This
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    Values
    
    character(*), parameter                                           ::    ProcName='GetValue2D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    allocate(Values, source=This%Input, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDegen( This )

    integer                                                           ::    GetNbDegen

    class(InputStoch_Type), intent(in)                                ::    This
    
    character(*), parameter                                           ::    ProcName='GetNbDegen'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    GetNbDegen = This%NbDegen

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsStochastic( This, Label )

    logical                                                           ::    IsStochastic

    class(InputStoch_Type), intent(in)                                ::    This
    character(*), intent(in)                                          ::    Label
    
    character(*), parameter                                           ::    ProcName='GetNbDegen'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    FoundLoc
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    FoundLoc = .false.
    IsStochastic = .true.
    i = 1
    do i = 1, This%NbInputs
      if ( This%Label(i)%GetValue() == Label ) then
        IsStochastic = This%Stochastic(i)
        FoundLoc = .true.
        exit
      end if
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInputDet0D( This, Num )

    type(InputDet_Type)                                               ::    GetInputDet0D

    class(InputStoch_Type), intent(in)                                ::    This
    integer                                                           ::    Num

    character(*), parameter                                           ::    ProcName='GetInputDet0D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )
  
    if ( Num > This%NbDegen ) call Error%Raise( Line='Requested deterministic input out of bounds', ProcName=ProcName )

    call GetInputDet0D%Construct( Input=This%Input(:,Num), Labels=This%Label )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInputDet1D( This )

    type(InputDet_Type), allocatable, dimension(:)                    ::    GetInputDet1D

    class(InputStoch_Type), intent(in)                                ::    This

    character(*), parameter                                           ::    ProcName='GetInputDet1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )
  
    allocate(GetInputDet1D(This%NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetInputDet1D', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbDegen
      call GetInputDet1D(i)%Construct( Input=This%Input(:,i), Labels=This%Label )
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform0D( This, Transformation, Label )

    class(InputStoch_Type), intent(inout)                             ::    This
    character(*), intent(in)                                          ::    Transformation
    character(*), intent(in)                                          ::    Label
    
    character(*), parameter                                           ::    ProcName='Transform0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    if ( len_trim(Label) > 0 ) then
      i = 1
      do i = 1, This%NbInputs
        if ( This%Label(i)%GetValue() == Label ) then
          call Transform( Transformation=Transformation, Values=This%Input(i,:) )
          exit
        end if
      end do
    else
      call Error%Raise( Line='Specified an empty parameter label', ProcName=ProcName )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform1D( This, Transformations, Label )

    class(InputStoch_Type), intent(inout)                             ::    This
    character(*), dimension(:), intent(in)                            ::    Transformations
    character(*), intent(in)                                          ::    Label
    
    character(*), parameter                                           ::    ProcName='Transform1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    if ( len_trim(Label) > 0 ) then
      i = 1
      do i = 1, This%NbInputs
        if ( This%Label(i)%GetValue() == Label ) then
          call Transform( Transformations=Transformations, Values=This%Input(i,:) )
          exit
        end if
      end do
    else
      call Error%Raise( Line='Specified an empty parameter label', ProcName=ProcName )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(InputStoch_Type), intent(out)                               ::    LHS
    class(Input_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (InputStoch_Type)
        call LHS%Reset

        LHS%Initialized = LHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          allocate(LHS%Input, source=RHS%Input, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Input', ProcName=ProcName, stat=StatLoc )
          LHS%NbInputs = RHS%NbInputs
          LHS%NbDegen = RHS%NbDegen
          allocate(LHS%Label, source=RHS%Label, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%InputLabel', ProcName=ProcName, stat=StatLoc )
        end if
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(InputStoch_Type),intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%Input) ) deallocate(This%Input, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Input', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
