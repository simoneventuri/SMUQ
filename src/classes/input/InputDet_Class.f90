module InputDet_Class

use Parameters_Library
use String_Library
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    InputDet_Type

type, extends(Input_Type)                                             ::    InputDet_Type
  real(rkp), dimension(:), allocatable                                ::    Input
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructCase1
  generic, public                                                     ::    GetValue                =>    GetValue0D_Label,       &
                                                                                                          GetValue1D_Labels,      &
                                                                                                          GetValue1D
  procedure, public                                                   ::    GetValue0D_Label
  procedure, public                                                   ::    GetValue1D_Labels
  procedure, public                                                   ::    GetValue1D
  generic, public                                                     ::    Append                  =>    AppendInput0D,          &
                                                                                                          AppendInput1D
  procedure, public                                                   ::    AppendInput0D
  procedure, public                                                   ::    AppendInput1D
  procedure, public                                                   ::    Transform0D
  procedure, public                                                   ::    Transform1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(InputDet_Type), intent(inout)                               ::    This

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

    class(InputDet_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%Input) ) deallocate(This%Input, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Input', ProcName=ProcName, stat=StatLoc )

    This%NbInputs = 0

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(InputDet_Type),intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------ 

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Input, Labels )

    class(InputDet_Type), intent(inout)                               ::    This
    real(rkp), dimension(:), intent(in)                               ::    Input
    type(String_Type), dimension(:), intent(in)                       ::    Labels
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    if ( size(Labels,1) /= size(Input,1) ) call Error%Raise( Line='Mismatch between number of labels and number of inputs',       &
                                                                                                                ProcName=ProcName)

    This%NbInputs = size(Input,1)

    allocate(This%Input, source=Input, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Input', ProcName=ProcName, stat=StatLoc )

    allocate(This%Label, source=Labels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AppendInput0D( This, Value, Label )

    class(InputDet_Type), intent(inout)                               ::    This
    real(rkp), intent(in)                                             ::    Value
    character(*), intent(in)                                          ::    Label
    
    character(*), parameter                                           ::    ProcName='AppendInput0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    type(String_Type), allocatable, dimension(:)                      ::    LabelsLoc
    real(rkp), allocatable, dimension(:)                              ::    ValuesLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    allocate(LabelsLoc(This%NbInputs+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )
    
    i = 1
    do i = 1, This%NbInputs
      LabelsLoc(i) = This%Label(i)%GetValue()
      if ( LabelsLoc(i)%GetValue() == Label ) call Error%Raise( Line='Tried to append an input that was already part of the ' //  &
                                                                                    'input object :' // Label, ProcName=ProcName )
    end do
    LabelsLoc(This%NbInputs+1) = Label

    allocate(ValuesLoc(This%NbInputs+1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )
    ValuesLoc(1:This%NbInputs) = This%Input
    ValuesLoc(This%NbInputs+1) = Value

    call This%Construct( Input=ValuesLoc, Labels=LabelsLoc )

    deallocate(ValuesLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(LabelsLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine AppendInput1D( This, Values, Labels )

    class(InputDet_Type), intent(inout)                               ::    This
    real(rkp), dimension(:), intent(in)                               ::    Values
    type(String_Type), dimension(:), intent(in)                       ::    Labels
    
    character(*), parameter                                           ::    ProcName='AppendInput1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii
    type(String_Type), allocatable, dimension(:)                      ::    LabelsLoc
    real(rkp), allocatable, dimension(:)                              ::    ValuesLoc
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    NbAppendInputs

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    NbAppendInputs = size(Values,1)

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

    allocate(ValuesLoc(This%NbInputs+NbAppendInputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )
    ValuesLoc(1:This%NbInputs) = This%Input
    ValuesLoc(This%NbInputs+1:) = Values

    call This%Construct( Input=ValuesLoc, Labels=LabelsLoc )

    deallocate(ValuesLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ValuesLoc', ProcName=ProcName, stat=StatLoc )

    deallocate(LabelsLoc, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='LabelsLoc', ProcName=ProcName, stat=StatLoc )
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetValue0D_Label( This, Value, Label, Mandatory, Found )

    class(InputDet_Type), intent(in)                                  ::    This
    real(rkp), intent(out)                                            ::    Value
    character(*), intent(in)                                          ::    Label
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    
    character(*), parameter                                           ::    ProcName='GetValue0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    logical                                                           ::    FoundLoc
    logical                                                           ::    MandatoryLoc

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    Value = Zero

    MandatoryLoc = .true.
    if ( present(Mandatory) ) MandatoryLoc = Mandatory

    FoundLoc = .false.

    if ( len_trim(Label) > 0 ) then
      i = 1
      do i = 1, This%NbInputs
        if ( This%Label(i)%GetValue() == Label ) then
          Value = This%Input(i)
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
  subroutine GetValue1D_Labels( This, Values, Labels, Mandatory, Found )

    class(InputDet_Type), intent(in)                                  ::    This
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Values
    type(String_Type), dimension(:), intent(in)                       ::    Labels
    logical, optional, intent(in)                                     ::    Mandatory
    logical, optional, intent(out)                                    ::    Found
    
    character(*), parameter                                           ::    ProcName='GetValue1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbLabels
    integer                                                           ::    i
    integer                                                           ::    ii
    logical                                                           ::    FoundLoc
    logical                                                           ::    MandatoryLoc
    character(:), allocatable                                         ::    VarC0D

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    MandatoryLoc = .true.
    if ( present(Mandatory) ) MandatoryLoc = Mandatory

    NbLabels = size(Labels,1)

    allocate(Values(NbLabels), stat=StatLoc)
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
            Values(ii) = This%Input(i)
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
  subroutine GetValue1D( This, Values )

    class(InputDet_Type), intent(in)                                  ::    This
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Values
    
    character(*), parameter                                           ::    ProcName='GetValue1D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    allocate(Values, source=This%Input, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetValue1D_NonAlloc( This, Values )

    class(InputDet_Type), intent(in)                                  ::    This
    real(rkp), allocatable, dimension(:), intent(out)                 ::    Values
    
    character(*), parameter                                           ::    ProcName='GetValue1D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    allocate(Values, source=This%Input, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Values', ProcName=ProcName, stat=StatLoc )
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform0D( This, Transformation, Label )

    class(InputDet_Type), intent(inout)                               ::    This
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
          call Transform( Transformation=Transformation, Value=This%Input(i) )
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

    class(InputDet_Type), intent(inout)                               ::    This
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
          call Transform( Transformations=Transformations, Value=This%Input(i) )
          exit
        end if
      end do
    else
      call Error%Raise( Line='Specified an empty parameter label', ProcName=ProcName )
    end if
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(InputDet_Type), intent(out)                                 ::    LHS
    class(Input_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (InputDet_Type)
        call LHS%Reset

        LHS%Initialized = LHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          allocate(LHS%Input, source=RHS%Input, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Input', ProcName=ProcName, stat=StatLoc )
          LHS%NbInputs = RHS%NbInputs
          allocate(LHS%Label, source=RHS%Label, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%InputLabel', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(InputDet_Type),intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%Input) ) deallocate(This%Input, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Input', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
