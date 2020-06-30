module Output_Class

use Parameters_Library
use Logger_Class                  ,only: Logger
use Error_Class                   ,only: Error

implicit none

private

public                                                                ::    Output_Type

type                                                                  ::    Output_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Label
  real(rkp), dimension(:,:), allocatable                              ::    Values
  integer                                                             ::    NbNodes
  integer                                                             ::    NbDegen
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1,         &
                                                                                                          ConstructCase2
  procedure, private                                                  ::    ConstructCase1
  procedure, private                                                  ::    ConstructCase2
  procedure, public                                                   ::    GetValues
  procedure, public                                                   ::    GetValuesPointer
  procedure, public                                                   ::    GetNbNodes
  procedure, public                                                   ::    GetNbDegen
  procedure, public                                                   ::    GetLabel
  procedure, public                                                   ::    IsConstructed
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(Output_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Initialized = .true.
      This%Name = 'output'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(Output_Type), intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if (allocated(This%Values)) deallocate(This%Values, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Label)) deallocate(This%Label, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Name)) deallocate(This%Name, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Name', ProcName=ProcName, stat=StatLoc)

    This%NbNodes = 0
    This%NbDegen = 0

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(Output_Type),intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Label = ''

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------ 

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, Values, Label)

    class(Output_Type), intent(inout)                                 ::    This
    real(rkp), dimension(:), intent(in)                               ::    Values
    character(*), optional, intent(in)                                ::    Label
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) then
      if (This%NbNodes /= size(Values,1) .or. This%NbDegen /= 1) then
        call This%Reset()
      else
        This%Values(:,1) = Values
      end if
    end if

    if (.not. This%Initialized) call This%Initialize()

    if (.not. This%Constructed) then
      allocate(This%Values(size(Values,1),1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)
      This%Values(:,1) = Values
      This%NbDegen = 1
      This%NbNodes = size(Values,1)
    end if

    if (present(Label)) This%Label = Label

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase2(This, Values, Label)

    class(Output_Type), intent(inout)                                 ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Values
    character(*), optional, intent(in)                                ::    Label
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) then
      if (This%NbNodes /= size(Values,1) .or. This%NbDegen /= size(Values,2)) then
        call This%Reset()
      else
        This%Values = Values
      end if
    end if

    if (.not. This%Initialized) call This%Initialize()

    if (.not. This%Constructed) then
      allocate(This%Values, source=Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)
      This%NbDegen = size(Values,2)
      This%NbNodes = size(Values,1)
    end if

    if (present(Label)) This%Label = Label

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetValues(This, Values)

    real(rkp), allocatable, dimension(:,:), intent(inout)             ::    Values

    class(Output_Type), intent(inout)                                 ::    This
    
    character(*), parameter                                           ::    ProcName='GetValues'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)        
   
    if (allocated(Values)) then
      if (size(Values,1) /= This%NbNodes .or. size(Values,2) /= This%NbDegen) then
        deallocate(Values, stat=StatLoc)
        if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
      end if
    end if
    if (.not. allocated(Values)) then
      allocate(Values, mold=This%Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if

    Values = This%Values

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetValuesPointer(This)

    real(rkp), dimension(:,:), pointer                                ::    GetValuesPointer

    class(Output_Type), target, intent(in)                            ::    This
    
    character(*), parameter                                           ::    ProcName='GetValuesPointer'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetValuesPointer => This%Values

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbNodes(This)

    integer                                                           ::    GetNbNodes

    class(Output_Type), intent(in)                                    ::    This 

    character(*), parameter                                           ::    ProcName='GetNbNodes'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)
    
    GetNbNodes = This%NbNodes

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDegen(This)

    integer                                                           ::    GetNbDegen

    class(Output_Type), intent(in)                                    ::    This 

    character(*), parameter                                           ::    ProcName='GetNbDegen'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)
    
    GetNbDegen = This%NbDegen

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel(This)

    character(:), allocatable                                         ::    GetLabel

    class(Output_Type), intent(in)                                    ::    This 

    character(*), parameter                                           ::    ProcName='GetName'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetLabel = This%Label

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsConstructed(This)

    logical, allocatable                                              ::    IsConstructed

    class(Output_Type), intent(in)                                    ::    This 

    character(*), parameter                                           ::    ProcName='IsConstructed'

    IsConstructed = This%Constructed

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(Output_Type), intent(inout)                                 ::    LHS
    class(Output_Type), intent(in)                                    ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    call LHS%Reset()

    LHS%Initialized = LHS%Initialized
    LHS%Constructed = RHS%Constructed

    if (RHS%Constructed) then
      LHS%Name = RHS%Name
      LHS%Label = RHS%Label
      allocate(LHS%Values, source=RHS%Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='LHS%Values', ProcName=ProcName, stat=StatLoc)
      LHS%NbNodes = RHS%NbNodes
      LHS%NbDegen = RHS%NbDegen
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(Output_Type),intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%Values)) deallocate(This%Values, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Values', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Label)) deallocate(This%Label, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Name)) deallocate(This%Name, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Name', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
