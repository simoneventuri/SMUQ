module Output_Class

use Parameters_Library
use Logger_Class                  ,only: Logger
use Error_Class                   ,only: Error

implicit none

private

public                                                                ::    Output_Type

type                                                                  ::    Output_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Label
  real(rkp), dimension(:,:), pointer                                  ::    Values=>null()
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
  procedure, public                                                   :;    GetValues
  procedure, public                                                   :;    GetValuesPointer
  procedure, public                                                   ::    GetNbNodes
  procedure, public                                                   ::    GetNbDegen
  procedure, public                                                   ::    GetLabel
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(Output_Type), intent(inout)                                 ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'output'
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(Output_Type), intent(inout)                                 ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( associated(This%Values) ) deallocate(This%Values, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )

    This%NbNodes = 0
    This%NbDegen = 0

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(Output_Type),intent(inout)                                  ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='SetDefaults'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Label = ''

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------ 

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Values, Name, Label, Debug )

    class(Output_Type), intent(inout)                                 ::    This
    real(rkp), dimension(:), intent(in)                               ::    Values
    character(*), optional, intent(in)                                ::    Name
    character(*), optional, intent(in)                                ::    Label
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) then
      if ( This%NbNodes /= size(Values,1) .or. This%NbDegen /= 1 ) then
        call This%Reset()
      else
        This%Values = Values
      end if
    end if

    if ( .not. This%Initialized ) call This%Initialize()

    if ( .not. This%Constructed ) then
      allocate(This%Values, source=Values, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )
      This%NbDegen = 1
      This%NbNodes = size(Values)
    end if

    if ( present(Name) ) This%Name = Name
    if ( present(Label) ) This%Label = Label

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase2( This, Values, Name, Label, Debug )

    class(Output_Type), intent(inout)                                 ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Values
    character(*), optional, intent(in)                                ::    Name
    character(*), optional, intent(in)                                ::    Label
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) then
      if ( This%NbNodes /= size(Values,1) .or. This%NbDegen /= size(Values,2) ) then
        call This%Reset()
      else
        This%Values = Values
      end if
    end if

    if ( .not. This%Initialized ) call This%Initialize()

    if ( .not. This%Constructed ) then
      allocate(This%Values, source=Values, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )
      This%NbDegen = size(Values,2)
      This%NbNodes = size(Values)
    end if

    if ( present(Name) ) This%Name = Name
    if ( present(Label) ) This%Label = Label

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetValues( This, Debug )

    real(rkp), dimension(:,:), allocatable                            ::    GetValues

    class(Output_Type), intent(inout)                                 ::    This
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetValues'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )        
   
    allocate( GetValues, source=This%Values, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetValues', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetValuesPointer( This, Debug )

    real(rkp), dimension(:,:), pointer                                ::    GetValuesPointer

    class(Output_Type), intent(in)                                    ::    This
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetValuesPointer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetValuesPointer => This%Values

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbNodes( This, Debug )

    integer                                                           ::    GetNbNodes

    class(Output_Type), intent(in)                                    ::    This 
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbNodes'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    
    GetNbNodes = This%NbNodes

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDegen( This, Debug )

    integer                                                           ::    GetNbDegen

    class(Output_Type), intent(in)                                    ::    This 
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbDegen'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    
    GetNbDegen = This%NbDegen

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel( This, Debug )

    character(:), allocatable                                         ::    GetLabel

    class(Output_Type), intent(in)                                    ::    This 
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetLabel = This%Label

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(Output_Type), intent(out)                                   ::    LHS
    class(Output_Type), intent(in)                                    ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    call LHS%Reset

    LHS%Initialized = LHS%Initialized
    LHS%Constructed = RHS%Constructed

    if ( RHS%Constructed ) then
      LHS%Name = RHS%Name
      LHS%Label = RHS%Label
      allocate(LHS%Values, source=RHS%Values, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Values', ProcName=ProcName, stat=StatLoc )
      LHS%NbNodes = RHS%NbNodes
      LHS%NbDegen = RHS%NbDegen
    end if

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(Output_Type),intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%Values) ) deallocate(This%Values, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Values', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
