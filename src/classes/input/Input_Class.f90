module Input_Class

use Parameters_Library
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    Input_Type

type, abstract                                                        ::    Input_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Name
  integer                                                             ::    NbInputs=0
  type(String_Type), dimension(:), allocatable                        ::    Label
contains
  generic, public                                                     ::    Construct               =>    ConstructEmpty
  procedure, private                                                  ::    ConstructEmpty
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    GetLabel                =>    GetLabel0D,             &
                                                                                                          GetLabel1D
  generic, public                                                     ::    Transform               =>    Transform0D,            &
                                                                                                          Transform1D
  procedure, private                                                  ::    GetLabel0D
  procedure, private                                                  ::    GetLabel1D
  procedure, public                                                   ::    GetNbInputs
  procedure(Transform0D_Input), deferred, public                      ::    Transform0D
  procedure(Transform1D_Input), deferred, public                      ::    Transform1D
  procedure(Initialize_Input), deferred, public                       ::    Initialize
  procedure(Reset_Input), deferred, public                            ::    Reset
  procedure(SetDefaults_Input), deferred, public                      ::    SetDefaults
  procedure(Copy_Input), deferred, public                             ::    Copy
end type

logical, parameter                                                    ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_Input( This )
    import                                                            ::    Input_Type
    class(Input_Type), intent(inout)                                  ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_Input( This )
    import                                                            ::    Input_Type
    class(Input_Type), intent(inout)                                  ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_Input( This )
    import                                                            ::    Input_Type
    class(Input_Type), intent(inout)                                  ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform0D_Input( This, Transformation, Label )
    import                                                            ::    Input_Type
    class(Input_Type), intent(inout)                                  ::    This
    character(*), intent(in)                                          ::    Transformation
    character(*), intent(in)                                          ::    Label
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform1D_Input( This, Transformations, Label )
    import                                                            ::    Input_Type
    import                                                            ::    String_Type
    class(Input_Type), intent(inout)                                  ::    This
    character(*), dimension(:), intent(in)                            ::    Transformations
    character(*), intent(in)                                          ::    Label
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_Input( LHS, RHS )
    import                                                            ::    Input_Type
    class(Input_Type), intent(out)                                    ::    LHS
    class(Input_Type), intent(in)                                     ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructEmpty( This )

    class(Input_Type), intent(inout)                                  ::    This
    
    character(*), parameter                                           ::    ProcName='ConstructEmpty'
    integer                                                           ::    StatLoc=0

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    This%NbInputs = 0

    allocate(This%Label(0), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbInputs( This )

    integer                                                           ::    GetNbInputs

    class(Input_Type), intent(in)                                     ::    This
    
    character(*), parameter                                           ::    ProcName='GetNbInputs'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object not constructed', ProcName=ProcName )

    GetNbInputs = This%NbInputs

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel0D( This, Num )

    character(:), allocatable                                         ::    GetLabel0D

    class(Input_Type), intent(in)                                     ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetLabel0D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetLabel0D = This%Label(Num)%GetValue()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel1D( This )

    type(String_Type), allocatable, dimension(:)                      ::    GetLabel1D
    class(Input_Type), intent(in)                                     ::    This

    character(*), parameter                                           ::    ProcName='GetLabel1D'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetLabel1D, source=This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetLabel1D', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
