module Output_Class

use Parameters_Library
use Logger_Class                  ,only: Logger
use Error_Class                   ,only: Error

implicit none

private

public                                                                ::    Output_Type

type                                                                  ::    Abscissa_Type
  integer                                                             ::    Size1=0
  real(rkp), dimension(:), pointer                                    ::    Val => null()
  character(:), allocatable                                           ::    Name
contains
  final                                                               ::    Finalizer_Abscissa
end type

type                                                                  ::    Ordinate_Type
  integer                                                             ::    Size1=0
  integer                                                             ::    Size2=0
  real(rkp), dimension(:,:), pointer                                  ::    Val => null()
  character(:), allocatable                                           ::    Name
contains
  final                                                               ::    Finalizer_Ordinate
end type

type                                                                  ::    Output_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Name
  character(:), allocatable                                           ::    Label
  
  type(Abscissa_Type)                                                 ::    Abscissa
  type(Ordinate_Type)                                                 ::    Ordinate
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1,         &
                                                                                                          ConstructCase2
  procedure, private                                                  ::    ConstructCase1
  procedure, private                                                  ::    ConstructCase2
  procedure, public                                                   ::    GetAbscissa
  procedure, public                                                   ::    GetAbscissaPointer
  procedure, public                                                   ::    GetOrdinate
  procedure, public                                                   ::    GetOrdinatePointer
  procedure, public                                                   ::    GetAbscissaSize
  procedure, public                                                   ::    GetAbscissaName
  procedure, public                                                   ::    GetOrdinateSize
  procedure, public                                                   ::    GetOrdinateNbDegen
  procedure, public                                                   ::    GetOrdinateName
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    GetLabel
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy

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
      This%Name = '<undefined>'
      call This%SetDefaults()
      if (DebugLoc) call Logger%Write( "Initialization Successful" )
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

    if ( associated(This%Abscissa%Val) ) deallocate( This%Abscissa%Val, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Abscissa%Val', ProcName=ProcName, stat=StatLoc )
    This%Abscissa%Size1 = 0

    if ( associated(This%Ordinate%Val) ) deallocate( This%Ordinate%Val, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Ordinate%Val', ProcName=ProcName, stat=StatLoc )
    This%Ordinate%Size1 = 0
    This%Ordinate%Size2 = 0

    call This%Initialize()
    if (DebugLoc) call Logger%Write( "Reset Successful" )

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

    This%Abscissa%Name = '<undefined>'
    This%Ordinate%Name = '<undefined>'
    This%Label = ''
    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------ 

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase1( This, Abscissa, Ordinate, AbscissaName, OrdinateName, Name, Label, Debug )

    class(Output_Type), intent(inout)                                 ::    This
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), dimension(:), intent(in)                               ::    Ordinate
    character(*), optional, intent(in)                                ::    AbscissaName
    character(*), optional, intent(in)                                ::    OrdinateName
    character(*), optional, intent(in)                                ::    Name
    character(*), optional, intent(in)                                ::    Label
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( size(Abscissa,1) /= size(Ordinate,1) ) call Error%Raise( Line="Incompatible sizes of Abscissa and Ordinate Arrays",      &
                                                                                                               ProcName=ProcName )

    if ( This%Constructed ) then
      if ( (This%Abscissa%Size1/=size(Abscissa,1)).or.(This%Ordinate%Size1/=size(Ordinate,1)).or.(This%Ordinate%Size2/=1) ) then
        call This%Reset()
      else
        This%Abscissa%Val = Abscissa
        This%Ordinate%Val(:,1) = Ordinate(:)
      end if
    end if

    if ( .not. This%Initialized ) call This%Initialize()

    if ( .not. This%Constructed ) then
      allocate( This%Abscissa%Val, source=Abscissa, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Abscissa%Val', ProcName=ProcName, stat=StatLoc )
      This%Abscissa%Size1 = size(This%Abscissa%Val,1)
      allocate( This%Ordinate%Val(size(Ordinate,1),1), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Ordinate%Val', ProcName=ProcName, stat=StatLoc )
      This%Ordinate%Val(:,1) = Ordinate
      This%Ordinate%Size1 = size(This%Ordinate%Val,1)
      This%Ordinate%Size2 = size(This%Ordinate%Val,2)
    end if

    if ( present(Name) ) This%Name = Name
    if ( present(Label) ) This%Label = Label
    if ( present(AbscissaName) ) This%Abscissa%Name = AbscissaName
    if ( present(OrdinateName) ) This%Ordinate%Name = OrdinateName

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructCase2( This, Abscissa, Ordinate, AbscissaName, OrdinateName, Name, Label, Debug )

    class(Output_Type), intent(inout)                                 ::    This
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), dimension(:,:), intent(in)                             ::    Ordinate
    character(*), optional, intent(in)                                ::    AbscissaName
    character(*), optional, intent(in)                                ::    OrdinateName
    character(*), optional, intent(in)                                ::    Name
    character(*), optional, intent(in)                                ::    Label
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( size(Abscissa,1) /= size(Ordinate,1) ) call Error%Raise( Line="Incompatible sizes of Abscissa and Ordinate Arrays",      &
                                                                                                               ProcName=ProcName )

    if ( This%Constructed ) then
      if ( (This%Abscissa%Size1/=size(Abscissa,1)).or.(This%Ordinate%Size1/=size(Ordinate,1)).or.                                 &
                                                                                    (This%Ordinate%Size2/=size(Ordinate,2)) ) then
        call This%Reset()
      else
        This%Abscissa%Val(:) = Abscissa(:)
        This%Ordinate%Val(:,:) = Ordinate(:,:)
      end if
    end if

    if ( .not. This%Initialized ) call This%Initialize()

    if ( .not. This%Constructed ) then
      allocate( This%Abscissa%Val, source=Abscissa, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Abscissa%Val', ProcName=ProcName, stat=StatLoc )
      This%Abscissa%Size1 = size(This%Abscissa%Val,1)
      allocate( This%Ordinate%Val, source=Ordinate, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Ordinate%Val', ProcName=ProcName, stat=StatLoc )
      This%Ordinate%Size1 = size(This%Ordinate%Val,1)
      This%Ordinate%Size2 = size(This%Ordinate%Val,2)
    end if

    if ( present(Name) ) This%Name = Name
    if ( present(Label) ) This%Label = Label
    if ( present(AbscissaName) ) This%Abscissa%Name = AbscissaName
    if ( present(OrdinateName) ) This%Ordinate%Name = OrdinateName

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetAbscissa( This, Debug )

    real(rkp), dimension(:), allocatable                              ::    GetAbscissa

    class(Output_Type), intent(inout)                                 ::    This
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetAbscissa'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )        
   
    allocate( GetAbscissa, source=This%Abscissa%Val, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetAbscissa', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetAbscissaPointer( This, Debug )

    real(rkp), dimension(:), pointer                                  ::    GetAbscissaPointer

    class(Output_Type), intent(in)                                    ::    This
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetAbscissaPointer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )        

    GetAbscissaPointer => This%Abscissa%Val

    if (DebugLoc) call Logger%Exiting

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

   !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetOrdinate( This, Debug )

    real(rkp), dimension(:,:), allocatable                            ::    GetOrdinate

    class(Output_Type), intent(inout)                                 ::    This
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetOrdinate'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )        
   
    allocate( GetOrdinate, source=This%Ordinate%Val, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetOrdinate', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetOrdinatePointer( This, Debug )

    real(rkp), dimension(:,:), pointer                                ::    GetOrdinatePointer

    class(Output_Type), intent(in)                                    ::    This
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='GetOrdinatePointer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetOrdinatePointer => This%Ordinate%Val

    if (DebugLoc) call Logger%Exiting

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetAbscissaSize( This, Debug )

    integer                                                           ::    GetAbscissaSize

    class(Output_Type), intent(in)                                    ::    This 
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetAbscissaSize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    
    GetAbscissaSize = This%Abscissa%Size1

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetAbscissaName( This, Debug )

    character(:), allocatable                                         ::    GetAbscissaName

    class(Output_Type), intent(in)                                    ::    This 
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetAbscissaName'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetAbscissaName = This%Abscissa%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetOrdinateSize( This, Debug )

    integer                                                           ::    GetOrdinateSize

    class(Output_Type), intent(in)                                    ::    This 
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetOrdinateSize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    
    GetOrdinateSize = This%Ordinate%Size1

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetOrdinatenbDegen( This, Debug )

    integer                                                           ::    GetOrdinateNbDegen

    class(Output_Type), intent(in)                                    ::    This 
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetOrdinateNbDegen'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )
    
    GetOrdinateNbDegen = This%Ordinate%Size2

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetOrdinateName( This, Debug )

    character(:), allocatable                                         ::    GetOrdinateName

    class(Output_Type), intent(in)                                    ::    This 
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetOrdinateName'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetOrdinateName = This%Ordinate%Name 

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName

    class(Output_Type), intent(in)                                    ::    This 
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetName = This%Name 

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
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
  !!----------------------------------------------------------------------------------------------------------------------------!!

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
      LHS%Abscissa%Name = RHS%Abscissa%Name
      LHS%Abscissa%Size1 = RHS%Abscissa%Size1
      allocate( LHS%Abscissa%Val, source=RHS%Abscissa%Val, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Abscissa%Val', ProcName=ProcName, stat=StatLoc )
      LHS%Ordinate%Name = RHS%Ordinate%Name
      LHS%Ordinate%Size1 = RHS%Ordinate%Size1
      LHS%Ordinate%Size2 = RHS%Ordinate%Size2
      allocate( LHS%Ordinate%Val, source=RHS%Ordinate%Val, stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Ordinate%Val', ProcName=ProcName, stat=StatLoc )
    end if

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer_Abscissa( This )

    type(Abscissa_Type),intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%Val) ) deallocate(This%Val, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Val', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer_Ordinate( This )

    type(Ordinate_Type),intent(inout)                                 ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%Val) ) deallocate(This%Val, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Val', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
