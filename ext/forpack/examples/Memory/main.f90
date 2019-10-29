! cd /home/blopez/Codes/ForPack/build/forpack-1.0-Serial-Debug-ifort-17.1/Memory/bin; ./MemoryExample.x

Module Universal_Class
  use Memory_Library    ,only:  Memory_Type
  implicit none
  private
  public  ::  Universal_Type
  Type    ::  Universal_Type
    type(Memory_Type)   ::    Memory
  End Type
End Module

Module SubObject_Class
  use Universal_Class    ,only:  Universal_Type
  implicit none
  private
  public  ::  SubObject_Type
  Type  ,extends(Universal_Type)    ::  SubObject_Type
    logical                 ,allocatable                  ::    Logi0D
    integer                 ,allocatable                  ::    Inte0D
    real(8)                 ,allocatable                  ::    Real0D
!     type(SubObject_Type)    ,allocatable                  ::    SubO0D
    character(:)            ,allocatable                  ::    Char0D
    logical                 ,allocatable  ,dimension(:)   ::    Logi1D
    integer                 ,allocatable  ,dimension(:)   ::    Inte1D
    real(8)                 ,allocatable  ,dimension(:)   ::    Real1D
!     type(SubObject_Type)    ,allocatable  ,dimension(:)   ::    SubO1D
    character(:)            ,allocatable  ,dimension(:)   ::    Char1D
    logical                 ,allocatable  ,dimension(:,:) ::    Logi2D
    integer                 ,allocatable  ,dimension(:,:) ::    Inte2D
    real(8)                 ,allocatable  ,dimension(:,:) ::    Real2D
!     type(SubObject_Type)    ,allocatable  ,dimension(:,:) ::    SubO2D
    character(:)            ,allocatable  ,dimension(:,:) ::    Char2D
  contains
    procedure ,public ::  Initialize
  End Type
  contains
Subroutine Initialize( This, Length, N1, N2 )
  class(SubObject_Type)    ,intent(out)  ::  This
  integer                 ,intent(in)   ::  Length, N1, N2
  allocate( This%Logi0D        )
  allocate( This%Inte0D        )
  allocate( This%Real0D        )
!   allocate( This%SubO0D        )
  allocate( character(Length) :: This%Char0D        )
  allocate( This%Logi1D(N1)    )
  allocate( This%Inte1D(N1)    )
  allocate( This%Real1D(N1)    )
!   allocate( This%SubO1D(N1)    )
  allocate( character(Length) :: This%Char1D(N1)    )
  allocate( This%Logi2D(N1,N2) )
  allocate( This%Inte2D(N1,N2) )
  allocate( This%Real2D(N1,N2) )
!   allocate( This%SubO2D(N1,N2) )
  allocate( character(Length) :: This%Char2D(N1,N2) )
End Subroutine
End Module

Module Object_Class
  use Universal_Class     ,only:  Universal_Type
  use SubObject_Class     ,only:  SubObject_Type
  implicit none
  private
  public  ::  Object_Type
  Type  ,extends(Universal_Type)    ::  Object_Type
    logical                 ,allocatable                  ::    Logi0D
    integer                 ,allocatable                  ::    Inte0D
    real(8)                 ,allocatable                  ::    Real0D
    type(SubObject_Type)    ,allocatable                  ::    SubO0D
    character(:)            ,allocatable                  ::    Char0D
    logical                 ,allocatable  ,dimension(:)   ::    Logi1D
    integer                 ,allocatable  ,dimension(:)   ::    Inte1D
    real(8)                 ,allocatable  ,dimension(:)   ::    Real1D
    type(SubObject_Type)    ,allocatable  ,dimension(:)   ::    SubO1D
    character(:)            ,allocatable  ,dimension(:)   ::    Char1D
    logical                 ,allocatable  ,dimension(:,:) ::    Logi2D
    integer                 ,allocatable  ,dimension(:,:) ::    Inte2D
    real(8)                 ,allocatable  ,dimension(:,:) ::    Real2D
    type(SubObject_Type)    ,allocatable  ,dimension(:,:) ::    SubO2D
    character(:)            ,allocatable  ,dimension(:,:) ::    Char2D
  contains
    procedure ,public ::  Initialize
!     procedure ,public ::  Finalize
!     Final             ::  FinalizeObject
  End Type
  contains
Subroutine Initialize( This, Length, N1, N2 )
  class(Object_Type)      ,intent(out)  ::  This
  integer                 ,intent(in)   ::  Length, N1, N2
  allocate( This%Logi0D        )
  allocate( This%Inte0D        )
  allocate( This%Real0D        )
  allocate( This%SubO0D        )
  allocate( character(Length) :: This%Char0D        )
  allocate( This%Logi1D(N1)    )
  allocate( This%Inte1D(N1)    )
  allocate( This%Real1D(N1)    )
  allocate( This%SubO1D(N1)    )
  allocate( character(Length) :: This%Char1D(N1)    )
  allocate( This%Logi2D(N1,N2) )
  allocate( This%Inte2D(N1,N2) )
  allocate( This%Real2D(N1,N2) )
  allocate( This%SubO2D(N1,N2) )
  allocate( character(Length) :: This%Char2D(N1,N2) )
End Subroutine
! Subroutine Finalize( This, Length, N1, N2 )
!   class(Object_Type)      ,intent(out)  ::  This
!   integer                 ,intent(in)   ::  Length, N1, N2
!   allocate( This%Logi0D        )
!   allocate( This%Inte0D        )
!   allocate( This%Real0D        )
!   allocate( This%SubO0D        )
!   allocate( character(Length) :: This%Char0D        )
!   allocate( This%Logi1D(N1)    )
!   allocate( This%Inte1D(N1)    )
!   allocate( This%Real1D(N1)    )
!   allocate( This%SubO1D(N1)    )
!   allocate( character(Length) :: This%Char1D(N1)    )
!   allocate( This%Logi2D(N1,N2) )
!   allocate( This%Inte2D(N1,N2) )
!   allocate( This%Real2D(N1,N2) )
!   allocate( This%SubO2D(N1,N2) )
!   allocate( character(Length) :: This%Char2D(N1,N2) )
! End Subroutine
! Subroutine FinalizeObject( This )
!   type(Object_Type)     ,intent(inout)  ::      This
! End Subroutine
End Module

Program MemoryExample

  use Logger_Class      ,only:  Logger
  use Memory_Library    ,only:  Memory_Type

  implicit none

  character(*)  ,parameter  ::    ProcName = 'MemoryExample'
  logical       ,parameter  ::    i_Debug_Loc = .True.

  if (i_Debug_Loc) call Logger%Entering( ProcName )

!   if (i_Debug_Loc) call Logger%Write( "Calling SimpleTest", NewLine=.True. )
!   call SimpleTest()

  if (i_Debug_Loc) call Logger%Write( "Calling AdvancedTest", NewLine=.True. )
  call AdvancedTest()

  if (i_Debug_Loc) call Logger%Exiting()

  contains

Subroutine SimpleTest()

  implicit none

  character(*)  ,parameter  ::    ProcName = 'SimpleTest'
  type(Memory_Type)         ::    Memory
  integer ,dimension(100)   ::    VectorInteger

  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Write( "Calling Memory%Initialize" )
  call Memory%Initialize( Type_Name="Memory_Type", Var_Name="Memory" )

  if (i_Debug_Loc) call Logger%Write( "Calling Memory%OutputToLogger" )
  call Memory%OutputToLogger()

  if (i_Debug_Loc) call Logger%Write( "Calling Memory%Add_Variable( VectorInteger )" )
  call Memory%Add_Variable( VectorInteger )

  if (i_Debug_Loc) call Logger%Write( "Calling Memory%OutputToLogger" )
  call Memory%OutputToLogger()

  if (i_Debug_Loc) call Logger%Exiting()

End Subroutine


Subroutine AdvancedTest()

  use Object_Class    ,only:  Object_Type

  implicit none

  character(*)  ,parameter  ::    ProcName = 'AdvancedTest'
  type(Object_Type)       ::    Object

  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Write( "Calling Object%Initialize" )
  call Object%Initialize( 100, 10, 5 )

        if (i_Debug_Loc) call Logger%Write( "Calling Object%SubO0D%Initialize" )
        call Object%SubO0D%Initialize( 100, 10, 5 )

        if (i_Debug_Loc) call Logger%Write( "Calling Object%SubO0D%Memory%Initialize" )
        call Object%SubO0D%Memory%Initialize( Type_Name="SubObject_Type", Var_Name="SubO0D" )

        if (i_Debug_Loc) call Logger%Write( "Calling Object%SubO0D%Memory%Add_Variable for each component of Object" )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Logi0D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Inte0D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Real0D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Char0D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Logi1D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Inte1D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Real1D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Char1D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Logi2D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Inte2D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Real2D )
        call Object%SubO0D%Memory%Add_Variable( Object%SubO0D%Char2D )




  if (i_Debug_Loc) call Logger%Write( "Calling Object%Memory%Initialize" )
  call Object%Memory%Initialize( Type_Name="Object_Type", Var_Name="Object" )

  if (i_Debug_Loc) call Logger%Write( "Calling Object%Memory%Add_Variable for each component of Object" )
  call Object%Memory%Add_Variable( Object%Logi0D )
  call Object%Memory%Add_Variable( Object%Inte0D )
  call Object%Memory%Add_Variable( Object%Real0D )
!   call Object%Memory%Add_Variable( Object%Subo0D, Name="Subo0D"  )
  call Add_To_Memory_U0( Object, Object%Subo0D, Name="Subo0D" )
  call Object%Memory%Add_Variable( Object%Char0D )
  call Object%Memory%Add_Variable( Object%Logi1D )
  call Object%Memory%Add_Variable( Object%Inte1D )
  call Object%Memory%Add_Variable( Object%Real1D )
  call Object%Memory%Add_Variable( Object%Char1D )
!   call Object%Memory%Add_Variable( Object%Subo1D )
  call Object%Memory%Add_Variable( Object%Logi2D )
  call Object%Memory%Add_Variable( Object%Inte2D )
  call Object%Memory%Add_Variable( Object%Real2D )
!   call Object%Memory%Add_Variable( Object%Subo2D )
  call Object%Memory%Add_Variable( Object%Char2D )

  if (i_Debug_Loc) call Logger%Write( "Calling Object%Memory%OutputToLogger" )
  call Object%Memory%OutputToLogger()

  if (i_Debug_Loc) call Logger%Write( "Calling Object%Memory%Write" )
  call Object%Memory%Write( Logger%Unit )

  if (i_Debug_Loc) call Logger%Exiting()

End Subroutine

Subroutine Add_To_Memory_U0( This, Var, Name, Update )

  use Memory_Class    ,only:  Addition_Memory_0d
  use Universal_Class ,only:  Universal_Type

  class(Universal_Type)                                         ,intent(inout)  ::      This                    !< Passed-object dummy argument
  class(Universal_Type)                                         ,intent(in)     ::      Var                     !< Variable whose storage size is to be added to the one of current object
  character(*)                                                  ,intent(in)     ::      Name                    !< Name of the derived-type variable to be added to the Memory object
  logical                                             ,optional ,intent(in)     ::      Update                  !< Indicator whether or not the Memory of the input variable 'Var' must be updated
  class(Universal_Type)         ,allocatable                                    ::      Object                  ! Temporary object associated to the input variable

  character(*)  ,parameter  ::    ProcName = 'Add_To_Memory_U0'

  if (i_Debug_Loc) call Logger%Entering( ProcName )

  allocate( Object, source = Var )                                                                              ! Copying the input object in order to update it if required without breaking the 'intent(in)' attribute
!   if ( Object%Set_Optional_Argument(.True.,Update) ) then
!     call Object%Set_Memory()                                   ! Setting the memory storage size of the temporary variable if required
!   end if
  if (i_Debug_Loc) call Logger%Write( "Object%Memory%Bits = ", Object%Memory%Bits )
  if ( Object%Memory%Bits == 0 ) then
    if (i_Debug_Loc) call Logger%Exiting()
    return                                                                  ! Exiting the procedure if the object has a zero storage size
  end if
  call Object%Memory%Set_Variable_Name( Name )                                                                  ! Setting the variable name

  if (i_Debug_Loc) call Logger%Write( "Calling Addition_Memory_0d" )
  This%Memory   =       Addition_Memory_0d(This%Memory , Object%Memory)
  deallocate( Object )

  if (i_Debug_Loc) call Logger%Exiting()

End Subroutine

End Program
