SubModule(DebuggerComponentsList_Class) DebuggerComponentsList_SubClass

  use Logger_Class              ,only:  Logger, LogLevel_NOLOGS, LogLevel_HEAVYDEBUG

  implicit none

  integer                                                   ,parameter  ::  NItemsBlock=10

  contains

! This procedure initialzies the list of components handle by the debugger.
! The array of component object containing the component names and indicator are
! allocated by blocks of 10 elements. This avoid frequent allocation/deallocation
! of the object component as element are added or removed from the list.
! The actual number of items stored in the list is tracked using the "NItems"
! variable.
Module Procedure InitializeComponentsList
  use String_Library            ,only:  Equal
  integer                                                               ::  i
  This%NItems   =   0
  allocate( This%Items(NItemsBlock) )
  if ( .Not. allocated(This%Items) ) return
  do i = 1,size(This%Items)
  associate( Component => This%Items(i) )
    Component%Name    =   ""
    Component%Active  =   .False.
  end associate
  end do
End Procedure

Module Procedure FindComponentIndex
  use String_Library            ,only:  Equal
  integer                                                               ::  i
  Idx   =   0
  if ( .Not. allocated(This%Items) ) return
  do i = 1,This%NItems
    if ( .Not. Equal(ComponentName,This%Items(i)%Name,CaseSensitive=.False.,Trimed=.True.) ) cycle
    Idx =   i
    return
  end do
End Procedure

! This procedure activates a component from its name. It the input component
! name matches a component name stored in the list, then it is activated by
! setting its indicator to true.
Module Procedure ActivateComponentFromName
  integer                                                               ::  i
  i   =   This%Find(ComponentName)
  if ( i /= 0 ) This%Items(i)%Active = .True.
End Procedure

! This procedure activates a component from its name. It the input component
! name matches a component name stored in the list, then it is activated by
! setting its indicator to false.
Module Procedure DeactivateComponentFromName
  integer                                                               ::  i
  i   =   This%Find(ComponentName)
  if ( i /= 0 ) This%Items(i)%Active = .False.
!   if ( i /= 0 ) call Logger%Write( "-> Deactivating component '"//This%Items(i)%Name//"' at position ", i )
End Procedure

! This procedure adds a component to the list.
! The component is only added if it is not already there.
Module Procedure AddComponemtFromName
  integer                                                               ::  i, NItemOld, NItemNew, NItemsMax, NBlocks
  type(DebuggerComponent_Type)                                          ::  Item
  type(DebuggerComponent_Type)  ,allocatable                            ::  List(:)
  if ( This%Find(ComponentName) /= 0 ) return ! Exit if component already in the list
  if ( .Not. allocated(This%Items) ) then
    allocate( This%Items(NItemsBlock) )
    This%NItems   =   0
  end if
  NItemOld    =   This%NItems
  NItemNew    =   This%NItems + 1
  if ( NItemNew > size(This%Items) ) then
    allocate( List(NItemNew) )
    List(1:NItemOld)   =   This%Items(1:NItemOld)
    List(NItemNew)   =   DebuggerComponent_Type(ComponentName,.True.)
    deallocate( This%Items )
    NBlocks   =   ceiling(real(NItemNew)/NItemsBlock)
    NItemsMax =   NBlocks * NItemsBlock
    allocate( This%Items(NItemsMax) )
    This%Items(1:NItemNew) = List(1:NItemNew)
    do i = NItemOld+2,size(This%Items)
      This%Items(i)%Name    =   ""
      This%Items(i)%Active  =   .False.
    end do
  else
    This%Items(NItemNew)    =   DebuggerComponent_Type(ComponentName,.True.)
  end if
  This%NItems   =   NItemNew
End Procedure

Module Procedure RemoveComponentFromName
  integer                                                               ::  Idx, i, j, NItemOld, NItemNew
  type(DebuggerComponent_Type)  ,allocatable                            ::  List(:)
  Idx   =   This%Find(ComponentName)
  if ( Idx == 0 ) return
  NItemOld    =   This%NItems
  NItemNew    =   This%NItems - 1
  This%NItems =   NItemNew
  allocate( List(NItemNew) )
  do i = 1,Idx-1
    j         =   i
    List(j)   =   This%Items(i)
  end do
  do i = Idx+1,NItemOld
    j         =   i - 1
    List(j)   =   This%Items(i)
  end do
  This%Items(1:NItemNew) = List(1:NItemNew)
  This%Items(NItemOld)%Name    =   ""
  This%Items(NItemOld)%Active  =   .False.
End Procedure

Module Procedure GetComponentNames
  use Utilities_Library     ,only:  PresentAndTrue
  integer                                                               ::  i, L, N
  if ( PresentAndTrue(OnlyActive) ) then
    L     =   0
    N     =   0
    do i = 1,This%NItems
      if ( .Not. This%Items(i)%Active ) cycle
      L   =   max( L , len_trim(This%Items(i)%Name) )
      N   =   N + 1
    end do
    allocate( Character(L) :: ComponentNames(N) )
    N     =   0
    do i = 1,This%NItems
      if ( .Not. This%Items(i)%Active ) cycle
      N   =   N + 1
      ComponentNames(N)   =   trim(This%Items(i)%Name)
    end do
  else
    L     =   0
    do i = 1,This%NItems
      L   =   max( L , len_trim(This%Items(i)%Name) )
    end do
    N   =   This%NItems
    allocate( Character(L) :: ComponentNames(N) )
    do i = 1,This%NItems
      ComponentNames(i)   =   trim(This%Items(i)%Name)
    end do
  end if
End Procedure

Module Procedure IncludesAnyInComponent
  use String_Library      ,only:  CountPresence
  integer                                                               ::  Number
!   call Logger%Write( "-> This%GetNames(OnlyActive=.True.) = ", This%GetNames(OnlyActive=.True.) )
  Number    =   sum( CountPresence(ComponentNames,This%GetNames(OnlyActive=.True.),Trimed=.True.,CaseSensitive=.False.) )
!   call Logger%Write( "-> i = ", i, "Name = ", C%Name, "Active = ", C%Active )
  Included  =   ( Number /= 0 )
End Procedure


Module Procedure WriteComponentsStatus
  character(*)                                              ,parameter  ::  ProcName='WriteComponentsStatus'
  integer                                                               ::  i
  call Logger%Entering( ProcName )
  do i = 1,This%NItems
  associate( C => This%Items(i) )
    call Logger%Write( "-> i = ", i, "Name = ", C%Name, "Active = ", C%Active )
  end associate
  end do
  call Logger%Exiting
End Procedure



End SubModule
