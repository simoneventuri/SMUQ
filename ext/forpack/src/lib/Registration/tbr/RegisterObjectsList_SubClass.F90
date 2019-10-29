SubModule(Registration_Class) Registration_RegisterObjectsList_SubClass

  implicit none

  contains

Module Procedure AddToRegisterObjectsList
  logical                                                               ::  AddObjectToList
  integer                                                               ::  i
  type(SelfRegisterContainer_Type)  ,allocatable                        ::  List(:)
  if ( .Not. allocated(This%Items) ) allocate( This%Items(0) )
  This%N  =   size(This%Items)
  AddObjectToList   =   .True.
  do i = 1,This%N
    if ( .Not. Same_Type_As(Object,This%Items(i)%O) ) cycle
    AddObjectToList =   .False.
    exit
  end do
  if ( .Not. AddObjectToList ) return
  allocate( List(This%N+1) )
  do i = 1,This%N
    allocate( List(i)%O , source = This%Items(i)%O )
  end do
  i         =   This%N+1
  allocate( List(i)%O , source = Object )
  call move_alloc( List, This%Items )
  This%N  =   size(This%Items)
End Procedure

Module Procedure OutputRegisterObjectsListToLogger
  use Logger_Class              ,only:  Logger
  character(:)  ,allocatable                                            ::  String(:)
  integer                                                               ::  i
  call This%Output( String )
!   do i = 1,size(String)
!     call Logger%Write( trim(String(i)), LogLevel=0  )
!   end do
End Procedure

Module Procedure OutputRegisterObjectsListToString
  use Logger_Class              ,only:  Logger
  use String_Library            ,only:  Convert_Ratio
  integer                                                               ::  i, NameLength, CategoryLength
  character(:)  ,allocatable                                            ::  Name, Category
  call Logger%Write( "List of self-registrating objects" )
  call Logger%Write( "-> This%N = ", This%N )
  NameLength      =   0
  CategoryLength  =   0
  do i = 1,This%N
    NameLength      =   max(NameLength,len_trim(This%Items(i)%O%GetName()) )
    CategoryLength  =   max(NameLength,len_trim(This%Items(i)%O%GetCategory()) )
  end do
  allocate( Character(NameLength) :: Name )
  allocate( Character(CategoryLength) :: Category )

  do i = 1,This%N
  associate( Object => This%Items(i)%O )
    Name(:)     =   Object%GetName()
    Category(:) =   Object%GetCategory()
    call Logger%Write(                                  &
            "-> i = "       , Convert_Ratio(i,This%N) , &
            "Name = "       , Name                    , &
            "Category = "   , Category                , &
            "Description = ", Object%GetDesc()          )
  end associate
  end do
End Procedure

Module Procedure NumberOfRegistratingObjects
  NItems    =   This%N
End Procedure

Module Procedure GetRegistratingObjectsProperties
  use Utilities_Library         ,only:  SetOptArg, PresentAndNotEmpty
  use String_Library            ,only:  Equal
  integer                                                               ::  i
  integer                                                               ::  Number_
  integer                                                               ::  Length_
  integer                                                               ::  IdxMap_(This%N)
  character(:)  ,allocatable                                            ::  CurrentCategory
  Number_       =   0
  Length_       =   0
  IdxMap_       =   0
  do i = 1,This%NumberOfItems()
  associate( RegisteredObject => This%Items(i)%O )
    if ( PresentAndNotEmpty(Category) ) then
      if (.Not.Equal(Category,RegisteredObject%GetCategory(),Trimed=.True.,CaseSensitive=.False.)) cycle
    end if
    Number_     =   Number_ + 1
    Length_     =   max( Length_ , len_trim(RegisteredObject%GetName()) )
    IdxMap_(i)  =   i
  end associate
  end do
  call SetOptArg( Number_, Number )
  call SetOptArg( Length_, Length )
  if ( present(IdxMap) ) allocate( IdxMap, source = pack(IdxMap_,IdxMap_>0) )
End Procedure

End SubModule