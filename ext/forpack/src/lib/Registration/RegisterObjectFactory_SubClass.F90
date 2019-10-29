SubModule(RegisterObjectFactory_Class) RegisterObjectFactory_SubClass

  use Logger_Class                    ,only:  Logger, LogLevel_NOLOGS, LogLevel_HEAVYDEBUG

  implicit none

  contains

! This will look in the local (ie. inside the Factory) list of self-registering objects to find the object
! to be allocated. All objects in this list are type-compatible with the object being created.
! Thus, there is no need to find the object category and only loop on the objects of a given category.
! All the objects in This%List are of the same category
Module Procedure BuildRegisterObject

  use String_Library                  ,only:  SetLength

  character(*)                                              ,parameter  ::  ProcName='BuildRegisterObject'
  logical                                                               ::  Identified
  integer                                                               ::  i, Length

!   call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_HEAVYDEBUG )

  Identified  =   .False.

!   call Logger%Write( "-> Calling This%List%GetProperties" )
  call This%List%GetProperties( Length=Length )
!   call Logger%Write( "-> Length = ", Length )

!   call Logger%Write( "Loop on objects known by factory: This%List%N = ", This%List%N )
  do i = 1,This%List%N
    associate( O => This%List%Items(i)%O )
      Identified  =   O%Identified(Input)
!       call Logger%Write( "-> i = ", i, "Name = ", SetLength(O%GetName(),Length), "Identified = ", Identified )
      if ( Identified ) then
        allocate( Object , source = O )
        exit
      end if
    end associate
  end do

  if ( allocated(Object) ) then
!     call Logger%Write( "Object of type '"//This%GetCategory()//"' found: Object%GetName = ", Object%GetName() )
  else
!     call Logger%Write( "No object of type '"//This%GetCategory()//"' found" )
! ! ! !     call Logger%Write( "-> Setting the default type" )
! ! ! !     if ( present(DefaultObject) ) then
! ! ! !       allocate( Object, source=DefaultObject )
! ! ! !       call Logger%Write( "Object of type '"//Category//"' found: Object%GetName = ", Object%GetName() )
! ! ! !     end if
  end if

!   call Logger%Exiting()

End Procedure


Module Procedure OutputRegisterObjectFactory
  call This%List%Output()
End Procedure



! This procedure loads the build-in objects into the factory.
! By default, no object are being loading since there are not yet known.
! This procedure need to be overrided by the concrete Factory object.
! If so, it must contains call to the Registration procedure of each object
! to be loaded into the factory.
! For example, if the concrete factory is building objects of the parent
! class "Animal_Type", then for adding a set of concrete animal objects to
! this fatcory one would need to call the "Register" procedure of all those
! objects inside this "LoadBuildins" procedure. Assuming we have the following
! objects extending the parent "Animal_Type" abstract object:
! * object of type "DogAnimal_Type" defined in the module "DogAnimal_Class"
! * object of type "CatAnimal_Type" defined in the module "CatAnimal_Class"
! * object of type "AntAnimal_Type" defined in the module "AntAnimal_Class"
! them the implementation of thsi "LoadBuildins" procedure will look like:
!       use DogAnimal_Class ,only: DogAnimal_Type
!       use CatAnimal_Class ,only: CatAnimal_Type
!       use AntAnimal_Class ,only: AntAnimal_Type
!       type(DogAnimal_Type) :: DogAnimal
!       type(CatAnimal_Type) :: CatAnimal
!       type(AntAnimal_Type) :: AntAnimal
!       call DogAnimal%Register( This%List, LogLevel=LogLevel )
!       call CatAnimal%Register( This%List, LogLevel=LogLevel )
!       call AntAnimal%Register( This%List, LogLevel=LogLevel )
! This "LoadBuildins" procedure is not made as a "deferred" procedure in order
! to allow objects to be only activated as plugin, without having any build-in
! concrete objects. In such case, having a non-deferred "LoadBuildins" procedure
! avoids the need to declaring an empty procedure in eahc concrete factory.
! To ensure that build-in objects are only loaded once, the "BuildInObjectLoaded"
! indicator need to be set to False inside this procedure. Its initial value is
! False and the "LoadBuildins" procedure is only called is LoadBuildins is True.
! Note, however, that there will be no issue if the build-in objects are being
! loading the the Factory several times.
Module Procedure LoadBuildins
  character(*)                                              ,parameter  ::  ProcName='LoadBuildins'
!   call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_HEAVYDEBUG )
!   call Logger%Write( "No build-in object avaialable" )
  This%BuildInObjectLoaded  = .True.
!   call Logger%Exiting()
End Procedure

! This procedure loads the plug-in objects into the factory. These plug-in
! objects are taken from the global list of self-registrating objects
! "RegisterObjectsList". Only the objects in this list whose category matches
! the category provided in input will be loading into the Factory list of objects.
Module Procedure LoadPlugins

  use Registration_Class              ,only:  RegisterObjectsList

  character(*)                                              ,parameter  ::  ProcName='LoadPlugins'
  integer                                                               ::  i, j, NumObject
  integer ,allocatable                                                  ::  IdxObject(:)

!   call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_HEAVYDEBUG )

!   call Logger%Write( "-> Calling RegisterObjectsList%GetProperties: This%GetCategory() = ", This%GetCategory() )
  call RegisterObjectsList%GetProperties(         &
              Category  =   This%GetCategory()  , &
              Number    =   NumObject           , &
              IdxMap    =   IdxObject             )
!   call Logger%Write( "-> Number of found object: NumObject = ", NumObject )
!   call Logger%Write( "-> Index  of found object: IdxObject = ", IdxObject )

!   call Logger%Write( "Loop on RegistratingObjects" )
  do j = 1,size(IdxObject)
    i         =   IdxObject(j)
    associate( Object => RegisterObjectsList%Items(i)%O )
!       call Logger%Write( "-> Calling This%List%Add: j = ", j, "Name = ", Object%GetName()  )
      call This%List%Add( Object )
    end associate
  end do

!   call Logger%Exiting()

End Procedure

! This procedure loads concrete objects into the current factory object.
! These objects corresponds to all the objects the Factory is able to build.
! There are two type of objects, the build-in and plug-in objects.
! * Build-in objects
!     The build-in objects are defined by the application code and need to be
!     known are compile type. Thise objects are loaded in the "LoadBuildins"
!     procedure which need to be defined by the concrete Factory object.
!     These build-in objects only need to be loaded once since there will not
!     changed at run-time so the "LoadBuildins" is only called if the value
!     of BuildInObjectLoaded is False.
! * Plug-in objects
!     The plug-in objects are defined after the application code has been
!     compiled. As a result, there are not known are compile-time. These
!     objects first loaded from a shared library and then added to a global
!     list of sefl-registrating objects. The "LoadPlugins" procedure then
!     pull out from this global list all the object which are being build
!     by the current Factory object. This is done based on the object category.
!     Since plug-in objects can be added at run-time, we want the loading of
!     these plug-in objects to happen eahc time the Factory is trying to build
!     an object so that new objects added at run-time are available to the Factory.
Module Procedure LoadObjects

  character(*)                                              ,parameter  ::  ProcName='LoadObjects'

!   call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_HEAVYDEBUG )

! ==============================================================================================================
!   LOADING BUILDIN OBJECTS INTO FACTORY
! ==============================================================================================================
  if ( .Not. This%BuildInObjectLoaded ) then
!     call Logger%Write( "Loading buildin objects into factory" )
!     call Logger%Write( "-> Calling This%LoadBuildins" )
    call This%LoadBuildins( LogLevel=LogLevel )
  end if
! ==============================================================================================================

! ==============================================================================================================
!    LOADING PLUGIN OBJECTS INTO FACTORY
! ==============================================================================================================
!   call Logger%Write( "Loading plugin objects into factory" )
!   call Logger%Write( "-> Calling This%LoadPlugins" )
  call This%LoadPlugins( LogLevel=LogLevel )
! ==============================================================================================================

!   call Logger%Exiting()

End Procedure

End SubModule