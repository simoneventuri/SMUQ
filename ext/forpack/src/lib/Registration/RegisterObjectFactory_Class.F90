Module RegisterObjectFactory_Class

  use Object_Class              ,only:  Object_Type
  use Registration_Class        ,only:  RegisterObject_Type, RegisterList_Type

  implicit none

  private
  public  ::  RegisterObjectFactory_Type

  Type  ,extends(Object_Type) ,abstract ::  RegisterObjectFactory_Type
    logical                 ::  BuildInObjectLoaded = .False.
    type(RegisterList_Type) ::  List
  contains
    procedure(GetCategory),nopass ,deferred   ::  GetCategory
    procedure                                 ::  LoadBuildins
    procedure ,non_overridable                ::  LoadPlugins
    procedure ,non_overridable                ::  LoadObjects
    procedure ,non_overridable                ::  Output      =>  OutputRegisterObjectFactory
    procedure                                 ::  BuildBase   =>  BuildRegisterObject
  End Type

  Abstract Interface

    Pure Function GetCategory() result(Category)
      character(:)  ,allocatable                                            ::  Category
    End Function

  End Interface

  Interface

    Module Subroutine LoadObjects( This, LogLevel )
      class(RegisterObjectFactory_Type)                     ,intent(inout)  ::  This
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine LoadBuildins( This, LogLevel )
      class(RegisterObjectFactory_Type)                     ,intent(inout)  ::  This
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine LoadPlugins( This, LogLevel )
      class(RegisterObjectFactory_Type)                     ,intent(inout)  ::  This
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine OutputRegisterObjectFactory( This )
      class(RegisterObjectFactory_Type)                     ,intent(in)     ::  This
    End Subroutine

    Module Subroutine BuildRegisterObject( This, Input, Object, LogLevel )
      class(RegisterObjectFactory_Type)                     ,intent(in)     ::  This
      class(*)                                              ,intent(in)     ::  Input
      class(RegisterObject_Type)  ,allocatable              ,intent(out)    ::  Object
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

  End Interface

End Module