Module Registration_Class

! This file defines 3 public entities:
! * the derived-type "RegisterObject_Type"
! * the derived-type "RegisterList_Type"
! * the object "RegisterObjectsList"
! Note that the 2 derived-types need to be defined in the same module.

  use Object_Class            ,only:  Object_Type

  implicit none

  private
  public  ::  RegisterObject_Type
  public  ::  RegisterList_Type
  public  ::  RegisterObjectsList

  Type  ,extends(Object_Type)   ,abstract ::  RegisterObject_Type
  contains
    private
    procedure ,non_overridable                ,public ::  Register      =>  RegisterRegisterObject
    procedure ,non_overridable                ,public ::  Output        =>  OutputRegisterObjectToString
    procedure                                 ,public ::  GetName       =>  GetRegisterObjectName
    procedure                                 ,public ::  GetDescription=>  GetRegisterObjectDescription
    procedure                                 ,public ::  GetInputKeys  =>  GetRegisterObjectInputKeys
    procedure                                 ,public ::  GetAuthor     =>  GetRegisterObjectAuthor
    procedure                                 ,public ::  GetHelp       =>  GetRegisterObjectHelp
    procedure                                 ,public ::  Identified    =>  IdentifiedRegisterObject
    procedure               ,nopass           ,public ::  GetProperties =>  GetPropertiesRegisterObject
    procedure(GetCategory)  ,nopass ,deferred ,public ::  GetCategory
  End Type

  Type  ::  SelfRegisterContainer_Type
    class(RegisterObject_Type)  ,allocatable  ::  O
  End Type

  Type  ,extends(Object_Type)  ::  RegisterList_Type
    integer                                         ::  N = 0
    type(SelfRegisterContainer_Type)  ,allocatable  ::  Items(:)
  contains
    private
    procedure ,public   ::  Add           =>  AddToRegisterObjectsList
    procedure ,public   ::  NumberOfItems =>  NumberOfRegistratingObjects
    procedure ,public   ::  GetProperties =>  GetRegistratingObjectsProperties
    generic   ,public   ::  Output        =>  OutputRegisterObjectsListToLogger, OutputRegisterObjectsListToString
    procedure           ::  OutputRegisterObjectsListToLogger
    procedure           ::  OutputRegisterObjectsListToString
  End Type

  type(RegisterList_Type)  :: RegisterObjectsList

  ! =======================================================
  !    Interfaces for object: RegisterObject_Type
  ! =======================================================
  Abstract Interface

    Pure Function GetCategory() result(Category)
      character(:)  ,allocatable                                            ::  Category
    End Function

    Function Identification( This, Input, LogLevel ) result(Identified)
      import  RegisterObject_Type
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      class(*)                                              ,intent(in)     ::  Input
      integer                                     ,optional ,intent(in)     ::  LogLevel
      logical                                                               ::  Identified
    End Function

  End Interface

  Interface

    Module Subroutine RegisterRegisterObject( This, List, LogLevel )
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      type(RegisterList_Type)                     ,optional ,intent(inout)  ::  List
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine OutputRegisterObjectToString( This, Strings )
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                            ,intent(out)    ::  Strings(:)
    End Subroutine


    Module Pure Subroutine GetPropertiesRegisterObject( Name, InputKeys, Author, Description, Help )
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Name
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  InputKeys(:)
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Author
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Description
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Help(:)
    End Subroutine

    Module Pure Function GetRegisterObjectName( This ) result(Name)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Name
    End Function

    Module Pure Function GetRegisterObjectDescription( This ) result(Description)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Description
    End Function

    Module Pure Function GetRegisterObjectInputKeys( This ) result(InputKeys)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  InputKeys(:)
    End Function

    Module Pure Function GetRegisterObjectAuthor( This ) result(Author)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Author
    End Function

    Module Pure Function GetRegisterObjectHelp( This ) result(Help)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Help(:)
    End Function

    Module Function IdentifiedRegisterObject( This, Input, LogLevel ) result(Identified)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      class(*)                                              ,intent(in)     ::  Input
      integer                                     ,optional ,intent(in)     ::  LogLevel
      logical                                                               ::  Identified
    End Function

  End Interface


  ! =======================================================
  !    Interfaces for object: RegisterList_Type
  ! =======================================================
  Interface

    Module Subroutine AddToRegisterObjectsList( This, Object )
      class(RegisterList_Type)                              ,intent(inout)  ::  This
      class(RegisterObject_Type)                            ,intent(in)     ::  Object
    End Subroutine

    Module Subroutine OutputRegisterObjectsListToLogger( This )
      class(RegisterList_Type)                              ,intent(in)     ::  This
    End Subroutine

    Module Subroutine OutputRegisterObjectsListToString( This, String )
      class(RegisterList_Type)                              ,intent(in)     ::  This
      character(:)  ,allocatable                            ,intent(out)    ::  String(:)
    End Subroutine

    Module Pure Function NumberOfRegistratingObjects( This ) result(NItems)
      class(RegisterList_Type)                              ,intent(in)     ::  This
      integer                                                               ::  NItems
    End Function

!     Pure
    Module Subroutine GetRegistratingObjectsProperties( This, Category, Object, Length, Number, IdxMap )
      class(RegisterList_Type)                              ,intent(in)     ::  This
      character(*)                                ,optional ,intent(in)     ::  Category
      class(RegisterObject_Type)                  ,optional ,intent(in)     ::  Object
      integer                                     ,optional ,intent(out)    ::  Length
      integer                                     ,optional ,intent(out)    ::  Number
      integer ,allocatable                        ,optional ,intent(out)    ::  IdxMap(:)
    End Subroutine

  End Interface



End Module