Module RegisterObject_Class

! The derived-type definitions for "RegisterObject_Type" and "RegisterObject_Type"
! need to be in the same fiile

  use Object_Class            ,only:  Object_Type

  implicit none

  private
  public  ::  RegisterObject_Type
  public  ::  RegisterObjectsList_Type
  public  ::  RegisterObjectsList

  Type  ,extends(Object_Type)   ,abstract ::  RegisterObject_Type
    character(:)  ,allocatable  ::  Category
    character(:)  ,allocatable  ::  InputKeys(:)
  contains
    private
    procedure ,non_overridable                ,public ::  Register      =>  RegisterRegisterObject
    procedure ,non_overridable                ,public ::  GetName       =>  GetRegisterObjectName
    procedure ,non_overridable                ,public ::  GetDesc       =>  GetRegisterObjectDescription
    procedure ,non_overridable                ,public ::  GetInputKeys  =>  GetRegisterObjectInputKeys
    procedure ,non_overridable                ,public ::  GetAuthor     =>  GetRegisterObjectAuthor
    procedure ,non_overridable                ,public ::  GetVersion    =>  GetRegisterObjectVersion
    procedure ,non_overridable                ,public ::  GetInstitution=>  GetRegisterObjectInstitution
    procedure ,non_overridable                ,public ::  GetHelp       =>  GetRegisterObjectHelp
    procedure(Identification)       ,deferred ,public ::  Identified
    procedure(Getting)      ,nopass ,deferred ,public ::  GetProperties
    procedure(GetCategory)  ,nopass ,deferred ,public ::  GetCategory
  End Type

  Type  ::  SelfRegisterContainer_Type
    class(RegisterObject_Type)  ,allocatable  ::  O
  End Type

  Type  ,extends(Object_Type)  ::  RegisterObjectsList_Type
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

  type(RegisterObjectsList_Type)  :: RegisterObjectsList

  ! =======================================================
  !    Interfaces for object: RegisterObject_Type
  ! =======================================================
  Abstract Interface

    Pure Subroutine Getting( Name, Desc, Category, InputKeys, Author, Version, Institution, Help )
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Name
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Desc
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Category
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  InputKeys(:)
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Author
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Version
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Institution
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Help(:)
    End Subroutine

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

    Module Subroutine RegisterRegisterObject( This, LogLevel )
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Pure Function GetRegisterObjectName( This ) result(Name)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Name
    End Function
!
!     Module Pure Function GetRegisterObjectCategory( This ) result(Category)
!       class(RegisterObject_Type)                            ,intent(in)     ::  This
!       character(:)  ,allocatable                                            ::  Category
!     End Function

    Module Pure Function GetRegisterObjectDescription( This ) result(Desc)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Desc
    End Function

    Module Pure Function GetRegisterObjectInputKeys( This ) result(InputKeys)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  InputKeys(:)
    End Function

    Module Pure Function GetRegisterObjectAuthor( This ) result(Author)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Author
    End Function

    Module Pure Function GetRegisterObjectVersion( This ) result(Version)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Version
    End Function

    Module Pure Function GetRegisterObjectInstitution( This ) result(Institution)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Institution
    End Function

    Module Pure Function GetRegisterObjectHelp( This ) result(Help)
      class(RegisterObject_Type)                            ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Help(:)
    End Function

  End Interface


  ! =======================================================
  !    Interfaces for object: RegisterObjectsList_Type
  ! =======================================================
  Interface

    Module Subroutine AddToRegisterObjectsList( This, Object )
      class(RegisterObjectsList_Type)                       ,intent(inout)  ::  This
      class(RegisterObject_Type)                            ,intent(in)     ::  Object
    End Subroutine

    Module Subroutine OutputRegisterObjectsListToLogger( This )
      class(RegisterObjectsList_Type)                       ,intent(in)     ::  This
    End Subroutine

    Module Subroutine OutputRegisterObjectsListToString( This, String )
      class(RegisterObjectsList_Type)                       ,intent(in)     ::  This
      character(:)  ,allocatable                            ,intent(out)    ::  String(:)
    End Subroutine

    Module Pure Function NumberOfRegistratingObjects( This ) result(NItems)
      class(RegisterObjectsList_Type)                       ,intent(in)     ::  This
      integer                                                               ::  NItems
    End Function

!     Pure
    Module Subroutine GetRegistratingObjectsProperties( This, Category, Object, Length, Number, IdxMap )
      class(RegisterObjectsList_Type)                       ,intent(in)     ::  This
      character(*)                                ,optional ,intent(in)     ::  Category
      class(RegisterObject_Type)                  ,optional ,intent(in)     ::  Object
      integer                                     ,optional ,intent(out)    ::  Length
      integer                                     ,optional ,intent(out)    ::  Number
      integer ,allocatable                        ,optional ,intent(out)    ::  IdxMap(:)
    End Subroutine

  End Interface



End Module