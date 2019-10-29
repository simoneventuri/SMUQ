! Module RegisterObjectsList_Class
!
!   use Object_Class              ,only:  Object_Type
!   use RegisterObject_Class      ,only:  RegisterObject_Type
!
!   implicit none
!
!   private
!   public  ::  RegisterObjectsList_Type
!
!   Type  ::  SelfRegisterContainer_Type
!     class(RegisterObject_Type)  ,allocatable  ::  O
!   End Type
!
!   Type  ,extends(Object_Type)  ::  RegisterObjectsList_Type
!     integer                                         ::  N = 0
!     type(SelfRegisterContainer_Type)  ,allocatable  ::  Items(:)
!   contains
!     private
!     procedure ,public   ::  Add           =>  AddToRegisterObjectsList
!     procedure ,public   ::  NumberOfItems =>  NumberOfRegistratingObjects
!     procedure ,public   ::  GetProperties =>  GetRegistratingObjectsProperties
!     generic   ,public   ::  Output        =>  OutputRegisterObjectsListToLogger, OutputRegisterObjectsListToString
!     procedure           ::  OutputRegisterObjectsListToLogger
!     procedure           ::  OutputRegisterObjectsListToString
!   End Type
!
!   Interface
!
!     Module Subroutine AddToRegisterObjectsList( This, Object )
!       class(RegisterObjectsList_Type)                       ,intent(inout)  ::  This
!       class(RegisterObject_Type)                            ,intent(in)     ::  Object
!     End Subroutine
!
!     Module Subroutine OutputRegisterObjectsListToLogger( This )
!       class(RegisterObjectsList_Type)                       ,intent(in)     ::  This
!     End Subroutine
!
!     Module Subroutine OutputRegisterObjectsListToString( This, String )
!       class(RegisterObjectsList_Type)                       ,intent(in)     ::  This
!       character(:)  ,allocatable                            ,intent(out)    ::  String(:)
!     End Subroutine
!
!     Module Pure Function NumberOfRegistratingObjects( This ) result(NItems)
!       class(RegisterObjectsList_Type)                       ,intent(in)     ::  This
!       integer                                                               ::  NItems
!     End Function
!
! !     Pure
!     Module Subroutine GetRegistratingObjectsProperties( This, Category, Object, Length, Number, IdxMap )
!       class(RegisterObjectsList_Type)                       ,intent(in)     ::  This
!       character(*)                                ,optional ,intent(in)     ::  Category
!       class(RegisterObject_Type)                  ,optional ,intent(in)     ::  Object
!       integer                                     ,optional ,intent(out)    ::  Length
!       integer                                     ,optional ,intent(out)    ::  Number
!       integer ,allocatable                        ,optional ,intent(out)    ::  IdxMap(:)
!     End Subroutine
!
!   End Interface
!
! End Module