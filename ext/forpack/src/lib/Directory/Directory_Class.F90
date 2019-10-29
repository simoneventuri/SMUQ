Module Directory_Class

  use Object_Class      ,only:  Object_Type

  implicit none

  private
  public    ::  Directory_Type

  Type  ,extends(Object_Type)       ::  Directory_Type
    logical                         ::  Initialized = .False.
    character(:)      ,allocatable  ::  Name
    character(:)      ,allocatable  ::  Path
    character(:)      ,allocatable  ::  Key
    character(:)      ,allocatable  ::  PublicName
    character(:)      ,allocatable  ::  Description
  contains
    private
    procedure ,public   ::  Initialize    =>  InitializeDirectory
    procedure ,public   ::  Clear         =>  ClearDirectory
    procedure ,public   ::  SetPublicName =>  SetDirectoryPublicName
    procedure ,public   ::  Substitute    =>  SubstituteDirectoryPath
    procedure ,public   ::  Output        =>  OuputDirectory
  End Type

  Interface           Directory_Type
    Module Procedure  DirectoryConstructor
  End Interface

  Interface
    Module Function DirectoryConstructor( FullPath, PublicName, Key, Description ) result(This)
      character(*)                                          ,intent(in)     ::  FullPath
      character(*)                                ,optional ,intent(in)     ::  PublicName
      character(*)                                ,optional ,intent(in)     ::  Key
      character(*)                                ,optional ,intent(in)     ::  Description
      type(Directory_Type)                                                  ::  This
    End Function
    Module Subroutine InitializeDirectory( This, FullPath, PublicName, Key, Description )
      class(Directory_Type)                                 ,intent(out)    ::  This
      character(*)                                          ,intent(in)     ::  FullPath
      character(*)                                ,optional ,intent(in)     ::  PublicName
      character(*)                                ,optional ,intent(in)     ::  Key
      character(*)                                ,optional ,intent(in)     ::  Description
    End Subroutine
    Pure Module Subroutine ClearDirectory( This )
      class(Directory_Type)                                 ,intent(inout)  ::  This
    End Subroutine
    Pure Module Subroutine SetDirectoryPublicName( This, PublicName )
      class(Directory_Type)                                 ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  PublicName
    End Subroutine
    Pure Module Function SubstituteDirectoryPath( This, InputString ) result(OutputString)
      class(Directory_Type)                                 ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  InputString
      character(:)  ,allocatable                                            ::  OutputString
    End Function
    Pure Module Function OuputDirectory( This, LengthDescription, LengthKey ) result(String)
      class(Directory_Type)                                 ,intent(in)     ::  This
      integer                                     ,optional ,intent(in)     ::  LengthDescription
      integer                                     ,optional ,intent(in)     ::  LengthKey
      character(:)  ,allocatable                                            ::  String
    End Function
  End Interface

End Module