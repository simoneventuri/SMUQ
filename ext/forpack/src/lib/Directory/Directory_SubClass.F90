SubModule(Directory_Class) Directory_SubClass

  implicit none

  contains

Module Procedure DirectoryConstructor
  call This%Initialize( FullPath, PublicName, Key, Description )
End Procedure

Module Procedure InitializeDirectory
  use File_Library    ,only:    GetBaseName, CleanupPath
  integer   ::  N
  call This%Clear()
  This%Initialized  =   .True.
  This%Path         =   CleanupPath(FullPath)
!   This%Name         =   ""
  N                 =   len_trim(This%Path)
  if ( N /= 0 ) then
!     if ( This%Path(N:N) /= '/' ) This%Path = This%Path // '/'
    N               =   len_trim(This%Path) - 1
    This%Name       =   GetBaseName( This%Path(1:N) )
  end if
!   This%PublicName   =   ""
!   This%Key          =   ""
  if ( present(PublicName) )  This%PublicName = PublicName
  if ( present(Key) )         This%Key = Key
  if ( present(Description) ) This%Description = Description
End Procedure

Module Procedure ClearDirectory
  This%Initialized  =   .False.
  This%Name         =   ""
  This%Path         =   ""
  This%Key          =   ""
  This%PublicName   =   ""
  This%Description  =   ""
End Procedure

Module Procedure SetDirectoryPublicName
  This%PublicName    =   PublicName
End Procedure

Module Procedure SubstituteDirectoryPath
  use String_Library    ,only:  ReplaceCharacter
  OutputString  =   ReplaceCharacter( InputString, This%Path, This%PublicName )
End Procedure

Module Procedure OuputDirectory
  use String_Library    ,only:    SetLength
  character(:)  ,allocatable                                            ::  DescPart
  character(:)  ,allocatable                                            ::  PathPart
  if ( present(LengthDescription) ) then
    String  =   SetLength(trim(This%Description)//": ",LengthDescription)
  else
    String  =   trim(This%Description)//": "
  end if

  if ( present(LengthKey) ) then
    String  =   String // SetLength(trim(This%Key),LengthKey)
  else
    String  =   String // trim(This%Key)
  end if

  String    =   String // " = " // trim(This%Path)

!   String  =   SetLength(This%Description//": ",LengthDescription)   &
!           //  SetLength(This%Key,LengthKey)  //  " = " // trim(This%Path)
End Procedure

End SubModule