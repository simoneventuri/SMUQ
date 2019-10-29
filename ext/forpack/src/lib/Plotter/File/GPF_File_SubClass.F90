SubModule(GPF_File_Class) GPF_File_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure InitializeFile

  use GPF_TermFactory_Class     ,only:  GPF_TermFactory_Type

  character(*)                                              ,parameter  ::  ProcName = "InitializeFile"
  logical                                                               ::  Dbg
  type(GPF_TermFactory_Type)                                            ::  Factory

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling This%Output%Initialize" )
  call This%Output%Initialize( Name, Directory, HardCopy, Debug )

  if (Dbg) call Logger%Write( "Calling Factory%Build" )
  call Factory%Build( This%Term, This%Output%GetExtension(), Terminal, Debug )

  if (Dbg) call Logger%Write( "Calling Factory%Build" )
  call This%Term%Initialize(      &
          Color     =   Color     &
        , FontName  =   FontName  &
        , FontSize  =   FontSize  &
        , Enhanced  =   Enhanced  &
        , Extension =   This%Output%GetExtension() &
        , Title     =   This%Output%GetName()      &
!         , Dashed    =   Dashed    &
!         , Persist   =   Persist   &
        , Size      =   Size      &
        , Debug     =   Debug     )


  if (Dbg) call Logger%Write( "Calling Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure WriteFileCommands
  if ( len_trim(This%Command) /= 0 ) write(Unit,"(a)") This%Command
  call This%Term%Write(   Unit )
  call This%Output%Write( Unit )
End Procedure

Module Procedure Assign
  if ( allocated(File%Term) ) allocate( This%Term, source=File%Term )
  This%Output =   File%Output
End Procedure

Module Procedure GetFileExtension
  Extension   =   This%Output%GetExtension()
End Procedure

Module Procedure GetFileName
  FileName    =   This%Output%GetFileName()
End Procedure

Module Procedure GetDataFileName
  DataFile    =   This%Output%GetDataFileName()
End Procedure

Module Procedure GetCommandFileName
  CommandFile =   This%Output%GetCommandFileName()
End Procedure

Module Procedure GetFileDirectory
  Directory   =   This%Output%GetDirectory()
End Procedure

Module Procedure GetFileFullName
  use File_Library      ,only:  AddPathToFile
  FullName  =                           &
    AddPathToFile(                      &
      This%Output%GetDirectory()      , &
      This%Output%GetCommandFileName()  )
End Procedure

Module Procedure GetFileHardCopy
  HardCopy      =   This%Output%GetHardCopy()
End Procedure

Module Procedure SetFileCommand
  This%Command  =   '# File commands'
End Procedure

End SubModule