SubModule(SourceLocation_CLass) SourceLocation_SubCLass

  use Logger_Class        ,only:  Logger

  implicit none

  contains

Module Procedure InitializeSourceLocation
  if ( present(Name) ) This%Name = Name
  if ( present(Root) ) This%Root = Root
End Procedure

Module Procedure AddParamSourceLocation_CHAR
  use Utilities_Library   ,only:  AddElementToArray
  integer                                                               ::  Length
  character(:)  ,allocatable                                            ::  NewParam(:)
  Length    =   max(len(Name),len(Value))
  NewParam  =   [ Character(Length) :: Name, Value ]
  call AddElementToArray( NewParam, This%Param, dim=2 )
End Procedure

Module Procedure AddParamSourceLocation_INT32
  use String_Library      ,only:  Convert_To_String
  call This%AddParam( Name, Convert_To_String(Value) )
End Procedure

Module Procedure GetSourceLocation
  integer                                                               ::  i
  character(1)                                                          ::  Sep
  SourceLocation   =   This%Root
  Sep   =   "?"
  do i = 1,size(This%Param,2)
    SourceLocation =   SourceLocation //Sep//trim(This%Param(1,i))//"="//trim(This%Param(2,i))
    Sep =   "&"
  end do
End Procedure

End SubModule