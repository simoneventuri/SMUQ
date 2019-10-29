SubModule(URL_CLass) URL_SubCLass

  use Logger_Class        ,only:  Logger

  implicit none

  contains

Module Procedure InitializeURL
  if ( present(Name) ) This%Name = Name
  if ( present(Root) ) This%Root = Root
End Procedure

Module Procedure AddParamURL_CHAR
  use Utilities_Library   ,only:  AddElementToArray
  integer                                                               ::  Length
  character(:)  ,allocatable                                            ::  NewParam(:)
  Length    =   max(len(Name),len(Value))
  NewParam  =   [ Character(Length) :: Name, Value ]
  call AddElementToArray( NewParam, This%Param, dim=2 )
End Procedure

Module Procedure AddParamURL_INT32
  use String_Library      ,only:  Convert_To_String
  call This%AddParam( Name, Convert_To_String(Value) )
End Procedure

Module Procedure AddParamURL_LOGICAL
  use String_Library      ,only:  Convert_To_String
  call This%AddParam( Name, Convert_To_String( Value, T="1", F="0" ) )
!   if (Value) then;  call This%AddParam( Name, "1" )
!   else;             call This%AddParam( Name, "0" );  end if
End Procedure


Module Procedure GetURL
  integer                                                               ::  i
  character(1)                                                          ::  Sep
  URL   =   This%Root
  Sep   =   "?"
  do i = 1,size(This%Param,2)
    URL =   URL //Sep//trim(This%Param(1,i))//"="//trim(This%Param(2,i))
    Sep =   "&"
  end do
End Procedure

End SubModule