! File:         ParseNamesValues_Implementation_Inlined.F90
! Included in:  String_Parsing_SubModule.F90
! Required definitions:
!     _VarType_   real(REAL32)  real(REAL64)  ...
!     _VarKind_   REAL32        REAL64        ...
! ------------------------------------------------
Module Procedure CONCAT(ParseNamesValues_,_VarKind_)
  use Utilities_Library    ,only:  GetOptArgValue, AddElementToArray
  character(*)                                              ,parameter  ::  DefaultSeparator  = ":"
  _VarType_                                                 ,parameter  ::  DefaultValue      = 1 ! CONCAT(1.0_,_VarKind_)
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  List(:), Name, Separator_
  _VarType_                                                             ::  Value, DefVal_
  Separator_  =   GetOptArgValue(DefaultSeparator,Separator)
  DefVal_     =   GetOptArgValue(DefaultValue,DefVal)
  do i = 1,size(Strings)
    call Parse( Strings(i), Separator_, List )
    Name      =   trim(List(1))
    Value     =   DefVal_
!     if ( size(List) >= 2 ) Value = Convert_To_Real( List(2) )
    if ( size(List) >= 2 ) call Convert( List(2), Value )
    call AddElementToArray( Name,  Names  )
    call AddElementToArray( Value, Values )
  end do
End Procedure
# undef   _VarType_
# undef   _VarKind_
! ------------------------------------------------

