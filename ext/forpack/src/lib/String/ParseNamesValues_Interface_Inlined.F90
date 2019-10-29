! File:         ParseNamesValues_Interface_Inlined.F90
! Included in:  String_Module.F90
! Required definitions:
!     _VarType_   real(REAL32)  real(REAL64)  ...
!     _VarKind_   REAL32        REAL64        ...
! ------------------------------------------------
    Module Subroutine CONCAT(ParseNamesValues_,_VarKind_)( Strings, Names, Values, Separator, DefVal )
      character(*)                                          ,intent(in)     ::  Strings(:)
      character(:)  ,allocatable                            ,intent(out)    ::  Names(:)
      _VarType_     ,allocatable                            ,intent(out)    ::  Values(:)
      character(*)                                ,optional ,intent(in)     ::  Separator
      _VarType_                                   ,optional ,intent(in)     ::  DefVal
    End Subroutine
# undef   _VarType_
# undef   _VarKind_
! ------------------------------------------------