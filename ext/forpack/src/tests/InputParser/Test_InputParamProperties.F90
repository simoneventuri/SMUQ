Module Test_InputParamProperties

! Include file needed for: STRINGIFY
# include "forpack-include.inc"

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength
  use Input_Library     ,only:  InputParamProperties_Type

  implicit none

  contains

@test
Subroutine Test_InputParamProperties_CheckValueValidity()

  use iso_fortran_env   ,only:  INT32, INT64, REAL32, REAL64, REAL128
  use Input_Library     ,only:  ConstructorInputParamProperties

  character(*)                                              ,parameter  ::  ProcName = "CheckValueValidity"
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  character(:)  ,allocatable                                            ::  sKin
  character(6)                                                          ::  Key
  character(2)                                                          ::  sOp1, sOp2
  character(1)                                                          ::  sVal, sBnd1, sBnd2
  class(*)      ,allocatable                                            ::  Value, Bnd1, Bnd2
  logical                                                               ::  Expected, Found
  type(InputParamProperties_Type)                                       ::  Properties
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! =============================================================================
# undef   _VarType_
# undef   _VarString_
# define  _VarType_     real(REAL64)
# define  _VarString_   "real(REAL64)"
  if ( allocated(Value) ) deallocate( Value, Bnd1, Bnd2 )
  allocate( _VarType_ :: Value, Bnd1, Bnd2 )
!   sKin        =   GetVarKind(STRINGIFY(_VarType_))
  sKin        =   GetVarKind(_VarString_)
  Bnd1        =   0._REAL64
  Value       =   1._REAL64
  Bnd2        =   2._REAL64
  select type (Bnd1);  type is (_VarType_); write(sBnd1,"(i1)") int(Bnd1);  end select
  select type (Value); type is (_VarType_); write(sVal, "(i1)") int(Value); end select
  select type (Bnd2);  type is (_VarType_); write(sBnd2,"(i1)") int(Bnd2);  end select
! =============================================================================
# define  _Op1_    GreaterThan
  Expected    =   .True.
  Key         =   "True "
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
  sOp1        =   GetOperator("GreaterThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
! =============================================================================
# define  _Op1_    GreaterEqualThan
  Expected    =   .True.
  Key         =   "True "
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
  sOp1        =   GetOperator("GreaterEqualThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
! =============================================================================
# define  _Op1_    LowerThan
  Expected    =   .False.
  Key         =   "False"
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
  sOp1        =   GetOperator("LowerThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
! =============================================================================
# define  _Op1_    LowerEqualThan
  Expected    =   .False.
  Key         =   "False"
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
  sOp1        =   GetOperator("LowerEqualThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
! =============================================================================
# define  _Op1_    GreaterEqualThan
# define  _Op2_    LowerEqualThan
  Expected    =   .True.
  Key         =   "True "
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
!   sOp2        =   GetOperator(STRINGIFY(_Op2_))
  sOp1        =   GetOperator("GreaterEqualThan")
  sOp2        =   GetOperator("LowerEqualThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" and "//sOp2//" "//sBnd2//"  ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1, _Op2_ = Bnd2 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
# undef   _Op2_
! =============================================================================
# define  _Op1_    GreaterEqualThan
# define  _Op2_    LowerEqualThan
  Expected    =   .True.
  Key         =   "True "
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
!   sOp2        =   GetOperator(STRINGIFY(_Op2_))
  sOp1        =   GetOperator("GreaterEqualThan")
  sOp2        =   GetOperator("LowerEqualThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" and "//sOp2//" "//sBnd2//"  ("//sKin//"-mixed)"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = 0, _Op2_ = 2 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
# undef   _Op2_
! =============================================================================



# undef   _VarType_
# undef   _VarString_
# define  _VarType_     integer(INT64)
# define  _VarString_   "integer(INT64)"
  if ( allocated(Value) ) deallocate( Value, Bnd1, Bnd2 )
  allocate( _VarType_ :: Value, Bnd1, Bnd2 )
!   sKin        =   GetVarKind(STRINGIFY(_VarType_))
  sKin        =   GetVarKind(_VarString_)
  Bnd1        =   0_INT64
  Value       =   1_INT64
  Bnd2        =   2_INT64
  select type (Bnd1);  type is (_VarType_); write(sBnd1,"(i1)") int(Bnd1);  end select
  select type (Value); type is (_VarType_); write(sVal, "(i1)") int(Value); end select
  select type (Bnd2);  type is (_VarType_); write(sBnd2,"(i1)") int(Bnd2);  end select
! =============================================================================
# define  _Op1_    GreaterThan
  Expected    =   .True.
  Key         =   "True "
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
  sOp1        =   GetOperator("GreaterThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
! =============================================================================
# define  _Op1_    GreaterEqualThan
  Expected    =   .True.
  Key         =   "True "
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
  sOp1        =   GetOperator("GreaterEqualThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
! =============================================================================
# define  _Op1_    LowerThan
  Expected    =   .False.
  Key         =   "False"
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
  sOp1        =   GetOperator("LowerThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
! =============================================================================
# define  _Op1_    LowerEqualThan
  Expected    =   .False.
  Key         =   "False"
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
  sOp1        =   GetOperator("LowerEqualThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
! =============================================================================
# define  _Op1_    GreaterEqualThan
# define  _Op2_    LowerEqualThan
  Expected    =   .True.
  Key         =   "True "
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
!   sOp2        =   GetOperator(STRINGIFY(_Op2_))
  sOp1        =   GetOperator("GreaterEqualThan")
  sOp2        =   GetOperator("LowerEqualThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" and "//sOp2//" "//sBnd2//"  ("//sKin//")"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = Bnd1, _Op2_ = Bnd2 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
# undef   _Op2_
! =============================================================================
# define  _Op1_    GreaterEqualThan
# define  _Op2_    LowerEqualThan
  Expected    =   .True.
  Key         =   "True "
!   sOp1        =   GetOperator(STRINGIFY(_Op1_))
!   sOp2        =   GetOperator(STRINGIFY(_Op2_))
  sOp1        =   GetOperator("GreaterEqualThan")
  sOp2        =   GetOperator("LowerEqualThan")
  Description =   "'"//ProcName//"': "//Key//" -> "//sVal//" "//sOp1//" "//sBnd1//" and "//sOp2//" "//sBnd2//"  ("//sKin//"-mixed)"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Properties  =   ConstructorInputParamProperties( _Op1_ = 0.0, _Op2_ = 2.0 )
  call Properties%CheckValueValidity( Value, Found )
  @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
  call Logger%Write( "[ok]" )
# undef   _Op1_
# undef   _Op2_
! =============================================================================

End Subroutine

Function GetVarKind( VarType ) result(VarKind)
  character(*)                            ,intent(in)     ::  VarType
  character(:)  ,allocatable                              ::  VarKind
  integer                                                 ::  i, j
  VarKind =   VarType
  select case (VarKind)
    case ("logical")
      VarKind   =   "LOG"
    case ("character")
      VarKind   =   "CHAR"
    case default
      i         =   index( VarKind, "(" ) + 1
      j         =   index( VarKind, ")" ) - 1
      VarKind   =   VarKind(i:j)
  end select
End Function

Function GetOperator( Op ) result(Symbol)
  use String_Library    ,only:  Equal
  character(*)                            ,intent(in)     ::  Op
  character(:)  ,allocatable                              ::  Symbol
  if ( Equal( Op, "LowerThan"       , CaseSensitive=.False.) ) Symbol = "<"
  if ( Equal( Op, "LowerEqualThan"  , CaseSensitive=.False.) ) Symbol = "<="
  if ( Equal( Op, "GreaterThan"     , CaseSensitive=.False.) ) Symbol = ">"
  if ( Equal( Op, "GreaterEqualThan", CaseSensitive=.False.) ) Symbol = ">="
End Function

End Module