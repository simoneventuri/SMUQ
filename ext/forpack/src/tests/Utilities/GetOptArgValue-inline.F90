
Block

# ifdef _Is_Character_
#   define  _Attribute_   ,allocatable
# else
#   define  _Attribute_
# endif

  character(*)                ,parameter  ::  RealName = ProcName//"_"//STRINGIFY(_VarKind_)
  _VarType_ ,pointer                      ::  Optional
  _VarType_ ,target   _Attribute_         ::  Dummy
  _VarType_           _Attribute_         ::  Found, Expected, Default
! # endif
! =============================================================================
  Description   =   "'"//RealName//"': w/o opt arg."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
# if defined _Is_Character_
    Default   =   "1"
    Expected  =   "1"
# elif defined _Is_Logical_
    Default   =   .True.
    Expected  =   .True.
# else
    Default   =   1
    Expected  =   1
# endif
  Optional  =>  null()
  Found     =   GetOptArgValue( Default, Optional )
# if defined _Is_integer_
    Found_INT32     =   Found
    Expected_INT32  =   Expected
    @assertEqual( Expected_INT32, Found_INT32, Description//": Wrong value between Expected/Found. " )
# elif defined _Is_real_
    Found_REAL64    =   Found
    Expected_REAL64 =   Expected
    @assertEqual( Expected_REAL64, Found_REAL64, Description//": Wrong value between Expected/Found. " )
# else
    @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
# endif
  call Logger%Write( "[ok]" )
! =============================================================================
  Description   =   "'"//RealName//"': with opt arg."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  Found     =   GetOptArgValue( Default, Optional )
# if defined _Is_Character_
    Default   =   "1"
    Expected  =   "0"
    Dummy     =   "0"
# elif defined _Is_Logical_
    Default   =   .True.
    Expected  =   .False.
    Dummy     =   .False.
# else
    Default   =   1
    Expected  =   0
    Dummy     =   0
# endif
  Optional  =>  Dummy
  Found     =   GetOptArgValue( Default, Optional )
# if defined _Is_integer_
    Found_INT32     =   Found
    Expected_INT32  =   Expected
    @assertEqual( Expected_INT32, Found_INT32, Description//": Wrong value between Expected/Found. " )
# elif defined _Is_real_
    Found_REAL64    =   Found
    Expected_REAL64 =   Expected
    @assertEqual( Expected_REAL64, Found_REAL64, Description//": Wrong value between Expected/Found. " )
# else
    @assertEqual( Expected, Found, Description//": Wrong value between Expected/Found. " )
# endif
  call Logger%Write( "[ok]" )
! =============================================================================
End Block

# undef _Is_Logical_
# undef _Is_Character_
# undef _Is_integer_
# undef _Is_real_
# undef _VarType_
# undef _VarKind_
# undef _Attribute_

