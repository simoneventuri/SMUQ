@test
Subroutine Tests_Utilities_GetOptArgValue()

! Include file needed for: STRINGIFY
# include "forpack-include.inc"

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use Utilities_Library ,only:  GetOptArgValue
  use String_Library    ,only:  SetLength
  use iso_fortran_env   ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  character(*)                                              ,parameter  ::  ProcName='GetOptArgValue'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  real(REAL64)                                                          ::  Found_REAL64, Expected_REAL64
  real(INT32)                                                           ::  Found_INT32, Expected_INT32

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

# define  _Is_Logical_      1
# define  _VarType_         logical
# define  _VarKind_         LOG
# include "GetOptArgValue-inline.F90"

# define  _Is_Character_    1
# define  _VarType_         character(:)
# define  _VarKind_         CHAR
# include "GetOptArgValue-inline.F90"

# define  _Is_integer_      1
# define  _VarType_         integer(INT8)
# define  _VarKind_         INT8
# include "GetOptArgValue-inline.F90"

# define  _Is_integer_      1
# define  _VarType_         integer(INT16)
# define  _VarKind_         INT16
# include "GetOptArgValue-inline.F90"

# define  _Is_integer_      1
# define  _VarType_         integer(INT32)
# define  _VarKind_         INT32
# include "GetOptArgValue-inline.F90"

# define  _Is_integer_      1
# define  _VarType_         integer(INT64)
# define  _VarKind_         INT64
# include "GetOptArgValue-inline.F90"

# define  _Is_real_         1
# define  _VarType_         real(REAL32)
# define  _VarKind_         REAL32
# include "GetOptArgValue-inline.F90"

# define  _Is_real_         1
# define  _VarType_         real(REAL64)
# define  _VarKind_         REAL64
# include "GetOptArgValue-inline.F90"

# define  _Is_real_         1
# define  _VarType_         real(REAL128)
# define  _VarKind_         REAL128
# include "GetOptArgValue-inline.F90"

  contains

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

End Subroutine
