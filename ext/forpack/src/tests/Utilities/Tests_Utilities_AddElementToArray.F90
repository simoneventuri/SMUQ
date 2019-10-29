Module Tests_Utilities_AddElementToArray

! Include file needed for: STRINGIFY
# include "forpack-include.inc"

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use Input_Library     ,only:  InputReader_Type, InputParameter_Type
  use String_Library    ,only:  SetLength, Convert_To_String, Inline
  use Utilities_Library ,only:  AddElementToArray
  use iso_fortran_env   ,only:  INT32, INT64, REAL32, REAL64

  implicit none

  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description, RealName

  contains

@test
Subroutine test_AddVar0dToVar1d()
  character(*)                                              ,parameter  ::  ProcName='AddElementToArray->AddVar0dToVar1d'
  logical                                                   ,parameter  ::  Detailed = .False.
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! ===============================================
# define  _VarType_         integer(INT32)
# define  _VarKind_         INT32
# include "test_AddVar0dToVar1d_Inline.F90"
! ===============================================
# define  _VarType_         integer(INT64)
# define  _VarKind_         INT64
# include "test_AddVar0dToVar1d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL32)
# define  _VarKind_         REAL32
# include "test_AddVar0dToVar1d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL64)
# define  _VarKind_         REAL64
# include "test_AddVar0dToVar1d_Inline.F90"
! ===============================================
End Subroutine

@test
Subroutine test_AddVar1dToVar1d()
  character(*)                                              ,parameter  ::  ProcName='AddElementToArray->AddVar1dToVar1d'
  logical                                                   ,parameter  ::  Detailed = .False.
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! ===============================================
# define  _VarType_         integer(INT32)
# define  _VarKind_         INT32
# include "test_AddVar1dToVar1d_Inline.F90"
! ===============================================
# define  _VarType_         integer(INT64)
# define  _VarKind_         INT64
# include "test_AddVar1dToVar1d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL32)
# define  _VarKind_         REAL32
# include "test_AddVar1dToVar1d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL64)
# define  _VarKind_         REAL64
# include "test_AddVar1dToVar1d_Inline.F90"
! ===============================================
End Subroutine

@test
Subroutine test_AddVar1dToVar2d()
  character(*)                                              ,parameter  ::  ProcName='AddElementToArray->AddVar1dToVar2d'
  logical                                                   ,parameter  ::  Detailed = .False.
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

! ===============================================
# define  _VarType_         integer(INT32)
# define  _VarKind_         INT32
# include "test_AddVar1dToVar2d_Inline.F90"
! ===============================================
# define  _VarType_         integer(INT64)
# define  _VarKind_         INT64
# include "test_AddVar1dToVar2d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL32)
# define  _VarKind_         REAL32
# include "test_AddVar1dToVar2d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL64)
# define  _VarKind_         REAL64
# include "test_AddVar1dToVar2d_Inline.F90"
! ===============================================
End Subroutine

@test
Subroutine test_AddVar2dToVar2d()
  character(*)                                              ,parameter  ::  ProcName='AddElementToArray->AddVar2dToVar2d'
  logical                                                   ,parameter  ::  Detailed = .False.
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! ===============================================
# define  _VarType_         integer(INT32)
# define  _VarKind_         INT32
# include "test_AddVar2dToVar2d_Inline.F90"
! ===============================================
# define  _VarType_         integer(INT64)
# define  _VarKind_         INT64
# include "test_AddVar2dToVar2d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL32)
# define  _VarKind_         REAL32
# include "test_AddVar2dToVar2d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL64)
# define  _VarKind_         REAL64
# include "test_AddVar2dToVar2d_Inline.F90"
! ===============================================
End Subroutine

@test
Subroutine test_AddVar3dToVar4d()
  character(*)                                              ,parameter  ::  ProcName='AddElementToArray->AddVar3dToVar4d'
  logical                                                   ,parameter  ::  Detailed = .False.
  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )
! ===============================================
# define  _VarType_         integer(INT32)
# define  _VarKind_         INT32
# include "test_AddVar3dToVar4d_Inline.F90"
! ===============================================
# define  _VarType_         integer(INT64)
# define  _VarKind_         INT64
# include "test_AddVar3dToVar4d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL32)
# define  _VarKind_         REAL32
# include "test_AddVar3dToVar4d_Inline.F90"
! ===============================================
# define  _VarType_         real(REAL64)
# define  _VarKind_         REAL64
# include "test_AddVar3dToVar4d_Inline.F90"
! ===============================================
End Subroutine


End Module