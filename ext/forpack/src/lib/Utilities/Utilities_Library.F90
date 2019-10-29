Module Utilities_Library

! Include file needed for: CONCAT (for AddVar0dToVar1d_*), CONCAT3 (for SetOptArg_*)
# include "forpack-include.inc"

  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  private
  public  ::  SetOptArg
  public  ::  GetOptArgValue
  public  ::  GetOptArgValue_LOG
  public  ::  GetOptArgValue_INT8
  public  ::  GetOptArgValue_INT16
  public  ::  GetOptArgValue_INT32
  public  ::  GetOptArgValue_INT64
  public  ::  GetOptArgValue_REAL32
  public  ::  GetOptArgValue_REAL64
# ifndef  PGI_COMPILER
  public  ::  GetOptArgValue_REAL128
# endif
  public  ::  GetOptArgValue_CHAR
  public  ::  LocateValue
  public  ::  AddElementToArray
  public  ::  RemoveElementFromArray
  public  ::  PresentAndTrue
  public  ::  PresentAndFalse
  public  ::  AbsentOrTrue
  public  ::  AbsentOrFalse
  public  ::  PresentAndDifferentThan
  public  ::  PresentAndNotEmpty
  public  ::  GetPosition
  public  ::  GetPositions
  public  ::  WorkaroundCharacterAssignment
  public  ::  IsIncluded

  public  ::  SelectedItem
  public  ::  SkippedItem
  public  ::  FirstSkippedItem
  public  ::  SkippedItemsRange

  public  ::  GetVarRank

  public  ::  GetSourceLocation


# define _ProcName_(_VarKind_) \
  Module Procedure  \
    CONCAT3(SetOptArg_,_VarKind_,_0d), \
    CONCAT3(SetOptArg_,_VarKind_,_1d), \
    CONCAT3(SetOptArg_,_VarKind_,_2d), \
    CONCAT3(SetOptArg_,_VarKind_,_3d), \
    CONCAT3(SetOptArg_,_VarKind_,_4d)
  Interface             SetOptArg
    _ProcName_(LOG)
    _ProcName_(INT8)
    _ProcName_(INT16)
    _ProcName_(INT32)
    _ProcName_(INT64)
    _ProcName_(REAL32)
    _ProcName_(REAL64)
# ifndef  PGI_COMPILER
    _ProcName_(REAL128)
# endif
    _ProcName_(CHAR)
  End Interface
# undef _ProcName_

  Interface             GetOptArgValue
    Module Procedure    GetOptArgValue_LOG
    Module Procedure    GetOptArgValue_INT8, GetOptArgValue_INT16, GetOptArgValue_INT32, GetOptArgValue_INT64
    Module Procedure    GetOptArgValue_REAL32, GetOptArgValue_REAL64
    Module Procedure    GetOptArgValue_CHAR
# ifndef  PGI_COMPILER
    Module Procedure    GetOptArgValue_REAL128
# endif
  End Interface

  Interface             LocateValue
    Module Procedure    LocateValue_INT8, LocateValue_INT16, LocateValue_INT32, LocateValue_INT64
    Module Procedure    LocateValue_REAL32, LocateValue_REAL64
# ifndef  PGI_COMPILER
    Module Procedure    LocateValue_REAL128
# endif
  End Interface

  Interface             AddElementToArray
    Module Procedure    AddVar0dToVar1d_Logical,  AddVar1dToVar1d_Logical,  AddVar1dToVar2d_Logical,  AddVar2dToVar2d_Logical,  AddVar3dToVar4d_Logical
    Module Procedure    AddVar0dToVar1d_INT8,     AddVar1dToVar1d_INT8,     AddVar1dToVar2d_INT8,     AddVar2dToVar2d_INT8,     AddVar3dToVar4d_INT8
    Module Procedure    AddVar0dToVar1d_INT16,    AddVar1dToVar1d_INT16,    AddVar1dToVar2d_INT16,    AddVar2dToVar2d_INT16,    AddVar3dToVar4d_INT16
    Module Procedure    AddVar0dToVar1d_INT32,    AddVar1dToVar1d_INT32,    AddVar1dToVar2d_INT32,    AddVar2dToVar2d_INT32,    AddVar3dToVar4d_INT32
    Module Procedure    AddVar0dToVar1d_INT64,    AddVar1dToVar1d_INT64,    AddVar1dToVar2d_INT64,    AddVar2dToVar2d_INT64,    AddVar3dToVar4d_INT64
    Module Procedure    AddVar0dToVar1d_REAL32,   AddVar1dToVar1d_REAL32,   AddVar1dToVar2d_REAL32,   AddVar2dToVar2d_REAL32,   AddVar3dToVar4d_REAL32
    Module Procedure    AddVar0dToVar1d_REAL64,   AddVar1dToVar1d_REAL64,   AddVar1dToVar2d_REAL64,   AddVar2dToVar2d_REAL64,   AddVar3dToVar4d_REAL64
# ifndef  PGI_COMPILER
    Module Procedure    AddVar0dToVar1d_REAL128,  AddVar1dToVar1d_REAL128,  AddVar1dToVar2d_REAL128,  AddVar2dToVar2d_REAL128,  AddVar3dToVar4d_REAL128
# endif
    Module Procedure    AddVar0dToVar1d_CHAR,     AddVar1dToVar1d_CHAR,     AddVar1dToVar2d_CHAR,     AddVar2dToVar2d_CHAR,     AddVar3dToVar4d_CHAR
  End Interface

  Interface             RemoveElementFromArray
    Module Procedure    RemoveElementFromArray_CHAR1d_CHAR0d
    Module Procedure    RemoveElementFromArray_CHAR1d_From_INT0d
    Module Procedure    RemoveElementFromArray_CHAR1d_From_Mask
    Module Procedure    RemoveElementFromArray_REAL64_3d_From_Index
  End Interface

  Interface             PresentAndTrue
    Module Procedure    PresentAndTrue
  End Interface

  Interface             PresentAndFalse
    Module Procedure    PresentAndFalse
  End Interface

  Interface             PresentAndDifferentThan
    Module Procedure    PresentAndDifferentThan
  End Interface

  Interface             PresentAndNotEmpty
    Module Procedure    PresentAndNotEmpty
  End Interface

  Interface             GetPosition
    Module Procedure    GetPosition_INT8_0d, GetPosition_INT16_0d, GetPosition_INT32_0d, GetPosition_INT64_0d
    Module Procedure    GetPosition_CHAR_0d
!     Module Procedure    GetPosition_CHAR_1d !@TODO: To be implemented
    Module Procedure    GetPosition_INT8_1d, GetPosition_INT16_1d, GetPosition_INT32_1d, GetPosition_INT64_1d
  End Interface

  Interface             GetPositions
    Module Procedure    GetPositions_INT8_0d, GetPositions_INT16_0d, GetPositions_INT32_0d, GetPositions_INT64_0d
  End Interface

  Interface             IsIncluded
    Module Procedure    IsIncluded_LOG_0d
    Module Procedure    IsIncluded_INT8_0d    , IsIncluded_INT8_1d
    Module Procedure    IsIncluded_INT16_0d   , IsIncluded_INT16_1d
    Module Procedure    IsIncluded_INT32_0d   , IsIncluded_INT32_1d
    Module Procedure    IsIncluded_INT64_0d   , IsIncluded_INT64_1d
    Module Procedure    IsIncluded_REAL32_0d  , IsIncluded_REAL32_1d
    Module Procedure    IsIncluded_REAL64_0d  , IsIncluded_REAL64_1d
# ifndef  PGI_COMPILER
    Module Procedure    IsIncluded_REAL128_0d , IsIncluded_REAL128_1d
# endif
    Module Procedure    IsIncluded_CHAR_0d    , IsIncluded_CHAR_1d
  End Interface

  Interface             GetVarRank
    Module Procedure    GetVarRank_REAL64_3d
  End Interface



  Interface
    ! **************************************************************************************************************
    !                             PROCEDURES FOR PROCESSING OPTIONAL ARGUMENTS
    ! **************************************************************************************************************
    Pure Module Function GetOptArgValue_LOG( VarDef, VarOpt ) result(VarLoc)
      logical                                                       ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
      logical                                             ,optional ,intent(in)     ::  VarOpt                  !< Optional variable
      logical                                                                       ::  VarLoc                  !< Local variable to be set
    End Function
    Pure Module Function GetOptArgValue_INT8( VarDef, VarOpt ) result(VarLoc)
      integer(INT8)                                                 ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
      integer(INT8)                                       ,optional ,intent(in)     ::  VarOpt                  !< Optional variable
      integer(INT8)                                                                 ::  VarLoc                  !< Local variable to be set
    End Function
    Pure Module Function GetOptArgValue_INT16( VarDef, VarOpt ) result(VarLoc)
      integer(INT16)                                                ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
      integer(INT16)                                      ,optional ,intent(in)     ::  VarOpt                  !< Optional variable
      integer(INT16)                                                                ::  VarLoc                  !< Local variable to be set
    End Function
    Pure Module Function GetOptArgValue_INT32( VarDef, VarOpt ) result(VarLoc)
      integer(INT32)                                                ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
      integer(INT32)                                      ,optional ,intent(in)     ::  VarOpt                  !< Optional variable
      integer(INT32)                                                                ::  VarLoc                  !< Local variable to be set
    End Function
    Pure Module Function GetOptArgValue_INT64( VarDef, VarOpt ) result(VarLoc)
      integer(INT64)                                                ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
      integer(INT64)                                      ,optional ,intent(in)     ::  VarOpt                  !< Optional variable
      integer(INT64)                                                                ::  VarLoc                  !< Local variable to be set
    End Function
    Pure Module Function GetOptArgValue_REAL32( VarDef, VarOpt ) result(VarLoc)
      real(REAL32)                                                  ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
      real(REAL32)                                        ,optional ,intent(in)     ::  VarOpt                  !< Optional variable
      real(REAL32)                                                                  ::  VarLoc                  !< Local variable to be set
    End Function
    Pure Module Function GetOptArgValue_REAL64( VarDef, VarOpt ) result(VarLoc)
      real(REAL64)                                                  ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
      real(REAL64)                                        ,optional ,intent(in)     ::  VarOpt                  !< Optional variable
      real(REAL64)                                                                  ::  VarLoc                  !< Local variable to be set
    End Function
# ifndef  PGI_COMPILER
    Pure Module Function GetOptArgValue_REAL128( VarDef, VarOpt ) result(VarLoc)
      real(REAL128)                                                 ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
      real(REAL128)                                       ,optional ,intent(in)     ::  VarOpt                  !< Optional variable
      real(REAL128)                                                                 ::  VarLoc                  !< Local variable to be set
    End Function
# endif
    Pure Module Function GetOptArgValue_CHAR( VarDef, VarOpt ) result(VarLoc)
      character(*)                                                  ,intent(in)     ::  VarDef                  !< Default value of the local variable if the optional variable is absent
      character(*)                                        ,optional ,intent(in)     ::  VarOpt                  !< Optional variable
      character(:)  ,allocatable                                                    ::  VarLoc                  !< Local variable to be set
    End Function


! ============================================================
# define _ProcName_(_VarKind_,_VarType_) \
    Pure Module Subroutine CONCAT3(SetOptArg_,_VarKind_,_0d)( Var, VarOpt);\
      _VarType_                                                  ,intent(in)     ::  Var;\
      _VarType_                                        ,optional ,intent(inout)  ::  VarOpt;\
    End Subroutine
    _ProcName_(LOG,logical)
    _ProcName_(INT8,integer(INT8))
    _ProcName_(INT16,integer(INT16))
    _ProcName_(INT32,integer(INT32))
    _ProcName_(INT64,integer(INT64))
    _ProcName_(REAL32,real(REAL32))
    _ProcName_(REAL64,real(REAL64))
#   ifndef  PGI_COMPILER
    _ProcName_(REAL128,real(REAL128))
#   endif
    _ProcName_(CHAR,character(*))
# undef _ProcName_
! ============================================================
# define _ProcName_(_VarKind_,_VarType_) \
    Pure Module Subroutine CONCAT3(SetOptArg_,_VarKind_,_1d)( Var, VarOpt);\
      _VarType_                                                  ,intent(in)     ::  Var(:);\
      _VarType_                                        ,optional ,intent(inout)  ::  VarOpt(:);\
    End Subroutine;
    _ProcName_(LOG,logical)
    _ProcName_(INT8,integer(INT8))
    _ProcName_(INT16,integer(INT16))
    _ProcName_(INT32,integer(INT32))
    _ProcName_(INT64,integer(INT64))
    _ProcName_(REAL32,real(REAL32))
    _ProcName_(REAL64,real(REAL64))
#   ifndef  PGI_COMPILER
    _ProcName_(REAL128,real(REAL128))
#   endif
    _ProcName_(CHAR,character(*))
# undef _ProcName_
! ============================================================
# define _ProcName_(_VarKind_,_VarType_) \
    Pure Module Subroutine CONCAT3(SetOptArg_,_VarKind_,_2d)( Var, VarOpt);\
      _VarType_                                                  ,intent(in)     ::  Var(:,:);\
      _VarType_                                        ,optional ,intent(inout)  ::  VarOpt(:,:);\
    End Subroutine;
    _ProcName_(LOG,logical)
    _ProcName_(INT8,integer(INT8))
    _ProcName_(INT16,integer(INT16))
    _ProcName_(INT32,integer(INT32))
    _ProcName_(INT64,integer(INT64))
    _ProcName_(REAL32,real(REAL32))
    _ProcName_(REAL64,real(REAL64))
#   ifndef  PGI_COMPILER
    _ProcName_(REAL128,real(REAL128))
#   endif
    _ProcName_(CHAR,character(*))
# undef _ProcName_
! ============================================================
# define _ProcName_(_VarKind_,_VarType_) \
    Pure Module Subroutine CONCAT3(SetOptArg_,_VarKind_,_3d)( Var, VarOpt);\
      _VarType_                                                  ,intent(in)     ::  Var(:,:,:);\
      _VarType_                                        ,optional ,intent(inout)  ::  VarOpt(:,:,:);\
    End Subroutine;
    _ProcName_(LOG,logical)
    _ProcName_(INT8,integer(INT8))
    _ProcName_(INT16,integer(INT16))
    _ProcName_(INT32,integer(INT32))
    _ProcName_(INT64,integer(INT64))
    _ProcName_(REAL32,real(REAL32))
    _ProcName_(REAL64,real(REAL64))
#   ifndef  PGI_COMPILER
    _ProcName_(REAL128,real(REAL128))
#   endif
    _ProcName_(CHAR,character(*))
# undef _ProcName_
! ============================================================
# define _ProcName_(_VarKind_,_VarType_) \
    Pure Module Subroutine CONCAT3(SetOptArg_,_VarKind_,_4d)( Var, VarOpt);\
      _VarType_                                                  ,intent(in)     ::  Var(:,:,:,:);\
      _VarType_                                        ,optional ,intent(inout)  ::  VarOpt(:,:,:,:);\
    End Subroutine;
    _ProcName_(LOG,logical)
    _ProcName_(INT8,integer(INT8))
    _ProcName_(INT16,integer(INT16))
    _ProcName_(INT32,integer(INT32))
    _ProcName_(INT64,integer(INT64))
    _ProcName_(REAL32,real(REAL32))
    _ProcName_(REAL64,real(REAL64))
#   ifndef  PGI_COMPILER
    _ProcName_(REAL128,real(REAL128))
#   endif
    _ProcName_(CHAR,character(*))
# undef _ProcName_
! ============================================================



    ! **************************************************************************************************************
    !                             PROCEDURES FOR LOCATING AN ELEMENT IN AN ARRAY
    ! **************************************************************************************************************
    Pure Module Function LocateValue_INT8( Value, Array ) result(iValue)
      integer(INT8)                                                  ,intent(in)     ::  Value                  !< Value to be locatd
      integer(INT8) ,dimension(:)                                    ,intent(in)     ::  Array                  !< Tabulated vector where the localisation is to be performed
      integer                                                                        ::  iValue                 !< Index such that the input "Val" value is between Array(iValue) and Array(iValue+1)
    End Function
    Pure Module Function LocateValue_INT16( Value, Array ) result(iValue)
      integer(INT16)                                                 ,intent(in)     ::  Value                  !< Value to be locatd
      integer(INT16),dimension(:)                                    ,intent(in)     ::  Array                  !< Tabulated vector where the localisation is to be performed
      integer                                                                        ::  iValue                 !< Index such that the input "Val" value is between Array(iValue) and Array(iValue+1)
    End Function
    Pure Module Function LocateValue_INT32( Value, Array ) result(iValue)
      integer(INT32)                                                 ,intent(in)     ::  Value                  !< Value to be locatd
      integer(INT32),dimension(:)                                    ,intent(in)     ::  Array                  !< Tabulated vector where the localisation is to be performed
      integer                                                                        ::  iValue                 !< Index such that the input "Val" value is between Array(iValue) and Array(iValue+1)
    End Function
    Pure Module Function LocateValue_INT64( Value, Array ) result(iValue)
      integer(INT64)                                                 ,intent(in)     ::  Value                  !< Value to be locatd
      integer(INT64),dimension(:)                                    ,intent(in)     ::  Array                  !< Tabulated vector where the localisation is to be performed
      integer                                                                        ::  iValue                 !< Index such that the input "Val" value is between Array(iValue) and Array(iValue+1)
    End Function
    Pure Module Function LocateValue_REAL32( Value, Array ) result(iValue)
      real(REAL32)                                                   ,intent(in)     ::  Value                  !< Value to be locatd
      real(REAL32)  ,dimension(:)                                    ,intent(in)     ::  Array                  !< Tabulated vector where the localisation is to be performed
      integer                                                                        ::  iValue                 !< Index such that the input "Val" value is between Array(iValue) and Array(iValue+1)
    End Function
    Pure Module Function LocateValue_REAL64( Value, Array ) result(iValue)
      real(REAL64)                                                   ,intent(in)     ::  Value                  !< Value to be locatd
      real(REAL64)  ,dimension(:)                                    ,intent(in)     ::  Array                  !< Tabulated vector where the localisation is to be performed
      integer                                                                        ::  iValue                 !< Index such that the input "Val" value is between Array(iValue) and Array(iValue+1)
    End Function
# ifndef  PGI_COMPILER
    Pure Module Function LocateValue_REAL128( Value, Array ) result(iValue)
      real(REAL128)                                                   ,intent(in)     ::  Value                  !< Value to be locatd
      real(REAL128)  ,dimension(:)                                    ,intent(in)     ::  Array                  !< Tabulated vector where the localisation is to be performed
      integer                                                                        ::  iValue                 !< Index such that the input "Val" value is between Array(iValue) and Array(iValue+1)
    End Function
# endif

    ! **************************************************************************************************************
    !                             PROCEDURES FOR ADDING AN ELEMENT TO AN ARRAY
    ! **************************************************************************************************************

# define  _VarType_         logical
# define  _VarKind_         logical
    Pure Module Subroutine CONCAT(AddVar0dToVar1d_,_VarKind_)( InpVar, OutVar, At, OnlyIfAbsent, PushEnd )
      _VarType_                                             ,intent(in)     ::  InpVar
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
      logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent
      logical                                     ,optional ,intent(in)     ::  PushEnd
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar1d_,_VarKind_)( InpVar, OutVar, At )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar2dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Module Subroutine CONCAT(AddVar3dToVar4d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:,:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
# undef   _VarType_
# undef   _VarKind_

# define  _VarType_         integer(INT8)
# define  _VarKind_         INT8
    Pure Module Subroutine CONCAT(AddVar0dToVar1d_,_VarKind_)( InpVar, OutVar, At, OnlyIfAbsent, PushEnd )
      _VarType_                                             ,intent(in)     ::  InpVar
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
      logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent
      logical                                     ,optional ,intent(in)     ::  PushEnd
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar1d_,_VarKind_)( InpVar, OutVar, At )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar2dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Module Subroutine CONCAT(AddVar3dToVar4d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:,:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
# undef   _VarType_
# undef   _VarKind_

# define  _VarType_         integer(INT16)
# define  _VarKind_         INT16
    Pure Module Subroutine CONCAT(AddVar0dToVar1d_,_VarKind_)( InpVar, OutVar, At, OnlyIfAbsent, PushEnd )
      _VarType_                                             ,intent(in)     ::  InpVar
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
      logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent
      logical                                     ,optional ,intent(in)     ::  PushEnd
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar1d_,_VarKind_)( InpVar, OutVar, At )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar2dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Module Subroutine CONCAT(AddVar3dToVar4d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:,:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
# undef   _VarType_
# undef   _VarKind_

# define  _VarType_         integer(INT32)
# define  _VarKind_         INT32
    Pure Module Subroutine CONCAT(AddVar0dToVar1d_,_VarKind_)( InpVar, OutVar, At, OnlyIfAbsent, PushEnd )
      _VarType_                                             ,intent(in)     ::  InpVar
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
      logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent
      logical                                     ,optional ,intent(in)     ::  PushEnd
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar1d_,_VarKind_)( InpVar, OutVar, At )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar2dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Module Subroutine CONCAT(AddVar3dToVar4d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:,:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
# undef   _VarType_
# undef   _VarKind_

# define  _VarType_         integer(INT64)
# define  _VarKind_         INT64
    Pure Module Subroutine CONCAT(AddVar0dToVar1d_,_VarKind_)( InpVar, OutVar, At, OnlyIfAbsent, PushEnd )
      _VarType_                                             ,intent(in)     ::  InpVar
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
      logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent
      logical                                     ,optional ,intent(in)     ::  PushEnd
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar1d_,_VarKind_)( InpVar, OutVar, At )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar2dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Module Subroutine CONCAT(AddVar3dToVar4d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:,:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
# undef   _VarType_
# undef   _VarKind_

# define  _VarType_         real(REAL32)
# define  _VarKind_         REAL32
    Pure Module Subroutine CONCAT(AddVar0dToVar1d_,_VarKind_)( InpVar, OutVar, At, OnlyIfAbsent, PushEnd )
      _VarType_                                             ,intent(in)     ::  InpVar
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
      logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent
      logical                                     ,optional ,intent(in)     ::  PushEnd
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar1d_,_VarKind_)( InpVar, OutVar, At )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar2dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Module Subroutine CONCAT(AddVar3dToVar4d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:,:)
      _VarType_     ,allocatable                             ,intent(inout)  ::  OutVar(:,:,:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
# undef   _VarType_
# undef   _VarKind_

# define  _VarType_         real(REAL64)
# define  _VarKind_         REAL64
    Pure Module Subroutine CONCAT(AddVar0dToVar1d_,_VarKind_)( InpVar, OutVar, At, OnlyIfAbsent, PushEnd )
      _VarType_                                             ,intent(in)     ::  InpVar
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
      logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent
      logical                                     ,optional ,intent(in)     ::  PushEnd
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar1d_,_VarKind_)( InpVar, OutVar, At )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar2dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Module Subroutine CONCAT(AddVar3dToVar4d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:,:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
# undef   _VarType_
# undef   _VarKind_

# ifndef  PGI_COMPILER
# define  _VarType_         real(REAL128)
# define  _VarKind_         REAL128
    Pure Module Subroutine CONCAT(AddVar0dToVar1d_,_VarKind_)( InpVar, OutVar, At, OnlyIfAbsent, PushEnd )
      _VarType_                                             ,intent(in)     ::  InpVar
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
      logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent
      logical                                     ,optional ,intent(in)     ::  PushEnd
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar1d_,_VarKind_)( InpVar, OutVar, At )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar2dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Module Subroutine CONCAT(AddVar3dToVar4d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_                                             ,intent(in)     ::  InpVar(:,:,:)
      _VarType_     ,allocatable                            ,intent(inout)  ::  OutVar(:,:,:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
# undef   _VarType_
# undef   _VarKind_
# endif

# define  _VarType_         character
# define  _VarKind_         CHAR
    Pure Module Subroutine CONCAT(AddVar0dToVar1d_,_VarKind_)( InpVar, OutVar, At, OnlyIfAbsent, PushEnd )
      _VarType_(*)                                          ,intent(in)     ::  InpVar
      _VarType_(:)    ,allocatable                          ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
      logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent
      logical                                     ,optional ,intent(in)     ::  PushEnd
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar1d_,_VarKind_)( InpVar, OutVar, At )
      _VarType_(*)                                          ,intent(in)     ::  InpVar(:)
      _VarType_(:)    ,allocatable                          ,intent(inout)  ::  OutVar(:)
      integer                                     ,optional ,intent(in)     ::  At
!       logical                                     ,optional ,intent(in)     ::  OnlyIfAbsent  ! @TODO
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar1dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_(*)                                          ,intent(in)     ::  InpVar(:)
      _VarType_(:)    ,allocatable                          ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Pure Module Subroutine CONCAT(AddVar2dToVar2d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_(*)                                          ,intent(in)     ::  InpVar(:,:)
      _VarType_(:)    ,allocatable                          ,intent(inout)  ::  OutVar(:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
    Module Subroutine CONCAT(AddVar3dToVar4d_,_VarKind_)( InpVar, OutVar, Dim )
      _VarType_(*)                                          ,intent(in)     ::  InpVar(:,:,:)
      _VarType_(:)    ,allocatable                          ,intent(inout)  ::  OutVar(:,:,:,:)
      integer                                     ,optional ,intent(in)     ::  Dim
    End Subroutine
# undef   _VarType_
# undef   _VarKind_
  End Interface


  Interface

    Pure Module Subroutine RemoveElementFromArray_CHAR1d_CHAR0d( Array, Element, CaseSensitive )
      character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array                             !< Array to be processed
      character(*)                                          ,intent(in)     ::  Element                           !< Element to remove
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the search should be case sensitive
    End Subroutine

    Pure Module Subroutine RemoveElementFromArray_CHAR1d_From_INT0d( Array, i )
      character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array                             !< Array to be processed
      integer                                               ,intent(in)     ::  i                                 !< Index of the element to be removed
    End Subroutine

    Pure Module Subroutine RemoveElementFromArray_CHAR1d_From_Mask( Array, Mask )
      character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array                             !< Array to be processed
      logical       ,dimension(:)                           ,intent(in)     ::  Mask                              !< Mask: True elements will be removed
    End Subroutine

    Pure Module Subroutine RemoveElementFromArray_REAL64_3d_From_Index( Array, Idx, Dim )
      real(REAL64)  ,allocatable                            ,intent(inout)  ::  Array(:,:,:)                      !< Array to be processed
      integer                                               ,intent(in)     ::  Idx                               !< Index of the element to be removed
      integer                                               ,intent(in)     ::  Dim
    End Subroutine


    ! **************************************************************************************************************
    !       PROCEDURES FOR CHECKING WHETHER A VARIABLE IS PRESENT IN A LIST OF VARIABLES
    ! **************************************************************************************************************

! ============================================================
  ! Var       !< Variable to be checked for presence in the list of variable
  ! List(:)   !< List of variable used for presence checking
  ! Included  !< Presence indicator of the variable in the list of variables
! # define _ProcName_(_VarKind_,_VarType_) \
!     Pure Module Function CONCAT3(IsIncluded_,_VarKind_,_0d)( Var, List ) result(Included);\
!       _VarType_                                             ,intent(in)     ::  Var;\
!       _VarType_                                             ,intent(in)     ::  List(:);\
!       logical                                                               ::  Included;\
!     End Function
!     _ProcName_(LOG,logical)
!     _ProcName_(INT8,integer(INT8))
!     _ProcName_(INT16,integer(INT16))
!     _ProcName_(INT32,integer(INT32))
!     _ProcName_(INT64,integer(INT64))
!     _ProcName_(REAL32,real(REAL32))
!     _ProcName_(REAL64,real(REAL64))
! #   ifndef  PGI_COMPILER
!     _ProcName_(REAL128,real(REAL128))
! #   endif
! # undef _ProcName_
! ============================================================

    Pure Module Function IsIncluded_LOG_0d( Var, List ) result(Included)
!       logical                                               ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      type(logical)                                         ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      logical                                               ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical                                                               ::  Included                        !< Presence indicator of the variable in the list of variables
    End Function

    Pure Module Function IsIncluded_INT8_0d( Var, List ) result(Included)
      integer(INT8)                                         ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      integer(INT8)                                         ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical                                                               ::  Included                        !< Presence indicator of the variable in the list of variables
    End Function

    Pure Module Function IsIncluded_INT16_0d( Var, List ) result(Included)
      integer(INT16)                                        ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      integer(INT16)                                        ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical                                                               ::  Included                        !< Presence indicator of the variable in the list of variables
    End Function

    Pure Module Function IsIncluded_INT32_0d( Var, List ) result(Included)
      integer(INT32)                                        ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      integer(INT32)                                        ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical                                                               ::  Included                        !< Presence indicator of the variable in the list of variables
    End Function

    Pure Module Function IsIncluded_INT64_0d( Var, List ) result(Included)
      integer(INT64)                                        ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      integer(INT64)                                        ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical                                                               ::  Included                        !< Presence indicator of the variable in the list of variables
    End Function

    Pure Module Function IsIncluded_REAL32_0d( Var, List, Tol ) result(Included)
      real(REAL32)                                          ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      real(REAL32)                                          ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      real(REAL32)                                ,optional ,intent(in)     ::  Tol                             !< Tolerence
      logical                                                               ::  Included                        !< Presence indicator of the variable in the list of variables
    End Function

    Pure Module Function IsIncluded_REAL64_0d( Var, List, Tol ) result(Included)
      real(REAL64)                                          ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      real(REAL64)                                          ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      real(REAL64)                                ,optional ,intent(in)     ::  Tol                             !< Tolerence
      logical                                                               ::  Included                        !< Presence indicator of the variable in the list of variables
    End Function

# ifndef  PGI_COMPILER
    Pure Module Function IsIncluded_REAL128_0d( Var, List, Tol ) result(Included)
      real(REAL128)                                         ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      real(REAL128)                                         ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      real(REAL128)                               ,optional ,intent(in)     ::  Tol                             !< Tolerence
      logical                                                               ::  Included                        !< Presence indicator of the variable in the list of variables
    End Function
# endif
!
    Pure Module Function IsIncluded_CHAR_0d( Var, List, CaseSensitive ) result(Included)
      character(*)                                          ,intent(in)     ::  Var                             !< Variable to be checked for presence in the list of variable
      character(*)                                          ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the search should be case sensitive
      logical                                                               ::  Included                        !< Presence indicator of the input variable in the list of variables
    End Function

    Pure Module Function IsIncluded_INT8_1d( Var, List ) result(Included)
      integer(INT8)                                         ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
      integer(INT8)                                         ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
    End Function

    Pure Module Function IsIncluded_INT16_1d( Var, List ) result(Included)
      integer(INT16)                                        ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
      integer(INT16)                                        ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
    End Function

    Pure Module Function IsIncluded_INT32_1d( Var, List ) result(Included)
      integer(INT32)                                        ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
      integer(INT32)                                        ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
    End Function

    Pure Module Function IsIncluded_INT64_1d( Var, List ) result(Included)
      integer(INT64)                                        ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
      integer(INT64)                                        ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
    End Function

    Pure Module Function IsIncluded_REAL32_1d( Var, List, Tol ) result(Included)
      real(REAL32)                                          ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
      real(REAL32)                                          ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      real(REAL32)                                ,optional ,intent(in)     ::  Tol                             !< Tolerence
      logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
    End Function

    Pure Module Function IsIncluded_REAL64_1d( Var, List, Tol ) result(Included)
      real(REAL64)                                          ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
      real(REAL64)                                          ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      real(REAL64)                                ,optional ,intent(in)     ::  Tol                             !< Tolerence
      logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
    End Function

# ifndef  PGI_COMPILER
    Pure Module Function IsIncluded_REAL128_1d( Var, List, Tol ) result(Included)
      real(REAL128)                                         ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
      real(REAL128)                                         ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      real(REAL128)                               ,optional ,intent(in)     ::  Tol                             !< Tolerence
      logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
    End Function
# endif

    Pure Module Function IsIncluded_CHAR_1d( Var, List, CaseSensitive ) result(Included)
      character(*)                                          ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
      character(*)                                          ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the search should be case sensitive
      logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
    End Function

  End Interface

  Interface

    ! **************************************************************************************************************
    !       PROCEDURES FOR GETTING THE POSITION OF A ELEMENT IN AN ARRAY
    ! **************************************************************************************************************
# define  _VarType_         integer(INT8)
    Pure Module Function GetPosition_INT8_0d( Element, Array, Back ) result(iLoc)
      _VarType_                                             ,intent(in)     ::  Element                             !< Element whose position is to be found
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator whether the position index is counted from the end of the list of strings
      integer                                                               ::  iLoc                                !< Index of the position of the input string in the list of strings
    End Function
    Pure Module Function GetPosition_INT8_1d( Elements, Array, Back ) result(iLoc)
      _VarType_                                             ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of element used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of elements
      integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input elements in the list of elements
    End Function
    Pure Module Subroutine GetPositions_INT8_0d( Element, Array, Positions )
      _VarType_                                             ,intent(in)     ::  Element                             !< String to be checked for presence
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
    End Subroutine
# undef   _VarType_

# define  _VarType_         integer(INT16)
    Pure Module Function GetPosition_INT16_0d( Element, Array, Back ) result(iLoc)
      _VarType_                                             ,intent(in)     ::  Element                             !< Element whose position is to be found
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator whether the position index is counted from the end of the list of strings
      integer                                                               ::  iLoc                                !< Index of the position of the input string in the list of strings
    End Function
    Pure Module Function GetPosition_INT16_1d( Elements, Array, Back ) result(iLoc)
      _VarType_                                             ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of element used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of elements
      integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input elements in the list of elements
    End Function
    Pure Module Subroutine GetPositions_INT16_0d( Element, Array, Positions )
      _VarType_                                             ,intent(in)     ::  Element                             !< String to be checked for presence
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
    End Subroutine
# undef   _VarType_

# define  _VarType_         integer(INT32)
    Pure Module Function GetPosition_INT32_0d( Element, Array, Back ) result(iLoc)
      _VarType_                                             ,intent(in)     ::  Element                             !< Element whose position is to be found
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator whether the position index is counted from the end of the list of strings
      integer                                                               ::  iLoc                                !< Index of the position of the input string in the list of strings
    End Function
    Pure Module Function GetPosition_INT32_1d( Elements, Array, Back ) result(iLoc)
      _VarType_                                             ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of element used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of elements
      integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input elements in the list of elements
    End Function
    Pure Module Subroutine GetPositions_INT32_0d( Element, Array, Positions )
      _VarType_                                             ,intent(in)     ::  Element                             !< String to be checked for presence
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
    End Subroutine
# undef   _VarType_

# define  _VarType_         integer(INT64)
    Pure Module Function GetPosition_INT64_0d( Element, Array, Back ) result(iLoc)
      _VarType_                                             ,intent(in)     ::  Element                             !< Element whose position is to be found
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator whether the position index is counted from the end of the list of strings
      integer                                                               ::  iLoc                                !< Index of the position of the input string in the list of strings
    End Function
    Pure Module Function GetPosition_INT64_1d( Elements, Array, Back ) result(iLoc)
      _VarType_                                             ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of element used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of elements
      integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input elements in the list of elements
    End Function
    Pure Module Subroutine GetPositions_INT64_0d( Element, Array, Positions )
      _VarType_                                             ,intent(in)     ::  Element                             !< String to be checked for presence
      _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
    End Subroutine
# undef   _VarType_

    Pure Module Function GetPosition_CHAR_0d( Element, Array, Back, CaseSensitive ) result(iLoc)
      character(*)                                          ,intent(in)     ::  Element                             !< String to be checked for presence
      character(*)                                          ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
      logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator whether the position index is counted from the end of the list of strings
      logical                                     ,optional ,intent(in)     ::  CaseSensitive                       !< Indicator whether the search should be case sensitive
      integer                                                               ::  iLoc                                !< Index of the position of the input string in the list of strings
    End Function

  End Interface


  Interface
    Pure Module Function PresentAndTrue( Var ) result(Indicator)
      logical                                     ,optional ,intent(in)     ::  Var
      logical                                                               ::  Indicator
    End Function
    Pure Module Function PresentAndFalse( Var ) result(Indicator)
      logical                                     ,optional ,intent(in)     ::  Var
      logical                                                               ::  Indicator
    End Function
    Pure Module Function AbsentOrTrue( Var ) result(Indicator)
      logical                                     ,optional ,intent(in)     ::  Var
      logical                                                               ::  Indicator
    End Function
    Pure Module Function AbsentOrFalse( Var ) result(Indicator)
      logical                                     ,optional ,intent(in)     ::  Var
      logical                                                               ::  Indicator
    End Function
    Pure Module Function PresentAndDifferentThan( RefVal, Var ) result(Indicator)
      integer                                               ,intent(in)     ::  RefVal
      integer                                     ,optional ,intent(in)     ::  Var
      logical                                                               ::  Indicator
    End Function
    Pure Module Function PresentAndNotEmpty( Var ) result(Indicator)
      character(*)                                ,optional ,intent(in)     ::  Var
      logical                                                               ::  Indicator
    End Function
  End Interface


  Interface
    Pure Module Subroutine WorkaroundCharacterAssignment( Inp, Out )
      character(*)  ,dimension(:)                           ,intent(in)     ::  Inp
      character(:)  ,dimension(:)   ,allocatable            ,intent(out)    ::  Out
    End Subroutine
  End Interface


  Interface

    Pure Module Function SelectedItem( i, NItemTot, NItemMax ) result(Indicator)
      integer                                               ,intent(in)     ::  i                                   !< Index of a given element to be eventually printed out of 'NItemTot' elements
      integer                                               ,intent(in)     ::  NItemTot                            !< Total number of elements
      integer                                               ,intent(in)     ::  NItemMax                            !< Maximum number of element to be printed
      logical                                                               ::  Indicator                           !< Indicator whether the element 'i' corresponds to an element to be printed
    End Function

    Pure Module Function SkippedItem( i, NItemTot, NItemMax ) result(Indicator)
      integer                                               ,intent(in)     ::  i                                   !< Index of a given element to be eventually printed out of 'NItemTot' elements
      integer                                               ,intent(in)     ::  NItemTot                            !< Total number of elements
      integer                                               ,intent(in)     ::  NItemMax                            !< Maximum number of element to be printed
      logical                                                               ::  Indicator                           !< Indicator whether the element 'i' corresponds to an element to be printed
    End Function

    Pure Module Function FirstSkippedItem( i, NItemTot, NItemMax ) result(Indicator)
      integer                                               ,intent(in)     ::  i                                   !< Index of a given element to be eventually printed out of 'NItemTot' elements
      integer                                               ,intent(in)     ::  NItemTot                            !< Total number of elements
      integer                                               ,intent(in)     ::  NItemMax                            !< Maximum number of element to be printed
      logical                                                               ::  Indicator                           !< Indicator whether the element 'i' corresponds to the first element to be skipped
    End Function

    Pure Module Function SkippedItemsRange( NItemTot, NItemMax ) result(String)
      integer                                               ,intent(in)     ::  NItemTot                            !< Total number of elements
      integer                                               ,intent(in)     ::  NItemMax                            !< Maximum number of element to be printed
      character(:)  ,allocatable                                            ::  String                              !< Character string describing the skipped items
    End Function

  End Interface


  Interface

#   define  _ProcedureName_   GetVarRank_REAL64_2d
#   define  _VarType_         real(REAL64)
#   define  _VarKind_         REAL64
#   define  _NumDims_         2
#   define  _VarDims_         (:,:)
#   include "GetVarRank_Interface_Inline.F90"

#   define  _ProcedureName_   GetVarRank_REAL64_3d
#   define  _VarType_         real(REAL64)
#   define  _VarKind_         REAL64
#   define  _NumDims_         3
#   define  _VarDims_         (:,:,:)
#   include "GetVarRank_Interface_Inline.F90"

  End Interface



  Interface
    Pure Module Function GetSourceLocation( FileName, LineNumber ) result(String)
      character(*)                                          ,intent(in)     ::  FileName
      integer                                               ,intent(in)     ::  LineNumber
      character(:)  ,allocatable                                            ::  String
    End Function
  End Interface

End Module
