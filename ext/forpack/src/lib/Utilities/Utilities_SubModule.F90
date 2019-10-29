SubModule(Utilities_Library) Utilities_SubModule

! Include file needed for: CONCAT
# include "forpack-include.inc"

  implicit none

  contains

! **************************************************************************************************************
!                             PROCEDURES FOR PROCESSING OPTIONAL ARGUMENTS
! **************************************************************************************************************
Module Procedure GetOptArgValue_LOG
  if (present(VarOpt)) then; VarLoc = VarOpt; else; VarLoc = VarDef; end if                                     ! Setting the local variable to the optional variable if present or to the default variable if absent
End Procedure
Module Procedure GetOptArgValue_INT8
  if (present(VarOpt)) then; VarLoc = VarOpt; else; VarLoc = VarDef; end if                                     ! Setting the local variable to the optional variable if present or to the default variable if absent
End Procedure
Module Procedure GetOptArgValue_INT16
  if (present(VarOpt)) then; VarLoc = VarOpt; else; VarLoc = VarDef; end if                                     ! Setting the local variable to the optional variable if present or to the default variable if absent
End Procedure
Module Procedure GetOptArgValue_INT32
  if (present(VarOpt)) then; VarLoc = VarOpt; else; VarLoc = VarDef; end if                                     ! Setting the local variable to the optional variable if present or to the default variable if absent
End Procedure
Module Procedure GetOptArgValue_INT64
  if (present(VarOpt)) then; VarLoc = VarOpt; else; VarLoc = VarDef; end if                                     ! Setting the local variable to the optional variable if present or to the default variable if absent
End Procedure
Module Procedure GetOptArgValue_REAL32
  if (present(VarOpt)) then; VarLoc = VarOpt; else; VarLoc = VarDef; end if                                     ! Setting the local variable to the optional variable if present or to the default variable if absent
End Procedure
Module Procedure GetOptArgValue_REAL64
  if (present(VarOpt)) then; VarLoc = VarOpt; else; VarLoc = VarDef; end if                                     ! Setting the local variable to the optional variable if present or to the default variable if absent
End Procedure
Module Procedure GetOptArgValue_REAL128
  if (present(VarOpt)) then; VarLoc = VarOpt; else; VarLoc = VarDef; end if                                     ! Setting the local variable to the optional variable if present or to the default variable if absent
End Procedure
Module Procedure GetOptArgValue_CHAR
  if (present(VarOpt)) then; VarLoc = VarOpt; else; VarLoc = VarDef; end if                                     ! Setting the local variable to the optional variable if present or to the default variable if absent
End Procedure



! ============================================================
# define _ProcName_(_VarKind_) \
Module Procedure CONCAT3(SetOptArg_,_VarKind_,_0d);\
  if (present(VarOpt)) VarOpt = Var;\
End Procedure
_ProcName_(LOG)
_ProcName_(INT8)
_ProcName_(INT16)
_ProcName_(INT32)
_ProcName_(INT64)
_ProcName_(REAL32)
_ProcName_(REAL64)
_ProcName_(REAL128)
_ProcName_(CHAR)
# undef _ProcName_
! ============================================================
# define _ProcName_(_VarKind_) \
Module Procedure CONCAT3(SetOptArg_,_VarKind_,_1d);\
  if (present(VarOpt)) VarOpt = Var;\
End Procedure
_ProcName_(LOG)
_ProcName_(INT8)
_ProcName_(INT16)
_ProcName_(INT32)
_ProcName_(INT64)
_ProcName_(REAL32)
_ProcName_(REAL64)
_ProcName_(REAL128)
_ProcName_(CHAR)
# undef _ProcName_
! ============================================================
# define _ProcName_(_VarKind_) \
Module Procedure CONCAT3(SetOptArg_,_VarKind_,_2d);\
  if (present(VarOpt)) VarOpt = Var;\
End Procedure
_ProcName_(LOG)
_ProcName_(INT8)
_ProcName_(INT16)
_ProcName_(INT32)
_ProcName_(INT64)
_ProcName_(REAL32)
_ProcName_(REAL64)
_ProcName_(REAL128)
_ProcName_(CHAR)
# undef _ProcName_
! ============================================================
# define _ProcName_(_VarKind_) \
Module Procedure CONCAT3(SetOptArg_,_VarKind_,_3d);\
  if (present(VarOpt)) VarOpt = Var;\
End Procedure
_ProcName_(LOG)
_ProcName_(INT8)
_ProcName_(INT16)
_ProcName_(INT32)
_ProcName_(INT64)
_ProcName_(REAL32)
_ProcName_(REAL64)
_ProcName_(REAL128)
_ProcName_(CHAR)
# undef _ProcName_
! ============================================================
# define _ProcName_(_VarKind_) \
Module Procedure CONCAT3(SetOptArg_,_VarKind_,_4d);\
  if (present(VarOpt)) VarOpt = Var;\
End Procedure
_ProcName_(LOG)
_ProcName_(INT8)
_ProcName_(INT16)
_ProcName_(INT32)
_ProcName_(INT64)
_ProcName_(REAL32)
_ProcName_(REAL64)
_ProcName_(REAL128)
_ProcName_(CHAR)
# undef _ProcName_
! ============================================================


!
! ! ============================================================
!
! Module Procedure SetOptArg_LOG_0d;     if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT8_0d;    if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT16_0d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT32_0d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT64_0d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL32_0d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL64_0d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL128_0d; if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_CHAR_0d;    if (present(VarOpt)) VarOpt = Var; End Procedure
!
!
! Module Procedure SetOptArg_LOG_1d;     if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT8_1d;    if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT16_1d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT32_1d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT64_1d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL32_1d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL64_1d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL128_1d; if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_CHAR_1d;    if (present(VarOpt)) VarOpt = Var; End Procedure
!
!
! Module Procedure SetOptArg_LOG_2d;     if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT8_2d;    if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT16_2d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT32_2d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT64_2d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL32_2d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL64_2d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL128_2d; if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_CHAR_2d;    if (present(VarOpt)) VarOpt = Var; End Procedure
!
!
! Module Procedure SetOptArg_LOG_3d;     if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT8_3d;    if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT16_3d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT32_3d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT64_3d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL32_3d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL64_3d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL128_3d; if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_CHAR_3d;    if (present(VarOpt)) VarOpt = Var; End Procedure
!
!
! Module Procedure SetOptArg_LOG_4d;     if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT8_4d;    if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT16_4d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT32_4d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_INT64_4d;   if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL32_4d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL64_4d;  if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_REAL128_4d; if (present(VarOpt)) VarOpt = Var; End Procedure
! Module Procedure SetOptArg_CHAR_4d;    if (present(VarOpt)) VarOpt = Var; End Procedure
! ! ============================================================


! **************************************************************************************************************
!                             PROCEDURES FOR LOCATING AN ELEMENT IN AN ARRAY
! **************************************************************************************************************
Module Procedure LocateValue_INT8
# include "LocateValue_Inline.F90"
End Procedure
Module Procedure LocateValue_INT16
# include "LocateValue_Inline.F90"
End Procedure
Module Procedure LocateValue_INT32
# include "LocateValue_Inline.F90"
End Procedure
Module Procedure LocateValue_INT64
# include "LocateValue_Inline.F90"
End Procedure
Module Procedure LocateValue_REAL32
# include "LocateValue_Inline.F90"
End Procedure
Module Procedure LocateValue_REAL64
# include "LocateValue_Inline.F90"
End Procedure
Module Procedure LocateValue_REAL128
# include "LocateValue_Inline.F90"
End Procedure

! **************************************************************************************************************
!                             PROCEDURES FOR ADDING AN ELEMENT TO AN ARRAY
! **************************************************************************************************************
! Logical
! -------
# define  _Is_Logical_      1
# define  _VarType_         logical

# define  _ProcedureName_   AddVar0dToVar1d_Logical
# include "AddVar0dToVar1d_Inline.F90"

# define  _ProcedureName_   AddVar1dToVar1d_Logical
# include "AddVar1dToVar1d_Inline.F90"

# define  _ProcedureName_   AddVar1dToVar2d_Logical
# include "AddVar1dToVar2d_Inline.F90"

# define  _ProcedureName_   AddVar2dToVar2d_Logical
# include "AddVar2dToVar2d_Inline.F90"

# undef   _VarType_
# undef   _Is_Logical_

! INT8
! ----
# define  _Is_integer_      1
# define  _VarType_         integer(INT8)
# define  _VarKind_         INT8

# define  _ProcedureName_   CONCAT(AddVar0dToVar1d_,_VarKind_)
# include "AddVar0dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar1d_,_VarKind_)
# include "AddVar1dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar2d_,_VarKind_)
# include "AddVar1dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar2dToVar2d_,_VarKind_)
# include "AddVar2dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar3dToVar4d_,_VarKind_)
# include "AddVar3dToVar4d_Inline.F90"

# undef   _VarType_
# undef   _VarKind_

! INT16
! -----
# define  _VarType_         integer(INT16)
# define  _VarKind_         INT16

# define  _ProcedureName_   CONCAT(AddVar0dToVar1d_,_VarKind_)
# include "AddVar0dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar1d_,_VarKind_)
# include "AddVar1dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar2d_,_VarKind_)
# include "AddVar1dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar2dToVar2d_,_VarKind_)
# include "AddVar2dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar3dToVar4d_,_VarKind_)
# include "AddVar3dToVar4d_Inline.F90"

# undef   _VarType_
# undef   _VarKind_

! INT32
! -----
# define  _VarType_         integer(INT32)
# define  _VarKind_         INT32

# define  _ProcedureName_   CONCAT(AddVar0dToVar1d_,_VarKind_)
# include "AddVar0dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar1d_,_VarKind_)
# include "AddVar1dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar2d_,_VarKind_)
# include "AddVar1dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar2dToVar2d_,_VarKind_)
# include "AddVar2dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar3dToVar4d_,_VarKind_)
# include "AddVar3dToVar4d_Inline.F90"

# undef   _VarType_
# undef   _VarKind_

! INT64
! -----
# define  _VarType_         integer(INT64)
# define  _VarKind_         INT64

# define  _ProcedureName_   CONCAT(AddVar0dToVar1d_,_VarKind_)
# include "AddVar0dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar1d_,_VarKind_)
# include "AddVar1dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar2d_,_VarKind_)
# include "AddVar1dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar2dToVar2d_,_VarKind_)
# include "AddVar2dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar3dToVar4d_,_VarKind_)
# include "AddVar3dToVar4d_Inline.F90"

# undef   _VarType_
# undef   _VarKind_
# undef   _Is_integer_

! REAL32
! -----
# define  _Is_real_       1
# define  _VarType_         real(REAL32)
# define  _VarKind_         REAL32

# define  _ProcedureName_   CONCAT(AddVar0dToVar1d_,_VarKind_)
# include "AddVar0dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar1d_,_VarKind_)
# include "AddVar1dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar2d_,_VarKind_)
# include "AddVar1dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar2dToVar2d_,_VarKind_)
# include "AddVar2dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar3dToVar4d_,_VarKind_)
# include "AddVar3dToVar4d_Inline.F90"

# undef   _VarType_
# undef   _VarKind_

! REAL64
! ------
# define  _VarType_         real(REAL64)
# define  _VarKind_         REAL64

# define  _ProcedureName_   CONCAT(AddVar0dToVar1d_,_VarKind_)
# include "AddVar0dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar1d_,_VarKind_)
# include "AddVar1dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar2d_,_VarKind_)
# include "AddVar1dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar2dToVar2d_,_VarKind_)
# include "AddVar2dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar3dToVar4d_,_VarKind_)
# include "AddVar3dToVar4d_Inline.F90"

# undef   _VarType_
# undef   _VarKind_

! REAL128
! ------
# define  _VarType_         real(REAL128)
# define  _VarKind_         REAL128

# define  _ProcedureName_   CONCAT(AddVar0dToVar1d_,_VarKind_)
# include "AddVar0dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar1d_,_VarKind_)
# include "AddVar1dToVar1d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar1dToVar2d_,_VarKind_)
# include "AddVar1dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar2dToVar2d_,_VarKind_)
# include "AddVar2dToVar2d_Inline.F90"

# define  _ProcedureName_   CONCAT(AddVar3dToVar4d_,_VarKind_)
# include "AddVar3dToVar4d_Inline.F90"

# undef   _VarType_
# undef   _VarKind_
# undef   _Is_real_

! Character
! ---------
# define  _Is_Character_  1
# define  _VarType_         character(:)

# define  _ProcedureName_   AddVar0dToVar1d_CHAR
# include "AddVar0dToVar1d_Inline.F90"

# define  _ProcedureName_   AddVar1dToVar1d_CHAR
# include "AddVar1dToVar1d_Inline.F90"

# define  _ProcedureName_   AddVar1dToVar2d_CHAR
# include "AddVar1dToVar2d_Inline.F90"

# define  _ProcedureName_   AddVar2dToVar2d_CHAR
# include "AddVar2dToVar2d_Inline.F90"

# undef   _VarType_
# undef   _Is_Character_



! Nothing is done if:
! * the procedure if the Array is not allocated
Module Procedure RemoveElementFromArray_CHAR1d_CHAR0d
  integer                                                               ::  i
  integer                                                               ::  NOut
  integer                                                               ::  Length
  if ( .Not. allocated(Array) ) return
  i   =   GetPosition( Element, Array, CaseSensitive=CaseSensitive )
  if ( i == 0 ) return
  call RemoveElementFromArray(Array,i)
End Procedure


! Nothing is done if:
! * the procedure if the Array is not allocated
! * the index of the element to be removed is out-of-bound
Module Procedure RemoveElementFromArray_CHAR1d_From_INT0d
  character(:)  ,dimension(:)   ,allocatable                            ::  TmpArray
  integer                                                               ::  k, j                                  ! Index of elements
  integer                                                               ::  NOut
  integer                                                               ::  Length
  if ( .Not. allocated(Array) ) return
  NOut    =   size(Array)
  if ( (i<1) .or. (i>NOut) ) return

  Length  =   0
  do k = 1,NOut
    if ( k == i ) cycle
    Length  =   max( Length, len_trim(Array(k)) )
  end do
  allocate( character(Length) :: TmpArray(NOut-1) )

  j   =   0
  do k = 1,NOut
    if ( k == i ) cycle
    j         =   j + 1
    TmpArray(j) =   Array(k)
  end do

  call move_alloc( TmpArray, Array )                                                                       ! Transfering the values from the temporary variable to the output variable
End Procedure

Module Procedure RemoveElementFromArray_CHAR1d_From_Mask
  character(:)  ,dimension(:)   ,allocatable                            ::  TmpArray
  integer                                                               ::  i, j                                  ! Index of elements
  integer                                                               ::  NElements
  integer                                                               ::  Length
  if ( .Not. allocated(Array) ) return

  Length      =   0
  NElements   =   0
  do i = 1,size(Array)
    if ( Mask(i) ) cycle
    NElements =   NElements + 1
    Length    =   max( Length, len_trim(Array(i)) )
  end do
  allocate( character(Length) :: TmpArray(NElements) )

  j   =   0
  do i = 1,size(Array)
    if ( Mask(i) ) cycle
    j           =   j + 1
    TmpArray(j) =   Array(i)
  end do

  call move_alloc( TmpArray, Array )

End Procedure


Module Procedure RemoveElementFromArray_REAL64_3d_From_Index

  real(REAL64)  ,allocatable                                            ::  Copy(:,:,:)
  integer                                                               ::  NOut
  integer                                                               ::  N1, N2, N3
  integer                                                               ::  i1, i2, i3
  integer                                                               ::  j1, j2, j3

  if ( .Not. allocated(Array) ) return
  if ( (Dim < 1) .or. (Dim > 3)  ) return

  NOut    =   size(Array,dim=Dim)

  if ( (Idx<1) .or. (Idx>NOut) ) return

  N1    =   size(Array,1)
  N2    =   size(Array,2)
  N3    =   size(Array,3)

  select case (Dim)

    case (1)
      allocate( Copy(1:N1-1,1:N2,1:N3) )
      do i3 = 1,N3
        j3      =   j3 + 1
        j2      =   0
        do i2 = 1,N2
          j2    =   j2 + 1
          j1    =   0
          do i1 = 1,N1
            if ( i1 == Idx ) cycle
            j1  =   j1 + 1
            Copy(j1,j2,j3) =   Array(i1,i2,i3)
          end do
        end do
      end do

    case (2)
      allocate( Copy(1:N1,1:N2-1,1:N3) )
      do i3 = 1,N3
        j3      =   j3 + 1
        j2      =   0
        do i2 = 1,N2
          if ( i2 == Idx ) cycle
          j2    =   j2 + 1
          j1    =   0
          do i1 = 1,N1
            j1  =   j1 + 1
            Copy(j1,j2,j3) =   Array(i1,i2,i3)
          end do
        end do
      end do

    case (3)
      allocate( Copy(1:N1,1:N2,1:N3-1) )
      do i3 = 1,N3
        if ( i3 == Idx ) cycle
        j3      =   j3 + 1
        j2      =   0
        do i2 = 1,N2
          j2    =   j2 + 1
          j1    =   0
          do i1 = 1,N1
            j1  =   j1 + 1
            Copy(j1,j2,j3) =   Array(i1,i2,i3)
          end do
        end do
      end do

  end select

  call move_alloc( Copy, Array )

End Procedure


! **************************************************************************************************************
!       PROCEDURES FOR GETTING THE POSITION OF A ELEMENT IN AN ARRAY
! **************************************************************************************************************

! INT8
! ----

# define  _VarType_         integer(INT8)

Module Procedure GetPosition_INT8_0d
  integer                                                               ::  i                                   ! Index of elements
  integer                                                               ::  iStart                              ! Starting index used to found the element position
  integer                                                               ::  iStop                               ! Stopping index used to found the element position
  integer                                                               ::  iStep                               ! Step in index incrementation used to found the element position
  character(:)  ,allocatable                                            ::  CurrentElement
  iLoc          =   0                                                                                           ! Initialization of the position index
  iStart        =   1                                                                                           ! Setting the starting index to unity
  iStop         =   size(Array)                                                                                 ! Setting the stopping index to the dimension of the list of elements
  iStep         =   1                                                                                           ! Setting index step to one
  if ( present(Back) ) then                                                                                     ! If the "Back" optional argument is present
    if (Back) then                                                                                              ! If the "Back" argument is true
      iStart    =    size(Array)                                                                                ! Setting the starting index to the dimension of the list of elements
      iStop     =    1                                                                                          ! Setting the stopping index to unity
      iStep     =   -1                                                                                          ! Setting index step to -1 to go backward
    end if                                                                                                      ! End if case on the argument value
  end if                                                                                                        ! End if case on optional argument presence
  do i = iStart,iStop,iStep                                                                                     ! Loop on all elements
    if ( Element /= Array(i) ) cycle                                                                            ! If current element from the list is different from the searched element, then going to the next element
    iLoc    =   i                                                                                               ! Setting the index if the element
    return                                                                                                      ! Exiting the procedure if no counting of occurence is required
  end do                                                                                                        ! End do loop on elements
End Procedure

Pure Module Function GetPosition_INT8_1D( Elements, Array, Back ) result(iLoc)
  _VarType_                                             ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
  _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of element used for presence checking
  logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of elements
  integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input elements in the list of elements
  integer                                                               ::  i                                   ! Index of element of the input element in the list of elements
  do i = 1,size(Elements)                                                                                       ! Loop on all elements to be cheched for presence
     iLoc(i)  =   GetPosition_INT8_0d( Elements(i), Array, Back )                                                       ! Finding the position of the current element in the list of elements
  end do                                                                                                        ! End loop on elements to be cheched for presence
End Function

Pure Module Subroutine GetPositions_INT8_0d( Element, Array, Positions )
  _VarType_                                             ,intent(in)     ::  Element                             !< String to be checked for presence
  _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
  integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
  integer                                                               ::  i                                   ! Index of element of the input element in the list of elements
  do i = 1,size(Array)
    if ( Element == Array(i) ) call AddElementToArray( i, Positions )                               ! If the two elements match, then adding the position of current element to the output list of positions
  end do                                                                                                        ! End do loop on elements
  if ( .Not. allocated(Positions) ) allocate(Positions(0))
End Subroutine

# undef   _VarType_


! INT16
! -----

# define  _VarType_         integer(INT16)

Module Procedure GetPosition_INT16_0d
  integer                                                               ::  i                                   ! Index of elements
  integer                                                               ::  iStart                              ! Starting index used to found the element position
  integer                                                               ::  iStop                               ! Stopping index used to found the element position
  integer                                                               ::  iStep                               ! Step in index incrementation used to found the element position
  character(:)  ,allocatable                                            ::  CurrentElement
  iLoc          =   0                                                                                           ! Initialization of the position index
  iStart        =   1                                                                                           ! Setting the starting index to unity
  iStop         =   size(Array)                                                                                 ! Setting the stopping index to the dimension of the list of elements
  iStep         =   1                                                                                           ! Setting index step to one
  if ( present(Back) ) then                                                                                     ! If the "Back" optional argument is present
    if (Back) then                                                                                              ! If the "Back" argument is true
      iStart    =    size(Array)                                                                                ! Setting the starting index to the dimension of the list of elements
      iStop     =    1                                                                                          ! Setting the stopping index to unity
      iStep     =   -1                                                                                          ! Setting index step to -1 to go backward
    end if                                                                                                      ! End if case on the argument value
  end if                                                                                                        ! End if case on optional argument presence
  do i = iStart,iStop,iStep                                                                                     ! Loop on all elements
    if ( Element /= Array(i) ) cycle                                                                            ! If current element from the list is different from the searched element, then going to the next element
    iLoc    =   i                                                                                               ! Setting the index if the element
    return                                                                                                      ! Exiting the procedure if no counting of occurence is required
  end do                                                                                                        ! End do loop on elements
End Procedure

Pure Module Function GetPosition_INT16_1D( Elements, Array, Back ) result(iLoc)
  _VarType_                                             ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
  _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of element used for presence checking
  logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of elements
  integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input elements in the list of elements
  integer                                                               ::  i                                   ! Index of element of the input element in the list of elements
  do i = 1,size(Elements)                                                                                       ! Loop on all elements to be cheched for presence
     iLoc(i)  =   GetPosition_INT16_0d( Elements(i), Array, Back )                                                       ! Finding the position of the current element in the list of elements
  end do                                                                                                        ! End loop on elements to be cheched for presence
End Function

Pure Module Subroutine GetPositions_INT16_0d( Element, Array, Positions )
  _VarType_                                             ,intent(in)     ::  Element                             !< String to be checked for presence
  _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
  integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
  integer                                                               ::  i                                   ! Index of element of the input element in the list of elements
  do i = 1,size(Array)
    if ( Element == Array(i) ) call AddElementToArray( i, Positions )                               ! If the two elements match, then adding the position of current element to the output list of positions
  end do                                                                                                        ! End do loop on elements
  if ( .Not. allocated(Positions) ) allocate(Positions(0))
End Subroutine

# undef   _VarType_



! INT32
! -----

# define  _VarType_         integer(INT32)

Module Procedure GetPosition_INT32_0d
  integer                                                               ::  i                                   ! Index of elements
  integer                                                               ::  iStart                              ! Starting index used to found the element position
  integer                                                               ::  iStop                               ! Stopping index used to found the element position
  integer                                                               ::  iStep                               ! Step in index incrementation used to found the element position
  character(:)  ,allocatable                                            ::  CurrentElement
  iLoc          =   0                                                                                           ! Initialization of the position index
  iStart        =   1                                                                                           ! Setting the starting index to unity
  iStop         =   size(Array)                                                                                 ! Setting the stopping index to the dimension of the list of elements
  iStep         =   1                                                                                           ! Setting index step to one
  if ( present(Back) ) then                                                                                     ! If the "Back" optional argument is present
    if (Back) then                                                                                              ! If the "Back" argument is true
      iStart    =    size(Array)                                                                                ! Setting the starting index to the dimension of the list of elements
      iStop     =    1                                                                                          ! Setting the stopping index to unity
      iStep     =   -1                                                                                          ! Setting index step to -1 to go backward
    end if                                                                                                      ! End if case on the argument value
  end if                                                                                                        ! End if case on optional argument presence
  do i = iStart,iStop,iStep                                                                                     ! Loop on all elements
    if ( Element /= Array(i) ) cycle                                                                            ! If current element from the list is different from the searched element, then going to the next element
    iLoc    =   i                                                                                               ! Setting the index if the element
    return                                                                                                      ! Exiting the procedure if no counting of occurence is required
  end do                                                                                                        ! End do loop on elements
End Procedure

Pure Module Function GetPosition_INT32_1D( Elements, Array, Back ) result(iLoc)
  _VarType_                                             ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
  _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of element used for presence checking
  logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of elements
  integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input elements in the list of elements
  integer                                                               ::  i                                   ! Index of element of the input element in the list of elements
  do i = 1,size(Elements)                                                                                       ! Loop on all elements to be cheched for presence
     iLoc(i)  =   GetPosition_INT32_0d( Elements(i), Array, Back )                                                       ! Finding the position of the current element in the list of elements
  end do                                                                                                        ! End loop on elements to be cheched for presence
End Function

Pure Module Subroutine GetPositions_INT32_0d( Element, Array, Positions )
  _VarType_                                             ,intent(in)     ::  Element                             !< String to be checked for presence
  _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
  integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
  integer                                                               ::  i                                   ! Index of element of the input element in the list of elements
  do i = 1,size(Array)
    if ( Element == Array(i) ) call AddElementToArray( i, Positions )                               ! If the two elements match, then adding the position of current element to the output list of positions
  end do                                                                                                        ! End do loop on elements
  if ( .Not. allocated(Positions) ) allocate(Positions(0))
End Subroutine

# undef   _VarType_



! INT64
! -----

# define  _VarType_         integer(INT64)

Module Procedure GetPosition_INT64_0d
  integer                                                               ::  i                                   ! Index of elements
  integer                                                               ::  iStart                              ! Starting index used to found the element position
  integer                                                               ::  iStop                               ! Stopping index used to found the element position
  integer                                                               ::  iStep                               ! Step in index incrementation used to found the element position
  character(:)  ,allocatable                                            ::  CurrentElement
  iLoc          =   0                                                                                           ! Initialization of the position index
  iStart        =   1                                                                                           ! Setting the starting index to unity
  iStop         =   size(Array)                                                                                 ! Setting the stopping index to the dimension of the list of elements
  iStep         =   1                                                                                           ! Setting index step to one
  if ( present(Back) ) then                                                                                     ! If the "Back" optional argument is present
    if (Back) then                                                                                              ! If the "Back" argument is true
      iStart    =    size(Array)                                                                                ! Setting the starting index to the dimension of the list of elements
      iStop     =    1                                                                                          ! Setting the stopping index to unity
      iStep     =   -1                                                                                          ! Setting index step to -1 to go backward
    end if                                                                                                      ! End if case on the argument value
  end if                                                                                                        ! End if case on optional argument presence
  do i = iStart,iStop,iStep                                                                                     ! Loop on all elements
    if ( Element /= Array(i) ) cycle                                                                            ! If current element from the list is different from the searched element, then going to the next element
    iLoc    =   i                                                                                               ! Setting the index if the element
    return                                                                                                      ! Exiting the procedure if no counting of occurence is required
  end do                                                                                                        ! End do loop on elements
End Procedure

Pure Module Function GetPosition_INT64_1D( Elements, Array, Back ) result(iLoc)
  _VarType_                                             ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
  _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of element used for presence checking
  logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of elements
  integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input elements in the list of elements
  integer                                                               ::  i                                   ! Index of element of the input element in the list of elements
  do i = 1,size(Elements)                                                                                       ! Loop on all elements to be cheched for presence
     iLoc(i)  =   GetPosition_INT64_0d( Elements(i), Array, Back )                                                       ! Finding the position of the current element in the list of elements
  end do                                                                                                        ! End loop on elements to be cheched for presence
End Function

Pure Module Subroutine GetPositions_INT64_0d( Element, Array, Positions )
  _VarType_                                             ,intent(in)     ::  Element                             !< String to be checked for presence
  _VarType_                                             ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
  integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
  integer                                                               ::  i                                   ! Index of element of the input element in the list of elements
  do i = 1,size(Array)
    if ( Element == Array(i) ) call AddElementToArray( i, Positions )                               ! If the two elements match, then adding the position of current element to the output list of positions
  end do                                                                                                        ! End do loop on elements
  if ( .Not. allocated(Positions) ) allocate(Positions(0))
End Subroutine

# undef   _VarType_


! **************************************************************************************************************
!       PROCEDURES FOR GETTING THE POSITION OF A STRING IN A LIST OF STRINGS
! **************************************************************************************************************
Module Procedure GetPosition_CHAR_0d
  integer                                                               ::  i                                   ! Index of string of the input string in the list of strings
  integer                                                               ::  iStart                              ! Starting index used to found the string position
  integer                                                               ::  iStop                               ! Stopping index used to found the string position
  integer                                                               ::  iStep                               ! Step in index incrementation used to found the string position
  logical                                                               ::  CaseSensitive_
  character(:)  ,allocatable                                            ::  TargetElement
  character(:)  ,allocatable                                            ::  CurrentElement
  CaseSensitive_ = .True.; if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive
  iLoc          =   0                                                                                           ! Initialization of the position index
  iStart        =   1                                                                                           ! Setting the starting index to unity
  iStop         =   size(Array)                                                                                 ! Setting the stopping index to the dimension of the list of strings
  iStep         =   1                                                                                           ! Setting index step to one
  if ( present(Back) ) then                                                                                     ! If the "Back" optional argument is present
    if (Back) then                                                                                              ! If the "Back" argument is true
      iStart    =    size(Array)                                                                                ! Setting the starting index to the dimension of the list of strings
      iStop     =    1                                                                                          ! Setting the stopping index to unity
      iStep     =   -1                                                                                          ! Setting index step to -1 to go backward
    end if                                                                                                      ! End if case on the argument value
  end if                                                                                                        ! End if case on optional argument presence
  TargetElement =   trim(Element)
  if ( .Not. CaseSensitive_ ) TargetElement = UpperCase(TargetElement)
  do i = iStart,iStop,iStep                                                                                     ! Loop on all strings
    CurrentElement  =   trim(Array(i))
    if ( .Not. CaseSensitive_ ) CurrentElement = UpperCase(CurrentElement)
    if ( TargetElement /= CurrentElement ) cycle                                                                ! If current string from the list is different from the searched string, then going to the next element
    iLoc    =   i                                                                                               ! Setting the index if the string
    return                                                                                                      ! Exiting the procedure if no counting of occurence is required
  end do                                                                                                        ! End do loop on strings
End Procedure

Pure Function UpperCase(StrInp) result(StrOut)
  character(*)                  ,intent(in)     ::  StrInp                                                  !<
  character(:)  ,allocatable                                            ::  StrOut                          ! Output character string
  integer                                       ::  i, ilen, ioffset, iquote, iav, iqc
  ilen          =   len_trim(StrInp)
  ioffset       =   iachar('A') - iachar('a')
  iquote        =   0
  StrOut        =   StrInp
  do i = 1,ilen
    iav=iachar(StrInp(i:i))
    if(iquote==0 .and. (iav==34 .or.iav==39)) then
      iquote    =   1
      iqc       =   iav
      cycle
    end if
    if(iquote==1 .and. iav==iqc) then
      iquote    =   0
    cycle
    end if
    if (iquote==1) cycle
    if(iav >= iachar('a') .and. iav <= iachar('z')) then
      StrOut(i:i)       =   achar(iav+ioffset)
    else
      StrOut(i:i)       =   StrInp(i:i)
    end if
  end do
End Function


! **************************************************************************************************************
!           PROCEDURES FOR CHECKING THAT AND OPTIONAL ARGUMENT IS PRESENT AND TRUE
! **************************************************************************************************************
Module Procedure PresentAndTrue
  if ( present(Var) ) then
    Indicator   =   Var
  else
    Indicator   =   .False.
  end if
End Procedure

Module Procedure PresentAndFalse
  Indicator   =   .False.
  if ( present(Var) ) Indicator = .Not. Var
End Procedure

Module Procedure AbsentOrTrue
  if ( present(Var) ) then
    Indicator   =   Var
  else
    Indicator   =   .True.
  end if
End Procedure

Module Procedure AbsentOrFalse
  Indicator   =   .True.
  if ( present(Var) ) Indicator = .Not. Var
End Procedure

Module Procedure PresentAndDifferentThan    ! PresentAndNonEqualTo
  Indicator   =   .False.
  if ( present(Var) ) Indicator = ( Var /= RefVal )
End Procedure

Module Procedure PresentAndNotEmpty
  if ( present(Var) ) then
    Indicator   =   len_trim(Var) /= 0
  else
    Indicator   =   .False.
  end if
End Procedure

Module Procedure WorkaroundCharacterAssignment
  Out   =   Inp
End Procedure

!
! ! Logical
! ! -------
! # define  _VarType_         logical
! # define  _ProcedureName_   PresentAndTrue_Logical_0d
! # include "PresentAndTrue0d_Inline.F90"
! # undef   _VarType_
!
! ! INT8
! ! ----
! # define  _VarType_         integer(INT8)
! # define  _ProcedureName_   PresentAndTrue_INT8_0d
! # include "PresentAndTrue0d_Inline.F90"
! # undef   _VarType_
!
! ! INT16
! ! -----
! # define  _VarType_         integer(INT16)
! # define  _ProcedureName_   PresentAndTrue_INT16_0d
! # include "PresentAndTrue0d_Inline.F90"
! # undef   _VarType_
!
! ! INT32
! ! -----
! # define  _VarType_         integer(INT32)
! # define  _ProcedureName_   PresentAndTrue_INT32_0d
! # include "PresentAndTrue0d_Inline.F90"
! # undef   _VarType_
!
! ! INT64
! ! -----
! # define  _VarType_         integer(INT64)
! # define  _ProcedureName_   PresentAndTrue_INT64_0d
! # include "PresentAndTrue0d_Inline.F90"
! # undef   _VarType_
!
! ! REAL32
! ! -----
! # define  _VarType_         real(REAL32)
! # define  _ProcedureName_   PresentAndTrue_REAL32_0d
! # include "PresentAndTrue0d_Inline.F90"
! # undef   _VarType_
!
! ! REAL64
! ! ------
! # define  _VarType_         real(REAL64)
! # define  _ProcedureName_   PresentAndTrue_REAL64_0d
! # include "PresentAndTrue0d_Inline.F90"
! # undef   _VarType_
!
! ! REAL128
! ! ------
! # define  _VarType_         real(REAL128)
! # define  _ProcedureName_   PresentAndTrue_REAL128_0d
! # include "PresentAndTrue0d_Inline.F90"
! # undef   _VarType_
!
! ! Character
! ! ---------
! # define  _VarType_         character(:)
! # define  _ProcedureName_   PresentAndTrue_CHAR_0d
! # include "PresentAndTrue0d_Inline.F90"
! # undef   _VarType_



Module Procedure SelectedItem
  Indicator =   ( i <= NItemMax ) .or. ( i > NItemTot-NItemMax )
End Procedure

Module Procedure SkippedItem
  Indicator =   .Not. SelectedItem( i, NItemTot, NItemMax )
End Procedure

Module Procedure FirstSkippedItem
  Indicator =   ( i == NItemMax + 1 ) .and. ( NItemTot > 2*NItemMax )
End Procedure

Module Procedure SkippedItemsRange
  integer                                                               ::  iIni, iFin
  character(15)                                                         ::  si, sf
  iIni      =   NItemMax + 1
  iFin      =   NItemTot - NItemMax
  write(si,"(i0)") iIni
  write(sf,"(i0)") iFin
  String    =   "["//trim(si)//":"//trim(sf)//"]"
End Procedure



# define  _ProcedureName_   GetVarRank_REAL64_2d
# define  _VarKind_         REAL64
# define  _NumDims_         3
#   include "GetVarRank_Implementation_Inline.F90"

# define  _ProcedureName_   GetVarRank_REAL64_3d
# define  _VarKind_         REAL64
# define  _NumDims_         2
#   include "GetVarRank_Implementation_Inline.F90"


Module Procedure GetSourceLocation
  character(*)                                              ,parameter  ::  UNKNOWN_FILE_NAME   = '<unknown file>'
  character(1000)                                                       ::  LongString
  integer                                                               ::  Status
  if ( len_trim(FileName) == 0 ) then
    if ( LineNumber <= 0 ) then
      String  =   '<unknown location>'
    else
      write(LongString,'(a,":",i0)', iostat=Status) trim(UNKNOWN_FILE_NAME), LineNumber
      String  =   trim(LongString)
    end if
  else
    if ( LineNumber <= 0 ) then
      String  =   trim(FileName)
    else
      write(LongString,'(a,":",i0)', iostat=Status) trim(FileName), LineNumber
      String  =   trim(LongString)
    end if
  end if
  String  = '[' // trim(String) // ']'
End Procedure

End SubModule