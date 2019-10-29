! File:   GetVarRank_Implementation_Inline.F90
! Module Procedure _ProcedureName_
Module Procedure CONCAT4(GetVarRank_,_VarKind_,_,_NumDims_)d
  use String_Library          ,only:  s=>Convert_To_String
  character(*)                                              ,parameter  ::  Fmt="g0,:,':',g0"
# if _NumDims_ == 1
    VarRank   =   "[" // s(size(Var,1)) // "]"
# elseif _NumDims_ == 2
    VarRank   =   "[" // s(size(Var,1)) // "," // s(size(Var,2)) // "]"
# elseif _NumDims_ == 3
    VarRank   =   "[" // s(size(Var,1)) // "," // s(size(Var,2)) // "," // s(size(Var,3)) // "]"
# endif

!   VarRank   =   "[" // &
!               s(size(Var,1)) // &
! # if _NumDims_ >= 2
!     "," // &  s(size(Var,2)) // &
! # endif
! # if _NumDims_ >= 3
!     "," // &  s(size(Var,3)) // &
! # endif
!     "]"
! !   VarRank   =   "[" // &
! !               s(lbound(Var,1))//":"//s(ubound(Var,1)) // &
! ! # if _NumDims_ >= 2
! !     "," // &  s(lbound(Var,2))//":"//s(ubound(Var,2)) // &
! ! # endif
! ! # if _NumDims_ >= 3
! !     "," // &  s(lbound(Var,3))//":"//s(ubound(Var,3)) // &
! ! # endif
! !     "]"
End Procedure
# undef   _VarKind_
# undef   _NumDims_
# undef   _ProcedureName_
