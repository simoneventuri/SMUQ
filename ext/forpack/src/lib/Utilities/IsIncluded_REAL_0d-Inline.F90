Module Procedure CONCAT(IsIncluded_,_VarKind_)_0d
  integer                                                               ::  i
  real(_VarKind_)                                                       ::  Tol_
  Included    =   .False.
  Tol_        =   GetOptArgValue(real(0,kind=_VarKind_),Tol)
  do i = 1,size(List)
    if ( Var /= List(i) ) cycle
    if ( abs(Var-List(i)) > Tol_ ) cycle
    Included  =   .True.
    return
  end do
End Procedure
# undef   _VarKind_