Module Procedure CONCAT(IsIncluded_,_VarKind_)_0d
  integer                                                               ::  i
  Included    =   .False.
  do i = 1,size(List)
    if ( Var /= List(i) ) cycle
    Included  =   .True.
    return
  end do
End Procedure
# undef   _VarKind_