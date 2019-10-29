Pure Function _ProcedureName_( Var ) result(Indicator)
  _VarType_                                   ,optional ,intent(in)     ::  Var
  logical                                                               ::  Indicator
  if ( present(Var) ) then
    Indicator = Var
  else
    Indicator = .False.
  end if
End Function
