! File: test_AddVar1dToVar1d_Inline.F90
Block
  _VarType_     ,allocatable  ::  Expected(:), Obtained(:)
  _VarType_     ,allocatable  ::  A1(:), B1(:)
! =======================================================================================
  RealName    =   "AddVar1dToVar1d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 2 1d-var to unallocated 1d-var w/o opt arg."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A1       ) ) deallocate(A1       ); allocate( A1(3) )
  if ( allocated(B1       ) ) deallocate(B1       ); allocate( B1(2) )
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(5) )
  A1 = [1,2,3] ; Expected(1:3) = A1
  B1 = [4,5]   ; Expected(4:5) = B1
  if ( allocated(Obtained) ) deallocate(Obtained)
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:   ", A1, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> then Element:  ", B1, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", "<unallocated>" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "<none>" )
  call AddElementToArray( A1, Obtained )
  call AddElementToArray( B1, Obtained )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected        = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained        = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected, Description )
  call Logger%Write( "[ok]" )
! =======================================================================================
End Block

# undef _VarType_
# undef _VarKind_
