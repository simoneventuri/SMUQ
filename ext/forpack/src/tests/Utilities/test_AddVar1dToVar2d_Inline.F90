! File: test_AddVar1dToVar2d_Inline.F90
Block
  _VarType_     ,allocatable  ::  Expected(:,:), Obtained(:,:)
  _VarType_     ,allocatable  ::  A1(:)
  integer                     ::  AddDim, i, j
! =======================================================================================
  RealName    =   "AddVar1dToVar2d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 3 1d-var to unallocated 2d-var with 'AddDim=1'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A1       ) ) deallocate(A1     ); allocate( A1(4) )
  if ( allocated(Obtained) ) deallocate(Obtained)
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(3,4) )
  AddDim    =   1
  A1        =   [   1,   2,   3,   4 ]
  forall (i=1:3) Expected(i,:) = A1
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:   ", A1, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> then Element:  ", A1, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> then Element:  ", A1, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", "<unallocated>" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "AddDim=1" )
  call AddElementToArray( A1, Obtained, Dim=AddDim )
  call AddElementToArray( A1, Obtained, Dim=AddDim )
  call AddElementToArray( A1, Obtained, Dim=AddDim )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected )
  call Logger%Write( "[ok]" )
! =======================================================================================
  RealName    =   "AddVar1dToVar2d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 3 1d-var to unallocated 2d-var with 'AddDim=2'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A1       ) ) deallocate(A1     ); allocate( A1(4) )
  if ( allocated(Obtained) ) deallocate(Obtained)
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(4,3) )
  AddDim    =   2
  A1        =   [   1,   2,   3,   4 ]
  forall (j=1:3) Expected(:,j) = A1
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:   ", A1, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> then Element:  ", A1, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> then Element:  ", A1, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", "<unallocated>" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "Dim=2" )
  call AddElementToArray( A1, Obtained, Dim=AddDim )
  call AddElementToArray( A1, Obtained, Dim=AddDim )
  call AddElementToArray( A1, Obtained, Dim=AddDim )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected )
  call Logger%Write( "[ok]" )
! =======================================================================================
End Block

# undef _VarType_
# undef _VarKind_
