! File: test_AddVar2dToVar2d_Inline.F90
Block
  _VarType_     ,allocatable  ::  Expected(:,:), Obtained(:,:)
  _VarType_     ,allocatable  ::  A2(:,:), B2(:,:)
  integer                     ::  AddDim
! =======================================================================================
  RealName    =   "AddVar2dToVar2d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 2d-var to unallocated 2d-var w/o opt arg."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A2       ) ) deallocate(A2     ); allocate( A2(2,4) )
  if ( allocated(Obtained) ) deallocate(Obtained)
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(2,4) )
  A2(1,:)  =   [   1,   2,   3,   4 ]
  A2(2,:)  =   [   5,   6,   7,   8 ]
  Expected = A2
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:   ", A2, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", "<unallocated>" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "<none>" )
  call AddElementToArray( A2, Obtained )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected )
  call Logger%Write( "[ok]" )
! =======================================================================================
  RealName    =   "AddVar2dToVar2d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 2 2d-var to unallocated 2d-var with 'AddDim=1'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A2       ) ) deallocate(A2     ); allocate( A2(2,4) )
  if ( allocated(B2       ) ) deallocate(B2     ); allocate( B2(3,4) )
  if ( allocated(Obtained) ) deallocate(Obtained)
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(5,4) ); Expected = 0
  AddDim    =   1                     ! For intel v18.0.3, the following initialization fails
  A2(1,:)  =   [   1,   2,   3,   4 ] !; Expected(1,:)  =   A2(1,:)      |
  A2(2,:)  =   [   5,   6,   7,   8 ] !; Expected(2,:)  =   A2(2,:)      /
  B2(1,:)  =   [  10,  20,  30,  40 ] !; Expected(3,:)  =   B2(1,:)     /
  B2(2,:)  =   [  15,  25,  35,  45 ] !; Expected(4,:)  =   B2(2,:)   <-
  B2(3,:)  =   [  55,  66,  77,  88 ] !; Expected(5,:)  =   B2(3,:)
  Expected(1:2,:)  =   transpose( reshape( [A2(1,:),A2(2,:)], [4,2] ) )
  Expected(3:5,:)  =   transpose( reshape( [B2(1,:),B2(2,:),B2(3,:)], [4,3] ) )
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:   ", A2, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> then Element:  ", B2, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", "<unallocated>" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "Dim=1" )
  call AddElementToArray( A2, Obtained, Dim=AddDim )
  call AddElementToArray( B2, Obtained, Dim=AddDim )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected )
  call Logger%Write( "[ok]" )
! =======================================================================================
  RealName    =   "AddVar2dToVar2d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 2 2d-var to unallocated 2d-var with 'AddDim=2'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A2       ) ) deallocate(A2     ); allocate( A2(2,3) )
  if ( allocated(B2       ) ) deallocate(B2     ); allocate( B2(2,4) )
  if ( allocated(Obtained) ) deallocate(Obtained)
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(2,7) )
  A2(1,:) = [1,2,3];      B2(1,:) = [10,20,30,40];     Expected(1,:)  =  [ A2(1,:) , B2(1,:) ]
  A2(2,:) = [4,5,6];      B2(2,:) = [55,66,77,88];     Expected(2,:)  =  [ A2(2,:) , B2(2,:) ]
  AddDim        =   2
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:   ", A2, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> then Element:  ", B2, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", "<unallocated>" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "Dim=2" )
  call AddElementToArray( A2, Obtained, Dim=AddDim )
  call AddElementToArray( B2, Obtained, Dim=AddDim )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected )
  call Logger%Write( "[ok]" )
! =======================================================================================
End Block

# undef _VarType_
# undef _VarKind_
