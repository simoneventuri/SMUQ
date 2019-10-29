! File: test_AddVar0dToVar1d_Inline.F90
Block

  _VarType_     ,allocatable  ::  Expected(:), Obtained(:)
  _VarType_     ,allocatable  ::  A0
! =======================================================================================
  RealName    =   "AddVar0dToVar1d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 0d-var to unallocated 1d-var w/o opt arg."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A0       ) ) deallocate(A0       ); A0  =   5
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected, source = [A0] )
  if ( allocated(Obtained) ) deallocate(Obtained)
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:  ", A0, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", "<unallocated>" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "<none>" )
  call AddElementToArray( A0, Obtained )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected        = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained        = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected, Description )
  call Logger%Write( "[ok]" )
! =======================================================================================
  RealName    =   "AddVar0dToVar1d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 0d-var to allocated 1d-var w/o opt arg."
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A0       ) ) deallocate(A0       );
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(4) )
  if ( allocated(Obtained) ) deallocate(Obtained); allocate( Obtained(4) )
  A0          =   22
  Obtained   =   [   1,   2,   3,   4 ]
  Expected   =   [   1,   2,   3,   4, 22 ]
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:   ", A0, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", Obtained, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "<none>" )
  call AddElementToArray( A0, Obtained )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected        = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained        = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected, Description )
  call Logger%Write( "[ok]" )
! =======================================================================================
  RealName    =   "AddVar0dToVar1d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 0d-var to unallocated 1d-var with 'at=3'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A0       ) ) deallocate(A0       );
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(4) )
  if ( allocated(Obtained) ) deallocate(Obtained); allocate( Obtained(4) )
  A0          =   22
  Obtained   =   [   1,   2,   3,   4 ]
  Expected   =   [   1,   2,   22, 3,   4 ]
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:   ", A0, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", Obtained, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "at=3" )
  call AddElementToArray( A0, Obtained, at=3 )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected        = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained        = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected, Description )
  call Logger%Write( "[ok]" )
! =======================================================================================
  RealName    =   "AddVar0dToVar1d" // "_" // STRINGIFY(_VarKind_)
  Description =   "'"//RealName//"': add 0d-var to 1d-var with 'OnlyIfAbsent=.True.'"
  call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
  if (Detailed) call Logger%Write( "" )
  if ( allocated(A0       ) ) deallocate(A0       );
  if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(4) )
  if ( allocated(Obtained) ) deallocate(Obtained); allocate( Obtained(4) )
  A0          =   22
  Obtained   =   [   1,   2,   3,   4 ]
  Expected   =   [   1,   2,   3,   4, 22 ]
  if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
  if (Detailed) call Logger%Write( "  -> Add Element:   ", A0, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> to Array:      ", Obtained, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> with opts:     ", "OnlyIfAbsent=.True." )
  call AddElementToArray( A0, Obtained, OnlyIfAbsent=.True. )
  if (Detailed) call Logger%Write( "-> Results" )
  if (Detailed) call Logger%Write( "  -> Expected        = ", Expected, Fi="i3", Fr="f4.1" )
  if (Detailed) call Logger%Write( "  -> Obtained        = ", Obtained, Fi="i3", Fr="f4.1" )
  @assertEqual( Obtained , Expected, Description )
  call Logger%Write( "[ok]" )
! ! =======================================================================================
! ! FAILING TEST !!!
! ! =======================================================================================
!   RealName  =   "AddVar0dToVar1d" // "_" // GetVarKind(STRINGIFY(_VarType_))
! !   RealName  =   "AddVar0dToVar1d" // "_" // "&
!                                           _VarKind_&
!                                           "
!   Description   =   "'"//GenericName//"' ("//RealName//") add 0d-var to unallocated 1d-var w/o opt arg."
!   call Logger%Write( "Testing ", SetLength(Description,NPad,Pad="."), Advance=.False. )
!   if (Detailed) call Logger%Write( "" )
!   if (Detailed) call Logger%Write( "  Addition of a scalar to a allocated vector" )
!   if (Detailed) call Logger%Write( "  with the option 'OnlyIfAbsent' when the value" )
!   if (Detailed) call Logger%Write( "  to add is present (and so it should not be added)" )
!   if ( allocated(A0       ) ) deallocate(A0       );
!   if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(4) )
!   if ( allocated(Obtained) ) deallocate(Obtained); allocate( Obtained(4) )
!   A0          =   2
!   Obtained   =   [   1,   2,   3,   4 ]
!   Expected   =   [   1,   2,   3,   4 ]
!   if (Detailed) call Logger%Write( "-> Calling AddElementToArray" )
!   if (Detailed) call Logger%Write( "  -> Add Element:   ", A0, Fi="i3", Fr="f4.1" )
!   if (Detailed) call Logger%Write( "  -> to Array:      ", Obtained, Fi="i3", Fr="f4.1" )
!   if (Detailed) call Logger%Write( "  -> with opts:     ", "OnlyIfAbsent=.True." )
!   call AddElementToArray( A0, Obtained, OnlyIfAbsent=.True. )
!   if (Detailed) call Logger%Write( "-> Results" )
!   if (Detailed) call Logger%Write( "  -> Expected        = ", Expected, Fi="i3", Fr="f4.1" )
!   if (Detailed) call Logger%Write( "  -> Obtained        = ", Obtained, Fi="i3", Fr="f4.1" )
!   @assertEqual( Obtained , Expected, Description )
!   call Logger%Write( "[ok]" )
!   NOT YET IMPLEMENTED
! ! =======================================================================================
!   RealName  =   "AddVar0dToVar1d" // "_" // GetVarKind(STRINGIFY(_VarType_))
!   RealName  =   "AddVar0dToVar1d" // "_" // "&
!                                           _VarKind_&
!                                           "
!   Info      =   "with opt. 'OnlyIfAbsent' when the value is present (and so it should NOT be added)"
!   Description   =   "'"//GenericName//"' ("//RealName//") add 0d-var to unallocated 1d-var w/o opt arg." // Info
!   call Logger%Write( "Testing ", SetLength(Description,NPad,Pad="."), Advance=.False. )
!   if (Detailed) call Logger%Write( "" )
!   if ( allocated(A0       ) ) deallocate(A0       );
!   if ( allocated(Expected) ) deallocate(Expected); allocate( Expected(4) )
!   if ( allocated(Obtained) ) deallocate(Obtained); allocate( Obtained(4) )
!   A0          =   3
!   Obtained   =   [   1,   2,   3,   4 ]
!   Expected   =   [   1,   2,   3,   4 ]
!   call AddElementToArray( A0, Obtained )
!   if (Detailed) call Logger%Write( "-> Expected = ", Expected, Fi="i3", Fr="f4.1" )
!   if (Detailed) call Logger%Write( "-> Obtained = ", Obtained, Fi="i3", Fr="f4.1" )
!   @assertEqual( Obtained , Expected, Description )
!   call Logger%Write( "[ok]" )
! =======================================================================================

End Block

# undef _VarType_
# undef _VarKind_
