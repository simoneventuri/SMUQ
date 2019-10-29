  integer                                                                       ::      i
  integer                                                                       ::      NPoints_Loc
  integer(kind(Vector))                                                         ::      Min_Loc
  integer(kind(Vector))                                                         ::      Max_Loc
  integer(kind(Vector))                                                         ::      Step_Loc
  Min_Loc     =       0
  Max_Loc     =       1
  Step_Loc    =       10
  NPoints_Loc =       10
  if ( present(Min)  ) Min_Loc = Min
  if ( present(Max)  ) Max_Loc = Max
  if ( present(Step)    ) then
    Step_Loc    =       Step
    NPoints_Loc =       floor(real((Max_Loc-Min_Loc)/Step_Loc),kind=kind(NPoints_Loc)) + 1
  end if
  if ( present(NPoints) ) then
    NPoints_Loc =       NPoints
    Step_Loc    =       Zero
    if ( NPoints_Loc /= 1 ) Step_Loc = int( Max_Loc - Min_Loc , kind=(kind(Vector)) ) / (NPoints_Loc-1)
  end if
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  allocate( Vector(NPoints_Loc), source = [ ( int(Min_Loc+(i-1)*Step_Loc,kind(Min_Loc)) , i=1,NPoints_Loc ) ] )
#else
  allocate( Vector, source = [ ( int(Min_Loc+(i-1)*Step_Loc,kind(Min_Loc)) , i=1,NPoints_Loc ) ] )
#endif