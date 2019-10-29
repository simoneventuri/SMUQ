  integer                                                                       ::  i
  integer                                                                       ::  NPoints_Loc
  real(kind(Vector))                                                            ::  Min_Loc
  real(kind(Vector))                                                            ::  Max_Loc
  real(kind(Vector))                                                            ::  Step_Loc
  Min_Loc     =   Zero
  Max_Loc     =   One
  Step_Loc    =   Ten
  NPoints_Loc =   10
  if ( present(Min)  ) Min_Loc = Min
  if ( present(Max)  ) Max_Loc = Max
  if ( present(Step)    ) then
    Step_Loc    =       Step
    NPoints_Loc =       floor( (Max_Loc - Min_Loc) / Step_Loc ) + 1
  end if
  if ( present(NPoints) ) then
    NPoints_Loc =       NPoints
    Step_Loc    =       Zero
    if ( NPoints_Loc /= 1 ) Step_Loc = ( Max_Loc - Min_Loc ) / ( NPoints_Loc - 1 )
  end if
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  allocate( Vector(NPoints_Loc), source = [ ( Min_Loc + (i-1) * Step_Loc , i=1,NPoints_Loc ) ] )
#else
  allocate( Vector, source = [ ( Min_Loc + (i-1) * Step_Loc , i=1,NPoints_Loc ) ] )
#endif