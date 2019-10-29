Module Timing_Utilities

  use Logger_Class                ,only:  Logger

  implicit none

  private
  public  ::  DoSomething

  contains

Subroutine DoSomething( Percentage, TotalIterations, i_Debug )
  use String_Library      ,only:  Convert_Ratio
  use iso_fortran_env
  integer                                               ,intent(in)     ::    Percentage
  integer(int64)                                        ,intent(in)     ::    TotalIterations
  logical                                     ,optional ,intent(in)     ::    i_Debug                         !< Debugging indicator
  logical                                                               ::    i_Debug_Loc                     ! Local debugging indicator
  character(*)                                              ,parameter  ::    ProcName = 'DoSomething'
  integer(int64)                                                        ::    NumberIterations
  integer(int64)                                                        ::    i
  real(8)                                                               ::    Output
  i_Debug_Loc = .False.; if (present(i_Debug)) i_Debug_Loc = i_Debug                                            ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )
  NumberIterations = int(TotalIterations*Percentage,kind=int64) / 100
!   if (i_Debug_Loc) call Logger%Write( "TotalIterations = ", TotalIterations )
!   if (i_Debug_Loc) call Logger%Write( "Percentage = ", Percentage )
!   if (i_Debug_Loc) call Logger%Write( "int(TotalIterations*Percentage,kind=int64) = ", int(TotalIterations*Percentage,kind=int64) )
  if (i_Debug_Loc) call Logger%Write( "Percentage = ", Percentage, "NumberIterations = ", NumberIterations )!, "Iteration: ", Convert_Ratio( NumberIterations, TotalIterations ) )
  do i = 1,NumberIterations
    call Computation( int(i), Output )
  end do
  if (i_Debug_Loc) call Logger%Exiting()
End Subroutine

Subroutine Computation( i, Output )
  integer                                               ,intent(in)     ::    i
  real(8)                                               ,intent(out)    ::    Output
  real(8)                                                               ::    a, b, c
  real(8) ,dimension(:) ,allocatable                                    ::    T
  integer                                                               ::    N, k
  N   =   100
  allocate( T(N) )
  T = 0.0
  do k = 1,N
    a       =   123.456 * i
    b       =   0.98765 * i
    c       =   3.45678 / k
    T(k)    =   real(i,kind=8)
    if (k/=1) T(k) = T(k-1)
    Output  =   Output + ( a * T(k) * T(k)**b * exp(-c/T(k)) )
    Output  =   Output + T(k)**1.0 - T(k)**2.0 + T(k)**3.0 - T(k)**4.0
  end do
End Subroutine

End Module