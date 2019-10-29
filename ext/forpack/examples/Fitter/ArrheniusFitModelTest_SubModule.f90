SubModule(FitModelTest_Module) ArrheniusFitModelTest_SubModule

  implicit none

  contains

Module Procedure Test_Arrhenius

  use Fitter_Library              ,only:  Fitter_Type, FitModel_Type, SpecificFitModel_Type => ArrheniusFitModel_Type

  character(*)                                              ,parameter  ::  ProcName = 'Test_Arrhenius'
  character(*)                                              ,parameter  ::  TestName = 'Arrhenius'
  integer                                                               ::  i
  real(rkp) ,dimension(:)   ,allocatable                                ::  X_Tab, Y_Tab, Y_Fit
  real(rkp) ,dimension(:)   ,allocatable                                ::  Intervals
  type(Fitter_Type)                                                     ::  Curve
  class(FitModel_Type)  ,allocatable                                    ::  Model
  real(rkp) ,dimension(3) ,parameter                                    ::  Param = [3.0E+16_rkp, -1.60E+00_rkp,113200.0_rkp] ! N2 + M <-> N + N + M = Kf = Arrhenius( 3.00E+16, -1.60E+00, 0.11320E+06); M = (N,O,N+,O+)

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_INFO, MsgLogLevel=LogLevel_INFO )

  call Logger%Write( "Setting the input data" )
  allocate( SpecificFitModel_Type :: Model )
  call Model%Initialize()
  allocate( X_Tab , source =  [ (i * 1000.0_rkp , i=1,100 )] )
  allocate( Y_Tab , source = Model%Eval( X_Tab, Param=Param ) )


  call Logger%Write( "Calling Curve%Initialize" )
  allocate( Intervals , source = [ 1000.0_rkp , 4000.0_rkp , 6000.0_rkp, 10000.0_rkp ] )
  call Curve%Initialize( X_Tab=X_Tab, Y_Tab=Y_Tab, Intervals=Intervals, FitModel=Model )

  call Logger%Write( "Calling Curve%Fit" )
  call Curve%Fit()

  allocate( Y_Fit , source= Curve%Evaluate(X_Tab) )

  call Logger%Write( "Calling Plot" )
  call Plot( TestName, Curve )

!   if ( Logger%On(LogLevel_DEBUG) ) then
!     do i = 1,size(X_Tab)
!       write(Logger%Unit,"(*(es15.8,3x))") X_Tab(i), Y_Tab(i), Y_Fit(i)
!     end do
!   end if

  call Logger%Exiting()

End Procedure

End SubModule