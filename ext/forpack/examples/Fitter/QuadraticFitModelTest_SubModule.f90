SubModule(FitModelTest_Module) QuadraticFitModelTest_SubModule

  implicit none

  contains

Module Procedure Test_Quadratic

  use Fitter_Library              ,only:  SpecificFitModel_Type => QuadraticFitModel_Type

  character(*)                                              ,parameter  ::  ProcName = 'Test_Quadratic'
  character(*)                                              ,parameter  ::  TestName = 'Quadratic'
  integer                                                               ::  i
  real(rkp) ,dimension(:)   ,allocatable                                ::  X_Tab, Y_Tab, Y_TabPer
  type(Fitter_Type)                                                     ::  Curve
  class(FitModel_Type)  ,allocatable                                    ::  Model
  real(rkp) ,dimension(3) ,parameter                                    ::  Param = [1.0_rkp,2.0_rkp,3.0_rkp]

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_INFO, MsgLogLevel=LogLevel_INFO )

  call Logger%Write( "Setting the input data" )
  allocate( SpecificFitModel_Type :: Model )
  call Model%Initialize()
  allocate( X_Tab , source = [ (i * 1.0_rkp , i=1,50)] )
  allocate( Y_Tab , source = Model%Eval( X_Tab, Param=Param ) )
  allocate( Y_TabPer, source = AddNoise( Y_Tab, Scaling=1.0E-1_rkp ) )
  Y_TabPer = Y_Tab

  call Logger%Write( "Calling Curve%Initialize" )
  call Curve%Initialize( X_Tab=X_Tab, Y_Tab=Y_TabPer, FitModel=Model, i_Debug=.False. )

  call Logger%Write( "Calling Curve%Fit" )
  call Curve%Fit( i_Debug=.False. )

  call Logger%Write( "Calling Plot" )
  call Plot( TestName, Curve, Y_Tab=Y_Tab )

  call Logger%Exiting()

End Procedure

End SubModule