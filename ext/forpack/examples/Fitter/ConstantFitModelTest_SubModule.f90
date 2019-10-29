SubModule(FitModelTest_Module) ConstantFitModelTest_SubModule

  implicit none

  contains

Module Procedure Test_Constant

  use Fitter_Library              ,only:  SpecificFitModel_Type => ConstantFitModel_Type

  character(*)                                              ,parameter  ::  ProcName = 'Test_Constant'
  character(*)                                              ,parameter  ::  TestName = 'Constant'
  integer                                                               ::  i
  real(rkp) ,dimension(:)   ,allocatable                                ::  X_Tab, Y_Tab, Y_Fit
  type(Fitter_Type)                                                     ::  Curve
  class(FitModel_Type)  ,allocatable                                    ::  Model
  real(rkp) ,dimension(1) ,parameter                                    ::  Param = [1.0_rkp]

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_INFO, MsgLogLevel=LogLevel_INFO )

  call Logger%Write( "Setting the input data" )
  allocate( SpecificFitModel_Type :: Model )
  call Model%Initialize()
  allocate( X_Tab , source = [ (i * 1.0_rkp , i=1,50 )] )
  allocate( Y_Tab , source = Model%Eval( X_Tab, Param=Param ) )
  Y_Tab   =     AddNoise( Y_Tab, Scaling=1.0E-2_rkp )

  call Logger%Write( "Calling Curve%Initialize" )
  call Curve%Initialize( X_Tab=X_Tab, Y_Tab=Y_Tab, FitModel=Model )

  call Logger%Write( "Calling Curve%Fit" )
  call Curve%Fit()

  call Logger%Write( "Calling Plot" )
  call Plot( TestName, Curve, Ymin=0.9_rkp, Ymax=1.1_rkp )

  call Logger%Exiting()

End Procedure

End SubModule