SubModule(FitModelTest_Module) NASA9FitModelTest_SubModule

  implicit none

  contains

Module Procedure Test_NASA9

  use Fitter_Library              ,only:  SpecificFitModel_Type => NASA9FitModel_Type
  use Arithmetic_Library          ,only:  LinSpace

  character(*)                                              ,parameter  ::  ProcName = 'Test_NASA9'
  character(*)                                              ,parameter  ::  TestName = 'NASA9'
  integer                                                               ::  i
  real(rkp) ,dimension(:)   ,allocatable                                ::  X_Tab, Y_Tab, Y_Fit
  real(rkp) ,dimension(:)   ,allocatable                                ::  Intervals
  type(Fitter_Type)                                                     ::  Curve
  class(FitModel_Type)  ,allocatable                                    ::  Model
  real(rkp) ,dimension(:,:) ,allocatable                                ::  Param

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_INFO, MsgLogLevel=LogLevel_INFO )

! ==============================================================================================================
!    SETTING THE INPUT DATA
! ==============================================================================================================
  call Logger%Write( "Setting the input data" )
  allocate( Intervals, source = [  200.0_rkp,  1000.0_rkp,  6000.0_rkp, 20000.0_rkp ] )
  allocate( Param(3,7) )
! N
  Param(1,:)  =   [0.000000000E+00_rkp, 0.000000000E+00_rkp, 2.500000000E+00_rkp, 0.000000000E+00_rkp, 0.000000000E+00_rkp, 0.000000000E+00_rkp, 0.000000000E+00_rkp] !   N
  Param(2,:)  =   [8.876501380E+04_rkp,-1.071231500E+02_rkp, 2.362188287E+00_rkp, 2.916720081E-04_rkp,-1.729515100E-07_rkp, 4.012657880E-11_rkp,-2.677227571E-15_rkp] !   N
  Param(3,:)  =   [5.475181050E+08_rkp,-3.107574980E+05_rkp, 6.916782740E+01_rkp,-6.847988130E-03_rkp, 3.827572400E-07_rkp,-1.098367709E-11_rkp, 1.277986024E-16_rkp] !   N
! ! N2
!   Param(1,:)  =   [2.210371497E+04_rkp,-3.818461820E+02_rkp, 6.082738360E+00_rkp,-8.530914410E-03_rkp, 1.384646189E-05_rkp,-9.625793620E-09_rkp, 2.519705809E-12_rkp] !   N2
!   Param(2,:)  =   [5.877124060E+05_rkp,-2.239249073E+03_rkp, 6.066949220E+00_rkp,-6.139685500E-04_rkp, 1.491806679E-07_rkp,-1.923105485E-11_rkp, 1.061954386E-15_rkp] !   N2
!   Param(3,:)  =   [8.310139160E+08_rkp,-6.420733540E+05_rkp, 2.020264635E+02_rkp,-3.065092046E-02_rkp, 2.486903333E-06_rkp,-9.705954110E-11_rkp, 1.437538881E-15_rkp] !   N2
  allocate( SpecificFitModel_Type :: Model )
  call Model%Initialize()
  allocate( X_Tab , source = LinSpace( minval(Intervals), maxval(Intervals), 50 ) )
  allocate( Y_Tab( size(X_Tab) ) )
! ==============================================================================================================

  call Logger%Write( "Calling Curve%Initialize" )
  call Curve%Initialize( X_Tab=X_Tab, Intervals=Intervals, FitModel=Model )

  call Logger%Write( "Setting the model parameters for each temperature interval" )
  do i = 1,size(Param,1)      ! Loop on all temperature intervals
    call Logger%Write( "-> Calling Curve%SetModelParam for i = ", i )
    call Curve%SetModelParam( Param(i,:), Interval=i, i_Debug=.False. )
  end do


  call Logger%Write( "Calling Curve%Evaluate" )
  Y_Tab     =     Curve%Evaluate( X_Tab )
  Y_Tab     =     AddNoise( Y_Tab, Scaling=0.0_rkp )

  call Logger%Write( "Calling Curve%Initialize" )
  call Curve%SetYTab( Y_Tab=Y_Tab )


  call Logger%Write( "Calling Curve%Fit" )
  call Curve%Fit( i_Debug=.False. )


  if ( Logger%On() ) then
    call Logger%Write( "Curve%NIntervals = ", Curve%NIntervals )
    do i = 1,Curve%NIntervals
      call Logger%Write( "i = ", i, "Param = ", Curve%Intervals(i)%FitModel%Param, F2="i3", F4="es15.8" )
    end do
  end if

  allocate( Y_Fit , source= Curve%Evaluate( X_Tab ) )

  call Logger%Write( "Calling Plot" )
  call Plot( TestName, Curve )

  call Logger%Exiting()

End Procedure

End SubModule