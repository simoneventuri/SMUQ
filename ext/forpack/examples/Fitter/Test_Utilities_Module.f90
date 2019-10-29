Module Test_Utilities_Module

  use Parameters_Library          ,only:  rkp

  implicit none

  private
  public  ::  Plot
  public  ::  AddNoise

  contains

Subroutine Plot( Name, Curve, Y_Tab, Ymin, Ymax )

  use GPF_Module          ,only:  GPF_Figure_Type
  use Fitter_Class        ,only:  Fitter_Type

  character(*)                                          ,intent(in)     ::  Name
  type(Fitter_Type)                                     ,intent(in)     ::  Curve
  real(rkp)     ,allocatable  ,dimension(:)   ,optional ,intent(in)     ::  Y_Tab
  real(rkp)                                   ,optional ,intent(in)     ::  Ymin
  real(rkp)                                   ,optional ,intent(in)     ::  Ymax

  character(:)  ,allocatable                                            ::  FileName
  real(rkp)     ,allocatable  ,dimension(:,:)                           ::  Y_2D
  type(GPF_Figure_Type)                                                 ::  Figure

  FileName    =     Name // ".pdf"

  if ( present(Y_Tab) ) then
    allocate( Y_2D(3,Curve%NPtsTab) )
    Y_2D(1,:)   =   Curve%Y_Tab
    Y_2D(2,:)   =   Y_Tab
    Y_2D(3,:)   =   Curve%Evaluate(Curve%X_Tab)
    call Figure%Plot( Debug = .False.,                                                          &               ! Setting the debugging indicator
                Name            =       FileName,                                               &               ! Setting the file name
                Title           =       Name,                                                   &               ! Setting the graph title
                CurveStyle      =       ['p','p','l'],                                          &               ! Setting the linetype
                LS_PointSize    =       ['0.5','0.5','0.5'],                                    &               ! Setting the pointsize
                LineTitle       =       ['y(clean)','y(noise)','y(fit)'],                       &
                X_Label         =       'x',                                                    &               ! Setting the X-axis label
                Y_Label         =       'y',                                                    &               ! Setting the Y-axis label
                Y_Min           =       Ymin,                                                   &               ! Setting the min value for Y
                Y_Max           =       Ymax,                                                   &               ! Setting the max value for Y
                X_1D            =       Curve%X_Tab,                                            &               ! Setting the X data
                Y_2D            =       Y_2D                                                    )               ! Setting the Y data
  else
    allocate( Y_2D(2,Curve%NPtsTab) )
    Y_2D(1,:)   =   Curve%Y_Tab
    Y_2D(2,:)   =   Curve%Evaluate(Curve%X_Tab)
    call Figure%Plot( Debug = .False.,                                                          &               ! Setting the debugging indicator
                Name            =       FileName,                                               &               ! Setting the file name
                Title           =       Name,                                                   &               ! Setting the graph title
                CurveStyle      =       ['p','l'],                                              &               ! Setting the linetype
                LS_PointSize    =       ['0.5','0.5'],                                          &               ! Setting the pointsize
                X_Label         =       'x',                                                    &               ! Setting the X-axis label
                Y_Label         =       'y',                                                    &               ! Setting the Y-axis label
                Y_Min           =       Ymin,                                                   &               ! Setting the min value for Y
                Y_Max           =       Ymax,                                                   &               ! Setting the max value for Y
                X_1D            =       Curve%X_Tab,                                            &               ! Setting the X data
                Y_2D            =       Y_2D                                                    )               ! Setting the Y data
  end if

End Subroutine

Function AddNoise( OriginalData, Scaling ) result(NoisyData)
  real(rkp) ,dimension(:)                               ,intent(in)     ::  OriginalData
  real(rkp)                                   ,optional ,intent(in)     ::  Scaling
  real(rkp) ,dimension(size(OriginalData))                              ::  NoisyData
  real(rkp) ,parameter                                                  ::  DefaultScaling  =   1.0_rkp
  real(rkp)                                                             ::  LocalScaling
  real(rkp) ,dimension(size(OriginalData))                              ::  Noise
  LocalScaling  =   DefaultScaling
  if ( present(Scaling) ) LocalScaling  =   Scaling
  LocalScaling  = LocalScaling * maxval(OriginalData)
  call Random_Number( Noise )
  Noise         =     Noise - 0.5_rkp         ! Centering
  Noise         =     LocalScaling * Noise    ! Scaling
  NoisyData     =     OriginalData + Noise
End Function

End Module