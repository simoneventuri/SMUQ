Module GPF_Variables_Module

  use GPF_Parameters            ,only:  rkp

  implicit none

  public

  integer                                                               ::  NLines                          ! Number of lines per graph
  integer                                                               ::  NPoint                          ! Number of points per line
  character(100)        ,dimension(:)   ,allocatable                    ::  LS_Color                        ! Line's color (DIM=NLines)
  character(100)        ,dimension(:)   ,allocatable                    ::  LS_Type                         ! Line's type (DIM=NLines)
  character(100)        ,dimension(:)   ,allocatable                    ::  LS_Width                        ! Line's width (DIM=NLines)
  character(100)        ,dimension(:)   ,allocatable                    ::  LS_PointType
  character(100)        ,dimension(:)   ,allocatable                    ::  LS_PointSize
  character(100)        ,dimension(:)   ,allocatable                    ::  LS_PointInterval
  character(100)        ,dimension(:)   ,allocatable                    ::  LineTitle                       ! Line's title (DIM=NLines)
  character(100)        ,dimension(:)   ,allocatable                    ::  CurveStyle                      ! Curve's style (DIM=NLines)
  real(rkp)             ,dimension(:)   ,allocatable                    ::  CB_Values                       ! ColorBox values (DIM=NLines)
  real(rkp)                                                             ::  CB_Min                          ! ColorBox minimum value
  real(rkp)                                                             ::  CB_Max                          ! ColorBox maximum value
  character(:)  ,allocatable                                            ::  FileName                        ! Name of the image file including extension
  character(:)  ,allocatable                                            ::  FileName_Prefix
  character(:)  ,allocatable                                            ::  Title                           ! Graph title
  real(rkp)     ,dimension(:)   ,allocatable                            ::  X_1D
  real(rkp)     ,dimension(:)   ,allocatable                            ::  Y_1D
  real(rkp)     ,dimension(:,:) ,allocatable                            ::  X_2D, X_2Dp
  real(rkp)     ,dimension(:,:) ,allocatable                            ::  Y_2D
  real(rkp)     ,dimension(:,:) ,allocatable                            ::  Z_2D
  character(:)  ,allocatable                                            ::  Directory
  character(:)  ,allocatable                                            ::  X_Label,    Y_Label,    Z_Label
  character(:)  ,allocatable                                            ::  X_Format,   Y_Format,   Z_Format
  logical                                                               ::  X_LogScale, Y_LogScale, Z_LogScale
  integer       ,dimension(:)   ,allocatable                            ::  NPoints                         ! Number of points per line

End Module