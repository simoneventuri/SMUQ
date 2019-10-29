Module KindParameters_Module

  use, intrinsic :: iso_fortran_env ,only: REAL32, REAL64, REAL128

  implicit none

  private
  public  ::  rkp, ikp, ckp
  public  ::  srkp, drkp, qrkp

  integer       ,parameter      ,private        ::  Ndig    =     15                                      ! Number of digits for kind
  integer       ,parameter      ,private        ::  Ndec    =     307                                     ! Number of representable decades

  integer       ,parameter                      ::  srkp    =     REAL32
  integer       ,parameter                      ::  drkp    =     REAL64
  integer       ,parameter                      ::  qrkp    =     REAL128
  integer       ,parameter                      ::  rkp     =     selected_real_kind(Ndig,Ndec)           ! Real kind precision
  integer       ,parameter                      ::  ikp     =     selected_int_kind(Ndig)                 ! Integer kind precision
  integer       ,parameter                      ::  ckp     =     selected_char_kind('default')           ! Charactr kind precision

End Module