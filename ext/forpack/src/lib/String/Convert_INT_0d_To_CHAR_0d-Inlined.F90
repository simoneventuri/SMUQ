! This procedure converts a integer variable into a character string. Using
! character length allocation, the output character has a length which exactly
! matches the number of digits of the input integer. The ouput string can be
! further customized using the following input optional arguments:
! * LeadingZeros: This argument forces the number of leading zeros. Default is no leading zeros
! * Fmt: This argument set the format for converting the integer into a string. Default is "i0"
! * Len: This argument set the length of the output string. Default is actual length
! * Pos: This argument set the position of the string
! Examples:
!       String = Convert( 50, LeadingZeros=4 )    =>      "0050"
!       String = Convert( 50 )                    =>      "50"
! @TODO: Check validity of input format
! ======================================================================
! Module Procedure Convert_From_INT<ikp>_0d_To_CHAR_0D
  character(*)                                              ,parameter  ::  DefaultFormat = "i0"
  character(1000)                                                       ::  Str     ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Fmt_     ! Local value of the number format
  integer                                                               ::  ios
  if ( present(Fmt) ) then
    Fmt_  =   Fmt
  else
    Fmt_  =   DefaultFormat
    if ( present(LeadingZeros) ) Fmt_ = Fmt_ // "." // Convert_To_String(LeadingZeros)
  end if
  Fmt_    =   "("//Fmt_//")"
  write(Str,Fmt_,iostat=ios) Var
  if ( ios /= 0 ) write(Str,"("//DefaultFormat//")",iostat=ios) Var
  String  =   trim(Str)
  if ( .Not. present(Fmt) ) String = trim(adjustl(Str))
  if ( present(Len) ) String = SetLength( String, Len )
  if ( present(Pos) ) then
    if ( Pos == 'L') String(:) = adjustl(String)
    if ( Pos == 'R') String(:) = adjustr(String)
  end if
! End Procedure
! ======================================================================