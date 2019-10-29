SubModule(String_Module) String_Format_SubModule

  implicit none

  contains

! This procedure returns the length associated to a fortran format specification string.
! For example:
!   Format            Length
!   ------            ------
!   "(es15.8)"        15
!   "es15.8"          15
!   "(3(es15.8))"     45 ( = 3 * 15 )
Module Procedure GetLengthFromFormat
  Length  =   0
End Procedure


! This procedure returns a real format from the total length.
! Format = es<X>.<X-8>E3
! @TODO: Check that L-8 does not become negative
! Examples:
!   Length              =>              Format
!   15                                  es15.8E3
Module Procedure Real_Format_From_Length
  character(:)  ,allocatable                                            ::  L, Lm8
  L       =   Convert_To_String(Length)
  Lm8     =   Convert_To_String(Length - 8)
  Format  =   "es" // L // "." // Lm8 // "E3"
End Procedure

! Only works for integers for now
! The optional input variable "Full" is an indicator for adding the opening and cloing
! parenthesis to have a full format.
! For example
! * Without:    GetFormat(5)  =>  "i5"
! * With:       GetFormat(5)  =>  "(i5)"
Module Procedure GetFormat
  use Utilities_Library    ,only:  PresentAndTrue
  select type ( Variable )
    type is ( character(*)   ); Format = "a"  // Convert_To_String( len(Variable) )
    type is ( logical        ); Format = "g0"
    type is ( integer(INT8)  ); Format = "i"  // Convert_To_String( GetNumberOfDigits(Variable) )
    type is ( integer(INT16) ); Format = "i"  // Convert_To_String( GetNumberOfDigits(Variable) )
    type is ( integer(INT32) ); Format = "i"  // Convert_To_String( GetNumberOfDigits(Variable) )
    type is ( integer(INT64) ); Format = "i"  // Convert_To_String( GetNumberOfDigits(Variable) )
!     type is ( real(REAL32)   );
!     type is ( real(REAL64)   );
!     type is ( real(REAL128)  );
  end select
  if ( PresentAndTrue(Full) ) Format = "("//Format//")"
End Procedure

! Module Procedure GetFormat_INT8
!   Format = "i"//Convert_To_String(GetNumberOfDigits(Variable))
! End Procedure



End SubModule