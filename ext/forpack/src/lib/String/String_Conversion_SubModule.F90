SubModule(String_Module) String_Conversion_SubModule

! Include file needed for: CONCAT
# include "forpack-include.inc"

  implicit none

  character(*)                  ,parameter  ::  DefaultLogicalTrue  = "T"
  character(*)                  ,parameter  ::  DefaultLogicalFalse = "F"
  character(*)  ,dimension(6)   ,parameter  ::  ValidLogicalTrue  = [ character(7) :: &
                                                  'TRUE   ',  &
                                                  'ON     ',  &
                                                  'YES    ',  &
                                                  '1      ',  &
                                                  'T      ',  &
                                                  '.TRUE. '   ]
  character(*)  ,dimension(6)   ,parameter  ::  ValidLogicalFalse = [ character(7) :: &
                                                  'FALSE  ',  &
                                                  'OFF    ',  &
                                                  'NO     ',  &
                                                  '0      ',  &
                                                  'F      ',  &
                                                  '.FALSE.'   ]

  contains


! **************************************************************************************************************
!       PROCEDURES FOR CONVERTING VARIABLES-0D TO CHARACTER-0D
! **************************************************************************************************************

Module Procedure Convert_INT8_0d_To_CHAR_0d
# include "Convert_INT_0d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_INT16_0d_To_CHAR_0d
# include "Convert_INT_0d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_INT32_0d_To_CHAR_0d
# include "Convert_INT_0d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_INT64_0d_To_CHAR_0d
# include "Convert_INT_0d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_REAL32_0d_To_CHAR_0d
# include "Convert_REAL_0d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_REAL64_0d_To_CHAR_0d
# include "Convert_REAL_0d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_REAL128_0d_To_CHAR_0d
# include "Convert_REAL_0d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_CHAR_0d_To_CHAR_0d
  use Utilities_Library   ,only:  GetOptArgValue
  character(*)                                              ,parameter  ::  DefaultFormat = "g0"
  character(1000)                                                       ::  Str
  character(:)  ,allocatable                                            ::  Fmt_
  Fmt_      =   "("//GetOptArgValue(DefaultFormat,Fmt)//")"
  write(Str,Fmt_) Var
  String    = trim(Str)
  if ( present(Len) ) then
    String  =   SetLength( String, Len )
    if ( present(Pos) ) then
      if ( Pos == 'L') String(:) = adjustl(String)
      if ( Pos == 'R') String(:) = adjustr(String)
    end if
  end if
End Procedure


! **************************************************************************************************************
!       PROCEDURES FOR CONVERTING VARIABLES-1D TO CHARACTER-0D
! **************************************************************************************************************

Module Procedure Convert_LOG_1d_To_CHAR_0d
  use Utilities_Library   ,only:  GetOptArgValue
  character(*)                                              ,parameter  ::  DefaultSeparator = " "
  character(:)  ,allocatable                                            ::  Str, True, False, Sep
  integer                                                               ::  i
  Sep       =   GetOptArgValue(DefaultSeparator,Separator)
  String    =   ""
  do i = 1,size(Var)
    Str     =   Convert_To_String( Var(i), T, F )
    String  =   String // Sep // Str
  end do
End Procedure

Module Procedure Convert_INT8_1d_To_CHAR_0d
# include "Convert_INT_1d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_INT16_1d_To_CHAR_0d
# include "Convert_INT_1d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_INT32_1d_To_CHAR_0d
# include "Convert_INT_1d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_INT64_1d_To_CHAR_0d
# include "Convert_INT_1d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_REAL32_1d_To_CHAR_0d
# include "Convert_REAL_1d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_REAL64_1d_To_CHAR_0d
# include "Convert_REAL_1d_To_CHAR_0d-Inlined.F90"
End Procedure

Module Procedure Convert_REAL128_1d_To_CHAR_0d
# include "Convert_REAL_1d_To_CHAR_0d-Inlined.F90"
End Procedure


! **************************************************************************************************************
!       PROCEDURES FOR CONVERTING VARIABLES-1D TO CHARACTER-1D
! **************************************************************************************************************

Pure Module Function Convert_CHAR_1d_To_LOG_1d( Input ) result(Output)
  character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
  logical                                                               ::  Output(size(Input))               !< Converted output variable
  integer                                                               ::  i
  do i = 1,size(Input)
    Output(i)   =   Convert_CHAR_0d_To_LOG_0d( Input(i) )
  end do
End Function

Pure Module Function Convert_CHAR_1d_To_INT8_1d( Input ) result(Output)
  character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
  integer(INT8)                                                         ::  Output(size(Input))               !< Converted output variable
  integer                                                               ::  i
  do i = 1,size(Input)
    Output(i)   =   Convert_CHAR_0d_To_INT8_0d( Input(i) )
  end do
End Function

Pure Module Function Convert_CHAR_1d_To_INT16_1d( Input ) result(Output)
  character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
  integer(INT16)                                                        ::  Output(size(Input))               !< Converted output variable
  integer                                                               ::  i
  do i = 1,size(Input)
    Output(i)   =   Convert_CHAR_0d_To_INT16_0d( Input(i) )
  end do
End Function

Pure Module Function Convert_CHAR_1d_To_INT32_1d( Input ) result(Output)
  character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
  integer(INT32)                                                        ::  Output(size(Input))               !< Converted output variable
  integer                                                               ::  i
  do i = 1,size(Input)
    Output(i)   =   Convert_CHAR_0d_To_INT32_0d( Input(i) )
  end do
End Function

Pure Module Function Convert_CHAR_1d_To_INT64_1d( Input ) result(Output)
  character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
  integer(INT64)                                                        ::  Output(size(Input))               !< Converted output variable
  integer                                                               ::  i
  do i = 1,size(Input)
    Output(i)   =   Convert_CHAR_0d_To_INT64_0d( Input(i) )
  end do
End Function

Pure Module Function Convert_CHAR_1d_To_REAL32_1d( Input ) result(Output)
  character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
  real(REAL32)                                                          ::  Output(size(Input))               !< Converted output variable
  integer                                                               ::  i
  do i = 1,size(Input)
    Output(i)   =   Convert_CHAR_0d_To_REAL32_0d( Input(i) )
  end do
End Function

Pure Module Function Convert_CHAR_1d_To_REAL64_1d( Input ) result(Output)
  character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
  real(REAL64)                                                          ::  Output(size(Input))               !< Converted output variable
  integer                                                               ::  i
  do i = 1,size(Input)
    Output(i)   =   Convert_CHAR_0d_To_REAL64_0d( Input(i) )
  end do
End Function

Pure Module Function Convert_CHAR_1d_To_REAL128_1d( Input ) result(Output)
  character(*)                                          ,intent(in)     ::  Input(:)                          !< Input variable to be converted
  real(REAL128)                                                         ::  Output(size(Input))               !< Converted output variable
  integer                                                               ::  i
  do i = 1,size(Input)
    Output(i)   =   Convert_CHAR_0d_To_REAL128_0d( Input(i) )
  end do
End Function



    ! **************************************************************************************************************
    !       PROCEDURES FOR CONVERTING CHARACTERS TO OTHER TYPES: LOGICAL, INTEGER, REAL
    ! **************************************************************************************************************

! Module Procedure Convert_CHAR_0d_To_INT32_0d
!   use Utilities_Library   ,only:  GetOptArgValue
! !   integer                                                   ,parameter  ::  DefaultKind = INT32
! !   integer                                                               ::  Kind_
!   real(REAL64)                                                          ::  RealNumber
! !   Kind_       =   GetOptArgValue(DefaultKind,Kind)
!   RealNumber  =   Convert_To_Real( String )
! !   Number      =   nint( RealNumber, kind=Kind
!   Number      =   nint( RealNumber )
! End Procedure

! This procedure convert a variable from 'character' to 'logical'
! The output logical variable is set to true if the input character variable
! has any of the following values: TRUE, ON, YES, 1, T, .TRUE.
Module Procedure Convert_CHAR_0d_To_LOG_0d
  use Utilities_Library   ,only:  IsIncluded
  character(:)  ,allocatable                                            ::  Str
  Str     =   UpperCase(adjustl(trim(String)))
  Value   =   IsIncluded( Str , ValidLogicalTrue, CaseSensitive=.False. )
End Procedure


! **************************************************************************************************************
!       PROCEDURES FOR CONVERTING CHARACTERS TO INTEGERS
! **************************************************************************************************************

Module Procedure Convert_CHAR_0d_To_INT8_0d
  real(REAL64)                                                          ::  RealNumber
  read(Input,*) RealNumber
  Output  =   nint( RealNumber )
End Procedure

Module Procedure Convert_CHAR_0d_To_INT16_0d
  real(REAL64)                                                          ::  RealNumber
  read(Input,*) RealNumber
  Output  =   nint( RealNumber )
End Procedure

Module Procedure Convert_CHAR_0d_To_INT32_0d
  real(REAL64)                                                          ::  RealNumber
  read(Input,*) RealNumber
  Output  =   nint( RealNumber )
End Procedure

Module Procedure Convert_CHAR_0d_To_INT64_0d
  real(REAL64)                                                          ::  RealNumber
  read(Input,*) RealNumber
  Output  =   nint( RealNumber )
End Procedure


! **************************************************************************************************************
!       PROCEDURES FOR CONVERTING CHARACTERS TO REALS
! **************************************************************************************************************

Module Procedure Convert_CHAR_0d_To_REAL32_0d
  read(Input,*) Output
End Procedure

Module Procedure Convert_CHAR_0d_To_REAL64_0d
  read(Input,*) Output
End Procedure

Module Procedure Convert_CHAR_0d_To_REAL128_0d
  read(Input,*) Output
End Procedure










Module Procedure Convert_LOG_0d_To_INT8_0d
  if (Input) then;  Output = 1_INT8
  else;             Output = 0_INT8; end if
End Procedure

Module Procedure Convert_LOG_0d_To_INT16_0d
  if (Input) then;  Output = 1_INT16
  else;             Output = 0_INT16; end if
End Procedure

Module Procedure Convert_LOG_0d_To_INT32_0d
  if (Input) then;  Output = 1_INT32
  else;             Output = 0_INT32; end if
End Procedure

Module Procedure Convert_LOG_0d_To_INT64_0d
  if (Input) then;  Output = 1_INT64
  else;             Output = 0_INT64; end if
End Procedure

Module Procedure Convert_LOG_0d_To_REAL32_0d
  if (Input) then;  Output = 1.0_REAL32
  else;             Output = 0.0_REAL32; end if
End Procedure

Module Procedure Convert_LOG_0d_To_REAL64_0d
  if (Input) then;  Output = 1.0_REAL64
  else;             Output = 0.0_REAL64; end if
End Procedure

Module Procedure Convert_LOG_0d_To_REAL128_0d
  if (Input) then;  Output = 1.0_REAL128
  else;             Output = 0.0_REAL128; end if
End Procedure

Module Procedure Convert_LOG_0d_To_CHAR_0d
  use Utilities_Library   ,only:  GetOptArgValue
  character(:)  ,allocatable                                            ::  True, False
  True    =   GetOptArgValue(DefaultLogicalTrue ,T)
  False   =   GetOptArgValue(DefaultLogicalFalse,F)
  if (Input) then;  Output = True
  else;             Output = False; end if
End Procedure






Module Procedure ConvertVariableKind_0d

  select type (From)

    type is ( character(*) )
      select type (To)
        type is ( character(*)   ); To = From
        type is ( logical        ); To = Convert_CHAR_0d_To_LOG_0d(From)
        type is ( integer(INT8)  ); To = Convert_CHAR_0d_To_INT8_0d(From)
        type is ( integer(INT16) ); To = Convert_CHAR_0d_To_INT16_0d(From)
        type is ( integer(INT32) ); To = Convert_CHAR_0d_To_INT32_0d(From)
        type is ( integer(INT64) ); To = Convert_CHAR_0d_To_INT64_0d(From)
        type is ( real(REAL32)   ); To = Convert_CHAR_0d_To_REAL32_0d(From)
        type is ( real(REAL64)   ); To = Convert_CHAR_0d_To_REAL64_0d(From)
        type is ( real(REAL128)  ); To = Convert_CHAR_0d_To_REAL128_0d(From)
      end select

    type is ( logical        )
      select type (To)
        type is ( character(*)   ); To = Convert_LOG_0d_To_CHAR_0d(From)
        type is ( logical        ); To = From
        type is ( integer(INT8)  ); To = Convert_LOG_0d_To_INT8_0d(From)
        type is ( integer(INT16) ); To = Convert_LOG_0d_To_INT16_0d(From)
        type is ( integer(INT32) ); To = Convert_LOG_0d_To_INT32_0d(From)
        type is ( integer(INT64) ); To = Convert_LOG_0d_To_INT64_0d(From)
        type is ( real(REAL32)   ); To = Convert_LOG_0d_To_REAL32_0d(From)
        type is ( real(REAL64)   ); To = Convert_LOG_0d_To_REAL64_0d(From)
        type is ( real(REAL128)  ); To = Convert_LOG_0d_To_REAL128_0d(From)
      end select

    type is ( integer(INT8)  )
      select type (To)
        type is ( character(*)   );
        type is ( logical        );
        type is ( integer(INT8)  ); To = From
        type is ( integer(INT16) );
        type is ( integer(INT32) );
        type is ( integer(INT64) );
        type is ( real(REAL32)   );
        type is ( real(REAL64)   );
        type is ( real(REAL128)  );
      end select

    type is ( integer(INT16) )
      select type (To)
        type is ( character(*)   );
        type is ( logical        );
        type is ( integer(INT8)  );
        type is ( integer(INT16) ); To = From
        type is ( integer(INT32) );
        type is ( integer(INT64) );
        type is ( real(REAL32)   );
        type is ( real(REAL64)   );
        type is ( real(REAL128)  );
      end select

    type is ( integer(INT32) )
      select type (To)
        type is ( character(*)   );
        type is ( logical        );
        type is ( integer(INT8)  );
        type is ( integer(INT16) );
        type is ( integer(INT32) ); To = From
        type is ( integer(INT64) );
        type is ( real(REAL32)   );
        type is ( real(REAL64)   );
        type is ( real(REAL128)  );
      end select

    type is ( integer(INT64) )
      select type (To)
        type is ( character(*)   );
        type is ( logical        );
        type is ( integer(INT8)  );
        type is ( integer(INT16) );
        type is ( integer(INT32) );
        type is ( integer(INT64) ); To = From
        type is ( real(REAL32)   );
        type is ( real(REAL64)   );
        type is ( real(REAL128)  );
      end select

    type is ( real(REAL32)   )
      select type (To)
        type is ( character(*)   );
        type is ( logical        );
        type is ( integer(INT8)  );
        type is ( integer(INT16) );
        type is ( integer(INT32) );
        type is ( integer(INT64) );
        type is ( real(REAL32)   ); To = From
        type is ( real(REAL64)   );
        type is ( real(REAL128)  );
      end select

    type is ( real(REAL64)   )
      select type (To)
        type is ( character(*)   );
        type is ( logical        );
        type is ( integer(INT8)  );
        type is ( integer(INT16) );
        type is ( integer(INT32) );
        type is ( integer(INT64) );
        type is ( real(REAL32)   );
        type is ( real(REAL64)   ); To = From
        type is ( real(REAL128)  );
      end select

    type is ( real(REAL128)  )
      select type (To)
        type is ( character(*)   );
        type is ( logical        );
        type is ( integer(INT8)  );
        type is ( integer(INT16) );
        type is ( integer(INT32) );
        type is ( integer(INT64) );
        type is ( real(REAL32)   );
        type is ( real(REAL64)   );
        type is ( real(REAL128)  ); To = From
      end select

  end select

End Procedure



Module Procedure Convert_From_CHAR_0d_To_LOG_0d
  use Utilities_Library   ,only:  SetOptArg
  integer                                                               ::  Status_
  Status_ =   0
  To      =   Convert_CHAR_0d_To_LOG_0d(From)
  call SetOptArg( Status_, Status )
End Procedure


Module Procedure Convert_From_CHAR_0d_To_INT8_0d
  use Utilities_Library   ,only:  SetOptArg
  integer                                                               ::  Status_
  real(REAL64)                                                          ::  Number
  read(From,*,iostat=Status_) Number
  To = nint(Number)
  call SetOptArg( Status_, Status )
End Procedure

Module Procedure Convert_From_CHAR_0d_To_INT16_0d
  use Utilities_Library   ,only:  SetOptArg
  integer                                                               ::  Status_
  real(REAL64)                                                          ::  Number
  read(From,*,iostat=Status_) Number
  To = nint(Number)
  call SetOptArg( Status_, Status )
End Procedure

Module Procedure Convert_From_CHAR_0d_To_INT32_0d
  use Utilities_Library   ,only:  SetOptArg
  integer                                                               ::  Status_
  real(REAL64)                                                          ::  Number
  read(From,*,iostat=Status_) Number
  To = nint(Number)
  call SetOptArg( Status_, Status )
End Procedure

Module Procedure Convert_From_CHAR_0d_To_INT64_0d
  use Utilities_Library   ,only:  SetOptArg
  integer                                                               ::  Status_
  real(REAL64)                                                          ::  Number
  read(From,*,iostat=Status_) Number
  To = nint(Number)
  call SetOptArg( Status_, Status )
End Procedure

Module Procedure Convert_From_CHAR_0d_To_REAL32_0d
  use Utilities_Library   ,only:  SetOptArg
  integer                                                               ::  Status_
  read(From,*,iostat=Status_) To
  call SetOptArg( Status_, Status )
End Procedure

Module Procedure Convert_From_CHAR_0d_To_REAL64_0d
  use Utilities_Library   ,only:  SetOptArg
  integer                                                               ::  Status_
  read(From,*,iostat=Status_) To
  call SetOptArg( Status_, Status )
End Procedure

Module Procedure Convert_From_CHAR_0d_To_REAL128_0d
  use Utilities_Library   ,only:  SetOptArg
  integer                                                               ::  Status_
  read(From,*,iostat=Status_) To
  call SetOptArg( Status_, Status )
End Procedure


Module Procedure ConvertFromString
  select type (Value)
    type is ( logical        ); Value = Convert_To_Logical(String)
    type is ( integer(INT8)  ); Value = Convert_To_Integer(String)
    type is ( integer(INT16) ); Value = Convert_To_Integer(String)
    type is ( integer(INT32) ); Value = Convert_To_Integer(String)
    type is ( integer(INT64) ); Value = Convert_To_Integer(String)
    type is ( real(REAL32)   ); Value = Convert_To_Real(String)
    type is ( real(REAL64)   ); Value = Convert_To_Real(String)
    type is ( real(REAL128)  ); Value = Convert_To_Real(String)
  end select
End Procedure


End SubModule