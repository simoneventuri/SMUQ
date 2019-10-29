SubModule(InputParamProperties_Class) InputParamProperties_SubClass

  use Logger_Class      ,only:  Logger
  use Utilities_Library  ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! This procedure returns a InputParamProperties object from a set of optional
! input arguments. Eahc component of the object is set according to the
! presence/value of an optional input argument. There is also a 'Properties'
! optional input argument which can be used to clone the InputParamProperties
! object to an input value. If this procedure is called with only this
! 'Properties' argument, the output object is then simply a copy of this object
! If, in addition to this 'Properties' argument, other optional input arguments
! are specified, then the input 'Properties' object is first copied into the
! output object, and then the component of the output object are modified
! according to the values of the other optional input arguments. This enables
! to copy an entire InputParamProperties object whicle modifying some of its
! component
!@TODO: COMPILER_BUG:ifort-18.0.0: On output, This%HasBounds=False when it should be True
!       A workaround is to use the 'IsValueBounded' function for now
Module Procedure ConstructorInputParamProperties
  use String_Library      ,only:  VecTrim
  if ( present(Properties)        ) This                  =   Properties
  if ( present(Name)              ) This%Name             =   Name
  if ( present(CaseSensitive)     ) This%CaseSensitive    =   CaseSensitive
  if ( present(Mandatory)         ) This%Mandatory        =   Mandatory
  if ( present(ValidValues)       ) This%HasValidValues   =   .True.
!   if ( present(DefaultValue)      ) then
!     allocate( This%DefaultValue, source = DefaultValue )
!     This%HasDefaultValue  =   .True.
!   end if
  if ( present(DefaultValue)      ) then
    This%DefaultValue     =   DefaultValue
    This%HasDefaultValue  =   .True.
  end if
  if ( present(DataType)          ) This%DataType         =   DataType

  if ( present(ValidValues)       ) This%ValidValues      =   VecTrim(ValidValues)
  if ( present(VariableKind)      ) This%VariableKind     =   VariableKind
                                    This%CheckValidValues =   This%HasDefaultValue
  if ( present(CheckValidValues)  ) This%CheckValidValues =   CheckValidValues


!   if ( present(LowerThan       )  ) allocate( This%LowerThan        , source = LowerThan        )
!   if ( present(LowerEqualThan  )  ) allocate( This%LowerEqualThan   , source = LowerEqualThan   )
!   if ( present(GreaterThan     )  ) allocate( This%GreaterThan      , source = GreaterThan      )
!   if ( present(GreaterEqualThan)  ) allocate( This%GreaterEqualThan , source = GreaterEqualThan )

!   This%HasBounds  =     allocated(This%LowerThan)         &
!                   .or.  allocated(This%LowerEqualThan)    &
!                   .or.  allocated(This%GreaterThan)       &
!                   .or.  allocated(This%GreaterEqualThan)
! *** Workaround ***
  if ( present(LowerThan       )  ) This%Val_lt = SetBound( LowerThan        )
  if ( present(LowerEqualThan  )  ) This%Val_le = SetBound( LowerEqualThan   )
  if ( present(GreaterThan     )  ) This%Val_gt = SetBound( GreaterThan      )
  if ( present(GreaterEqualThan)  ) This%Val_ge = SetBound( GreaterEqualThan )
  if ( present(LowerThan       )  ) This%ilt = .True.
  if ( present(LowerEqualThan  )  ) This%ile = .True.
  if ( present(GreaterThan     )  ) This%igt = .True.
  if ( present(GreaterEqualThan)  ) This%ige = .True.
  This%HasBounds  =   This%ilt .or. This%ile .or. This%igt .or. This%ige
! *** Workaround ***

  contains
Function SetBound( pValue ) result(rValue)
  use iso_fortran_env   ,only:  INT32, INT64, REAL32, REAL64, REAL128
  class(*)    ,intent(in)     ::  pValue
  real(8)                     ::  rValue
  select type (pValue)
    type is ( integer(INT32) ); rValue = pValue
    type is ( integer(INT64) ); rValue = pValue
    type is ( real(REAL32)   ); rValue = pValue
    type is ( real(REAL64)   ); rValue = pValue
    type is ( real(REAL128)  ); rValue = pValue
  end select
End Function

End Procedure

Module Procedure SetInputParamProperties
  This    =                           &
    ConstructorInputParamProperties(  &
      Properties                    , &
      Name                          , &
      CaseSensitive                 , &
      Mandatory                     , &
      DefaultValue                  , &
      ValidValues                   , &
      DataType                      , &
      VariableKind                  , &
      CheckValidValues              , &
      LowerThan                     , &
      LowerEqualThan                , &
      GreaterThan                   , &
      GreaterEqualThan                &
    )
End Procedure

Module Procedure FinalizeInputParamProperties
  This%CaseSensitive      =   .False.
  This%Mandatory          =   .False.
  This%HasDefaultValue    =   .False.
  This%HasValidValues     =   .False.
  This%CheckValidValues   =   .False.
  This%HasBounds          =   .False.
  This%ilt                =   .False.
  This%ile                =   .False.
  This%igt                =   .False.
  This%ige                =   .False.
  This%Val_lt             =   0
  This%Val_le             =   0
  This%Val_gt             =   0
  This%Val_ge             =   0
  if ( allocated( This%Name             ) ) deallocate( This%Name             )
  if ( allocated( This%DefaultValue     ) ) deallocate( This%DefaultValue     )
  if ( allocated( This%ValidValues      ) ) deallocate( This%ValidValues      )
  if ( allocated( This%DataType         ) ) deallocate( This%DataType         )
  if ( allocated( This%VariableKind     ) ) deallocate( This%VariableKind     )

!   if ( allocated( This%LowerThan        ) ) deallocate( This%LowerThan        )
!   if ( allocated( This%LowerEqualThan   ) ) deallocate( This%LowerEqualThan   )
!   if ( allocated( This%GreaterThan      ) ) deallocate( This%GreaterThan      )
!   if ( allocated( This%GreaterEqualThan ) ) deallocate( This%GreaterEqualThan )
End Procedure

Module Procedure FreeInputParamProperties
  call FinalizeInputParamProperties(This)
End Procedure

! This procedure assigns a InputParamProperties object to another one.
Module Procedure AssignInputParamProperties
# ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  integer                                                               ::  Length
# endif
!   ******************************************************************************************
!   call lhs%Free()     !@COMPILER_BUG:ifort-19.0.1
!   ******************************************************************************************
  lhs%CaseSensitive      =   .False.
  lhs%Mandatory          =   .False.
  lhs%HasDefaultValue    =   .False.
  lhs%HasValidValues     =   .False.
  lhs%CheckValidValues   =   .False.
  lhs%HasBounds          =   .False.
  lhs%ilt                =   .False.
  lhs%ile                =   .False.
  lhs%igt                =   .False.
  lhs%ige                =   .False.
  lhs%Val_lt             =   0
  lhs%Val_le             =   0
  lhs%Val_gt             =   0
  lhs%Val_ge             =   0
  if ( allocated( lhs%Name             ) ) deallocate( lhs%Name             )
  if ( allocated( lhs%DefaultValue     ) ) deallocate( lhs%DefaultValue     )
  if ( allocated( lhs%ValidValues      ) ) deallocate( lhs%ValidValues      )
  if ( allocated( lhs%DataType         ) ) deallocate( lhs%DataType         )
  if ( allocated( lhs%VariableKind     ) ) deallocate( lhs%VariableKind     )
!   ******************************************************************************************
  lhs%Name              =   rhs%Name
  lhs%CaseSensitive     =   rhs%CaseSensitive
  lhs%Mandatory         =   rhs%Mandatory
  lhs%HasDefaultValue   =   rhs%HasDefaultValue
  lhs%HasValidValues    =   rhs%HasValidValues
  lhs%CheckValidValues  =   rhs%CheckValidValues
  lhs%HasBounds         =   rhs%HasBounds
  lhs%ilt               =   rhs%ilt
  lhs%ile               =   rhs%ile
  lhs%igt               =   rhs%igt
  lhs%ige               =   rhs%ige
  lhs%Val_lt            =   rhs%Val_lt
  lhs%Val_le            =   rhs%Val_le
  lhs%Val_gt            =   rhs%Val_gt
  lhs%Val_ge            =   rhs%Val_ge

  if ( allocated( rhs%DefaultValue    ) ) lhs%DefaultValue  =   rhs%DefaultValue
!   if ( allocated( rhs%DefaultValue      ) ) allocate( lhs%DefaultValue    , source = rhs%DefaultValue )
  if ( allocated( rhs%DataType          ) ) lhs%DataType      =   rhs%DataType
  if ( allocated( rhs%VariableKind      ) ) allocate( lhs%VariableKind     , source = rhs%VariableKind     )
# ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  if ( allocated( rhs%ValidValues       ) ) then
    allocate( character(len(rhs%ValidValues)) :: lhs%ValidValues( size(rhs%ValidValues) ) )
    lhs%ValidValues(:)  =   rhs%ValidValues
  end if
# else
  if ( allocated( rhs%ValidValues       ) ) allocate( lhs%ValidValues      , source = rhs%ValidValues      )
# endif



!   if ( allocated( rhs%LowerThan         ) ) allocate( lhs%LowerThan        , source = rhs%LowerThan        )
!   if ( allocated( rhs%LowerEqualThan    ) ) allocate( lhs%LowerEqualThan   , source = rhs%LowerEqualThan   )
!   if ( allocated( rhs%GreaterThan       ) ) allocate( lhs%GreaterThan      , source = rhs%GreaterThan      )
!   if ( allocated( rhs%GreaterEqualThan  ) ) allocate( lhs%GreaterEqualThan , source = rhs%GreaterEqualThan )
! # endif

End Procedure

Module Procedure OutputInputParamPropertiesToString

  use String_Library      ,only:  Add_Line_To_String, Inline

  character(1000)                                                               ::  LongString

  if ( allocated(This%Name) ) &
  call Add_Line_To_String( String, "Name              = " // trim(This%Name) )

  write(LongString,*) This%CaseSensitive
  call Add_Line_To_String( String, "CaseSensitive     = " // trim(LongString) )

  write(LongString,*) This%Mandatory
  call Add_Line_To_String( String, "Mandatory         = " // trim(LongString) )

  write(LongString,*) This%HasDefaultValue
  call Add_Line_To_String( String, "HasDefaultValue   = " // trim(LongString) )

  if ( This%HasDefaultValue ) &
  call Add_Line_To_String( String, "DefaultValue      = " // trim(This%DefaultValue) )

  write(LongString,*) This%HasValidValues
  call Add_Line_To_String( String, "HasValidValues    = " // trim(LongString) )

  write(LongString,*) This%CheckValidValues
  call Add_Line_To_String( String, "CheckValidValues  = " // trim(LongString) )

  if ( This%HasValidValues ) &
  call Add_Line_To_String( String, "ValidValues       = " // Inline(This%ValidValues,Separator="   ") )

  if ( allocated(This%DataType  ) ) &
  call Add_Line_To_String( String, "DataType          = " // This%DataType )

! @COMPILER_BUG: gcc-6.1.3 09/02/2017 Gfortran has a ICE if the 'select type' construct is used
# ifndef WORKAROUND_GFORTRAN_SELECT_TYPE
  if ( allocated(This%VariableKind  ) ) then
  associate( VariableKind => This%VariableKind)
    select type (VariableKind)
    type is (logical);          call Add_Line_To_String( String, "VariableKind    => " // "logical" )
    type is (integer);          call Add_Line_To_String( String, "VariableKind    => " // "integer" )
    type is (real(4));          call Add_Line_To_String( String, "VariableKind    => " // "real(4)" )
    type is (real(8));          call Add_Line_To_String( String, "VariableKind    => " // "real(8)" )
    type is (character(*));     call Add_Line_To_String( String, "VariableKind    => " // "character" )
    class default       ! Error
    end select
  end associate
  end if
# endif

End Procedure

Module Procedure OutputInputParamPropertiesToFile
  integer                                                                       ::  i
  integer                                                                       ::  OutUnit
  character(1000)                                                               ::  LongString
  character(:)  ,allocatable    ,dimension(:)                                   ::  LocalString
  character(:)  ,allocatable                                                    ::  Prefix
  character(*)  ,parameter                                                      ::  ProcName = "OutputInputParamPropertiesToFile"
  call This%Output( LocalString )
  if ( present(Unit) ) then
    OutUnit     =   Unit
    Prefix      =   "(3x,"
  else
    call Logger%Entering( ProcName )
    OutUnit     =   Logger%GetUnit()
    Prefix      =   Logger%GetFormatPrefix()
  end if
  do i = 1,size(LocalString)
    LongString        =   Prefix // ",'" // LocalString(i) // "',g0)"
    write(OutUnit,LongString)
  end do
  if ( .Not. present(Unit) ) then
    call Logger%Exiting()
  end if
End Procedure

Module Procedure GetValueIndex
  use String_Library    ,only:  Equal
  integer                                                               ::  i
  logical                                                               ::  Found_, CaseSens_
  Found_      =   .False.
  CaseSens_   =   GetOptArgValue(This%CaseSensitive,CaseSensitive)
  if ( This%HasValidValues ) then
    do i = lbound(This%ValidValues,1),ubound(This%ValidValues,1)
      if ( .Not. Equal( Value, This%ValidValues(i), CaseSensitive=CaseSens_ ) ) cycle
      iValue  =   i                                                                               ! Setting the output valid index
      Found_  =   .True.
      exit
    end do                                                                                                        ! End loop on all valid values of current parameter
  end if
  if ( present(Found) ) Found = Found_
End Procedure

Module Procedure GetName
  if (.Not. allocated(This%Name) ) then
    Name    =   "<oooooppppsss....>"
  else
    Name    =   This%Name
  end if
End Procedure

Module Procedure SetName
  This%Name   =   Name
End Procedure



! This procedure returns the character string corresponding to the element of the list of valid values
! whose index is given in input. If the input index is out-of-range (ie. if it is lower/higher than the
! lower/upper bound of the ValidValues vector) then an empty string is returned.
Module Procedure GetValue
  Value         =   "<UNKNOWN>"                                                                                      ! Initializing the output character string to an empty string (Required if there is no match)
  if ( .Not. This%HasValidValues ) return                                                                     ! If there are no possible values, then exiting the procedure
  if ( (iVal<lbound(This%ValidValues,1)) .or. (iVal>ubound(This%ValidValues,1)) ) return                      ! If the input index is out-of-range, then exiting the procedure
  Value         =   trim( This%ValidValues(iVal) )
End Procedure

Module Procedure GetDescription
  use String_Library    ,only:  Convert_To_String
  Summary   =   Convert_To_String(iVal) // " => " // This%GetName() // " = " // This%GetValue(iVal)
  if ( .Not. This%HasValidValues ) Summary = Summary // " (Has No Valid Values)"
End Procedure

! This procedure returns an indicator whether the input string corresponds to a valid value for the current
! InputParamProperties object.
! If InputParamProperties object has no valid values, then true is always return.
! If InputParamProperties object has a set of valid values, then true is return if and only if the input string corresponds
! to one of them.
Module Procedure IsValueValid
  use Utilities_Library   ,only:  IsIncluded
  IsValid   =   .True.                                                                                  ! Initializing the output indicator to true
  if ( .Not. This%HasValidValues ) return                                                                     ! If there are no possible values, then exiting the procedure
  IsValid   =   IsIncluded( Value, This%ValidValues, CaseSensitive=This%CaseSensitive )
End Procedure

! This procedure returns an indicator whether the integer input variable 'Index' corresponds to a valid index
! of an element of the array 'This%ValidValues'l, that is, if 'Index' is a valid indexs for a given valid value.
! The following cases can appear:
! * If the object has no valid values (ie. if the component 'ValidValues' is not allocated)
!     IsValid = False is always return since it make no sense to define an Index of a ValidValues array if this array is not define.
! * If the object has a set of valid values  (ie. if the component 'ValidValues' is allocated and has a set of elements)
!     * IsValid = True  =>  lbound(ValidValues) <= Index <= ubound(ValidValues)
!     * IsValid = False =>  Otherwise
Module Procedure IsIndexValid
  IsValid   =   .False.                                                                                  ! Initializing the output indicator to true
  if ( .Not. This%HasValidValues ) return                                                                     ! If there are no possible values, then exiting the procedure
  IsValid   =   (Index >= lbound(This%ValidValues,1) ) .and. (Index <= ubound(This%ValidValues,1) )
End Procedure


Module Procedure GetDefault
  use String_Library      ,only:  GetPosition
  Indicator     =   0
  if ( .Not. (This%HasDefaultValue .and. This%HasValidValues) ) return
  Indicator     =   GetPosition( This%DefaultValue, This%ValidValues, CaseSensitive=This%CaseSensitive )
!   select type ( DefaultValue => This%DefaultValue )
!     type is (character(*))
!       Indicator     =   GetPosition( This%DefaultValue, This%ValidValues, CaseSensitive=This%CaseSensitive )
!   end select
End Procedure

! This procedure returns the default value stored in the InputParamProperties
! object. Once found, this default value is then returned in the "Value"
! polymorphic variable. The type of the actual agument associated to the
! polymorphic dummy argument must match the type of the expected value, which
! is store in "VariableKind" component.
! If the current object has no default value, then an error is raised.
!
Module Procedure GetDefaultValue

  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  character(*)                                              ,parameter  ::  ProcName = "GetDefaultValue"
  logical                                                               ::  Dbg
  logical                                                               ::  FatalError, Error
  integer                                                               ::  Status_
  character(:)  ,allocatable                                            ::  Message

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  Error       =   .False.
  FatalError  =   .Not. ( present(Status) .or. present(ErrMsg) )
  if ( present(ErrMsg) ) ErrMsg = ""
  if ( present(Status) ) Status = 0

  if ( .Not. This%HasDefaultValue ) then
    if ( FatalError ) call Error_NoDefaultValue( This, ProcName )
    if ( present(ErrMsg) ) ErrMsg = "["//ProcName//"] Error: No default value in InputParamProperties object"
    if ( present(Status) ) Status = 1
    Error   =   .True.
  end if

! Checking that the actual and expected values have the same type
  if ( allocated(This%VariableKind  ) ) then
    if ( .Not. Same_Type_As(Value,This%VariableKind) ) then
      if ( FatalError ) call Error_DefaultValueTypeMismatch( This, ProcName )
      if ( present(ErrMsg) ) ErrMsg = "["//ProcName//"] Error: Type mismatch found for default value"
      if ( present(Status) ) Status = 1
      Error   =   .True.
    end if
  end if

! Converting the default value into the actual value kind
  if ( .Not. Error ) then
    select type (Value)
    type is (character(*))
      Value       =   This%DefaultValue
    class default
      call Convert_From_Character( This%DefaultValue, Value )
    end select
  end if

  if (Dbg) call Logger%Exiting()

End Procedure



! Subroutine CopyPolymorphicVar( Input, Output )
!   use String_Library      ,only:  Convert, Convert_To_Logical, Convert_To_Integer, Convert_To_Real
!   character(*)                                          ,intent(in)     ::  InpVar                          !< Input variable to be converted
!   class(*)                                              ,intent(out)    ::  OutVar                          !< Output variable resulting from the conversion
!
!
!
!   select type (DefaultValue)
!
!     type is ( character(*)    )
!       select type (Value)
!         type is ( character(*)    ); Value = DefaultValue
!         type is ( logical         ); Value = Convert_To_Logical(DefaultValue)
!         type is ( integer(INT8)   ); call Convert( DefaultValue, Value )
!         type is ( integer(INT16)  ); call Convert( DefaultValue, Value )
!         type is ( integer(INT32)  ); call Convert( DefaultValue, Value )
!         type is ( integer(INT64)  ); call Convert( DefaultValue, Value )
!         type is ( real(REAL32)    ); call Convert( DefaultValue, Value )
!         type is ( real(REAL64)    ); call Convert( DefaultValue, Value )
!         type is ( real(REAL128)   ); call Convert( DefaultValue, Value )
!       end select
!
!     type is ( logical         )
!       select type (Value)
!         type is ( character(*)    ); Value = "True"
!         type is ( logical         ); Value = DefaultValue
!         type is ( integer(INT8)   ); Value = Convert_To_Integer(DefaultValue)
!         type is ( integer(INT16)  ); Value = Convert_To_Integer(DefaultValue)
!         type is ( integer(INT32)  ); Value = Convert_To_Integer(DefaultValue)
!         type is ( integer(INT64)  ); Value = Convert_To_Integer(DefaultValue)
!         type is ( real(REAL32)    ); Value = Convert_To_Real(DefaultValue)
!         type is ( real(REAL64)    ); Value = Convert_To_Real(DefaultValue)
!         type is ( real(REAL128)   ); Value = Convert_To_Real(DefaultValue)
!       end select
!     type is ( integer(INT8)   )
!     type is ( integer(INT16)  )
!     type is ( integer(INT32)  )
!     type is ( integer(INT64)  )
!     type is ( real(REAL32)    )
!     type is ( real(REAL64)    )
!     type is ( real(REAL128)   )
!   end select
!
!
!
!
!   select type (Value)
!     type is (logical);
!     type is (integer);
!     type is (real(8));
!   end select
! End Subroutine



Module Procedure GetIndexDefaultValue
  use String_Library      ,only:  GetPosition
  character(*)                                              ,parameter  ::  ProcName = "GetIndexDefaultValue"
  logical                                                               ::  Dbg
  logical                                                               ::  RaiseError
  character(:)  ,allocatable                                            ::  Message
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  RaiseError   =   .True.
  if ( present(Error) .or. present(ErrMsg) ) RaiseError = .False.
! ==============================================================================================================
!    CHECKING THAT THE PROPERTIES OBJECT HAS A DEFAULT VALUE
! ==============================================================================================================
  if ( .Not. This%HasDefaultValue ) then                                                                       ! If the Properties object has no default value: Error
    Message     =   "Properties object has no default value => Error"                                       ! Setting the error message
    if ( RaiseError ) then                                                                                     ! If the error is to be raised in the current procedure
      call Logger%Write( Message )                                                                              ! Print the error message
      error stop                                                                                                ! Stopping the code
    else                                                                                                        ! If the error is to be passed to the calling procedure
      if ( present(ErrMsg) ) ErrMsg       =   Message                                                       ! Copying the error message
      if ( present(Error ) ) Error        =   .True.                                                        ! Copying the error indicator
      if (Dbg) call Logger%Exiting()
      return                                                                                                    ! Exiting the procedure
    end if                                                                                                      ! End if case error raising
  end if                                                                                                        ! End if case on no default value
! ==============================================================================================================
!    CHECKING THAT THE PROPERTIES OBJECT HAS A SET OF VALID VALUES
! ==============================================================================================================
  if ( .Not. This%HasValidValues) then                                                                        ! If the Properties object has no default set of valid values: Error
    Message     =   "Properties object has no set of valu values => Error"                                  ! Setting the error message
    if ( RaiseError ) then                                                                                     ! If the error is to be raised in the current procedure
      call Logger%Write( Message )                                                                              ! Print the error message
      error stop                                                                                                ! Stopping the code
    else                                                                                                        ! If the error is to be passed to the calling procedure
      if ( present(ErrMsg) ) ErrMsg       =   Message                                                       ! Copying the error message
      if ( present(Error ) ) Error        =   .True.                                                        ! Copying the error indicator
      if (Dbg) call Logger%Exiting()
      return                                                                                                    ! Exiting the procedure
    end if                                                                                                      ! End if case error raising
  end if                                                                                                        ! End if case on no default value
! ==============================================================================================================
!    GETTING THE INDEX
! ==============================================================================================================
  IndexDefaultValue   =   GetPosition( This%DefaultValue, This%ValidValues )
! ==============================================================================================================
  if ( present(ErrMsg) ) ErrMsg       =   ""                                                                ! Setting an empty error message
  if ( present(Error ) ) Error        =   .False.                                                            ! Setting the error indicator
  if (Dbg) call Logger%Exiting()
End Procedure

Module Procedure GetDefaultLogical
  use String_Library      ,only:  Convert => Convert_To_Logical
  if ( This%HasDefaultValue ) then
    Value       =   Convert(This%DefaultValue)
  else
    Value       =   .False.
  end if
End Procedure

Module Procedure GetDefaultInteger
  use String_Library      ,only:  Convert => Convert_To_Integer
  if ( This%HasDefaultValue ) then
    Value       =   Convert(This%DefaultValue)
  else
    Value       =   0
  end if
End Procedure

Module Procedure GetDefaultReal8
  use String_Library      ,only:  Convert => Convert_To_Real
  if ( This%HasDefaultValue ) then
    Value       =   Convert(This%DefaultValue)
  else
    Value       =   0.0_8
  end if
End Procedure

Module Procedure GetDefaultCharacter
  if ( This%HasDefaultValue ) then
    Value       =   This%DefaultValue
  else
    Value       =   ""
  end if
End Procedure

Module Procedure GetValidValuesLength
  use String_Library      ,only:  LenTrim
  Length    =   0
  if ( .Not. allocated(This%ValidValues) ) return
  if ( .Not. size(This%ValidValues) <= 0 ) return
  Length    =   LenTrim(This%ValidValues)
End Procedure

! ! !
! ! ! Module Function GetDefault_New( This ) result( Value )
! ! !
! ! !   class(InputParamProperties_Type)                              ,intent(in)     ::  This
! ! !   class(*)      ,allocatable                                                    ::  Value
! ! !
! ! !   character(*)  ,parameter                                                      ::  ProcName = "GetDefault_New"
! ! !
! ! !   call Logger%Entering( ProcName )
! ! !
! ! !   select type (Kind => This%VariableKind)
! ! !   type is (logical)
! ! !     allocate( Value, source = .True. )
! ! ! !     allocate( logical :: Value )
! ! ! !     Value       =   .True.
! ! !
! ! !   type is (integer)
! ! !     allocate( Value, source = 123 )
! ! ! !     Value       =   123
! ! ! !
! ! !   type is (real(8))
! ! !     allocate( Value, source = 5.678_8 )
! ! ! !     Value       =   5.678_rkp
! ! ! !
! ! !   type is (character(*))
! ! !     allocate( Value, source = "Value_set" )
! ! ! !     Value       =   "Value_set"
! ! ! !
! ! !   end select
! ! !
! ! !   call Logger%Exiting()
! ! !
! ! ! End Function

Subroutine Convert_From_Character( InpVar, OutVar )
  use String_Library      ,only:  Convert_To_Logical, Convert_To_Integer, Convert_To_Real
  character(*)                                          ,intent(in)     ::  InpVar                          !< Input variable to be converted
  class(*)                                              ,intent(out)    ::  OutVar                          !< Output variable resulting from the conversion
  select type (OutVar)
    type is (logical); OutVar = Convert_To_Logical(InpVar)
    type is (integer); OutVar = Convert_To_Integer(InpVar)
    type is (real(8)); OutVar = Convert_To_Real(InpVar)
  end select
End Subroutine

! Pure Subroutine IsValueInRange( This, Value, InRange )!, LowerThan, LowerEqualThan, GreaterThan, GreaterEqualThan )
!   use iso_fortran_env   ,only:  INT32, INT64, REAL32, REAL64, REAL128
!   class(InputParamProperties_Type)                      ,intent(in)     ::  This
!   class(*)                                              ,intent(in)     ::  Value
!   logical                                               ,intent(out)    ::  InRange
!   InRange   =   .True.
!   select type (Value)
!     type is ( integer(INT32) )
!       if ( This%ilt ) InRange = InRange .and. ( Value <  This%Val_lt )
!       if ( This%ile ) InRange = InRange .and. ( Value <= This%Val_le )
!       if ( This%igt ) InRange = InRange .and. ( Value >  This%Val_gt )
!       if ( This%ige ) InRange = InRange .and. ( Value >= This%Val_ge )
!     type is ( integer(INT64) )
!       if ( This%ilt ) InRange = InRange .and. ( Value <  This%Val_lt )
!       if ( This%ile ) InRange = InRange .and. ( Value <= This%Val_le )
!       if ( This%igt ) InRange = InRange .and. ( Value >  This%Val_gt )
!       if ( This%ige ) InRange = InRange .and. ( Value >= This%Val_ge )
!     type is ( real(REAL32) )
!       if ( This%ilt ) InRange = InRange .and. ( Value <  This%Val_lt )
!       if ( This%ile ) InRange = InRange .and. ( Value <= This%Val_le )
!       if ( This%igt ) InRange = InRange .and. ( Value >  This%Val_gt )
!       if ( This%ige ) InRange = InRange .and. ( Value >= This%Val_ge )
!     type is ( real(REAL64) )
!       if ( This%ilt ) InRange = InRange .and. ( Value <  This%Val_lt )
!       if ( This%ile ) InRange = InRange .and. ( Value <= This%Val_le )
!       if ( This%igt ) InRange = InRange .and. ( Value >  This%Val_gt )
!       if ( This%ige ) InRange = InRange .and. ( Value >= This%Val_ge )
!     type is ( real(REAL128) )
!       if ( This%ilt ) InRange = InRange .and. ( Value <  This%Val_lt )
!       if ( This%ile ) InRange = InRange .and. ( Value <= This%Val_le )
!       if ( This%igt ) InRange = InRange .and. ( Value >  This%Val_gt )
!       if ( This%ige ) InRange = InRange .and. ( Value >= This%Val_ge )
!   end select

!       if ( This%ilt ) then
!         select type ( Bound => This%LowerThan)
!           type is ( integer(INT32) ); InRange = InRange .and. ( Value < Bound )
!           type is ( integer(INT64) ); InRange = InRange .and. ( Value < Bound )
!           type is ( real(REAL32)   ); InRange = InRange .and. ( Value < Bound )
!           type is ( real(REAL64)   ); InRange = InRange .and. ( Value < Bound )
!           type is ( real(REAL128)  ); InRange = InRange .and. ( Value < Bound )
!         end select
!       end if
!
!       if ( This%ile ) then
!         select type ( Bound => This%LowerEqualThan)
!           type is ( integer(INT32) ); InRange = InRange .and. ( Value <= Bound )
!           type is ( integer(INT64) ); InRange = InRange .and. ( Value <= Bound )
!           type is ( real(REAL32)   ); InRange = InRange .and. ( Value <= Bound )
!           type is ( real(REAL64)   ); InRange = InRange .and. ( Value <= Bound )
!           type is ( real(REAL128)  ); InRange = InRange .and. ( Value <= Bound )
!         end select
!       end if
!
!       if ( This%igt ) then
!         select type ( Bound => This%GreaterThan)
!           type is ( integer(INT32) ); InRange = InRange .and. ( Value > Bound )
!           type is ( integer(INT64) ); InRange = InRange .and. ( Value > Bound )
!           type is ( real(REAL32)   ); InRange = InRange .and. ( Value > Bound )
!           type is ( real(REAL64)   ); InRange = InRange .and. ( Value > Bound )
!           type is ( real(REAL128)  ); InRange = InRange .and. ( Value > Bound )
!         end select
!       end if
!
!       if ( This%ige ) then
!         select type ( Bound => This%GreaterEqualThan)
!           type is ( integer(INT32) ); InRange = InRange .and. ( Value >= Bound )
!           type is ( integer(INT64) ); InRange = InRange .and. ( Value >= Bound )
!           type is ( real(REAL32)   ); InRange = InRange .and. ( Value >= Bound )
!           type is ( real(REAL64)   ); InRange = InRange .and. ( Value >= Bound )
!           type is ( real(REAL128)  ); InRange = InRange .and. ( Value >= Bound )
!         end select
!       end if

! End Subroutine

! Function IsValueBounded( This ) result(ValueHasBounds)
!   type(InputParamProperties_Type)                       ,intent(in)     ::  This
!   logical                                                               ::  ValueHasBounds
!   ValueHasBounds  =     allocated(This%LowerThan)         &
!                   .or.  allocated(This%LowerEqualThan)    &
!                   .or.  allocated(This%GreaterThan)       &
!                   .or.  allocated(This%GreaterEqualThan)
! End Function

! @TODO: Propagate the optut optional arguments 'Status' and 'ErrMsg if present and an error occurred.
Module Procedure CheckValueValidity
  use iso_fortran_env     ,only:  INT32, INT64, REAL32, REAL64, REAL128
  use Arithmetic_Library  ,only:  IsValueInRange
  character(*)                                              ,parameter  ::  ProcName = "CheckValueValidity"
  IsValid   =   .True.
  if ( .Not. This%HasBounds ) return
!   IsValid   =   IsValueInRange( Value, lt, le, gt, ge )
!   call IsValueInRange( This, Value, IsValid )
  select type (Value)
    type is ( integer(INT32) )
      if ( This%ilt ) IsValid = IsValid .and. ( Value <  This%Val_lt )
      if ( This%ile ) IsValid = IsValid .and. ( Value <= This%Val_le )
      if ( This%igt ) IsValid = IsValid .and. ( Value >  This%Val_gt )
      if ( This%ige ) IsValid = IsValid .and. ( Value >= This%Val_ge )
    type is ( integer(INT64) )
      if ( This%ilt ) IsValid = IsValid .and. ( Value <  This%Val_lt )
      if ( This%ile ) IsValid = IsValid .and. ( Value <= This%Val_le )
      if ( This%igt ) IsValid = IsValid .and. ( Value >  This%Val_gt )
      if ( This%ige ) IsValid = IsValid .and. ( Value >= This%Val_ge )
    type is ( real(REAL32) )
      if ( This%ilt ) IsValid = IsValid .and. ( Value <  This%Val_lt )
      if ( This%ile ) IsValid = IsValid .and. ( Value <= This%Val_le )
      if ( This%igt ) IsValid = IsValid .and. ( Value >  This%Val_gt )
      if ( This%ige ) IsValid = IsValid .and. ( Value >= This%Val_ge )
    type is ( real(REAL64) )
      if ( This%ilt ) IsValid = IsValid .and. ( Value <  This%Val_lt )
      if ( This%ile ) IsValid = IsValid .and. ( Value <= This%Val_le )
      if ( This%igt ) IsValid = IsValid .and. ( Value >  This%Val_gt )
      if ( This%ige ) IsValid = IsValid .and. ( Value >= This%Val_ge )
    type is ( real(REAL128) )
      if ( This%ilt ) IsValid = IsValid .and. ( Value <  This%Val_lt )
      if ( This%ile ) IsValid = IsValid .and. ( Value <= This%Val_le )
      if ( This%igt ) IsValid = IsValid .and. ( Value >  This%Val_gt )
      if ( This%ige ) IsValid = IsValid .and. ( Value >= This%Val_ge )
  end select
End Procedure












Subroutine Error_NoDefaultValue( Object, CallingProc )
  use Error_Class                ,only:  Error
  type(InputParamProperties_Type)                       ,intent(in)     ::  Object
  character(*)                                ,optional ,intent(in)     ::  CallingProc
  character(*)                                              ,parameter  ::  ProcName = "Error_NoDefaultValue"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines
  ProcPath      =   Logger%GetPath(CallingProc)
  call Object%Output( Lines )                                                                         ! Extracting the information from the parameter properties object
  call Error%Set_Title( "Error while processing a 'InputParamProperties' object" )
  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  The 'InputParamProperties' object has no default value." )
  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure path:              " // ProcPath )
  call Error%Add_Line( "  Properties:" )
  call Error%Add_Line( "  * " // Lines )
  call Error%Raise( Unit=Logger%GetUnit() )
End Subroutine

Subroutine Error_DefaultValueTypeMismatch( Object, CallingProc )
  use Error_Class                ,only:  Error
  type(InputParamProperties_Type)                       ,intent(in)     ::  Object
  character(*)                                ,optional ,intent(in)     ::  CallingProc
  character(*)                                              ,parameter  ::  ProcName = "Error_NoDefaultValue"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines
  ProcPath      =   Logger%GetPath(CallingProc)
  call Object%Output( Lines )                                                                         ! Extracting the information from the parameter properties object
  call Error%Set_Title( "Type mismatch found for default value" )
  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  The type of the default value of a 'InputParamProperties' object" )
  call Error%Add_Line( "  does not match the type of the variable in which this default" )
  call Error%Add_Line( "  value has to be stored into." )
  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Type of default value:        " )
  call Error%Add_Line( "  Type of output variable:      " )
  call Error%Add_Line( "  Procedure path:               " // ProcPath )
  call Error%Add_Line( "  Properties:" )
  call Error%Add_Line( "  * " // Lines )
  call Error%Raise( Unit=Logger%GetUnit() )
End Subroutine

End SubModule