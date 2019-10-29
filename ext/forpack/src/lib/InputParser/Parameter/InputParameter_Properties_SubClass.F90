SubModule(InputParameter_Class) InputParameter_Properties_SubClass

  use Logger_Class        ,only:  Logger
  use Utilities_Library   ,only:  GetOptArgValue

  implicit none

! @TODO: Add a custom separator character for 1D get value procedures
! @TODO: Propagate the use of the Status optional arg to all procedure in the Section object

! All GetValue procedure should implement the following optional arguments:
!  - CallProc: Input character variable containing the name of the calling procedure (Only used to gives some additional info if an error occurs)
!  - Found:          Output logical variable indicating whether the value has been found
!  - Mandatory:      Input logical variable indicating whether the value is mandatory (This value override the indicator in the PArameter object)
!  - Error:             Input logical variable indicating whether the an error should be raised if an error occurs
!  - Debug:         Input logical variable indicating whether the debug info should be printed to the Logger object

  logical               ,parameter      ::  DefaultDebug = .False.
  character(*)          ,parameter      ::  DefaultSeparator =   ","
  character(*)          ,parameter      ::  DefaultIgnoreBetween(1) = ['"'] !      ['"',"'"] ! ['"']

  contains

Module Procedure GetParameterName
  Name  =   This%Name
End Procedure

Module Procedure GetParameterRaw
  Raw   =   This%Raw
End Procedure

Module Procedure GetRawParameterValue
  if ( allocated(This%Value) ) then
    RawValue  =   This%Value
  else
    RawValue  =   ""
  end if
End Procedure

! This procedure extracts the value associated to a parameter assuming that the value is a logical scalar.
! The following can occure
! - If the parameter is defined, then the character string containing the value is converted into a logical
!   variable and stored inside the output variable "Value". Not that there is no check that the actual string
!   correspodns to a logical value (@TODO).
! - If the parameter is not defined, then the character string containing the value is empty.
!   Therefore, the actual actions performed depends on whether the parameter is marked as mandatory and  if
!   it has a default value:
!   - If the parameter is mandatory, then an error is raised.
!   - If the parameter is not mandatory and has a default value, then the value is set to this default value.
!   - If the parameter is not mandatory and has no default value, then the value is not set.
!     Since the "Value" argument has the "intent(inout)" attribute, this variable will kept it previous value.
Module Procedure GetValue_LOG_0d

  use String_Library      ,only:  Convert

  character(*)                                              ,parameter  ::  ProcName = "GetValue_LOG_0d"
  logical                                                               ::  Dbg
  integer                                                               ::  Status_

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

! ==============================================================================================================
!    PRINTING INFORMATION ON THE PARAMETER BEING PROCESSED
! ==============================================================================================================
  if (Dbg) then
    if (Dbg) call Logger%Write( "Parameter properties" )
    call Logger%Write( "-> This%Defined                     = ", This%Defined )
    call Logger%Write( "-> This%Properties%Mandatory        = ", This%Properties%Mandatory  )
    call Logger%Write( "-> This%Properties%HasDefaultValue  = ", This%Properties%HasDefaultValue )
    if ( This%Properties%HasDefaultValue ) &
    call Logger%Write( "-> This%Properties%DefaultValue     = ", This%Properties%DefaultValue )
    if ( This%Properties%HasValidValues  ) &
    call Logger%Write( "-> This%Properties%ValidValues      = ", This%Properties%ValidValues )
  end if
! ==============================================================================================================

  if ( present(Found) ) Found = This%Defined

! ==============================================================================================================
!    CASE OF A DEFINED PARAMETER
! ==============================================================================================================
! If the parameter is defined, then convert its value from character to logical. Note that there is no error
! if the character string does not correspond to a possible logical value.
! This should be deal with (@TODO)
! ==============================================================================================================
  if ( This%Defined ) then                                                                                      ! If the parameter is defined, then its value can be set (TODO: Check that it is of correct type)
    if (Dbg) call Logger%Write( "Defined parameter" )
    if (Dbg) call Logger%Write( "-> This%Value = ", This%Value )
    if ( len_trim(This%Value) == 0 ) then ! If the parameter has no string corresponding to the value, then set value to true (Only for logical)
      Value   =   .True.
      Status_ =   0
    else
      call Convert( This%Value, Value, Status=Status_ )
    end if
    if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
    if ( present(Status) ) Status = Status_
    if ( Status_ /= 0 ) then
      if (Dbg) call Logger%Write( "-> Error converting string" )
        if ( Status /= 0 ) then
          call Status%Set(                            &
                ProcName  =   ProcName              , &
                Messages  =   [ character(1000) ::    &
                  "Error during variable conversion from 'character' to 'logical'" , &
                  "when extracting the value '"//This%Value//"'from the parameter '"//This%Name//"'"]  )
        if (Dbg) call Logger%Exiting()
        return
      else
        call Error_Converting_String( This, CallProc )
      end if
      if (Dbg) call Logger%Write( "-> Value = ", Value )
    end if
! ==============================================================================================================


! ==============================================================================================================
!    CASE OF A UNDEFINED PARAMETER
! ==============================================================================================================
! If the parameter is not defined, it has no associated value. The actual actions performed depends on whether
! the parameter is marked as mandatory and if it has a default value. The following cases are considered:
! - If the parameter is mandatory, then an error is raised.
! - If the parameter is not mandatory and has a default value, then the value is set to this default value.
! - If the parameter is not mandatory and has no default value, then the value is not set.
!   Since the "Value" argument has the "intent(inout)" attribute, this variable will kept it previous value.
! ==============================================================================================================
  else                                                                                                          ! If the parameter is not defined
    if (Dbg) call Logger%Write( "Undefined parameter" )
    if (This%Properties%Mandatory) then                                                                         ! If the parameter is mandatory, then...
      if (Dbg) call Logger%Write( "-> Mandatory parameter => Error" )                                     ! ... debugging
      call Error_Undefined_Mandatory_Value( This, CallProc )                                           ! ... error
    else                                                                                                        ! If the parameter is not mandatory, then...
      if (Dbg) call Logger%Write( "-> Optional parameter" )
      if (This%Properties%HasDefaultValue) then                                                               ! If the parameter has a default value
        if (Dbg) call Logger%Write( "-> Setting parameter value to default value" )
        call Convert( This%Properties%DefaultValue, Value, Status=Status_ )
        if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
        if ( present(Status) ) Status = Status_
        if ( Status_ /= 0 ) then
          if (Dbg) call Logger%Write( "-> Error converting string" )
          if ( present(Status) ) then
            call Status%Set(                            &
                  ProcName  =   ProcName              , &
                  Messages  =   [ character(1000) ::    &
                    "Error during variable conversion from 'character' to 'logical'"  , &
                    "when extracting the default value '"//This%Properties%DefaultValue//"'from the parameter '"//This%Name//"'"]  )
            if (Dbg) call Logger%Exiting()
            return
          else
            call Error_Converting_String( This, CallProc )
          end if
        end if
        if (Dbg) call Logger%Write( "-> Value = ", Value )
      else                                                                                                      ! If the parameter has no default value
        if (Dbg) call Logger%Write( "-> No default value => Keep parameter value unchanged" )
      end if                                                                                                    ! End if case on default value
    end if                                                                                                      ! End if case on mandatory parameter
  end if                                                                                                        ! End if case on defined parameter
  if (Dbg) call Logger%Write( "-> On exit: Value = ", Value )
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure



! This procedure extracts the value associated to a parameter assuming that
! the value is a numeric scalar (integer/real). The following cases can occure:
! - If the parameter is defined, then the character string containing the
!   value is converted into the expected type and stored inside the output
!   variable 'Value'. If an error occures during the type conversion, that is,
!   if the character string does not correspond to the expected type, then
!   different actions are performed depending on the presence of the optional
!   argument 'Status':
!   - if absent:  an error is raised and the code is stopped
!   - if present: an error message/code is set in Status and the procedure is exited
! - If the parameter is not defined, then the action performed depends on whether
!   the parameter is marked as mandatory and if it has a default value:
!   - If mandatory:               an error is raised.
!   - If not mandatory
!     - If has a default value:   The value is set to this default value.
!     - If has no default value:  The value is not set and kept its previous value.
Module Procedure GetValue_INT8_0d
  use String_Library      ,only:  Convert
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT8_0d"
  character(*)                                              ,parameter  ::  VarType  = "integer(INT8)"
# include "inline-GetValue_0d.F90"
End Procedure

! This procedure extracts the value associated to a parameter assuming that
! the value is a numeric scalar (integer/real). The following cases can occure:
! - If the parameter is defined, then the character string containing the
!   value is converted into the expected type and stored inside the output
!   variable 'Value'. If an error occures during the type conversion, that is,
!   if the character string does not correspond to the expected type, then
!   different actions are performed depending on the presence of the optional
!   argument 'Status':
!   - if absent:  an error is raised and the code is stopped
!   - if present: an error message/code is set in Status and the procedure is exited
! - If the parameter is not defined, then the action performed depends on whether
!   the parameter is marked as mandatory and if it has a default value:
!   - If mandatory:               an error is raised.
!   - If not mandatory
!     - If has a default value:   The value is set to this default value.
!     - If has no default value:  The value is not set and kept its previous value.
Module Procedure GetValue_INT16_0d
  use String_Library      ,only:  Convert
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT16_0d"
  character(*)                                              ,parameter  ::  VarType  = "integer(INT16)"
# include "inline-GetValue_0d.F90"
End Procedure

! This procedure extracts the value associated to a parameter assuming that
! the value is a numeric scalar (integer/real). The following cases can occure:
! - If the parameter is defined, then the character string containing the
!   value is converted into the expected type and stored inside the output
!   variable 'Value'. If an error occures during the type conversion, that is,
!   if the character string does not correspond to the expected type, then
!   different actions are performed depending on the presence of the optional
!   argument 'Status':
!   - if absent:  an error is raised and the code is stopped
!   - if present: an error message/code is set in Status and the procedure is exited
! - If the parameter is not defined, then the action performed depends on whether
!   the parameter is marked as mandatory and if it has a default value:
!   - If mandatory:               an error is raised.
!   - If not mandatory
!     - If has a default value:   The value is set to this default value.
!     - If has no default value:  The value is not set and kept its previous value.
Module Procedure GetValue_INT32_0d
  use String_Library      ,only:  Convert
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT32_0d"
  character(*)                                              ,parameter  ::  VarType  = "integer(INT32)"
# include "inline-GetValue_0d.F90"
End Procedure

! This procedure extracts the value associated to a parameter assuming that
! the value is a numeric scalar (integer/real). The following cases can occure:
! - If the parameter is defined, then the character string containing the
!   value is converted into the expected type and stored inside the output
!   variable 'Value'. If an error occures during the type conversion, that is,
!   if the character string does not correspond to the expected type, then
!   different actions are performed depending on the presence of the optional
!   argument 'Status':
!   - if absent:  an error is raised and the code is stopped
!   - if present: an error message/code is set in Status and the procedure is exited
! - If the parameter is not defined, then the action performed depends on whether
!   the parameter is marked as mandatory and if it has a default value:
!   - If mandatory:               an error is raised.
!   - If not mandatory
!     - If has a default value:   The value is set to this default value.
!     - If has no default value:  The value is not set and kept its previous value.
Module Procedure GetValue_INT64_0d
  use String_Library      ,only:  Convert
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT64_0d"
  character(*)                                              ,parameter  ::  VarType  = "integer(INT64)"
# include "inline-GetValue_0d.F90"
End Procedure


! This procedure extracts the value associated to a parameter assuming that
! the value is a numeric scalar (integer/real). The following cases can occure:
! - If the parameter is defined, then the character string containing the
!   value is converted into the expected type and stored inside the output
!   variable 'Value'. If an error occures during the type conversion, that is,
!   if the character string does not correspond to the expected type, then
!   different actions are performed depending on the presence of the optional
!   argument 'Status':
!   - if absent:  an error is raised and the code is stopped
!   - if present: an error message/code is set in Status and the procedure is exited
! - If the parameter is not defined, then the action performed depends on whether
!   the parameter is marked as mandatory and if it has a default value:
!   - If mandatory:               an error is raised.
!   - If not mandatory
!     - If has a default value:   The value is set to this default value.
!     - If has no default value:  The value is not set and kept its previous value.
Module Procedure GetValue_REAL32_0d
  use String_Library      ,only:  Convert
  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL32_0d"
  character(*)                                              ,parameter  ::  VarType  = "real(REAL32)"
# include "inline-GetValue_0d.F90"
End Procedure

Module Procedure GetValue_REAL64_0d
  use String_Library      ,only:  Convert
  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL64_0d"
  character(*)                                              ,parameter  ::  VarType  = "real(REAL64)"
# include "inline-GetValue_0d.F90"
End Procedure

Module Procedure GetValue_REAL128_0d
  use String_Library      ,only:  Convert
  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL128_0d"
  character(*)                                              ,parameter  ::  VarType  = "real(REAL128)"
# include "inline-GetValue_0d.F90"
End Procedure

Module Procedure GetValue_CHAR_0d

  use String_Library      ,only:  Add_Line_To_String, UpperCase, Convert => Convert_To_String
  use Utilities_Library   ,only:  IsIncluded

  character(*)                                              ,parameter  ::  ProcName = "GetValue_CHAR_0d"
  logical                                                               ::  Dbg
  integer                                                               ::  Status_
  logical                                                               ::  Check_Values
  character(:)  ,dimension(:)   ,allocatable                            ::  ValidValues

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( present(CheckValidValues) ) then
    Check_Values        =   CheckValidValues
  else
    Check_Values        =   This%Properties%CheckValidValues
  end if

! ==============================================================================================================
!    PRINTING INFORMATION ON THE PARAMETER BEING PROCESSED
! ==============================================================================================================
  if (Dbg) then
    call Logger%Write( "This%Defined = ", This%Defined )
    call Logger%Write( "This%Properties%HasDefaultValue = ", This%Properties%HasDefaultValue )
    call Logger%Write( "This%Properties%CheckValidValues = ", This%Properties%CheckValidValues )
    if ( This%Properties%HasDefaultValue ) call Logger%Write( "This%Properties%DefaultValue = ", This%Properties%DefaultValue )
    if ( This%Properties%HasValidValues  ) call Logger%Write( "This%Properties%ValidValues  = ", This%Properties%ValidValues )
  end if
! ==============================================================================================================

  if ( present(Found) ) Found = This%Defined

! ==============================================================================================================
!    CASE OF A DEFINED PARAMETER
! ==============================================================================================================
! If the parameter is defined, then convert its value from character to real
! ==============================================================================================================
  if ( This%Defined ) then                                                                                      ! If the parameter is defined, then its value can be set (TODO: Check that it is of correct type)
    if (Dbg) call Logger%Write( "The parameter is defined: This%Value = ", This%Value )
    if ( This%Properties%HasValidValues ) then
# ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
      allocate( character(This%Properties%GetValidValuesLength()) :: ValidValues(size(This%Properties%ValidValues)) )
      ValidValues = This%Properties%ValidValues
# else
      allocate( ValidValues , source = This%Properties%ValidValues )
# endif
      Value             =   trim(This%Value)
      if ( .Not. This%Properties%CaseSensitive ) then
        ValidValues(:)  =   UpperCase(ValidValues)
        Value           =   UpperCase(Value)
      end if
      if ( Check_Values .and. (.Not.IsIncluded(Value,ValidValues)) ) then
        call Error_Value_Not_In_ValidValues( This, CallProc )
      end if
    end if
    Value   =   Convert( This%Value)
    Status_ =   0
    if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
    if ( Status_ /= 0 ) then
      if (Dbg) call Logger%Write( "-> Error converting string" )
      if ( present(Status) ) then
        call Status%Set(                            &
              ProcName  =   ProcName              , &
              Messages  =   [ character(1000) ::    &
                "Error during variable conversion from 'character' to 'character'"  , &
                "when extracting the value '"//This%Value//"'from the parameter '"//This%Name//"'"]  )
        if (Dbg) call Logger%Exiting()
        return
      else
        call Error_Converting_String( This, CallProc )
      end if
    end if
    if (Dbg) call Logger%Write( "-> Value = ", Value )
! ==============================================================================================================


! ==============================================================================================================
!    CASE OF A UNDEFINED PARAMETER
! ==============================================================================================================
! If the parameter is not defined, it has no associated value. The actual actions performed depends on whether
! the parameter is marked as mandatory and if it has a default value. The following cases are considered:
! - If the parameter is mandatory, then an error is raised.
! - If the parameter is not mandatory and has a default value, then the value is set to this default value.
! - If the parameter is not mandatory and has no default value, then the value is not set.
!   Since the "Value" argument has the "intent(inout)" attribute, this variable will kept it previous value.
! ==============================================================================================================
  else                                                                                                          ! If the parameter is not defined
    if (Dbg) call Logger%Write( "The parameter is not defined ... " )
    if (This%Properties%Mandatory) then                                                                         ! If the parameter is mandatory, then...
      if (Dbg) call Logger%Write( "... and is mandatory => Error" )                                     ! ... debugging
      call Error_Undefined_Mandatory_Value( This, CallProc )                                          ! ... error
    else                                                                                                        ! If the parameter is not mandatory, then...
      if (Dbg) call Logger%Write( "... but it is not mandatory" )
      if (This%Properties%HasDefaultValue) then                                                               ! If the parameter has a default value
        if (Dbg) call Logger%Write( "... and it has a default value => Setting the parameter value to the default value" )
        Value   =   Convert( This%Properties%DefaultValue )
        Status_ =   0
        if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
        if ( Status_ /= 0 ) then
          if (Dbg) call Logger%Write( "-> Error converting string" )
          if ( present(Status) ) then
            if (Dbg) call Logger%Exiting()
            return
          else
            call Error_Converting_String( This, CallProc )
          end if
        end if
      else                                                                                                      ! If the parameter has no default value
        if (Dbg) call Logger%Write( "... and it has no default value => Do not change the parameter value" )
      end if                                                                                                    ! End if case on default value
    end if                                                                                                      ! End if case on mandatory parameter
  end if                                                                                                        ! End if case on defined parameter
  if (Dbg) call Logger%Write( "Value = ", Value )
! ==============================================================================================================


  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure GetValue_LOG_1d

  use String_Library      ,only:  Convert, Parse
  use Arithmetic_Library  ,only:  Compute_LinSpace, Insertion_Sort

  character(*)                                              ,parameter  ::  ProcName = "GetValue_LOG_1d"
!   real(8)                                                               ::  StartValue
!   real(8)                                                               ::  EndValue
!   real(8)                                                               ::  StepValue
!   _VarType_ ,dimension(:)   ,allocatable                                ::  LocalVector
!   _VarType_ ,dimension(:)   ,allocatable                                ::  GlobalVector
!   real(8)   ,dimension(:)   ,allocatable                                ::  List_Elements

  logical                                                               ::  Dbg
  integer                                                               ::  Status_
  logical                                                               ::  Is_Mandatory
  logical                                                               ::  Check_Values
!   logical                                                               ::  IsValid
!   character(:)  ,dimension(:)   ,allocatable                            ::  ValidValues
  character(:)  ,dimension(:)   ,allocatable                            ::  List_Values!, LeftRightStrings
  character(:)  ,allocatable                                            ::  Value
  character(:)  ,allocatable                                            ::  Separator_                         ! Separation character
  integer                                                               ::  i

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

! ==============================================================================================================
!       PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if ( present(Mandatory) ) then;           Is_Mandatory  =   Mandatory
  else;                                     Is_Mandatory  =   This%Properties%Mandatory; end if
  if ( present(Separator) ) then;           Separator_    =   Separator                                                           ! Setting the local variable to the optional variable if present ...
  else;                                     Separator_    =   ' ';        end if                                           ! ... otherwise setting the local variable to the default variable
  if ( present(CheckValidValues) ) then;    Check_Values  =   CheckValidValues
  else;                                     Check_Values  =   This%Properties%CheckValidValues; end if
! ==============================================================================================================


! ==============================================================================================================
!    PRINTING INFORMATION ON THE PARAMETER BEING PROCESSED
! ==============================================================================================================
  if (Dbg) then
    call Logger%Write( "This%Defined = ", This%Defined )
    call Logger%Write( "This%Properties%HasDefaultValue = ", This%Properties%HasDefaultValue )
    if ( This%Properties%HasDefaultValue ) call Logger%Write( "This%Properties%DefaultValue = ", This%Properties%DefaultValue )
    if ( This%Properties%HasValidValues  ) call Logger%Write( "This%Properties%ValidValues  = ", This%Properties%ValidValues )
  end if
! ==============================================================================================================

  if ( present(Found) ) Found = This%Defined

! ==============================================================================================================
!    CASE OF A DEFINED PARAMETER
! ==============================================================================================================
! This section deals with the case when the parameter is defined, that is, it has an associated value.
! If the parameter is defined, then its value is stored as a character string inside the 'String' component.
! This string is parsed using the 'Parse' procedures into several sub-string separated by the separation
! character. Then, each element of th string is converted into a real(8) variable, which is then stored in the
! output variable 'Values'.
! ==============================================================================================================
  if ( This%Defined ) then                                                                                      ! If the parameter is defined, then its value can be set (TODO: Check that it is of correct type)
    if (Dbg) call Logger%Write( "The parameter is defined" )
    Value         =   trim(This%Value)                                                                     ! Copying the string corresponding to the parameter value in a local variable
    if (Dbg) call Logger%Write( "Calling Parse: Value = '"//Value//"' Separator_ = '"//Separator_//"'" )
    call Parse( Value, Separator_, List_Values, IgnoreBetween=DefaultIgnoreBetween )                                             ! Parsing the string into multiple elements separated by the separation character
    if (Dbg) call Logger%Write( "List_Values = ", List_Values )
    List_Values(:)  =   Remove_Quotes( List_Values )
    if (Dbg) then
      do i = 1,size(List_Values)
        call Logger%Write( "i = ", i, "List_Values(i) = ", List_Values(i) )
      end do
    end if

    if ( allocated(Values) ) deallocate(Values)
    allocate( Values( size(List_Values) ) )
    do i = 1,size(List_Values)
      call Convert( List_Values(i), Values(i), Status=Status_ ) !@TODO: Add in String_Module 1d version of the convert subroutine
      if ( present(Status) ) Status = Status_
      if ( Status_ /= 0 ) then
        if (Dbg) call Logger%Write( "-> Error converting string" )
        if ( present(Status) ) then
          call Status%Set(                            &
                ProcName  =   ProcName              , &
                Messages  =   [ character(1000) ::    &
                  "Error during variable conversion from 'character' to 'logical'" , &
                  "when extracting the value '"//List_Values(i)//"'from the parameter '"//This%Name//"'"]  )
          if (Dbg) call Logger%Exiting()
          return
        else
          call Error_Converting_String( This, CallProc )
        end if
      end if
    end do

!       if ( allocated(LocalVector) ) deallocate( LocalVector )
!       if ( size(LeftRightStrings) >= 2 ) then
!         select case ( size(LeftRightStrings) )
!           case (1)  ! Case when only the Min and Max values are provided as in 'Max:Min'
!           case (2)  ! Case when only the Min and Max values are provided as in 'Max:Min'
!             StartValue  =   Convert( LeftRightStrings(1) )
!             EndValue    =   Convert( LeftRightStrings(2) )
!             StepValue   =   1.0_8
!           case (3)  ! Case when the Min, Max and Step values are provided as in 'Max:Min:Step'
!             StartValue  =   Convert( LeftRightStrings(1) )
!             EndValue    =   Convert( LeftRightStrings(2) )
!             StepValue   =   Convert( LeftRightStrings(3) )
!         end select
!         call Compute_LinSpace( LocalVector, Min=StartValue, Max=EndValue, Step=StepValue )
!         if (Dbg) call Logger%Write( "  LocalVector  = ", LocalVector, Fr="f4.1" )
!       else
!         StartValue  =   Convert( List_Values(i) )
!         allocate( LocalVector, source = [StartValue] )
!       end if
! ! *****************************************************************************
! !       call AddElementToArray( Elements, Array )
!       if ( .not. allocated(GlobalVector) ) allocate( GlobalVector(0) )
!       allocate( List_Elements, source = [GlobalVector,LocalVector] )
!       call move_alloc( List_Elements, GlobalVector )
! ! *****************************************************************************
!       if (Dbg) call Logger%Write( "  GlobalVector = ", GlobalVector, Fr="f4.1" )
!     end do

!     if ( allocated(Values) ) deallocate(Values)
!     allocate( Values, source = Convert( List_Values ) )                                                      ! Getting its value
! ==============================================================================================================


! ==============================================================================================================
!    CASE OF A UNDEFINED PARAMETER
! ==============================================================================================================
! This section deals with the case when the parameter is not defined, that is, it has no associated value.
! If the parameter is not defined, then the actual actions to be performed depend on whether the parameter
! is mandatory and/or if it has a default value.
! The following cases are considered:
! - If the parameter is mandatory, then an error is raised.
! - If the parameter is not mandatory and has a default value, then the value is set to this default value.
! - If the parameter is not mandatory and has no default value, then the value is not set.
!   Since the "Value" argument has the "intent(inout)" attribute, this variable will kept it previous value.
! ==============================================================================================================
  else                                                                                                          ! If the parameter is not defined
    if (Dbg) call Logger%Write( "The parameter is not defined ... " )
    if (Is_Mandatory) then                                                                                      ! If the parameter is mandatory, then...
      if (Dbg) call Logger%Write( "... and is mandatory => Error" )                                     ! ... debugging
      call Error_Undefined_Mandatory_Value( This, CallProc )                                          ! ... error
    else                                                                                                        ! If the parameter is not mandatory, then...
      if (Dbg) call Logger%Write( "... but it is not mandatory" )
      if (This%Properties%HasDefaultValue) then                                                               ! If the parameter has a default value
        if (Dbg) call Logger%Write( "... and it has a default value => Setting the parameter value to the default value" )
        Value         =   trim(This%Properties%DefaultValue)
        if (Dbg) call Logger%Write( "Calling Parse: Value = '"//Value//"' Separator_ = '"//Separator_//"'" )
        call Parse( Value, Separator_, List_Values, IgnoreBetween=DefaultIgnoreBetween )                                         ! Parsing the string into multiple elements separated by the separation character
        if (Dbg) call Logger%Write( "List_Values = ", List_Values )
        List_Values(:)  =   Remove_Quotes( List_Values )


        if ( allocated(Values) ) deallocate(Values)
        allocate( Values( size(List_Values) ) )
        do i = 1,size(List_Values)
          call Convert( List_Values(i), Values(i), Status=Status_ ) !@TODO: Add in String_Module 1d version of the convert subroutine
          if ( present(Status) ) Status = Status_
          if ( Status_ /= 0 ) then
            if (Dbg) call Logger%Write( "-> Error converting string" )
            if ( present(Status) ) then
              call Status%Set(                            &
                    ProcName  =   ProcName              , &
                    Messages  =   [ character(1000) ::    &
                      "Error during variable conversion from 'character' to 'logical'"  , &
                      "when extracting the default value '"//List_Values(i)//"'from the parameter '"//This%Name//"'"]  )
              if (Dbg) call Logger%Exiting()
              return
            else
              call Error_Converting_String( This, CallProc )
            end if
          end if
        end do


!         if ( allocated(Values) ) deallocate(Values)
!         allocate( Values, source = Convert(List_Values) )                                                      ! Getting its value
      else                                                                                                      ! If the parameter has no default value
        if (Dbg) call Logger%Write( "... and it has no default value => Do not change the parameter value" )
      end if                                                                                                    ! End if case on default value
    end if                                                                                                      ! End if case on mandatory parameter
  end if                                                                                                        ! End if case on defined parameter
  if (Dbg) call Logger%Write( "Values = ", Values, Fr="es15.8" )
! ==============================================================================================================

!   call This%Properties%CheckValueValidity( Value, IsValid )
!   if ( .Not. IsValid ) then
!     call Error_ValueNotValid( This, CallProc )
!   end if

  if (Dbg) call Logger%Exiting()

End Procedure

# define  _VarType_   integer(INT8)
Module Procedure GetValue_INT8_1d
  use String_Library      ,only:  Convert, Parse
  use Arithmetic_Library  ,only:  Compute_LinSpace, Insertion_Sort
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT8_1d"
  character(*)                                              ,parameter  ::  VarType  = "integer(INT8)"
  _VarType_                                                             ::  StartValue, EndValue, StepValue
  _VarType_ ,allocatable                                                ::  LocalVector(:), GlobalVector(:), List_Elements(:)
  integer                                                               ::  Status_
# include "inline-GetValue_1d.F90"
End Procedure
# undef   _VarType_


# define  _VarType_   integer(INT16)
Module Procedure GetValue_INT16_1d
  use String_Library      ,only:  Convert, Parse
  use Arithmetic_Library  ,only:  Compute_LinSpace, Insertion_Sort
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT16_1d"
  character(*)                                              ,parameter  ::  VarType  = "integer(INT16)"
  _VarType_                                                             ::  StartValue, EndValue, StepValue
  _VarType_ ,allocatable                                                ::  LocalVector(:), GlobalVector(:), List_Elements(:)
  integer                                                               ::  Status_
# include "inline-GetValue_1d.F90"
End Procedure
# undef   _VarType_


# define  _VarType_   integer(INT32)
Module Procedure GetValue_INT32_1d
  use String_Library      ,only:  Convert, Parse
  use Arithmetic_Library  ,only:  Compute_LinSpace, Insertion_Sort
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT32_1d"
  character(*)                                              ,parameter  ::  VarType  = "integer(INT32)"
  _VarType_                                                             ::  StartValue, EndValue, StepValue
  _VarType_ ,allocatable                                                ::  LocalVector(:), GlobalVector(:), List_Elements(:)
  integer                                                               ::  Status_
# include "inline-GetValue_1d.F90"
End Procedure
# undef   _VarType_


# define  _VarType_   integer(INT64)
Module Procedure GetValue_INT64_1d
  use String_Library      ,only:  Convert, Parse
  use Arithmetic_Library  ,only:  Compute_LinSpace, Insertion_Sort
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT64_1d"
  character(*)                                              ,parameter  ::  VarType  = "integer(INT64)"
  _VarType_                                                             ::  StartValue, EndValue, StepValue
  _VarType_ ,allocatable                                                ::  LocalVector(:), GlobalVector(:), List_Elements(:)
  integer                                                               ::  Status_
# include "inline-GetValue_1d.F90"
End Procedure
# undef   _VarType_

# define  _VarType_   real(REAL32)
Module Procedure GetValue_REAL32_1d
  use String_Library      ,only:  Convert, Parse
  use Arithmetic_Library  ,only:  Compute_LinSpace, Insertion_Sort
  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL32_1d"
  character(*)                                              ,parameter  ::  VarType  = "real(REAL32)"
  _VarType_                                                             ::  StartValue, EndValue, StepValue
  _VarType_ ,allocatable                                                ::  LocalVector(:), GlobalVector(:), List_Elements(:)
  integer                                                               ::  Status_
# include "inline-GetValue_1d.F90"
End Procedure
# undef   _VarType_

# define  _VarType_   real(REAL64)
Module Procedure GetValue_REAL64_1d
  use String_Library      ,only:  Convert, Parse
  use Arithmetic_Library  ,only:  Compute_LinSpace, Insertion_Sort
  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL64_1d"
  character(*)                                              ,parameter  ::  VarType  = "real(REAL64)"
  _VarType_                                                             ::  StartValue, EndValue, StepValue
  _VarType_ ,allocatable                                                ::  LocalVector(:), GlobalVector(:), List_Elements(:)
  integer                                                               ::  Status_
# include "inline-GetValue_1d.F90"
End Procedure
# undef   _VarType_

# define  _VarType_   real(REAL128)
Module Procedure GetValue_REAL128_1d
  use String_Library      ,only:  Convert, Parse
  use Arithmetic_Library  ,only:  Compute_LinSpace, Insertion_Sort
  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL128_1d"
  character(*)                                              ,parameter  ::  VarType  = "real(REAL128)"
  _VarType_                                                             ::  StartValue, EndValue, StepValue
  _VarType_ ,allocatable                                                ::  LocalVector(:), GlobalVector(:), List_Elements(:)
  integer                                                               ::  Status_
# include "inline-GetValue_1d.F90"
End Procedure
# undef   _VarType_


! For this one, possible values make no sens (?)
!  - CallProc: Input character variable containing the name of the calling procedure (Only used to gives some additional info if an error occurs)
!  - Found:          Output logical variable indicating whether the value has been found
!  - Mandatory:      Input logical variable indicating whether the value is mandatory (This value override the indicator in the PArameter object)
!  - Error:             Input logical variable indicating whether the an error should be raised if an error occurs

Module Procedure GetValue_CHAR_1d

  use String_Library      ,only:  Parse, TrimCharacter

  character(*)                                              ,parameter  ::  ProcName = "GetValue_CHAR_1d"
  logical                                                               ::  Dbg
  integer                                                               ::  Status_
  logical                                                               ::  Is_Mandatory
  logical                                                               ::  Check_Values
  character(:)  ,allocatable                                            ::  Value
  character(:)  ,allocatable                                            ::  Separator_                         ! Separation character

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

! ==============================================================================================================
!       PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if ( present(Mandatory) ) then;           Is_Mandatory  =   Mandatory
  else;                                     Is_Mandatory  =   This%Properties%Mandatory; end if
  if ( present(Separator) ) then;           Separator_    =   Separator                                         ! Setting the local variable to the optional variable if present ...
  else;                                     Separator_    =   DefaultSeparator;        end if                  ! ... otherwise setting the local variable to the default variable
  if ( present(CheckValidValues) ) then;  Check_Values  =   CheckValidValues
  else;                                     Check_Values  =   This%Properties%CheckValidValues; end if
! ==============================================================================================================


! ==============================================================================================================
!    PRINTING INFORMATION ON THE PARAMETER BEING PROCESSED
! ==============================================================================================================
  if (Dbg) then
    call Logger%Write( "Information on the parameter being processed" )
    call Logger%Write( "-> This%Defined = ", This%Defined )
    call Logger%Write( "-> Separator_      = ", Separator_ )
    call Logger%Write( "-> Is_Mandatory = ", Is_Mandatory )
    call Logger%Write( "-> This%Properties%HasDefaultValue = ", This%Properties%HasDefaultValue )
    if ( allocated(This%Properties%DefaultValue) ) call Logger%Write( "-> This%Properties%DefaultValue   = ", This%Properties%DefaultValue )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    CASE OF A DEFINED PARAMETER
! ==============================================================================================================
! This section deals with the case when the parameter is defined, that is, it has an associated value.
! If the parameter is defined, then its value is stored as a character string inside the 'String' component.
! This string is parsed using the 'Parse' procedures into several sub-string separated by the separation
! character. The resulting strings is stored in the output variable 'Values'.
! ==============================================================================================================
  if ( This%Defined ) then                                                                                      ! If the parameter is defined, then its value can be set (TODO: Check that it is of correct type)
    if (Dbg) call Logger%Write( "Case of a defined parameter" )
    if (Dbg) call Logger%Write( "-> Raw value: This%Value = '" // trim(This%Value) //"'" )
    Value       =   trim(This%Value)                                                                       ! Copying the string corresponding to the parameter value in a local variable
    if ( len(Value) > 0 ) then
      if (Dbg) call Logger%Write( "-> Calling Parse with option IgnoreBetween" )
      call Parse( Value, Separator_, Values, IgnoreBetween=DefaultIgnoreBetween )                                 ! Parsing the string into multiple elements separated by the separation character
      if (Dbg) call Logger%Write( "-> allocated(Values) = ", allocated(Values) )
      if (Dbg) call Logger%Write( "-> size(Values) = ", size(Values) )
      if (Dbg) call Logger%Write( "-> After Parse:          Values = ", Values )
      Values(:)      =   Remove_Quotes( Values )
      if (Dbg) call Logger%Write( "-> After Remove_Quotes:  Values = ", Values )
      call TrimCharacter( Values )
    else
      if ( allocated(Values) ) deallocate(Values)
      allocate( Values , source = [Value] )
    end if
    if (Dbg) call Logger%Write( "-> Values = ", Values )
! ==============================================================================================================


! ==============================================================================================================
!    CASE OF A UNDEFINED PARAMETER
! ==============================================================================================================
! This section deals with the case when the parameter is not defined, that is, it has no associated value.
! If the parameter is not defined, then the actual actions to be performed depend on whether the parameter
! is mandatory and/or if it has a default value.
! The following cases are considered:
! - If the parameter is mandatory, then an error is raised.
! - If the parameter is not mandatory and has a default value, then the value is set to this default value.
! - If the parameter is not mandatory and has no default value, then the value is not set.
!   Since the "Value" argument has the "intent(inout)" attribute, this variable will kept it previous value.
! ==============================================================================================================
  else                                                                                                          ! If the parameter is not defined
    if (Dbg) call Logger%Write( "Case of a un-defined parameter" )
    if (Is_Mandatory) then                                                                                      ! If the parameter is mandatory, then...
      if (Dbg) call Logger%Write( "-> The parameter is not NOT defiend and is mandatory => Error" )     ! ... debugging
      call Error_Undefined_Mandatory_Value( This, CallProc )                                           ! ... error
    else                                                                                                        ! If the parameter is not mandatory, then...
      if (Dbg) call Logger%Write( "-> The parameter is NOT mandatory" )
      if (This%Properties%HasDefaultValue) then                                                                 ! If the parameter has a default value
        if (Dbg) call Logger%Write( "-> The parameter has a default value => Setting the parameter value to the default value" )
        Value   =   trim(This%Properties%DefaultValue)                                                      ! Copying the string
        call Parse( Value, Separator_, Values )                                                                 ! Parsing the string into multiple elements separated by the separation character
        call TrimCharacter( Values )                                                                            ! Removing trailing blanks
      else                                                                                                      ! If the parameter has no default value
        if (Dbg) call Logger%Write( "-> The parameter has no default value => Do NOT change the parameter value" )
      end if                                                                                                    ! End if case on default value
    end if                                                                                                      ! End if case on mandatory parameter
  end if                                                                                                        ! End if case on defined parameter
! ==============================================================================================================

  if (Dbg) then
    call Logger%Write( "On exit" )
    call Logger%Write( "-> allocated(Values) = ", allocated(Values) )
    if ( allocated(Values) ) call Logger%Write( "-> Values = ", Values )
  end if


  if (Dbg) call Logger%Exiting()

End Procedure


# define  _VarType_   integer(INT8)
Module Procedure GetValue_INT8_2d
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT8_2d"
# include "inline-GetValue_2d.F90"
End Procedure

# define  _VarType_   integer(INT16)
Module Procedure GetValue_INT16_2d
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT16_2d"
# include "inline-GetValue_2d.F90"
End Procedure

# define  _VarType_   integer(INT32)
Module Procedure GetValue_INT32_2d
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT32_2d"
# include "inline-GetValue_2d.F90"
End Procedure

# define  _VarType_   integer(INT64)
Module Procedure GetValue_INT64_2d
  character(*)                                              ,parameter  ::  ProcName = "GetValue_INT64_2d"
# include "inline-GetValue_2d.F90"
End Procedure

# define  _VarType_   real(REAL32)
Module Procedure GetValue_REAL32_2d
  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL32_2d"
# include "inline-GetValue_2d.F90"
End Procedure

# define  _VarType_   real(REAL64)
Module Procedure GetValue_REAL64_2d
  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL64_2d"
# include "inline-GetValue_2d.F90"
End Procedure

# define  _VarType_   real(REAL128)
Module Procedure GetValue_REAL128_2d
  character(*)                                              ,parameter  ::  ProcName = "GetValue_REAL128_2d"
# include "inline-GetValue_2d.F90"
End Procedure



! This procedure retrieves the index of the ValidValues corresponding to the actual value of a parameter.
! The call to this procedure only make sense if the parameter has a set of ValidValues.
! If not, an error should appear.
Module Procedure GetParameterIndex

  use String_Library      ,only:  UpperCase

  integer                                                               ::  i
  character(*)                                              ,parameter  ::  ProcName = "GetParameterIndex" ! Name of current procedure
  logical                                                               ::  Dbg
  integer                                                               ::  Status_
  character(:)  ,allocatable                                            ::  Current_Value
  character(:)  ,allocatable                                            ::  Accepted_Value

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  iParam        =   0

! ==============================================================================================================
!       DEALING WITH THE CASE WHENT THE PARAMETERS HAS NO VALID VALUES: ERROR
! ==============================================================================================================
! This section deals with the case when the Parameter has no valid value.
! Every call to this procedure for a parameter which has no ValidValues is an error.

! ==============================================================================================================
  if (Dbg) call Logger%Write( "allocated(This%Properties%ValidValues) = ", allocated(This%Properties%ValidValues) )
  if (Dbg) call Logger%Write( "This%Properties%HasValidValues = ", This%Properties%HasValidValues )
  if ( .Not. This%Properties%HasValidValues ) then                                                            ! If the parameter has no set of valid values, then it is an error since no index can be returned
    if (Dbg) call Logger%Write( "No set of possible values => Error" )
    call Error_Undefined_Mandatory_ValidValues( This, CallProc )                                     ! Raising the error message and stopping the code
  end if                                                                                                        ! End if case on absent valid values
! ==============================================================================================================

! ==============================================================================================================
!       GETTING THE PARAMETER VALUE
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Extracting the value of the paarameter" )
  if (Dbg) call Logger%Write( "-> Calling This%GetValue" )
  call This%GetValue(             &
        Current_Value           , &
        CallProc  =   ProcName  , &
        Status    =   Status    , &
        Debug     =   Debug       )
  if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return
  if (Dbg) call Logger%Write( "-> Current_Value = ", Current_Value )
  if (Dbg) call Logger%Write( "-> This%Properties%CaseSensitive = ", This%Properties%CaseSensitive )
  if ( .Not. This%Properties%CaseSensitive ) then
    if (Dbg) call Logger%Write( "-> Parameter is NOT case sensitive" )
    Current_Value = UpperCase(Current_Value)
    if (Dbg) call Logger%Write( "-> Current_Value = ", Current_Value )
  end if
! ==============================================================================================================


! ==============================================================================================================
!       SEARCHING FOR THE INDEX OF THE VALIDVALUES MATCHING THE CURRENT VALUE
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Loop over all possible values to find the index of the ValidValues matching the current value" )
  do i = 1,size(This%Properties%ValidValues)
    Accepted_Value    =   This%Properties%GetValue(i)
    if ( .Not. This%Properties%CaseSensitive ) Accepted_Value = UpperCase(Accepted_Value)
    if (Dbg) call Logger%Write( "  i = ", i, "Accepted_Value = ", Accepted_Value, Fi="i5" )
    if ( Current_Value /= Accepted_Value ) cycle
    if (Dbg) call Logger%Write( "  Match found !!!" )
    iParam              =   i
    exit
  end do
! ==============================================================================================================


! ==============================================================================================================
!       DEALING WITH THE CASE WHENT THE PARAMETERS HAS NOT BEEN FOUND: ERROR
! ==============================================================================================================
  if ( iParam == 0 ) then                                                                                       ! If the parameter has not been found, then error
    if (Dbg) call Logger%Write( "The parameter has not been found => Error" )
    call Error_Mandatory_ValidValues_Not_Found( This, CallProc )                                       ! Raising the error message and stopping the code
  end if                                                                                                        ! End if case on unfound parameter
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure



! This procedure idensify is the current Parameter object corresponds to a function string.
! A function string is a string which has the following format:
!       FctName( ... )
! So the string must start by a keyword -- "FctName" in the above example -- and have a list
! of argument inbetween some parentesis.
!                       NoArg         =   .True. )
Module Procedure IsParameterFunctionNoName
  use String_Library      ,only:  IsFunction
  IsFct = IsFunction( This%Raw  , &
                      CaseSensitive =   CaseSensitive )
End Procedure

Module Procedure IsParameterFunctionFromName
  use String_Library      ,only:  IsFunction
  IsFct = IsFunction( This%Raw, Name  , &
                      CaseSensitive =   CaseSensitive )
End Procedure

Module Procedure IsParameterFunctionFromNames
  use String_Library      ,only:  IsFunction
  IsFct = IsFunction( This%Raw, Names , &
                      CaseSensitive =   CaseSensitive )
End Procedure





! ! This procedure idensify is the current Parameter object corresponds to a function string.
! ! A function string is a string which has the following format:
! !       FctName( ... )
! ! So the string must start by a keyword -- "FctName" in the above example -- and have a list
! ! of argument inbetween some parentesis.
! Module Procedure IsParameterFunctionFromName
!
! !   use String_Library      ,only:  UpperCase, ParseFunction
!   use String_Library      ,only:  IsFunction
!
!   character(*)                                              ,parameter  ::  ProcName = "IsParameterFunctionFromName"
!   logical                                                               ::  Dbg
! !   character(:)  ,allocatable                                            ::  Name_, FctName, FctArgs
!
!   Dbg   =   GetOptArgValue(.False.,Debug)
!   if (Dbg) call Logger%Entering( ProcName )
!
!
!   if ( present(Name) ) then
!     IsFunction  =   IsFunction( This%Raw, Name, CaseSensitive=.False. )   ! , NoArg=.True.
!   else
!     IsFunction  =   IsFunction( This%Raw, CaseSensitive=.False. )   ! , NoArg=.True.
!   end if
!
! !   IsFunction  =   .False.
! !
! ! !   if (Dbg) call Logger%Write( "Calling ParseFunction" )
! !   call ParseFunction( This%Raw, FctName, FctArgs, FctSep="(" )
! ! !   if (Dbg) call Logger%Write( "-> FctName  = ", FctName )
! ! !   if (Dbg) call Logger%Write( "-> FctArgs  = ", FctArgs )
! !   if ( len_trim(FctName) == 0 ) then
! ! !     if (Dbg) call Logger%Write( "-> Parameter object not a function-type parameter => Exiting" )
! ! !     if (Dbg) call Logger%Exiting()
! !     return
! !   end if
! !
! !   if ( present(Name) ) then
! !     FctName   =   UpperCase(FctName)
! !     Name_     =   UpperCase(trim(Name))
! !     if ( FctName == Name_ ) IsFunction = .True.
! !   else
! !     IsFunction  = .True.
! !   end if
! !
! ! !   if (Dbg) call Logger%Write( "-> IsFunction = ", IsFunction )
!
!   if (Dbg) call Logger%Exiting()
!
! End Procedure



! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

! @TODO: User procedure from String
! @COMPILER_BUG: gcc-6.3.1: This function need to be called using the "(:)" on the LHS string otherwise there is a ICE: "String(:) = Remove_Quotes(String)"
Pure Function Remove_Quotes( InputString ) result(OutputString)
  character(*)          ,dimension(:)                   ,intent(in)     ::  InputString
  character(len(InputString))   ,dimension(size(InputString))           ::  OutputString
  integer                                                               ::  i
!   character(len(InputString))                                           ::  Value
  character(:)  ,allocatable                                            ::  Value
  character(1)                                                          ::  FirstChar, LastChar
  OutputString      =   InputString
  do i = 1,size(InputString)
    Value           =   trim( InputString(i) )
    if ( len_trim(Value) < 2 ) cycle
    FirstChar       =   Value(1:1)                                                                              ! Getting the first character
    LastChar        =   Value(len(Value):len(Value))                                                            ! Getting the last character
    if ( .Not. ( (FirstChar=='"') .and. (LastChar=='"') ) ) cycle
    Value           =   Value(2:len(Value)-1)
    OutputString(i) =   Value
  end do
End Function

Subroutine Error_Value_Not_In_ValidValues( Param, CallProc )

  use Error_Class                ,only:  Error

  type(InputParameter_Type)                             ,intent(in)     ::  Param
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Value_Not_In_ValidValues"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                                        ! Getting the path of all calling procedures stored in the Logger object
  call Param%Properties%Output( Lines )                                                                         ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  The parameter being processed has a value which does not match any possible values." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  This error can be fixed by either:" )
  call Error%Add_Line( "  * add new valid values to the current parameter" )
  call Error%Add_Line( "  * give this parameter a value which corresponds one of its possible values." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( allocated(Param%ParentSection) )  call Error%Add_Line( "  Parent section:              " // Param%ParentSection )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine


!   @TODO: Improve error message
!   Note that if Param%Properties%Mandatory is false, then the constrain that this parameter is mandatory
!   was applied when calling the GetValue procedure of current parameter.
!   It might be worth mentionning this point in the error message.
Subroutine Error_Undefined_Mandatory_Value( Param, CallProc )

  use Error_Class                ,only:  Error

  type(InputParameter_Type)                             ,intent(in)     ::  Param
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Undefined_Mandatory_Value"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object
  call Param%Properties%Output( Lines )                                                                         ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  The parameter being processed is mandatory but it has not be defined and has no default value." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  Potential solutions for this problem are:" )
  call Error%Add_Line( "  * remove the mandatory attribute from this parameter," )
  call Error%Add_Line( "  * set a default value for this parameter," )
  call Error%Add_Line( "  * specify a value for this parameter in the associated section." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( allocated(Param%ParentSection) )  call Error%Add_Line( "  Parent section:              " // Param%ParentSection )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_ValueNotValid( Param, CallProc )

  use Error_Class                ,only:  Error

  type(InputParameter_Type)                             ,intent(in)     ::  Param
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_ValueNotValid"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath    =   Logger%GetPath(CallProc)
  call Param%Properties%Output( Lines )

  call Error%Set_Title( "Error while processing a parameter" )
  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  The parameter being processed is not within imposed range." )
  call Error%Add_Line( "Details:" )
  if ( present(CallProc) )     call Error%Add_Line( "  Calling procedure:           " // ProcPath )
  if ( allocated(Param%ParentSection) ) call Error%Add_Line( "  Parent section:              " // Param%ParentSection )
  if ( len_trim(ProcPath) /= 0 )        call Error%Add_Line( "  Procedure path:              " // ProcPath )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_Undefined_Mandatory_ValidValues( Param, CallProc )

  use Error_Class                ,only:  Error

  type(InputParameter_Type)                             ,intent(in)     ::  Param
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Undefined_Mandatory_ValidValues"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object
  call Param%Properties%Output( Lines )                                                                         ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  One tries to find the index within the set of possible values of a parameter," )
  call Error%Add_Line( "  but the parameter has no set of possible values." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  Potential solutions for this problem are:" )
  call Error%Add_Line( "  * add a set of possible value to the current parameter," )
  call Error%Add_Line( "  * change the way the parameter is processes." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( allocated(Param%ParentSection) )  call Error%Add_Line( "  Parent section:              " // Param%ParentSection )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_Mandatory_ValidValues_Not_Found( Param, CallProc )

  use Error_Class                ,only:  Error

  type(InputParameter_Type)                             ,intent(in)     ::  Param
  character(*)                                ,optional ,intent(in)     ::  CallProc

  character(*)                                              ,parameter  ::  ProcName = "Error_Mandatory_ValidValues_Not_Found"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines

  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object
  call Param%Properties%Output( Lines )                                                                         ! Extracting the information from the parameter properties object

  call Error%Set_Title( "Error while processing a parameter" )

  call Error%Add_Line( "Description:" )
  call Error%Add_Line( "  You are trying to get the index of the 'possible value' of a parameter which matches a given value." )
  call Error%Add_Line( "  However, the actual parameter value do not match value in the list of 'possible values'." )
  call Error%Add_Line( "  As a result, the index cannot be defined." )

  call Error%Add_Line( "Solutions:" )
  call Error%Add_Line( "  This error is probably due to the fact that the parameter has not been given a value," )
  call Error%Add_Line( "  or has been given a wrong value." )
  call Error%Add_Line( "  This error can also occures if a parameter is not being defined in a section and it has no default values." )
  call Error%Add_Line( "  To solve the problem, make sur that the parameter is defined in the correct section." )
  call Error%Add_Line( "  Also, if current procedure is always going to be call on the current parameter," )
  call Error%Add_Line( "  then it might be a good idea to defined the parameter has mandatory." )
  call Error%Add_Line( "  This will allow to catch sooner the error." )

  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )       call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( allocated(Param%ParentSection) )  call Error%Add_Line( "  Parent section:              " // Param%ParentSection )
  if ( len_trim(ProcPath) /= 0 )          call Error%Add_Line( "  Procedure path:              " // ProcPath )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )

  call Error%Raise( Unit=Logger%GetUnit() )

End Subroutine

Subroutine Error_Converting_String( Param, CallProc )
  use Error_Class                ,only:  Error
  type(InputParameter_Type)                             ,intent(in)     ::  Param
  character(*)                                ,optional ,intent(in)     ::  CallProc
  character(*)                                              ,parameter  ::  ProcName = "Error_Converting_String"
  character(:)  ,allocatable                                            ::  ProcPath
  character(:)  ,allocatable    ,dimension(:)                           ::  Lines
  ProcPath      =   Logger%GetPath()                                                             ! Getting the path of all calling procedures stored in the Logger object
  call Param%Properties%Output( Lines )                                                                         ! Extracting the information from the parameter properties object
  call Error%Set_Title( "Error while converting a character" )
  call Error%Add_Line( "Details:" )
  call Error%Add_Line( "  Procedure raising the error: " // ProcName )
  if ( present(CallProc) )     call Error%Add_Line( "  Calling procedure:           " // CallProc )
  if ( allocated(Param%ParentSection) ) call Error%Add_Line( "  Parent section:              " // Param%ParentSection )
  if ( len_trim(ProcPath) /= 0 )        call Error%Add_Line( "  Procedure path:              " // ProcPath )
  call Error%Add_Line( "  Parameter properties:" )
  call Error%Add_Line( "  * Name = " // Param%Name )
  call Error%Add_Line( "  * Value = " // Param%Value )
  call Error%Add_Line( "  * " // Lines )
  call Error%Raise( Unit=Logger%GetUnit() )
End Subroutine

End SubModule