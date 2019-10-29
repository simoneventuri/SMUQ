! File: inline-GetValue_1d.F90

  logical                                                               ::  Dbg
  logical                                                               ::  Is_Mandatory
  logical                                                               ::  Check_Values
  logical                                                               ::  Defined
!   logical                                                               ::  IsValid
  character(:)  ,dimension(:)   ,allocatable                            ::  List_Values, LeftRightStrings
  character(:)  ,allocatable                                            ::  Value
  character(:)  ,allocatable                                            ::  Separator_                         ! Separation character
  logical                                                               ::  IncreasingOrder_
  logical                                                               ::  DecreasingOrder_
  integer                                                               ::  i, j

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
  if ( present(IncreasingOrder) ) then;     IncreasingOrder_  =   IncreasingOrder
  else;                                     IncreasingOrder_  =   .False.; end if
  if ( present(DecreasingOrder) ) then;     DecreasingOrder_  =   DecreasingOrder
  else;                                     DecreasingOrder_=   .False.; end if
! ==============================================================================================================

  Defined   =   This%Defined  .and. len_trim(This%Value)/=0

! ==============================================================================================================
!    PRINTING INFORMATION ON THE PARAMETER BEING PROCESSED
! ==============================================================================================================
  if (Dbg) then
    call Logger%Write( "This%Defined = ", This%Defined )
    call Logger%Write( "Defined     = ", Defined )
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
  if ( Defined ) then                                                                                      ! If the parameter is defined, then its value can be set (TODO: Check that it is of correct type)
!     if (Dbg) call Logger%Write( "The parameter is defined" )
    Value         =       trim(This%Value)                                                                     ! Copying the string corresponding to the parameter value in a local variable
    if (Dbg) call Logger%Write( "Calling Parse: Value = '"//Value//"' Separator_ = '"//Separator_//"'" )
    call Parse( Value, Separator_, List_Values, IgnoreBetween=['"'] )                                             ! Parsing the string into multiple elements separated by the separation character
    if (Dbg) call Logger%Write( "List_Values = ", List_Values )
!@@@    List_Values   =       Remove_Quotes( List_Values )
    if (Dbg) then
      do i = 1,size(List_Values)
        call Logger%Write( "i = ", i, "List_Values(i) = ", List_Values(i) )
      end do
    end if

    do i = 1,size(List_Values)
      call Parse( List_Values(i), ":", LeftRightStrings, IgnoreBetween=['"'] )                                             ! Parsing the string into multiple elements separated by the separation character
      if (Dbg) then
        call Logger%Write( "size(LeftRightStrings) = ", size(LeftRightStrings) )
        do j = 1,size(LeftRightStrings)
          call Logger%Write( "  j = ", j, "LeftRightStrings(j) = ", LeftRightStrings(j) )
        end do
        call Logger%Write( "  size(LeftRightStrings) = ", size(LeftRightStrings) )
      end if
      if ( allocated(LocalVector) ) deallocate( LocalVector )
      if ( size(LeftRightStrings) >= 2 ) then
        select case ( size(LeftRightStrings) )

          case (2)  ! Case when only the Min and Max values are provided as in 'Max:Min'

            StepValue   =   1.0_8

            call Convert( LeftRightStrings(1), StartValue , Status=Status_ )
            if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
            if ( UpdateStatus(Status,iStat=Status_,Desc=ErrorMessage("1st part"),Proc=ProcName,ExitLogger=Dbg) ) return
            if ( (Status_ /= 0) .and. ( .Not. present(Status) ) ) call Error_Converting_String( This, CallProc )

            call Convert( LeftRightStrings(2), EndValue   , Status=Status_ )
            if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
            if ( UpdateStatus(Status,iStat=Status_,Desc=ErrorMessage("2nd part"),Proc=ProcName,ExitLogger=Dbg) ) return
            if ( (Status_ /= 0) .and. ( .Not. present(Status) ) ) call Error_Converting_String( This, CallProc )


          case (3)  ! Case when the Min, Max and Step values are provided as in 'Max:Min:Step'

            call Convert( LeftRightStrings(1), StartValue , Status=Status_ )
            if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
            if ( UpdateStatus(Status,iStat=Status_,Desc=ErrorMessage("1st part"),Proc=ProcName,ExitLogger=Dbg) ) return
            if ( (Status_ /= 0) .and. ( .Not. present(Status) ) ) call Error_Converting_String( This, CallProc )

            call Convert( LeftRightStrings(2), EndValue   , Status=Status_ )
            if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
            if ( UpdateStatus(Status,iStat=Status_,Desc=ErrorMessage("2nd part"),Proc=ProcName,ExitLogger=Dbg) ) return
            if ( (Status_ /= 0) .and. ( .Not. present(Status) ) ) call Error_Converting_String( This, CallProc )

            call Convert( LeftRightStrings(3), StepValue  , Status=Status_ )
            if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
            if ( UpdateStatus(Status,iStat=Status_,Desc=ErrorMessage("3rd part"),Proc=ProcName,ExitLogger=Dbg) ) return
            if ( (Status_ /= 0) .and. ( .Not. present(Status) ) ) call Error_Converting_String( This, CallProc )

        end select

        call Compute_LinSpace( LocalVector, Min=StartValue, Max=EndValue, Step=StepValue )
        if (Dbg) call Logger%Write( "  LocalVector  = ", LocalVector, Fr="f4.1" )

      else

        call Convert( List_Values(i), StartValue , Status=Status_ )
        if (Dbg) call Logger%Write( "-> Status_ = ", Status_ )
        if ( UpdateStatus(Status,iStat=Status_,Desc=ErrorMessage("default"),Proc=ProcName,ExitLogger=Dbg) ) return
        if ( (Status_ /= 0) .and. ( .Not. present(Status) ) ) call Error_Converting_String( This, CallProc )
        allocate( LocalVector, source = [StartValue] )

      end if
! *****************************************************************************
!       call AddElementToArray( Elements, Array )
      if ( .not. allocated(GlobalVector) ) allocate( GlobalVector(0) )
      allocate( List_Elements, source = [GlobalVector,LocalVector] )
      call move_alloc( List_Elements, GlobalVector )
! *****************************************************************************
      if (Dbg) call Logger%Write( "  GlobalVector = ", GlobalVector, Fr="f4.1" )
    end do

    if ( allocated(Values) ) deallocate(Values)
    allocate( Values, source = GlobalVector )
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
  else
    if (Dbg) call Logger%Write( "The parameter is not defined ... " )
    if (Is_Mandatory) then
      if (Dbg) call Logger%Write( "... and is mandatory => Error" )
      if ( present(Status) ) then
        if ( UpdateStatus(Status,iStat=1,Desc="Mandatory parameter not found",Proc=ProcName,ExitLogger=Dbg) ) return
      else
        call Error_Undefined_Mandatory_Value( This, CallProc )
      end if
    else
      if (Dbg) call Logger%Write( "... but it is not mandatory" )
      if (This%Properties%HasDefaultValue) then                                                               ! If the parameter has a default value
        if (Dbg) call Logger%Write( "... and it has a default value => Setting the parameter value to the default value" )
        Value         =       trim(This%Properties%DefaultValue)
        if (Dbg) call Logger%Write( "Calling Parse: Value = '"//Value//"' Separator_ = '"//Separator_//"'" )
        call Parse( Value, Separator_, List_Values, IgnoreBetween=['"'] )                                         ! Parsing the string into multiple elements separated by the separation character
        if (Dbg) call Logger%Write( "List_Values = ", List_Values )
!@@@        List_Values   =       Remove_Quotes( List_Values )
        if ( allocated(Values) ) deallocate(Values)
!         allocate( Values, source = Convert(List_Values) )                                                      ! Getting its value
        allocate( Values(size(List_Values)) )

        do i = 1,size(List_Values)
          call Convert( List_Values(i), Values(i) , Status=Status_ )
          if ( UpdateStatus(Status,iStat=Status_,Desc=ErrorMessage("default"),Proc=ProcName,ExitLogger=Dbg) ) return
          if ( (Status_ /= 0) .and. ( .Not. present(Status) ) ) call Error_Converting_String( This, CallProc )
        end do

      else                                                                                                      ! If the parameter has no default value
        if (Dbg) call Logger%Write( "... and it has no default value => Do not change the parameter value" )
!         Values   =       Default_Character_Value
      end if                                                                                                    ! End if case on default value
    end if                                                                                                      ! End if case on mandatory parameter
  end if                                                                                                        ! End if case on defined parameter
  if (Dbg) call Logger%Write( "Values = ", Values, Fr="es15.8" )
! ==============================================================================================================


!   call This%Properties%CheckValueValidity( Value, IsValid )
!   if ( .Not. IsValid ) then
!     call Error_ValueNotValid( This, CallProc )
!   end if

! ==============================================================================================================
!    SORTING THE VALUES IF REQUIRED
! ==============================================================================================================
  if (Dbg) then
    call Logger%Write( "Sorting the values if required" )
    call Logger%Write( "-> IncreasingOrder_ = ", IncreasingOrder_ )
    call Logger%Write( "-> DecreasingOrder_ = ", DecreasingOrder_ )
  end if
  if ( IncreasingOrder_ ) then
    if (Dbg) call Logger%Write( "-> Calling Insertion_Sort with Reverse=.False." )
    call Insertion_Sort( Values, Reverse=.False. )
    if (Dbg) call Logger%Write( "-> Values = ", Values, Fr="es15.8" )
  end if
  if ( DecreasingOrder_ ) then
    if (Dbg) call Logger%Write( "-> Calling Insertion_Sort with Reverse=.True." )
    call Insertion_Sort( Values, Reverse=.True. )
    if (Dbg) call Logger%Write( "-> Values = ", Values, Fr="es15.8" )
  end if
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

  contains

Pure Function ErrorMessage(Str) result(ErrMsg)
  character(*)                                          ,intent(in)     ::  Str
  character(:)  ,allocatable                                            ::  ErrMsg
  ErrMsg  =   "Error converting variable from 'character' to '"//VarType//"' when extracting "//Str//" of value '"//List_Values(i)//"'from parameter '"//This%Name//"'."
End Function



