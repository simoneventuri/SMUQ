SubModule(String_Module) String_Parsing_SubModule

! Include file needed for
! * CONCAT (inside 'ParseNamesValues_Implementation_Inlined.F90')
! * _ASSIGN_ALLOCATABLE_CHARACTER_' in Parse
# include "forpack-include.inc"
# include "forpack-include.inc"

  implicit none

  contains

! This procedure returns the number of items contained in an string.
! The items are separated by a specific separator character which can
! be specified using the optional argument 'Separator'.
! By default, the separator character is a space.
! Example:
!   Procedure call                                      Results
!   --------------                                      -------
!   Nitem = GetNumberOfItems( "An example" )            4
!   Nitem = GetNumberOfItems( "SingleWorld" )           1
!   Nitem = GetNumberOfItems( "a    b" )                2
!   Nitem = GetNumberOfItems( "a;b;c", Separator=";" )  3
Module Procedure GetNumberOfItems
  character(:)  ,allocatable                                            ::  Strings(:)
  character(:)  ,allocatable                                            ::  Separator_
  Separator_  =   " "
  if ( present(Separator) ) Separator_  = Separator
  call Parse( String, Separator_, Strings )
  NItems      =   size(Strings)
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!       PROCEDURES FOR PARSING A STRING INTO NUMBER AND LETTERS
! **************************************************************************************************************
! **************************************************************************************************************

! This procedure extracts all characters from a string which are on the right
! of the first set of letters.  If the input string starts with a letter, then
! the ouput and input string are identical. If the input string has only
! numbers, then the ouput is an empty string.
! Example:
!   Procedure call                                  Results
!   --------------                                  -------
!   Letters = GetLettersRightOfNumbers( "N2"   )     "N2"
!   Letters = GetLettersRightOfNumbers( "154"  )     ""
!   Letters = GetLettersRightOfNumbers( "15N2" )     "N2"
Module Procedure GetLettersRightOfNumbers_0d

  logical                                                               ::  DotFound
  integer                                                               ::  i, j, k
  integer                                                               ::  Length
  character(1)                                                          ::  s
  character(:)  ,allocatable                                            ::  String

  DotFound  =   .False.
  String    =   NumbersLetters
  Length    =   len_trim(String)
  i         =   0
  k         =   0

  do
    k       =   k + 1
    if ( k > Length ) exit
    s       =   String(k:k)

!   If current character is a digit, then loop on all remaining characters
!   until a non-digit is found. If the last characterof the string is a
!   digit, then the if case ( j > Length ) is entered and we set k to a
!   number just above the size of the string so taht Letter will be en
!   empty string
    if ( Is_Digit(s) ) then
      j    =   k
      do
        j   =   j + 1
        if ( j > Length ) then
          k =   j
          exit
        end if
        s   =   String(j:j)
        if ( .Not. Is_Digit(s) ) exit
        k   =   j
      end do
      i     =   k
      cycle
    end if

!   If a '.' is found, then notify that it has been found and cycle to the
!   next character. If another '.' is found, then this character will be the
!   start of 'Letters' since a number cannot have 2 '.'.
    if ( s == "." ) then
      if (DotFound) then
        i   =   k
        exit
      end if
      DotFound = .True.
      cycle
    end if

!   If current character correspond to an exponent character {e,E,d,D}, then
!   add the i the lasty index of the exponent sub string. This is given by
!   the function 'GetFinalIndexExponentPart' (0 is there is no exponenet spec.)
    if ( IsExponentCharacter( s ) ) then
      i   =   k + GetFinalIndexExponentPart( String(k:) )
      exit
    end if

    i     =   k
    if ( Is_Letter(s) ) exit

  end do

  Letters       =   trim( String(i:) )

End Procedure

! This procedure extracts all characters from a string which are on the right
! of the first set of letters.  If the input string starts with a letter, then
! the ouput and input string are identical. If the input string has only
! numbers, then the ouput is an empty string. This procedure do the same
! than the procedure 'GetLettersRightOfNumbers_0d' but it operates on rank-1
! arrays.
! Example:
!   Procedure call                                  Results
!   --------------                                  -------
!   Letters = GetLettersRightOfNumbers( ["N2","O2"] )       ["N2","O2"]
!   Letters = GetLettersRightOfNumbers( ["154","1"] )       ["154","1"]
!   Letters = GetLettersRightOfNumbers( ["15N2","12O2"] )   ["N2","O2"]
Module Procedure GetLettersRightOfNumbers_1d
  integer                                                               ::  i, k
  integer                                                               ::  Length                          ! Length of the output string
  integer                                                               ::  LenLoc                          ! Local length a element
  integer                                                               ::  NElements                       ! Number of elements in the input string
  character(len(NumbersLetters))  ,dimension(size(NumbersLetters))      ::  LocalLetters
  character(:)  ,allocatable                                            ::  String
  character(:)  ,allocatable                                            ::  String_Letters
  character(:)  ,allocatable                                            ::  String_Numbers
  NElements             =   size(NumbersLetters)
  Length                =   0
  do i = 1,NElements
    String              =   NumbersLetters(i)
    LenLoc              =   len_trim(String)
    do k = 1,LenLoc
      if ( Is_Letter(String(k:k)) ) exit
    end do
    if ( k == 1 ) then
      String_Numbers    =   ""
      String_Letters    =   trim(String)
    else
      String_Numbers    =   String(1:k-1)
      String_Letters    =   String(k:LenLoc)
    end if
    Length              =   max( Length, len_trim(String_Letters) )
    LocalLetters(i)     =   trim(String_Letters)
  end do
# ifdef WORKAROUND_GCC_ALLOCATABLE_OUTPUT_CHARACTER_ARRAY_IN_FUNCTION
  Block
    character(:) ,allocatable   :: WASTR(:)
    allocate( WASTR, source = VecTrim(LocalLetters) )
    call move_alloc( WASTR, Letters )
  End Block
# else
  allocate( Letters, source = VecTrim(LocalLetters) )   ! @COMPILER_BUG: gcc-8.2.0: ICE
# endif
End Procedure


! This procedure extracts all characters from a string which corresponds to
! number on the left of the first set of letters.
! Example:
Module Procedure GetNumbersLeftOfLetters_0d
  use Utilities_Library    ,only:  GetOptArgValue
  character(*)                                              ,parameter  ::  DefaultNumber="1"
  integer                                                               ::  i
  Numbers   =   GetOptArgValue(DefaultNumber,Default)
  i         =   0
  do
    i       =   i + 1
    if ( i > len_trim(NumbersLetters)   ) exit
    if ( Is_Letter(NumbersLetters(i:i)) ) exit
  end do
  if ( i /= 1 ) Numbers = NumbersLetters(1:i-1)
End Procedure

Module Procedure GetNumbersLeftOfLetters_1d
  integer                                                               ::  i
  character(len(NumbersLetters))  ,dimension(size(NumbersLetters))      ::  LocalNumbers
  do i = 1,size(NumbersLetters)
    LocalNumbers(i)   =   GetNumbersLeftOfLetters( trim(NumbersLetters(i)), Default )
  end do
# ifdef WORKAROUND_GCC_ALLOCATABLE_OUTPUT_CHARACTER_ARRAY_IN_FUNCTION
  Block
    character(:) ,allocatable   :: WASTR(:)
    allocate( WASTR, source = VecTrim(LocalNumbers) )
    call move_alloc( WASTR, Numbers )
  End Block
# else
  allocate( Numbers, source = VecTrim(LocalNumbers) )   ! @COMPILER_BUG: gcc-8.2.0: ICE
# endif
End Procedure

Module Procedure ParseNumbersLetters_CHAR_0d
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Ci, Cii
  i         =   0
  do
    i       =   i + 1
    if ( i > len_trim(NumbersLetters)   ) exit
    Ci  =   NumbersLetters(i:i)
    if ( Is_Letter(Ci) ) then
      Ci = UpperCase(Ci)
      if ( (Ci=="E") .or. (Ci=="D") ) then
        Cii = NumbersLetters(i+1:i+1)
        if ( Is_Digit(Cii) .or. (Cii=="+")  .or. (Cii=="-") ) then    ! After a E of D we can have a num, +, -:  E+00, E-00, E00
          i       =   i + 1
          cycle
        else
          exit
        end if
      else
        exit
      end if
    end if
  end do
  Numbers   =   trim( adjustl(NumbersLetters(1:i-1)) )
  Letters   =   trim( adjustl(NumbersLetters(i:)) )
End Procedure

Module Procedure ParseNumbersLetters_INT8_0d
  character(:)  ,allocatable                                            ::  Numbers_
  call ParseNumbersLetters( NumbersLetters, Numbers_, Letters )
  call Convert( Numbers_, Numbers )
End Procedure

Module Procedure ParseNumbersLetters_INT16_0d
  character(:)  ,allocatable                                            ::  Numbers_
  call ParseNumbersLetters( NumbersLetters, Numbers_, Letters )
  call Convert( Numbers_, Numbers )
End Procedure

Module Procedure ParseNumbersLetters_INT32_0d
  character(:)  ,allocatable                                            ::  Numbers_
  call ParseNumbersLetters( NumbersLetters, Numbers_, Letters )
  call Convert( Numbers_, Numbers )
End Procedure

Module Procedure ParseNumbersLetters_INT64_0d
  character(:)  ,allocatable                                            ::  Numbers_
  call ParseNumbersLetters( NumbersLetters, Numbers_, Letters )
  call Convert( Numbers_, Numbers )
End Procedure

Module Procedure ParseNumbersLetters_REAL32_0d
  character(:)  ,allocatable                                            ::  Numbers_
  call ParseNumbersLetters( NumbersLetters, Numbers_, Letters )
  call Convert( Numbers_, Numbers )
End Procedure

Module Procedure ParseNumbersLetters_REAL64_0d
  character(:)  ,allocatable                                            ::  Numbers_
  call ParseNumbersLetters( NumbersLetters, Numbers_, Letters )
  call Convert( Numbers_, Numbers )
End Procedure

Module Procedure ParseNumbersLetters_REAL128_0d
  character(:)  ,allocatable                                            ::  Numbers_
  call ParseNumbersLetters( NumbersLetters, Numbers_, Letters )
  call Convert( Numbers_, Numbers )
End Procedure

Module Procedure GetNumberLetterPairs_CHAR
  use Utilities_Library         ,only:  AddElementToArray

  integer                                                               ::  i, j, k
  character(:)  ,allocatable                                            ::  Ci
  character(:)  ,allocatable                                            ::  Num, Str
  character(:)  ,allocatable                                            ::  Remaining

  Remaining   =   NumbersLetters

  !call Logger%Write( "NumbersLetters = ", NumbersLetters )
  k     =   0
  do
    k   =   k + 1
    if ( len_trim(Remaining) <= 0   ) exit
    !call Logger%Write( "-> k = ", k, "Remaining = ", Remaining )

!   Finding the final index of the letters: i
    i       =   0
    do
      i     =   i + 1
      if ( i > len_trim(Remaining) ) exit
      Ci    =   Remaining(i:i)
      if ( .Not. Is_Letter(Ci) ) cycle
      Ci    =   UpperCase(Ci)
!       if ( (Ci/="E") .and. (Ci/="D") ) exit
      if ( (Ci/="E") ) exit
      Ci    =   Remaining(i+1:i+1)
      if ( Is_Digit(Ci) .or. (Ci=="+")  .or. (Ci=="-") ) then    ! After a E of D we can have a num, +, -:  E+00, E-00, E00
        i   =   i + 1
        cycle
      end if
      exit
    end do
    i       =   i - 1
    Num     =   trim( adjustl(Remaining(1:i)) )
    !call Logger%Write( "  -> i = ", i, "Num = ", Num )

    if ( i >= len_trim(Remaining) ) then
      Remaining =   ""
      Str   =   ""
    else

  !   Finding the final index of the string: j
      Remaining =   trim( adjustl(Remaining(i+1:)) )
      j         =   0
      do
        j       =   j + 1
        if ( j > len_trim(Remaining)   ) exit
        Ci  =   Remaining(j:j)
        if ( .Not. Is_Letter(Ci) ) exit
      end do
      j = j - 1
      Str       =   trim( adjustl(Remaining(1:j) ))
      Remaining =   trim( adjustl(Remaining(j+1:)) )
      !call Logger%Write( "  -> j = ", j, "Str = ", Str )

    end if

    call AddElementToArray( Num, Numbers )
    call AddElementToArray( Str, Letters )

  end do

End Procedure

Module Procedure GetNumberLetterPairs_REAL64
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  NumbersStr(:)
  call GetNumberLetterPairs_CHAR( String, NumbersStr, Letters )
  allocate( Numbers(size(NumbersStr)) )
  do i = 1,size(NumbersStr)
    call Convert( NumbersStr(i), Numbers(i) )
  end do
End Procedure




! **************************************************************************************************************
! **************************************************************************************************************
!       PROCEDURES FOR PARSING A STRING SPECIFIED AS A FUNCTION
! **************************************************************************************************************
! **************************************************************************************************************

! This procedure returns whether the input character string corresponds to a function specification.
! In order to be a function specification, the string must have the following structure:
!       String = "functionName(...)"
! !@TODO: Add pure
Module Procedure IsFunction_NoName
  character(:)  ,allocatable                                            ::  FctName, FctArgument
  call ParseFunction( String, FctName, FctArgument, FctSep )
  IsFct   =   FctName /= ""
End Procedure

Module Procedure IsFunction_Name0d
  character(:)  ,allocatable                                            ::  FctName, FctArgument
  call ParseFunction( String, FctName, FctArgument, FctSep )
  IsFct     =   Equal( FctName, Name, CaseSensitive=CaseSensitive )
End Procedure

Module Procedure IsFunction_Name1d
  use Utilities_Library   ,only:  IsIncluded
  character(:)  ,allocatable                                            ::  FctName, FctArgument
  call ParseFunction( String, FctName, FctArgument, FctSep )
  IsFct     =   IsIncluded(FctName,Names,CaseSensitive=CaseSensitive)
End Procedure

Module Procedure GetFunctionName
  character(:)  ,allocatable                                            ::  Argument
  call ParseFunction( String, Name, Argument, FctSep )
End Procedure

! This procedure parses a string which corresponds to a function specification into
! the function name and its list of arguments.
! If the input string 'String' has then following format
!       String = "Name(A=1,B="test",C=78)"
! then the two arguments returned from this procedure will be:
!       Name      = "Name"
!       Argument  = "A=1,B="test",C=78"
Module Procedure ParseFunctionNameArgument_0d
  use Utilities_Library    ,only:  GetOptArgValue
  character(*)                                              ,parameter  ::  DefaultFctSep="("
  integer                                                               ::  Length
  integer       ,dimension(2)                                           ::  Indexes
  character(:)  ,allocatable                                            ::  Left, Right
  character(:)  ,allocatable                                            ::  LeftRight(:)
  Left      =   GetOptArgValue(DefaultFctSep,FctSep)
  Right     =   GetAssociatedString( Left )
  Length    =   max( len_trim(Left) , len_trim(Right) )
  LeftRight =   [ Character(Length) :: Left,Right ]
  Indexes   =   GetSubStringIndexes( String , LeftRight )
  if ( any( Indexes == 0 ) ) then
    Name      =   trim(String)
    Argument  =   ""
  else
    Name      =   String(1:Indexes(1)-2)
    Argument  =   trim( adjustl( String(Indexes(1):Indexes(2)) ) )
  end if
End Procedure


! This procedure parses a string which corresponds to a function specification into
! the function name and its list of arguments.
! If the input string 'String' has then following format
!       String = "Name(A=1,B="test",C=78)"
! then the two arguments returned from this procedure will be:
!       Name      = "Name"
!       Argument(1)    = "A=1"
!       Argument(2)    = "B=test"
!       Argument(3)    = "C=78"
Module Procedure ParseFunctionNameArgument_1d
  use Utilities_Library          ,only:  GetOptArgValue
  character(*)                                              ,parameter  ::  DefaultArgSep=","
  character(:)  ,allocatable                                            ::  ArgSep_
  character(:)  ,allocatable                                            ::  Arg
  ArgSep_   =   GetOptArgValue(DefaultArgSep,ArgSep)
  call ParseFunction( String, Name, Arg, FctSep=FctSep )
!   write(*,*) '    [ParseFunctionNameArgument_1d] allocated(Arg) = ', allocated(Arg)
!   write(*,*) '    [ParseFunctionNameArgument_1d] size(Arg) = ', size(Arg)
!   write(*,*) '    [ParseFunctionNameArgument_1d] Arg = ', Arg
  if ( len_trim(Arg) == 0 ) then
    call EmptyString(Argument)
  else
!     write(*,*) '    [ParseFunctionNameArgument_1d] Calling Parse'
    call Parse( Arg, ArgSep_, Argument, IgnoreBetween=['"'] )
!     write(*,*) '    [ParseFunctionNameArgument_1d] allocated(Argument) = ', allocated(Argument)
!     write(*,*) '    [ParseFunctionNameArgument_1d] size(Argument) = ', size(Argument)
!     write(*,*) '    [ParseFunctionNameArgument_1d] Argument = ', Argument
  end if
!   write(*,*) '    [ParseFunctionNameArgument_1d] allocated(Argument) = ', allocated(Argument)
!   write(*,*) '    [ParseFunctionNameArgument_1d] size(Argument) = ', size(Argument)
!   write(*,*) '    [ParseFunctionNameArgument_1d] Exiting'
End Procedure

! This procedure parses a string which corresponds to a function specification into
! the function name and its list of arguments.
! If the input string 'String' has then following format
!       String = "Name(A=1;B="test",C=78)"
! then the two arguments returned from this procedure will be:
!       Name      = "Name"
!       Argument(1,:)  = ["A","1"]
!       Argument(2,:)  = ["B","test"]
!       Argument(3,:)  = ["C","78"]
! The dimension of the variable 'Argument' are:
! - 1st rank: number arguments (3 here)
! - 2nd rank: always 2: 1->name, 2->value
Module Procedure ParseFunctionNameArgument_2d
  use Utilities_Library          ,only:  GetOptArgValue, AddElementToArray, PresentAndTrue
  character(*)                                              ,parameter  ::  DefaultValSep="="
  logical                                                               ::  DefToVal_
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  ValSep_
  character(:)  ,allocatable                                            ::  Arguments(:), TmpString(:)
  character(:)  ,allocatable                                            ::  ArgName, ArgValue
  character(:)  ,allocatable                                            ::  NameValue(:)
  DefToVal_  =   PresentAndTrue(DefToVal)
  ValSep_         =   GetOptArgValue(DefaultValSep,ValSep)
  call ParseFunction( String, Name, Arguments, FctSep=FctSep, ArgSep=ArgSep )
  do i = 1,size(Arguments)
    call Parse( Arguments(i), ValSep_, TmpString, IgnoreBetween=['"'] )
    ArgName   =   TmpString(1)
    ArgValue  =   ""
    if ( size(TmpString) > 1 ) ArgValue = TmpString(2)
    ArgName   =   RemoveQuotes(ArgName)
    ArgValue  =   RemoveQuotes(ArgValue)
    if ( DefToVal_ .and. ( len_trim(ArgValue) == 0) ) call Swap( ArgName, ArgValue )
    call AddElementToArray( ArgName , NameValue )
    call AddElementToArray( ArgValue, NameValue )
!     call AddElementToArray( NameValue, Argument, Dim=1 )
    call AddElementToArray( NameValue, Argument, Dim=2 )
    deallocate(NameValue)
  end do
End Procedure

! If the function is declared with an argument which does not have any
! associated value, as in 'f(5)', then the default behavior is to fill
! the argument value (with '5' here) and to keep the name empty. This
! behavior can be changed using the 'DefToVal' optional argument
! which has the default value 'DefToVal=.True.'. We have the
! following behavior:
! * DefToVal=.True.   =>  Empty name and filled value(default)
! * DefToVal=.False.  =>  Filled name and empty value

! |-------------------------------------|---------|--------|
! | String                              | Name    | Value  |
! |-------------------------------------|---------|--------|
! | 'f(x)'                              | ''      | 'x'    |
! | 'f(x,DefToVal=.True.)'              | ''      | 'x'    |
! | 'f(x,DefToVal=.False.)'             | 'x'     | ''     |
! |-------------------------------------|---------|--------|

Module Procedure ParseFunctionNameArgument_1d_1d

  use Utilities_Library          ,only:  GetOptArgValue, AddElementToArray
!   use Logger_Class    ,only: Logger

  character(*)                                              ,parameter  ::  DefaultValSep="="
  logical                                                   ,parameter  ::  DefaultDefToVal=.True.
  logical                                                               ::  DefToVal_, OneItem
  integer                                                               ::  i, j, N
  character(:)  ,allocatable                                            ::  ValSep_, ArgName, ArgValue, DefArgName
  character(:)  ,allocatable                                            ::  Arguments(:), NameValue(:)

!   write(*,*) '  [ParseFunctionNameArgument_1d_1d] Entering'
!   call Logger%Write( "Processing optional argument")
  DefToVal_   =   GetOptArgValue( DefaultDefToVal , DefToVal )
  ValSep_     =   GetOptArgValue( DefaultValSep   , ValSep   )
!   call Logger%Write( "-> present(DefArgNames) = ", present(DefArgNames) )
!   call Logger%Write( "-> DefToVal_    = ", DefToVal_  )
!   call Logger%Write( "-> ValSep_      = ", ValSep_    )

!   call Logger%Write( "Calling ParseFunction")
!   write(*,*) '  [ParseFunctionNameArgument_1d_1d] Calling ParseFunction'
  call ParseFunction( String, Name, Arguments, FctSep=FctSep, ArgSep=ArgSep )
!   write(*,*) '  [ParseFunctionNameArgument_1d_1d] allocated(Arguments) = ', allocated(Arguments)

  if ( size(Arguments) == 0 ) then
    allocate( character(0) :: ArgNames(0) )
    allocate( character(0) :: ArgValues(0) )
!     call Emptystring(ArgNames)
!     call Emptystring(ArgValues)
  else

!     write(*,*) '  [ParseFunctionNameArgument_1d_1d] size(Arguments) = ', size(Arguments)
    do i = 1,size(Arguments)
  !     call Logger%Write( "-> i = ", i, "Arguments(i)= ", Arguments(i) )
!       write(*,*) '  [ParseFunctionNameArgument_1d_1d] i = ', i
!       write(*,*) '  [ParseFunctionNameArgument_1d_1d] Calling Parse: Arguments(i) = ', Arguments(i)
      call Parse( Arguments(i), ValSep_, NameValue, IgnoreBetween=['"'] )
!       write(*,*) '  [ParseFunctionNameArgument_1d_1d] size(NameValue) = ', size(NameValue)
!       write(*,*) '  [ParseFunctionNameArgument_1d_1d] NameValue = ', NameValue

      N             =   size(NameValue)
      if ( N == 1 ) then
        OneItem     =   .True.
        if ( DefToVal_ ) then ! Default behavior
          ArgName   =   ""
          ArgValue  =   NameValue(1)
        else
          ArgName   =   NameValue(1)
          ArgValue  =   ""
        end if
      else if ( N == 2 ) then
        OneItem     =   .False.
        ArgName     =   NameValue(1)
        ArgValue    =   NameValue(2)
      end if
      ArgName       =   RemoveQuotes(ArgName)
      ArgValue      =   RemoveQuotes(ArgValue)
!       write(*,*) '  [ParseFunctionNameArgument_1d_1d] ArgName = ', ArgName
!       write(*,*) '  [ParseFunctionNameArgument_1d_1d] ArgValue = ', ArgValue
  !     call Logger%Write( "  -> ArgName = ", ArgName, "ArgValue = ", ArgValue )

  !     call Logger%Write(   "  -> OneItem = ", OneItem )
      if ( OneItem .and. present(DefArgNames) ) then
        if ( i <= size(DefArgNames) ) then
          DefArgName  =   trim(DefArgNames(i))
  !         call Logger%Write( "  -> DefArgName = ", DefArgName )
  !         call Logger%Write( "  -> ArgName = ", ArgName, "ArgValue = ", ArgValue )
          if ( DefToVal_ ) then
            if ( len_trim(ArgName) == 0 ) ArgName = DefArgName
          else
            if ( ArgName /= DefArgName ) ArgValue = ArgName
            ArgName     =   DefArgName
          end if
  !         call Logger%Write(   "-> Defaulting: i = ", i, "ArgName = ", ArgName, "ArgValue = ", ArgValue )
      end if
      end if

      call AddElementToArray( ArgName , ArgNames  )
      call AddElementToArray( ArgValue, ArgValues )

    end do
  end if


!   write(*,*) '  [ParseFunctionNameArgument_1d_1d] allocated(ArgNames)  = ', allocated(ArgNames)
!   write(*,*) '  [ParseFunctionNameArgument_1d_1d] allocated(ArgValues) = ', allocated(ArgValues)
!
!   write(*,*) '  [ParseFunctionNameArgument_1d_1d] Exiting'

End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!       PROCEDURES FOR PARSING A STRING INTO PARTS
! **************************************************************************************************************
! **************************************************************************************************************

! This procedure parses a scalar character string into several sub-string based on a given
! separation character. Character can be escaped by providing the optional input argument.
Module Procedure Parse
  character(:)  ,allocatable                                            ::  String
  character(:)  ,allocatable                                            ::  LHS, RHS
  character(:)  ,allocatable                                            ::  Copy(:)
  integer                                                               ::  Length
  String        =   Compact(Input)                                                                   ! Storing local value of the input character string and compacting it
  if ( len_trim(String) == 0 ) then
    allocate( Output, source = [String] )
    return                                                                                                      ! If empty input string, then exiting the procedure
  end if
  do                                                                                                            ! Infinite loop on all characters of the input string
    if ( len_trim(String)==0 ) exit                                                                             ! If the current local character string is empty, then exiting the procedure
    call Split( String, LHS, Separator, RHS, EscRHS=EscRHS, IgnoreBetween=IgnoreBetween )                     ! Spliting the string into a left and right string separated by the Separator character
    String      =   RHS                                                                                         ! Setting the remaining string to be proceed to the RHS string
    if ( .not. allocated(Output) ) then                                                                          ! If un-allocated output Output variable, then the first sub-string to being proceeded
      allocate( Output, source = [ LHS ] )                                                                      ! And so allocating the local list of string to unity and storing only the current value (since no previous values exists)
    else                                                                                                        ! If allocated output Output variable, then sub-strings have already been proceeded
      Length    =   max(len(Output),len(LHS))
#     ifdef WORKAROUND_GFORTRAN_DIFFERENT_CHARACTER_LENGTHS_ARRAY_CONSTRUCTOR
        Block
          integer :: i
          allocate( Character(Length) :: Copy(size(Output)+1) )
          do i = 1,size(Output)
            Copy(i) = Output(i)
          end do
          Copy(size(Output)+1)  =   LHS
!           call Logger%Write( "[ character(Length) :: Output, LHS ]   = ", [ character(Length) :: Output, LHS ] )
        End Block
#     else
      allocate( Copy, source = [ character(Length) :: Output, LHS ] )   ! GCC-9.1.0: Fortran runtime error: Different CHARACTER lengths (0/1) in array constructor
#     endif
      call move_alloc( Copy, Output )                                                                           ! Transfering allocation from temporary to final variable
    end if                                                                                                      ! End if cas on allocation status
  end do                                                                                                        ! End of loop

# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
    _ASSIGN_ALLOCATABLE_CHARACTER_(Output,VecTrim(Output))
# else
  Output    =   VecTrim(Output)
# endif
End Procedure

!! This procedure splits a string into a LHS and A RHS part separated by a given string. \n
!! The first instance of a character from separation string \c Separator is located in the input string \c RHS. \n
!! The characters before the found delimiter are output in \c LHS, it is the Left-Hand Side string. \n
!! The characters after  the found delimiter are output in \c RHS, it is the Right-Hand-Side string. \n
!! The optional input strings contain the LHS and RHS escape characters which can be used for escaping
!! characters.
!! @TODO: It maigh be a good idea to invert the arguments !!!
Module Procedure Split
  logical                                                               ::  i_Debug_Loc
  logical                                                               ::  i_EscLHS                        ! Escape character indicator
  logical                                                               ::  i_EscRHS                        ! Escape character indicator
  logical                                                               ::  i_protect                       ! Character protection indicator
  logical                                                               ::  i_Length_Eq_Inp_Local
  logical                                                               ::  i_IgnoreBetween
!   character(:)  ,allocatable    ,dimension(:)                           ::  Char_IgnoreBetween
  integer                                                               ::  iStrInp                         ! Index of the input string
  integer                                                               ::  iRHS                            ! Index of the RHS string
  integer                                                               ::  iLHS                            ! Index of the LHS string
  integer                                                               ::  iSepIni                         ! Initial index of the separation character in the input string
  integer                                                               ::  iSepFin                         ! Final index of the separation character in the input string
  character(1)                                                          ::  Char1                           ! String single character
  character(1)                                                          ::  Char1_Next                      ! Next character
  character(len=len_trim(Input))                                 ::  StrInp                          ! Local copy of the input string
  integer                                                               ::  iEscRHS                         ! Index of element in the array of RHS escape character

  character(:)  ,allocatable                                            ::  String
  character(:)  ,allocatable                                            ::  Ignore_CharIni                  ! Initial character from which the separation character should be ignored
  character(:)  ,allocatable                                            ::  Ignore_CharFin                  ! final character   to   which the separation character should be ignored

  integer                                                               ::  NIgnores
  integer                                                               ::  Position_Ignore_CharIni
  integer                                                               ::  Position_Ignore_CharFin
  integer                                                               ::  i

  i_Debug_Loc = .False.; if ( present(i_Debug) ) i_Debug_Loc = i_Debug
!   i_Debug_Loc   =   .True.
!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Entering')")
!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: String to be splitted: ',a)") trim(Input)
!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Separation character:  ',a)") trim(Separator)


  RHS   =   Input

! ==============================================================================================================
!       PROCESSING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  i_EscRHS              =   .False.                                                                         ! Initialization of the RHS escape character indicator
  i_EscLHS              =   .False.                                                                         ! Initialization of the LHS escape character indicator
  i_Length_Eq_Inp_Local =   .False.
  i_IgnoreBetween      =   .False.
  if ( present(EscRHS) )                i_EscRHS = .True.                                                       ! If a RHS escape character is provided, the activating the RHS escape indicator
  if ( present(EscLHS) )                i_EscLHS = .True.                                                       ! If a LHS escape character is provided, the activating the LHS escape indicator
  if ( present(i_Length_Eq_Inp) )       i_Length_Eq_Inp_Local = i_Length_Eq_Inp
  if ( present(IgnoreBetween) )        i_IgnoreBetween = .True.
! !  if (i_Debug_Loc) then
!     write(LogUnit,"(12x,'[Split]: Dealing with optional input arguments')")
!     write(LogUnit,"(12x,'[Split]:  -> i_EscRHS              = ',g0)") i_EscRHS
!     write(LogUnit,"(12x,'[Split]:  -> i_EscLHS              = ',g0)") i_EscLHS
!     write(LogUnit,"(12x,'[Split]:  -> i_Length_Eq_Inp_Local = ',g0)") i_Length_Eq_Inp_Local
!     write(LogUnit,"(12x,'[Split]:  -> i_IgnoreBetween      = ',g0)") i_IgnoreBetween
!   end if
! ==============================================================================================================




  if ( i_EscRHS ) RHS = RemoveSpace( adjustl(RHS) )                                                          ! Removing blanks and adjusting the to LHS


  allocate( character(len(RHS)) :: LHS )
  LHS(:)        =   ''                                                                                   ! Initialization of the LHS character string
  iSepIni   =   index( RHS, Separator )                                                                 ! Index of initial position of the separation character in the input character
  iSepFin   =   iSepIni + len_trim(Separator) - 1                                                      ! Index of final   position of the separation character in the input character
!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: iSepIni = ',i0)") iSepIni
!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: iSepFin = ',i0)") iSepFin


  if ( i_IgnoreBetween ) then
  !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Ignoring the separation character in-between some given characters')")
  !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: size(IgnoreBetween) = ',g0)") size(IgnoreBetween)
  !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: IgnoreBetween) = ',*(a,3x))") IgnoreBetween

    do NIgnores = 1,size(IgnoreBetween)

        Ignore_CharIni    =   IgnoreBetween(NIgnores)
        Ignore_CharFin    =   GetAssociatedString(Ignore_CharIni)
    !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Ignore_CharIni = ',g0)") Ignore_CharIni
    !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Ignore_CharFin = ',g0)") Ignore_CharFin

  !   This only works for ignore characters of 1 character
      Position_Ignore_CharIni     =   0
      Position_Ignore_CharFin     =   0
      do i = 1,len_trim(RHS)
        Char1     =   RHS(i:i)
        if ( (Position_Ignore_CharIni == 0) .and.  (Char1 == Ignore_CharIni) ) Position_Ignore_CharIni = i
        if ( (Position_Ignore_CharFin == 0) .and.  (Char1 == Ignore_CharFin) .and. (Position_Ignore_CharIni/=i) ) Position_Ignore_CharFin = i
      end do
    !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position of Ignore_CharIni in the string: Position_Ignore_CharIni = ',g0)") Position_Ignore_CharIni
    !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position of Ignore_CharFin in the string: Position_Ignore_CharFin = ',g0)") Position_Ignore_CharFin
      if ( ( Position_Ignore_CharIni < iSepIni ) .and. ( iSepIni < Position_Ignore_CharFin ) ) then
      !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: The separation character is inside the ignore set of characters => Finding the next splitting character')")

        String            =   RHS(Position_Ignore_CharFin+1:)
      !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: New string where to find the position of the separation character: String = ',g0)") String

        iSepIni   =   index( String, Separator )                                                                  ! Index of initial position of the separation character in the input character
        iSepFin   =   iSepIni + len_trim(Separator) - 1                                                      ! Index of final   position of the separation character in the input character

      !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position in String: iSepIni = ',i0)") iSepIni
      !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position in String: iSepFin = ',i0)") iSepFin

  !       if iSepIni == 0 , then the separation character is not present in the string (outside the ignore character)
  !       So the zero value is kept so that the procedure is exited below.
        if ( iSepIni /= 0 ) then
          iSepIni   =   iSepIni + (Position_Ignore_CharFin)
          iSepFin   =   iSepIni + len_trim(Separator) - 1                                                      ! Index of final   position of the separation character in the input character
        end if


      !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Original string where to find the position of the separation character: RHS = ',g0)") RHS
      !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position in RHS: iSepIni = ',i0)") iSepIni
      !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position in RHS: iSepFin = ',i0)") iSepFin

  !     !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: String = ',g0)") RHS()
      end if


    end do

      !     if ( size(IgnoreBetween) == 1 ) then
      !       Ignore_CharIni    =   IgnoreBetween(1)
      ! !       Ignore_CharFin    =   IgnoreBetween(1)
      !       Ignore_CharFin    =   GetAssociatedString(Ignore_CharIni)
      !     else if ( size(IgnoreBetween) >= 2 ) then
      !       Ignore_CharIni    =   IgnoreBetween(1)
      !       Ignore_CharFin    =   IgnoreBetween(2)
      !     end if
      !   !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Ignore_CharIni = ',g0)") Ignore_CharIni
      !   !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Ignore_CharFin = ',g0)") Ignore_CharFin
      !
      ! !   This only works for ignore characters of 1 character
      !     Position_Ignore_CharIni     =   0
      !     Position_Ignore_CharFin     =   0
      !     do i = 1,len_trim(RHS)
      !       Char1     =   RHS(i:i)
      !       if ( (Position_Ignore_CharIni == 0) .and.  (Char1 == Ignore_CharIni) ) Position_Ignore_CharIni = i
      !       if ( (Position_Ignore_CharFin == 0) .and.  (Char1 == Ignore_CharFin) .and. (Position_Ignore_CharIni/=i) ) Position_Ignore_CharFin = i
      !     end do
      !   !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position of Ignore_CharIni in the string: Position_Ignore_CharIni = ',g0)") Position_Ignore_CharIni
      !   !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position of Ignore_CharFin in the string: Position_Ignore_CharFin = ',g0)") Position_Ignore_CharFin
      !     if ( ( Position_Ignore_CharIni < iSepIni ) .and. ( iSepIni < Position_Ignore_CharFin ) ) then
      !     !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: The separation character is inside the ignore set of characters => Finding the next splitting character')")
      !
      !       String            =   RHS(Position_Ignore_CharFin+1:)
      !     !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: New string where to find the position of the separation character: String = ',g0)") String
      !
      !       iSepIni   =   index( String, Separator )                                                                  ! Index of initial position of the separation character in the input character
      !       iSepFin   =   iSepIni + len_trim(Separator) - 1                                                      ! Index of final   position of the separation character in the input character
      !
      !     !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position in String: iSepIni = ',i0)") iSepIni
      !     !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position in String: iSepFin = ',i0)") iSepFin
      !
      ! !       if iSepIni == 0 , then the separation character is not present in the string (outside the ignore character)
      ! !       So the zero value is kept so that the procedure is exited below.
      !       if ( iSepIni /= 0 ) then
      !         iSepIni   =   iSepIni + (Position_Ignore_CharFin)
      !         iSepFin   =   iSepIni + len_trim(Separator) - 1                                                      ! Index of final   position of the separation character in the input character
      !       end if
      !
      !
      !     !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Original string where to find the position of the separation character: RHS = ',g0)") RHS
      !     !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position in RHS: iSepIni = ',i0)") iSepIni
      !     !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Position in RHS: iSepFin = ',i0)") iSepFin
      !
      ! !     !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: String = ',g0)") RHS()
      !     end if



!     if ( Inf < Val )
!     if ( Val < Sup )
!     if ( Inf < Val ) .and. ( Val < Sup )        !     Inf < Val < Sup
!     Val = [Inf:Sup]
!     indicator   =   val .inside. [Inf,Sup]


!     if ( iSepIni )


  end if


! ==============================================================================================================
!       TREATING CASES FOR WHICH THE INPUT STRING DOES NOT NEED ANY SPLITTING
! ==============================================================================================================
! This section deals with the cases when no splitting is required. There are 3 situations for which the input
! string does not need any splitting. For all these cases the input string is stored in the output LHS string
! variable 'LHS' and the output RHS string variable 'RHS' is set to an empty string.
! This ensure the "Parse" calling procedure to stop its splitting iteration, the iteration being stopped when
! RHS string variable 'RHS' has a zero length. The 3 situations are:
!  - When the input character correspond to an empty string (obvious)
!  - When the separation character is absent from the input string
!  - When the separation character corresponds to the last character of the input string and that the
!    separation character is an RHS escape character (yes, it's a tricky one !)
! ==============================================================================================================
  if ( len_trim(RHS) == 0 ) then                                                                                ! If the input string is empty, then exiting the procedure without changing the input string
  !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Input string is empty => Exiting the procedure')")
    return                                                                                                      ! Exiting the procedure
  end if                                                                                                        ! End if case on input string length
! --------------------------------------------------------------------------------------------------------------
  if ( iSepIni == 0 ) then                                                                                      ! If the separation character is absent from the input string, then
  !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: The separation character is absent from the input string => Exiting the procedure')")
    LHS      =   RHS                                                                                        ! Storing the input string in the LHS string
    RHS      =   ''                                                                                         ! Setting the RHS string to an empty string
    return                                                                                                      ! Exiting the procedure
  end if                                                                                                        ! End of if case on initial separation character index
! --------------------------------------------------------------------------------------------------------------
  if ( (i_EscRHS) .and. (iSepIni == len_trim(RHS)) )  then                                                      ! If a RHS escape character exists and if the last character of the input string corresponds to the separation character
  Char1 =   RHS(iSepIni:iSepIni)                                                                            ! Storing the last character
    do iEscRHS = 1,size(EscRHS)                                                                                 ! Loop on all RHS escape character
      if        (Char1 /= EscRHS(iEscRHS))      cycle                                                           ! Going to the next RHS escape character if the last character from the input string does not correspond to the current RHS escape character
        LHS  =   RHS                                                                                        ! Storing the input string in the LHS string
        RHS  =   ''                                                                                         ! Setting the RHS string to an empty string
        return                                                                                                  ! Exiting the procedure
    end do                                                                                                      ! End loop on RHS escape character
  end if                                                                                                        ! End of if case on initial separation character index
! ==============================================================================================================

  StrInp        =   RHS                                                                                     ! Storing the input string in a new variable
  RHS(:)        =   ''                                                                                      ! Initialization of RHS string
  iLHS          =   0                                                                                       ! Initialization of LHS string index
  iRHS          =   0                                                                                       ! Initialization of RHS string index
  iStrInp       =   0                                                                                       ! Initialization of input string index
  i_protect     =   .False.                                                                                 ! Initialization of the character protection indicator

!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Loop on all characters inside the string: ',a)") StrInp
  do                                                                                                            ! Infinit loop on all characters of the input string

    iStrInp     =   iStrInp + 1                                                                             ! Incrementing the input string index
    if  (iStrInp > len_trim(StrInp))    exit                                                                    ! Exiting the loop if the index is greater than the string length
    Char1       =   StrInp(iStrInp:iStrInp)                                                                 ! Getting the current character
  !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]:   iStrInp = ',i3,3x,'Char1 = ',a)") iStrInp, Char1


!   Case of a LHS escape character
!   ------------------------------
    if (i_EscLHS) then                                                                                          ! If an escape character exists, then
    !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]:   Case of a LHS escape character')")
      if (i_protect) then                                                                                       ! If the current character is protected, then
        i_protect       =   .False.                                                                         ! Setting off the character protection mode
        iLHS            =   iLHS + 1                                                                        ! Index incrementation
        LHS(iLHS:iLHS)  =   Char1                                                                           ! Storing the current character in the LHS string
        cycle                                                                                                   ! Going to the next character
      end if                                                                                                    ! End of if case on character protection
      if ( Char1 == EscLHS ) then                                                                               ! If the current character corresponds to the escape character, then
        iLHS            =   iLHS + 1                                                                        ! Index incrementation
        LHS(iLHS:iLHS)  =   Char1                                                                           ! Storing the current character in the LHS string
        i_protect       =   .True.                                                                          ! Activating the character protection mode
        cycle                                                                                                   ! Going to the next character
      end if                                                                                                    ! End of if case on escape character
    end if                                                                                                      ! End of if case on existence of an escape character

!   Case of a RHS escape character
!   ------------------------------
    if (i_EscRHS) then                                                                                          ! If an RHS escape character exists, then
    !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]:   Case of a RHS escape character')")
      if ( iStrInp == iSepIni ) then                                                                            ! If the current input string index correspond to the initial separation index
        Char1_Next      =   StrInp(iStrInp+1:iStrInp+1)                                                     ! Storing the value of the next character
        do iEscRHS = 1,size(EscRHS)                                                                             ! Loop on all RHS escape characters
          if    (Char1_Next == EscRHS(iEscRHS)) then                                                            ! If the next character corresponds to the RHS escape character, then
             iSepIni        =   iSepIni + index( StrInp(iStrInp+1:), Separator )                            ! Modyfiying the index of initial position of the separation character in the input character: Moving to the next separation character index
             iSepFin        =   iSepIni + len_trim(Separator) - 1                                           ! Modyfiying the index of final   position of the separation character in the input character
          end if                                                                                                ! End of if case on escape character
        end do                                                                                                  ! End loop on RHS escape characters
      end if                                                                                                    ! End if case on input string index
    end if                                                                                                      ! End of if case on RHS escape indicator

    if ( iStrInp < iSepIni ) then                                                                               ! If the current input string index is lower than the initial separation index
    !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: iStrInp < iSepIni')")
      iLHS              =   iLHS + 1                                                                        ! Index incrementation
      LHS(iLHS:iLHS)    =   Char1                                                                           ! Storing the current character in the LHS string
      cycle                                                                                                     ! Going to the next character
    end if                                                                                                      ! End if case on input string index

    if (iStrInp > iSepFin) then                                                                                 ! If the current input string index is greater than the final separation index
    !  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: iStrInp > iSepFin')")
      iRHS              =   iRHS + 1                                                                        ! Index incrementation
      RHS(iRHS:iRHS)    =   Char1                                                                           ! Storing the current character in the LHS string
      cycle                                                                                                     ! Going to the next character
    end if                                                                                                      ! End if case on input string index

  end do                                                                                                        ! End loop on character of the input string
!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: End loop')")

!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: LHS = ',a)") LHS
!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: RHS = ',a)") RHS

  RHS           =   adjustl(RHS)                                                                         ! Removing initial spaces
  RHS           =   trim(RHS)
  LHS           =   trim(LHS)

!  if (i_Debug_Loc) write(LogUnit,"(12x,'[Split]: Exiting')")

End Procedure

#   define  _VarType_         integer(INT8)
#   define  _VarKind_         INT8
#   include "ParseNamesValues_Implementation_Inlined.F90"

#   define  _VarType_         integer(INT16)
#   define  _VarKind_         INT16
#   include "ParseNamesValues_Implementation_Inlined.F90"

#   define  _VarType_         integer(INT32)
#   define  _VarKind_         INT32
#   include "ParseNamesValues_Implementation_Inlined.F90"

#   define  _VarType_         integer(INT64)
#   define  _VarKind_         INT64
#   include "ParseNamesValues_Implementation_Inlined.F90"

#   define  _VarType_         real(REAL32)
#   define  _VarKind_         REAL32
#   include "ParseNamesValues_Implementation_Inlined.F90"

#   define  _VarType_         real(REAL64)
#   define  _VarKind_         REAL64
#   include "ParseNamesValues_Implementation_Inlined.F90"

#   define  _VarType_         real(REAL128)
#   define  _VarKind_         REAL128
#   include "ParseNamesValues_Implementation_Inlined.F90"

End SubModule



! ! ! **************************************************************************************************************
! ! ! **************************************************************************************************************
! ! !                                       WORK-IN-PROGRESS
! ! ! **************************************************************************************************************
! ! ! **************************************************************************************************************
! !
! ! Subroutine Parse_Fixed_Size( String_Input, Separator, String_Output, EscRHS, IgnoreBetween )
! !   character(*)                                          ,intent(in)     ::  String_Input                    !< Input scalar character string to be proceeded
! !   character(*)                                          ,intent(in)     ::  Separator                       !< Separation character string
! !   character(*)  ,dimension(:)                           ,intent(out)    ::  String_Output                   !< Output vector character string containing sub-strings
! !   character(1)  ,dimension(:)                 ,optional ,intent(in)     ::  EscRHS                          !< RHS escape character
! !   character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  IgnoreBetween
! !   character(len=len(String_Input))                                      ::  String                    ! Local copy of the input string
! !   character(len=len(String_Input))                                      ::  String_LHS
! !   character(len=len(String_Input))                                      ::  String_RHS
! !   integer                                                               ::  i
! !   String_Output =   ""
! !   String        =   Compact(String_Input)                                                                   ! Storing local value of the input character string and compacting it
! !   if ( len_trim(String) == 0 ) return                                                                           ! If empty input string, then exiting the procedure
! !   i          =   0
! !   do                                                                                                            ! Infinite loop on all characters of the input string
! !     i        =   i + 1
! !     if ( i > size(String_Output) ) exit
! !     if ( len_trim(String)==0 ) exit                                                                             ! If the current local character string is empty, then exiting the procedure
! !     call Split_Fixed_Size( String, String_LHS, Separator, String_RHS, EscRHS=EscRHS, IgnoreBetween=IgnoreBetween )       ! Spliting the string into a left and right string separated by the Separator character
! !     String              =   String_RHS                                                                      ! Setting the remaining string to be proceed to the RHS string                                                                                                     ! If allocated output String_Output variable, then sub-strings have already been proceeded
! !     String_Output(i)    =   String_LHS                                                                      !
! !   end do                                                                                                        ! End of loop
! ! End Subroutine
! !
! ! Subroutine Split_Fixed_Size( Input_String, LHS, Separator, RHS, EscLHS, EscRHS, i_Length_Eq_Inp, IgnoreBetween, i_Debug )
! !
! !   character(*)                                          ,intent(in)     ::  Input_String                          !< Input character string to be splitted into a LHS and a RHS parts
! !   character(*)                                          ,intent(out)    ::  LHS                             !< Left-hand-side part of the splitted string
! !   character(*)                                          ,intent(in)     ::  Separator                       !< Separation character string
! !   character(*)                                          ,intent(out)    ::  RHS                             !< Right-hand-side part of the splitted string
! !   character(1)                                ,optional ,intent(in)     ::  EscLHS                          !< Left-hand-side escape character string
! !   character(1)  ,dimension(:)                 ,optional ,intent(in)     ::  EscRHS                          !< Right-hand-side escape character string
! !   logical                                     ,optional ,intent(in)     ::  i_Length_Eq_Inp                 ! Indicator to force the length of the output string to equal the one of the input string (required because otherwise the error:: fort: (4): Variable STRLHS has substring ending point 1 which is greater than the variable length of 0) Mayu be possible to
! !   character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  IgnoreBetween
! !   logical                                     ,optional ,intent(in)     ::  i_Debug
! !
! !   logical                                                               ::  i_Debug_Loc
! !   logical                                                               ::  i_EscLHS                        ! Escape character indicator
! !   logical                                                               ::  i_EscRHS                        ! Escape character indicator
! !   logical                                                               ::  i_protect                       ! Character protection indicator
! !   logical                                                               ::  i_Length_Eq_Inp_Local
! !   logical                                                               ::  i_IgnoreBetween
! ! !   character(:)  ,allocatable    ,dimension(:)                           ::  Char_IgnoreBetween
! !   integer                                                               ::  iStrInp                         ! Index of the input string
! !   integer                                                               ::  iRHS                            ! Index of the RHS string
! !   integer                                                               ::  iLHS                            ! Index of the LHS string
! !   integer                                                               ::  iSepIni                         ! Initial index of the separation character in the input string
! !   integer                                                               ::  iSepFin                         ! Final index of the separation character in the input string
! !   character(1)                                                          ::  Char1                           ! String single character
! !   character(1)                                                          ::  Char1_Next                      ! Next character
! !   character(len=len_trim(Input_String))                                 ::  StrInp                          ! Local copy of the input string
! !   integer                                                               ::  iEscRHS                         ! Index of element in the array of RHS escape character
! !
! !   character(:)  ,allocatable                                            ::  String
! !   character(:)  ,allocatable                                            ::  Ignore_CharIni                  ! Initial character from which the separation character should be ignored
! !   character(:)  ,allocatable                                            ::  Ignore_CharFin                  ! final character   to   which the separation character should be ignored
! !
! !   integer                                                               ::  Position_Ignore_CharIni
! !   integer                                                               ::  Position_Ignore_CharFin
! !   integer                                                               ::  i
! !
! !   i_Debug_Loc = .False.; if ( present(i_Debug) ) i_Debug_Loc = i_Debug
! !
! !   RHS   =   Input_String
! !
! ! ! ==============================================================================================================
! ! !       PROCESSING OPTIONAL INPUT ARGUMENTS
! ! ! ==============================================================================================================
! !   i_EscRHS              =   .False.                                                                         ! Initialization of the RHS escape character indicator
! !   i_EscLHS              =   .False.                                                                         ! Initialization of the LHS escape character indicator
! !   i_Length_Eq_Inp_Local =   .False.
! !   i_IgnoreBetween      =   .False.
! !   if ( present(EscRHS) )                i_EscRHS = .True.                                                       ! If a RHS escape character is provided, the activating the RHS escape indicator
! !   if ( present(EscLHS) )                i_EscLHS = .True.                                                       ! If a LHS escape character is provided, the activating the LHS escape indicator
! !   if ( present(i_Length_Eq_Inp) )       i_Length_Eq_Inp_Local = i_Length_Eq_Inp
! !   if ( present(IgnoreBetween) )        i_IgnoreBetween = .True.
! ! ! ==============================================================================================================
! !
! !
! !    if ( i_EscRHS ) RHS = RemoveSpace( adjustl(RHS) )                                                          ! Removing blanks and adjusting the to LHS
! !   LHS       =   ''                                                                                   ! Initialization of the LHS character string
! !
! !   iSepIni   =   index( RHS, Separator )                                                                 ! Index of initial position of the separation character in the input character
! !   iSepFin   =   iSepIni + len_trim(Separator) - 1                                                      ! Index of final   position of the separation character in the input character
! !
! !
! ! ! ==============================================================================================================
! !   if ( i_IgnoreBetween ) then
! !     if ( size(IgnoreBetween) == 1 ) then
! !       Ignore_CharIni    =   IgnoreBetween(1)
! !       Ignore_CharFin    =   IgnoreBetween(1)
! !     else if ( size(IgnoreBetween) >= 2 ) then
! !       Ignore_CharIni    =   IgnoreBetween(1)
! !       Ignore_CharFin    =   IgnoreBetween(2)
! !     end if
! !     Position_Ignore_CharIni     =   0   !     This only works for ignore characters of 1 character
! !     Position_Ignore_CharFin     =   0
! !     do i = 1,len_trim(RHS)
! !       Char1     =   RHS(i:i)
! !       if ( (Position_Ignore_CharIni == 0) .and.  (Char1 == Ignore_CharIni) ) Position_Ignore_CharIni = i
! !       if ( (Position_Ignore_CharFin == 0) .and.  (Char1 == Ignore_CharFin) .and. (Position_Ignore_CharIni/=i) ) Position_Ignore_CharFin = i
! !     end do
! !     if ( ( Position_Ignore_CharIni < iSepIni ) .and. ( iSepIni < Position_Ignore_CharFin ) ) then
! !       String            =   RHS(Position_Ignore_CharFin+1:)
! !       iSepIni   =   index( String, Separator )                                                                  ! Index of initial position of the separation character in the input character
! !       iSepFin   =   iSepIni + len_trim(Separator) - 1                                                      ! Index of final   position of the separation character in the input character
! !       if ( iSepIni /= 0 ) then                                        ! if iSepIni == 0 , then the separation character is not present in the string (outside the ignore character). So the zero value is kept so that the procedure is exited below.
! !         iSepIni   =   iSepIni + (Position_Ignore_CharFin)
! !         iSepFin   =   iSepIni + len_trim(Separator) - 1                                                      ! Index of final   position of the separation character in the input character
! !       end if
! !     end if
! !   end if
! ! ! ==============================================================================================================
! !
! !
! ! ! ==============================================================================================================
! ! !       TREATING CASES FOR WHICH THE INPUT STRING DOES NOT NEED ANY SPLITTING
! ! ! ==============================================================================================================
! ! ! This section deals with the cases when no splitting is required. There are 3 situations for which the input
! ! ! string does not need any splitting. For all these cases the input string is stored in the output LHS string
! ! ! variable 'LHS' and the output RHS string variable 'RHS' is set to an empty string.
! ! ! This ensure the "Parse" calling procedure to stop its splitting iteration, the iteration being stopped when
! ! ! RHS string variable 'RHS' has a zero length. The 3 situations are:
! ! !  - When the input character correspond to an empty string (obvious)
! ! !  - When the separation character is absent from the input string
! ! !  - When the separation character corresponds to the last character of the input string and that the
! ! !    separation character is an RHS escape character (yes, it's a tricky one !)
! ! ! ==============================================================================================================
! !   if ( len_trim(RHS) == 0 ) return                                                                                ! If the input string is empty, then exiting the procedure without changing the input string
! ! ! --------------------------------------------------------------------------------------------------------------
! !   if ( iSepIni == 0 ) then                                                                                      ! If the separation character is absent from the input string, then
! !     LHS      =   RHS                                                                                        ! Storing the input string in the LHS string
! !     RHS      =   ''                                                                                         ! Setting the RHS string to an empty string
! !     return                                                                                                      ! Exiting the procedure
! !   end if                                                                                                        ! End of if case on initial separation character index
! ! ! --------------------------------------------------------------------------------------------------------------
! !   if ( (i_EscRHS) .and. (iSepIni == len_trim(RHS)) )  then                                                      ! If a RHS escape character exists and if the last character of the input string corresponds to the separation character
! !   Char1 =   RHS(iSepIni:iSepIni)                                                                            ! Storing the last character
! !     do iEscRHS = 1,size(EscRHS)                                                                                 ! Loop on all RHS escape character
! !       if        (Char1 /= EscRHS(iEscRHS))      cycle                                                           ! Going to the next RHS escape character if the last character from the input string does not correspond to the current RHS escape character
! !         LHS  =   RHS                                                                                        ! Storing the input string in the LHS string
! !         RHS  =   ''                                                                                         ! Setting the RHS string to an empty string
! !         return                                                                                                  ! Exiting the procedure
! !     end do                                                                                                      ! End loop on RHS escape character
! !   end if                                                                                                        ! End of if case on initial separation character index
! ! ! ==============================================================================================================
! !
! !
! !
! !
! !
! !   StrInp        =   RHS                                                                                     ! Storing the input string in a new variable
! !   RHS(:)        =   ''                                                                                      ! Initialization of RHS string
! !   iLHS          =   0                                                                                       ! Initialization of LHS string index
! !   iRHS          =   0                                                                                       ! Initialization of RHS string index
! !   iStrInp       =   0                                                                                       ! Initialization of input string index
! !   i_protect     =   .False.                                                                                 ! Initialization of the character protection indicator
! !   do                                                                                                            ! Infinit loop on all characters of the input string
! !     iStrInp     =   iStrInp + 1                                                                             ! Incrementing the input string index
! !     if  (iStrInp > len_trim(StrInp))    exit                                                                    ! Exiting the loop if the index is greater than the string length
! !     Char1       =   StrInp(iStrInp:iStrInp)                                                                 ! Getting the current character
! ! !   Case of a LHS escape character
! ! !   ------------------------------
! !     if (i_EscLHS) then                                                                                          ! If an escape character exists, then
! !       if (i_protect) then                                                                                       ! If the current character is protected, then
! !         i_protect       =   .False.                                                                         ! Setting off the character protection mode
! !         iLHS            =   iLHS + 1                                                                        ! Index incrementation
! !         LHS(iLHS:iLHS)  =   Char1                                                                           ! Storing the current character in the LHS string
! !         cycle                                                                                                   ! Going to the next character
! !       end if                                                                                                    ! End of if case on character protection
! !       if ( Char1 == EscLHS ) then                                                                               ! If the current character corresponds to the escape character, then
! !         iLHS            =   iLHS + 1                                                                        ! Index incrementation
! !         LHS(iLHS:iLHS)  =   Char1                                                                           ! Storing the current character in the LHS string
! !         i_protect       =   .True.                                                                          ! Activating the character protection mode
! !         cycle                                                                                                   ! Going to the next character
! !       end if                                                                                                    ! End of if case on escape character
! !     end if                                                                                                      ! End of if case on existence of an escape character
! !
! ! !   Case of a RHS escape character
! ! !   ------------------------------
! !     if (i_EscRHS) then                                                                                          ! If an RHS escape character exists, then
! !       if ( iStrInp == iSepIni ) then                                                                            ! If the current input string index correspond to the initial separation index
! !         Char1_Next      =   StrInp(iStrInp+1:iStrInp+1)                                                     ! Storing the value of the next character
! !         do iEscRHS = 1,size(EscRHS)                                                                             ! Loop on all RHS escape characters
! !           if    (Char1_Next == EscRHS(iEscRHS)) then                                                            ! If the next character corresponds to the RHS escape character, then
! !              iSepIni        =   iSepIni + index( StrInp(iStrInp+1:), Separator )                            ! Modyfiying the index of initial position of the separation character in the input character: Moving to the next separation character index
! !              iSepFin        =   iSepIni + len_trim(Separator) - 1                                           ! Modyfiying the index of final   position of the separation character in the input character
! !           end if                                                                                                ! End of if case on escape character
! !         end do                                                                                                  ! End loop on RHS escape characters
! !       end if                                                                                                    ! End if case on input string index
! !     end if                                                                                                      ! End of if case on RHS escape indicator
! !
! !     if ( iStrInp < iSepIni ) then                                                                               ! If the current input string index is lower than the initial separation index
! !       iLHS              =   iLHS + 1                                                                        ! Index incrementation
! !       LHS(iLHS:iLHS)    =   Char1                                                                           ! Storing the current character in the LHS string
! !       cycle                                                                                                     ! Going to the next character
! !     end if                                                                                                      ! End if case on input string index
! !
! !     if (iStrInp > iSepFin) then                                                                                 ! If the current input string index is greater than the final separation index
! !       iRHS              =   iRHS + 1                                                                        ! Index incrementation
! !       RHS(iRHS:iRHS)    =   Char1                                                                           ! Storing the current character in the LHS string
! !       cycle                                                                                                     ! Going to the next character
! !     end if                                                                                                      ! End if case on input string index
! !
! !   end do                                                                                                        ! End loop on character of the input string
! !
! !   RHS           =   adjustl(RHS)                                                                         ! Removing initial spaces
! !   RHS           =   trim(RHS)
! !   LHS           =   trim(LHS)
! !
! ! End Subroutine
!
! ! Subroutine TestCompilation
! !   write(*,*) VecTrim(["a","b","c"])
! ! !   write(*,*) VecTrim(["a","b     ","c"])
! !   write(*,*) VecTrim([character(10) :: "a","b     ","c"])
! ! End Subroutine


