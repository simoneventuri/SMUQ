! #include "preprocessor_variables.inc"

Module GPF_Tools

! @TODO: Some procedure could be put directcly in the program unit that uses them since they are the only one to use them.


  use GPF_Parameters            ,only:  rkp, DbgUnit

  implicit none

  private
  public  ::  Check_Validity
  public  ::  Check_Numeric
  public  ::  Find_Position
  public  ::  Get_Format_i
  public  ::  Set_IntNumChar
  public  ::  Right_Dimension
  public  ::  Replace_Character
  public  ::  Is_Numeric
  public  ::  Is_Valid, Is_Valid_Coordinates
  public  ::  Parse
  public  ::  Allocate_Data
  public  ::  Convert_To_String
  public  ::  Convert_To_Integer
  public  ::  Remove_Trailing_Zeros
  public  ::  Add_Apostroph
  public  ::  Extract_Label
  public  ::  Get_Extension
  public  ::  Remove_Extension

! **************************************************************************************************************
! ******************************************   PROCEDURE INTERFACES   ******************************************
! **************************************************************************************************************

  Interface             Check_Validity
    Module Procedure    Check_Validity_0D, Check_Validity_1D
  End Interface

  Interface             Check_Numeric
    Module Procedure    Check_Numeric_0D, Check_Numeric_1D
  End Interface

  Interface             Right_Dimension
    Module Procedure    Right_Dimension_I, Right_Dimension_R, Right_Dimension_C
  End Interface

  Interface             Convert_To_String
    Module Procedure    Integer_To_String_0D!,   Integer_To_String_1D
    Module Procedure    Real_To_String_0D
  End Interface

  Interface             Convert_To_Integer
    Module Procedure    String_To_Integer_0D, String_To_Integer_1D
  End Interface

  contains

Pure Function Is_Valid( String, Valid_Strings )
  logical                                                       ::  Is_Valid                                !< Indicator of input object validity
  character(*)                                  ,intent(in)     ::  String                                  !< String to be checked for validity
  character(*)          ,dimension(:)           ,intent(in)     ::  Valid_Strings                           !< Valid object used for validity check
  integer                                                       ::  iValStr                                 ! Index of valid strings
  Is_Valid      =       .False.                                                                                 ! Initialization of the object validity indicator to false
  do iValStr = 1,size(Valid_Strings)                                                                            ! Loop on all valid strings
    if ( trim(String) == trim(Valid_Strings(iValStr)) ) Is_Valid = .True.                                       ! If the object if found in the list of valid strings, then setting validity indicator to true
  end do                                                                                                        ! End do loop on valid Nature
End Function

Pure Function Is_Numeric( Object )
  logical                                                       ::  Is_Numeric                              !< Indicator of numeric value stored in the input string
  character(*)                                  ,intent(in)     ::  Object                                  !< Object to be checked for validity as a numeric
  character(*)          ,parameter                              ::  Valid_Charaters=' 1234567890.'          ! List of characters wich are valid numbers
  Is_Numeric    =       ( verify(Object,Valid_Charaters) == 0 )                                                 ! Setting the numeric indicator ( verify returns 0 if each character in Object appears in Valid_Charaters)
End Function

! ! #ifdef GFORTRAN_WORKAROUND_ALLOCATABLE_CHARACTER
! ! Pure Function Check_Validity_0D( String, Valid, Default ) result( Checked_String )
! !   character(*)                                          ,intent(in)     ::  String                          !< String to be checked for validity
! !   character(*)  ,dimension(:)                           ,intent(in)     ::  Valid                           !< Valid strings
! !   character(*)                                ,optional ,intent(in)     ::  Default                         !< Default string
! !   character(1000)                                                       ::  Checked_String                  !< Checked String
! !   character(1000)                                                       ::  Default_Local                   !< Local default string
! !   Default_Local =       ""                                                                                      ! Initializing the local default string to an empty string
! !   if ( present(Default) ) Default_Local = Default                                                               ! Setting the local default string to the input value if present
! !   if ( Is_Valid(String,Valid) ) then                                                                            ! If valid input string
! !     Checked_String      =       trim(String)                                                                    ! Setting cheched string to input string
! !   else                                                                                                          ! If unvalid input string
! !     Checked_String      =       trim(Default_Local)                                                             ! Setting checked string to default string
! !   end if                                                                                                        ! End if case on input string validity
! ! End Function
! ! #else
! ! ! This procedure causes a problem in GPF_Output_Class::SetOutputFileName
Pure Function Check_Validity_0D( String, Valid, Default ) result( Checked_String )
  character(*)                                          ,intent(in)     ::  String                          !< String to be checked for validity
  character(*)  ,dimension(:)                           ,intent(in)     ::  Valid                           !< Valid strings
  character(*)                                ,optional ,intent(in)     ::  Default                         !< Default string
  character(:)        ,allocatable                                      ::  Checked_String                  !< Checked String
  character(:)        ,allocatable                                      ::  Default_Local                   !< Local default string
  Default_Local =       ""                                                                                      ! Initializing the local default string to an empty string
  if ( present(Default) ) Default_Local = Default                                                               ! Setting the local default string to the input value if present
  if ( Is_Valid(String,Valid) ) then                                                                            ! If valid input string
    Checked_String      =       trim(String)                                                                    ! Setting cheched string to input string
  else                                                                                                          ! If unvalid input string
    Checked_String      =       trim(Default_Local)                                                             ! Setting checked string to default string
  end if                                                                                                        ! End if case on input string validity
End Function
! #endif



Pure Function Check_Validity_1D( Strings, Valid, Default ) result( Checked_Strings )
  character(*)  ,dimension(:)                           ,intent(in)     ::  Strings                         !< Strings to be checked for validity
  character(*)  ,dimension(:)                           ,intent(in)     ::  Valid                           !< Valid strings
  character(*)                                ,optional ,intent(in)     ::  Default                         !< Default string
  character(:) ,allocatable ,dimension(:)                               ::  Checked_Strings                 !< Checked Strings
  integer                                                               ::  i                               ! Index of strings to be checked for validity
  character(len(Valid)) ,dimension( size(Strings) )                     ::  Temporary_Strings               ! Temporary strings
  integer                                                               ::  Length                          ! Maximum length without trailling blanks
  forall ( i=1:size(Strings) ) Temporary_Strings(i) = Check_Validity( Strings(i), Valid, Default )              ! Checking string validity and setting the output string
  Length        =       Len_Trim_Max( Temporary_Strings )                                                       ! Setting the maximum length without trailling blanks
! #ifndef GFORTRAN
  allocate( character(Length) :: Checked_Strings(size(Strings)) )                                               ! Allocating length and dimension
! #else
!   allocate( Checked_Strings(size(Strings)) )                                               ! Allocating length and dimension
! #endif
#ifdef GFORTRAN_WORKAROUND_ALLOCATABLE_CHARACTER
  do i = 1,size(Checked_Strings)
    Checked_Strings(i)  =       Temporary_Strings(i)                                                            ! Setting Checked strings in output variable
end do
#else
  Checked_Strings       =       Temporary_Strings                                                               ! Setting Checked strings in output variable
#endif
End Function


Pure Function Len_Trim_Max( Strings ) result( Length )
  character(*)  ,dimension(:)                   ,intent(in)             ::  Strings                         !< Array of character string
  integer                                                               ::  Length                          !< Maximum length without trailling blanks
  integer                                                               ::  i                               ! Index of string' elements
  Length        =       0                                                                                       ! Initialization of maximum length of string
  do i = 1,size(Strings)                                                                                        ! Loop on all elements
    Length      =       max( Length, len_trim(Strings(i)) )                                                     ! Setting the maximum length
  end do                                                                                                        ! End loop on all elements
End Function


Pure Function Check_Numeric_0D( Object, Default_Object ) result( Checked_Object )
  character(*)                                  ,intent(in)     ::  Object                                  !< Object to be checked for validity as a numeric number
  character(*)                                  ,intent(in)     ::  Default_Object                          !< Default object
  character(:)  ,allocatable                                    ::  Checked_Object                          !< Checked Object
  Checked_Object        =       trim(Default_Object)                                                            ! Setting checked object to default object
  if ( Is_Numeric(Object) ) Checked_Object = trim(Object)                                                       ! If valid input object, then setting the cheched object to the input object
End Function

Pure Function Check_Numeric_1D( Objects, Default_Object ) result( Checked_Objects )
  character(*)  ,dimension(:)                   ,intent(in)     ::  Objects                                 !< Objects to be checked for validity as a numeric number
  character(*)                                  ,intent(in)     ::  Default_Object                          !< Default object
  character(:)  ,dimension(:)   ,allocatable                    ::  Checked_Objects                         !< Checked Objects
  integer                                                       ::  iObj                                    ! Index of objects to be checked for validity
  integer                                                       ::  MaxLen                                  ! Maximum length
  MaxLen        =       0                                                                                       ! Initialization of maximum length of string
  do iObj = 1,size(Objects)                                                                                     ! Loop on all input objects
    MaxLen      =       max( MaxLen, len_trim(Check_Numeric_0D(Objects(iObj),Default_Object)) )                 ! Setting the maximum length
  end do                                                                                                        ! End loop on input objects
  allocate( character(MaxLen) :: Checked_Objects(size(Objects)) )                                               ! Allocating length and dimension
  forall ( iObj=1:size(Objects) )                                                                               ! Loop on all input objects
    Checked_Objects(iObj) = Check_Numeric_0D( Objects(iObj), Default_Object )                                   ! Checking object validity and setting the output object
  end forall                                                                                                    ! End loop on input objects
End Function

Pure Function Find_Position( Object, List_Objects ) result( iObj )
  character(*)                                  ,intent(in)     ::  Object                                  !< Object to be located in List_Objects
  character(*)  ,dimension(:)                   ,intent(in)     ::  List_Objects                            !< List of objects
  integer                                                       ::  iObj                                    !< Index of Object in List_Objects
  iObj  =       0                                                                                               ! Initialization of index to zero
  do iObj = 1,size(List_Objects)                                                                                ! Loop on the object list
    if ( trim(Object) == trim(List_Objects(iObj)) ) exit                                                        ! If input object corresponds to current object, then index is found and so exiting the loop
  end do                                                                                                        ! End loop on the object list
End Function

Function Get_Format_i(Number) result(fmt)
  character(:)          ,allocatable    ::  fmt
  integer               ,intent(in)     ::  Number
  integer                               ::  MaxLen
  character(*)          ,parameter      ::  Char_Prefix='(i'
  character(*)          ,parameter      ::  Char_Suffix=')'
  character(100)                        ::  Char_Number
  MaxLen        =       len_trim(Char_Prefix) + Set_IntNumChar(Number) + len_trim(Char_Suffix)
  allocate( character(MaxLen) :: fmt )
  write(Char_Number,"(i100)") Number
  fmt           =       Char_Prefix // trim(adjustl(Char_Number)) // Char_Suffix
End Function

Pure Function Set_IntNumChar(Number) result(N)
  integer                               ::  N
  integer       ,intent(in)             ::  Number
  N     =       floor(log10(real(Number))) + 1
End Function

Function Right_Dimension_I( Array, NLine )
  integer               ,dimension(:)                   ,intent(in)     ::  Array                           !< Array varible which dimension need to be checked
  integer                                               ,intent(in)     ::  NLine                           !< Dimension used for checking (number of lines)
  logical                                                               ::  Right_Dimension_I               !< Indicator whether of not the input array has the right dimension
  Right_Dimension_I     =       size(Array) == NLine                                                            ! Setting the dimension indicator
End Function

Function Right_Dimension_R( Array, NLine )
  real(rkp)             ,dimension(:)                   ,intent(in)     ::  Array                           !< Array varible which dimension need to be checked
  integer                                               ,intent(in)     ::  NLine                           !< Dimension used for checking (number of lines)
  logical                                                               ::  Right_Dimension_R               !< Indicator whether of not the input array has the right dimension
  Right_Dimension_R     =       size(Array) == NLine                                                            ! Setting the dimension indicator
End Function

Function Right_Dimension_C( Array, NLine )
  character(*)          ,dimension(:)                   ,intent(in)     ::  Array                           !< Array varible which dimension need to be checked
  integer                                               ,intent(in)     ::  NLine                           !< Dimension used for checking (number of lines)
  logical                                                               ::  Right_Dimension_C               !< Indicator whether of not the input array has the right dimension
  Right_Dimension_C     =       size(Array) == NLine                                                            ! Setting the dimension indicator
End Function

Function Replace_Character( String_Inp, CharOld, CharNew ) result(String_Out)
  character(*)                                          ,intent(in)     ::  String_Inp                         !< String variable to be modified
  character(*)                                          ,intent(in)     ::  CharOld                        !<
  character(*)                                          ,intent(in)     ::  CharNew                        !<
  character(:)  ,allocatable                                            ::  String_Out                         !<
  integer                                                               ::  iOldIni
  integer                                                               ::  iOldFin
  integer                                                               ::  iNewIni
  integer                                                               ::  iNewFin
  character(:)  ,allocatable                                            ::  String_Tmp
  String_Out    =       ""
  String_Tmp    =       String_Inp                                                                              ! Loading the input string in the temporary variable
  do                                                                                                            ! Loop on all character for character replacement
    iOldIni     =       index(String_Tmp,CharOld)                                                               ! Setting index of first (initial) character to be replaced in the old string
    iOldFin     =       iOldIni - 1 + len(CharOld)                                                              ! Setting index of last (final) character to be replaced in the old string
    iNewIni     =       iOldIni                                                                                 ! Setting index of first (initial) character to be replaced in the new string
    iNewFin     =       iOldIni - 1 + len(CharNew)                                                              ! Setting index of last (final) character to be replaced in the new string
    if ( iOldIni /= 0 ) then
      String_Out        =       String_Out // String_Tmp(1:iOldIni-1) // CharNew
      String_Tmp        =       String_Tmp(iOldFin+1:)
    else
      String_Out        =       String_Out // String_Tmp
      exit                                                                                    ! If character index is zero, then no character to be replace and so exiting the loop
    end if
  end do                                                                                                        ! End of loop on string characters
End Function

Function Is_Valid_Coordinates( StrInp )
  logical                                                       ::  Is_Valid_Coordinates                    !< Indicator of input coordinates validity
  character(*)                                  ,intent(in)     ::  StrInp                                  !< Input string to be checked as valid coordinates
  character(*)  ,parameter                                      ::  StrSep=','                              ! Separation character between the x and y coordinates
  character(len(StrInp))        ,dimension(:)   ,allocatable    ::  StrVec
  Is_Valid_Coordinates  =       .False.                                                                                 ! Initialization of validity indicator to false
  call Parse( StrInp, StrSep, StrVec )
  if ( allocated(StrVec) ) then
    if ( size(StrVec) == 2 ) then
      if ( Is_Numeric(StrVec(1)) .and. Is_Numeric(StrVec(2)) ) Is_Valid_Coordinates = .True.
    end if
  end if
End Function

!<==============================================================================================================
!> @brief       Parses a string into multiple string separated by a given character
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure parses the input string \c StrInp into sub-string based on the delimiters contained in the
!! input string \c StrSep. \n
!! Those sub-string are stored in the output array string \c StrVec which is allocated inside  the procedure. \n
!! The number of sub-string, i.e. the number of elements of the ouptput string \c StrVec, is stored in the
!! optional output variable \c Nvec, if this argument is present in the procedure call. \n
!! The optional input variable \c EscRHS corresponds to a RHS escape character. \n
!<==============================================================================================================
Pure Subroutine Parse( StrInp, StrSep, StrVec, Nvec, EscRHS )
  character(*)                                          ,intent(in)     ::  StrInp                          !< Input character string to be proceeded
  character(*)                                          ,intent(in)     ::  StrSep                          !< Separation character string
  character(*)          ,dimension(:)   ,allocatable    ,intent(out)    ::  StrVec                          !< Output string vector
  integer                                     ,optional ,intent(out)    ::  Nvec                            !< Number of element in the output string vector
  character(1)  ,dimension(:)                 ,optional ,intent(in)     ::  EscRHS                          !< RHS escape character
  character(len(StrInp))                                                ::  StrLoc                          ! Local copy of the input string
  character(len(StrVec))        ,dimension(:)   ,allocatable            ::  local_StrVec                    ! Local string vector
  character(len(StrVec))                                                ::  StrSca                          ! Scalar string
  integer                                                               ::  Nelt                            ! Number of element in the output string vector (local value of the optional Nelt output argument)
!   StrLoc        =       compact(StrInp)                                                                         ! Storing local value of the input character string and compacting it
  StrLoc        =       StrInp                                                                                  ! Storing local value of the input character
  Nelt          =       0                                                                                       ! Initialization of the number of elements
  do                                                                                                            ! Infinite loop on all characters of the input string
    if ( len_trim(StrLoc) == 0 ) exit                                                                           ! If the current local character string is empty, then exiting the procedure
    Nelt        =       Nelt + 1                                                                                ! Incrementation of the number of elements
    call split( StrSca, StrSep, StrLoc, EscRHS=EscRHS )                                                         ! Spliting the string into a left and right string separated by the StrSep character
    allocate( local_StrVec(Nelt) )                                                                              ! Allocating the local character vector
    local_StrVec(Nelt)          =       StrSca                                                                  ! Affecting current element of the character vector
    if ( Nelt > 1 ) local_StrVec(1:Nelt-1) = StrVec(:)                                                  ! Affecting previous elements of the character vector
    call move_alloc( local_StrVec, StrVec )                                                                     ! Transfering allocation from local to output variable
  end do                                                                                                        ! End of loop
  if ( present(Nvec) ) Nvec     =       Nelt                                                                    ! If the optional Nvec ouput argument is present, then setting its value
End Subroutine

!<==============================================================================================================
!> @brief       Splits a string into a RHS and A LHS part separated by a given string
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure splits a string into a RHS and A LHS part separated by a given string. \n
!! The first instance of a character from separation string \c StrSep is located in the input string \c StrRhs. \n
!! The characters before the found delimiter are output in \c StrLhs, it is the Left-Hand Side string. \n
!! The characters after  the found delimiter are output in \c StrRhs, it is the Right-Hand-Side string. \n
!! The optional input strings contain the LHS and RHS escape characters which can be used for escaping
!! characters.
!<==============================================================================================================
Pure Subroutine Split( StrLhs, StrSep, StrRhs, EscLHS, EscRHS )
  character(*)                                  ,intent(out)    ::  StrLhs                                  !< Light-hand-side part of the splitted string
  character(*)                                  ,intent(in)     ::  StrSep                                  !< Separation character string
  character(*)                                  ,intent(inout)  ::  StrRhs                                  !< Charcter string to be splitted on inut and Right-hand-side part of the splitted string on output
  character(1)                        ,optional ,intent(in)     ::  EscLHS                                  !< Left-hand-side escape character string
  character(1)        ,optional ,dimension(:)   ,intent(in)     ::  EscRHS                                  !< Right-hand-side escape character string
  logical                                                       ::  i_EscLHS                                ! Escape character indicator
  logical                                                       ::  i_EscRHS                                ! Escape character indicator
  logical                                                       ::  i_protect                               ! Character protection indicator
  integer                                                       ::  iStrInp                                 ! Index of the input string
  integer                                                       ::  iStrRhs                                 ! Index of the RHS string
  integer                                                       ::  iStrLhs                                 ! Index of the LHS string
  integer                                                       ::  iStrSep_ini                             ! Initial index of the separation character in the input string
  integer                                                       ::  iStrSep_fin                             ! Final index of the separation character in the input string
  character(1)                                                  ::  Char1                                   ! String single character
  character(1)                                                  ::  Char1_Next                              ! Next character
  character(len=len_trim(StrRhs))                               ::  StrInp                                  ! Local copy of the input string
  integer                                                       ::  iEscRHS                                 ! Index of element in the array of RHS escape character

  i_EscRHS      =       .False.                                                                                 ! Initialization of the RHS escape character indicator
  if    (present(EscRHS))       i_EscRHS        =       .True.                                                  ! If a RHS escape character is provided, the activating the RHS escape indicator

  i_EscLHS      =       .False.                                                                                 ! Initialization of the LHS escape character indicator
  if    (present(EscLHS))       i_EscLHS        =       .True.                                                  ! If a LHS escape character is provided, the activating the LHS escape indicator

  if    (i_EscRHS)      then
    StrRhs      =       RemoveSpace( adjustl(StrRhs) )                                                          ! Removing blanks and adjusting the to LHS
  else
!     StrRhs    =       Compact( adjustl(StrRhs) )                                                              ! Removing blanks and adjusting the to LHS
  end if


  StrLhs        =       ''                                                                                      ! Initialization of the LHS character string
  iStrSep_ini   =       index( StrRhs, StrSep )                                                                 ! Index of initial position of the separation character in the input character
  iStrSep_fin   =       iStrSep_ini + len_trim(StrSep) - 1                                                      ! Index of final   position of the separation character in the input character

! ************************************************************************
! *   TREATING CASES FOR WHICH THE INPUT STRING DOES NOT ANY SPLITTING   *
! ************************************************************************

! REMARK:       There are tree cases for which the input string does not need any splitting. For these cases,
!               the input string is stored in the outplut LHS string and the output RHS string is set to an
!               empty string. This ensure that the "Pase" calling procedure will stop its splitting itteration
!               (which stops when StrRhs has a zero length). These cases are:
!               1) When the input character correspond to an empty string (obvious)
!               2) When the separation character is absent from the input string
!               3) When the separation character corresponds to the last character of the input string
!                  and that the separation character is an RHS escape character

  if    (len_trim(StrRhs) == 0) return                                                                          ! Exiting the routine if the input string is empty
  if    (iStrSep_ini == 0)      then                                                                            ! If the separation character is absent from the input string, then
    StrLhs      =       StrRhs                                                                                  ! Storing the input string in the LHS string
    StrRhs      =       ''                                                                                      ! Setting the RHS string to an empty string
    return                                                                                                      ! Exiting the procedure
  end if                                                                                                        ! End of if case on initial separation character index
  if    ( (i_EscRHS) .and. (iStrSep_ini == len_trim(StrRhs)) )  then                                            ! If a RHS escape character exists and if the last character of the input string corresponds to the separation character
  Char1 =       StrRhs(iStrSep_ini:iStrSep_ini)                                                                 ! Storing the last character
    do iEscRHS = 1,size(EscRHS)                                                                                 ! Loop on all RHS escape character
      if        (Char1 /= EscRHS(iEscRHS))      cycle                                                                   ! Going to the next RHS escape character if the last character from the input string does not correspond to the current RHS escape character
        StrLhs  =       StrRhs                                                                                  ! Storing the input string in the LHS string
        StrRhs  =       ''                                                                                      ! Setting the RHS string to an empty string
        return                                                                                                  ! Exiting the procedure
    end do                                                                                                      ! End loop on RHS escape character
  end if                                                                                                        ! End of if case on initial separation character index

  StrInp        =       StrRhs                                                                                  ! Storing the input string in a new variable
  StrLhs        =       ''                                                                                      ! Initialization of LHS string
  StrRhs        =       ''                                                                                      ! Initialization of RHS string
  iStrLhs       =       0                                                                                       ! Initialization of LHS string index
  iStrRhs       =       0                                                                                       ! Initialization of RHS string index
  iStrInp       =       0                                                                                       ! Initialization of input string index
  i_protect     =       .False.                                                                                 ! Initialization of the character protection indicator

  do                                                                                                            ! Infinit loop on all character of the input string
    iStrInp     =       iStrInp +       1                                                                       ! Incrementation of the input string index
    if  (iStrInp > len_trim(StrInp))    exit                                                                    ! Exiting the loop if the index is greater than the string length
    Char1       =       StrInp(iStrInp:iStrInp)                                                                 ! Storing the current character
    if  (i_EscLHS)      then                                                                                    ! If an escape character exists, then
      if        (i_protect)     then                                                                            ! If the current character is protected, then
        i_protect               =       .False.                                                                 ! Setting off the character protection mode
        iStrLhs                 =       iStrLhs + 1                                                             ! Index incrementation
        StrLhs(iStrLhs:iStrLhs) =       Char1                                                                   ! Storing the current character in the LHS string
        cycle                                                                                                   ! Going to the next character
      end if                                                                                                    ! End of if case on character protection
      if        (Char1 == EscLHS)       then                                                                    ! If the current character corresponds to the escape character, then
        iStrLhs                 =       iStrLhs + 1                                                             ! Index incrementation
        StrLhs(iStrLhs:iStrLhs) =       Char1                                                                   ! Storing the current character in the LHS string
        i_protect               =       .True.                                                                  ! Activating the character protection mode
        cycle                                                                                                   ! Going to the next character
      end if                                                                                                    ! End of if case on escape character
    end if                                                                                                      ! End of if case on existence of an escape character
    if  (i_EscRHS)      then                                                                                    ! If an RHS escape character exists, then
      if        (iStrInp == iStrSep_ini)        then                                                            ! If the current input string index correspond to the initial separation index
        Char1_Next      =       StrInp(iStrInp+1:iStrInp+1)                                                     ! Storing the value of the next character
        do iEscRHS = 1,size(EscRHS)                                                                                     ! Loop on all RHS escape characters
          if    (Char1_Next == EscRHS(iEscRHS)) then                                                            ! If the next character corresponds to the RHS escape character, then
             iStrSep_ini        =       iStrSep_ini + index( StrInp(iStrInp+1:), StrSep )                       ! Modyfiying the index of initial position of the separation character in the input character: Moving to the next separation character index
             iStrSep_fin        =       iStrSep_ini + len_trim(StrSep) - 1                                      ! Modyfiying the index of final   position of the separation character in the input character
          end if                                                                                                ! End of if case on escape character
        end do                                                                                                  ! End loop on RHS escape characters
      end if                                                                                                    ! End if case on input string index
    end if                                                                                                      ! End of if case on RHS escape indicator
    if  (iStrInp < iStrSep_ini) then                                                                            ! If the current input string index is lower than the initial separation index
      iStrLhs                   =       iStrLhs + 1                                                             ! Index incrementation
      StrLhs(iStrLhs:iStrLhs)   =       Char1                                                                   ! Storing the current character in the LHS string
      cycle                                                                                                     ! Going to the next character
    end if                                                                                                      ! End if case on input string index
    if  (iStrInp > iStrSep_fin) then                                                                            ! If the current input string index is greater than the final separation index
      iStrRhs                   =       iStrRhs + 1                                                             ! Index incrementation
      StrRhs(iStrRhs:iStrRhs)   =       Char1                                                                   ! Storing the current character in the LHS string
      cycle                                                                                                     ! Going to the next character
    end if                                                                                                      ! End if case on input string index
  end do                                                                                                        ! End loop on character of the input string
  StrRhs        =       adjustl(StrRhs)                                                                         ! Removing initial spaces

End Subroutine

!<==============================================================================================================
!> @brief       Removes spaces and tabulation
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure removes spaces and tabulation. \n
!<==============================================================================================================
Pure Function RemoveSpace ( StrInp )    result(StrOut)
  character(*)                                          ,intent(in)     ::  StrInp                          !< Input character string
  character(len(StrInp))                                                ::  StrOut                          ! Output character string
  integer                                                               ::  i, k                            ! Character index
  integer                                                               ::  i_ASCII                         ! Interger ASCII code for a given characters
  integer       ,parameter      ::  i_ASCII_Space   =       32
  StrOut        =       ''                                                                                      ! Initialization of the output string
  k             =       0                                                                                       ! Initialization of the character index
  do i = 1,len_trim(StrInp)                                                                                     ! Loop on all string characters
    i_ASCII     =       iachar(StrInp(i:i))                                                                     ! Getting the code for the ASCII character of the current character
    if  (i_ASCII == i_ASCII_Space)      cycle                                                                   ! If the character is a space, then going to the next character
    k           =       k + 1                                                                                   ! Incrementation of the output character string index
    StrOut(k:k) =       StrInp(i:i)                                                                             ! Setting the current charatcer to the input character
  end do                                                                                                        ! End loop on string characters
End Function


Subroutine Allocate_Data( Nlines, Npoint, X_1D, Y_1D, X_2D, Y_2D, LineTitle, LS_Color, LS_Type, LS_Width, CB_Values, NPoints )
  use GPF_Parameters            ,only:  rkp
  integer                                                       ,intent(in)     ::  Nlines          !< Number of lines to be plotted
  integer                                                       ,intent(in)     ::  Npoint          !< Maximum number of point per line
  real(rkp)     ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  X_1D            !<
  real(rkp)     ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  Y_1D            !<
  real(rkp)     ,dimension(:,:) ,allocatable          ,optional ,intent(inout)  ::  X_2D            !<
  real(rkp)     ,dimension(:,:) ,allocatable          ,optional ,intent(inout)  ::  Y_2D            !<
  character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LineTitle       !<
  character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LS_Color       !<
  character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LS_Type       !<
  character(*)  ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  LS_Width
  real(rkp)     ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  CB_Values
  integer       ,dimension(:)   ,allocatable          ,optional ,intent(inout)  ::  NPoints

  if ( present(X_1D) ) then
    if ( allocated(X_1D) ) deallocate( X_1D )
    allocate( X_1D(Npoint) )
  end if

  if ( present(Y_1D) ) then
    if ( allocated(Y_1D) ) deallocate( Y_1D )
    allocate( Y_1D(Npoint) )
  end if

  if ( present(X_2D) ) then
    if ( allocated(X_2D) ) deallocate( X_2D )
    allocate( X_2D(Nlines,Npoint) )
  end if

  if ( present(Y_2D) ) then
    if ( allocated(Y_2D) ) deallocate( Y_2D )
    allocate( Y_2D(Nlines,Npoint) )
  end if

  if ( present(LineTitle) ) then
    if ( allocated(LineTitle) ) deallocate( LineTitle )
    allocate( LineTitle(Nlines) )
  end if

  if ( present(LS_Color) ) then
    if ( allocated(LS_Color) ) deallocate( LS_Color )
    allocate( LS_Color(Nlines) )
  end if

  if ( present(LS_Type) ) then
    if ( allocated(LS_Type) ) deallocate( LS_Type )
    allocate( LS_Type(Nlines) )
  end if

  if ( present(LS_Width) ) then
    if ( allocated(LS_Width) ) deallocate( LS_Width )
    allocate( LS_Width(Nlines) )
  end if

  if ( present(CB_Values) ) then
    if ( allocated(CB_Values) ) deallocate( CB_Values )
    allocate( CB_Values(Nlines) )
  end if

  if ( present(NPoints) ) then
    if ( allocated(NPoints) ) deallocate( NPoints )
    allocate( NPoints(Nlines) )
  end if

End Subroutine



! This procedure extract a label from a given string under the following conditions:
!  - the label which is extracted must be contained between the character "
!  - the last character in the string must be the closing "
! For example:
!       String  =       set xlabel "x [m]"
!       Label   =       x [m]
! @TODO: The search is the label coulb be grealty improved.
Function Extract_Label( String ) result(Label)
  character(*)                                          ,intent(in)     ::  String
  character(:)  ,allocatable                                            ::  Label
  integer                                                               ::  iIni                            ! Initial index of the label to be extracted from String
  integer                                                               ::  iFin                            ! Final index of the label to be extracted from String
  iIni  =       index(String,'"') + 1                                                                           ! Getting the initial index of the label
  iFin  =       len_trim(String) - 1                                                                            ! Getting the final index of the label
  Label =       String(iIni:iFin)                                                                               ! Extracting the label from the string
  if ( iIni == 0 ) Label = ""
End Function

Function Add_Apostroph( Str_Inp ) result(Str_Out)
  character(*)                                          ,intent(in)     ::  Str_Inp                         !< Character string to be modified
  character(:)  ,allocatable                                            ::  Str_Out                         !< Character string with the " character at the begining and at the end
  integer                                                               ::  i                               ! Index of character
  character(*)  ,parameter      ::  Chara           =       '"'
  character(*)  ,parameter      ::  Replace_Str     =       "'"
  Str_Out       =       Str_Inp
  do
    i   =       index(Str_Inp,Chara)
    if ( i == 0 ) exit
    Str_Out(i:i)        =       Replace_Str
  end do
  Str_Out       =       '"' // Str_Out // '"'
End Function




! REMARK:
! This procedure converts a integer number into a character string.
! Using character length allocation, the output character has a length wich exactly fits the number of digits
! of the input integer.
! An optional input argument <LeadingZeros> can be supplied to the procedure in order to force the number
! of leading zeros in the output character string.
! For example, using the Convert_To_String interface, one obtains the following output strings for the
! above calling sequences:
!       call Convert_To_String( 50, LeadingZeros=4 )    =>  "0050"
!       call Convert_To_String( 50 )                    =>  "50"
Recursive Function Integer_To_String_0D( Num, LeadingZeros ) result(Str)
  integer                                               ,intent(in)     ::  Num                             !< Number to be converted into a string
  integer                                     ,optional ,intent(in)     ::  LeadingZeros                    !< Number of leading zeros
  character(:)  ,allocatable                                            ::  Str                             !< Character corresponding to the input number
  character(100)                                                        ::  Str_Loc                         ! Local character string required to store the number of a very large string before allocation of the output variable
!   character(:)  ,allocatable                                            ::  Fmt_Loc
  character(20)   ::  Fmt_Loc

  character(:)  ,allocatable                                            ::  NZeros
  NZeros      =       ""
  if ( present(LeadingZeros) ) NZeros = "." // Convert_To_String(LeadingZeros)
  Fmt_Loc   =       "(i0" // NZeros // ")"
  write(Str_Loc,Fmt_Loc) Num                                                                                        ! Interger-to-string conversion
  Str   =       trim(adjustl(Str_Loc))                                                                          ! Storing the string in the output variable with the right number of characters
End Function

! Subroutine Integer_To_String_1D( Num, Str )
!   implicit none
!   integer               ,dimension(:)                   ,intent(in)     ::  Num                             !< Integer numbers to be converted into a strings
!   character(:)          ,dimension(:)   ,allocatable    ,intent(out)    ::  Str                             !< Character string corresponding to the input number
!   integer                                                               ::  NElements                       ! Number of elements
!   integer                                                               ::  Length                          ! Maximum length of the output character string
!   integer                                                               ::  iElt                            ! Element index
!   character(:)                          ,allocatable                    ::  fmt                             ! Format for integer to character conversion
!   NElements     =       size(Num)                                                                               ! Setting the number of elements
!   Length        =       floor(log10(real(maxval(Num)))) + 1                                                     ! Setting the maximum length
!   fmt           =       Get_Format_i(Length)                                                                    ! Setting the format for integer to character conversion
!   allocate( character(Length) :: Str(NElements) )                                                               ! Allocating length and dimension
!   do iElt = 1,NElements                                                                                         ! Loop on all Lines
!     write(Str(iElt),fmt) Num(iElt)                                                                              ! Converting column index from integer to string
!   end do                                                                                                        ! End loop Line
! End Subroutine

Function Real_To_String_0D( Num ) result(Str)
  real(rkp)                                             ,intent(in)     ::  Num                             !< Real number to be converted into a string
  character(:)  ,allocatable                                            ::  Str                             !< Character corresponding to the input number
  character(100)                                                        ::  Str_Loc                         ! Local character string required to store the number of a very large string before allocation of the output variable
  character(:)  ,allocatable                                            ::  Fmt
  Fmt   =       "(g0)"
  write(Str_Loc,Fmt) Num                                                                                        ! Interger-to-string conversion
  Str   =       Remove_Trailing_Zeros(trim(adjustl(Str_Loc)))                                                   ! Storing the string in the output variable with the right number of characters
End Function

Function String_To_Integer_0D( Str ) result(Num)
  character(*)                                          ,intent(in)     ::  Str                             !< Character strings to be converted into integer numbers
  integer                                                               ::  Num                             !< Numbers corresponding to the input numbers
  read(Str,*) Num                                                                                               ! Converting current element of the character string into an integer number
End Function

Function String_To_Integer_1D( Str ) result(Num)
  character(*)          ,dimension(:)                   ,intent(in)     ::  Str                             !< Character strings to be converted into integer numbers
  integer               ,dimension(size(Str))                           ::  Num                             !< Numbers corresponding to the input numbers
  integer                                                               ::  i                               ! Index if elements
  do i = 1,size(Str)                                                                                            ! Loop on all elements
    read(Str(i),*) Num(i)                                                                                       ! Converting current element of the character string into an integer number
  end do                                                                                                        ! End loop on elements
End Function

! REMARK:
! The input argument corresponds to a character string corresponding to a real number.
! This procedure will remove the trailling zeros from the fractional part of the real number.
! If no fractional part is found, then the input character string is unchanged

Function Remove_Trailing_Zeros( StrInp ) result(StrOut)
  character(*)                                          ,intent(in)     ::  StrInp
  character(:)  ,allocatable                                            ::  StrOut
  character(*)                                              ,parameter  ::  Separator_Int_Fra="."
  character(*)                                              ,parameter  ::  Separator_Fra_Exp="E"
  integer                                                               ::  iSpe
  character(:)  ,allocatable                                            ::  Integer_Part
  character(:)  ,allocatable                                            ::  Fractional_Part
  character(:)  ,allocatable                                            ::  Exponent_Part
  integer                                                               ::  i
  StrOut        =       UpperCase( StrInp )                                                                     ! The conversion to upper case is required to be sure that the exponent character is "E" and not "e"
  iSpe          =       index( StrOut, Separator_Int_Fra )                                                      ! Getting the index of the separation character between the integer and fractional parts (zero if any)
  if ( iSpe /= 0 ) then                                                                                         ! If a separation character is found
    Integer_Part        =       trim( StrOut(1:iSpe-1) )                                                        ! Getting the integer part of the real number, ie. all characters before the '.' character
    Fractional_Part     =       trim( StrOut(iSpe+1:)  )                                                        ! Getting the fractional part of the real number, ie. all characters after the '.' character
    Exponent_Part       =       ""                                                                              ! Initializing the exponent part
    iSpe          =       index( Fractional_Part, Separator_Fra_Exp )                                           ! Getting the index of the separation character between the fractional and exponent part (zero if any)
    if ( iSpe /= 0 ) then                                                                                       ! If a exponent part existe, then further splitting
      Exponent_Part     =       trim( Fractional_Part(iSpe+1:)  )                                               ! Getting the exponent part of the real number, ie. all characters after the 'E' character
      Fractional_Part   =       trim( Fractional_Part(1:iSpe-1)  )                                              ! Getting the fractional part of the real number, ie. all characters before the 'E' character
    end if                                                                                                      ! End if case on separation character presence
    if ( len_trim(Integer_Part) == 0 ) Integer_Part = "0"                                                       ! If no integer part (the input string has the format ".1234"), then explicitly setting the integer part is zero
    i   =       len(Fractional_Part) + 1                                                                        ! Initializing the character index to the length+1 of the fractional part string (beacause of backward processing of the string)
    do                                                                                                          ! Loop on all characters of the fractional part string
      i = i - 1                                                                                                 ! Incrementing the character index
      if ( i == 0 )                     exit                                                                    ! Exiting the loop if the 1st character the entire string has been processed, (that is, if i=0)
      if ( Fractional_Part(i:i)/="0" )  exit                                                                    ! Exiting the loop if current character does not correspond to a zero ("0")
      Fractional_Part(i:i)      =       " "                                                                     ! Replacing the zero character by a blank character
    end do                                                                                                      ! End loop on characters of the fractional part string
    Fractional_Part     =       trim( Fractional_Part )                                                         ! Removing trailling blanks
    if ( len_trim(Fractional_Part) == 0 ) Fractional_Part = "0"                                                 ! If the no fractional part (the input string has the format "X.000...000"), then setting it to zero so that it become "X.0"
    StrOut      =       Integer_Part                                                                            ! Reconstructing the character string from the integer ...
    if ( len_trim(Fractional_Part) /= 0 ) StrOut = StrOut // Separator_Int_Fra // Fractional_Part               ! ... the fractional part if required
    if ( len_trim(Exponent_Part) /= 0 )   StrOut = StrOut // Separator_Fra_Exp // Exponent_Part                 ! ... and the exponent part if required
  end if                                                                                                        ! End if case on separation character presence
End Function

Pure Function UpperCase (StrInp) result(StrOut)
  character(*)                  ,intent(in)     ::  StrInp                                                  !<
  character(len(StrInp))                        ::  StrOut
  integer                                       ::  i, ilen, ioffset, iquote, iav, iqc
  ilen          =       len_trim(StrInp)
  ioffset       =       iachar('A') - iachar('a')
  iquote        =       0
  StrOut        =       StrInp
  do i = 1,ilen
    iav=iachar(StrInp(i:i))
    if(iquote==0 .and. (iav==34 .or.iav==39)) then
      iquote    =       1
      iqc       =       iav
      cycle
    end if
    if(iquote==1 .and. iav==iqc) then
      iquote    =       0
    cycle
    end if
    if (iquote==1) cycle
    if(iav >= iachar('a') .and. iav <= iachar('z')) then
      StrOut(i:i)       =       achar(iav+ioffset)
    else
      StrOut(i:i)       =       StrInp(i:i)
    end if
  end do
End Function


Function Get_Extension( FileName ) result (Extension)
  character(*)                                          ,intent(in)     ::  FileName
  character(:)  ,allocatable                                            ::  Extension
  integer                                                               ::  iSeparator
  character(1)  ,parameter                                              ::  Separator='.'
  logical       ,parameter                                              ::  iBackward=.True.
  iSeparator    =       scan( FileName, Separator, Back=iBackward )
  if ( iSeparator == 0 ) then
    Extension   =       ''
  else
    Extension   =       FileName(iSeparator+1:)
  end if
End Function

Function Remove_Extension( FileName ) result (FileName_NoExt)
  character(*)                                          ,intent(in)     ::  FileName
  character(:)  ,allocatable                                            ::  FileName_NoExt
  integer                                                               ::  iSeparator
  character(1)  ,parameter                                              ::  Separator='.'
  logical       ,parameter                                              ::  iBackward=.True.
  iSeparator            =       scan( FileName, Separator, Back=iBackward )
  if ( iSeparator == 0 ) then
    FileName_NoExt      =       FileName
  else
    FileName_NoExt      =       FileName(:iSeparator-1)
  end if
End Function


End Module