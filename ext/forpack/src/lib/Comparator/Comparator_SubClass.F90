SubModule(Comparator_Class) Comparator_SubClass

  use Logger_Class          ,only:  Logger, LogLevel_DEBUG

  implicit none

  contains

Module Procedure InitializeComparator
  if ( present(ProcName)  ) This%ProcName   =   ProcName
  if ( present(AbsTol)    ) then
    This%AbsoluteDifference =   .True.
    This%AbsTol             =   AbsTol
  end if
  if ( present(RelTol)    ) then
    This%RelativeDifference =   .True.
    This%RelTol             =   RelTol
  end if
End Procedure

Module Procedure SetComparatorProperties
  if ( present(ProcName)  ) This%ProcName   =   ProcName
  if ( present(AbsTol)    ) then
    This%AbsoluteDifference =   .True.
    This%AbsTol             =   AbsTol
  end if
  if ( present(RelTol)    ) then
    This%RelativeDifference =   .True.
    This%RelTol             =   RelTol
  end if
End Procedure

Module Procedure CompareComparator

  use String_Library    ,only:  Convert_To_String

  real(REAL64)                                                            ::  Diff, Tol
  character(:)  ,allocatable                                              ::  Description_
  character(:)  ,allocatable                                              ::  Str, Res, Message
  character(15)                                                           ::  Number1, Number2
  logical                                                                 ::  Passed
!   type(String_Type)                                                       ::  Message

  call This%SetProperties(            &
              RelTol    =   RelTol,   &
              AbsTol    =   AbsTol,   &
              ProcName  =   ProcName  )


  if ( allocated(This%ProcName) ) then
    call Logger%Entering( This%ProcName )
  end if

  Description_  =   ""
  if ( present(Description) ) Description_ = Description // ": "

  Number1   =   Convert_To_String(Expected,Fmt="es15.8")
  Number2   =   Convert_To_String(Found,Fmt="es15.8")
  Message   =   Description_//"Expected = "//Number1//" Found = "//Number2
!   call Logger%Write( Description_ // "Expected = ", Expected, "Found = ", Found,  Fr="es15.8" )

!   call Logger%Write( "-> Abs. tolerence:  ", This%AbsTol, Fr="es15.8" )
!   call Logger%Write( "-> Rel. tolerence:  ", This%RelTol, Fr="es15.8" )

  if ( This%AbsoluteDifference ) then
    Tol     =   This%AbsTol
    Diff    =   abs(Expected-Found)

    Passed  =   ( Diff <= Tol )
    if (Passed) then
      Str   =   " <= "
      Res   =   ""
    else
      Str   =   " >= "
      Res   =   " <FAILED>"
    end if
    This%Passed   =   This%Passed .and. Passed
    Number1   =   Convert_To_String(Diff,Fmt="es15.8")
    Number2   =   Convert_To_String(Tol,Fmt="es15.8")
    Message   =   Message // " -> Abs. diff.: "//Number1//Str//Number2//Res
!     call Logger%Write( "-> Absolute difference: ", Diff, Str, Tol, Res, Fr="es15.8" )
  end if

  if ( This%RelativeDifference ) then
    Tol     =   This%RelTol
    if ( Expected /= 0.0_REAL64 ) then
      Diff  =   abs( (Expected-Found) / Expected )
    else
      Diff  =   abs( Expected-Found )
    end if
    Passed  =   ( Diff <= Tol )
    if (Passed) then
      Str   =   " <= "
      Res   =   ""
    else
      Str   =   " >= "
      Res   =   " <FAILED>"
    end if
    This%Passed   =   This%Passed .and. Passed
!     call Logger%Write( "-> Relative difference: ", Diff, Str, Tol, Res, Fr="es15.8" )
    Number1   =   Convert_To_String(Diff,Fmt="es15.8")
    Number2   =   Convert_To_String(Tol,Fmt="es15.8")
    Message   =   Message // " -> Rel. diff.: "//Number1//Str//Number2//Res
  end if

  call Logger%Write( Message )
!   call Logger%Write( Message%GetValue() )

  if ( allocated(This%ProcName) ) then
    call Logger%Exiting()
  end if

End Procedure

End SubModule