SubModule(Utilities_Library) Utilities_IsIncluded_SubModule

! Include file needed for: CONCAT
# include "forpack-include.inc"

  implicit none

  contains


! **************************************************************************************************************
! **************************************************************************************************************
!       PROCEDURES FOR CHECKING WHETHER A VARIABLE IS PRESENT IN A LIST OF VARIABLES
! **************************************************************************************************************
! **************************************************************************************************************

Module Procedure IsIncluded_LOG_0d
  integer                                                               ::  i
  Included    =   .False.
  do i = 1,size(List)
    if ( Var .neqv. List(i) ) cycle
    Included  =   .True.
    return
  end do
End Procedure

# define  _VarKind_         INT8
# include "IsIncluded_INT_0d-Inline.F90"

# define  _VarKind_         INT16
# include "IsIncluded_INT_0d-Inline.F90"

# define  _VarKind_         INT32
# include "IsIncluded_INT_0d-Inline.F90"

# define  _VarKind_         INT64
# include "IsIncluded_INT_0d-Inline.F90"

# define  _VarKind_         REAL32
# include "IsIncluded_REAL_0d-Inline.F90"

# define  _VarKind_         REAL64
# include "IsIncluded_REAL_0d-Inline.F90"

# define  _VarKind_         REAL128
# include "IsIncluded_REAL_0d-Inline.F90"

Module Procedure IsIncluded_CHAR_0d
  logical                                                               ::  CaseSensitive_
  integer                                                               ::  i                            ! Index of elements in the list of variable
  character(:)  ,allocatable                                            ::  Str1, Str2
  CaseSensitive_ =   .True.
  if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive
  Str1       =   trim(Var)
  if ( .Not.CaseSensitive_ ) Str1 = UpperCase( Str1 )
  Included    =   .False.                                                                                 ! Initialization of the variable presence indicator to false
  do i = 1,size(List)                                                                                       ! Loop on all variables elements
    Str2        =   trim(List(i))
    if ( .Not.CaseSensitive_ ) Str2 = UpperCase( Str2 )
    if ( Str1 /= Str2 ) cycle                                                                                   ! If current variable from the list is different from the searched variable, then going to the next element
    Included  =   .True.                                                                                  ! Setting presence indicator to true
    return                                                                                                      ! Exiting the procedure if no counting of occurence is required
  end do                                                                                                        ! End do loop on Vars
End Procedure



Pure Module Function IsIncluded_INT8_1d( Var, List ) result(Included)
  integer(INT8)                                         ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
  integer(INT8)                                         ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
  logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
  integer                                                               ::  i
  forall(i=1:size(Var)) Included(i) = IsIncluded( Var(i), List )
End Function

Pure Module Function IsIncluded_INT16_1d( Var, List ) result(Included)
  integer(INT16)                                        ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
  integer(INT16)                                        ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
  logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
  integer                                                               ::  i
  forall(i=1:size(Var)) Included(i) = IsIncluded( Var(i), List )
End Function

Pure Module Function IsIncluded_INT32_1d( Var, List ) result(Included)
  integer(INT32)                                        ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
  integer(INT32)                                        ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
  logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
  integer                                                               ::  i
  forall(i=1:size(Var)) Included(i) = IsIncluded( Var(i), List )
End Function

Pure Module Function IsIncluded_INT64_1d( Var, List ) result(Included)
  integer(INT64)                                        ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
  integer(INT64)                                        ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
  logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
  integer                                                               ::  i
  forall(i=1:size(Var)) Included(i) = IsIncluded( Var(i), List )
End Function

Pure Module Function IsIncluded_REAL32_1d( Var, List, Tol ) result(Included)
  real(REAL32)                                          ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
  real(REAL32)                                          ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
  real(REAL32)                                ,optional ,intent(in)     ::  Tol                             !< Tolerence
  logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
  integer                                                               ::  i
  forall(i=1:size(Var)) Included(i) = IsIncluded( Var(i), List, Tol )
End Function

Pure Module Function IsIncluded_REAL64_1d( Var, List, Tol ) result(Included)
  real(REAL64)                                          ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
  real(REAL64)                                          ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
  real(REAL64)                                ,optional ,intent(in)     ::  Tol                             !< Tolerence
  logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
  integer                                                               ::  i
  forall(i=1:size(Var)) Included(i) = IsIncluded( Var(i), List, Tol )
End Function

Pure Module Function IsIncluded_REAL128_1d( Var, List, Tol ) result(Included)
  real(REAL128)                                         ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
  real(REAL128)                                         ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
  real(REAL128)                               ,optional ,intent(in)     ::  Tol                             !< Tolerence
  logical               ,dimension(size(Var))                           ::  Included                        !< Array of presence indicator of the variables in the list of variables
  integer                                                               ::  i
  forall(i=1:size(Var)) Included(i) = IsIncluded( Var(i), List, Tol )
End Function

Pure Module Function IsIncluded_CHAR_1d( Var, List, CaseSensitive ) result(Included)
  character(*)                                          ,intent(in)     ::  Var(:)                          !< Array of variables to be checked for presence in the list of variable
  character(*)                                          ,intent(in)     ::  List(:)                         !< List of variable used for presence checking
  logical                                     ,optional ,intent(in)     ::  CaseSensitive                   !< Indicator whether the search should be case sensitive
  logical               ,dimension( size(Var) )                         ::  Included                        !< Array of presence indicator of the variables in the list of variables
  integer                                                               ::  i
  forall(i=1:size(Var)) Included(i) = IsIncluded( Var(i), List, CaseSensitive )
End Function





Pure Function UpperCase(StrInp) result(StrOut)
  character(*)                  ,intent(in)     ::  StrInp                                                  !<
  character(:)  ,allocatable                                            ::  StrOut                          ! Output character string
!       character(len(StrInp))                                                ::  StrOut                          ! Output character string

  integer                                       ::  i, ilen, ioffset, iquote, iav, iqc
  ilen          =   len_trim(StrInp)
  ioffset       =   iachar('A') - iachar('a')
  iquote        =   0
  StrOut        =   StrInp
  do i = 1,ilen
    iav=iachar(StrInp(i:i))
    if(iquote==0 .and. (iav==34 .or.iav==39)) then
      iquote    =   1
      iqc       =   iav
      cycle
    end if
    if(iquote==1 .and. iav==iqc) then
      iquote    =   0
    cycle
    end if
    if (iquote==1) cycle
    if(iav >= iachar('a') .and. iav <= iachar('z')) then
      StrOut(i:i)       =   achar(iav+ioffset)
    else
      StrOut(i:i)       =   StrInp(i:i)
    end if
  end do
End Function

End SubModule
