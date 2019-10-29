Module FitterConstrain_Class

  use iso_fortran_env       ,only:  rkp => REAL64
  use FitterInterval_Class  ,only:  FitterInterval_Type

  implicit none

  private
  public    ::  FitterConstrain_Type

  Type                                                ::  FitterConstrain_Type
    integer                                           ::  Index
    integer                                           ::  iIniGlo                                             ! Index of the first element of the component 'C' in the global constrain matrix C(i,:), i being the index of current constrain
    integer                                           ::  iFinGlo                                             ! Index of the last  element of the component 'C' in the global constrain matrix C(i,:), i being the index of current constrain
    integer                                           ::  N                                                   ! Local number of unknowns, ie. number of unknowns associated to the current Constrain object. Corresponds to the 'global' vector 'iIniGlo:iFinGlo'
    real(rkp)     ,dimension(:) ,allocatable          ::  C                                                   ! Contribution of current constrain to the LHS constrain matrix C x = d (One line of the matrix C)
    real(rkp)     ,allocatable                        ::  d                                                   ! Contribution of current constrain to the RHS constrain vector C x = d (One element of the vector d)
    real(rkp)                                         ::  x                                                   ! x-coordinate where the constrain is being applied
  contains
    procedure ,public   ::  Initialize_Value      =>    Initialize_Constrain_Value
    procedure ,public   ::  Initialize_Derivative =>    Initialize_Constrain_Derivative
    procedure ,public   ::  Update_C_d
    procedure ,private  ::  Initialize_Constrain_Parameters
  End Type

  Interface
    Module Subroutine Initialize_Constrain_Value( This, IntervalIndex, Intervals, Debug )
      class(FitterConstrain_Type)                           ,intent(out)    ::  This                              !< Passed-object dummy argument
      integer   ,dimension(2)                               ,intent(in)     ::  IntervalIndex                     !< Index of the 2 intervals associated to the constrain to be constructed
      type(FitterInterval_Type) ,dimension(:)               ,intent(in)     ::  Intervals                         !< Array of Intervals
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
    Module Subroutine Initialize_Constrain_Derivative( This, IntervalIndex, Intervals, Debug )
      class(FitterConstrain_Type)                           ,intent(out)    ::  This                              !< Passed-object dummy argument
      integer   ,dimension(2)                               ,intent(in)     ::  IntervalIndex                     !< Index of the 2 intervals associated to the constrain to be constructed
      type(FitterInterval_Type)     ,dimension(:)           ,intent(in)     ::  Intervals                         !< Array of Intervals
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
    Module Subroutine Initialize_Constrain_Parameters( This, IntervalIndex, Intervals, Debug )
      class(FitterConstrain_Type)                           ,intent(out)    ::  This                              !< Passed-object dummy argument
      integer   ,dimension(2)                               ,intent(in)     ::  IntervalIndex                     !< Index of the 2 intervals associated to the constrain to be constructed
      type(FitterInterval_Type)     ,dimension(:)           ,intent(in)     ::  Intervals                         !< Array of Intervals
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
    Pure Module Subroutine Update_C_d( This, C, d )
      class(FitterConstrain_Type)                           ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:,:)                         ,intent(inout)  ::  C                                 !
      real(rkp)     ,dimension(:)                           ,intent(inout)  ::  d                                 !
    End Subroutine
  End Interface

End Module