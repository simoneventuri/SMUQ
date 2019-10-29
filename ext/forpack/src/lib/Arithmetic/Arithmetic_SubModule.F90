SubModule(Arithmetic_Module) Arithmetic_SubModule

! TODO: Write the single precision version of the procedures

  implicit none

  real(REAL64)  ,parameter                      ::  Zero    =       0.0_8
  real(REAL64)  ,parameter                      ::  One     =       1.0_8
  real(REAL64)  ,parameter                      ::  Ten     =       10.0_8

  contains

! **************************************************************************************************************
!                               PROCEDURES FOR LINEAR SPACING
! **************************************************************************************************************

! This procedure returns a vector whose elements are evenly spaced on a linear scale from a min value, a max
! value and the number of points of the output vector.
Pure Module Function LinSpace_From_MinMaxNPoints( ValMin, ValMax, NPoints ) result(Vector)
  real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
  real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
  integer                                               ,intent(in)     ::  NPoints                         !< Number of points
  real(REAL64)  ,dimension(NPoints)                                     ::  Vector                          !< Vector of NPoints equally spaced values on a linear scale
  integer                                                               ::  i
  associate ( Step => ( ValMax - ValMin ) / ( Npoints - 1 ) )
    Vector      =       [ ( ValMin + (i-1) * Step , i=1,Npoints ) ]
  end associate
End Function

! This procedure returns a vector whose elements are evenly spaced on a linear scale from a min value, a max
! value and the step between each element.
Pure Module Function LinSpace_From_MinMaxStep( ValMin, ValMax, Step ) result(Vector)
  real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
  real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
  real(REAL64)                                          ,intent(in)     ::  Step                            !< Step
  real(REAL64)  ,dimension(floor((ValMax-ValMin)/Step)+1)               ::  Vector                          !< Vector of NPoints equally spaced values on a linear scale
  integer                                                               ::  i
  integer                                                               ::  NPoints
  NPoints       =       floor( (ValMax - ValMin) / Step ) + 1
  Vector        =       [ ( ValMin + (i-1) * Step , i=1,NPoints ) ]
End Function

Module Procedure Compute_LinSpace_INT8
# include "Compute_LinSpace_INT_Inline.F90"
End Procedure
Module Procedure Compute_LinSpace_INT16
# include "Compute_LinSpace_INT_Inline.F90"
End Procedure
Module Procedure Compute_LinSpace_INT32
# include "Compute_LinSpace_INT_Inline.F90"
End Procedure
Module Procedure Compute_LinSpace_INT64
# include "Compute_LinSpace_INT_Inline.F90"
End Procedure
Module Procedure Compute_LinSpace_REAL32
# include "Compute_LinSpace_REAL_Inline.F90"
End Procedure
Module Procedure Compute_LinSpace_REAL64
# include "Compute_LinSpace_REAL_Inline.F90"
End Procedure
Module Procedure Compute_LinSpace_REAL128
# include "Compute_LinSpace_REAL_Inline.F90"
End Procedure

!   integer                                                                       ::  i
!   integer                                                                       ::  NPoints_Loc
!   integer                                                                       ::  Min_Loc
!   integer                                                                       ::  Max_Loc
!   integer                                                                       ::  Step_Loc
!   Min_Loc    =       Zero
!   Max_Loc    =       One
!   Step_Loc      =       Ten
!   NPoints_Loc   =       10
!   if ( present(Min)  ) Min_Loc    =       Min
!   if ( present(Max)  ) Max_Loc    =       Max
!   if ( present(Step)    ) then
!     Step_Loc    =       Step
!     NPoints_Loc =       floor( real((Max_Loc - Min_Loc) / Step_Loc) ) + 1
!   end if
!   if ( present(NPoints) ) then
!     NPoints_Loc =       NPoints
!     Step_Loc    =       Zero
!     if ( NPoints_Loc /= 1 ) Step_Loc = ( Max_Loc - Min_Loc ) / ( NPoints_Loc - 1 )
!   end if
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!   allocate( Vector(NPoints_Loc), source = [ ( int(Min_Loc+(i-1)*Step_Loc,kind=INT8) , i=1,NPoints_Loc ) ] )
! #else
!   allocate( Vector, source = [ ( int(Min_Loc+(i-1)*Step_Loc,kind=INT8) , i=1,NPoints_Loc ) ] )
! #endif

! **************************************************************************************************************
!                               PROCEDURES FOR LOGARITHMIC SPACING
! **************************************************************************************************************

! This procedure return a vector whose elements are evenly spaced on a logarithmic scale from a min value, a max
! value and the number of points of the output vector.
Pure Module Function LogSpace_From_MinMaxNPoints( ValMin, ValMax, NPoints ) result(Vector)
  real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
  real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
  integer                                               ,intent(in)     ::  NPoints                         !< Number of points
  real(REAL64)  ,dimension(NPoints)                                     ::  Vector                          !< Vector of NPoints equally spaced values on a logarithmic scale
  integer ,parameter                                                    ::  Base=10
  Vector        =       LinSpace( log10(ValMin), log10(ValMax), NPoints )
  Vector        =       Base**Vector
End Function

! This procedure return a vector whose elements are evenly spaced on a logarithmic scale from a min value, a max
! value and the step between each element.
Pure Module Function LogSpace_From_MinMaxStep( ValMin, ValMax, Step ) result(Vector)
  real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
  real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
  real(REAL64)                                          ,intent(in)     ::  Step                            !< Step
  real(REAL64)  ,dimension(floor((ValMax-ValMin)/Step)+1)               ::  Vector                          !< Vector of NPoints equally spaced values on a logarithmic scale
  integer ,parameter                                                    ::  Base=10
  Vector        =       LinSpace( log10(ValMin), log10(ValMax), Step )
  Vector        =       Base**Vector
End Function

Pure Module Subroutine Compute_LogSpace( Vector, ValMin, ValMax, NPoints )
  real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
  real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
  integer                                               ,intent(in)     ::  NPoints                         !< Number of points
  real(REAL64)  ,dimension(:)   ,allocatable            ,intent(out)    ::  Vector                          !< Vector of NPoints equally spaced values on a logarithmic scale
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  allocate( Vector(NPoints), source = LogSpace( ValMin, ValMax, NPoints ) )
#else
  allocate( Vector, source = LogSpace( ValMin, ValMax, NPoints ) )
#endif
End Subroutine

! **************************************************************************************************************
!         PROCEDURES FOR GETTING THE NUMBER OF ELEMENT IN A VECTOR FROM MIN, MAX, STEP
! **************************************************************************************************************
Module Procedure GetNumberElements_INT8
  NumberOfElements  =   floor( real((ValMax-ValMin)/Step) ) + 1
End Procedure
Module Procedure GetNumberElements_INT16
  NumberOfElements  =   floor( real((ValMax-ValMin)/Step) ) + 1
End Procedure
Module Procedure GetNumberElements_INT32
  NumberOfElements  =   floor( real((ValMax-ValMin)/Step) ) + 1
End Procedure
Module Procedure GetNumberElements_INT64
  NumberOfElements  =   floor( real((ValMax-ValMin)/Step) ) + 1
End Procedure
Module Procedure GetNumberElements_REAL32
  NumberOfElements  =   floor( (ValMax - ValMin) / Step ) + 1
End Procedure
Module Procedure GetNumberElements_REAL64
  NumberOfElements  =   floor( (ValMax - ValMin) / Step ) + 1
End Procedure
Module Procedure GetNumberElements_REAL128
  NumberOfElements  =   floor( (ValMax - ValMin) / Step ) + 1
End Procedure




! **************************************************************************************************************
!                               PROCEDURE FOR ROUNDING NUMBERS
! **************************************************************************************************************

Pure Elemental Module Function RoundUp_REAL64(NbrInp,Decade) result(NbrOut)
  real(REAL64)                                          ,intent(in)     ::  NbrInp
  logical                                     ,optional ,intent(in)     ::  Decade
  real(REAL64)                                                          ::  NbrOut
  real(REAL64)                                                          ::  Base
  if ( present(Decade) ) then
    if ( Decade ) then
      NbrOut    =       10.0_8**(Get_Exponent(NbrInp) + 1 )
      return
    end if
  end if
  Base          =       Get_Base(NbrInp)
  NbrOut        =       Base * ceiling( NbrInp / Base )
End Function

Pure Elemental Module Function RoundDown_REAL64(NbrInp,Decade) result(NbrOut)
  real(REAL64)                                          ,intent(in)     ::  NbrInp
  logical                                     ,optional ,intent(in)     ::  Decade
  real(REAL64)                                                          ::  NbrOut
  real(REAL64)                                                          ::  Base
  Base          =       Get_Base(NbrInp)
  if ( present(Decade) ) then
    if ( Decade ) then
      NbrOut    =       Base
      return
    end if
  end if
  NbrOut        =       Base * floor( NbrInp / Base )
End Function

Pure Elemental Module Function Get_Exponent(NbrInp) result(Expo)
  real(REAL64)                                          ,intent(in)     ::  NbrInp
  integer                                                               ::  Expo
  if ( NbrInp /= Zero ) then
    Expo        =       floor(log10(abs(NbrInp)))
  else
    Expo        =       0
  end if
End Function

Pure Elemental Module Function Get_Base(NbrInp) result(Base)
  real(REAL64)                                          ,intent(in)     ::  NbrInp
  real(REAL64)                                                          ::  Base
  Base          =       10.0_8**Get_Exponent(NbrInp)
End Function

Pure Module Function Get_Number_Different_Decades( Variable, StrictlyPositive ) result(NDecades)
  real(REAL64)  ,dimension(:)                           ,intent(in)     ::  Variable
  logical                                     ,optional ,intent(in)     ::  StrictlyPositive
  integer                                                               ::  NDecades
  integer                                                               ::  i
  real(REAL64)                                                          ::  Current_Exponent
  real(REAL64)                                                          ::  Previous_Exponent
  logical                                                               ::  i_StrictlyPositive
  i_StrictlyPositive    =       .False.
  if ( present(StrictlyPositive) ) i_StrictlyPositive = StrictlyPositive
  NDecades              =       0
  do i = 1,size(Variable)
    Current_Exponent    =       Get_Exponent( Variable(i) )
    if ( (i_StrictlyPositive) .and. (Variable(i) <= Zero) ) cycle
    if ( NDecades == 0 ) then
      NDecades          =       1
      Previous_Exponent =       Current_Exponent
    else
      if ( Current_Exponent /= Previous_Exponent ) NDecades = NDecades + 1
      Previous_Exponent =       Current_Exponent
    end if
  end do
End Function

Pure Module Subroutine GetValues_At_Different_Decades( FullVar, SubVar, Position, StrictlyPositive )
  real(REAL64)  ,dimension(:)                           ,intent(in)     ::  FullVar
  real(REAL64)  ,dimension(:)   ,allocatable            ,intent(out)    ::  SubVar
  integer       ,dimension(:)   ,allocatable  ,optional ,intent(out)    ::  Position
  logical                                     ,optional ,intent(in)     ::  StrictlyPositive
  integer                                                               ::  NSubVar
  integer                                                               ::  i
  real(REAL64)                                                          ::  Current_Exponent
  real(REAL64)                                                          ::  Previous_Exponent
  logical                                                               ::  i_StrictlyPositive
  integer                                                               ::  iSubVar
  i_StrictlyPositive    =       .False.
  if ( present(StrictlyPositive) ) i_StrictlyPositive = StrictlyPositive
  NSubVar       =       Get_Number_Different_Decades( FullVar, StrictlyPositive )
  allocate( SubVar(NSubVar) )
  if ( present(Position) ) allocate( Position(NSubVar) )
  iSubVar       =       0
  if ( present(Position) ) Position(1) = 1
  do i = 1,size(FullVar)
    Current_Exponent    =       Get_Exponent( FullVar(i) )
    if ( (i_StrictlyPositive) .and. (FullVar(i) <= Zero) ) cycle
    if ( iSubVar == 0 ) then
      iSubVar           =       1
      SubVar(iSubVar)   =       FullVar(i)
      if ( present(Position) ) Position(iSubVar) = i
      Previous_Exponent =       Current_Exponent
    else
      if ( Current_Exponent /= Previous_Exponent ) then
        iSubVar         =       iSubVar + 1
        SubVar(iSubVar) =       FullVar(i)
        if ( present(Position) ) Position(iSubVar) = i
      end if
      Previous_Exponent   =       Current_Exponent
    end if
  end do
End Subroutine

Pure Module Function Get_Digit_Integer( i, Number ) result(Digit)
  integer                                               ,intent(in)     ::  i
  integer                                               ,intent(in)     ::  Number
  integer                                                               ::  Digit
  integer       ,parameter                                              ::  Ten=10
  if ( i > 0 ) then
    Digit       =       mod(Number,Ten**i) / Ten**(i-1)
  else
    Digit       =       0
  end if
End Function

! **************************************************************************************************************
!                                 SORTING PROCEDURE
! **************************************************************************************************************
Pure Module Subroutine Insertion_Sort_INT8( a, Sorted_Index, Reverse )
  integer(INT8) ,dimension(:)                           ,intent(inout)  ::  a
  integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
  logical                                     ,optional ,intent(in)     ::  Reverse
  integer(INT8)                                                         ::  temp
# include "Insertion_Sort_Inline.F90"
End Subroutine
Pure Module Subroutine Insertion_Sort_INT16( a, Sorted_Index, Reverse )
  integer(INT16),dimension(:)                           ,intent(inout)  ::  a
  integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
  logical                                     ,optional ,intent(in)     ::  Reverse
  integer(INT16)                                                        ::  temp
# include "Insertion_Sort_Inline.F90"
End Subroutine
Pure Module Subroutine Insertion_Sort_INT32( a, Sorted_Index, Reverse )
  integer(INT32),dimension(:)                           ,intent(inout)  ::  a
  integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
  logical                                     ,optional ,intent(in)     ::  Reverse
  integer(INT32)                                                        ::  temp
# include "Insertion_Sort_Inline.F90"
End Subroutine
Pure Module Subroutine Insertion_Sort_INT64( a, Sorted_Index, Reverse )
  integer(INT64),dimension(:)                           ,intent(inout)  ::  a
  integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
  logical                                     ,optional ,intent(in)     ::  Reverse
  integer(INT64)                                                        ::  temp
# include "Insertion_Sort_Inline.F90"
End Subroutine
Pure Module Subroutine Insertion_Sort_REAL32( a, Sorted_Index, Reverse )
  real(REAL32)  ,dimension(:)                           ,intent(inout)  ::  a
  integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
  logical                                     ,optional ,intent(in)     ::  Reverse
  real(REAL32)                                                          ::  temp
# include "Insertion_Sort_Inline.F90"
End Subroutine
Pure Module Subroutine Insertion_Sort_REAL64( a, Sorted_Index, Reverse )
  real(REAL64)  ,dimension(:)                           ,intent(inout)  ::  a
  integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
  logical                                     ,optional ,intent(in)     ::  Reverse
  real(REAL64)                                                          ::  temp
# include "Insertion_Sort_Inline.F90"
End Subroutine
Pure Module Subroutine Insertion_Sort_REAL128( a, Sorted_Index, Reverse )
  real(REAL128) ,dimension(:)                           ,intent(inout)  ::  a
  integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
  logical                                     ,optional ,intent(in)     ::  Reverse
  real(REAL128)                                                         ::  temp
# include "Insertion_Sort_Inline.F90"
End Subroutine

! Module Procedure Insertion_Sort_INT8
!   integer(INT8)                                                         ::  temp
! # include "Insertion_Sort_Inline.F90"
! End Procedure
! Module Procedure Insertion_Sort_INT16
!   integer(INT16)                                                        ::  temp
! # include "Insertion_Sort_Inline.F90"
! End Procedure
! Module Procedure Insertion_Sort_INT32
!   integer(INT32)                                                        ::  temp
! # include "Insertion_Sort_Inline.F90"
! End Procedure
! Module Procedure Insertion_Sort_INT64
!   integer(INT64)                                                        ::  temp
! # include "Insertion_Sort_Inline.F90"
! End Procedure
! Module Procedure Insertion_Sort_REAL32
!   real(REAL32)                                                          ::  temp
! # include "Insertion_Sort_Inline.F90"
! End Procedure
! Module Procedure Insertion_Sort_REAL64
!   real(REAL64)                                                          ::  temp
! # include "Insertion_Sort_Inline.F90"
! End Procedure
! Module Procedure Insertion_Sort_REAL128
!   real(REAL128)                                                          ::  temp
! # include "Insertion_Sort_Inline.F90"
! End Procedure

! **************************************************************************************************************
!                                 NORMALIZATION PROCEDURE
! **************************************************************************************************************
Pure Module Function Normalize_1D( Q ) result(Qn)
  real(REAL64)  ,dimension(:)                           ,intent(in)     ::  Q                               !< Variable to be normalized to unity
  real(REAL64)  ,dimension( size(Q) )                                   ::  Qn                              !< Normalized variable
  Qn            =       Q / sum(Q)                                                                              ! Normalisation
End Function

! **************************************************************************************************************
!                                PROCEDURES FOR TESTING IF A NUMBER IS FINITE
! **************************************************************************************************************

Pure Module Function IsFinite_0d( Variable ) result(Finite)
  use ieee_arithmetic   ,only:  ieee_Is_Finite
  class(*)                                              ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = ieee_Is_Finite(real(Variable,kind=REAL64))
    type is (integer(INT16)); Finite = ieee_Is_Finite(real(Variable,kind=REAL64))
    type is (integer(INT32)); Finite = ieee_Is_Finite(real(Variable,kind=REAL64))
    type is (integer(INT64)); Finite = ieee_Is_Finite(real(Variable,kind=REAL64))
    type is (real(REAL32));   Finite = ieee_Is_Finite(Variable)
    type is (real(REAL64));   Finite = ieee_Is_Finite(Variable)
    type is (real(REAL128));  Finite = ieee_Is_Finite(Variable)
    class default;            Finite = .False.
  end select
End Function

Pure Module Function IsFinite_1d( Variable ) result(Finite)
  use ieee_arithmetic   ,only:  ieee_Is_Finite
  class(*)       ,dimension(:)                          ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Module Function IsFinite_2d( Variable ) result(Finite)
  use ieee_arithmetic   ,only:  ieee_Is_Finite
  class(*)       ,dimension(:,:)                        ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Module Function IsFinite_3d( Variable ) result(Finite)
  use ieee_arithmetic   ,only:  ieee_Is_Finite
  class(*)       ,dimension(:,:,:)                      ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Module Function IsFinite_4d( Variable ) result(Finite)
  use ieee_arithmetic   ,only:  ieee_Is_Finite
  class(*)       ,dimension(:,:,:,:)                    ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Module Function IsFinite_5d( Variable ) result(Finite)
  use ieee_arithmetic   ,only:  ieee_Is_Finite
  class(*)       ,dimension(:,:,:,:,:)                  ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Module Function IsFinite_6d( Variable ) result(Finite)
  use ieee_arithmetic   ,only:  ieee_Is_Finite
  class(*)       ,dimension(:,:,:,:,:,:)                ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

Pure Module Function IsFinite_7d( Variable ) result(Finite)
  use ieee_arithmetic   ,only:  ieee_Is_Finite
  class(*)       ,dimension(:,:,:,:,:,:,:)              ,intent(in)     ::  Variable                        !< Variable to be tested
  logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
  select type (Variable)
    type is (integer(INT8));  Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT16)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT32)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (integer(INT64)); Finite = all( ieee_Is_Finite(real(Variable,kind=REAL64)) )
    type is (real(REAL32));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL64));   Finite = all( ieee_Is_Finite(Variable)                   )
    type is (real(REAL128));  Finite = all( ieee_Is_Finite(Variable)                   )
    class default;            Finite = .False.
  end select
End Function

! **************************************************************************************************************
!                                PROCEDURES FOR GETTING THE IEEE CLASS NAME OF A VARIABLE
! **************************************************************************************************************
! ! Pure
! Module Function Get_IEEE_Class_Name_REAL32( Variable ) result( Name )
!   use ieee_arithmetic
!   real(REAL32)                                          ,intent(in)     ::  Variable
!   character(:)  ,allocatable                                            ::  Name
! # include "Get_IEEE_Class_Name_Inline.F90"
! End Function

Module Procedure Get_IEEE_Class_Name_REAL64
  use ieee_arithmetic
# include "Get_IEEE_Class_Name_Inline.F90"
End Procedure

! ! Pure
! Module Function Get_IEEE_Class_Name_REAL128( Variable ) result( Name )
!   use ieee_arithmetic
!   real(REAL128)                                         ,intent(in)     ::  Variable
!   character(:)  ,allocatable                                            ::  Name
! # include "Get_IEEE_Class_Name_Inline.F90"
! End Function

! ! ! ! To go in ForPack
! ! ! ! Variables 'Input' and 'Map' should have the same dimension
! ! ! Pure Function PermuteIndex( Input, Map ) result( Output )
! ! !   real(rkp) ,dimension(:)                           ,intent(in)     ::  Input
! ! !   integer   ,dimension(:)                           ,intent(in)     ::  Map
! ! !   real(rkp) ,dimension(size(Input))                                 ::  Output
! ! !   integer                                                           ::  i, j
! ! !   do i = 1,size(Input)
! ! !     Output(i) =   Input(Map(i))
! ! !   end do
! ! ! End Function
! ! !



Module Procedure IsValueInRange
  use iso_fortran_env   ,only:  INT32, INT64, REAL32, REAL64, REAL128
  use String_Library    ,only:  ConvertVariableKind
  real(REAL64)                                                          ::  lt_, le_, gt_, ge_
  InRange   =   .True.
  if ( present(lt) ) call ConvertVariableKind( lt, lt_)
  if ( present(le) ) call ConvertVariableKind( le, le_)
  if ( present(gt) ) call ConvertVariableKind( gt, gt_)
  if ( present(ge) ) call ConvertVariableKind( ge, ge_)
  select type (Value)
    type is ( integer(INT32) )
      if ( present(lt) ) InRange = InRange .and. ( Value <  lt_ )
      if ( present(le) ) InRange = InRange .and. ( Value <= le_ )
      if ( present(gt) ) InRange = InRange .and. ( Value >  gt_ )
      if ( present(ge) ) InRange = InRange .and. ( Value >= ge_ )
    type is ( integer(INT64) )
      if ( present(lt) ) InRange = InRange .and. ( Value <  lt_ )
      if ( present(le) ) InRange = InRange .and. ( Value <= le_ )
      if ( present(gt) ) InRange = InRange .and. ( Value >  gt_ )
      if ( present(ge) ) InRange = InRange .and. ( Value >= ge_ )
    type is ( real(REAL32) )
      if ( present(lt) ) InRange = InRange .and. ( Value <  lt_ )
      if ( present(le) ) InRange = InRange .and. ( Value <= le_ )
      if ( present(gt) ) InRange = InRange .and. ( Value >  gt_ )
      if ( present(ge) ) InRange = InRange .and. ( Value >= ge_ )
    type is ( real(REAL64) )
      if ( present(lt) ) InRange = InRange .and. ( Value <  lt_ )
      if ( present(le) ) InRange = InRange .and. ( Value <= le_ )
      if ( present(gt) ) InRange = InRange .and. ( Value >  gt_ )
      if ( present(ge) ) InRange = InRange .and. ( Value >= ge_ )
    type is ( real(REAL128) )
      if ( present(lt) ) InRange = InRange .and. ( Value <  lt_ )
      if ( present(le) ) InRange = InRange .and. ( Value <= le_ )
      if ( present(gt) ) InRange = InRange .and. ( Value >  gt_ )
      if ( present(ge) ) InRange = InRange .and. ( Value >= ge_ )
  end select
End Procedure


End SubModule