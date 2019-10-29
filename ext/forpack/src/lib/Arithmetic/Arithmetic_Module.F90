Module Arithmetic_Module

  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  private
  public  ::  LinSpace, Compute_LinSpace
  public  ::  LogSpace, Compute_LogSpace
  public  ::  GetNumberElements
  public  ::  Get_Exponent
  public  ::  Get_Number_Different_Decades
  public  ::  GetValues_At_Different_Decades
  public  ::  Insertion_Sort
  public  ::  Get_Digit
  public  ::  Get_Base
  public  ::  RoundUp
  public  ::  RoundDown
  public  ::  Normalize
  public  ::  IsFinite
  public  ::  Get_IEEE_Class_Name
  public  ::  IsValueInRange

  public  ::  cbrt
  public  ::  InterpLinear
!   public  ::  Point_In_Polygon

  Interface             Get_Digit
    Module procedure    Get_Digit_Integer
  End Interface

  Interface             RoundUp
    Module procedure    RoundUp_REAL64
  End Interface

  Interface             RoundDown
    Module procedure    RoundDown_REAL64
  End Interface

  Interface             Normalize
    Module procedure    Normalize_1D
  End Interface

  Interface             Compute_LinSpace
    Module Procedure    Compute_LinSpace_INT8, Compute_LinSpace_INT16, Compute_LinSpace_INT32, Compute_LinSpace_INT64
    Module Procedure    Compute_LinSpace_REAL32, Compute_LinSpace_REAL64, Compute_LinSpace_REAL128
  End Interface

  Interface             LinSpace
    Module Procedure    LinSpace_From_MinMaxStep
    Module Procedure    LinSpace_From_MinMaxNPoints
  End Interface

  Interface             LogSpace
    Module Procedure    LogSpace_From_MinMaxStep
    Module Procedure    LogSpace_From_MinMaxNPoints
  End Interface

  Interface             GetNumberElements
    Module Procedure    GetNumberElements_INT8,   GetNumberElements_INT16,  GetNumberElements_INT32, GetNumberElements_INT64
    Module Procedure    GetNumberElements_REAL32, GetNumberElements_REAL64, GetNumberElements_REAL128
  End Interface

  Interface             Insertion_Sort
    Module Procedure    Insertion_Sort_INT8, Insertion_Sort_INT16, Insertion_Sort_INT32, Insertion_Sort_INT64
    Module Procedure    Insertion_Sort_REAL32, Insertion_Sort_REAL64, Insertion_Sort_REAL128
  End Interface

  Interface             IsFinite
    Module Procedure    IsFinite_0d, IsFinite_1d, IsFinite_2d, IsFinite_3d, IsFinite_4d, IsFinite_5d, IsFinite_6d, IsFinite_7d
  End Interface

  Interface             Get_IEEE_Class_Name
!     Module Procedure    Get_IEEE_Class_Name_REAL32
    Module Procedure    Get_IEEE_Class_Name_REAL64
!     Module Procedure    Get_IEEE_Class_Name_REAL128
  End Interface

  Interface             InterpLinear
    Module Procedure    InterpLinear_0D,        InterpLinear_1D
  End Interface


  Interface
    ! **************************************************************************************************************
    !                               PROCEDURES FOR LINEAR SPACING
    ! **************************************************************************************************************
    Pure Module Function LinSpace_From_MinMaxNPoints( ValMin, ValMax, NPoints ) result(Vector)
      real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
      real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
      integer                                               ,intent(in)     ::  NPoints                         !< Number of points
      real(REAL64)  ,dimension(NPoints)                                     ::  Vector                          !< Vector of NPoints equally spaced values on a linear scale
    End Function
    Pure Module Function LinSpace_From_MinMaxStep( ValMin, ValMax, Step ) result(Vector)
      real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
      real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
      real(REAL64)                                          ,intent(in)     ::  Step                            !< Step
      real(REAL64)  ,dimension(floor((ValMax-ValMin)/Step)+1)               ::  Vector                          !< Vector of NPoints equally spaced values on a linear scale
    End Function
    Pure Module Subroutine Compute_LinSpace_INT8( Vector, Min, Max, Step, NPoints )
      integer(INT8)     ,dimension(:)   ,allocatable        ,intent(out)    ::  Vector
      integer(INT8)                               ,optional ,intent(in)     ::  Min
      integer(INT8)                               ,optional ,intent(in)     ::  Max
      integer(INT8)                               ,optional ,intent(in)     ::  Step
      integer                                     ,optional ,intent(in)     ::  NPoints
    End Subroutine
    Pure Module Subroutine Compute_LinSpace_INT16( Vector, Min, Max, Step, NPoints )
      integer(INT16)    ,dimension(:)   ,allocatable        ,intent(out)    ::  Vector
      integer(INT16)                              ,optional ,intent(in)     ::  Min
      integer(INT16)                              ,optional ,intent(in)     ::  Max
      integer(INT16)                              ,optional ,intent(in)     ::  Step
      integer                                     ,optional ,intent(in)     ::  NPoints
    End Subroutine
    Pure Module Subroutine Compute_LinSpace_INT32( Vector, Min, Max, Step, NPoints )
      integer(INT32)    ,dimension(:)   ,allocatable        ,intent(out)    ::  Vector
      integer(INT32)                              ,optional ,intent(in)     ::  Min
      integer(INT32)                              ,optional ,intent(in)     ::  Max
      integer(INT32)                              ,optional ,intent(in)     ::  Step
      integer                                     ,optional ,intent(in)     ::  NPoints
    End Subroutine
    Pure Module Subroutine Compute_LinSpace_INT64( Vector, Min, Max, Step, NPoints )
      integer(INT64)    ,dimension(:)   ,allocatable        ,intent(out)    ::  Vector
      integer(INT64)                              ,optional ,intent(in)     ::  Min
      integer(INT64)                              ,optional ,intent(in)     ::  Max
      integer(INT64)                              ,optional ,intent(in)     ::  Step
      integer                                     ,optional ,intent(in)     ::  NPoints
    End Subroutine
    Pure Module Subroutine Compute_LinSpace_REAL32( Vector, Min, Max, Step, NPoints )
      real(REAL32)      ,dimension(:)   ,allocatable        ,intent(out)    ::  Vector
      real(REAL32)                                ,optional ,intent(in)     ::  Min
      real(REAL32)                                ,optional ,intent(in)     ::  Max
      real(REAL32)                                ,optional ,intent(in)     ::  Step
      integer                                     ,optional ,intent(in)     ::  NPoints
    End Subroutine
    Pure Module Subroutine Compute_LinSpace_REAL64( Vector, Min, Max, Step, NPoints )
      real(REAL64)      ,dimension(:)   ,allocatable        ,intent(out)    ::  Vector
      real(REAL64)                                ,optional ,intent(in)     ::  Min
      real(REAL64)                                ,optional ,intent(in)     ::  Max
      real(REAL64)                                ,optional ,intent(in)     ::  Step
      integer                                     ,optional ,intent(in)     ::  NPoints
    End Subroutine
    Pure Module Subroutine Compute_LinSpace_REAL128( Vector, Min, Max, Step, NPoints )
      real(REAL128)     ,dimension(:)   ,allocatable        ,intent(out)    ::  Vector
      real(REAL128)                               ,optional ,intent(in)     ::  Min
      real(REAL128)                               ,optional ,intent(in)     ::  Max
      real(REAL128)                               ,optional ,intent(in)     ::  Step
      integer                                     ,optional ,intent(in)     ::  NPoints
    End Subroutine

    ! **************************************************************************************************************
    !                               PROCEDURES FOR LOGARITHMIC SPACING
    ! **************************************************************************************************************
    Pure Module Function LogSpace_From_MinMaxNPoints( ValMin, ValMax, NPoints ) result(Vector)
      real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
      real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
      integer                                               ,intent(in)     ::  NPoints                         !< Number of points
      real(REAL64)  ,dimension(NPoints)                                     ::  Vector                          !< Vector of NPoints equally spaced values on a logarithmic scale
    End Function
    Pure Module Function LogSpace_From_MinMaxStep( ValMin, ValMax, Step ) result(Vector)
      real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
      real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
      real(REAL64)                                          ,intent(in)     ::  Step                            !< Step
      real(REAL64)  ,dimension(floor((ValMax-ValMin)/Step)+1)               ::  Vector                          !< Vector of NPoints equally spaced values on a logarithmic scale
    End Function
    Pure Module Subroutine Compute_LogSpace( Vector, ValMin, ValMax, NPoints )
      real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
      real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
      integer                                               ,intent(in)     ::  NPoints                         !< Number of points
      real(REAL64)  ,dimension(:)   ,allocatable            ,intent(out)    ::  Vector                          !< Vector of NPoints equally spaced values on a logarithmic scale
    End Subroutine

    ! **************************************************************************************************************
    !         PROCEDURES FOR GETTING THE NUMBER OF ELEMENT IN A VECTOR FROM MIN, MAX, STEP
    ! **************************************************************************************************************
    Pure Module Function GetNumberElements_INT8( ValMin, ValMax, Step ) result(NumberOfElements)
      integer(INT8)                                         ,intent(in)     ::  ValMin                          !< Initial value
      integer(INT8)                                         ,intent(in)     ::  ValMax                          !< Final value
      integer(INT8)                                         ,intent(in)     ::  Step                            !< Step
      integer                                                               ::  NumberOfElements                !< Number of elements
    End Function
    Pure Module Function GetNumberElements_INT16( ValMin, ValMax, Step ) result(NumberOfElements)
      integer(INT16)                                        ,intent(in)     ::  ValMin                          !< Initial value
      integer(INT16)                                        ,intent(in)     ::  ValMax                          !< Final value
      integer(INT16)                                        ,intent(in)     ::  Step                            !< Step
      integer                                                               ::  NumberOfElements                !< Number of elements
    End Function
    Pure Module Function GetNumberElements_INT32( ValMin, ValMax, Step ) result(NumberOfElements)
      integer(INT32)                                        ,intent(in)     ::  ValMin                          !< Initial value
      integer(INT32)                                        ,intent(in)     ::  ValMax                          !< Final value
      integer(INT32)                                        ,intent(in)     ::  Step                            !< Step
      integer                                                               ::  NumberOfElements                !< Number of elements
    End Function
    Pure Module Function GetNumberElements_INT64( ValMin, ValMax, Step ) result(NumberOfElements)
      integer(INT64)                                        ,intent(in)     ::  ValMin                          !< Initial value
      integer(INT64)                                        ,intent(in)     ::  ValMax                          !< Final value
      integer(INT64)                                        ,intent(in)     ::  Step                            !< Step
      integer                                                               ::  NumberOfElements                !< Number of elements
    End Function
    Pure Module Function GetNumberElements_REAL32( ValMin, ValMax, Step ) result(NumberOfElements)
      real(REAL32)                                          ,intent(in)     ::  ValMin                          !< Initial value
      real(REAL32)                                          ,intent(in)     ::  ValMax                          !< Final value
      real(REAL32)                                          ,intent(in)     ::  Step                            !< Step
      integer                                                               ::  NumberOfElements                !< Number of elements
    End Function
    Pure Module Function GetNumberElements_REAL64( ValMin, ValMax, Step ) result(NumberOfElements)
      real(REAL64)                                          ,intent(in)     ::  ValMin                          !< Initial value
      real(REAL64)                                          ,intent(in)     ::  ValMax                          !< Final value
      real(REAL64)                                          ,intent(in)     ::  Step                            !< Step
      integer                                                               ::  NumberOfElements                !< Number of elements
    End Function
    Pure Module Function GetNumberElements_REAL128( ValMin, ValMax, Step ) result(NumberOfElements)
      real(REAL128)                                         ,intent(in)     ::  ValMin                          !< Initial value
      real(REAL128)                                         ,intent(in)     ::  ValMax                          !< Final value
      real(REAL128)                                         ,intent(in)     ::  Step                            !< Step
      integer                                                               ::  NumberOfElements                !< Number of elements
    End Function

    ! **************************************************************************************************************
    !                               PROCEDURE FOR ROUNDING NUMBERS
    ! **************************************************************************************************************
    Pure Elemental Module Function RoundUp_REAL64(NbrInp,Decade) result(NbrOut)
      real(REAL64)                                          ,intent(in)     ::  NbrInp
      logical                                     ,optional ,intent(in)     ::  Decade
      real(REAL64)                                                          ::  NbrOut
    End Function
    Pure Elemental Module Function RoundDown_REAL64(NbrInp,Decade) result(NbrOut)
      real(REAL64)                                          ,intent(in)     ::  NbrInp
      logical                                     ,optional ,intent(in)     ::  Decade
      real(REAL64)                                                          ::  NbrOut
    End Function

    ! **************************************************************************************************************
    !                               PROCEDURE TO GET NUMBER INFORMATIONS
    ! **************************************************************************************************************
    Pure Elemental Module Function Get_Exponent(NbrInp) result(Expo)
      real(REAL64)                                          ,intent(in)     ::  NbrInp
      integer                                                               ::  Expo
    End Function
    Pure Elemental Module Function Get_Base(NbrInp) result(Base)
      real(REAL64)                                          ,intent(in)     ::  NbrInp
      real(REAL64)                                                          ::  Base
    End Function
    Pure Module Function Get_Number_Different_Decades( Variable, StrictlyPositive ) result(NDecades)
      real(REAL64)  ,dimension(:)                           ,intent(in)     ::  Variable
      logical                                     ,optional ,intent(in)     ::  StrictlyPositive
      integer                                                               ::  NDecades
    End Function
    Pure Module Subroutine GetValues_At_Different_Decades( FullVar, SubVar, Position, StrictlyPositive )
      real(REAL64)  ,dimension(:)                           ,intent(in)     ::  FullVar
      real(REAL64)  ,dimension(:)   ,allocatable            ,intent(out)    ::  SubVar
      integer       ,dimension(:)   ,allocatable  ,optional ,intent(out)    ::  Position
      logical                                     ,optional ,intent(in)     ::  StrictlyPositive
    End Subroutine
    Pure Module Function Get_Digit_Integer( i, Number ) result(Digit)
      integer                                               ,intent(in)     ::  i
      integer                                               ,intent(in)     ::  Number
      integer                                                               ::  Digit
    End Function

    ! **************************************************************************************************************
    !                                 SORTING PROCEDURE
    ! **************************************************************************************************************
    Pure Module Subroutine Insertion_Sort_INT8( a, Sorted_Index, Reverse )
      integer(INT8) ,dimension(:)                           ,intent(inout)  ::  a
      integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
      logical                                     ,optional ,intent(in)     ::  Reverse
    End Subroutine
    Pure Module Subroutine Insertion_Sort_INT16( a, Sorted_Index, Reverse )
      integer(INT16),dimension(:)                           ,intent(inout)  ::  a
      integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
      logical                                     ,optional ,intent(in)     ::  Reverse
    End Subroutine
    Pure Module Subroutine Insertion_Sort_INT32( a, Sorted_Index, Reverse )
      integer(INT32),dimension(:)                           ,intent(inout)  ::  a
      integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
      logical                                     ,optional ,intent(in)     ::  Reverse
    End Subroutine
    Pure Module Subroutine Insertion_Sort_INT64( a, Sorted_Index, Reverse )
      integer(INT64),dimension(:)                           ,intent(inout)  ::  a
      integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
      logical                                     ,optional ,intent(in)     ::  Reverse
    End Subroutine
    Pure Module Subroutine Insertion_Sort_REAL32( a, Sorted_Index, Reverse )
      real(REAL32)  ,dimension(:)                           ,intent(inout)  ::  a
      integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
      logical                                     ,optional ,intent(in)     ::  Reverse
    End Subroutine
    Pure Module Subroutine Insertion_Sort_REAL64( a, Sorted_Index, Reverse )
      real(REAL64)  ,dimension(:)                           ,intent(inout)  ::  a
      integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
      logical                                     ,optional ,intent(in)     ::  Reverse
    End Subroutine
    Pure Module Subroutine Insertion_Sort_REAL128( a, Sorted_Index, Reverse )
      real(REAL128) ,dimension(:)                           ,intent(inout)  ::  a
      integer       ,dimension( size(a) )         ,optional ,intent(out)    ::  Sorted_Index
      logical                                     ,optional ,intent(in)     ::  Reverse
    End Subroutine

    ! **************************************************************************************************************
    !                                 NORMALIZATION PROCEDURE
    ! **************************************************************************************************************
    Pure Module Function Normalize_1D( Q ) result(Qn)
      real(REAL64)  ,dimension(:)                           ,intent(in)     ::  Q                               !< Variable to be normalized to unity
      real(REAL64)  ,dimension( size(Q) )                                   ::  Qn                              !< Normalized variable
    End Function

    ! **************************************************************************************************************
    !                                PROCEDURES FOR TESTING IF A NUMBER IS FINITE
    ! **************************************************************************************************************
    Pure Module Function IsFinite_0d( Variable ) result(Finite)
      class(*)                                              ,intent(in)     ::  Variable                        !< Variable to be tested
      logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
    End Function
    Pure Module Function IsFinite_1d( Variable ) result(Finite)
      class(*)       ,dimension(:)                          ,intent(in)     ::  Variable                        !< Variable to be tested
      logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
    End Function
    Pure Module Function IsFinite_2d( Variable ) result(Finite)
      class(*)       ,dimension(:,:)                        ,intent(in)     ::  Variable                        !< Variable to be tested
      logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
    End Function
    Pure Module Function IsFinite_3d( Variable ) result(Finite)
      class(*)       ,dimension(:,:,:)                      ,intent(in)     ::  Variable                        !< Variable to be tested
      logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
    End Function
    Pure Module Function IsFinite_4d( Variable ) result(Finite)
      class(*)       ,dimension(:,:,:,:)                    ,intent(in)     ::  Variable                        !< Variable to be tested
      logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
    End Function
    Pure Module Function IsFinite_5d( Variable ) result(Finite)
      class(*)       ,dimension(:,:,:,:,:)                  ,intent(in)     ::  Variable                        !< Variable to be tested
      logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
    End Function
    Pure Module Function IsFinite_6d( Variable ) result(Finite)
      class(*)       ,dimension(:,:,:,:,:,:)                ,intent(in)     ::  Variable                        !< Variable to be tested
      logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
    End Function
    Pure Module Function IsFinite_7d( Variable ) result(Finite)
      class(*)       ,dimension(:,:,:,:,:,:,:)              ,intent(in)     ::  Variable                        !< Variable to be tested
      logical                                                               ::  Finite                          !< Indicator whether all elements of the input variable are finite
    End Function

    ! **************************************************************************************************************
    !                                PROCEDURES FOR GETTING THE IEEE CLASS NAME OF A VARIABLE
    ! **************************************************************************************************************
!     Module Function Get_IEEE_Class_Name_REAL32( Variable ) result( Name )
!       real(REAL32)                                          ,intent(in)     ::  Variable
!       character(:)  ,allocatable                                            ::  Name
!     End Function
    Module Function Get_IEEE_Class_Name_REAL64( Variable ) result( Name )
      real(REAL64)                                          ,intent(in)     ::  Variable
      character(:)  ,allocatable                                            ::  Name
    End Function
!     Module Function Get_IEEE_Class_Name_REAL128( Variable ) result( Name )
!       real(REAL128)                                         ,intent(in)     ::  Variable
!       character(:)  ,allocatable                                            ::  Name
!     End Function

    ! **************************************************************************************************************
    !                                PROCEDURES TO TEST IF A GIVEN VALUE IS WITH A GIVEN RANGE
    ! **************************************************************************************************************
    Pure Module Function IsValueInRange( Value, lt, le, gt, ge ) result(InRange)
      class(*)                                              ,intent(in)     ::  Value
      class(*)                                    ,optional ,intent(in)     ::  lt
      class(*)                                    ,optional ,intent(in)     ::  le
      class(*)                                    ,optional ,intent(in)     ::  gt
      class(*)                                    ,optional ,intent(in)     ::  ge
      logical                                                               ::  InRange
    End Function


    Pure Elemental Module Function cbrt(X)
      real(REAL64)                                          ,intent(in)     ::  X                               ! Input variable
      real(REAL64)                                                          ::  cbrt                            ! Cubic root of the input variable
    End Function

    Pure Module Function InterpLinear_0D( X_Old, Y_Old, X_New, i_Extrapolation ) result(Y_New)
      real(REAL64)  ,dimension( : )                         ,intent(in)     ::  X_Old                           ! Abscisse of old dataset
      real(REAL64)  ,dimension( : )                         ,intent(in)     ::  Y_Old                           ! Data in old dataset
      real(REAL64)                                          ,intent(in)     ::  X_New                           ! Abscisse of new mesh
      logical                                     ,optional ,intent(in)     ::  i_Extrapolation                 ! Extrapolation indicator
      real(REAL64)                                                          ::  Y_New                           ! Data interpolated from old to new dataset
    End Function

    Pure Module Function InterpLinear_1D( X_Old, Y_Old, X_New, i_Extrapolation ) result(Y_New)
      real(REAL64)  ,dimension( : )                         ,intent(in)     ::  X_Old                           ! Abscisse of old dataset
      real(REAL64)  ,dimension( : )                         ,intent(in)     ::  Y_Old                           ! Data in old dataset
      real(REAL64)  ,dimension( : )                         ,intent(in)     ::  X_New                           ! Abscisse of new mesh
      logical                                     ,optional ,intent(in)     ::  i_Extrapolation                 ! Extrapolation indicator
      real(REAL64)  ,dimension( size(X_New,1) )                             ::  Y_New                           ! Data interpolated from old to new dataset
    End Function



  End Interface

End Module