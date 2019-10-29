SubModule(Fitter_Class) Fitter_SubClass

  use Logger_Class      ,only:  Logger
  use Utilities_Library  ,only:  GetOptArgValue

  implicit none

  logical                                                               ::  DefaultDebug = .False.       !< Global debugging indicator

  contains

Module Procedure InitializeFitter

  character(*)                                              ,parameter  ::  ProcName='InitializeFitter'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) then
    call Logger%Write( "present( X_Tab   )  = ", present( X_Tab   ) )
    call Logger%Write( "present( Y_Tab   )  = ", present( Y_Tab   ) )
    call Logger%Write( "present( FitModel ) = ", present( FitModel ) )
    call Logger%Write( "present( X_Ini   )  = ", present( X_Ini   ) )
    call Logger%Write( "present( X_Fin   )  = ", present( X_Fin   ) )
    call Logger%Write( "present( iX_Ini  )  = ", present( iX_Ini  ) )
    call Logger%Write( "present( iX_Fin  )  = ", present( iX_Fin  ) )
  end if

  if ( present(X_Tab) ) then
    if (Dbg) call Logger%Write( "Calling This%SetXTab" )
    call This%SetXTab( X_Tab, Debug=.False. )
  end if

  if ( present(Y_Tab) ) then
    if (Dbg) call Logger%Write( "Calling This%SetYTab" )
    call This%SetYTab( Y_Tab, Debug=.False. )
  end if

  if (Dbg) call Logger%Write( "Calling This%SetIntervals" )
  call This%SetIntervals( Intervals=Intervals, FitModel=FitModel, Debug=Debug )

  if (Dbg) call Logger%Write( "Calling This%SetFitRange" )
  call This%SetFitRange( X_Ini, X_Fin, iX_Ini, iX_Fin , Debug=Dbg )

  if (Dbg) call Logger%Write( "Calling This%Initialize_Constrains" )
  call This%Initialize_Constrains( Debug )

  if (Dbg) call Logger%Exiting()

End Procedure

! Module Procedure SetFitterProperties
!
!   character(*)                                              ,parameter  ::  ProcName='SetFitterProperties'
!   logical                                                               ::  Dbg
!
!   Dbg   =   GetOptArgValue(DefaultDebug,Debug)
!   if (Dbg) call Logger%Entering( ProcName )
!
!   if ( present(X_Tab) ) then
!     if (Dbg) call Logger%Write( "Calling This%SetXTab" )
!     call This%SetXTab( X_Tab, Debug=.False. )
!   end if
!
!   if ( present(Y_Tab) ) then
!     if (Dbg) call Logger%Write( "Calling This%SetYTab" )
!     call This%SetYTab( Y_Tab, Debug=.False. )
!   end if
!
!   if (Dbg) call Logger%Write( "Calling This%SetIntervals" )
!   call This%SetIntervals( Intervals=Intervals, FitModel=FitModel, Debug=Debug )
!
!   if (Dbg) call Logger%Write( "Calling This%SetFitRange" )
!   call This%SetFitRange( X_Ini, X_Fin, iX_Ini, iX_Fin , Debug=Dbg )
!
!   if (Dbg) call Logger%Write( "Calling This%Initialize_Constrains" )
!   call This%Initialize_Constrains( Debug )
!
!   if (Dbg) call Logger%Exiting()
!
! End Procedure

! @TODO: If Y_Tab is allocated, check that X_Tab has the same dimension
Module Procedure SetXTab
  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName='SetXTab'
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  if ( allocated(This%X_Tab) ) deallocate( This%X_Tab )
  allocate( This%X_Tab, source = X_Tab )   ! @COMPILER_BUG:GFORTRAN                                             ! Allocating and setting the tabulated X-coordinates
  This%NPtsTab          =       size(This%X_Tab)                                                                ! Setting the number of point in the tabulated data
  if (Dbg) call Logger%Write( "This%NPtsTab = ", This%NPtsTab )
  if (Dbg) call Logger%Exiting()
End Procedure

! @TODO: If X_Tab is allocated, check that Y_Tab has the same dimension
Module Procedure SetYTab
  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName='SetYTab'
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  if ( allocated(This%Y_Tab) ) deallocate( This%Y_Tab )
  allocate( This%Y_Tab, source = Y_Tab )    ! @COMPILER_BUG:GFORTRAN                                            ! Allocating and setting the tabulated X-coordinates
  This%Defined_Y_Tab  =       .True.
  This%NPtsTab        =       size(This%Y_Tab)                                                                  ! Setting the number of point in the tabulated data
  if (Dbg) call Logger%Write( "This%NPtsTab = ", This%NPtsTab )
  if (Dbg) call Logger%Exiting()
End Procedure


Module Procedure SetModelParam

  use String_Library    ,only:  Convert_To_String
  use Error_Class       ,only:  Error

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName='SetModelParam'
  integer                                                               ::  i
  integer                                                               ::  iIni
  integer                                                               ::  iFin
  integer                                                               ::  Status

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( .Not. Allocated(This%Intervals) ) then
    allocate( This%Intervals(1) )
!     call Error%Raise( "The component 'This%Intervals' is not allocated", ProcName=ProcName )
  end if

  if ( present(Interval) ) then   ! Set only the paraneters associated to the input interval
    i = Interval
    if ( ( i < lbound(This%Intervals,1) ) .or. ( i > ubound(This%Intervals,1) ) ) then
      call Error%Raise( "The input interval is out-of-bound: Interval = "//Convert_To_String(i), ProcName=ProcName )
    end if
    iIni   =   i
    iFin   =   i
  else
    iIni   =   lbound(This%Intervals,1)
    iFin   =   ubound(This%Intervals,1)
  end if

! ==============================================================================================================
!     SETTING THE PARAMETERS IN THE 'FITMODEL' OBJECT AND MODIFYING THEM TO THE ORIGINAL SYSTEM
! ==============================================================================================================
! !   if (Dbg) call Logger%Write( "Setting the parameters in the 'fitmodel' object and modifying them to the original system" )
  do i = iIni,iFin                                                                                   ! Loop
    if ( .Not. Allocated(This%Intervals(i)%FitModel) ) &
    call Error%Raise( "The 'FitModel' component of the element i = "//Convert_To_String(i)//" of the interval array 'This%Intervals(:)' is not allocated", ProcName=ProcName )

    if (Dbg) call Logger%Write( "Calling This%Intervals(i)%FitModel%Set_Param for i = ", i, "Param = ", Param, F2="i3", F4="es9.2" )

    call This%Intervals(i)%FitModel%Set_Param( Param, Status )                                                          ! Puching the solution back into the FitModel
    if ( Status /= 0 ) call Error%Raise( "Error pushing parameters for interval i = "//Convert_To_String(i), ProcName=ProcName )
  end do                                                                                                        ! End loop on intervals
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure SetIntervals

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName='SetIntervals'
  integer                                                               ::  i

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) then
    call Logger%Write( "Input arguments:" )
    call Logger%Write( " - present(Intervals) = ", present(Intervals) )
    call Logger%Write( " - present(FitModel)  = ", present(FitModel) )
  end if

  if ( present(Intervals) ) then
    if (Dbg) call Logger%Write( "Setting the fitting intervals" )
    if (Dbg) call Logger%Write( "-> Intervals = ", Intervals, Fr="es15.8" )
    This%NIntervals    =     size(Intervals)  - 1
    if ( allocated(This%Intervals) ) deallocate( This%Intervals )
    allocate( This%Intervals(This%NIntervals) )
    do i = 1,This%NIntervals
      This%Intervals(i)%Index     =   i
      This%Intervals(i)%Xmin      =   Intervals(i)
      This%Intervals(i)%Xmax      =   Intervals(i+1)
      This%Intervals(i)%Bounded   =   .True.
    end do
  else
    This%NIntervals    =     1
    allocate( This%Intervals(This%NIntervals) )
  end if
  if (Dbg) call Logger%Write( "-> This%NIntervals = ", This%NIntervals )

  if (Dbg) call Logger%Write( "Setting the fitting models" )
  if (Dbg) call Logger%Write( "-> present(FitModel) = ", present(FitModel) )
  if ( present(FitModel) ) then
    do i = 1,This%NIntervals
      if (Dbg) call Logger%Write( "-> i = ", i, "Calling This%Intervals(i)%SetFitModel" )
      call This%Intervals(i)%SetFitModel( FitModel, Debug )
      if (Dbg) call Logger%Write( "-> i = ", i, "Calling This%Intervals(i)%Set_MappingLocalToGlobalUnknows" )
      call This%Intervals(i)%Set_MappingLocalToGlobalUnknows( Debug )
    end do
  end if

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetFitModel

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName='SetFitModel'
  integer                                                               ::  i

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( .Not. allocated(This%Intervals) ) then
    This%NIntervals    =     1
    allocate( This%Intervals(This%NIntervals) )
  end if

  do i = 1,This%NIntervals
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling This%Intervals(i)%SetFitModel" )
    call This%Intervals(i)%SetFitModel( FitModel, Debug )
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling This%Intervals(i)%Set_MappingLocalToGlobalUnknows" )
    call This%Intervals(i)%Set_MappingLocalToGlobalUnknows( Debug )
  end do

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure Initialize_Constrains

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName='Initialize_Constrains'
  integer                                                               ::  i, k
  integer   ,dimension(2)                                               ::  IntervalIndex
  integer                                                               ::  NConstrain_Value
  integer                                                               ::  NConstrain_Derivative

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

! ==============================================================================================================
!    SETTING THE NUMBER OF CONSTRAINS AND ALLOCATING THE CONSTRAINS COMPONENT
! ==============================================================================================================
! There are always 2 constrains for each interface between intervals, one to constrain the value of the function
! and another one to constrains the fonction derivative.
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting the number of constrains" )
  NConstrain_Value      =     (This%NIntervals - 1)                                                             ! Setting the number of constrains
  NConstrain_Derivative =     (This%NIntervals - 1)                                                             ! Setting the number of constrains
!   NConstrain_Derivative =     0
  This%NConstrains      =     NConstrain_Value + NConstrain_Derivative
  if ( allocated(This%Constrains) ) deallocate( This%Constrains )                                               ! Deallocation if required
  allocate( This%Constrains(This%NConstrains) )                                                                 ! Allocating the array of Constrain objects
  if (Dbg) then
    call Logger%Write( "-> Number of constrains on the function value:      ", NConstrain_Value )
    call Logger%Write( "-> Number of constrains on the function derivative: ", NConstrain_Derivative )
    call Logger%Write( "-> Total number of constrains:   This%NConstrains = ", This%NConstrains )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE CONSTAINS ASSOCIATED TO THE Module Function VALUES AT THE INTERVALS'S INTERFACES
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting the constains associated to the function values at the intervals's interfaces" )
  do i = 1,NConstrain_Value
  associate( Constrain => This%Constrains(i) )
    k     =   i
    IntervalIndex     =     [k,k+1]
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling Constrain%Initialize_Value for IntervalIndex = ", IntervalIndex )
    call Constrain%Initialize_Value( IntervalIndex, This%Intervals, Debug=Dbg )
    Constrain%Index   =   i
  end associate
  end do
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE CONSTAINS ASSOCIATED TO THE Module Function DERIVATIVES AT THE INTERVALS'S INTERFACES
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting the constains associated to the function derivatives at the intervals's interfaces" )
  do i = NConstrain_Value+1,This%NConstrains
  associate( Constrain => This%Constrains(i) )
    k     =   i - This%NIntervals + 1
    IntervalIndex     =     [k,k+1]
    if (Dbg) call Logger%Write( "-> i = ", i, "Calling Constrain%Initialize_Derivative for IntervalIndex = ", IntervalIndex )
    call Constrain%Initialize_Derivative( IntervalIndex, This%Intervals, Debug=Dbg )
    Constrain%Index   =   i
  end associate
  end do
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure FitData

  character(*)                                              ,parameter  ::  ProcName='FitData'
  logical                                                               ::  Dbg                     ! Local debugging indicator


  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if ( present(Y_Tab) ) then
    if (Dbg) call Logger%Write( "Calling This%SetYTab" )
    call This%SetYTab( Y_Tab, Debug=Dbg )
  end if

  if (Dbg) call Logger%Write( "Calling This%SolveLinearSystem" )
  call This%SolveLinearSystem( Debug=Dbg )                                                             ! Computing fitting coefficients
!
!   if (Dbg) call Logger%Write( "Calling This%Compute_Fit_Data" )
!   call This%Compute_Fit_Data( Debug=Dbg )                                                             ! Computing fitted data
!
!   if (Dbg) call Logger%Write( "Calling This%Compute_Fit_Error" )
!   call This%Compute_Fit_Error( Debug=Dbg )                                                            ! Computing fitting errors

  if ( present(Param) ) then
    if (Dbg) call Logger%Write( "Calling This%GetParameters" )
    Param   =   This%GetParameters()
  end if


  if ( present(Param_) ) then
  Block
    use Parameters_Library    ,only:  Zero
    integer                     ::  i
    real(rkp)   ,allocatable    ::  AllocParam(:)
    if (Dbg) call Logger%Write( "Calling This%GetParameters" )
    AllocParam  =   This%GetParameters()
    Param_(:)   =   Zero
    do i = 1,min( size(Param_), size(AllocParam) )
      Param_(i) =   AllocParam(i)
    end do
  End Block
  end if




  if (Dbg) call Logger%Exiting()

End Procedure



Module Procedure Output_Summary
  use String_Library    ,only:  Add_Line_To_String
  integer                                                               ::  i                               ! Index of the fit interval
  character(:)  ,allocatable                                            ::  LocalSummary
  do i = 1,This%NIntervals                                                                                    ! Loop on all intervals
    LocalSummary    =     This%Intervals(i)%GetSummary()
    call Add_Line_To_String( Summary, LocalSummary )
  end do                                                                                                      ! End loop on intervals
End Procedure


! ****************
! WRONG ****************
! ****************
Module Procedure GetParameters
  integer                                                               ::  NParam                          !<
  integer                                                               ::  i, iIni, iFin

  if ( present(Interval) )  then
    if ( ( Interval < lbound(This%Intervals,1) ) .or. ( Interval > ubound(This%Intervals,1) ) ) return
    associate( FitModel => This%Intervals(Interval)%FitModel )                                             ! Associating current fit model
      allocate( Param , source = FitModel%Get_Param() )
!       NParam      =     FitModel%Get_NParam()
!       allocate( Param(NParam) )
!       Param   =     FitModel%Get_Param()
    end associate                                                                                                 ! End of variable association for current grouped level

  else
    NParam    =     This%GetNumberOfParameters() ! Get the total number of parameters: Sum of all parameters over all intervals
    allocate( Param(NParam) )
    if ( NParam == 0 ) return
    iFin      =     0
    do i = 1,This%NIntervals                                                                                      ! Loop on all intervals
    associate( FitModel => This%Intervals(i)%FitModel )                                                           ! Associating current fit model
      iIni              =     iFin + 1
      iFin              =     iIni + FitModel%NParam - 1
      Param(iIni:iFin)  =     FitModel%Get_Param()                                                                ! Extracting the modified parameters back in the 'Solution' variable
    end associate                                                                                                 ! End of variable association for current grouped level
    end do
  end if

End Procedure

! ****************
! WRONG ****************
! ****************


Module Procedure Allocate_Matrix
  integer                                                               ::  NRow
  integer                                                               ::  NCol
  NRow        =     This%NIntervals                                                                             ! Setting the number number of rows or the matrix used to stored the Tmin/Tmax/Coef for each temperature interval
  NCol        =     This%GetNParamMaxPerInterval() + 2                                                          ! Setting the number number of columns: 7 (the coef for cp) + 2 (the 2 integration cst for h and s) + 2 (Tmin and Tmax)
  allocate( Matrix(NRow,NCol) )                                                                                 ! Allocating the matrix
  Matrix      =     0.0_rkp
End Procedure

Module Procedure Get_Matrix
  integer                                                               ::  i                                 ! Index of intervals
  integer                                                               ::  NParam                            ! Number of parameters on a given interval
  Matrix      =     0.0_rkp
  NParam      =     This%GetNParamMaxPerInterval()      ! Getting the maximum number of parameters
  if ( size(Matrix,1) < This%NIntervals ) return
  if ( size(Matrix,2) < NParam+2        ) return
  do i = 1,This%NIntervals                                                                                      ! Loop on all temperature intervals
  associate( Interval => This%Intervals(i) )
    if ( .Not. allocated(Interval%FitModel) ) cycle
    NParam                =     Interval%FitModel%Get_NParam()
    Matrix(i,1)           =     Interval%Xmin                                                                           ! Setting the value of the lower bound of current interval in the matrix
    Matrix(i,2)           =     Interval%Xmax                                                                           ! Setting the value of the upper bound of current interval in the matrix
    Matrix(i,3:NParam+2)  =     Interval%FitModel%Get_Param()
  end associate
  end do                                                                                                        ! End loop on temperature intervals
End Procedure

! Get the number of parameters associated to current Fitter object.
! If there are several intervals, then the number of parameters is
! to sum of the number of parameters associated to each interval
! If the "Interval" optional arguement is provided, then the returned
! number of parameters corresponds to the one of this specific interval
! only.
Module Procedure GetNumberOfParameters
  integer                                                               ::  i

  NParam    =   0
! Getting the number of parameters on the interval specified in input only
  if ( present(Interval) ) then
    i   =   Interval
    if ( ( i >= lbound(This%Intervals,1) ) .and. ( i <= ubound(This%Intervals,1) ) ) then
      if ( allocated(This%Intervals(i)%FitModel) ) then
        NParam  =   This%Intervals(i)%FitModel%NParam
      end if
    end if

! Getting the total number of parameters, considering all the intervals
  else
    do i = 1,This%NIntervals
      if ( .Not. allocated(This%Intervals(i)%FitModel) ) cycle
      NParam  =   NParam + This%Intervals(i)%FitModel%NParam
    end do
  end if

End Procedure

Module Procedure GetNParamMaxPerInterval
  integer                                                               ::  i                                 ! Index of fit intervals
  NParamMax   =   0                                                                                             ! Initializing to zero (Required)
  do i = 1,This%NIntervals                                                                                      ! Loop on all fit intervals
    if ( .Not. allocated(This%Intervals(i)%FitModel) ) cycle                                                    ! Going to next interval of FitModel of current interval is not allocated
    NParamMax =   max( NParamMax , This%Intervals(i)%FitModel%NParam )                                          ! Computing the maximum number of parameters
  end do                                                                                                        ! End loop on fit intervals
End Procedure
!
!
! !   #   Stating from a set of points {xi,yi} the current object fit the data using a iinput defined function.
! Module Function Get_Summary_Header( This ) result(Header)
!
!   use String_Library    ,only:  Add_Line_To_String
!
!   class(Fitter_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument
!   character(:)  ,allocatable  ,dimension(:)                             ::  Header                          !< Character string corresponding to  header of the fitting error summary
!   call Add_Line_To_String( Header, "#   NPoints   :   Number of points used to fit the data                                                             " )
!   call Add_Line_To_String( Header, "#   Xmin      :   Minimum x value used to fit the data                                                              " )
!   call Add_Line_To_String( Header, "#   Xmax      :   Maximum x value used to fit the data                                                              " )
!   call Add_Line_To_String( Header, "#   Y_ErrMax  :   Maximum of the local fitting relative error in percent Y_ErrMax = Max( ( yi - f(xi) ) / yi * 100 )" )
!   call Add_Line_To_String( Header, "#   RMSD      :   Root-Mean-Square Deviation between tabulated and fitted data                                      " )
!   call Add_Line_To_String( Header, "#   NRMSD     :   Normalized root-Mean-Square Deviation between tabulated and fitted data                           " )
!   call Add_Line_To_String( Header, "#   CV_RMSD   :   Coefficient of variation of the RMSD                                                              " )
!   call Add_Line_To_String( Header, "#  NPoints      Xmin         Xmax         Y_ErrMax     RMSD         NRMSD        CV_RMSD   " )
! End Function
!
! Module Function Get_Summary( This, Debug ) result(Summary)
!
!   use String_Library    ,only:  Convert_To_String
!
!   class(Fitter_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument
!   logical                                     ,optional ,intent(in)     ::  Debug                         !< Debugging indicator
!   character(:)  ,allocatable                                            ::  Summary                         !< Character string corresponding to  summary of the fit error
!
!   integer                                                               ::  i_Fit                           ! Index of fitted data
!   integer                                                               ::  i_Tab                           ! Index of tabulated data
!   real(rkp)                                                             ::  Y_Tab_Min
!   real(rkp)                                                             ::  Y_Tab_Max
!   real(rkp)                                                             ::  Y_Tab_Mean
!
! ! #  NPoints      Xmin         Xmax         Y_ErrMax     RMSD         NRMSD        CV_RMSD
!   Summary     =     Convert_To_String(  This%NPtsFit             , Fmt="i5,3x,"      ) // "   "  &
!               //    Convert_To_String(  This%X_Fit(1)            , Fmt="es10.3,3x,"  ) // "   "  &
!               //    Convert_To_String(  This%X_Fit(This%NPtsFit) , Fmt="es10.3,3x,"  ) // "   "  &
!               //    Convert_To_String(  This%Y_ErrMax            , Fmt="es10.3,3x,"  ) // "   "  &
!               //    Convert_To_String(  This%RMSD                , Fmt="es10.3,3x,"  ) // "   "  &
!               //    Convert_To_String(  This%NRMSD               , Fmt="es10.3,3x,"  ) // "   "  &
!               //    Convert_To_String(  This%CV_RMSD             , Fmt="es10.3,3x,"  ) // "   "
! End Function

Module Procedure Allocate_Fit_Data
  logical                                                               ::  Dbg                     ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='Allocate_Fit_Data'
  integer                                                               ::  k                               ! Index
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  This%NPtsFit  =       This%iX_Fin - This%iX_Ini + 1                                                           ! Setting the number of fitted temperature
  if ( allocated(This%iX_Fit_To_Tab) ) deallocate( This%iX_Fit_To_Tab )
  if ( allocated(This%X_Fit        ) ) deallocate( This%X_Fit         )
  if ( allocated(This%Y_Fit        ) ) deallocate( This%Y_Fit         )
  if ( allocated(This%Y_Err        ) ) deallocate( This%Y_Err         )
  allocate( This%iX_Fit_To_Tab  (This%NPtsFit) )                                                                ! Allocating the index mapping from fitted to tabluated data
  allocate( This%X_Fit          (This%NPtsFit) )                                                                ! Allocating the fitting X-coordinates
  allocate( This%Y_Fit          (This%NPtsFit) )                                                                ! Allocating the fitting Y-coordinates (To be computed later, once the fitting coefficient have been computed)
  allocate( This%Y_Err          (This%NPtsFit) )                                                                ! Allocating the fitting error
  This%iX_Fit_To_Tab(1:This%NPtsFit)    =       [ (k, k = This%iX_Ini,This%iX_Fin) ]                            ! Setting the temperature index correspondance  from tabulated to fitted index
  This%X_Fit(1:This%NPtsFit)            =       This%X_Tab(This%iX_Ini:This%iX_Fin)                             ! Affecting fitting X-coordinates
  if (Dbg) then
    call Logger%Write( "This%iX_Ini  = ", This%iX_Ini  )
    call Logger%Write( "This%iX_Fin  = ", This%iX_Fin  )
    call Logger%Write( "This%NPtsFit = ", This%NPtsFit )
    call Logger%Exiting()
  end if
End Procedure

Module Procedure SetFitRange
  use Error_Class       ,only:  Error
  use Utilities_Library  ,only:  LocateValue
  logical                                                               ::  Dbg                     ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='SetFitRange'
  integer                                                               ::  k                               ! Index
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )
  if ( present(iX_Ini) .and. present(X_Ini) ) call Error%Raise( "Input arguments iX_Ini and X_Ini should not be used together", ProcName=ProcName )
  if ( present(iX_Fin) .and. present(X_Fin) ) call Error%Raise( "Input arguments iX_Fin and X_Fin should not be used together", ProcName=ProcName )
  This%iX_Ini   =       1                                                                                       ! Initializing the initial X index to one
  This%iX_Fin   =       This%NPtsTab                                                                            ! Initializing the final X index to the number of tabulated points
  if ( present(iX_Ini) ) This%iX_Ini    =       iX_Ini                                                          ! Setting the initial temperature index to optional input value
  if ( present(iX_Fin) ) This%iX_Fin    =       iX_Fin                                                          ! Setting the final temperature index to optional input value
  if ( present(X_Ini)  ) This%iX_Ini    =       LocateValue( X_Ini, This%X_Tab )                      ! Setting the initial temperature index to the index corresponding to the optional input temperature
  if ( present(X_Fin)  ) This%iX_Fin    =       LocateValue( X_Fin, This%X_Tab )                      ! Setting the initial temperature index to the index corresponding to the optional input temperature
  This%NPtsFit  =       This%iX_Fin - This%iX_Ini + 1                                                           ! Setting the number of fitted temperature
  if ( allocated(This%iX_Fit_To_Tab) ) deallocate( This%iX_Fit_To_Tab )
  if ( allocated(This%X_Fit        ) ) deallocate( This%X_Fit         )
  if ( allocated(This%Y_Fit        ) ) deallocate( This%Y_Fit         )
  if ( allocated(This%Y_Err        ) ) deallocate( This%Y_Err         )
  allocate( This%iX_Fit_To_Tab  (This%NPtsFit) )                                                                ! Allocating the X-coordinates index correspondance from tabulated to fitted data
  allocate( This%X_Fit          (This%NPtsFit) )                                                                ! Allocating the fitting X-coordinates
  allocate( This%Y_Fit          (This%NPtsFit) )                                                                ! Allocating the fitting Y-coordinates (To be computed later, once the fitting coefficient have been computed)
  allocate( This%Y_Err          (This%NPtsFit) )                                                                ! Allocating the fitting error
  This%iX_Fit_To_Tab(1:This%NPtsFit)    =       [ (k, k = This%iX_Ini,This%iX_Fin) ]                            ! Setting the temperature index correspondance  from tabulated to fitted index
  This%X_Fit(1:This%NPtsFit)            =       This%X_Tab(This%iX_Ini:This%iX_Fin)                             ! Affecting fitting X-coordinates
  if (Dbg) then
    call Logger%Write( "This%iX_Ini  = ", This%iX_Ini  )
    call Logger%Write( "This%iX_Fin  = ", This%iX_Fin  )
    call Logger%Write( "This%NPtsFit = ", This%NPtsFit )
    call Logger%Exiting()
  end if
End Procedure

Module Procedure CheckFitPreConditions

!   use Error_Class       ,only:  Error
  use String_Library    ,only:  Convert_To_String

  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName='CheckFitPreConditions'
  character(*)                                              ,parameter  ::  ErrorTitle='Error in Fitter object'
  integer                                                               ::  k                                 ! Index

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Checking fit pre-conditions" )

! ==============================================================================================================
!    CHECKING THAT THE TABULATED DATA IS SET
! ==============================================================================================================
  if (Dbg) call Logger%Write( "-> Checking tabulated data" )
  if ( .Not. allocated(This%X_Tab) ) then
!     call Error%Raise(                                                                         &
!       [ character(1000) :: "The tabulated x-coordinates have not been set and 'This%X_Tab' is not allocated.",   &
!         "This data can be set either during the initialization of the Fitter object:",        &
!         "       call Fitter%Initialize( X_Tab = Var, ... )",                                  &
!         "or anytime after the object initialization suing:",                                  &
!         "       call Fitter%SetXTab( Var )",                                                &
!         "where 'var' is a 1d real(8) array." ],                                               &
!       Title     =   ErrorTitle,                                                               &
!       ProcName  =   ProcName                                                                  )
  else
    if (Dbg) call Logger%Write( "   * The tabulated x-coordinates have been set" )
  end if
  if ( .Not. allocated(This%Y_Tab) ) then
!     call Error%Raise(                                                                         &
!       [ character(1000) :: "The tabulated y-coordinates have not been set and 'This%Y_Tab' is not allocated.",   &
!         "This data can be set either during the initialization of the Fitter object:",        &
!         "       call Fitter%Initialize( Y_Tab = Var, ... )",                                  &
!         "or anytime after the object initialization suing:",                                  &
!         "       call Fitter%SetYTab( Var )",                                                &
!         "where 'var' is a 1d real(8) array." ],                                               &
!       Title     =   ErrorTitle,                                                               &
!       ProcName  =   ProcName                                                                  )
  else
    if (Dbg) call Logger%Write( "   * The tabulated y-coordinates have been set" )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    CHECKING THAT THE 'FITINTERVAL' OBJECTS ARE SET
! ==============================================================================================================
  if (Dbg) call Logger%Write( "-> Checking the 'FitInterval' objects" )
  if ( .Not. Allocated(This%Intervals) ) then
!     call Error%Raise(                                                                         &
!       [ character(1000) :: "The array of 'FitInterval' objects is not allocated.",                               &
!         "This object is automatically set by when initializing the Fitter object by calling", &
!         "       call Fitter%Initialize( ... )",                                               &
!         "So, I guess you forgot to initialize the Fitter object before using it !"],          &
!       Title     =   ErrorTitle,                                                               &
!       ProcName  =   ProcName                                                                  )
  else
    if (Dbg) call Logger%Write( "   * The array of 'FitInterval' objects is allocated and has " // Convert_To_String(This%NIntervals) // " elements." )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    CHECKING THAT THE 'FitModel' OBJECTS ARE SET
! ==============================================================================================================
  if (Dbg) call Logger%Write( "-> Checking the 'FitModel' objects" )
  do k = 1,This%NIntervals
    if ( .Not. Allocated(This%Intervals(k)%FitModel) ) then
!       call Error%Raise(                                                                         &
!         [ character(1000) :: "The component 'FitModel' of the " // Convert_To_String(k) // "-th element of the 'FitInterval' array is not allocated.", &
!           "This object is set by calling the initialization procedure with the 'FitModel' optional argument:",  &
!           "       call Fitter%Initialize( FitModel=Var, ... )", &
!         "where 'var' is a varaible of type 'FitModel_Type'." ], &
!         Title     =   ErrorTitle, &
!         ProcName  =   ProcName    )
    else
      if (Dbg) call Logger%Write( "   * The 'FitModel' object of 'FitInterval(" // Convert_To_String(k) // ")' is allocated" )
    end if
  end do
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure


! Let us considere the global constrain system: C x = d
! where C is the global constrain PxN matrix, x is the N solution vector and d is the constrain P-vector.
! There are P constrains to be applied to the N unknown.
! Each the contribution of each individual constain to the global constrain system are denoted with a subscript 'l' for local.
! Each indidivual constrain corresponds to a line in the global constrain matrix C
! Let us consider 2 constrains C1 and C2 and an solution vector of size 5.
! The constrains C1 and C2 are associated to the element 1:3 and 4:6 respectively.
! That is P=2 and N=4
! Then the constrain system C x = d is
!
!                                           | x(1) |
!                                           | x(2) |
! | C1(1) C1(2) C1(3) 0     0     0     |   | x(3) |      | d1(1) |
! |                                     | . |------|  =   |       |
! | 0     0     0     C2(1) C2(2) C2(3) |   | x(4) |      | d2(1) |
!                                           | x(5) |
!                                           | x(6) |

Module Procedure SolveLinearSystem

  use ,intrinsic ::       IEEE_Arithmetic
  use Error_Class       ,only:  Error
  use String_Library    ,only:  Convert_To_String

  logical                                                               ::  Dbg                     ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='SolveLinearSystem'
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  String

  integer                                                               ::  INFO                            ! Indicator
  integer                                                               ::  M                               ! The number of rows of the matrix A.  M >= 0.
  integer                                                               ::  N                               ! The number of columns of the matrix A.  N >= 0.
  integer                                                               ::  P                               ! Number of constrains
  integer                                                               ::  NRHS                            ! The number of right hand sides, i.e., the number of columns of the matrices B and X. NRHS >=0.
  integer                                                               ::  LDA                             ! The leading dimension of the array A.  LDA >= max(1,M).
  integer                                                               ::  LDB                             ! The leading dimension of the array B. LDB >= MAX(1,M,N).
  integer                                                               ::  LDC
  integer                                                               ::  MN                              !
  integer                                                               ::  LWORK                           !
  integer                 ,parameter                                    ::  NB    =   128                   ! The optimum block size
  character(len=1)        ,parameter                                    ::  TRANS =   'N'                   ! 'N': the linear system involves A; 'T': the linear system involves A**T.
  real(rkp)   ,dimension(:)     ,allocatable                            ::  b                               !
  real(rkp)   ,dimension(:)     ,allocatable                            ::  d                               !
  real(rkp)   ,dimension(:,:)   ,allocatable                            ::  A                               !
  real(rkp)   ,dimension(:,:)   ,allocatable                            ::  C                               ! On entry, the P-by-N constrain matrix C. On exit, the upper triangle of the subarray (1:P,N-P+1:N) contains the P-by-P upper triangular matrix R.
  real(rkp)   ,dimension(:)     ,allocatable                            ::  WORK                            !

  real(rkp)   ,dimension(:)   ,allocatable                              ::  x, y
  real(rkp)     ,allocatable                                            ::  Solution(:)    ! Solution vector of the LLS system: contains the set of fitting coefficient on all intervals
  integer                                                               ::  Status

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )


! ==============================================================================================================
!    CHECKING PRECONDITIONS: IS THE FITTER OBJECT READY TO DO ITS JOB
! ==============================================================================================================
  call This%CheckFitPreConditions( Debug )
! ==============================================================================================================



  if (Dbg) call Logger%Write( "Calling This%Allocate_Fit_Data()" )
  call This%Allocate_Fit_Data( Debug=.False. )

! ==============================================================================================================
!    SETTING THE DIMENSIONS OF THE LOCAL/GLOBAL ELEMENTS OF THE LINEAR LEAST SQUARE SYSTEM (A, b)
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Allocating the global elements of the linear least square system" )
  allocate( x, source = This%X_Tab(This%iX_Ini:This%iX_Fin) )
  allocate( y, source = This%Y_Tab(This%iX_Ini:This%iX_Fin) )
  if (Dbg) call Logger%Write( "-> This%Set_Local_Index" )
  call This%Set_Local_Index( x, Debug=.False. )
  N     =     0
  M     =     0
  do i = 1,This%NIntervals
  associate( Interval => This%Intervals(i) )
    if (Dbg) call Logger%Write( "-> Calling InitializeLinearSystem for i = ", i, NewLine=.True. )
    call Interval%InitializeLinearSystem( x, y, Debug=Dbg )
    M   =   M + Interval%M
    N   =   N + Interval%N
  end associate
  end do
  if (Dbg) then
    call Logger%Write( "-> Global number of points: M = ", M )
    call Logger%Write( "-> Global number of coeff.: N = ", N )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE 'LINEAR SYSTEM' PART OF THE (CONSTRAINED) LINEAR LEAST SQUARE SYSTEM: 'A x = b'
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting the 'linear system' part of the LLS system: 'A x = b'" )
  if (Dbg) call Logger%Write( "-> Allocating the LHS matrix 'A' and RHS vector 'b'" )
  LDA       =       max(1,M)                                                                                    ! The leading dimension of the array A.  LDA >= max(1,M).
  LDB       =       max(1,M,N)                                                                                  ! The leading dimension of the array B. LDB >= MAX(1,M,N).
  allocate( A(LDA,N) ); A  =  0.0_rkp                                                                           ! Allocating and initializing the LHS matrix
  allocate( b(LDB)   ); b  =  0.0_rkp                                                                           ! Allocating and initializing the RHS vector
  if (Dbg) then
    call Logger%Write( "-> A: " // Convert_To_String(size(A,1)) // "-by-" // Convert_To_String(size(A,2)) // " matrix" )
    call Logger%Write( "-> b: " // Convert_To_String(size(b,1)) // "-elements vector" )
  end if
  if (Dbg) call Logger%Write( "-> Setting the contribution of each interval to 'A' and 'b'" )
  do i = 1,This%NIntervals
    if (Dbg) call Logger%Write( "-> Calling This%Intervals(i)%Update_A_b for i = ", i )
    call This%Intervals(i)%Update_A_b( A, b, Debug=Dbg )
  end do
  if (Dbg) then
    call Logger%Write( "-> Elements of the global LLS:" )
!     call Logger%Write( "-> A = ", A, F2="es9.2" )
    call Logger%Write( "-> b = ", b, F2="es9.2" )
  end if
! ==============================================================================================================


! ==============================================================================================================
!    SETTING THE 'CONSTRAIN' PART OF THE (CONSTRAINED) LINEAR LEAST SQUARE SYSTEM: 'C x = d'
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting the 'constrain' part of the LLS system: 'C x = d'" )
  if (Dbg) call Logger%Write( "-> This%NConstrains = ", This%NConstrains )
  if ( This%NConstrains /= 0 ) then
    P       =   This%NConstrains                                                                                  ! Getting the number of constrains
    LDC     =   P                                                                                                 ! The leading dimension of the array C. LDC >= max(1,P).
    allocate( C(P,N) ); C = 0.0_rkp                                                                               ! Allocating and initializing the constrain LHS matrix
    allocate( d(P)   ); d = 0.0_rkp                                                                               ! Allocating and initializing the constrain RHS vector
    do i = 1,This%NConstrains                                                                                     ! Loop on all constrains
      call This%Constrains(i)%Update_C_d( C, d )                                                                  ! Updating the contribution of current constrain inside the global LHS matrix and RHS vector
    end do                                                                                                        ! End loop on constrains
    if (Dbg) then
      call Logger%Write( "-> Elements of the constrain system:" )
      call Logger%Write( "-> C = ", C, F2="es9.2" )
      call Logger%Write( "-> d = ", d, F2="es9.2" )
    end if
  end if
! ==============================================================================================================

  allocate( Solution(N) )

! ==============================================================================================================
!    SOLVING THE UN-CONSTRAINED LINEAR LEAST SQUARE SYSTEM
! ==============================================================================================================
  if (  This%NConstrains == 0 ) then                                                                            ! If a un-constrained linear least square system is considered
    if (Dbg) call Logger%Write( "Solving the un-constrained linear least square system" )
    if (Dbg) call Logger%Write( "-> Allocating the work array" )
    NRHS      =       1                                                                                           ! The number of right hand sides, i.e., the number of columns of the matrices B and X. NRHS >=0.
    MN        =       min(M,N)                                                                                    ! Minimum value of M and N
    LWORK     =       max( 1, MN + max(MN,NRHS) * NB )                                                            ! The dimension of the array WORK
    allocate( WORK(LWORK) )                                                                                       ! Allocating array

    if (Dbg) call Logger%Write( "-> Getting the optimal block size" )
    call DGELS( TRANS, M, N, NRHS, A, LDA, b, LDB, WORK, -1, INFO )                                               !
    if (Dbg) call Logger%Write( "-> Initial size of work: LWORK = ", LWORK )
    LWORK     =       int( WORK(1) )
    if (Dbg) call Logger%Write( "-> Optimal size of work: LWORK = ", LWORK )
    if ( LWORK > 0 ) then
      deallocate( WORK ); allocate( WORK(LWORK) )                                                                 ! Allocating array
    else
      LWORK   =       size(WORK)
    end if

    if (Dbg) call Logger%Write( "-> Calling DGELS (from Lapack)" )
    call DGELS( TRANS, M, N, NRHS, A, LDA, b, LDB, WORK, LWORK, INFO )                                            ! Fitting the data: Solve Ax = b using LAPACK
    if (Dbg) then
      call Logger%Write( "-> INFO = ", Info )
      select case (INFO)
        case(0);    call Logger%Write( "-> Successful exit." )
        case default;  call Logger%Write( "-> The Interpolate fit matrix is singular!" )
      end select
    end if
    if ( INFO /= 0 ) call Error%Raise( "Error in DGELS", ProcName=ProcName )
    Solution  =       b(1:N)                                                                                      ! Getting the solution vector
! ==============================================================================================================


! ==============================================================================================================
!    SOLVING THE CONSTRAINED LINEAR LEAST SQUARE SYSTEM
! ==============================================================================================================
! This section solves the linear equality-constrained least squares (LSE) problem
!       minimize ||A*x - b || (in the 2-norm) subject to C*x = D
! This is done using the 'DGGLSE' procedure from Lapack
! DGGLSE solves the linear equality-constrained least squares (LSE) problem:
!         minimize || b - A*x ||_2 subject to C*x = d
! where A is an M-by-N matrix, C is a P-by-N matrix, b is a given M-vector, and d is a given P-vector.
! It is assumed that P <= N <= M+P, and rank(C) = P and rank( (A) ) = N.
! These conditions ensure that the LSE problem has a unique solution, which is obtained using a generalized
! RQ factorization of the matrices (B, A) given by C = (0 R)*Q, A = Z*T*Q.
!       DGGLSE( M, N, P, A, LDA, C, LDC, B, D, X, WORK, LWORK, INFO )
!
!       M     The number of rows of the matrix A. M >= 0.
!       N     The number of columns of the matrices A and C. N >= 0.
!       P     The number of rows of the matrix C. 0 <= P <= N <= M+P.
!       A     On entry, the M-by-N matrix A. On exit, the elements on and above the diagonal of the array contain the min(M,N)-by-N upper trapezoidal matrix T.
!       LDA   The leading dimension of the array A. LDA >= max(1,M).
!       C     On entry, the P-by-N matrix C. On exit, the upper triangle of the subarray C(1:P,N-P+1:N) contains the P-by-P upper triangular matrix R.
!       LDC   The leading dimension of the array C. LDC >= max(1,P).
!       B     On entry, B contains the right hand side vector for the least squares part of the LSE problem. On exit, the residual sum of squares for the solution is given by the sum of squares of elements N-P+1 to M of vector B.
!       D     On entry, D contains the right hand side vector for the constrained equation. On exit, D is destroyed.
!       X     On exit, X is the solution of the LSE problem.
!       WORK  On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!       LWORK The dimension of the array WORK. LWORK >= max(1,M+N+P). For optimum performance LWORK >= P+min(M,N)+max(M,N)*NB, where NB is an upper bound for the optimal blocksizes for DGEQRF, SGERQF, DORMQR and SORMRQ. If LWORK = -1, then a workspace query is assumed; the routine only calculates the optimal size of the WORK array, returns this value as the first entry of the WORK array, and no error message related to LWORK is issued by XERBLA.
!       INFO  = 0: successful exit.
!             < 0: if INFO = -i, the i-th argument had an illegal value.
!             = 1: the upper triangular factor R associated with C in the generalized RQ factorization of the pair (C, A) is singular, so that rank(C) < P; the least squares solution could not be computed.
!             = 2: the (N-P) by (N-P) part of the upper trapezoidal factor T associated with A in the generalized RQ factorization of the pair (C, A) is singular, so that rank( (A) ) < N; the least squares solution could not ( (C) ) be computed.
! ==============================================================================================================
  else                                                                                                          ! If a constrained linear least square system is considered
    if (Dbg) call Logger%Write( "Solving the constrained linear least square system" )
    if (Dbg) call Logger%Write( "-> Allocating the work array" )
    LWORK     =   max(1,M+N+P)
    LWORK     =   P + min(M,N) + max(M,N)*NB
    allocate( WORK(LWORK) )


    if (Dbg) call Logger%Write( "-> Getting the optimal block size" )
    call DGGLSE( M, N, P, A, LDA, C, LDC, b, d, Solution, WORK, -1, INFO )
    if (Dbg) call Logger%Write( "-> Initial size of work: LWORK = ", LWORK )
    LWORK     =       int( WORK(1) )
    if (Dbg) call Logger%Write( "-> Optimal size of work: LWORK = ", LWORK )
    if ( LWORK > 0 ) then
      deallocate( WORK ); allocate( WORK(LWORK) )                                                                 ! Allocating array
    else
      LWORK   =       size(WORK)
    end if
!
!     if (Dbg) then
!       do i = 1,size( b )
!         call Logger%Write( "<DEBUG> i = ", i, "b = ", b(i), F2="i3", F4="es15.8" )
!       end do
!     end if

    if (Dbg) call Logger%Write( "-> Calling DGGLSE (from Lapack)" )
    call DGGLSE( M, N, P, A, LDA, C, LDC, b, d, Solution, WORK, LWORK, INFO )
    if (Dbg) then
      call Logger%Write( "-> INFO = ", Info )
      select case (INFO)
        case(0);    call Logger%Write( "-> Successful exit." )
        case(:-1);  call Logger%Write( "-> The "//Convert_To_String(abs(i))//"-th argument had an illegal value." )
        case(1);    call Logger%Write( "-> The upper triangular factor R associated with C in the generalized RQ factorization of the pair (C, A) is singular, so that rank(C) < P; the least squares solution could not be computed." )
        case(2);    call Logger%Write( "-> The (N-P) by (N-P) part of the upper trapezoidal factor T associated with A in the generalized RQ factorization of the pair (C, A) is singular, so that rank( (A) ) < N; the least squares solution could not be computed." )
      end select
    end if
    if ( INFO /= 0 ) call Error%Raise( "Error in DGGLSE", ProcName=ProcName )

  end if
! ==============================================================================================================
!   if (Dbg) call Logger%Write( "-> Solution = ", Solution, F2="es15.8" )


! ==============================================================================================================
!     SETTING THE PARAMETERS IN THE 'FITMODEL' OBJECT AND MODIFYING THEM TO THE ORIGINAL SYSTEM
! ==============================================================================================================
!   if (Dbg) call Logger%Write( "Setting the parameters in the 'fitmodel' object and modifying them to the original system" )

  do i = 1,This%NIntervals                                                                                      ! Loop on all intervals
# ifdef WORKAROUND_GFORTRAN_RECURSIVE_ASSOCIATION
  associate( Interval => This%Intervals(i), Param => Solution(This%Intervals(i)%iIniGlo:This%Intervals(i)%iFinGlo), Model => This%Intervals(i)%FitModel )
# else
  associate(  Interval  =>  This%Intervals(i),                            &                                     ! Associating current interval and its local paraneters to new variables for commodity
              Param     =>  Solution(Interval%iIniGlo:Interval%iFinGlo),  &
              Model     =>  Interval%FitModel                             )
# endif
    call Model%Set_Param( Param, Status )                                                           ! Puching the solution back into the FitModel
    if ( Status /= 0 ) call Error%Raise( "Error pushing parameters for interval i = "//Convert_To_String(i), ProcName=ProcName )
    call Model%DeLinearize()                                                                                   ! Modifying the model parameters to go back from the linear system to the original system (only used for Arrhenius fit so far)
    Param   =     Model%Get_Param()                                                                              ! Extracting the modified parameters back in the 'Solution' variable
  end associate                                                                                                 ! End of variable association
  end do                                                                                                        ! End loop on intervals
  if (Dbg) call Logger%Write( "-> Solution = ", Solution, F2="es15.8" )
! ==============================================================================================================


! ==============================================================================================================
!     PRINTING THE MODEL PARAMETERS FOR EACH INTERVAL
! ==============================================================================================================
  if (Dbg) then
    call Logger%Write( "Printing the model parameters for each interval" )
    do i = 1,This%NIntervals                                                                                      ! Loop on all intervals                                                                                      ! Loop on all intervals
# ifdef WORKAROUND_GFORTRAN_RECURSIVE_ASSOCIATION
    associate( Interval => This%Intervals(i), Model => This%Intervals(i)%FitModel )
# else
    associate( Interval => This%Intervals(i), Model => Interval%FitModel )
# endif
      String  =   "[" // Convert_To_String(Interval%Xmin,Fmt="es15.8") // ":"// Convert_To_String(Interval%Xmax,Fmt="es15.8") // "]"
      call Logger%Write( "-> Interval i = ", i, "X-range = ", String, "Param = ", Model%Param, F2="i3", F4="es15.8", F6="es15.8" )
    end associate                                                                                                 ! End of variable association
    end do                                                                                                        ! End loop on intervals
  end if
! ==============================================================================================================


!   This%Success    =     ( INFO /= 0 ) .And. (.Not. any( IEEE_IS_NAN(This%FitModel%Coef)) )                     ! Setting the indicator whether of not the fitting has succeeded or failed (Failure is considered whenever at least one coefficient is a NaN)
  if (Dbg) call Logger%Write( "This%Success = ", This%Success )

  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure Set_Local_Index
  logical                                                               ::  Dbg
  character(*)                                              ,parameter  ::  ProcName='Set_Local_Index'
  integer                                                               ::  i, j, is, ie
  real(rkp)     ,dimension( size(x) )                                   ::  Vector
  logical       ,dimension( size(x) )                                   ::  Mask

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Setting the local index in each Intervals" )
  if ( This%NIntervals == 1 ) then
    This%Intervals(1)%is    =     1
    This%Intervals(1)%ie    =     size(x)
  else
    do i = 1,This%NIntervals
    associate( Interval => This%Intervals(i) )
      if (Dbg) call Logger%Write( "i = ", i, "Interval%Xmin = ", Interval%Xmin, "Interval%Xmax = ", Interval%Xmax, Fr="es15.8" )
      Vector  =   x - Interval%Xmin
      Mask    =   Vector >= 0.0_rkp
      is      =   minloc( Vector, dim=1, Mask=Mask )
      if (Dbg) call Logger%Write( "-> Starting index" )
      if (Dbg) call Logger%Write( "-> x      = ", x, Fr="es15.8" )
      if (Dbg) call Logger%Write( "-> Vector = ", Vector, Fr="es15.8" )
      if (Dbg) call Logger%Write( "-> Mask   = ", Mask, Fr="es15.8" )
      if (Dbg) call Logger%Write( "-> is     = ", is )
      Vector  =   Interval%Xmax - x
      if ( i == This%NIntervals ) then
        Mask  =   Vector >= 0.0_rkp
      else
        Mask  =   Vector >  0.0_rkp
      end if
      ie      =   minloc( Vector, dim=1, Mask=Mask )
      if (Dbg) call Logger%Write( "-> Ending index" )
      if (Dbg) call Logger%Write( "-> x      = ", x, Fr="es15.8" )
      if (Dbg) call Logger%Write( "-> Vector = ", Vector, Fr="es15.8" )
      if (Dbg) call Logger%Write( "-> Mask   = ", Mask, Fr="es15.8" )
      if (Dbg) call Logger%Write( "-> ie     = ", ie )
      Interval%is    =     0
      Interval%ie    =     0
      do j = 1,size(x)
        if ( (Interval%is == 0) .And. (x(j) >= Interval%Xmin) ) Interval%is = j
        if (                          (x(j) <= Interval%Xmax) ) Interval%ie = j
      end do
      Interval%is    =     is
      Interval%ie    =     ie
    end associate
    end do
  end if

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure EvaluateFitModel

  use Error_Class       ,only:  Error

  character(*)                                              ,parameter  ::  ProcName='EvaluateFitModel'
  logical                                                               ::  Extrapolation                     ! Local extrapolation indicator (By default model extrapolation is performed)
  integer                                                               ::  i, k, N
  real(rkp)                                                             ::  xl                                ! Local x-coordinate value

  Extrapolation   =   .True.;
  if ( present(Extrapolate) ) Extrapolation = Extrapolate

  if ( .Not. This%ReadyToUse() ) then
    call Error%Raise( "The Fitter object has not been properly initialized", ProcName=ProcName )
!     call Logger%Write( "<<<<< The Fitter object has not been properly initialized >>>>>" )
!     call Logger%Write( "-> Setting y = 0 and exiting" )
!     y = 0.0_rkp
!     return
  end if

  N   =   This%NIntervals

  do i = 1,size(x)                                                                                              ! Loop on all x-coordinates
    xl        =     x(i)                                                                                        ! Initializing the local x-coordinate
    if ( xl <= This%Intervals(1)%Xmin ) then                                                                    ! If current x-coord. <= than the minimum value of the first interval
      k       =     1                                                                                           ! Setting the interval index to the first interval
      if (.Not.Extrapolation) xl = This%Intervals(k)%Xmin                                                       ! If no extrapolation, then setting x-coord. to the lower bound of the first interval
    else if ( xl >= This%Intervals(N)%Xmax ) then                                                               ! If current x-coord. >= than the maximum value of the last interval
      k       =     N                                                                                           ! Setting the interval index to the last interval
      if (.Not.Extrapolation) xl = This%Intervals(k)%Xmax                                                       ! If no extrapolation, then setting x-coord. to the upper bound of the last interval
    else                                                                                                        ! If current x-coord. is between the minimum and maximum values
      do k = 1,N                                                                                                ! Loop on all intervals
        if ( (xl>This%Intervals(k)%Xmin) .and. (xl<=This%Intervals(k)%Xmax) ) exit                            ! If current x-coord. is within the current x-coord. range, then exiting the procedure since the correct temperature range has been found
      end do                                                                                                    ! End loop on intervals
    end if                                                                                                      ! End if case on temperature value
    y(i)    =     This%Intervals(k)%FitModel%Evaluate_NoParam_0d( xl )   ! @COMPILER_BUG gcc-6.3.1: In Module Function 'Fitter_Type%Evaluate' the call to 'This%Intervals(k)%FitModel%Eval(x)' cause an ICE. Calling diretcly this procedure is a work
  end do

End Procedure



Module Procedure ReadyToUse
  integer                                                               ::  k
  Ready     =     .True.
  if ( .Not. Allocated(This%Intervals) ) then
    Ready   =     .False.
!     call Logger%Write( "The component 'This%Intervals' is not allocated" )
    return
  end if
  do k = 1,This%NIntervals
    if ( .Not. Allocated(This%Intervals(k)%FitModel) ) then
      Ready   =     .False.
!       call Logger%Write( "The component 'This%Intervals(k)%FitModel' is not allocated with k = ", k )
      return
    end if
    if ( .Not. This%Intervals(k)%FitModel%Param_Defined ) then
      Ready   =     .False.
!       call Logger%Write( "The model parameters for the interval k have not been defined 'This%Intervals(k)%FitModel%Param' for k = ", k )
      return
    end if
  end do
End Procedure


! Module Subroutine Compute_Fit_Data( This, Debug )
!   class(Fitter_Type)                                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
!   logical                                     ,optional ,intent(in)     ::  Debug                         !< Debugging indicator
!
!   logical                                                               ::  Dbg                     ! Local debugging indicator
!   character(*)                                              ,parameter  ::  ProcName='Compute_Fit_Data'
!
!   Dbg   =   GetOptArgValue(DefaultDebug,Debug)
!   if (Dbg) call Logger%Entering( ProcName )
!
!   if ( This%Success ) then                                                                                      ! If the fitting was a success
!     This%Y_Fit  =       This%FitModel%Eval( This%X_Fit )                                                      ! Computing the fitted data
!   else                                                                                                          ! If the fitting was a failure
!     This%Y_Fit  =       0.0_rkp                                                                                 ! Setting the fitted data to zero
!   end if                                                                                                        ! End if case on the fitting success indicator
!   if (Dbg) call Logger%Exiting()
! End Subroutine
!
! Module Subroutine Compute_Fit_Error( This, Debug )
!   class(Fitter_Type)                                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
!   logical                                     ,optional ,intent(in)     ::  Debug                         !< Debugging indicator
!
!   logical                                                               ::  Dbg                     ! Local debugging indicator
!   character(*)                                              ,parameter  ::  ProcName='Compute_Fit_Error'
!   integer                                                               ::  i_Fit                           ! Index of fitted data
!   integer                                                               ::  i_Tab                           ! Index of tabulated data
!   real(rkp)                                                             ::  Y_Tab_Min
!   real(rkp)                                                             ::  Y_Tab_Max
!   real(rkp)                                                             ::  Y_Tab_Mean
!   Dbg   =   GetOptArgValue(DefaultDebug,Debug)
!   if (Dbg) call Logger%Entering( ProcName )
!
!   Y_Tab_Min             =       minval( This%Y_Tab(This%iX_Ini:This%iX_Fin) )
!   Y_Tab_Max             =       maxval( This%Y_Tab(This%iX_Ini:This%iX_Fin) )
!   Y_Tab_Mean            =       sum(This%Y_Tab(This%iX_Ini:This%iX_Fin)) / This%NPtsFit
!   This%RMSD             =       0.0_rkp
!   do i_Fit = 1,This%NPtsFit                                                                                     ! Loop on all fitted points
!     i_Tab               =       This%iX_Fit_To_Tab(i_Fit)                                                       ! Getting the index of the associated point in the tabulated data
!     This%Y_Err(i_Fit)   =       ( This%Y_Tab(i_Tab) - This%Y_Fit(i_Fit) ) / This%Y_Tab(i_Tab) * 100             ! Computation of the fitting relative error in percent
!     if ( This%Y_Tab(i_Tab) <= epsilon(0.0_rkp) ) This%Y_Err(i_Fit) = 0.0_rkp                                    ! If tabulated data is zero, then division by zero in the above expression and so setting the error to zero
!     This%RMSD           =       This%RMSD + abs(This%Y_Tab(i_Tab) - This%Y_Fit(i_Fit))**2
!   end do                                                                                                        ! End loop ,on fitted points
!   This%Y_ErrMax         =       maxval( abs(This%Y_Err) )                                                       ! Computing the max. fitting error
!   This%RMSD             =       sqrt( This%RMSD / This%NPtsFit)
!   This%NRMSD            =       This%RMSD / ( Y_Tab_Max - Y_Tab_Min )
!   This%CV_RMSD          =       This%RMSD / Y_Tab_Mean
!
!   if (Debug_Loc) then
!     call Logger%Write( "This%Y_ErrMax   = ", This%Y_ErrMax , F2="es15.8" )
!     call Logger%Write( "This%RMSD       = ", This%RMSD     , F2="es15.8" )
!     call Logger%Write( "This%NRMSD      = ", This%NRMSD    , F2="es15.8" )
!     call Logger%Write( "This%CV_RMSD    = ", This%CV_RMSD  , F2="es15.8" )
!     call Logger%Exiting()
!   end if
!
! End Subroutine
!
! Module Subroutine Compute_Fit_RMS( This, Debug )
!   class(Fitter_Type)                                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
!   logical                                     ,optional ,intent(in)     ::  Debug                         !< Debugging indicator
!
!   logical                                                               ::  Debug_Loc                     ! Local debugging indicator
!   character(*)                                              ,parameter  ::  ProcName='Compute_Fit_RMS'
!
!   integer                                                               ::  i_Fit                           ! Index of fitted data
!   integer                                                               ::  i_Tab                           ! Index of tabulated data
!
!   Debug_Loc   =   GetOptArgValue(DefaultDebug,Debug)
!   if (Debug_Loc) call Logger%Entering( ProcName )
!
!   do i_Fit = 1,This%NPtsFit                                                                                     ! Loop on all fitted points
!     i_Tab               =       This%iX_Fit_To_Tab(i_Fit)                                                       ! Getting the index of the associated point in the tabulated data
!     This%Y_Err(i_Fit)   =       ( This%Y_Tab(i_Tab) - This%Y_Fit(i_Fit) ) / This%Y_Tab(i_Tab) * 100             ! Computation of the fitting relative error in percent
!     if ( abs(This%Y_Tab(i_Tab)) <= epsilon(0.0_rkp) ) This%Y_Err(i_Fit) = 0.0_rkp                                             ! If tabulated data is zero, then division by zero in the above expression and so setting the error to zero
!   end do                                                                                                        ! End loop ,on fitted points
!   This%Y_ErrMax =       maxval( abs(This%Y_Err) )                                                               ! Computing the max. fitting error
!   if (Debug_Loc) call Logger%Write( "This%Y_ErrMax = ", This%Y_ErrMax, F2="es15.8" )
!   if (Debug_Loc) call Logger%Exiting()
! End Subroutine


End SubModule