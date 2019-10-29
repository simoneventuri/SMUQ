! File: inline-GetValue_2d.F90
! # define  _VarType_   <...>
! Module Procedure GetValue_<...>_2d
!   character(*)                                              ,parameter  ::  ProcName = "GetValue_<...>_2d"
! # include "inline-GetValue_2d.F90"
!************************************************************************************
  logical                                                               ::  Dbg, Found_
  integer                                                               ::  Status_, i, j, k, n, NElements, MissingRank, FixedDim, l, ArrayShape(2)
  _VarType_ ,allocatable                                                ::  Vector(:)
  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Setting the values in a vector" )
  if (Dbg) call Logger%Write( "-> Calling This%GetValue" )
  call This%GetValue(                 &
        Vector,                       &
        CallProc    =   CallProc    , &
        Found       =   Found_      , &
        Mandatory   =   Mandatory   , &
        Separator   =   Separator   , &
        CheckValidValues = CheckValidValues , &
        Status      =   Status      , &
        Debug       =   Debug         )
  if ( UpdateStatus(Status,Proc=ProcName,ExitLogger=Dbg) ) return
  if (Dbg) call Logger%Write( "-> Found_ = ", Found_ )
  if ( present(Found) ) Found = Found_
  if ( .Not. Found_ ) then
    if (Dbg) call Logger%Exiting()
    return
  end if
  if (Dbg) call Logger%Write( "-> size(Vector) = ", size(Vector) )

  if (Dbg) call Logger%Write( "Setting the shape of the ouput array" )
  NElements         =   size(Vector)                                                                            ! Getting the number of elements in the vector
  ArrayShape        =   [NElements,1]                                                                           ! Setting the default array shape
  FixedDim      =   1
  if (Dbg) call Logger%Write( "-> NElements = ", NElements )
  if (Dbg) call Logger%Write( "-> present(Shape) = ", present(Shape) )
  if ( present(Shape) ) then                                                                                    ! If the shape optional argument is provided, thenextract the shape of the output array
    ArrayShape      =   0                                                                                       ! Setting the default array shape
    do i = 1,size(Shape)                                                                                        ! Loop on all dimensions: should always goes from 1 to 2 for rank 2 arrays (matrix) !!!
      if ( Shape(i) == 0 ) cycle                                                                                !
      ArrayShape(i) =   Shape(i)                                                                                !
      FixedDim  =   i
      exit                                                                                                      !
    end do                                                                                                      ! End loop on dimensions
    if (Dbg) call Logger%Write( "-> ArrayShape = ", ArrayShape )
    MissingRank     =   NElements / sum(ArrayShape)                                                             ! Computing the missing rank
    if (Dbg) call Logger%Write( "-> MissingRank = ", MissingRank )
    do i = 1,size(ArrayShape)                                                                                   ! This should always goes from 1 to 2 for rank 2 arrays (matrix) !!!
      if ( Shape(i) /= 0 ) cycle
      ArrayShape(i) =   MissingRank
      exit
    end do
!     if ( ArrayShape(1)*ArrayShape(2) /= NElements ) Error @TODO
  end if
  if (Dbg) call Logger%Write( "-> ArrayShape   = ", ArrayShape )
  if (Dbg) call Logger%Write( "-> FixedDim = ", FixedDim )


  if (Dbg) call Logger%Write( "Assigning the values in the Output array" )
  l    = ArrayShape(FixedDim)
  if ( allocated(Values) ) deallocate(Values)
  allocate( Values(ArrayShape(1),ArrayShape(2)) )
  Values    =   0.0_8
  i = 1
  j = 1
  do k = 1,size(Vector)
    n   =   mod(k,l)
    if (Dbg) call Logger%Write( "-> k = ", k, "n = ", n, "i = ", i, "j = ", j, Fi="i3" )
    if ( (i>ArrayShape(1)) .or. (j>ArrayShape(2)) ) exit
    Values(i,j)   =   Vector(k)
    if ( n /= 0 ) then
      j = j + 1
    else
      i = i + 1
      j = 1
    end if
  end do
  if (Dbg) call Logger%Write( "-> Values  = ", Values, Fr="es15.8" )

  if (Dbg) call Logger%Exiting()
!************************************************************************************
! End Procedure
# undef   _VarType_
