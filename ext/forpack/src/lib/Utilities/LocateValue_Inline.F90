
! ! ! ! The "Tab(n)>=Tab(1)" condition enables to deal with monotonically increasing and decreasing tabulated vector
! ! ! ! Given an array Tab(1:Ntab), and given a value Val, this function returns a value Locate_Value_in_Table=i such that Val is between Tab(i) and Tab(i+1).
! ! ! ! Tab(1:Ntab) must be monotonic, either increasing or decreasing.
! ! ! ! The values i=0 or i=Ntab is returned to indicate that Val is out of range.
  integer                                       ::  i_Inf                                                   !< Inferior index
  integer                                       ::  i_Med                                                   !< Medium index
  integer                                       ::  i_Sup                                                   !< Superior index
  integer                                       ::  Ntab                                                    !< Number of elements in the tabulated vector
  integer                                       ::  iter                                                    !< Iteration count
  Ntab  =       size(Array)                                                                                       ! Setting the number of element in the tabulated vector
  i_Inf =       0                                                                                               ! Initialisation of the inf. limit
  i_Sup =       Ntab + 1                                                                                        ! Initialisation of the sup. limit
  iter  =       0                                                                                               ! Intiialisation of the iteration count
  do                                                                                                            ! Loop
   iter         =       iter    +       1                                                                       ! Iteration incrementation
    if  (i_Sup-i_Inf <= 1)      exit                                                                            ! Exit confition for if case
    i_Med       =       int(0.5d0*(i_Sup+i_Inf))                                                                ! Computing the midpoint index
    if  ( (Array(Ntab)>=Array(1)) .eqv. (Value>=Array(i_Med)) ) then                                                    ! If the current tabulated mid-value is smaller than the requested value, then
      i_Inf     =       i_Med                                                                                   ! Replacing the lower limit index by the midpoint index
    else                                                                                                        ! If the current tabulated mid-value is greater than the requested value, then
      i_Sup     =       i_Med                                                                                   ! Replacing the upper limit index by the midpoint index
    end if                                                                                                      ! End if case on current tabulated mid-value
  end do                                                                                                        ! End loop
  if            (Value.eq.Array(1)) then                                                                            ! If value of interest correspond to the first tabluted value
    iValue   =       1                                                                                       ! Setting required index to the index of the first tabluted value
  else if       (Value.eq.Array(Ntab))      then                                                                    ! If value of interest correspond to the last tabluted value
    iValue   =       Ntab                                                                                    ! Setting required index to the index of the last tabluted value
  else                                                                                                          ! If value of interest is between the first and last tabluted values
    iValue   =       i_Inf                                                                                   ! Setting required index to the computed index
  end if                                                                                                        ! End if case


