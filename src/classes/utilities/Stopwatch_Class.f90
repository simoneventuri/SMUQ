! -*-f90-*-
!!--------------------------------------------------------------------------------------------------------------------------------
!!
!! Stochastic Modeling & Uncertainty Quantification (SMUQ)
!!
!! Copyright (C) 2016 Venturi, Simone & Rostkowski, Przemyslaw (University of Illinois at Urbana-Champaign)
!!
!! This program is free software; you can redistribute it and/or modify it under the terms of the Version 2.1 GNU Lesser General
!! Public License as published by the Free Software Foundation.
!!
!! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free 
!! Software Foundation, Inc. 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!!
!!--------------------------------------------------------------------------------------------------------------------------------

module Stopwatch_Class

use Parameters_Library

implicit none

private

public                                                                ::    Stopwatch_Type, Stopwatch

type                                                                  ::    StopWatch_Type
  logical                                                             ::    On=.false.
  integer(8), dimension(8)                                            ::    StartTime
  integer(8), dimension(8)                                            ::    LapTime
  integer(8), dimension(8)                                            ::    StopTime
contains
  procedure, public                                                   ::    Start                   =>    StartTimer
  procedure, public                                                   ::    Stop                    =>    StopTimer
  procedure, public                                                   ::    Reset                   =>    ResetTimer
  procedure, public                                                   ::    Restart                 =>    RestartTimer
  procedure, public                                                   ::    Lap                     =>    LapTimer
  procedure, public                                                   ::    Elapsed                 =>    ElapsedTimer
  procedure, nopass, private                                          ::    ConvertToSeconds
  procedure, nopass, private                                          ::    SecondsInMonth
  procedure, nopass, private                                          ::    IsLeapYear
end type

type(Stopwatch_Type)                                                  ::    Stopwatch

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine StartTimer(This)

    class(StopWatch_Type), intent(inout)                              ::    This

    if (.not. This%On) then
      This%On = .true.
      call date_and_time(values=This%StartTime)
      This%LapTime = This%StartTime
    else
      call date_and_time(values=This%StartTime)
      This%LapTime = This%StartTime
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine StopTimer(This)

    class(StopWatch_Type), intent(inout)                              ::    This

    if (This%On) then
      call date_and_time(values=This%StopTime)
      This%On = .false.
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ResetTimer(This)

    class(StopWatch_Type), intent(inout)                              ::    This

    This%StartTime = 0
    This%LapTime = 0
    This%StopTime = 0
    call This%Stop()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RestartTimer(This)

    class(StopWatch_Type), intent(inout)                              ::    This

    call This%Reset()
    call This%Start()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function LapTimer(This)

    real(rkp)                                                         ::    LapTimer

    class(StopWatch_Type), intent(inout)                              ::    This
    integer(8), dimension(8)                                          ::    CurrentTime
    real(rkp)                                                         ::    OneHundred=100.

    if (This%On) then
      call date_and_time(values=CurrentTime)
      LapTimer = ConvertToSeconds(Time=CurrentTime) - ConvertToSeconds(Time=This%LapTime)
      This%LapTime = CurrentTime
    else
      LapTimer = ConvertToSeconds(Time=This%StopTime) - ConvertToSeconds(Time=This%LapTime)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ElapsedTimer(This)

    real(rkp)                                                         ::    ElapsedTimer

    class(StopWatch_Type), intent(in)                                 ::    This

    integer(8), dimension(8)                                          ::    CurrentTime
    real(rkp)                                                         ::    OneHundred=100.

    if (This%On) then
      call date_and_time(values=CurrentTime)
      ElapsedTimer = ConvertToSeconds(Time=CurrentTime) - ConvertToSeconds(Time=This%StartTime)
    else
      ElapsedTimer = ConvertToSeconds(Time=This%StopTime) - ConvertToSeconds(Time=This%StartTime)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ConvertToSeconds(Time)

    real(rkp)                                                         ::    ConvertToSeconds

    integer(8), dimension(8), intent(in)                              ::    Time

    integer                                                           ::    i
    integer                                                           ::    CurrentMonthm1

    ConvertToSeconds = 0
    i = 1
    CurrentMonthm1 = int(Time(2)-1,4)
    do i = 1, CurrentMonthm1
      ConvertToSeconds = ConvertToSeconds + real(SecondsInMonth(Month=i, LeapYear=IsLeapYear(Time(1))),rkp)
    end do

    ConvertToSeconds = ConvertToSeconds + real(Time(3)*86400 + Time(5)*3600 + Time(6)*60 + Time(7),rkp) +                 &
                                                                                                           real(Time(8),rkp)*0.001

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function SecondsInMonth(Month, LeapYear)

    integer(8)                                                        ::    SecondsInMonth

    integer, intent(in)                                               ::    Month
    logical, intent(in)                                               ::    LeapYear

    select case (Month)

      case (1,3,5,7,8,10,12) !31 days
        SecondsInMonth = 2678400
      case (4,6,9,11) ! 30 days
        SecondsInMonth = 2592000
      case (2)
        if (LeapYear) then ! 28 or 29 days
          SecondsInMonth = 2505600
        else
          SecondsInMonth = 2419200
        end if        
      case default
        SecondsInMonth = 0

    end select

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsLeapYear(Year)

    logical                                                           ::    IsLeapYear

    integer(8), intent(in)                                            ::    Year

    integer(8)                                                        ::    Four=4

    if (mod(Year,Four) == 0) then
      IsLeapYear = .true.
    else
      IsLeapYear = .false.
    end if      

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
