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
  real(8)                                                             ::    StartTime
  real(8)                                                             ::    LapTime
  real(8)                                                             ::    StopTime
contains
  procedure, public                                                   ::    Start                   =>    StartTimer
  procedure, public                                                   ::    Stop                    =>    StopTimer
  procedure, public                                                   ::    Reset                   =>    ResetTimer
  procedure, public                                                   ::    Restart                 =>    RestartTimer
  procedure, public                                                   ::    Lap                     =>    LapTimer
  procedure, public                                                   ::    Elapsed                 =>    ElapsedTimer
end type

type(Stopwatch_Type)                                                  ::    Stopwatch

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StartTimer(This)

  class(StopWatch_Type), intent(inout)                                ::    This

  if (.not. This%On) This%On = .true.
  call cpu_time(This%StartTime)
  This%LapTime = This%StartTime
  This%StopTime = This%StartTime

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StopTimer(This)

  class(StopWatch_Type), intent(inout)                                ::    This

  if (This%On) then
    call cpu_time(This%StopTime)
    This%LapTime = This%StopTime
    This%On = .false.
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ResetTimer(This)

  class(StopWatch_Type), intent(inout)                                ::    This

  call This%Stop()
  This%StartTime = 0.0
  This%LapTime = 0.0
  This%StopTime = 0.0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine RestartTimer(This)

  class(StopWatch_Type), intent(inout)                                ::    This

  call This%Reset()
  call This%Start()

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function LapTimer(This)

  real(8)                                                             ::    LapTimer

  class(StopWatch_Type), intent(inout)                                ::    This

  if (This%On) then
    call cpu_time(LapTimer)
    LapTimer = LapTimer - This%LapTime
    This%LapTime = LapTimer
  else
    LapTimer = This%StopTime - This%LapTime
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function ElapsedTimer(This)

  real(8)                                                             ::    ElapsedTimer

  class(StopWatch_Type), intent(in)                                   ::    This

  if (This%On) then
    call cpu_time(ElapsedTimer)
    ElapsedTimer = ElapsedTimer - This%StartTime
  else
    ElapsedTimer = This%StopTime - This%LapTime
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

end module
