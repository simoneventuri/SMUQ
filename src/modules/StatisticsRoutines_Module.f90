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

module StatisticsRoutines_Module

use Parameters_Library
use String_Library
use Logger_Class                  ,only: Logger
use Error_Class                   ,only: Error

implicit none

private

public                                                                ::    ComputeMean
public                                                                ::    ComputeSampleVar
public                                                                ::    ComputePopulationVar

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeMean( Values, Debug )

    real(rkp)                                                         ::    ComputeMean
    real(rkp), dimension(:), intent(in)                               ::    Values
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeMean'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    ComputeMean = sum(Values/real(size(Values,1),rkp))

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeSampleVar( Values, Mean, Debug )

    real(rkp)                                                         ::    ComputeSampleVar
    real(rkp), dimension(:), intent(in)                               ::    Values
    real(rkp), optional, intent(in)                                   ::    Mean
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeSampleVar'
    real(rkp)                                                         ::    Mean_Loc
    integer(ikp)                                                      ::    i, iMax

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (size(Values,1)>1) then
      if ( present(Mean) ) then
        Mean_Loc = Mean
      else
        Mean_Loc = ComputeMean( Values )
      end if
      ComputeSampleVar = sum( (Values - Mean_Loc)**2 ) / real(size(Values)-1,rkp)
    else
      ComputeSampleVar = Zero
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputePopulationVar( Values, Mean, Debug )

    real(rkp)                                                         ::    ComputePopulationVar
    real(rkp), dimension(:), intent(in)                               ::    Values
    real(rkp), optional, intent(in)                                   ::    Mean
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputePopulationVar'
    real(rkp)                                                         ::    Mean_Loc
    integer(ikp)                                                      ::    i, iMax

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (size(Values,1)>1) then
      if ( present(Mean) ) then
        Mean_Loc = Mean
      else
        Mean_Loc = ComputeMean( Values )
      end if
      ComputePopulationVar = sum( (Values - Mean_Loc)**2 ) / real(size(Values),rkp)
    else
      ComputePopulationVar = Zero
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
