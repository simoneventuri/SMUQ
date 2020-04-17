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

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMean(Values)

    real(rkp)                                                         ::    ComputeMean
    real(rkp), dimension(:), intent(in)                               ::    Values 

    character(*), parameter                                           ::    ProcName='ComputeMean'

    ComputeMean = sum(Values/real(size(Values,1),rkp))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeSampleVar(Values, Mean)

    real(rkp)                                                         ::    ComputeSampleVar
    real(rkp), dimension(:), intent(in)                               ::    Values
    real(rkp), optional, intent(in)                                   ::    Mean 

    character(*), parameter                                           ::    ProcName='ComputeSampleVar'
    real(rkp)                                                         ::    Mean_Loc
    integer(ikp)                                                      ::    i, iMax

    if (size(Values,1)>1) then
      if (present(Mean)) then
        Mean_Loc = Mean
      else
        Mean_Loc = ComputeMean(Values)
      end if
      ComputeSampleVar = sum((Values - Mean_Loc)**2) / real(size(Values)-1,rkp)
    else
      ComputeSampleVar = Zero
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputePopulationVar(Values, Mean)

    real(rkp)                                                         ::    ComputePopulationVar
    real(rkp), dimension(:), intent(in)                               ::    Values
    real(rkp), optional, intent(in)                                   ::    Mean 

    character(*), parameter                                           ::    ProcName='ComputePopulationVar'
    real(rkp)                                                         ::    Mean_Loc
    integer(ikp)                                                      ::    i, iMax

    if (size(Values,1)>1) then
      if (present(Mean)) then
        Mean_Loc = Mean
      else
        Mean_Loc = ComputeMean(Values)
      end if
      ComputePopulationVar = sum((Values - Mean_Loc)**2) / real(size(Values),rkp)
    else
      ComputePopulationVar = Zero
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
