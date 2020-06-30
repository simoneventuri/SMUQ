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
use ArrayRoutines_Module
use Logger_Class                  ,only: Logger
use Error_Class                   ,only: Error

implicit none

private

public                                                                ::    ComputeMean
public                                                                ::    ComputeVariance

logical, parameter                                                    ::    DebugGlobal = .false.

interface ComputeMean
  module procedure                                                    ::    ComputeMean_R41D
  module procedure                                                    ::    ComputeMean_R81D
end interface

interface ComputeVariance
  module procedure                                                    ::    ComputeVariance_R41D
  module procedure                                                    ::    ComputeVariance_R81D
end interface

contains

!!------------------------------------------------------------------------------------------------------------------------------
function ComputeMean_R41D(Values)

  real(4)                                                             ::    ComputeMean_R41D
  real(4), dimension(:), intent(in)                                   ::    Values 

  character(*), parameter                                             ::    ProcName='ComputeMean_R41D'

  ComputeMean_R41D = sum(Values/real(size(Values,1),4))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function ComputeMean_R81D(Values)

  real(8)                                                             ::    ComputeMean_R81D
  real(8), dimension(:), intent(in)                                   ::    Values 

  character(*), parameter                                             ::    ProcName='ComputeMean_R81D'

  ComputeMean_R81D = sum(Values/real(size(Values,1),8))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function ComputeVariance_R41D(Values, Mean, Population)

  real(4)                                                             ::    ComputeVariance_R41D
  real(4), dimension(:), intent(in)                                   ::    Values
  real(4), optional, intent(in)                                       ::    Mean 
  logical, optional, intent(in)                                       ::    Population

  character(*), parameter                                             ::    ProcName='ComputeVariance_R41D'
  real(4)                                                             ::    MeanLoc
  logical                                                             ::    PopulationLoc

  PopulationLoc = .false.
  if (present(Population)) PopulationLoc = Population 

  if (.not. IsArrayConstant(Array=Values)) then
    if (present(Mean)) then
      MeanLoc = Mean
    else
      MeanLoc = ComputeMean(Values)
    end if
    if (PopulationLoc) then
      ComputeVariance_R41D = sum((Values - MeanLoc)**2) / real(size(Values),4)
    else
      ComputeVariance_R41D = sum((Values - MeanLoc)**2) / real(size(Values)-1,4)
    end if
  else
    ComputeVariance_R41D = Zero
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function ComputeVariance_R81D(Values, Mean, Population)

  real(8)                                                             ::    ComputeVariance_R81D
  real(8), dimension(:), intent(in)                                   ::    Values
  real(8), optional, intent(in)                                       ::    Mean 
  logical, optional, intent(in)                                       ::    Population

  character(*), parameter                                             ::    ProcName='ComputeVariance_R81D'
  real(8)                                                             ::    MeanLoc
  logical                                                             ::    PopulationLoc

  PopulationLoc = .false.
  if (present(Population)) PopulationLoc = Population 

  if (.not. IsArrayConstant(Array=Values)) then
    if (present(Mean)) then
      MeanLoc = Mean
    else
      MeanLoc = ComputeMean(Values)
    end if
    if (PopulationLoc) then
      ComputeVariance_R81D = sum((Values - MeanLoc)**2) / real(size(Values),8)
    else
      ComputeVariance_R81D = sum((Values - MeanLoc)**2) / real(size(Values)-1,8)
    end if
  else
    ComputeVariance_R81D = Zero
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------


end module
