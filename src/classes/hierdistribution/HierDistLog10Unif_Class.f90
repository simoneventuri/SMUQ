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

module HierDistLog10Unif_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use HierDistUnif_Class                                            ,only:    HierDistUnif_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistLog10Unif_Class                                           ,only:    DistLog10Unif_Type
use DistProb_Class                                                ,only:    DistProb_Type

implicit none

private

public                                                                ::    HierDistLog10Unif_Type

type, extends(HierDistUnif_Type)                                      ::    HierDistLog10Unif_Type
contains
  procedure, private                                                  ::    GenerateDistribution
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GenerateDistribution(This, A, B, Distribution)

  class(HierDistLog10Unif_Type), intent(in)                           ::    This
  real(rkp), intent(in)                                               ::    A
  real(rkp), intent(in)                                               ::    B
  class(DistProb_Type), allocatable, intent(out)                      ::    Distribution

  character(*), parameter                                             ::    ProcName='GenerateDistribution'
  integer                                                             ::    StatLoc=0  

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  allocate(DistLog10Unif_Type :: Distribution)

  select type (Distribution)
    type is (DistLog10Unif_Type) 
      call Distribution%Construct(A=A, B=B)
    class default
      call Error%Raise("Something went wrong", ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
