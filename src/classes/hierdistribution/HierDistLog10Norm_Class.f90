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

module HierDistLog10Norm_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use HierDistNorm_Class                                            ,only:    HierDistNorm_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistLog10Norm_Class                                           ,only:    DistLog10Norm_Type
use DistProb_Class                                                ,only:    DistProb_Type

implicit none

private

public                                                                ::    HierDistLog10Norm_Type

type, extends(HierDistNorm_Type)                                      ::    HierDistLog10Norm_Type
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    GenerateDistribution
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(HierDistLog10Norm_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'hierarchical_log10normal'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(HierDistLog10Norm_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%A = - huge(One)
    This%B = huge(One)
    This%Mu = Zero
    This%Sigma = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .true.
    This%MuDependency=''
    This%SigmaDependency=''
    This%ADependency=''
    This%BDependency=''

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateDistribution( This, Mu, Sigma, A, B, Distribution )

    class(HierDistLog10Norm_Type), intent(in)                         ::    This
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    Sigma
    real(rkp), intent(in)                                             ::    A
    real(rkp), intent(in)                                             ::    B
    class(DistProb_Type), allocatable, intent(out)                    ::    Distribution

    character(*), parameter                                           ::    ProcName='GenerateDistribution'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    allocate( DistLog10Norm_Type :: Distribution )

    select type ( Distribution )
      type is ( DistLog10Norm_Type ) 
        if ( This%TruncatedLeft .and. This%TruncatedRight ) then
          call Distribution%Construct( Mu=Mu, Sigma=Sigma, A=A, B=B )
        else
          call Distribution%Construct( Mu=Mu, Sigma=Sigma, A=A )
        end if
      class default
        call Error%Raise( "Something went wrong", ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
