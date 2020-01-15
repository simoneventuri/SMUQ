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

module ModelInternal_class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use Input_Class                                                   ,only:    Input_Type
use Output_Class                                                  ,only:    Output_Type
implicit none

private

public                                                                ::    ModelInternal_Type

type, abstract, extends(Model_Type)                                   ::    ModelInternal_Type
contains
  procedure, public                                                   ::    Run_1D
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_1D( This, Input, Output, Stat )

    class(ModelInternal_Type), intent(inout)                          ::    This
    type(Input_Type), dimension(:), intent(in)                        ::    Input
    type(Output_Type), dimension(:,:), intent(inout)                  ::    Output
    integer, dimension(:), optional, intent(inout)                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_1D'
    integer                                                           ::    NbInputs
    integer                                                           ::    NbSubModels
    integer                                                           ::    StatLoc
    integer                                                           ::    i, ii
    character(:), allocatable                                         ::    Line

    NbInputs = size(Input,1)
    NbSubModels = 1

    if ( size(Output,1) /= This%NbOutputs .or. size(Output,2) /= NbInputs )                                                       &
                                       call Error%Raise( 'Passed an output array of incorrect dimensionality', ProcName=ProcName )

    if ( size(Stat,1) /= NbInputs ) call Error%Raise( 'Passed a stat array of incorrect length', ProcName=ProcName )

    if ( .not. This%Silent ) then
      write(*,*)
      Line = 'Scheduling ' // ConvertToString(Value=NbInputs) // ' inputs with' // ConvertToString(Value=NbSubModels) //          &
                                      'submodels for a total of ' // ConvertToString(Value=NbInputs*NbSubModels) // ' evaluations'
      write(*,'(A)') Line
      Line = '  Number of concurrent input evaluations : 1'
      write(*,'(A)') Line
      Line = '  Number of concurrent submodel calls : 1'
      write(*,'(A)') Line
    end if

    i = 1
    do i = 1, NbInputs
      if ( .not. This%Silent ) then
        Line = '  Evaluation ' // ConvertToString(Value=i) // ' : Input ' // ConvertToString(Value=i) // ' Submodel 1'
        write(*,'(A)') Line
        Line = '  Initializing evaluation ' // ConvertToString(Value=i)
        write(*,'(A)') Line
      end if
      call This%Run(Input=Input(i), Output=Output(:,i), Stat=StatLoc )
      if ( .not. This%Silent ) then
        Line = '  Evaluation ' // ConvertToString(Value=i) // ' Complete'
        write(*,'(A)') Line
      end if
      if ( present(Stat) ) Stat(i) = StatLoc
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
