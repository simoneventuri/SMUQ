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

module Input_Class

use Parameters_Library
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    Input_Type

type                                                                  ::    Input_Type
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbInputs=0
  type(SMUQString_Type), dimension(:), allocatable                    ::    Label
  real(rkp), dimension(:), allocatable                                ::    Input
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructCase1,         &
                                                                                                          ConstructCase2
  procedure, private                                                  ::    ConstructCase1
  procedure, private                                                  ::    ConstructCase2
  generic, public                                                     ::    GetValue                =>    GetValue0D_LabelString, &
                                                                                                          GetValue0D_LabelChar,   &
                                                                                                          GetValue1D_Labels,      &
                                                                                                          GetValue1D
  procedure, public                                                   ::    GetValue0D_LabelString
  procedure, public                                                   ::    GetValue0D_LabelChar
  procedure, public                                                   ::    GetValue1D_Labels
  procedure, public                                                   ::    GetValue1D
  procedure, public                                                   ::    GetValuesPointer
  generic, public                                                     ::    Append                  =>    AppendInput0D_Char,     &
                                                                                                          AppendInput0D_String,   &
                                                                                                          AppendInput1D
  procedure, public                                                   ::    AppendInput0D_Char
  procedure, public                                                   ::    AppendInput0D_String
  procedure, public                                                   ::    AppendInput1D
  generic, public                                                     ::    Replace                 =>    Replace0D_String,       &
                                                                                                          Replace0D_Char,         &
                                                                                                          Replace1D
  procedure, public                                                   ::    Replace0D_String
  procedure, public                                                   ::    Replace0D_Char
  procedure, public                                                   ::    Replace1D
  generic, public                                                     ::    HasParameter            =>    HasParameter0D_Char,    &
                                                                                                          HasParameter0D_String,  &
                                                                                                          HasParameter1D
  procedure, public                                                   ::    HasParameter0D_Char
  procedure, public                                                   ::    HasParameter0D_String
  procedure, public                                                   ::    HasParameter1D
  generic, public                                                     ::    HasParameters           =>    HasParameters1D
  procedure, public                                                   ::    HasParameters1D
  generic, public                                                     ::    Transform               =>    Transform0D_Char,       &
                                                                                                          Transform0D_String,     &
                                                                                                          Transform1D
  procedure, public                                                   ::    Transform0D_String
  procedure, public                                                   ::    Transform0D_Char
  procedure, public                                                   ::    Transform1D
  procedure, public                                                   ::    GetNbInputs
  generic, public                                                     ::    GetLabel                =>    GetLabel0D,             &
                                                                                                          GetLabel1D
  procedure, private                                                  ::    GetLabel0D
  procedure, private                                                  ::    GetLabel1D
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(Input_Type), intent(inout)                                    ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  if (allocated(This%Input)) deallocate(This%Input, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Input', ProcName=ProcName, stat=StatLoc)

  This%NbInputs = 0
  
  if (allocated(This%Label)) deallocate(This%Label, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, Input, Labels)

  class(Input_Type), intent(inout)                                    ::    This
  real(rkp), dimension(:), intent(in)                                 ::    Input
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Labels
  
  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  call This%Reset()

  if (size(Labels,1) /= size(Input,1)) call Error%Raise(Line='Mismatch between number of labels and number of inputs',       &
                                                                                                              ProcName=ProcName)

  This%NbInputs = size(Input,1)

  allocate(This%Input, source=Input, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Input', ProcName=ProcName, stat=StatLoc)

  allocate(This%Label, source=Labels, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

  This%Constructed = .true.


  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase2(This)

  class(Input_Type), intent(inout)                                    ::    This
  
  character(*), parameter                                             ::    ProcName='ConstructEmpty'
  integer                                                             ::    StatLoc=0

  call This%Reset()

  This%NbInputs = 0

  allocate(This%Label(0), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine AppendInput0D_Char(This, Value, Label)

  class(Input_Type), intent(inout)                                    ::    This
  real(rkp), intent(in)                                               ::    Value
  character(*), intent(in)                                            ::    Label
  
  character(*), parameter                                             ::    ProcName='AppendInput0D_Char'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  type(SMUQString_Type), allocatable, dimension(:)                    ::    LabelsLoc
  real(rkp), allocatable, dimension(:)                                ::    ValuesLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  if (This%HasParameter(Label=Label)) call Error%Raise('Tried to append an input that was already part ' //                &
                                                                                      'of the input object', ProcName=ProcName)

  allocate(LabelsLoc(This%NbInputs+1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='LabelsLoc', ProcName=ProcName, stat=StatLoc)
  LabelsLoc(1:THis%NbInputs) = This%Label
  LabelsLoc(This%NbInputs+1) = Label

  allocate(ValuesLoc(This%NbInputs+1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='ValuesLoc', ProcName=ProcName, stat=StatLoc)
  ValuesLoc(1:This%NbInputs) = This%Input
  ValuesLoc(This%NbInputs+1) = Value

  call This%Construct(Input=ValuesLoc, Labels=LabelsLoc)

  deallocate(ValuesLoc, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='ValuesLoc', ProcName=ProcName, stat=StatLoc)

  deallocate(LabelsLoc, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='LabelsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------


!!------------------------------------------------------------------------------------------------------------------------------
  subroutine AppendInput0D_String(This, Value, Label)

    class(Input_Type), intent(inout)                                    ::    This
    real(rkp), intent(in)                                               ::    Value
    type(SMUQString_Type), intent(in)                                   ::    Label
    
    character(*), parameter                                             ::    ProcName='AppendInput0D_String'
    integer                                                             ::    StatLoc=0
    integer                                                             ::    i
    type(SMUQString_Type), allocatable, dimension(:)                    ::    LabelsLoc
    real(rkp), allocatable, dimension(:)                                ::    ValuesLoc
  
    if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)
  
    if (This%HasParameter(Label=Label)) call Error%Raise('Tried to append an input that was already part ' //                &
                                                                                        'of the input object', ProcName=ProcName)
  
    allocate(LabelsLoc(This%NbInputs+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LabelsLoc', ProcName=ProcName, stat=StatLoc)
    LabelsLoc(1:THis%NbInputs) = This%Label
    LabelsLoc(This%NbInputs+1) = Label
  
    allocate(ValuesLoc(This%NbInputs+1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='ValuesLoc', ProcName=ProcName, stat=StatLoc)
    ValuesLoc(1:This%NbInputs) = This%Input
    ValuesLoc(This%NbInputs+1) = Value
  
    call This%Construct(Input=ValuesLoc, Labels=LabelsLoc)
  
    deallocate(ValuesLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='ValuesLoc', ProcName=ProcName, stat=StatLoc)
  
    deallocate(LabelsLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='LabelsLoc', ProcName=ProcName, stat=StatLoc)
  
    end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine AppendInput1D(This, Values, Labels)

  class(Input_Type), intent(inout)                                    ::    This
  real(rkp), dimension(:), intent(in)                                 ::    Values
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Labels
  
  character(*), parameter                                             ::    ProcName='AppendInput1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii
  type(SMUQString_Type), allocatable, dimension(:)                    ::    LabelsLoc
  real(rkp), allocatable, dimension(:)                                ::    ValuesLoc
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    NbAppendInputs

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  if (This%HasParameters(Labels=Labels)) call Error%Raise('Tried to append inputs where at least one was already ' //        &
                                                                                  'part of the input object', ProcName=ProcName)

  NbAppendInputs = size(Values,1)

  allocate(LabelsLoc(This%NbInputs+NbAppendInputs), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='LabelsLoc', ProcName=ProcName, stat=StatLoc)
  LabelsLoc(1:This%NbInputs) = This%Label
  LabelsLoc(This%NbInputs+1:This%NbInputs+NbAppendInputs) = Labels

  allocate(ValuesLoc(This%NbInputs+NbAppendInputs), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='ValuesLoc', ProcName=ProcName, stat=StatLoc)
  ValuesLoc(1:This%NbInputs) = This%Input
  ValuesLoc(This%NbInputs+1:) = Values

  call This%Construct(Input=ValuesLoc, Labels=LabelsLoc)

  deallocate(ValuesLoc, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='ValuesLoc', ProcName=ProcName, stat=StatLoc)

  deallocate(LabelsLoc, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='LabelsLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetValue0D_LabelChar(This, Value, Label, Mandatory, Found)

  class(Input_Type), intent(in)                                       ::    This
  real(rkp), intent(out)                                              ::    Value
  character(*), intent(in)                                            ::    Label
  logical, optional, intent(in)                                       ::    Mandatory
  logical, optional, intent(out)                                      ::    Found
  
  character(*), parameter                                             ::    ProcName='GetValue0D_LabelChar'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  logical                                                             ::    FoundLoc
  logical                                                             ::    MandatoryLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  Value = Zero

  MandatoryLoc = .true.
  if (present(Mandatory)) MandatoryLoc = Mandatory

  FoundLoc = .false.

  i = 1
  do i = 1, This%NbInputs
    if (This%Label(i) == Label) then
      Value = This%Input(i)
      FoundLoc = .true.
      exit
    end if
  end do

  if (MandatoryLoc .and. .not. FoundLoc) call Error%Raise(Line='Mandatory label not found : ' // Label, ProcName=ProcName)

  if (present(Found)) Found = FoundLoc

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetValue0D_LabelString(This, Value, Label, Mandatory, Found)

    class(Input_Type), intent(in)                                       ::    This
    real(rkp), intent(out)                                              ::    Value
    type(SMUQString_Type), intent(in)                                   ::    Label
    logical, optional, intent(in)                                       ::    Mandatory
    logical, optional, intent(out)                                      ::    Found
    
    character(*), parameter                                             ::    ProcName='GetValue0D_LabelString'
    integer                                                             ::    StatLoc=0
    integer                                                             ::    i
    logical                                                             ::    FoundLoc
    logical                                                             ::    MandatoryLoc
  
    if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)
  
    Value = Zero
  
    MandatoryLoc = .true.
    if (present(Mandatory)) MandatoryLoc = Mandatory
  
    FoundLoc = .false.

    i = 1
    do i = 1, This%NbInputs
      if (This%Label(i) == Label) then
        Value = This%Input(i)
        FoundLoc = .true.
        exit
      end if
    end do
  
    if (MandatoryLoc .and. .not. FoundLoc) call Error%Raise(Line='Mandatory label not found : ' // Label, ProcName=ProcName)
  
    if (present(Found)) Found = FoundLoc
  
    end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetValue1D_Labels(This, Values, Labels, Mandatory, Found)

  class(Input_Type), intent(in)                                       ::    This
  real(rkp), allocatable, dimension(:), intent(out)                   ::    Values
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Labels
  logical, optional, intent(in)                                       ::    Mandatory
  logical, optional, intent(out)                                      ::    Found
  
  character(*), parameter                                             ::    ProcName='GetValue1D_Labels'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    NbLabels
  integer                                                             ::    i
  integer                                                             ::    ii
  logical                                                             ::    FoundLoc
  logical                                                             ::    MandatoryLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  MandatoryLoc = .true.
  if (present(Mandatory)) MandatoryLoc = Mandatory

  NbLabels = size(Labels,1)

  allocate(Values(NbLabels), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  Values = Zero

  ii = 1
  do ii = 1, NbLabels    
    i = 1
    FoundLoc = .false.
    do i = 1, This%NbInputs
      if (This%Label(i) == Labels(ii)) then
        Values(ii) = This%Input(i)
        FoundLoc = .true.
        exit
      end if
    end do
    if (.not. FoundLoc) then
      if (MandatoryLoc) call Error%Raise(Line='Mandatory label not found : ' // Labels(ii)%Get(), ProcName=ProcName)
      exit
    end if
  end do

  if (present(Found)) Found = FoundLoc

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine GetValue1D(This, Values)

  class(Input_Type), intent(in)                                       ::    This
  real(rkp), allocatable, dimension(:), intent(out)                   ::    Values
  
  character(*), parameter                                             ::    ProcName='GetValue1D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  allocate(Values, source=This%Input, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetValuesPointer(This)

  real(rkp), pointer, dimension(:)                                    ::    GetValuesPointer
  class(Input_Type), target, intent(in)                               ::    This

  character(*), parameter                                             ::    ProcName='GetValuesPointer'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  GetValuesPointer => This%Input

  end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function HasParameter0D_String(This, Label)

  logical                                                             ::    HasParameter0D_String

  class(Input_Type), target, intent(in)                               ::    This
  type(SMUQString_Type), intent(in)                                   ::    Label

  character(*), parameter                                             ::    ProcName='HasParameter0D_String'
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  HasParameter0D_String = .false.

  i = 1
  do i = 1, This%NbInputs
    if (This%Label(i) /= Label) cycle
    HasParameter0D_String = .true.
    exit
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function HasParameter0D_Char(This, Label)

  logical                                                             ::    HasParameter0D_Char

  class(Input_Type), target, intent(in)                               ::    This
  character(*), intent(in)                                            ::    Label

  character(*), parameter                                             ::    ProcName='HasParameter0D_Char'
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  HasParameter0D_Char = .false.
  i = 1
  do i = 1, This%NbInputs
    if (This%Label(i) /= Label) cycle
    HasParameter0D_Char = .true.
    exit
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function HasParameter1D(This, Labels)

  logical, allocatable, dimension(:)                                  ::    HasParameter1D

  class(Input_Type), target, intent(in)                               ::    This
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Labels

  character(*), parameter                                             ::    ProcName='HasParameter1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    NbLabels

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  NbLabels = size(Labels,1)

  allocate(HasParameter1D(NbLabels), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='HasParameter1D', ProcName=ProcName, stat=StatLoc)

  HasParameter1D = .false.

  ii = 1
  do ii = 1, NbLabels
    i = 1
    do i = 1, This%NbInputs
      if (This%Label(i) /= Labels(ii)) cycle
      HasParameter1D(ii) = .true.
      exit
    end do
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function HasParameters1D(This, Labels)

  logical                                                             ::    HasParameters1D

  class(Input_Type), target, intent(in)                               ::    This
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Labels

  character(*), parameter                                             ::    ProcName='HasParameters1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    iii
  character(:), allocatable                                           ::    LabelLoc
  integer                                                             ::    NbLabels

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  NbLabels = size(Labels,1)

  HasParameters1D = .true.

  ii = 1
  do ii = 1, NbLabels
    i = 1
    iii = 0
    do i = 1, This%NbInputs
      if (This%Label(i) == Labels(ii)) then
        iii = i
      end if
    end do
    if (iii == 0) HasParameters1D = .false.
    if (.not. HasParameters1D) exit
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Replace0D_Char(This, Value, Label)

  class(Input_Type), intent(inout)                                    ::    This
  real(rkp), intent(in)                                               ::    Value
  character(*), intent(in)                                            ::    Label
  
  character(*), parameter                                             ::    ProcName='Replace0D_Char'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  logical                                                             ::    Found

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  Found = .false.
  i = 1
  do i = 1, This%NbInputs
    if (This%Label(i) /= Label) cycle
    This%Input(i) = Value
    Found = .true.  
    exit
  end do
  if (.not. Found) call Error%Raise('Did not find input with label :' // Label, ProcName=ProcName)

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Replace0D_String(This, Value, Label)

  class(Input_Type), intent(inout)                                    ::    This
  real(rkp), intent(in)                                               ::    Value
  type(SMUQString_Type), intent(in)                                   ::    Label
  
  character(*), parameter                                             ::    ProcName='Replace0D_String'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  logical                                                             ::    Found

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  Found = .false.
  i = 1
  do i = 1, This%NbInputs
    if (This%Label(i) /= Label) cycle
    This%Input(i) = Value
    Found = .true.  
    exit
  end do
  if (.not. Found) call Error%Raise('Did not find input with label :' // Label, ProcName=ProcName)

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Replace1D(This, Values, Labels)

  class(Input_Type), intent(inout)                                    ::    This
  real(rkp), dimension(:), intent(in)                                 ::    Values
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Labels
  
  character(*), parameter                                             ::    ProcName='Replace1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    LabelLoc
  integer                                                             ::    i
  integer                                                             ::    ii
  logical                                                             ::    Found
  integer                                                             ::    NbLabels

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  NbLabels = size(Labels,1)

  if (NbLabels /= size(Values,1)) call Error%Raise('Mismatch in number of labels and values', ProcName=ProcName)

  ii = 1
  do ii = 1, NbLabels
    Found = .false.
    i = 1
    do i = 1, This%NbInputs
      if (This%Label(i) /= Labels(ii)) cycle
      This%Input(i) = Values(ii)
      Found = .true.
      exit
    end do
    if (.not. Found) call Error%Raise('Did not find input with label :' // LabelLoc, ProcName=ProcName)
  end do

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Transform0D_Char(This, Transformation, Label, Mandatory, Found)

  class(Input_Type), intent(inout)                                    ::    This
  character(*), intent(in)                                            ::    Transformation
  character(*), intent(in)                                            ::    Label
  logical, optional, intent(in)                                       ::    Mandatory
  logical, optional, intent(out)                                      ::    Found
  
  character(*), parameter                                             ::    ProcName='Transform0D_Char'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  logical                                                             ::    MandatoryLoc
  logical                                                             ::    FoundLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  MandatoryLoc = .true.
  if (present(Mandatory)) MandatoryLoc = Mandatory

  FoundLoc = .true.

  i = 1
  do i = 1, This%NbInputs
    if (This%Label(i) == Label) then
      call Transform(Transformation=Transformation, Value=This%Input(i))
      exit
    end if
    if (i == This%NbInputs) FoundLoc = .false.
  end do
  if (.not. FoundLoc .and. MandatoryLoc) call Error%Raise('Did not find mandatory input parameter : ' // Label,                   &
                                                                                                                 ProcName=ProcName)

  if (present(Found)) Found = FoundLoc

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform0D_String(This, Transformation, Label, Mandatory, Found)

    class(Input_Type), intent(inout)                                    ::    This
    type(SMUQString_Type), intent(in)                                   ::    Transformation
    type(SMUQString_Type), intent(in)                                   ::    Label
    logical, optional, intent(in)                                       ::    Mandatory
    logical, optional, intent(out)                                      ::    Found
    
    character(*), parameter                                             ::    ProcName='Transform0D_String'
    integer                                                             ::    StatLoc=0
    integer                                                             ::    i
    logical                                                             ::    MandatoryLoc
    logical                                                             ::    FoundLoc
  
    if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)
  
    MandatoryLoc = .true.
    if (present(Mandatory)) MandatoryLoc = Mandatory
  
    FoundLoc = .true.
  
    i = 1
    do i = 1, This%NbInputs
      if (This%Label(i) == Label) then
        call Transform(Transformation=Transformation, Value=This%Input(i))
        exit
      end if
      if (i == This%NbInputs) FoundLoc = .false.
    end do
    if (.not. FoundLoc .and. MandatoryLoc) call Error%Raise('Did not find mandatory input parameter : ' // Label,                 &
                                                                                                                 ProcName=ProcName)
  
    if (present(Found)) Found = FoundLoc
  
    end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Transform1D(This, Transformations, Labels, Mandatory, Found)

  class(Input_Type), intent(inout)                                    ::    This
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Transformations
  type(SMUQString_Type), dimension(:), intent(in)                     ::    Labels
  logical, optional, intent(in)                                       ::    Mandatory
  logical, optional, intent(out)                                      ::    Found
  
  character(*), parameter                                             ::    ProcName='Transform1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  logical                                                             ::    MandatoryLoc
  logical                                                             ::    FoundLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  if (size(Labels,1) /= size(Transformations,1)) call Error%Raise('Mismatch in number of transformation and labels',         &
                                                                                                              ProcName=ProcName)

  MandatoryLoc = .true.
  if (present(Mandatory)) MandatoryLoc = Mandatory

  i = 1
  do i = 1, size(Labels,1)
    call This%Transform(Transformation=Transformations(i), Label=Labels(i), Mandatory=MandatoryLoc, Found=FoundLoc)
  end do

  if (present(Found)) Found = FoundLoc

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------
function GetNbInputs(This)

  integer                                                             ::    GetNbInputs

  class(Input_Type), intent(in)                                       ::    This
  
  character(*), parameter                                             ::    ProcName='GetNbInputs'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object not constructed', ProcName=ProcName)

  GetNbInputs = This%NbInputs

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetLabel0D(This, Num)

  character(:), allocatable                                           ::    GetLabel0D

  class(Input_Type), intent(in)                                       ::    This
  integer, intent(in)                                                 ::    Num

  character(*), parameter                                             ::    ProcName='GetLabel0D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetLabel0D = This%Label(Num)%Get()

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetLabel1D(This)

  type(SMUQString_Type), allocatable, dimension(:)                    ::    GetLabel1D
  class(Input_Type), intent(in)                                       ::    This

  character(*), parameter                                             ::    ProcName='GetLabel1D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  allocate(GetLabel1D, source=This%Label, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetLabel1D', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(Input_Type), intent(out)                                      ::    LHS
  class(Input_Type), intent(in)                                       ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (Input_Type)
      call LHS%Reset
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%Input, source=RHS%Input, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Input', ProcName=ProcName, stat=StatLoc)
        LHS%NbInputs = RHS%NbInputs
        allocate(LHS%Label, source=RHS%Label, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%InputLabel', ProcName=ProcName, stat=StatLoc)
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(Input_Type),intent(inout)                                      ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Input)) deallocate(This%Input, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Input', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Label)) deallocate(This%Label, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Name)) deallocate(This%Name, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Name', ProcName=ProcName, stat=StatLoc)

  end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
