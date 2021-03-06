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

module OutputReader_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Output_Class                                                  ,only:    Output_Type
use OFileFormated_Class                                           ,only:    OFileFormated_Type
use OFileFormatedContainer_Class                                  ,only:    OFileFormatedContainer_Type
use OFileFormated_Factory_Class                                   ,only:    OFileFormated_Factory
use LinkedList2D_Class                                            ,only:    LinkedList2D_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    OutputReader_Type

type                                                                  ::    Cell_Type
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Label
  type(OFileFormatedContainer_Type), allocatable, dimension(:)        ::    OFile
  integer                                                             ::    NbOFiles
contains
  procedure, public                                                   ::    Reset                   =>    Reset_Cell
  generic, public                                                     ::    Construct               =>    ConstructInput_Cell
  procedure, private                                                  ::    ConstructInput_Cell
  procedure, public                                                   ::    GetInput                =>    GetInput_Cell
  procedure, public                                                   ::    ReadOutput              =>    ReadOutput_Cell
  procedure, public                                                   ::    GetLabel                =>    GetLabel_Cell
  procedure, public                                                   ::    Exists                  =>    Exists_Cell
  generic, public                                                     ::    assignment(=)           =>    Copy_Cell
  procedure, public                                                   ::    Copy_Cell
  final                                                               ::    Finalizer_Cell
end type

type                                                                  ::    OutputReader_Type
  logical                                                             ::    Constructed=.false.
  type(Cell_Type), allocatable, dimension(:)                          ::    Cells
  integer                                                             ::    NbCells
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    ReadOutput
  procedure, public                                                   ::    AllFound
  procedure, public                                                   ::    GetNbOutputs
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(OutputReader_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0
  if (allocated(This%Cells)) deallocate(This%Cells, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)

  This%NbCells = 0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(OutputReader_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  SectionName = 'outputs'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  This%NbCells = InputSection%GetNumberofSubSections()
  nullify(InputSection)

  allocate(This%Cells(This%NbCells), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, This%NbCells
    SubSectionName = SectionName // '>output' // ConvertToString(i)
    call InputVerifier%AddSection(Section='output' // ConvertToString(i) ,ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call This%Cells(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
  end do

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput

  class(OutputReader_Type), intent(in)                                ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  class(OFileFormated_Type), pointer                                  ::    OFile=>null()
  integer                                                             ::    i
  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  SectionName = 'outputs'
  call GetInput%AddSection(SectionName=SectionName)
  
  i = 1
  do i = 1, This%NbCells
    SubSectionName = 'output' // ConvertToString(Value=i)
    if (ExternalFlag) DirectorySub = DirectoryLoc // 'output' // ConvertToString(Value=i) // '/'
    call GetInput%AddSection(Section=This%Cells(i)%GetInput(Name=SubSectionName, Prefix=PrefixLoc,                 &
                                                                            Directory=DirectorySub), To_SubSection=SectionName)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ReadOutput(This, Output, AllMandatory, AllFound, Found)

  class(OutputReader_Type), intent(inout)                             ::    This
  type(Output_Type), dimension(:), intent(inout)                      ::    Output
  logical, optional, intent(in)                                       ::    AllMandatory
  logical, optional, intent(out)                                      ::    AllFound
  logical, dimension(:), optional, intent(inout)                      ::    Found

  character(*), parameter                                             ::    ProcName='ReadOutput'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  logical                                                             ::    AllMandatoryLoc
  logical                                                             ::    AllFoundLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (size(Output,1) /= This%NbCells) call Error%Raise(Line='Passed down incorrect size Output array', ProcName=ProcName)

  if (present(Found)) then
    if (size(Found,1) /= This%NbCells) call Error%Raise(Line='Passed down incorrect size Output array', ProcName=ProcName)
  end if

  AllMandatoryLoc = .true.
  if (present(AllMandatory)) AllMandatoryLoc = AllMandatory

  if (present(AllFound)) AllFound = .true.

  if (present(Found)) Found = .true.

  i = 1
  do i = 1, This%NbCells
    if (.not. This%Cells(i)%Exists()) then
      if (AllMandatoryLoc) call Error%Raise('Could not find output : ' // This%Cells(i)%GetLabel(), ProcName=ProcName)
      if (present(AllFound)) AllFound = .false.
      if (present(Found)) Found(i) = .false.
      cycle
    end if
    call This%Cells(i)%ReadOutput(Output=Output(i))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function AllFound(This)

  logical                                                             ::    AllFound

  class(OutputReader_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='AllFound'
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  AllFound = .true.

  i = 1
  do i = 1, This%NbCells
    if (This%Cells(i)%Exists()) cycle
    AllFound = .false.
    exit
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetNbOutputs(This)

  integer                                                             ::    GetNbOutputs

  class(OutputReader_Type), intent(in)                                ::    This

  character(*), parameter                                             ::    ProcName='GetNbOutputs'
  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetNbOutputs = This%NbCells

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Copy(LHS, RHS)

  class(OutputReader_Type), intent(out)                               ::    LHS
  class(OutputReader_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  select type (RHS)

    type is (OutputReader_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%NbCells = RHS%NbCells
        allocate(LHS%Cells, source=RHS%Cells, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Cells', ProcName=ProcName, stat=StatLoc)
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Finalizer(This)

  type(OutputReader_Type),intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Cells)) deallocate(This%Cells, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Cells', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
!!------------------------------------------------------------------------------------------------------------------------------
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset_Cell(This)

  class(Cell_Type), intent(inout)                                     ::    This

  character(*), parameter                                             ::    ProcName='Reset_Cell'
  integer                                                             ::    StatLoc=0
  if (allocated(This%OFile)) deallocate(This%OFile, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%OFile', ProcName=ProcName, stat=StatLoc)

  This%NbOFiles = 0

  This%Label = '<undefined>'

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput_Cell(This, Input, Prefix)

  use StringConversion_Module

  class(Cell_Type), intent(inout)                                     ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput_Cell'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  class(OFileFormated_Type), allocatable                              ::    OFile
  class(OFileFormated_Type), pointer                                  ::    OFilePointer=>null()
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  integer                                                             ::    i
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'label'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Label = VarC0D

  SectionName = 'files'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  This%NbOFiles = InputSection%GetNumberofSubSections()
  nullify(InputSection)

  allocate(This%OFile(This%NbOFiles), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%OFile', ProcName=ProcName, stat=StatLoc)
  
  i = 1
  do i = 1, This%NbOFiles
    SubSectionName = SectionName // '>file' // ConvertToString(i)
    call InputVerifier%AddSection(Section='file' // ConvertToString(i) ,ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call OFileFOrmated_Factory%Construct(Object=OFile, Input=InputSection, Prefix=PrefixLoc)
    call This%OFile(i)%Set(Object=OFile) 
    deallocate(OFile, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='OFile', ProcName=ProcName, stat=StatLoc)
    nullify(InputSection)
  end do

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput_Cell(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput_Cell

  class(Cell_Type), intent(in)                                        ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput_Cell'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  class(OFileFormated_Type), pointer                                  ::    OFile=>null()
  integer                                                             ::    i
  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput_Cell%SetName(SectionName = trim(adjustl(Name)))

  call GetInput_Cell%AddParameter(Name='label', Value=This%Label)

  SectionName = 'files'
  call GetInput_Cell%AddSection(SectionName=SectionName)
  
  i = 1
  do i = 1, This%NbOFiles
    SubSectionName = 'file' // ConvertToString(Value=i)
    OFile => This%OFile(i)%GetPointer()
    if (ExternalFlag) DirectorySub = DirectoryLoc // 'file' // ConvertToString(Value=i) // '/'
    call GetInput_Cell%AddSection(Section=OFileFormated_Factory%GetObjectInput(Name=SubSectionName, Object=OFile,  &
                                                          Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SectionName)
    nullify(OFile)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ReadOutput_Cell(This, Output)

  class(Cell_Type), intent(inout)                                     ::    This
  type(Output_Type), intent(inout)                                    ::    Output

  character(*), parameter                                             ::    ProcName='ReadOutput_Cell'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii
  class(OFileFormated_Type), pointer                                  ::    OFile=>null()
  type(LinkedList2D_Type)                                             ::    Outputs
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  real(rkp), dimension(:,:), pointer                                  ::    VarR2DPointer=>null()
  integer                                                             ::    OutputSize
  integer                                                             ::    OutputNbDegen

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  OutputSize = 0
  i = 1
  do i = 1, This%NbOFiles
    OFile => This%OFile(i)%GetPointer()
    call OFile%ReadOutput(Values=VarR2D)
    if (i == 1) OutputNbDegen = size(VarR2D,2)
    if (OutputNbDegen /= size(VarR2D,2)) call Error%Raise('Output has different number of realizations between files : ' //  &
                                                                                                  This%Label, ProcName=ProcName)
    OutputSize = OutputSize + size(VarR2D,1)
    call Outputs%Append(Values=VarR2D)
    nullify(OFile)
    deallocate(VarR2D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)
  end do

  allocate(VarR2D(OutputSize,OutputNbDegen), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  ii = 0
  i = 1
  do i = 1, This%NbOFiles
    call Outputs%GetPointer(Node=i, Values=VarR2DPointer)
    VarR2D(ii+1:ii+size(VarR2DPointer,1),:) = VarR2DPointer
    ii = ii + size(VarR2DPointer,1)
    nullify(VarR2DPointer)
  end do

  call Output%Construct(Values=VarR2D, Label=This%Label)

  call Outputs%Purge()

  deallocate(VarR2D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Exists_Cell(This)

  logical, allocatable                                                ::    Exists_Cell

  class(Cell_Type), intent(inout)                                     ::    This

  character(*), parameter                                             ::    ProcName='Exists_Cell'
  integer                                                             ::    i
  class(OFileFormated_Type), pointer                                  ::    OFile=>null()

  Exists_Cell = .true.
  i = 1
  do i = 1, This%NbOFiles
    OFile => This%OFile(i)%GetPointer()
    if (OFile%Exists()) cycle
    Exists_Cell = .false.
    exit
  end do

  nullify(OFile)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetLabel_Cell(This)

  Character(:), allocatable                                           ::    GetLabel_Cell

  class(Cell_Type), intent(in)                                        ::    This

  character(*), parameter                                             ::    ProcName='GetLabel_Cell'

  GetLabel_Cell = This%Label

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy_Cell(LHS, RHS)

  class(Cell_Type), intent(out)                                       ::    LHS
  class(Cell_Type), intent(in)                                        ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  select type (RHS)

    type is (Cell_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%NbOFiles = RHS%NbOFiles
        allocate(LHS%OFile, source=RHS%OFile, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%OFile', ProcName=ProcName, stat=StatLoc)
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer_Cell(This)

  type(Cell_Type),intent(inout)                                       ::    This

  character(*), parameter                                             ::    ProcName='Finalizer_Cell'
  integer                                                             ::    StatLoc=0

  if (allocated(This%OFile)) deallocate(This%OFile, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%OFile', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
