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

module MFileTable_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use MFileInput_Class                                              ,only:    MFileInput_Type
use ITableValue_Class                                             ,only:    ITableValue_Type
use ITableValueContainer_Class                                    ,only:    ITableValueContainer_Type
use ITableValue_Factory_Class                                     ,only:    ITableValue_Factory
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use Input_Class                                                   ,only:    Input_Type
use List1D_Class                                                  ,only:    List1D_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                              ::    MFileTable_Type

type, extends(MFileInput_Type)                                      ::    MFileTable_Type
  type(ITableValueContainer_Type), allocatable, dimension(:)        ::    MParam
  integer                                                           ::    NbMParams=0
  integer                                                           ::    AbscissaColumn=1
  type(List1D_Type), allocatable, dimension(:)                      ::    ParamColumn
  type(SMUQString_Type), allocatable, dimension(:)                  ::    ParamFormat
  character(:), allocatable                                         ::    Identifier
contains
  procedure, public                                                 ::    Reset
  procedure, private                                                ::    ConstructInput
  procedure, public                                                 ::    GetInput
  procedure, public                                                 ::    WriteInput
  procedure, public                                                 ::    Copy
  final                                                             ::    Finalizer
end type

logical   ,parameter                                                ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(MFileTable_Type), intent(inout)                             ::    This
  character(*), parameter                                           ::    ProcName='Reset'
  integer                                                           ::    StatLoc = 0

  if (allocated(This%MParam)) deallocate(This%MParam, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%MParam', ProcName=ProcName, stat=StatLoc)

  This%NbMParams = 0

  if (allocated(This%ParamColumn)) deallocate(This%ParamColumn, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamColumn', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%ParamFormat)) deallocate(This%ParamFormat, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc)

  This%Identifier = ''

  This%Constructed = .false.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(MFileTable_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                               ::    Input
  character(*), optional, intent(in)                                ::    Prefix
  character(*), parameter                                           ::    ProcName='ConstructInput'
  character(:), allocatable                                         ::    PrefixLoc
  integer                                                           ::    StatLoc=0
  type(InputSection_Type), pointer                                  ::    InputSection=>null()
  class(ITableValue_Type), allocatable                              ::    MParam
  character(:), allocatable                                         ::    ParameterName
  character(:), allocatable                                         ::    SectionName
  character(:), allocatable                                         ::    SubSectionName
  character(:), allocatable                                         ::    VarC0D
  integer                                                           ::    VarI0D
  integer, allocatable, dimension(:)                                ::    VarI1D
  logical                                                           ::    Found
  integer                                                           ::    i, ii
  type(LinkedList0D_Type)                                           ::    ColumnRecord
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'identifier'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarC0D, ParameterName=Parametername, Mandatory=.true.)
  if (len_trim(VarC0D) == 0) call Error%Raise('Specified an empty identifier', ProcName=ProcName)
  This%Identifier = VarC0D

  This%AbscissaColumn = 1
  ParameterName = 'abscissa_column'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=Parametername, Mandatory=.false., Found=Found)
  if (Found) This%AbscissaColumn = VarI0D  

  SectionName = 'parameters'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  This%NbMParams = InputSection%GetNumberofSubSections()

  allocate(This%MParam(This%NbMParams), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%MParam', ProcName=ProcName, stat=StatLoc)

  allocate(This%ParamColumn(This%NbMParams), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%ParamColumn', ProcName=ProcName, stat=StatLoc)

  allocate(This%ParamFormat(This%NbMParams), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc)
  
  ParameterName = 'format'
  call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
  call Input%GetValue(Value=VarC0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.false., Found=Found)
  if (.not. Found) VarC0D = 'G0'
  i = 1
  do i = 1, THis%NbMParams
    This%ParamFormat(i) = VarC0D
  end do

  i = 1
  do i = 1, This%NbMParams
    SubSectionName = SectionName // '>parameter' // ConvertToString(Value=i)
    call InputVerifier%AddSection(Section='parameter' // ConvertToString(Value=i), ToSubSection=SectionName)

    ParameterName = 'format'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SubSectionName)
    call Input%GetValue(Value=VarC0D, ParameterName=Parametername, SectionName=SubSectionName, Mandatory=.false., Found=Found)
    if (Found) This%ParamFormat(i) = VarC0D

    ParameterName = 'column'
    call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SubSectionName)
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
    call ConvertToIntegers(String=VarC0D, Values=VarI1D)
    if (any(VarI1D <= 0)) call Error%Raise(Line='Specified column 0 or less', ProcName=ProcName)
    call This%ParamColumn(i)%Set(Values=VarI1D)
    call ColumnRecord%Append(Values=VarI1D)

    call InputVerifier%AddSection(Section='parameter', ToSubSection=SubSectionName)
    SubSectionName = SubSectionName // '>parameter'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call ITableValue_Factory%Construct(Object=MParam, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
    call This%MParam(i)%Set(Object=MParam)

    deallocate(MParam, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='MParam', ProcName=ProcName, stat=StatLoc)

    deallocate(VarI1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
  end do

  call ColumnRecord%Get(Values=VarI1D)
  if (any(VarI1D == This%AbscissaColumn)) call Error%Raise(Line='One of the parameter column specifications coincides with'  &
                                                                                      // ' abscissa column', ProcName=ProcName)
  VarI0D = size(VarI1D,1)
  i = 1
  do i = 1, VarI0D-1
    if (any(VarI1D(i+1:VarI0D) == VarI1D(i))) call Error%Raise(Line='Multiple parameters have same column specification',  &
                                                                                                              ProcName=ProcName)
  end do

  deallocate(VarI1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                           ::    GetInput

  class(MFileTable_Type), intent(inout)                             ::    This
  character(*), intent(in)                                          ::    Name
  character(*), optional, intent(in)                                ::    Prefix
  character(*), optional, intent(in)                                ::    Directory
  character(*), parameter                                           ::    ProcName='GetInput'
  character(:), allocatable                                         ::    PrefixLoc
  integer                                                           ::    StatLoc=0
  character(:), allocatable                                         ::    DirectoryLoc
  character(:), allocatable                                         ::    DirectorySub
  logical                                                           ::    ExternalFlag=.false.
  type(InputSection_Type), pointer                                  ::    InputSection=>null()
  class(ITableValue_Type), pointer                                  ::    MParam=>null()
  character(:), allocatable                                         ::    SectionName
  character(:), allocatable                                         ::    SubSectionName
  integer                                                           ::    i
  integer, allocatable, dimension(:)                                ::    VarI1D

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName=trim(adjustl(Name)))

  call GetInput%AddParameter(Name='identifier', Value=ConvertToString(Value=This%Identifier))

  call GetInput%AddParameter(Name='abscissa_column', Value=ConvertToString(Value=This%AbscissaColumn))

  SectionName = 'parameters'
  call GetInput%AddSection(SectionName=SectionName)

  i = 1
  do i = 1, This%NbMParams
    SubSectionName = 'parameter' // ConvertToString(Value=i)
    call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
    SubSectionName = SectionName // '>' // SubSectionName 
    call GetInput%AddParameter(Name='format', Value=This%ParamFormat(i)%Get(), SectionName=SubSectionName)
    call This%ParamColumn(i)%Get(Values=VarI1D)
    call GetInput%AddParameter(Name='column', Value=ConvertToString(Values=VarI1D), SectionName=SubSectionName)
    deallocate(VarI1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)

    MParam => This%MParam(i)%GetPointer()
    if (ExternalFlag) DirectorySub = DirectoryLoc // 'parameter' // ConvertToString(Value=i) // '/'
    call GetInput%AddSection(Section=ITableValue_Factory%GetObjectInput(Name='parameter', Object=MParam,            &
                                                        Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SubSectionName)
    nullify(MParam)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine WriteInput(This, Input, Template, ProcessedTemplate, File)

  class(MFileTable_Type), intent(inout)                             ::    This
  type(Input_Type), intent(in)                                      ::    Input
  type(SMUQString_Type), dimension(:), intent(in)                   ::    Template
  type(SMUQString_Type), dimension(:), intent(inout)                ::    ProcessedTemplate
  type(SMUQFile_Type), intent(in)                                   ::    File

  character(*), parameter                                           ::    ProcName='WriteInput'
  integer                                                           ::    StatLoc=0
  integer                                                           ::    NbLines=0
  character(:), allocatable                                         ::    VarC0D
  class(ITableValue_Type), pointer                                  ::    MParamPointer=>null()
  integer                                                           ::    i, ii, iii, iv
  integer, allocatable, dimension(:)                                ::    VarI1D
  type(SMUQString_Type), allocatable, dimension(:,:)                ::    NewEntry
  integer                                                           ::    NbEntries
  character(:), allocatable                                         ::    CommentChar
  integer                                                           ::    CommentCharLen
  character(:), allocatable                                         ::    SeparatorChar
  real(rkp), allocatable, dimension(:)                              ::    Abscissa
  type(SMUQString_Type), allocatable, dimension(:)                  ::    VarString1D
  integer                                                           ::    TableStart
  integer                                                           ::    TableEnd
  integer                                                           ::    NbColumns

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (size(ProcessedTemplate,1) /= size(Template,1)) call Error%Raise(Line='Mismatch in template and processed template ' // &
                                                                                                    'sizes', ProcName=ProcName)

  CommentChar = File%GetComment()
  CommentCharLen = len_trim(CommentChar)
  SeparatorChar = File%GetSeparator()
  NbColumns = 0
  TableStart = 0
  TableEnd = 0
  NbEntries = 0

  NbLines = size(Template,1)

  i = 1
  ii = 0
  do i = 1, NbLines
    VarC0D = trim(adjustl(Template(i)%Get()))
    if (len_trim(VarC0D) == 0) cycle
    if (VarC0D(1:CommentCharLen) == CommentChar) cycle
    if (VarC0D == '{' // This%Identifier // '}') then
      if (TableStart == 0) then
        TableStart = i
        cycle
      elseif (TableEnd == 0) then
        TableEnd = i
        cycle
      else
        call Error%Raise('Detected more than one specified table : ' // This%Identifier, ProcName=ProcName)
      end if
    end if
    if (TableStart == 0) cycle
    if (TableEnd /= 0) cycle
    NbEntries = NbEntries + 1
  end do

  if (TableStart - TableEnd == 1) call Error%Raise('Specified an empty template table', ProcName=ProcName)
  if (TableStart == 0) call Error%Raise('Did not find start of table: ' // This%Identifier, ProcName=ProcName)
  if (TableEnd == 0) call Error%Raise('Did not find end of table: ' // This%Identifier, ProcName=ProcName)

  i = TableStart
  do i = TableStart, TableEnd-2
    ProcessedTemplate(i) = Template(i+1)
  end do
  ProcessedTemplate(TableEnd-1) = ''
  ProcessedTemplate(TableEnd) = ''

  if (TableEnd < NbLines) then
    i = TableEnd-1
    do i = TableEnd-1, Nblines-2
      ProcessedTemplate(i) = Template(i+2)
    end do
  end if
  ProcessedTemplate(NbLines-1) = '' 
  ProcessedTemplate(NbLines) = '' 

  TableEnd = TableEnd - 2

  allocate(Abscissa(NbEntries), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Abscissa', ProcName=ProcName, stat=StatLoc)
  Abscissa = Zero

  call ProcessedTemplate(TableStart)%Split(Separator=SeparatorChar, Strings=VarString1D)
  NbColumns = size(VarString1D,1)

  if (This%AbscissaColumn <= NbColumns) then
    i = 1
    ii = 0
    do i = TableStart, TableEnd
      VarC0D = trim(adjustl(ProcessedTemplate(i)%Get()))  
      if (len_trim(VarC0D) == 0) cycle
      if (VarC0D(1:CommentCharLen) == CommentChar) cycle
      ii = ii + 1
      call ProcessedTemplate(i)%Split(Separator=SeparatorChar, Strings=VarString1D)
      Abscissa(ii) = ConvertToReal(String=VarString1D(This%AbscissaColumn)%Get())
    end do
  end if

  allocate(NewEntry(NbEntries,This%NbMParams), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='NewEntry', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, This%NbMParams
    MParamPointer => This%MParam(i)%GetPointer()
    call MParamPointer%GetStringValue(Input=Input, Abscissa=Abscissa, Strings=NewEntry(:,i), Format=This%ParamFormat(i)%Get())
    nullify(MParamPointer)
  end do

  i = 1
  ii = 0
  do i = TableStart, TableEnd
    VarC0D = trim(adjustl(ProcessedTemplate(i)%Get()))  
    if (len_trim(VarC0D) == 0) cycle
    if (VarC0D(1:CommentCharLen) == CommentChar) cycle
    ii = ii + 1
    call ProcessedTemplate(i)%Split(Separator=SeparatorChar, Strings=VarString1D)
    iii = 1
    do iii = 1, This%NbMParams
      call This%ParamColumn(iii)%Get(Values=VarI1D)
      iv = 1
      do iv = 1, size(VarI1D,1)
        VarString1D(VarI1D(iv)) = NewEntry(ii,iii)%Get()
      end do
      deallocate(VarI1D, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='VarI1D', ProcName=ProcName, stat=StatLoc)
    end do
    ProcessedTemplate(i) = ConvertToString(Values=VarString1D, Separator=SeparatorChar)
  end do

  deallocate(NewEntry, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='NewEntry', ProcName=ProcName, stat=StatLoc)

  deallocate(VarString1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(MFileTable_Type), intent(out)                               ::    LHS
  class(MFileInput_Type), intent(in)                                ::    RHS

  character(*), parameter                                           ::    ProcName='Copy'
  integer                                                           ::    StatLoc=0
  integer                                                           ::    i

  select type (RHS)

    type is (MFileTable_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%Identifier = RHS%Identifier
        LHS%NbMParams = RHS%NbMParams
        LHS%AbscissaColumn = RHS%AbscissaColumn
        allocate(LHS%ParamColumn, source=RHS%ParamColumn, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%ParamColumn', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%MParam, source=RHS%MParam, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%MParam', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%ParamFormat, source=RHS%ParamFormat, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%ParamFormat', ProcName=ProcName, stat=StatLoc)
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(MFileTable_Type),intent(inout)                               ::    This

  character(*), parameter                                           ::    ProcName='Finalizer'
  integer                                                           ::    StatLoc=0

  if (allocated(This%MParam)) deallocate(This%MParam, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%MParam', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%ParamColumn)) deallocate(This%ParamColumn, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamColumn', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%ParamFormat)) deallocate(This%ParamFormat, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamFormat', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
