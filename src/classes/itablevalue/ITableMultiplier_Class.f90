! -*-f90-*-
!!----------------------------------------------------------------------------------------------------------------------------------
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
!!----------------------------------------------------------------------------------------------------------------------------------

module ITableMultiplier_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use ComputingRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use ITableValue_Class                                             ,only:    ITableValue_Type
use Input_Class                                                   ,only:    Input_Type
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use IScalarValueContainer_Class                                   ,only:    IScalarValueContainer_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    ITableMultiplier_Type

type, extends(ITableValue_Type)                                       ::    ITableMultiplier_Type
  real(rkp), allocatable, dimension(:,:)                              ::    OriginalTable
  class(IScalarValue_Type), allocatable                               ::    Multiplier
contains
  procedure, public                                                   ::    Reset
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetValue
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(ITableMultiplier_Type), intent(inout)                         ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  if (allocated(This%OriginalTable)) deallocate(This%OriginalTable, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%OriginalTable', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Multiplier)) deallocate(This%Multiplier, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Multiplier', ProcName=ProcName, stat=StatLoc)

  This%Constructed = .false.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(ITableMultiplier_Type), intent(inout)                         ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  real(rkp)                                                           ::    VarR0D
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  integer                                                             ::    i, ii
  logical                                                             ::    Found
  logical                                                             ::    VarL0D
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  integer                                                             ::    AbscissaColumn
  integer                                                             ::    ParamColumn
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  SectionName = 'original_values'
  call InputVerifier%AddSection(Section=SectionName)

  SubSectionName = SectionName // '>values'
  call InputVerifier%AddSection(Section='values', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call ImportArray(Input=InputSection, Array=VarR2D, Prefix=PrefixLoc)
  nullify(InputSection)

  allocate(This%OriginalTable(size(VarR2D,2),2), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%OriginalTable', ProcName=ProcName, stat=StatLoc)

  ParamColumn = 2
  ParameterName = 'parameter_column'
  call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
  call Input%GetValue(Value=VarI0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true.)
  ParamColumn = VarI0D

  AbscissaColumn = 1
  ParameterName = 'abscissa_column'
  call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
  call Input%GetValue(Value=VarI0D, ParameterName=Parametername, SectionName=SectionName, Mandatory=.true.)
  AbscissaColumn = VarI0D

  if (ParamColumn > size(VarR2D,1)) call Error%Raise(Line='Specified parameter column is greater than number of columns',    &
                                                                                                              ProcName=ProcName)

  if (AbscissaColumn > size(VarR2D,1)) call Error%Raise(Line='Specified abscissa column is greater than number of columns',  &
                                                                                                              ProcName=ProcName) 

  if (AbscissaColumn == ParamColumn) call Error%Raise(Line='Abscissa and parameter columns set to be the same',              &
                                                                                                              ProcName=ProcName)  

  i = 1
  do i = 1, size(VarR2D,2)
    This%OriginalTable(i,1) = VarR2D(AbscissaColumn,i)
    This%OriginalTable(i,2) = VarR2D(ParamColumn,i)
  end do

  deallocate(VarR2D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR2D', ProcName=ProcName, stat=StatLoc)

  SectionName = 'multiplier'
  call InputVerifier%AddSection(Section=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Multiplier, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(ITableMultiplier_Type), intent(in)                            ::    This
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
  integer                                                             ::    i
  character(:), allocatable                                           ::    FileName
  type(SMUQFile_Type)                                                 ::    File
  type(InputSection_Type), pointer                                    ::    InputSection=>null()

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

  SectionName = 'original_values'
  call GetInput%AddSection(SectionName=SectionName)
  call GetInput%AddParameter(Name='abscissa_column', Value='1', SectionName=SectionName)
  call GetInput%AddParameter(Name='parameter_column', Value='2', SectionName=SectionName)

  SubSectionName = 'values'
  call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
  SubSectionName = SectionName // '>values'
  call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  if (ExternalFlag) then
    FileName = DirectoryLoc // 'values.dat'
    call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
    call ExportArray(Input=InputSection, Array=transpose(This%OriginalTable), File=File)
  else
    call ExportArray(Input=InputSection, Array=transpose(This%OriginalTable))
  end if

  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%Multiplier, Name='multiplier',                 &
                                                                       Prefix=PrefixLoc, Directory=DirectoryLoc))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GetValue(This, Input, Abscissa, Values)

  class(ITableMultiplier_Type), intent(in)                            ::    This
  type(Input_Type), intent(in)                                        ::    Input
  real(rkp), dimension(:), intent(in)                                 ::    Abscissa
  real(rkp), dimension(:), intent(inout)                              ::    Values

  character(*), parameter                                             ::    ProcName='GetValue'
  integer                                                             ::    StatLoc=0
  real(rkp)                                                           ::    MultiplierLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (size(Values,1) /= size(Abscissa,1)) call Error%Raise('Incompatible values array', ProcName=ProcName)

  call Interpolate(Abscissa=This%OriginalTable(:,1), Ordinate=This%OriginalTable(:,2), Nodes=Abscissa, Values=Values)

  MultiplierLoc = This%Multiplier%GetValue(Input=Input)

  Values = Values * MultiplierLoc

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(ITableMultiplier_Type), intent(out)                           ::    LHS
  class(ITableValue_Type), intent(in)                                 ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (ITableMultiplier_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed
      if (RHS%Constructed) then
        allocate(LHS%OriginalTable, source=RHS%OriginalTable, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%OriginalTable', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Multiplier, source=RHS%Multiplier, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Multiplier', ProcName=ProcName, stat=StatLoc)
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(ITableMultiplier_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%OriginalTable)) deallocate(This%OriginalTable, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%OriginalTable', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Multiplier)) deallocate(This%Multiplier, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Multiplier', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
