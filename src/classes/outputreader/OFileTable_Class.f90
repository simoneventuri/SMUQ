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

module OFileTable_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use ComputingRoutines_Module
use COmmandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use OFileFormated_Class                                           ,only:    OFileFormated_Type
use Output_Class                                                  ,only:    Output_Type

implicit none

private

public                                                                ::    OFileTable_Type

type, extends(OFileFormated_Type)                                     ::    OFileTable_Type
  integer, allocatable, dimension(:)                                  ::    OutputColumn
  integer                                                             ::    AbscissaColumn
  real(rkp), allocatable, dimension(:)                                ::    InterpolationNodes
  logical                                                             ::    Interpolated
  integer                                                             ::    NbColumns
  logical                                                             ::    DebugFlag
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    ReadOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(OFileTable_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    if (.not. This%Initialized) then
      This%Name = 'ofiletable'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(OFileTable_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0
    call This%OutputFile%Reset()

    if (allocated(This%InterpolationNodes)) deallocate(This%InterpolationNodes, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%InterpolationNodes', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%OutputColumn)) deallocate(This%OutputColumn, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%OutputColumn', ProcName=ProcName, stat=StatLoc)
    This%NbColumns = 0

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(OFileTable_Type), intent(inout)                             ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%AbscissaColumn = 0
    This%Interpolated = .false.
    This%DebugFlag = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(OFileTable_Type), intent(inout)                             ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    Found
    logical                                                           ::    VarL0D
    integer                                                           ::    i
    character(:), allocatable                                         ::    InterpNodesSource
    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'debug'
    call Input%GetValue(Value=VarL0D, ParameterName=Parametername, Mandatory=.false., Found=Found)
    if (Found) This%DebugFlag = VarL0D

    ParameterName = 'abscissa_column'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    This%AbscissaColumn = VarI0D

    ParameterName = 'output_column'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
    This%OutputColumn = ConvertToIntegers(VarC0D)

    This%NbColumns = size(This%OutputColumn,1)

    SectionName = 'file'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call This%OutputFile%Construct(Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)

    SectionName = 'interpolation_nodes'

    if (Input%HasSection(SubSectionName = SectionName)) then
      This%Interpolated = .true.
      ParameterName = 'source' 
      call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
      InterpNodesSource = VarC0D
      SubSectionName = SectionName // '>source'
      select case (InterpNodesSource)
        case ('values')
          call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
          call ImportArray(Input=InputSection, Array=This%InterpolationNodes, Prefix=PrefixLoc)
        case ('computed')
          call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
          This%InterpolationNodes = LinSpaceVec(Input=InputSection)
        case default
          call Error%Raise(Line='Interpolation nodes source not recognized', ProcName=ProcName)
      end select
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(OFileTable_Type), intent(in)                                ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    character(:), allocatable                                         ::    FileName
    type(SMUQFile_Type)                                               ::    File
    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/file'

    call GetInput%AddSection(Section=This%OutputFile%GetInput(Name='file', Prefix=PrefixLoc, Directory=DirectorySub))

    call GetInput%AddParameter(Name='debug', Value=Convert_To_String(This%DebugFlag))
    call GetInput%AddParameter(Name='abscissa_column', Value=Convert_To_String(This%AbscissaColumn))
    call GetInput%AddParameter(Name='output_column', Value=ConvertToString(Values=This%OutputColumn))

    if (This%Interpolated) then
      SectionName = 'interpolation_nodes'
      call GetInput%AddParameter(Name='source', Value='values', SectionName=SectionName)

      call GetInput%AddSection(SectionName=SectionName)
      SubSectionName = 'source'
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName // '>' // SubSectionName,           &
                                                                                                                Mandatory=.true.)
      if (ExternalFlag) then
        FileName = DirectoryLoc // '/interpolation_nodes.dat'
        call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
        call ExportArray(Input=InputSection, Array=This%InterpolationNodes, File=File)
      else
        call ExportArray(Input=InputSection, Array=This%InterpolationNodes)
      end if
      nullify(InputSection)
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReadOutput(This, Values)

    class(OFileTable_Type), intent(in)                                ::    This
    real(rkp), allocatable, dimension(:,:), intent(out)               ::    Values

    character(*), parameter                                           ::    ProcName='ReadOutput'
    integer                                                           ::    StatLoc=0
    type(String_Type), allocatable, dimension(:,:)                    ::    Strings
    real(rkp), allocatable, dimension(:)                              ::    TableOutput
    real(rkp), allocatable, dimension(:)                              ::    Abscissa
    real(rkp), allocatable, dimension(:)                              ::    InterpolatedOutput
    integer                                                           ::    NbLines
    integer                                                           ::    NbEntries
    integer                                                           ::    i
    integer                                                           ::    ii
    type(SMUQFile_Type)                                               ::    FileLoc

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (This%DebugFlag) then
      write(*,*) '*****************************************************************************'
      write(*,*) 'Debug information for reading of output file  ' // This%OutputFile%GetFullFile()
      write(*,*) '*****************************************************************************'
    end if

    FileLoc = This%OutputFile

    call ImportArray(Array=Strings, File=FileLoc, RowMajor=.true.)

    NbLines = size(Strings,1)

    if (This%Interpolated) then
      NbEntries = size(This%InterpolationNodes,1)
      allocate(Values(NbEntries*This%NbColumns,1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    else
      NbEntries = NbLines
      allocate(Values(NbLines*This%NbColumns,1), stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
      Values = Zero
    end if

    allocate(TableOutput(NbLines), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='TableOutput', ProcName=ProcName, stat=StatLoc)
    allocate(Abscissa(NbLines), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Abscissa', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, NbLines
      Abscissa(i) = ConvertToReal(String=Strings(i,This%AbscissaColumn)%GetValue())
    end do

    if (This%DebugFlag) then
      write(*,*)
      write(*,*) 'Interpolation Nodes'
      write(*,*)
      write(*,*) This%InterpolationNodes
    end if

    if (This%DebugFlag) then
      write(*,*)
      write(*,*) 'Read in Abscissa'
      write(*,*)
      write(*,*) Abscissa
    end if

    i = 1
    do i = 1, This%NbColumns

      ii = 1
      do ii = 1, NbLines
        TableOutput(ii) = ConvertToReal(String=Strings(ii,This%OutputColumn(i))%GetValue())
      end do

      if (This%Interpolated) then
        Values((i-1)*NbEntries+1:i*NbEntries,1) = Interpolate(Abscissa=Abscissa, Ordinate=TableOutput,                           &
                                                                                                    Nodes=This%InterpolationNodes)
        if (This%DebugFlag) then
          write(*,*)
          write(*,*) 'Interpolated values from column ' // ConvertToString(Value=This%OutputColumn(i))
          write(*,*)
          write(*,*) Values((i-1)*NbEntries+1:i*NbEntries,1)
        end if
      else
        Values((i-1)*NbEntries+1:i*NbEntries,1) = TableOutput
        if (This%DebugFlag) then
          write(*,*)
          write(*,*) 'Read in values from column ' // ConvertToString(Value=This%OutputColumn(i))
          write(*,*)
          write(*,*) Values((i-1)*NbEntries+1:i*NbEntries,1)
        end if
      end if
  
    end do

    deallocate(Strings, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Strings', ProcName=ProcName, stat=StatLoc)

    deallocate(Abscissa, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Abscissa', ProcName=ProcName, stat=StatLoc)

    deallocate(TableOutput, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='TableOutput', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(OFileTable_Type), intent(out)                               ::    LHS
    class(OFileFormated_Type), intent(in)                             ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (OFileTable_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%DebugFlag = RHS%DebugFlag
          LHS%OutputFile = RHS%OutputFile
          LHS%AbscissaColumn = RHS%AbscissaColumn
          LHS%Interpolated = RHS%Interpolated
          LHS%NbColumns = RHS%NbColumns
          allocate(LHS%OutputColumn, source=RHS%OutputColumn, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%OutputColumn', ProcName=ProcName, stat=StatLoc)
          if (LHS%Interpolated) then
            allocate(LHS%InterpolationNodes, source=RHS%InterpolationNodes, stat=StatLoc)
            if (StatLoc /= 0) call Error%Allocate(Name='LHS%InterpolationNodes', ProcName=ProcName, stat=StatLoc)
          end if
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(OFileTable_Type),intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%OutputColumn)) deallocate(This%OutputColumn, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%OutputColumn', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%InterpolationNodes)) deallocate(This%InterpolationNodes, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%InterpolationNodes', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
