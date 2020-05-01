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

module HierParamSpace_Class

use Parameters_Library
use Input_Library
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use Input_Class                                                   ,only:    Input_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use HierDistProb_Class                                            ,only:    HierDistProb_Type
use HierDistProb_Factory_Class                                    ,only:    HierDistProb_Factory
use HierDistProbContainer_Class                                   ,only:    HierDistProbContainer_Type
use ParamSpace_Class                                              ,only:    ParamSpace_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use DistProb_Class                                                ,only:    DistProb_Type
use DistProbContainer_Class                                       ,only:    DistProbContainer_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    HierParamSpace_Type

type                                                                  ::    HierParamSpace_Type
  character(:), allocatable                                           ::    Name
  integer                                                             ::    NbDim
  logical                                                             ::    Correlated=.false.
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  type(HierDistProbContainer_Type), allocatable, dimension(:)         ::    HierDistProb
  type(SMUQString_Type), allocatable, dimension(:)                    ::    ParamName
  type(SMUQString_Type), allocatable, dimension(:)                    ::    Label
  real(rkp), dimension(:,:), allocatable                              ::    CorrMat
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct                =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput             
  procedure, public                                                   ::    Generate
  generic, public                                                     ::    GetLabel                 =>   GetLabel0D,             &
                                                                                                          GetLabel1D
  generic, public                                                     ::    GetName                  =>   GetName0D,              &
                                                                                                          GetName1D
  procedure, private                                                  ::    GetName0D
  procedure, private                                                  ::    GetName1D
  procedure, private                                                  ::    GetLabel0D
  procedure, private                                                  ::    GetLabel1D
  generic, public                                                     ::    GetDistributionPointer  =>    GetDistPtr_LabChar,     &
                                                                                                          GetDistPtr_LabString,   &
                                                                                                          GetDistPtr_Num
  procedure, public                                                   ::    GetDistPtr_LabChar
  procedure, public                                                   ::    GetDistPtr_LabString
  procedure, public                                                   ::    GetDistPtr_Num
  procedure, public                                                   ::    IsCorrelated
  procedure, public                                                   ::    GetNbDim
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(HierParamSpace_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    if (.not. This%Initialized) then
      This%Initialized = .true.
      This%Name = 'HierParamSpace'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(HierParamSpace_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    if (allocated(This%HierDistProb)) deallocate(This%HierDistProb, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%HierDistProb', ProcName=ProcName, stat=StatLoc)
    This%NbDim = 0

    if (allocated(This%ParamName)) deallocate(This%ParamName, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamName', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Label)) deallocate(This%Label, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%CorrMat)) deallocate(This%CorrMat, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)
    This%Correlated = .false.

    This%Initialized=.false.
    This%Constructed=.false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(HierParamSpace_Type),intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------     

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructInput(This, Input, Prefix)

    class(HierParamSpace_Type), intent(inout)                         ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(HierDistProb_Type), allocatable                             ::    HierDistProb
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    ParameterName
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_1
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_2
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer                                                           ::    VarI0D
    character(:),  allocatable                                        ::    VarC0D
    logical                                                           ::    VarL0D
    integer                                                           ::    i, ii
    logical                                                           ::    Found
    character(:), allocatable                                         ::    SpaceType

    if (This%Constructed) call This%Reset
    if (.not. This%Initialized) call This%Initialize  

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    SectionName = 'parameters'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    This%NbDim = InputSection%GetNumberOfSubSections()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%ParamName', ProcName=ProcName, stat=StatLoc)

    allocate(This%HierDistProb(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%HierDistProb', ProcName=ProcName, stat=StatLoc)

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbDim
      SubSectionName = SectionName // '>parameter' // ConvertToString(Value=i)

      ParameterName = 'name'
      call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
      This%ParamName(i) = VarC0D

      ParameterName = 'label'
      call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
      This%Label(i) = VarC0D

      SubSectionName = SubSectionName // '>distribution'
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
      call HierDistProb_Factory%Construct(Object=HierDistProb, Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
      call This%HierDistProb(i)%Set(Object=HierDistProb)
      deallocate(HierDistProb, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='HierDistProb', ProcName=ProcName, stat=StatLoc)
    end do 

    i = 1
    do i = 1, This%NbDim-1
      ii = 1
      do ii = i+1 ,This%NbDim
        if (This%Label(i) == This%Label(ii)) call Error%Raise(Line='Duplicate labels : ' // This%Label(i), ProcName=ProcName)
      end do
    end do

    SectionName = 'correlation_matrix'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call ImportArray(Input=InputSection, Array=This%CorrMat, Prefix=PrefixLoc)
      nullify(InputSection)
      This%Correlated = .not. IsDiagonal(Array=This%CorrMat)
    else
      This%CorrMat = EyeR(N=This%NbDim)
      This%Correlated = .false.
    end if

    if (size(This%Corrmat,1) /= This%NbDim .or. size(This%CorrMat,2) /= This%NbDim) call Error%Raise(                        &
                                                       Line='Improper sizes for the input correlation matrix', ProcName=ProcName) 

    This%Constructed=.true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(HierParamSpace_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(HierDistProb_Type), pointer                                 ::    HierDistProb=>null()
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    i
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
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

    SectionName = 'parameters'
    call GetInput%AddSection(SectionName=SectionName)
    i = 1
    do i = 1, This%NbDim
      SubSectionName = 'parameter' // ConvertToString(Value=i)
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      SubSectionName= SectionName // '>' // SubSectionName
      call GetInput%AddParameter(Name='name', Value=This%ParamName(i)%Get(), SectionName=SubSectionName)
      call GetInput%AddParameter(Name='label', Value=This%Label(i)%Get(), SectionName=SubSectionName)
      HierDistProb => This%HierDistProb(i)%GetPointer()
      if (ExternalFlag) DirectorySub = DirectoryLoc // '/distribution' // ConvertToString(i)
      call GetInput%AddSection(Section=HierDistProb_Factory%GetObjectInput(Object=HierDistProb, Name='distribution', &
                                                        Prefix=PrefixLoc, Directory=DirectorySub), To_SubSection=SubSectionName)
      nullify(HierDistProb)
    end do

    SectionName='correlation_matrix'
    call GetInput%AddSection(SectionName=SectionName)
    call GetInput%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    if (ExternalFlag) then
      FileName = DirectoryLoc // '/correlation_matrix.dat'
      call File%Construct(File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ')
      call ExportArray(Input=InputSection, Array=This%CorrMat, File=File)
    else
      call ExportArray(Input=InputSection, Array=This%CorrMat)
    end if
    nullify(InputSection)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate(This, Input, ParamSpace)
  
    class(HierParamSpace_Type), intent(in)                            ::    This
    type(ParamSpace_Type), intent(out)                                ::    ParamSpace
    type(Input_Type), intent(in)                                      ::    Input

    character(*), parameter                                           ::    ProcName='Generate'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    class(DistProb_Type), allocatable                                 ::    DistProb
    class(HierDistProb_Type), pointer                                 ::    HierDistProbPtr=>null()
    type(DistProbContainer_Type), allocatable, dimension(:)           ::    DistProbVec

    allocate(DistProbVec(This%NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='DistProbVec', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, This%NbDim
      HierDistProbPtr => This%HierDistProb(i)%GetPointer()
      call HierDistProbPtr%Generate(Input=Input, Distribution=DistProb)
      call DistProbVec(i)%Set(Object=DistProb)
      deallocate(DistProb, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='DistProb', ProcName=ProcName, stat=StatLoc)
      nullify(HierDistProbPtr)
    end do

    call ParamSpace%Construct(Distributions=DistProbVec, CorrMat=This%CorrMat, Labels=This%Label, Names=This%ParamName)

    deallocate(DistProbVec, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='DistProbVec', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName0D(This, Num)

    character(:), allocatable                                         ::    GetName0D
    class(HierParamSpace_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetName0D'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetName0D = This%ParamName(Num)%Get()      

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName1D(This)

    type(SMUQString_Type), allocatable, dimension(:)                  ::    GetName1D
    class(HierParamSpace_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetName1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    allocate(GetName1D, source=This%ParamName, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='GetName1D', ProcName=ProcName, stat=StatLoc)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel0D(This, Num)

    character(:), allocatable                                         ::    GetLabel0D
    class(HierParamSpace_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetLabel0D'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetLabel0D = This%Label(Num)%Get()      

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel1D(This)

    type(SMUQString_Type), allocatable, dimension(:)                  ::    GetLabel1D
    class(HierParamSpace_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetLabel1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    allocate(GetLabel1D, source=This%Label, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='GetLabel1D', ProcName=ProcName, stat=StatLoc)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDim(This)

    integer                                                           ::    GetNbDim
    class(HierParamSpace_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetNbDim'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetNbDim = This%NbDim

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

 !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistPtr_LabChar(This, Label)

    class(HierDistProb_Type), pointer                                 ::    GetDistPtr_LabChar

    class(HierParamSpace_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetDistPtr_LabChar'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    i = 1
    ii = 0
    do i = 1, This%NbDim
      if (This%Label(i) /= Label) cycle
      ii = i
      exit
    end do

    if (ii == 0) call Error%Raise('Did not find required parameter with label : ' // Label, ProcName=ProcName)

    GetDistPtr_LabChar => This%HierDistProb(ii)%GetPointer()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

 !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistPtr_LabString(This, Label)

    class(HierDistProb_Type), pointer                                 ::    GetDistPtr_LabString

    class(HierParamSpace_Type), intent(in)                            ::    This
    type(SMUQString_Type), intent(in)                                 ::    Label

    character(*), parameter                                           ::    ProcName='GetDistPtr_LabString'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    i = 1
    ii = 0
    do i = 1, This%NbDim
      if (This%Label(i) /= Label) cycle
      ii = i
      exit
    end do

    if (ii == 0) call Error%Raise('Did not find required parameter with label : ' // Label, ProcName=ProcName)

    GetDistPtr_LabString => This%HierDistProb(ii)%GetPointer()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistPtr_Num(This, Num)

    class(HierDistProb_Type), pointer                                 ::    GetDistPtr_Num

    class(HierParamSpace_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetDistPtr_Num'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (Num > This%NbDim) call Error%Raise(Line='Num specifier above maximum number of distributions', ProcName=ProcName)
    if (Num < 1) call Error%Raise(Line='Num specifier below minimum of 1', ProcName=ProcName)

    GetDistPtr_Num => This%HierDistProb(Num)%GetPointer()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsCorrelated(This)

    logical                                                           ::    IsCorrelated
    class(HierParamSpace_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='IsCorrelated'
    integer                                                           ::    StatLoc=0

    IsCorrelated = This%Correlated

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(HierParamSpace_Type), intent(out)                           ::    LHS
    class(HierParamSpace_Type), intent(in)                            ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)

      type is (HierParamSpace_Type)
        call LHS%Reset()

        LHS%Name = RHS%Name
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        
        if (RHS%Constructed) then
          LHS%NbDim = RHS%NbDim
          LHS%Correlated = RHS%Correlated
          allocate(LHS%HierDistProb, source=RHS%HierDistProb, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%HierDistProb', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%ParamName, source=RHS%ParamName, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%Paramname', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%CorrMat, source=RHS%CorrMat, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%Label, source=RHS%Label, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%Label', ProcName=ProcName, stat=StatLoc)
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(HierParamSpace_Type),intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%CorrMat)) deallocate(This%CorrMat, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%CorrMat', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%ParamName)) deallocate(This%ParamName, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%ParamName', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Label)) deallocate(This%Label, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Label', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%HierDistProb)) deallocate(This%HierDistProb, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%HierDistProb', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
