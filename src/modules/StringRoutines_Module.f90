module StringRoutines_Module

use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    ConvertToString
public                                                                ::    ConvertToStrings
public                                                                ::    ConvertToInteger
public                                                                ::    ConvertToIntegers
public                                                                ::    ConvertToInteger4
public                                                                ::    ConvertToInteger8
public                                                                ::    ConvertToReal
public                                                                ::    ConvertToReals
public                                                                ::    ConvertToReal4
public                                                                ::    ConvertToReal8
public                                                                ::    ConvertToLogical
public                                                                ::    ConvertToLogicals
public                                                                ::    ConvertToComplex
public                                                                ::    ConvertToComplexs

logical, parameter                                                    ::    DebugGlobal = .false.


interface ConvertToString
  module procedure                                                    ::    Convert_R40D_To_C0D
  module procedure                                                    ::    Convert_R80D_To_C0D
  module procedure                                                    ::    Convert_I40D_To_C0D
  module procedure                                                    ::    Convert_I80D_To_C0D
  module procedure                                                    ::    Convert_C0D_To_C0D
  module procedure                                                    ::    Convert_L0D_To_C0D
  module procedure                                                    ::    Convert_CX0D_To_C0D
  module procedure                                                    ::    Convert_String0D_To_C0D
  module procedure                                                    ::    Convert_R41D_To_C0D
  module procedure                                                    ::    Convert_R81D_To_C0D
  module procedure                                                    ::    Convert_I41D_To_C0D
  module procedure                                                    ::    Convert_I81D_To_C0D
  module procedure                                                    ::    Convert_C1D_To_C0D
  module procedure                                                    ::    Convert_L1D_To_C0D
  module procedure                                                    ::    Convert_CX1D_To_C0D
  module procedure                                                    ::    Convert_String1D_To_C0D
end interface

interface ConvertToStrings
  module procedure                                                    ::    Convert_C0D_To_String1D
  module procedure                                                    ::    Convert_C1D_To_String1D
  module procedure                                                    ::    Convert_R41D_To_String1D
  module procedure                                                    ::    Convert_R81D_To_String1D
end interface

interface ConvertToInteger                                            
  module procedure                                                    ::    Convert_C0D_To_I0D
  module procedure                                                    ::    Convert_String0D_To_I0D
end interface

interface ConvertToIntegers                                            
  module procedure                                                    ::    Convert_C0D_To_I41D
  module procedure                                                    ::    Convert_C1D_To_I41D
  module procedure                                                    ::    Convert_String0D_To_I41D
  module procedure                                                    ::    Convert_String1D_To_I41D
  module procedure                                                    ::    Convert_C0D_To_I81D
  module procedure                                                    ::    Convert_C1D_To_I81D
  module procedure                                                    ::    Convert_String0D_To_I81D
  module procedure                                                    ::    Convert_String1D_To_I81D
end interface

interface ConvertToInteger4                                            
  module procedure                                                    ::    Convert_C0D_To_I40D
  module procedure                                                    ::    Convert_String0D_To_I40D
end interface

interface ConvertToInteger8                                            
  module procedure                                                    ::    Convert_C0D_To_I80D
  module procedure                                                    ::    Convert_String0D_To_I80D
end interface

interface ConvertToReal                                            
  module procedure                                                    ::    Convert_C0D_To_R0D
  module procedure                                                    ::    Convert_String0D_To_R0D
end interface

interface ConvertToReals
  module procedure                                                    ::    Convert_C0D_To_R41D
  module procedure                                                    ::    Convert_C1D_To_R41D
  module procedure                                                    ::    Convert_String0D_To_R41D
  module procedure                                                    ::    Convert_String1D_To_R41D
  module procedure                                                    ::    Convert_C0D_To_R81D
  module procedure                                                    ::    Convert_C1D_To_R81D
  module procedure                                                    ::    Convert_String0D_To_R81D
  module procedure                                                    ::    Convert_String1D_To_R81D
end interface

interface ConvertToReal4                                            
  module procedure                                                    ::    Convert_C0D_To_R40D
  module procedure                                                    ::    Convert_String0D_To_R40D
end interface

interface ConvertToReal8                                           
  module procedure                                                    ::    Convert_C0D_To_R80D
  module procedure                                                    ::    Convert_String0D_To_R80D
end interface

interface ConvertToLogical                                          
  module procedure                                                    ::    Convert_C0D_To_L0D
  module procedure                                                    ::    Convert_String0D_To_L0D
end interface

interface ConvertToLogicals                                          
  module procedure                                                    ::    Convert_C0D_To_L1D
  module procedure                                                    ::    Convert_C1D_To_L1D
  module procedure                                                    ::    Convert_String0D_To_L1D
  module procedure                                                    ::    Convert_String1D_To_L1D
end interface

interface ConvertToComplex                                          
  module procedure                                                    ::    Convert_C0D_To_CX0D
  module procedure                                                    ::    Convert_String0D_To_CX0D
end interface

interface ConvertToComplexs                                          
  module procedure                                                    ::    Convert_C0D_To_CX1D
  module procedure                                                    ::    Convert_C1D_To_CX1D
  module procedure                                                    ::    Convert_String0D_To_CX1D
  module procedure                                                    ::    Convert_String1D_To_CX1D
end interface

integer, parameter                                                    ::    BufferSize=100

contains

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C0D_To_I40D(String)

  integer(4)                                                          ::    Convert_C0D_To_I40D

  character(*), intent(in)                                            ::    String


  character(*), parameter                                             ::    ProcName='Convert_C0D_To_I40D'
  integer                                                             ::    StatLoc=0

  read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_I40D
  if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_I40D(String)

  integer(4)                                                          ::    Convert_String0D_To_I40D

  class(SMUQString_Type), intent(in)                                  ::    String

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_I40D'
  integer                                                             ::    StatLoc=0

  Convert_String0D_To_I40D = ConvertToInteger4(String=String%Get())

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C0D_To_I41D(String, Values, Separator)

  character(*), intent(in)                                            ::    String
  integer(4), allocatable, dimension(:), intent(inout)                ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_I41D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i
  integer                                                             ::    NbEntries

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  call VarString0D%Split(Separator=SeparatorLoc, Strings=VarString1D)

  NbEntries = size(VarString1D)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  i = 1
  do i = 1, NbEntries
    Values(i) = ConvertToInteger4(String=VarString1D(i)%Get())
  end do

  deallocate(VarString1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String0D_To_I41D(String, Values, Separator)

  class(SMUQString_Type), intent(in)                                  ::    String
  integer(4), allocatable, dimension(:), intent(inout)                ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_I41D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  call ConvertToIntegers(String=String%Get(), Values=Values, Separator=SeparatorLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C1D_To_I41D(Strings, Values)

  character(*), dimension(:), intent(in)                              ::    Strings
  integer(4), allocatable, dimension(:), intent(inout)                ::    Values

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_I41D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Values(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String1D_To_I41D(Strings, Values)

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings
  integer(4), allocatable, dimension(:), intent(inout)                ::    Values

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_I41D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    Values(i) = ConvertToInteger4(String=Strings(i)%Get())
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C0D_To_I80D(String)

  integer(8)                                                          ::    Convert_C0D_To_I80D

  character(*), intent(in)                                            ::    String

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_I80D'
  integer                                                             ::    StatLoc=0

  read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_I80D
  if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_I80D(String)

  integer(8)                                                          ::    Convert_String0D_To_I80D

  class(SMUQString_Type), intent(in)                                  ::    String

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_I80D'
  integer                                                             ::    StatLoc=0

  Convert_String0D_To_I80D = ConvertToInteger8(String=String%Get())

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C0D_To_I81D(String, Values, Separator)

  character(*), intent(in)                                            ::    String
  integer(8), allocatable, dimension(:), intent(inout)                ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_I81D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i
  integer                                                             ::    NbEntries

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  call VarString0D%Split(Separator=SeparatorLoc, Strings=VarString1D)

  NbEntries = size(VarString1D)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  i = 1
  do i = 1, NbEntries
    Values(i) = ConvertToInteger8(String=VarString1D(i)%Get())
  end do

  deallocate(VarString1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String0D_To_I81D(String, Values, Separator)

  class(SMUQString_Type), intent(in)                                  ::    String
  integer(8), allocatable, dimension(:), intent(inout)                ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_I81D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  call ConvertToIntegers(String=String%Get(), Values=Values, Separator=SeparatorLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C1D_To_I81D(Strings, Values)

  character(*), dimension(:), intent(in)                              ::    Strings
  integer(8), allocatable, dimension(:), intent(inout)                ::    Values

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_I81D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Values(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String1D_To_I81D(Strings, Values)

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings
  integer(8), allocatable, dimension(:), intent(inout)                ::    Values

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_I81D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    Values(i) = ConvertToInteger8(String=Strings(i)%Get())
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C0D_To_I0D(String)

  integer                                                             ::    Convert_C0D_To_I0D

  character(*), intent(in)                                            ::    String

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_I0D'
  integer                                                             ::    StatLoc=0

  read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_I0D
  if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_I0D(String)

  integer                                                             ::    Convert_String0D_To_I0D

  class(SMUQString_Type), intent(in)                                  ::    String

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_I0D'
  integer                                                             ::    StatLoc=0

  Convert_String0D_To_I0D = ConvertToInteger(String=String%Get())

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C0D_To_R0D(String)

  real(rkp)                                                           ::    Convert_C0D_To_R0D

  character(*), intent(in)                                            ::    String

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_R0D'
  integer                                                             ::    StatLoc=0

  read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_R0D
  if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_R0D(String)

  real(rkp)                                                           ::    Convert_String0D_To_R0D

  class(SMUQString_Type), intent(in)                                  ::    String

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_R0D'
  integer                                                             ::    StatLoc=0

  Convert_String0D_To_R0D = ConvertToReal(String=String%Get())

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C0D_To_R40D(String)

  real(4)                                                             ::    Convert_C0D_To_R40D

  character(*), intent(in)                                            ::    String

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_R40D'
  integer                                                             ::    StatLoc=0

  read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_R40D
  if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_R40D(String)

  real(4)                                                             ::    Convert_String0D_To_R40D

  class(SMUQString_Type), intent(in)                                  ::    String

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_R40D'
  integer                                                             ::    StatLoc=0

  Convert_String0D_To_R40D = ConvertToReal4(String=String%Get())

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C0D_To_R41D(String, Values, Separator)

  character(*), intent(in)                                            ::    String
  real(4), allocatable, dimension(:), intent(inout)                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_R41D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i
  integer                                                             ::    NbEntries

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  call VarString0D%Split(Separator=SeparatorLoc, Strings=VarString1D)

  NbEntries = size(VarString1D)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  i = 1
  do i = 1, NbEntries
    Values(i) = ConvertToReal4(String=VarString1D(i)%Get())
  end do

  deallocate(VarString1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String0D_To_R41D(String, Values, Separator)

  class(SMUQString_Type), intent(in)                                  ::    String
  real(4), allocatable, dimension(:), intent(inout)                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_R41D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  call ConvertToReals(String=String%Get(), Values=Values, Separator=SeparatorLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C1D_To_R41D(Strings, Values)

  character(*), dimension(:), intent(in)                              ::    Strings
  integer(4), allocatable, dimension(:), intent(inout)                ::    Values

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_R41D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Values(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String1D_To_R41D(Strings, Values)

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings
  real(4), allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_R41D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    Values(i) = ConvertToReal4(String=Strings(i)%Get())
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C0D_To_R80D(String)

  real(8)                                                             ::    Convert_C0D_To_R80D

  character(*), intent(in)                                            ::    String

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_R80D'
  integer                                                             ::    StatLoc=0

  read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_R80D
  if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_R80D(String)

  real(8)                                                             ::    Convert_String0D_To_R80D

  class(SMUQString_Type), intent(in)                                  ::    String

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_R80D'
  integer                                                             ::    StatLoc=0

  Convert_String0D_To_R80D = ConvertToReal8(String=String%Get())

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C0D_To_R81D(String, Values, Separator)

  character(*), intent(in)                                            ::    String
  real(8), allocatable, dimension(:), intent(inout)                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_R81D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i
  integer                                                             ::    NbEntries

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  call VarString0D%Split(Separator=SeparatorLoc, Strings=VarString1D)

  NbEntries = size(VarString1D)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  i = 1
  do i = 1, NbEntries
    Values(i) = ConvertToReal8(String=VarString1D(i)%Get())
  end do

  deallocate(VarString1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String0D_To_R81D(String, Values, Separator)

  class(SMUQString_Type), intent(in)                                  ::    String
  real(8), allocatable, dimension(:), intent(inout)                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_R81D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  call ConvertToReals(String=String%Get(), Values=Values, Separator=SeparatorLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C1D_To_R81D(Strings, Values)

  character(*), dimension(:), intent(in)                              ::    Strings
  real(8), allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_R81D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Values(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String1D_To_R81D(Strings, Values)

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings
  real(8), allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_R81D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    Values(i) = ConvertToReal8(String=Strings(i)%Get())
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C0D_To_L0D(String)

  logical                                                             ::    Convert_C0D_To_L0D

  character(*), intent(in)                                            ::    String

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_L0D'
  integer                                                             ::    StatLoc=0

  read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_L0D
  if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_L0D(String)

  logical                                                             ::    Convert_String0D_To_L0D

  class(SMUQString_Type), intent(in)                                  ::    String

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_L0D'
  integer                                                             ::    StatLoc=0

  Convert_String0D_To_L0D = ConvertToLogical(String=String%Get())

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C0D_To_L1D(String, Values, Separator)

  character(*), intent(in)                                            ::    String
  logical, allocatable, dimension(:), intent(inout)                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_L1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  call VarString0D%Split(Separator=SeparatorLoc, Strings=VarString1D)

  NbEntries = size(VarString1D,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  i = 1
  do i = 1, NbEntries
    Values(i) = ConvertToLogical(String=VarString1D(i)%Get())
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String0D_To_L1D(String, Values, Separator)

  class(SMUQString_Type), intent(in)                                  ::    String
  logical, allocatable, dimension(:), intent(inout)                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_L1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  call ConvertToLogicals(String=String%Get(), Values=Values, Separator=SeparatorLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C1D_To_L1D(Strings, Values)

  character(*), dimension(:), intent(in)                              ::    Strings
  logical, allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_L1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Values(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String1D_To_L1D(Strings, Values)

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings
  logical, allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_L1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    Values(i) = ConvertToLogical(String=Strings(i)%Get())
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C0D_To_CX0D(String)

  complex                                                             ::    Convert_C0D_To_CX0D

  character(*), intent(in)                                            ::    String

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_CX0D'
  integer                                                             ::    StatLoc=0

  read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_CX0D
  if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_CX0D(String)

  complex                                                             ::    Convert_String0D_To_CX0D

  class(SMUQString_Type), intent(in)                                  ::    String

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_CX0D'
  integer                                                             ::    StatLoc=0

  Convert_String0D_To_CX0D = ConvertToComplex(String=String%Get())

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C0D_To_CX1D(String, Values, Separator)

  character(*), intent(in)                                            ::    String
  complex, allocatable, dimension(:), intent(inout)                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_CX1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  call VarString0D%Split(Separator=SeparatorLoc, Strings=VarString1D)

  NbEntries = size(VarString1D,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  i = 1
  do i = 1, NbEntries
    Values(i) = ConvertToComplex(String=VarString1D(i)%Get())
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String0D_To_CX1D(String, Values, Separator)

  class(SMUQString_Type), intent(in)                                  ::    String
  complex, allocatable, dimension(:), intent(inout)                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_CX1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  call ConvertToComplexs(String=String%Get(), Values=Values, Separator=SeparatorLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C1D_To_CX1D(Strings, Values)

  character(*), dimension(:), intent(in)                              ::    Strings
  complex, allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_CX1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Values(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_String1D_To_CX1D(Strings, Values)

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings
  complex, allocatable, dimension(:), intent(inout)                   ::    Values

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_CX1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Strings,1)

  if(allocated(Values)) then
    if (size(Values,1) /= NbEntries) then
      deallocate(Values, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Values)) then
    allocate(Values(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  end if

  do i = 1, NbEntries
    Values(i) = ConvertToComplex(String=Strings(i)%Get())
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_R40D_To_C0D(Value, Format)

  character(:), allocatable                                           ::    Convert_R40D_To_C0D

  real(4), intent(in)                                                 ::    Value
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_R40D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    FormatLoc
  character(BufferSize)                                               ::    VarC0D

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  FormatLoc = '(' // FormatLoc // ')'

  write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
  if (StatLoc /= 0) call Error%Write(Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
  Convert_R40D_To_C0D = trim(adjustl(VarC0D))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_R80D_To_C0D(Value, Format)

  character(:), allocatable                                           ::    Convert_R80D_To_C0D

  real(8), intent(in)                                                 ::    Value
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_R80D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    FormatLoc
  character(BufferSize)                                               ::    VarC0D

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  FormatLoc = '(' // FormatLoc // ')'

  write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
  if (StatLoc /= 0) call Error%Write(Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
  Convert_R80D_To_C0D = trim(adjustl(VarC0D))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_I40D_To_C0D(Value, Format)

  character(:), allocatable                                           ::    Convert_I40D_To_C0D

  integer(4), intent(in)                                              ::    Value
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_I40D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    FormatLoc
  character(BufferSize)                                               ::    VarC0D

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  FormatLoc = '(' // FormatLoc // ')'

  write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
  if (StatLoc /= 0) call Error%Write(Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
  Convert_I40D_To_C0D = trim(adjustl(VarC0D))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_I80D_To_C0D(Value, Format)

  character(:), allocatable                                           ::    Convert_I80D_To_C0D

  integer(8), intent(in)                                              ::    Value
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_I80D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    FormatLoc
  character(BufferSize)                                               ::    VarC0D

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  FormatLoc = '(' // FormatLoc // ')'

  write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
  if (StatLoc /= 0) call Error%Write(Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
  Convert_I80D_To_C0D = trim(adjustl(VarC0D))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C0D_To_C0D(Value, Format)

  character(:), allocatable                                           ::    Convert_C0D_To_C0D

  character(*), intent(in)                                            ::    Value
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    FormatLoc
  character(BufferSize)                                               ::    VarC0D

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  FormatLoc = '(' // FormatLoc // ')'

  write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
  if (StatLoc /= 0) call Error%Write(Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
  Convert_C0D_To_C0D = trim(adjustl(VarC0D))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_L0D_To_C0D(Value, Format)

  character(:), allocatable                                           ::    Convert_L0D_To_C0D

  logical, intent(in)                                                 ::    Value
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_L0D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    FormatLoc
  character(BufferSize)                                               ::    VarC0D

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  FormatLoc = '(' // FormatLoc // ')'

  write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
  if (StatLoc /= 0) call Error%Write(Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
  Convert_L0D_To_C0D = trim(adjustl(VarC0D))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_CX0D_To_C0D(Value, Format)

  character(:), allocatable                                           ::    Convert_CX0D_To_C0D

  complex, intent(in)                                                 ::    Value
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_CX0D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    FormatLoc
  character(BufferSize)                                               ::    VarC0D

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  FormatLoc = '(' // FormatLoc // ')'

  write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
  if (StatLoc /= 0) call Error%Write(Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
  Convert_CX0D_To_C0D = trim(adjustl(VarC0D))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_C0D(Value, Format)

  character(:), allocatable                                           ::    Convert_String0D_To_C0D

  type(SMUQString_Type), intent(in)                                   ::    Value
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    FormatLoc
  character(BufferSize)                                               ::    VarC0D

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  FormatLoc = '(' // FormatLoc // ')'
  write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value%Get()

  Convert_String0D_To_C0D = trim(adjustl(VarC0D))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_R41D_To_C0D(Values, Format, Separator)

  character(:), allocatable                                           ::    Convert_R41D_To_C0D

  real(4), dimension(:), intent(in)                                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_R41D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  Convert_R41D_To_C0D = ConvertTOString(Value=Values(1), Format=FormatLoc)
  i = 2
  do i = 2, size(Values,1)
    Convert_R41D_To_C0D = Convert_R41D_To_C0D // SeparatorLoc // ConvertTOString(Value=Values(i), Format=FormatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_R81D_To_C0D(Values, Format, Separator)

  character(:), allocatable                                           ::    Convert_R81D_To_C0D

  real(8), dimension(:), intent(in)                                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_R81D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  Convert_R81D_To_C0D = ConvertTOString(Value=Values(1), Format=FormatLoc)
  i = 2
  do i = 2, size(Values,1)
    Convert_R81D_To_C0D = Convert_R81D_To_C0D // SeparatorLoc // ConvertTOString(Value=Values(i), Format=FormatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_I41D_To_C0D(Values, Format, Separator)

  character(:), allocatable                                           ::    Convert_I41D_To_C0D

  integer(4), dimension(:), intent(in)                                ::    Values
  character(*), optional, intent(in)                                  ::    Separator
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_I41D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  Convert_I41D_To_C0D = ConvertTOString(Value=Values(1), Format=FormatLoc)
  i = 2
  do i = 2, size(Values,1)
    Convert_I41D_To_C0D = Convert_I41D_To_C0D // SeparatorLoc // ConvertTOString(Value=Values(i), Format=FormatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_I81D_To_C0D(Values, Format, Separator)

  character(:), allocatable                                           ::    Convert_I81D_To_C0D

  integer(8), dimension(:), intent(in)                                ::    Values
  character(*), optional, intent(in)                                  ::    Separator
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_I81D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format
 
  Convert_I81D_To_C0D = ConvertTOString(Value=Values(1), Format=FormatLoc)
  i = 2
  do i = 2, size(Values,1)
    Convert_I81D_To_C0D = Convert_I81D_To_C0D // SeparatorLoc // ConvertToString(Value=Values(i), Format=FormatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_C0D(Values, Format, Separator)

  character(:), allocatable                                           ::    Convert_C1D_To_C0D

  character(*), dimension(:), intent(in)                              ::    Values
  character(*), optional, intent(in)                                  ::    Separator
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  Convert_C1D_To_C0D = ConvertTOString(Value=Values(1), Format=FormatLoc)
  i = 2
  do i = 2, size(Values,1)
    Convert_C1D_To_C0D = Convert_C1D_To_C0D // SeparatorLoc // ConvertToString(Value=Values(i), Format=FormatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_L1D_To_C0D(Values, Format, Separator)

  character(:), allocatable                                           ::    Convert_L1D_To_C0D

  logical, dimension(:), intent(in)                                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_L1D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  Convert_L1D_To_C0D = ConvertTOString(Value=Values(1), Format=FormatLoc)
  i = 2
  do i = 2, size(Values,1)
    Convert_L1D_To_C0D = Convert_L1D_To_C0D // SeparatorLoc // ConvertToString(Value=Values(i), Format=FormatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_CX1D_To_C0D(Values, Format, Separator)

  character(:), allocatable                                           ::    Convert_CX1D_To_C0D

  complex, dimension(:), intent(in)                                   ::    Values
  character(*), optional, intent(in)                                  ::    Separator
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_CX1D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  Convert_CX1D_To_C0D = ConvertTOString(Value=Values(1), Format=FormatLoc)
  i = 2
  do i = 2, size(Values,1)
    Convert_CX1D_To_C0D = Convert_CX1D_To_C0D // SeparatorLoc // ConvertToString(Value=Values(i), Format=FormatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String1D_To_C0D(Values, Format, Separator)

  character(:), allocatable                                           ::    Convert_String1D_To_C0D

  type(SMUQString_Type), dimension(:), intent(in)                     ::    Values
  character(*), optional, intent(in)                                  ::    Separator
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_C0D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i
  integer                                                             ::    Size1

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

!    if (SeparatorLoc == ' ') SeparatorLoc = '1X'
! 
!    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

  Convert_String1D_To_C0D = ConvertToString(Value=Values(1), Format=FormatLoc)
  i = 2
  do i = 2, size(Values,1)
    Convert_String1D_To_C0D = Convert_String1D_To_C0D // SeparatorLoc // ConvertToString(Value=Values(i), Format=FormatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C0D_To_String1D(Value, Strings, Separator)

  character(*), intent(in)                                            ::    Value
  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Strings
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_String1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = Value
  call VarString0D%Split(Separator=SeparatorLoc, Strings=Strings)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_C1D_To_String1D(Values, Strings)

  character(*), dimension(:), intent(in)                              ::    Values
  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_String1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Values,1)

  if(allocated(Strings)) then
    if (size(Strings,1) /= NbEntries) then
      deallocate(Strings, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Strings', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Strings)) then
    allocate(Strings(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Strings', ProcName=ProcName, stat=StatLoc)
  end if

  i = 1
  do i = 1, NbEntries
    Strings(i) = trim(adjustl(Values(i)(:)))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_R41D_To_String1D(Values, Strings)

  real(4), dimension(:), intent(in)                                   ::    Values
  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_R41D_To_String1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Values,1)

  if(allocated(Strings)) then
    if (size(Strings,1) /= NbEntries) then
      deallocate(Strings, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Strings', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Strings)) then
    allocate(Strings(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Strings', ProcName=ProcName, stat=StatLoc)
  end if

  i = 1
  do i = 1, NbEntries
    Strings(i) = ConvertToString(Value=Values(i))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Convert_R81D_To_String1D(Values, Strings)

  real(8), dimension(:), intent(in)                                   ::    Values
  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_R81D_To_String1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    NbEntries 

  NbEntries = size(Values,1)

  if(allocated(Strings)) then
    if (size(Strings,1) /= NbEntries) then
      deallocate(Strings, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Strings', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Strings)) then
    allocate(Strings(NbEntries), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Strings', ProcName=ProcName, stat=StatLoc)
  end if

  i = 1
  do i = 1, NbEntries
    Strings(i) = ConvertToString(Value=Values(i))
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
