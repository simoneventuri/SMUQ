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
public                                                                ::    ConvertToInteger4s
public                                                                ::    ConvertToInteger8
public                                                                ::    ConvertToInteger8s
public                                                                ::    ConvertToReal
public                                                                ::    ConvertToReals
public                                                                ::    ConvertToReal4
public                                                                ::    ConvertToReal4s
public                                                                ::    ConvertToReal8
public                                                                ::    ConvertToReal8s
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
  module procedure                                                    ::    Convert_C0D_To_I1D
  module procedure                                                    ::    Convert_C1D_To_I1D
  module procedure                                                    ::    Convert_String0D_To_I1D
  module procedure                                                    ::    Convert_String1D_To_I1D
end interface

interface ConvertToInteger4                                            
  module procedure                                                    ::    Convert_C0D_To_I40D
  module procedure                                                    ::    Convert_String0D_To_I40D
end interface

interface ConvertToInteger4s                                            
  module procedure                                                    ::    Convert_C0D_To_I41D
  module procedure                                                    ::    Convert_C1D_To_I41D
  module procedure                                                    ::    Convert_String0D_To_I41D
  module procedure                                                    ::    Convert_String1D_To_I41D
end interface

interface ConvertToInteger8                                            
  module procedure                                                    ::    Convert_C0D_To_I80D
  module procedure                                                    ::    Convert_String0D_To_I80D
end interface

interface ConvertToInteger8s                                            
  module procedure                                                    ::    Convert_C0D_To_I81D
  module procedure                                                    ::    Convert_C1D_To_I81D
  module procedure                                                    ::    Convert_String0D_To_I81D
  module procedure                                                    ::    Convert_String1D_To_I81D
end interface

interface ConvertToReal                                            
  module procedure                                                    ::    Convert_C0D_To_R0D
  module procedure                                                    ::    Convert_String0D_To_R0D
end interface

interface ConvertToReals                                         
  module procedure                                                    ::    Convert_C0D_To_R1D
  module procedure                                                    ::    Convert_C1D_To_R1D
  module procedure                                                    ::    Convert_String0D_To_R1D
  module procedure                                                    ::    Convert_String1D_To_R1D
end interface

interface ConvertToReal4                                            
  module procedure                                                    ::    Convert_C0D_To_R40D
  module procedure                                                    ::    Convert_String0D_To_R40D
end interface

interface ConvertToReal4s                                         
  module procedure                                                    ::    Convert_C0D_To_R41D
  module procedure                                                    ::    Convert_C1D_To_R41D
  module procedure                                                    ::    Convert_String0D_To_R41D
  module procedure                                                    ::    Convert_String1D_To_R41D
end interface

interface ConvertToReal8                                           
  module procedure                                                    ::    Convert_C0D_To_R80D
  module procedure                                                    ::    Convert_String0D_To_R80D
end interface

interface ConvertToReal8s                                           
  module procedure                                                    ::    Convert_C0D_To_R81D
  module procedure                                                    ::    Convert_C1D_To_R81D
  module procedure                                                    ::    Convert_String0D_To_R81D
  module procedure                                                    ::    Convert_String1D_To_R81D
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
function Convert_C0D_To_I41D(String, Separator)

  integer(4), allocatable, dimension(:)                               ::    Convert_C0D_To_I41D

  character(*), intent(in)                                            ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_I41D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  allocate(VarString1D, source=VarString0D%Split(Separator=SeparatorLoc), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

  allocate(Convert_C0D_To_I41D(size(VarString1D,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C0D_To_I41D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(VarString1D)
    Convert_C0D_To_I41D(i) = ConvertToInteger4(String=VarString1D(i)%Get())
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_I41D(String, Separator)

  integer(4), allocatable, dimension(:)                               ::    Convert_String0D_To_I41D

  class(SMUQString_Type), intent(in)                                  ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_I41D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  Convert_String0D_To_I41D = ConvertToInteger4s(String=String%Get(), Separator=SeparatorLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_I41D(Strings)

  integer(4), allocatable, dimension(:)                               ::    Convert_C1D_To_I41D

  character(*), dimension(:), intent(in)                              ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_I41D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_C1D_To_I41D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_I41D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_I41D(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String1D_To_I41D(Strings)

  integer(4), allocatable, dimension(:)                               ::    Convert_String1D_To_I41D

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_I41D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_String1D_To_I41D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_I41D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    Convert_String1D_To_I41D(i) = ConvertToInteger4(String=Strings(i)%Get())
  end do

end function
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
function Convert_C0D_To_I81D(String, Separator)

  integer(8), allocatable, dimension(:)                               ::    Convert_C0D_To_I81D

  character(*), intent(in)                                            ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_I81D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  allocate(VarString1D, source=VarString0D%Split(Separator=SeparatorLoc), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

  allocate(Convert_C0D_To_I81D(size(VarString1D,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C0D_To_I81D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(VarString1D)
    Convert_C0D_To_I81D(i) = ConvertToInteger8(String=VarString1D(i)%Get())
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_I81D(String, Separator)

  integer(8), allocatable, dimension(:)                               ::    Convert_String0D_To_I81D

  class(SMUQString_Type), intent(in)                                  ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_I81D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  Convert_String0D_To_I81D = ConvertToInteger8s(String=String%Get(), Separator=SeparatorLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_I81D(Strings)

  integer(8), allocatable, dimension(:)                               ::    Convert_C1D_To_I81D

  character(*), dimension(:), intent(in)                              ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_I81D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_C1D_To_I81D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_I81D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_I81D(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String1D_To_I81D(Strings)

  integer(8), allocatable, dimension(:)                               ::    Convert_String1D_To_I81D

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_I81D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_String1D_To_I81D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_String1D_To_I81D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    Convert_String1D_To_I81D(i) = ConvertToInteger8(String=Strings(i)%Get())
  end do

end function
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
function Convert_C0D_To_I1D(String, Separator)

  integer, allocatable, dimension(:)                                  ::    Convert_C0D_To_I1D

  character(*), intent(in)                                            ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_I1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  allocate(VarString1D, source=VarString0D%Split(Separator=SeparatorLoc), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

  allocate(Convert_C0D_To_I1D(size(VarString1D,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C0D_To_I1D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(VarString1D)
    Convert_C0D_To_I1D(i) = ConvertToInteger(String=VarString1D(i)%Get())
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_I1D(String, Separator)

  integer, allocatable, dimension(:)                                  ::    Convert_String0D_To_I1D

  class(SMUQString_Type), intent(in)                                  ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_I1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  Convert_String0D_To_I1D = ConvertToIntegers(String=String%Get(), Separator=SeparatorLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_I1D(Strings)

  integer, allocatable, dimension(:)                                  ::    Convert_C1D_To_I1D

  character(*), dimension(:), intent(in)                              ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_I1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_C1D_To_I1D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_I1D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_I1D(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String1D_To_I1D(Strings)

  integer, allocatable, dimension(:)                                  ::    Convert_String1D_To_I1D

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_I1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_String1D_To_I1D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_String1D_To_I1D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    Convert_String1D_To_I1D(i) = ConvertToInteger(String=Strings(i)%Get())
  end do

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
function Convert_C0D_To_R1D(String, Separator)

  real(rkp), allocatable, dimension(:)                                ::    Convert_C0D_To_R1D

  character(*), intent(in)                                            ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_R1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  allocate(VarString1D, source=VarString0D%Split(Separator=SeparatorLoc), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

  allocate(Convert_C0D_To_R1D(size(VarString1D,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C0D_To_R1D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(VarString1D)
    Convert_C0D_To_R1D(i) = ConvertToReal(String=VarString1D(i)%Get())
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_R1D(String, Separator)

  real(rkp), allocatable, dimension(:)                                ::    Convert_String0D_To_R1D

  class(SMUQString_Type), intent(in)                                  ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_R1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  Convert_String0D_to_R1D = ConvertToReals(String=String%Get(), Separator=SeparatorLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_R1D(Strings)

  real(rkp), allocatable, dimension(:)                                ::    Convert_C1D_To_R1D

  character(*), dimension(:), intent(in)                              ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_R1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_C1D_To_R1D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_R1D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_R1D(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String1D_To_R1D(Strings)

  real(rkp), allocatable, dimension(:)                                ::    Convert_String1D_To_R1D

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_R1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_String1D_To_R1D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_String1D_To_R1D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    Convert_String1D_To_R1D(i) = ConvertToReal(String=Strings(i)%Get())
  end do

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
function Convert_C0D_To_R41D(String, Separator)

  real(4), allocatable, dimension(:)                                  ::    Convert_C0D_To_R41D

  character(*), intent(in)                                            ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_R41D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  allocate(VarString1D, source=VarString0D%Split(Separator=SeparatorLoc), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

  allocate(Convert_C0D_To_R41D(size(VarString1D,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C0D_To_R41D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(VarString1D)
    Convert_C0D_To_R41D(i) = ConvertToReal4(String=VarString1D(i)%Get())
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_R41D(String, Separator)

  real(4), allocatable, dimension(:)                                  ::    Convert_String0D_To_R41D

  class(SMUQString_Type), intent(in)                                  ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_R41D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  Convert_String0D_To_R41D = ConvertToReal4s(String=String%Get(), Separator=SeparatorLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_R41D(Strings)

  real(4), allocatable, dimension(:)                                  ::    Convert_C1D_To_R41D

  character(*), dimension(:), intent(in)                              ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_R41D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_C1D_To_R41D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_R41D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_R41D(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String1D_To_R41D(Strings)

  real(4), allocatable, dimension(:)                                  ::    Convert_String1D_To_R41D

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_R41D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_String1D_To_R41D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_String1D_To_R41D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    Convert_String1D_To_R41D(i) = ConvertToReal4(String=Strings(i)%Get())
  end do

end function
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
function Convert_C0D_To_R81D(String, Separator)

  real(8), allocatable, dimension(:)                                  ::    Convert_C0D_To_R81D

  character(*), intent(in)                                            ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_R81D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  allocate(VarString1D, source=VarString0D%Split(Separator=SeparatorLoc), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

  allocate(Convert_C0D_To_R81D(size(VarString1D,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C0D_To_R81D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(VarString1D)
    Convert_C0D_To_R81D(i) = ConvertToReal8(String=VarString1D(i)%Get())
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_R81D(String, Separator)

  real(8), allocatable, dimension(:)                                  ::    Convert_String0D_To_R81D

  class(SMUQString_Type), intent(in)                                  ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_R81D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  Convert_String0D_To_R81D = ConvertToReal8s(String=String%Get(), Separator=SeparatorLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_R81D(Strings)

  real(8), allocatable, dimension(:)                                  ::    Convert_C1D_To_R81D

  character(*), dimension(:), intent(in)                              ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_R81D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_C1D_To_R81D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_R81D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_R81D(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String1D_To_R81D(Strings)

  real(8), allocatable, dimension(:)                                  ::    Convert_String1D_To_R81D

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_R81D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_String1D_To_R81D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_String1D_To_R81D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    Convert_String1D_To_R81D(i) = ConvertToReal8(String=Strings(i)%Get())
  end do

end function
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
function Convert_C0D_To_L1D(String, Separator)

  logical, allocatable, dimension(:)                                  ::    Convert_C0D_To_L1D

  character(*), intent(in)                                            ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_L1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  allocate(VarString1D, source=VarString0D%Split(Separator=SeparatorLoc), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

  allocate(Convert_C0D_To_L1D(size(VarString1D,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C0D_To_L1D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(VarString1D)
    Convert_C0D_To_L1D(i) = ConvertToLogical(String=VarString1D(i)%Get())
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_L1D(String, Separator)

  logical, allocatable, dimension(:)                                  ::    Convert_String0D_To_L1D

  class(SMUQString_Type), intent(in)                                  ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_L1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  Convert_String0D_To_L1D = ConvertToLogicals(String=String%Get())

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_L1D(Strings)

  logical, allocatable, dimension(:)                                  ::    Convert_C1D_To_L1D

  character(*), dimension(:), intent(in)                              ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_L1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_C1D_To_L1D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_L1D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_L1D(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String1D_To_L1D(Strings)

  logical, allocatable, dimension(:)                                  ::    Convert_String1D_To_L1D

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_L1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_String1D_To_L1D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_String1D_To_L1D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    Convert_String1D_To_L1D(i) = ConvertToLogical(String=Strings(i)%Get())
  end do

end function
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
function Convert_C0D_To_CX1D(String, Separator)

  complex, allocatable, dimension(:)                                  ::    Convert_C0D_To_CX1D

  character(*), intent(in)                                            ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_CX1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D
  type(SMUQString_Type), allocatable, dimension(:)                    ::    VarString1D
  integer                                                             ::    i

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = String
  allocate(VarString1D, source=VarString0D%Split(Separator=SeparatorLoc), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarString1D', ProcName=ProcName, stat=StatLoc)

  allocate(Convert_C0D_To_CX1D(size(VarString1D,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C0D_To_CX1D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(VarString1D)
    Convert_C0D_To_CX1D(i) = ConvertToComplex(String=VarString1D(i)%Get())
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String0D_To_CX1D(String, Separator)

  complex, allocatable, dimension(:)                                  ::    Convert_String0D_To_CX1D

  class(SMUQString_Type), intent(in)                                  ::    String
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_String0D_To_CX1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  Convert_String0D_To_CX1D = ConvertToComplexs(String=String%Get(), Separator=SeparatorLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_CX1D(Strings)

  complex, allocatable, dimension(:)                                  ::    Convert_C1D_To_CX1D

  character(*), dimension(:), intent(in)                              ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_CX1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_C1D_To_CX1D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_CX1D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_CX1D(i)
    if (StatLoc /= 0) call Error%Read(Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_String1D_To_CX1D(Strings)

  complex, allocatable, dimension(:)                                  ::    Convert_String1D_To_CX1D

  class(SMUQString_Type), dimension(:), intent(in)                    ::    Strings

  character(*), parameter                                             ::    ProcName='Convert_String1D_To_CX1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_String1D_To_CX1D(size(Strings,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_String1D_To_CX1D', ProcName=ProcName, stat=StatLoc)

  do i = 1, size(Strings,1)
    Convert_String1D_To_CX1D(i) = ConvertToComplex(String=Strings(i)%Get())
  end do

end function
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
function Convert_C0D_To_String1D(Value, Separator)

  type(SMUQString_Type), dimension(:), allocatable                    ::    Convert_C0D_To_String1D

  character(*), intent(in)                                            ::    Value
  character(*), optional, intent(in)                                  ::    Separator

  character(*), parameter                                             ::    ProcName='Convert_C0D_To_String1D'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    SeparatorLoc
  type(SMUQString_Type)                                               ::    VarString0D

  SeparatorLoc = ' '
  if (present(Separator)) SeparatorLoc = Separator

  VarString0D = Value
  allocate(Convert_C0D_To_String1D, source=VarString0D%Split(Separator=SeparatorLoc), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C0D_To_String1D', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_C1D_To_String1D(Values)

  type(SMUQString_Type), dimension(:), allocatable                    ::    Convert_C1D_To_String1D

  character(*), dimension(:), intent(in)                              ::    Values

  character(*), parameter                                             ::    ProcName='Convert_C1D_To_String1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_C1D_To_String1D(size(Values,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_C1D_To_String1D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(Values,1)
    Convert_C1D_To_String1D(i) = trim(adjustl(Values(i)(:)))
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_R41D_To_String1D(Values)

  type(SMUQString_Type), dimension(:), allocatable                    ::    Convert_R41D_To_String1D

  real(4), dimension(:), intent(in)                                   ::    Values

  character(*), parameter                                             ::    ProcName='Convert_R41D_To_String1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_R41D_To_String1D(size(Values,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_R41D_To_String1D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(Values,1)
    Convert_R41D_To_String1D(i) = ConvertToString(Value=Values(i))
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Convert_R81D_To_String1D(Values)

  type(SMUQString_Type), dimension(:), allocatable                    ::    Convert_R81D_To_String1D

  real(8), dimension(:), intent(in)                                   ::    Values

  character(*), parameter                                             ::    ProcName='Convert_R81D_To_String1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  allocate(Convert_R81D_To_String1D(size(Values,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Convert_R81D_To_String1D', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, size(Values,1)
    Convert_R81D_To_String1D(i) = ConvertToString(Value=Values(i))
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

end module
