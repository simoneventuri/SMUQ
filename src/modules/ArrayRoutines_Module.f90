module ArrayRoutines_Module

use Input_Library
use Parameters_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    Eye
public                                                                ::    StrictTriangular
public                                                                ::    Diagonal
public                                                                ::    IsDiagonal
public                                                                ::    IsArrayConstant
public                                                                ::    EnsureArraySize

logical, parameter                                                    ::    DebugGlobal = .false.

interface IsArrayConstant
  module procedure                                                    ::    IsArrayConstant_R41D
  module procedure                                                    ::    IsArrayConstant_R81D
end interface

interface IsDiagonal
  module procedure                                                    ::    IsDiagonal_Real8
end interface

interface Eye
  module procedure                                                    ::    Eye_R42D
  module procedure                                                    ::    Eye_R42D_NonAlloc
  module procedure                                                    ::    Eye_R82D
  module procedure                                                    ::    Eye_R82D_NonAlloc
  module procedure                                                    ::    Eye_I42D
  module procedure                                                    ::    Eye_I42D_NonAlloc
  module procedure                                                    ::    Eye_I82D
  module procedure                                                    ::    Eye_I82D_NonAlloc
end interface

interface StrictTriangular
  module procedure                                                    ::    StrictTriangular_I42D
  module procedure                                                    ::    StrictTriangular_I42D_NonAlloc
  module procedure                                                    ::    StrictTriangular_I82D
  module procedure                                                    ::    StrictTriangular_I82D_NonAlloc
  module procedure                                                    ::    StrictTriangular_R42D
  module procedure                                                    ::    StrictTriangular_R42D_NonAlloc
  module procedure                                                    ::    StrictTriangular_R82D
  module procedure                                                    ::    StrictTriangular_R82D_NonAlloc
end interface

interface Diagonal
  module procedure                                                    ::    Diagonal_I42D
  module procedure                                                    ::    Diagonal_I82D
  module procedure                                                    ::    Diagonal_R42D
  module procedure                                                    ::    Diagonal_R82D
end interface

interface EnsureArraySize
  module procedure                                                    ::    EnsureArraySize_R41D
  module procedure                                                    ::    EnsureArraySize_R81D
  module procedure                                                    ::    EnsureArraySize_I41D
  module procedure                                                    ::    EnsureArraySize_I81D
  module procedure                                                    ::    EnsureArraySize_R42D
  module procedure                                                    ::    EnsureArraySize_R82D
  module procedure                                                    ::    EnsureArraySize_I42D
  module procedure                                                    ::    EnsureArraySize_I82D
  module procedure                                                    ::    EnsureArraySize_L1D
  module procedure                                                    ::    EnsureArraySize_L2D
  module procedure                                                    ::    EnsureArraySize_CX1D
  module procedure                                                    ::    EnsureArraySize_CX2D
  module procedure                                                    ::    EnsureArraySize_String1D
  module procedure                                                    ::    EnsureArraySize_String2D
end interface

contains

!!------------------------------------------------------------------------------------------------------------------------------
function IsArrayConstant_R41D(Array)

  logical                                                             ::    IsArrayConstant_R41D

  real(4), dimension(:), intent(in)                                   ::    Array

  character(*), parameter                                             ::    ProcName='IsArrayConstant_R41D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    N
  real(4), parameter                                                  ::    ZeroLoc=0.0_4
 
  N = size(Array,1)

  isArrayConstant_R41D = .true.
  i = 2
  do i = 2, N
    if (.not. abs(Array(i)-Array(i-1)) > ZeroLoc) cycle
    IsArrayConstant_R41D = .false.
    exit
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function IsArrayConstant_R81D(Array)

  logical                                                             ::    IsArrayConstant_R81D

  real(8), dimension(:), intent(in)                                   ::    Array

  character(*), parameter                                             ::    ProcName='IsArrayConstant_R81D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    N
  real(8), parameter                                                  ::    ZeroLoc=0.0_8
 
  N = size(Array,1)

  isArrayConstant_R81D = .true.
  i = 2
  do i = 2, N
    if (.not. dabs(Array(i)-Array(i-1)) > ZeroLoc) cycle
    IsArrayConstant_R81D = .false.
    exit
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function IsDiagonal_Real8(Array)

  logical                                                             ::    IsDiagonal_Real8

  real(rkp), dimension(:,:), intent(in)                               ::    Array

  character(*), parameter                                             ::    ProcName='IsDiagonal_Real8'
  integer                                                             ::    M
  integer                                                             ::    N
  integer                                                             ::    i, ii

  M = size(Array,1)
  N = size(Array,2)

  IsDiagonal_Real8 = .true.

  if (M /= N) then
    IsDiagonal_Real8 = .false.
  else if (M == 1) then
    IsDiagonal_Real8 = .true.
  else
    do i = 2, M
      do ii = 1, i-1
        if (Array(i,ii) > precision(Array) .or. Array(ii,i) > precision(Array)) then
          IsDiagonal_Real8 = .false.
          exit
        end if
      end do
      if (.not. IsDiagonal_Real8) exit
    end do     
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StrictTriangular_I42D_NonAlloc(Array, UL)

  integer(4), dimension(:,:), intent(inout)                           ::    Array
  character(*), intent(in)                                            ::    UL

  character(*), parameter                                             ::    ProcName='StrictTriangular_I42D_NonAlloc'
  integer                                                             ::    i
  integer                                                             ::    M
  integer                                                             ::    N
  integer(4)                                                          ::    imax

  M = size(Array,1)
  N = size(Array,2)

  Array = 0

  if (UL == 'U') then
    i = 2
    do i = 2, N
      if (i > M) then
        Array(:,i:) = 1
        exit
      end if
      Array(1:i-1,i) = 1
    end do
  elseif (UL == 'L') then
    if (M >= N) then
      imax = N 
    else
      imax = N - 1
    end if
    i = 1
    do i = 1, imax
      Array(i+1:,i) = 1
    end do
  else
    call Error%Raise('Upper or Lower option not recognized', ProcName=ProcName)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StrictTriangular_I42D(Array, UL, M, N)

  integer(4), allocatable, dimension(:,:), intent(inout)              ::    Array
  character(*), intent(in)                                            ::    UL
  integer, intent(in)                                                 ::    M 
  integer, intent(in)                                                 ::    N

  character(*), parameter                                             ::    ProcName='StrictTriangular_I42D'
  integer                                                             ::    i
  integer(4)                                                          ::    imax

  call EnsureArraySize(Array=Array, Size1=M, Size2=N, DefaultValue=.false.)
  Array = 0

  if (UL == 'U') then
    i = 2
    do i = 2, N
      if (i > M) then
        Array(:,i:) = 1
        exit
      end if
      Array(1:i-1,i) = 1
    end do
  elseif (UL == 'L') then
    if (M >= N) then
      imax = N 
    else
      imax = N - 1
    end if
    i = 1
    do i = 1, imax
      Array(i+1:,i) = 1
    end do
  else
    call Error%Raise('Upper or Lower option not recognized', ProcName=ProcName)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StrictTriangular_I82D_NonAlloc(Array, UL)

  integer(8), dimension(:,:), intent(inout)                           ::    Array
  character(*), intent(in)                                            ::    UL

  character(*), parameter                                             ::    ProcName='StrictTriangular_I82D_NonAlloc'
  integer                                                             ::    i
  integer                                                             ::    M
  integer                                                             ::    N
  integer(4)                                                          ::    imax

  M = size(Array,1)
  N = size(Array,2)

  Array = 0

  if (UL == 'U') then
    i = 2
    do i = 2, N
      if (i > M) then
        Array(:,i:) = 1
        exit
      end if
      Array(1:i-1,i) = 1
    end do
  elseif (UL == 'L') then
    if (M >= N) then
      imax = N 
    else
      imax = N - 1
    end if
    i = 1
    do i = 1, imax
      Array(i+1:,i) = 1
    end do
  else
    call Error%Raise('Upper or Lower option not recognized', ProcName=ProcName)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StrictTriangular_I82D(Array, UL, M, N)

  integer(8), allocatable, dimension(:,:), intent(inout)              ::    Array
  character(*), intent(in)                                            ::    UL
  integer, intent(in)                                                 ::    M 
  integer, intent(in)                                                 ::    N

  character(*), parameter                                             ::    ProcName='StrictTriangular_I82D'
  integer                                                             ::    i
  integer(4)                                                          ::    imax

  call EnsureArraySize(Array=Array, Size1=M, Size2=N, DefaultValue=.false.)
  Array = 0

  if (UL == 'U') then
    i = 2
    do i = 2, N
      if (i > M) then
        Array(:,i:) = 1
        exit
      end if
      Array(1:i-1,i) = 1
    end do
  elseif (UL == 'L') then
    if (M >= N) then
      imax = N 
    else
      imax = N - 1
    end if
    i = 1
    do i = 1, imax
      Array(i+1:,i) = 1
    end do
  else
    call Error%Raise('Upper or Lower option not recognized', ProcName=ProcName)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StrictTriangular_R42D_NonAlloc(Array, UL)

  real(4), dimension(:,:), intent(inout)                              ::    Array
  character(*), intent(in)                                            ::    UL

  character(*), parameter                                             ::    ProcName='StrictTriangular_R42D_NonAlloc'
  integer                                                             ::    i
  integer                                                             ::    M
  integer                                                             ::    N
  integer(4)                                                          ::    imax

  M = size(Array,1)
  N = size(Array,2)

  Array = 0.0

  if (UL == 'U') then
    i = 2
    do i = 2, N
      if (i > M) then
        Array(:,i:) = 1.0
        exit
      end if
      Array(1:i-1,i) = 1.0
    end do
  elseif (UL == 'L') then
    if (M >= N) then
      imax = N 
    else
      imax = N - 1
    end if
    i = 1
    do i = 1, imax
      Array(i+1:,i) = 1.0
    end do
  else
    call Error%Raise('Upper or Lower option not recognized', ProcName=ProcName)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StrictTriangular_R42D(Array, UL, M, N)

  real(4), allocatable, dimension(:,:), intent(inout)                 ::    Array
  character(*), intent(in)                                            ::    UL
  integer, intent(in)                                                 ::    M 
  integer, intent(in)                                                 ::    N

  character(*), parameter                                             ::    ProcName='StrictTriangular_R42D'
  integer                                                             ::    i
  integer(4)                                                          ::    imax

  call EnsureArraySize(Array=Array, Size1=M, Size2=N, DefaultValue=.false.)
  Array = 0.0

  if (UL == 'U') then
    i = 2
    do i = 2, N
      if (i > M) then
        Array(:,i:) = 1.0
        exit
      end if
      Array(1:i-1,i) = 1.0
    end do
  elseif (UL == 'L') then
    if (M >= N) then
      imax = N 
    else
      imax = N - 1
    end if
    i = 1
    do i = 1, imax
      Array(i+1:,i) = 1.0
    end do
  else
    call Error%Raise('Upper or Lower option not recognized', ProcName=ProcName)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StrictTriangular_R82D_NonAlloc(Array, UL)

  real(8), dimension(:,:), intent(inout)                              ::    Array
  character(*), intent(in)                                            ::    UL

  character(*), parameter                                             ::    ProcName='StrictTriangular_R82D_NonAlloc'
  integer                                                             ::    i
  integer                                                             ::    M
  integer                                                             ::    N
  integer(4)                                                          ::    imax

  M = size(Array,1)
  N = size(Array,2)

  Array = 0.0

  if (UL == 'U') then
    i = 2
    do i = 2, N
      if (i > M) then
        Array(:,i:) = 1.0
        exit
      end if
      Array(1:i-1,i) = 1.0
    end do
  elseif (UL == 'L') then
    if (M >= N) then
      imax = N 
    else
      imax = N - 1
    end if
    i = 1
    do i = 1, imax
      Array(i+1:,i) = 1.0
    end do
  else
    call Error%Raise('Upper or Lower option not recognized', ProcName=ProcName)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine StrictTriangular_R82D(Array, UL, M, N)

  real(8), allocatable, dimension(:,:), intent(inout)                 ::    Array
  character(*), intent(in)                                            ::    UL
  integer, intent(in)                                                 ::    M
  integer, intent(in)                                                 ::    N

  character(*), parameter                                             ::    ProcName='StrictTriangular_R82D'
  integer                                                             ::    i
  integer(4)                                                          ::    imax

  call EnsureArraySize(Array=Array, Size1=M, Size2=N, DefaultValue=.false.)
  Array = 0.0

  if (UL == 'U') then
    i = 2
    do i = 2, N
      if (i > M) then
        Array(:,i:) = 1.0
        exit
      end if
      Array(1:i-1,i) = 1.0
    end do
  elseif (UL == 'L') then
    if (M >= N) then
      imax = N 
    else
      imax = N - 1
    end if
    i = 1
    do i = 1, imax
      Array(i+1:,i) = 1.0
    end do
  else
    call Error%Raise('Upper or Lower option not recognized', ProcName=ProcName)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Diagonal_I42D(Diagonal, Array)

  integer(4), dimension(:), intent(in)                                ::    Diagonal
  integer(4), allocatable, dimension(:,:), intent(inout)              ::    Array

  character(*), parameter                                             ::    ProcName='Diagonal_I42D'
  integer                                                             ::    i
  integer                                                             ::    N
  integer                                                             ::    StatLoc

  N = size(Diagonal,1)

  call EnsureArraySize(Array=Array, Size1=N, Size2=N, DefaultValue=.false.)
  Array = 0

  i = 1
  do i = 1, N
    Array(i,i) = Diagonal(i)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Diagonal_I82D(Diagonal, Array)

  integer(8), dimension(:), intent(in)                                ::    Diagonal
  integer(8), allocatable, dimension(:,:), intent(inout)              ::    Array

  character(*), parameter                                             ::    ProcName='Diagonal_I82D'
  integer                                                             ::    i
  integer                                                             ::    N
  integer                                                             ::    StatLoc

  N = size(Diagonal,1)

  call EnsureArraySize(Array=Array, Size1=N, Size2=N, DefaultValue=.false.)
  Array = 0

  i = 1
  do i = 1, N
    Array(i,i) = Diagonal(i)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Diagonal_R42D(Diagonal, Array)

  real(4), dimension(:), intent(in)                                   ::    Diagonal
  real(4), allocatable, dimension(:,:), intent(inout)                 ::    Array

  character(*), parameter                                             ::    ProcName='Diagonal_R42D'
  integer                                                             ::    i
  integer                                                             ::    N
  integer                                                             ::    StatLoc

  N = size(Diagonal,1)

  call EnsureArraySize(Array=Array, Size1=N, Size2=N, DefaultValue=.false.)
  Array = 0.0_4

  i = 1
  do i = 1, N
    Array(i,i) = Diagonal(i)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Diagonal_R82D(Diagonal, Array)

  real(8), dimension(:), intent(in)                                   ::    Diagonal
  real(8), allocatable, dimension(:,:), intent(inout)                 ::    Array

  character(*), parameter                                             ::    ProcName='Diagonal_R82D'
  integer                                                             ::    i
  integer                                                             ::    N
  integer                                                             ::    StatLoc

  N = size(Diagonal,1)

  call EnsureArraySize(Array=Array, Size1=N, Size2=N, DefaultValue=.false.)
  Array = 0.0_8

  i = 1
  do i = 1, N
    Array(i,i) = Diagonal(i)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eye_R42D(Array, N)

  real(4), allocatable, dimension(:,:), intent(inout)                 ::    Array
  integer, intent(in)                                                 ::    N

  character(*), parameter                                             ::    ProcName='Eye_R42D'
  integer                                                             ::    StatLoc
  integer                                                             ::    i

  call EnsureArraySize(Array=Array, Size1=N, Size2=N)

  Array = 0.0_4
  do i = 1, N
    Array(i,i) = 1.0_4
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eye_R42D_NonAlloc(Array)

  real(4), dimension(:,:), intent(inout)                              ::    Array

  character(*), parameter                                             ::    ProcName='Eye_R42D_NonAlloc'
  integer                                                             ::    StatLoc
  integer                                                             ::    i
  integer                                                             ::    N

  N = size(Array,1)
  if (N <=0 ) call Error%Raise('Array of size 0', ProcName=ProcName)
  if (size(Array,2) /= N) call Error%Raise('Non-square array', ProcName=ProcName)

  Array = 0.0_4
  do i = 1, N
    Array(i,i) = 1.0_4
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eye_R82D(Array, N)

  real(8), allocatable, dimension(:,:), intent(inout)                 ::    Array
  integer, intent(in)                                                 ::    N

  character(*), parameter                                             ::    ProcName='Eye_R82D'
  integer                                                             ::    StatLoc
  integer                                                             ::    i

  call EnsureArraySize(Array=Array, Size1=N, Size2=N)

  Array = 0.0_8
  do i = 1, N
    Array(i,i) = 1.0_8
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eye_R82D_NonAlloc(Array)

  real(8), dimension(:,:), intent(inout)                              ::    Array

  character(*), parameter                                             ::    ProcName='Eye_R82D_NonAlloc'
  integer                                                             ::    StatLoc
  integer                                                             ::    i
  integer                                                             ::    N

  N = size(Array,1)
  if (N <=0 ) call Error%Raise('Array of size 0', ProcName=ProcName)
  if (size(Array,2) /= N) call Error%Raise('Non-square array', ProcName=ProcName)

  Array = 0.0_8
  do i = 1, N
    Array(i,i) = 1.0_8
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eye_I42D(Array, N)

  integer(4), allocatable, dimension(:,:), intent(inout)              ::    Array
  integer, intent(in)                                                 ::    N

  character(*), parameter                                             ::    ProcName='Eye_I42D'
  integer                                                             ::    StatLoc
  integer                                                             ::    i

  call EnsureArraySize(Array=Array, Size1=N, Size2=N)

  Array = 0
  do i = 1, N
    Array(i,i) = 1
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eye_I42D_NonAlloc(Array)

  integer(4), dimension(:,:), intent(inout)                           ::    Array

  character(*), parameter                                             ::    ProcName='Eye_I42D_NonAlloc'
  integer                                                             ::    StatLoc
  integer                                                             ::    i
  integer                                                             ::    N

  N = size(Array,1)
  if (N <=0 ) call Error%Raise('Array of size 0', ProcName=ProcName)
  if (size(Array,2) /= N) call Error%Raise('Non-square array', ProcName=ProcName)

  Array = 0
  do i = 1, N
    Array(i,i) = 1
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eye_I82D(Array, N)

  integer(8), allocatable, dimension(:,:), intent(inout)              ::    Array
  integer, intent(in)                                                 ::    N

  character(*), parameter                                             ::    ProcName='Eye_I82D'
  integer                                                             ::    StatLoc
  integer                                                             ::    i

  call EnsureArraySize(Array=Array, Size1=N, Size2=N)

  Array = 0
  do i = 1, N
    Array(i,i) = 1
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eye_I82D_NonAlloc(Array)

  integer(8), dimension(:,:), intent(inout)                           ::    Array

  character(*), parameter                                             ::    ProcName='Eye_I82D_NonAlloc'
  integer                                                             ::    StatLoc
  integer                                                             ::    i
  integer                                                             ::    N

  N = size(Array,1)
  if (N <=0 ) call Error%Raise('Array of size 0', ProcName=ProcName)
  if (size(Array,2) /= N) call Error%Raise('Non-square array', ProcName=ProcName)

  Array = 0
  do i = 1, N
    Array(i,i) = 1
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_R41D(Array, Size1, DefaultValue)

  real(4), allocatable, dimension(:), intent(inout)                   ::    Array
  integer, intent(in)                                                 ::    Size1
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_R41D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0.0_4

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_R81D(Array, Size1, DefaultValue)

  real(8), allocatable, dimension(:), intent(inout)                   ::    Array
  integer, intent(in)                                                 ::    Size1
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_R81D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0.0_8

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_I41D(Array, Size1, DefaultValue)

  integer(4), allocatable, dimension(:), intent(inout)                ::    Array
  integer, intent(in)                                                 ::    Size1
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_I41D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_I81D(Array, Size1, DefaultValue)

  integer(8), allocatable, dimension(:), intent(inout)                ::    Array
  integer, intent(in)                                                 ::    Size1
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_I81D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_R42D(Array, Size1, Size2, DefaultValue)

  real(4), allocatable, dimension(:,:), intent(inout)                 ::    Array
  integer, intent(in)                                                 ::    Size1
  integer, intent(in)                                                 ::    Size2
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_R42D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1 .or. size(Array,2) /= Size2) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1, Size2), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0.0_4

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_R82D(Array, Size1, Size2, DefaultValue)

  real(8), allocatable, dimension(:,:), intent(inout)                 ::    Array
  integer, intent(in)                                                 ::    Size1
  integer, intent(in)                                                 ::    Size2
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_R82D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1 .or. size(Array,2) /= Size2) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1, Size2), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0.0_8

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_I42D(Array, Size1, Size2, DefaultValue)

  integer(4), allocatable, dimension(:,:), intent(inout)              ::    Array
  integer, intent(in)                                                 ::    Size1
  integer, intent(in)                                                 ::    Size2
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_I42D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1 .or. size(Array,2) /= Size2) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1, Size2), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_I82D(Array, Size1, Size2, DefaultValue)

  integer(8), allocatable, dimension(:,:), intent(inout)              ::    Array
  integer, intent(in)                                                 ::    Size1
  integer, intent(in)                                                 ::    Size2
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_I82D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1 .or. size(Array,2) /= Size2) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1, Size2), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_L1D(Array, Size1, DefaultValue)

  logical, allocatable, dimension(:), intent(inout)                   ::    Array
  integer, intent(in)                                                 ::    Size1
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_L1D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = .false.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_L2D(Array, Size1, Size2, DefaultValue)

  logical, allocatable, dimension(:,:), intent(inout)                 ::    Array
  integer, intent(in)                                                 ::    Size1
  integer, intent(in)                                                 ::    Size2
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_L2D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1 .or. size(Array,2) /= Size2) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1, Size2), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = .false.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_CX1D(Array, Size1, DefaultValue)

  complex, allocatable, dimension(:), intent(inout)                   ::    Array
  integer, intent(in)                                                 ::    Size1
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_CX1D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0.0

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_CX2D(Array, Size1, Size2, DefaultValue)

  complex, allocatable, dimension(:,:), intent(inout)                 ::    Array
  integer, intent(in)                                                 ::    Size1
  integer, intent(in)                                                 ::    Size2
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_CX2D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1 .or. size(Array,2) /= Size2) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1, Size2), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = 0.0

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_String1D(Array, Size1, DefaultValue)

  type(SMUQString_Type), allocatable, dimension(:), intent(inout)     ::    Array
  integer, intent(in)                                                 ::    Size1
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_String1D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = ''

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine EnsureArraySize_String2D(Array, Size1, Size2, DefaultValue)

  type(SMUQString_Type), allocatable, dimension(:,:), intent(inout)   ::    Array
  integer, intent(in)                                                 ::    Size1
  integer, intent(in)                                                 ::    Size2
  logical, optional, intent(in)                                       ::    DefaultValue

  character(*), parameter                                             ::    ProcName='EnsureArraySize_String2D'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    DefaultValueLoc

  DefaultValueLoc = .true.
  if (present(DefaultValue)) DefaultValueLoc = DefaultValue

  if (allocated(Array)) then
    if (size(Array,1) /= Size1 .or. size(Array,2) /= Size2) then
      deallocate(Array, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='Array', ProcName=ProcName, stat=StatLoc)
    end if
  end if
  if (.not. allocated(Array)) then
    allocate(Array(Size1, Size2), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Array', ProcName=ProcName, stat=StatLoc)
  end if

  if (DefaultValueLoc) Array = ''

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
