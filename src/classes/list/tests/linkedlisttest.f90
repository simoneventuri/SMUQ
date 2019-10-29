program test

use Parameters_Library
use String_Library
use Logger_Class                  , only: Logger
use Error_Class                   , only: Error
use LinkedList0D_Class            , only: LinkedList0D_Type
use LinkedList1D_Class            , only: LinkedList1D_Type
use LinkedList2D_Class            , only: LinkedList2D_Type

implicit none

type(LinkedList0D_Type)                                               ::    list0d
type(LinkedList1D_Type)                                               ::    list1d
type(LinkedList2D_Type)                                               ::    list2d
real(rkp)                                                             ::    VarR0D
real(rkp), dimension(:), allocatable                                  ::    VarR1D
real(rkp), dimension(:,:), allocatable                                ::    VarR2D
character(:), allocatable                                             ::    FileName
character(:), allocatable                                             ::    Prefix
character(1000)                                                       ::    Argument
character(:), allocatable                                             ::    ArgumentLC, ArgumentShort
integer                                                               ::    i, ArgumentLength
logical                                                               ::    i_Debug_Loc = .false.              

i = 1
do i = 1, iargc()
  call get_command_argument(Number=i, Value=Argument, Length=ArgumentLength)
  if ( ArgumentLength > 7 ) then
    ArgumentShort = trim(adjustl(Argument))
    ArgumentLC = LowerCase(ArgumentShort)
    if ( ArgumentShort(1:7) == 'rundir=' ) then
      Prefix = ArgumentShort(8:ArgumentLength)
      exit
    end if
  end if
  if ( i == iargc() ) call Error%Raise( "Run directory command line argument not detected" )
end do

FileName = Prefix // '/runlog.log'

call Logger%Initialize( FileName=FileName,  &
            Status          =       'REPLACE',                                                   &
            Position        =       'REWIND',                                                    &
            Procedure       =       'test',                                                      &
            Indentation     =       0                                                            )

allocate(VarR1D(3))
allocate(VarR2D(3,3))

VarR0D = Zero
VarR1D = One
VarR2D = Two

do i = 1, 10
call list0d%append( VarR0D )
call list1d%append( VarR1D )
call list2d%append( VarR2D )
end do

deallocate(VarR1D)
deallocate(VarR2D)

do i = 1, 10
call list0d%get( 1, VarR0D )
call list1d%get( 1, VarR1D )
call list2d%get( 2, VarR2D )
write(*,*) VarR0D
write(*,*) VarR1D
write(*,*) VarR2D
end do

end program
