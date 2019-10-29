Module GPF_Plot_Using_Class

  use GPF_Parameters            ,only:  DbgUnit, KEY_splot, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_LineStyle_Class       ,only:  GPF_LineStyle_Type

  implicit none

  private

  public  ::  GPF_Plot_Using_Type

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Plot_Using_Type
    integer                                             ::  iX_Column
    integer                                             ::  iY_Column
    integer                                             ::  iZ_Column
    character(:)        ,allocatable                    ::  Column_X
    character(:)        ,allocatable                    ::  Column_Y
    character(:)        ,allocatable                    ::  Column_Z
    character(:)        ,allocatable    ,dimension(:)   ::  Entries
  contains
    private                                                                                                     ! Setting private type-bound procedures
!     procedure   ,public   ::  Set_Command             =>  Set_Using_Command_From_String                   ! Sets the command associated to the "using" option from a character string
    procedure             ::  Set_Column_X            =>  Set_Using_Column_X                              ! Sets the column index corresponding the X data
    procedure             ::  Set_Column_Y            =>  Set_Using_Column_Y                              ! Sets the column index corresponding the Y data
    procedure             ::  Set_Column_Z            =>  Set_Using_Column_Z                              ! Sets the column index corresponding the Z data
    procedure             ::  Set_Entries             =>  Set_Using_Entries                               ! Sets the entries of the "using" option
    procedure   ,public   ::  Set_Command             =>  Set_Using_Command                               ! Sets the Using command
!     procedure             ::  Set_Using_Command_From_String
  End Type

  Interface             GPF_Plot_Using_Type
    Module Procedure    Construct_Plot_Using
  End Interface


#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
#define _THIS_ENTRIES_  This_Entries
    character(:)        ,allocatable    ,dimension(:)   ::  This_Entries
#else
#define _THIS_ENTRIES_  This%Entries
#endif

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Plot_Using( iElt, NLines, NAbsci, LineStyle, CB_Values, Debug ) result(This)

  implicit none

  type(GPF_Plot_Using_Type)                                             ::  This                            !< Passed-object dummy argument corresponding to the Plot-Using object
  integer                                               ,intent(in)     ::  iElt                            !< Index of current element
  integer                                               ,intent(in)     ::  NLines                          !< Number of lines to be plotted
  integer                                               ,intent(in)     ::  NAbsci                          !< Number of abscisse
  type(GPF_LineStyle_Type)                              ,intent(in)     ::  LineStyle                       !< LineStyle object
  real(rkp)             ,dimension(:)         ,optional ,intent(in)     ::  CB_Values                       !< ColorBox values (DIM=NLine)
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  character(*)                                              ,parameter  ::  Keyword='using'

  call This%Set_Debug( Debug )
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Using]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Using]: Calling This%Set_Column_X')")
  call This%Set_Column_X( iElt, NLines, NAbsci )                                                                ! Setting the column index corresponding the X data

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Using]: Calling This%Set_Column_Y')")
  call This%Set_Column_Y( iElt, LineStyle, NAbsci )                                                             ! Setting the column index corresponding the Y data

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Using]: Calling This%Set_Column_Z')")
  call This%Set_Column_Z( LineStyle, CB_Values )                                                                ! Setting the column index corresponding the Z data

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Using]: Calling This%Set_Entries')")
  call This%Set_Entries()                                                                                       ! Setting entries of the using option

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Using]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Plot_Using]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command

  if (This%i_Debug) then
    write(DbgUnit,"(14x,'[Construct_Plot_Using]: This%iX_Column = ',i0,3x,'This%Column_X = ',a)") This%iX_Column, This%Column_X
    write(DbgUnit,"(14x,'[Construct_Plot_Using]: This%iY_Column = ',i0,3x,'This%Column_Y = ',a)") This%iY_Column, This%Column_Y
    write(DbgUnit,"(14x,'[Construct_Plot_Using]: This%iZ_Column = ',i0,3x,'This%Column_Z = ',a)") This%iZ_Column, This%Column_Z
    write(DbgUnit,"(14x,'[Construct_Plot_Using]: This%Entries   = ',*(a,3x))")  _THIS_ENTRIES_
    write(DbgUnit,"(14x,'[Construct_Plot_Using]: This%Command   = ',a)")        This%Command
    write(DbgUnit,"(14x,'[Construct_Plot_Using]: Exiting')")
  end if

End Function
!
! ! **************************************************************************************************************
! ! **************************************************************************************************************
! ! *                                         PUBLIC PROCEDURES                                                  *
! ! **************************************************************************************************************
! ! **************************************************************************************************************
!
! Subroutine Set_Using_Command_From_String( This, Command )
!   implicit none
!   class(GPF_Plot_Using_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Using object
!   character(*)                                          ,intent(in)     ::  Command                         !< Command to be set
!   This%Command          =       Command
! End Subroutine

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_Using_Column_X( This, iElt, NLines, NAbsci )
  use GPF_Tools                 ,only:  Convert_To_String
  implicit none
  class(GPF_Plot_Using_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
  integer                                               ,intent(in)     ::  iElt                            !< Index of current element
  integer                                               ,intent(in)     ::  NLines                          !< Number of lines to be plotted
  integer                                               ,intent(in)     ::  NAbsci                          !< Number of abscisse
  This%iX_Column        =       1                                                                               ! Initializing the column index corresponding to the X abscisse
  if ( NLines == NAbsci ) This%iX_Column = iElt                                                                 ! Setting the column index corresponding to the X abscisse to current element index if there is multiple abscisse data

!   **************************************************************************************************************
! WORKAROUND for the GPF_Data_X2Y2_p_Type objects
  if ( NAbsci > 1) then
!     This%iX_Column      =       iElt + 1 - mod(iElt,NLines)
    This%iX_Column      =       2 * iElt - 1
!     iElt = 1 => This%iX_Column = 1
!     iElt = 2 => This%iX_Column = 3
  end if
!   **************************************************************************************************************
  This%Column_X =       Convert_To_String( This%iX_Column )                                                     ! Converting the colum index from integer to character string
End Subroutine

Subroutine Set_Using_Column_Y( This, iElt, LineStyle, NAbsci )
  use GPF_Tools                 ,only:  Convert_To_String
  implicit none
  class(GPF_Plot_Using_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
  integer                                               ,intent(in)     ::  iElt                            !< Index of current element
  type(GPF_LineStyle_Type)                              ,intent(in)     ::  LineStyle                       !< LineStyle object
  integer                                               ,intent(in)     ::  NAbsci                          !< Number of abscisse
  integer ::  idum
  idum = iElt
  if ( LineStyle%i_Multiple_Lines ) then                                                                   ! If a mulit-line Linestyle exist
    This%iY_Column      =       0                               ! Not applicable in that case
    This%Column_Y       =       "(column(i))"
  else
    This%iY_Column      =       This%iX_Column + LineStyle%iLine_Ini

!   **************************************************************************************************************
  ! WORKAROUND for the GPF_Data_X2Y2_p_Type objects
    if ( NAbsci > 1 ) then
      This%iY_Column      =       This%iX_Column + 1
  !     iElt = 1 => This%iX_Column = 1 => This%iY_Column = 2
  !     iElt = 2 => This%iX_Column = 3 => This%iY_Column = 4
    end if
!   **************************************************************************************************************

    This%Column_Y       =       Convert_To_String( This%iY_Column )                                                   ! Converting the integer into a character string array
  end if
End Subroutine

Subroutine Set_Using_Column_Z( This, LineStyle, CB_Values )
  implicit none
  class(GPF_Plot_Using_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
  type(GPF_LineStyle_Type)                              ,intent(in)     ::  LineStyle                       !< LineStyle object
  real(rkp)             ,dimension(:)         ,optional ,intent(in)     ::  CB_Values                       !< ColorBox values (DIM=NElements)
  This%Column_Z         =       ''
  This%iZ_Column        =       0
  if ( LineStyle%i_Multiple_Lines ) then
    if ( present(CB_Values) ) then                                                                                    ! If present optional input argument
! ! !     ?????????????? TODO
    else
      This%Column_Z     =       "(i-1)"
    end if
  end if
End Subroutine
!
! Subroutine Set_Using_Entries( This )
!   implicit none
!   class(GPF_Plot_Using_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
!   character(:)  ,dimension(:)   ,allocatable                            ::  Entries
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!  integer                                                                ::  Length, i
! #endif
!   if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
!   allocate ( character(0) :: _THIS_ENTRIES_(0) )
!
!   if ( len_trim(This%Column_X) /= 0 ) then
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     allocate( character(len(trim(This%Column_X))) :: Entries(1) )
! !     Entries = [ trim(This%Column_X) ]     ! GFORTRAN_COMPILER_BUG 5.3.1
!     Entries(1) = trim(This%Column_X)
! #else
!     allocate( Entries, source = [ trim(This%Column_X) ] )
! #endif
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
!     allocate( character(len(Entries)) :: _THIS_ENTRIES_(size(Entries)) )
!     do i = 1,size(Entries)
!       _THIS_ENTRIES_(i) = Entries(i)
!     end do
! #else
!     call move_alloc( Entries, _THIS_ENTRIES_ )  ! GFORTRAN_COMPILER_BUG 5.3.1
! #endif
!
!
!   end if
!
!   if ( len_trim(This%Column_Y) /= 0 ) then
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     Length  = max( len(_THIS_ENTRIES_), len(trim(This%Column_Y)) )
!     allocate( character(Length) :: Entries(size(_THIS_ENTRIES_)+1) )
!     Entries = [ _THIS_ENTRIES_, trim(This%Column_Y) ]
! #else
!     allocate( Entries, source = [ _THIS_ENTRIES_, trim(This%Column_Y) ] )
! #endif
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
!     allocate( character(len(Entries)) :: _THIS_ENTRIES_(size(Entries)) )
!     do i = 1,size(Entries)
!       _THIS_ENTRIES_(i) = Entries(i)
!     end do
! #else
!     call move_alloc( Entries, _THIS_ENTRIES_ )  ! GFORTRAN_COMPILER_BUG 5.3.1
! #endif
!   end if
!
!   if ( len_trim(This%Column_Z) /= 0 ) then
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     Length  = max( len(_THIS_ENTRIES_), len(trim(This%Column_Z)) )
!     allocate( character(Length) :: Entries(size(_THIS_ENTRIES_)+1) )
!     Entries = [ _THIS_ENTRIES_, trim(This%Column_Z) ]
! #else
!     allocate( Entries, source = [ _THIS_ENTRIES_, trim(This%Column_Z) ] )
! #endif
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
!     allocate( character(len(Entries)) :: _THIS_ENTRIES_(size(Entries)) )
!     do i = 1,size(Entries)
!       _THIS_ENTRIES_(i) = Entries(i)
!     end do
! #else
!     call move_alloc( Entries, _THIS_ENTRIES_ )  ! GFORTRAN_COMPILER_BUG 5.3.1
! #endif
!   end if
!
! End Subroutine
!



#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
Subroutine Set_Using_Entries( This )
  implicit none
  class(GPF_Plot_Using_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
!   character(:)  ,dimension(:)   ,allocatable                            ::  Entries
  character(1000)  ,dimension(:) ,allocatable                                       ::  Entries
 integer                                                                ::  Length, i

  if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
  allocate ( character(0) :: _THIS_ENTRIES_(0) )


  if ( len_trim(This%Column_X) /= 0 ) then
!     allocate( character(len(trim(This%Column_X))) :: Entries(1) )
    allocate( Entries(1) )
!     Entries = [ trim(This%Column_X) ]     ! GFORTRAN_COMPILER_BUG 5.3.1
    do i = 1,size(Entries)
      Entries(i) = trim(This%Column_X)
    end do
!     Entries(1) = trim(This%Column_X)
    if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
    Length = 0
    do i = 1,size(Entries)
      Length = max( Length , len(trim(Entries(i))) )
    end do
    allocate( character(Length) :: _THIS_ENTRIES_(size(Entries)) )
!     allocate( character(len(trim(Entries))) :: _THIS_ENTRIES_(size(Entries)) )
    do i = 1,size(Entries)
      _THIS_ENTRIES_(i) = trim(Entries(i))
    end do
    deallocate( Entries )
  end if

  if ( len_trim(This%Column_Y) /= 0 ) then
!     Length  = max( len(_THIS_ENTRIES_), len(trim(This%Column_Y)) )
!     allocate( character(Length) :: Entries(size(_THIS_ENTRIES_)+1) )
    allocate( Entries(size(_THIS_ENTRIES_)+1) )
    Entries = [ _THIS_ENTRIES_, trim(This%Column_Y) ]
    if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
    Length = 0
    do i = 1,size(Entries)
      Length = max( Length , len(trim(Entries(i))) )
    end do
    allocate( character(Length) :: _THIS_ENTRIES_(size(Entries)) )
    do i = 1,size(Entries)
      _THIS_ENTRIES_(i) = trim(Entries(i))
    end do
    deallocate( Entries )
  end if

  if ( len_trim(This%Column_Z) /= 0 ) then
    Length  = max( len(_THIS_ENTRIES_), len(trim(This%Column_Z)) )
!     allocate( character(Length) :: Entries(size(_THIS_ENTRIES_)+1) )
    allocate( Entries(size(_THIS_ENTRIES_)+1) )
    Entries = [ _THIS_ENTRIES_, trim(This%Column_Z) ]
    if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
    Length = 0
    do i = 1,size(Entries)
      Length = max( Length , len(trim(Entries(i))) )
    end do
    allocate( character(Length) :: _THIS_ENTRIES_(size(Entries)) )
!     allocate( character(len(trim(Entries))) :: _THIS_ENTRIES_(size(Entries)) )
    do i = 1,size(Entries)
      _THIS_ENTRIES_(i) = Entries(i)
    end do
    deallocate( Entries )
  end if


End Subroutine
#else
Subroutine Set_Using_Entries( This )
  implicit none
  class(GPF_Plot_Using_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(:)  ,dimension(:)   ,allocatable                            ::  Entries
  if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
  allocate ( character(0) :: _THIS_ENTRIES_(0) )
  if ( len_trim(This%Column_X) /= 0 ) then
    allocate( Entries, source = [ trim(This%Column_X) ] )
    call move_alloc( Entries, _THIS_ENTRIES_ )  ! GFORTRAN_COMPILER_BUG 5.3.1
  end if
  if ( len_trim(This%Column_Y) /= 0 ) then
    allocate( Entries, source = [ _THIS_ENTRIES_, trim(This%Column_Y) ] )
    call move_alloc( Entries, _THIS_ENTRIES_ )  ! GFORTRAN_COMPILER_BUG 5.3.1
  end if
  if ( len_trim(This%Column_Z) /= 0 ) then
    allocate( Entries, source = [ _THIS_ENTRIES_, trim(This%Column_Z) ] )
    call move_alloc( Entries, _THIS_ENTRIES_ )  ! GFORTRAN_COMPILER_BUG 5.3.1
  end if
End Subroutine
#endif

!
! Subroutine Set_Using_Entries( This )
!   class(GPF_Plot_Using_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument
!   character(:)  ,dimension(:)   ,allocatable                            ::  Entries
!   if ( allocated(_THIS_ENTRIES_) ) deallocate( _THIS_ENTRIES_ )
!   allocate ( character(0) :: _THIS_ENTRIES_(0) )
!
!   if ( len_trim(This%Column_X) /= 0 ) then
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     allocate( character(len(trim(This%Column_X))) :: Entries(1) )
!     Entries(1) = trim(This%Column_X)
! #else
!     allocate( Entries, source = [ trim(This%Column_X) ] )
! #endif
!     call move_alloc( Entries, _THIS_ENTRIES_ )  ! GFORTRAN_COMPILER_BUG 5.3.1
!   end if
!
!   if ( len_trim(This%Column_Y) /= 0 ) then
! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     Length  = max( len(_THIS_ENTRIES_), len(trim(This%Column_Y)) )
!     allocate( character(Length) :: Entries(size(_THIS_ENTRIES_)+1) )
!     Entries = [ _THIS_ENTRIES_, trim(This%Column_Y) ]
! ! #else
! !     allocate( Entries, source = [ _THIS_ENTRIES_, trim(This%Column_Y) ] )
! #endif
! ! !     call move_alloc( Entries, _THIS_ENTRIES_ )  ! GFORTRAN_COMPILER_BUG 5.3.1
!   end if
!
! !   if ( len_trim(This%Column_Z) /= 0 ) then
! ! #ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
! !     Length  = max( len(_THIS_ENTRIES_), len(trim(This%Column_Z)) )
! !     allocate( character(Length) :: Entries(size(_THIS_ENTRIES_)+1) )
! !     Entries = [ _THIS_ENTRIES_, trim(This%Column_Z) ]
! ! #else
! !     allocate( Entries, source = [ _THIS_ENTRIES_, trim(This%Column_Z) ] )
! ! #endif
! ! !     call move_alloc( Entries, _THIS_ENTRIES_ )  ! GFORTRAN_COMPILER_BUG 5.3.1
! !   end if
!
! End Subroutine

Subroutine Set_Using_Command( This )
  implicit none
  class(GPF_Plot_Using_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Using object
  integer                                                               ::  i                               ! Index of entries
  character(*)                                              ,parameter  ::  Separator=":"                   ! Seprator character between each value
  This%Command          =       ""                                                                              ! Initializing the command to an empty string
  if ( size(_THIS_ENTRIES_) > 0 ) then                                                                            !
    do i = 1,size(_THIS_ENTRIES_)                                                                                 ! Loop on all entries
      This%Command      =       This%Command // trim(_THIS_ENTRIES_(i))                                           ! Adding current entry to the command
      if ( i /= size(_THIS_ENTRIES_) ) This%Command = This%Command // Separator                                   ! Setting the entry separator if more entries follows
    end do                                                                                                      ! End loop on entries
  end if                                                                                                        !
  This%Command          =       This%Keyword // This%Command // " "                                             ! Adding the command prefix and a blank character at the end
End Subroutine

End Module