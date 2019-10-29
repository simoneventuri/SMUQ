SubModule(String_Module) String_Decoration_SubModule

  implicit none

  contains

Module Procedure Frame
  integer                                                               ::  i
  integer                                                               ::  MaxLen                          ! Maximum length of the output character string
  integer                                                               ::  NLines                          ! Number of lines of the frame including the text
  integer                                                   ,parameter  ::  NSpace=2                        ! Number of character used for spacing before and after the text in the frame
  integer                                                   ,parameter  ::  NTab=2                      ! Number of character used for spacing before and after the text in the frame
  character(*)                                              ,parameter  ::  Char_Frame='#'                  ! Character used for the frame
  character(3)                                                          ::  Char_MaxLen                     ! String representing the number of characters in the output string
  character(3)                                                          ::  Char_NSpace                     ! String representing the number of blanc characters before and after the text message
  character(3)                                                          ::  Char_NTab
  character(:)  ,allocatable                                            ::  Str                          ! Text to be framed (same tha input text but with no blanks at the begining and at the end)
  character(:)  ,allocatable                                            ::  Fmt_Ext                         ! Format for the external frame, ie. the upper and lower lines of the frame
  character(:)  ,allocatable                                            ::  Fmt_Int                         ! Format for the internal frame, ie. the text inside the frame
  Str        =   trim(adjustl(InputString))
  NLines        =   3                                                                                       ! Setting the number of lines to 3
  MaxLen        =   len(Str) + 2 * (NSpace + 1)                                                         ! Number of characters in the output string including blanks and the framing characters
  allocate( character(MaxLen) :: OutputString(NLines) )                                                                ! Allocation of dimension and length of the output string
  write(Char_MaxLen,"(i3)") MaxLen                                                                              ! Interger-to-string conversion of the length of the output string
  write(Char_NSpace,"(i3)") NSpace                                                                              ! Interger-to-string conversion of the number of blanks characters
  write(Char_NTab,"(i3)") NTab
  Fmt_Ext       =   "(" // Char_MaxLen // "('"// Char_Frame // "'))"                                        ! Setting the format for the external frame, ie. the upper and lower lines of the frame
  Fmt_Int       =   "('"//Char_Frame//"',"//Char_NSpace//"x,'"//Str//"',"//Char_NSpace//"x,'"//Char_Frame//"')"  ! Setting the format for the internal frame, ie. the text inside the frame
  write(OutputString(1),Fmt_Ext)                                                                                       ! Writing the upper line of the frame
  do i = 2,Nlines-1                                                                                             ! Loop on all internal lines
    write(OutputString(i),Fmt_Int)                                                                                     ! Writing the middle line of the frame
  end do                                                                                                        ! End loop on
  write(OutputString(Nlines),Fmt_Ext)                                                                                  ! Writing the lower line of the frame
End Procedure

Module Procedure Justify
!       character(*)                                          ,intent(in)     ::  InputString(:)
!       integer                                               ,intent(in)     ::  Length
!       character(:)  ,allocatable                            ,intent(out)    ::  OutputString(:)
  use Utilities_Library  ,only:  AddElementToArray
  use Logger_Class         ,only:  Logger
  integer                                                               ::  i, k, LineLength
  logical                                                               ::  Append
  character(:)  ,allocatable                                            ::  Line, OldLine, NewLine, PreviousLine
  integer                             ,parameter  :: NMax=10
  integer                             ::iter
  call Logger%Entering("Justify" )
  call Logger%Write( "-> N = ", N )


  Append    =   present(M)

!   allocate( OutputString , source = InputString )

  PreviousLine  =  ""
!   M

  i   =   0
  do
    i = i + 1
    if ( i > size(InputString) ) exit

    Line  =   trim(InputString(i))
    if ( len_trim(PreviousLine) > 0 ) Line  =   PreviousLine // " " // Line

    LineLength    =   len_trim(Line)

    call Logger%Write( "-> i = ", i, "Line = |"//Line//"|" )
    call Logger%Write( "    -> PreviousLine = ", PreviousLine )




    if ( LineLength <= N ) then

!       if ( Append ) then
!         if ( LineLength > M ) then
! !           j = 0
!           do
!             i = i + 1
!             if ( i > size(InputString) ) exit
!           end do
!
!           call AddElementToArray( Line, OutputString )
!           cycle
!         end if
!       end if

      call Logger%Write( "    -> AddElementToArray" )
      call AddElementToArray( Line, OutputString )
      PreviousLine  = ""





    else
      OldLine     =   Line
      NewLine     =   ""
      iter  = 0
      do
        iter = iter + 1
        if ( iter > NMax ) exit
        k   =   index(OldLine," ",back=.True.)
        call Logger%Write( "    -> k = ", k, "OldLine = ", OldLine )
        if ( k==0 ) exit

        NewLine =   OldLine(k:) // trim(NewLine)
        OldLine =   OldLine(1:k-1)
!         call Logger%Write( "    -> OldLine = ", OldLine )
!         call Logger%Write( "    -> NewLine = ", NewLine )
        if ( k <= N ) exit
      end do

      NewLine   =   trim(adjustl(NewLine))
      call AddElementToArray( OldLine, OutputString )
      PreviousLine = NewLine

      call Logger%Write( "    -> Line    = |"//Line   //"|" )
      call Logger%Write( "    -> OldLine = |"//OldLine//"|" )
      call Logger%Write( "    -> NewLine = |"//NewLine//"|" )

    end if

  end do

  call Logger%Exiting

End Procedure
# if 0

!000000000111111111122222222223333333333444444444455555555556
123456789012345678901234567890123456789012345678901234567890
dks dis ikds io dsk dsi sdhdfsd w dujwd diow duid uopejs fd56f4 rt54 fefd

dks dis ikds io dsk dsi sdhdfsd w dujwd diow duid uopejs
fd56f4 rt54 fefd
"dks dis ikds io dsk dsi sdhdfsd js dks dkjwe djkhw ujdgws ujdgw dujwd"       , &
"dks dis ikds io d eu iedf ei wlie  uiwd u iodhw dsk dsi sdhdfsd"             , &
"dks dis ik sdhdfsd js dks dkjwe djkhw ujdgws ujdgw dujwd fwedf  flwe u jbs"  , &
"dks dis ikds io dsk dsi sdkhw uj ujdgw dujwd fje u ydfe uy e fjyw "          , &
"d uj ujdgw dujwd. "                                                          , &
"d uj ujdgw dujwdix die u dfuwe eui wef asddzqwzs eq"                           &
# endif
End SubModule
