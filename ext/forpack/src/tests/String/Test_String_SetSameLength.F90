Module Test_String_SetSameLength

  use pfunit_mod
  use Logger_Class      ,only:  Logger
  use String_Library    ,only:  SetLength

  implicit none

  contains

@test
Subroutine Test_SetSameLength()

  use String_Library   ,only:  SetSameLength

  character(*)                                              ,parameter  ::  ProcName='SetSameLength'
  integer                                                   ,parameter  ::  NPad = 100
  character(:)  ,allocatable                                            ::  Description
  integer                                                               ::  i
  character(*)                                              ,parameter  ::  TestedProc = "SetSameLength"
  character(1)  ,parameter                                              ::  Pad = "*"
  character(:)  ,allocatable                                            ::  Found1_0d, Found2_0d, Expected1_0d, Expected2_0d
  character(:)  ,allocatable                                            ::  Found1_1d(:), Found2_1d(:), Expected1_1d(:), Expected2_1d(:)

  call Logger%Write( "Testing '"//ProcName//"' procedure", NewLine=.True. )

! ***************************
! Testing SetSameLength_C0_C0
! ***************************
  Block
    character(:)  ,allocatable                                          ::  Found1, Found2, Expected1, Expected2
! =============================================================================
    Description   =   "'SetSameLength' 0d-0d version, No padding, len(S1)>len(S2))"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = "123456"  ; Expected1 = "123456"
    Found2 = "abc"     ; Expected2 = "abc   "
    call SetSameLength(Found1,Found2)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( Expected1//"|", Found1//"|", Description//": Wrong string value for 1st arg. ")
    @assertEqual( Expected2//"|", Found2//"|", Description//": Wrong string value for 2nd arg. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 0d-0d version, No padding, len(S1)<len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = "abc"     ; Expected1 = "abc   "
    Found2 = "123456"  ; Expected2 = "123456"
    call SetSameLength(Found1,Found2)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( Expected1//"|", Found1//"|", Description//": Wrong string value for 1st arg. ")
    @assertEqual( Expected2//"|", Found2//"|", Description//": Wrong string value for 2nd arg. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 0d-0d version, No padding, len(S1)=len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = "abcdef"  ; Expected1 = "abcdef"
    Found2 = "123456"  ; Expected2 = "123456"
    call SetSameLength(Found1,Found2)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( Expected1//"|", Found1//"|", Description//": Wrong string value for 1st arg. ")
    @assertEqual( Expected2//"|", Found2//"|", Description//": Wrong string value for 2nd arg. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 0d-0d version, With padding, len(S1)>len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = "123456"  ; Expected1 = "123456"
    Found2 = "abc"     ; Expected2 = "abc***"
    call SetSameLength(Found1,Found2,Pad=Pad)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( Expected1//"|", Found1//"|", Description//": Wrong string value for 1st arg. ")
    @assertEqual( Expected2//"|", Found2//"|", Description//": Wrong string value for 2nd arg. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 0d-0d version, With padding, len(S1)<len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = "abc"     ; Expected1 = "abc***"
    Found2 = "123456"  ; Expected2 = "123456"
    call SetSameLength(Found1,Found2,Pad=Pad)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( Expected1//"|", Found1//"|", Description//": Wrong string value for 1st arg. ")
    @assertEqual( Expected2//"|", Found2//"|", Description//": Wrong string value for 2nd arg. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 0d-0d version, With padding, len(S1)=len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = "abcdef"  ; Expected1 = "abcdef"
    Found2 = "123456"  ; Expected2 = "123456"
    call SetSameLength(Found1,Found2,Pad=Pad)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( Expected1//"|", Found1//"|", Description//": Wrong string value for 1st arg. ")
    @assertEqual( Expected2//"|", Found2//"|", Description//": Wrong string value for 2nd arg. ")
    call Logger%Write( "[ok]" )
! =============================================================================
  End Block


! ***************************
! Testing SetSameLength_C1_C1
! ***************************
  Block
    character(:)  ,allocatable  ,dimension(:)                           ::  Found1, Found2, Expected1, Expected2
! =============================================================================
    Description   =   "'SetSameLength' 1d-1d version, No padding, len(S1)>len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = ["123456","abcdef","qwerty"]  ; Expected1 = ["123456","abcdef","qwerty"]
    Found2 = ["ABC","ijk"]                 ; Expected2 = ["ABC   ","ijk   "]
    call SetSameLength(Found1,Found2)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( size(Expected1), size(Found1), Description//": Wrong string size for 1st arg. ")
    do i =1,size(Expected1)
      @assertEqual( Expected1(i)//"|", Found1(i)//"|", Description//": Wrong string value for 1st arg. ")
    end do
    @assertEqual( size(Expected2), size(Found2), Description//": Wrong string size for 2nd arg. ")
    do i =1,size(Expected2)
      @assertEqual( Expected2(i)//"|", Found2(i)//"|", Description//": Wrong string value for 2nd arg. ")
    end do
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 1d-1d version, No padding, len(S1)<len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = ["ABC","ijk"]                 ; Expected1 = ["ABC   ","ijk   "]
    Found2 = ["123456","abcdef","qwerty"]  ; Expected2 = ["123456","abcdef","qwerty"]
    call SetSameLength(Found1,Found2)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( size(Expected1), size(Found1), Description//": Wrong string size for 1st arg. ")
    do i =1,size(Expected1)
      @assertEqual( Expected1(i)//"|", Found1(i)//"|", Description//": Wrong string value for 1st arg. ")
    end do
    @assertEqual( size(Expected2), size(Found2), Description//": Wrong string size for 2nd arg. ")
    do i =1,size(Expected2)
      @assertEqual( Expected2(i)//"|", Found2(i)//"|", Description//": Wrong string value for 2nd arg. ")
    end do
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 1d-1d version, No padding, len(S1)=len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = ["123456","abcdef","qwerty"]  ; Expected1 = ["123456","abcdef","qwerty"]
    Found2 = ["ABCDEF","ijklmn"]           ; Expected2 = ["ABCDEF","ijklmn"]
    call SetSameLength(Found1,Found2,Pad=Pad)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( size(Expected1), size(Found1), Description//": Wrong string size for 1st arg. ")
    do i =1,size(Expected1)
      @assertEqual( Expected1(i)//"|", Found1(i)//"|", Description//": Wrong string value for 1st arg. ")
    end do
    @assertEqual( size(Expected2), size(Found2), Description//": Wrong string size for 2nd arg. ")
    do i =1,size(Expected2)
      @assertEqual( Expected2(i)//"|", Found2(i)//"|", Description//": Wrong string value for 2nd arg. ")
    end do
    call Logger%Write( "[ok]" )
! =============================================================================
  End Block


! ***************************
! Testing SetSameLength_C0_C1
! ***************************
  Block
    character(:)  ,allocatable                                          ::  Found1, Expected1
    character(:)  ,allocatable  ,dimension(:)                           ::  Found2, Expected2
! =============================================================================
    Description   =   "'SetSameLength' 0d-1d version, No padding, len(S1)>len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 =  "123456"       ; Expected1 =  "123456"
    Found2 = ["ABC","ijk"]   ; Expected2 = ["ABC   ","ijk   "]
    call SetSameLength(Found1,Found2)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( Expected1//"|", Found1//"|", Description//": Wrong string value for 1st arg. ")
    @assertEqual( size(Expected2), size(Found2), Description//": Wrong string size for 2nd arg. ")
    do i =1,size(Expected2)
      @assertEqual( Expected2(i)//"|", Found2(i)//"|", Description//": Wrong string value for 2nd arg. ")
    end do
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 0d-1d version, No padding, len(S1)<len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 =  "ABC"                        ; Expected1 =  "ABC   "
    Found2 = ["123456","abcdef","qwerty"]  ; Expected2 = ["123456","abcdef","qwerty"]
    call SetSameLength(Found1,Found2)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( Expected1//"|", Found1//"|", Description//": Wrong string value for 1st arg. ")
    @assertEqual( size(Expected2), size(Found2), Description//": Wrong string size for 2nd arg. ")
    do i =1,size(Expected2)
      @assertEqual( Expected2(i)//"|", Found2(i)//"|", Description//": Wrong string value for 2nd arg. ")
    end do
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 0d-1d version, No padding, len(S1)=len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 =  "123456"           ; Expected1 =  "123456"
    Found2 = ["ABCDEF","ijklmn"] ; Expected2 = ["ABCDEF","ijklmn"]
    call SetSameLength(Found1,Found2,Pad=Pad)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( Expected1//"|", Found1//"|", Description//": Wrong string value for 1st arg. ")
    @assertEqual( size(Expected2), size(Found2), Description//": Wrong string size for 2nd arg. ")
    do i =1,size(Expected2)
      @assertEqual( Expected2(i)//"|", Found2(i)//"|", Description//": Wrong string value for 2nd arg. ")
    end do
    call Logger%Write( "[ok]" )
! =============================================================================
  End Block


! ***************************
! Testing SetSameLength_C1_C0
! ***************************
  Block
    character(:)  ,allocatable  ,dimension(:)                           ::  Found1, Expected1
    character(:)  ,allocatable                                          ::  Found2, Expected2
    Description   =   "'SetSameLength' 1d-0d version, No padding, len(S1)>len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = ["123456","abcdef","qwerty"]  ; Expected1 = ["123456","abcdef","qwerty"]
    Found2 =  "ABC"                        ; Expected2 =  "ABC   "
    call SetSameLength(Found1,Found2)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( size(Expected1), size(Found1), Description//": Wrong string size for 1st arg. ")
    do i =1,size(Expected1)
    @assertEqual( Expected1(i)//"|", Found1(i)//"|", Description//": Wrong string value for 1st arg. ")
    end do
    @assertEqual( Expected2//"|", Found2//"|", Description//": Wrong string value for 2nd arg. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 1d-0d version, No padding, len(S1)<len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = ["ABC","ijk"]  ; Expected1 = ["ABC   ","ijk   "]
    Found2 =  "123456"      ; Expected2 =  "123456"
    call SetSameLength(Found1,Found2)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( size(Expected1), size(Found1), Description//": Wrong string size for 1st arg. ")
    do i =1,size(Expected1)
    @assertEqual( Expected1(i)//"|", Found1(i)//"|", Description//": Wrong string value for 1st arg. ")
    end do
    @assertEqual( Expected2//"|", Found2//"|", Description//": Wrong string value for 2nd arg. ")
    call Logger%Write( "[ok]" )
! =============================================================================
    Description   =   "'SetSameLength' 1d-0d version, No padding, len(S1)=len(S2)"
    call Logger%Write( SetLength("-> "//Description,NPad,Pad="."), Advance=.False. )
    Found1 = ["123456","abcdef","qwerty"]  ; Expected1 = ["123456","abcdef","qwerty"]
    Found2 =  "ABCDEF"                     ; Expected2 =  "ABCDEF"
    call SetSameLength(Found1,Found2,Pad=Pad)
    @assertEqual( len(Found1), len(Found2), Description//": Wrong string length. ")
    @assertEqual( size(Expected1), size(Found1), Description//": Wrong string size for 1st arg. ")
    do i =1,size(Expected1)
    @assertEqual( Expected1(i)//"|", Found1(i)//"|", Description//": Wrong string value for 1st arg. ")
    end do
    @assertEqual( Expected2//"|", Found2//"|", Description//": Wrong string value for 2nd arg. ")
    call Logger%Write( "[ok]" )
! =============================================================================
  End Block

End Subroutine

End Module