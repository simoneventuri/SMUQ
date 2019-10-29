! The include file 'ConvertVar2dToChar1d_Inline.F90' is called from the file 'Logger_SubClass.F90'.
! Before calling the procedure, the following marco should be set:
! * _TypFormat        The format type

      j = 0
      do i = 1,NRowInp

        if ( SelectRowSubSet ) then
          if ( FirstSkippedItem(i,NRowInp,NRowMax) ) then
            j         =   j + 1
            String(j) =   "..." // SkippedItemsRange( NRowInp, NRowMax ) // "..."
            cycle
          else if ( SelectedItem(i,NRowInp,NRowMax) ) then
            j         =   j + 1
          else
            cycle
          end if
        else
          j   =   i
        end if

        if ( SelectColSubSet ) then  !
          call Convert_To_String( Variable(i,i1:i2), FirstColumns, VarFmt=VarFmt, _TypFormat ComFmt=ComFmt, Status=Local_Status )
          call Convert_To_String( Variable(i,i3:i4), LastColumns , VarFmt=VarFmt, _TypFormat ComFmt=ComFmt, Status=Local_Status )
          Columns   =   FirstColumns // SkippedColumnString // LastColumns
        else
          call Convert_To_String( Variable(i,:), Columns, VarFmt=VarFmt,   _TypFormat    ComFmt=ComFmt, Status=Local_Status )
        end if

        Length      =   len_trim(Columns)
        if ( Length <= len(String) ) then
          String(j) =   Columns
        else
          allocate( Character(Length) :: StringTmp(NEltOut) )
          if (j>1) StringTmp(1:j-1) = String(1:j-1)
          StringTmp(j)  =   Columns
          call move_alloc( StringTmp , String )
        end if
      end do

# undef  _TypFormat