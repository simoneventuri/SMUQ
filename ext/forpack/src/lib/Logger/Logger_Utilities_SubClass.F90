SubModule(Logger_Class) Logger_Utilities_SubClass

  implicit none

  integer                                                   ,parameter  ::  NItemMaxDefault = 10

  contains

Module Procedure SelectedLine
  use Utilities_Library   ,only:  GetOptArgValue, SelectedItem
  integer                                                               ::  NItemMax_
  NItemMax_ =   GetOptArgValue(NItemMaxDefault,NItemMax)
  Indicator =   SelectedItem( i, NItemTot, NItemMax_ )
End Procedure

Module Procedure SkippedLine
  Indicator =   .Not. SelectedLine( i, NItemTot, NItemMax )
End Procedure

Module Procedure FirstSkippedLine
  use Utilities_Library   ,only:  GetOptArgValue, FirstSkippedItem
  integer                                                               ::  NItemMax_
  NItemMax_ =   GetOptArgValue(NItemMaxDefault,NItemMax)
  Indicator =   FirstSkippedItem( i, NItemTot, NItemMax_ )
End Procedure

Module Procedure SkippedLinesRange
  use Utilities_Library   ,only:  GetOptArgValue, SkippedItemsRange
  integer                                                               ::  NItemMax_
  NItemMax_ =   GetOptArgValue(NItemMaxDefault,NItemMax)
  String    =   SkippedItemsRange( NItemTot, NItemMax_ )
End Procedure

End SubModule