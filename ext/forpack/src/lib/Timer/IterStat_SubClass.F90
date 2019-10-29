SubModule(IterStat_Class) IterStat_SubClass

  implicit none

  contains

Module Procedure InitializeIterStat
  integer   ,parameter  ::  DefaultEvalMax = 1e6
  This%N    =   N
  This%Freq =   1
  if ( present(Freq) ) This%Freq = Freq
  if ( present(EvalMax) ) then
    This%Freq   = max( This%Freq, ceiling(real(N,REAL64)/EvalMax) )  ! Setting the iteration frequency to the number which will lead to EvalMax evaluations
  else
    This%Freq   = max( This%Freq, ceiling(real(N,REAL64)/DefaultEvalMax) )  ! Setting the iteration frequency to the number which will lead to EvalMax evaluations
  end if
  call This%Timer%Start()
End Procedure

Module Procedure NeedIterStatUpdate
  Indicator =   mod(i,This%Freq) == 0
End Procedure

Module Procedure UpdateIterStat
  This%Percentage   =   100.0_REAL64*i / This%N
  This%CurrentTime  =   This%Timer%GetTime()
  This%TotalTime    =   This%CurrentTime * This%N / i
  This%LeftTime     =   This%TotalTime - This%CurrentTime
End Procedure

Module Procedure GetIterStatString
  use Utilities_Library         ,only:  AbsentOrTrue
  use Timer_Library             ,only:  Convert_Secondes_To_HMD
  use String_Library            ,only:  Convert_Ratio, Convert_To_String
  character(:)  ,allocatable                                            ::  IterInfo, PercInfo, TimeInfo
  if ( AbsentOrTrue(Update) ) call This%Update( i )
  IterInfo  =   Convert_Ratio(i,This%N)
  PercInfo  =   "("//Convert_To_String(This%Percentage,Fmt="f5.1")//"%)"
  TimeInfo  =   "Time left: " // Convert_Secondes_To_HMD(This%LeftTime)
  String    =   IterInfo // " " // PercInfo // " " // TimeInfo
End Procedure

Module Procedure GetIterStatInfo
  call This%Update( i )
  if ( Present(Percentage)  ) Percentage  =   This%Percentage
  if ( Present(LeftTime)    ) LeftTime    =   This%LeftTime
  if ( Present(String)      ) String      =   This%GetString(i,Update=.False.)
End Procedure

End SubModule