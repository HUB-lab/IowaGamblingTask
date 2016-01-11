[WARNING]
WARNING: DO NOT EDIT THIS FILE DIRECTLY!
Use only E-Studio to edit this file.
Editing of this file from any other means is not supported
and may corrupt the experiment design specification.
Technical support will not be able to address any issue in
regards to this file format.

[Experiment]
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
_VersionPersist=6
Root="SessionProc"
Author="  (LSUHSC-S)"
Abstract=""
Notes=""
Creation=1034367696

[Device0]
_VersionPersist=1
OpenDevice=1
Width=640
Height=480
Bpp=16
Name="Display"
Class="Display"

[Device1]
_VersionPersist=1
OpenDevice=0
Channels=2
Samples=22050
Bps=16
Name="Sound"
Class="Sound"

[Device2]
_VersionPersist=1
OpenDevice=1
CollectionMode=1
EmulateDeviceName=""
CapsLock=0
NumLock=1
Name="Keyboard"
Class="Keyboard"

[Device3]
_VersionPersist=1
OpenDevice=1
CollectionMode=1
EmulateDeviceName=""
OpenMode=0
ShowCursor=0
Name="Mouse"
Class="Mouse"

[DataFile]
WarnBeforeOverwrite=1
WarnIfConvertFailed=1
RemoveERecoveryFile=0
ReceiveFeedbackDuringDataFileConversion=1
LogLevel(2).Name="Block"
LogLevel(3).Name="Trial"
LogLevel(4).Name="SubTrial"
LogLevel(5).Name="LogLevel5"
LogLevel(6).Name="LogLevel6"
LogLevel(7).Name="LogLevel7"
LogLevel(8).Name="LogLevel8"
LogLevel(9).Name="LogLevel9"
LogLevel(10).Name="LogLevel10"

[StartupInfo]
DisplaySummary=0
UseDefaults=1

[StartupInfo1]
Name="Subject"
Prompt="Please enter the Subject Number (1-32767, 0=No Data Logging):"
DataType=0
Enabled=1
PromptEnabled=1
Default="1"
Min=0
Max=32767
PrivateFlags=268435477
PrivateInfo=1

[StartupInfo2]
Name="Session"
Prompt="Please enter the Session Number (1-32767):"
DataType=0
Enabled=1
PromptEnabled=1
Default="1"
Min=1
Max=32767
PrivateFlags=268435477
PrivateInfo=1

[StartupInfo3]
Name="Group"
Prompt="Please enter Subject's Group (0-32767):"
DataType=0
Enabled=0
PromptEnabled=0
Default="1"
Min=0
Max=32767

[StartupInfo4]
Name="Name"
Prompt="Please enter Subject's Name:"
DataType=1
Enabled=0
PromptEnabled=0
Default=""
MaxLength=255

[StartupInfo5]
Name="Age"
Prompt="Please enter Subject's Age (0-150):"
DataType=0
Enabled=0
PromptEnabled=0
Default="0"
Min=0
Max=150

[StartupInfo6]
Name="Sex"
Prompt="Please enter Subject's Sex:"
DataType=2
Enabled=0
PromptEnabled=0
Default="male"
Choice(1).Value="male"
Choice(2).Value="female"

[StartupInfo7]
Name="Handedness"
Prompt="Enter Subject's Handedness:"
DataType=2
Enabled=0
PromptEnabled=0
Default="left"
Choice(1).Value="left"
Choice(2).Value="right"

[StartupInfo8]
Name="ResearcherID"
Prompt="Please enter Researcher's ID:"
DataType=0
Enabled=0
PromptEnabled=0
Default="1"
Min=0
Max=32767

[Object0]
_Version=65536
_ExtentX=2646
_ExtentY=1323
_StockProps=0
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
_ItemList="BlockList"
_VersionPersist=1
FlowLines(0).Count=1
FlowLines(0).FlowItem(0).Name="BlockList"
LogData=1
Name="SessionProc"
TypeName="Procedure"
Tag=""
Notes=""

[Object1]
_Version=65536
_ExtentX=2646
_ExtentY=1323
_StockProps=0
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
_ItemList="BlockProc"
_VersionPersist=1
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
Order=0
OrderBy=0
ResetEveryRun=0
HideLevelsWithZeroWeight=0
CycleDef=0
CycleValueSamples=0
CycleValueCondition=""
ExitDef=0
ExitValueCycles=1
ExitValueSamples=1
ExitValueSeconds=0
ExitValueCondition=""
LoadMethod=0
Filename=""
UserAttributes=0
Attributes("Weight").DefaultValue="1"
Attributes("Weight").Visible=1
Attributes("Weight").Width=75
Attributes("Weight").OrderIndex=0
Attributes("Nested").DefaultValue=""
Attributes("Nested").Visible=1
Attributes("Nested").Width=75
Attributes("Nested").OrderIndex=1
Attributes("Procedure").DefaultValue="TrialProc"
Attributes("Procedure").Visible=1
Attributes("Procedure").Width=95
Attributes("Procedure").OrderIndex=2
Levels=1
Levels(1).ValueString="1\t\tBlockProc\t"
Name="BlockList"
TypeName="List"
Tag=""
Notes=""

[Object2]
_Version=65536
_ExtentX=2646
_ExtentY=1323
_StockProps=0
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
_ItemList="Instructions;TrialList"
_VersionPersist=1
FlowLines(0).Count=2
FlowLines(0).FlowItem(0).Name="Instructions"
FlowLines(0).FlowItem(1).Name="TrialList"
LogData=1
Name="BlockProc"
TypeName="Procedure"
Tag=""
Notes=""

[Object3]
_Version=65536
_ExtentX=2646
_ExtentY=1323
_StockProps=0
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
Logging("OnsetDelay").Log=-1
Logging("OnsetTime").Log=-1
Logging("DurationError").Log=-1
Logging("PreRelease").Log=0
Logging("Duration").Log=0
Logging("StartTime").Log=0
Logging("OffsetTime").Log=0
Logging("FinishTime").Log=0
Logging("TimingMode").Log=0
Logging("CustomOnsetTime").Log=0
Logging("CustomOffsetTime").Log=0
Logging("ActionDelay").Log=0
Logging("ActionTime").Log=0
Logging("TargetOffsetTime").Log=0
Logging("TargetOnsetTime").Log=0
Logging("OffsetDelay").Log=0
Logging("RTTime").Log=-1
Logging("ACC").Log=-1
Logging("RT").Log=-1
Logging("RESP").Log=-1
Logging("CRESP").Log=-1
Logging("Tag").Log=0
Input(0)=!Data0
_VersionPersist=1
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
Duration="-1"
JumpLabel=""
TimingMode=0
PreRelease="0"
OnsetSync=1
OffsetSync=0
Filename="0.bmp"
Stretch="No"
MirrorUpDown="No"
MirrorLeftRight="No"
BackColor="white"
BackStyle="opaque"
BorderColor="black"
BorderWidth="0"
X="center"
Y="center"
Width="100%"
Height="100%"
XAlign="center"
YAlign="center"
AlignHorizontal="center"
AlignVertical="center"
ClearAfter="No"
SourceColorKey="black"
UseSourceColorKey="No"
Name="Tokens"
TypeName="ImageDisplay"
Tag=""
Notes=""

[Data0]
DeviceName="Keyboard"
DeviceClass="Keyboard"
AllowableInput="ricm"
CorrectInput="ricm"
TimeLimit=""
MaxCount="1"
InputAction=1
SyncOwnerDuration=1
Enabled=1
FlushInputBuffer="Yes"
TerminationInput=""
UserTag=""
ResponseMode="All"
ProcessBackspace="Yes"

[Object4]
_Version=65536
_ExtentX=2646
_ExtentY=1323
_StockProps=0
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
Logging("Circle").Log=-1
Logging("Crystal").Log=-1
Logging("Square").Log=-1
Logging("Diamond").Log=-1
_ItemList="TrialProc"
_VersionPersist=1
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
Order=0
OrderBy=0
ResetEveryRun=0
HideLevelsWithZeroWeight=0
CycleDef=0
CycleValueSamples=30
CycleValueCondition=""
ExitDef=0
ExitValueCycles=2
ExitValueSamples=1
ExitValueSeconds=0
ExitValueCondition=""
LoadMethod=0
Filename=""
UserAttributes=4
Attributes("Weight").DefaultValue="1"
Attributes("Weight").Visible=0
Attributes("Weight").Width=75
Attributes("Weight").OrderIndex=0
Attributes("Nested").DefaultValue=""
Attributes("Nested").Visible=0
Attributes("Nested").Width=75
Attributes("Nested").OrderIndex=1
Attributes("Procedure").DefaultValue="TrialProc"
Attributes("Procedure").Visible=1
Attributes("Procedure").Width=75
Attributes("Procedure").OrderIndex=2
Attributes(1).Name="Circle"
Attributes(1).DefaultValue="?"
Attributes(1).VarType=8
Attributes(1).VarTypeInternal=8
Attributes(1).Visible=1
Attributes(1).Width=75
Attributes(2).Name="Crystal"
Attributes(2).DefaultValue="?"
Attributes(2).VarType=8
Attributes(2).VarTypeInternal=8
Attributes(2).Visible=1
Attributes(2).Width=75
Attributes(3).Name="Square"
Attributes(3).DefaultValue="?"
Attributes(3).VarType=8
Attributes(3).VarTypeInternal=8
Attributes(3).Visible=1
Attributes(3).Width=75
Attributes(4).Name="Diamond"
Attributes(4).DefaultValue="?"
Attributes(4).VarType=8
Attributes(4).VarTypeInternal=8
Attributes(4).Visible=1
Attributes(4).Width=75
Levels=150
Levels(1).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(2).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(3).ValueString="1\t\tTrialProc\t150\t-310\t-125\t-465\t"
Levels(4).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(5).ValueString="1\t\tTrialProc\t150\t200\t-145\t-310\t"
Levels(6).ValueString="1\t\tTrialProc\t150\t-2150\t150\t200\t"
Levels(7).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(8).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(9).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(10).ValueString="1\t\tTrialProc\t-1000\t200\t-145\t200\t"
Levels(11).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(12).ValueString="1\t\tTrialProc\t150\t-2150\t150\t-465\t"
Levels(13).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(14).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(15).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(16).ValueString="1\t\tTrialProc\t150\t200\t-145\t200\t"
Levels(17).ValueString="1\t\tTrialProc\t150\t-2150\t150\t-465\t"
Levels(18).ValueString="1\t\tTrialProc\t150\t200\t-125\t-310\t"
Levels(19).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(20).ValueString="1\t\tTrialProc\t-950\t200\t150\t200\t"
Levels(21).ValueString="1\t\tTrialProc\t150\t200\t-145\t200\t"
Levels(22).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(23).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(24).ValueString="1\t\tTrialProc\t150\t-2150\t150\t200\t"
Levels(25).ValueString="1\t\tTrialProc\t150\t200\t-145\t-310\t"
Levels(26).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(27).ValueString="1\t\tTrialProc\t150\t-310\t150\t-465\t"
Levels(28).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(29).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(30).ValueString="1\t\tTrialProc\t-1000\t-2150\t-145\t200\t"
Levels(31).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(32).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(33).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(34).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(35).ValueString="1\t\tTrialProc\t150\t-310\t-145\t-310\t"
Levels(36).ValueString="1\t\tTrialProc\t150\t-310\t150\t200\t"
Levels(37).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(38).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(39).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(40).ValueString="1\t\tTrialProc\t-950\t200\t150\t200\t"
Levels(41).ValueString="1\t\tTrialProc\t150\t200\t-145\t-465\t"
Levels(42).ValueString="1\t\tTrialProc\t150\t-2150\t150\t200\t"
Levels(43).ValueString="1\t\tTrialProc\t150\t200\t-125\t-465\t"
Levels(44).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(45).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(46).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(47).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(48).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(49).ValueString="1\t\tTrialProc\t150\t-310\t-145\t-465\t"
Levels(50).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(51).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(52).ValueString="1\t\tTrialProc\t150\t-2150\t-125\t200\t"
Levels(53).ValueString="1\t\tTrialProc\t150\t200\t-145\t-465\t"
Levels(54).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(55).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(56).ValueString="1\t\tTrialProc\t-1000\t200\t-125\t200\t"
Levels(57).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(58).ValueString="1\t\tTrialProc\t150\t-2150\t150\t-465\t"
Levels(59).ValueString="1\t\tTrialProc\t150\t200\t-145\t200\t"
Levels(60).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(61).ValueString="1\t\tTrialProc\t150\t200\t-125\t-310\t"
Levels(62).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(63).ValueString="1\t\tTrialProc\t150\t-2150\t150\t-465\t"
Levels(64).ValueString="1\t\tTrialProc\t150\t200\t-145\t-310\t"
Levels(65).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(66).ValueString="1\t\tTrialProc\t-950\t200\t-125\t200\t"
Levels(67).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(68).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(69).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(70).ValueString="1\t\tTrialProc\t150\t-2150\t150\t200\t"
Levels(71).ValueString="1\t\tTrialProc\t150\t200\t-125\t-310\t"
Levels(72).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(73).ValueString="1\t\tTrialProc\t150\t-310\t-145\t-465\t"
Levels(74).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(75).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(76).ValueString="1\t\tTrialProc\t-1000\t-2150\t-125\t200\t"
Levels(77).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(78).ValueString="1\t\tTrialProc\t150\t200\t-145\t-465\t"
Levels(79).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(80).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(81).ValueString="1\t\tTrialProc\t150\t-310\t-125\t-310\t"
Levels(82).ValueString="1\t\tTrialProc\t150\t-310\t150\t200\t"
Levels(83).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(84).ValueString="1\t\tTrialProc\t150\t200\t-145\t200\t"
Levels(85).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(86).ValueString="1\t\tTrialProc\t-950\t200\t-125\t200\t"
Levels(87).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(88).ValueString="1\t\tTrialProc\t150\t-2150\t150\t200\t"
Levels(89).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(90).ValueString="1\t\tTrialProc\t150\t200\t-145\t-310\t"
Levels(91).ValueString="1\t\tTrialProc\t150\t-2150\t150\t200\t"
Levels(92).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(93).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(94).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(95).ValueString="1\t\tTrialProc\t-1000\t200\t-145\t200\t"
Levels(96).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(97).ValueString="1\t\tTrialProc\t150\t-2150\t150\t-465\t"
Levels(98).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(99).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(100).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(101).ValueString="1\t\tTrialProc\t150\t200\t-145\t200\t"
Levels(102).ValueString="1\t\tTrialProc\t150\t-2150\t150\t-465\t"
Levels(103).ValueString="1\t\tTrialProc\t150\t200\t-125\t-310\t"
Levels(104).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(105).ValueString="1\t\tTrialProc\t-950\t200\t150\t200\t"
Levels(106).ValueString="1\t\tTrialProc\t150\t200\t-145\t200\t"
Levels(107).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(108).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(109).ValueString="1\t\tTrialProc\t150\t-2150\t150\t200\t"
Levels(110).ValueString="1\t\tTrialProc\t150\t200\t-145\t-310\t"
Levels(111).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(112).ValueString="1\t\tTrialProc\t150\t-310\t150\t-465\t"
Levels(113).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(114).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(115).ValueString="1\t\tTrialProc\t-1000\t-2150\t-145\t200\t"
Levels(116).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(117).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(118).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(119).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(120).ValueString="1\t\tTrialProc\t150\t-310\t-145\t-310\t"
Levels(121).ValueString="1\t\tTrialProc\t150\t-310\t150\t200\t"
Levels(122).ValueString="1\t\tTrialProc\t150\t200\t150\t-465\t"
Levels(123).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(124).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(125).ValueString="1\t\tTrialProc\t-950\t200\t150\t200\t"
Levels(126).ValueString="1\t\tTrialProc\t150\t200\t-145\t-465\t"
Levels(127).ValueString="1\t\tTrialProc\t150\t-2150\t150\t200\t"
Levels(128).ValueString="1\t\tTrialProc\t150\t200\t-125\t-465\t"
Levels(129).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(130).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(131).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(132).ValueString="1\t\tTrialProc\t150\t200\t-125\t200\t"
Levels(133).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(134).ValueString="1\t\tTrialProc\t150\t-310\t-145\t-465\t"
Levels(135).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(136).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(137).ValueString="1\t\tTrialProc\t150\t-2150\t-125\t200\t"
Levels(138).ValueString="1\t\tTrialProc\t150\t200\t-145\t-465\t"
Levels(139).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(140).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(141).ValueString="1\t\tTrialProc\t-1000\t200\t-125\t200\t"
Levels(142).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(143).ValueString="1\t\tTrialProc\t150\t-2150\t150\t-465\t"
Levels(144).ValueString="1\t\tTrialProc\t150\t200\t-145\t200\t"
Levels(145).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Levels(146).ValueString="1\t\tTrialProc\t150\t200\t-125\t-310\t"
Levels(147).ValueString="1\t\tTrialProc\t150\t200\t150\t200\t"
Levels(148).ValueString="1\t\tTrialProc\t150\t-2150\t150\t-465\t"
Levels(149).ValueString="1\t\tTrialProc\t150\t200\t-145\t-310\t"
Levels(150).ValueString="1\t\tTrialProc\t150\t200\t150\t-310\t"
Name="TrialList"
TypeName="List"
Tag=""
Notes=""

[Object5]
_Version=65536
_ExtentX=2646
_ExtentY=1323
_StockProps=0
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
_ItemList="Tokens;InLine1;Feedback"
_VersionPersist=1
FlowLines(0).Count=3
FlowLines(0).FlowItem(0).Name="Tokens"
FlowLines(0).FlowItem(1).Name="InLine1"
FlowLines(0).FlowItem(2).Name="Feedback"
LogData=1
Name="TrialProc"
TypeName="Procedure"
Tag=""
Notes=""

[Object6]
_Version=65536
_ExtentX=2646
_ExtentY=1323
_StockProps=0
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
Logging("OnsetDelay").Log=0
Logging("OnsetTime").Log=0
Logging("DurationError").Log=0
Logging("PreRelease").Log=0
Logging("Duration").Log=0
Logging("StartTime").Log=0
Logging("OffsetTime").Log=0
Logging("FinishTime").Log=0
Logging("TimingMode").Log=0
Logging("CustomOnsetTime").Log=0
Logging("CustomOffsetTime").Log=0
Logging("ActionDelay").Log=0
Logging("ActionTime").Log=0
Logging("TargetOffsetTime").Log=0
Logging("TargetOnsetTime").Log=0
Logging("OffsetDelay").Log=0
Logging("RTTime").Log=0
Logging("ACC").Log=0
Logging("RT").Log=0
Logging("RESP").Log=0
Logging("CRESP").Log=0
Logging("Tag").Log=0
Input(0)=!Data1
_VersionPersist=1
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
Text="You will see four tokens displayed on the screen. Each token has both rewards and penalties associated with it. Choose a token by pressing its key, and you will be either rewarded or penalized. This is shown as two values on the screen (for example):\n\n100\n1250\n\nThe top number is the points received on the token just chosen. The bottom is the running total of how many points you have. You start with 0 points. You can choose any token, and can switch tokens at any time. The goal is to earn as many points as possible. This is not random; you can earn more by choosing the right token(s).\nPress the SPACE bar to begin."
ForeColor="black"
BackColor="white"
BackStyle="opaque"
BorderColor="black"
BorderWidth="0"
X="center"
Y="center"
Width="100%"
Height="100%"
XAlign="center"
YAlign="center"
AlignHorizontal="center"
AlignVertical="center"
FontName="Courier New"
FontSize="12"
FontBold="Yes"
FontItalic="No"
FontUnderline="No"
FontStrikeout="No"
WordWrap=1
ClearAfter="No"
Duration="-1"
JumpLabel=""
TimingMode=0
PreRelease="0"
OnsetSync=1
OffsetSync=0
Name="Instructions"
TypeName="TextDisplay"
Tag=""
Notes=""

[Data1]
DeviceName="Keyboard"
DeviceClass="Keyboard"
AllowableInput="{ANY}"
CorrectInput=""
TimeLimit=""
MaxCount="1"
InputAction=1
SyncOwnerDuration=1
Enabled=1
FlushInputBuffer="Yes"
TerminationInput=""
UserTag=""
ResponseMode="All"
ProcessBackspace="Yes"

[Object7]
_Version=65536
_ExtentX=2646
_ExtentY=1323
_StockProps=0
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
Logging("OnsetDelay").Log=0
Logging("OnsetTime").Log=0
Logging("DurationError").Log=0
Logging("PreRelease").Log=0
Logging("Duration").Log=0
Logging("StartTime").Log=0
Logging("OffsetTime").Log=0
Logging("FinishTime").Log=0
Logging("TimingMode").Log=0
Logging("CustomOnsetTime").Log=0
Logging("CustomOffsetTime").Log=0
Logging("ActionDelay").Log=0
Logging("ActionTime").Log=0
Logging("TargetOffsetTime").Log=0
Logging("TargetOnsetTime").Log=0
Logging("OffsetDelay").Log=0
Logging("RTTime").Log=0
Logging("ACC").Log=0
Logging("RT").Log=0
Logging("RESP").Log=0
Logging("CRESP").Log=0
Logging("Tag").Log=0
_VersionPersist=1
States("Correct").X="center"
States("Correct").Y="center"
States("Correct").Width="100%"
States("Correct").Height="100%"
States("Correct").XAlign="center"
States("Correct").YAlign="center"
States("Correct").BackColor="black"
States("Correct").BackStyle="opaque"
States("Correct").BorderColor="black"
States("Correct").BorderWidth="0"
States("Correct").ClearAfter="No"
States("Correct").Enabled="Yes"
States("Correct").ChildCount=2
States("Correct").Objects(1).Type="Text"
States("Correct").Objects(1).Text="[var]\r\n"
States("Correct").Objects(1).Width="21%"
States("Correct").Objects(1).Height="13%"
States("Correct").Objects(1).X="50%"
States("Correct").Objects(1).Y="44%"
States("Correct").Objects(1).AlignHorizontal="center"
States("Correct").Objects(1).AlignVertical="center"
States("Correct").Objects(1).FontName="Courier New"
States("Correct").Objects(1).FontSize="24"
States("Correct").Objects(1).FontBold="Yes"
States("Correct").Objects(1).FontUnderline="No"
States("Correct").Objects(1).FontStrikeout="No"
States("Correct").Objects(1).FontItalic="No"
States("Correct").Objects(1).ForeColor="white"
States("Correct").Objects(1).BackColor="black"
States("Correct").Objects(1).BackStyle="transparent"
States("Correct").Objects(1).BorderWidth="0"
States("Correct").Objects(1).BorderColor="black"
States("Correct").Objects(1).XAlign="center"
States("Correct").Objects(1).YAlign="center"
States("Correct").Objects(1).WordWrap=1
States("Correct").Objects(1).Name="Text1"
States("Correct").Objects(2).Type="Text"
States("Correct").Objects(2).Text="[totalsum]\r\n"
States("Correct").Objects(2).Width="49%"
States("Correct").Objects(2).Height="10%"
States("Correct").Objects(2).X="50%"
States("Correct").Objects(2).Y="61%"
States("Correct").Objects(2).AlignHorizontal="center"
States("Correct").Objects(2).AlignVertical="center"
States("Correct").Objects(2).FontName="Courier New"
States("Correct").Objects(2).FontSize="24"
States("Correct").Objects(2).FontBold="Yes"
States("Correct").Objects(2).FontUnderline="No"
States("Correct").Objects(2).FontStrikeout="No"
States("Correct").Objects(2).FontItalic="No"
States("Correct").Objects(2).ForeColor="white"
States("Correct").Objects(2).BackColor="black"
States("Correct").Objects(2).BackStyle="transparent"
States("Correct").Objects(2).BorderWidth="0"
States("Correct").Objects(2).BorderColor="black"
States("Correct").Objects(2).XAlign="center"
States("Correct").Objects(2).YAlign="center"
States("Correct").Objects(2).WordWrap=1
States("Correct").Objects(2).Name="Text3"
States("Incorrect").X="center"
States("Incorrect").Y="center"
States("Incorrect").Width="100%"
States("Incorrect").Height="100%"
States("Incorrect").XAlign="center"
States("Incorrect").YAlign="center"
States("Incorrect").BackColor="white"
States("Incorrect").BackStyle="opaque"
States("Incorrect").BorderColor="black"
States("Incorrect").BorderWidth="0"
States("Incorrect").ClearAfter="No"
States("Incorrect").Enabled="No"
States("Incorrect").ChildCount=0
States("NoResponse").X="center"
States("NoResponse").Y="center"
States("NoResponse").Width="100%"
States("NoResponse").Height="100%"
States("NoResponse").XAlign="center"
States("NoResponse").YAlign="center"
States("NoResponse").BackColor="white"
States("NoResponse").BackStyle="opaque"
States("NoResponse").BorderColor="black"
States("NoResponse").BorderWidth="0"
States("NoResponse").ClearAfter="No"
States("NoResponse").Enabled="No"
States("NoResponse").ChildCount=0
States("Pending").X="center"
States("Pending").Y="center"
States("Pending").Width="100%"
States("Pending").Height="100%"
States("Pending").XAlign="center"
States("Pending").YAlign="center"
States("Pending").BackColor="white"
States("Pending").BackStyle="opaque"
States("Pending").BorderColor="black"
States("Pending").BorderWidth="0"
States("Pending").ClearAfter="No"
States("Pending").Enabled="No"
States("Pending").ChildCount=0
UseScriptActivation=1
CollectACCStats=1
CollectNoRespACCStats=1
CollectCorrectRTStats=1
CollectIncorrectRTStats=1
ACCDivisor="1.0"
ACCFormat="Percent"
RTDivisor="1000.0"
RTFormat="###0.000"
CorrectRTDivisor="1000.0"
CorrectRTFormat="###0.000"
IncorrectRTDivisor="1000.0"
IncorrectRTFormat="###0.000"
Duration="1500"
JumpLabel=""
TimingMode=0
PreRelease="0"
OnsetSync=1
OffsetSync=0
InputObjectName="Tokens"
Name="Feedback"
TypeName="FeedbackDisplay"
Tag=""
Notes=""

[Object8]
_Version=131072
_ExtentX=2646
_ExtentY=1323
_StockProps=0
VersionMajor=1
VersionMinor=1
VersionInternal=3
VersionBuild=1
_VersionPersist=1
Code="\n\nif Tokens.RESP = \"r\" then\n\tvar = c.getAttrib (\"circle\")\n\ttotalsum = totalsum + var\nend if\nif Tokens.RESP = \"i\" then\n\tvar = c.getAttrib (\"crystal\")\n\ttotalsum = totalsum + var\nend if\nif Tokens.RESP = \"c\" then\n\tvar = c.getAttrib (\"square\")\n\ttotalsum = totalsum + var\nend if\nif Tokens.RESP = \"m\" then\n\tvar = c.getAttrib (\"diamond\")\n\ttotalsum = totalsum + var\nend if\n\n\tc.SetAttrib \"totalsum\", totalsum\n\tc.SetAttrib \"var\", var\n"
Name="InLine1"
TypeName="InLine"
Tag=""
Notes=""

[UserScript]
Dim totalsum As integer
Dim var As integer

