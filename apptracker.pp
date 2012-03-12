unit apptracker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiConstants, mdlproject, mdltodo, mdlperson, mdltask, mdlTimeTrack,
  mdlinterrupt, inifiles;

Type

   { TTrackerSettings }

   TTrackerSettings = Class(TPersistent)
   private
     FDatabaseName: String;
     FDatabasePWd: String;
     FDatabaseUser: String;
     FEndOfWorkingDay: TDateTime;
     FInterruptTimeOut: Integer;
     FLimitTracking: Boolean;
     FWeeksOfInterrupts: Integer;
   Public
     Constructor Create;
     Procedure LoadFromFile(Const AFileName : String);
     Procedure LoadFromIni(AIni : TCustomIniFile);virtual;
     Procedure SaveToFile(Const AFileName : String);
     Procedure SaveToIni(AIni : TCustomIniFile); virtual;
   Published
     Property DatabaseName : String Read FDatabaseName Write FDatabaseName;
     Property DatabaseUser : String Read FDatabaseUser Write FDatabaseUser;
     Property DatabasePwd : String Read FDatabasePWd Write FDatabasePwd;
     Property InterruptTimeOut : Integer Read FInterruptTimeOut Write FInterruptTimeout;
     Property EndOfWorkingDay : TDateTime Read FEndOfWorkingDay Write FEndOfWorkingDay;
     Property LimitTracking : Boolean Read FLimitTracking Write FLimitTracking;
     Property WeeksOfInterrupts : Integer Read FWeeksOfInterrupts Write FWeeksOfInterrupts;
   end;
   { TTrackerApp }

   TTrackerApp = Class(TComponent)
   private
     FCurrentTask: TTask;
     FCurrentTimer: TTimeTrack;
     FInterrupt: TInterrupt;
     FInterrupts: TInterruptList;
     FPersons: TPersonList;
     FProjects: TProjectList;
     FSettings: TTrackerSettings;
     FTasks: TTaskList;
     FToDos: TTodoList;
     procedure LoadGLobalLists;
   Public
     Constructor Create(AOwner : TComponent); override;
     Destructor Destroy; override;
     // Load settings from configuration file
     Procedure LoadSettings;
     // Save Settings to configuration file
     Procedure SaveSettings;
     // Connect to database (will load global lists)
     Procedure ConnectToDatabase;
     // Check whether database settings have changed
     Function NeedReconnect : boolean;
     // Check whether the current task has timed out.
     // This is so if the time is later then EndOfWorkingDay
     // and tracking started befor end of working day.
     Function CheckTaskTimeOut : Boolean;
     // Stop current interrupt. if ForceDuration  then the duration is forcedly set.
     function StopInterrupt(ForceDuration: Boolean): Integer;
     // Create new interrpt
     Function CreateInterrupt(APerson : TPerson) : TInterrupt;
     // Set ATask as current task, starts tracking it.
     Procedure SetCurrentTask(ATask : TTask);
     // End current task, stops tracking it.
     Procedure EndCurrentTask;
     // Start timing current task
     Procedure StartTiming;
     // Stop timing current task
     Procedure StopTiming;
   Published
     Property Settings : TTrackerSettings Read FSettings;
     Property Persons : TPersonList Read FPersons;
     Property Projects : TProjectList Read FProjects;
     Property Todos : TTodoList Read FToDos;
     Property Tasks : TTaskList Read FTasks;
     Property Interrupts : TInterruptList Read FInterrupts;
     Property CurrentInterrupt : TInterrupt REad FInterrupt;
     // timeout for interrupts, in minutes. Default 5
     Property CurrentTask : TTask Read FCurrentTask;
     Property CurrentTimer : TTimeTrack Read FCurrentTimer;
   end;

Function TrackerApp : TTrackerApp;

Const
  SPersistentLayerName = cTIPersistSqldbIB;

Const
  SConnection = 'Connection';
  KeyDB       = 'Database';
  KeyUser     = 'User';
  KeyPassword = 'Password';

  SConfig             = 'Config';
  KeyInterruptTimeOut = 'InterruptTimeOut';
  KeyLimitTracking    = 'LimitTracking';
  KeyEndofWorkingDay  = 'EndOfWorkingDay';
  KeyWeeksOfInterrupts = 'WeeksOfInterrupts';

implementation

uses strutils, dateutils, baseopf, tiOPFManager, tiQuerySqldbIB;


Var
  App : TTrackerApp;

function TrackerApp: TTrackerApp;
begin
  If App=Nil then
    begin
    App:=TTrackerApp.Create(Nil);
    end;
end;

{ TTrackerSettings }

constructor TTrackerSettings.Create;
begin
  InterruptTimeOut:=5;
  EndOfWorkingDay:=EncodeTime(18,0,0,0);
  LimitTracking:=True;
  FWeeksOfInterrupts:=2;
end;

procedure TTrackerSettings.LoadFromFile(const AFileName: String);

Var
  Ini : TmemIniFile;

begin
  Ini:=TMemIniFile.Create(AFileName);
  try
    LoadFromIni(Ini);
  finally
     Ini.Free;
  end;
end;

procedure TTrackerSettings.LoadFromIni(AIni: TCustomIniFile);

Var
  S : String;

begin
  With AIni do
    begin
    FDatabaseName:= ReadString(SConnection,KeyDB,'');
    FDatabaseUser:=ReadString(SConnection,KeyUser,'');
    S:=ReadString(SConnection,KeyPassword,'');
    If (S<>'') then
      FDatabasePWd:=XorDecode('FPC',S)
    else
      FDatabasePWd:=S;
    FInterruptTimeOut:=ReadInteger(SConfig,KeyInterruptTimeOut,FInterruptTimeOut);
    FLimitTracking:=ReadBool(SConfig,KeyLimitTracking,FLimitTracking);
    FEndOfWorkingDay:=ReadTime(SConfig,KeyEndOfWorkingDay,FEndOfWorkingDay);
    FWeeksOfInterrupts:=ReadInteger(SConfig,KeyWeeksOfInterrupts,FWeeksOfInterrupts);
    end;
end;

procedure TTrackerSettings.SaveToFile(const AFileName: String);

Var
  Ini : TmemIniFile;

begin
  Ini:=TMemIniFile.Create(AFileName);
  try
    SaveToIni(Ini);
    Ini.UpdateFile;
  finally
     Ini.Free;
  end;
end;

procedure TTrackerSettings.SaveToIni(AIni: TCustomIniFile);

begin
  With AIni do
    begin
    WriteString(SConnection,KeyDB,FDatabaseName);
    WriteString(SConnection,KeyUser,FDatabaseUser);
    WriteString(SConnection,KeyPassword,XorEncode('FPC',FDatabasePwd));
    WriteInteger(SConfig,KeyInterruptTimeOut,FInterruptTimeOut);
    WriteBool(SConfig,KeyLimitTracking,FLimitTracking);
    WriteTime(SConfig,KeyEndOfWorkingDay,FEndOfWorkingDay);
    WriteInteger(SConfig,KeyWeeksOfInterrupts,FWeeksOfInterrupts);
    end;
end;

{ TTrackerApp }

constructor TTrackerApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPersons:=TPersonList.Create;
  FPersons.OwnsObjects:=True;
  FProjects:=TProjectList.Create;
  FProjects.OwnsObjects:=True;
  FTodos:=TTodoList.Create;
  FTodos.OwnsObjects:=True;
  FTasks:=TTaskList.Create;
  FTasks.OwnsObjects:=True;
  FInterrupts:=TInterruptList.Create;
  FInterrupts.OwnsObjects:=True;
  FSettings:=TTrackerSettings.Create;
end;

destructor TTrackerApp.Destroy;
begin
  FreeAndNil(FPersons);
  FreeAndNil(FProjects);
  FreeAndNil(FTodos);
  FreeAndNil(FTasks);
  inherited Destroy;
end;


procedure TTrackerApp.LoadSettings;

begin
  Settings.LoadFromFile(GetAppConfigFile(False));
end;

procedure TTrackerApp.SaveSettings;
begin
  Settings.SaveToFile(GetAppConfigFile(False));
end;

procedure TTrackerApp.LoadGLobalLists;

  Procedure LL(L : TBaseObjectList; ASort : array of string; ADescending : Boolean = False );

  begin
    L.Clear;
    L.Read;
    if (High(ASort)<>-1) then
      L.SortByProps(ASort,not ADescending);
  end;

begin
  LL(FPersons,['FirstName','LastName']);
  LL(FProjects,['Name']);
  LL(FTodos,['CreatedOn'],True);
  LL(FTasks,['CreatedOn'],True);
  With FInterrupts do
    begin
    Clear;
    Since:=IncWeek(Now,- Settings.WeeksOfInterrupts); // Calls read
    end;
end;

procedure TTrackerApp.ConnectToDatabase;
begin
  gTIOPFManager.DefaultPersistenceLayerName:=SPersistentLayerName;
  gTIOPFManager.ConnectDatabase('default',Settings.DatabaseName, Settings.DatabaseUser, Settings.DatabasePwd, '', SPersistentLayerName);
  gTIOPFManager.DefaultDBConnectionName:='default';
  LoadGLobalLists;
end;

function TTrackerApp.NeedReconnect: boolean;
begin
  Result:=(Not Assigned(gTiOPFManager.DefaultDBConnectionPool)) or
          (gTiOPFManager.DefaultDBConnectionPool.DBConnectParams.DatabaseName<>Settings.DatabaseName);
end;

function TTrackerApp.CheckTaskTimeOut: Boolean;
begin
  Result:=Assigned(CurrentTimer) and (Time>Settings.EndOfWorkingDay);
  If Result then
    Result:=(TimeOf(CurrentTimer.Start)<Settings.EndOfWorkingDay);
end;

function TTrackerApp.StopInterrupt(ForceDuration : Boolean) : Integer;

begin
  If not Assigned(FInterrupt) then
    Result:=-1
  else
    begin
    If ForceDuration or Not Finterrupt.TimedOut(Settings.InterruptTimeOut) then
      begin
      FInterrupt.Duration:=Now-Finterrupt.TimeStamp;
      FInterrupt.Save;
      end;
    Result:=MinutesBetween(Now,Finterrupt.TimeStamp);
    FInterrupt:=Nil;
    end;
end;


function TTrackerApp.CreateInterrupt(APerson: TPerson): TInterrupt;
begin
  StopInterrupt(False);
  Result:=TInterrupt.Create;
  Result.TimeStamp:=Now;
  Result.Person:=APerson;
  Result.Save;
  FInterrupt:=Result;
  FInterrupts.Add(FInterrupt);
end;

procedure TTrackerApp.SetCurrentTask(ATask: TTask);
begin
  If Assigned(CurrentTask) And (CurrentTask<>ATask) then
    EndCurrentTask;
  FCurrentTask:=ATask;
  If Assigned(FCurrentTask) then
    StartTiming;
end;

procedure TTrackerApp.EndCurrentTask;
begin
  StopTiming;
  FCurrentTask:=Nil;
end;

procedure TTrackerApp.StartTiming;
begin
   If Assigned(FCurrentTimer) and (FCurrentTimer.Task<>CurrentTask) then
     StopTiming;
   If Not Assigned(CurrentTask) then
     exit;
   If CurrentTask.Status<>tsInProgress then
     begin
     CurrentTask.Status:=tsInProgress;
     CurrentTask.Save;
     end;
   FCurrentTimer:=TTimeTrack.Create;
   FCurrentTimer.Task:=CurrentTask;
   FCurrentTimer.StartTiming;
end;

procedure TTrackerApp.StopTiming;
begin
  If Assigned(FCurrentTimer) then
    begin
    FCurrentTimer.StopTiming;
    FreeAndNil(FCurrentTimer)
    end;
end;

Function GetTimeTrackerName : string;

begin
  result:='timetrack';
end;

initialization
  OnGetApplicationName:=@GetTimeTrackerName;
end.

