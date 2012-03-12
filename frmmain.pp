unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  mdlPerson, mdlProject, mdlTodo, mdlTask, Menus, ComCtrls, ExtCtrls, StdCtrls,
  tiBaseMediator, tiMediators, tiListMediators, baseopf, medmenu, mdlinterrupt,
  mdlTimeTrack, AppTracker;

type
  TObjectType = (otUnknown,otTodo,otProject,otTask,otPerson,otInterrupt);
  { TMainForm }

  TMainForm = class(TForm)
    LLTotalStatDays: TLabel;
    LLTotalStats: TLabel;
    LTotalStatDays: TLabel;
    LTotalStats: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MIIntSep: TMenuItem;
    MIStopInterrupt: TMenuItem;
    MIDisplayTime: TMenuItem;
    AShowTaskTiming: TAction;
    AEditConfig: TAction;
    ANewObject: TAction;
    ADeleteObject: TAction;
    AEditObject: TAction;
    AStopInterrupt: TAction;
    AStopCurrentTask: TAction;
    APersonInterrupt: TAction;
    AToggleTrack: TAction;
    ATaskFromTodo: TAction;
    AStartTask: TAction;
    ADeleteTask: TAction;
    AEditTask: TAction;
    ANewTask: TAction;
    ADeleteTodo: TAction;
    AEditTodo: TAction;
    ANewTodo: TAction;
    ADeleteProject: TAction;
    AEditProject: TAction;
    ANewProject: TAction;
    AEditPerson: TAction;
    ADeletePerson: TAction;
    ANewPerson: TAction;
    AHide: TAction;
    AQuit: TAction;
    ALTracker: TActionList;
    ILtracker: TImageList;
    LInterruptsSince: TLabel;
    LStatsSince: TLabel;
    LLInterruptsSince: TLabel;
    LLInterruptsSince1: TLabel;
    LVInterrupts: TListView;
    LVDateStats: TListView;
    LVPersonStats: TListView;
    LVTasks: TListView;
    LVTodo: TListView;
    LVPersons: TListView;
    LVProjects: TListView;
    MMTracker: TMainMenu;
    PMINewTask: TMenuItem;
    MIToggleTrack: TMenuItem;
    MIConfig: TMenuItem;
    MIShowTaskTiming: TMenuItem;
    MIDeleteTask: TMenuItem;
    MIModifyTask: TMenuItem;
    MINewTask: TMenuItem;
    MTasks: TMenuItem;
    PMIModifyTask: TMenuItem;
    PMIDeleteTask: TMenuItem;
    MenuItem5: TMenuItem;
    PMIltoggletrack: TMenuItem;
    PMITINewTodo: TMenuItem;
    PMIStopInterrupt: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PCStats: TPageControl;
    PInterruptsSince: TPanel;
    PMIDeletePerson: TMenuItem;
    PMIEditPerson: TMenuItem;
    PMINewPerson: TMenuItem;
    MISep2: TMenuItem;
    MIInterrupt: TMenuItem;
    PMInterrupt: TPopupMenu;
    PMITaskFromTodo: TMenuItem;
    MISep: TMenuItem;
    PMIEditTodo: TMenuItem;
    PMIDeleteToDo: TMenuItem;
    PMINewTodo: TMenuItem;
    PMINewProject: TMenuItem;
    PMIEditproject: TMenuItem;
    PMIDeleteProject: TMenuItem;
    MenuItem4: TMenuItem;
    MIStartTask: TMenuItem;
    PMIINterrupt: TMenuItem;
    MInterrupt: TMenuItem;
    PMITIQuit: TMenuItem;
    MIStopCurrent: TMenuItem;
    MIDeleteItem: TMenuItem;
    MIEditItem: TMenuItem;
    MINewItem: TMenuItem;
    MTodo: TMenuItem;
    MIDeletePerson: TMenuItem;
    MINewProject: TMenuItem;
    MIEditProject: TMenuItem;
    MIDeleteProject: TMenuItem;
    MPerson: TMenuItem;
    MProject: TMenuItem;
    MINewTodo: TMenuItem;
    MIEditToDo: TMenuItem;
    MIDeleteTodo: TMenuItem;
    MIStart: TMenuItem;
    MINewPerson: TMenuItem;
    MIEditperson: TMenuItem;
    MIEdit: TMenuItem;
    MIQuit: TMenuItem;
    MISep1: TMenuItem;
    MIHide: TMenuItem;
    MTracker: TMenuItem;
    PCtracker: TPageControl;
    PMPerson: TPopupMenu;
    PMProject: TPopupMenu;
    PMTasks: TPopupMenu;
    PMTodo: TPopupMenu;
    PMTray: TPopupMenu;
    StatusBar1: TStatusBar;
    TBInterrupt: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    TBAddObject: TToolButton;
    TBEditObject: TToolButton;
    TBDeleteObject: TToolButton;
    TSPersonStats: TTabSheet;
    TSDateStats: TTabSheet;
    TSStats: TTabSheet;
    TSInterrupts: TTabSheet;
    TITracker: TTrayIcon;
    TSTasks: TTabSheet;
    TBQuit: TToolButton;
    TBHide: TToolButton;
    TBStartStop: TToolButton;
    TSProjects: TTabSheet;
    TSPersons: TTabSheet;
    TSTodo: TTabSheet;
    TBTracker: TToolBar;
    TTimeOut: TTimer;
    procedure AEditConfigExecute(Sender: TObject);
    procedure ANewObjectExecute(Sender: TObject);
    procedure ANewObjectUpdate(Sender: TObject);
    procedure ADeleteObjectExecute(Sender: TObject);
    procedure ADeleteObjectUpdate(Sender: TObject);
    procedure ADeletePersonExecute(Sender: TObject);
    procedure ADeleteProjectExecute(Sender: TObject);
    procedure ADeleteTaskExecute(Sender: TObject);
    procedure ADeleteTodoExecute(Sender: TObject);
    procedure AEditObjectExecute(Sender: TObject);
    procedure AEditObjectUpdate(Sender: TObject);
    procedure AEditPersonExecute(Sender: TObject);
    procedure AEditProjectExecute(Sender: TObject);
    procedure AEditTaskExecute(Sender: TObject);
    procedure AEditTaskUpdate(Sender: TObject);
    procedure AEditTodoExecute(Sender: TObject);
    procedure ANewProjectExecute(Sender: TObject);
    procedure ANewTaskExecute(Sender: TObject);
    procedure ANewTodoExecute(Sender: TObject);
    procedure APersonInterruptExecute(Sender: TObject);
    procedure APersonInterruptUpdate(Sender: TObject);
    procedure AStartTaskExecute(Sender: TObject);
    procedure AStartTaskUpdate(Sender: TObject);
    procedure AStopCurrentTaskExecute(Sender: TObject);
    procedure AStopCurrentTaskUpdate(Sender: TObject);
    procedure AStopInterruptExecute(Sender: TObject);
    procedure AStopInterruptUpdate(Sender: TObject);
    procedure ATaskFromTodoExecute(Sender: TObject);
    procedure ATaskFromTodoUpdate(Sender: TObject);
    procedure AToggleTrackExecute(Sender: TObject);
    procedure AToggleTrackUpdate(Sender: TObject);
    procedure HavePerson(Sender: TObject);
    procedure AHideExecute(Sender: TObject);
    procedure ANewPersonExecute(Sender: TObject);
    procedure AQuitExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HaveProject(Sender: TObject);
    procedure HaveTask(Sender: TObject);
    procedure HaveToDo(Sender: TObject);
    procedure LVPersonsDblClick(Sender: TObject);
    procedure LVProjectsDblClick(Sender: TObject);
    procedure LVTasksDblClick(Sender: TObject);
    procedure LVTodoDblClick(Sender: TObject);
    procedure MINewTodoClick(Sender: TObject);
    procedure PCtrackerChange(Sender: TObject);
    procedure AShowTaskTimingExecute(Sender: TObject);
    procedure AShowTaskTimingUpdate(Sender: TObject);
    procedure TITrackerDblClick(Sender: TObject);
    procedure TTimeOutTimer(Sender: TObject);
  private
    { private declarations }
    FQuitting : Boolean;
    FTotalStatsMediator,
    FTotalStatsDaysMediator,
    FStatsSinceMediator,
    FInterruptsSinceMediator : TtiStaticTextMediatorView;
    FInterruptsMediator,
    FPersonsMediator,
    FProjectsMediator,
    FTaskMediator,
    FDateStatsMediator,
    FPersonStatsMediator,
    FTodosMediator : TtiListViewMediatorView ;
    FPopupInterruptMediator,
    FPMInterruptMediator,
    FInterruptMediator : TtiMenuMediatorView;
    FDateStats : TDateInterruptStatList;
    FPersonStats : TPersonInterruptStatList;
    procedure BuildStats;
    function CreateTask(AProject: TProject): TTask;
    procedure DeleteObject(AObject: TBaseObject; FromList: TBaseObjectList);
    function EditableObject: Boolean;
    procedure SetEditCaptions;
    procedure SetIconHint;
    procedure SetupMediators;
    Function SelectedPerson : TPerson;
    Function SelectedTodo : TTodo;
    Function SelectedTask : TTask;
    Function SelectedProject : TProject;
    Function SelectedInterrupt : TInterrupt;
    Procedure DoInterruptClick(Sender : TObject);
    procedure StartTracking(T: TTask);
    Procedure StopTracking(T: TTask);
    procedure ToggleTracking;
    Function CurrentObjectType : TObjectType;
    Function SelectedObject : TBaseObject;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

Const
  ObjectTypeNames : Array[TObjectType] of String
    = ('Unknown','Todo','Project','Task','Person','Interrupt');

implementation

uses visitors, frmConfig, frmPerson, frmproject, frmTodo, frmTask, frmtasktrack;

Resourcestring
  SCurrentTask  = 'Current task: "%s"';
  SCurrentTInterrupt = 'Current interrupt: "%s"';
  SStartTask    = 'Start task';
  SStartTaskTracking = 'Start time tracking for task:'+sLinebreak+'"%s" ?';
  SImportBug   = 'Import bug';
  SImportBugID = 'Enter ID of bug to import';
  rsANewProject = 'A new project';
  rsANewTodo = 'A new todo';
  rsFirstName = 'First name';
  rsLastName = 'Last name';
  SEditObject = 'Modify %s';
  SNewObject  = 'New %s';
  SDeleteObject = 'Delete %s';

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AQuitExecute(Sender: TObject);
begin
  FQuitting:=True;
  if Assigned(TrackerApp.CurrentTimer) then
    StopTracking(TrackerApp.CurrentTimer.Task);
  Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  If FQuitting then
    CloseAction:=caFree
  else
    CloseAction:=caHide;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LoadPersonsList:=TrackerApp.Persons;
  LoadProjectsList:=TrackerApp.Projects;
  FDateStats:=TDateInterruptStatList.Create;
  FDateStats.OwnsObjects:=True;
  FPersonStats:=TPersonInterruptStatList.Create;
  FPersonStats.OwnsObjects:=True;
  TrackerApp.LoadSettings;
  With gMediatorManager do
    begin
    RegisterMediator(TtiDynamicComboBoxMediatorView, TToDo, 'Project');
    RegisterMediator(TtiDynamicComboBoxMediatorView, TTask, 'Project');
    RegisterMediator(TtiComboBoxItemMediatorView, TTask, 'Status');
    RegisterMediator(TTiIntegerEditMediatorView, TTask, 'BugID');
    end;
  RegisterFallBackMediators;
  RegisterFallBackListMediators;
  SetupMediators;
  If (TrackerApp.Settings.DatabaseName<>'') then
    begin
    TrackerApp.ConnectToDatabase;
    TTimeOut.Enabled:=TrackerApp.Settings.LimitTracking;
    end
  else
    AEditConfig.Execute;
end;

procedure TMainForm.HaveProject(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(SelectedProject);
end;

procedure TMainForm.HaveTask(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(SelectedTask);
end;

procedure TMainForm.HaveToDo(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(SelectedTodo);
end;

procedure TMainForm.LVPersonsDblClick(Sender: TObject);
begin
  With AEditPerson do
    If Enabled then
      Execute
end;

procedure TMainForm.LVProjectsDblClick(Sender: TObject);
begin
  With AEditProject Do
    if Enabled then
      Execute;
end;

procedure TMainForm.LVTasksDblClick(Sender: TObject);
begin
  With AEditTask do
    if Enabled then
      Execute;
end;

procedure TMainForm.LVTodoDblClick(Sender: TObject);
begin
  With AEditToDo do
    If Enabled then
      Execute;
end;

procedure TMainForm.MINewTodoClick(Sender: TObject);
begin

end;

procedure TMainForm.PCtrackerChange(Sender: TObject);

begin
  If PCTracker.ActivePage=TSStats then
    BuildStats;
  SetEditCaptions;
end;

procedure TMainForm.AShowTaskTimingExecute(Sender: TObject);
begin
  SHowTaskTime(SelectedTask);
end;

procedure TMainForm.AShowTaskTimingUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(SelectedTask<>Nil);
end;

procedure TMainForm.SetEditCaptions;

Var
  S : String;

begin
  S:=ObjectTypeNames[CurrentObjectType];
  ANewObject.Caption:=Format(SNewObject,[S]);
  AEditObject.Caption:=Format(SEditObject,[S]);
  ADeleteObject.Caption:=Format(SDeleteObject,[S]);
end;

procedure TMainForm.BuildStats;

Var
  I : Integer;

begin
  FDateStatsMediator.Subject:=Nil;
  FPersonStatsMediator.Subject:=Nil;
  FDateStats.BeginUpdate();
  try
    FDateStats.Clear;
    FPersonStats.BeginUpdate();
    try
      FPersonStats.Clear;
      For I:=0 to TrackerApp.Interrupts.Count-1 do
        begin
        FDateStats.AddFromInterrupt(TrackerApp.Interrupts[i]);
        FPersonStats.AddFromInterrupt(TrackerApp.Interrupts[i]);
        end;
    finally
      FPersonStats.EndUpdate;
    end;
  finally
    FDateStats.EndUpdate;
  end;
  FDateStatsMediator.Subject:=FDateStats;
  FPersonStatsMediator.Subject:=FPersonStats;
end;

procedure TMainForm.TITrackerDblClick(Sender: TObject);
begin
  Show;
end;

procedure TMainForm.TTimeOutTimer(Sender: TObject);
begin
  If TrackerApp.CheckTaskTimeOut then
    StopTracking(TrackerApp.CurrentTask);
end;

procedure TMainForm.SetupMediators;

  Procedure FM(M : TtiListViewMediatorView);

  begin
    M.ShowDeleted:=False;
    M.Active:=true;
    if (M.Subject as TBaseObjectList).Count>0 then
      M.SelectedObject:=(M.Subject as TBaseObjectList).Items[0];
  end;

begin
  FPersonsMediator:=TtiListViewMediatorView.CreateCustom(TrackerApp.Persons,LVPersons,'FirstName(120,"First name",<);Lastname(120,"Last name",<)');
  FM(FPersonsMediator);
  FTodosMediator:=TtiListViewMediatorView.CreateCustom(TrackerApp.Todos,LVTodo,'Done;DisplayCreatedOn(160,"Created on",|);Title');
  FM(FTodosMediator);
  FTaskMediator:=TtiListViewMediatorView.CreateCustom(TrackerApp.Tasks,LVTasks,'DisplayStatus(100,"Status",|);DisplayCreatedOn(160,"Created on",|);BugID(70,"Bug ID",>);Title');
  FM(FTaskMediator);
  FProjectsMediator:=TtiListViewMediatorView.CreateCustom(TrackerApp.Projects,LVProjects,'Name');
  FM(FProjectsMediator);
  FInterruptsMediator:=TtiListViewMediatorView.CreateCustom(TrackerApp.Interrupts,LVInterrupts,'DisplayTimeStamp(180,"Timestamp",|);DisplayDuration(80,"Duration",|);DisplayPerson(200,"Person",<)');
  FM(FInterruptsMediator);
  FInterruptMediator:=TtiMenuMediatorView.CreateCustom(TrackerApp.Persons,MINterrupt,'',True,2);
  FInterruptMediator.OnClick:=@DoInterruptClick;
  FPMInterruptMediator:=TtiMenuMediatorView.CreateCustom(TrackerApp.Persons,PMInterrupt.Items,'',True );
  FPMInterruptMediator.OnClick:=@DoInterruptClick;
  FInterruptsSinceMediator:=TtiStaticTextMediatorView.CreateCustom(LInterruptsSince,TrackerApp.Interrupts,'DisplaySince','Caption');
  FInterruptsSinceMediator.Active:=True;
  FInterruptsSinceMediator.ObjectToGUI;
  FStatsSinceMediator:=TtiStaticTextMediatorView.CreateCustom(LStatsSince,TrackerApp.Interrupts,'DisplaySince','Caption');
  FStatsSinceMediator.Active:=True;
  FStatsSinceMediator.ObjectToGUI;
  FTotalStatsMediator:=TtiStaticTextMediatorView.CreateCustom(LTotalStats,FDateStats,'DisplayTotalTime','Caption');
  FTotalStatsMediator.Active:=True;
  FTotalStatsMediator.ObjectToGUI;
  FTotalStatsDaysMediator:=TtiStaticTextMediatorView.CreateCustom(LTotalStatDays,FDateStats,'DisplayTotalDays','Caption');
  FTotalStatsDaysMediator.Active:=True;
  FTotalStatsDaysMediator.ObjectToGUI;
  FDateStatsMediator:=TtiListViewMediatorView.CreateCustom(FDateStats,LVDateStats,'DisplayDate(80,"Date",|);Count;DisplayDuration(80,"Duration",|)');
  Fm(FDateStatsMediator);
  FPersonStatsMediator:=TtiListViewMediatorView.CreateCustom(FPersonStats,LVPersonStats,'DisplayPerson(200,"Person",|);Count;DisplayDuration(80,"Duration",|)');
  Fm(FPersonStatsMediator);
  FPopupInterruptMediator:=TtiMenuMediatorView.CreateCustom(TrackerApp.Persons,PMIInterrupt,'',True );
  FPopupInterruptMediator.OnClick:=@DoInterruptClick;
  SetEditCaptions;
end;

function TMainForm.SelectedPerson: TPerson;
begin
  Result:=TPerson(FPersonsMediator.SelectedObject);
end;

function TMainForm.SelectedTodo: TTodo;
begin
  Result:=TTodo(FTodosMediator.SelectedObject);
end;

function TMainForm.SelectedTask: TTask;
begin
  Result:=TTask(FTaskMediator.SelectedObject);
end;

function TMainForm.SelectedProject: TProject;
begin
  Result:=TProject(FProjectsMediator.SelectedObject);
end;

function TMainForm.SelectedInterrupt: TInterrupt;
begin
  Result:=TInterrupt(FInterruptsMediator.SelectedObject);
end;

procedure TMainForm.DoInterruptClick(Sender: TObject);
begin
  TrackerApp.CreateInterrupt(Sender as TPerson);
  SetIconHint;
end;

procedure TMainForm.AHideExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HavePerson(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(SelectedPerson);
end;

procedure TMainForm.AEditPersonExecute(Sender: TObject);
begin
  EditPerson(SelectedPerson);
end;

procedure TMainForm.AEditProjectExecute(Sender: TObject);
begin
  EditProject(SelectedProject);
end;

procedure TMainForm.AEditTaskExecute(Sender: TObject);
begin
  EditTask(SelectedTask);
end;

procedure TMainForm.AEditTaskUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(SelectedTask);
end;

procedure TMainForm.AEditTodoExecute(Sender: TObject);
begin
  EditToDo(SelectedToDo);
end;

procedure TMainForm.ANewProjectExecute(Sender: TObject);

Var
  P : TProject;

begin
  P:=TProject.Create;
  P.Name:=rsANewProject;
  If Not EditProject(P) then
    P.free
  else
    TrackerApp.Projects.Add(P);
end;

procedure TMainForm.ANewTaskExecute(Sender: TObject);

begin
  CreateTask(SelectedProject);
end;

Function TMainForm.CreateTask(AProject : TProject) : TTask;


begin
  Result:=TTask.Create;
  Result.Title:='A new task';
  Result.CreatedOn:=Now;
  Result.Project:=AProject;
  If Not EditTask(Result) then
    FreeAndNil(Result)
  else
    TrackerApp.Tasks.Add(Result);
end;

procedure TMainForm.ANewTodoExecute(Sender: TObject);

Var
  T : TTodo;

begin
  T:=TTodo.Create;
  T.Title:=rsANewTodo;
  T.CreatedOn:=Now;
  If Not EditTodo(T) then
    T.free
  else
    TrackerApp.Todos.Add(T);
end;

procedure TMainForm.APersonInterruptExecute(Sender: TObject);
begin
  TrackerApp.CreateInterrupt(SelectedPerson);
  SetIconHint;
end;

procedure TMainForm.APersonInterruptUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(SelectedPerson);
end;

procedure TMainForm.AStartTaskExecute(Sender: TObject);

Var
  T : TTask;

begin
  T:=CreateTask(SelectedProject);
  If Assigned(T) then
    StartTracking(T);
end;

procedure TMainForm.SetIconHint;

Var
  S : String;
begin
  S:='Time tracker';
  If (TrackerApp.CurrentTask<>Nil) then
    S:=S+SLineBreak+Format(SCurrentTask,[TrackerApp.CurrentTask.Title]);
  If (TrackerApp.CurrentInterrupt<>Nil) then
    S:=S+SLineBreak+Format(SCurrentTInterrupt,[TrackerApp.CurrentInterrupt.Caption]);
  TITracker.Hint:=S
end;

procedure TMainForm.StartTracking(T : TTask);

begin
  TrackerApp.SetCurrentTask(T);
  SetIconHint;
  AToggleTrack.ImageIndex:=4;
end;

procedure TMainForm.StopTracking(T : TTask);
begin
  TrackerApp.StopTiming;
  AToggleTrack.ImageIndex:=3;
end;

procedure TMainForm.AStartTaskUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=(TrackerApp.Projects.Count>0);
end;

procedure TMainForm.AStopCurrentTaskExecute(Sender: TObject);
begin
  StopTracking(TrackerApp.CurrentTask);
end;

procedure TMainForm.AStopCurrentTaskUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(TrackerApp.CurrentTimer);
end;

procedure TMainForm.AStopInterruptExecute(Sender: TObject);
begin
  TrackerApp.StopInterrupt(True);
  SetIconHint;
end;

procedure TMainForm.AStopInterruptUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(TrackerApp.CurrentInterrupt);
end;

procedure TMainForm.ATaskFromTodoExecute(Sender: TObject);

Var
  T : TTask;

begin
  T:=TTask.Create;
  T.Title:=SelectedToDo.Title;
  T.Description:=SelectedToDo.Remark;
  T.CreatedOn:=Now;
  if EditTask(T) then
    begin
    TrackerApp.Tasks.Add(T);
    If MessageDlg(SStartTask,Format(SStartTaskTracking,[T.Title]),mtInformation,[mbYes,mbNo],0)=mrYes then
      StartTracking(T);
    end
  else
    FreeAndNil(T);
end;

procedure TMainForm.ATaskFromTodoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(SelectedTodo<>Nil);
end;

procedure TMainForm.ToggleTracking;

Var
  T : TTimeTrack;

begin
  T:=TrackerApp.CurrentTimer;
  If Assigned(T) then
    StopTracking(T.Task)
  else if Assigned(SelectedTask) then
    StartTracking(SelectedTask);
end;

function TMainForm.CurrentObjectType : TObjectType;
begin
  Result:=otUnknown;
  If PCTracker.ActivePage=tsTOdo then
    Result:=otTodo
  else if PCTracker.ActivePage=tsprojects then
    Result:=otProject
  else if PCTracker.ActivePage=tstasks then
    Result:=otTask
  else if PCTracker.ActivePage=tsPersons then
    Result:=otPerson
  else if PCTracker.ActivePage=tsInterrupts then
    Result:=otInterrupt
end;

function TMainForm.SelectedObject: TBaseObject;

begin
  Case CurrentObjectType of
    otTodo    : Result:=SelectedTodo;
    otProject : Result:=SelectedProject;
    otPerson  : Result:=SelectedPerson;
    otTask    : Result:=SelectedTask;
    otInterrupt : Result:=SelectedInterrupt;
  else
    Result:=Nil;
  end;
end;

procedure TMainForm.AToggleTrackExecute(Sender: TObject);
begin
  ToggleTracking;
end;

procedure TMainForm.AToggleTrackUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(SelectedTask) or Assigned(TrackerApp.CurrentTask);
end;

procedure TMainForm.ADeletePersonExecute(Sender: TObject);

begin
  DeleteObject(SelectedPerson,TrackerApp.Persons);
end;

procedure TMainForm.ANewObjectUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=EditableObject;
end;

procedure TMainForm.ADeleteObjectExecute(Sender: TObject);
begin
  Case CurrentObjectType of
    otTodo    : ADeleteTodo.Execute;
    otTask    : ADeleteTask.Execute;
    otPerson  : ADeletePerson.Execute;
    otProject : ADeleteProject.Execute;
  end;
end;

procedure TMainForm.ADeleteObjectUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=EditableObject and (SelectedObject<>Nil);

end;

procedure TMainForm.ANewObjectExecute(Sender: TObject);
begin
  Case CurrentObjectType of
    otTodo    : ANewTodo.Execute;
    otTask    : ANewTask.Execute;
    otPerson  : ANewPerson.Execute;
    otProject : ANewProject.Execute;
  end;
end;

procedure TMainForm.AEditConfigExecute(Sender: TObject);
begin
  With TTimeTrackerConfigForm.Create(Self) do
    try
      if ShowModal=mrOK then
        begin
        if TrackerApp.NeedReconnect then
          TrackerApp.ConnectToDatabase;
        TTimeOut.Enabled:=TrackerApp.Settings.LimitTracking;
        end;
    finally
      Free;
    end;
end;

procedure TMainForm.DeleteObject(AObject : TBaseObject; FromList : TBaseObjectList);

begin
  if not Assigned(AObject) then
    exit;
  AObject.Deleted:=True;
  AObject.Save;
  If Assigned(FromList) then
     FromList.FreeDeleted;
end;

procedure TMainForm.ADeleteProjectExecute(Sender: TObject);
begin
  DeleteObject(SelectedProject,TrackerApp.Projects)
end;

procedure TMainForm.ADeleteTaskExecute(Sender: TObject);
begin
  DeleteObject(SelectedTask,TrackerApp.Tasks)
end;

procedure TMainForm.ADeleteTodoExecute(Sender: TObject);

begin
  DeleteObject(SelectedTodo,TrackerApp.Todos);
end;

procedure TMainForm.AEditObjectExecute(Sender: TObject);
begin
  Case CurrentObjectType of
    otTodo : AEditTodo.Execute;
    otTask : AEditTask.Execute;
    otPerson : AEditPerson.Execute;
    otProject : AEditProject.Execute;
  end;
end;

Function TMainForm.EditableObject : Boolean;

begin
  Result:=CurrentObjectType in [otTodo,otTask,otPerson,otProject]
end;

procedure TMainForm.AEditObjectUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=EditableObject and (SelectedObject<>Nil)
end;

procedure TMainForm.ANewPersonExecute(Sender: TObject);

Var
  P : TPerson;

begin
  P:=TPerson.Create;
  P.FirstName:=rsFirstName;
  P.Lastname:=rsLastName;
  If Not Editperson(P) then
    P.free
  else
    TrackerApp.Persons.Add(P);
end;



end.

