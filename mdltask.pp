Unit mdltask;

Interface

Uses Classes, SysUtils, tiVisitor, tiVisitorDB, tiObject, tiOPFManager,
     baseopf, mdlproject;

Type
  TTaskStatus = (tsNew, tsInProgress, tsDone);

  { TTaskMemento }

  TTaskMemento = Class(TBaseMemento)
  private
    FBugID: Integer;
    FCreatedOn: TDateTime;
    FDescription: AnsiString;
    FStatus : TTaskStatus;
    FProject: TProject;
    FTitle: AnsiString;
  Public
    constructor CustomCreate(const ASubject: TtiObject); override;
    Property CreatedOn : TDateTime Read FCreatedOn;
    Property Status : TTaskStatus Read FStatus;
    Property Project : TProject Read FProject;
    Property Description : AnsiString Read FDescription;
    Property Title : AnsiString Read FTitle;
    Property BugID : Integer Read FBugID;
  end;

  { TTask }

  TTask = Class(TBaseObject)
  Private
    FCreatedOn : TDateTime;
    FStatus : TTaskStatus;
    FProject : TProject;
    FDescription : AnsiString;
    FTitle : AnsiString;
    FBugID : Integer;
    function GetDC: String;
    function GetDS: String;
    procedure SetCreatedOn(const AValue: TDateTime);
    procedure SetStatus(const AValue: TTaskStatus);
    procedure SetProject(const AValue: TProject);
    procedure SetDescription(const AValue: AnsiString);
    procedure SetTitle(const AValue: AnsiString);
    procedure SetBugID(const AValue : integer);
  protected
    Function GetCaption : String; override;
    function GetMemento: TBaseMemento; override;
    procedure SetMemento(const AValue: TBaseMemento);override;
  Public
    Function ObjectDescription : String; override;
    Property CreatedOn : TDateTime Read FCreatedOn Write SetCreatedOn;
  Published
    Property DisplayCreatedOn : String Read GetDC;
    Property DisplayStatus : String Read GetDS;
    Property Status : TTaskStatus Read FStatus Write SetStatus;
    Property Project : TProject Read FProject Write SetProject;
    Property Description : AnsiString Read FDescription Write SetDescription;
    Property Title : AnsiString Read FTitle Write SetTitle;
    Property BugID : Integer Read FBugID Write SetBugId;
  end;


  { TTaskList }

  TTaskList = Class(TBaseObjectList)
  Private
    Function GetObj(AIndex : Integer) : TTask;
    Procedure SetObj(AIndex : Integer; AValue : TTask);
  Public
    Function Add(AnItem : TTask) : Integer; reintroduce;
    Property Items[AIndex : Integer] : TTask Read GetObj Write SetObj; Default;
  end;

Const
  TAskStatusNames : Array[TTaskStatus] of string
    = ('New', 'In progress', 'Done');


Implementation

{ TTaskMemento }

constructor TTaskMemento.CustomCreate(const ASubject: TtiObject);

Var
  T : TTask;
begin
  inherited CustomCreate(ASubject);
  T:=ASubject as TTask;
  FBugID:=T.BugID;
  FCreatedOn:=T.CreatedOn;
  FDescription:=T.Description;
  FStatus:=T.Status;
  FProject:=Project;
  FTitle:=T.Title;
end;

 { TTask } 

procedure TTask.SetCreatedOn(const AValue: TDateTime);
begin
  if FCreatedOn=AValue then exit;
  FCreatedOn:=AValue;
  ValueChange;
end;

procedure TTask.SetBugID(const AValue: Integer);
begin
  if FBugID=AValue then exit;
  FBugID:=AValue;
  ValueChange;
end;

function TTask.GetDC: String;
begin
  Result:=FormatDateTime('yyyy/mm/dd hh:mm',CreatedOn);
end;

function TTask.GetDS: String;
begin
  Result:=TaskStatusNames[FStatus];
end;

procedure TTask.SetStatus(const AValue: TTaskStatus);
begin
  if FStatus=AValue then exit;
  FStatus:=AValue;
  ValueChange;
end;

procedure TTask.SetProject(const AValue: TProject);
begin
  if FProject=AValue then exit;
  FProject:=AValue;
  ValueChange;
end;

procedure TTask.SetDescription(const AValue: AnsiString);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
  ValueChange;
end;

procedure TTask.SetTitle(const AValue: AnsiString);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
  ValueChange;
end;

function TTask.GetCaption: String;
begin
  Result:=Title;
end;

function TTask.GetMemento: TBaseMemento;
begin
  Result:=TTaskMemento.CustomCreate(Self);
end;

procedure TTask.SetMemento(const AValue: TBaseMemento);

Var
  T : TTaskMemento;

begin
  inherited SetMemento(AValue);
  T:=AValue as TTaskMemento;
  FBugID:=T.BugID;
  FCreatedOn:=T.CreatedOn;
  FDescription:=T.Description;
  FStatus:=T.Status;
  FProject:=Project;
  FTitle:=T.Title;
  ValueChange;
end;

function TTask.ObjectDescription: String;
begin
  Result:='Task';
end;



{ TTaskList }

Function TTaskList.GetObj(AIndex : Integer) : TTask;

begin
  Result:=TTask(Inherited Items[AIndex]);
end;



Procedure TTaskList.SetObj(AIndex : Integer; AValue : TTask);

begin
  Inherited Items[AIndex]:=AValue;
end;



Function TTaskList.Add(AnItem : TTask) : Integer;

begin
  Result:=inherited Add(AnItem);
end;






end.
