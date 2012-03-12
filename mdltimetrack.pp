Unit mdltimetrack;

Interface

Uses Classes, SysUtils, tiObject, mdltask, baseopf;

Type

  { TTimeTrackMemento }

  TTimeTrackMemento = Class(TBaseMemento)
  private
    FStart: TDateTime;
    FStop: TDateTime;
    FTask: TTask;
  Public
    constructor CustomCreate(const ASubject: TtiObject); override;
    Property Task : TTask Read FTask;
    Property Start : TDateTime Read FStart;
    Property Stop : TDateTime Read FStop;
  end;

  { TTimeTrack }

  TTimeTrack = Class(TBaseObject)
  Private
    FTask : TTask;
    FStart : TDateTime;
    FStop : TDateTime;
    function GetDSA: String;
    function GetDSO: String;
    procedure SetTask(const AValue: TTask);
    procedure SetStart(const AValue: TDateTime);
    procedure SetStop(const AValue: TDateTime);
  Protected
    function GetMemento: TBaseMemento; override;
    procedure SetMemento(const AValue: TBaseMemento);override;
  Public
     Procedure StartTiming;
     Procedure StopTiming;
  Published
    Property DisplayStart : String Read GetDSA;
    Property DisplayStop : String Read GetDSO;
    Property Task : TTask Read FTask Write SetTask;
    Property Start : TDateTime Read FStart Write SetStart;
    Property Stop : TDateTime Read FStop Write SetStop;
  end;


  { TBaseTimeTrackList }

  TBaseTimeTrackList = Class(TBaseObjectList)
  Private
    function GetDisplayTotalDays: String;
    function GetDisplayTotalTime: String;
    Function GetObj(AIndex : Integer) : TTimeTrack;
    function GetTotalDays: Double;
    function GetTotalTime: TDateTime;
    Procedure SetObj(AIndex : Integer; AValue : TTimeTrack);
  Public
    Function Add(AnItem : TTimeTrack) : Integer; reintroduce;
    Property Items[AIndex : Integer] : TTimeTrack Read GetObj Write SetObj; Default;
  Published
    Property DisplayTotalTime : String Read GetDisplayTotalTime;
    Property DisplayTotalDays : String Read GetDisplayTotalDays;
    Property TotalTime : TDateTime Read GetTotalTime;
    Property TotalDays : Double Read GetTotalDays;
  end;

  { TTimeTrackList }

  TTimeTrackList = Class(TBaseTimeTrackList)
  Private
    FSince: TDateTime;
    procedure SetSince(const AValue: TDateTime);
  Public
    Property Since : TDateTime Read FSince Write SetSince;
  end;

  { TTaskTimeTrackList }

  TTaskTimeTrackList  = Class(TBaseTimeTrackList)
  Private
    FTask: TTask;
    procedure SetTask(const AValue: TTask);
  Public
    Property Task : TTask Read FTask Write SetTask;
  end;

Implementation

uses dateutils;

{ TTaskTimeTrackList }

procedure TTaskTimeTrackList.SetTask(const AValue: TTask);
begin
  if FTask=AValue then exit;
  FTask:=AValue;
  Clear;
  Read;
end;

{ TTimeTrackMemento }

constructor TTimeTrackMemento.CustomCreate(const ASubject: TtiObject);

Var
  T : TTimeTrack;

begin
  inherited CustomCreate(ASubject);
  T:=ASubject as TTimeTrack;
  FStart:=T.Start;
  FStop:=T.Stop;
  FTask:=T.Task;
end;

{ TTimeTrack }

procedure TTimeTrack.SetTask(const AValue: TTask);
begin
  if FTask=AValue then exit;
  FTask:=AValue;
  ValueChange;
end;

function TTimeTrack.GetDSA: String;
begin
  Result:=FormatDateTime('dd/mm/yyyy hh:nn:ss',Start);
end;

function TTimeTrack.GetDSO: String;
begin
  Result:=FormatDateTime('dd/mm/yyyy hh:nn:ss',Stop);
end;

procedure TTimeTrack.SetStart(const AValue: TDateTime);
begin
  if FStart=AValue then exit;
  FStart:=AValue;
  ValueChange;
end;

procedure TTimeTrack.SetStop(const AValue: TDateTime);
begin
  if FStop=AValue then exit;
  FStop:=AValue;
  ValueChange;
end;

function TTimeTrack.GetMemento: TBaseMemento;
begin
  Result:=TTimeTrackMemento.CustomCreate(Self);
end;

procedure TTimeTrack.SetMemento(const AValue: TBaseMemento);

Var
  T : TTimeTrackMemento;

begin
  inherited SetMemento(AValue);
  T:=AValue as TTimeTrackMemento;
  FStart:=T.Start;
  FStop:=T.Stop;
  FTask:=T.Task;
  ValueChange;
end;

procedure TTimeTrack.StartTiming;
begin
  FStart:=Now;
end;

procedure TTimeTrack.StopTiming;
begin
  FStop:=Now;
  Save;
end;

{ TBaseTimeTrackList }

function TBaseTimeTrackList.GetDisplayTotalDays: String;
begin
  Result:=FormatFloat('#0.00',GetTotalDays);
end;

function TBaseTimeTrackList.GetDisplayTotalTime: String;

Var
  D : TDateTime;

begin
  D:=TotalTime;
  If Trunc(D)<>0 then
    Result:=Format('%d days ',[Trunc(D)]);
  Result:=Result+FormatDateTime('hh" h "nn" m."ss" sec."',TimeOf(D));
end;

Function TBaseTimeTrackList.GetObj(AIndex : Integer) : TTimeTrack;

begin
  Result:=TTimeTrack(Inherited Items[AIndex]);
end;

function TBaseTimeTrackList.GetTotalDays: Double;

begin
  Result:=GetTotalTime * 24 / 8;
end;

function TBaseTimeTrackList.GetTotalTime: TDateTime;

Var
  T : TTimeTrack;
  I : Integer;

begin
  Result:=0;
  For I:=0 to Count-1 do
    begin
    T:=GetObj(I);
    Result:=Result+(T.Stop-T.Start);
    end;
end;

Procedure TBaseTimeTrackList.SetObj(AIndex : Integer; AValue : TTimeTrack);

begin
  Inherited Items[AIndex]:=AValue;
end;

Function TBaseTimeTrackList.Add(AnItem : TTimeTrack) : Integer;

begin
  Result:=inherited Add(AnItem);
end;

{ TTimeTrackList }

procedure TTimeTrackList.SetSince(const AValue: TDateTime);
begin
  if FSince=AValue then exit;
  FSince:=AValue;
  Clear;
  Read;
end;







end.
