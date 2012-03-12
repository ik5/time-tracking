Unit mdlinterrupt;

Interface

Uses Classes, SysUtils, tiVisitor, tiVisitorDB, tiObject, tiOPFManager, baseopf, mdlperson;

Type

  { TInterruptMemento }

  TInterruptMemento = Class(TBaseMemento)
  private
    FDuration: TDateTime;
    FPerson: TPerson;
    FTimeStamp: TDateTime;
  Public
    constructor CustomCreate(const ASubject: TtiObject); override;
    Property Duration : TDateTime Read FDuration;
    Property Person : TPerson Read FPerson;
    Property TimeStamp : TDateTime Read FTimeStamp;
  end;

  { TInterrupt }

  TInterrupt = Class(TBaseObject)
  Private
    FDuration : TDateTime;
    FPerson : TPerson;
    FTimeStamp : TDateTime;
    function GetDisplayDuration: String;
    function GetDisplayPerson: String;
    function GetDisplayTimeStamp: String;
    procedure SetDuration(const AValue: TDateTime);
    procedure SetPerson(const AValue: TPerson);
    procedure SetTimeStamp(const AValue: TDateTime);
  Protected
    Function GetCaption : String; override;
    function GetMemento: TBaseMemento; override;
    procedure SetMemento(const AValue: TBaseMemento);override;
  Public
    Function ObjectDescription : String; override;
    Function TimedOut(ATimeOut : Integer) : Boolean;
  Published
    Property DisplayTimeStamp : String Read GetDisplayTimeStamp;
    Property DisplayDuration : String Read GetDisplayDuration;
    Property DisplayPerson : String Read GetDisplayPerson;
    Property Duration : TDateTime Read FDuration Write SetDuration;
    Property Person : TPerson Read FPerson Write SetPerson;
    Property TimeStamp : TDateTime Read FTimeStamp Write SetTimeStamp;
  end;


  { TInterruptList }

  TInterruptList = Class(TBaseObjectList)
  Private
    FSince: TDateTime;
    function GetDisplaySince: String;
    Function GetObj(AIndex : Integer) : TInterrupt;
    Procedure SetObj(AIndex : Integer; AValue : TInterrupt);
    procedure SetSince(const AValue: TDateTime);
  Public
    Function Add(AnItem : TInterrupt) : Integer; reintroduce;
    Property Items[AIndex : Integer] : TInterrupt Read GetObj Write SetObj; Default;
  Published
    Property DisplaySince : String Read GetDisplaySince;
    Property Since : TDateTime Read FSince Write SetSince;
  end;

  { TPersonInterruptStat }

  TPersonInterruptStat = Class(TBaseObject)
  private
    FCount: Integer;
    FDuration: TDateTime;
    FPerson: TPerson;
    function GetDisplayDuration: String;
    function GetDisplayPerson: String;
    procedure SetCount(const AValue: Integer);
    procedure SetDuration(const AValue: TDateTime);
    procedure SetPerson(const AValue: TPerson);
  Published
    Property DisplayPerson : String Read GetDisplayPerson;
    Property DisplayDuration : String Read GetDisplayDuration;
    Property Person : TPerson Read FPerson Write SetPerson;
    Property Count : Integer Read FCount Write SetCount;
    Property Duration : TDateTime Read FDuration Write SetDuration;
  end;

  { TPersonInterruptStatList }

  TPersonInterruptStatList = Class(TBaseObjectList)
  Private
    Function GetObj(AIndex : Integer) : TPersonInterruptStat;
    Procedure SetObj(AIndex : Integer; AValue : TPersonInterruptStat);
  Public
    Function FindPerson(APerson : TPerson) : TPersonInterruptStat;
    Function AddFromInterrupt(AnItem : TInterrupt) : TPersonInterruptStat;
    Property Stats[AIndex : Integer] : TPersonInterruptStat Read GetObj Write SetObj; Default;
  end;

  { TDateInterruptStat }

  TDateInterruptStat = Class(TBaseObject)
  private
    FCount: Integer;
    FDate: TDateTime;
    FDuration: TDateTime;
    FPersons: TPersonInterruptStatList;
    function GetDisplayDate: String;
    function GetDisplayDuration: String;
    procedure SetCount(const AValue: Integer);
    procedure SetDate(const AValue: TDateTime);
    procedure SetDuration(const AValue: TDateTime);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
  Published
    Property DisplayDate : String Read GetDisplayDate;
    Property DisplayDuration : String Read GetDisplayDuration;
    Property Date : TDateTime Read FDate Write SetDate;
    Property Count : Integer Read FCount Write SetCount;
    Property Duration : TDateTime Read FDuration Write SetDuration;
    Property Persons : TPersonInterruptStatList Read FPersons;
  end;

  { TDateInterruptStatList }

  TDateInterruptStatList = Class(TBaseObjectList)
  Private
    function GetDisplayTotalDays: String;
    function GetDisplayTotalTime: String;
    Function GetObj(AIndex : Integer) : TDateInterruptStat;
    function GetTotalDays: Double;
    function GetTotalTime: TDateTime;
    Procedure SetObj(AIndex : Integer; AValue : TDateInterruptStat);
  Public
    Function FindDate(ADate : TDateTime) : TDateInterruptStat;
    Function AddFromInterrupt(AnItem : TInterrupt) : TDateInterruptStat;
    Property Stats[AIndex : Integer] : TDateInterruptStat Read GetObj Write SetObj; Default;
  Published
    Property DisplayTotalTime : String Read GetDisplayTotalTime;
    Property DisplayTotalDays : String Read GetDisplayTotalDays;
    Property TotalTime : TDateTime Read GetTotalTime;
    Property TotalDays : Double Read GetTotalDays;
  end;

Implementation

uses dateutils;

{ TDateInterruptStatList }

function TDateInterruptStatList.GetDisplayTotalDays: String;
begin
  Result:=FormatFloat('##0.00',TotalDays);
end;

function TDateInterruptStatList.GetDisplayTotalTime: String;
begin
  Result:=FormatDateTime('hh"h "nn"m "ss"s"',TotalTime);
end;

function TDateInterruptStatList.GetObj(AIndex: Integer): TDateInterruptStat;
begin
  Result:=Items[AIndex] as TDateInterruptStat;
end;

function TDateInterruptStatList.GetTotalDays: Double;
begin
  Result:=TotalTime * 24/8
end;

function TDateInterruptStatList.GetTotalTime: TDateTime;

Var
  I : integer;

begin
  Result:=0;
  For I:=0 to Count-1 do
    Result:=Result+GetObj(i).Duration;
end;

procedure TDateInterruptStatList.SetObj(AIndex: Integer;
  AValue: TDateInterruptStat);
begin
  Items[AIndex]:=AValue;
end;

function TDateInterruptStatList.FindDate(ADate: TDateTime
  ): TDateInterruptStat;

Var
  I : Integer;

begin
  I:=Count-1;
  While (I>=0) and (GetObj(I).Date<>ADate) do
    Dec(I);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetObj(I);
end;

function TDateInterruptStatList.AddFromInterrupt(AnItem: TInterrupt
  ): TDateInterruptStat;

Var
  D : TDateInterruptStat;
  dt : TDateTime;

begin
  Dt:=DateOf(AnItem.TimeStamp);
  D:=FindDate(Dt);
  If (D=Nil) then
    begin
    D:=TDateInterruptStat.Create;
    D.Date:=DateOf(Dt);
    Add(D);
    end;
  D.Count:=D.Count+1;
  D.Duration:=D.Duration+AnItem.Duration;
  Writeln('Adding to date stats : ', AnItem.Caption , '  Count =  ', D.Count ,' Duration = ', D.Duration, ' (',D.DisplayDuration,')');
  D.Persons.AddFromInterrupt(AnItem);
end;

{ TDateInterruptStat }

procedure TDateInterruptStat.SetCount(const AValue: Integer);
begin
  if FCount=AValue then exit;
  FCount:=AValue;
  ValueChange;
end;

function TDateInterruptStat.GetDisplayDate: String;
begin
  Result:=DateToStr(Date);
end;

function TDateInterruptStat.GetDisplayDuration: String;
begin
  Result:=TimeToStr(Duration);
end;

procedure TDateInterruptStat.SetDate(const AValue: TDateTime);
begin
  if FDate=AValue then exit;
  FDate:=AValue;
  ValueChange;
end;

procedure TDateInterruptStat.SetDuration(const AValue: TDateTime);
begin
  if FDuration=AValue then exit;
  FDuration:=AValue;
  ValueChange;
end;

constructor TDateInterruptStat.Create;
begin
  inherited Create;
  FPersons:=TPersonInterruptStatList.Create;
  FPersons.OwnsObjects:=True;
end;

destructor TDateInterruptStat.Destroy;
begin
  FreeAndNil(FPersons);
  inherited Destroy;
end;

{ TPersonInterruptStatList }

function TPersonInterruptStatList.GetObj(AIndex: Integer): TPersonInterruptStat;
begin
  Result:=Items[AIndex] as TPersonInterruptStat;
end;

procedure TPersonInterruptStatList.SetObj(AIndex: Integer; AValue: TPersonInterruptStat);
begin
  Items[AIndex]:=AValue;
end;

function TPersonInterruptStatList.FindPerson(APerson: TPerson
  ): TPersonInterruptStat;

Var
  I : Integer;

begin
  I:=Count-1;
  While (I>=0) and (GetObj(I).Person<>APerson) do
    Dec(I);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetObj(I);
end;

function TPersonInterruptStatList.AddFromInterrupt(AnItem: TInterrupt
  ): TPersonInterruptStat;

var
  S :  TPersonInterruptStat;

begin
  Result:=FindPerson(AnItem.Person);
  If (Result=Nil) then
    begin
    Result:=TPersonInterruptStat.Create;
    Result.Person:=AnItem.Person;
    Add(Result);
    end;
  Result.Count:=Result.Count+1;
  Result.Duration:=Result.Duration+AnItem.Duration;
end;

{ TPersonInterruptStat }

procedure TPersonInterruptStat.SetCount(const AValue: Integer);
begin
  if FCount=AValue then exit;
  FCount:=AValue;
  ValueChange;
end;

function TPersonInterruptStat.GetDisplayDuration: String;
begin
  Result:=TimeToStr(Duration);
end;

function TPersonInterruptStat.GetDisplayPerson: String;
begin
  If (person=Nil) then
    Result:='(unknown)'
  else
    Result:=Person.Caption;
end;

procedure TPersonInterruptStat.SetDuration(const AValue: TDateTime);
begin
  if FDuration=AValue then exit;
  FDuration:=AValue;
  ValueChange;
end;

procedure TPersonInterruptStat.SetPerson(const AValue: TPerson);
begin
  if FPerson=AValue then exit;
  FPerson:=AValue;
  ValueChange;
end;

{ TInterruptMemento }

constructor TInterruptMemento.CustomCreate(const ASubject: TtiObject);

Var
  I : TInterrupt;

begin
  inherited CustomCreate(ASubject);
  I:=ASubject as TInterrupt;
  FDuration:=I.Duration;
  FPerson:=I.Person;
  FTimeStamp:=I.TimeStamp;
end;

 { TInterrupt } 

procedure TInterrupt.SetDuration(const AValue: TDateTime);
begin
  if FDuration=AValue then exit;
  FDuration:=AValue;
  ValueChange;
end;

function TInterrupt.GetDisplayPerson: String;
begin
  If (FPerson=Nil) then
    Result:='(unknown)'
  else
    Result:=FPerson.Caption;
end;

function TInterrupt.GetDisplayDuration: String;
begin
  Result:=FormatDateTime('hh:nn:ss',Duration);
end;

function TInterrupt.GetDisplayTimeStamp: String;
begin
  Result:=FormatDateTime('yyyy:mm:dd - hh:nn:ss',TimeStamp);
end;

procedure TInterrupt.SetPerson(const AValue: TPerson);
begin
  if FPerson=AValue then exit;
  FPerson:=AValue;
  ValueChange;
end;

procedure TInterrupt.SetTimeStamp(const AValue: TDateTime);
begin
  if FTimeStamp=AValue then exit;
  FTimeStamp:=AValue;
  ValueChange;
end;

function TInterrupt.GetCaption: String;
begin
  Result:=FormatDateTime('dd"/"mm"/"yyyy hh:nn',TimeStamp);
  If Duration<>0 then
    Result:='('+Result+' - '+FormatDateTime('hh:nn',TimeStamp+Duration)+')';
  If Assigned(Person) then
    Result:=Result+ ' : '+Person.Caption
  else
    Result:=Result+ ' : (unknown)';
end;

function TInterrupt.GetMemento: TBaseMemento;
begin
  Result:=TInterruptMemento.CustomCreate(Self);
end;

procedure TInterrupt.SetMemento(const AValue: TBaseMemento);

Var
  I : TInterruptMemento;

begin
  inherited SetMemento(AValue);
  I:=AValue as TInterruptMemento;
  FDuration:=I.Duration;
  FPerson:=I.Person;
  FTimeStamp:=I.TimeStamp;
  ValueCHange;
end;

function TInterrupt.ObjectDescription: String;
begin
  Result:='interruption';
end;

function TInterrupt.TimedOut(ATimeOut: Integer): Boolean;
begin
  Result:=MinutesBetween(Now,TimeStamp)>ATimeOut;
end;


{ TInterruptList }

Function TInterruptList.GetObj(AIndex : Integer) : TInterrupt;

begin
  Result:=TInterrupt(Inherited Items[AIndex]);
end;

function TInterruptList.GetDisplaySince: String;
begin
  Result:=DateToStr(Since);
end;



Procedure TInterruptList.SetObj(AIndex : Integer; AValue : TInterrupt);

begin
  Inherited Items[AIndex]:=AValue;
end;

procedure TInterruptList.SetSince(const AValue: TDateTime);
begin
  if FSince=AValue then exit;
  FSince:=AValue;
  Clear;
  Read;
  NotifyObservers()
end;



Function TInterruptList.Add(AnItem : TInterrupt) : Integer;

begin
  Result:=inherited Add(AnItem);
end;

end.
