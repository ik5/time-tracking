unit baseopf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiVisitor, tiObject{, idlist};

Type

  TBaseMemento = class(TObject)
  protected
    FOID: string;
    FObjectState: TPerObjectState;
  public
    constructor CustomCreate(const ASubject: TtiObject); virtual;
    Property OID : String Read FOID;
    Property ObjectState : TPerObjectState Read FObjectState;
  end;

  { TBaseObject }

  TBaseObject = Class(TTiObject)
  protected
    // Override to create correct memento. Do not call inherited.
    function GetMemento: TBaseMemento; virtual;
    // Override to apply correct memento. Call inherited;
    procedure SetMemento(const AValue: TBaseMemento);virtual;
  Public
    Function ObjectDescription : String; virtual;
    Function CheckLength(Const AString,AName : String; Const MaxLen : Integer;Const AErrors: TtiObjectErrors) : Boolean;
    Function CheckRequired(Const AString,AName : String; Const AErrors: TtiObjectErrors) : Boolean;
    Function CheckRequired(Const ADate : TDateTime; Const AName : String; Const AErrors: TtiObjectErrors) : Boolean;
    Function CheckRequired(Const AObj : TObject; Const AName : String; Const AErrors: TtiObjectErrors) : Boolean;
    Function CheckRange(Const AValue,AMin,AMax : TDateTime; AName : String; Const AErrors: TtiObjectErrors) : Boolean;
    Procedure ValueChange;
    Procedure DisplayChange;
    procedure ReadPK(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure ReadThis(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure Read(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure Save(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure ReadPK; override;
    procedure ReadThis; override;
    procedure Read; override;
    procedure Save; override;
    // Caller is responsible for freeing the returned memento.
    Property Memento : TBaseMemento Read GetMemento Write SetMemento;
  end;


  TBaseObjectClass = Class of TBaseObject;
  
  { TBaseObjectList }

  TBaseObjectList = Class(TTiObjectList)
  Public
    Procedure ListChange;
    procedure ReadPK(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure ReadThis(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure Read(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure Save(const ADBConnectionName: string; APersistenceLayerName: string = ''); override;
    procedure ReadPK; override;
    procedure ReadThis; override;
    procedure Read; override;
    procedure Save; override;
  end;

  EBaseObject = Class(Exception);

  TVisitorType = (vtSave,vtRead,vtReadThis,vtReadPK);

Function VisitorGroupName(C : TClass; VisitorType : TVisitorType) : String;
Function VisitorGroupName(C : TTiObject; VisitorType : TVisitorType) : String;
Procedure RegisterVisitorForClass(AVisitorClass : TTiVisitorClass; ABaseClass : TTiObjectClass; VisitorType : TVisitorType);
Procedure RegisterVisitorsForClass(ABaseClass : TTiObjectClass; ACreate,AUpdate,ADelete,ARead,AReadThis,AReadPK : TTiVisitorClass);
Procedure RegisterVisitorsForClass(ABaseClass : TTiObjectClass; ACreate,AUpdate,ADelete,ARead,AReadThis : TTiVisitorClass);
Procedure RegisterVisitorsForClass(ABaseClass : TTiObjectClass; ACreate,AUpdate,ADelete,ARead : TTiVisitorClass);

Function IndexOfString(S : String; List : Array of string) : Integer;

const
  SaveAllGroup = 'SaveAll';

implementation

uses
  typinfo,
  tiOPFManager;
Resourcestring
  SErrorMaxLength = 'Maximum fieldlength for "%s" is %d';
  SErrorFieldValueRequired = 'A value for "%s" is required';
  SErrorFieldValueBetween = 'The value for field "%s" must be between %s and %s';


var
  RegisteredSaveVisitors : TList;

Function IndexOfString(S : String; List : Array of string) : Integer;

begin
  S:=UpperCase(S);
  Result:=High(List);
  While (Result>=0) and (S<>UpperCase(List[Result])) do
    Dec(Result);
end;


Function VisitorGroupName(C : TClass; VisitorType : TVisitorType) : String;

Const
  Suffix : Array [TVisitorType] of String
         = ('Save','Read','ReadThis','ReadPK');

begin
  Assert(C<>Nil,'No class type specified');
  Result:=C.ClassName;
  Result:=Result+Suffix[VisitorType];
end;

Function VisitorGroupName(C : TTiObject; VisitorType : TVisitorType) : String;

begin
  Assert(C<>Nil,'No class instance specified');
  Result:=VisitorGroupName(C.ClassType,VisitorType);
end;

Procedure RegisterVisitorForClass(AVisitorClass : TTiVisitorClass; ABaseClass : TTiObjectClass; VisitorType : TVisitorType);

begin
  with gTiOPFManager.VisitorManager do
    begin
    RegisterVisitor(VisitorGroupName(ABaseClass,VisitorType),AVisitorClass);
    if VisitorType = vtSave then
      begin
      // alleen erin steken als visitor nog niet in lijst zit.
      if RegisteredSaveVisitors.indexof (AVisitorClass) < 0 then
        begin
        RegisterVisitor(SaveAllGroup, AVisitorClass);
        RegisteredSaveVisitors.Add (AVisitorClass);
        end;
      end
    end;
end;

Procedure RegisterVisitorsForClass(ABaseClass : TTiObjectClass; ACreate,AUpdate,ADelete,ARead,AReadThis,AReadPK : TTiVisitorClass);

  Procedure MaybeRegVisitor(Vis : TTiVisitorClass; AType : TVisitorType);

  begin
    If (Vis<>Nil) then
      RegisterVisitorForClass(Vis,ABaseClass,AType);
  end;

begin
  MaybeRegVisitor(AReadPK,vtReadPK);
  MaybeRegVisitor(AReadThis,vtReadThis);
  MaybeRegVisitor(ARead,vtRead);
  MaybeRegVisitor(AUpdate,vtSave);
  If ADelete<>AUpdate then
    MaybeRegVisitor(ADelete,vtSave);
  If ADelete<>AUpdate then
    MaybeRegVisitor(ACreate,vtSave);
end;

Procedure RegisterVisitorsForClass(ABaseClass : TTiObjectClass; ACreate,AUpdate,ADelete,ARead,AReadThis : TTiVisitorClass);

begin
  RegisterVisitorsForClass(ABaseClass,ACreate,AUpdate,ADelete,ARead,AReadThis,Nil);
end;

Procedure RegisterVisitorsForClass(ABaseClass : TTiObjectClass; ACreate,AUpdate,ADelete,ARead : TTiVisitorClass);

begin
  RegisterVisitorsForClass(ABaseClass,ACreate,AUpdate,ADelete,ARead,Nil,Nil);
end;


{ TBaseObject }

function TBaseObject.GetMemento: TBaseMemento;
begin
  Result:=TBaseMemento.CustomCreate(Self);
end;

procedure TBaseObject.SetMemento(const AValue: TBaseMemento);
begin
  OID.AsString:=AValue.OID;
  ObjectState:=AValue.FObjectState;
end;

function TBaseObject.ObjectDescription: String;
begin
  Result:=ClassName;
end;

function TBaseObject.CheckLength(const AString, AName: String;
  const MaxLen: Integer; const AErrors: TtiObjectErrors): Boolean;
begin
  Result:=Length(AString)<=MaxLen;
  If Not Result then
    AErrors.AddError(AName,Format(SErrorMaxLength,[AName,MaxLen]));
end;

function TBaseObject.CheckRequired(const AString, AName: String;
  const AErrors: TtiObjectErrors): Boolean;
begin
  Result:=Length(AString)>0;
  If Not Result then
    AErrors.AddError(AName,Format(SErrorFieldValueRequired,[AName]));
end;

function TBaseObject.CheckRequired(const ADate: TDateTime; Const AName : String;
  const AErrors: TtiObjectErrors): Boolean;
begin
  Result:=(ADate<>0);
  If Not Result then
    AErrors.AddError(AName,Format(SErrorFieldValueRequired,[AName]));
end;

function TBaseObject.CheckRequired(const AObj: TObject; const AName: String;
  const AErrors: TtiObjectErrors): Boolean;
begin
  Result:=(AObj<>Nil);
  If Not Result then
    AErrors.AddError(AName,Format(SErrorFieldValueRequired,[AName]));
end;

function TBaseObject.CheckRange(const AValue, AMin, AMax: TDateTime;
  AName: String; const AErrors: TtiObjectErrors): Boolean;
begin
  Result:=(AValue>=AMin) and (AValue<=AMax);
  If Not Result then
    AErrors.AddError(AName,Format(SErrorFieldValueBetween,[AName, DateToStr(AMin), DateToStr(AMax)]));
end;

procedure TBaseObject.ValueChange;

Var
  S : String;

begin
  // Do something
  Case ObjectState of
    posEmpty :
      Dirty:=True;
  else
    S:=ObjectStateAsString;
    if Not (ObjectState in [posDelete,PosCreate]) then
      ObjectState:=posUpdate;
  end;
  DisplayChange;
end;

procedure TBaseObject.DisplayChange;
begin
  NotifyObservers;
end;

procedure TBaseObject.ReadPK(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  gTiOPFManager.VisitorManager.Execute(VisitorGroupName(Self,vtReadPK),Self,ADBConnectionName,APersistenceLayerName);
end;

procedure TBaseObject.ReadThis(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  gTiOPFManager.VisitorManager.Execute(VisitorGroupName(Self,vtReadThis),Self,ADBConnectionName,APersistenceLayerName);
end;

procedure TBaseObject.Read(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  BeginUpdate;
  gTiOPFManager.VisitorManager.Execute(VisitorGroupName(Self,vtRead),Self,ADBConnectionName,APersistenceLayerName);
  EndUpdate;
end;

procedure TBaseObject.Save(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  gTiOPFManager.VisitorManager.Execute(VisitorGroupName(Self,vtSave),Self,ADBConnectionName,APersistenceLayerName);
end;

procedure TBaseObject.ReadPK;
begin
  inherited ReadPK;
end;

procedure TBaseObject.ReadThis;
begin
  inherited ReadThis;
end;

procedure TBaseObject.Read;
begin
  inherited Read;
end;

procedure TBaseObject.Save;
begin
  inherited Save;
end;

{ TBaseObjectList }

procedure TBaseObjectList.ListChange;
begin
  NotifyObservers;
end;

procedure TBaseObjectList.ReadPK(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  beginupdate;
  try
    gTiOPFManager.VisitorManager.Execute(VisitorGroupName(Self,vtReadPK),Self,ADBConnectionName,APersistenceLayerName);
  finally
    endupdate;
  end;
end;

procedure TBaseObjectList.ReadThis(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  beginupdate;
  try
    gTiOPFManager.VisitorManager.Execute(VisitorGroupName(Self,vtReadThis),Self,ADBConnectionName,APersistenceLayerName);
  finally
    endupdate;
  end;
end;

procedure TBaseObjectList.Read(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  beginupdate;
  try
    gTiOPFManager.VisitorManager.Execute(VisitorGroupName(Self,vtRead),Self,ADBConnectionName,APersistenceLayerName);
  finally
    endupdate;
  end;
end;

procedure TBaseObjectList.Save(const ADBConnectionName: string;
  APersistenceLayerName: string);
begin
  gTiOPFManager.VisitorManager.Execute(VisitorGroupName(Self,vtSave),Self,ADBConnectionName,APersistenceLayerName);
end;

procedure TBaseObjectList.ReadPK;
begin
  inherited ReadPK;
end;

procedure TBaseObjectList.ReadThis;
begin
  inherited ReadThis;
end;

procedure TBaseObjectList.Read;
begin
  inherited Read;
end;

procedure TBaseObjectList.Save;
begin
  inherited Save;
end;

{ TBaseMemento }

constructor TBaseMemento.CustomCreate(const ASubject: TtiObject);
begin
  Create;
  FOID           := ASubject.OID.AsString;
  FObjectState   := ASubject.ObjectState;
end;


initialization

  RegisteredSaveVisitors := TList.Create;
  
finalization

  RegisteredSaveVisitors.Free;

end.

