Unit mdltodo;

Interface

Uses Classes, SysUtils, tiVisitor, tiVisitorDB, tiObject, tiOPFManager, baseopf, mdlproject;

Type

  { TTodoMemento }

  TTodoMemento = Class(TBaseMemento)
  private
    FDone: Boolean;
    FProject: TProject;
    FRemark: AnsiString;
    FTitle: AnsiString;
  Public
    constructor CustomCreate(const ASubject: TtiObject); override;
    Property Done : Boolean Read FDone;
    Property Project : TProject Read FProject;
    Property Remark : AnsiString Read FRemark;
    Property Title : AnsiString Read FTitle;
  end;

  { TTodo }

  TTodo = Class(TBaseObject)
  Private
    FCreatedOn : TDateTime;
    FDone : Boolean;
    FProject : TProject;
    FRemark : AnsiString;
    FTitle : AnsiString;
    function GetDC: String;
    procedure SetCreatedOn(const AValue: TDateTime);
    procedure SetDone(const AValue: Boolean);
    procedure SetProject(const AValue: TProject);
    procedure SetRemark(const AValue: AnsiString);
    procedure SetTitle(const AValue: AnsiString);
  protected
    Function GetCaption : String; override;
    function GetMemento: TBaseMemento; override;
    procedure SetMemento(const AValue: TBaseMemento);override;
  Public
    Function ObjectDescription : String; override;
    Property CreatedOn : TDateTime Read FCreatedOn Write SetCreatedOn;
  Published
    Property DisplayCreatedOn : String Read GetDC;
    Property Done : Boolean Read FDone Write SetDone;
    Property Project : TProject Read FProject Write SetProject;
    Property Remark : AnsiString Read FRemark Write SetRemark;
    Property Title : AnsiString Read FTitle Write SetTitle;
  end;


  { TTodoList }

  TTodoList = Class(TBaseObjectList)
  Private
    Function GetObj(AIndex : Integer) : TTodo;
    Procedure SetObj(AIndex : Integer; AValue : TTodo);
  Public
    Function Add(AnItem : TTodo) : Integer; reintroduce;
    Property Items[AIndex : Integer] : TTodo Read GetObj Write SetObj; Default;
  end;


Implementation

{ TTodoMemento }

constructor TTodoMemento.CustomCreate(const ASubject: TtiObject);

Var
  T : TTodo;

begin
  inherited CustomCreate(ASubject);
  T:=ASubject as TTodo;
  FDone:=T.Done;
  FProject:=T.Project;
  FRemark:=T.Remark;
  FTitle:=T.Title;
end;

 { TTodo } 

procedure TTodo.SetCreatedOn(const AValue: TDateTime);
begin
  if FCreatedOn=AValue then exit;
  FCreatedOn:=AValue;
  ValueChange;
end;

function TTodo.GetDC: String;
begin
  Result:=FormatDateTime('yyyy/mm/dd hh:mm',CreatedOn);
end;

procedure TTodo.SetDone(const AValue: Boolean);
begin
  if FDone=AValue then exit;
  FDone:=AValue;
  ValueChange;
end;

procedure TTodo.SetProject(const AValue: TProject);
begin
  if FProject=AValue then exit;
  FProject:=AValue;
  ValueChange;
end;

procedure TTodo.SetRemark(const AValue: AnsiString);
begin
  if FRemark=AValue then exit;
  FRemark:=AValue;
  ValueChange;
end;

procedure TTodo.SetTitle(const AValue: AnsiString);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
  ValueChange;
end;

function TTodo.GetCaption: String;
begin
  Result:=Title;
end;

function TTodo.GetMemento: TBaseMemento;
begin
  Result:=TTodoMemento.CustomCreate(Self);
end;

procedure TTodo.SetMemento(const AValue: TBaseMemento);

Var
  T : TTodoMemento;

begin
  inherited SetMemento(AValue);
  T:=AValue as TTodoMemento;
  FDone:=T.Done;
  FProject:=T.Project;
  FRemark:=T.Remark;
  FTitle:=T.Title;
  ValueChange;
end;

function TTodo.ObjectDescription: String;
begin
  Result:='Todo item';
end;



{ TTodoList }

Function TTodoList.GetObj(AIndex : Integer) : TTodo;

begin
  Result:=TTodo(Inherited Items[AIndex]);
end;



Procedure TTodoList.SetObj(AIndex : Integer; AValue : TTodo);

begin
  Inherited Items[AIndex]:=AValue;
end;



Function TTodoList.Add(AnItem : TTodo) : Integer;

begin
  Result:=inherited Add(AnItem);
end;






end.
