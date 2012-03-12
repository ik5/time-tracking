Unit mdlproject;

Interface

Uses Classes, SysUtils, tiVisitor, tiVisitorDB, tiObject, tiOPFManager, baseopf;

Type

  { TProjectMemento }

  TProjectMemento = Class(TBaseMemento)
  private
    FName: Ansistring;
  Public
    constructor CustomCreate(const ASubject: TtiObject); override;
    Property Name : Ansistring Read FName;
  end;

  { TProject }

  TProject = Class(TBaseObject)
  Private
    FName : AnsiString;
    procedure SetName(const AValue: AnsiString);
  Protected
    Function GetCaption : String; override;
    function GetMemento: TBaseMemento; override;
    procedure SetMemento(const AValue: TBaseMemento);override;
  Public
    Function ObjectDescription : String; override;
  Published
    Property Name : AnsiString Read FName Write SetName;
  end;


  { TProjectList }

  TProjectList = Class(TBaseObjectList)
  Private
    Function GetObj(AIndex : Integer) : TProject;
    Procedure SetObj(AIndex : Integer; AValue : TProject);
  Public
    Function Add(AnItem : TProject) : Integer; reintroduce;
    Property Items[AIndex : Integer] : TProject Read GetObj Write SetObj; Default;
  end;



Implementation

{ TProjectMemento }

constructor TProjectMemento.CustomCreate(const ASubject: TtiObject);
begin
  inherited CustomCreate(ASubject);
  FName:=(ASubject as TProject).Name;
end;

 { TProject } 

procedure TProject.SetName(const AValue: AnsiString);
begin
  if FName=AValue then exit;
  FName:=AValue;
  ValueChange;
end;

function TProject.GetCaption: String;
begin
  Result:=Name
end;

function TProject.GetMemento: TBaseMemento;
begin
  Result:=TProjectMemento.CustomCreate(Self);
end;

procedure TProject.SetMemento(const AValue: TBaseMemento);
begin
  inherited SetMemento(AValue);
  Name:=(AValue as TProjectMemento).Name;
end;

function TProject.ObjectDescription: String;
begin
  Result:='project';
end;


{ TProjectList }

Function TProjectList.GetObj(AIndex : Integer) : TProject;

begin
  Result:=TProject(Inherited Items[AIndex]);
end;



Procedure TProjectList.SetObj(AIndex : Integer; AValue : TProject);

begin
  Inherited Items[AIndex]:=AValue;
end;



Function TProjectList.Add(AnItem : TProject) : Integer;

begin
  Result:=inherited Add(AnItem);
end;





end.
