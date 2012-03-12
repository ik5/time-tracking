Unit mdlperson;

Interface

Uses Classes, SysUtils, tiVisitor, tiVisitorDB, tiObject, tiOPFManager, baseopf;

Type

  { TPerson }

  { TPersonMemento }

  TPersonMemento = Class(TBaseMemento)
  private
    FFirstname: String;
    FLastname: String;
  Public
    constructor CustomCreate(const ASubject: TtiObject); override;
    Property FirstName : String Read FFirstname;
    Property Lastname : String Read FLastname;
  end;

  TPerson = Class(TBaseObject)
  Private
    FFirstName : AnsiString;
    FLastName : AnsiString;
    procedure SetFirstName(const AValue: AnsiString);
    procedure SetLastName(const AValue: AnsiString);
  protected
    Function GetCaption : string; override;
    function GetMemento: TBaseMemento; override;
    procedure SetMemento(const AValue: TBaseMemento);override;
  Public
    Function ObjectDescription : String; override;
  Published
    Property FirstName : AnsiString Read FFirstName Write SetFirstName;
    Property LastName : AnsiString Read FLastName Write SetLastName;
  end;

  { TPersonList }

  TPersonList = Class(TBaseObjectList)
  Private
    Function GetObj(AIndex : Integer) : TPerson;
    Procedure SetObj(AIndex : Integer; AValue : TPerson);
  Public
    Function Add(AnItem : TPerson) : Integer; reintroduce;
    Property Items[AIndex : Integer] : TPerson Read GetObj Write SetObj; Default;
  end;


Implementation

{ TPersonMemento }

constructor TPersonMemento.CustomCreate(const ASubject: TtiObject);

Var
  P : TPerson;

begin
  Inherited;
  P:=ASubject as TPerson;
  FFirstName:=P.FirstName;
  FLastName:=P.LastName;
end;

 { TPerson } 


procedure TPerson.SetFirstName(const AValue: AnsiString);
begin
  if FFirstName=AValue then exit;
  FFirstName:=AValue;
  ValueChange;
end;

procedure TPerson.SetLastName(const AValue: AnsiString);
begin
  if FLastName=AValue then exit;
  FLastName:=AValue;
  ValueChange;
end;

function TPerson.GetCaption: string;
begin
  Result:=FirstName+' '+LastName;
end;

function TPerson.GetMemento: TBaseMemento;
begin
  Result:=TPersonMemento.CustomCreate(Self);
end;

procedure TPerson.SetMemento(const AValue: TBaseMemento);

Var
  P : TPersonmemento;

begin
  inherited SetMemento(AValue);
  P:=AValue as TPersonMemento;
  FirstName:=P.FirstName;
  LastName:=P.LastName;
end;

function TPerson.ObjectDescription: String;
begin
  Result:='person';
end;


{ TPersonList }

Function TPersonList.GetObj(AIndex : Integer) : TPerson;

begin
  Result:=TPerson(Inherited Items[AIndex]);
end;



Procedure TPersonList.SetObj(AIndex : Integer; AValue : TPerson);

begin
  Inherited Items[AIndex]:=AValue;
end;



Function TPersonList.Add(AnItem : TPerson) : Integer;

begin
  Result:=inherited Add(AnItem);
end;






end.
