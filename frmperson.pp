unit frmperson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, mdlPerson, frmedit;

type
  TForm = TEditForm;
  { TPersonForm }

  TPersonForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EFirstname: TEdit;
    ELastname: TEdit;
    Label1: TLabel;
    LELastName: TLabel;
  private
    { private declarations }
  protected
    Procedure ConfigureMediator; override;
  public
    { public declarations }
  end; 

var
  PersonForm: TPersonForm;

Function EditPerson(APerson : TPerson) : Boolean;

implementation

Function EditPerson(APerson : TPerson) : Boolean;

begin
  With TPersonForm.Create(Application) do
    try
      Subject:=APerson;
      Result:=ShowModal=mrOK;
    finally
      Free;
    end;
end;


{$R *.lfm}

{ TPersonForm }

procedure TPersonForm.ConfigureMediator;
begin
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='FirstName';
    Component:=EFirstName;
    end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='LastName';
    Component:=ELastName;
    end;
end;

end.

