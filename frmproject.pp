unit frmproject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, mdlproject, frmEdit;

type
  TForm = TEditForm;
  { TProjectForm }

  TProjectForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EName: TEdit;
    Label1: TLabel;
  private
    { private declarations }
  protected
    Procedure ConfigureMediator; override;
  public
    { public declarations }
  end; 

var
  ProjectForm: TProjectForm;

Function EditProject(AProject : TProject) : Boolean;

implementation

{$R *.lfm}
Function EditProject(AProject : TProject) : Boolean;

begin
  With TProjectForm.Create(Application) do
    try
      Subject:=AProject;
      Result:=ShowModal=mrOK;
    finally
      Free;
    end;
end;

{ TProjectForm }

procedure TProjectForm.ConfigureMediator;
begin
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='Name';
    Component:=EName;
    end;
end;

end.

