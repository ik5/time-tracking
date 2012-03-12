unit frmtodo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, frmedit, mdltodo;

type
  TForm = TEditForm;
  { TTOdoForm }

  TTodoForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBDone: TCheckBox;
    CBProject: TComboBox;
    ETitle: TEdit;
    LCBProject: TLabel;
    LLCreatedOn: TLabel;
    LCreatedOn: TLabel;
    LMRemarks: TLabel;
    LETitle: TLabel;
    MRemark: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
  public
    { public declarations }
    Procedure ConfigureMediator; override;
  end;

var
  TodoForm: TTodoForm;

Function EditTodo(ATodo : TTodo) : Boolean;

implementation

uses apptracker;

function EditTodo(ATodo: TTodo): Boolean;
begin
  With TTodoForm.Create(Application) do
    try
      Subject:=ATodo;
      Result:=ShowModal=mrOK;
    finally
      Free;
    end;
end;

{$R *.lfm}

{ TTodoForm }

procedure TTodoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TTodoForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TTodoForm.ConfigureMediator;
begin
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='Title';
    Component:=ETitle;
    end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='DisplayCreatedOn';
    Component:=LCreatedOn;
    end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='Done';
    Component:=CBDone;
    end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='Remark';
    Component:=MRemark;
    end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='Project';
    Component:=CBProject;
    ValueList:=TrackerApp.Projects;
    end;
{
   Property CreatedOn : TDateTime Read FCreatedOn Write SetCreatedOn;
    Property Done : Boolean Read FDone Write SetDone;
    Property Project : TProject Read FProject Write SetProject;
    Property Remark : AnsiString Read FRemark Write SetRemark;
    Property Title : AnsiString Read FTitle Write SetTitle;
}
end;

end.

