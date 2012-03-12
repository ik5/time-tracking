unit frmtask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, frmedit, mdltask;

type
  TForm = TEditForm;

  { TTaskForm }

  TTaskForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBProject: TComboBox;
    CBStatus: TComboBox;
    EBugID: TEdit;
    ETitle: TEdit;
    LCBProject: TLabel;
    LCBStatus: TLabel;
    LEBugID: TLabel;
    LLCreatedOn: TLabel;
    LCreatedOn: TLabel;
    LMRemarks: TLabel;
    LETitle: TLabel;
    MRemark: TMemo;
  private
    { private declarations }
  public
    { public declarations }
    Procedure ConfigureMediator; override;
  end;

var
  TaskForm: TTaskForm;

Function EditTask(ATask : TTask) : Boolean;

implementation

uses apptracker;

function EditTask(ATask: TTask): Boolean;
begin
  With TTaskForm.Create(Application) do
    try
      Subject:=ATask;
      Result:=ShowModal=mrOK;
    finally
      Free;
    end;
end;

{$R *.lfm}

{ TTaskForm }


procedure TTaskForm.ConfigureMediator;
begin
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='Title';
    Component:=ETitle;
    end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
      begin
      FieldName:='BugID';
      Component:=EBugID;
      end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='DisplayCreatedOn';
    Component:=LCreatedOn;
    end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='Status';
    Component:=CBStatus;
    end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='Description';
    Component:=MRemark;
    end;
  With Mediator.PropertyLinks.AddPropertyLinkDef do
    begin
    FieldName:='Project';
    Component:=CBProject;
    ValueList:=TrackerApp.Projects;
    end;
end;

end.

