unit frmtasktrack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, ComCtrls, StdCtrls, mdlTask, mdlTimeTrack, tiMediators, tiListmediators;

type

  { TTaskTimeTrackForm }

  TTaskTimeTrackForm = class(TForm)
    BPTaskTrack: TButtonPanel;
    LInDays: TLabel;
    LLIndays: TLabel;
    LLTotalTime: TLabel;
    LTask: TLabel;
    LTaskTitle: TLabel;
    LTotalTime: TLabel;
    LVTimeTrack: TListView;
    PTop: TPanel;
    PStats: TPanel;
  private
    FTask: TTask;
    FList : TTaskTimeTrackList;
    FListMediator : TtiListViewMediatorView ;
    FLTask,
    FLTotal,
    FLDays : TTiStaticTextMediatorView;
    procedure SetTask(const AValue: TTask);
    procedure SetupMediators;
    { private declarations }
  public
    { public declarations }
    Property Task : TTask Read FTask Write SetTask;
  end; 

var
  TaskTimeTrackForm: TTaskTimeTrackForm;

Procedure ShowTaskTime(T : TTask);

implementation

{$R *.lfm}

Procedure ShowTaskTime(T : TTask);

begin
  with TTaskTimeTrackForm.Create(Nil) do
    try
      Task:=T;
      ShowModal;
    finally
      Free;
    end;
end;

{ TTaskTimeTrackForm }

procedure TTaskTimeTrackForm.SetTask(const AValue: TTask);
begin
  if FTask=AValue then exit;
  FTask:=AValue;
  If Not Assigned(FList) then
    begin
    FList:=TTaskTimeTrackList.Create;
    FList.OwnsObjects:=True;
    SetupMediators;
    end;
  FList.Task:=FTask; // Will read
  FLTask.Subject:=FTask;
end;

procedure TTaskTimeTrackForm.SetupMediators;

begin
  FListMediator:=TtiListViewMediatorView.CreateCustom(FList,LVTimeTrack,'DisplayStart(180,"Start",|);DisplayStop(180,"Stop",|)');
  FListMediator.ShowDeleted:=False;
  FListMediator.Active:=true;
  if (FList.Count>0) then
    FListMediator.SelectedObject:=Flist.Items[0];
  FLTotal:=TtiStaticTextMediatorView.CreateCustom(LTotalTime,Flist,'DisplayTotalTime','Caption');
  FLTotal.Active:=True;
  FLTotal.ObjectToGUI;
  FLDays:=TtiStaticTextMediatorView.CreateCustom(LInDays,Flist,'DisplayTotalDays','Caption');
  FLDays.Active:=True;
  FLDays.ObjectToGUI;
  FLTask:=TtiStaticTextMediatorView.CreateCustom(LTaskTitle,FTask,'Caption','Caption');
  FLTask.Active:=True;
  FLTask.ObjectToGUI;
end;

end.

