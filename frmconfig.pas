unit frmconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  EditBtn, StdCtrls, Spin;

type

  { TTimeTrackerConfigForm }

  TTimeTrackerConfigForm = class(TForm)
    BPConfig: TButtonPanel;
    CBLimitTracking: TCheckBox;
    EEndOfWorkingDay: TEdit;
    EPassWord: TEdit;
    EUserName: TEdit;
    FEDB: TFileNameEdit;
    LEEndOfWorkingDay: TLabel;
    LEPassword: TLabel;
    LEUserName: TLabel;
    LFEDB: TLabel;
    LSEInterruptTimeout: TLabel;
    LSEInterruptTimeout1: TLabel;
    SEInterruptTimeOut: TSpinEdit;
    SEWeeksOfInterrupts: TSpinEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    procedure CheckLimit;
    procedure SaveConfig;
    procedure ShowConfig;
    { private declarations }
  public
    { public declarations }
  end; 

var
  TimeTrackerConfigForm: TTimeTrackerConfigForm;

implementation

uses apptracker;
{$R *.lfm}

{ TTimeTrackerConfigForm }

procedure TTimeTrackerConfigForm.FormShow(Sender: TObject);
begin
  ShowConfig;
end;

procedure TTimeTrackerConfigForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult=mrOK then
    SaveConfig;
end;

procedure TTimeTrackerConfigForm.CheckLimit;
begin
  EEndOfWorkingDay.Enabled:=CBLimitTracking.Checked;
end;

procedure TTimeTrackerConfigForm.ShowConfig;

Var
  s : TTrackerSettings;

begin
  S:=TrackerApp.Settings;
  FEDB.FileName:=S.DatabaseName;
  EUserName.Text:=S.DatabaseUser;
  EPassword.Text:=S.DatabasePwd;
  SEInterruptTimeOut.Value:=S.InterruptTimeOut;
  CBLimitTracking.Checked:=S.LimitTracking;
  SEWeeksOfInterrupts.Value:=S.WeeksOfInterrupts;
  CheckLimit;
  If (S.EndOfWorkingDay<>0) then
    EEndOfWorkingDay.Text:=FormatDateTime('hh:nn',S.EndOfWorkingDay)
end;

procedure TTimeTrackerConfigForm.SaveConfig;

Var
  s : TTrackerSettings;

begin
  S:=TrackerApp.Settings;
  S.DatabaseName     := FEDB.FileName;
  S.DatabaseUser     := EUserName.Text;
  S.DatabasePwd      := EPassword.Text;
  S.InterruptTimeOut := SEInterruptTimeOut.Value;
  S.WeeksOfInterrupts:=SEWeeksOfInterrupts.Value;
  S.LimitTracking    := CBLimitTracking.Checked;
  If (EEndOfWorkingDay.Text<>'') then
    S.EndOfWorkingDay:=StrToTime(EEndOfWorkingDay.Text)
  else
    S.EndOfWorkingDay:=0;
  TrackerApp.SaveSettings;
end;

end.

