program timetrack;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmmain, apptracker, baseopf, mdlinterrupt, mdlperson, mdlproject,
  mdltimetrack, mdltodo, visitors, frmperson, frmedit, frmtodo, frmproject,
  medmenu, mdltask, frmtask, frmconfig, frmtasktrack
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Time tracker';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

