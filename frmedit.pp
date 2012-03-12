unit frmedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, controls, forms, dialogs, tiObject, baseopf, tiBaseMediator, tiModelMediator;

Type

  { TEditForm }

  TEditForm = Class(TForm)
  private
    FMediator: TtiModelMediator;
    FSubject: TBaseObject;
    FMemento : TBaseMemento;
    procedure SetSubject(const AValue: TBaseObject);
  Protected
    Function CheckSubject : boolean;
    Procedure CancelEdits; virtual;
    Procedure SaveEdits; virtual;
    Procedure ConfigureMediator; virtual; abstract;
  Public
    Destructor Destroy; override;
    function CloseQuery: boolean; override;
    Property Subject : TBaseObject Read FSubject Write SetSubject;
    Property Mediator : TtiModelMediator Read FMediator;
 end;

implementation

{ TEditForm }

procedure TEditForm.SetSubject(const AValue: TBaseObject);
begin
  if FSubject=AValue then exit;
  FSubject:=AValue;
  FMemento:=FSubject.Memento;
  If Not Assigned(FMediator) and Assigned(AValue) then
    FMediator:=TtiModelMediator.Create(Nil)
  else
    begin
    FMediator.Active:=False;
    FMediator.PropertyLinks.Clear;
    end;
  If Assigned(AValue) then
    begin
    ConfigureMediator;
    FMediator.Subject:=AValue;
    FMediator.Active:=True;
    end;
  if (Subject.ObjectState=posEmpty) then
    Caption:=Format('Create a new %s record',[Subject.ObjectDescription])
  else
    Caption:=Format('Editing %s record "%s"' ,[Subject.ObjectDescription,Copy(Subject.Caption,1,50)])
end;

function TEditForm.CheckSubject: boolean;

Var
  A : TTiObjectErrors;

begin
  A:=TTiObjectErrors.Create;
  try
    Result:=Subject.IsValid(A);
    If Not Result then
      MessageDlg('Invalid data',A.Items[0].ErrorMessage,mtError,[mbOK],0);
  finally
    A.Free;
  end;
end;

procedure TEditForm.CancelEdits;
begin
  FSubject.Memento:=FMemento;
end;

procedure TEditForm.SaveEdits;
begin
  FSubject.Save;
end;

destructor TEditForm.Destroy;
begin
  If Assigned(FMediator) then
    FMediator.Active:=False;
  FMediator.Free;
  inherited Destroy;
end;

function TEditForm.CloseQuery: boolean;
begin
  Result:=inherited CloseQuery;
  If Result then
    begin
    If ModalResult=mrCancel then
      CancelEdits
    else
      begin
      Result:=CheckSubject;
      If Result then
        SaveEdits;
      end;
    end;
end;

end.

