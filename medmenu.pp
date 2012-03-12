unit medmenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, tiObject, tiMediators, tiBaseMediator, menus;

Type

  { TTiIntegerEditMediatorView }

  TTiIntegerEditMediatorView = Class(TtiEditMediatorView)
  private
    FEmptyValue: Integer;
  Protected
    procedure DoGUIToObject; override;
    procedure   SetupGUIandObject; override;
  Published
    Property EmptyValue : Integer Read FEmptyValue Write FEmptyValue;
  end;

  { TMediatorMenuItem }

  TMediatorMenuItem = Class(TMenuItem)
  private
    FMediator: TtiListItemMediator;
  Public
    Property Mediator : TtiListItemMediator Read FMediator Write Fmediator;
  end;

  { TtiMenuMediatorView }

  TtiMenuMediatorView = class(TtiCustomListMediatorView)
  private
    FCheckedProperty: String;
    FDisplayName: String;
    FFixedItems: Integer;
    FOnClick: TNotifyEvent;
    procedure SetFixedItems(const AValue: Integer);
  Protected
    Procedure DoOnClick(Sender : TObject); virtual;
    function DoCreateItemMediator(AData: TtiObject; ARowIdx: integer): TtiListItemMediator; override;
    procedure DoDeleteItemMediator(AIndex : Integer; AMediator : TtiListItemMediator); override;
    procedure SetupGUIandObject; override;
    procedure ClearList; override;
    procedure RebuildList; override;
    procedure CreateColumns; override;
  public
    constructor CreateCustom(AModel: TtiObjectList; AView: TMenuItem; ADisplayName: string; AIsObserving: Boolean = True; AFixedItems : Integer =0); reintroduce; overload;
    class function ComponentClass: TClass; override;
    function GetObjectFromItem(AItem: TMenuItem): TtiObject;
    destructor Destroy; override;
    function  View: TMenuItem; reintroduce;
    Property DisplayName : String Read FDisplayName Write FDisplayName;
    Property CheckedProperty : String Read FCheckedProperty Write FCheckedProperty;
    Property FixedItems : Integer Read FFixedItems Write SetFixedItems;
    Property OnClick : TNotifyEvent Read FOnClick Write FOnClick;

  end;

  { TtiMenuItemListItemMediator }

  TtiMenuItemListItemMediator = class(TtiListItemMediator)
  private
    FUpdating: Boolean;
    FView: TMenuItem;
  public
    constructor CreateCustom(AModel: TtiObject; AView: TMenuItem; AIsObserving: Boolean = True); reintroduce; overload;
    procedure Update(ASubject: TtiObject); override;
    Property Updating : Boolean Read FUpdating ;
  published
    property View: TMenuItem read FView;
  end;

implementation

{ TTiIntegerEditMediatorView }

procedure TTiIntegerEditMediatorView.DoGUIToObject;

Var
  I : Integer;
begin
  I:=StrToIntDef(View.Text,EmptyValue);
  SetOrdProp(Subject,FieldName,I);
end;

procedure TTiIntegerEditMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
end;

{ TtiMenuItemListItemMediator }

constructor TtiMenuItemListItemMediator.CreateCustom(AModel: TtiObject;
  AView: TMenuItem; AIsObserving: Boolean);
begin
  inherited Create;
  Model:=AModel;
  FView:=AView;
  If Assigned(AModel) and Assigned(AView) then
    Update(AModel);
  Active:=AIsObserving;
end;

procedure TtiMenuItemListItemMediator.Update(ASubject: TtiObject);

Var
  L : TtiMenuMediatorView;
  s : string;

begin
  FUpdating:=True;
  try
    If Not Assigned(ListMediator) then exit;
    L:=Listmediator as TtiMenuMediatorView;
    If L.DisplayName='' then
      View.Caption:=ASubject.Caption
    else
      begin
      S:=ASubject.PropValue[L.DisplayName];
      View.Caption:=S
      end;
    If (L.CheckedProperty<>'') then
      View.Checked:=ASubject.PropValue[L.CheckedProperty];
  finally
    FUpdating:=True;
  end;
end;

{ TtiMenuMediatorView }

procedure TtiMenuMediatorView.SetFixedItems(const AValue: Integer);
begin
  if FFixedItems=AValue then exit;
  FFixedItems:=AValue;
  If Assigned(View) then
    RebuildList;
end;

procedure TtiMenuMediatorView.DoOnClick(Sender: TObject);

Var
  M : TtiMenuItemListItemMediator;

begin
  M:=(Sender as TMediatorMenuItem).Mediator as TtiMenuItemListItemMediator;
  If (CheckedProperty<>'') then
    M.PropValue[CheckedProperty]:=(Sender as TMediatorMenuItem).Checked;
  If Assigned(FOnClick) then
    FOnClick(M.Model)
end;

function TtiMenuMediatorView.DoCreateItemMediator(AData: TtiObject;
  ARowIdx: integer): TtiListItemMediator;

Var
  MI : TMediatorMenuItem;
begin
  MI:=TMediatorMenuItem.Create(View);
  MI.OnClick:=@DoOnClick;
  Result:=TtiMenuItemListItemMediator.CreateCustom(AData,MI,Active);
  Result.ListMediator:=Self;
  MI.Mediator:=Result;
  MediatorList.Add(result);
  View.Add(MI);
  If Active then
    Result.Update(AData);

end;

procedure TtiMenuMediatorView.DoDeleteItemMediator(AIndex: Integer;
  AMediator: TtiListItemMediator);

Var
  M : TtiMenuItemListItemMediator;
  I : integer;

begin
  M:=AMEdiator as TtiMenuItemListItemMediator;
  I:=View.IndexOf(M.View);
  If I<>-1 then
    View.Delete(I);
end;


procedure TtiMenuMediatorView.SetupGUIandObject;
begin
  if (FixedItems=0) then
    View.Clear
  else
    While (View.Count>FixedItems) do
      View.Delete(View.Count-1);
end;

procedure TtiMenuMediatorView.ClearList;
begin
  MediatorList.Clear;
  If View <> nil then
    if (FixedItems=0) then
      View.Clear
    else
      While (View.Count>FixedItems) do
        View.Delete(View.Count-1);
end;

procedure TtiMenuMediatorView.RebuildList;
begin
  CreateSubMediators;
end;

procedure TtiMenuMediatorView.CreateColumns;
begin
  // nothing to do
end;

constructor TtiMenuMediatorView.CreateCustom(AModel: TtiObjectList;
  AView: TMenuItem; ADisplayName: string; AIsObserving: Boolean; AFixedItems : Integer = 0);
begin
  Inherited create;
  FDisplayName:=ADisplayName;
  FFixedItems:=AFixedItems;
  Subject:=AModel;
  SetView(AView);
  Active:=AIsObserving;
end;

class function TtiMenuMediatorView.ComponentClass: TClass;
begin
  Result:=TMenuItem;
end;

function TtiMenuMediatorView.GetObjectFromItem(AItem: TMenuItem): TtiObject;
begin
  Result:=(AItem as TMediatorMenuItem).Mediator.Model;
end;

destructor TtiMenuMediatorView.Destroy;
begin
  inherited Destroy;
end;

function TtiMenuMediatorView.View: TMenuItem;
begin
  Result:=TMenuItem(Inherited View);
end;

end.

