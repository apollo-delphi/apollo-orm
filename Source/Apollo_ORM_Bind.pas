unit Apollo_ORM_Bind;

interface

uses
  Apollo_ORM,
  System.Classes;

type
  TBindItem = record
    Control: TObject;
    Entity: TEntityAbstract;
    Index: Integer;
    PropName: string;
  end;

  IORMBind = interface
  ['{27E3AC30-5A47-498C-BFF0-9F644BC312AC}']
    function GetBindItems(aEntity: TEntityAbstract): TArray<TBindItem>;
    function GetEntity(aControl: TObject; const aIndex: Integer = 0): TEntityAbstract;
    function TryGetFirstBindItem(aEntity: TEntityAbstract; out aBindItem: TBindItem): Boolean;
    procedure Bind(aEntity: TEntityAbstract; aPrefix: string = ''); overload;
    procedure Bind(aEntity: TEntityAbstract; aControl: TObject; const aIndex: Integer = 0); overload;
    procedure RemoveBinds(aControl: TObject);
    property BindItems[aEntity: TEntityAbstract]: TArray<TBindItem> read GetBindItems;
    property Entity[aControl: TObject; const aIndex: Integer = 0]: TEntityAbstract read GetEntity;
  end;

  TORMBind = class abstract(TInterfacedObject, IORMBind)
  strict private
    FBindItems: TArray<TBindItem>;
    function GetPropNameByComponent(aComponent: TComponent; aPrefix: string): string;
    procedure AddBindItem(aControl: TObject; aEntity: TEntityAbstract;
      const aIndex: Integer; const aPropName: string);
  private
    function GetBindItems(aEntity: TEntityAbstract): TArray<TBindItem>;
    function GetEntity(aControl: TObject; const aIndex: Integer = 0): TEntityAbstract;
    function TryGetFirstBindItem(aEntity: TEntityAbstract; out aBindItem: TBindItem): Boolean;
    procedure Bind(aEntity: TEntityAbstract; aPrefix: string = ''); overload;
    procedure Bind(aEntity: TEntityAbstract; aControl: TObject; const aIndex: Integer = 0); overload;
    procedure RemoveBinds(aControl: TObject);
  protected
    FOwner: TComponent;
    function GetBindItem(aControl: TObject; const aIndex: Integer; out aBindItem: TBindItem): Boolean;
    procedure SetControlProps(aControl: TComponent; aValue: Variant; aNotifyEvent: TNotifyEvent); virtual; abstract;
    procedure PropControlChange(Sender: TObject); virtual; abstract;
  public
    constructor Create(aOwner: TComponent); virtual;
  end;

implementation

uses
  System.SysUtils;

{ TORMBind }

procedure TORMBind.AddBindItem(aControl: TObject; aEntity: TEntityAbstract;
  const aIndex: Integer; const aPropName: string);
var
  BindItem: TBindItem;
begin
  BindItem.Control := aControl;
  BindItem.Entity := aEntity;
  BindItem.Index := aIndex;
  BindItem.PropName := aPropName;

  FBindItems := FBindItems + [BindItem];
end;

procedure TORMBind.Bind(aEntity: TEntityAbstract; aControl: TObject;
  const aIndex: Integer);
begin
  AddBindItem(aControl, aEntity, aIndex, '');
end;

procedure TORMBind.Bind(aEntity: TEntityAbstract; aPrefix: string);
var
  Component: TComponent;
  i: Integer;
  PropName: string;
begin
  for i := 0 to FOwner.ComponentCount - 1 do
  begin
    Component := FOwner.Components[i];
    PropName := GetPropNameByComponent(Component, aPrefix);

    if not PropName.IsEmpty and
       aEntity.PropExists(PropName)
    then
    begin
      AddBindItem(Component, aEntity, 0, PropName);
      SetControlProps(Component, aEntity.Prop[PropName], PropControlChange);
    end;
  end;
end;

constructor TORMBind.Create(aOwner: TComponent);
begin
  FBindItems := [];
  FOwner := aOwner;
end;

function TORMBind.GetBindItem(aControl: TObject;
  const aIndex: Integer; out aBindItem: TBindItem): Boolean;
var
  BindItem: TBindItem;
begin
  Result := False;

  for BindItem in FBindItems do
    if (BindItem.Control = aControl) and (BindItem.Index = aIndex) then
    begin
      aBindItem := BindItem;
      Exit(True);
    end;
end;

function TORMBind.GetBindItems(aEntity: TEntityAbstract): TArray<TBindItem>;
var
  BindItem: TBindItem;
begin
  Result := [];

  for BindItem in FBindItems do
    if BindItem.Entity = aEntity then
      Result := Result + [BindItem];
end;

function TORMBind.GetEntity(aControl: TObject;
  const aIndex: Integer): TEntityAbstract;
var
  BindItem: TBindItem;
begin
  if GetBindItem(aControl, aIndex, BindItem) then
    Result := BindItem.Entity
  else
    Result := nil;
end;

function TORMBind.TryGetFirstBindItem(aEntity: TEntityAbstract; out aBindItem: TBindItem): Boolean;
var
  BindItem: TBindItem;
begin
  Result := False;

  for BindItem in FBindItems do
    if BindItem.Entity = aEntity then
    begin
      aBindItem := BindItem;
      Exit(True);
    end;
end;

function TORMBind.GetPropNameByComponent(aComponent: TComponent;
  aPrefix: string): string;
begin
  if Copy(aComponent.Name, 1, 2 + aPrefix.Length) = 'bc' + aPrefix then
    Result := Copy(aComponent.Name, 3 + aPrefix.Length, Length(aComponent.Name))
  else
    Result := '';
end;

procedure TORMBind.RemoveBinds(aControl: TObject);
var
  i: Integer;
begin
  for i := High(FBindItems) downto 0 do
    if FBindItems[i].Control = aControl then
      Delete(FBindItems, i, 1);
end;

end.
