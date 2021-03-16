unit Apollo_ORM_BindFMX;

interface

uses
  Apollo_ORM_Bind,
  System.Classes;

type
  IBindFMX = interface(IORMBind)
  ['{B13B1DD7-DE19-4C4A-A900-8970E92FC992}']
  end;

  function MakeFMXBind(aOwner: TComponent): IBindFMX;

implementation

uses
  Apollo_ORM,
  FMX.Edit;

type
  TBindFMX = class(TORMBind, IBindFMX)
  protected
    procedure SetControlProps(aControl: TComponent; aValue: Variant; aNotifyEvent: TNotifyEvent); override;
    procedure PropControlChange(Sender: TObject); override;
  end;

function MakeFMXBind(aOwner: TComponent): IBindFMX;
begin
  Result := TBindFMX.Create(aOwner);
end;

{ TBindFMX }

procedure TBindFMX.PropControlChange(Sender: TObject);
var
  BindItem: TBindItem;
  Entity: TEntityAbstract;
  PropName: string;
begin
  if GetBindItem(Sender, 0, BindItem) then
  begin
    Entity := BindItem.Entity;
    PropName := BindItem.PropName;
  end
  else
    Exit;

  if Sender is TEdit then
    Entity.Prop[PropName] := TEdit(Sender).Text;
end;

procedure TBindFMX.SetControlProps(aControl: TComponent; aValue: Variant;
  aNotifyEvent: TNotifyEvent);
begin
  if aControl is TEdit then
  begin
    TEdit(aControl).Text := aValue;
    TEdit(aControl).OnChange := aNotifyEvent;
  end;
end;

end.
