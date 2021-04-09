unit Apollo_ORM_BindFMX;

interface

uses
  Apollo_ORM,
  Apollo_ORM_Bind,
  FMX.Types,
  System.Classes;

type
  IBindFMX = interface(IORMBind)
  ['{B13B1DD7-DE19-4C4A-A900-8970E92FC992}']
    procedure Bind(aRootControl: TFmxObject; aEntity: TEntityAbstract;
      const aPrefix: string = ''); overload;
  end;

  TBindItem = Apollo_ORM_Bind.TBindItem;

  function MakeFMXBind(aOwner: TComponent): IBindFMX;

implementation

uses
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  FMX.Edit,
  FMX.Objects,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,

  System.Bindings.Helper,
  System.Bindings.Expression;

 type
  TBindFMX = class(TORMBind, IBindFMX)
  private
    FBindingsList: TBindingsList;
    FPrototypeBindSourceList: TObjectDictionary<TClass, TPrototypeBindSource>;

    BindingExpression1: TBindingExpression;

    function GetPrototypeBindSource(aEntity: TEntityAbstract): TPrototypeBindSource;

    procedure Observer(Sender: TObject);

  protected
    procedure Bind(aRootControl: TFmxObject; aEntity: TEntityAbstract;
      const aPrefix: string = ''); overload;
    procedure SetControlProps(aControl: TComponent; aValue: Variant; aNotifyEvent: TNotifyEvent); override;
    procedure PropControlChange(Sender: TObject); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

function MakeFMXBind(aOwner: TComponent): IBindFMX;
begin
  Result := TBindFMX.Create(aOwner);
end;

{ TBindFMX }

procedure TBindFMX.Bind(aRootControl: TFmxObject; aEntity: TEntityAbstract;
  const aPrefix: string);
var
  Control: TFmxObject;
  i: Integer;
  PropName: string;
  PrototypeBindSource: TPrototypeBindSource;

  LinkPropertyToFieldText: TLinkPropertyToField;
begin
  PrototypeBindSource := GetPrototypeBindSource(aEntity);

  for i := 0 to aRootControl.ChildrenCount - 1 do
  begin
    Control := aRootControl.Children.Items[i];
    PropName := Copy(Control.Name, 3 + Length(aPrefix), Length(Control.Name));

    if aEntity.PropExists(PropName) then
    begin
      //if Control is TText then
      begin
        {LinkPropertyToFieldText := TLinkPropertyToField.Create(PrototypeBindSource);
        LinkPropertyToFieldText.DataSource := PrototypeBindSource;
        LinkPropertyToFieldText.FieldName := PropName;
        LinkPropertyToFieldText.Component := TText(Control);
        LinkPropertyToFieldText.ComponentProperty := 'Text';

        FBindingsList.InsertComponent(LinkPropertyToFieldText);

        PrototypeBindSource.Active := False;
        PrototypeBindSource.Active := True;}

        BindingExpression1 := TBindings.CreateManagedBinding(
          [
            TBindings.CreateAssociationScope([Associate(aEntity, 'o1')])
          ],
          'o1.' + PropName,
          [
            TBindings.CreateAssociationScope([Associate(Control, 'res')])
          ],
          'res.Text', nil
        );

        //if Control is TEdit then
        //  TEdit(Control).OnC

        TBindings.Notify(aEntity, 'Name');
      end;
    end;
  end;
end;

constructor TBindFMX.Create(aOwner: TComponent);
begin
  inherited;

  FBindingsList := TBindingsList.Create(aOwner);
  FBindingsList.UseAppManager := True;
  FPrototypeBindSourceList := TObjectDictionary<TClass, TPrototypeBindSource>.Create([doOwnsValues]);
end;

destructor TBindFMX.Destroy;
begin
  FPrototypeBindSourceList.Free;

  inherited;
end;

function TBindFMX.GetPrototypeBindSource(
  aEntity: TEntityAbstract): TPrototypeBindSource;
var
  Context: TRttiContext;
  GeneratorFieldDef: TGeneratorFieldDef;
  GeneratorFieldType: TGeneratorFieldType;
  RttiProperty: TRttiProperty;
  RttiType: TRttiType;
begin
  if FPrototypeBindSourceList.TryGetValue(aEntity.ClassType, Result) then
    Exit(Result);

  Context := TRttiContext.Create;
  try
    Result := TPrototypeBindSource.Create(nil);
    Result.AutoActivate := True;
    RttiType := Context.GetType(aEntity.ClassType);

    for RttiProperty in RttiType.GetProperties do
    begin
      case RttiProperty.PropertyType.TypeKind of
        tkString: GeneratorFieldType := ftString;
      else
        GeneratorFieldType := ftString;
      end;

      GeneratorFieldDef := Result.FieldDefs.AddFieldDef;
      GeneratorFieldDef.Name := RttiProperty.Name;
      GeneratorFieldDef.FieldType := GeneratorFieldType;
    end;

    FPrototypeBindSourceList.Add(aEntity.ClassType, Result);
  finally
    Context.Free;
  end;
end;

procedure TBindFMX.Observer(Sender: TObject);
begin

end;

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
    TEdit(aControl).OnChange := Observer;
  end;
end;

end.
