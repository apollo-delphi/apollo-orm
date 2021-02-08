unit Apollo_ORM;

interface

uses
  Apollo_DB_Core,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  System.Generics.Collections;

type
  TFieldType = Data.DB.TFieldType;
  TEntityAbstract = class;
  TEntityClass = class of TEntityAbstract;

  TKeyField = record
    FieldType: TFieldType;
    PropName: string;
  end;

  TPrimaryKey = TArray<TKeyField>;

  TPrimaryKeyHelper = record helper for TPrimaryKey
    function TryGetKeyField(const aPropName: string; out aKeyField: TKeyField): Boolean;
    procedure Add(const aPropName: string; const aFieldType: TFieldType);
  end;

  TRelType = (rtUnknown, rtOne2One, rtOne2Many);

  TFKey = record
    PropName: string;
    ReferEntityClass: TEntityClass;
    ReferPropName: string;
    RelType: TRelType;
  end;

  TFKeys = TArray<TFKey>;

  TFKeysHelper = record helper for TFKeys
    procedure Add(const aPropName: string; aReferEntityClass: TEntityClass;
      const aReferPropName: string; aRelType: TRelType = rtUnknown);
    function Contains(const aPropName: string): Boolean;
  end;

  TStructure = record
    FKeys: TFKeys;
    PrimaryKey: TPrimaryKey;
    TableName: string;
  end;

  PInstanceField = ^TInstanceField;
  TInstanceField = record
    FieldName: string;
    FieldType: TFieldType;
    Value: Variant;
  end;

  TInstance = TArray<TInstanceField>;

  TIstanceHelper = record helper for TInstance
    function Count: Integer;
    function FieldByName(const aFieldName: string): PInstanceField;
  end;

  TORMTools = record
    class function GetFieldNameByPropName(const aPropName: string): string; static;
    class function GetInstance(aQuery: TFDQuery): TInstance; static;
    class function GetPropNameByFieldName(const aFieldName: string): string; static;
  end;

{$M+}
  TEntityAbstract = class abstract
  private
    FDBEngine: TDBEngine;
    FInstance: TInstance;
    FIsNew: Boolean;
    function GetInsertSQL: string;
    function GetInstanceFieldType(const aFieldName: string): TFieldType;
    function GetInstanceFieldValue(const aFieldName: string): Variant;
    function GetNormInstanceFieldValue(aInstanceField: TInstanceField): Variant;
    function GetNormPropValue(const aPropName: string): Variant;
    function GetPKFieldType(const aPropName: string): TFieldType;
    function GetProp(const aPropName: string): Variant;
    function GetSelectNoRowsSQL: string;
    function GetSelectSQL: string;
    function GetWherePart: string;
    function PropExists(const aPropName: string): Boolean;
    procedure AssignInstanceFromProps;
    procedure AssignProps;
    procedure AssignPropsFromInstance;
    procedure AssignPropsFromPKeyValues(const aPKeyValues: TArray<Variant>);
    procedure ExecToDB(const aSQL: string);
    procedure FillParam(aParam: TFDParam; const aPropName: string; aValue: Variant);
    procedure InsertToDB;
    procedure ReadInstance(const aPKeyValues: TArray<Variant>);
    procedure SetProp(const aPropName: string; aValue: Variant);
  public
    class function GetPrimaryKey: TPrimaryKey; virtual;
    class function GetStructure: TStructure; virtual; abstract;
    class function GetTableName: string;
    procedure Store;
    constructor Create(aDBEngine: TDBEngine); overload;
    constructor Create(aDBEngine: TDBEngine; const aPKeyValues: TArray<Variant>); overload;
    property IsNew: Boolean read FIsNew;
    property Prop[const aPropName: string]: Variant read GetProp write SetProp;
  end;

  TEntityFeatID = class abstract(TEntityAbstract)
  private
    FID: Integer;
  public
    class function GetPrimaryKey: TPrimaryKey; override;
  published
    property ID: Integer read FID write FID;
  end;

  TEntityAbstractList<T: TEntityAbstract> = class(TObjectList<T>)
  end;

  ORMUniqueID = class(TCustomAttribute)
  private
    FUniqueID: string;
  public
    constructor Create(const aUniqueID: string);
    property UniqueID: string read FUniqueID;
  end;

  NotNull = class(TCustomAttribute)
  end;

implementation

uses
  Apollo_ORM_Exception,
  System.Character,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Variants;

{ TORMTools }

class function TORMTools.GetFieldNameByPropName(
  const aPropName: string): string;
var
  i: Integer;
  PrivCharIsLower: Boolean;
begin
  Result := '';

  PrivCharIsLower := False;
  for i := 1 to Length(aPropName) do
  begin
    if PrivCharIsLower and aPropName[i].IsUpper then
      Result := Result + '_';

    Result := Result + aPropName[i].ToUpper;

    if aPropName[i].IsLower then
      PrivCharIsLower := True
    else
      PrivCharIsLower := False;
  end;
end;


class function TORMTools.GetInstance(aQuery: TFDQuery): TInstance;
var
  i: Integer;
  InstanceField: TInstanceField;
begin
  Result := [];

  for i := 0 to aQuery.Fields.Count - 1 do
    begin
      InstanceField.FieldName := aQuery.Fields[i].FullName.ToUpper;
      InstanceField.FieldType := aQuery.Fields[i].DataType;

      if (InstanceField.FieldType = ftBlob) and
         (not aQuery.Fields[i].IsNull)
      then
        InstanceField.Value := StringOf(aQuery.Fields[i].Value)
      else
        InstanceField.Value := aQuery.Fields[i].Value;

      Result := Result + [InstanceField];
    end;
end;

class function TORMTools.GetPropNameByFieldName(
  const aFieldName: string): string;
begin
  Result := aFieldName.Replace('_', '');
end;

{ TEntityAbstract }

procedure TEntityAbstract.AssignInstanceFromProps;
var
  i: Integer;
  PropName: string;
begin
  for i := 0 to FInstance.Count - 1 do
  begin
    PropName := TORMTools.GetPropNameByFieldName(FInstance[i].FieldName);
    if not PropExists(PropName) then
      Continue;

    if GetNormInstanceFieldValue(FInstance[i]) <> GetNormPropValue(PropName) then
      FInstance[i].Value := Prop[PropName];
  end;
end;

procedure TEntityAbstract.AssignProps;
begin
  AssignPropsFromInstance;
end;

procedure TEntityAbstract.AssignPropsFromInstance;
var
  InstanceField: TInstanceField;
  PropName: string;
begin
  for InstanceField in FInstance do
  begin
    if VarIsNull(InstanceField.Value) then
      Continue;

    PropName := TORMTools.GetPropNameByFieldName(InstanceField.FieldName);
    if not PropExists(PropName) then
      Continue;

    Prop[PropName] := InstanceField.Value;
  end;
end;

procedure TEntityAbstract.AssignPropsFromPKeyValues(
  const aPKeyValues: TArray<Variant>);
var
  i: Integer;
  KeyField: TKeyField;
begin
  i := 0;

  for KeyField in GetPrimaryKey do
  begin
    if not PropExists(KeyField.PropName) then
      raise EORMKeyPropNotExists.Create;

    if KeyField.FieldType <> ftAutoInc then
      Prop[KeyField.PropName] := aPKeyValues[i];

    Inc(i);
  end;
end;

constructor TEntityAbstract.Create(aDBEngine: TDBEngine);
begin
  Create(aDBEngine, []);
end;

constructor TEntityAbstract.Create(aDBEngine: TDBEngine;
  const aPKeyValues: TArray<Variant>);
begin
  FDBEngine := aDBEngine;

  if Length(aPKeyValues) > 0 then
  begin
    ReadInstance(aPKeyValues);
    if FIsNew then
      AssignPropsFromPKeyValues(aPKeyValues)
    else
      AssignProps;
  end
  else
    FIsNew := True;
end;

procedure TEntityAbstract.ExecToDB(const aSQL: string);
var
  dsQuery: TFDQuery;
  i: Integer;
  FieldName: string;
  ParamName: string;
  ParamValue: Variant;
  PropName: string;

  PropInfo: PPropInfo;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := aSQL;

    for i := 0 to dsQuery.Params.Count - 1 do
    begin
      ParamName := dsQuery.Params[i].Name;

      if ParamName.StartsWith('OLD_') then
        begin
          FieldName := ParamName.Substring(4);
          ParamValue := GetInstanceFieldValue(FieldName);
        end
      else
        begin
          PropInfo := GetPropInfo(Self, ParamName);
          if not Assigned(PropInfo) then
            raise EORMWrongParamName.Create;

          PropName := string(PropInfo.Name);
          ParamValue := GetNormPropValue(PropName);
        end;

      FillParam(dsQuery.Params[i], PropName, ParamValue);
    end;

    FDBEngine.ExecQuery(dsQuery);
  finally
    dsQuery.Free;
  end;
end;

procedure TEntityAbstract.FillParam(aParam: TFDParam; const aPropName: string;
  aValue: Variant);
var
  FieldName: string;
  FieldType: TFieldType;
begin
  if Length(FInstance) > 0 then
  begin
    FieldName := TORMTools.GetFieldNameByPropName(aPropName);
    FieldType := GetInstanceFieldType(FieldName);
  end
  else
    FieldType := GetPKFieldType(aPropName);

  aParam.DataType := FieldType;

  case FieldType of
    ftFloat, ftInteger, ftSmallint, ftAutoInc, ftCurrency:
    begin
      {if (aValue = 0) and
         (GetStructure.FKeys.Contains(aFieldName))
      then
        aParam.Clear
      else}
        aParam.AsFloat := aValue;
    end;
    ftDateTime: aParam.AsDateTime := aValue;
    ftBoolean: aParam.AsBoolean := aValue;
    ftString, ftWideString, ftWideMemo:
    begin
      if aValue = '' then
        aParam.Clear
      else
        aParam.AsString := aValue;
    end;
  else
    aParam.AsString := aValue;
  end;
end;

function TEntityAbstract.GetInsertSQL: string;
var
  FieldsPart: string;
  i: Integer;
  InstanceField: TInstanceField;
  PropName: string;
  ValuesPart: string;
begin
  FieldsPart := '';
  ValuesPart := '';
  i := 0;

  for InstanceField in FInstance do
  begin
    PropName := TORMTools.GetPropNameByFieldName(InstanceField.FieldName);

    if (InstanceField.FieldType <> ftAutoInc) and PropExists(PropName) then
    begin
      if i > 0 then
      begin
        FieldsPart := FieldsPart + ', ';
        ValuesPart := ValuesPart + ', ';
      end;
      FieldsPart := FieldsPart + Format('`%s`', [InstanceField.FieldName]);
      ValuesPart := ValuesPart + ':' + PropName;

      Inc(i);
    end;
  end;

  Result := Format('INSERT INTO %s (%s) VALUES (%s)', [GetTableName, FieldsPart, ValuesPart]);
end;

function TEntityAbstract.GetInstanceFieldType(
  const aFieldName: string): TFieldType;
begin
  Result := FInstance.FieldByName(aFieldName).FieldType;
end;

function TEntityAbstract.GetInstanceFieldValue(
  const aFieldName: string): Variant;
begin
  Result := FInstance.FieldByName(aFieldName).Value;
end;

function TEntityAbstract.GetNormInstanceFieldValue(
  aInstanceField: TInstanceField): Variant;
begin
  Result := aInstanceField.Value;

  if (aInstanceField.FieldType = ftString) and
     VarIsNull(Result)
  then
    Result := '';

  if (aInstanceField.FieldType in [ftInteger, ftShortint, ftFloat]) and
     VarIsNull(Result)
  then
    Result := 0;
end;

function TEntityAbstract.GetNormPropValue(const aPropName: string): Variant;
var
  PropInfo: PPropInfo;
begin
  Result := Prop[aPropName];

  PropInfo := GetPropInfo(Self, aPropName);

  if PropInfo^.PropType^.Name = 'Boolean' then
    if Result = 'True' then
      Result := 1
    else
      Result := 0;
end;

function TEntityAbstract.GetPKFieldType(const aPropName: string): TFieldType;
var
  KeyField: TKeyField;
begin
  Result := ftUnknown;

  for KeyField in GetPrimaryKey do
    if KeyField.PropName = aPropName then
      Exit(KeyField.FieldType);
end;

class function TEntityAbstract.GetPrimaryKey: TPrimaryKey;
begin
  Result := GetStructure.PrimaryKey;
end;

function TEntityAbstract.GetProp(const aPropName: string): Variant;
begin
  Result := GetPropValue(Self, aPropName);
end;

function TEntityAbstract.GetSelectNoRowsSQL: string;
begin
  Result := Format('SELECT * FROM %s WHERE 1 = 2', [GetTableName]);
end;

function TEntityAbstract.GetSelectSQL: string;
begin
  Result := Format('SELECT * FROM %s WHERE %s', [GetTableName, GetWherePart]);
end;

class function TEntityAbstract.GetTableName: string;
begin
  Result := GetStructure.TableName;
end;

function TEntityAbstract.GetWherePart: string;
var
  FieldName: string;
  i: Integer;
  KeyField: TKeyField;
begin
  i := 0;
  Result := '';

  for KeyField in GetPrimaryKey do
  begin
    if not PropExists(KeyField.PropName) then
      raise EORMKeyPropNotExists.Create;

    if i > 0 then
      Result := Result + ' AND ';

    FieldName := TORMTools.GetFieldNameByPropName(KeyField.PropName);
    Result := Result + Format('%s = :OLD_%s', [FieldName, KeyField.PropName]);
    Inc(i);
  end;
end;

procedure TEntityAbstract.InsertToDB;
var
  SQL: string;
begin
  if FInstance.Count = 0 then
    ReadInstance([]);

  SQL := GetInsertSQL;
  ExecToDB(SQL);
  AssignInstanceFromProps;
end;

function TEntityAbstract.PropExists(const aPropName: string): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Self, aPropName);

  if PropInfo <> nil then
    Result := True
  else
    Result := False;
end;

procedure TEntityAbstract.ReadInstance(const aPKeyValues: TArray<Variant>);
var
  dsQuery: TFDQuery;
  i: Integer;
  PropName: string;
  SQL: string;
begin
  if Length(aPKeyValues) = 0 then
    SQL := GetSelectNoRowsSQL
  else
    SQL := GetSelectSQL;

  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := SQL;

    if Length(aPKeyValues) <> dsQuery.Params.Count then
      raise EORMWrongInputCount.Create;

    for i := 0 to dsQuery.Params.Count - 1 do
    begin
      PropName := dsQuery.Params[i].Name.Substring(4);
      FillParam(dsQuery.Params[i], PropName, aPKeyValues[i]);
    end;

    FDBEngine.OpenQuery(dsQuery);

    if dsQuery.IsEmpty then
      FIsNew := True;

    FInstance := TORMTools.GetInstance(dsQuery);
  finally
    dsQuery.Free;
  end;
end;

procedure TEntityAbstract.SetProp(const aPropName: string; aValue: Variant);
begin
  SetPropValue(Self, aPropName, aValue);
end;

procedure TEntityAbstract.Store;
begin
  if FIsNew then
  begin
    InsertToDB;
    FIsNew := False;
  end
  else
end;

{ TIstanceHelper }

function TIstanceHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TIstanceHelper.FieldByName(const aFieldName: string): PInstanceField;
var
  i: Integer;
  InstanceField: PInstanceField;
begin
  Result := nil;

  for i := 0 to Length(Self) - 1 do
  begin
    InstanceField := @Self[i];

    if InstanceField.FieldName = aFieldName then
      Exit(InstanceField);
  end;
end;

{ TPrimaryKeyHelper }

procedure TPrimaryKeyHelper.Add(const aPropName: string;
  const aFieldType: TFieldType);
var
  KeyField: TKeyField;
begin
  KeyField.PropName := aPropName;
  KeyField.FieldType := aFieldType;

  Self := Self + [KeyField];
end;

function TPrimaryKeyHelper.TryGetKeyField(const aPropName: string; out aKeyField: TKeyField): Boolean;
var
  KeyField: TKeyField;
begin
  Result := False;

  for KeyField in Self do
    if KeyField.PropName = aPropName then
    begin
      aKeyField := KeyField;
      Exit(True);
    end;
end;

{ TEntityFeatID }

class function TEntityFeatID.GetPrimaryKey: TPrimaryKey;
var
  KeyField: TKeyField;
begin
  KeyField.PropName := 'ID';
  KeyField.FieldType := ftAutoInc;

  Result := [KeyField];
end;

{ TFKeysHelper }

procedure TFKeysHelper.Add(const aPropName: string;
  aReferEntityClass: TEntityClass; const aReferPropName: string; aRelType: TRelType);
var
  FKey: TFKey;
begin
  FKey.PropName := aPropName;
  FKey.ReferEntityClass := aReferEntityClass;
  FKey.ReferPropName := aReferPropName;
  FKey.RelType := aRelType;

  Self := Self + [FKey];
end;

function TFKeysHelper.Contains(const aPropName: string): Boolean;
var
  FKey: TFKey;
begin
  Result := False;

  for FKey in Self do
    if FKey.PropName = aPropName then
      Exit(True);
end;

{ ORMUniqueID }

constructor ORMUniqueID.Create(const aUniqueID: string);
begin
  FUniqueID := aUniqueID;
end;

end.
