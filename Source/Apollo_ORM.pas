unit Apollo_ORM;

interface

uses
  Apollo_DB_Core,
  Apollo_Types,
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
    procedure AddField(const aPropName: string; const aFieldType: TFieldType);
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
    class function TryGetPropNameByParamName(aEntity: TEntityAbstract; const aParamName: string;
      out aPropName: string): Boolean; static;
  end;

{$M+}
  TEntityAbstract = class abstract
  private
    FDBEngine: TDBEngine;
    FFreeListProcs: TSimpleMethods;
    FInstance: TInstance;
    FIsNew: Boolean;
    FStoreListProcs: TSimpleMethods;
    function GetDeleteSQL: string;
    function GetInsertSQL: string;
    function GetInstanceFieldType(const aFieldName: string): TFieldType;
    function GetInstanceFieldValue(const aFieldName: string): Variant;
    function GetNormInstanceFieldValue(aInstanceField: TInstanceField): Variant;
    function GetNormPropValue(const aPropName: string): Variant;
    function GetPKFieldType(const aPropName: string): TFieldType;
    function GetProp(const aPropName: string): Variant;
    function GetSelectNoRowsSQL: string;
    function GetSelectSQL: string;
    function GetUpdateSQL: string;
    function GetWherePart: string;
    procedure AssignInstanceFromProps;
    procedure AssignProps;
    procedure AssignPropsFromInstance;
    procedure AssignPropsFromPKeyValues(const aPKeyValues: TArray<Variant>);
    procedure ExecToDB(const aSQL: string);
    procedure FillParam(aParam: TFDParam; const aPropName: string; aValue: Variant);
    procedure ReadInstance(const aPKeyValues: TArray<Variant>);
    procedure SetProp(const aPropName: string; aValue: Variant);
    procedure UpdateToDB;
  protected
    procedure InsertToDB; virtual;
  public
    class function GetPrimaryKey: TPrimaryKey; virtual;
    class function GetStructure: TStructure; virtual; abstract;
    class function GetTableName: string;
    class procedure RegisterForService;
    function PropExists(const aPropName: string): Boolean;
    procedure Delete;
    procedure Revert;
    procedure Store;
    procedure StoreAll;
    constructor Create(aDBEngine: TDBEngine); overload;
    constructor Create(aDBEngine: TDBEngine; const aInstance: TInstance); overload;
    constructor Create(aDBEngine: TDBEngine; const aPKeyValues: TArray<Variant>); overload;
    destructor Destroy; override;
    property IsNew: Boolean read FIsNew;
    property Prop[const aPropName: string]: Variant read GetProp write SetProp;
  end;

  SkipStructure = class(TCustomAttribute)
  end;

  ORMUniqueID = class(TCustomAttribute)
  private
    FUniqueID: string;
  public
    constructor Create(const aUniqueID: string);
    property UniqueID: string read FUniqueID;
  end;

  [SkipStructure]
  TEntityFeatID = class abstract(TEntityAbstract)
  private
    FID: Integer;
  protected
    procedure InsertToDB; override;
  public
    class function GetPrimaryKey: TPrimaryKey; override;
    constructor Create(aDBEngine: TDBEngine; const aID: Integer = 0);
  published
    [ORMUniqueID('f8740b69ab3a151d75284ab144786a2e')]
    property ID: Integer read FID write FID;
  end;
{$M-}

  TEntityListAbstract<T: TEntityAbstract> = class abstract(TObjectList<T>)
  public
    class function GetEntityClass: TEntityClass;
    constructor Create(aOwnsObjects: Boolean = True); reintroduce;
  end;

  TFilterMode = (fmUnknown, fmGetAll, fmGetWhere);

  TFilter = record
  private
    FFilterMode: TFilterMode;
    FParamValues: TArray<Variant>;
    FWhereString: string;
    procedure Init;
  public
    class function GetAll: TFilter; static;
    class function GetWhere(const aWhereString: string;
      const aParamValues: TArray<Variant>): TFilter; static;
  end;

  TEntityListBase<T: TEntityAbstract> = class(TEntityListAbstract<T>)
  private
    FDBEngine: TDBEngine;
    FFKey: TFKey;
    FOwnerEntity: TEntityAbstract;
    FRecycleBin: TArray<T>;
    function GetSelectSQL(const aFilter: TFilter): string;
    procedure CleanRecycleBin;
    procedure FillList(const aFilter: TFilter);
  public
    procedure Clear;
    procedure Remove(const aEntity: T); overload;
    procedure Remove(const aIndex: Integer); overload;
    procedure Store;
    constructor Create(aDBEngine: TDBEngine; const aFilter: TFilter;
      const aOrderBy: string = ''); overload;
    constructor Create(aOwnerEntity: TEntityAbstract); overload;
    destructor Destroy; override;
  end;

  NotNull = class(TCustomAttribute)
  end;

  FieldLength = class(TCustomAttribute)
  private
    FLength: Integer;
  public
    constructor Create(const aLength: Integer);
    property Length: Integer read FLength;
  end;

  Text = class(TCustomAttribute)
  end;

implementation

uses
  Apollo_Helpers,
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

class function TORMTools.TryGetPropNameByParamName(aEntity: TEntityAbstract; const aParamName: string;
  out aPropName: string): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(aEntity, aParamName);

  if not Assigned(PropInfo) then
    Exit(False);

  Result := True;
  aPropName := string(PropInfo.Name);
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

constructor TEntityAbstract.Create(aDBEngine: TDBEngine; const aInstance: TInstance);
begin
  FDBEngine := aDBEngine;
  FInstance := aInstance;
  FIsNew := False;

  AssignProps;
end;

constructor TEntityAbstract.Create(aDBEngine: TDBEngine);
var
  PKeyValues: TArray<Variant>;
begin
  PKeyValues := [];
  Create(aDBEngine, PKeyValues);
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

procedure TEntityAbstract.Delete;
var
  SQL: string;
begin
  if not FIsNew then
  begin
    SQL := GetDeleteSQL;
    ExecToDB(SQL);
    FIsNew := True;
  end;
end;

destructor TEntityAbstract.Destroy;
begin
  FFreeListProcs.Exec;

  inherited;
end;

procedure TEntityAbstract.ExecToDB(const aSQL: string);
var
  dsQuery: TFDQuery;
  i: Integer;
  FieldName: string;
  ParamName: string;
  ParamValue: Variant;
  PropName: string;
  UseInstance: Boolean;
begin
  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := aSQL;

    for i := 0 to dsQuery.Params.Count - 1 do
    begin
      ParamName := dsQuery.Params[i].Name;

      if ParamName.StartsWith('OLD_') then
      begin
        ParamName := ParamName.Substring(4);
        UseInstance := True;
      end
      else
        UseInstance := False;

      if not TORMTools.TryGetPropNameByParamName(Self, ParamName, PropName) then
        raise EORMWrongParamName.Create;

      if UseInstance then
      begin
        FieldName := TORMTools.GetFieldNameByPropName(PropName);
        ParamValue := GetInstanceFieldValue(FieldName);
      end
      else
        ParamValue := GetNormPropValue(PropName);

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

function TEntityAbstract.GetDeleteSQL: string;
begin
  Result := Format('DELETE FROM %s WHERE %s', [GetTableName, GetWherePart]);
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

  if Result.IsEmpty then
    raise EORMTableNameNotDefined.Create(TEntityClass(GetObjectPropClass(ClassInfo)));
end;

function TEntityAbstract.GetUpdateSQL: string;
var
  i: Integer;
  InstanceField: TInstanceField;
  PropName: string;
  SetPart: string;
begin
  Result := '';
  i := 0;

  for InstanceField in FInstance do
  begin
    PropName := TORMTools.GetPropNameByFieldName(InstanceField.FieldName);

    if (PropExists(PropName)) and
       (GetNormInstanceFieldValue(InstanceField) <> GetNormPropValue(PropName))
    then
    begin
      if i > 0 then
        SetPart := SetPart + ', ';
      SetPart := SetPart + Format('`%s` = :%s', [InstanceField.FieldName, PropName]);
      Inc(i);
    end;
  end;

  if i > 0 then
    Result := Format('UPDATE %s SET %s WHERE %s', [GetTableName, SetPart, GetWherePart]);
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

class procedure TEntityAbstract.RegisterForService;
begin
  //required to call in initialization section if ORM Service will be used
end;

procedure TEntityAbstract.Revert;
begin
  if not FIsNew then
    AssignPropsFromInstance;
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
    UpdateToDB;
end;

procedure TEntityAbstract.StoreAll;
begin
  Store;
  FStoreListProcs.Exec;
end;

procedure TEntityAbstract.UpdateToDB;
var
  SQL: string;
begin
  SQL := GetUpdateSQL;
  if SQL.IsEmpty then
    Exit;

  ExecToDB(SQL);
  AssignInstanceFromProps;
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

procedure TPrimaryKeyHelper.AddField(const aPropName: string;
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

constructor TEntityFeatID.Create(aDBEngine: TDBEngine; const aID: Integer);
begin
  inherited Create(aDBEngine, [aID]);
end;

class function TEntityFeatID.GetPrimaryKey: TPrimaryKey;
var
  KeyField: TKeyField;
begin
  KeyField.PropName := 'ID';
  KeyField.FieldType := ftAutoInc;

  Result := [KeyField];
end;

procedure TEntityFeatID.InsertToDB;
begin
  inherited;

  ID := FDBEngine.GetLastInsertedID;
  FInstance.FieldByName('ID').Value := ID;
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

{ TEntityListAbstract<T> }

constructor TEntityListAbstract<T>.Create(aOwnsObjects: Boolean = True);
begin
  inherited Create(aOwnsObjects);
end;

class function TEntityListAbstract<T>.GetEntityClass: TEntityClass;
begin
  Result := T;
end;

{ TFilter }

class function TFilter.GetAll: TFilter;
begin
  Result.Init;
  Result.FFilterMode := fmGetAll;
end;

class function TFilter.GetWhere(const aWhereString: string; const aParamValues: TArray<Variant>): TFilter;
begin
  Result.Init;
  Result.FFilterMode := fmGetWhere;
  Result.FWhereString := aWhereString;
  Result.FParamValues := aParamValues;
end;

procedure TFilter.Init;
begin
  FFilterMode := fmUnknown;
  FParamValues := [];
  FWhereString := '';
end;

{ TEntityListBase<T> }

constructor TEntityListBase<T>.Create(aDBEngine: TDBEngine; const aFilter: TFilter;
  const aOrderBy: string);
begin
  inherited Create(True);

  FDBEngine := aDBEngine;
  FillList(aFilter);
end;

procedure TEntityListBase<T>.CleanRecycleBin;
var
  Entity: T;
begin
  for Entity in FRecycleBin do
  begin
    Entity.Delete;
    Entity.Free;
  end;

  FRecycleBin := [];
end;

procedure TEntityListBase<T>.Clear;
var
  Entity: T;
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    Entity := Items[i];
    Remove(Entity);
  end;
end;

constructor TEntityListBase<T>.Create(aOwnerEntity: TEntityAbstract);
var
  FieldName: string;
  FKey: TFKey;
  FKeys: TFKeys;
  ParamValues: TArray<Variant>;
  sFilter: string;
begin
  FKeys := GetEntityClass.GetStructure.FKeys;
  ParamValues := [];

  for FKey in FKeys do
    if (FKey.ReferEntityClass = aOwnerEntity.ClassType) and
       (FKey.RelType <> rtOne2One)
    then
    begin
      FieldName := TORMTools.GetFieldNameByPropName(FKey.PropName);
      sFilter := Format('%s = :%s', [FieldName, FKey.PropName]);

      ParamValues := ParamValues + [aOwnerEntity.Prop[FKey.ReferPropName]];

      FFKey := FKey;
      Break;
    end;

  if not sFilter.IsEmpty then
  begin
    FOwnerEntity := aOwnerEntity;
    Create(aOwnerEntity.FDBEngine, TFilter.GetWhere(sFilter, ParamValues));

    aOwnerEntity.FFreeListProcs.Add(Free);
    aOwnerEntity.FStoreListProcs.Add(Store);
  end;
end;

destructor TEntityListBase<T>.Destroy;
var
  Entity: T;
  i: Integer;
begin
  for i := 0 to Length(FRecycleBin) - 1 do
  begin
    Entity := FRecycleBin[i];
    FreeAndNil(Entity);
  end;

  inherited;
end;

procedure TEntityListBase<T>.FillList(const aFilter: TFilter);
var
  dsQuery: TFDQuery;
  Entity: TEntityAbstract;
  i: Integer;
  Instance: TInstance;
  SQL: string;
begin
  SQL := GetSelectSQL(aFilter);

  if SQL.IsEmpty then
    Exit;

  dsQuery := TFDQuery.Create(nil);
  try
    dsQuery.SQL.Text := SQL;

    if Length(aFilter.FParamValues) <> dsQuery.Params.Count then
      raise EORMWrongInputCount.Create;

    for i := 0 to dsQuery.Params.Count - 1 do
      dsQuery.Params.Items[i].Value := aFilter.FParamValues[i];

    FDBEngine.OpenQuery(dsQuery);

    while not dsQuery.EOF do
      begin
        Instance := TORMTools.GetInstance(dsQuery);

        Entity := GetEntityClass.Create(FDBEngine, Instance);
        Add(Entity);

        dsQuery.Next;
      end;
  finally
    dsQuery.Free;
  end;
end;

function TEntityListBase<T>.GetSelectSQL(const aFilter: TFilter): string;
var
  FromPart: string;
  i: Integer;
  OrderPart: string;
  WherePart: string;
begin
  FromPart := GetEntityClass.GetTableName;
  WherePart := '';
  OrderPart := '';

  case aFilter.FFilterMode of
    fmUnknown: Exit('');
    fmGetAll: WherePart := '';
    fmGetWhere: WherePart := ' WHERE ' + aFilter.FWhereString;
  end;

  Result := 'SELECT * FROM %s%s%s';
  Result := Format(Result, [FromPart, WherePart, OrderPart]).Trim;
end;

procedure TEntityListBase<T>.Remove(const aIndex: Integer);
var
  Entity: T;
begin
  Entity := Items[aIndex];
  Remove(Entity);
end;

procedure TEntityListBase<T>.Remove(const aEntity: T);
begin
  Extract(aEntity);
  FRecycleBin := FRecycleBin + [aEntity];
end;

procedure TEntityListBase<T>.Store;
var
  Entity: T;
  KeyPropName: string;
  RefPropName: string;
begin
  CleanRecycleBin;

  for Entity in Self do
  begin
    if Assigned(FOwnerEntity) then
      Entity.Prop[FFKey.PropName] := FOwnerEntity.Prop[FFKey.ReferPropName];

    Entity.StoreAll;
  end;
end;

{ FieldLength }

constructor FieldLength.Create(const aLength: Integer);
begin
  FLength := aLength;
end;

end.
