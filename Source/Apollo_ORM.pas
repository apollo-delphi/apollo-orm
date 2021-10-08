unit Apollo_ORM;

interface

uses
  Apollo_DB_Core,
  Apollo_DB_Utils,
  Apollo_Types,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults;

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

  TFKey = record
    PropName: string;
    ReferEntityClass: TEntityClass;
    ReferPropName: string;
  end;

  TFKeys = TArray<TFKey>;

  TFKeysHelper = record helper for TFKeys
    procedure Add(const aPropName: string; aReferEntityClass: TEntityClass;
      const aReferPropName: string);
    function Contains(const aPropName: string): Boolean;
    function TryGetFKey(const aPropName: string; out aFKey: TFKey): Boolean; overload;
    function TryGetFKey(aReferEntityClass: TEntityClass; out aFKey: TFKey): Boolean; overload;
  end;

  TStructure = record
    FKeys: TFKeys;
    PrimaryKey: TPrimaryKey;
    TableName: string;
  end;

  PInstanceField = ^TInstanceField;
  TInstanceField = record
    BlobStream: TStream;
    FieldName: string;
    FieldType: TFieldType;
    Value: Variant;
  end;

  TInstance = class
  strict private
    FFields: TArray<TInstanceField>;
    FQueryKeeper: IQueryKeeper;
  private
    function Count: Integer;
    function FieldByName(const aFieldName: string): PInstanceField;
    procedure SetQueryKeeper(aQueryKeeper: IQueryKeeper);
    constructor Create(aQueryKeeper: IQueryKeeper);
    property Fields: TArray<TInstanceField> read FFields write FFields;
  end;

  TORMTools = record
    class function GetFieldNameByPropName(const aPropName: string): string; static;
    class function GetPropNameByFieldName(const aFieldName: string): string; static;
    class function TryGetPropNameByParamName(aEntity: TEntityAbstract; const aParamName: string;
      out aPropName: string): Boolean; static;
  end;

  TORMBlob = class
  private
    FInstanceStream: TStream;
    FModifiedStream: TStream;
    function GetStream: TStream;
    procedure FreeModifiedStream;
    procedure FreeStreams;
    procedure MoveModifiedToInstance;
    procedure SetStream(aStream: TStream);
  public
    function IsModified: Boolean;
    procedure LoadFromFile(const aFilePath: string);
    procedure LoadFromStream(aSourceStream: TStream);
    procedure SaveToFile(const aFilePath: string);
    constructor Create(aBlobStream: TStream);
    destructor Destroy; override;
    property Stream: TStream read GetStream write SetStream;
  end;

  TForEachBlobProp = reference to procedure(aBlob: TORMBlob);
  TCreateListFunc<T> = reference to function: T;

{$M+}
  TEntityAbstract = class abstract(TIntefacedObjectNotUsingReference, ISourceFreeNotification)
  private
    FDBEngine: TDBEngine;
    FFreeListProcs: TSimpleMethods;
    FFreeNotifications: TNotifyEvents;
    FInstance: TInstance;
    FIsNew: Boolean;
    FJoinedEntities: TObjectList<TEntityAbstract>;
    FNeedToRefresh: Boolean;
    FOwner: TEntityAbstract;
    FRevertListProcs: TSimpleMethods;
    FStoreListProcs: TSimpleMethods;
    function CreateJoinedEntity(aEntityClass: TEntityClass; const aKeyPropName,
      aReferPropName: string): TEntityAbstract;
    function GetBlobProp(const aPropName: string): TORMBlob;
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
    function HasBlob: Boolean;
    function SetNormPropValue(const aPropName: string; const aValue: Variant): Variant;
    procedure AssignBlobPropsFromInstance;
    procedure AssignInstanceFromProps;
    procedure AssignProps;
    procedure AssignPropsFromInstance;
    procedure AssignPropsFromPKeyValues(const aPKeyValues: TArray<Variant>);
    procedure ExecToDB(const aSQL: string);
    procedure FillParam(aParam: TFDParam; const aPropName: string; aValue: Variant);
    procedure ForEachBlobProp(aForEachBlobProp: TForEachBlobProp);
    procedure FreeBlobProps;
    procedure FreeFInstance;
    procedure FreeJoinedEntities;
    procedure Init;
    procedure InsertToDB;
    procedure ReadInstance(const aPKeyValues: TArray<Variant>);
    procedure RemoveJoinedEntity(aValue: TEntityAbstract);
    procedure SetBlobProp(const aPropName: string; aBlob: TORMBlob);
    procedure SetProp(const aPropName: string; aValue: Variant);
    procedure StoreJoinedChildEntities;
    procedure StoreJoinedParentEntities;
    procedure UpdateToDB;
  protected
    function GetJoinedEntity<T: TEntityAbstract>(const aFKPropName: string; aCurrentValue: T): T;
    function GetJoinedList<T: class>(aCurrentList: T; aCreateListFunc: TCreateListFunc<T>): T;
    function SetJoinedEntity<T: TEntityAbstract>(const aFKPropName: string; aOldValue, aNewValue: T): T;
    procedure AfterCreate; virtual;
    procedure ApplyAutoGenFields; virtual;
    procedure BeforeDelete; virtual;
    procedure BeforeDestroy; virtual;
    procedure BeforeStore; virtual;
  public
    class function GetPrimaryKey: TPrimaryKey; virtual;
    class function GetStructure: TStructure; virtual; abstract;
    class function GetTableName: string;
    class procedure RegisterForService;
    function PropExists(const aPropName: string): Boolean;
    procedure AddFreeNotify(aNotifyEvent: TNotifyEvent);
    procedure Delete;
    procedure Refresh;
    procedure Revert;
    procedure Store;
    procedure StoreAll;
    constructor Create(aDBEngine: TDBEngine); overload;
    constructor Create(aDBEngine: TDBEngine; const aInstance: TInstance); overload;
    constructor Create(aDBEngine: TDBEngine; const aPKeyValues: TArray<Variant>); overload;
    destructor Destroy; override;
    property IsNew: Boolean read FIsNew;
    property Owner: TEntityAbstract read FOwner write FOwner;
    property Prop[const aPropName: string]: Variant read GetProp write SetProp;
  end;
{$M-}

  SkipStructure = class(TCustomAttribute)
  end;

  ORMUniqueID = class(TCustomAttribute)
  private
    FUniqueID: string;
  public
    constructor Create(const aUniqueID: string);
    property UniqueID: string read FUniqueID;
  end;

  Index = class(TCustomAttribute)
  end;

  Unique = class(TCustomAttribute)
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

{$M+}
  [SkipStructure]
  TEntityFeatID = class abstract(TEntityAbstract)
  private
    FID: Integer;
  protected
    procedure ApplyAutoGenFields; override;
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

  IEntityListBuilder = interface
  ['{1E55579C-26C1-47AD-974E-1051B249D148}']
    function AddAndWhere(const aAlias, aPropName: string; const a흎uality: T흎uality;
      const aParamName: string): IEntityListBuilder;
    function AddCount(const aAlias, aPropName, aAsFieldName: string): IEntityListBuilder;
    function AddLeftJoin(aEntityClass: TEntityClass; const aAlias: string): IEntityListBuilder;
    function AddOrderBy(const aOrderItem: TOrderItem): IEntityListBuilder;
    function BuildSQL: string;
    function FromTable(aEntityClass: TEntityClass): IEntityListBuilder;
    function GetLimit: Integer;
    function SetLimit(const aValue: Integer): IEntityListBuilder;
    function SetOffset(const aValue: Integer): IEntityListBuilder;
    function SetParam(const aParamName: string; const aValue: Variant): IEntityListBuilder;
    procedure FillParams(aQuery: TFDQuery);
    property Limit: Integer read GetLimit;
  end;

  TEntityListBase<T: TEntityAbstract> = class(TEntityListAbstract<T>)
  private
    FBuilder: IEntityListBuilder;
    FCanLoadMore: Boolean;
    FDBEngine: TDBEngine;
    FFKey: TFKey;
    FOffset: Integer;
    FOwnerEntity: TEntityAbstract;
    FRecycleBin: TArray<T>;
    function GetSelectSQL(const aFilter: TFilter; const aOrder: POrder): string;
    procedure CleanRecycleBin;
    procedure FillList(const aFilter: TFilter; const aOrder: POrder);
    procedure LoadList;
  protected
    FComparer: IComparer<T>;
    procedure AfterCreate; virtual;
  public
    function CanLoadMore(out aFromRec: Integer): Boolean;
    procedure Clear;
    procedure Delete(const aEntity: T); overload;
    procedure Delete(const aIndex: Integer); overload;
    procedure DeleteAll;
    procedure LoadMore;
    procedure Remove(const aEntity: T); overload;
    procedure Remove(const aIndex: Integer); overload;
    procedure Revert;
    procedure Store;
    constructor Create(aDBEngine: TDBEngine; const aFilter: TFilter;
      const aOrderBy: POrder = nil); overload;
    constructor Create(aOwnerEntity: TEntityAbstract;
      const aOrderBy: POrder = nil); overload;
    constructor Create(aDBEngine: TDBEngine; const aBuilder: IEntityListBuilder = nil); overload;
    destructor Destroy; override;
  end;

  function MakeEntityListBuilder(const aAlias: string): IEntityListBuilder;

implementation

uses
  Apollo_Helpers,
  Apollo_ORM_Exception,
  System.Character,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Variants;

type
  TJoinEntItem = record
    Alias: string;
    EntityClass: TEntityClass;
    LeftJoin: Boolean;
  end;

  TEntityListBuilder = class(TInterfacedObject, IEntityListBuilder)
  private
    FAgrFields: TArray<string>;
    FAlias: string;
    FEntityClass: TEntityClass;
    FJoinEntItems: TArray<TJoinEntItem>;
    FNeedGroupBy: Boolean;
    FQueryBuilder: IQueryBuilder;
    function GetPublishedProps(aClass: TClass): TArray<string>;
  protected
    function AddAndWhere(const aAlias, aPropName: string; const a흎uality: T흎uality;
      const aParamName: string): IEntityListBuilder;
    function AddCount(const aAlias, aPropName, aAsFieldName: string): IEntityListBuilder;
    function AddLeftJoin(aEntityClass: TEntityClass; const aAlias: string): IEntityListBuilder;
    function AddOrderBy(const aOrderItem: TOrderItem): IEntityListBuilder;
    function BuildSQL: string;
    function FromTable(aEntityClass: TEntityClass): IEntityListBuilder;
    function GetLimit: Integer;
    function SetLimit(const aValue: Integer): IEntityListBuilder;
    function SetOffset(const aValue: Integer): IEntityListBuilder;
    function SetParam(const aParamName: string; const aValue: Variant): IEntityListBuilder;
    procedure FillParams(aQuery: TFDQuery);
  public
    procedure AfterConstruction; override;
  end;

function MakeEntityListBuilder(const aAlias: string): IEntityListBuilder;
var
  Builder: TEntityListBuilder;
begin
  Builder := TEntityListBuilder.Create;
  Builder.FAlias := aAlias;

  Result := Builder as IEntityListBuilder;
end;

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

procedure TEntityAbstract.AfterCreate;
begin
end;

procedure TEntityAbstract.ApplyAutoGenFields;
begin
end;

procedure TEntityAbstract.AssignBlobPropsFromInstance;
var
  i: Integer;
  InstanceField: TInstanceField;
  ORMBlob: TORMBlob;
  PropName: string;
begin
  for i := 0 to FInstance.Count - 1 do
  begin
    InstanceField := FInstance.Fields[i];

    if not Assigned(InstanceField.BlobStream) then
      Continue;

    PropName := TORMTools.GetPropNameByFieldName(InstanceField.FieldName);
    if not PropExists(PropName) then
    begin
      FreeAndNil(FInstance.Fields[i].BlobStream);
      Continue;
    end;

    ORMBlob := TORMBlob.Create(InstanceField.BlobStream);
    SetBlobProp(PropName, ORMBlob);
  end;
end;

procedure TEntityAbstract.AssignInstanceFromProps;
var
  i: Integer;
  PropName: string;
begin
  for i := 0 to FInstance.Count - 1 do
  begin
    if Assigned(FInstance.Fields[i].BlobStream) then
      Continue;
  
    PropName := TORMTools.GetPropNameByFieldName(FInstance.Fields[i].FieldName);
    if not PropExists(PropName) then
      Continue;

    if GetNormInstanceFieldValue(FInstance.Fields[i]) <> GetNormPropValue(PropName) then
      FInstance.Fields[i].Value := GetNormPropValue(PropName);
  end;
end;

procedure TEntityAbstract.AssignProps;
begin
  AssignPropsFromInstance;
  AssignBlobPropsFromInstance;
end;

procedure TEntityAbstract.AssignPropsFromInstance;
var
  InstanceField: TInstanceField;
  PropName: string;
begin
  for InstanceField in FInstance.Fields do
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

procedure TEntityAbstract.BeforeDelete;
begin
end;

procedure TEntityAbstract.BeforeDestroy;
begin
end;

procedure TEntityAbstract.BeforeStore;
begin
end;

constructor TEntityAbstract.Create(aDBEngine: TDBEngine; const aInstance: TInstance);
begin
  Init;
  FDBEngine := aDBEngine;
  FInstance := aInstance;

  AssignProps;
  AfterCreate;
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
  Init;
  FDBEngine := aDBEngine;

  if (Length(aPKeyValues) > 0) or HasBlob then
  begin
    ReadInstance(aPKeyValues);
    if FIsNew then
    begin
      AssignPropsFromPKeyValues(aPKeyValues);
      AssignBlobPropsFromInstance;
    end
    else
      AssignProps;
  end
  else
    FIsNew := True;

  AfterCreate;
end;

function TEntityAbstract.CreateJoinedEntity(aEntityClass: TEntityClass;
  const aKeyPropName, aReferPropName: string): TEntityAbstract;
var
  dsQuery: TFDQuery;
  Instance: TInstance;
  KeyValue: Variant;
  QueryKeeper: IQueryKeeper;
  SQL: string;
begin
  Result := nil;
  KeyValue := Prop[aKeyPropName];

  if (VarToStr(KeyValue) <> '') and
     (VarToStr(KeyValue) <> '0')
  then
  begin
    SQL := 'SELECT * FROM %s WHERE %s = :%s';
    SQL := Format(SQL, [
      aEntityClass.GetTableName,
      TORMTools.GetFieldNameByPropName(aReferPropName),
      aReferPropName
    ]);

    dsQuery := TFDQuery.Create(nil);
    QueryKeeper := MakeQueryKeeper(dsQuery);

    dsQuery.SQL.Text := SQL;
    dsQuery.ParamByName(aReferPropName).Value := KeyValue;

    FDBEngine.OpenQuery(dsQuery);

    if not dsQuery.IsEmpty then
    begin
      Instance := TInstance.Create(QueryKeeper);

      Result := aEntityClass.Create(FDBEngine, Instance);
      Result.FOwner := Self;
      FJoinedEntities.Add(Result);
      FRevertListProcs.Add(Result.Revert);
    end;
  end;
end;

procedure TEntityAbstract.Delete;
var
  SQL: string;
begin
  BeforeDelete;

  if not FIsNew then
  begin
    SQL := GetDeleteSQL;
    ExecToDB(SQL);
    FIsNew := True;
  end;
end;

destructor TEntityAbstract.Destroy;
begin
  BeforeDestroy;

  FFreeNotifications.Exec;
  FreeBlobProps;
  FreeFInstance;
  FreeJoinedEntities;
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
  if Assigned(FInstance) then
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
      if (aValue = 0) and
         (GetStructure.FKeys.Contains(aPropName))
      then
        aParam.Clear
      else
        aParam.AsFloat := aValue;
    end;
    ftDateTime: aParam.AsDateTime := aValue;
    //ftBoolean: aParam.AsBoolean := aValue;
    ftString, ftWideString, ftWideMemo, ftBoolean:
    begin
      if aValue = '' then
        aParam.Clear
      else
        aParam.AsString := aValue;
    end;
    ftBlob: aParam.LoadFromStream(GetBlobProp(aPropName).Stream, ftBlob);
  else
    aParam.AsString := aValue;
  end;
end;

procedure TEntityAbstract.ForEachBlobProp(aForEachBlobProp: TForEachBlobProp);
var
  ORMBlob: TORMBlob;
  InstanceField: TInstanceField;
  PropName: string;
begin
  if not Assigned(FInstance) then
    Exit;

  for InstanceField in FInstance.Fields do
    if Assigned(InstanceField.BlobStream) then
    begin
      PropName := TORMTools.GetPropNameByFieldName(InstanceField.FieldName);
      ORMBlob := GetBlobProp(PropName);
      if Assigned(ORMBlob) then
        aForEachBlobProp(ORMBlob);
    end;
end;

procedure TEntityAbstract.FreeBlobProps;
begin
  ForEachBlobProp(
    procedure(aBlob: TORMBlob)
    begin
      aBlob.Free;
    end
  );
end;

procedure TEntityAbstract.FreeFInstance;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
end;

procedure TEntityAbstract.FreeJoinedEntities;
var
  JoinedEntity: TEntityAbstract;
begin
  for JoinedEntity in FJoinedEntities do
    if JoinedEntity.Owner = Self then
      JoinedEntity.Free;

  FJoinedEntities.Free;
end;

function TEntityAbstract.GetBlobProp(const aPropName: string): TORMBlob;
begin
  Result := GetObjectProp(Self, aPropName) as TORMBlob;
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

  for InstanceField in FInstance.Fields do
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

function TEntityAbstract.GetJoinedEntity<T>(const aFKPropName: string;
  aCurrentValue: T): T;
var
  FKey: TFKey;
  FKeyValue: Variant;
begin
  if GetStructure.FKeys.TryGetFKey(aFKPropName, {out}FKey) then
  begin
    if Assigned(aCurrentValue) then
    begin
      FKeyValue := Prop[FKey.PropName];
      if not FNeedToRefresh and (FKeyValue = aCurrentValue.Prop[FKey.ReferPropName]) then
        Result := aCurrentValue
      else
      begin
        RemoveJoinedEntity(aCurrentValue);
        Result := CreateJoinedEntity(FKey.ReferEntityClass, FKey.PropName, FKey.ReferPropName) as T;
        FNeedToRefresh := False;
      end;
    end
    else
      Result := CreateJoinedEntity(FKey.ReferEntityClass, FKey.PropName, FKey.ReferPropName) as T;
  end
  else
    raise Exception.CreateFmt('TEntityAbstract.GetJoinedEntity: FK for property % did not find.', [aFKPropName]);
end;

function TEntityAbstract.GetJoinedList<T>(aCurrentList: T; aCreateListFunc: TCreateListFunc<T>): T;
begin
  if not Assigned(aCurrentList) then
    Result := aCreateListFunc
  else
  if FNeedToRefresh then
  begin
    FFreeListProcs.Remove(Pointer(aCurrentList));
    FRevertListProcs.Remove(Pointer(aCurrentList));
    FStoreListProcs.Remove(Pointer(aCurrentList));
    aCurrentList.Free;

    Result := aCreateListFunc;
    FNeedToRefresh := False;
  end
  else
    Result := aCurrentList;
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
      Exit('1')
    else
      Exit('0');

  if PropInfo.PropType^.Kind = tkEnumeration then
    Result := GetOrdProp(Self, aPropName);
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
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Self, aPropName);

  if not Assigned(PropInfo) then
    raise Exception.CreateFmt('TEntityAbstract.GetProp: Prop %s does not exist.', [aPropName]);

  if PropInfo^.PropType^.Kind = tkClass then
    Result := Null
  else
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

  for InstanceField in FInstance.Fields do
  begin
    PropName := TORMTools.GetPropNameByFieldName(InstanceField.FieldName);

    if (PropExists(PropName)) and (
        (GetNormInstanceFieldValue(InstanceField) <> GetNormPropValue(PropName)) or
        ((InstanceField.FieldType = ftBlob) and GetBlobProp(PropName).IsModified)
       )
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

function TEntityAbstract.HasBlob: Boolean;
var
  i: Integer;
  PropCount: Integer;
  PropList: PPropList;
begin
  Result := False;

  PropCount := GetPropList(Self, PropList);
  try
    for i := 0 to PropCount - 1 do
      if string(PropList^[i].PropType^.Name) = TORMBlob.ClassName then
        Exit(True);

  finally
    FreeMem(PropList);
  end;
end;

procedure TEntityAbstract.Init;
begin
  FIsNew := False;
  FJoinedEntities := TObjectList<TEntityAbstract>.Create(False{aOwnsObjects});
end;

procedure TEntityAbstract.InsertToDB;
var
  SQL: string;
begin
  if not Assigned(FInstance) then
    ReadInstance([]);

  SQL := GetInsertSQL;
  ExecToDB(SQL);

  ApplyAutoGenFields;
  AssignInstanceFromProps;

  ForEachBlobProp(
    procedure(aBlob: TORMBlob)
    begin
      aBlob.MoveModifiedToInstance;
    end
  );
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
  QueryKeeper: IQueryKeeper;
  SQL: string;
begin
  FreeFInstance;

  if Length(aPKeyValues) = 0 then
    SQL := GetSelectNoRowsSQL
  else
    SQL := GetSelectSQL;

  dsQuery := TFDQuery.Create(nil);
  QueryKeeper := MakeQueryKeeper(dsQuery);

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

  FInstance := TInstance.Create(QueryKeeper);
end;

procedure TEntityAbstract.Refresh;
begin
  FNeedToRefresh := True;
end;

class procedure TEntityAbstract.RegisterForService;
begin
  //required to call in initialization section if ORM Service will be used
end;

procedure TEntityAbstract.RemoveJoinedEntity(aValue: TEntityAbstract);
begin
  FJoinedEntities.Remove(aValue);
  if aValue.Owner = Self then
    aValue.Free;
end;

procedure TEntityAbstract.Revert;
begin
  FRevertListProcs.Exec;

  if FIsNew then
    Exit;

  AssignPropsFromInstance;
end;

procedure TEntityAbstract.SetBlobProp(const aPropName: string; aBlob: TORMBlob);
begin
  SetObjectProp(Self, aPropName, aBlob);
end;

function TEntityAbstract.SetJoinedEntity<T>(const aFKPropName: string; aOldValue, aNewValue: T): T;
var
  FKey: TFKey;
begin
  if aOldValue = aNewValue then
    Exit(aOldValue);

  if GetStructure.FKeys.TryGetFKey(aFKPropName, {out}FKey) then
  begin
    if Assigned(aOldValue) then
      RemoveJoinedEntity(aOldValue);

    Result := aNewValue;
    FJoinedEntities.Add(Result);
    FRevertListProcs.Add(Result.Revert);
    Prop[FKey.PropName] := Result.Prop[FKey.ReferPropName];
  end
  else
    raise Exception.CreateFmt('TEntityAbstract.SetJoinedEntity: FK for property % did not find.', [aFKPropName]);
end;

function TEntityAbstract.SetNormPropValue(const aPropName: string;
  const aValue: Variant): Variant;
var
  PropInfo: PPropInfo;
begin
  Result := aValue;

  PropInfo := GetPropInfo(Self, aPropName);

  if PropInfo^.PropType^.Name = 'Boolean' then
    if Result = 1 then
      Exit(True)
    else
      Exit(False);
end;

procedure TEntityAbstract.SetProp(const aPropName: string; aValue: Variant);
begin
  SetPropValue(Self, aPropName, SetNormPropValue(aPropName, aValue));
end;

procedure TEntityAbstract.Store;
begin
  BeforeStore;

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
  FDBEngine.TransactionStart;
  try
    StoreJoinedParentEntities;
    Store;
    StoreJoinedChildEntities;
    FStoreListProcs.Exec;

    FDBEngine.TransactionCommit;
  except;
    FDBEngine.TransactionRollback;
    raise;
  end;
end;

procedure TEntityAbstract.StoreJoinedChildEntities;
var
  FKey: TFKey;
  JoinedEntity: TEntityAbstract;
begin
  for JoinedEntity in FJoinedEntities do
  begin
    for FKey in JoinedEntity.GetStructure.FKeys do
      if ClassType = FKey.ReferEntityClass then
      begin
        JoinedEntity.Prop[FKey.PropName] := Prop[FKey.ReferPropName];
        JoinedEntity.StoreAll;
      end;
  end;
end;

procedure TEntityAbstract.StoreJoinedParentEntities;
var
  FKey: TFKey;
  JoinedEntity: TEntityAbstract;
begin
  for JoinedEntity in FJoinedEntities do
  begin
    for FKey in GetStructure.FKeys do
      if FKey.ReferEntityClass = JoinedEntity.ClassType then
      begin
        JoinedEntity.Store;
        Prop[FKey.PropName] := JoinedEntity.Prop[FKey.ReferPropName];
      end;
  end;
end;

procedure TEntityAbstract.AddFreeNotify(
  aNotifyEvent: TNotifyEvent);
begin
  FFreeNotifications.Add(Self, aNotifyEvent);
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

{ TInstance }

function TInstance.Count: Integer;
begin
  Result := Length(FFields);
end;

constructor TInstance.Create(aQueryKeeper: IQueryKeeper);
var
  i: Integer;
  InstanceField: TInstanceField;
  Query: TFDQuery;
begin
  Fields := [];
  Query := aQueryKeeper.Query;

  for i := 0 to Query.Fields.Count - 1 do
  begin
    InstanceField.FieldName := Query.Fields[i].FullName.ToUpper;
    InstanceField.FieldType := Query.Fields[i].DataType;

    if InstanceField.FieldType in [ftBlob] then
    begin
      InstanceField.BlobStream := Query.CreateBlobStream(Query.Fields[i], bmRead);
      InstanceField.Value := Null;
      SetQueryKeeper(aQueryKeeper);
    end
    else
    begin
      InstanceField.BlobStream := nil;
      InstanceField.Value := Query.Fields[i].Value;
    end;

    Fields := Fields + [InstanceField];
  end;
end;

function TInstance.FieldByName(const aFieldName: string): PInstanceField;
var
  i: Integer;
  InstanceField: PInstanceField;
begin
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    InstanceField := @Fields[i];

    if InstanceField.FieldName = aFieldName then
      Exit(InstanceField);
  end;
end;

procedure TInstance.SetQueryKeeper(aQueryKeeper: IQueryKeeper);
begin
  if not Assigned(FQueryKeeper) then
    FQueryKeeper := aQueryKeeper;
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
  if aID > 0 then
    inherited Create(aDBEngine, [aID])
  else
    inherited Create(aDBEngine);
end;

class function TEntityFeatID.GetPrimaryKey: TPrimaryKey;
var
  KeyField: TKeyField;
begin
  KeyField.PropName := 'ID';
  KeyField.FieldType := ftAutoInc;

  Result := [KeyField];
end;

procedure TEntityFeatID.ApplyAutoGenFields;
begin
  inherited;

  ID := FDBEngine.GetLastInsertedID;
end;

{ TFKeysHelper }

procedure TFKeysHelper.Add(const aPropName: string;
  aReferEntityClass: TEntityClass; const aReferPropName: string);
var
  FKey: TFKey;
begin
  FKey.PropName := aPropName;
  FKey.ReferEntityClass := aReferEntityClass;
  FKey.ReferPropName := aReferPropName;

  Self := Self + [FKey];
end;

function TFKeysHelper.Contains(const aPropName: string): Boolean;
var
  FKey: TFKey;
begin
  Result := TryGetFKey(aPropName, FKey);
end;

function TFKeysHelper.TryGetFKey(aReferEntityClass: TEntityClass;
  out aFKey: TFKey): Boolean;
var
  FKey: TFKey;
begin
  Result := False;
  for FKey in Self do
    if FKey.ReferEntityClass = aReferEntityClass then
    begin
      aFKey := FKey;
      Exit(True);
    end;
end;

function TFKeysHelper.TryGetFKey(const aPropName: string; out aFKey: TFKey): Boolean;
var
  FKey: TFKey;
begin
  Result := False;

  for FKey in Self do
    if FKey.PropName = aPropName then
    begin
      aFKey := FKey;
      Exit(True);
    end;
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

{ TEntityListBase<T> }

constructor TEntityListBase<T>.Create(aDBEngine: TDBEngine; const aFilter: TFilter;
  const aOrderBy: POrder);
begin
  inherited Create(True);

  FDBEngine := aDBEngine;
  FillList(aFilter, aOrderBy);

  AfterCreate;
end;

procedure TEntityListBase<T>.AfterCreate;
begin
end;

procedure TEntityListBase<T>.CleanRecycleBin;
var
  i: Integer;
begin
  for i := 0 to Length(FRecycleBin) - 1 do
  begin
    FRecycleBin[i].Delete;
    FreeAndNil(FRecycleBin[i]);
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

constructor TEntityListBase<T>.Create(aDBEngine: TDBEngine;
  const aBuilder: IEntityListBuilder);
begin
  inherited Create(True);

  FDBEngine := aDBEngine;
  FBuilder := aBuilder;
  FOffset := 0;

  LoadList;
end;

constructor TEntityListBase<T>.Create(aOwnerEntity: TEntityAbstract;
  const aOrderBy: POrder = nil);
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
    if FKey.ReferEntityClass = aOwnerEntity.ClassType then
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
    Create(aOwnerEntity.FDBEngine, TFilter.GetWhere(sFilter, ParamValues), aOrderBy);

    aOwnerEntity.FFreeListProcs.Add(Free);
    aOwnerEntity.FStoreListProcs.Add(Store);
    aOwnerEntity.FRevertListProcs.Add(Revert);
  end;
end;

procedure TEntityListBase<T>.Delete(const aIndex: Integer);
begin
  Items[aIndex].Delete;
  inherited Remove(Items[aIndex]);
end;

procedure TEntityListBase<T>.DeleteAll;
begin
  Clear;
  Store;
end;

procedure TEntityListBase<T>.Delete(const aEntity: T);
begin
  aEntity.Delete;
  inherited Remove(aEntity);
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

procedure TEntityListBase<T>.FillList(const aFilter: TFilter; const aOrder: POrder);
var
  dsQuery: TFDQuery;
  Entity: TEntityAbstract;
  i: Integer;
  Instance: TInstance;
  QueryKeeper: IQueryKeeper;
  SQL: string;
begin
  SQL := GetSelectSQL(aFilter, aOrder);

  if SQL.IsEmpty then
    Exit;

  dsQuery := TFDQuery.Create(nil);
  QueryKeeper := MakeQueryKeeper(dsQuery);

  dsQuery.SQL.Text := SQL;

  if Length(aFilter.ParamValues) <> dsQuery.Params.Count then
    raise EORMWrongInputCount.Create;

  for i := 0 to dsQuery.Params.Count - 1 do
    dsQuery.Params.Items[i].Value := aFilter.ParamValues[i];

  FDBEngine.OpenQuery(dsQuery);

  while not dsQuery.EOF do
  begin
    Instance := TInstance.Create(QueryKeeper);

    Entity := GetEntityClass.Create(FDBEngine, Instance);
    Add(Entity);

    dsQuery.Next;
  end;
end;

function TEntityListBase<T>.GetSelectSQL(const aFilter: TFilter; const aOrder: POrder): string;
var
  FromPart: string;
  i: Integer;
  OrderItem: TOrderItem;
  OrderPart: string;
  WherePart: string;
begin
  FromPart := GetEntityClass.GetTableName;
  WherePart := '';
  OrderPart := '';

  case aFilter.FilterMode of
    fmUnknown: Exit('');
    fmGetAll: WherePart := '';
    fmGetWhere: WherePart := ' WHERE ' + aFilter.WhereString;
  end;

  if Assigned(aOrder) and (Length(aOrder.OrderItems) > 0) then
  begin
    OrderPart := ' ORDER BY';
    for i := Low(aOrder.OrderItems) to High(aOrder.OrderItems) do
    begin
      if i > 0 then
        OrderPart := OrderPart + ',';
      OrderItem := aOrder.OrderItems[i];
      OrderPart := OrderPart + Format(' `%s`', [TORMTools.GetFieldNameByPropName(OrderItem.FieldName)]);
      if OrderItem.Direction = odDESC then
        OrderPart := OrderPart + ' DESC';
    end;
  end;

  Result := 'SELECT * FROM %s%s%s';
  Result := Format(Result, [FromPart, WherePart, OrderPart]).Trim;
end;

function TEntityListBase<T>.CanLoadMore(out aFromRec: Integer): Boolean;
begin
  Result := FCanLoadMore;
  if Result then
    aFromRec := FOffset + FBuilder.Limit;
end;

procedure TEntityListBase<T>.LoadList;
var
  Entity: TEntityAbstract;
  Instance: TInstance;
  QueryKeeper: IQueryKeeper;
begin
  FBuilder.FromTable(GetEntityClass);
  QueryKeeper := MakeQueryKeeper;
  QueryKeeper.Query.SQL.Text := FBuilder.BuildSQL;
  FBuilder.FillParams(QueryKeeper.Query);

  FDBEngine.OpenQuery(QueryKeeper.Query);

  while not QueryKeeper.Query.EOF do
  begin
    Instance := TInstance.Create(QueryKeeper);
    Entity := GetEntityClass.Create(FDBEngine, Instance);
    Add(Entity);

    QueryKeeper.Query.Next;
  end;

  if FBuilder.Limit > 0 then
    FCanLoadMore := QueryKeeper.Query.RecordCount = FBuilder.Limit;
end;

procedure TEntityListBase<T>.LoadMore;
begin
  if FCanLoadMore then
  begin
    FOffset := FOffset + FBuilder.Limit;
    FBuilder.SetOffset(FOffset);
    LoadList;
  end;
end;

procedure TEntityListBase<T>.Remove(const aIndex: Integer);
var
  Entity: T;
begin
  Entity := Items[aIndex];
  Remove(Entity);
end;

procedure TEntityListBase<T>.Revert;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if Items[i].IsNew then
      inherited Delete(i)
    else
      Items[i].Revert;

  for i := 0 to Length(FRecycleBin) - 1 do
    Add(FRecycleBin[i]);

  FRecycleBin := [];

  if Assigned(FComparer) then
    Sort(FComparer);
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

{ TORMBlob }

constructor TORMBlob.Create(aBlobStream: TStream);
begin
  FInstanceStream := aBlobStream;
end;

destructor TORMBlob.Destroy;
begin
  FreeStreams;

  inherited;
end;

procedure TORMBlob.FreeModifiedStream;
begin
  if Assigned(FModifiedStream) then
    FreeAndNil(FModifiedStream);
end;

procedure TORMBlob.FreeStreams;
begin
  FInstanceStream.Free;
  FreeModifiedStream;
end;

function TORMBlob.GetStream: TStream;
begin
  if Assigned(FModifiedStream) then
    Result := FModifiedStream
  else
    Result := FInstanceStream;

  Result.Position := 0;
end;

function TORMBlob.IsModified: Boolean;
begin
  Result := Assigned(FModifiedStream) and (FModifiedStream.Size > 0);
end;

procedure TORMBlob.LoadFromFile(const aFilePath: string);
begin
  FreeModifiedStream;
  FModifiedStream := TFileTools.CreateFileStream(aFilePath);
end;

procedure TORMBlob.LoadFromStream(aSourceStream: TStream);
begin
  aSourceStream.Position := 0;
  FreeModifiedStream;
  FModifiedStream := TMemoryStream.Create;
  FModifiedStream.CopyFrom(aSourceStream, aSourceStream.Size);
end;

procedure TORMBlob.MoveModifiedToInstance;
begin
  if not Assigned(FModifiedStream) then
    Exit;

  FInstanceStream.Free;
  FInstanceStream := FModifiedStream;

  FModifiedStream := nil;
end;

procedure TORMBlob.SaveToFile(const aFilePath: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(aFilePath, fmCreate);
  try
    FileStream.CopyFrom(GetStream, GetStream.Size);
  finally
    FileStream.Free;
  end;
end;

procedure TORMBlob.SetStream(aStream: TStream);
begin
  FreeModifiedStream;
  FModifiedStream := aStream;
end;

{ TEntityListBuilder }

function TEntityListBuilder.AddAndWhere(const aAlias, aPropName: string;
  const a흎uality: T흎uality; const aParamName: string): IEntityListBuilder;
begin
  FQueryBuilder.AddAndWhere(
    aAlias,
    TORMTools.GetFieldNameByPropName(aPropName),
    a흎uality,
    aParamName
  );

  Result := Self;
end;

function TEntityListBuilder.AddCount(const aAlias, aPropName,
  aAsFieldName: string): IEntityListBuilder;
begin
  FNeedGroupBy := True;

  if aAlias = FAlias then
    FAgrFields := FAgrFields + [aPropName];

  FQueryBuilder.AddSelectCount(
    aAlias,
    TORMTools.GetFieldNameByPropName(aPropName),
    TORMTools.GetFieldNameByPropName(aAsFieldName)
  );

  Result := Self;
end;

function TEntityListBuilder.AddLeftJoin(aEntityClass: TEntityClass;
  const aAlias: string): IEntityListBuilder;
var
  JoinEntItems: TJoinEntItem;
begin
  JoinEntItems.Alias := aAlias;
  JoinEntItems.EntityClass := aEntityClass;
  JoinEntItems.LeftJoin := True;
  FJoinEntItems := FJoinEntItems + [JoinEntItems];

  Result := Self;
end;

function TEntityListBuilder.AddOrderBy(
  const aOrderItem: TOrderItem): IEntityListBuilder;
var
  OrderItem: TOrderItem;
  Words: TArray<string>;
begin
  OrderItem.Alias := aOrderItem.Alias;
  if aOrderItem.FieldName.Contains('.') then
  begin
    Words := aOrderItem.FieldName.Split(['.']);
    OrderItem.Alias := Words[0];
    OrderItem.FieldName := Words[1];
  end
  else
    OrderItem.FieldName := TORMTools.GetFieldNameByPropName(aOrderItem.FieldName);
  OrderItem.Direction := aOrderItem.Direction;

  FQueryBuilder.AddOrderBy(OrderItem);

  Result := Self;
end;

procedure TEntityListBuilder.AfterConstruction;
begin
  inherited;

  FQueryBuilder := MakeQueryBuilder;
  FJoinEntItems := [];
  FAgrFields := [];
end;

function TEntityListBuilder.BuildSQL: string;
begin
  Result := FQueryBuilder.BuildSQL;
end;

procedure TEntityListBuilder.FillParams(aQuery: TFDQuery);
begin
  FQueryBuilder.FillParams(aQuery);
end;

function TEntityListBuilder.FromTable(aEntityClass: TEntityClass): IEntityListBuilder;
var
  FKey: TFKey;
  JoinEntItem: TJoinEntItem;
  Prop: string;
  PublishedProps: TArray<string>;
begin
  if Assigned(FEntityClass) then
    Exit(Self);

  FEntityClass := aEntityClass;
  FQueryBuilder.FromTable(aEntityClass.GetTableName, FAlias);

  PublishedProps := GetPublishedProps(aEntityClass);
  for Prop in PublishedProps do
    if not FAgrFields.Contains(Prop) then
      FQueryBuilder.AddSelect(FAlias, TORMTools.GetFieldNameByPropName(Prop));

  for JoinEntItem in FJoinEntItems do
  begin
    if JoinEntItem.EntityClass.GetStructure.FKeys.TryGetFKey(aEntityClass, {out}FKey) then
    begin
      if JoinEntItem.LeftJoin then
        FQueryBuilder.AddLeftJoin(
          JoinEntItem.EntityClass.GetTableName,
          JoinEntItem.Alias,
          TORMTools.GetFieldNameByPropName(FKey.PropName),
          FAlias,
          TORMTools.GetFieldNameByPropName(FKey.ReferPropName)
        )
      else
        FQueryBuilder.AddJoin(
          JoinEntItem.EntityClass.GetTableName,
          JoinEntItem.Alias,
          TORMTools.GetFieldNameByPropName(FKey.PropName),
          FAlias,
          TORMTools.GetFieldNameByPropName(FKey.ReferPropName)
        );
    end;
  end;

  if FNeedGroupBy then
    for Prop in PublishedProps do
      if not FAgrFields.Contains(Prop) then
        FQueryBuilder.AddGroupBy(FAlias, TORMTools.GetFieldNameByPropName(Prop));

  Result := Self;
end;

function TEntityListBuilder.GetLimit: Integer;
begin
  Result := FQueryBuilder.Limit;
end;

function TEntityListBuilder.GetPublishedProps(aClass: TClass): TArray<string>;
var
  RttiContext: TRttiContext;
  RttiProperties: TArray<TRttiProperty>;
  RttiProperty: TRttiProperty;
begin
  Result := [];

  RttiContext := TRttiContext.Create;
  try
    RttiProperties := RttiContext.GetType(aClass).GetProperties;
    for RttiProperty in RttiProperties do
      if RttiProperty.Visibility = mvPublished then
        Result := Result + [RttiProperty.Name];
  finally
    RttiContext.Free;
  end;
end;

function TEntityListBuilder.SetLimit(const aValue: Integer): IEntityListBuilder;
begin
  FQueryBuilder.SetLimit(aValue);

  Result := Self;
end;

function TEntityListBuilder.SetOffset(
  const aValue: Integer): IEntityListBuilder;
begin
  FQueryBuilder.SetOffset(aValue);

  Result := Self;
end;

function TEntityListBuilder.SetParam(const aParamName: string;
  const aValue: Variant): IEntityListBuilder;
begin
  FQueryBuilder.SetParam(aParamName, aValue);

  Result := Self;
end;

end.
