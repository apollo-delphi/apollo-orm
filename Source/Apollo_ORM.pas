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
  System.Contnrs,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Rtti;

type
  TFieldType = Data.DB.TFieldType;
  TEntityAbstract = class;
  TInstance = class;
  TEntityClass = class of TEntityAbstract;

  TKeyField = record
    FieldType: TFieldType;
    PropName: string;
  end;

  TPrimaryKey = TArray<TKeyField>;

  TPrimaryKeyHelper = record helper for TPrimaryKey
    function Contains(const aPropName: string): Boolean;
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
    function TryGetFKey(const aPropName: string; out aFKey: TFKey): Boolean;
  end;

  TStructure = record
    FKeys: TFKeys;
    PrimaryKey: TPrimaryKey;
    TableName: string;
  end;

  PInstanceField = ^TInstanceField;
  TInstanceField = record
  public
    BlobStream: TStream;
    FieldName: string;
    FieldType: TFieldType;
    Value: Variant;
    procedure Init;
  end;

  TInstanceList = record
    Alias: string;
    EntityClassName: string;
    EntityListClassName: string;
    List: TArray<TInstance>;
    PropName: string;
    function Count: Integer;
    function FindByHash(const aHash: string): TInstance;
    function NeedToMount: Boolean;
    procedure Init;
  end;

  TInstanceProp = record
    Alias: string;
    EntityClassName: string;
    Instance: TInstance;
    PropName: string;
    function NeedToMount: Boolean;
  end;

  TInstance = class
  strict private
    FEntity: TEntityAbstract;
    FFields: TArray<TInstanceField>;
    FQueryKeeper: IQueryKeeper;
  private
    FHash: string;
    FInstanceLists: TArray<TInstanceList>;
    FInstanceProps: TArray<TInstanceProp>;
    function Count: Integer;
    function FieldByName(const aFieldName: string): PInstanceField;
    procedure SetQueryKeeper(aQueryKeeper: IQueryKeeper);
  public
    type
      TForEachInstanceFieldProc = reference to procedure(const aInstanceField: TInstanceField;
        const aPropName: string; const aPropValue: Variant);
  public
    function GetFieldByName(const aFieldName: string): TInstanceField;
    procedure ForEachField(const aModifiedOnly: Boolean; aCallbackProc: TForEachInstanceFieldProc);
    procedure SetEntity(aEntity: TEntityAbstract);
    property Fields: TArray<TInstanceField> read FFields write FFields;
    constructor Create(aQueryKeeper: IQueryKeeper; const aAlias: string = ''); overload;
  end;

  TORMTools = record
    class function GetFieldNameByPropName(const aPropName: string): string; static;
    class function GetFieldNameWithAlias(const aAlias, aPropName: string): string; static;
    class function GetPropNameByFieldName(const aFieldName: string): string; static;
    class function GetPublishedProps(aClass: TClass): TArray<string>; static;
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

  IEntitySelectBuilder = interface
  ['{1E55579C-26C1-47AD-974E-1051B249D148}']
    function AddAndWhere(const aAlias, aPropName: string; const aEquality: TEquality;
      const aParamName: string): IEntitySelectBuilder;
    function AddCount(const aAlias, aPropName, aAsFieldName: string): IEntitySelectBuilder;
    function AddOrderBy(const aOrderItem: TOrderItem): IEntitySelectBuilder;
    function AddOrWhere(const aAlias, aPropName: string; const aEquality: TEquality;
      const aParamName: string): IEntitySelectBuilder;
    function BuildSQL: string;
    function JoinEntity(aEntityClass: TEntityClass; const aAlias: string;
      const aOwnerAlias: string = ''): IEntitySelectBuilder;
    function SetLimit(const aValue: Integer): IEntitySelectBuilder;
    function SetOffset(const aValue: Integer): IEntitySelectBuilder;
    function SetParam(const aParamName: string; const aValue: Variant): IEntitySelectBuilder;
  end;

  IEntitySelectBuilderImpl = interface(IEntitySelectBuilder)
  ['{CDE70C4D-A542-4858-A4E0-53FBB3DF8E3F}']
    function EncodeInstancesFromQuery(aQueryKeeper: IQueryKeeper): TInstanceList;
    function GetLimit: Integer;
    function GetOffset: Integer;
    procedure DecodeInstanceList(aDBEngine: TDBEngine; aOwnerEntity: TEntityAbstract;
      aEntityList: TObject; const aInstanceList: TInstanceList);
    procedure DecodeInstanceProps(aDBEngine: TDBEngine; aOwnerEntity: TEntityAbstract;
      const aInstanceProps: TArray<TInstanceProp>);
    procedure FillParams(aQuery: TFDQuery);
    procedure RecursiveJoinTables(aEntityClass: TEntityClass);
    procedure SetOrderBy(const aOrderBy: POrder);
    property Limit: Integer read GetLimit;
    property Offset: Integer read GetOffset;
  end;

{$M+}
  TEntityAbstract = class abstract(TIntefacedObjectNotUsingReference, ISourceFreeNotification)
  private
    type
      TForEachBlobProp = reference to procedure(aBlob: TORMBlob);
  private
    FBuilder: IEntitySelectBuilderImpl;
    FDBEngine: TDBEngine;
    FEagerLoadingMode: Boolean;
    FFreeListProcs: TSimpleMethods;
    FFreeNotifications: TNotifyEvents;
    FIsNew: Boolean;
    FJoinedEntities: TObjectList<TEntityAbstract>;
    FNeedToRefreshContainer: TObjectList;
    FOwner: TObject;
    FRevertListProcs: TSimpleMethods;
    FStoreListProcs: TSimpleMethods;
    function CreateJoinedEntity(aEntityClass: TEntityClass; const aKeyPropName,
      aReferPropName: string): TEntityAbstract;
    function DoCreateJoinedEntity(aEntityClass: TEntityClass; const aInstance: TInstance): TEntityAbstract;
    function GetBlobProp(const aPropName: string): TORMBlob;
    function GetDeleteSQL: string;
    function GetInsertSQL: string;
    function GetInstanceFieldType(const aFieldName: string): TFieldType;
    function GetInstanceFieldValue(const aFieldName: string): Variant;
    function GetNormInstanceFieldValue(aInstanceField: TInstanceField): Variant;
    function GetNormPropValue(const aPropName: string): Variant;
    function GetPKeyValues: TArray<Variant>;
    function GetPKFieldType(const aPropName: string): TFieldType;
    function GetProp(const aPropName: string): Variant;
    function GetSelectNoRowsSQL: string;
    function GetUpdateSQL: string;
    function GetWherePart: string;
    function HasBlob: Boolean;
    function SetNormPropValue(const aPropName: string; const aValue: Variant): Variant;
    procedure AssignBlobPropsFromInstance;
    procedure AssignProps;
    procedure AssignPropsFromPKeyValues(const aPKeyValues: TArray<Variant>);
    procedure ExecToDB(const aSQL: string);
    procedure FillParam(aParam: TFDParam; const aPropName: string; aValue: Variant);
    procedure ForEachBlobProp(aForEachBlobProp: TForEachBlobProp);
    procedure FreeBlobProps;
    procedure FreeFInstance;
    procedure FreeJoinedEntities;
    procedure Init;
    procedure RemoveJoinedEntity(aValue: TEntityAbstract);
    procedure SetBlobProp(const aPropName: string; aBlob: TORMBlob);
    procedure SetProp(const aPropName: string; aValue: Variant);
    procedure StoreJoinedChildEntities;
    procedure StoreJoinedParentEntities;
  protected
    type
      TCreateListFunc<T> = reference to function: T;
  protected
    FInstance: TInstance;
    function GetJoinedEntity<T: TEntityAbstract>(const aFKPropName: string; var aCurrentValue: T): T;
    function GetJoinedList<T: class>(var aCurrentList: T; aCreateListFunc: TCreateListFunc<T>): T;
    function SetJoinedEntity<T: TEntityAbstract>(const aFKPropName: string; aOldValue, aNewValue: T): T;
    procedure AfterCreate; virtual;
    procedure ApplyAutoGenFields; virtual;
    procedure AssignInstanceFromProps;
    procedure AssignPropsFromInstance(aInstance: TInstance);
    procedure BeforeDelete; virtual;
    procedure BeforeDestroy; virtual;
    procedure BeforeStore; virtual;
    procedure DeleteToDB; virtual;
    procedure InsertToDB; virtual;
    procedure ReadInstance(const aPKeyValues: TArray<Variant>); virtual;
    procedure UpdateToDB; virtual;
  public
    class function GetPrimaryKey: TPrimaryKey; virtual;
    class function GetStructure: TStructure; virtual; abstract;
    class function GetTableName: string;
    class procedure RegisterForService;
    function GetClassType: TEntityClass;
    function PropExists(const aPropName: string): Boolean;
    procedure AddFreeNotify(aNotifyEvent: TNotifyEvent);
    procedure Delete;
    procedure Refresh;
    procedure Revert;
    procedure Store;
    procedure StoreAll;
    constructor Create(aDBEngine: TDBEngine); overload;
    constructor Create(aDBEngine: TDBEngine; const aPKeyValues: TArray<Variant>); overload;
    constructor CreateByInstance(aDBEngine: TDBEngine; const aInstance: TInstance); overload;
    constructor CreateByKeeper(const aDBEngine: TDBEngine; const aPKeyValues: TArray<Variant>);
    destructor Destroy; override;
    property IsNew: Boolean read FIsNew;
    property Owner: TObject read FOwner write FOwner;
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

  DefaultValue = class(TCustomAttribute)
  private
    FValue: Variant;
  public
    constructor Create(const aValue: Variant);
    property Value: Variant read FValue;
  end;

  EagerLoading = class(TCustomAttribute)
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
    function CheckIsValueUnique(const aPropName: string; const aValue: Variant): Boolean; virtual;
    constructor Create(aDBEngine: TDBEngine; const aID: Integer = 0); reintroduce;
  published
    [ORMUniqueID('f8740b69ab3a151d75284ab144786a2e')]
    property ID: Integer read FID write FID;
  end;
{$M-}

  IEntityKeeper<T: TEntityAbstract> = interface
    function GetOne: T;
    property One: T read GetOne;
  end;

  TEntityKeeper<T: TEntityAbstract, constructor> = class(TInterfacedObject, IEntityKeeper<T>)
  private
    FDBEngine: TDBEngine;
    FEntity: T;
    FIsEntityOwner: Boolean;
    FPKeyValues: TArray<Variant>;
    procedure EntityFreeNotification(Sender: TObject);
  protected
    function GetOne: T;
  public
    constructor Create(aEntity: T);
    destructor Destroy; override;
  end;

  TEntityListAbstract<T: TEntityAbstract> = class abstract(TObjectList<T>)
  public
    class function GetEntityClass: TEntityClass;
    constructor Create(aOwnsObjects: Boolean = True); reintroduce;
  end;

  TEntityListBase<T: TEntityAbstract> = class(TEntityListAbstract<T>)
  private
    type
      TPostDeleteProc = reference to procedure;
  private
    FBuilder: IEntitySelectBuilderImpl;
    FDBEngine: TDBEngine;
    FEagerLoadingMode: Boolean;
    FFKey: TFKey;
    FOffset: Integer;
    FOwnerEntity: TEntityAbstract;
    FRecycleBin: TArray<T>;
    procedure CleanRecycleBin;
    procedure LoadList;
  protected
    type
      TForEachFKeyProc = reference to procedure(const aFieldName: string; const aFKey: TFKey);
  protected
    FCanLoadMore: Boolean;
    FComparer: IComparer<T>;
    function DoCreateRefList(aOwnerEntity: TEntityAbstract; aForEachFKeyProc: TForEachFKeyProc): Boolean;
    procedure AfterCreate; virtual;
  public
    function CanLoadMore(out aNextRecNum: Integer): Boolean;
    function GetNextRecNum: Integer; virtual;
    procedure Clear;
    procedure Delete(const aEntity: T); overload;
    procedure Delete(const aEntity: T; aPostDeleteProc: TPostDeleteProc); overload;
    procedure Delete(const aIndex: Integer); overload;
    procedure DeleteAll;
    procedure LoadMore; virtual;
    procedure Remove(const aEntity: T); overload;
    procedure Remove(const aIndex: Integer); overload;
    procedure Revert;
    procedure Store;
    constructor Create(aDBEngine: TDBEngine; const aOrderBy: POrder = nil); overload;
    constructor Create(aOwnerEntity: TEntityAbstract;
      const aOrderBy: POrder = nil); overload;
    constructor Create(aDBEngine: TDBEngine; const aBuilder: IEntitySelectBuilder); overload;
    destructor Destroy; override;
  end;

  function MakeEntitySelectBuilder(const aAlias: string): IEntitySelectBuilder;

var
  gRttiContext: TRttiContext;

implementation

uses
  Apollo_Helpers,
  Apollo_ORM_Exception,
  System.Character,
  System.SysUtils,
  System.Types,
  System.TypInfo,
  System.Variants;

type
  TJoinEntItem = record
    Alias: string;
    EntityClass: TEntityClass;
    Index: Integer;
    OwnerAlias: string;
    OwnerEntityClass: TClass;
    OwnerEntityListClass: TClass;
    OwnerPropName: string;
    procedure Init;
  end;

  TEntitySelectBuilder = class(TInterfacedObject, IEntitySelectBuilder, IEntitySelectBuilderImpl)
  private
    FAgrFields: TArray<string>;
    FAlias: string;
    FEntityClass: TEntityClass;
    FJoinEntItems: TArray<TJoinEntItem>;
    FNeedGroupBy: Boolean;
    FQueryBuilder: IQueryBuilder;
    function GetAlias(const aIndex: Integer): string;
    function GetClassByName(const aQualifiedName: string): TClass;
    function GetEagerLoadingItems(aEntityClass: TEntityClass): TArray<TJoinEntItem>;
    function GetHash(aQuery: TFDQuery; const aPrimaryKey: TPrimaryKey; const aAlias: string): string;
    function HasReference(aEntityClass, aReferEntityClass: TEntityClass; out aFKey: TFKey): Boolean;
    function IsEntityClass(aClass: TClass; out aEntityClass: TEntityClass): Boolean;
    function IsEntityListClass(aClass: TClass; out aEntityClass: TEntityClass): Boolean;
    procedure DoRecursiveJoinTables(aEntityClass: TEntityClass; var aIndex: Integer; const aOwnerAlias: string);
  protected
    function AddAndWhere(const aAlias, aPropName: string; const aEquality: TEquality;
      const aParamName: string): IEntitySelectBuilder;
    function AddCount(const aAlias, aPropName, aAsFieldName: string): IEntitySelectBuilder;
    function AddOrderBy(const aOrderItem: TOrderItem): IEntitySelectBuilder;
    function AddOrWhere(const aAlias, aPropName: string; const aEquality: TEquality;
      const aParamName: string): IEntitySelectBuilder;
    function BuildSQL: string;
    function EncodeInstancesFromQuery(aQueryKeeper: IQueryKeeper): TInstanceList;
    function GetLimit: Integer;
    function GetOffset: Integer;
    function JoinEntity(aEntityClass: TEntityClass; const aAlias: string;
      const aOwnerAlias: string = ''): IEntitySelectBuilder;
    function SetLimit(const aValue: Integer): IEntitySelectBuilder;
    function SetOffset(const aValue: Integer): IEntitySelectBuilder;
    function SetParam(const aParamName: string; const aValue: Variant): IEntitySelectBuilder;
    procedure DecodeInstanceList(aDBEngine: TDBEngine;
      aOwnerEntity: TEntityAbstract; aEntityList: TObject;
      const aInstanceList: TInstanceList);
    procedure DecodeInstanceProps(aDBEngine: TDBEngine; aOwnerEntity: TEntityAbstract;
      const aInstanceProps: TArray<TInstanceProp>);
    procedure FillParams(aQuery: TFDQuery);
    procedure RecursiveJoinTables(aEntityClass: TEntityClass);
    procedure SetOrderBy(const aOrderBy: POrder);
  public
    procedure AfterConstruction; override;
  end;

function MakeEntitySelectBuilder(const aAlias: string): IEntitySelectBuilder;
var
  Builder: TEntitySelectBuilder;
begin
  Builder := TEntitySelectBuilder.Create;
  Builder.FAlias := aAlias;

  Result := Builder as IEntitySelectBuilder;
end;

{ TORMTools }

class function TORMTools.GetFieldNameWithAlias(const aAlias, aPropName: string): string;
begin
  Result := Format('%s$%s', [aAlias, GetFieldNameByPropName(aPropName)]);
end;

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

class function TORMTools.GetPublishedProps(aClass: TClass): TArray<string>;
var
  RttiProperties: TArray<TRttiProperty>;
  RttiProperty: TRttiProperty;
begin
  Result := [];

  gRttiContext := TRttiContext.Create;
  try
    RttiProperties := gRttiContext.GetType(aClass).GetProperties;
    for RttiProperty in RttiProperties do
      if RttiProperty.Visibility = mvPublished then
        Result := Result + [RttiProperty.Name];
  finally
    gRttiContext.Free;
  end;
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
  AssignPropsFromInstance(FInstance);
  AssignBlobPropsFromInstance;
end;

procedure TEntityAbstract.AssignPropsFromInstance(aInstance: TInstance);
var
  InstanceField: TInstanceField;
  InstanceList: TInstanceList;
  PropName: string;
begin
  for InstanceField in aInstance.Fields do
  begin
    if VarIsNull(InstanceField.Value) then
      Continue;

    PropName := TORMTools.GetPropNameByFieldName(InstanceField.FieldName);
    if not PropExists(PropName) then
      Continue;

    Prop[PropName] := InstanceField.Value;
  end;

  if Assigned(FBuilder) then
  begin
    for InstanceList in aInstance.FInstanceLists do
      FBuilder.DecodeInstanceList(FDBEngine, Self, nil{aEntityList}, InstanceList);

    FBuilder.DecodeInstanceProps(FDBEngine, Self, aInstance.FInstanceProps);
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

constructor TEntityAbstract.CreateByInstance(aDBEngine: TDBEngine; const aInstance: TInstance);
begin
  Init;
  FDBEngine := aDBEngine;
  FInstance := aInstance;
  FInstance.SetEntity(Self);

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

constructor TEntityAbstract.CreateByKeeper(const aDBEngine: TDBEngine; const aPKeyValues: TArray<Variant>);
begin
  Create(aDBEngine, aPKeyValues);
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
      Result := DoCreateJoinedEntity(aEntityClass, Instance);
    end;
  end;
end;

procedure TEntityAbstract.Delete;
begin
  BeforeDelete;

  if not FIsNew then
  begin
    DeleteToDB;
    FIsNew := True;
  end;
end;

procedure TEntityAbstract.DeleteToDB;
var
  SQL: string;
begin
  SQL := GetDeleteSQL;
  ExecToDB(SQL);
end;

destructor TEntityAbstract.Destroy;
begin
  BeforeDestroy;

  FFreeNotifications.Exec;
  FreeBlobProps;
  FreeFInstance;
  FreeJoinedEntities;
  FFreeListProcs.Exec;
  FNeedToRefreshContainer.Free;

  inherited;
end;

function TEntityAbstract.DoCreateJoinedEntity(aEntityClass: TEntityClass;
  const aInstance: TInstance): TEntityAbstract;
begin
  Result := aEntityClass.CreateByInstance(FDBEngine, aInstance);
  Result.FOwner := Self;
  FJoinedEntities.Add(Result);
  FRevertListProcs.Add(Result.Revert);
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
    ftString, ftWideString, ftWideMemo, ftBoolean:
    begin
      if aValue = '' then
        aParam.Clear
      else
        aParam.AsWideString := aValue;
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

function TEntityAbstract.GetClassType: TEntityClass;
begin
  Result := TEntityClass(ClassType);
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

  Result := Format('INSERT INTO `%s` (%s) VALUES (%s)', [GetTableName, FieldsPart, ValuesPart]);
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
  var aCurrentValue: T): T;
var
  FKey: TFKey;
  FKeyValue: Variant;
  NeedToRefresh: Boolean;
begin
  if GetStructure.FKeys.TryGetFKey(aFKPropName, {out}FKey) then
  begin
    if Assigned(aCurrentValue) then
    begin
      NeedToRefresh := False;

      if FNeedToRefreshContainer.IndexOf(aCurrentValue) >= 0 then
      begin
        FNeedToRefreshContainer.Remove(aCurrentValue);
        NeedToRefresh := True;
      end;

      if not NeedToRefresh then
      begin
        FKeyValue := Prop[FKey.PropName];
        if aCurrentValue.Prop[FKey.ReferPropName] <> FKeyValue then
          NeedToRefresh := True;
      end;

      if NeedToRefresh then
      begin
        RemoveJoinedEntity(aCurrentValue);
        Result := CreateJoinedEntity(FKey.ReferEntityClass, FKey.PropName, FKey.ReferPropName) as T;
      end
      else
        Result := aCurrentValue;
    end
    else
      Result := CreateJoinedEntity(FKey.ReferEntityClass, FKey.PropName, FKey.ReferPropName) as T;
  end
  else
    raise Exception.CreateFmt('TEntityAbstract.GetJoinedEntity: FK for property % did not find.', [aFKPropName]);

  aCurrentValue := Result;
end;

function TEntityAbstract.GetJoinedList<T>(var aCurrentList: T; aCreateListFunc: TCreateListFunc<T>): T;
begin
  if not Assigned(aCurrentList) then
    Result := aCreateListFunc
  else
  if FNeedToRefreshContainer.IndexOf(aCurrentList) >= 0 then
  begin
    FNeedToRefreshContainer.Remove(aCurrentList);

    FFreeListProcs.Remove(Pointer(aCurrentList));
    FRevertListProcs.Remove(Pointer(aCurrentList));
    FStoreListProcs.Remove(Pointer(aCurrentList));
    aCurrentList.Free;

    Result := aCreateListFunc;
  end
  else
    Result := aCurrentList;

  aCurrentList := Result;
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

function TEntityAbstract.GetPKeyValues: TArray<Variant>;
var
  PKeyField: TKeyField;
begin
  Result := [];

  for PKeyField in GetPrimaryKey do
    Result := Result + [GetNormPropValue(PKeyField.PropName)];
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
  Result := Format('SELECT * FROM `%s` WHERE 1 = 2', [GetTableName]);
end;

class function TEntityAbstract.GetTableName: string;
begin
  Result := GetStructure.TableName;

  if Result.IsEmpty then
    raise EORMTableNameNotDefined.Create(ClassName);
end;

function TEntityAbstract.GetUpdateSQL: string;
var
  i: Integer;
  SetPart: string;
begin
  Result := '';
  i := 0;

  FInstance.ForEachField(True{aModifiedOnly},
    procedure(const aInstanceField: TInstanceField; const aPropName: string; const aPropValue: Variant)
    begin
      if i > 0 then
        SetPart := SetPart + ', ';
      SetPart := SetPart + Format('`%s` = :%s', [aInstanceField.FieldName, aPropName]);
      Inc(i);
    end
  );

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
  FNeedToRefreshContainer := TObjectList.Create(False{aOwnsObjects});
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
  InstanceList: TInstanceList;
  KeyField: TKeyField;
  PropName: string;
  QueryKeeper: IQueryKeeper;
  SQL: string;
begin
  FreeFInstance;
  gRttiContext := TRttiContext.Create;
  try
    if Length(aPKeyValues) = 0 then
      SQL := GetSelectNoRowsSQL
    else
    begin
      FBuilder := MakeEntitySelectBuilder('T') as IEntitySelectBuilderImpl;
      FBuilder.RecursiveJoinTables(GetClassType);

      for KeyField in GetPrimaryKey do
        FBuilder.AddAndWhere('T', KeyField.PropName, eEquals, 'OLD_' + KeyField.PropName);

      SQL := FBuilder.BuildSQL;
    end;

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
    begin
      FIsNew := True;
      FInstance := TInstance.Create(QueryKeeper, 'T');
    end
    else
    begin
      InstanceList := FBuilder.EncodeInstancesFromQuery(QueryKeeper);
      if InstanceList.Count = 1 then
        FInstance := InstanceList.List[0]
      else
        raise EORMMoreThenOneInstanceFound.Create;
    end;

    FInstance.SetEntity(Self);
  finally
    gRttiContext.Free;
  end;
end;

procedure TEntityAbstract.Refresh;
var
  i: Integer;
  JoinedEntity: TEntityAbstract;
begin
  for i := 0 to FFreeListProcs.Count - 1 do
  if FNeedToRefreshContainer.IndexOf(FFreeListProcs.GetInstance(i)) < 0 then
      FNeedToRefreshContainer.Add(FFreeListProcs.GetInstance(i));

  for JoinedEntity in FJoinedEntities do
    if FNeedToRefreshContainer.IndexOf(JoinedEntity) < 0 then
      FNeedToRefreshContainer.Add(JoinedEntity);

  if not IsNew then
  begin
    ReadInstance(GetPKeyValues);
    AssignProps;
  end;
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

  AssignPropsFromInstance(FInstance);
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

    if not Assigned(aNewValue.Owner) then
      aNewValue.Owner := Self;

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

constructor TInstance.Create(aQueryKeeper: IQueryKeeper; const aAlias: string);

  function MatchAlias(const aFullName: string; out FieldName: string): Boolean;
  var
    Alias: string;
  begin
    Result := True;

    if aFullName.Contains('$') then
    begin
      Alias := TStringTools.SubStrByKey(aFullName, '', '$');
      Result := Alias = aAlias;
      FieldName := TStringTools.SubStrByKey(aFullName, '$', '');
    end
    else
      FieldName := aFullName;
  end;

var
  FieldName: string;
  i: Integer;
  InstanceField: TInstanceField;
  Query: TFDQuery;
begin
  Fields := [];
  FInstanceLists := [];
  FInstanceProps := [];

  Query := aQueryKeeper.Query;

  for i := 0 to Query.Fields.Count - 1 do
  begin
    if MatchAlias(Query.Fields[i].FullName, {out}FieldName) then
    begin
      InstanceField.FieldName := FieldName;
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

procedure TInstance.ForEachField(const aModifiedOnly: Boolean; aCallbackProc: TForEachInstanceFieldProc);
var
  InstanceField: TInstanceField;
  PropName: string;
begin
  for InstanceField in Fields do
  begin
    PropName := TORMTools.GetPropNameByFieldName(InstanceField.FieldName);

    if not aModifiedOnly then
      aCallbackProc(InstanceField, PropName, FEntity.GetNormPropValue(PropName))
    else
    if (FEntity.PropExists(PropName)) and (
        (FEntity.GetNormInstanceFieldValue(InstanceField) <> FEntity.GetNormPropValue(PropName)) or
        ((InstanceField.FieldType = ftBlob) and FEntity.GetBlobProp(PropName).IsModified)
       )
    then
      aCallbackProc(InstanceField, PropName, FEntity.GetNormPropValue(PropName));
  end;
end;

function TInstance.GetFieldByName(const aFieldName: string): TInstanceField;
var
  InstanceField: TInstanceField;
begin
  InstanceField.Init;

  for InstanceField in Fields do
    if InstanceField.FieldName = aFieldName then
      Exit(InstanceField);
end;

procedure TInstance.SetEntity(aEntity: TEntityAbstract);
begin
  FEntity := aEntity;
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

function TPrimaryKeyHelper.Contains(const aPropName: string): Boolean;
var
  KeyField: TKeyField;
begin
  Result := TryGetKeyField(aPropName, {out}KeyField);
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

function TEntityFeatID.CheckIsValueUnique(const aPropName: string;
  const aValue: Variant): Boolean;
var
  QueryKeeper: IQueryKeeper;
begin
  QueryKeeper := MakeQueryKeeper;
  QueryKeeper.Query.SQL.Text := Format('SELECT COUNT(*) FROM `%s` WHERE `%s` = :VALUE AND ID <> :ID',
    [GetTableName, TORMTools.GetFieldNameByPropName(aPropName)]);
  QueryKeeper.Query.ParamByName('VALUE').Value := aValue;
  QueryKeeper.Query.ParamByName('ID').AsInteger := ID;

  FDBEngine.OpenQuery(QueryKeeper.Query);

  if QueryKeeper.Query.Fields[0].AsInteger = 0 then
    Result := True
  else
    Result := False;
end;

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
  Result := TryGetFKey(aPropName, {out}FKey);
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

constructor TEntityListBase<T>.Create(aDBEngine: TDBEngine; const aOrderBy: POrder);
begin
  inherited Create(True);

  FDBEngine := aDBEngine;
  FBuilder := MakeEntitySelectBuilder('T') as IEntitySelectBuilderImpl;
  FBuilder.SetOrderBy(aOrderBy);
  LoadList;

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
  const aBuilder: IEntitySelectBuilder);
begin
  inherited Create(True);

  FDBEngine := aDBEngine;
  FBuilder := IEntitySelectBuilderImpl(aBuilder);
  FOffset := FBuilder.Offset;

  if not FEagerLoadingMode then
    LoadList;

  AfterCreate;
end;

constructor TEntityListBase<T>.Create(aOwnerEntity: TEntityAbstract;
  const aOrderBy: POrder);
begin
  FBuilder := MakeEntitySelectBuilder('T') as IEntitySelectBuilderImpl;
  FBuilder.SetOrderBy(aOrderBy);

  if DoCreateRefList(aOwnerEntity,
    procedure(const aFieldName: string; const aFKey: TFKey)
    begin
      FBuilder
        .AddAndWhere('T', aFKey.PropName, eEquals, aFKey.PropName)
        .SetParam(aFKey.PropName, aOwnerEntity.Prop[aFKey.ReferPropName]);
    end
  )
  then
    Create(aOwnerEntity.FDBEngine, FBuilder);
end;

procedure TEntityListBase<T>.Delete(const aIndex: Integer);
begin
  Items[aIndex].Delete;
  inherited Remove(Items[aIndex]);
end;

procedure TEntityListBase<T>.Delete(const aEntity: T;
  aPostDeleteProc: TPostDeleteProc);
begin
  aEntity.Delete;
  aPostDeleteProc;
  inherited Remove(aEntity);
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

function TEntityListBase<T>.DoCreateRefList(aOwnerEntity: TEntityAbstract;
  aForEachFKeyProc: TForEachFKeyProc): Boolean;
var
  FieldName: string;
  FKey: TFKey;
  FKeys: TFKeys;
begin
  Result := False;
  FKeys := GetEntityClass.GetStructure.FKeys;

  for FKey in FKeys do
    if FKey.ReferEntityClass = aOwnerEntity.ClassType then
    begin
      FFKey := FKey;
      FieldName := TORMTools.GetFieldNameByPropName(FKey.PropName);

      aForEachFKeyProc(FieldName, FKey);
      Result := True;

      Break;
    end;

  if Result then
  begin
    FOwnerEntity := aOwnerEntity;
    FEagerLoadingMode := aOwnerEntity.FEagerLoadingMode;

    FOwnerEntity.FFreeListProcs.Add(Free);
    FOwnerEntity.FStoreListProcs.Add(Store);
    FOwnerEntity.FRevertListProcs.Add(Revert);
  end;
end;

function TEntityListBase<T>.GetNextRecNum: Integer;
begin
  if FCanLoadMore and Assigned(FBuilder) then
    Result := FOffset + FBuilder.Limit
  else
    Result := Count;
end;

function TEntityListBase<T>.CanLoadMore(out aNextRecNum: Integer): Boolean;
begin
  Result := FCanLoadMore;
  if Result then
    aNextRecNum := GetNextRecNum;
end;

procedure TEntityListBase<T>.LoadList;
var
  Entity: TEntityAbstract;
  Instance: TInstance;
  Instances: TInstanceList;
  QueryKeeper: IQueryKeeper;
  sSQL: string;
begin
  gRttiContext := TRttiContext.Create;
  try
    FBuilder.RecursiveJoinTables(GetEntityClass);
    sSQL := FBuilder.BuildSQL;
    if sSQL.IsEmpty then
      Exit;

    QueryKeeper := MakeQueryKeeper;
    QueryKeeper.Query.SQL.Text := sSQL;
    FBuilder.FillParams(QueryKeeper.Query);

    FDBEngine.OpenQuery(QueryKeeper.Query);
    Instances := FBuilder.EncodeInstancesFromQuery(QueryKeeper);
    FBuilder.DecodeInstanceList(FDBEngine, nil{aOwnerEntity}, Self, Instances);

    if Assigned(FBuilder) and (FBuilder.Limit > 0) then
      FCanLoadMore := QueryKeeper.Query.RecordCount = FBuilder.Limit;
  finally
    gRttiContext.Free;

    if FEagerLoadingMode then
      FEagerLoadingMode := False;
  end;
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

{ TEntitySelectBuilder }

function TEntitySelectBuilder.GetClassByName(const aQualifiedName: string): TClass;
begin
  Result := gRttiContext.FindType(aQualifiedName).AsInstance.MetaclassType;
end;

procedure TEntitySelectBuilder.DecodeInstanceList(aDBEngine: TDBEngine;
  aOwnerEntity: TEntityAbstract; aEntityList: TObject; const aInstanceList: TInstanceList);
var
  Entity: TEntityAbstract;
  EntityClass: TEntityClass;
  Instance: TInstance;
  Instances: TInstanceList;
begin
  if not aInstanceList.NeedToMount then
    Exit;

  if not Assigned(aEntityList) then
  begin
    aOwnerEntity.FEagerLoadingMode := True;

    aEntityList := gRttiContext
      .GetType(aOwnerEntity.ClassInfo)
      .AsInstance.GetProperty(aInstanceList.PropName)
      .GetValue(aOwnerEntity)
      .AsObject;
  end;

  for Instance in aInstanceList.List do
  begin
    EntityClass := TEntityClass(GetClassByName(aInstanceList.EntityClassName));
    Entity := EntityClass.CreateByInstance(aDBEngine, Instance);

    for Instances in Instance.FInstanceLists do
      DecodeInstanceList(aDBEngine, Entity, nil{aEntityList}, Instances);

    DecodeInstanceProps(aDBEngine, Entity, Instance.FInstanceProps);

    gRttiContext
      .GetType(aEntityList.ClassInfo)
      .GetMethod('Add')
      .Invoke(aEntityList, [Entity]);
  end;
end;

procedure TEntitySelectBuilder.DecodeInstanceProps(aDBEngine: TDBEngine;
  aOwnerEntity: TEntityAbstract; const aInstanceProps: TArray<TInstanceProp>);
var
  Entity: TEntityAbstract;
  EntityClass: TEntityClass;
  InstanceProp: TInstanceProp;
  Instances: TInstanceList;
begin
  for InstanceProp in aInstanceProps do
  begin
    if not InstanceProp.NeedToMount then
      Continue;

    EntityClass := TEntityClass(GetClassByName(InstanceProp.EntityClassName));
    Entity := EntityClass.CreateByInstance(aDBEngine, InstanceProp.Instance);

    for Instances in InstanceProp.Instance.FInstanceLists do
      DecodeInstanceList(aDBEngine, Entity, nil{aEntityList}, Instances);

    DecodeInstanceProps(aDBEngine, Entity, InstanceProp.Instance.FInstanceProps);

    gRttiContext
      .GetType(aOwnerEntity.ClassInfo)
      .AsInstance.GetProperty(InstanceProp.PropName)
      .SetValue(aOwnerEntity, Entity);

    Entity.FOwner := aOwnerEntity;
  end;
end;

function TEntitySelectBuilder.GetHash(aQuery: TFDQuery; const aPrimaryKey: TPrimaryKey;
  const aAlias: string): string;
var
  FieldNameWithAlias: string;
  KeyField: TKeyField;
begin
  Result := aAlias;

  for KeyField in aPrimaryKey do
  begin
    FieldNameWithAlias := TORMTools.GetFieldNameWithAlias(aAlias, KeyField.PropName);
    Result := Result + aQuery.FieldByName(FieldNameWithAlias).AsString;
  end;

  Result := TStringTools.GetHash16(Result);
end;

function TEntitySelectBuilder.EncodeInstancesFromQuery(aQueryKeeper: IQueryKeeper): TInstanceList;

  function EncodeInstanceProp(const aJoinEntItem: TJoinEntItem; var aProps: TArray<TInstanceProp>): TInstance;
  var
    i: Integer;
    InstanceProp: TInstanceProp;
  begin
    Result := nil;

    for i := Length(aProps) - 1 downto 0 do
    begin
      if aProps[i].Alias = aJoinEntItem.Alias then
      begin
        Result := aProps[i].Instance;
        Exit;
      end;
    end;

    if not Assigned(Result) then
    begin
      Result := TInstance.Create(aQueryKeeper, aJoinEntItem.Alias);

      InstanceProp.Alias := aJoinEntItem.Alias;
      InstanceProp.Instance := Result;
      InstanceProp.EntityClassName := aJoinEntItem.EntityClass.QualifiedClassName;
      InstanceProp.PropName := aJoinEntItem.OwnerPropName;

      aProps := aProps + [InstanceProp];
    end;
  end;

  function EncodeInstanceList(const aJoinEntItem: TJoinEntItem; var aLists: TArray<TInstanceList>): TInstance;
  var
    Hash: string;
    i: Integer;
    ListIndex: Integer;
    List: TInstanceList;
  begin
    ListIndex := -1;
    for i := Length(aLists) - 1 downto 0 do
    begin
      if aLists[i].Alias = aJoinEntItem.Alias then
      begin
        ListIndex := i;
        List := aLists[ListIndex];
        Break;
      end;
    end;

    if ListIndex = -1 then
    begin
      List.Init;
      List.Alias := aJoinEntItem.Alias;
      List.EntityClassName := aJoinEntItem.EntityClass.QualifiedClassName;
      List.EntityListClassName := aJoinEntItem.OwnerEntityListClass.QualifiedClassName;
      List.PropName := aJoinEntItem.OwnerPropName;
      aLists := aLists + [List];
      ListIndex := 0;
    end;

    Hash := GetHash(aQueryKeeper.Query, aJoinEntItem.EntityClass.GetPrimaryKey, aJoinEntItem.Alias);
    Result := List.FindByHash(Hash);

    if not Assigned(Result) then
    begin
      Result := TInstance.Create(aQueryKeeper, aJoinEntItem.Alias);
      Result.FHash := Hash;

      List.List := List.List + [Result];
      aLists[ListIndex] := List;
    end;
  end;

var
  Instance: TInstance;
  JoinEntItem: TJoinEntItem;
  Lists: TArray<TInstanceList>;
begin
  Result.Init;
  Result.Alias := FAlias;
  Result.EntityClassName := FEntityClass.QualifiedClassName;
  Lists := [Result];

  aQueryKeeper.Query.First;
  while not aQueryKeeper.Query.EOF do
  begin
    JoinEntItem.Init;
    JoinEntItem.Alias := FAlias;
    JoinEntItem.EntityClass := FEntityClass;
    Instance := EncodeInstanceList(JoinEntItem, Lists);

    for JoinEntItem in FJoinEntItems do
    begin
      if Assigned(JoinEntItem.OwnerEntityListClass) then
        Instance := EncodeInstanceList(JoinEntItem, Instance.FInstanceLists)
      else
        Instance := EncodeInstanceProp(JoinEntItem, Instance.FInstanceProps)
    end;

    aQueryKeeper.Query.Next;
  end;

  Result := Lists[0];
end;

function TEntitySelectBuilder.IsEntityClass(aClass: TClass; out aEntityClass: TEntityClass): Boolean;
begin
  Result := aClass.InheritsFrom(TEntityAbstract);
  if Result then
    aEntityClass := TEntityClass(aClass);
end;

function TEntitySelectBuilder.IsEntityListClass(aClass: TClass; out aEntityClass: TEntityClass): Boolean;
var
  GetEntityClassMethod: TRttiMethod;
begin
  GetEntityClassMethod := gRttiContext.GetType(aClass).GetMethod('GetEntityClass');
  Result := Assigned(GetEntityClassMethod);

  if Result then
    aEntityClass := GetEntityClassMethod.Invoke(aClass, []).AsType<TEntityClass>;
end;

function TEntitySelectBuilder.HasReference(aEntityClass, aReferEntityClass: TEntityClass; out aFKey: TFKey): Boolean;
var
  FKey: TFKey;
  FKeys: TFKeys;
begin
  Result := False;

  FKeys := aEntityClass.GetStructure.FKeys;
  for FKey in FKeys do
    if FKey.ReferEntityClass = aReferEntityClass then
    begin
      aFKey := FKey;
      Exit(True);
    end;
end;

function TEntitySelectBuilder.GetEagerLoadingItems(aEntityClass: TEntityClass): TArray<TJoinEntItem>;

  function HasEagerLoadingAttribute(aRttiProperty: TRttiProperty): Boolean;
  var
    Attribute: TCustomAttribute;
  begin
    Result := False;

    for Attribute in aRttiProperty.GetAttributes do
      if Attribute is EagerLoading then
        Exit(True);
  end;

var
  FKey: TFKey;
  JoinEntItem: TJoinEntItem;
  Matched: Boolean;
  PropEntityClass: TEntityClass;
  RttiProperties: TArray<TRttiProperty>;
  RttiProperty: TRttiProperty;
begin
  Result := [];

  RttiProperties := gRttiContext.GetType(aEntityClass.ClassInfo).GetDeclaredProperties;
  for RttiProperty in RttiProperties do
  begin
    if RttiProperty.PropertyType.IsInstance and HasEagerLoadingAttribute(RttiProperty) then
    begin
      Matched := False;
      JoinEntItem.Init;

      if IsEntityClass(RttiProperty.PropertyType.AsInstance.MetaclassType, {out}PropEntityClass) and
         (HasReference(aEntityClass, PropEntityClass, {out}FKey) or HasReference(PropEntityClass, aEntityClass, {out}FKey))
      then
      begin
        Matched := True;
      end
      else
      if IsEntityListClass(RttiProperty.PropertyType.AsInstance.MetaclassType, {out}PropEntityClass) and
         (HasReference(aEntityClass, PropEntityClass, {out}FKey) or HasReference(PropEntityClass, aEntityClass, {out}FKey))
      then
      begin
        Matched := True;
        JoinEntItem.OwnerEntityListClass := RttiProperty.PropertyType.AsInstance.MetaclassType;
      end;

      if Matched then
      begin
        JoinEntItem.EntityClass := PropEntityClass;
        JoinEntItem.OwnerEntityClass := aEntityClass;
        JoinEntItem.OwnerPropName := RttiProperty.Name;

        Result := Result + [JoinEntItem];
      end;
    end;
  end;
end;

procedure TEntitySelectBuilder.DoRecursiveJoinTables(aEntityClass: TEntityClass;
  var aIndex: Integer; const aOwnerAlias: string);
var
  EagerLoadingItems: TArray<TJoinEntItem>;
  i: Integer;
begin
  EagerLoadingItems := GetEagerLoadingItems(aEntityClass);

  for i := 0 to Length(EagerLoadingItems) - 1 do
  begin
    Inc(aIndex);

    EagerLoadingItems[i].Index := aIndex;
    EagerLoadingItems[i].Alias := GetAlias(aIndex);
    EagerLoadingItems[i].OwnerAlias := aOwnerAlias;

    FJoinEntItems := FJoinEntItems + EagerLoadingItems;

    DoRecursiveJoinTables(EagerLoadingItems[i].EntityClass, aIndex, EagerLoadingItems[i].Alias);
  end;
end;

function TEntitySelectBuilder.GetAlias(const aIndex: Integer): string;
begin
  if aIndex = 0 then
    Result := FAlias
  else
    Result := Format('T%d', [aIndex]);
end;

function TEntitySelectBuilder.AddAndWhere(const aAlias, aPropName: string;
  const aEquality: TEquality; const aParamName: string): IEntitySelectBuilder;
begin
  FQueryBuilder.AddAndWhere(
    aAlias,
    TORMTools.GetFieldNameByPropName(aPropName),
    aEquality,
    aParamName
  );

  Result := Self;
end;

function TEntitySelectBuilder.AddCount(const aAlias, aPropName,
  aAsFieldName: string): IEntitySelectBuilder;
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

function TEntitySelectBuilder.JoinEntity(aEntityClass: TEntityClass;
  const aAlias: string; const aOwnerAlias: string): IEntitySelectBuilder;
var
  JoinEntItems: TJoinEntItem;
begin
  JoinEntItems.Init;
  JoinEntItems.Alias := aAlias;
  JoinEntItems.OwnerAlias := aOwnerAlias;
  JoinEntItems.EntityClass := aEntityClass;
  FJoinEntItems := FJoinEntItems + [JoinEntItems];

  Result := Self;
end;

function TEntitySelectBuilder.AddOrderBy(
  const aOrderItem: TOrderItem): IEntitySelectBuilder;
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

function TEntitySelectBuilder.AddOrWhere(const aAlias, aPropName: string;
  const aEquality: TEquality; const aParamName: string): IEntitySelectBuilder;
begin
  FQueryBuilder.AddOrWhere(
    aAlias,
    TORMTools.GetFieldNameByPropName(aPropName),
    aEquality,
    aParamName
  );

  Result := Self;
end;

procedure TEntitySelectBuilder.AfterConstruction;
begin
  inherited;

  FQueryBuilder := MakeQueryBuilder;
  FJoinEntItems := [];
  FAgrFields := [];
end;

function TEntitySelectBuilder.BuildSQL: string;

  procedure AddToSelectAndGroupBy(aEntityClass: TEntityClass; const aAlias: string);
  var
    FieldName: string;
    FieldNameWithAlias: string;
    Prop: string;
    PublishedProps: TArray<string>;
  begin
    PublishedProps := TORMTools.GetPublishedProps(aEntityClass);
    for Prop in PublishedProps do
    begin
      if not FAgrFields.Contains(Prop) then
      begin
        FieldName := TORMTools.GetFieldNameByPropName(Prop);
        FieldNameWithAlias := TORMTools.GetFieldNameWithAlias(aAlias, Prop);

        FQueryBuilder.AddSelect(aAlias, FieldName, FieldNameWithAlias);

        if FNeedGroupBy then
          FQueryBuilder.AddGroupBy(aAlias, FieldNameWithAlias);
      end;
    end;
  end;

  function GetReferEntItem(const aAlias: string): TJoinEntItem;
  var
    EntItem: TJoinEntItem;
  begin
    for EntItem in FJoinEntItems do
      if EntItem.Alias = aAlias then
        Exit(EntItem);

    Result.Alias := FAlias;
    Result.EntityClass := FEntityClass;
  end;

var
  FKey: TFKey;
  FoundInFK: Boolean;
  JoinEntItem: TJoinEntItem;
  PropName: string;
  ReferEntItem: TJoinEntItem;
  ReferPropName: string;
begin
  AddToSelectAndGroupBy(FEntityClass, FAlias);

  for JoinEntItem in FJoinEntItems do
  begin
    FoundInFK := False;

    ReferEntItem := GetReferEntItem(JoinEntItem.OwnerAlias);

    if HasReference(JoinEntItem.EntityClass, ReferEntItem.EntityClass, {out}FKey) then
    begin
      PropName := FKey.PropName;
      ReferPropName := FKey.ReferPropName;
      FoundInFK := True;
    end
    else
    if HasReference(ReferEntItem.EntityClass, JoinEntItem.EntityClass, {out}FKey) then
    begin
      PropName := FKey.ReferPropName;
      ReferPropName := FKey.PropName;
      FoundInFK := True;
    end;

    if FoundInFK then
    begin
      FQueryBuilder.AddLeftJoin(
        JoinEntItem.EntityClass.GetTableName,
        JoinEntItem.Alias,
        TORMTools.GetFieldNameByPropName(PropName),
        ReferEntItem.Alias,
        TORMTools.GetFieldNameByPropName(ReferPropName)
      );

      AddToSelectAndGroupBy(JoinEntItem.EntityClass, JoinEntItem.Alias);
    end;
  end;

  Result := FQueryBuilder.BuildSQL;
end;

procedure TEntitySelectBuilder.SetOrderBy(const aOrderBy: POrder);
var
  OrderItem: TOrderItem;
begin
  if not Assigned(aOrderBy) then
    Exit;

  for OrderItem in aOrderBy.OrderItems do
    AddOrderBy(OrderItem);
end;

procedure TEntitySelectBuilder.FillParams(aQuery: TFDQuery);
begin
  FQueryBuilder.FillParams(aQuery);
end;

procedure TEntitySelectBuilder.RecursiveJoinTables(aEntityClass: TEntityClass);
var
  Index: Integer;
  OwnerAlias: string;
begin
  FEntityClass := aEntityClass;
  FQueryBuilder.FromTable(aEntityClass.GetTableName, FAlias);

  Index := 0;
  OwnerAlias := GetAlias(Index);

  DoRecursiveJoinTables(aEntityClass, {var}Index, {var}OwnerAlias);
end;

function TEntitySelectBuilder.GetLimit: Integer;
begin
  Result := FQueryBuilder.Limit;
end;

function TEntitySelectBuilder.GetOffset: Integer;
begin
  Result := FQueryBuilder.Offset;
end;

function TEntitySelectBuilder.SetLimit(const aValue: Integer): IEntitySelectBuilder;
begin
  FQueryBuilder.SetLimit(aValue);

  Result := Self;
end;

function TEntitySelectBuilder.SetOffset(
  const aValue: Integer): IEntitySelectBuilder;
begin
  FQueryBuilder.SetOffset(aValue);

  Result := Self;
end;

function TEntitySelectBuilder.SetParam(const aParamName: string;
  const aValue: Variant): IEntitySelectBuilder;
begin
  FQueryBuilder.SetParam(aParamName, aValue);

  Result := Self;
end;

{ TEntityKeeper<T> }

constructor TEntityKeeper<T>.Create(aEntity: T);
var
  EntityFreeNotify: ISourceFreeNotification;
begin
  FEntity := aEntity;
  FDBEngine := aEntity.FDBEngine;
  FPKeyValues := aEntity.GetPKeyValues;

  if aEntity.GetInterface(ISourceFreeNotification, EntityFreeNotify) then
    EntityFreeNotify.AddFreeNotify(EntityFreeNotification);
end;

destructor TEntityKeeper<T>.Destroy;
begin
  if FIsEntityOwner then
    FEntity.Free;

  inherited;
end;

procedure TEntityKeeper<T>.EntityFreeNotification(Sender: TObject);
begin
  FEntity := nil;
end;

function TEntityKeeper<T>.GetOne: T;
begin
  if not Assigned(FEntity) then
  begin
    FEntity := T.CreateByKeeper(FDBEngine, FPKeyValues);
    FIsEntityOwner := True;
  end;

  Result := FEntity;
end;

{ TInstanceField }

procedure TInstanceField.Init;
begin
  BlobStream := nil;
  FieldName := '';
  FieldType := ftUnknown;
  Value := Null;
end;

{ DefaultValue }

constructor DefaultValue.Create(const aValue: Variant);
begin
  FValue := aValue;
end;

{ TJoinEntItem }

procedure TJoinEntItem.Init;
begin
  Alias := '';
  EntityClass := nil;
  Index := 0;
  OwnerAlias := '';
  OwnerEntityClass := nil;
  OwnerEntityListClass := nil;
  OwnerPropName := '';
end;

{TInstanceList}

function TInstanceList.NeedToMount: Boolean;
begin
  Result := Length(List) > 0;
end;

procedure TInstanceList.Init;
begin
  EntityClassName := '';
  EntityListClassName := '';
  List := [];
  Alias := '';
  PropName := '';
end;

function TInstanceList.Count: Integer;
begin
  Result := Length(List);
end;

function TInstanceList.FindByHash(const aHash: string): TInstance;
var
  Instance: TInstance;
begin
  Result := nil;

  for Instance in List do
    if Instance.FHash = aHash then
      Exit(Instance);
end;

{TInstanceProp}

function TInstanceProp.NeedToMount: Boolean;
begin
  Result := Assigned(Instance);
end;

end.
