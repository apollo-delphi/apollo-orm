unit Apollo_ORM;

interface

uses
  Apollo_DB_Core,
  Apollo_Types,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  System.Classes,
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
    PropNameForJoinedEntity: string;
  end;

  TFKeys = TArray<TFKey>;

  TFKeysHelper = record helper for TFKeys
    procedure Add(const aPropName: string; aReferEntityClass: TEntityClass;
      const aReferPropName: string; aRelType: TRelType = rtUnknown;
      aPropNameForJoinedEntity: string = '');
    function Contains(const aPropName: string): Boolean;
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

  IQueryKeeper = interface
    function GetQuery: TFDQuery;
    property Query: TFDQuery read GetQuery;
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
    constructor Create(aBlobStream: TStream);
    destructor Destroy; override;
    property Stream: TStream read GetStream write SetStream;
  end;

  TForEachJoinedEntityProc = procedure(aJoinedEntity: TEntityAbstract;
    aEntityClass: TEntityClass; const aKeyPropName, aEntityPropName, aReferPropName: string) of object;

  TForEachBlobProp = reference to procedure(aBlob: TORMBlob);

{$M+}
  TEntityAbstract = class abstract(TIntefacedObjectNotUsingReference, ISourceFreeNotification)
  private
    FDBEngine: TDBEngine;
    FFreeListProcs: TSimpleMethods;
    FFreeNotifications: TNotifyEvents;
    FInstance: TInstance;
    FIsNew: Boolean;
    FRevertListProcs: TSimpleMethods;
    FStoreListProcs: TSimpleMethods;
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
    procedure AssignBlobPropsFromInstance;
    procedure AssignInstanceFromProps;
    procedure AssignProps;
    procedure AssignPropsFromInstance;
    procedure AssignPropsFromPKeyValues(const aPKeyValues: TArray<Variant>);
    procedure CreateJoinedEntity(aJoinedEntity: TEntityAbstract;
      aEntityClass: TEntityClass; const aKeyPropName, aEntityPropName, aReferPropName: string);
    procedure ExecToDB(const aSQL: string);
    procedure FillParam(aParam: TFDParam; const aPropName: string; aValue: Variant);
    procedure ForEachBlobProp(aForEachBlobProp: TForEachBlobProp);
    procedure ForEachJoinedChildProp(aForEachJoinedEntityProc: TForEachJoinedEntityProc);
    procedure ForEachJoinedParentProp(aForEachJoinedEntityProc: TForEachJoinedEntityProc);
    procedure FreeBlobProps;
    procedure FreeFInstance;
    procedure FreeJoinedEntity(aJoinedEntity: TEntityAbstract;
      aEntityClass: TEntityClass; const aKeyPropName, aEntityPropName, aReferPropName: string);
    procedure InsertToDB;
    procedure ReadInstance(const aPKeyValues: TArray<Variant>);
    procedure SetBlobProp(const aPropName: string; aBlob: TORMBlob);
    procedure SetProp(const aPropName: string; aValue: Variant);
    procedure StoreJoinedChildEnt(aJoinedEntity: TEntityAbstract;
      aEntityClass: TEntityClass; const aKeyPropName, aEntityPropName, aReferPropName: string);
    procedure StoreJoinedParentEnt(aJoinedEntity: TEntityAbstract;
      aEntityClass: TEntityClass; const aKeyPropName, aEntityPropName, aReferPropName: string);
    procedure UpdateToDB;
  protected
    procedure AfterCreate; virtual;
    procedure ApplyAutoGenFields; virtual;
    procedure BeforeDelete; virtual;
    procedure BeforeDestroy; virtual;
  public
    class function GetPrimaryKey: TPrimaryKey; virtual;
    class function GetStructure: TStructure; virtual; abstract;
    class function GetTableName: string;
    class procedure RegisterForService;
    function PropExists(const aPropName: string): Boolean;
    procedure AddFreeNotify(aNotifyEvent: TNotifyEvent);
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

  TOrderItem = record
    IsDESC: Boolean;
    PropName: string;
  end;

  POrder = ^TOrder;
  TOrder = record
  private
    FOrderItems: TArray<TOrderItem>;
  public
    class function New: TOrder; static;
    function Add(const aPropName: string): POrder;
    function AddDESC(const aPropName: string): POrder;
  end;

  TEntityListBase<T: TEntityAbstract> = class(TEntityListAbstract<T>)
  private
    FDBEngine: TDBEngine;
    FFKey: TFKey;
    FOwnerEntity: TEntityAbstract;
    FRecycleBin: TArray<T>;
    function GetSelectSQL(const aFilter: TFilter; const aOrder: POrder): string;
    procedure CleanRecycleBin;
    procedure FillList(const aFilter: TFilter; const aOrder: POrder);
  public
    procedure Clear;
    procedure Delete(const aEntity: T); overload;
    procedure Delete(const aIndex: Integer); overload;
    procedure DeleteAll;
    procedure Remove(const aEntity: T); overload;
    procedure Remove(const aIndex: Integer); overload;
    procedure Revert;
    procedure Store;
    constructor Create(aDBEngine: TDBEngine; const aFilter: TFilter;
      const aOrderBy: POrder = nil); overload;
    constructor Create(aOwnerEntity: TEntityAbstract); overload;
    destructor Destroy; override;
  end;

function MakeQueryKeeper(aQuery: TFDQuery): IQueryKeeper;

implementation

uses
  Apollo_Helpers,
  Apollo_ORM_Exception,
  System.Character,
  System.SysUtils,
  System.TypInfo,
  System.Variants;

type
  TQueryKeeper = class(TInterfacedObject, IQueryKeeper) 
  private
    FQuery: TFDQuery;
    function GetQuery: TFDQuery;
    constructor Create(aQuery: TFDQuery); 
    destructor Destroy; override;
  end;

function MakeQueryKeeper(aQuery: TFDQuery): IQueryKeeper;
begin
  Result := TQueryKeeper.Create(aQuery);
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
      FInstance.Fields[i].Value := Prop[PropName];
  end;
end;

procedure TEntityAbstract.AssignProps;
begin
  AssignPropsFromInstance;
  AssignBlobPropsFromInstance;
  ForEachJoinedParentProp(CreateJoinedEntity);
  ForEachJoinedChildProp(CreateJoinedEntity);
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
  FFreeNotifications.Exec;
end;

constructor TEntityAbstract.Create(aDBEngine: TDBEngine; const aInstance: TInstance);
begin
  FDBEngine := aDBEngine;
  FInstance := aInstance;
  FIsNew := False;

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

procedure TEntityAbstract.CreateJoinedEntity(aJoinedEntity: TEntityAbstract;
  aEntityClass: TEntityClass; const aKeyPropName, aEntityPropName, aReferPropName: string);
var
  dsQuery: TFDQuery;
  Instance: TInstance;
  KeyValue: Variant;
  QueryKeeper: IQueryKeeper;
  SQL: string;
begin
  if aJoinedEntity <> nil then
    Exit;

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

      aJoinedEntity := aEntityClass.Create(FDBEngine, Instance);

      SetObjectProp(Self, aEntityPropName, aJoinedEntity);
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

  FreeBlobProps;
  FreeFInstance;
  ForEachJoinedParentProp(FreeJoinedEntity);
  ForEachJoinedChildProp(FreeJoinedEntity);
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
    ftBoolean: aParam.AsBoolean := aValue;
    ftString, ftWideString, ftWideMemo:
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

procedure TEntityAbstract.ForEachJoinedChildProp(
  aForEachJoinedEntityProc: TForEachJoinedEntityProc);
var
  Entity: TEntityAbstract;
  EntityClass: TEntityClass;
  FKey: TFKey;
  FKeys: TFKeys;
  i: Integer;
  PropCount: Integer;
  PropClass: TClass;
  PropList: PPropList;
  PropName: string;
begin
  PropCount := GetPropList(Self, PropList);
  try
    for i := 0 to PropCount - 1 do
    begin
      if PropList^[i].PropType^.Kind = tkClass then
      begin
        PropClass := GetObjectPropClass(Self, PropList^[i]);

        if PropClass.InheritsFrom(TEntityAbstract) then
        begin
          EntityClass := TEntityClass(PropClass);
          FKeys := EntityClass.GetStructure.FKeys;

          for FKey in FKeys do
            if (Self.ClassType = FKey.ReferEntityClass) and
               (FKey.RelType <> rtOne2Many)
            then
            begin
              PropName := GetPropName(PropList^[i]);
              Entity := GetObjectProp(Self, PropName) as EntityClass;

              aForEachJoinedEntityProc(
                Entity,
                EntityClass,
                FKey.PropName,
                PropName,
                FKey.ReferPropName
              );
            end;
        end;
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

procedure TEntityAbstract.ForEachJoinedParentProp(
  aForEachJoinedEntityProc: TForEachJoinedEntityProc);
var
  Entity: TEntityAbstract;
  FKey: TFKey;
  FKeys: TFKeys;
  i: Integer;
  PropCount: Integer;
  PropClass: TClass;
  PropList: PPropList;
  PropName: string;
begin
  PropCount := GetPropList(Self, PropList);
  try
    FKeys := GetStructure.FKeys;

    for FKey in FKeys do
    begin
      if FKey.RelType = rtOne2Many then
        Continue;

      for i := 0 to PropCount - 1 do
      begin
        PropClass := GetObjectPropClass(Self, PropList^[i]);
        PropName := GetPropName(PropList^[i]);

        if (FKey.ReferEntityClass = PropClass) and
           ((FKey.PropNameForJoinedEntity = PropName) or FKey.PropNameForJoinedEntity.IsEmpty)
        then
        begin
          Entity := GetObjectProp(Self, PropName) as FKey.ReferEntityClass;

          aForEachJoinedEntityProc(
            Entity,
            FKey.ReferEntityClass,
            FKey.PropName,
            PropName,
            FKey.ReferPropName
          );
        end;
      end;
    end;
  finally
    FreeMem(PropList);
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

procedure TEntityAbstract.FreeJoinedEntity(aJoinedEntity: TEntityAbstract;
  aEntityClass: TEntityClass; const aKeyPropName, aEntityPropName,
  aReferPropName: string);
begin
  if Assigned(aJoinedEntity) then
    FreeAndNil(aJoinedEntity);
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
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Self, aPropName);

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

class procedure TEntityAbstract.RegisterForService;
begin
  //required to call in initialization section if ORM Service will be used
end;

procedure TEntityAbstract.Revert;
begin
  if FIsNew then
    Exit;

  AssignPropsFromInstance;
  FRevertListProcs.Exec;
end;

procedure TEntityAbstract.SetBlobProp(const aPropName: string; aBlob: TORMBlob);
begin
  SetObjectProp(Self, aPropName, aBlob);
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
  ForEachJoinedParentProp(StoreJoinedParentEnt);
  Store;
  ForEachJoinedChildProp(StoreJoinedChildEnt);
  FStoreListProcs.Exec;  
end;

procedure TEntityAbstract.StoreJoinedChildEnt(aJoinedEntity: TEntityAbstract;
  aEntityClass: TEntityClass; const aKeyPropName, aEntityPropName,
  aReferPropName: string);
begin
  if not Assigned(aJoinedEntity) then
    Exit;

  aJoinedEntity.Prop[aReferPropName] := Prop[aKeyPropName];
  aJoinedEntity.StoreAll;
end;

procedure TEntityAbstract.StoreJoinedParentEnt(aJoinedEntity: TEntityAbstract;
  aEntityClass: TEntityClass; const aKeyPropName, aEntityPropName,
  aReferPropName: string);
begin
  if not Assigned(aJoinedEntity) then
    Exit;

  aJoinedEntity.StoreAll;
  Prop[aKeyPropName] := aJoinedEntity.Prop[aReferPropName];
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
  aReferEntityClass: TEntityClass; const aReferPropName: string; aRelType: TRelType;
  aPropNameForJoinedEntity: string);
var
  FKey: TFKey;
begin
  FKey.PropName := aPropName;
  FKey.ReferEntityClass := aReferEntityClass;
  FKey.ReferPropName := aReferPropName;
  FKey.RelType := aRelType;
  FKey.PropNameForJoinedEntity := aPropNameForJoinedEntity;

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
  const aOrderBy: POrder);
begin
  inherited Create(True);

  FDBEngine := aDBEngine;
  FillList(aFilter, aOrderBy);
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

  if Length(aFilter.FParamValues) <> dsQuery.Params.Count then
    raise EORMWrongInputCount.Create;

  for i := 0 to dsQuery.Params.Count - 1 do
    dsQuery.Params.Items[i].Value := aFilter.FParamValues[i];

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

  case aFilter.FFilterMode of
    fmUnknown: Exit('');
    fmGetAll: WherePart := '';
    fmGetWhere: WherePart := ' WHERE ' + aFilter.FWhereString;
  end;

  if Assigned(aOrder) and (Length(aOrder.FOrderItems) > 0) then
  begin
    OrderPart := ' ORDER BY';
    for i := Low(aOrder.FOrderItems) to High(aOrder.FOrderItems) do
    begin
      if i > 0 then
        OrderPart := OrderPart + ',';
      OrderItem := aOrder.FOrderItems[i];
      OrderPart := OrderPart + Format(' %s', [TORMTools.GetFieldNameByPropName(OrderItem.PropName)]);
      if OrderItem.IsDESC then
        OrderPart := OrderPart + ' DESC';
    end;
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

{ TOrder }

function TOrder.Add(const aPropName: string): POrder;
var
  OrderItem: TOrderItem;
begin
  OrderItem.IsDESC := False;
  OrderItem.PropName := aPropName;

  FOrderItems := FOrderItems + [OrderItem];

  Result := @Self;
end;

function TOrder.AddDESC(const aPropName: string): POrder;
var
  OrderItem: TOrderItem;
begin
  OrderItem.IsDESC := True;
  OrderItem.PropName := aPropName;

  FOrderItems := FOrderItems + [OrderItem];

  Result := @Self;
end;

class function TOrder.New: TOrder;
var
  Order: TOrder;
begin
  Order.FOrderItems := [];
  Result := Order;
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

procedure TORMBlob.SetStream(aStream: TStream);
begin
  FreeModifiedStream;
  FModifiedStream := aStream;
end;

{ TQueryKeeper }

constructor TQueryKeeper.Create(aQuery: TFDQuery);
begin
  inherited Create;

  FQuery := aQuery;
end;

destructor TQueryKeeper.Destroy;
begin
  FQuery.Free;

  inherited;
end;

function TQueryKeeper.GetQuery: TFDQuery;
begin
  Result := FQuery;
end;

end.
