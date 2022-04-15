unit Apollo_ORM_Service;

interface

uses
  Apollo_DB_Core,
  Apollo_DB_Utils,
  Apollo_ORM,
  System.Generics.Collections,
  System.Rtti;

type
{$M+}
  TORMField = class(TEntityAbstract)
  strict private
    FFieldName: string;
    FORMUniqueID: string;
    FTableID: string;
  public
    class function GetStructure: TStructure; override;
  published
    [NotNull]
    property FieldName: string read FFieldName write FFieldName;
    property ORMUniqueID: string read FORMUniqueID write FORMUniqueID;
    property TableID: string read FTableID write FTableID;
  end;

  TORMFieldList = class(TEntityListBase<TORMField>);

  TORMTable = class(TEntityAbstract)
  strict private
    FORMFields: TORMFieldList;
    FORMUniqueID: string;
    FTableName: string;
    function GetORMFields: TORMFieldList;
  public
    function GetORMField(const aORMUniqueID: string): TORMField;
    class function GetStructure: TStructure; override;
    property ORMFields: TORMFieldList read GetORMFields;
  published
    property ORMUniqueID: string read FORMUniqueID write FORMUniqueID;
    [NotNull]
    property TableName: string read FTableName write FTableName;
  end;

  TORMTransfer = class(TEntityAbstract)
  strict private
    FNum: Integer;
    FSQLCode: string;
  public
    class function GetStructure: TStructure; override;
  published
    [NotNull]
    property Num: Integer read FNum write FNum;
    [Text]
    property SQLCode: string read FSQLCode write FSQLCode;
  end;
{$M-}

  TTransferData = TDictionary<string, IQueryKeeper>;

  TInitDataItem = record
    EntityClass: TEntityClass;
    PropNames: TArray<string>;
    PropValues: TArray<TArray<Variant>>;
    procedure Init;
  end;

  TInitData = record
  private
    Data: TArray<TInitDataItem>;
  public
    procedure AddInitData(aEntityClass: TEntityClass; const aPropNames: TArray<string>;
      const aPropValues: TArray<TArray<Variant>>);
  end;

  THandleEntityFieldProc = reference to procedure(aRttiProperty: TRttiProperty);

  TORMService = class
  private
    FDBEngine: TDBEngine;
    FMaxDataTransferNum: Integer;
    FNextMaxDataTransferNum: Integer;
    FRttiContext: TRttiContext;
    FTransferData: TTransferData;
    function GetFieldDef(aRttiProperty: TRttiProperty): TFieldDef;
    function GetORMDef(aEntityClass: TEntityClass): TTableDef;
    function GetORMTable(const aORMUniqueID: string): TORMTable;
    function GetORMUniqueID(aRttiObject: TRttiObject): string;
    function GetPrimaryKey(aEntityType: TRttiInstanceType): TPrimaryKey;
    function GetSQLFieldType(aRttiProperty: TRttiProperty): string;
    function GetTableDef(const aStructure: TStructure; aORMTable: TORMTable;
      aEntityType: TRttiInstanceType): TTableDef;
    function GetTypesInheritFrom(aClass: TClass): TArray<TRttiInstanceType>;
    function HandleEntityType(aEntityType: TRttiInstanceType): string;
    function HandleTransferNum(const aNum: Integer): Boolean;
    function IsAbstract(aRttiObject: TRttiObject): Boolean;
    function IsORMEntity(aClass: TClass): Boolean;
    function TryGetIndexDef(aRttiProperty: TRttiProperty; out aIndexDef: TIndexDef): Boolean;
    function TryGetStructure(aEntityType: TRttiInstanceType; out aStructure: TStructure): Boolean;
    procedure CreateORMTablesIfNotExist;
    procedure HandleORMTransferRecord(const aSQLCode: string);
    procedure HandleEntityFields(aEntityProperties: TArray<TRttiProperty>;
      aHandleEntityFieldProc: THandleEntityFieldProc);
    procedure HandleInitData(aEntityClass: TEntityClass);
  protected
    function FinishTransfer(const aNum: Integer): Boolean;
    function GetInitData: TInitData; virtual;
    function MakeQuery(const aTableName: string): IQueryKeeper;
    function StartTransfer(const aNum: Integer): Boolean;
    procedure AfterMigrate(aData: TTransferData); virtual;
    procedure BeforeMigrate(aData: TTransferData); virtual;
  public
    class function GetMaxDataTransferNum(aDBEngine: TDBEngine): Integer;
    class function NeedToFirstMigrate(aDBEngine: TDBEngine): Boolean;
    procedure Migrate(aDBEngine: TDBEngine);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Apollo_Helpers,
  Apollo_ORM_Exception,
  System.Classes,
  System.Math,
  System.SysUtils,
  System.TypInfo;

{ TORMService }

function TORMService.TryGetStructure(aEntityType: TRttiInstanceType; out aStructure: TStructure): Boolean;
var
  GetStructureFunc: TRttiMethod;
begin
  if IsAbstract(aEntityType) then
    Exit(False);

  Result := True;
  GetStructureFunc := aEntityType.GetMethod('GetStructure');

  if not IsAbstract(GetStructureFunc) then
    aStructure := GetStructureFunc.Invoke(aEntityType.MetaclassType, []).AsType<TStructure>
  else
    raise EORMGetStructureIsAbstract.Create(TEntityClass(aEntityType.MetaclassType));
end;

procedure TORMService.AfterMigrate(aData: TTransferData);
begin
end;

procedure TORMService.BeforeMigrate(aData: TTransferData);
begin
end;

constructor TORMService.Create;
begin
  FRttiContext := TRttiContext.Create;
end;

procedure TORMService.CreateORMTablesIfNotExist;
var
  EntityClass: TEntityClass;
  EntityClasses: TArray<TEntityClass>;
  SQLList: TStringList;
begin
  EntityClasses := [TORMTable, TORMField, TORMTransfer];

  for EntityClass in EntityClasses do
    if not FDBEngine.TableExists(EntityClass.GetTableName) then
    begin
      SQLList := FDBEngine.GetCreateTableSQL(GetORMDef(EntityClass));
      try
        FDBEngine.ExecSQL(SQLList.Text);
      finally
        SQLList.Free;
      end;
    end;
end;

destructor TORMService.Destroy;
begin
  FRttiContext.Free;
  inherited;
end;

function TORMService.FinishTransfer(const aNum: Integer): Boolean;
begin
  Result := HandleTransferNum(aNum);
end;

function TORMService.GetFieldDef(aRttiProperty: TRttiProperty): TFieldDef;
var
  Attribute: TCustomAttribute;
  NumFieldSize: TArray<string>;
  Words: TArray<string>;
begin
  Result.Init;

  Result.FieldName := TORMTools.GetFieldNameByPropName(aRttiProperty.Name);
  Result.SQLType := GetSQLFieldType(aRttiProperty);

  for Attribute in aRttiProperty.GetAttributes do
  begin
    if Attribute is NotNull then
      Result.NotNull := True;

    if Attribute is FieldLength then
      Result.FieldLength := FieldLength(Attribute).Length;

    if Attribute is DefaultValue then
      Result.DefaultValue := DefaultValue(Attribute).Value;
  end;

  if (Result.SQLType = 'VARCHAR') and (Result.FieldLength = 0) then
    Result.FieldLength := 255;

  if Result.SQLType.Contains('(') then
  begin
    Words := Result.SQLType.Split(['(', ')']);
    Result.SQLType := Words[0];

    if not Words[1].Contains(',') then
      Result.FieldLength := Words[1].ToInteger
    else
    begin
      NumFieldSize := Words[1].Split([',']);
      Result.FieldPrecision := NumFieldSize[0].ToInteger;
      Result.FieldScale := NumFieldSize[1].ToInteger;
    end;
  end;
end;

function TORMService.GetInitData: TInitData;
begin
  Result.Data := [];
end;

class function TORMService.GetMaxDataTransferNum(aDBEngine: TDBEngine): Integer;
var
  QueryKeeper: IQueryKeeper;
begin
  if not aDBEngine.TableExists('ORM_TRANSFER') then
    Exit(-1);

  QueryKeeper := MakeQueryKeeper;
  QueryKeeper.Query.SQL.Text := 'SELECT MAX(NUM) FROM ORM_TRANSFER';
  aDBEngine.OpenQuery(QueryKeeper.Query);

  if not QueryKeeper.Query.Fields[0].IsNull then
    Result := QueryKeeper.Query.Fields[0].AsInteger
  else
    Result := -1;
end;

function TORMService.TryGetIndexDef(aRttiProperty: TRttiProperty; out aIndexDef: TIndexDef): Boolean;
var
  Attribute: TCustomAttribute;
begin
  Result := False;
  aIndexDef.Init;

  for Attribute in aRttiProperty.GetAttributes do
    if (Attribute is Index) or (Attribute is Unique) then
    begin
      aIndexDef.IndexName := '';
      aIndexDef.FieldNames := [TORMTools.GetFieldNameByPropName(aRttiProperty.Name)];
      if Attribute is Unique then
        aIndexDef.Unique := True;
      Result := True;
    end;
end;

function TORMService.GetORMDef(aEntityClass: TEntityClass): TTableDef;
var
  RttiInstanceType: TRttiInstanceType;
  Structure: TStructure;
begin
  RttiInstanceType := FRttiContext.GetType(aEntityClass.ClassInfo).AsInstance;
  Structure := aEntityClass.GetStructure;
  Result := GetTableDef(Structure, nil, RttiInstanceType);
end;

function TORMService.GetORMTable(const aORMUniqueID: string): TORMTable;
begin
  Result := TORMTable.Create(FDBEngine, [aORMUniqueID]);
end;

function TORMService.GetORMUniqueID(aRttiObject: TRttiObject): string;
var
  CustomAttribute: TCustomAttribute;
begin
  Result := '';

  for CustomAttribute in aRttiObject.GetAttributes do
    if CustomAttribute is ORMUniqueID then
    begin
      Result := (CustomAttribute as ORMUniqueID).UniqueID;
      Break;
    end;

  if Result.IsEmpty then
    raise EORMUniqueIDNotExists.Create(aRttiObject);
end;

function TORMService.GetPrimaryKey(aEntityType: TRttiInstanceType): TPrimaryKey;
var
  GetPrimaryKeyFunc: TRttiMethod;
begin
  GetPrimaryKeyFunc := aEntityType.GetMethod('GetPrimaryKey');
  if not Assigned(GetPrimaryKeyFunc) then
    EORMPrimaryKeyNotDefined.Create;

  Result := GetPrimaryKeyFunc.Invoke(aEntityType.MetaclassType, []).AsType<TPrimaryKey>;
end;

function TORMService.GetSQLFieldType(aRttiProperty: TRttiProperty): string;
var
  Attribute: TCustomAttribute;
begin
  for Attribute in aRttiProperty.GetAttributes do
    if Attribute is Text then
      Exit('TEXT');

  if (aRttiProperty.PropertyType.IsInstance) and
     (aRttiProperty.PropertyType.AsInstance.MetaclassType = TORMBlob)
  then
    Exit('BLOB');

  if aRttiProperty.PropertyType.Name.ToUpper = 'TDATETIME' then
    Exit('DATETIME')
  else
  if aRttiProperty.PropertyType.Name.ToUpper = 'BOOLEAN' then
    Exit('TYNYINT(1)')
  else
  if aRttiProperty.PropertyType.Name.ToUpper = 'CURRENCY' then
    Exit('NUMERIC(18,2)');

  case aRttiProperty.PropertyType.TypeKind of
    tkInteger, tkEnumeration: Result := 'INTEGER';
    tkString, tkUString: Result:= 'VARCHAR';
  end;
end;

function TORMService.GetTableDef(const aStructure: TStructure; aORMTable: TORMTable;
  aEntityType: TRttiInstanceType): TTableDef;
var
  FieldName: string;
  FKey: TFKey;
  FKeyDef: TFKeyDef;
  KeyField: TKeyField;
  TableDef: TTableDef;
begin
  TableDef.Init;

  if aStructure.TableName.IsEmpty then
    raise EORMTableNameNotDefined.Create(TEntityClass(aEntityType.MetaclassType));

  TableDef.TableName := aStructure.TableName.ToUpper;
  if Assigned(aORMTable) then
    TableDef.OldTableName := aORMTable.TableName;

  for KeyField in GetPrimaryKey(aEntityType) do
  begin
    FieldName := TORMTools.GetFieldNameByPropName(KeyField.PropName);
    TableDef.PKey.FieldNames := TableDef.PKey.FieldNames + [FieldName];

    if KeyField.FieldType = TFieldType.ftAutoInc then
      TableDef.PKey.Autoincrement := True;
  end;

  HandleEntityFields(aEntityType.GetProperties, procedure(aRttiProperty: TRttiProperty)
    var
      FieldDef: TFieldDef;
      IndexDef: TIndexDef;
      ORMField: TORMField;
    begin
      FieldDef := GetFieldDef(aRttiProperty);

      if Assigned(aORMTable) then
      begin
        ORMField := aORMTable.GetORMField(GetORMUniqueID(aRttiProperty));

        if Assigned(ORMField) then
          FieldDef.OldFieldName := ORMField.FieldName;
      end;

      if TableDef.PKey.FieldNames.Contains(FieldDef.FieldName) then
        FieldDef.NotNull := True;

      TableDef.FieldDefs := TableDef.FieldDefs + [FieldDef];

      if TryGetIndexDef(aRttiProperty, {out}IndexDef) then
      begin
        IndexDef.IndexName := 'IDX_' + TStringTools.GetHash16(TableDef.TableName + IndexDef.FieldNames.CommaText);
        TableDef.IndexDefs := TableDef.IndexDefs + [IndexDef];
      end;
    end
  );

  for FKey in aStructure.FKeys do
  begin
    FKeyDef.TableName := TableDef.TableName;
    FKeyDef.FieldName := TORMTools.GetFieldNameByPropName(FKey.PropName);
    FKeyDef.ReferenceTableName := FKey.ReferEntityClass.GetTableName.ToUpper;
    FKeyDef.ReferenceFieldName := TORMTools.GetFieldNameByPropName(FKey.ReferPropName);

    TableDef.FKeyDefs := TableDef.FKeyDefs + [FKeyDef];

    if not TableDef.IndexDefs.Contains([FKeyDef.FieldName]) then
      TableDef.IndexDefs := TableDef.IndexDefs + [FKeyDef.GetIndexDef];
  end;

  Result := TableDef;
end;

function TORMService.GetTypesInheritFrom(aClass: TClass): TArray<TRttiInstanceType>;
var
  RttiType: TRttiType;
  RttiTypes: TArray<TRttiType>;
begin
  Result := [];

  RttiTypes := FRttiContext.GetTypes;
  for RttiType in RttiTypes do
  begin
    if (not RttiType.IsInstance) or (RttiType.AsInstance.MetaclassType = aClass) then
      Continue;

    if RttiType.AsInstance.MetaclassType.InheritsFrom(aClass) then
      Result := Result + [RttiType.AsInstance];
  end;
end;

procedure TORMService.HandleEntityFields(aEntityProperties: TArray<TRttiProperty>;
  aHandleEntityFieldProc: THandleEntityFieldProc);
var
  RttiProperty: TRttiProperty;
begin
  for RttiProperty in aEntityProperties do
  begin
    if (RttiProperty.Visibility <> mvPublished) or
       (RttiProperty.PropertyType.IsInstance and (RttiProperty.PropertyType.AsInstance.MetaclassType <> TORMBlob))
    then
      Continue;

    aHandleEntityFieldProc(RttiProperty);
  end;
end;

function TORMService.HandleEntityType(aEntityType: TRttiInstanceType): string;
var
  ORMField: TORMField;
  ORMTable: TORMTable;
  Structure: TStructure;
  SQLList: TStringList;
  TableDef: TTableDef;
begin
  Result := '';

  if TryGetStructure(aEntityType, Structure) then
  begin
    ORMTable := GetORMTable(GetORMUniqueID(aEntityType));
    try
      TableDef := GetTableDef(Structure, ORMTable, aEntityType);

      if ORMTable.IsNew then
      begin
        SQLList := FDBEngine.GetCreateTableSQL(TableDef);
        try
          Result := SQLList.Text;
        finally
          SQLList.Free;
        end;
      end
      else
      begin
        SQLList := FDBEngine.GetModifyTableSQL(TableDef);
        try
          if SQLList.Count > 0 then
            Result := SQLList.Text;
        finally
          SQLList.Free;
        end;
      end;

      if not Result.IsEmpty then
      begin
        FDBEngine.ExecSQL(Result);
        if ORMTable.IsNew then
          HandleInitData(TEntityClass(aEntityType.MetaclassType));

        ORMTable.TableName := TableDef.TableName;
        ORMTable.ORMFields.Clear;

        HandleEntityFields(aEntityType.GetProperties, procedure(aRttiProperty: TRttiProperty)
          begin
            ORMField := TORMField.Create(FDBEngine);
            ORMField.FieldName := TORMTools.GetFieldNameByPropName(aRttiProperty.Name);
            ORMField.ORMUniqueID := GetORMUniqueID(aRttiProperty);
            ORMTable.ORMFields.Add(ORMField);
          end
        );

        ORMTable.StoreAll;
      end;
    finally
      ORMTable.Free;
    end;
  end;
end;

procedure TORMService.HandleInitData(aEntityClass: TEntityClass);
var
  Entity: TEntityAbstract;
  i: Integer;
  InitData: TInitData;
  InitDataItem: TInitDataItem;
  PropName: string;
  PropValues: TArray<Variant>;
begin
  InitData := GetInitData;

  for InitDataItem in InitData.Data do
    if aEntityClass = InitDataItem.EntityClass  then
      for PropValues in InitDataItem.PropValues do
      begin
        if Length(PropValues) <> Length(InitDataItem.PropNames) then
          raise EORMWrongInputCount.Create;

        Entity := aEntityClass.Create(FDBEngine);
        try
          for i := 0 to Length(PropValues) - 1 do
          begin
            PropName := InitDataItem.PropNames[i];
            Entity.Prop[PropName] := PropValues[i];
          end;

          Entity.Store;
        finally
          Entity.Free;
        end;
      end;
end;

procedure TORMService.HandleORMTransferRecord(const aSQLCode: string);
var
  ORMTransfer: TORMTransfer;
begin
  if (FNextMaxDataTransferNum > FMaxDataTransferNum) or (FMaxDataTransferNum = -1) then
  begin
    ORMTransfer := TORMTransfer.Create(FDBEngine);
    try
      if FMaxDataTransferNum = -1 then
        ORMTransfer.Num := Max(FNextMaxDataTransferNum, 0)
      else
      begin
        ORMTransfer.Num := FNextMaxDataTransferNum;
        ORMTransfer.SQLCode := aSQLCode;
      end;

      ORMTransfer.Store;
    finally
      ORMTransfer.Free;
    end;
  end;
end;

function TORMService.HandleTransferNum(const aNum: Integer): Boolean;
begin
  if FMaxDataTransferNum = -1 then
  begin
    Result := False;
    FNextMaxDataTransferNum := Max(aNum, FNextMaxDataTransferNum);
  end
  else
  if aNum > FMaxDataTransferNum then
  begin
    Result := True;
    FNextMaxDataTransferNum := Max(aNum, FNextMaxDataTransferNum);
  end
  else
    Result := False;
end;

function TORMService.IsAbstract(aRttiObject: TRttiObject): Boolean;
var
  CustomAttribute: TCustomAttribute;
begin
  if (aRttiObject is TRttiInstanceType) then
  begin
    Result := False;
    for CustomAttribute in aRttiObject.GetAttributes do
      if CustomAttribute is SkipStructure then
        Exit(True);
  end
  else
    Result := PVmtMethodExEntry(aRttiObject.Handle).Flags and (1 shl 7) <> 0;
end;

function TORMService.IsORMEntity(aClass: TClass): Boolean;
begin
  if (aClass = TORMTable) or
     (aClass = TORMField) or
     (aClass = TORMTransfer)
  then
    Result := True
  else
    Result := False;
end;

function TORMService.MakeQuery(const aTableName: string): IQueryKeeper;
begin
  Result := MakeQueryKeeper;
  Result.Query.SQL.Text := Format('SELECT * FROM %s', [aTableName]);
  FDBEngine.OpenQuery(Result.Query);
end;

procedure TORMService.Migrate(aDBEngine: TDBEngine);
var
  EntityType: TRttiInstanceType;
  EntityTypes: TArray<TRttiInstanceType>;
  SQL: string;
  SQLList: TStringList;
begin
  FDBEngine := aDBEngine;

  FTransferData := TTransferData.Create;
  SQLList := TStringList.Create;
  FDBEngine.DisableForeignKeys;
  try
    FDBEngine.TransactionStart;
    try
      CreateORMTablesIfNotExist;
      FMaxDataTransferNum := GetMaxDataTransferNum(FDBEngine);
      FNextMaxDataTransferNum := FMaxDataTransferNum;

      BeforeMigrate(FTransferData);

      EntityTypes := GetTypesInheritFrom(TEntityAbstract);
      for EntityType in EntityTypes do
      begin
        if not IsORMEntity(EntityType.MetaclassType) then
        begin
          SQL := HandleEntityType(EntityType);
          if not SQL.IsEmpty then
            SQLList.Add(SQL);
        end;
      end;

      if SQLList.Count > 0 then
      begin
        AfterMigrate(FTransferData);
        HandleORMTransferRecord(SQLList.Text);
      end
      else
      begin
        FDBEngine.TransactionRollback;
        Exit;
      end;

      FDBEngine.TransactionCommit;
    except;
      FDBEngine.TransactionRollback;
      raise;
    end;
  finally
    FDBEngine.EnableForeignKeys;
    FTransferData.Free;
    SQLList.Free;
  end;
end;

class function TORMService.NeedToFirstMigrate(aDBEngine: TDBEngine): Boolean;
begin
  Result := GetMaxDataTransferNum(aDBEngine) = -1;
end;

function TORMService.StartTransfer(const aNum: Integer): Boolean;
begin
  Result := HandleTransferNum(aNum);
end;

{ TORMTable }

function TORMTable.GetORMField(const aORMUniqueID: string): TORMField;
var
  ORMField: TORMField;
begin
  Result := nil;

  for ORMField in GetORMFields do
    if ORMField.ORMUniqueID = aORMUniqueID then
      Exit(ORMField);
end;

function TORMTable.GetORMFields: TORMFieldList;
begin
  if not Assigned(FORMFields) then
    FORMFields := TORMFieldList.Create(Self);

  Result := FORMFields;
end;

class function TORMTable.GetStructure: TStructure;
begin
  Result.TableName := 'ORM_TABLE';
  Result.PrimaryKey.AddField('ORMUniqueID', TFieldType.ftString);
end;

{ TORMField }

class function TORMField.GetStructure: TStructure;
begin
  Result.TableName := 'ORM_FIELD';

  Result.PrimaryKey.AddField('ORMUniqueID', TFieldType.ftString);
  Result.PrimaryKey.AddField('TableID', TFieldType.ftString);

  Result.FKeys.Add('TableID', TORMTable, 'ORMUniqueID');
end;

{ TORMTransfer }

class function TORMTransfer.GetStructure: TStructure;
begin
  Result.TableName := 'ORM_TRANSFER';
  Result.PrimaryKey.AddField('Num', TFieldType.ftInteger);
end;

{ TInitData }

procedure TInitData.AddInitData(aEntityClass: TEntityClass;
  const aPropNames: TArray<string>; const aPropValues: TArray<TArray<Variant>>);
var
  aDataItem: TInitDataItem;
begin
  aDataItem.EntityClass := aEntityClass;
  aDataItem.PropNames := aPropNames;
  aDataItem.PropValues := aPropValues;

  Data := Data + [aDataItem];
end;

{ TInitDataItem }

procedure TInitDataItem.Init;
begin
  PropNames := [];
  PropValues := [];
end;

end.
