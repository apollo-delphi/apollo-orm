unit Apollo_ORM_Service;

interface

uses
  Apollo_DB_Core,
  Apollo_ORM,
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
{$M-}

  THandleEntityFieldProc = reference to procedure(aRttiProperty: TRttiProperty);

  TORMService = class
  private
    FDBEngine: TDBEngine;
    FRttiContext: TRttiContext;
    function GetFieldDef(aRttiProperty: TRttiProperty): TFieldDef;
    function GetORMDef(aEntityClass: TEntityClass): TTableDef;
    function GetORMTable(const aORMUniqueID: string): TORMTable;
    function GetORMUniqueID(aRttiObject: TRttiObject): string;
    function GetPrimaryKey(aEntityType: TRttiInstanceType): TPrimaryKey;
    function GetSQLFieldType(aRttiProperty: TRttiProperty): string;
    function GetTableDef(const aStructure: TStructure; aORMTable: TORMTable;
      aEntityType: TRttiInstanceType): TTableDef;
    function GetTypesInheritFrom(aClass: TClass): TArray<TRttiInstanceType>;
    function IsAbstract(aRttiObject: TRttiObject): Boolean;
    function IsORMEntity(aClass: TClass): Boolean;
    function TryGetIndexDef(aRttiProperty: TRttiProperty; out aIndexDef: TIndexDef): Boolean;
    function TryGetStructure(aEntityType: TRttiInstanceType; out aStructure: TStructure): Boolean;
    procedure CreateORMTablesIfNotExist;
    procedure HandleEntityFields(aEntityProperties: TArray<TRttiProperty>;
      aHandleEntityFieldProc: THandleEntityFieldProc);
    procedure HandleEntityType(aEntityType: TRttiInstanceType);
    public
    procedure Migrate(aDBEngine: TDBEngine);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Apollo_Helpers,
  Apollo_ORM_Exception,
  System.Classes,
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
  EntityClasses := [TORMTable, TORMField];

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

function TORMService.GetFieldDef(aRttiProperty: TRttiProperty): TFieldDef;
var
  Attribute: TCustomAttribute;
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
  end;

  if (Result.SQLType = 'VARCHAR') and (Result.FieldLength = 0) then
    Result.FieldLength := 255;
end;

function TORMService.TryGetIndexDef(aRttiProperty: TRttiProperty; out aIndexDef: TIndexDef): Boolean;
var
  Attribute: TCustomAttribute;
begin
  Result := False;

  for Attribute in aRttiProperty.GetAttributes do
    if Attribute is Index then
    begin
      aIndexDef.IndexName := '';
      aIndexDef.FieldNames := [TORMTools.GetFieldNameByPropName(aRttiProperty.Name)];
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

  case aRttiProperty.PropertyType.TypeKind of
    tkInteger: Result := 'INTEGER';
    tkString, tkUString: Result:= 'VARCHAR';
  end;
end;

function TORMService.GetTableDef(const aStructure: TStructure; aORMTable: TORMTable;
  aEntityType: TRttiInstanceType): TTableDef;
var
  FieldDef: TFieldDef;
  FieldName: string;
  FKey: TFKey;
  FKeyDef: TFKeyDef;
  KeyField: TKeyField;
  ORMField: TORMField;
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
      IndexDef: TIndexDef;
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

      if TryGetIndexDef(aRttiProperty, IndexDef) then
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

procedure TORMService.HandleEntityType(aEntityType: TRttiInstanceType);
var
  ORMField: TORMField;
  ORMTable: TORMTable;
  Structure: TStructure;
  SQL: string;
  SQLList: TStringList;
  TableDef: TTableDef;
begin
  if TryGetStructure(aEntityType, Structure) then
  begin
    ORMTable := GetORMTable(GetORMUniqueID(aEntityType));
    try
      TableDef := GetTableDef(Structure, ORMTable, aEntityType);

      SQL := '';
      if ORMTable.IsNew then
      begin
        SQLList := FDBEngine.GetCreateTableSQL(TableDef);
        try
          SQL := SQLList.Text;
        finally
          SQLList.Free;
        end;
      end
      else
      begin
        SQLList := FDBEngine.GetModifyTableSQL(TableDef);
        try
          if SQLList.Count > 0 then
            SQL := SQLList.Text;
        finally
          SQLList.Free;
        end;
      end;

      if not SQL.IsEmpty then
      begin
        FDBEngine.TransactionStart;
        try
          FDBEngine.ExecSQL(SQL);

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

          FDBEngine.TransactionCommit;
        except;
          FDBEngine.TransactionRollback;
          raise;
        end;
      end;
    finally
      ORMTable.Free;
    end;
  end;
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
     (aClass = TORMField)
  then
    Result := True
  else
    Result := False;
end;

procedure TORMService.Migrate(aDBEngine: TDBEngine);
var
  EntityType: TRttiInstanceType;
  EntityTypes: TArray<TRttiInstanceType>;
begin
  FDBEngine := aDBEngine;
  CreateORMTablesIfNotExist;

  EntityTypes := GetTypesInheritFrom(TEntityAbstract);

  for EntityType in EntityTypes do
  begin
    if not IsORMEntity(EntityType.MetaclassType) then
      HandleEntityType(EntityType);
  end;
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
  Result.FKeys.Add('TableID', TORMTable, 'ORMUniqueID', rtOne2Many);
end;

end.
