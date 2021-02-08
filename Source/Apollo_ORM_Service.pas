unit Apollo_ORM_Service;

interface

uses
  Apollo_DB_Core,
  Apollo_ORM,
  System.Rtti;

type
{$M+}
  TORMTable = class(TEntityAbstract)
  strict private
    FORMUniqueID: string;
    FTableName: string;
  public
    class function GetStructure: TStructure; override;
  published
    property ORMUniqueID: string read FORMUniqueID write FORMUniqueID;

    [NotNull]
    property TableName: string read FTableName write FTableName;
  end;

  TORMField = class(TEntityAbstract)
  strict private
    FFieldName: string;
    FORMUniqueID: string;
    FTableID: string;
  public
    class function GetStructure: TStructure; override;
  published
    property FieldName: string read FFieldName write FFieldName;
    property ORMUniqueID: string read FORMUniqueID write FORMUniqueID;
    property TableID: string read FTableID write FTableID;
  end;

  TORMService = class
  private
    FDBEngine: TDBEngine;
    FRttiContext: TRttiContext;
    function GetFieldDef(aRttiProperty: TRttiProperty): TFieldDef;
    function GetORMField(const aORMUniqueID: string): TORMField;
    function GetORMFieldDef: TTableDef;
    function GetORMTable(const aORMUniqueID: string): TORMTable;
    function GetORMTableDef: TTableDef;
    function GetORMUniqueID(aRttiObject: TRttiObject): string;
    function GetSQLFieldType(aRttiType: TRttiType): string;
    function GetTableDef(const aStructure: TStructure; aEntityType: TRttiInstanceType): TTableDef;
    function GetTypesInheritFrom(aClass: TClass): TArray<TRttiInstanceType>;
    function IsAbstract(aRttiObject: TRttiObject): Boolean;
    function IsAutoincrement(aEntityType: TRttiInstanceType; aRttiProperty: TRttiProperty): Boolean;
    function IsORMEntity(aClass: TClass): Boolean;
    function TryGetStructure(aEntityType: TRttiInstanceType; out aStructure: TStructure): Boolean;
    procedure CreateORMTablesIfNotExist;
    procedure HandleEntityType(aEntityType: TRttiInstanceType);
    public
    procedure Migrate(aDBEngine: TDBEngine);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Apollo_ORM_Exception,
  System.Classes,
  System.SysUtils,
  System.TypInfo;

{ TORMService }

function TORMService.TryGetStructure(aEntityType: TRttiInstanceType; out aStructure: TStructure): Boolean;
var
  GetStructureFunc: TRttiMethod;
begin
  Result := True;
  GetStructureFunc := aEntityType.GetMethod('GetStructure');

  if not IsAbstract(GetStructureFunc) then
    aStructure := GetStructureFunc.Invoke(aEntityType.MetaclassType, []).AsType<TStructure>
  else
    Result := False;
end;

constructor TORMService.Create;
begin
  FRttiContext := TRttiContext.Create;
end;

procedure TORMService.CreateORMTablesIfNotExist;
var
  SQL: string;
begin
  if not FDBEngine.TableExists(TORMTable.GetTableName) then
  begin
    SQL := FDBEngine.GetCreateTableSQL(GetORMTableDef);
    FDBEngine.ExecSQL(SQL);
  end;

  if not FDBEngine.TableExists(TORMField.GetTableName) then
  begin
    SQL := FDBEngine.GetCreateTableSQL(GetORMFieldDef);
    FDBEngine.ExecSQL(SQL);
  end;
end;

destructor TORMService.Destroy;
begin
  FRttiContext.Free;
  inherited;
end;

function TORMService.GetFieldDef(aRttiProperty: TRttiProperty): TFieldDef;
var
  CustomAttribute: TCustomAttribute;
begin
  Result.Init;

  Result.FieldName := TORMTools.GetFieldNameByPropName(aRttiProperty.Name);
  Result.SQLType := GetSQLFieldType(aRttiProperty.PropertyType);

  for CustomAttribute in aRttiProperty.GetAttributes do
  begin
    if CustomAttribute is NotNull then
      Result.NotNull := True;
  end;
end;

function TORMService.GetORMField(const aORMUniqueID: string): TORMField;
begin
  Result := TORMField.Create(FDBEngine, [aORMUniqueID]);
end;

function TORMService.GetORMFieldDef: TTableDef;
var
  RttiInstanceType: TRttiInstanceType;
  Structure: TStructure;
begin
  RttiInstanceType := FRttiContext.GetType(TORMField.ClassInfo).AsInstance;
  Structure := TORMField.GetStructure;
  Result := GetTableDef(Structure, RttiInstanceType);
end;

function TORMService.GetORMTable(const aORMUniqueID: string): TORMTable;
begin
  Result := TORMTable.Create(FDBEngine, [aORMUniqueID]);
end;

function TORMService.GetORMTableDef: TTableDef;
var
  RttiInstanceType: TRttiInstanceType;
  Structure: TStructure;
begin
  RttiInstanceType := FRttiContext.GetType(TORMTable.ClassInfo).AsInstance;
  Structure := TORMTable.GetStructure;
  Result := GetTableDef(Structure, RttiInstanceType);
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
    raise EORMUniqueIDNotExists.Create;
end;

function TORMService.GetSQLFieldType(aRttiType: TRttiType): string;
begin
  case aRttiType.TypeKind of
    tkInteger: Result := 'INTEGER';
    tkString, tkUString: Result:= 'VARCHAR(255)';
  end;
end;

function TORMService.GetTableDef(const aStructure: TStructure; aEntityType: TRttiInstanceType): TTableDef;
var
  FieldDef: TFieldDef;
  FieldName: string;
  KeyField: TKeyField;
  ORMField: TORMField;
  RttiProperty: TRttiProperty;
begin
  Result.TableName := aStructure.TableName.ToUpper;

  if aEntityType.MetaclassType.InheritsFrom(TEntityFeatID) then
  begin
    Result.PKey.FieldNames := ['ID'];
    Result.PKey.Autoincrement := True;
  end
  else
  begin
    Result.PKey.FieldNames := [];
    for KeyField in aStructure.PrimaryKey do
    begin
      FieldName := TORMTools.GetFieldNameByPropName(KeyField.PropName);
      Result.PKey.FieldNames := Result.PKey.FieldNames + [FieldName];
    end;
  end;

  Result.FieldDefs := [];
  for RttiProperty in aEntityType.GetProperties do
  begin
    if RttiProperty.Visibility <> mvPublished then
      Continue;

    FieldDef := GetFieldDef(RttiProperty);

    if (IsORMEntity(aEntityType.MetaclassType)) or
       (IsAutoincrement(aEntityType, RttiProperty))
    then
    begin
      //do nothing
    end
    else
    begin
      ORMField := GetORMField(GetORMUniqueID(RttiProperty));
      try
        if not ORMField.IsNew then
          FieldDef.OldFieldName := ORMField.FieldName;

        if ORMField.FieldName <> FieldDef.FieldName then
        begin
          ORMField.FieldName := FieldDef.FieldName;
          ORMField.Store;
        end;
      finally
        ORMField.Free;
      end;
    end;

    Result.FieldDefs := Result.FieldDefs + [FieldDef];
  end;
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

procedure TORMService.HandleEntityType(aEntityType: TRttiInstanceType);
var
  ORMTable: TORMTable;
  Structure: TStructure;
  SQL: string;
  SQLList: TStringList;
  TableDef: TTableDef;
begin
  if TryGetStructure(aEntityType, Structure) then
  begin
    //try find ormtable by guid
    //if not found, add to ormtable, get create table statment
    //if found and table name the same, get modify table statment
    //if found but table name differ, rename table, update ormtable, get modify statement

    {if Structure.TableGUID.IsEmpty then
      raise Exception.CreateFmt('Entity class %s must specifies GUID in the GetStructure function!', [aEntityType.Name]);

    if Structure.TableName.IsEmpty then
      raise Exception.CreateFmt('Entity class %s must specifies TableName in the GetStructure function!', [aEntityType.Name]);
    }

    TableDef := GetTableDef(Structure, aEntityType);

    ORMTable := GetORMTable(GetORMUniqueID(aEntityType));
    try
      SQL := '';
      if ORMTable.IsNew then
        SQL := FDBEngine.GetCreateTableSQL(TableDef)
      else
      begin
        SQLList := FDBEngine.GetModifyTableSQL(ORMTable.TableName, TableDef);
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
          FDBEngine.TransactionCommit;
        except;
          FDBEngine.TransactionRollback;
          raise;
        end;
      end;

      if ORMTable.TableName <> TableDef.TableName then
      begin
        ORMTable.TableName := TableDef.TableName;
        ORMTable.Store;
      end;
    finally
      ORMTable.Free;
    end;
  end;
end;

function TORMService.IsAbstract(aRttiObject: TRttiObject): Boolean;
begin
  Result := PVmtMethodExEntry(aRttiObject.Handle).Flags and (1 shl 7) <> 0;
end;

function TORMService.IsAutoincrement(aEntityType: TRttiInstanceType; aRttiProperty: TRttiProperty): Boolean;
var
  GetPrimaryKeyFunc: TRttiMethod;
  KeyField: TKeyField;
  PrimaryKey: TPrimaryKey;
begin
  Result := False;

  GetPrimaryKeyFunc := aEntityType.GetMethod('GetPrimaryKey');
  PrimaryKey := GetPrimaryKeyFunc.Invoke(aEntityType.MetaclassType, []).AsType<TPrimaryKey>;

  if PrimaryKey.TryGetKeyField(aRttiProperty.Name, KeyField) then
    if KeyField.FieldType = TFieldType.ftAutoInc then
      Exit(True);
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

class function TORMTable.GetStructure: TStructure;
begin
  Result.TableName := 'ORM_TABLE';
  Result.PrimaryKey.Add('ORMUniqueID', TFieldType.ftString);
end;

{ TORMField }

class function TORMField.GetStructure: TStructure;
begin
  Result.TableName := 'ORM_FIELD';
  Result.PrimaryKey.Add('ORMUniqueID', TFieldType.ftString);
  Result.FKeys.Add('TableID', TORMTable, 'ORMUniqueID', rtOne2Many);
end;

end.
