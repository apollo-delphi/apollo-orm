unit Apollo_ORM_Exception;

interface

uses
  Apollo_ORM,
  System.Rtti,
  System.SysUtils;

type
  EORMException = class abstract(Exception)
  protected
    function GetErrorMsg(aEntityClass: TEntityClass; const aPropName: string): string; virtual; abstract;
  public
    constructor Create(aEntityClass: TEntityClass; const aPropName: string = ''); reintroduce;
  end;

  EORMGetStructureIsAbstract = class(EORMException)
  protected
    function GetErrorMsg(aEntityClass: TEntityClass; const aPropName: string): string; override;
  end;

  EORMTableNameNotDefined = class(EORMException)
  protected
    function GetErrorMsg(aEntityClass: TEntityClass; const aPropName: string): string; override;
  end;

  EORMWrongInputCount = class(Exception)
  public
    constructor Create;
  end;

  EORMKeyPropNotExists = class(Exception)
  public
    constructor Create;
  end;

  EORMWrongParamName = class(Exception)
  public
    constructor Create;
  end;

  EORMUniqueIDNotExists = class(EORMException)
  protected
    function GetErrorMsg(aEntityClass: TEntityClass; const aPropName: string): string; override;
  public
    constructor Create(aRttiObject: TRttiObject);
  end;

  EORMPrimaryKeyNotDefined = class(Exception)
  public
    constructor Create;
  end;

  EORMInitDataPrimaryKeyNotDefined = class(EORMException)
  protected
    function GetErrorMsg(aEntityClass: TEntityClass; const aPropName: string): string; override;
  public
    constructor Create(aEntityClass: TEntityClass);
  end;

implementation

{ EORMPropNotExists }

constructor EORMKeyPropNotExists.Create;
begin
end;

{ EORMWrongInputCount }

constructor EORMWrongInputCount.Create;
begin
//FillList: the quantity of declared and filled params is different!
end;

{ EORMWrongParamName }

constructor EORMWrongParamName.Create;
begin
end;

{ EORMUniqueIDNotExists }

constructor EORMUniqueIDNotExists.Create(aRttiObject: TRttiObject);
var
  EntityClass: TEntityClass;
  PropName: string;
begin
  PropName := '';
  EntityClass := nil;

  if aRttiObject is TRttiInstanceType then
    EntityClass := TEntityClass(TRttiInstanceType(aRttiObject).MetaclassType)
  else
  if aRttiObject is TRttiProperty then
  begin
    EntityClass := TEntityClass(TRttiProperty(aRttiObject).Parent.AsInstance.MetaclassType);
    PropName := TRttiProperty(aRttiObject).Name;
  end;

  inherited Create(EntityClass, PropName);
end;

function EORMUniqueIDNotExists.GetErrorMsg(aEntityClass: TEntityClass; const aPropName: string): string;
begin
  if aPropName.IsEmpty then
    Result := Format('Class %s should has ORMUniqueID attribute', [aEntityClass.ClassName])
  else
    Result := Format('Property "%s" of class %s should has ORMUniqueID attribute', [aPropName, aEntityClass.ClassName]);
end;

{ EORMPrimaryKeyNotDefined }

constructor EORMPrimaryKeyNotDefined.Create;
begin
end;

{ EORMException }

constructor EORMException.Create(aEntityClass: TEntityClass; const aPropName: string);
begin
  inherited Create('ORM error. ' + GetErrorMsg(aEntityClass, aPropName));
end;

{ EORMGetStructureIsAbstract }

function EORMGetStructureIsAbstract.GetErrorMsg(
  aEntityClass: TEntityClass; const aPropName: string): string;
begin
  Result := Format('Class %s: override GetStructure function or set [SkipStructure] attribute', [aEntityClass.ClassName]);
end;

{ EORMTableNameNotDefined }

function EORMTableNameNotDefined.GetErrorMsg(aEntityClass: TEntityClass;
  const aPropName: string): string;
begin
  Result := Format('Class %s: table name is not defined in the GetStructure function', [aEntityClass.ClassName]);
end;

{ EORMInitDataPrimaryKeyNotDefined }

constructor EORMInitDataPrimaryKeyNotDefined.Create(aEntityClass: TEntityClass);
begin
  inherited Create(aEntityClass, '');
end;

function EORMInitDataPrimaryKeyNotDefined.GetErrorMsg(
  aEntityClass: TEntityClass; const aPropName: string): string;
begin
  Result := Format('Primary key did not find in Entity %s data initialization', [aEntityClass.ClassName]);
end;

end.
