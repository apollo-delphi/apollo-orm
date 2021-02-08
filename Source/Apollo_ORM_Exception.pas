unit Apollo_ORM_Exception;

interface

uses
  System.SysUtils;

type
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

  EORMUniqueIDNotExists = class(Exception)
  public
    constructor Create;
  end;

implementation

{ EORMPropNotExists }

constructor EORMKeyPropNotExists.Create;
begin

end;

{ EORMWrongInputCount }

constructor EORMWrongInputCount.Create;
begin

end;

{ EORMWrongParamName }

constructor EORMWrongParamName.Create;
begin

end;

{ EORMUniqueIDNotExists }

constructor EORMUniqueIDNotExists.Create;
begin

end;

end.
