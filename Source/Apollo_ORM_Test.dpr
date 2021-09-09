program Apollo_ORM_Test;

{$STRONGLINKTYPES ON}
uses
  Vcl.Forms,
  System.SysUtils,
  DUnitX.Loggers.GUI.VCL,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Apollo_ORM in 'Apollo_ORM.pas',
  tstApollo_ORM in 'tstApollo_ORM.pas',
  Apollo_DB_Core in '..\Vendors\Apollo_DB_Core\Apollo_DB_Core.pas',
  Apollo_Helpers in '..\Vendors\Apollo_Helpers\Apollo_Helpers.pas',
  Apollo_Types in '..\Vendors\Apollo_Types\Apollo_Types.pas';

begin
  Application.Initialize;
  Application.Title := 'DUnitX';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end.
