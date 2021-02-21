program Apollo_ORM_Test;

{$STRONGLINKTYPES ON}
uses
  Vcl.Forms,
  System.SysUtils,
  DUnitX.Loggers.GUI.VCL,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Apollo_ORM in 'Apollo_ORM.pas',
  tstYourUnitName in 'tstYourUnitName.pas',
  Apollo_DB_Core in '..\Vendors\Apollo_DB_Core\Source\Apollo_DB_Core.pas',
  Apollo_Helpers in '..\Vendors\Apollo_Helpers\Source\Apollo_Helpers.pas',
  Apollo_Types in '..\Vendors\Apollo_Types\Apollo_Types.pas';

begin
  Application.Initialize;
  Application.Title := 'DUnitX';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end.
