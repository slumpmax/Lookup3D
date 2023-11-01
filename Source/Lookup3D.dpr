program Lookup3D;

uses
  Forms,
  Form_Main in 'Form_Main.pas' {FormMain},
  Form_Structure in 'Form_Structure.pas' {FormStructure},
  Form_XML in 'Form_XML.pas' {FormXML},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  TStyleManager.TrySetStyle('Sapphire Kamri');
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
