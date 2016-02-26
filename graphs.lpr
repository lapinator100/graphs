program graphs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, app
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Graphen';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.Run;
end.

