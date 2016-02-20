unit app;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Spin,
  StdCtrls;

type
  TForm1 = class(TForm)
    btn_tree: TButton;
    input: TSpinEdit;
    grid: TStringGrid;
    procedure btn_treeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure inputChange(Sender: TObject);
    procedure fillGrid;
    procedure refresh;
  private
  private
    { private declarations }
  public
    { public declarations }
  end;

  TVertexState = (Unset=0, Unconnected=1, Connected=2);

var
  Form1: TForm1;
  matrix: array of array of TVertexState;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  inputChange(Sender);
end;

procedure TForm1.gridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (aCol > 0) and (aRow > 0) then
  begin
    if matrix[aCol - 1, aRow - 1] = Connected then
      matrix[aCol - 1, aRow - 1] := Unconnected
    else
      matrix[aCol - 1, aRow - 1] := Connected;

    CanSelect := True;
    refresh;
  end
  else
    CanSelect := False;
end;

procedure TForm1.inputChange(Sender: TObject);
var
  i: Integer;
begin
  grid.RowCount := input.Value + 1;
  grid.ColCount := input.Value + 1;

  setLength(matrix, input.Value);

  for i := 0 to length(matrix) - 1 do
    setLength(matrix[i], input.Value);

  fillGrid;
  refresh;
end;

procedure TForm1.btn_treeClick(Sender: TObject);
var
  current, processed: array of Integer;
  current_node, i: Integer;
begin
  {current.setLength(1);
  current[0] := 0;
  processed.setLength(1);
  processed[0] := 0;

  if length(current) == 0 then
    result := //e0
  else
  begin
    current_node := current[0];

    neighbour := neighbour_nodes(current, current_node);
    if neighbours then
    begin
      for i := 0 to length(current) - 2 do
        current[i] := current[i + 1];

      setLength(current, length(current) - 1);
    end
    else
    begin

    end;
  end;}
end;

function neighbour_nodes(current: array of Integer; current_node: Integer): Integer;
var
  y, i: Integer;
begin
  {for y := 0 to length(matrix[current_node]) - 1 do
    if matrix[current_node, y] = Connected then
      for i := 0 to length(current[current_node]) - 1 do                     // <----- Hier nach Fehlern suchen!
        if not matrix[current_node, i] = current[current_node, y] then
        begin
          result := i;
          exit;
        end;

  result := -1;}
end;

procedure TForm1.fillGrid;
var
  x, y: Integer;
begin
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      if matrix[x, y] = Unset then
        matrix[x, y] := Unconnected;
end;

procedure TForm1.refresh;
var
  x, y: Integer;
begin
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      if (x < grid.ColCount - 1) and (y < grid.RowCount - 1) then
        if matrix[x, y] = Unconnected then
          grid.Cells[x + 1, y + 1] := '0'
        else
          grid.Cells[x + 1, y + 1] := '1';
end;

end.
