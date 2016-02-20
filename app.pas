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

    procedure refreshGrid;
  private
  private
    { private declarations }
  public
    { public declarations }
  end;

  TEdge = record
    first: Integer;
    second: Integer;
  end;

  TVertexState = (Unset=0, Unconnected=1, Connected=2);
  TNodeList = array of Integer;
  TEdgeList = array of TEdge;

var
  Form1: TForm1;
  matrix: array of array of TVertexState;

implementation

procedure fillGrid;
var
  x, y: Integer;
begin
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      if matrix[x, y] = Unset then
        matrix[x, y] := Unconnected;
end;

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
    refreshGrid;
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
  refreshGrid;
end;

procedure TForm1.btn_treeClick(Sender: TObject);
begin
  breadthFirstSearch();
end;

procedure TForm1.refreshGrid;
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
