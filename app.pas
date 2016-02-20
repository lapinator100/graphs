unit app;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Spin,
  StdCtrls;

type

  { TForm1 }

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

procedure clearMatrix;
var
  x, y: Integer;
begin
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      matrix[x, y] := Unconnected;
end;

procedure fillMatrix;
var
  x, y: Integer;
begin
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      if matrix[x, y] = Unset then
        matrix[x, y] := Unconnected;
end;

procedure applyEdgesToMatrix(edges: TEdgeList);
var
  i: Integer;
begin
  for i := 0 to length(edges) - 1 do
  begin
    matrix[edges[i].first][edges[i].second] := Connected;
    matrix[edges[i].second][edges[i].first] := Connected;
  end;
end;

function isVertexInNodeList(node: Integer; nodeList: TNodeList): Boolean;
var
  i: Integer;
begin
  for i := 0 to length(nodeList) - 1 do
    if nodeList[i] = node then
    begin
      result := True;
      exit;
    end;

  result := False;
end;

function getNextNeighbourNode(node: Integer; exludeNodes: TNodeList): Integer;
var
  y: Integer;
begin
  for y := 0 to length(matrix[node]) - 1 do
    if matrix[node, y] = Connected then
    begin
      if not isVertexInNodeList(y, exludeNodes) then
      begin
        result := y;
        exit;
      end;
    end;

  result := -1;
end;

function breadthFirstSearch: TEdgeList;
var
  currentNode, nextNeighbourNode: Integer;
  activeNodes, processedNodes: TNodeList;
  currentEdge: TEdge;
  edges: TEdgeList;
begin
  setLength(activeNodes, 1);
  activeNodes[0] := 0;
  setLength(processedNodes, 1);
  processedNodes[0] := 0;

  while length(activeNodes) <> 0 do
  begin
    currentNode := activeNodes[0];
    nextNeighbourNode := getNextNeighbourNode(currentNode, processedNodes);

    if nextNeighbourNode = -1 then
    begin
      Move(activeNodes[1], activeNodes[0], SizeOf(activeNodes[0]) * (Length(activeNodes) - 1));
      SetLength(activeNodes, Length(activeNodes) - 1);
    end
    else
    begin
      setLength(activeNodes, length(activeNodes) + 1);
      activeNodes[length(activeNodes) - 1] := nextNeighbourNode;
      setLength(processedNodes, length(processedNodes) + 1);
      processedNodes[length(processedNodes) - 1] := nextNeighbourNode;

      currentEdge.first := currentNode;
      currentEdge.second := nextNeighbourNode;
      setLength(edges, length(edges) + 1);
      edges[length(edges) -1] := currentEdge;
    end;
  end;

  result := edges;
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

  fillMatrix;
  refreshGrid;
end;

procedure TForm1.btn_treeClick(Sender: TObject);
var edges: TEdgeList;
begin
  edges := breadthFirstSearch();

  clearMatrix;
  applyEdgesToMatrix(edges);
  refreshGrid;
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
