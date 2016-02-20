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
    procedure gridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
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
  TAdjacencyMatrix = array of array of TVertexState;
  TVertexList = array of Integer;
  TEdgeList = array of TEdge;

var
  Form1: TForm1;
  adjacencyMatrix: TAdjacencyMatrix;

implementation


procedure clearMatrix(var matrix: TAdjacencyMatrix);
var
  x, y: Integer;
begin
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      matrix[x, y] := Unconnected;
end;

procedure fillMatrix(var matrix: TAdjacencyMatrix);
var
  x, y: Integer;
begin
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      if matrix[x, y] = Unset then
        matrix[x, y] := Unconnected;
end;

procedure applyEdgesToMatrix(var matrix: TAdjacencyMatrix; edges: TEdgeList);
var
  i: Integer;
begin
  for i := 0 to length(edges) - 1 do
  begin
    matrix[edges[i].first][edges[i].second] := Connected;
    matrix[edges[i].second][edges[i].first] := Connected;
  end;
end;

function isVertexInList(vertex: Integer; vertexList: TVertexList): Boolean;
var
  i: Integer;
begin
  for i := 0 to length(vertexList) - 1 do
    if vertexList[i] = vertex then
    begin
      result := True;
      exit;
    end;

  result := False;
end;

function getNextNeighbourVertex(matrix: TAdjacencyMatrix; vertex: Integer;
  exludeVertexes: TVertexList): Integer;
var
  y: Integer;
begin
  for y := 0 to length(matrix[vertex]) - 1 do
    if matrix[vertex, y] = Connected then
    begin
      if not isVertexInList(y, exludeVertexes) then
      begin
        result := y;
        exit;
      end;
    end;

  result := -1;
end;

function breadthFirstSearch(matrix: TAdjacencyMatrix): TEdgeList;
var
  currentVertex, nextNeighbourVertex: Integer;
  activeVertexes, processedVertexes: TVertexList;
  currentEdge: TEdge;
  edges: TEdgeList;
begin
  setLength(activeVertexes, 1);
  activeVertexes[0] := 0;
  setLength(processedVertexes, 1);
  processedVertexes[0] := 0;

  while length(activeVertexes) <> 0 do
  begin
    currentVertex := activeVertexes[0];
    nextNeighbourVertex := getNextNeighbourVertex(matrix, currentVertex, processedVertexes);

    if nextNeighbourVertex = -1 then
    begin
      Move(activeVertexes[1], activeVertexes[0], SizeOf(activeVertexes[0]) * (Length(activeVertexes) - 1));
      SetLength(activeVertexes, Length(activeVertexes) - 1);
    end
    else
    begin
      setLength(activeVertexes, length(activeVertexes) + 1);
      activeVertexes[length(activeVertexes) - 1] := nextNeighbourVertex;
      setLength(processedVertexes, length(processedVertexes) + 1);
      processedVertexes[length(processedVertexes) - 1] := nextNeighbourVertex;

      currentEdge.first := currentVertex;
      currentEdge.second := nextNeighbourVertex;
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
    if adjacencyMatrix[aCol - 1, aRow - 1] = Connected then
      adjacencyMatrix[aCol - 1, aRow - 1] := Unconnected
    else
      adjacencyMatrix[aCol - 1, aRow - 1] := Connected;

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

  setLength(adjacencyMatrix, input.Value);

  for i := 0 to length(adjacencyMatrix) - 1 do
    setLength(adjacencyMatrix[i], input.Value);

  fillMatrix(adjacencyMatrix);
  refreshGrid;
end;

procedure TForm1.btn_treeClick(Sender: TObject);
var edges: TEdgeList;
begin
  edges := breadthFirstSearch(adjacencyMatrix);

  clearMatrix(adjacencyMatrix);
  applyEdgesToMatrix(adjacencyMatrix, edges);
  refreshGrid;
end;

procedure TForm1.refreshGrid;
var
  x, y: Integer;
begin
  for x := 0 to length(adjacencyMatrix) - 1 do
    for y := 0 to length(adjacencyMatrix[x]) - 1 do
      if (x < grid.ColCount - 1) and (y < grid.RowCount - 1) then
        if adjacencyMatrix[x, y] = Unconnected then
          grid.Cells[x + 1, y + 1] := '0'
        else
          grid.Cells[x + 1, y + 1] := '1';
end;

end.
