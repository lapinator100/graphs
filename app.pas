unit app;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Spin,
  StdCtrls, Types;

type

  { TAppForm }

  TAppForm = class(TForm)
    breadthFirstSearchButton: TButton;
    kruskalAlgorithmButton: TButton;
    PrimAlgorithmButton: TButton;
    weightedCheckBox: TCheckBox;
    symmetryCheckBox: TCheckBox;
    depthFirstSearchButton: TButton;
    matrixSizeInput: TSpinEdit;
    grid: TStringGrid;

    procedure breadthFirstSearchButtonClick(Sender: TObject);
    procedure kruskalAlgorithmButtonClick(Sender: TObject);
    procedure depthFirstSearchButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure gridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    procedure matrixSizeInputChange(Sender: TObject);
    procedure PrimAlgorithmButtonClick(Sender: TObject);
    procedure symmetryCheckBoxChange(Sender: TObject);

    procedure refreshGrid;
    procedure weightedCheckBoxChange(Sender: TObject);
  end;

  TEdge = record
    first: Integer;
    second: Integer;
    weight: Integer;
  end;

  TMatrix = array of array of Integer;
  TVertexList = array of Integer;
  TEdgeList = array of TEdge;

var
  AppForm: TAppForm;
  matrix: TMatrix;
  weighted, symmetric: boolean;

const
  Unset = 0;
  Unconnected = -1;
  Connected = 1;

implementation


{ Set all vertexes in matrix given to unconnected }
procedure clearMatrix(var matrix: TMatrix);
var
  x, y: Integer;
begin
  //loop through matrix and set each value to unconnected
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      matrix[x, y] := Unconnected;
end;

{ Fill matrix given by settings all unset vertexes to unconnected }
procedure fillMatrix(var matrix: TMatrix);
var
  x, y: Integer;
begin
  //loop through matrix and set each value to unconnected if it was not set before
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      if matrix[x, y] = Unset then
        matrix[x, y] := Unconnected;
end;

{ Add edges to matrix given }
procedure applyEdgesToMatrix(var matrix: TMatrix; edges: TEdgeList);
var
  i: Integer;
begin
  //loop through edges and set both directions for adjacency matrix as connected
  for i := 0 to length(edges) - 1 do
  begin
    if weighted then
    begin
      matrix[edges[i].first][edges[i].second] := edges[i].weight;
      matrix[edges[i].second][edges[i].first] := edges[i].weight;
    end
    else
    begin
      matrix[edges[i].first][edges[i].second] := Connected;
      matrix[edges[i].second][edges[i].first] := Connected;
    end;
  end;
end;

{ Test wether a certain vertex is contained in list for vertices given }
function isVertexInList(vertex: Integer; vertexList: TVertexList): Boolean;
var
  i: Integer;
begin
  //loop through list of vertexes
  for i := 0 to length(vertexList) - 1 do
    //early return true if current vertex equals vertex given
    if vertexList[i] = vertex then
    begin
      result := True;
      exit;
    end;

  result := False;
end;

{ Get next neighbour node of vertex given

  Return first neighbour vertex for given vertex that is not in the
  to-be-exluded list. If no neighbour was found, return -1.
}
function getNextNeighbourVertex(matrix: TMatrix; vertex: Integer;
  exludeVertexes: TVertexList): Integer;
var
  y: Integer;
begin
  //loop through adjacency matrix column for vertex given
  for y := 0 to length(matrix[vertex]) - 1 do
    //early return vertex if is connected and not in to-be-excluded list
    if (matrix[vertex, y] = Connected) and (not isVertexInList(y, exludeVertexes)) then
    begin
      result := y;
      exit;
    end;

  result := -1;
end;

{ Perform breadth-first search on adjacency matrix given

  Return list of edges being a spanning tree.
}
function breadthFirstSearch(matrix: TMatrix): TEdgeList;
var
  currentVertex, nextNeighbourVertex: Integer;
  activeVertexes, processedVertexes: TVertexList;
  currentEdge: TEdge;
  edges: TEdgeList;
begin
  //initialize active and processed vertexes list with first vertex
  setLength(activeVertexes, 1);
  activeVertexes[0] := 0;
  setLength(processedVertexes, 1);
  processedVertexes[0] := 0;

  //repeat until all active nodes' neighbours are processed
  while length(activeVertexes) <> 0 do
  begin
    //get first vertex from active vertex and its next neighbour
    currentVertex := activeVertexes[0];
    nextNeighbourVertex := getNextNeighbourVertex(matrix, currentVertex, processedVertexes);

    //test wether a neighbour was found
    if nextNeighbourVertex = -1 then
    begin
      //remove first vertex from active vertexes list
      Move(activeVertexes[1], activeVertexes[0], SizeOf(activeVertexes[0]) * (Length(activeVertexes) - 1));
      SetLength(activeVertexes, Length(activeVertexes) - 1);
    end
    else
    begin
      //add neighbour vertex to active and proccessed neighbor list
      setLength(activeVertexes, length(activeVertexes) + 1);
      activeVertexes[length(activeVertexes) - 1] := nextNeighbourVertex;
      setLength(processedVertexes, length(processedVertexes) + 1);
      processedVertexes[length(processedVertexes) - 1] := nextNeighbourVertex;

      //add edge from currently active to neighbour vertex
      currentEdge.first := currentVertex;
      currentEdge.second := nextNeighbourVertex;
      setLength(edges, length(edges) + 1);
      edges[length(edges) -1] := currentEdge;
    end;
  end;

  result := edges;
end;

{ Perform depth-first search on adjacency matrix given

  Return list of edges being a spanning tree.
}
function depthFirstSearch(matrix: TMatrix): TEdgeList;
var
  currentVertex, nextNeighbourVertex: Integer;
  activeVertexes, processedVertexes: TVertexList;
  currentEdge: TEdge;
  edges: TEdgeList;
begin
  //initialize active and processed vertexes list with first vertex
  setLength(activeVertexes, 1);
  activeVertexes[0] := 0;
  setLength(processedVertexes, 1);
  processedVertexes[0] := 0;

  //repeat until all active nodes' neighbours are processed
  while length(activeVertexes) <> 0 do
  begin
    //get last vertex from active vertex and its next neighbour
    currentVertex := activeVertexes[length(activeVertexes) - 1];
    nextNeighbourVertex := getNextNeighbourVertex(matrix, currentVertex, processedVertexes);

    //test wether a neighbour was found
    if nextNeighbourVertex = -1 then
    begin
      //remove last vertex from active vertexes list
      SetLength(activeVertexes, Length(activeVertexes) - 1);
    end
    else
    begin
      //add neighbour vertex to active and proccessed neighbor list
      setLength(activeVertexes, length(activeVertexes) + 1);
      activeVertexes[length(activeVertexes) - 1] := nextNeighbourVertex;
      setLength(processedVertexes, length(processedVertexes) + 1);
      processedVertexes[length(processedVertexes) - 1] := nextNeighbourVertex;

      //add edge from currently active to neighbour vertex
      currentEdge.first := currentVertex;
      currentEdge.second := nextNeighbourVertex;
      setLength(edges, length(edges) + 1);
      edges[length(edges) -1] := currentEdge;
    end;
  end;

  result := edges;
end;

{ return edge (convenience function }
function createEdge(first, second, weight: Integer): TEdge;
begin
  result.first := first;
  result.second := second;
  result.weight := weight;
end;

{ convert weighted matrix to list of weighted edges  }
function matrixToEdges(matrix: TMatrix): TEdgeList;
var
  x, y: Integer;
  edges: TEdgeList;
begin
  setLength(edges, 0);

  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix) - 1 do
      if not matrix[x, y] = Unconnected then
      begin
        setLength(edges, length(edges) + 1);
        edges[length(edges) - 1] := createEdge(x, y, matrix[x, y]);
      end;

  result := edges;
end;

{ sort edges by weight ascending or descending (bubble sort) }
function sortEdges(edges: TEdgeList; ascending: Boolean): TEdgeList;
var
  i: Integer;
  temp: TEdge;
  done: Boolean;
begin
  repeat
  begin
    done := True;

    for i := 0 to length(edges) - 2 do
    begin
      if ((edges[i].weight > edges[i + 1].weight) and ascending) or
        ((edges[i].weight < edges[i + 1].weight) and (not ascending)) then
      begin
        temp := edges[i];
        edges[i] := edges[i + 1];
        edges[i + 1] := temp;

        done := False;
      end;
    end;
  end;
  until done;

  result := edges;
end;

function circleExisting(edges: TEdgeList): Boolean;
begin
  result := False; //dummy, modified depth-first search to be applied
end;

function kruskalAlgorithm(matrix: TMatrix): TEdgeList;
var
  i: Integer;
  e: TEdge;
  e0, e1: TEdgeList;
begin
  e1 := matrixToEdges(matrix);
  e1 := sortEdges(e1, true);

  //loop edges from lowest to highest
  for i := 0 to length(e1) - 1 do
    //add next edge
    setLength(e0, length(e0) + 1);
    e0[high(e0)] := e1[i];

    //remove edge if circle existing
    if circleExisting(e0) then
      setLength(e0, length(e0) - 1);

  result := e0;
end;

function primAlgorithm(matrix: TMatrix): TEdgeList;
var
  i, j: Integer;
  edge: TEdge;
  v_done, v_pending: TVertexList;
  e_used, e_neighbours: TEdgeList;
begin
  //create initial lists
  setLength(e_used, 0);

  //fill v_done
  setLength(v_done, 1);
  v_done[0] := 0;

  //fill v_pending
  setLength(v_pending, length(matrix) - 1);
  for i := 0 to length(matrix) - 2 do
  begin
    v_pending[i] := i + 1;
  end;

  repeat
  begin
    //deplete list of neighbour edges
    setLength(e_neighbours, 0);

    //list all neighbour vertices
    for i := 0 to length(v_done) - 1 do
      for j := 0 to length(v_pending) - 1 do
      begin
        //add connected neighbours to list
        if matrix[v_done[i], v_pending[j]] <> Unconnected then
        begin
          setlength(e_neighbours, length(e_neighbours) + 1);
          e_neighbours[high(e_neighbours)] := createEdge(v_done[i],
            v_pending[j], matrix[v_done[i], v_pending[j]]);
        end;
      end;

    //sort neighbour edges descending
    e_neighbours := sortEdges(e_neighbours, false);

    //take neighbour edge with smallest weight and add it to output
    edge := e_neighbours[high(e_neighbours)];

    //append new edge
    setlength(e_used, length(e_used) + 1);
    e_used[high(e_used)] := edge;

    //append new vertex
    setLength(v_done, length(v_done) + 1);
    v_done[high(v_done)] := edge.second;

    //remove added vertex from pending list
    for i := 0 to length(v_pending) - 1 do
      if v_pending[i] = edge.second then
      begin
        for j := 0 to high(v_pending) - i - 1 do
          v_pending[i + j] := v_pending[i + j + 1];
        break;
      end;
    setLength(v_pending, length(v_pending) - 1);
  end;
  until length(v_done) >= length(matrix);

  result := e_used;
end;

{$R *.lfm}

{ executed on form creation }
procedure TAppForm.FormCreate(Sender: TObject);
begin
  //emulate change of adjacency matrix's size for initialization
  matrixSizeInputChange(Sender);

  //initialize symmetry and weighted bools
  symmetric := symmetryCheckBox.checked;
  weighted := weightedCheckBox.checked;
end;

{ Change matrix data when edited (Only weighted graphs) }
procedure TAppForm.gridValidateEntry(sender: TObject; aCol, aRow: Integer;
  const OldValue: string; var NewValue: String);
begin
  if not (acol = arow) then
    if (NewValue = '-') or (NewValue = '∞') then
      matrix[aCol - 1, aRow - 1] := Unconnected
    else
      matrix[aCol - 1, aRow - 1] := StrToInt(NewValue);

  refreshGrid;
end;

{ Handle cell selection in grid }
procedure TAppForm.gridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  //don't do anything if matrix is weighted
  if weighted then
    exit;

  CanSelect := True;

  //make sure a valid position was selected
  if (aCol = 0) or (aRow = 0) then
    exit;

  //disallow a vertex being connected with itself
  if aCol = aRow then
    exit;

  //change vertex state based on its current state
  if matrix[aCol - 1, aRow - 1] = Connected then
  begin
    matrix[aCol - 1, aRow - 1] := Unconnected;

    if symmetric then
      matrix[aRow - 1, aCol - 1] := Unconnected;
  end
  else
  begin
    matrix[aCol - 1, aRow - 1] := Connected;

    if symmetryCheckBox.checked then
      matrix[aRow - 1, aCol - 1] := Connected;
  end;

  refreshGrid;
end;

{ Handle change of matrix size matrixSizeInput value }
procedure TAppForm.matrixSizeInputChange(Sender: TObject);
var
  i: Integer;
begin
  //adapt size of grid
  grid.RowCount := matrixSizeInput.Value + 1;
  grid.ColCount := matrixSizeInput.Value + 1;

  //adapt size of matrix
  setLength(matrix, matrixSizeInput.Value, matrixSizeInput.Value);

  //fill new values and refresh UI
  fillMatrix(matrix);
  refreshGrid;
end;

{ Perform breadth-first serach on adjacency matrix }
procedure TAppForm.breadthFirstSearchButtonClick(Sender: TObject);
var edges: TEdgeList;
begin
  edges := breadthFirstSearch(matrix);

  //reset current matrix, add edges, and resfresh UI
  clearMatrix(matrix);
  applyEdgesToMatrix(matrix, edges);
  refreshGrid;
end;

{ Perform Kruskal algorithm creating minimal spanning tree }
procedure TAppForm.kruskalAlgorithmButtonClick(Sender: TObject);
var
  edges: TEdgeList;
begin
  if not weighted then
  begin
    showMessage('Der Kruskal-Algorithmus ist nur auf gewichtete Graphen anwendbar.');
    exit;
  end;

  edges := kruskalAlgorithm(matrix);

  //reset current matrix, add edges and refresh UI
  clearMatrix(matrix);
  applyEdgesToMatrix(matrix, edges);
  refreshGrid;
end;

procedure TAppForm.PrimAlgorithmButtonClick(Sender: TObject);
var
  edges: TEdgeList;
begin
  if not weighted then
  begin
    showMessage('Der Prim-Algorithmus ist nur auf gewichtete Graphen anwendbar.');
    exit;
  end;

  edges := primAlgorithm(matrix);

  //reset current matrix, add edges and refresh UI
  clearMatrix(matrix);
  applyEdgesToMatrix(matrix, edges);
  refreshGrid;
end;

{ Perform depth-first serach on adjacency matrix }
procedure TAppForm.depthFirstSearchButtonClick(Sender: TObject);
var edges: TEdgeList;
begin
  edges := depthFirstSearch(matrix);

  //reset current matrix, add edges, and resfresh UI
  clearMatrix(matrix);
  applyEdgesToMatrix(matrix, edges);
  refreshGrid;
end;

{ Refresh grid UI from adjacency matrix }
procedure TAppForm.refreshGrid;
var
  x, y: Integer;
begin
  //loop through matrix
  for x := 0 to length(matrix) - 1 do
    for y := 0 to length(matrix[x]) - 1 do
      //make sure coordinates are inside bounds of grid
      if (x < grid.ColCount - 1) and (y < grid.RowCount - 1) then
        //grid.Cells[x + 1, y + 1] := IntToStr(matrix[x, y]);exit;               //for testing
        //display '∞' for unconnected and else the element's value if weighted
        if weighted then
          if matrix[x, y] = Unconnected then
            grid.Cells[x + 1, y + 1] := '∞'
          else
            grid.Cells[x + 1, y + 1] := IntToStr(matrix[x, y])
        else
        //display '0' for unconnected and '1' for connected if matrix is not weighted
          if matrix[x, y] = Unconnected then
            grid.Cells[x + 1, y + 1] := '0'
          else if matrix[x, y] = Connected then
            grid.Cells[x + 1, y + 1] := '1'
end;

{ Enable or Disable Editing according to weightedCheckBox }
procedure TAppForm.weightedCheckBoxChange(Sender: TObject);
var
  i: Integer;
begin
  if weightedCheckBox.checked then
  begin
    weighted := true;

    for i := 0 to length(matrix) - 1 do
      matrix[i, i] := Unconnected;

    grid.Options := grid.Options + [goEditing]
  end
  else
  begin
    weighted := false;

    for i := 0 to length(matrix) - 1 do
      matrix[i, i] := Unconnected;

    grid.Options := grid.Options - [goEditing];
  end;

  refreshGrid;
end;

{ Mirror adjacency matrix if checkbox is checked }
procedure TAppForm.symmetryCheckBoxChange(Sender: TObject);
var
  x, y: Integer;
begin
  if symmetryCheckBox.checked then
  begin
    symmetric := true;

    //loop through matrix
    for x := 0 to length(matrix) - 1 do
      for y := (x + 1) to length(matrix[x]) - 1 do
        //mirror values from upper half to lower half
        matrix[x, y] := matrix[y, x];

    refreshGrid;
  end
  else
    symmetric := false;
end;

end.
