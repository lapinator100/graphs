unit app;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Spin,
  StdCtrls, ExtCtrls, Menus, Types;

type

  TEdge = record
    first: Integer;
    second: Integer;
    weight: Integer;
  end;

  TMatrix = array of array of Integer;
  TVertexList = array of Integer;
  TEdgeList = array of TEdge;
  TStringArray = array of String;

  { TAppForm }

  TAppForm = class(TForm)
    mainMenu: TMainMenu;
    markOnlyCheckbox: TCheckBox;
    openMenuItem: TMenuItem;
    saveMenuItem: TMenuItem;
    fileMenuItem: TMenuItem;
    panel: TPanel;
    matrixSizeInput: TSpinEdit;
    grid: TStringGrid;
    graph: TImage;
    splitter: TSplitter;
    breadthFirstSearchButton, depthFirstSearchButton, kruskalAlgorithmButton,
      PrimAlgorithmButton: TButton;
    openDialog: TOpenDialog;
    saveDialog: TSaveDialog;
    weightedCheckBox: TCheckBox;

    procedure FormCreate(Sender: TObject);

    procedure openMenuItemClick(Sender: TObject);
    procedure saveMenuItemClick(Sender: TObject);

    procedure matrixSizeInputChange(Sender: TObject);
    procedure weightedCheckBoxChange(Sender: TObject);

    procedure gridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure gridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    procedure graphResize(Sender: TObject);

    procedure breadthFirstSearchButtonClick(Sender: TObject);
    procedure depthFirstSearchButtonClick(Sender: TObject);
    procedure kruskalAlgorithmButtonClick(Sender: TObject);
    procedure PrimAlgorithmButtonClick(Sender: TObject);

    procedure updateUIWithEdges(edges: TEdgeList);
    procedure refreshGrid;
    procedure refreshGraph;
  end;

var
  AppForm: TAppForm;
  matrix, markedMatrix: TMatrix;
  weighted: boolean;

const
  Unset = 0;
  Unconnected = -1;
  Connected = 1;

  FileSeparator = ',';
  VertexDiameter = 24;

implementation


//-----data methods-----

{ Set all vertices in matrix given to unconnected }
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
  for x := 0 to high(matrix) do
    for y := 0 to high(matrix[x]) do
      if matrix[x, y] = Unset then
        matrix[x, y] := Unconnected;
end;

{ Add edges to matrix given }
procedure applyEdgesToMatrix(var matrix: TMatrix; edges: TEdgeList);
var
  i: Integer;
begin
  //loop through edges and set both directions for adjacency matrix as connected
  for i := 0 to high(edges) do
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

  for x := 1 to high(matrix) do
    for y := 0 to x - 1 do
      if matrix[x, y] <> Unconnected then
      begin
        setLength(edges, length(edges) + 1);
        edges[high(edges)] := createEdge(x, y, matrix[x, y]);
      end;

  result := edges;
end;

{ sort edges (upper half) by weight ascending or descending (bubble sort) }       //TODO: Prefer edges with lowest vertex
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

{ create a minimal spanning tree by adding vertices from lowest to highest
avoiding circles }
function kruskalAlgorithm(matrix: TMatrix): TEdgeList;
var
  i, j, k, first, second, temp, first_length: Integer;
  edges: TEdgeList;
  min_tree: array of TEdgeList;
begin
  //init
  setLength(min_tree, 0, 0);

  //sort all edges ascending
  edges := matrixToEdges(matrix);
  edges := sortEdges(edges, true);

  //loop edges from lowest to highest
  for i := 0 to high(edges) do
  begin
    //test if vertex of edge exists in min_tree
    first := -1;
    second := -1;

    for j := 0 to high(min_tree) do
      for k := 0 to high(min_tree[j]) do
      begin
        if (edges[i].first = min_tree[j, k].first)
            or (edges[i].first = min_tree[j, k].second) then
          first := j;
        if (edges[i].second = min_tree[j, k].first)
            or (edges[i].first = min_tree[j, k].second) then
          second := j;
      end;

    //if edge connects 2 clusters, add it

    //add edge as new cluster if not connected to other clusters
    if (first = -1) and (second = -1) then
    begin
      setLength(min_tree, length(min_tree) + 1);
      setLength(min_tree[high(min_tree)], length(min_tree[high(min_tree)]) + 1);
      min_tree[high(min_tree), high(min_tree[high(min_tree)])] := edges[i];
    end
    else
      //add edge to second list if connected to it
      if first = -1 then
      begin
        setLength(min_tree[second], length(min_tree[second]) + 1);
        min_tree[second, high(min_tree[second])] := edges[i];
      end
      else
        //add edge to first list if connected to it
        if second = -1 then
        begin
          setLength(min_tree[first], length(min_tree[first]) + 1);
          min_tree[first, high(min_tree[first])] := edges[i];
        end
        else
          //if edge is connecting 2 clusters, join them
          if first <> second then
          begin
            //first always contains smaller index
            if first > second then
            begin
              temp := first;
              first := second;
              second := temp;
            end;

            //merge cluster lists connected by new edge
            first_length := length(min_tree[first]);
            setLength(min_tree[first], length(min_tree[first])
              + length(min_tree[second]));

            for j := 0 to high(min_tree[second]) do
              min_tree[first, first_length + j] := min_tree[second, j];

            setLength(min_tree[second], 0);
          end;
  end;

  //look for last non-empty list to be returned
  for i := 0 to high(min_tree) do
    if length(min_tree[i]) <> 0 then
    begin
      result := min_tree[i];
      break;
    end;
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

{ split a string into an array }
function split(s: string; delimiter: char): TStringArray;
var
  i: Integer;
  stringlist: TStringList;
begin
  stringlist := TStringList.create;

  try
    stringlist.delimiter := delimiter;
    stringlist.DelimitedText := s;

    setLength(result, stringlist.Count);
    for i := 0 to high(result) do
      result[i] := stringlist.ValueFromIndex[i];
  finally
    stringlist.free;
  end;
end;

{ calculate vertex coordinates defined by table edge length, column count and
vertex number }
function getVertexPoint(table_el: double; cols, vertexNo: Integer): TPoint;
var
  h_table_el: double;
begin
  h_table_el := table_el / 2;
  result.X := round((vertexNo mod cols) * table_el + h_table_el);
  result.Y := round((vertexNo div cols) * table_el + h_table_el);
end;


//-----UI methods-----

{$R *.lfm}

{ executed on form creation }
procedure TAppForm.FormCreate(Sender: TObject);
begin
  //emulate change of adjacency matrix's size for initialization
  matrixSizeInputChange(Sender);
  //imitate change of weightedCheckBox
  weightedCheckBoxChange(Sender);

  //initialize symmetry and weighted bools
  weighted := weightedCheckBox.checked;
end;

{ load from file }
procedure TAppForm.openMenuItemClick(Sender: TObject);
var
  x, y: Integer;
  line: String;
  meta, row: TStringArray;
  stringlist: TStringList;
begin
  if openDialog.Execute then
  begin
    stringlist := TStringList.create;
    stringlist.loadFromFile(openDialog.FileName);

    //meta
    meta := split(stringlist.ValueFromIndex[0], FileSeparator);

    matrixSizeInput.value := StrToInt(meta[0]);
    matrixSizeInputChange(Sender);

    weightedCheckBox.Checked := StrToBool(meta[2]);

    //data
    for y := 0 to high(matrix) do
    begin
      line := stringlist.ValueFromIndex[y + 1];
      row := split(line, FileSeparator);
      for x := 0 to high(matrix) do
        matrix[x, y] := StrToInt(row[x]);
    end;

    refreshGrid;
  end;
end;

{ save to file }
procedure TAppForm.saveMenuItemClick(Sender: TObject);
var
  x, y: Integer;
  line: String;
  stringlist: TStringList;
begin
  stringlist := TStringList.create;

  //convert matrix to stringlist
  //meta info
  line := IntToStr(length(matrix)) + FileSeparator + BoolToStr(weighted);
  stringlist.append(line);

  //data
  for y := 0 to high(matrix) do
  begin
    line := '';
    for x := 0 to high(matrix) do
    begin
      line += IntToStr(matrix[x, y]);
      if x < high(matrix) then
        line += FileSeparator;
    end;

    stringlist.add(line);
  end;

  //request file path
  if saveDialog.Execute then
  begin
    //save
    try
      stringlist.saveToFile(saveDialog.fileName);
    finally
      stringlist.free;
    end;
  end;
end;

{ Handle change of matrix size matrixSizeInput value }
procedure TAppForm.matrixSizeInputChange(Sender: TObject);
var
  size: Integer;
begin
  size := matrixSizeInput.value;

  //set size of matrix
  setLength(matrix, size, size);
  setLength(markedMatrix, size, size);

  //adapt size of grid
  grid.RowCount := size + 1;
  grid.ColCount := size + 1;

  //fill new values and refresh UI
  clearMatrix(matrix);
  fillMatrix(matrix);
  fillMatrix(markedMatrix);
  refreshGrid;
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

    //change UI
    depthFirstSearchButton.Enabled := False;
    breadthFirstSearchButton.Enabled := False;
    kruskalAlgorithmButton.Enabled := True;
    PrimAlgorithmButton.Enabled := True;
    grid.Options := grid.Options + [goEditing]
  end
  else
  begin
    weighted := false;

    for i := 0 to length(matrix) - 1 do
      matrix[i, i] := Unconnected;

    //change UI
    depthFirstSearchButton.Enabled := True;
    breadthFirstSearchButton.Enabled := True;
    kruskalAlgorithmButton.Enabled := False;
    PrimAlgorithmButton.Enabled := False;
    grid.Options := grid.Options - [goEditing];
  end;

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
    matrix[aRow - 1, aCol - 1] := Unconnected;
  end
  else
  begin
    matrix[aCol - 1, aRow - 1] := Connected;
    matrix[aRow - 1, aCol - 1] := Connected;
  end;

  clearMatrix(markedMatrix);
  refreshGrid;
end;

{ Change matrix data when edited (Only weighted graphs) }
procedure TAppForm.gridValidateEntry(sender: TObject; aCol, aRow: Integer;
  const OldValue: string; var NewValue: String);
begin
  if not (acol = arow) then
    if (NewValue = '-') or (NewValue = '∞') then
    begin
      matrix[aCol - 1, aRow - 1] := Unconnected;
      matrix[aRow - 1, aCol - 1] := Unconnected;
    end
    else
    begin
      matrix[aCol - 1, aRow - 1] := StrToInt(NewValue);
      matrix[aRow - 1, aCol - 1] := StrToInt(NewValue);
    end;

  refreshGrid;
end;

{ Resize graph bitmap if graph size changed }
procedure TAppForm.graphResize(Sender: TObject);
begin
  graph.Picture.Bitmap.SetSize(graph.width, graph.height);
  refreshGraph;
end;

{ Perform breadth-first serach on adjacency matrix }
procedure TAppForm.breadthFirstSearchButtonClick(Sender: TObject);
var edges: TEdgeList;
begin
  edges := breadthFirstSearch(matrix);
  updateUIWithEdges(edges);
end;

{ Perform depth-first serach on adjacency matrix }
procedure TAppForm.depthFirstSearchButtonClick(Sender: TObject);
var edges: TEdgeList;
begin
  edges := depthFirstSearch(matrix);
  updateUIWithEdges(edges);
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
  updateUIWithEdges(edges);
end;

{ Perform Prim algorithm creating minimal spanning tree }
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
  updateUIWithEdges(edges);
end;

{ Update grid and graph with the edges given and global settings }
procedure TAppForm.updateUIWithEdges(edges: TEdgeList);
begin
  //mark edges if checkbox is checked
  if markOnlyCheckbox.Checked then
  begin
    clearMatrix(markedMatrix);
    applyEdgesToMatrix(markedMatrix, edges);;
  end
  else
  begin
    clearMatrix(matrix);
    clearMatrix(markedMatrix);
    applyEdgesToMatrix(matrix, edges);
  end;

  refreshGrid;
end;

{ Refresh grid UI from adjacency matrix }
procedure TAppForm.refreshGrid;
var
  x, y: Integer;
begin
  for x := 1 to grid.ColCount - 1 do
    grid.Cells[x, 0] := IntToStr(x);
  for y := 1 to grid.RowCount - 1 do
    grid.Cells[0, y] := IntToStr(y);

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
            grid.Cells[x + 1, y + 1] := '1';

    refreshGraph;
end;

{ paint graph based on adjacency matrix }
procedure TAppForm.refreshGraph;
var
  rect: TRect;
  s: String;
  p1, p2: TPoint;
  ar, table_el: double;
  cols, rows, radius, i, x, y: Integer;
begin
  //calculate aspect ratio
  ar := graph.Canvas.Height / graph.Canvas.Width;

  //calculate optimal arrangement
  cols := 1;
  rows := 1;
  while cols * rows < length(matrix) do
  begin
    if ((rows + 1) / cols) < ar then
      Inc(rows)
    else
      Inc(cols);
  end;

  table_el := graph.Canvas.Width / cols;

  graph.Canvas.clear;

  //paint edges
  for x := 0 to high(matrix) do
    for y := 0 to high(matrix) do
    begin
      if markedMatrix[x, y] <> Unconnected then
      begin
         graph.Canvas.Pen.Color := clGreen;
         graph.Canvas.Pen.Width := 3;
      end
      else if matrix[x, y] <> Unconnected then
      begin
         graph.Canvas.pen.Color := clBlack;
         graph.Canvas.Pen.Width := 1;
      end
      else
        continue;

      p1 := getVertexPoint(table_el, cols, x);
      p2 := getVertexPoint(table_el, cols, y);

      graph.Canvas.Line(p1.x, p1.y, p2.x, p2.y);
    end;

  //paint vertices
  graph.Canvas.pen.Color := clBlack;
  graph.Canvas.Pen.Width := 1;

  for i := 0 to high(matrix) do
  begin
    p1 := getVertexPoint(table_el, cols, i);

    if VertexDiameter <= table_el then
      radius := round(VertexDiameter / 2)
    else
      radius := round(table_el / 2);

    //circle
    rect.Left := p1.x - radius;
    rect.Top := p1.y - radius;
    rect.Right := p1.x + radius;
    rect.Bottom := p1.y + radius;

    graph.Canvas.Ellipse(rect);

    //text
    s := IntToStr(i + 1);

    graph.Canvas.TextOut(p1.x - (graph.Canvas.TextWidth(s) div 2),
      p1.y - (graph.Canvas.TextHeight(s) div 2), s);
  end;
end;

end.
