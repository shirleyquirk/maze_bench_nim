import
  seq2d,
  point
import std/strformat
proc isReachable*(maze: BoolSeq;width,height:int; start, finish: Point): bool =
  var 
    visited = newBoolSeq(width*height)
    stack:seq[Point]
    p = start 
    
  
  visited[start.i] = true
  #echo &"{maze.len=},{visited.len=}"

  while true:
    if p == finish: return true

    for (di,dx,dy) in [(-width,0,-1),(width,0,1),(-1,-1,0),(1,1,0)]:
      let np:Point = (p.i+di,p.x+dx,p.y+dy)
      if likely(np.x>=0 and np.y >= 0):
        if likely(np.x < width and np.y < height):
          #doAssert(np.i div 256 < maze.len,&"nope: {np}")
          if not visited[np.i] and maze[np.i]:
            stack.add(np)
            visited[np.i] = true
    if stack.len == 0: return false
    p = stack.pop()

#[
proc isReachable*(maze: Seq2D, start, finish: Point): bool =
  let
    width = maze.width
    widthM1 = width - 1
    height = maze.height
    heightM1 = height - 1

  var
    visited = initSeq2D(width,height)
    stack = newSeq[Point]()
    p: Point = start

  visited[start.x, start.y] = true

  while true:
    # Check if we've reached the end
    if p == finish:
      return true

    # Check left
    if p.x > 0:
      let left = p.x - 1
      if not visited[left, p.y] and maze[left, p.y]:
        # Go left
        stack.add((left, p.y))
        visited[left, p.y] = true

    # Check right
    if p.x < widthM1:
      let right = p.x + 1
      if not visited[right, p.y] and maze[right, p.y]:
        # Go right
        stack.add((right, p.y))
        visited[right, p.y] = true

    # Check up
    if p.y > 0:
      let up = p.y - 1
      if not visited[p.x, up] and maze[p.x, up]:
        # Go up
        stack.add((p.x, up))
        visited[p.x, up] = true

    # Check down
    if p.y < heightM1:
      let down = p.y + 1
      if not visited[p.x, down] and maze[p.x, down]:
        # Go down
        stack.add((p.x, down))
        visited[p.x, down] = true

    if stack.len == 0:
      break
    
    p = stack.pop()

  return false

]#