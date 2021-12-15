import
  pixie,
  std/monotimes,
  strformat

import
  seq2d,
  point,
  mazesolver
proc main() =

  let image = readImage("maze1000.png")
  echo fmt"Loaded image: {image.width} x {image.height}"

  echo "Transforming image into maze..."
  var now = getMonoTime()

  var maze = initSeq2D(image.width, image.height)
  #var
  #  column: int
  #  row: int
  for i, rgba in image.data:
    maze[i] = (rgba.r + rgba.g + rgba.b) > 0
  #[  if column + 1 >= image.width:
      column = 0
      row += 1
    else:
      column += 1
  ]#
  echo fmt"Transformed image into maze in {getMonoTime() - now}"

  func toIdx(p:(int,int)):Point = (p[0] + p[1] * image.width,p[0],p[1])
  const 
    start = (0, 1)
    finish = (2000, 1999)
  let
    st_idx = start.toIdx
    fn_idx = finish.toIdx

  echo fmt"Solving maze from ({start[0]}, {start[1]}) to ({finish[0]}, {finish[1]}) ..."
  # Test "cold"
  now = getMonoTime()
  var success = maze.isReachable(image.width,image.height,st_idx, fn_idx)
  let coldTime = getMonoTime() - now
  echo &"cold:{success}"
  # Warm up
  for _ in 0..1000:
    discard maze.isReachable(image.width,image.height,st_idx, fn_idx)

  # Test "warm"
  now = getMonoTime()
  success = maze.isReachable(image.width,image.height,st_idx, fn_idx)
  let warmTime = getMonoTime() - now

  if success:
    echo fmt"Solved maze in: cold={coldTime}, warm={warmTime}"
  else:
    echo "Failed to solve maze."

main()