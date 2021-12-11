when not defined(danger):
  import strformat

type Seq2D*[T] = ref object
  data: seq[T]
  width: int
  height: int

proc newSeq2D*[T](width, height: int): Seq2D[T] =
  result = Seq2D[T]()
  result.data = newSeq[T](width * height)
  result.width = width
  result.height = height

proc `width`*[T](this: Seq2D[T]): int =
  return this.width

proc `height`*[T](this: Seq2D[T]): int =
  return this.height

proc `[]`*[T](this: Seq2D[T], x, y: int): T =
  when not defined(danger):
    if x >= this.width:
      raise newException(Exception, fmt"x value of {x} outside bounds")
    if y >= this.height:
      raise newException(Exception, fmt"x value of {x} outside bounds")
  return this.data[x + this.width * y]

proc `[]=`*[T](this: Seq2D[T], x, y: int, t: T) =
  when not defined(danger):
    if x >= this.width:
      raise newException(Exception, fmt"x value of {x} outside bounds")
    if y >= this.height:
      raise newException(Exception, fmt"x value of {x} outside bounds")
  this.data[x + this.width * y] = t
