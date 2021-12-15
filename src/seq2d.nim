import std/[decls,sugar,strformat,sequtils,setutils,bitops,math]
type
  BoolSeq* = distinct seq[set[uint8]]

func newBoolSeq*(length:int):BoolSeq = (BoolSeq) newSeq[set[uint8]](length div 256 + 2)
func len*(b:BoolSeq):int{.borrow.}
func `[]`*(b:BoolSeq,idx:int):bool = 
  #doAssert(idx div 256 < b.len,&"nope: {b.len=}")
  (idx mod 256).uint8 in (seq[set[uint8]])(b)[idx div 256]
func `[]=`*(b:var BoolSeq,idx:int,val:bool) =
  if val:
    (seq[set[uint8]])(b)[idx div 256].incl (idx mod 256).uint8
  else:
    (seq[set[uint8]])(b)[idx div 256].excl (idx mod 256).uint8
func `[]=`*(b:var BoolSeq,idx:int,val:static bool) =
  when val:
    (seq[set[uint8]])(b)[idx div 256].incl (idx mod 256).uint8
  else:
    (seq[set[uint8]])(b)[idx div 256].excl (idx mod 256).uint8


proc initSeq2D*(width,height: int): BoolSeq = newBoolSeq(width * height)

#func `[]=`*(b:var BoolSeq,x,y:int,val:bool) = b[x+b.width*y]=val
#[
  const D = if sizeof(int) == 8: 6 else: 5
const M = (1 shl D) - 1
type BoolSeq = distinct seq[int]

template `[]`(b: BoolSeq, i: int): bool =
  template s: auto = seq[int](b)
  (s[i shr D] and (1 shl (i and M))) != 0

template `[]=`(b: var BoolSeq, i: int, flag: bool) =
  template s: auto = seq[int](b)
  if flag:
    s[i shr D] = s[i shr D] or (1 shl (i and M)) 
  else:
    s[i shr D] = s[i shr D] and not (1 shl (i and M))

proc initBoolSeq(n: int): BoolSeq =
  BoolSeq(newSeq[int]((n+M) shr D))

type Seq2D* = object
  data: (when defined(boolseq): seq[set[uint8]] else: seq[bool])
  width*: int
  height*: int

proc initSeq2D*(width, height: int): Seq2D =
  result.data = when defined(boolseq): newSeq[set[uint8]](ceil(width.float*height.float/256.0).int else: newSeq[bool](width * height)
  #result.datacopy = newSeq[bool](width*height)
  result.width = width
  result.height = height
template modeight(x:int):int = x and 0x7
template xontains(s:set[range[0..2000]],x:int):bool =
  #[
  type Arr = object
    big:array[31,uint64]
    lit:array[17,byte]
  let arr = cast[Arr](s)
  cast[bool](
    if x < 1984:
      arr.big[x div 64] shr (x mod 64) and 1
    else:
      arr.lit[(x-1984) div 8] shr (x mod 8) and 1)
  ]#
  cast[bool](cast[array[32,uint64]](s)[x div 64] shr (x mod 64) and 1)
  #cast[bool](cast[array[251,byte]](s)[x shr 3] shr (x.modeight) and 1)

proc `[]`*(this: BoolSeq, x:int): bool {.inline.} = 
  when false:#not defined(danger):
    if x >= this.width:
      raise newException(Exception, fmt"x value of {x} outside bounds")
    if y >= this.height:
      raise newException(Exception, fmt"y value of {y} outside bounds")
  let idx = x + this.width * y
  when defined(boolseq):
    (idx mod 256).uint8 in this.data[idx div 256] #cast[uint8](idx) in this.data[idx shr 8]
  else: this.data[idx]

proc set(this:var set[range[0..2000]],y:int,t:bool) =
  var arr{.byaddr.} = cast[var array[32,uint64]](this.addr)
  template bit:auto = y mod 64
  template bitval:auto = cast[uint64](t)
  arr[y div 64] = (arr[y div 64] and (not (1'u64 shl bit))) or (bitval shl bit)
template `[]=`*(this:var Seq2D; x, y: int, t: bool) =
  when false:#not defined(danger):
    if x >= this.width:
      raise newException(Exception, fmt"x value of {x} outside bounds")
    if y >= this.height:
      raise newException(Exception, fmt"y value of {y} outside bounds")
  when defined(boolseq): 
    this.data[x].set(y,t)#(value & ~(1 << bit)) | (bitval << bit)
    #if t: this.data[x].incl y
    #else: this.data[x].excl y
  else: this.data[x + this.width * y] = t
  #this.datacopy[x + this.width * y] = t
  
]#

