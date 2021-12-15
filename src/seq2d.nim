import std/[decls,sugar,strformat,sequtils,setutils,bitops,math]

when not defined(nimSeqsV2):
  type 
    TGenericSeq {.compilerproc,pure,inheritable.} = object
      len,reserved: int
      when defined(gogc):
        elemSize: int
        elemAlign: int
    PGenericSeq = ptr TGenericSeq
    SeqUint64 = object of TGenericSeq
      data:ptr UncheckedArray[uint64]
else:
  type
    SeqUint64Content = object
      cap:int
      data:UncheckedArray[uint64]
    SeqUint64 = object
      len:int
      data: ptr SeqUint64Content
type
  BoolSeq*{.union.} = object
    data:seq[set[uint8]]
    fata:SeqUint64
func newBoolSeq*(length:int):BoolSeq = 
  let len = length div 256 + 2
  result.data = newSeqOfCap[set[uint8]](len)
  when defined(nimSeqsV2):
    cast[ptr int](addr result.data)[] = len
  else:
    var s = cast[PGenericSeq](result.data)
    s.len = len
  #newSeq[set[uint8]](length div 256 + 2)

func len*(b:BoolSeq):int{.inline.} = b.data.len

func `[]`*(b:BoolSeq,idx:int):bool{.inline.} = 
  #doAssert(idx div 256 < b.len,&"nope: {b.len=}")
  (idx mod 256).uint8 in b.data[idx div 256]

func `[]=`*(b:var BoolSeq,idx:int,val:bool){.inline.} =
  #template incl(x:array[4,uint64],y:uint)
  let
    x = idx div 256
    y = (uint8) (idx mod 256) # idx and 0xff
  #{.emit:"""NU v = (*b).data.p->data[x][(NU)(y)>>3];""".}
    #yshr = y shr 3
    #yand = y and 0x7
    #bitval = cast[uint64](val)
    #v = b.fata[x][yshr]
  #b.fata[x][yshr] = (v and not(1'u64 shl yand)) or (bitval shl yand)
  #{.emit:"""(*b).data.p->data[x][(NU)(y)>>3] &= ~(1U<<(y&7U));""".}
  #{.emit:"""(*b).data.p->data[x][(NU)(y)>>3] |= (val << (y&7U));""".}
  if val:
    b.data[x].incl y
  #  {.emit:"""(*b).data.p->data[x][(NU)(y)>>3] |= (1U<<(y&7U));//ha""".}
    #b.fata[x][y shr 3] = b.fata[x][y shr 3] or (1'u64 shl (y and 0x7'u64))
    #b.fata[x][y>>3] |=(1'uint shl (y and 0x7'u))
    #$1[(NU)($2)>>3] |=(1U<<($2&7U));$n
  else:
    b.data[x].excl y
    #$1[(NU){$2}>>3] &= ~(1U<<($2&7U));$n
#func set(b:var BoolSeq,idx:int,val:bool){.inline.} = 
#  b.fata[x][]
#template `[]=`*(b:var BoolSeq,idx:int,cond:untyped):untyped = 
#  if cond:
#    b.data[idx div 256].incl (idx mod 256).uint8
#  else:
#    b.data[idx div 256].excl (idx mod 256).uint8
func `[]=`*(b:var BoolSeq,idx:int,val:static bool){.inline.} =
  when val:
    b.data[idx div 256].incl (idx mod 256).uint8
  else:
    b.data[idx div 256].excl (idx mod 256).uint8


template initSeq2D*(width,height: int): BoolSeq = newBoolSeq(width * height)

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

