type
  BoolSeq* = object
    width*,height*:int
    data:seq[set[uint8]]

proc newBoolSeq*(width,height,length:int):BoolSeq =
  result.width = width
  result.height = height
  let len = length div 256 + 2
  result.data = newSeq[set[uint8]](len)

func `[]`*(b:BoolSeq,idx:int):bool{.inline.} = 
  (idx mod 256).uint8 in b.data[idx div 256]

func `[]=`*(b:var BoolSeq,idx:int,val:bool){.inline.} =
  let
    x = idx div 256
    y = uint8(idx mod 256)
  if val:
    b.data[x].incl y
  else:
    b.data[x].excl y

func `[]=`*(b:var BoolSeq,idx:int,val:static bool){.inline.} =
  when val:
    b.data[idx div 256].incl (idx mod 256).uint8
  else:
    b.data[idx div 256].excl (idx mod 256).uint8


template initSeq2D*(width,height: int): BoolSeq = newBoolSeq(width,height,width * height)
