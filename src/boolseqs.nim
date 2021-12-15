type
    BoolSeq = object
      data: seq[set[uint8]]
    
func newBoolSeq(length:int):BoolSeq = 
    result.data = newSeq[set[uint8]](length div 256 + 1)

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

