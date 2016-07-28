

data H = U | D | L | R | UR | DR | UL | DL deriving Read


p U (_,y) (px,(y0,y1)) = (px,(y0,y))
p D (_,y) (px,(y0,y1)) = (px,(y,y1))
p L (x,_) ((x0,x1),py) = ((x0,x),py)
p R (x,_) ((x0,x1),py) = ((x,x1),py)
p UR d l = p U d (p R d l)
p UL d l = p U d (p L d l)
p DL d l = p D d (p L d l)
p DR d l = p D d (p R d l)



