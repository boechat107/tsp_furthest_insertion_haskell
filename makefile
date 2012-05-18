# Andre Ambrosio Boechat

all:	tsp txttsp

tsp:	KDtree.hs Btree.hs FurthestInsertion.hs ReadPoint.hs OpenglFI.hs
	ghc --make -O OpenglFI.hs

txttsp: KDtree.hs Btree.hs FurthestInsertion.hs ReadPoint.hs TxtFI.hs
	ghc --make -O TxtFI

clean: 
	rm -f *.hi *.o OpenglFI TxtFI

kdtree:
	ghc --make KDtree.hs
