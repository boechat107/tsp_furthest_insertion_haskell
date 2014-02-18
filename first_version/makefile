# Andre Ambrosio Boechat

all:	tsp txttsp

tsp:	FurthestInsertion.hs ReadPoint.hs OpenglFI.hs
	ghc --make -O OpenglFI.hs

txttsp: FurthestInsertion.hs ReadPoint.hs TxtFI.hs
	ghc --make -O TxtFI

clean: 
	rm -f *.hi *.o
