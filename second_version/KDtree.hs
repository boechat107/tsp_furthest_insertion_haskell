{-# OPTIONS_GHC -fglasgow-exts #-}
module KDtree where
import Data.List
import ReadPoint

type Dimen = Point -> Double
type HyperRectangle = ((Double, Double), (Double, Double))

data Side = Before | After
	deriving(Eq)

data BelongT = Into | NoInto
	deriving(Show)

data KDtree = 
	KNode Point Dimen HyperRectangle BelongT KDtree KDtree 
    | KLeaf Point BelongT (Maybe Point) (Maybe Point) 
    | DeadKLeaf


instance Show KDtree where
	show (KNode a _ b c d e) =
		"KNode" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " (" ++ show d ++ ") " ++ " (" ++ show e ++ ") " 
	show (KLeaf a b c d) =
		"KLeaf " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d
	show (DeadKLeaf) = 
		"DeadKLeaf" 

isDead DeadKLeaf = True
isDead _ = False

left (KNode _ _ _ _ pts _) = pts
right (KNode _ _ _ _ _ pts) = pts


splitOnMedian dimsel median  = partition (\ p -> dimsel p <= dimsel median)


hyperRectangle (p:pts) = foldr xyminmax ((xcoord p, ycoord p), (xcoord p, ycoord p)) pts
    where xyminmax p ((xmin, ymin), (xmax, ymax)) =
            let nxmin = if xcoord p < xmin 
                        then xcoord p
                        else xmin
                nymin = if ycoord p < ymin
                        then ycoord p
                        else ymin
                nxmax = if xcoord p > xmax
                        then xcoord p
                        else xmax
                nymax = if ycoord p > ymax
                        then ycoord p
                        else ymax
            in ((nxmin, nymin), (nxmax, nymax))


largestDim ((xmin, ymin), (xmax, ymax)) 
    | xmax - xmin < ymax - ymin = ycoord
    | otherwise = xcoord


medianOfThree dimsel (p0:p1:p2:pts) = 
	let	x = ((xcoord p0) + (xcoord p1) + (xcoord p2)) / 3.0
		y = ((ycoord p0) + (ycoord p1) + (ycoord p2)) / 3.0
	in	if (dimsel p0) == (dimsel p1) && (dimsel p1) == (dimsel p2)
		then error (show $ p0:p1:pts)
		else (x,y)


-- Constroi a kdtree com todos os pontos do problema e marcando-os como nao-presentes no tour
kdtBuild [] = error "Uma das arvores eh vazia!"
kdtBuild [pt] = KLeaf pt NoInto Nothing Nothing
kdtBuild pts@[pt0,pt1] = 
    let hr = hyperRectangle pts
        ld = largestDim hr
    in  if pt0 == pt1
		then KLeaf pt0 NoInto Nothing Nothing
		else if ld pt0 <= ld pt1
		then KNode pt0 ld hr NoInto (KLeaf pt0 NoInto Nothing Nothing) (KLeaf pt1 NoInto Nothing Nothing)
        else KNode pt1 ld hr NoInto (KLeaf pt1 NoInto Nothing Nothing) (KLeaf pt0 NoInto Nothing Nothing)
kdtBuild pts = 
    let hr = hyperRectangle pts
        ld = largestDim hr
        md = medianOfThree ld pts
        (lpts, rpts) = splitOnMedian ld md pts
    in  KNode md ld hr NoInto (kdtBuild lpts) (kdtBuild rpts)


-- Verifica quais ramos da kdtree possuem pontos ja inseridos no tour
treep (KNode _ _ _ Into _ _) = True
treep (KLeaf _ Into _ _) = True
treep _ = False


-- Nearest Neighbour na kdtree
-- Usada para buscar o ponto do tour mais proximo ao vertice livre pt
kdtNN _ DeadKLeaf _ = error "No Nearest Neighbor in Dead KLeaf"
kdtNN _ (KLeaf _ NoInto _ _) _ = error "Este ponto ainda nao esta no tour"
kdtNN _ (KLeaf nnpt Into (Just ptPrev) (Just ptNext)) _ = (nnpt, ptPrev, ptNext)
kdtNN dist (KNode median dim hr _ lnode rnode) pt 
   | not(treep lnode) = kdtNN dist rnode pt
   | not(treep rnode) = kdtNN dist lnode pt
   | dim pt <= dim median 
   , (nnpt, ptP, ptN) <- kdtNN dist lnode pt 
   , d <- dist nnpt pt = 
         if dim pt +  d > dim median               
         then let (nnrpt, ptrP, ptrN) = kdtNN dist rnode pt
                  dr = dist pt nnrpt
              in  if dr < d
                  then (nnrpt, ptrP, ptrN)
                  else (nnpt, ptP, ptN)
         else (nnpt, ptP, ptN)
   | otherwise
   , (nnpt, ptP, ptN) <- kdtNN dist rnode pt
   , d <- dist nnpt pt = 
         if dim pt - d < dim median
         then let (nnlpt, ptlP, ptlN) = kdtNN dist lnode pt
                  dl = dist pt nnlpt
              in  if dl < d
                  then (nnlpt, ptlP, ptlN)
                  else (nnpt, ptP, ptN)
         else (nnpt, ptP, ptN)


-- Insercao de vertices no tour
insertKD (KNode med ld hr _ ltree rtree) pts@(pt0,_,_)
	| isDead ltree || isDead rtree = error "Alguma subarvore esta vazia" 
	| ld pt0 > ld med = case (insertKD rtree pts) of
					taux -> KNode med ld hr Into ltree taux
	| otherwise = case (insertKD ltree pts) of
					taux -> KNode med ld hr Into taux rtree

insertKD (KLeaf pt NoInto Nothing Nothing) (pt0, ptPrev, ptNext)
	| pt /= pt0 = error "Caminhou errado ou este ponto nao existe!"
	| otherwise = KLeaf pt0 Into (Just ptPrev) (Just ptNext)


-- Modificacao da KDtree
-- Altera apenas o ponto posterior ou anterior ao ponto pt0
modifyBySide (KNode med ld hr Into ltree rtree) pts@(pt0,_) side
	| isDead ltree || isDead rtree = error "Alguma subarvore esta vazia"
	| ld pt0 > ld med = case (modifyBySide rtree pts side) of
					taux -> KNode med ld hr Into ltree taux
	| otherwise = case (modifyBySide ltree pts side) of
					taux -> KNode med ld hr Into taux rtree

modifyBySide (KLeaf pt Into ptl ptr) (pt0,ptaux) side
	| pt /= pt0 = error "Caminhou errado ou este ponto nao existe!"
	| side == Before = KLeaf pt0 Into (Just ptaux) ptr
	| otherwise = KLeaf pt0 Into ptl (Just ptaux)


-- Procura pelo proximo ponto do tour na kdtree em relacao a ptRef
searchPrevNext (KNode med ld _ Into ltree rtree) ptRef 
	| isDead ltree || isDead rtree = error "Alguma subarvore esta vazia"
	| ld ptRef > ld med = searchPrevNext rtree ptRef
	| otherwise = searchPrevNext ltree ptRef

searchPrevNext (KLeaf pt Into (Just ptPrev) (Just ptNext)) ptRef 
	| pt /= ptRef = error "Caminhou errado ou este ponto nao existe!"
	| otherwise = (ptPrev,ptNext)




kdtToList DeadKLeaf = error "Alguma subarvore esta vazia"
kdtToList (KLeaf pt _ _ _) = [pt]
kdtToList (KNode _ _ _ _ ltree rtree) = kdtToList ltree ++ kdtToList rtree


