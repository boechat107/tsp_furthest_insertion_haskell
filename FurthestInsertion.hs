{-# OPTIONS_GHC -fglasgow-exts #-}
module FurthestInsertion where
import ReadPoint
import Btree
import KDtree
import Data.List
import Debug.Trace


data PointDist = PointDist (Point,Double)
--	deriving(Show)


fstPd (PointDist a) = fst a
sndPd (PointDist a) = snd a

instance Eq PointDist where
	(PointDist (pt1,_)) == (PointDist (pt2,_)) = pt1 == pt2

instance Ord PointDist where
	(PointDist (_,d1)) < (PointDist (_,d2)) = d1 <= d2
	(PointDist (_,d1)) > (PointDist (_,d2)) = d1 > d2

instance Show PointDist where
	show (PointDist (pt,d)) = show pt

-- Ponto mais distante ao tour
furthestPoint pts ktour  
	| newDist < (sndPd pt) = 
	    furthestPoint (insertTree (deleteMaxNode pts) (PointDist (fstPd pt, newDist))) ktour
	| newDist == (sndPd pt) = ((fstPd pt, nnpt), deleteMaxNode pts)
		where
		pt = maxNode pts
		(newDist, nnpt) = distanceFromTour (fstPd pt) ktour


-- Distancia de um ponto ao tour, justamente com o ponto do tour mais proximo
distanceFromTour pt ktour = 
	let	(nnpt, ptP, ptN) = kdtNN dist ktour pt
	in	(dist pt nnpt, nnpt)


-- Loop principal do Furthest Insertion
furthestInsertion Leaf ktour = 
	buildTour ktour (anything ktour) []

furthestInsertion pts ktour = 
	let	((newPt, nnpt), newPts) = furthestPoint pts ktour 
		ptPos = bestPosition ktour newPt nnpt
		(ptPrev,ptNext) = searchPrevNext ktour ptPos
		newTour = modifyKD ktour After newPt (ptPos,ptPrev,ptNext)
	in	furthestInsertion newPts newTour


-- Marca na kdtre o novo ponto inserido no tour e altera os dois pontos
-- diretamente envolvidos na insercao
modifyKD ktour pos newPt (nnpt,ptP,ptN)
	| pos == Before =
		let	k1 = insertKD ktour (newPt,ptP,nnpt)
			k2 = modifyBySide k1 (nnpt,newPt) Before
		in	modifyBySide k2 (ptP,newPt) After
	| pos == After =
		let	k1 = insertKD ktour (newPt,nnpt,ptN)
			k2 = modifyBySide k1 (nnpt,newPt) After
		in	modifyBySide k2 (ptN,newPt) Before

-- Retorna o ponto do tour que devera ser o anterior ao novo ponto a ser
-- inserido.
bestPosition ktour newPt fstPt = 
	let	(_,ptN) = searchPrevNext ktour fstPt
		d = distExp newPt fstPt ptN
	in	auxFunc ktour newPt (fstPt,ptN) (fstPt,d)
		where
		auxFunc ktour newPt (fstPt,curPt) (pt1,d1)
			| fstPt == curPt = pt1
			| d2 > d1 = auxFunc ktour newPt (fstPt,pt2) (pt1,d1)
			| otherwise = auxFunc ktour newPt (fstPt,pt2) (curPt,d2)
				where
				(_,pt2) = searchPrevNext ktour curPt
				d2 = distExp newPt curPt pt2


-- Expressao a ser minimizada para insercao de um novo vertice ao tour
distExp = \x y z -> (dist x y) + (dist x z) - (dist y z)


-- Criacao do tour
-- mkTreeDist cria uma Btree de (ponto, distancia)
startFurthestInsertion list@(pt0:pts) =
	let	btree = mkTreeDist pt0 pts Leaf
		pt1 = fstPd $ maxNode btree
		btree' = deleteMaxNode btree
		kdtree = insertKD (insertKD (kdtBuild list) (pt0,pt1,pt1)) (pt1,pt0,pt0)
	in	return $ furthestInsertion btree' kdtree 


-- Cria a Btree com os pontos e distancias iniciais
mkTreeDist ptRef [] btree = btree
mkTreeDist ptRef (pt:pts) btree = 
	mkTreeDist ptRef pts (insertTree btree (PointDist (pt, dist ptRef pt)))


-- Gera, a partir da kdtree, uma lista contendo a sequencia de pontos do tour
buildTour kdtree fstPT [] = 
	buildTour kdtree fstPT [snd $ searchPrevNext kdtree fstPT, fstPT]

buildTour kdtree fstPT tour =
	let	next = snd $ searchPrevNext kdtree (head tour)
	in	if next == fstPT
		then tour
		else buildTour kdtree fstPT (next:tour)


-- Seleciona uma elemento qualquer da kdtree como ponto inicial do tour
-- Neste caso, o ponto escolhido eh o mais a esquerda na arvore
anything (KLeaf pt _ _ _) = pt
anything kdtree = anything (left kdtree)


