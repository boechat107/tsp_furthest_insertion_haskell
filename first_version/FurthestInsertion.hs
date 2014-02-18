{-# OPTIONS_GHC -fglasgow-exts #-}
module FurthestInsertion where
import ReadPoint
import Data.List


-- Funcao que atualiza a lista de pontos e distancias e retorna o ponto mais distante ao tour
furthestPoint _ (fPt, fDist) [] _ newPts = ((fPt, fDist),newPts)
furthestPoint lPos (fPt, fDist) ((ptNext, ptDist):lastPts) tour newPts
	| newDist > fDist = furthestPoint lPos (ptNext, newDist) lastPts tour ((ptNext,newDist):newPts)
	| otherwise = furthestPoint lPos (fPt, fDist) lastPts tour ((ptNext,newDist):newPts)
		where
		newDist = distanceFromTour (ptNext, ptDist) lPos tour


-- Funcao para o calculo da distancia de um ponto livre ao tour
distanceFromTour (pt, oldDist) lPos tour   
		| newDist <- dist pt (tour!!lPos)
		, newDist < oldDist = newDist
		| otherwise = oldDist 


-- Loop principal do Furthest Insertion
furthestInsertion _ [] tour = tour
furthestInsertion lPos pts tour@(x:xs) = 
  let ((newPt,d),newPts) = furthestPoint lPos (x,0) pts tour []
      firstTest = (dist x newPt) + (dist (last tour) newPt) - (dist x (last tour))
      pos = bestPosition 0 1 newPt tour firstTest
  in  furthestInsertion 
	pos (deleteBy (\x y -> (fst x) == (fst y)) (newPt,d) newPts) (insertToTour pos newPt tour) 


-- "pos" e "markPosition" devem comecar em 1
-- "pos" indica a posicao que deve ser inserido o novo vertice, enquanto o antigo
-- ocupante da posicao eh jogado para uma posicao a frente
bestPosition pos _ _ (_:[]) _ = pos
bestPosition pos markPos pt tour@(first:second:rest) oldDist
	| newDist < oldDist  = bestPosition (markPos) (markPos+1) pt (second:rest) newDist
	| otherwise = bestPosition pos (markPos+1) pt (second:rest) oldDist
		where
			newDist = (dist first pt) + (dist second pt) - (dist first second)


-- Insercao propriamente dita
insertToTour pos newPoint tour = 
		(take pos tour) ++ (newPoint:(drop pos tour))


-- Criacao do tour
startFurthestInsertion (pt:pts) =
	return $ furthestInsertion 0 (mkListDist pt pts []) [pt] 
		where
		mkListDist _ [] list = list
		mkListDist ptRef (pt:pts) list =
			mkListDist ptRef pts ((pt, dist ptRef pt):list)


-- Versao com calculo da distancia
{-
startFurthestInsertion (pt:pts) =
	let tour = furthestInsertion 0 (mkListDist pt pts []) [pt]
	    d = calcDist 0 tour
	in  return d
		where
		mkListDist _ [] list = list
		mkListDist ptRef (pt:pts) list =
			mkListDist ptRef pts ((pt, dist ptRef pt):list)

		calcDist d (_:[]) = d
		calcDist d (p1:p2:pts)
			| d == 0.0000 = calcDist (dist p1 (last pts)) (p1:p2:pts)
			| otherwise = calcDist (d + (dist p1 p2)) (p2:pts)

-}


