{-# OPTIONS_GHC -fglasgow-exts #-}
module Btree where
import System.Environment


data Btree a =
	Leaf
	| Node (Btree a) a (Btree a)
	| Node2 (Btree a) a (Btree a) a (Btree a)
	deriving(Eq,Show,Read)




-- #####################################################################
-- #	Insertion functions
-- #####################################################################

data SplitTag a =
	Split (Btree a)
	| NoSplit (Btree a)
	deriving(Show)


-- Funcao que remove as tags para insercao
--insertTree (NoSplit t) x = insertTreeReal t x
--insertTree (Split t) x = insertTreeReal t x

insertTree t x = case (insertTreeReal t x) of
		Split r -> r
		NoSplit r -> r
	  


insertTreeReal Leaf x = NoSplit (Node Leaf x Leaf)
insertTreeReal n@(Node Leaf y Leaf) x
	| x < y = NoSplit (Node2 Leaf x Leaf y Leaf)
	| x == y = NoSplit n
	| x > y = NoSplit (Node2 Leaf y Leaf x Leaf)


insertTreeReal n@(Node2 Leaf y Leaf z Leaf) x
	| x < y = Split (Node (Node Leaf x Leaf) y (Node Leaf z Leaf))
	| x == y = NoSplit n
	| x < z = Split (Node (Node Leaf y Leaf) x (Node Leaf z Leaf))
	| x == z = NoSplit n
	| x > z = Split (Node (Node Leaf y Leaf) z (Node Leaf x Leaf))


insertTreeReal n@(Node t0 y t1) x
	| x < y = case (insertTreeReal t0 x) of
		Split (Node t00 r t01) -> NoSplit (Node2 t00 r t01 y t1)
		NoSplit taux -> NoSplit (Node taux y t1)
	| x == y = NoSplit n
	| x > y = case (insertTreeReal t1 x) of
		Split (Node t00 r t01) -> NoSplit (Node2 t0 y t00 r t01)
		NoSplit taux -> NoSplit (Node t0 y taux)


insertTreeReal n@(Node2 t0 y t1 z t2) x
	| x < y = case (insertTreeReal t0 x) of
		Split (Node t00 r t01) -> Split (Node (Node t00 r t01) y (Node t1 z t2))
		NoSplit taux ->	NoSplit (Node2 taux y t1 z t2)
	| x == y = NoSplit n
	| x < z = case (insertTreeReal t1 x) of
		Split (Node t00 r t01) -> Split (Node (Node t0 y t00) r (Node t01 z t2))
		NoSplit taux ->	NoSplit (Node2 t0 y taux z t2)
	| x == z = NoSplit n
	| x > z = case (insertTreeReal t2 x) of
		Split (Node t00 r t01) -> Split (Node (Node t0 y t1) z (Node t00 r t01))
		NoSplit taux ->	NoSplit (Node2 t0 y t1 z taux)


-- #####################################################################
-- #	Deletion functions
-- #
-- # A funcao deve ser usada apenas para remover elementos posicionados
-- # nas folhas.
-- #####################################################################

data HoleTag a =
	NoHole (Btree a)
	| Hole (Btree a)

-- Como a funcao delete nao sera chamada duas vezes consecutivas,
-- deve-se usar a funcao deleteNode
deleteMaxNode ti = case (deleteNodeReal ti (maxNode ti)) of
		Hole t -> t
		NoHole t -> t

deleteNodeReal n@(Node Leaf y Leaf) a
	| a == y = Hole Leaf
	| otherwise = NoHole n


deleteNodeReal n@(Node2 Leaf y Leaf z Leaf) a
	| a == y = NoHole (Node Leaf z Leaf)
	| a == z = NoHole (Node Leaf y Leaf)
	| otherwise = NoHole n

-- ### Remocao para nos pais com dois elementos ###
deleteNodeReal (Node2 s0 x s1@(Node t1 z t2) y s2) a
	| a < x = case (deleteNodeReal s0 a) of
		Hole t0 -> NoHole (Node (Node2 t0 x t1 z t2) y s2)
		NoHole taux -> NoHole (Node2 taux x s1 y s2)
	| a > y = case (deleteNodeReal s2 a) of
		Hole t3 -> NoHole (Node s0 x (Node2 t1 z t2 y t3)) 
		NoHole taux -> NoHole (Node2 s0 x s1 y taux)


deleteNodeReal (Node2 s0 x s1@(Node2 t1 w t2 z t3) y s2) a
	| a < x = case (deleteNodeReal s0 a) of
		Hole t0 -> NoHole (Node2 (Node t0 x t1) w (Node t2 z t3) y s2)
		NoHole taux -> NoHole (Node2 taux x s1 y s2)
	| a > y = case (deleteNodeReal s2 a) of
		Hole t4 -> NoHole (Node2 s0 x (Node t1 w t2) z (Node t3 y t4))
		NoHole taux -> NoHole (Node2 s0 x s1 y taux)


deleteNodeReal (Node2 s0@(Node2 t1 w t2 z t3) x s1 y s2) a
	| a > x && a < y = case (deleteNodeReal s1 a) of
		Hole t4 -> NoHole (Node2 (Node t1 w t2) z (Node t3 x t4) y s2)
		NoHole taux -> NoHole (Node2 s0 x taux y s2)


deleteNodeReal (Node2 s0 x s1 y s2@(Node2 t1 w t2 z t3)) a
	| a > x && a < y = case (deleteNodeReal s1 a) of
		Hole t4 -> NoHole (Node2 s0 x (Node t4 y t1) w (Node t2 z t3))
		NoHole taux -> NoHole (Node2 s0 x taux y s2)


deleteNodeReal (Node2 s0@(Node t1 z t2) x s1 y s2) a
	| a > x && a < y = case (deleteNodeReal s1 a) of
		Hole t3 -> NoHole (Node (Node2 t1 z t2 x t3) y s2)
		NoHole taux -> NoHole (Node2 s0 x taux y s2)


-- ### Remocao para nos pais de apenas um elemento ###
deleteNodeReal (Node s0 x s1@(Node t1 y t2)) a
	| a < x = case (deleteNodeReal s0 a) of
		Hole t0 -> Hole (Node2 t0 x t1 y t2)
		NoHole taux -> NoHole (Node taux x s1)


deleteNodeReal (Node s0@(Node t1 y t2) x s1) a
	| a > x = case (deleteNodeReal s1 a) of
		Hole t3 -> Hole (Node2 t1 y t2 x t3)
		NoHole taux -> NoHole (Node s0 x taux)


deleteNodeReal (Node s0 x s1@(Node2 t1 y t2 z t3)) a
	| a < x = case (deleteNodeReal s0 a) of
		Hole t0 -> NoHole (Node (Node t0 x t1) y (Node t2 z t3))
		NoHole taux -> NoHole (Node taux x s1)


deleteNodeReal (Node s0@(Node2 t1 y t2 z t3) x s1) a
	| a > x = case (deleteNodeReal s1 a) of
		Hole t4 -> NoHole (Node (Node t1 y t2) z (Node t3 x t4))
		NoHole taux -> NoHole (Node s0 x taux)


-- ########################################################################
-- ##			Other Functions                                   
-- ########################################################################

maxNode tree = case tree of
	Node Leaf y Leaf -> y
	Node2 Leaf x Leaf y Leaf -> y
	Node t0 y t1 -> maxNode t1
	Node2 t0 x t1 y t2 -> maxNode t2

-- ####################### testes #########################################

--list = [80,70,60,90,83,81,75]
--test = funcTeste list Leaf
--funcTeste [] tree = tree
--funcTeste (x:xs) tree = funcTeste xs (insertTree tree x)

