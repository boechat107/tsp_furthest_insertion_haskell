# Travelling Salesman Problem <br />(Haskell version)

This work was the result of the
[Programming Languages 2](http://www.inf.ufes.br/~raulh/) course's project.
The studied problem was the 
[*Travelling Salesman Problem*](http://en.wikipedia.org/wiki/Travelling_salesman_problem)
(TSP) and the application of binary tree variations as indexation and heap data
structures.
In summary, given a set of Euclidean 2D points, the problem consist of finding the
shortest possible tour, which should pass over each point just once and come back to
the initial tour.

![Euclidean points](http://users.cs.cf.ac.uk/C.L.Mumford/howard/FI1.gif)
![TSP tour](http://users.cs.cf.ac.uk/C.L.Mumford/howard/FI8.gif)

Two different algorithms were used to construct the tour (the path to visit all
vertices or cities):

* [*Farthest Insertion*](http://users.cs.cf.ac.uk/C.L.Mumford/howard/FarthestInsertion.html)
(FI)
* [*Double Minimum Spanning Tree*](http://en.wikipedia.org/wiki/Minimum_spanning_tree)
(DMST)

More theoretical discussion and analysis can be read 
[here](http://boechat107.github.io/research%20problems/2014/02/25/travelling-salesman-problem-tsp/).

## Dependencies

* `libghc-glut-dev` (tested with version 2.4.0.0)

## How to run

Go to the folder of an implementation and type

    make
    ./OpenglFI d.50

which runs the chose algorithm for a file with the coordinates of 50 cities and
shows an OpenGL window representation of the final tour.

![Tour for d.50](http://www.zimagez.com/miniature/screenshot-03142014-122334pm.php)
