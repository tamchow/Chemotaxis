#Scala/JavaFX Chemotaxis simulator

##Features

Simulates _E. coli_'s movement in a food concentration gradient.

Uses the run-and-tumble model with a hunger concept.

Bacteria can die from excess hunger. Amount of food is indicated by size (and possibly color),
and poison content in a food source by the color gradient (the darker in color the food circle, the more toxic it is).
 
Bacteria can also hybridize and reproduce by binary fission.
 
This can be used to visualize a simplified version of chemotactic optimization.

##Controls

_Implemented_:

1. <kbd> Return</kbd>/<kbd> Enter</kbd> or <kbd>Space</kbd> to play / pause simulation.
2. <kbd> =</kbd> (<kbd>+</kbd>) to speed up simulation
3. <kbd> -</kbd> to slow down simulation`
4. <kbd> Esc</kbd> to exit
5. <kbd> F11</kbd> to toggle into / out of fullscreen
6. <kbd> s</kbd> to save snapshot
7. <kbd> F5</kbd> to start new simulation
8. <kbd>Backspace</kbd> to rewind
9. Right-click on empty plate region to spawn new bacterium there
10. Left-click on empty plate region to spawn new food source there
11. Right-click on bacterium to kill it
12. Left-click on food source to remove it
13. General history navigation via the `"Go To"` controls.

_Unimplemented:_
14. <kbd>F2</kbd> to save session state
15. <kbd>F3</kbd> to restore session state

##Requirements

1. Scala 2.12.x (2.11+ should work)
2. Java 8 Update 121 + JavaFX (Oracle JDK on Linux) (Java 8 Update 40+ should work)
3. A powerful PC ;)

##Enhancements

1. Implement support for sessions (save and restore simulations) (Priority 1)
2. Implement configuration via files (priority 2-3)
2. Implement configuration via GUI (priority 10?)
