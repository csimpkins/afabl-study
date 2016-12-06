# AFABL Programmer Study

Thank you for participating in this study! Our goal is to quanitify the advantage or disadvantage of a shallowly embedded Scala DSL for agent programming. You will be helping to shape the future of AI language design.

# Your Tasks

You will write four simple agents: two agents in Scala and two in AFABL (A Friendly Adaptive Behavior Language). Next we discuss the programming tasks then we discuss the mechanics of the study.

## Task 1: Bunny-Food-Wolf

```
+---+---+---+---+---+
|   |   |   | W |   |
+---+---+---+---+---+
| F |   |   |   |   |
+---+---+---+---+---+
|   |   |   |   |   |
+---+---+---+---+---+
|   | B |   |   |   |
+---+---+---+---+---+
|   |   |   |   |   |
+---+---+---+---+---+
```

In the grid world above, the bunny (B) must pursue two goals simultaneously: find food (F) and avoid the wolf (W).  The bunny may move up, down, left, or right.  When it finds food it consumes the food and new food appears elsewhere in the grid world, when it meets the wolf it is eaten and "respawns" elsewhere.

The bunny world works as follows:

- The bunny world is a discrete grid of cells.  The bunny, wolf, and food each occupy one cell.
- During each time step the bunny may move north, south, east, or west -- this is the bunny agent's action set.
- Every two time steps the wolf moves towards the bunny.
- If the bunny moves to the cell currently occupied by the food, the agent should be written to recognize this fact and give the agent an appropriate reward signal. In any case the simulation assumes food is "eaten" and new food appears elsewhere.
- If the wolf moves to the cell currently occupied by the bunny it eats the bunny and the bunny "respawns" in a new location.

For Task 1 write an agent that controls the bunny. The bunny should meet the wolf as little as possible and eat as much food as possible.

## Task 2: Mating Bunny

```
+---+---+---+---+---+
|   |   |   | W |   |
+---+---+---+---+---+
| F |   |   |   |   |
+---+---+---+---+---+
|   |   |   | M |   |
+---+---+---+---+---+
|   | B |   |   |   |
+---+---+---+---+---+
|   |   |   |   |   |
+---+---+---+---+---+
```

For Task 2 write a bunny agent for a world that is identical to the world in Task 1 except that the bunny must also find mates.  This world includes one static potential mate (M) that behaves similarly to the food.  When the bunny finds the potential mate, the simulation assumes that the bunny has "mated," the mate disappears, and another potential mate appears elsewhere.  The simulation runs as in Task 1, and the scorer additionally keeps track of how many mates the bunny finds.  As in Task 1, programmers were asked to write bunny agents that meet the wolf as little as possible, eat as much food as possible, and find as many mates as possible.

# Study Mechanics

You will need Intellij IDEA with the Scala and Bunny plugins. Please follow the instructions found [here](https://github.com/kjcartledge/Bunny/blob/master/GettingStarted.md) to ensure your enviroment is set up correctly for this study.

You will place your code in four files:

- `src/main/scala/org/afabl/study/AfablTask1.scala`
- `src/main/scala/org/afabl/study/AfablTask2.scala`
- `src/main/scala/org/afabl/study/scalaTask1.scala`
- `src/main/scala/org/afabl/study/scalaTask2.scala`

### You should complete these tasks in a specific order: `scalaTask1`, then `scalaTask2`, then `AfablTask1` and finally `AfablTask2`.

A build configuration has been provided for you named "Run." You simply have to click the green run arrow to test your work, which will bring up a menu like this:

```sh
Multiple main classes detected, select one to run:

 [1] org.afabl.study.AfablTask1
 [2] org.afabl.study.AfablTask2
 [3] org.afabl.study.ScalaTask1
 [4] org.afabl.study.ScalaTask2

Enter number:
```

Enter the number corresponding to the task you'd like to run, and your agent will get a score. When you're happy with that score you can move on to the next agent. Don't spend too much time trying to get a good score. This is not a competition. We're only trying to get feedback on programmers' experiences writing agent programs using a traditional programming language and using AFABL.

When you have completed all four agents, in IntelliJ IDEA select *Tools* followed by *Bunny* then *Submit* to complete the study and submit your results.

If this fails, you can use the *Export Results* option instead of *Submit* to generate a text file that can be submited at a later time.
