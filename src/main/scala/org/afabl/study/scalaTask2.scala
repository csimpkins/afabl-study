package org.afabl.study

import org.afabl._
import org.afabl.util._


class ScalaBunny2 extends Agent[BunnyState, BunnyAction.Value]
    with Task2Scorer {

  // Your code goes in the body of this method. This method defines
  // your agent's behavior, that is, what action it takes in a given
  // state. The last expression in this method must be a
  // BunnyAction.  You may create as many helper functions as you
  // like, but please do not alter any of the provided code.
  def getAction(state: BunnyState) = {

    // This is a placeholder to make the code compile. Please
    // replace this with your code.
    BunnyAction.Up
  }
}


object ScalaTask2 {

  def main(args: Array[String]) = {
    val scalaBunny2 = new ScalaBunny2
    val result = evaluate(scalaBunny2, new BunnyWorld)
    val score = result.totalScore / result.timeSteps
    println(f"Your ScalaBunny2 scored $score%f (>0.4 is decent, >0.5 is good).")
  }
}
