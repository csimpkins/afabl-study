package org.afabl.study

import org.afabl._
import org.afabl.util._


class ScalaBunny1 extends Agent[BunnyState1, BunnyAction.Value]
    with Task1Scorer {

  // Your code goes in the body of this method. This method defines
  // your agent's behavior, that is, what action it takes in a given
  // state. The last expression in this method must be a
  // BunnyAction.  You may create as many helper functions as you
  // like, but please do not alter any of the provided code.
  def getAction(state: BunnyState1, shouldExplore: Boolean = false) = {

    // This is a placeholder to make the code compile. Please
    // replace this with your code.
    BunnyAction.Up
  }
}


object ScalaTask1 {

  def main(args: Array[String]) = {


  }
}
