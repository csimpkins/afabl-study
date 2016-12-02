package org.afabl.study

import org.afabl._
import org.afabl.util._


object AfablTask2 {

  // Use this val in your agent definitions.
  val bunnyWorld = new BunnyWorld

  // Please place all of your AFABL code for Task 2 in this singleton
  // object. You may reuse any code you wish from Task 1 by copying it
  // and pasting it here, modifying as necessary.


  // Your solution must assign your AFABL bunny agent for Task 2 to
  // the val afablBuny2.
  val afablBunny2 = ???

  def main(args: Array[String]) = {
    print("Training...")
    trainSteps(afablBunny2, bunnyWorld, 10000000)
    println("done.")
    val result = evaluate(afablBunny2, bunnyWorld)
    val score = result.totalScore / result.timeSteps
    println(f"Your AFABL Bunny2 scored $score%f (greater than 0.5 is good).")
  }
}
