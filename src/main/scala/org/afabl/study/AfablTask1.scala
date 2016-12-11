package org.afabl.study

import org.afabl._
import org.afabl.util._


object AfablTask1 {

  // Use this val in your agent definitions.
  val bunnyWorld = new BunnyWorld

  // Please place all of your AFABL code for Task 1 in this singleton
  // object.


  // Your solution must assign your AFABL bunny agent for Task 1 to
  // the val afablBuny1.
  val afablBunny1 = ???

  def main(args: Array[String]) = {
    print("Training...")
    trainSteps(afablBunny1, bunnyWorld, 10000000)
    println("done.")
    val result = evaluate(afablBunny1, bunnyWorld)
    val score = result.totalScore / result.timeSteps
    println(f"Your AfablBunny1 scored $score%f (>0.4 is decent, >0.5 is good).")
  }
}
