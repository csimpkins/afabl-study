package org.afabl.study

import org.afabl._
import org.afabl.util._


object AfablTask2 {

  // Use this val in your agent definitions.
  val bunnyWorld = new BunnyWorld

  // Please place all of your AFABL code for Task 2 in this singleton
  // object. You may reuse any code you wish from Task 1 by copying it
  // and pasting it here, modifying as necessary. You may also refer to
  // any members of AfablTask1 you created, for example, if you created
  // vals for modules that you wish to reuse directly instead of copying
  // their code.


  // Your solution must assign your AFABL bunny agent for Task 2 to
  // the val afablBuny2.
  val afablBunny2 = ???

  def main(args: Array[String]) = {
    print("Training...")
    trainSteps(afablBunny2, bunnyWorld, 10000000)
    println("done.")
    val result = evaluate(afablBunny2, bunnyWorld)
    val score = result.totalScore / result.timeSteps
    println(f"Your AfablBunny1 scored $score%f (>0.4 is decent, >0.5 is good).")
  }
}
