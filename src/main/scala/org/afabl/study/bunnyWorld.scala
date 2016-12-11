package org.afabl.study

import util.Random
import org.afabl._
import com.typesafe.scalalogging.LazyLogging
import scala.collection._

case class Location(x: Int, y: Int)

case class BunnyState(
  bunny: Location,
  wolf: Location,
  food: Location,
  mate: Location
)

object BunnyAction extends Enumeration {
  val Up = Value("^")
  val Down = Value("v")
  val Left = Value("<")
  val Right = Value(">")
}

class BunnyWorld(val width: Int = 5, val height: Int = 5)
  extends World[BunnyState, BunnyAction.Value] with LazyLogging {

  var everyOtherStep = false
  val random = new Random()
  var state = init()

  val states = {
    val bunnyLocs =
      for (x <- Range(0, width); y <- Range(0, height)) yield Location(x, y)
    val wolfLocs =
      for (x <- Range(0, width); y <- Range(0, height)) yield Location(x, y)
    val foodLocs =
      for (x <- Range(0, width); y <- Range(0, height)) yield Location(x, y)
    val mateLocs =
      for (x <- Range(0, width); y <- Range(0, height)) yield Location(x, y)
    for (b <- bunnyLocs; w <- wolfLocs; f <- foodLocs; m <- mateLocs) yield
      BunnyState(b, w, f, m)
  }

  val actions = BunnyAction.values.toSeq

  def init(): BunnyState = {
    val bunny = updateStart()
    val wolf = updateStart(List(bunny))
    val food = updateStart(List(bunny, wolf))
    val mate = updateStart(List(bunny, wolf, food))
    state = new BunnyState(bunny, wolf, food, mate)
    state
  }

  def resetAgent(): BunnyState = {
    val bunny = updateStart(List(state.wolf, state.food, state.mate))
    state = BunnyState(bunny, state.wolf, state.food, state.mate)
    state
  }

  def updateStart(deconflict: List[Location] = Nil) = {
    if (deconflict.isEmpty) {
      Location(Random.nextInt(width), Random.nextInt(height))
    } else {
      var candidateStart = Location(Random.nextInt(width), Random.nextInt(height))
      while (deconflict.contains(candidateStart)) {
        candidateStart = Location(Random.nextInt(width), Random.nextInt(height))
      }
      candidateStart
    }
  }

  def act(intendedAction: BunnyAction.Value): BunnyState = {
    logger.trace(s"Intended action: $intendedAction")
    val nextBunny = moveBunny(state, intendedAction)
    val reportedNextState =
      BunnyState(nextBunny, state.wolf, state.food, state.mate)
    val nextFood = moveFood(state)
    val nextWolf = moveWolf(state)
    val nextMate = moveMate(state)
    state = BunnyState(nextBunny, nextWolf, nextFood, nextMate)
    logMeetings(reportedNextState)
    reportedNextState
  }

  def moveBunny(state: BunnyState,
    intendedAction: BunnyAction.Value): Location = {
    val action = if (random.nextDouble < .1) {
      BunnyAction(random.nextInt(BunnyAction.values.size))
    } else {
      intendedAction
    }
    logger.trace(s"Actual action: $action")
    val bunny = action match {
      case BunnyAction.Up if (state.bunny.y < height - 1) =>
        Location(state.bunny.x, state.bunny.y + 1)
      case BunnyAction.Down if (state.bunny.y > 0) =>
        Location(state.bunny.x, state.bunny.y - 1)
      case BunnyAction.Left if (state.bunny.x > 0) =>
        Location(state.bunny.x - 1, state.bunny.y)
      case BunnyAction.Right if (state.bunny.x < width - 1) =>
        Location(state.bunny.x + 1, state.bunny.y)
      case _ => state.bunny
    }
    bunny
  }

  def moveWolf(state: BunnyState) = {
    val bunny = state.bunny
    val currentWolf = state.wolf
    if (everyOtherStep) {
      val xdiff = (bunny.x - currentWolf.x)
      val ydiff = (bunny.y - currentWolf.y)
      var wolfX = currentWolf.x
      var wolfY = currentWolf.y
      if (xdiff > 0) {
        wolfX = currentWolf.x + 1
      } else if (xdiff < 0) {
        wolfX = currentWolf.x - 1
      } else if (ydiff > 0) {
        wolfY = currentWolf.y + 1
      } else if (ydiff < 0) {
        wolfY = currentWolf.y - 1
      }
      everyOtherStep = !everyOtherStep
      Location(wolfX, wolfY)
    } else {
      everyOtherStep = !everyOtherStep
      currentWolf
    }
  }

  def moveFood(state: BunnyState) = {
    if (state.bunny == state.food) {
      updateStart(List(state.bunny, state.wolf, state.mate))
    } else {
      state.food
    }
  }

  def moveMate(state: BunnyState) = {
    if (state.bunny == state.mate) {
      updateStart(List(state.bunny, state.wolf, state.food))
    } else {
      state.mate
    }
  }

  def logMeetings(state: BunnyState) = {
    if (state.bunny == state.food)
      logger.trace(s"Bunny met food.")
    else if (state.bunny == state.wolf)
      logger.trace("Wolf met bunny.")
    else if (state.bunny == state.mate)
      logger.trace("Bunny met mate.")
  }
}
