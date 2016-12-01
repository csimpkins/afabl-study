package org.afabl.study

import org.afabl._

trait Task1Scorer extends Scorer[BunnyState1, BunnyAction.Value] {

  override def score(state: BunnyState1) =
    if (state.bunny == state.wolf) 0.0
    else if (state.bunny == state.food) 1.0
    else 0.5
}
