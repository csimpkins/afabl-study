package org.afabl.study

import org.afabl._

trait Task2Scorer extends Scorer[BunnyState, BunnyAction.Value] {

  override def score(state: BunnyState) =
    if (state.bunny == state.wolf) 0.0
    else if (state.bunny == state.food) 1.0
    else if (state.bunny == state.mate) 1.0
    else 0.5
}
