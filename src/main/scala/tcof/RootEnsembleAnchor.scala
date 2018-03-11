package tcof

import rescuecore2.log.Logger

class RootEnsembleAnchor[EnsembleType <: RootEnsemble] private[tcof](val builder: () => EnsembleType) {
  def resolve() = {
    init()

    while (solve()) {
    }
  }

  private var _solution: EnsembleType = _

  def instance: EnsembleType = _solution

  def init(): Unit = {
    _solution = builder()

    // This is not needed per se because ensembles are discarded in each step anyway. However, component are not. We keep it here for uniformity with components.
    val config = new Config(new SolverModel())
    for (stage <- InitStages.values) {
      _solution._init(stage, config)
    }
  }

  def solve(): Boolean = _solution._solverModel.solveAndRecord()
}

