package checkers.experiments

import checkers.core.RulesSettings

case class GameConfig[DS, LS](rulesSettings: RulesSettings, darkPlayer: Player[DS], lightPlayer: Player[LS])


object GameConfig {
  val test1 = GameConfig(RulesSettings.default, Human, Human)
}