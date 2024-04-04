module Model.LadderTeams.Adaptor where

import Model.LadderTeams.Types (LadderTeams (ladderTeams))
import Model.Player.Types (Player)

toPlayers :: LadderTeams -> [Player]
toPlayers = ladderTeams
