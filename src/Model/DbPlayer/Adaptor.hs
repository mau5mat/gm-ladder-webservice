module Model.DbPlayer.Adaptor where

import Database.Persist.Sqlite (Entity, entityVal, entityValues)
import Model.DbPlayer.Types (DbPlayer (..))
import Model.Player.Types (Player (..))
import Model.PlayerInfo.Types (PlayerInfo (..))

fromDbToPlayer :: DbPlayer -> PlayerInfo -> Player
fromDbToPlayer dbPlayer playerInfo =
  Player
    { teamMembers = [playerInfo]
    , previousRank = dbPlayerPreviousRank dbPlayer
    , points = dbPlayerPoints dbPlayer
    , wins = dbPlayerWins dbPlayer
    , losses = dbPlayerLosses dbPlayer
    , mmr = dbPlayerMmr dbPlayer
    , joinTimestamp = dbPlayerJoinTimestamp dbPlayer
    }

fromDbToPlayerInfo :: DbPlayer -> PlayerInfo
fromDbToPlayerInfo dbPlayer =
  PlayerInfo
    { realm = dbPlayerRealm dbPlayer
    , region = dbPlayerRegion dbPlayer
    , displayName = dbPlayerDisplayName dbPlayer
    , clanTag = dbPlayerClanTag dbPlayer
    , favoriteRace = dbPlayerFavoriteRace dbPlayer
    }

fromEntityToDbPlayer :: Entity DbPlayer -> DbPlayer
fromEntityToDbPlayer = entityVal
