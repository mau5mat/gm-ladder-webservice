module Model.Player.Adaptor where

import Model.DbPlayer.Types (DbPlayer (..))
import Model.Player.Types (Player (..))
import Model.PlayerInfo.Types (PlayerInfo (..))

toPlayerInfo :: Player -> [PlayerInfo]
toPlayerInfo = teamMembers

toDbPlayerFromTuple :: (Player, PlayerInfo) -> DbPlayer
toDbPlayerFromTuple (a, b) = toDbPlayer a b

toDbPlayer :: Player -> PlayerInfo -> DbPlayer
toDbPlayer player playerInfo =
  DbPlayer
    { dbPlayerPreviousRank = previousRank player
    , dbPlayerPoints = points player
    , dbPlayerWins = wins player
    , dbPlayerLosses = losses player
    , dbPlayerMmr = mmr player
    , dbPlayerJoinTimestamp = joinTimestamp player
    , dbPlayerRealm = realm playerInfo
    , dbPlayerRegion = region playerInfo
    , dbPlayerDisplayName = displayName playerInfo
    , dbPlayerClanTag = clanTag playerInfo
    , dbPlayerFavoriteRace = favoriteRace playerInfo
    }
