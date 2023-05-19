module ConvertEntities ( toDbPlayer
                       , fromDbToPlayer
                       , fromDbToPlayerInfo
                       , extractDbPlayerFromTuple
                       ) where

import Entities ( Player(..)
                , PlayerInfo(..)
                )

import ApiEntities (ApiPlayer(..))

import DbEntities (DbPlayer(..))


-- Conversion from response model -> db model and vice versa

extractDbPlayerFromTuple :: (Player, PlayerInfo) -> DbPlayer
extractDbPlayerFromTuple (a, b) = toDbPlayer a b

fromDbToApiPlayer :: DbPlayer -> ApiPlayer
fromDbToApiPlayer dbPlayer
  = ApiPlayer
  { apiPreviousRank = dbPlayerPreviousRank dbPlayer
  , apiPoints = dbPlayerPoints dbPlayer
  , apiWins = dbPlayerWins dbPlayer
  , apiLosses = dbPlayerLosses dbPlayer
  , apiMmr = dbPlayerMmr dbPlayer
  , apiJoinTimestamp = dbPlayerJoinTimestamp dbPlayer
  , apiRealm = dbPlayerRealm dbPlayer
  , apiRegion = dbPlayerRegion dbPlayer
  , apiDisplayName = dbPlayerDisplayName dbPlayer
  , apiClanTag = dbPlayerClanTag dbPlayer
  , apiFavoriteRace = dbPlayerFavoriteRace dbPlayer
  }


toDbPlayer :: Player -> PlayerInfo -> DbPlayer
toDbPlayer player playerInfo
  = DbPlayer
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

fromDbToPlayer :: DbPlayer -> PlayerInfo -> Player
fromDbToPlayer dbPlayer playerInfo
  = Player
  { teamMembers = [playerInfo]
  , previousRank = dbPlayerPreviousRank dbPlayer
  , points = dbPlayerPoints dbPlayer
  , wins = dbPlayerWins dbPlayer
  , losses = dbPlayerLosses dbPlayer
  , mmr = dbPlayerMmr dbPlayer
  , joinTimestamp = dbPlayerJoinTimestamp dbPlayer
  }

fromDbToPlayerInfo :: DbPlayer -> PlayerInfo
fromDbToPlayerInfo dbPlayer
  = PlayerInfo
  { realm = dbPlayerRealm dbPlayer
  , region = dbPlayerRegion dbPlayer
  , displayName = dbPlayerDisplayName dbPlayer
  , clanTag = dbPlayerClanTag dbPlayer
  , favoriteRace = dbPlayerFavoriteRace dbPlayer
  }
