module ConvertEntities ( toPlayers
                       , toPlayerInfo
                       , toDbPlayer
                       , toDbPlayerFromTuple
                       , fromDbToPlayer
                       , fromDbToPlayerInfo
                       , fromEntityToDbPlayer
                       ) where

import Entities ( LadderTeams(..)
                , Player(..)
                , PlayerInfo(..)
                )

import DbEntities ( DbPlayer(..)
                  , Entity(..)
                  , EntityField(..)
                  )

import Database.Persist.Sqlite (entityValues)


toPlayers :: LadderTeams -> [Player]
toPlayers = ladderTeams

toPlayerInfo :: Player -> [PlayerInfo]
toPlayerInfo = teamMembers

toDbPlayerFromTuple :: (Player, PlayerInfo) -> DbPlayer
toDbPlayerFromTuple (a, b) = toDbPlayer a b

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

fromEntityToDbPlayer :: Entity DbPlayer -> DbPlayer
fromEntityToDbPlayer = entityVal
