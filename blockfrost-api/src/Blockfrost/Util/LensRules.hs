-- | Rules for lens generation

{-# OPTIONS_HADDOCK hide #-}

module Blockfrost.Util.LensRules
  where

import Control.Lens
import Language.Haskell.TH (mkName, nameBase)

-- Bit of an overkill, since we only alter
-- one field name which would end up called `type`
blockfrostFieldRules :: LensRules
blockfrostFieldRules = defaultFieldRules
  & lensField %~ modNamer
  where
    modNamer namer dname fnames fname =
      map fixDefName (namer (fixTypeName dname) fnames fname)

    fixDefName (MethodName cname mname)=MethodName cname (fixName mname)
    fixDefName (TopName name)           = TopName (fixName name)

    fixTypeName = mkName . fixTypeName' . nameBase
    -- we coerce IPFS (and similar) to Ipfs so
    -- camelCase namer is happy
    -- and we don't have to rename our type or fields
    fixTypeName' "IPFSAdd"       = "IpfsAdd"
    fixTypeName' "IPFSPinChange" = "IpfsPinChange"
    fixTypeName' "IPFSPin"       = "IpfsPin"
    fixTypeName' "URLVersion"    = "UrlVersion"
    fixTypeName' x               = x

    fixName = mkName . fixName' . nameBase
    fixName' "type" = "type_"
    fixName' n      = n
