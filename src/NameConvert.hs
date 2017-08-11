-- | A type model for Haskell datatypes that bears a reasonable correspondence
--   to the XSD type model.
module NameConvert
  ( module NameConvert
  ) where

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Schema.NameConversion hiding (simpleNameConverter, escape)
import Data.Char
import Data.List

-- | A simple default set of rules for resolving XNames into HNames.
simpleNameConverter :: NameConverter
simpleNameConverter = NameConverter
    { modid    = \(XName qn)-> HName . mkConid . hierarchy $ qn
    , conid    = \(XName qn)-> HName . mkConid . hierarchy $ qn
    , varid    = \(XName qn)-> HName . mkVarid . last avoidKeywords
                                               . hierarchy $ qn
    , unqconid = \(XName qn)-> HName . mkConid . local $ qn
    , unqvarid = \(XName qn)-> HName . mkVarid . last avoidKeywords
                                               . local $ qn
    , fwdconid = \(XName qn)-> HName . ("Fwd"++) . mkConid . local $ qn
    , fieldid  = \(XName qnt) (XName qnf)->
                               HName $ (mkVarid . last id . hierarchy $ qnt)
                                       ++ "_" ++
                                       (mkVarid . last id . hierarchy $ qnf)
    }
  where
    hierarchy (N n)     = wordsBy (==':') n
    hierarchy (QN ns n) = [nsPrefix ns, n]

    local               = (:[]) . Prelude.last . hierarchy

    mkConid  []         = "Empty"
    mkConid  [c]        | map toLower c == "string"     = "Xsd.XsdString"
                        | otherwise = first toUpper $ map escape c
    mkConid [m,c]       | map toLower c == "string"     = "Xsd.XsdString"
                        | map toLower c == "date"       = "Xsd.Date"
                        | map toLower c == "double"     = "Xsd.Double"
                        | map toLower c == "integer"    = "Xsd.Integer"
                        | map toLower c == "boolean"    = "Xsd.Boolean"
                        | map toLower c == "decimal"    = "Xsd.Decimal"
                        | otherwise = first toUpper m++"."++first toUpper (map escape c)
    mkConid more        = mkConid [concat more]
    mkVarid  [v]        = first toLower (map escape v)
    mkVarid [m,v]       = first toUpper m++"."++first toLower (map escape v)

    first f (x:xs)
      | not (isAlpha x) = f 'v': x: xs
      | otherwise       = f x: xs
    last  f [x]         = [ f x ]
    last  f (x:xs)      = x: last f xs

-- | Character escapes to create a valid Haskell identifier.
escape :: Char -> Char
escape ' ' = '_'
escape '_' = '_'
escape '-' = '_'
escape x | isAlphaNum x = x
         | otherwise    = '\''
