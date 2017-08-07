{-# Language OverloadedStrings #-}
{-| Time-stamp: <2017-08-01 16:46:41 robert>

Module      : FHIRMain
Copyright   : (c) Robert Lee, 2017
License     : All Rights Reserved

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

-}

{-
infixr 9  .
infixr 8  ^, ^^, ⋆⋆
infixl 7  ⋆, /, ‘quot‘, ‘rem‘, ‘div‘, ‘mod‘
infixl 6  +, -
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >
infixl 4  <$, <*>, <*, *>, <**>
infixr 3  &&
infixr 2  ||
infixl 1  ?, >>, >>=
infixr 1  =<<, <=<, >=>
infixr 0  $, $!, ‘seq‘

⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ Omega Symbol Key ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
                   early or abnormal termination ⋅⋅⋅ Ω
                            termination (normal) ⋅⋅⋅ ω
                                    a new thread ⋅⋅⋅ ⋔
          code that can throw an error exception ⋅⋅⋅ ⏈
                                  loop-like code ⋅⋅⋅ ➿
                              a loop-like repeat ⋅⋅⋅ ↺
                           end of loop-like code ⋅⋅⋅ 🔚
               an uninterruptible exception mask ⋅⋅⋅ ☔
                code that can emit IO exceptions ⋅⋅⋅ ☢
                a warning about troublesome code ⋅⋅⋅ ⚠
  an imperative concerning imprudent code change ⋅⋅⋅ ⚡
                  a forbidden/nonsense condition ⋅⋅⋅ ⛞
                          a timed race condition ⋅⋅⋅ 🏁
⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
-}


module FHIRMain
where

-- Local Imports

-- Explicit Imports

-- Qualified Imports

-- Undisciplined Imports

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------


-- XsdToHaskell

-- This program is designed to convert an XML file containing an XSD
-- decl into a Haskell module containing data/newtype definitions which
-- mirror the XSD.  Once you have used this program to generate your type
-- definitions, you should import Xsd2Haskell wherever you intend
-- to read and write XML files with your Haskell programs.

import System.Environment
import System.Exit
import System.IO
import Control.Monad
--import Data.Either

--import Text.XML.HaXml.Wrappers   (fix2Args)
import Text.XML.HaXml            (version)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces (resolveAllNames ,qualify, nullNamespace)
import Text.XML.HaXml.Parse      (xmlParse')
import Text.XML.HaXml.Util       (docContent)
import Text.XML.HaXml.Posn       (posInNewCxt)

import Text.XML.HaXml.Schema.Parse
import Text.XML.HaXml.Schema.Environment
import Text.XML.HaXml.Schema.NameConversion
import Text.XML.HaXml.Schema.TypeConversion
import Text.XML.HaXml.Schema.PrettyHaskell
import qualified Text.XML.HaXml.Schema.HaskellTypeModel as Haskell
import Text.ParserCombinators.Poly
import Text.PrettyPrint.HughesPJ (render,vcat)

-- sucked in from Text.XML.HaXml.Wrappers to avoid dependency on T.X.H.Html
fix2Args :: IO (String,String)
fix2Args = do
  args <- getArgs
  when ("--version" `elem` args) $ do putStrLn $ "part of HaXml-" ++ version
                                      exitWith ExitSuccess
  when ("--help" `elem` args) $ do putStrLn $ "See http://haskell.org/HaXml"
                                   exitWith ExitSuccess
  case length args of
    0 -> pure ("-"      , "-")
    1 -> pure (args !! 0, "-")
    2 -> pure (args !! 0, args !! 1)
    _ -> do prog <- getProgName
            putStrLn ("Usage: " ++ prog ++ " [xmlfile] [outfile]")
            exitFailure

errLn, logLn :: String -> IO ()
errLn = hPutStrLn stderr
logLn = hPutStrLn stdout
        
fhirMain :: IO ()
fhirMain = do
  (inf, outf) <- fix2Args
  inputContent  <- if inf  == "-" then getContents else readFile inf
  outputHandler <- if outf == "-" then pure stdout else openFile outf WriteMode
  let xmlDoc = resolveAllNames qualify                                                 -- xmlDoc :: Document i
             . either (error . ("not XML:\n"++)) id
             . xmlParse' inf
             $ inputContent
      mainElement = docContent (posInNewCxt inf Nothing) xmlDoc                        -- mainElement :: Content i
      parsedPair = runParser schema [mainElement]
  case parsedPair of
    (Left msg,_) ->    errLn msg
    (Right tmSchema,[]) -> do                          -- tmSchema :: Text.XML.HaXml.Schema.XSDTypeModel.Schema
      logLn "-- Parse Success!"
      logLn "\n-- Text.XML.HaXml.Schema.XSDTypeModel.Schema ---------------\n"
      logLn $ show tmSchema
      logLn "\n-- ---------------\n"
      let decls = convert (mkEnvironment inf tmSchema emptyEnv) tmSchema
          haskl = Haskell.mkModule inf tmSchema decls
          doc   = ppModule simpleNameConverter haskl
      hPutStrLn outputHandler $ render doc
    (Right v,_)  -> do logLn "-- Parse incomplete!"
                       logLn "\n-- ---------------\n"
                       logLn $ show v
                       logLn "\n-- ---------------\n"
  hFlush outputHandler
  hClose outputHandler
  exitWith ExitSuccess


--do hPutStrLn o $ "Document contains XSD for target namespace "++
--                 targetNamespace e
  {-
  let (DTD name _ markup) = (getDtd . dtdParse inf) content
      decls = (nub . dtd2TypeDef) markup
      realname = if outf/="-" then mangle (trim outf)
                 else if null (localName name) then mangle (trim inf)
                 else mangle (localName name)
  in
  do hPutStrLn o ("module "++realname
                  ++" where\n\nimport Text.XML.HaXml.XmlContent"
                  ++"\nimport Text.XML.HaXml.OneOfN")
    --            ++"\nimport Char (isSpace)"
    --            ++"\nimport List (isPrefixOf)"
     hPutStrLn o "\n\n{-Type decls-}\n"
     (hPutStrLn o . render . vcat . map ppTypeDef) decls
     hPutStrLn o "\n\n{-Instance decls-}\n"
     mapM_ (hPutStrLn o . (++"\n") . render . mkInstance) decls
     hPutStrLn o "\n\n{-Done-}"
     hFlush o
  -}

{-
getDtd :: Maybe t -> t
getDtd (Just dtd) = dtd
getDtd (Nothing)  = error "No DTD in this document"

trim :: [Char] -> [Char]
trim name | '/' `elem` name  = (trim . tail . dropWhile (/='/')) name
          | '.' `elem` name  = takeWhile (/='.') name
          | otherwise        = name
-}

targetNamespace :: Element i -> String
targetNamespace (Elem qn attrs _) =
    if qn /= xsdSchema then "ERROR! top element not an xsd:schema tag"
    else case lookup (N "targetNamespace") attrs of
           Nothing -> "ERROR! no targetNamespace specified"
           Just atv -> show atv

xsdSchema :: QName
xsdSchema = QN (nullNamespace{nsURI="http://www.w3.org/2001/XMLSchema"}) "schema"

--  <xsd:schema xmlns:xsd="" xmlns:fpml="" targetNamespace="" version=""
--              attributeFormDefault="unqualified"
--              elementFormDefault="qualified">

