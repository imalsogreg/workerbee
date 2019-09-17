{-# LANGUAGE OverloadedStrings #-}

module RenderGraph where

import qualified GHC.IO.Handle         as IO
import qualified Algebra.Graph         as G
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString       as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Process        as Process
import qualified System.IO.Temp        as Temp

import           API
import           TaskTree

-- | Call out to `dot` to generate an image for
-- the graph of tasks and assignments
imageForState :: State -> IO HTMLPage
imageForState state = do
  let assignmentGraph = assignmentTree state

      formatNode (Task task,worker) = case worker of
        Nothing         -> "\"" <> task <> "\""
        Just (Worker w) -> "\"" <> task <> "\\n" <> w <> "\""

      formatPair (node1, node2) =
        "  " <> formatNode node1 <> " -> " <> formatNode node2
  
      dotCode = Text.encodeUtf8 $ Text.unlines $
        [ "digraph {" ]
        ++ fmap formatPair (G.edgeList assignmentGraph)
        ++ ["}"]

  dotOutput <- readProcess "dot" ["-Tjpeg"] dotCode >>= readProcess "base64" []
  
  return . HTMLPage $ Text.decodeUtf8 $ imagePage dotOutput

readProcess :: FilePath -> [String] -> BS.ByteString -> IO BS.ByteString
readProcess cmd opts stdin = do
  let p = (Process.shell (cmd ++ " " ++ unwords opts))
          { Process.std_in = Process.CreatePipe
          , Process.std_out = Process.CreatePipe
          }
  (Just pIn, Just pOut, _, _ ) <- Process.createProcess p
  IO.hSetBuffering pIn  IO.NoBuffering
  IO.hSetBuffering pOut IO.NoBuffering
  BS.hPutStr pIn stdin
  IO.hClose pIn
  BS.hGetContents pOut

imagePage :: BS.ByteString -> BS.ByteString
imagePage imageBytesBase64 = BSC.unlines
  [ "<html>"
  , "  <head></head>"
  , "  <body>"
  , "    <img src=\"data:image/jpeg;base64," <> imageBytesBase64 <> "\"/>"
  , "  </body>"
  , "</html>"
  ]

