{-# LANGUAGE OverloadedStrings #-}

module Docs where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Process as Process
import Servant.API as Servant
import qualified Servant.Docs as Servant
-- import qualified Text.Pandoc as Pandoc

import API
import TaskTree
import qualified RenderGraph as Render

apiDocs :: IO HTMLPage
apiDocs =
  let intro = Servant.DocIntro "WorkerBee Internal API"
              ["Add and remove workers, assign workers to tasks and visualize\
               \the work tree through this API"]

      markdownDocs = Servant.markdown $ Servant.docsWithIntros [intro] api 
  -- in  fmap HTMLPage $ pandocRenderHTML markdownDocs
  in  return $ HTMLPage $ markdownPage markdownDocs

markdownPage :: String -> Text.Text
markdownPage md = Text.unlines
  [ "<html>"
  , "  <head></head>"
  , "  <body>"
  , "    <pre>"
  , Text.pack md
  , "    </pre>"
  , "  </body>"
  , "</html>"
  ]
  

-- pandocRenderHTML :: String -> IO Text.Text
-- pandocRenderHTML md = Pandoc.runIOorExplode $
--   Pandoc.readMarkdown Pandoc.def (Text.pack md) >>=
--   Pandoc.writeHtml5String Pandoc.def
