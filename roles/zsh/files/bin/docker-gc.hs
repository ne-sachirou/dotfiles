#!/usr/bin/env runghc

import Data.Foldable ( for_ )
import Data.List ( intercalate, isSuffixOf )
import Data.Text ( pack, splitOn, unpack )
import System.IO ( hGetLine, hIsEOF )
import System.Process ( createProcess, shell, StdStream ( CreatePipe ), std_out )

main :: IO ()
main = do
  _ <- sh "docker system prune -f --volumes"
  _ <- updateImages
  _ <- sh "docker image prune -af --filter until=2160h"
  _ <- sh "docker run --rm --privileged alpine:latest hwclock -s"
  return ()

-- docker image ls | tail -n +2 | awk '{print $1 ":" $2}' | xargs -t -I{} docker pull {}
updateImages :: IO ()
updateImages = do
  _ : lines <- sh "docker image ls"
  let images =
        filter (not . isSuffixOf ":<none>") $ map (intercalate ":" . take 2 . filter (/= "") . map unpack . splitOn (pack " ") . pack) lines
  for_ images $ \image -> sh $ "docker pull " ++ image

sh :: String -> IO [String]
sh command = do
  putStrLn $ "+ " ++ command
  (_, Just hout, _, _) <- createProcess (shell command) { std_out = CreatePipe }
  streamOut hout []
  where
    streamOut hout out = do
      isEof <- hIsEOF hout
      if isEof
        then return $ reverse out
        else do
          line <- hGetLine hout
          putStrLn line
          streamOut hout (line:out)

-- vim:set ft=haskell:
