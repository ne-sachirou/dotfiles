#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ( liftA2 )
import Control.Monad ( forM_ )
import Data.List ( elemIndex, isInfixOf, isPrefixOf, isSuffixOf )
import Data.Maybe ( catMaybes, fromJust, isNothing, listToMaybe )
import qualified Data.Text as T
import System.IO ( hGetLine, hIsEOF )
import System.Process ( createProcess, readCreateProcess, shell, StdStream ( CreatePipe ), std_out )

main :: IO ()
main = do
  deleteLocalMergedBranches
  deleteLocalBranchesInteractively
  deleteRemoteBranchesInteractively
  _ <- sh "git gc --auto --prune"
  return ()

deleteLocalMergedBranches :: IO ()
deleteLocalMergedBranches = do
  branches <- localMergedBranches
  forM_ branches delete

deleteLocalBranchesInteractively :: IO ()
deleteLocalBranchesInteractively = do
  branches <- localForceRemovableBranches
  forM_ branches deleteForce

deleteRemoteBranchesInteractively :: IO ()
deleteRemoteBranchesInteractively = do
  branches <- remoteForceRemovableBranches
  forM_ branches deleteForce

data GitBranch = GitBranch {
  remote :: Maybe String,
  branch :: String
  }

fromString :: String -> GitBranch
fromString expr =
  let Just (remote_, '/' : branch_) = splitAt <$> elemIndex '/' expr <*> Just expr in
    GitBranch { remote = Just remote_, branch = branch_ }

currentUpstreamBranch :: IO (Maybe GitBranch)
currentUpstreamBranch = do
  lines_ <- sh "git rev-parse --abbrev-ref --symbolic-full-name @{u} 2> /dev/null || true"
  return $ do
    line <- listToMaybe lines_
    return $ fromString line

localMergedBranches :: IO [GitBranch]
localMergedBranches = do
  lines_ <- sh "git branch --merged"
  return [GitBranch { remote = Nothing, branch = line } |
          line <- lines_,
          not $ "*" `isPrefixOf` line,
          not $ " main" `isSuffixOf` line,
          not $ " master" `isSuffixOf` line,
          not $ " develop" `isSuffixOf` line]

localForceRemovableBranches :: IO [GitBranch]
localForceRemovableBranches = do
  lines_ <- sh "git branch -vv"
  selectedLines <- peco
    [line |
     line <- lines_,
     not $ "*" `isPrefixOf` line,
     not $ " main " `isInfixOf` line,
     not $ " master " `isInfixOf` line,
     not $ " develop " `isInfixOf` line,
     ": gone] " `isInfixOf` line]
    pecoDefaultOption { prompt = Just "LOCAL>" }
  return [GitBranch { remote = Nothing, branch = branch_ } |
          line <- selectedLines,
          let branch_ = head $ words line]

remoteForceRemovableBranches :: IO [GitBranch]
remoteForceRemovableBranches = do
  lines_ <- sh "git branch -r --merged"
  mCubr <- currentUpstreamBranch
  selectedLines <- peco
    [line |
     line <- lines_,
     not $ "/HEAD " `isInfixOf` line,
     not $ "/main" `isSuffixOf` line,
     not $ "/master" `isSuffixOf` line,
     not $ "/develop" `isSuffixOf` line,
     isNothing mCubr ||
     let cubr = fromJust mCubr in not ((fromJust (remote cubr) ++ "/" ++ branch cubr) `isSuffixOf` line)]
    pecoDefaultOption { prompt = Just "REMOTE>" }
  return $ map (fromString . head . words) selectedLines

delete :: GitBranch -> IO ()
delete GitBranch { remote = Nothing, branch = branch_ } = do
  _ <- sh $ "git branch -d " ++ shellescape branch_
  return ()
delete _ = return ()

deleteForce :: GitBranch -> IO ()
deleteForce GitBranch { remote = Nothing, branch = branch_ } = do
  _ <- sh $ "git branch -D " ++ shellescape branch_
  return ()
deleteForce GitBranch { remote = Just remote_, branch = branch_ } = do
  _ <- sh $ unwords ["git push -d", shellescape remote_, shellescape branch_]
  return ()

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

-- TODO: https://github.com/ruby/ruby/blob/4444025d16ae1a586eee6a0ac9bdd09e33833f3c/lib/shellwords.rb#L109
shellescape :: String -> String
shellescape str = T.unpack $ T.replace ">" "\\>" $ T.pack str

data PecoOption = PecoOption {
  layout :: Maybe String,
  prompt :: Maybe String
  }

pecoDefaultOption :: PecoOption
pecoDefaultOption = PecoOption {
  layout = Nothing,
  prompt = Nothing
  }

pecoCommand :: PecoOption -> String
pecoCommand option =
  let flags = [("--layout ", layout option),
               ("--prompt ", prompt option)]
      parts = catMaybes [(++) <$> Just flag <*> value | (flag, value) <- flags] in
    unwords $ "peco" : map shellescape parts

peco :: [String] -> PecoOption -> IO [String]
peco [] _ = return []
peco input option = do
  out <- readCreateProcess (shell $ pecoCommand option) $ unlines input
  return $ lines out

-- vim:set ft=haskell:
