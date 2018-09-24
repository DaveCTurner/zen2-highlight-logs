{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.ByteString as AP
import           Data.Conduit
import           Data.Conduit.Combinators
import           Data.Either
import           Data.Functor.Identity
import           Data.Word8
import           Test.Hspec

import           CmdLineArgs
import           CombinedLines
import           CyclicAllocator
import           DiscoveryNode
import           FirstLine
import           GradleExtractor
import           NumberLines

main :: IO ()
main = hspec $ do
  describe "CmdLineArgs" $ do
    let defaultFilename = "file.log"
        defaultArgs = CmdLineArgs
          { claContinuationLines = False
          , claLineNumbers       = False
          , claTimestamp         = False
          , claLogLevel          = False
          , claLogFile           = defaultFilename
          }
        parsesAs input expected = it ("parses " ++ show input) $ parseCmdLineArgs input `shouldBe` Just expected

    it "requires a filename" $ parseCmdLineArgs [] `shouldBe` Nothing
    it "rejects multiple filenames" $ parseCmdLineArgs ["foo.txt", "bar.txt"] `shouldBe` Nothing

    [defaultFilename]                         `parsesAs` defaultArgs
    ["otherfile.log"]                         `parsesAs` defaultArgs { claLogFile = "otherfile.log" }
    [defaultFilename, "--continuation-lines"] `parsesAs` defaultArgs { claContinuationLines = True }
    [defaultFilename, "--line-numbers"]       `parsesAs` defaultArgs { claLineNumbers       = True }
    [defaultFilename, "--original-timestamp"] `parsesAs` defaultArgs { claTimestamp         = True }
    [defaultFilename, "--log-level"]          `parsesAs` defaultArgs { claLogLevel          = True }

  describe "DiscoveryNode" $ do
    it "parses {node1}{blahblahblah}" $
      AP.parseOnly discoveryNode "{node1}{blahblahblah}" `shouldBe` Right DiscoveryNode {dnName = "node1", dnUUID = "blahblahblah"}
    it "parses {}{}" $
      AP.parseOnly discoveryNode "{}{}" `shouldBe` Right DiscoveryNode {dnName = "", dnUUID = ""}
    it "composes with later parsers" $
      AP.parseOnly ((,) <$> discoveryNode <*> AP.word8 _X) "{node1}{blahblahblah}X"
        `shouldBe` Right (DiscoveryNode {dnName = "node1", dnUUID = "blahblahblah"}, _X)
    it "does not consume leading whitespace" $
      AP.parseOnly discoveryNode " {}{}" `shouldSatisfy` isLeft

  describe "FirstLine" $ do
    it "parses a real first line without node id" $
      tryParseFirstLine "[2018-09-22T02:22:10,996][TRACE][o.e.c.c.DeterministicTaskQueue] [testCanUpdateClusterStateAfterStabilisation] running task 1 of 14: {node2}{LLo5XQAAQACCkGy3_____w}: UnicastConfiguredHostsResolver resolving unicast hosts list"
        `shouldBe` Just FirstLine
          { flTimestamp = "2018-09-22T02:22:10,996"
          , flLevel = "TRACE"
          , flComponent = "o.e.c.c.DeterministicTaskQueue"
          , flTest = "testCanUpdateClusterStateAfterStabilisation"
          , flNodeId = Nothing
          , flMessage = "running task 1 of 14: {node2}{LLo5XQAAQACCkGy3_____w}: UnicastConfiguredHostsResolver resolving unicast hosts list"
          }

    it "parses a real first line with a node id" $
      tryParseFirstLine "[2018-09-22T02:22:10,996][TRACE][o.e.c.c.C.CoordinatorPeerFinder] [testCanUpdateClusterStateAfterStabilisation{nodeId={node2}{LLo5XQAAQACCkGy3_____w}}] probing resolved transport addresses [0.0.0.0:1, 0.0.0.0:2, 0.0.0.0:3, 0.0.0.0:4, 0.0.0.0:5]"
        `shouldBe` Just FirstLine
          { flTimestamp = "2018-09-22T02:22:10,996"
          , flLevel = "TRACE"
          , flComponent = "o.e.c.c.C.CoordinatorPeerFinder"
          , flTest = "testCanUpdateClusterStateAfterStabilisation"
          , flNodeId = Just DiscoveryNode {dnName = "node2", dnUUID = "LLo5XQAAQACCkGy3_____w"}
          , flMessage = "probing resolved transport addresses [0.0.0.0:1, 0.0.0.0:2, 0.0.0.0:3, 0.0.0.0:4, 0.0.0.0:5]"
          }

    it "trims whitespace from log level and component" $
      tryParseFirstLine "[2018-09-22T02:22:10,996][INFO ][o.e.c.c.C.CoordinatorPeerFinder   ] [testCanUpdateClusterStateAfterStabilisation{nodeId={node2}{LLo5XQAAQACCkGy3_____w}}] probing resolved transport addresses [0.0.0.0:1, 0.0.0.0:2, 0.0.0.0:3, 0.0.0.0:4, 0.0.0.0:5]"
        `shouldBe` Just FirstLine
          { flTimestamp = "2018-09-22T02:22:10,996"
          , flLevel = "INFO"
          , flComponent = "o.e.c.c.C.CoordinatorPeerFinder"
          , flTest = "testCanUpdateClusterStateAfterStabilisation"
          , flNodeId = Just DiscoveryNode {dnName = "node2", dnUUID = "LLo5XQAAQACCkGy3_____w"}
          , flMessage = "probing resolved transport addresses [0.0.0.0:1, 0.0.0.0:2, 0.0.0.0:3, 0.0.0.0:4, 0.0.0.0:5]"
          }

    it "fills in blanks with '??' if unreadable" $
      unreadableFirstLine "actual line content" `shouldBe` FirstLine
          { flTimestamp = "??"
          , flLevel = "??"
          , flComponent = "??"
          , flTest = "??"
          , flNodeId = Nothing
          , flMessage = "actual line content"
          }

  describe "NumberLines" $ do
    it "numbers lines from 1, and combines/splits input at newlines" $
      runIdentity (runConduit
        $  yieldMany ["line1\n", "line2\n", "line3 ...", "which was split\nand combined with line4\n", "line5 has no trailing newline"]
        .| splitAndNumberLines
        .| sinkList) `shouldBe`
          [ (1, "line1")
          , (2, "line2")
          , (3, "line3 ...which was split")
          , (4, "and combined with line4")
          , (5, "line5 has no trailing newline")
          ]

  describe "GradleExtractor" $ do
    it "filters non-indented lines and removes the indent from the rest" $
      runIdentity (runConduit
        $  yieldMany [(1, "not a log line"), (2, "  1> log line"), (3, "also not a log line"), (4, "  1> another log line")]
        .| extractLogFromOutput
        .| sinkList) `shouldBe` [(2, "log line"), (4, "another log line")]

    it "identifies non-indented output" $
      runIdentity (runConduit
        $  yieldMany [(1, "not a log line"), (2, "[2018-09-22T02:22:10,996] log line"), (3, "continuation line"), (4, "another log line")]
        .| extractLogFromOutput
        .| sinkList) `shouldBe` [(2, "[2018-09-22T02:22:10,996] log line"), (3, "continuation line"), (4, "another log line")]

  describe "CombinedLines" $ do
    it "combines first-lines with their continuation lines" $
      runIdentity (runConduit
        $  yieldMany
            [(1, "[2018-09-22T02:22:10,996][level][component] [context] message without continuation lines")
            ,(4, "[2018-09-22T02:22:10,996][level][component] [context] message that continues")
            ,(6, "... on the next line")
            ,(7, "... and on the line below too")
            ]
        .| combineContinuationLines
        .| sinkList) `shouldBe`
          [ CombinedLines
              { clStart = 1
              , clEnd = 1
              , clFirstLine = FirstLine
                { flTimestamp = "2018-09-22T02:22:10,996"
                , flLevel = "level"
                , flComponent = "component"
                , flTest = "context"
                , flNodeId = Nothing
                , flMessage = "message without continuation lines"
                }
              , clContinuationLines = []
              }
          , CombinedLines
              { clStart = 4
              , clEnd = 7
              , clFirstLine = FirstLine
                { flTimestamp = "2018-09-22T02:22:10,996"
                , flLevel = "level"
                , flComponent = "component"
                , flTest = "context"
                , flNodeId = Nothing
                , flMessage = "message that continues"
                }
              , clContinuationLines = ["... on the next line","... and on the line below too"]
              }
          ]

    it "deals with unreadable lines at the start" $
      runIdentity (runConduit
        $  yieldMany
            [(1, "not parseable as the first line of a log message")
            ,(2, "[2018-09-22T02:22:10,996][level][component] [context] message without continuation lines")
            ]
        .| combineContinuationLines
        .| sinkList) `shouldBe`
          [ CombinedLines
              { clStart = 1
              , clEnd = 1
              , clFirstLine = unreadableFirstLine "not parseable as the first line of a log message"
              , clContinuationLines = []
              }
          , CombinedLines
              { clStart = 2
              , clEnd = 2
              , clFirstLine = FirstLine
                { flTimestamp = "2018-09-22T02:22:10,996"
                , flLevel = "level"
                , flComponent = "component"
                , flTest = "context"
                , flNodeId = Nothing
                , flMessage = "message without continuation lines"
                }
              , clContinuationLines = []
              }
          ]


  describe "CyclicAllocator" $ do
    let a0 = newCyclicAllocator [1,2,3] :: CyclicAllocator String Int
        (v1, a1) = allocate "foo" a0
    it "allocates the first value" $ v1 `shouldBe` 1

    let (v2, a2) = allocate "bar" a1
    it "allocates the second value" $ v2 `shouldBe` 2

    let (v3, a3) = allocate "foo" a2
    it "yields the first value again" $ v3 `shouldBe` 1
    it "doesn't change the state" $ a3 `shouldBe` a2

    let (v4, a4) = allocate "baz" a3
    it "allocates the third value" $ v4 `shouldBe` 3

    let (v5, _) = allocate "quuz" a4
    it "allocates the first value again" $ v5 `shouldBe` 1














