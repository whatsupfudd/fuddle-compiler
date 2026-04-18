{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fuddle.Compiler.Parse.Internal.State
  ( Parser
  , StateParse(..)
  , runParser
  , peekTokMay
  , bumpTok
  , markErr
  , anchorPush
  , anchorPop
  ) where

import Control.Monad.State.Strict (MonadState, State, get, modify', runState)

import Control.Applicative (Alternative (..), (<|>))

import Fuddle.Compiler.Base.Diag (Diag)
import Fuddle.Compiler.Syntax.Event (ParseEvent(..))
import Fuddle.Compiler.Syntax.Kind (SyntaxKind)
import Fuddle.Compiler.Syntax.Token (TokIx, TokenLex, TokenStream, lookupTok)

newtype Parser a = Parser { unParser :: State StateParse a }
  deriving newtype (Functor, Applicative, Monad, MonadState StateParse)


data StateParse = StateParse
  { toksSP :: !TokenStream
  , nextTokSP :: !TokIx
  , anchorsSP :: ![SyntaxKind]
  , eventsRevSP :: ![ParseEvent]
  , diagsRevSP :: ![Diag]
  , recoveredSP :: !Bool
  }
  deriving stock (Eq, Show)

runParser :: TokenStream -> Parser a -> (a, StateParse)
runParser toks parser = runState parser.unParser (initState toks)

peekTokMay :: Parser (Maybe (TokIx, TokenLex))
peekTokMay = do
  st <- get
  pure $ fmap (\tok -> (st.nextTokSP, tok)) (lookupTok st.toksSP st.nextTokSP)

bumpTok :: Parser (Maybe (TokIx, TokenLex))
bumpTok = do
  tokMay <- peekTokMay
  case tokMay of
    Nothing -> pure Nothing
    Just tokRef -> do
      modify' bumpNextTok
      pure (Just tokRef)

markErr :: Diag -> Parser ()
markErr diag =
  modify' $ \st ->
    st
      { diagsRevSP = diag : st.diagsRevSP
      , eventsRevSP = ErrorPE diag : st.eventsRevSP
      , recoveredSP = True
      }

anchorPush :: SyntaxKind -> Parser ()
anchorPush anchor =
  modify' $ \st -> st { anchorsSP = anchor : st.anchorsSP }

anchorPop :: Parser (Maybe SyntaxKind)
anchorPop = do
  st <- get
  case st.anchorsSP of
    [] -> pure Nothing
    anchor : rest -> do
      modify' $ \st0 -> st0 { anchorsSP = rest }
      pure (Just anchor)

initState :: TokenStream -> StateParse
initState toks =
  StateParse
    { toksSP = toks
    , nextTokSP = toEnum 0
    , anchorsSP = []
    , eventsRevSP = []
    , diagsRevSP = []
    , recoveredSP = False
    }

bumpNextTok :: StateParse -> StateParse
bumpNextTok st = st { nextTokSP = succ st.nextTokSP }