{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fuddle.Compiler.Parse.Internal.State
  ( Parser
  , StateParse(..)
  , runParser
  , peekTokMay
  , bumpTok
  , markErr
  , anchorPush
  , anchorPop
  , lookAheadP
  , softFail
  , tryP
  ) where

import Control.Applicative (Alternative(..), Alternative (..), (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.State.Strict (StateT (..), get, modify', runState)

import Fuddle.Compiler.Base.Diag (Diag)
import Fuddle.Compiler.Syntax.Event (ParseEvent(..))
import Fuddle.Compiler.Syntax.Kind (SyntaxKind)
import Fuddle.Compiler.Syntax.Token (TokIx, TokenLex, TokenStream, lookupTok)

data ResultP a =
    OkP !a !StateParse
  | SoftFailP


newtype Parser a = Parser { unParser :: StateParse -> ResultP a }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \st ->
    case p st of
      OkP a st' -> OkP (f a) st'
      SoftFailP -> SoftFailP

instance Applicative Parser where
  pure a = Parser $ \st -> OkP a st
  Parser pf <*> Parser pa = Parser $ \st ->
    case pf st of
      SoftFailP -> SoftFailP
      OkP f st1 ->
        case pa st1 of
          SoftFailP -> SoftFailP
          OkP a st2 -> OkP (f a) st2

instance Monad Parser where
  Parser pa >>= f = Parser $ \st ->
    case pa st of
      SoftFailP -> SoftFailP
      OkP a st1 -> unParser (f a) st1


instance Alternative Parser where
  empty = Parser $ const SoftFailP
  Parser p1 <|> Parser p2 = Parser $ \st ->
    case p1 st of
      SoftFailP -> p2 st
      ok -> ok

instance MonadPlus Parser
instance MonadState StateParse Parser where
  state f = Parser $ \st -> let (a, st') = f st in OkP a st'



data StateParse = StateParse
  { toksSP :: !TokenStream
  , nextTokSP :: !TokIx
  , anchorsSP :: ![SyntaxKind]
  , eventsRevSP :: ![ParseEvent]
  , diagsRevSP :: ![Diag]
  , recoveredSP :: !Bool
  }
  deriving stock (Eq, Show)

runParserState :: StateParse -> Parser a -> ResultP a
runParserState st p = unParser p st

runParser :: TokenStream -> Parser a -> Maybe (a, StateParse)
runParser toks p =
  case unParser p (initState toks) of
    OkP a st -> Just (a, st)
    SoftFailP -> Nothing


softFail :: Parser a
softFail = empty

tryP :: Parser a -> Parser a
tryP = id

lookAheadP :: Parser a -> Parser a
lookAheadP (Parser p) = Parser $ \st ->
  case p st of
    SoftFailP -> SoftFailP
    OkP a _ -> OkP a st


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
      pure $ Just anchor

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