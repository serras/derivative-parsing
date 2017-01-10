{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TupleSections #-}
module Text.Parsing.Derivative where

import Control.Applicative
import Data.Maybe
import Data.String

data Parser s a where
  Epsilon  :: a -> Parser s a
  -- Literals should *never* be null
  Literal  :: s -> Parser s s
  Failure  :: Parser s a
  Or       :: Parser s a -> Parser s a -> Parser s a
  Sequence :: Parser s a -> Parser s b -> Parser s (a,b)
  Apply    :: (a -> [b]) -> Parser s a -> Parser s b

instance Functor (Parser s) where
  fmap f = Apply (\x -> [f x])

instance Applicative (Parser s) where
  -- pure :: a -> Parser s a
  pure    = Epsilon
  -- <*> :: Parser s (a -> b) -> Parser s a -> Parser s b
  f <*> x = fmap (uncurry ($)) (Sequence f x)

instance Alternative (Parser s) where
  empty = Failure
  (<|>) = Or

instance IsString s => IsString (Parser s s) where
  fromString x = Literal (fromString x)

class (Eq (ParserElement s), Monoid s) => ParserSource s where
  type ParserElement s
  singleton :: ParserElement s -> s
  isNull    :: s -> Bool
  -- Precondition: first argument is not null
  uncons    :: s -> (ParserElement s, s)

instance Eq a => ParserSource [a] where
  type ParserElement [a] = a
  singleton x = [x]
  isNull      = null
  uncons x    = (head x, tail x) 

nullable :: ParserSource s => Parser s a -> Bool
nullable (Epsilon a)    = True
nullable (Literal s)    = False
nullable Failure        = True
nullable (Or a b)       = nullable a || nullable b
nullable (Sequence a b) = nullable a && nullable b
nullable (Apply f x)    = nullable x

derive :: ParserSource s => Parser s a -> ParserElement s -> Parser s a
derive (Epsilon a)    _ = Failure
derive (Literal s)    e
  = case uncons s of
      (se, sr)
        | se == e   -> fmap (mappend (singleton e))
                            (if isNull sr then Epsilon mempty else Literal sr)
        | otherwise -> Failure
derive Failure        _ = Failure
derive (Or a b)       e = Or (derive a e) (derive b e)
derive (Sequence a b) e
  = let nonNulled = Sequence (derive a e) b
     in if nullable a
           then case finish a of
                  Nothing -> nonNulled
                  Just vs -> Or nonNulled $ Apply (\b -> map (,b) vs) (derive b e)
           else nonNulled
derive (Apply f x)    e = Apply f (derive x e) 

finish :: Parser s a -> Maybe [a]
finish (Epsilon a)    = Just [a]
finish (Literal s)    = Nothing
finish Failure        = Nothing
finish (Or a b)       = (++) <$> finish a <*> finish b
                        <|> finish a
                        <|> finish b
finish (Sequence a b) = do fa <- finish a
                           fb <- finish b
                           return [(ea, eb) | ea <- fa, eb <- fb]
finish (Apply f x)    = do fx <- finish x
                           return $ concatMap f fx

parse :: ParserSource s => Parser s a -> s -> Maybe [a]
parse p x | isNull x  = finish p
          | otherwise = case uncons x of
                          (e, r) -> parse (derive p e) r