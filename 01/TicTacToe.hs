data TicTacToe a = TicTacToe
  { topLeft   :: a
  , topCenter :: a
  , topRight  :: a
  , midLeft   :: a
  , midCenter :: a
  , midRight  :: a
  , botLeft   :: a
  , botCenter :: a
  , botRight  :: a
  }

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard = TicTacToe Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                       Nothing Nothing

data Three = One | Two | Three deriving (Eq, Ord, Enum, Bounded)
type Coordinate = (Three, Three)

data TicTacToe2 a = TicTacToe2 { board :: Coordinate -> a }

emptyBoard2 :: TicTacToe2 (Maybe Bool)
emptyBoard2 = TicTacToe2 $ const Nothing
