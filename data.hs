
module AST where
import Data.ByteString (ByteString)
import Data.Array.IArray
import Data.Int

type Var = Int
-- TNap is at most 64
data Type = TBit | TNap Int64 deriving (Show, Eq)
type Value = Int64
data Binop = Or | Xor | And | Nand deriving (Show, Eq)
data Arg = Avar Var | Aconst Value deriving (Show)
data Exp =
    Earg Arg
  | Ereg Var
  | Enot Arg
  | Ebinop Binop Arg Arg
  | Emux Arg Arg Arg -- a3 selects either a1 or a2
  | Erom Int64 Int64 Arg -- addr_size word_size read_addr
  | Eram Int64 Int64 Arg Arg Arg Arg
    -- addr_size word_size read_addr write_enable write_addr data
  | Econcat Arg Arg
  | Eslice Int64 Int64 Arg -- slice a between i1 and i2
  | Eselect Int64 Arg -- select the ith element
  deriving (Show)

data Equation = Eq Var Exp

data Program = Prog {
    p_eqs     :: [Equation]
  , p_inputs  :: [Var]
  , p_outputs :: [Var]
  , p_types   :: Array Int Type
  }

