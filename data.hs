
module AST where
import Data.ByteString (ByteString)
import Data.Array.IArray
import Data.Int

type Var = Int
-- TNap is at most 64
data Type = TByte | TNap Int64 deriving (Show)
type Value = Int64
data Binop = Or | Xor | And | Nand deriving (Show)
data GArg a = Avar a | Aconst Value
type Arg = GArg Var
data GExp a =
    Earg (GArg a)
  | Ereg a
  | Enot (GArg a)
  | Ebinop Binop (GArg a) (GArg a)
  | Emux (GArg a) (GArg a) (GArg a)
  | Erom Int64 Int64 (GArg a)
  | Eram Int64 Int64 (GArg a) (GArg a) (GArg a) (GArg a)
  | Econcat (GArg a) (GArg a)
  | Eslice Int64 Int64 (GArg a)
  | Eselect Int64 (GArg a)
type Exp = GExp Var

data GEq a = GEq a (GExp a)
type Equation = GEq Var

data Program = Prog {
    p_eqs     :: [Equation]
  , p_inputs  :: [Var]
  , p_outputs :: [Var]
  , p_types   :: Array Int Type
  }

instance Show a => Show (GArg a) where
  show (Aconst v) = "Aconst " ++ show v
  show (Avar x)   = "Avar " ++ show x

instance Show a => Show (GExp a) where
  show (Earg a)                 = "Earg  " ++ show a
  show (Ereg a)                 = "Ereg " ++ show a
  show (Enot a)                 = "Enot " ++ show a
  show (Ebinop b a1 a2)         = "Ebinop " ++ show b ++ ' ' : show a1 ++ ' ' : show a2
  show (Emux a1 a2 a3)          = "Emux " ++ ' ': show a1 ++ ' ': show a2 ++ ' ': show a3
  show (Erom n1 n2 a)           = "Erom " ++ show n1 ++ ' ': show n2 ++ ' ': show a
  show (Eram n1 n2 a1 a2 a3 a4) = "Eram " ++ ' ': show n1 ++ ' ': show n2 ++ ' ': show a1 ++ ' ': show a2
                                          ++ ' ': show a3 ++ ' ': show a4
  show (Econcat a1 a2)          = "Econcat " ++ show a1 ++ ' ':show a2
  show (Eslice n1 n2 a)         = "Eslice " ++ show n1 ++ ' ': show n2 ++ ' ': show a
  show (Eselect n a)            = "Eselect " ++ show n ++ ' ': show a

instance Show a => Show (GEq a) where
  show (GEq a e) = "Exp: " ++ show a ++ " = " ++ show e

instance Functor GArg where
  fmap f (Avar a)   = Avar $ f a
  fmap f (Aconst v) = Aconst v

instance Functor GExp where
  fmap f (Earg a)                 = Earg $ fmap f a
  fmap f (Ereg a)                 = Ereg $ f a
  fmap f (Enot a)                 = Enot $ fmap f a
  fmap f (Ebinop b a1 a2)         = Ebinop b (fmap f a1) $ fmap f a2
  fmap f (Emux a1 a2 a3)          = Emux (fmap f a1) (fmap f a2) $ fmap f a3
  fmap f (Erom n1 n2 a)           = Erom n1 n2 $ fmap f a
  fmap f (Eram n1 n2 a1 a2 a3 a4) = Eram n1 n2 (fmap f a1) (fmap f a2) (fmap f a3) $ fmap f a4
  fmap f (Econcat a1 a2)          = Econcat (fmap f a1) $ fmap f a2
  fmap f (Eslice n1 n2 a)         = Eslice n1 n2 $ fmap f a
  fmap f (Eselect n a)            = Eselect n $ fmap f a

instance Functor GEq where
  fmap f (GEq v exp) = GEq (f v) $ fmap f exp

