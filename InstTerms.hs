{-# OPTIONS_GHC -Wall #-}

module InstTerms (instTerms) where

import qualified Data.Char                as Char
import qualified JVM.Assembler            as J
import qualified Koshucode.Baala.Core     as K
import qualified Content                  as K

op :: (K.CText c) => String -> (String, c)
op name = ("op", K.pText name)

op0 :: (K.CContent c) => String -> [(String, c)]
op0 name = [op name]

op1 :: (K.CContent c) => String -> c -> [(String, c)]
op1 name x1 = [op name, ("x1", x1)]

op2 :: (K.CContent c) => String -> c -> c -> [(String, c)]
op2 name x1 x2 = [op name, ("x1", x1), ("x2", x2)]

op3 :: (K.CContent c) => String -> c -> c -> c -> [(String, c)]
op3 name x1 x2 x3 = [op name, ("x1", x1), ("x2", x2), ("x3", x3)]

op4 :: (K.CContent c) => String -> c -> c -> c -> c -> [(String, c)]
op4 name x1 x2 x3 x4 = [op name, ("x1", x1), ("x2", x2), ("x3", x3), ("x4", x4)]

cmp :: J.CMP -> String
cmp J.C_EQ = "eq"
cmp J.C_NE = "ne"
cmp J.C_LT = "lt"
cmp J.C_GE = "ge"
cmp J.C_GT = "gt"
cmp J.C_LE = "le"

imm :: J.IMM -> String
imm J.I0 = "0"
imm J.I1 = "1"
imm J.I2 = "2"
imm J.I3 = "3"

instTerms :: (K.CContent c) => J.Instruction -> [(String, c)]

instTerms (J.BIPUSH x1)               = op1 "bipush" $ K.pWord8 x1
instTerms (J.SIPUSH x1)               = op1 "sipush" $ K.pWord16 x1
instTerms (J.LDC1 x1)                 = op1 "ldc1"   $ K.pWord8 x1
instTerms (J.LDC2 x1)                 = op1 "ldc2"   $ K.pWord16 x1
instTerms (J.LDC2W x1)                = op1 "ldc2w"  $ K.pWord16 x1

instTerms (J.ILOAD x1)                = op1 "iload"  $ K.pWord8 x1
instTerms (J.LLOAD x1)                = op1 "lload"  $ K.pWord8 x1
instTerms (J.FLOAD x1)                = op1 "fload"  $ K.pWord8 x1
instTerms (J.DLOAD x1)                = op1 "dload"  $ K.pWord8 x1
instTerms (J.ALOAD x1)                = op1 "aload"  $ K.pWord8 x1

instTerms (J.ILOAD_ i)                = op0 ("iload_" ++ imm i)
instTerms (J.LLOAD_ i)                = op0 ("lload_" ++ imm i)
instTerms (J.FLOAD_ i)                = op0 ("fload_" ++ imm i)
instTerms (J.DLOAD_ i)                = op0 ("dload_" ++ imm i)
instTerms (J.ALOAD_ i)                = op0 ("aload_" ++ imm i)

instTerms (J.ISTORE x1)               = op1 "istore"  $ K.pWord8 x1
instTerms (J.LSTORE x1)               = op1 "lstore"  $ K.pWord8 x1
instTerms (J.FSTORE x1)               = op1 "fstore"  $ K.pWord8 x1
instTerms (J.DSTORE x1)               = op1 "dstore"  $ K.pWord8 x1
instTerms (J.ASTORE x1)               = op1 "astore"  $ K.pWord8 x1

instTerms (J.ISTORE_ i)                = op0 ("istore_" ++ imm i)
instTerms (J.LSTORE_ i)                = op0 ("lstore_" ++ imm i)
instTerms (J.FSTORE_ i)                = op0 ("fstore_" ++ imm i)
instTerms (J.DSTORE_ i)                = op0 ("dstore_" ++ imm i)
instTerms (J.ASTORE_ i)                = op0 ("astore_" ++ imm i)

instTerms (J.IINC x1 x2)              = op2 "iinc" (K.pWord8 x1) (K.pWord8 x2)

instTerms (J.IF c x1)                 = op1 ("if" ++ cmp c) (K.pWord16 x1)
instTerms (J.IF_ICMP c x1)            = op1 ("if_icmp" ++ cmp c) $ K.pWord16 x1
instTerms (J.IF_ACMP c x1)            = op1 ("if_acmp" ++ cmp c) $ K.pWord16 x1
instTerms (J.GOTO x1)                 = op1 "goto"  $ K.pWord16 x1
instTerms (J.JSR x1)                  = op1 "jsr"   $ K.pWord16 x1

instTerms (J.TABLESWITCH x1 x2 x3 x4 _) = op4 "tableswitch"  (K.pWord8 x1) (K.pWord32 x2) (K.pWord32 x3) (K.pWord32 x4)
instTerms (J.LOOKUPSWITCH x1 x2 x3 _)   = op3 "lookupswitch" (K.pWord8 x1) (K.pWord32 x2) (K.pWord32 x3)

instTerms (J.GETSTATIC x1)            = op1 "getstatic" $ K.pWord16 x1
instTerms (J.PUTSTATIC x1)            = op1 "putstatic" $ K.pWord16 x1
instTerms (J.GETFIELD x1)             = op1 "getfield"  $ K.pWord16 x1
instTerms (J.PUTFIELD x1)             = op1 "putfield"  $ K.pWord16 x1

instTerms (J.INVOKEVIRTUAL x1)        = op1 "invokevirtual"   $ K.pWord16 x1
instTerms (J.INVOKESPECIAL x1)        = op1 "invokespecial"   $ K.pWord16 x1
instTerms (J.INVOKESTATIC x1)         = op1 "invokestatic"    $ K.pWord16 x1
instTerms (J.INVOKEINTERFACE x1 x2)   = op2 "invokeinterface" (K.pWord16 x1) (K.pWord8 x2)
instTerms (J.NEW x1)                  = op1 "new"             $ K.pWord16 x1
instTerms (J.NEWARRAY x1)             = op1 "newarray"        $ K.pWord8 x1
instTerms (J.ANEWARRAY x1)            = op1 "anewarray"       $ K.pWord16 x1

instTerms (J.CHECKCAST x1)            = op1 "checkcast"       $ K.pWord16 x1
instTerms (J.INSTANCEOF x1)           = op1 "instanceof"      $ K.pWord16 x1

instTerms (J.WIDE x1 i)               = op1 "wide"            $ K.pWord8 x1
instTerms (J.MULTINANEWARRAY x1 x2)   = op2 "multinanewarray" (K.pWord16 x1) (K.pWord8 x2)
instTerms (J.IFNULL x1)               = op1 "ifnull"         $ K.pWord16 x1
instTerms (J.IFNONNULL x1)            = op1 "ifnonnull"      $ K.pWord16 x1

instTerms (J.GOTO_W x1)               = op1 "goto_w"         $ K.pWord32 x1
instTerms (J.JSR_W x1)                = op1 "jsr_w"          $ K.pWord32 x1

instTerms name                        = op0 $ map Char.toLower $ show name
