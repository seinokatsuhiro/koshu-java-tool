{-# OPTIONS_GHC -Wall #-}

module InstSize (instSize) where

import qualified JVM.Assembler as J

instSize :: J.Instruction -> Int

instSize (J.BIPUSH _)               = 2
instSize (J.SIPUSH _)               = 3
instSize (J.LDC1 _)                 = 2
instSize (J.LDC2 _)                 = 3
instSize (J.LDC2W _)                = 3

instSize (J.ILOAD _)                = 2
instSize (J.LLOAD _)                = 2
instSize (J.FLOAD _)                = 2
instSize (J.DLOAD _)                = 2
instSize (J.ALOAD _)                = 2

instSize (J.ISTORE _)               = 2
instSize (J.LSTORE _)               = 2
instSize (J.FSTORE _)               = 2
instSize (J.DSTORE _)               = 2
instSize (J.ASTORE _)               = 2

instSize (J.IINC _ _)               = 3

instSize (J.IF _ _)                 = 3
instSize (J.IF_ICMP _ _)            = 3
instSize (J.IF_ACMP _ _)            = 3
instSize (J.GOTO _)                 = 3
instSize (J.JSR _)                  = 3

instSize (J.TABLESWITCH _ _ _ _ xs) = 14 + (4 * length xs)
instSize (J.LOOKUPSWITCH _ _ _ xs)  = 10 + (8 * length xs)

instSize (J.GETSTATIC _)            = 3
instSize (J.PUTSTATIC _)            = 3
instSize (J.GETFIELD _)             = 3
instSize (J.PUTFIELD _)             = 3

instSize (J.INVOKEVIRTUAL _)        = 3
instSize (J.INVOKESPECIAL _)        = 3
instSize (J.INVOKESTATIC _)         = 3
instSize (J.INVOKEINTERFACE _ _)    = 5
instSize (J.NEW _)                  = 3
instSize (J.NEWARRAY _)             = 2
instSize (J.ANEWARRAY _)            = 3

instSize (J.CHECKCAST _)            = 3
instSize (J.INSTANCEOF _)           = 3

instSize (J.WIDE _ i)               = 2 + instSize i
instSize (J.MULTINANEWARRAY _ _)    = 4
instSize (J.IFNULL _)               = 3
instSize (J.IFNONNULL _)            = 3

instSize (J.GOTO_W _)               = 5
instSize (J.JSR_W _)                = 5

instSize _                          = 1
