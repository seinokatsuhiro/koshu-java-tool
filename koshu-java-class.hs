{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Data.ByteString.Lazy          as B
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Word                     as W
import qualified System.Environment            as E

import qualified JVM.Assembler                 as J
import qualified JVM.ClassFile                 as J
import qualified JVM.Converter                 as J

import qualified Koshucode.Baala.Base          as K
import qualified Koshucode.Baala.Core          as K
import qualified Koshucode.Baala.Type.Vanilla  as K


main :: IO ()
main = do
  args <- E.getArgs
  mapM_ dump args

dump :: String -> IO ()
dump path = do
  cls <- J.parseClassFile path
  dumpClassDirect cls

type ClassD        = J.Class       J.Direct
type ConstantD     = J.Constant    J.Direct
type FieldD        = J.Field       J.Direct
type MethodD       = J.Method      J.Direct
type AttributesD   = J.Attributes  J.Direct
type AccessFlagsD  = J.AccessFlags J.Direct


-- --------------------------------------------  Class

dumpClassDirect :: ClassD -> IO ()
dumpClassDirect c = K.putLines texts where
    texts     = concatGap [ dumpComment, o ab, o $ dumpClass c
                          , o "**  Constant pool", pools
                          , o "**  Field", fields
                          , o "**  Method", methods
                          , o "**  End"]
    o text    = [text]
    ab        = about [ term "this-class" $ pBytes $ J.thisClass c ]
    pools     = dumpPool  `map`       (Map.assocs $ J.constsPool c)
    fields    = dumpField `map`       J.classFields c
    methods   = dumpMeth  `concatMap` J.classMethods c
    dumpMeth  = dumpMethod $ J.thisClass c

dumpComment :: [String]
dumpComment = [ "** -*- koshu -*-"
              , "** "
              , "**  Java class file in Koshucode generated by 'koshu-java-class' command."
              , "** "
              ]

dumpClass :: ClassD -> String
dumpClass c =
    judge "CLASS" [ term "major"        $ pWord16  $ J.majorVersion c
                  , term "minor"        $ pWord16  $ J.minorVersion c
                  , term "super-class"  $ pClass   $ J.superClass c
                  , term "pool-size"    $ pWord16  $ J.constsPoolSize c
                  , term "accessor"     $ pAccSet  $ J.accessFlags c
                  , term "if-count"     $ pWord16  $ J.interfacesCount c
                  , term "field-count"  $ pWord16  $ J.classFieldsCount c
                  , term "method-count" $ pWord16  $ J.classMethodsCount c
                  , term "attr-count"   $ pWord16  $ J.classAttributesCount c
                  , term "attr"         $ pAttrSet $ J.classAttributes c ]


-- --------------------------------------------  Constant pool

dumpPool :: (W.Word16, ConstantD) -> String
dumpPool (i, J.CClass c) = judge "POOL-CLASS" xs where
    xs = [ term "index"  $ pWord16 i
         , term "class"  $ pClass c ]
dumpPool (i, J.CField c (J.NameType n t)) = judge "POOL-FIELD" xs where
    xs = [ term "index"  $ pWord16 i
         , term "class"  $ pClass c
         , term "field"  $ pBytes n
         , term "type"   $ pShow t ]
dumpPool (i, J.CMethod c (J.NameType n t)) = judge "POOL-METHOD" xs where
    xs = [ term "index"  $ pWord16 i
         , term "class"  $ pClass c
         , term "method" $ pBytes n
         , term "type"   $ pShow t ]
dumpPool (i, J.CIfaceMethod c (J.NameType n t)) = judge "POOL-IF-METHOD" xs where
    xs = [ term "index"  $ pWord16 i
         , term "class"  $ pClass c
         , term "method" $ pBytes n
         , term "type"   $ K.pText (show t) ]
dumpPool (i, J.CString s) = judge "POOL-STRING" xs where
    xs = [ term "index"  $ pWord16 i
         , term "value"  $ pBytes s ]
dumpPool (i, J.CInteger n) = judge "POOL-INTEGER" xs where
    xs = [ term "index"  $ pWord16 i
         , term "value"  $ pWord32 n ]
dumpPool (i, J.CFloat _) = judge "POOL-FLOAT" xs where
    xs = [ term "index"  $ pWord16 i
         , term "value"  $ K.pText "unimplemented" ]
dumpPool (i, J.CLong _) = judge "POOL-LONG" xs where
    xs = [ term "index"  $ pWord16 i
         , term "value"  $ K.pText "unimplemented" ]
dumpPool (i, J.CDouble _) = judge "POOL-DOUBLE" xs where
    xs = [ term "index"  $ pWord16 i
         , term "value"  $ K.pText "unimplemented" ]
dumpPool (i, J.CNameType n t) = judge "POOL-NAME-TYPE" xs where
    xs = [ term "index"  $ pWord16 i
         , term "name"   $ pBytes n
         , term "type"   $ pBytes t]
dumpPool (i, J.CUTF8 s) = judge "POOL-UTF8" xs where
    xs = [ term "index"  $ pWord16 i
         , term "value"  $ pBytes s ]
dumpPool (i, J.CUnicode s) = judge "POOL-UNICODE" xs where
    xs = [ term "index"  $ pWord16 i
         , term "value"  $ pBytes s]


-- --------------------------------------------  Field

dumpField :: FieldD -> String
dumpField f =
    judge "FIELD"
         [ term "accessor"   $ pAccSet  $ J.fieldAccessFlags f
         , term "field"      $ pBytes   $ J.fieldName f
         , term "type"       $ pShow    $ J.fieldSignature f
         , term "attr-count" $ pWord16  $ J.fieldAttributesCount f
         , term "attr"       $ pAttrSet $ J.fieldAttributes f ]


-- --------------------------------------------  Method

dumpMethod :: B.ByteString -> MethodD -> [String]
dumpMethod clsName m = gap $ concatGap [[ab], [meth], code] where
    ab     = about [ term "this-class"  $ pBytes clsName
                   , term "method"      $ pBytes $ J.methodName m ]
    meth   = let J.MethodSignature args ret = J.methodSignature m
             in judge "METHOD"
                    [ term "accessor"   $ pAccSet  $ J.methodAccessFlags m
                    , term "args-type"  $ K.pList  $ pShow `map` args
                    , term "ret-type"   $ pShow    ret
                    , term "attr-count" $ pWord16  $ J.methodAttributesCount m
                    , term "attr"       $ pAttrSet $ J.methodAttributes m ]
    code   = case J.attrByName m "Code" of
               Nothing -> ["** no code"]
               Just bytecode ->
                   let inst = zip [1..] $ J.codeInstructions $ J.decodeMethod bytecode
                   in concatGap [map dumpInst inst, K.mapMaybe dumpInstDetail inst]


-- --------------------------------------------  Instruction

dumpInst :: (Int, J.Instruction) -> String
dumpInst (n, i) = judge "INST" [ term "seq"  $ K.pDecFromInt n
                               , term "inst" $ pShow i ]

dumpInstDetail :: (Int, J.Instruction) -> Maybe String
dumpInstDetail (n, J.GETSTATIC i)          = dumpInstField  n i "getstatic"
dumpInstDetail (n, J.PUTSTATIC i)          = dumpInstField  n i "putstatic"
dumpInstDetail (n, J.GETFIELD i)           = dumpInstField  n i "getfield"
dumpInstDetail (n, J.PUTFIELD i)           = dumpInstField  n i "putfield"

dumpInstDetail (n, J.INVOKEVIRTUAL i)      = dumpInstMethod n i "invokevirtual"
dumpInstDetail (n, J.INVOKESPECIAL i)      = dumpInstMethod n i "invokespecial"
dumpInstDetail (n, J.INVOKESTATIC i)       = dumpInstMethod n i "invokestatic"
dumpInstDetail (n, J.INVOKEINTERFACE i _)  = dumpInstMethod n i "invokeinterface"

dumpInstDetail (n, J.NEW i)                = dumpInstMethod n i "new"
dumpInstDetail (n, J.ANEWARRAY i)          = dumpInstClass  n i "anewarray"
dumpInstDetail (n, J.CHECKCAST i)          = dumpInstClass  n i "checkcast"
dumpInstDetail (n, J.INSTANCEOF i)         = dumpInstClass  n i "instanceof"
dumpInstDetail _ = Nothing

dumpInstClass :: Int -> W.Word16 -> String -> Maybe String
dumpInstClass n i op = Just $ judge "INST-CLASS" xs where
    xs = [ term "seq"   $ K.pDecFromInt n
         , term "op"    $ K.pText op
         , term "index" $ pWord16 i ]

dumpInstField :: Int -> W.Word16 -> String -> Maybe String
dumpInstField n i op = Just $ judge "INST-FIELD" xs where
    xs = [ term "seq"   $ K.pDecFromInt n
         , term "op"    $ K.pText op
         , term "index" $ pWord16 i ]

dumpInstMethod :: Int -> W.Word16 -> String -> Maybe String
dumpInstMethod n i op = Just $ judge "INST-METHOD" xs where
    xs = [ term "seq"   $ K.pDecFromInt n
         , term "op"    $ K.pText op
         , term "index" $ pWord16 i ]


-- --------------------------------------------  Putter

pAccSet :: AccessFlagsD -> K.VContent
pAccSet a = K.pSet $ K.pText `map` accessFlagsText a

pAttrSet :: AttributesD -> K.VContent
pAttrSet a = K.pSet $ (pBytes . fst) `map` J.arlist a

pClass :: B.ByteString -> K.VContent
pClass c = K.pText $ map dot $ J.toString c where
    dot '/' = '.'
    dot x   = x

pWord16 :: W.Word16 -> K.VContent
pWord16 i = K.pDecFromInt i' where
    i' = fromIntegral i :: Int

pWord32 :: W.Word32 -> K.VContent
pWord32 i = K.pDecFromInt i' where
    i' = fromIntegral i :: Int

pBytes :: B.ByteString -> K.VContent
pBytes = K.pText . J.toString

pShow :: (Show a) => a -> K.VContent
pShow = K.pText . show


-- --------------------------------------------  Utility

accessFlagsText :: AccessFlagsD -> [String]
accessFlagsText = map accessFlagText . Set.elems

accessFlagText :: J.AccessFlag -> String
accessFlagText J.ACC_PUBLIC        = "public"
accessFlagText J.ACC_PRIVATE       = "private"
accessFlagText J.ACC_PROTECTED     = "protected"
accessFlagText J.ACC_STATIC        = "static"
accessFlagText J.ACC_FINAL         = "final"
accessFlagText J.ACC_SYNCHRONIZED  = "synchronized"
accessFlagText J.ACC_VOLATILE      = "volatile"
accessFlagText J.ACC_TRANSIENT     = "transient"
accessFlagText J.ACC_NATIVE        = "native"
accessFlagText J.ACC_INTERFACE     = "interface"
accessFlagText J.ACC_ABSTRACT      = "abstract"

gap :: K.Map [String]
gap xs@("" : _) = xs
gap xs          = "" : xs

concatGap :: [[String]] -> [String]
concatGap [] = []
concatGap (x:xs) = x ++ concatMap gap xs

about :: (K.Write c) => [K.Named c] -> String
about xs = "about " ++ xs' where
    xs' = show $ K.writeTerms (K.write K.shortEmpty) xs

judge :: String -> [(String, K.VContent)] -> String
judge pat = K.judgeShow K.shortEmpty . K.JudgeAffirm pat

term :: String -> c -> (String, c)
term n c = (n, c)
