{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Word            as W
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Text.Printf (printf)
import qualified System.Environment  as E

import qualified JVM.Assembler       as J
import qualified JVM.ClassFile       as J
import qualified JVM.Converter       as J

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
  dumpClass cls

dumpClass :: J.Class J.Direct -> IO ()
dumpClass c =
  do putStr $ unlines texts
    where
      texts   = mode ++ gap (dumpHead c) ++ gap pools ++ gap fields ++ gap methods
      mode    = ["-*- koshu -*-"]
      clsName = show $ J.thisClass c
      poolMap = Map.assocs $ J.constsPool c
      pools   = map dumpPool poolMap
      fields  = map dumpField $ J.classFields c
      methods = concatMap (dumpMethod clsName) $ J.classMethods c

gap :: K.Map [String]
gap xs@("" : _) = xs
gap xs          = "" : xs

dumpHead :: J.Class J.Direct -> [String]
dumpHead c =
    [ printf "about /class %s\n" (show $ J.toString $ J.thisClass c)
    , judge "CLASS" [ term "major"        $ pWord16 $ J.majorVersion c
                    , term "minor"        $ pWord16 $ J.minorVersion c
                    , term "super-class"  $ K.pText $ J.toString $ J.superClass c
                    , term "pool-size"    $ pWord16 $J.constsPoolSize c
                    , term "accessor"     $ K.pText $ unwords $ accessFlagsText $ J.accessFlags c
                    , term "if-count"     $ pWord16 $ J.interfacesCount c
                    , term "field-count"  $ pWord16 $ J.classFieldsCount c
                    , term "method-count" $ pWord16 $ J.classMethodsCount c
                    , term "attr-count"   $ pWord16 $ J.classAttributesCount c ]]

accessFlagsText :: J.AccessFlags J.Direct -> [String]
accessFlagsText = map toText . Set.elems where
    toText J.ACC_PUBLIC        = "public"
    toText J.ACC_PRIVATE       = "private"
    toText J.ACC_PROTECTED     = "protected"
    toText J.ACC_STATIC        = "static"
    toText J.ACC_FINAL         = "final"
    toText J.ACC_SYNCHRONIZED  = "synchronized"
    toText J.ACC_VOLATILE      = "volatile"
    toText J.ACC_TRANSIENT     = "transient"
    toText J.ACC_NATIVE        = "native"
    toText J.ACC_INTERFACE     = "interface"
    toText J.ACC_ABSTRACT      = "abstract"

dumpPool :: (W.Word16, J.Constant J.Direct) -> String
dumpPool (i, J.CClass c) = judge "POOL-CLASS" xs where
    xs = [ term "index" $ pWord16 i
         , term "class" $ pByteString c ]
dumpPool (i, J.CField c (J.NameType n t)) = judge "POOL-FIELD" xs where
    xs = [ term "index"  $ pWord16 i
         , term "class"  $ pByteString c
         , term "field"  $ pByteString n
         , term "type"   $ K.pText (show t) ]
dumpPool (i, J.CMethod c (J.NameType n t)) = judge "POOL-METHOD" xs where
    xs = [ term "index"  $ pWord16 i
         , term "class"  $ pByteString c
         , term "method" $ pByteString n
         , term "type"   $ K.pText (show t) ]
dumpPool (i, J.CIfaceMethod c (J.NameType n t)) = judge "POOL-IF-METHOD" xs where
    xs = [ term "index"  $ pWord16 i
         , term "class"  $ pByteString c
         , term "method" $ pByteString n
         , term "type"   $ K.pText (show t) ]
dumpPool (i, J.CString s) = judge "POOL-STRING" xs where
    xs = [ term "index" $ pWord16 i
         , term "value" $ pByteString s ]
dumpPool (i, J.CInteger n) = judge "POOL-INTEGER" xs where
    xs = [ term "index" $ pWord16 i
         , term "value" $ pWord32 n ]
dumpPool (i, J.CFloat _) = judge "POOL-FLOAT" xs where
    xs = [ term "index" $ pWord16 i
         , term "value" $ K.pText "unimplemented" ]
dumpPool (i, J.CLong _) = judge "POOL-LONG" xs where
    xs = [ term "index" $ pWord16 i
         , term "value" $ K.pText "unimplemented" ]
dumpPool (i, J.CDouble _) = judge "POOL-DOUBLE" xs where
    xs = [ term "index" $ pWord16 i
         , term "value" $ K.pText "unimplemented" ]
dumpPool (i, J.CNameType n t) = judge "POOL-NAME-TYPE" xs where
    xs = [ term "index" $ pWord16 i
         , term "name"  $ pByteString n
         , term "type"  $ K.pText (show t)]
dumpPool (i, J.CUTF8 s) = judge "POOL-UTF8" xs where
    xs = [ term "index" $ pWord16 i
         , term "value" $ pByteString s ]
dumpPool (i, J.CUnicode s) = judge "POOL-UNICODE" xs where
    xs = [ term "index" $ pWord16 i
         , term "value" $ pByteString s]

dumpField :: J.Field J.Direct -> String
dumpField f =
    judge "FIELD"
         [ term "accessor"   $ K.pSet (map K.pText $ accessFlagsText $ J.fieldAccessFlags f)
         , term "name"       $ K.pText (J.toString $ J.fieldName f)
         , term "type"       $ K.pText (show $ J.fieldSignature f)
         , term "attr-count" $ pWord16 $ J.fieldAttributesCount f ]

dumpMethod :: String -> J.Method J.Direct -> [String]
dumpMethod clsName m = gap about ++ gap method ++ gap code
    where
    methName = show $ J.methodName m
    about    = [ printf "about /class %s  /method %s" clsName methName ]
    method   = let J.MethodSignature args ret = J.methodSignature m
               in [judge "METHOD" [ term "args-type" $ K.pList ((K.pText . show) `map` args)
                                  , term "ret-type"  $ K.pText (show ret) ]]
    code     = case J.attrByName m "Code" of
                 Nothing -> ["** no code"]
                 Just bytecode ->
                     let inst = zip [1..] $ J.codeInstructions $ J.decodeMethod bytecode
                     in gap (map dumpInst inst) ++ gap (K.mapMaybe dumpInstDetail inst)

dumpInst :: (Int, J.Instruction) -> String
dumpInst (n, i) = judge "INST" [ term "seq"  $ K.pDecFromInt n
                               , term "inst" $ K.pText $ show i ]

dumpInstDetail :: (Int, J.Instruction) -> Maybe String
dumpInstDetail (n, J.INVOKEVIRTUAL i)      = dumpInvoke "VIRTUAL" n i
dumpInstDetail (n, J.INVOKESPECIAL i)      = dumpInvoke "SPECIAL" n i
dumpInstDetail (n, J.INVOKESTATIC i)       = dumpInvoke "STATIC"  n i
dumpInstDetail (n, J.INVOKEINTERFACE i _)  = dumpInvoke "INTERFACE" n i
dumpInstDetail (n, J.NEW i)                = dumpInvoke "NEW" n i
dumpInstDetail _ = Nothing

dumpInvoke :: String -> Int -> W.Word16 -> Maybe String
dumpInvoke op n i = Just $ jud where
    jud   = judge pat [ term "seq"   $ K.pDecFromInt n
                      , term "index" $ pWord16 i ]
    pat   = "INST-INVOKE-" ++ op

judge :: String -> [(String, K.VContent)] -> String
judge pat = K.judgeShow K.shortEmpty . K.JudgeAffirm pat

term :: String -> c -> (String, c)
term n c = (n, c)

pWord16 :: W.Word16 -> K.VContent
pWord16 i = K.pDecFromInt i' where
    i' = fromIntegral i :: Int

pWord32 :: W.Word32 -> K.VContent
pWord32 i = K.pDecFromInt i' where
    i' = fromIntegral i :: Int

pByteString :: B.ByteString -> K.VContent
pByteString = K.pText . J.toString
